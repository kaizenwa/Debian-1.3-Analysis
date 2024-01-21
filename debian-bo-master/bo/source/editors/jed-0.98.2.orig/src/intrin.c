/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */

#include "config.h"
#include "jed-feat.h"

/*{{{ Include Files */

#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"


#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <string.h>

#include "buffer.h"
#include "keymap.h"
#include "file.h"
#include "ins.h"
#include "ledit.h"
#include "screen.h"
#include "window.h"
#include "display.h"
#include "search.h"
#include "misc.h"
#include "replace.h"
#include "paste.h"
#include "sysdep.h"
#include "cmds.h"
#include "text.h"
#include "abbrev.h"
#include "indent.h"

#if JED_HAS_LINE_ATTRIBUTES
#include "lineattr.h"
#endif

#if JED_HAS_SUBPROCESSES
#include "jprocess.h"
#endif

#if defined(__DECC) && defined(VMS)
# include <unixlib.h>
#endif

#if defined(__GO32__) || defined(__WATCOMC__)
#define LANG_INTERRUPT i386_check_kbd
extern void i386_check_kbd (void);
#endif


#if defined(msdos) || defined(__os2__)
#include <process.h>
#endif

/*}}}*/

extern char Jed_Root_Dir[JED_MAX_PATH_LEN];

static void do_buffer_keystring (char *s) /*{{{*/
{
   buffer_keystring (s, strlen(s));
}

/*}}}*/

static void do_tt_write_string (char *s) /*{{{*/
{
   if (Batch)
     {
	fputs (s, stdout);
     }
   else tt_write_string (s);
}

/*}}}*/

#ifndef pc_system
static void do_tt_set_term_vtxxx (int *i) /*{{{*/
{
   tt_set_term_vtxxx (i);
}

/*}}}*/
#endif

static void autoload_n (void) /*{{{*/
{
   int n;
   char *file, *fun;
   
   if (SLang_pop_integer (&n)) return;
   while (n > 0)
     {
	n--;
	if (SLpop_string (&file)) return;
	if (SLpop_string (&fun))
	  {
	     SLFREE (file);
	     return;
	  }
	SLang_autoload (fun, file);
	SLFREE (file);
	SLFREE (fun);
	if (SLang_Error) break;
     }
}

/*}}}*/

static void add_to_completion_n (void) /*{{{*/
{
   int n;
   char *fun;
   
   if (SLang_pop_integer (&n)) return;
   while (n > 0)
     {
	n--;
	if (SLpop_string (&fun))
	  return;
	
	add_to_completion (fun);
	SLFREE (fun);
	if (SLang_Error) break;
     }
}

/*}}}*/

static unsigned char Translate_Region_Array[256];
static void translate_region (void) /*{{{*/
{
   Line *last;
   int last_pnt, len, one = 1;
   unsigned char ch, ch1;
   
   if (!check_region (&one)) return;
   Suspend_Screen_Update = 1;
   
   last = CLine; last_pnt = Point;
   
   pop_mark(&one);
   
   Translate_Region_Array ['\n'] = '\n';
   
   while (CLine != last)
     {
	len = CLine->len;
	while (Point < len)
	  {
	     ch = *(CLine->data + Point);
	     ch1 = Translate_Region_Array [ch];
	     if (ch == ch1) Point++;
	     else
	       {
		  del ();
		  ins ((char) ch1);
	       }
	  }
	CLine = CLine->next;  LineNum++;
	Point = 0;
     }
   
   while (Point < last_pnt)
     {
	ch = *(CLine->data + Point);
	if (ch != '\n')
	  {
	     del ();
	     ins ((char) Translate_Region_Array[ch]);
	  }
     }
   pop_spot();
}

/*}}}*/

static int jed_getpid (void) /*{{{*/
{
   return getpid ();
}

/*}}}*/

#if JED_HAS_COLOR_COLUMNS
static void set_column_colors (int *color, int *c0p, int *c1p) /*{{{*/
{
   unsigned char *p, *pmax, ch;
   int c1 = *c1p, c0 = *c0p - 1;
   
   ch = (unsigned char) *color;
   if ((*color < 0) || (ch >= JMAX_COLORS)) return;
   
   if (NULL == (p = CBuf->column_colors))
     {
	p = CBuf->column_colors = (unsigned char *) SLMALLOC (*tt_Screen_Cols);
	if (p == NULL) return;
	SLMEMSET ((char *) p, 0, *tt_Screen_Cols);
	CBuf->coloring_style = 1;
     }
   if (c1 > *tt_Screen_Cols) c1 = *tt_Screen_Cols;
   if (c0 < 0) c0 = 0;
   pmax = p + c1;
   p += c0;
   while (p < pmax) *p++ = ch;
}

/*}}}*/
static void set_color_object (int *i, char *fg, char *bg) /*{{{*/
{
   int obj = *i;
   if ((obj < 0) || (obj >= JMAX_COLORS)) return;
   tt_set_color (obj, NULL, fg, bg);
}

/*}}}*/
#endif

int map_color_object_to_number (char *what) /*{{{*/
{
   int i;
   
   if (!strcmp("normal", what)) i = JNORMAL_COLOR;
   else if (!strcmp("status", what)) i = JSTATUS_COLOR;
   else if (!strcmp("region", what)) i = JREGION_COLOR;
   else if (!strcmp("cursor", what)) i = JCURSOR_COLOR;
   else if (!strcmp("operator", what)) i = JOP_COLOR;
   else if (!strcmp("number", what)) i = JNUM_COLOR;
   else if (!strcmp("string", what)) i = JSTR_COLOR;
   else if (!strcmp("comment", what)) i = JCOM_COLOR;
   else if (!strncmp("delimiter", what, 5)) i = JDELIM_COLOR;
   /* Oops!-- earlier version had delimiter spelled delimeter.  This is
    * for backward compatability.
    */
   else if (!strcmp("preprocess", what)) i = JPREPROC_COLOR;
   else if (!strcmp("menu", what)) i = JMENU_COLOR;
   else if (!strcmp("message", what)) i = JMESSAGE_COLOR;
   else if (!strcmp("error", what)) i = JERROR_COLOR;
   else if (!strcmp("dollar", what)) i = JDOLLAR_COLOR;
#if JED_HAS_LINE_ATTRIBUTES
   else if (!strcmp("...", what)) i = JDOTS_COLOR;
#endif
   else if (!strncmp("keyword", what, 7))
     {
	char ch = what[7];
	if ((ch >= '0') && (ch <= '9')) i = JKEY_COLOR + (ch - '0');
	else i = JKEY_COLOR;
     }
   else i = -1;
   return i;
}

/*}}}*/

static int write_region_cmd (char *file) /*{{{*/
{
   char buf[JED_MAX_PATH_LEN];
   int n = write_region (file);
   if (n == -1) 
     {
	sprintf (buf, "Error writing region to %s", file);
	msg_error (buf);
     }
   return n;
}

/*}}}*/

static void enable_menu_bar (int *what) /*{{{*/
{
   Window_Type *w;
   
   /* find the top window */
   w = JWindow;
   
   while (w->top != Top_Window_Row) w = w->next;
   if (*what)
     {
	if (Top_Window_Row != 1) return;
	if (w->rows < 3)
	  {
	     /* window is too small --- fix it. */
	     one_window ();  w = JWindow;
	     if (w->rows < 3) return;
	  }
	w->top = Top_Window_Row = 2;
	w->rows -= 1;
     }
   else
     {
	if (Top_Window_Row == 1) return;
	w->top = Top_Window_Row = 1;
	w->rows += 1;
     }
   touch_screen ();
}

/*}}}*/

static int rename_file (char *f1, char *f2) /*{{{*/
{
   return rename (f1, f2);
}

/*}}}*/

static void set_term_colors(char *what, char *fg, char *bg) /*{{{*/
{
   int i;
   i = map_color_object_to_number (what);
   tt_set_color (i, what, fg, bg);
}

/*}}}*/

#ifndef pc_system
static void set_term_color_esc (char *what, char *esc) /*{{{*/
{
   int i;
   if (tt_set_color_esc == NULL) return;
   
   i = map_color_object_to_number (what);
   tt_set_color_esc (i, esc);
}

/*}}}*/
#endif


static void exit_error_cmd (char *msg, int *severity) /*{{{*/
{
   exit_error (msg, *severity);
}

/*}}}*/

static int do_prefix_argument(int *n) /*{{{*/
{
   int ret = *n;
   if (Repeat_Factor != NULL)
     {
	ret = *Repeat_Factor;
	Repeat_Factor = NULL;
     }
   return ret;
}

/*}}}*/

static void bury_buffer(char *name) /*{{{*/
{
   Buffer *b, *cp, *bp, *bn;
   
   if ((NULL == (b = find_buffer(name)))
       || (b == CBuf)
       || (CBuf == (bn = b->next))) return;
   
   cp = CBuf->prev;
   bp = b->prev;
   
   CBuf->prev = b;		       /* my definition of bury buffer */
   b->next = CBuf;
   b->flags |= BURIED_BUFFER;
   bp->next = bn;
   bn->prev = bp;
   cp->next = b;
   b->prev = cp;
}

/*}}}*/

static void set_buffer_hook (char *s, char *h) /*{{{*/
{
   SLang_Name_Type *f;
   
   f = SLang_get_function (h);
   
   if (!strcmp("par_sep", s)) CBuf->par_sep = f;
   else if (!strcmp("indent_hook", s)) CBuf->indent_hook = f;
   else if (!strcmp("wrap_hook", s)) CBuf->wrap_hook = f;
   else if (!strcmp("newline_indent_hook", s)) CBuf->newline_indent_hook = f;
   else if (!strcmp("bob_eob_error_hook", s)) CBuf->bob_eob_error_hook = f;
#ifdef HAS_MOUSE
   else if (!strcmp("mouse_down", s)) CBuf->mouse_down_hook = f;
   else if (!strcmp("mouse_up", s)) CBuf->mouse_up_hook = f;
   else if (!strcmp("mouse_drag", s)) CBuf->mouse_drag_hook = f;
# if JED_HAS_MULTICLICK
   else if (!strcmp("mouse_2click", s)) CBuf->mouse_2click_hook = f;
   else if (!strcmp("mouse_3click", s)) CBuf->mouse_3click_hook = f;
# endif
#endif
}

/*}}}*/

#ifdef __unix__
#ifndef __GO32__
static char *get_termcap_string (char *cap) /*{{{*/
{
   char *s;
   
   if (Batch) s = NULL;
   else s = SLtt_tgetstr (cap);
   if (s == NULL) s = "";
   return s;
}

/*}}}*/
#endif
#endif


static SLang_Name_Type Jed_Intrinsics [] = /*{{{*/
{
   MAKE_INTRINSIC(".setkey", set_key, VOID_TYPE, 2),
   /* Prototype: Void setkey(String fun, String key);
    * This function may be used to define a key sequence specified by the
    * string @key@ to the function @fun@.  @key@ can contain the @^@
    * character which denotes that the following character is to be
    * interpreted as a control character, e.g.,
    * @ setkey("bob", "^Kt");
    * sets the key sequence @Ctrl-K t@ to the function @bob@.
    * 
    * The @fun@ argument is usually the name of an internal or a user
    * defined S-Lang function.  However, if may also be a sequence of
    * functions or even another keysequence (a keyboard macro).  For
    * example, 
    * @ setkey ("bol;insert(string(whatline()))", "^Kw");
    * assigns the key sequence @Ctrl-K w@ to move to the beginning of a line
    * and insert the current line number.  For more information about this 
    * important function, see the JED User Manual.
    * 
    * Note that @setkey@ works on the "global" keymap. 
    * Related Functions: @unsetkey@, @definekey@
    */
     MAKE_INTRINSIC("._autoload", autoload_n, VOID_TYPE, 0),
   /* Prototype: Void _autoload (String fun, String fn, ..., Integer n);
    * The @_autoload@ function is like the @autoload@ function except that
    * it takes @n@ pairs of function name (@fun@) / filename (@fn@) pairs.
    * For example, 
    * @ _autoload ("fun_a", "file_a", "fun_b", "file_b", 2);
    * is equivalent to
    * @ autoload ("fun_a", "file_a");
    * @ autoload ("fun_b", "file_b");
    * Related Functions: @autoload@
    */
     MAKE_INTRINSIC(".push_mark", push_mark,VOID_TYPE, 0),
   /* Prototype: Void push_mark();
    * This function marks the current position as the beginning of a region.
    * and pushes other marks onto a stack.  A region is defined by this
    * mark and the editing point.  The mark is removed from the stack only
    * when the function @pop_mark@ is called. 
    * For example, 
    * @ define mark_buffer ()
    * @ {
    * @   bob (); 
    * @   push_mark ();
    * @   eob ();
    * @ }
    * marks the entire buffer as a region.
    * 
    * Related Functions: @pop_mark@, @push_spot@, @markp@, @dupmark@, @check_region@ 
    */
     
     MAKE_INTRINSIC(".bol", bol,VOID_TYPE, 0),
   /* Prototype: Void bol();
    * This function moves the current editing point to the beginning of the
    * current line.  The function @bolp@ may be used to see if one is already
    * at the beginning of a line.
    * 
    * Related Functions: @eol@, @bob@, @eob@, @bolp@ 
    */
     
     MAKE_INTRINSIC(".insert", insert_string,VOID_TYPE, 1),
   /* Prototype: Void insert (String str);
    * Inserts string @str@ into buffer at the current position.  The editing
    * point is moved to the end of the of the string that was inserted.
    * 
    * Related Functions: @del@, @insert_file@, @insbuf@ 
    */
     MAKE_INTRINSIC(".eol", eol,VOID_TYPE, 0),
   /* Prototype: Void eol();
    * Moves the current position to the end of the current line.  The function
    * @eolp@ may be used to see if one is at the end of a line or not.
    * Related Functions: @eolp@, @bol@, @bob@, @eob@ */
     MAKE_INTRINSIC(".setbuf", set_buffer, VOID_TYPE, 1),
   /* Prototype: Void setbuf(String buf);
    * Changes the default buffer to one named @buf@.  If the buffer does not
    * exist, it will be created. 
    * Note: This change only lasts until top
    * level of editor loop is reached at which point the the buffer
    * associated with current window will be made the default.  That is this
    * change should only be considered as temporary.  To make a long lasting
    * change, use the function @sw2buf@. 
    * Related Functions: @sw2buf@, @pop2buf@, @whatbuf@, @pop2buf_whatbuf@ 
    */
     
     MAKE_INTRINSIC(".message", message, VOID_TYPE, 1),
   /* Prototype: Void message(String msg);
    * The @message@ function displays the string @msg@ at the bottom of the
    * display where the minibuffer is located.  This in itself does not
    * immediately cause the message to be display.  In fact, it may not even
    * be displayed at all.  The message is only displayed when the editor
    * updates the screen.  If the @message@ function is called again before
    * the screen update, the new message will be the one that will be
    * displayed when the screen is finally updated.  For an immediate effect,
    * the function @flush@ should be used instead.
    * Related Functions: @flush@, @error@, @update@, @clear_message@ */
     
     MAKE_INTRINSIC("._add_completion", add_to_completion_n, VOID_TYPE, 0),
   /* Prototype: Void _add_completion (String f1, String f2, ..., Integer n);
    * The @_add_completion@ function is like the @add_completion@ function
    * except that it takes @n@ names @f1@, ... @fn@.
    * For example, 
    * @ _add_completion ("fun_a", "fun_b", 2);
    * is equivalent to
    * @ add_completion ("fun_a");
    * @ add_completion ("fun_b");
    * Related Functions: @add_completion@
    */
     
     MAKE_INTRINSIC(".del_region", delete_region, VOID_TYPE, 0),
   /* Prototype: Void del_region ();
    * This function deletes the region defined by the mark and the current 
    * editing point.  For example,
    * @ define delete_this_line ()
    * @ {
    * @   bol (); push_mark (); eol ();
    * @   del_region ();
    * @ }
    * defines a function that deletes all characters on the current line
    * from the beginning of the line until the end of the line.  It does not 
    * delete the line itself.
    * Related Functions: @push_mark@, @markp@, @check_region@
    */
     MAKE_INTRINSIC(".bufsubstr", buffer_substring, VOID_TYPE, 0),
   /* Prototype: String bufsubstr ();
    * This function returns a string that contains the characters in the
    * region specified by a mark and the current editing point.
    * If the region crosses lines, the string will contain newline
    * characters.
    * Related Functions: @insbuf@, @push_mark@
    */
     MAKE_INTRINSIC(".right", forwchars,INT_TYPE, 1),
   /* Prototype: Integer right(Integer n);
    * This function moves the editing position forward forward @n@
    * characters. It returns the number of characters actually moved.  The
    * number returned will be smaller than @n@ if the end of the buffer is
    * reached.
    * 
    * Related Functions: @left@, @up@, @down@, @eol@, @eob@ */
     
     MAKE_INTRINSIC(".definekey", set_key_in_keymap, VOID_TYPE, 3),
   /* Prototype: Void definekey(String f, String key, String kmap);
    * Unlike @setkey@ which operates on the global keymap, this function is
    * used for binding keys to functions in a specific keymap.  Here @f@ is
    * the function to be bound, @key@ is a string of characters that make up
    * the key sequence and @kmap@ is the name of the keymap to be used.  See
    * @setkey@ for more information about the arguments.
    * 
    * Related Functions: @setkey@, @undefinekey@, @make_keymap@, @use_keymap@ 
    */
     MAKE_INTRINSIC(".left", backwchars,INT_TYPE, 1),
   /* Prototype: Integer left(Integer n);
    * @left@ moves the editing point backward @n@ characters and returns the
    * number actually moved.  The number returned will be less than @n@ only
    * if the top of the buffer is reached. 
    * Related Functions: @right@, @up@, @down@, @bol@, @bob@ 
    */
     MAKE_INTRINSIC(".whatbuf", what_buffer, STRING_TYPE, 0),
   /* Prototype: String what_buffer();
    * @whatbuf@ returns the name of the current buffer.  It is usually used 
    * in functions when one wants to work with more than one buffer.  The 
    * function @setbuf_info@ may be used to change the name of the buffer.
    * Related Functions: @getbuf_info@, @bufferp@ 
    */
     MAKE_INTRINSIC(".error", msg_error, VOID_TYPE, 1),
   /* Prototype: Void error(String msg);
    * This function signals an error condition and displays the string @msg@
    * at the bottom of the display.  The error can be caught by an
    * @ERROR_BLOCK@ and cleared with the S-Lang @_clear_error@ function.
    * Unless caught, the error will abort execution of S-Lang functions and
    * the S-Lang stack will unwind to the top level.  For more information
    * about handling error conditions see the S-Lang User's Guide.
    * 
    * Related Functions: @message@, @flush@
    */
     MAKE_INTRINSIC(".getbuf_info", get_buffer_info,  VOID_TYPE, 0),
   /* Prototype: getbuf_info ();
    * This function returns values to the stack.  The four values from the
    * top are:
    * @ Integer  % buffer flags
    * @ String   % name of buffer
    * @ String   % directory associated with buffer
    * @ String   % name of file associated with buffer (if any).
    * The integer that corresponds to the buffer flags are encoded as:
    * @ bit 0: buffer modified
    * @ bit 1: auto save mode
    * @ bit 2: file on disk modified
    * @ bit 3: read only bit
    * @ bit 4: overwrite mode
    * @ bit 5: undo enabled
    * @ bit 6: buffer buried
    * @ bit 7: Force save upon exit.
    * @ bit 8: Do not backup
    * @ bit 9: File associated with buffer is a binary file
    * @ bit 10: Add CR to end of lines when writing buffer to disk.
    * @ bit 11: Abbrev mode 
    * For example, 
    * @ (file,,,flags) = getbuf_info();
    * returns the file and the flags associated with the current buffer.
    * Related Functions: @setbuf_info@, @whatbuf@ */
     MAKE_INTRINSIC(".otherwindow", other_window, VOID_TYPE, 0),
   /* Prototype: Void otherwindow ();
    * This function will make the next window in the ring of windows as the
    * default window. For example,
    * @ define zoom_next_window ()
    * @ {
    * @   otherwindow (); onewindow ();
    * @ }
    * defines a function that moves to the next window and then makes it the 
    * only window on the screen.
    * Related Functions: @nwindows@, @onewindow@
    * Related Variables: @MINIBUFFER_ACTIVE@
    */
     MAKE_INTRINSIC(".is_internal", is_internal, INT_TYPE, 1),
   /* Prototype: Integer is_internal(String f);
    * @is_internal@ returns non-zero is function @f@ is defined as an
    * internal function or returns zero if not.  Internal functions not
    * immediately accessable from S-Lang; rather, they must be called using
    * the @call@ function.  See also the related S-Lang function
    * @is_defined@ in the S-Lang Programmer's Reference.
    * Related Functions: @call@ */
     MAKE_INTRINSIC(".setbuf_info", set_buffer_info,  VOID_TYPE, 4),
   /* Prototype: Void setbuf_info(String file, String dir, String buf, Integer flags);
    * This function may be used to change attributes regarding the current
    * buffer.  It performs the opposite function of the related function
    * @getbuf_info@.   Here @file@ is the name of the file to be associated
    * with the buffer; @dir@ is the directory to be associated with the
    * buffer; @buf@ is the name to be assigned to the buffer, and @flags@
    * describe the buffer attributes.  See @getbuf_info@ for a discussion of
    * @flags@.  Note that the actual file associated with the buffer is
    * located in directory @dir@ with the name @file@.
    * For example, the function
    * @ define set_overwrite_mode ()
    * @ {
    * @    variable dir, file, flags, name;
    * @    (file, dir, name, flags) = getbuf_info ();
    * @    flags = flags | (1 shl 4);
    * @    setbuf_info (file, dir, name, flags);
    * @ }
    * may be used to turn on overwrite mode for the current buffer.  Note
    * that it is better exploit the fact that S-Lang is a stack based language
    * and simply write the above function as:
    * @ define set_overwrite_mode ()
    * @ {
    * @    setbuf_info (getbuf_info () | 0x10);
    * @ }
    * Here, @(1 shl 4)@ has been written as the hexidecimal number @0x10@.
    * Related Functions: @getbuf_info@, @setbuf@, @whatbuf@
    */
     MAKE_INTRINSIC(".up", prevline, INT_TYPE, 1),
   /* Prototype: Integer up(Integer n);
    * This function moves the current point up @n@ lines and returns the
    * number of lines actually moved.  The number returned will be less than
    * @n@ only if the top of the buffer is reached.
    * Related Functions: @down@, @left@, @right@ 
    */
     MAKE_INTRINSIC(".down", nextline,INT_TYPE, 1),
   /* Prototype: Integer down(Integer n);
    * The @down@ function is used to move the editing point down a number of
    * lines specified by the integer @n@.  It returns the number of lines
    * actually moved.  The number returned will be less than @n@ only if the
    * last line of the buffer has been reached.  The editing point will be
    * left at the beginning of the line if it succeeds in going down more
    * than one line.
    * Example: The function
    * @ define trim_buffer
    * @ {
    * @   bob ();
    * @   do 
    * @     {
    * @        eol (); trim ();
    * @     }
    * @   while (down (1));
    * @ }
    * removes excess whitespace from the end of every line in the buffer.
    * Related Functions: @down@, @left@, @right@, @goto_line@
    */
     MAKE_INTRINSIC(".call", call_cmd, VOID_TYPE, 1),
   /* Prototype: Void call(String f);
    * The @call@ function is used to execute an internal function which is
    * not directly accessable to the S-Lang interpreter.
    * Related Functions: @is_internal@ */
     MAKE_INTRINSIC(".eob", eob,VOID_TYPE, 0),
   /* Prototype: Void eob();
    * The @eob@ function is used to move the current point to the end of the
    * buffer.  The function @eobp@ may be used to see if the current
    * position is at the end of the buffer.
    * 
    * Related Functions: @eobp@, @bob@, @bol@, @eol@ */
     
     MAKE_INTRINSIC(".unsetkey", unset_key, VOID_TYPE, 1),
   /* Prototype: Void unsetkey(String key);
    * This function is used to remove the definition of the key sequence
    * @key@ from the "global" keymap.  This is sometimes necessary to bind
    * new key sequences which conflict with other ones.  For example, the
    * "global" keymap binds the keys @"^[[A"@, @"^[[B"@, @"^[[C"@, and
    * @"^[[D"@ to the character movement functions.  Using
    * @unsetkey("^[[A")@ will remove the binding of @"^[[A"@ from the global
    * keymap but the other three will remain.  However, @unsetkey("^[[")@
    * will remove the definition of all the above keys.  This might be
    * necessary to bind, say, @"^[["@ to some function.
    * Related Functions: @setkey@, @undefinekey@
    */
     MAKE_INTRINSIC(".bob", bob,VOID_TYPE, 0),
   /* Prototype: Void bob ();
    * The function @bob@ is used to move the current editing point to the
    * beginning of the buffer.  The function @bobp@ may be used to determine
    * if the editing point is at the beginning of the buffer or not.
    * Related Functions: @bobp@, @eob@, @bol@, @eol@
    */
     MAKE_INTRINSIC(".looking_at", looking_at, INT_TYPE, 1),
   /* Prototype: Integer looking_at (String s);
    * This function returns non-zero if the characters immediately following
    * the current editing point match the string specified by @s@.  Whether
    * the match is case-sensitive or not depends upon the value of the
    * variable @CASE_SEARCH@.  The function returns zero if there is no match.
    * Related Functions: @ffind@, @fsearch@, @re_fsearch@, @bfind@
    */
     MAKE_INTRINSIC(".del", del,VOID_TYPE, 0),
   /* Prototype: Void del ();
    * The @del@ function deletes the character at the current editing
    * position.  If the position is at the end of the buffer, nothing happens.
    * If the deletion occurs at the end of a line, the next line will be joined
    * with the current one.
    * Related Functions: @eobp@, @erase_buffer@, @insert@
    */
     MAKE_INTRINSIC(".markp", markp, INT_TYPE, 0),
   /* Prototype: Void markp ();
    * This function returns a non-zero value if the mark is set; otherwise, it
    * returns zero.  If a mark is set, a region is defined.
    * Related Functions: @push_mark@, @pop_mark@, @check_region@, @push_spot@
    */
     MAKE_INTRINSIC(".nwindows", num_windows,INT_TYPE, 0),
   /* Prototype: Integer nwindows ();
    * The @nwindows@ function returns the number of windows currently visible.
    * If the variable @MINIBUFFER_ACTIVE@ is non-zero, the minibuffer is busy and 
    * contributes to the number of windows.
    * Related Functions: @splitwindow@, @onewindow@, @window_size@
    * Related Variables: @MINIBUFFER_ACTIVE@
    */
     
     MAKE_INTRINSIC(".add_completion", add_to_completion, VOID_TYPE, 1),
   /* Prototype: Void add_completion(String f);
    * The @add_completion@ function adds the user defined S-Lang function
    * with name specified by the string @f@ to the list of functions that
    * are eligible for mini-buffer completion.  The function specified by
    * @f@ must be already defined before this function is called.  The
    * S-Lang function @is_defined@ may be used to test whether or not the
    * function is defined.
    * Related Functions: @read_with_completion@, @_add_completion@
    */
     MAKE_INTRINSIC(".what_column", calculate_column,INT_TYPE, 0),
   /* Prototype: Integer what_column ();
    * The @what_column@ function returns the current column number expanding
    * tabs, control characters, etc...  The beginning of the line is at
    * column number one. 
    * Related Functions: @whatline@, @whatpos@, @goto_column@, @bolp@, @eolp@ */
     MAKE_INTRINSIC(".eobp", eobp,INT_TYPE, 0),
   /* Prototype: Integer eobp ();
    * The functio @eobp@ is used to determine if the current position is at
    * the end of the buffer or not.  It returns a non-zero value if at the
    * end of the buffer or zero if not.
    * Related Functions: @eob@, @bolp@, @eolp@
    */
     
     MAKE_INTRINSIC(".set_mode", set_mode_cmd, VOID_TYPE, 2),
   /* Prototype:  Void set_mode(String mode, Integer flags);
    * This function sets buffer mode flags and status line mode name.  @mode@
    * is a string which is displayed on the status line if the @%m@ status
    * line format specifier is used. The second argument, @flags@ is an
    * integer with the possible values:
    * @ 0 : no mode. Very generic
    * @ 1 : Wrap mode.  Lines are automatically wrapped at wrap column.
    * @ 2 : C mode.
    * @ 4 : Language mode.  Mode does not wrap but is useful for computer 
    * @     languages.
    * @ 8 : S-Lang mode
    * @ 16: Fortran mode highlighting
    * @ 32: TeX mode highlighting
    * 
    * Related Functions: @whatmode@, @getbuf_info@, @setbuf_info@.  
    */
     
     MAKE_INTRINSIC(".buffer_visible", jed_buffer_visible, INT_TYPE, 1),
   /* Prototype: Integer buffer_visible (String buf);
    * This function is used to determine whether or not a buffer with name
    * specified by the string @buf@ is in a window or not.  More explicitly,
    * it returns the number of windows containing @buf@.  This means that if
    * @buf@ does not occupy a window, it returns zero.  For Example,
    * @ define find_buffer_in_window (buf)
    * @ {
    * @    !if (buffer_visible (buf)) return 0;
    * @    pop2buf (buf);
    * @    return 1;
    * @ }
    * is a function that moves to the window containing @buf@ if @buf@ is in
    * a window.
    * Related Functions: @bufferp@, @nwindows@
    */
     
     MAKE_INTRINSIC(".exit_jed", exit_jed, VOID_TYPE, 0),
   /* Prototype: Void exit_jed ();
    * This function should be called to exit JED is a graceful and safe
    * manner.  If any buffers have been modified but not saved, the user is
    * queried about whether or not to save each one first.  @exit_jed@ calls
    * the S-Lang hook @exit_hook@ if it is defined.  If @exit_hook@ is
    * defined, it must either call @quit_jed@ or @exit_jed@ to really exit
    * the editor.  If @exit_jed@ is called from @exit_hook@, @exit_hook@ will
    * not be called again.  For example:
    * @ define exit_hook ()
    * @ {
    * @   flush ("Really Exit?");
    * @   
    * @   forever 
    * @     {
    * @       switch (getkey () & 0x20)    % map to lowercase
    * @        { case 'y': exit_jed (); }
    * @        { case 'n': return; }
    * @       beep ();
    * @     }
    * @ }
    * may be used to prompt user for confirmation of exit.
    * Related Functions: @quit_jed@, @suspend@, @flush@, @getkey@
    * Related Variables: @BATCH@
    */
     MAKE_INTRINSIC(".set_color", set_term_colors, VOID_TYPE, 3),
   /* Prototype: Void set_color(String obj, String fg, String bg);
    * This function sets the foreground and background colors of an object
    * specified by the string @obj@ to @fg@ and @bg@.  The exact values of
    * the strings @fg@ and @bg@ are system dependent.  For the X-Window
    * system, they can be any string that the server understands, e.g.,
    * @"SteelBlue"@.  For other systems, the color must be one of the
    * following:
    * @ "black"            "gray"         
    * @ "red"              "brightred"    
    * @ "green"            "brightgreen"  
    * @ "brown"            "yellow"       
    * @ "blue"             "brightblue"   
    * @ "magenta"          "brightmagenta"
    * @ "cyan"             "brightcyan"   
    * @ "lightgray"        "white"        
    * One most terminals, the values in the second column have no affect
    * when used as the background color.
    * 
    * The valid names for @obj@ are:
    * @ "normal"      Default foreground/background
    * @ "status"      The status window line
    * @ "region"      Highlighted Regions
    * @ "cursor"      Text Cursor (X-Windows)
    * @ "menu"        The menu bar
    * @ "error"       Error messages
    * @ "message"     Other messages
    * @ "dollar"      Color of the indicator that text extends beyond the
    * @                boundary of the window.
    * If color syntax highlighting is enabled, the following object names
    * are also meaningful:
    * @ "number"      Numbers in C-mode and Equations in TeX-mode
    * @ "delimiter"   Commas, semi-colons, etc...
    * @ "keyword"     Language dependent
    * @ "string"      Literal strings
    * @ "comment"     Comments
    * @ "operator"    Such as +, -, etc...
    * @ "preprocess"  Preprocessor lines
    * If line attributes are available, then you may also specifiy the color 
    * of the hidden line indicator:
    * @ "..."         Hidden line indicator
    * See a discussion in the JED User Manual for more discussion.
    * Related Functions: @set_color_esc@, @set_column_colors@, @set_color_object@
    * Related Variables: @WANT_SYNTAX_HIGHLIGHT@, @USE_ANSI_COLORS@
    */
     MAKE_INTRINSIC(".color_number", map_color_object_to_number, INT_TYPE, 1),
   /* Prototype: Integer color_number (String obj);
    * This function returns the object number associated with the
    * string @obj@.  Valid names for @obj@ are as per @set_color@.
    * 
    * Related Functions: @set_color@, @set_column_colors@
    * Related Variables: @WANT_SYNTAX_HIGHLIGHT@, @USE_ANSI_COLORS@
    */     
     
#ifndef pc_system
     MAKE_INTRINSIC(".set_color_esc", set_term_color_esc, VOID_TYPE, 2),
   /* Prototype: Void set_color_esc (String object, String esc_seq);
    * This function may be used to associate an escape sequence with an
    * object.  The escape sequence will be sent to the terminal prior to
    * sending updating the object.  It may be used on mono terminals to
    * underline objects, etc...  The object names are the same names used by
    * the @set_color@ function. 
    * Note: Care should be exercised when using
    * this function.  Also, one may need to experiment around a little to
    * get escape sequences that work together.
    * Related Functions: @set_color@
    */
#endif
     MAKE_INTRINSIC(".extract_filename", extract_file, STRING_TYPE, 1),
   /* Prototype: String extract_filename (String filespec);
    * This function may be used to separate the file name from the path of
    * of a file specified by @filespec@.  For example, under Unix, the
    * expression
    * @ extract_filename ("/tmp/name");
    * returns the string @"name"@.
    * Related Functions: @expand_filename@
    */
     MAKE_INTRINSIC(".trim", trim_whitespace,VOID_TYPE, 0),
   /* Prototype: Void trim (); 
    * The @trim@ function removes all whitespace around the current editing
    * point.  In this context, whitespace is considered to be any
    * combination of tab and space characters.  In particular, it does not
    * include the newline character.  This means that the @trim@ function
    * will not delete across lines. 
    * Related Functions: @skip_chars@, @skip_white@, @del@, @del_region@
    */
     MAKE_INTRINSIC(".pop2buf", pop_to_buffer, VOID_TYPE, 1),
   /* Prototype: Void pop2buf (String buf);
    * The @pop2buf@ function will switch to another window and display the
    * buffer specified by @buf@ in it.  If @buf@ does not exist, it will be
    * created. If @buf@ already exists in a window, the window containing
    * @buf@ will become the active one.  This function will create a new
    * window if necessary.  All that is guaranteed is that the current
    * window will continue to display the same buffer before and after the
    * call to @pop2buf@.
    * 
    * Related Functions: @whatbuf@, @pop2buf_whatbuf@, @setbuf@, @sw2buf@, @nwindows@
    */
     MAKE_INTRINSIC(".pop2buf_whatbuf", pop_to_buffer, STRING_TYPE, 1),
   /* Prototype: String pop2buf_whatbuf (String buf);
    * This function performs the same function as @pop2buf@ except that the
    * name of the buffer that @buf@ replaced in the window is returned.
    * This allows one to restore the buffer in window to what it was before
    * the call to @pop2buf_whatbuf@.
    * Related Functions: @pop2buf@, @whatbuf@
    */
#ifdef REAL_UNIX_SYSTEM
     MAKE_INTRINSIC(".copy_file", jed_copy_file, INT_TYPE, 2),
   /* Prototype: Integer copy_file (String src, String dest);
    * This function may be used to copy a file named @src@ to a new file
    * named @dest@.  It attempts to preserve the file access and modification 
    * times as well as the ownership and protection.
    * 
    * It returns @0@ upon success and @-1@ upon failure.
    * 
    * Related Functions: @rename_file@, @file_status@
    */
#endif
#ifndef pc_system
     MAKE_VARIABLE(".USE_ANSI_COLORS", &tt_Use_Ansi_Colors, INTP_TYPE, 0),
#endif
#ifdef REAL_UNIX_SYSTEM
     MAKE_VARIABLE(".BACKUP_BY_COPYING", &Jed_Backup_By_Copying, INT_TYPE, 0),
     /* Prototype: Integer BACKUP_BY_COPYING = 0;
      * If non-zero, backup files will be made by copying the original file
      * to the backup file.  If zero, the backup file will be created by
      * renaming the original file to the backup file.
      * Related Functions: @rename_file@, @copy_file@
      */
#endif
     MAKE_VARIABLE(".Status_Line_String", Default_Status_Line, STRING_TYPE, 1),
   /*A read-only String variable containing the format of the status line
    * applied to newly created buffers.
    Related Functions: set_status_line */
     MAKE_VARIABLE(".MINIBUFFER_ACTIVE", &MiniBuffer_Active, INT_TYPE, 1),
   /*A read-only variable that is non-zero when the MiniBuffer is actively
    * being used. */
     
     MAKE_VARIABLE(".DISPLAY_EIGHT_BIT", &Display_Eight_Bit, INT_TYPE, 0),
   /* Prototype: Integer DISPLAY_EIGHT_BIT;
    * This variable determines how characters with the high bit set are to
    * be displayed.  Specifically, any character whose value is greater than
    * or equal to the value of @DISPLAY_EIGHT_BIT@ is output to the terminal
    * as is.  Characters with the high bit set but less than this value are
    * sent to the terminal in a multiple character representation. For Unix
    * and VMS systems the value should be set to 160.  This is because many
    * terminals use the characters with values between 128 and 160 as eight
    * bit control characters.  For other systems, it can be set to zero.
    * Related Variables: @META_CHAR@
    */
     MAKE_VARIABLE(".JED_CSI", &JED_CSI, INT_TYPE, 0),
   /* Control Sequence Introducer. --- reserved for future use */
     MAKE_INTRINSIC(".copy_region", copy_region_cmd, VOID_TYPE, 1),
   /* Prototype: Void copy_region (String buf);
    * This function may be used to copy a region defined by a mark and the
    * current position to the buffered specified by the name @buf@. It does
    * not delete the characters in region but it does pop the mark that
    * determines the region.
    * Related Functions: @insbuf@, @bufsubstr@, @push_mark@, @pop_mark@, @bufferp@
    */
     MAKE_INTRINSIC(".insbuf", insert_buffer_name, VOID_TYPE, 1),
   /* Prototype: Void insbuf (String buf);
    * This function may be used to insert the contents of a buffer specified
    * by the name @buf@ into the current buffer.  The editing position is
    * advanced to the end of the insertion.
    * Related Functions: @copy_region@, @narrow@, @narrow_to_region@
    */
     MAKE_INTRINSIC(".bolp", bolp,INT_TYPE, 0),
   /* Prototype: Integer bolp ();
    * @bolp@ is used to test if the current position is at the beginning of
    * a line or not.  It returns non-zero if the position is at the
    * beginning of a line or zero if not. 
    * Related Functions: @bol@, @eolp@, @bobp@, @eobp@
    */
     MAKE_INTRINSIC(".beep", beep, VOID_TYPE, 0),
   /* Prototype: Void beep ();
    * The @beep@ function causes the terminal to beep according to the value
    * of the variable @IGNORE_BEEP@.
    * Related Functions: @tt_send@
    * Related Variables: @IGNORE_BEEP@
    */
     MAKE_INTRINSIC(".onewindow", one_window,VOID_TYPE, 0),
   /* Prototype: Void onewindow ();
    * This function deletes all other windows except the current window and
    * the mini-buffer window.
    * Related Functions: @nwindows@, @splitwindow@, @enlargewin@
    * Related Variables: @MINIBUFFER_ACTIVE@
    */
     MAKE_INTRINSIC(".pop_spot", pop_spot,VOID_TYPE, 0),
   /* Prototype: Void pop_spot ();
    * This function is used after a call to @push_spot@ to return to the
    * editing position at the last call to @push_spot@ in the current buffer.
    * Related Functions: @push_spot@, @pop_mark@
    */
     MAKE_INTRINSIC(".push_spot", push_spot,VOID_TYPE, 0),
   /* Prototype: Void push_spot ();
    * @push_spot@ pushes the location of the current buffer location onto a
    * stack.  This function does not set the mark.  The function @push_mark@
    * should be used for that purpose. The spot can be returned to using the
    * function @pop_spot@. 
    * Note: Spots are local to each buffer.  It is not
    * possible to call @push_spot@ from one buffer and then subsequently
    * call @pop_spot@ from another buffer to return to the position in the
    * first buffer.  For this purpose, one must use user marks instead.
    * Related Functions: @pop_spot@, @push_mark@, @create_user_mark@
    */
     MAKE_INTRINSIC(".sw2buf", switch_to_buffer_cmd, VOID_TYPE, 1),
   /* Prototype: Void sw2buf (String buf);
    * This function is used to switch to another buffer whose name is
    * specified by the parameter @buf@.  If the buffer specified by @buf@
    * does not exist, one will be created. 
    * Note: Unlike @setbuf@, the change
    * to the new buffer is more permanent in the sense that when control
    * passed back out of S-Lang to the main editor loop, if the current
    * buffer at that time is the buffer specified here, this buffer will be
    * attached to the window.
    * Related Functions: @setbuf@, @pop2buf@, @bufferp@
    */
     MAKE_INTRINSIC(".tt_send", do_tt_write_string, VOID_TYPE, 1),
   /* Prototype: Void tt_send (String s);
    * This function may be used to send a string specified by @s@ directly
    * to the terminal with no interference by the editor.  One should
    * exercise caution when using this routine since it may interfere with
    * JED's screen management routines forcing one to redraw the screen.
    * Nevertheless, it can serve a useful purpose.  For example, when run in
    * an XTerm window, using
    * @ tt_send ("\e[?9h");
    * will enable sending mouse click information to JED encoded as
    * keypresses.
    * Related Functions: @beep@
    */
     MAKE_INTRINSIC(".eolp", eolp,INT_TYPE, 0),
   /* Prototype: Integer eolp ();
    * This function may be used to determine whether or not the current
    * position is at the end of a line ot not.  If it is, the routine
    * returns a non-zero value; otherwise it returns zero.
    * Related Functions: @eol@, @bolp@, @eobp@, @bobp@
    */
     MAKE_INTRINSIC(".what_keymap", what_keymap, STRING_TYPE, 0),
   /* Prototype: String what_keymap ();
    * This function returns the name of the keymap associated with the
    * current buffer.
    * Related Functions: @create_keymap@, @keymap_p@
    */
     MAKE_INTRINSIC(".find_file", find_file_in_window, INT_TYPE, 1),
   /* Prototype: Integer find_file (String name);
    * The @find_file@ function switches to the buffer associated with the
    * file specified by @name@.  If no such buffer exists, one is created
    * and the file specified by @name@ is read from the disk and associated
    * with the new buffer.  The buffer will also become attached to the
    * current window.  Use the @read_file@ function to find a file but not
    * associate it with the current window.
    * Related Functions: @read_file@
    */
     MAKE_INTRINSIC(".set_status_line", set_status_format, VOID_TYPE, 2),
   /* Prototype: set_status_line (String format, Integer flag);
    * This function may be used to customize the status line of the current
    * window according to the string @format@.  If the second parameter
    * @flag@ is non-zero, @format@ will apply to the global format string;
    * otherwise it applies to current buffer only.  Newly created buffer
    * inherit the global format string when they appear in a window.
    * The format string may contain the following format specifiers:
    * @ %b   buffer name
    * @ %f   file name
    * @ %v   JED version
    * @ %t   current time --- only used if variable DISPLAY_TIME is non-zero
    * @ %p   line number or percent string
    * @ %%   literal '%' character
    * @ %m   mode string
    * @ %a   If abbrev mode, expands to "abbrev"
    * @ %n   If buffer is narrowed, expands to "Narrow"
    * @ %o   If overwrite mode, expands to "Ovwrt"
    * @ %c   If the variable LINENUMBERS is 2, this expands to the current
    * @        column number. 
    * For example, the default status line used by JED's EDT emulation uses
    * the format string:
    * @ "(Jed %v) EDT: %b   (%m%a%n%o)  %p,%c   Advance   %t"
    * Related Functions: @set_mode@, @narrow@, @whatbuf@, @getbuf_info@
    * Related Variables: @DISPLAY_TIME@, @LINENUMBERS@
    */
     MAKE_INTRINSIC(".bury_buffer", bury_buffer, VOID_TYPE, 1),
   /* Prototype: Void bury_buffer (String name);
    * The @bury_buffer@ function may be used to make it unlikely for the
    * buffer specified by the paramter @name@ to appear in a window.
    * Related Functions: @sw2buf@, @getbuf_info@
    */
     
     MAKE_INTRINSIC(".dupmark", dup_mark, INT_TYPE, 0),
   /* Prototype: Integer dupmark ();
    * This function returns zero if the mark is not set or, if the mark is
    * set, a duplicate of it is pushed onto the mark stack and a non-zero
    * value is returned.
    * Related Functions: @push_mark@, @markp@, @pop_mark@
    */
     MAKE_INTRINSIC(".erase_buffer", erase_buffer, VOID_TYPE, 0),
   /* Prototype: erase_buffer ();
    * The @erase_buffer@ function erases all text from the current buffer.
    * However, it does not delete the buffer itself. 
    * 
    * Note: This function destroys all undo information associated with the
    * buffer making it impossible to undo the result of this function. 
    * Related Functions: @delbuf@, @del@
    */
     MAKE_INTRINSIC(".window_info", window_size_intrinsic, INT_TYPE, 1),
   /* Prototype: Integer window_info(Integer item);
    * The @window_info@ function returns information concerning the current
    * window.  The actual information that is returned depends on the @item@
    * parameter.  Acceptable values of @item@ and the description of the
    * information returned is given in the following table:
    * @ 'r'  : Number of rows
    * @ 'w'  : Width of window 
    * @ 'c'  : Starting column (from 1)
    * @ 't'  : Screen line of top line of window (from 1)
    * Related Functions: @otherwindow@, @nwindows@
    * Related Variables: @SCREEN_HEIGHT@, @SCREEN_WIDTH@
    */
#ifndef Sixteen_Bit_System
     MAKE_INTRINSIC(".copy_region_to_kill_array", copy_region_to_kill_array, VOID_TYPE, 1),
   /* Prototype: Void copy_region_to_kill_array (Integer n);
    * This function copies the currently defined region to the nth element,
    * specified by @n@, of an internal array of character strings replacing
    * what is currently there. 
    * 
    * Note: This function is not available on 16 bit systems. 
    * 
    * Related Functions: @insert_from_kill_array@, @append_region_kill_array@ 
    * Related Variables: @KILL_ARRAY_SIZE@
    */
     
     MAKE_INTRINSIC(".append_region_to_kill_array", append_region_to_kill_array, VOID_TYPE, 1),
   /* Prototype: Void append_region_to_kill_array (Integer n);
    * This function appends the currently defined region to the contents of
    * nth element, specified by @n@, of an internal array of character strings.
    * 
    * Note: This function is not available on 16 bit systems. 
    * 
    * Related Functions: @insert_from_kill_array@, @copy_region_to_kill_array@ 
    * Related Variables: @KILL_ARRAY_SIZE@
    */
     
     MAKE_INTRINSIC(".insert_from_kill_array", insert_from_kill_array, VOID_TYPE, 1),
   /* Prototype: Void insert_from_kill_array (Integer n);
    * This function inserts the contents of the nth element, specified by
    * @n@, of an internal array of character strings.
    * 
    * Note: This function is not available on 16 bit systems. 
    * 
    * Related Functions: @insert_from_kill_array@, @copy_region_to_kill_array@ 
    * Related Variables: @KILL_ARRAY_SIZE@
    */
     
     MAKE_VARIABLE(".KILL_ARRAY_SIZE", &Kill_Array_Size, INT_TYPE, 1),
   /* Prototype: Integer KILL_ARRAY_SIZE;
    * This variable contains the value of the size of of the internal kill
    * array of character strings.  Any number from zero upto but not
    * including the value of @KILL_ARRAY_SIZE may be used as an argument in
    * the functions that manipulate this array.
    * 
    * Note: This variable is a read-only varaible and it is not available on
    * 16 bit systems. 
    * 
    * Related Functions: @insert_from_kill_array@, @copy_region_to_kill_array@, @append_region_to_kill_array@
    */
#endif
     MAKE_VARIABLE(".what_line", &LineNum,INT_TYPE, 1),
   /* Prototype: Integer what_line ();
    * This function returns the value of the current line number.  Lines are
    * numbered from one. Note: The actual number is measured from the top of
    * the buffer which is itself is affected by whether the buffer is
    * narrowed or not.  For example, 
    * @ define one ()
    * @ {
    * @   push_mark (); narrow
    * @   push_mark (); narrow ();
    * @   return what_line ();
    * @ }
    * always returns 1.
    * Related Functions: @goto_line@, @what_column@, @whatbuf@
    */
     MAKE_VARIABLE(".BLINK", &Blink_Flag, INT_TYPE, 0),
   /* Prototype: Integer BLINK = 1;
    * The @BLINK@ variable determines whether or not matching parenthesis
    * are blinked.  If its value is one, they are blinked; otherwise, they
    * are not.
    * Related Variable: @C_INDENT@, @C_BRA_NEWLINE@
    */
     MAKE_VARIABLE(".WRAP_INDENTS", &Indented_Text_Mode, INT_TYPE, 0),
   /* Prototype: Integer WRAP_INDENTS = 1;
    * If this variable is non-zero, after a line is wrapped, the new line
    * will start at the same indentation as the current one.  On the other
    * hand, if the value of @WRAP_INDENTS@ is zero, the new line will begin
    * in the first column.
    * Related Variables:
    */
     MAKE_INTRINSIC(".goto_column", goto_column, VOID_TYPE, 1),
   /* Prototype: Void goto_column (Integer n);
    * This function moves the current editing point to the column specified
    * by the parameter @n@.  It will insert a combination of spaces and tabs
    * if necessary to achieve the goal.
    * Note: The actual character number offset from the beginning of the
    * line depends upon tab settings and the visual expansion of other
    * control characters.
    * Related Functions: @goto_column_best_try@, @what_column@, @left@, @right@, @goto_line@
    * Related Variables: @TAB@, @TAB_DEFAULT@, @DISPLAY_EIGHT_BIT@
    */
     MAKE_INTRINSIC(".goto_column_best_try", goto_column1, INT_TYPE, 1),
   /* Prototype: Integer goto_column_best_try (Integer c);
    * This function is like @goto_column@ except that it will not insert
    * whitespace.  This means that it may fail to achieve the column number
    * specified by the argument @c@.  It returns the current column number.
    * Related Functions: @goto_column@, @what_column@
    */
     MAKE_VARIABLE(".TAB_DEFAULT", &Jed_Tab_Default, INT_TYPE, 0),
   /* Prototype: Integer TAB_DEFAULT = 8;
    * @TAB_DEFAULT@ is the default tab setting given to all newly created
    * buffers.  The variable @TAB@ may be used to change the current
    * buffer's tab setting.  A value of zero means that tab characters are
    * not expanded and that tabs are never used to produce whitespace.
    * Related Variables: @TAB@, @DISPLAY_EIGHT_BIT@, @USE_TABS@
    */
     MAKE_INTRINSIC(".goto_line", goto_line,VOID_TYPE, 1),
   /* Prototype: Void goto_line (Integer n);
    * The @goto_line@ function may be used to move to a specific line number
    * specified by the parameter @n@.
    * Note: The actual column that the editing point will be left in is
    * indeterminate.
    * Related Functions: @what_line@, @goto_column@, @down@, @up@.
    */
     MAKE_INTRINSIC(".pop_mark", pop_mark, VOID_TYPE, 1),
   /* Prototype: pop_mark (Integer g);
    * @pop_mark@ pops the most recent mark pushed onto the mark stack.  If
    * the argument @g@ is non-zero, the editing position will be moved to
    * the location of the mark.  However, if @g@ is zero, the editing
    * position will be unchanged.
    * Related Functions: @push_mark@, @pop_spot@, @markp@, @check_region@, @goto_user_mark@
    */
     MAKE_INTRINSIC(".read_mini", mini_read, VOID_TYPE, 3),
   /* Prototype: String read_mini (String prompt, String dflt, String init);
    * The @read_mini@ function reads a line of input from the user in the
    * mini-buffer.  The first parameter, @prompt@, is used to prompt the
    * user.  The second parameter, @dflt@, is what is returned as a default
    * value if the user simply presses the return key.  The final parameter,
    * @init@, is stuffed into the mini-buffer for editing by the user.
    * For example, 
    * @ define search_whole_buffer ()
    * @ {
    * @   variable str;
    * @   str = read_mini ("Search for:", "", "");
    * @   !if (strlen (str)) return;
    * @   !if (fsearch (str))
    * @      {      
    * @        push_mark (); bob ();
    * @        if (fsearch (str)) pop_mark (0);
    * @        else pop_mark (1);
    * @          {
    * @             pop_mark (1);
    * @             error ("Not found");
    * @          }
    * @      }
    * @ }
    * reads a string from the user and then searches forward for it and if
    * not found, it resumes the search from the beginning of the buffer.
    * Note: If the user aborts the function @mini_read@ by pressing the
    * keyboard quit character (e.g., Ctrl-G), an error is signaled.  This
    * error can be caught by an @ERROR_BLOCK@ and the appropriate action
    * taken. Also if the mini-buffer is already in use, this function should
    * not be called.  The variable @MINIBUFFER_ACTIVE@ may be checked to
    * determine if this is the case or not.
    * Related Functions: @read_with_completion@, @getkey@, @input_pending@ 
    * Related Variables: @MINIBUFFER_ACTIVE@
    */
     MAKE_INTRINSIC(".file_status", file_status, INT_TYPE, 1),
   /* Prototype: Integer file_status (String filename);
    * The @file_status@ function returns information about a file specified
    * by the name @filename@.  It returns an integer describing the file
    * type:
    *  2     file is a directory
    *  1     file exists and is not a directory
    *  0     file does not exist.
    * -1     no access.
    * -2     path invalid
    * -3     unknown error
    */
     MAKE_INTRINSIC(".skip_white", skip_whitespace, VOID_TYPE, 0),
   /* Prototype: Void skip_white ();
    * The @skip_white@ function moves the current point forward until it
    * reaches a non-whitespace character or the end of the current line,
    * whichever happens first.  In this context, whitespace is considered to
    * be any combination of space and tab characters.  To skip newline
    * characters as well, the function @skip_chars@ may be used.
    * Related Functions: @bskip_chars@, @what_char@, @trim@, @right@
    */
     MAKE_INTRINSIC(".bobp", bobp,INT_TYPE, 0),
   /* Prototype: Integer bobp ();
    * The @bolp@ function is used to determine if the current position is at
    * the beginning of the buffer or not.  If so, it returns a non-zero
    * value.  However, if it is not, it returns zero.  This simple example,
    * @ define is_buffer_empty ()
    * @ {
    * @   return bobp () and eobp (); 
    * @ }
    * returns non-zero if the buffer is empty; otherwise, it returns zero.
    * Related Functions: @bob@, @eobp@, @bolp@, @eolp@
    */
     MAKE_INTRINSIC(".flush", flush_message, VOID_TYPE, 1),
   /* Prototype: Void flush (String msg);
    * The @flush@ function behaves like @message@ except that it immediately
    * displays its argument @msg@ as a message in the mini-buffer.  That is,
    * it is not necessary to call @update@ to see the message appear.
    * Related Functions: @message@, @error@
    */
     
     MAKE_INTRINSIC(".input_pending", input_pending, INT_TYPE, 1),
   /* Prototype: Integer input_pending (Integer tsecs);
    * This function is used to see if keyboard input is available to be read
    * or not. The paramter @tsecs@ is the amount of time to wait for input
    * before returning if input is not available.  The time unit for @tsecs@
    * is one-tenth of a second.  That is, to wait up to one second, pass a
    * value of ten to this routine.  It returns zero if no input is
    * available, otherwise it returns non-zero.  As an example, 
    * @ define peek_key ()
    * @ {
    * @   variable ch;
    * @   !if (input_pending (0)) return -1;
    * @   ch = getkey ();
    * @   ungetkey (ch);
    * @   return ch;
    * @ }       
    * returns the value of the next character to be read if one is
    * available; otherwise, it returns -1.
    * Related Functions: @getkey@, @ungetkey@
    */
     
     MAKE_INTRINSIC(".usleep", jed_pause, VOID_TYPE, 0),
   /* Prototype: Void usleep (Integer ms);
    * A call to usleep will cause the editor to pause for @ms@ milliseconds.
    * Related Functions: @input_psnding@
    */
     MAKE_VARIABLE(".IGNORE_BEEP", &tt_Ignore_Beep, INTP_TYPE, 0),
   /* Prototype: Integer IGNORE_BEEP = 3;
    * This variable determines how the terminal is to be ``beeped''.  It may
    * be any one of the following values:
    * @ 0    Do not beep the terminal in any way.
    * @ 1    Produce an audible beep only.
    * @ 2    Produce an visible beep only by flashing the display.
    * @ 3    Produce both audible and visible bells.
    * Note: Not all terminals support visible bells.
    */
     MAKE_VARIABLE(".ADD_NEWLINE", &Require_Final_Newline, INT_TYPE, 0),
   /* Prototype: Integer ADD_NEWLINE = 1;
    * If this variable is non-zero, a newline character will be silently
    * added to the end of a buffer when the buffer is written out to a file
    * if the buffer does not end with a newline character.  If it is zero,
    * one will not be added.
    */
     MAKE_VARIABLE(".LASTKEY", Jed_Key_Buffer, STRING_TYPE, 1),
   /* Prototype: String LASTKEY;
    * The LASTKEY variable contains the most recently entered keyboard
    * sequence.
    * Note: Key sequences using the null character will not be recorded 
    * accurately.
    * Related Variable: @LAST_CHAR@
    */
     MAKE_VARIABLE(".DISPLAY_TIME", &Display_Time, INT_TYPE, 0),
   /* Prototype: Integer DISPLAY_TIME = 1;
    * If this variable is non-zero, the current time will be displayed on the
    * status line if the format for the status line permits it.  If it is zero,
    * the time will not be displayed even if the @%t@ format string is part
    * of the status line format.
    * Related Functions: @set_status_line@
    */
     MAKE_VARIABLE(".WANT_EOB", &Want_Eob, INT_TYPE, 0),
   /* Prototype: Integer WANT_EOB = 0;
    * If this variable is non-zero, an ``end of buffer'' indicator @"[EOB]"@
    * will be displayed at the end of the buffer.  Such an indicator is used 
    * for various editor emulations such as the VAX/VMS EDT editor.
    */
     MAKE_INTRINSIC(".insert_file",  insert_file, INT_TYPE, 1),
   /* Prototype: Integer insert_file (String f);
    * This function may be used to insert the contents of a file named @f@
    * into the buffer at the current position.  The current editing point
    * will be placed at the end of the inserted text.  The function returns
    * @-1@ if the file was unable to be opened; otherwise it returns the
    * number of lines inserted.  This number can be zero if the file is empty.
    * Related Functions: @read_file@, @find_file@, @insert@
    */
     MAKE_INTRINSIC(".keymap_p",  keymap_p, INT_TYPE, 1),
   /* Prototype: Integer keymap_p (String kmap);
    * The @keymap_p@ function may be used to determine whether or not a
    * keymap with name @kmap@ exists.  If the keymap specified by @kmap@
    * exists, the function returns non-zero.  It returns zero if the keymap
    * does not exist.
    * Related Functions: @make_keymap@, @definekey@
    */
     MAKE_VARIABLE(".WRAP", &Jed_Wrap_Column, INT_TYPE, 0),
   /* Prototype: Integer WRAP = 78;
    * The @WRAP@ variable determines the column number where text is
    * wrapped at.  When entering text, if the current point goes beyond
    * this column, the text will automatically wrap to the next line.
    * This will only happen for those buffers for which the wrap flag is set.
    * Related Functions: @getbuf_info@, @setbuf_info@, @set_mode@
    * Related Variables: @WRAP_INDENTS@
    */
     MAKE_INTRINSIC(".what_char", what_char, INT_TYPE, 0),
   /* Prototype: Integer what_char ();
    * The @what_char@ function returns the value of the character at the
    * current position as an integer in the range 0 to 256.  This simple
    * example,
    * @ while (not (eolp ()))
    * @   {
    * @      if (what_char () == '_') 
    * @        {
    * @           del (); insert ("\\_");
    * @        }
    * @   }
    * has the effect of replacing all underscore characters on the current
    * line with a backslash-underscore combination.
    * Related Functions: @looking_at@
    */
     MAKE_INTRINSIC(".recenter", recenter, VOID_TYPE, 1),
   /* Prototype: Void recenter (Integer nth);
    * This function may be used to scroll the window such that the @nth@ line
    * of the window contains the current line.  If @nth@ is zero, the current 
    * line will be placed at the center of the window and the screen will be
    * completely redrawn.
    * Related Functions: @nwindows@, @window_info@
    */
     MAKE_INTRINSIC(".bufferp", bufferp, INT_TYPE, 1),
   /* Prototype: Integer bufferp (String buf);
    * This function is used to see if a buffer exists or not.  If a buffer with
    * name @buf@ exists, it returns a non-zero value.  If it does not exist,
    * it returns zero.
    * Related Functions: @setbuf@, @getbuf_info@
    */
     MAKE_INTRINSIC(".get_key_function", get_key_function, VOID_TYPE, 0),
   /* Prototype: String get_key_function ();
    * @get_key_function@ waits for a key to be pressed and returns a string
    * that represents the binding of the key.  If the key has no binding
    * the empty string is returned.  Otherwise, it also returns an integer
    * that describes whether or not the function is an internal one.  If
    * the function is internal, @1@ will be returned; otherwise zero will
    * be returned to indicate that the binding is either to an S-Lang
    * function or a macro.  If it is a macro, the first character of the 
    * of returned string will be the `@@' character.
    * Related Functions: @getkey@, @input_pending@
    */
     MAKE_INTRINSIC(".dump_bindings", dump_bindings, VOID_TYPE, 1),
   /* Prototype: Void dump_bindings(String map);
    * This functions inserts a formatted list of keybindings for the keymap
    * specified by @map@ into the buffer at the current point.
    * Related Functions: @get_key_function@
    */
     MAKE_VARIABLE(".META_CHAR", &Meta_Char, INT_TYPE, 0),
   /* Prototype: Integer META_CHAR;
    * This variable determines how input characters with the high bit set
    * are to be treated.  If @META_CHAR@ is less than zero, the character
    * is passed through un-processed.  However, if @META_CHAR@ is greater
    * than or equal to zero, an input character with the high bit set is
    * mapped to a two character sequence.  The first character of the
    * sequence is the character whose ascii value is @META_CHAR@ and the
    * second character is the input with its high bit stripped off.
    */
     MAKE_VARIABLE(".DEC_8BIT_HACK", &DEC_8Bit_Hack, INT_TYPE, 0),
   /* Prototype: Integer DEC_8BIT_HACK = 1;
    * If set to a non-zero value, a input character between 128 and 160
    * will be converted into a two character sequence: @ESC@ and the
    * character itself stripped of the high bit + 64.  The motivation
    * behind this variable is to enable the editor to work with VTxxx
    * terminals that are in eight bit mode.
    * Related Variables: @META_CHAR@
    */
     MAKE_INTRINSIC(".undefinekey", unset_key_in_keymap, VOID_TYPE, 2),
   /* Prototype: Void undefinekey (String key, String kmap);
    * This function may be used to remove a keybinding from a specified
    * keymap.  The key sequence is given by the parameter @key@ and the
    * keymap is specified by the second parameter @kmap@.
    * Related Functions: @unsetkey@, @definekey@, @what_keymap@
    */
     MAKE_INTRINSIC(".getpid", jed_getpid, INT_TYPE, 0),
   /* Prototype: Integer getpid ();
    * This function returns the process identification number for the current
    * editor process.
    */
     
     MAKE_INTRINSIC(".update", update_cmd, VOID_TYPE, 1),
   /* Prototype: Void update (Integer f);
    * This function may be called to update the display.  If the parameter
    * @f@ is non-zero, the display will be updated even if there is input
    * pending.  If @f@ is zero, the display may only be partially updated if
    * input is pending.
    * Related Functions: @input_pending@, @flush@
    */
     MAKE_INTRINSIC(".skip_word_chars", skip_word_chars, VOID_TYPE, 0),
   /* Prototype: Void skip_word_chars ();
    * This function moves the current editing point forward across all
    * characters that constitute a word until a non-word character is
    * encountered. Characters that make up a word are set by the
    * @define_word@ function.
    * Related Functions: @define_word@, @skip_non_word_chars@, @skip_chars@, @bskip_word_chars@
    */
     MAKE_INTRINSIC(".skip_non_word_chars", skip_non_word_chars, VOID_TYPE, 0),
   /* Prototype: Void skip_non_word_chars ();
    * This function moves the current editing point forward past all
    * non-word characters until a word character is encountered.
    * Characters that make up a word are set by the @define_word@ function.
    * Related Functions: @define_word@, @skip_word_chars@, @skip_chars@, @bskip_non_word_chars@
    */
     MAKE_INTRINSIC(".bskip_word_chars", bskip_word_chars, VOID_TYPE, 0),
   /* Prototype: Void bskip_word_chars ();
    * This function moves the current editing point backward past all
    * word characters until a non-word character is encountered.
    * Characters that make up a word are set by the @define_word@ function.
    * Related Functions: @define_word@, @skip_word_chars@, @bskip_chars@, @bskip_non_word_chars@
    */
     MAKE_INTRINSIC(".bskip_non_word_chars", bskip_non_word_chars, VOID_TYPE, 0),
   /* Prototype: Void bskip_word_chars ();
    * This function moves the current editing point backward past all
    * non-word characters until a word character is encountered.
    * Characters that make up a word are set by the @define_word@ function.
    * Related Functions: @define_word@, @skip_non_word_chars@, @bskip_chars@, @bskip_word_chars@
    */
     MAKE_INTRINSIC(".which_key", which_key, INT_TYPE, 1),
   /* Prototype: Integer which_key (String f);
    * The @which_key@ function returns the the number of keys that are
    * bound to the function @f@ in the current keymap.  It also returns
    * that number of key sequences with control characters expanded as the
    * two character sequence @^@ and the the whose ascii value is the
    * control character + 64. For example, 
    * @ define insert_key_bindings (f)
    * @ {
    * @    variable n, key;
    * @    n = which_key (f);
    * @    loop (n)
    * @      {
    * @          str = ();
    * @          insert (str);
    * @          insert ("\n");
    * @      }
    * @ }
    * inserts into the buffer all the key sequences that are bound to the
    * function @f@.
    * Related Functions: @get_key_function@, @setkey@, @what_keymap@
    */
     MAKE_INTRINSIC(".whitespace", insert_whitespace,VOID_TYPE, 1),
   /* Prototype: whitespace (Integer n);
    * The @whitespace@ function inserts white space of length @n@ into the
    * current buffer using a combination of spaces and tabs.  The actual
    * combination of spaces and tabs used depends upon the buffer local
    * variable @TAB@.  In particular, if @TAB@ is zero, no tab characters
    * will be used for the expansion.
    * Related Functions: @insert@, @trim@, @goto_column@
    * Related Variables: @TAB@, @TAB_DEFAULT@
    */
     
     MAKE_INTRINSIC(".enlargewin", enlarge_window,VOID_TYPE, 0),
   /* Prototype: Void enlargewin ();
    * This function increases the size of the current window by one line by
    * adjusting the size of the other windows accordingly.
    * Related Functions: @window_info@, @onewindow@
    */
     MAKE_INTRINSIC(".splitwindow", split_window,VOID_TYPE, 0),
   /* Prototype: Void splitwindow ();
    * This function splits the current window vertically creating another
    * window that carries the current window's buffer.
    * Related Functions: @onewindow@, @enlargewin@, @window_info@
    */
     MAKE_INTRINSIC(".file_time_compare", file_time_cmp, INT_TYPE, 2),
   /* Prototype: Integer file_time_cmp (String file1, String file2);
    * This function compares the modification times of two files, @file1@
    * and @file2@. It returns an integer that is either positive, negative,
    * or zero integer for @file1 > file2@, @file1 < file2@, 
    * or @file1 == file2@, respectively.  In this context, the comparison
    * operators are comparing file modification times.  That is, the
    * operator @>@ should be read ``is more recent than''.  The convention
    * adopted by this routine is that if a file does not exist, its
    * modification time is taken to be at the beginning of time.  Thus, if
    * @f@ exists, but @g@ does not, the @file_time_compare (f, g)@ will
    * return a positive number. Related Functions: @file_status@, @time@
    */
     MAKE_INTRINSIC(".xform_region", transform_region, VOID_TYPE, 1),
   /* Prototype: Void xform_region (Integer how);
    * This function changes the characters in the region in a way specified 
    * by the parameter @how@.  This is an integer that can be any of of the
    * following:
    * @ 'u'       Upcase_region
    * @ 'd'       Downcase_region
    * @ 'c'       Capitalize region
    * Anything else will change case of region.
    * Related Functions: @translate_region@, @define_case@
    */
#ifdef pc_system
#if !defined(__GO32__) && !defined(__WATCOMC__)
     MAKE_VARIABLE(".NUMLOCK_IS_GOLD", &NumLock_Is_Gold, INT_TYPE, 0),
#endif
#if !defined(__os2__) && !defined(__WATCOMC__) && !defined(__GO32__)
#ifndef MSWINDOWS
     MAKE_VARIABLE(".CHEAP_VIDEO", &SLtt_Msdos_Cheap_Video, INTP_TYPE, 0),
   /* non zero if snow appears on screen when updating it. */
#endif
#endif
#else
     MAKE_VARIABLE(".OUTPUT_RATE", &tt_Baud_Rate, INTP_TYPE, 0),
   /* Terminal baud rate */
#endif
     
     MAKE_INTRINSIC(".skip_chars", skip_chars, VOID_TYPE, 1),
   /* Prototype: Void skip_chars(String s);
    * This fnction may be used to move the editing point forward past all
    * characters in string @s@ which contains the chars to skip, or a range
    * of characters.  A character range is denoted by two charcters
    * separated by a hyphen.  If the first character of the string @s@ is a
    * @'^'@ character, then the list of characters actually denotes the
    * complement of the set of characters to be skipped.  To explicitly
    * include the hyphen character in the list, it must be either the first
    * or the second character of the string, depending upon whether or not
    * the @'^'@ character is present. So for example, 
    * @ skip_chars ("- \t0-9ai-o_");
    * will skip the hyphen, space, tab, numerals @0@ to @9@, the letter @a@,
    * the letters @i@ to @o@, and underscore.  An example which illustrates 
    * the complement of a range is
    * @ skip_chars("^A-Za-z");
    * which skips all characters except the letters. 
    * Note: The backslash character may be used to escape only the first
    * character in the string.  That is, @"\\^"@ is to be used to skip over
    * @^@ characters. 
    * Related Functions: @bskip_chars@, @skip_white@ 
    */
     
     MAKE_INTRINSIC(".command_line_arg", command_line_argv, STRING_TYPE, 1),
   /* Prototype: String command_line_argv (Integer n);
    * This function may be used to determine the command line parameters
    * that invoked the editor.  The parameter @n@ is an integer the range zero
    * up-to but not including the value of the variable @MAIN_ARGC@.  
    * Related Variables: @MAIN_ARGC@
    */
     
     MAKE_VARIABLE(".MAIN_ARGC", &Main_Argc, INT_TYPE, 1),
   /* Prototype: Integer MAIN_ARGC;
    * @MAIN_ARGC@ is a global read-only variable that indicates the number
    * of command line parameters.
    * Related Functions: @command_line_argv@
    */
     MAKE_INTRINSIC(".set_file_translation", set_file_trans, VOID_TYPE, 1),
   /* Prototype: set_file_translation (Integer n);
    * This function affects only the way the next file is opened.  Its
    * affect does not last beyond that.  If it the value of the parameter
    * is 1, the next file will be opened in binary mode.  If the parameter is
    * zero, the file will be opened in text mode.
    */
     
#if defined (__unix__) || defined (__os2__)
#ifndef __GO32__
     MAKE_INTRINSIC(".pipe_region", pipe_region, INT_TYPE, 1),
     /* Prototype: Integer pipe_region (String cmd);
      * The @pipe_region@ function executes @cmd@ in a separate process and
      * sends the region of characters defined by the mark and the current
      * point to the standard input of the process.  It successful, it returns 
      * the exit status of the process.  Upon failure it signals an error.
      * Note: This function is only available for Unix and OS/2 systems.
      * Related Functions: @run_shell_cmd@, @push_mark@
      */
     MAKE_INTRINSIC(".run_shell_cmd", shell_command, INT_TYPE, 1),
   /* Prototype: Void run_shell_cmd (String cmd);
    * The @run_shell_cmd@ function may be used to run @cmd@ in a separate
    * process.  Any output generated by the process is inserted into the
    * buffer at the current point.  It generates a S-Lang error if the
    * process specified by @cmd@ could not be opened.  Otherwise, it
    * returns the exit status of the process.
    */
#endif
#endif
     MAKE_INTRINSIC(".mkdir", make_directory, INT_TYPE, 1),
   /* Prototype: Integer mkdir (String dir);
    * The @mkdir@ function may be used to create a new directory with name
    * specified by @dir@.  It returns zero if the directory could not be
    * created or non-zero if a directory with the specified name was created.
    * Related Functions: @rmdir@, @file_status@
    */
     MAKE_INTRINSIC(".rmdir", delete_directory, INT_TYPE, 1),
   /* Prototype: Integer rmdir (dir);
    * @rmdir@ may be used to delete the directory specified by the name
    * @dir@.  If the directory was sucessfully deleted, the function
    * returns a non-zero value.  Otherwise, the directory could not be
    * deleted and zero will be returned.
    * Related Functions: @mkdir@, @delete_file@
    */
     MAKE_INTRINSIC(".append_region_to_file", append_to_file, INT_TYPE, 1),
   /* Prototype: Integer append_region_to_file (String file);
    * Appends a marked region to @file@ returning number of lines written or -1
    * on error.  This does NOT modify a buffer visiting the file; however,
    * it does flag the buffer as being changed on disk. */
     MAKE_INTRINSIC(".autosave", auto_save, VOID_TYPE, 0),
   /* Prototype: Void autosave ();
    * The @autosave@ function saves the current buffer in an auto save file
    * if the buffer has been marked for the auto save operation. 
    * Related Functions: @setbuf_info@, @autosaveall@
    * Related Variables: @MAX_HITS@
    */
     MAKE_INTRINSIC(".autosaveall", auto_save_all, VOID_TYPE, 0),
   /* Prototype: Void autosaveall ();
    * This function is like @autosave@ except that it causes all files
    * marked for the auto save operation to be auto-saved.
    * Related Functions: @autosave@, @setbuf_info@
    * Related Variables: @MAX_HITS@
    */
     
     MAKE_INTRINSIC(".backward_paragraph", backward_paragraph, VOID_TYPE, 0),
   /* Prototype: Void backward_paragraph ();
    * This function moves the current editing point backward past the
    * current paragraph to the line that is a paragraph separator.  Such a
    * line is determined by the S-Lang hook @is_paragraph_separator@.  This
    * hook can be modified on a buffer by buffer basis by using the
    * function @set_buffer_hook@. 
    * Related Functions: @forward_paragraph@, @set_buffer_hook@
    */
     MAKE_INTRINSIC(".blank_rect", blank_rectangle, VOID_TYPE, 0),
   /* The @blank_rect@ function replaces all text in the rectangle defined by
    * the current editing point and the mark by spaces.
    * Related Functions: @push_mark@, @kill_rect@, @insert_rect@, @copy_rect@
    */
     MAKE_INTRINSIC(".bskip_chars", bskip_chars, VOID_TYPE, 1),
   /* Prototype: Void bskip_chars (String str);
    * This function may be used to skip past all characters defined by the 
    * string @str@.  See @skip_chars@ for the definition of @str@.
    * The following example illustrates how to skip past all whitespace 
    * including newline characters:
    * @ bskip_chars (" \t\n");
    * Related Functions: @skip_chars@, @left@
    */
     MAKE_INTRINSIC(".buffer_list", make_buffer_list,  VOID_TYPE, 0),
   /* Prototype: Integer buffer_list ();
    * This function returns an integer indicating the number of buffers and
    * leaves the names of the buffers on the stack.  For example, the 
    * following function displays the names of all buffers attached to 
    * files:
    * @ define show_buffers ()
    * @ {
    * @    variable b, str = "", file;
    * @    loop (buffer_list ())
    * @      {
    * @          b = ();
    * @          setbuf (b);
    * @          (file,,,) = getbuf_info ();
    * @          if (strlen (file)) str = strcat (str, strcat (" ", b));
    * @      }
    * @    message (str);
    * @ }
    * Related Functions: @getbuf_info@, @whatbuf@
    */
     MAKE_INTRINSIC(".check_region", check_region, VOID_TYPE, 1),
   /* Prototype: Void check_region (Integer ps);
    * This function checks to see if a region is defined and may exchange
    * the current editing point and the mark to define a canonical region.
    * If the mark is not set, it signals an S-Lang error.  A canonical
    * region is one with the mark set earlier in the buffer than than the
    * editing point.  Always call this if using a region which requires
    * such a situation. 
    * 
    * If the argument @ps@ is non-zero, @push_spot@ will be called,
    * otherwise, @ps@ is zero and it will not be called.
    * 
    * As an example, the following function counts the number of lines in 
    * a region:
    * @ define count_lines_region ()
    * @ {
    * @    variable n;
    * @    check_region (1);   % spot pushed
    * @    narrow ();
    * @    n = what_line ();
    * @    widen ();
    * @    pop_spot ();
    * @    return n;
    * @  }
    * Related Functions: @markp@, @push_mark@
    */
     MAKE_INTRINSIC(".copy_rect", copy_rectangle, VOID_TYPE, 0),
   /* Prototype: Void copy_rect ();
    * The @copy_rect@ function is used to copy the contents of the
    * currently defined rectangle to the rectangle buffer.  It overwrites
    * the previous contents of the rectangle buffer.  A rectangle is defined 
    * by the diagonal formed by the mark and the current point.
    * Related Functions: @insert_rect@, @kill_rect@, @blank_rect@
    */
     MAKE_INTRINSIC(".define_word", define_word, VOID_TYPE, 1),
   /* Prototype: Void define_word (String s);
    * This function is used to define the set of characters that form a
    * word. The string @s@ consists of those characters or ranges of
    * characters that define the word.  For example, to define only the
    * characters @A-Z@ and @a-z@ as word characters, use:
    * @ define_word ("A-Za-z");
    * To include a hyphen as part of a word, it must be the first character
    * of the control string @s@.  So for example,
    * @ define_word ("-i-n");
    * defines a word to consist only of the letters @i@ to @n@ and the
    * hyphen character.
    */
     
     MAKE_INTRINSIC(".delbuf", kill_buffer_cmd, VOID_TYPE, 1),
   /* Prototype: Void delbuf (String buf);
    * @delbuf@ may be used to delete a buffer with the name specified by
    * @buf@.  If the buffer does not exist, a S-Lang error will be generated.
    * Related Functions: @whatbuf@, @bufferp@, @sw2buf@
    */
     MAKE_INTRINSIC(".delete_file",  sys_delete_file, INT_TYPE, 1),
   /* Prototype: Integer delete_file (String file);
    * This function may be used to delete a file specified by the @file@
    * parameter.  It returns non-zero if the file was sucessfully deleted
    * or zero otherwise.
    * Related Functions: @rmdir@
    */
     MAKE_INTRINSIC(".directory", expand_wildcards, INT_TYPE, 1),
   /* returns number of files and list of files which match filename. 
      On unix, this defaults to filename*.  It is primarily useful for
      DOS and VMS to expand wilcard filenames */
     MAKE_INTRINSIC(".evalbuffer", load_buffer, VOID_TYPE ,0),
   /* Prototype: Void evalbuffer ();
    * This function causes the current buffer to be sent to the S-Lang
    * interpreter for evaluation.  If an error is encountered while parsing
    * the buffer, the cursor will be placed at the location of the error.
    * Related Functions: @evalfile@
    */
     MAKE_INTRINSIC(".expand_filename", expand_filename, STRING_TYPE, 1),
   /* Prototype: String expand_filename (String file);
    * The @expand_filename@ function expands a file to a canonical form.
    * For example, under Unix, if @file@ has the value @"/a/b/../c/d"@, it
    * returns @"/a/c/d"@.  Similarly, if @file@ has the value
    * @"/a/b/c//d/e"@, @"/d/e"@ is returned.
    */
     MAKE_INTRINSIC(".filechgondsk", file_changed_on_disk, INT_TYPE, 1),
   /* Prototype: Integer filechgondsk (String fn);
    * This function may be used to determine if the disk file specified by the 
    * parameter @fn@ is more recent than the current buffer.  
    * Related Functions: @file_time_compare@
    */
     MAKE_INTRINSIC(".forward_paragraph", forward_paragraph, VOID_TYPE, 0),
   /* Prototype: Void forward_paragraph ();
    * This function moves the current editing point forward past the end of
    * the current paragraph.  Paragraph delimiters are defined through either
    * a buffer hook or via the hook @is_paragraph_separator@.
    * Related Functions: @backward_paragraph@, @set_buffer_hook@
    */
     MAKE_INTRINSIC(".getkey", jed_getkey, INT_TYPE, 0),
   /* Prototype: Integer getkey ();
    * The @getkey@ function may be used to read an input character from the 
    * keyboard.  It returns an integer in the range 0 to 256 which represents 
    * the ASCII or extended ASCII value of the character.
    * Related Functions: @input_pending@, @ungetkey@
    */
     MAKE_INTRINSIC(".indent_line", indent_line, VOID_TYPE, 0),
   /* Prototype: Void indent_line ();
    * The @indent_line@ line function indents the current line in a manner
    * which depends upon the current buffer.  The actual function that gets 
    * called is set via a prior call the @set_buffer_hook@ to set the indent
    * hook.  The default value is to indent the line to the indentation
    * level of the previous line.
    * Related Functions: @set_buffer_hook@
    */
     MAKE_INTRINSIC(".insert_rect", insert_rectangle, VOID_TYPE, 0),
   /* Prototype: insert_rect ();
    * The @insert_rect@ function inserts the contents of the rectangle buffer
    * at the current editing point.  The rectangle buffer is not modified.
    * Any text that the rectangle would overwrite is moved to the right by an 
    * amount that is equal to the width of the rectangle.
    * Related Functions: @kill_rect@, @blank_rect@, @copy_rect@
    */
     MAKE_INTRINSIC(".kill_rect", kill_rectangle, VOID_TYPE, 0),
   /* Prototype: Void kill_rect ();
    * This function deletes the rectangle defined by the mark and the current 
    * point.  The contents of the rectangle are saved in the rectangle buffer
    * for later retrieval via the @insert_rect@ function.  The previous 
    * contents of the rectangle buffer will be lost.
    * Related Functions: @insert_rect@, @blank_rect@, @copy_rect@
    */
     MAKE_INTRINSIC(".make_keymap", create_keymap, VOID_TYPE, 1),
   /* Prototype: Void make_keymap (String km);
    * The @make_keymap@ function creates a keymap with a name specified by
    * the @km@ parameter.  The new keymap is an exact copy of the
    * pre-defined @"global"@ keymap.
    * Related Functions: @use_keymap@, @keymap_p@, @definekey@, @setkey@
    */
     MAKE_INTRINSIC(".map_input", map_character, VOID_TYPE, 2),
   /* Prototype: Void map_input (Integer x, Integer y);
    * The @map_input@ function may be used to remap an input character with
    * ascii value @x@ from the keyboard to a different character with ascii
    * value @y@.  This mapping can be quite useful because it takes place
    * before the editor interprets the character. One simply use of this
    * function is to swap the backspace and delete characters.  Since the
    * backspace character has an ascii value of @8@ and the delete character 
    * has ascii value @127@, the statement 
    * @ map_input (8, 127);
    * maps the backspace character to a delete character and 
    * @ map_input (127, 8);
    * maps the delete character to a backspace character.  Used together,
    * these two statement effectively swap the delete and backspace keys.
    * Related Functions: @getkey@
    */
     MAKE_INTRINSIC(".narrow_to_region", narrow_to_region, VOID_TYPE, 0),
   /* Prototype: Void narrow_to_region (void);
    * The @narrow_to_region@ function behaves like the @narrow@ function
    * that @narrow@ operates on lines and @narrow_to_region@ restricts 
    * editing to only characters within the region.
    * Related Functions: @widen_region@, @narrow@.
    */
     MAKE_INTRINSIC(".narrow", narrow_to_lines, VOID_TYPE, 0),
   /* Prototype: Void narrow ();
    * This function may be used to restict editing to the region of lines
    * between the mark and the editing point.  The region includes the line
    * containing the mark as well as the line at the current point. All
    * other lines outside this region are completely inacessable without
    * first lifting the restriction using the @widen@ function. As a simple
    * example, suppose that there is a function called @print_buffer@ that
    * operates on the entire buffer.  Then the following function will work
    * on a region of lines:
    * @ define print_region ()
    * @ {
    * @    narrow ();
    * @    print_buffer ();
    * @    widen ();
    * @ }
    * The @narrow@ function will signal an error if the mark is not set.
    * Note also that the narrow function may be used recursively in the
    * sense that a narrowed region may be further restricted using the
    * @narrow@ function.  For each narrow, the @widen@ function must be called 
    * to lift each restriction.
    * Related Functions: @widen@, @narrow_to_region@
    */
     MAKE_INTRINSIC(".open_rect", open_rectangle, VOID_TYPE, 0),
   /* Prototype: Void open_rect ();
    * The @open_rect@ function may be used to insert a blank rectangle whose
    * size is determined by the mark and the current editing point.  Any text
    * that lies in the region of the rectangle will be pushed to the right.
    * Related Functions: @insert_rect@, @kill_rect@, @copy_rect@
    */
     MAKE_INTRINSIC(".quit_jed", quit_jed, VOID_TYPE, 0),
   /* Prototype: Void quit_jed ();
    * This function quits the editor immediately.  No buffers are
    * auto-saved and no hooks are called.  The function @exit_jed@ should be
    * called when it is desired to exit in a safe way.
    * Related Functions: @exit_jed@
    */
     MAKE_INTRINSIC(".read_file", find_file_cmd, INT_TYPE, 1),
   /* Prototype: Integer read_file (string fn);
    * The @read_file@ function may be used to read a file specified by @fn@
    * into its own buffer.  It returns a non-zero value upon success and
    * signals an error upon failure.  The hook @find_file_hook@ is called
    * after the file is read in.  Unlike the related function, @find_file@, 
    * this function does not create a window for the newly created buffer.
    * Related Functions: @find_file@, @file_status@, @write_buffer@
    */
     MAKE_INTRINSIC(".read_with_completion", read_object_with_completion, VOID_TYPE, 4),
   /* Prototype: Void read_with_completion (String prt, String dflt, String s, Integer type);
    * This function may be used to read one of the objects specified by the
    * last parameter @type@.  The first parameter, @prt@, is used as a
    * prompt, the second parameter, @dflt@, is used to specify a default,
    * and the third parameter, @s@, is used to initialize the string to 
    * be read.
    * @type@ is an integer with the following meanings:
    * @ 'f'   file name
    * @ 'b'   buffer name
    * @ 'F'   function name
    * @ 'V'   variable name.
    *
    * Finally, if @type@ has the value @'s'@, then the set of completions
    * will be defined by a zeroth parameter, @list@, to the function call.
    * This parameter is simple a comma separated list of completions.
    * For example, 
    * @ read_with_completion ("Larry,Curly,Moe", "Favorite Stooge:",
    * @                       "Larry", "", 's');
    * provides completion over the set of three stooges.
    * The function returns the string read.
    * Related Functions: @read_mini@
    */
     MAKE_INTRINSIC(".set_abort_char", set_abort_char, VOID_TYPE, 1),
   /* Prototype: Void set_abort_char (Integer ch);
    * This function may be used to change the keyboard character that
    * generates an S-Lang interrupt.  The parameter @ch@ is the ASCII value
    * of the character that will become the new abort character. The
    * default abort character @Ctrl-G@ corresponds to @ch=7@.
    */
     MAKE_INTRINSIC(".suspend", sys_spawn_cmd, VOID_TYPE, 0),
   /* Prototype: Void suspend ();
    * The action of this command varies with the operating system.  
    * Under Unix, the editor will be suspended and control will pass to the
    * parent process.  Under VMS and MSDOS, a new subprocess will be spawned.
    * Before suspension, @suspend_hook@ is called.  When the editor is 
    * resumed, @resume_hook@ will be called.  These hooks are user-defined 
    * functions that take no arguments and return no values.
    */
     MAKE_INTRINSIC(".ungetkey", ungetkey, VOID_TYPE, 1),
   /* Prototype: Void ungetkey (Integer ch);
    * This function may be used to push a character @ch@ represented by its
    * ASCII value, onto the input stream.  This means that the next keyboard
    * to be read will be @ch@.
    * Related Functions: @buffer_keystring@, @getkey@, @get_key_function@
    */
     MAKE_INTRINSIC(".buffer_keystring", do_buffer_keystring, VOID_TYPE, 1),
   /* Prototype: Void buffer_keystring (String str);
    * Append string @str@ to the end of the input stream to be read by JED's
    * getkey routines.
    * Related Functions: @ungetkey@, @getkey@
    */
     MAKE_INTRINSIC(".use_keymap",  use_keymap, VOID_TYPE, 1),
   /* Prototype: Void use_keymap (String km);
    * This function may be used to dictate which keymap will be used by the 
    * current buffer.  @km@ is a string value that corresponds to the name
    * of a keymap.
    * Related Functions: @make_keymap@, @keymap_p@, @what_keymap@
    */
#ifndef pc_system
     MAKE_INTRINSIC(".w132", screen_w132, VOID_TYPE, 0),
   /* Prototype: Void w132 ();
    * This function may be used to set the number of columns on a vtxxx
    * compatable terminal to 132.
    * Related Functions: @w80@, @set_term_vtxxx@
    */     
     MAKE_INTRINSIC(".w80", screen_w80, VOID_TYPE, 0),
   /* Prototype: Void w80 ();
    * This function may be used to set the number of columns on a vtxxx
    * compatable terminal to 80.
    * Related Functions: @w132@, @set_term_vtxxx@
    */
#endif
     MAKE_INTRINSIC(".what_mode", what_mode, INT_TYPE, 0),
   /* Prototype: (String name, Integer flags) = Integer what_mode ();
    * This function may be used to obtain the mode flags and mode name of the 
    * current buffer.  See @set_mode@ for more details.
    * Related Functions: @set_mode@, @getbuf_info@, @setbuf_info@
    */
     MAKE_INTRINSIC(".widen", widen, VOID_TYPE, 0),
   /* Prototype: Void widen ();
    * This function undoes the effect of @narrow@.  Consult the documentation
    * for @narrow@ for more information.
    * Related Functions: @widen_region@, @narrow@
    */
     MAKE_INTRINSIC(".widen_region", widen_region, VOID_TYPE, 0),
   /* Prototype: Void widen_region ();
    * This function undoes the effect of @narrow_to_region@.  Consult the 
    * documentation for @narrow_to_region@ for more information.
    * Related Functions: @widen@, @narrow_to_region@
    */
     MAKE_INTRINSIC(".window_line", window_line, INT_TYPE, 0),
   /* Prototype: Integer window_line ();
    * This function returns the number of rows from the top of the current
    * window for the current line.  If the current line is the very first line
    * in the window, a value of @1@ will be returned, i.e., it is the first
    * line of the window.
    * Related Functions: @window_info@, @nwindows@
    * Related Variables: @TOP_WINDOW_ROW@
    */
     MAKE_INTRINSIC(".write_buffer", write_buffer_cmd, INT_TYPE, 1),
   /* Prototype: Integer write_buffer (String filename);
    * This function may be used to write the current buffer out to a file 
    * specified by @filename@.  The buffer will then become associated with 
    * that file.  The number of lines written to the file is returned.  An
    * error condition will be signaled upon error.
    * Related Functions: @write_region_to_file@, @setbuf_info@
    */
     MAKE_INTRINSIC(".write_region_to_file", write_region_cmd, INT_TYPE, 1),
   /* Prototype: Integer write_region_to_file (String filename);
    * This function may be used to write a region of the current buffer to
    * the file specified by @filename@.  It returns the number of lines
    * written to the file or signals an error upon failure.
    * Related Functions: @write_buffer@, @append_region_to_file@, @push_mark@
    */
     MAKE_INTRINSIC(".count_chars", count_chars, VOID_TYPE, 0),
   /* Prototype: String count_chars ();
    * This function returns information about the size of the current buffer
    * and current position in the buffer.  The string returned is of the form:
    * @ 'h'=104/0x68/0150, point 90876 of 127057
    * Related Functions: @what_char@
    */
     MAKE_INTRINSIC(".get_yes_no", get_yes_no, INT_TYPE, 1),
   /* Prototype: Integer get_yes_no (String s);
    * This function may be used to get a yes or no response from the
    * user.  The string parameter @s@ will be used to construct the prompt
    * by concating the string @"? (yes/no)"@ to @s@.
    * It returns @1@ if the answer is yes or @0@ if the answer is no.
    * Related Functions: @getkey@, @flush@, @message@
    */
     MAKE_INTRINSIC(".rename_file", rename_file, INT_TYPE, 2),
   /* Prototype: Integer rename_file (String old_name, String new_name);
    * This function may be used to change the name of a disk file from 
    * @old_name@ to @new_name@.  Upon success, zero is returned.  Any other
    * value indicates failure.
    * Note: Both filenames must refer to the same file system.
    * Related Functions: @file_status@, @stat_file@
    */
     MAKE_INTRINSIC(".change_default_dir", ch_dir, INT_TYPE, 1),
   /* Prototype: Integer change_default_dir (String new_dir);
    * This function may be used to change the current working directory
    * of the editor to @new_dir@.  It returns zero upon success or @-1@ upon
    * failure. 
    * Note: Each buffer has its own working directory.  This function does not
    * change the working directory of the buffer.  Rather, it changes the 
    * working directory of the whole editor.  This has an effect on functions
    * such as @rename_file@ when such functions are passed relative filenames.
    * Related Functions: @setbuf_info@, @getbuf_info@, @rename_file@
    */
     MAKE_INTRINSIC(".prefix_argument",  do_prefix_argument, INT_TYPE, 1),
   /* Prototype: Integer prefix_argument (Integer dflt);
    * This function may be used to determine whether or not the user has entered
    * a prefix argument from the keyboard.  If a prefix argument is present, 
    * its value is returned; otherwise, @dflt@ is returned.  Calling this 
    * function cancels the prefix argument.
    * For example,
    * @ variable arg = prefix_argument (-9999);
    * @ if (arg == -9999)
    * @   message ("No Prefix Argument");
    * @ else
    * @   message (Sprintf ("Prefix argument: %d", arg, 1));
    * displays the prefix argument in the message area.
    * Note: This function is incapable of distinguishing between the case of
    * no prefix argument and when the argument's value is @dflt@.  Currently,
    * this is not a problem because the editor does not allow negative prefix 
    * arguments.
    */
     MAKE_INTRINSIC(".set_buffer_hook", set_buffer_hook, VOID_TYPE, 2),
   /* Prototype: Void set_buffer_hook (String hook, String f);
    * Set current buffer hook @hook@ to function @f@. @f@ is a user
    * defined S-Lang function.  Currently, name can be any one of:
    * @  "par_sep"  -- returns zero if the current line does not
    * @       constitute the beginning or end of a paragraph.  
    * @       It returns non-zero otherwise.  The default value of hook is
    * @       is_paragraph_separator.
    * @  "indent_hook" -- returns nothing.  It is called by the indent line
    * @       routines.
    * @  "wrap_hook"   hook that is called after a line is wrapped.  Returns
    * @       nothing.
    * @  "newline_indent_hook"  --- returns nothing.  If this hook is defined,
    * @       it will be called instead of the internal function 
    * @       newline_and_indent is called.
    * @  "bob_eob_error_hook"  --- returns nothing.  If this hook is defined,
    * @       it will be called whenever an error one of the internal cursor
    * @       movement functions would have generated an end of buffer or beginning of
    * @       buffer error.  It is passed an integer that indicates which function
    * @       would have generated the error.  Specifically:
    * @ 
    * @                -1  previous_line_cmd
    * @                -2  previous_char_cmd
    * @                -3  page_up
    * @                 1  next_line_cmd
    * @                 2  next_char_cmd
    * @                 3  page_down
    * @  "mouse_down", "mouse_up", "mouse_drag" "mouse_2click" "mouse_3click"
    * @       These hooks are used to override default hooks defined by the
    * @       mouse_set_default_hook function.
    * 
    * Related Functions: @mouse_set_default_hook@
    */
     MAKE_INTRINSIC(".insert_file_region", insert_file_region, INT_TYPE, 3),
   /* Prototype: Integer insert_file_region (String file, String beg, String end);
    * This function may be used to insert a region specified by the strings
    * @beg@ and @end@ of the file with name @file@ into the current buffer.
    * The file is scanned line by line until a line that begins with the
    * string given by @beg@ is encountered.  Then, that line and all
    * successive lines up to the one that starts with the string specified
    * by @end@ is inserted into the buffer.  The line beginning with the
    * value of @end@ is not inserted although the one beginning with @beg@ is.
    * The function returns the number of lines inserted or @-1@ upon failure
    * to open the file.
    * 
    * Note that a zero length @beg@ corresponds to the first line
    * and that a zero length @end@ corresponds to the last line.
    * 
    * Related Functions: @insert_file@
    */
     MAKE_INTRINSIC(".search_file", search_file, INT_TYPE, 3),
   /* Prototype: Integer search_file (String filename, String re, Integer nmax);
    * This function may be used to search for strings in a disk file
    * matching the regular expression @re@.  The first argument @filename@
    * specifies which file to search.  The last argument @nmax@ specifies
    * how many matches to return.  Each line that is matched is pushed onto
    * the S-Lang stack.  The number of matches (limited by @nmax@) is returned.
    * If the file contains no matches, zero is returned.
    */
     MAKE_INTRINSIC(".random", make_random_number, INT_TYPE, 2),
   /* Prototype: Integer random (Integer seed, Integer nmax);
    * The @random@ function returns a random number in the range 0 to, but
    * not including, @nmax@.  If the first parameter @seed@ is 0, the
    * number generated depends on a previous seed.  If @seed@ is -1, the
    * current time and process id will be used to seed the random number
    * generator; otherwise @seed@ will be used.
    */
#ifndef pc_system
     MAKE_INTRINSIC(".set_term_vtxxx", do_tt_set_term_vtxxx, VOID_TYPE, 1),
   /* Set terminal display appropriate for a vtxxx terminal.  This function 
    * takes a single integer parameter.  If non-zero, the terminal type is set 
    * for a vt100.  This means the terminal lacks the ability to insert/delete
    * lines and characters.  If the parameter is zero, the terminal is assumed
    * to be vt102 compatable.  Unless you are using a VERY old terminal or 
    * a primitive emulator, use zero as the parameter. */
     MAKE_VARIABLE(".TERM_CANNOT_INSERT", &tt_Term_Cannot_Insert, INTP_TYPE, 0),
   /* Set this variable to 1 in your jed startup file (jed.rc) if your 
    terminal is unable to insert (not vt102 compatable) */
     MAKE_VARIABLE(".TERM_CANNOT_SCROLL", &tt_Term_Cannot_Scroll, INTP_TYPE, 0),
   /* Set this variable to 1 in your jed startup file (jed.rc) if your 
    terminal is unable to scroll. */
     MAKE_VARIABLE(".TERM_BLINK_MODE", &tt_Blink_Mode, INTP_TYPE, 0),
   /* Prototype: Integer TERM_BLINK_MODE = 1;
    * If this variable is non-zero, jed will interpret high-intensity 
    * background colors as blinking characters.  On some terminals, the blink
    * bit will be mapped to an actual high intensity background color.
    * Set this variable to zero if you want to disable blinking characters.
    * Note: This variable is used only on VMS and DOS systems.
    */
#endif
     
     MAKE_VARIABLE(".BATCH", &Batch, INT_TYPE, 1),
   /* non-zero if JED is running in batch mode.  This variable
       is read only. */
     MAKE_VARIABLE(".TAB", &Buffer_Local.tab, INT_TYPE, 0),
   /* Prototype: Integer TAB = 8;
    * This variable controls the tab setting of the current buffer.  All
    * newly created buffers will have the variable set to the value of @TAB_DEFAULT@.
    * Related Variables: @TAB_DEFAULT@, @USE_TABS@
    */
     MAKE_VARIABLE(".SELECTIVE_DISPLAY", &Buffer_Local.sd, INT_TYPE, 0),
   /* If negative, ^M (RET) makes rest of line invisible.  Hidden 
      text is indicated by @...@.  */
     MAKE_VARIABLE(".LAST_CHAR", &SLang_Last_Key_Char, INT_TYPE, 0),
   /* Last character entered from the keyboard */
     MAKE_VARIABLE(".MAX_HITS", &Jed_Max_Hits, INT_TYPE, 0),
   /* maximum number of @hits@ on a buffer before an autosave is performed. */
     MAKE_VARIABLE(".POINT", &Point, INT_TYPE, 0),
     MAKE_VARIABLE(".MESSAGE_BUFFER", Message_Buffer, STRING_TYPE, 1),
   /* Read only string indicating current message to be displayed or
      is displayed in the message buffer */
     MAKE_VARIABLE(".IGNORE_USER_ABORT", &Ignore_User_Abort, INT_TYPE, 0),
   /* If set to a non-zero value, the Abort Character will not trigger a
      S-Lang error.  When JED starts up, this value is set to 1 so that 
      the user cannot abort the loading of site.sl.  Later, it is set to 0 */
     
     
     MAKE_VARIABLE(".KILL_LINE_FEATURE", &Kill_Line_Feature, INT_TYPE, 0),
   /* If non-zero, kill_line will kill through end of line character if
    Point is at beginning of the line.  Otherwise, it will kill only until
    the end of the line.  By default, this feature is turned on. */
     MAKE_VARIABLE(".SCREEN_HEIGHT", &tt_Screen_Rows, INTP_TYPE, 1),
   /* number of rows on the screen. */
     MAKE_VARIABLE(".SCREEN_WIDTH", &tt_Screen_Cols, INTP_TYPE, 1),
   /* number of columns on the screen */
     MAKE_VARIABLE(".JED_LIBRARY", Jed_Library, STRING_TYPE, 1),
   /* Read only string variable indicating the directory where JED library
    files are kept.  This variable may be set using an environment variable */
     MAKE_VARIABLE(".JED_ROOT", Jed_Root_Dir, STRING_TYPE, 1),
   /* Read only string variable indicating JED's root directory.
      This variable may be set using an environment variable */
     MAKE_VARIABLE(".LINENUMBERS", &User_Prefers_Line_Numbers, INT_TYPE, 0),
   /* If set to 0, line numbers are not displayed on the screen. If set to 
      1, line numbers will be displayed.  If set to anything else, the 
      %c column format specifier will be parsed allowing the column number
      to be displayed on the screen. */
#ifdef pc_system
     MAKE_VARIABLE(".ALT_CHAR", &PC_Alt_Char, INT_TYPE, 0),
   /*  If this variable is non-zero, characters pressed in combination
     the ALT key will generate a two character sequence: the first character 
     is the value of the ALT_CHAR itself followed by the character pressed. 
     For example, if ALT-X is pressed and ALT_CHAR has a value of 27, the 
     characters ESCAPE X will be generated. */
#endif
     
#ifdef HAS_MOUSE
     MAKE_INTRINSIC(".mouse_get_event_info", jed_mouse_get_event_info, VOID_TYPE, 0),
     /* Prototype: (x, y, state) = mouse_get_event_info ();
      * This function returns the position of the last processed
      * mouse event, and the state of the mouse buttons and shift
      * keys before the event.  
      *
      * @x@ and @y@ represent the column and row, respectively, where
      * the event took place. They are measured with relative to the
      * top left corner of the editor's display.  
      *
      * @state@ is a bitmapped integer whose bits are defined as follows:
      * @  1  Left button pressed
      * @  2  Middle button pressed
      * @  4  Right button pressed
      * @  8  Shift key pressed
      * @ 16  Ctrl key pressed
      *
      * Other information such as the button that triggered the event is 
      * available when the mouse handler is called.  As a result, this information
      * is not returned by @mouse_get_event_info@.
      * 
      * Related Functions: @mouse_set_default_hook@, @set_buffer_hook@.
      */
     
     MAKE_INTRINSIC(".mouse_set_current_window", jed_set_current_mouse_window, VOID_TYPE, 0),
     /* Prototype: Void mouse_set_current_window ();
      * Use of this function results in changing windows to the window that
      * was current at the time of the mouse event.
      * 
      * Related Functions: @mouse_set_default_hook@
      */
     MAKE_INTRINSIC(".mouse_set_default_hook", jed_set_default_mouse_hook, VOID_TYPE, 2),
     /* Prototype: Void set_default_mouse_hook (String name, String fun);
      * This function associates a slang function @fun@ with the mouse event
      * specified by @name@.  The first parameter @name@ must be one of the
      * following:
      * @     "mouse_up"          "mouse_status_up"    
      * @     "mouse_down"        "mouse_status_down"  
      * @     "mouse_drag"        "mouse_status_drag"  
      * @     "mouse_2click"      "mouse_status_2click"
      * @     "mouse_3click"      "mouse_status_3click"
      * The meaning of these names should be obvious.  The second parameter,
      * @fun@ must be defined as
      *
      * @    define fun (line, column, btn, shift)
      *
      * and it must return an integer.  The parameters @line@ and
      * @column@ correspond to the line and column numbers in the
      * buffer where the event took place. @btn@ is an integer that
      * corresonds to the button triggering the event.  It can take
      * on values @1@, @2@, and @4@ corresponding to the left,
      * middle, and right buttons, respectively.  @shift@ can take on
      * values @0@, @1@, or @2@ where @0@ indicates that no modifier
      * key was pressed, @1@ indicates that the SHIFT key was
      * pressed, and @2@ indicates that the CTRL key was pressed.
      * For more detailed information about the modifier keys, use
      * the function @mouse_get_event_info@.
      * 
      * When the hook is called, the editor will automatically change
      * to the window where the event occured.  The return value of
      * the hook is used to dictate whether or not hook handled the
      * event or whether the editor should switch back to the window
      * prior to the event.  Specifically, the return value is interpreted
      * as follows:
      * 
      * @   -1     Event not handled, pass to default hook.
      * @    0     Event handled, return active window prior to event
      * @    1     Event handled, stay in current window.
      * 
      * Related Functions: @mouse_get_event_info@, @mouse_set_current_window@, @set_buffer_hook@
      */
     MAKE_INTRINSIC(".mouse_map_buttons", jed_map_mouse_buttons, VOID_TYPE, 2),
     /* Prototype: Void mouse_map_buttons (Integer x, Integer y);
      * This function may be used to map one mouse button to another.  The
      * button represented by @x@ will appear as @y@.
      */

#endif
     
     MAKE_VARIABLE(".HIGHLIGHT", &Wants_Attributes, INT_TYPE, 0),
   /* Set this variable non-zero to highlight marked regions */
     MAKE_VARIABLE(".HORIZONTAL_PAN", &Wants_HScroll, INT_TYPE, 0),
   /* If this variable is non-zero, the window pans with the Point.  Actually
    * if the value is less than zero, the entire window pans.  If the value is 
    * positive, only the current line will pan.  The absolute value of the 
    * number determines the panning increment.  */
#ifdef VMS
     MAKE_INTRINSIC(".vms_get_help", vms_get_help,VOID_TYPE, 2),
   /* Prototype: Void vms_get_help (String hlp_file, String hlp_topic);
    * This function may be used on VMS systems to interact with the VMS help
    * system from within the editor.  @hlp_file@ is the name of the help file
    * to use and @hlp_topic@ is the topic for which help is desired.
    */
     MAKE_INTRINSIC(".vms_send_mail", vms_send_mail, INT_TYPE, 2),
   /* Prototype: Integer vms_send_mail (String recip_lis, String subj);
    * This VMS specific function provides an interface to the VMS callable
    * mail facility.  The first argument, @recip_lis@, is a comma separated list
    * of email addresses and @subj@ is a string that represents the subject of
    * the email.  The current buffer will be emailed.  It returns @1@ upon 
    * success and @0@ upon failure.
    */
#endif
#ifdef __unix__
#ifndef __GO32__
     MAKE_INTRINSIC(".enable_flow_control", enable_flow_control, VOID_TYPE, 1),
   /* Prototype: Void enable_flow_control (Integer flag);
    * This Unix specific function may be used to turn XON/XOFF flow control
    * on or off.  If @flag@ is non-zero, flow control is turned on; otherwise,
    * it is turned off.
    */
#endif
#endif
     MAKE_INTRINSIC(".core_dump", exit_error_cmd, VOID_TYPE, 2),
   /* Prototype: Void core_dump(String msg, Integer severity);
    * @core_dump@ will exit the editor dumping the state of some crucial 
    * variables. If @severity@ is @1@, a core dump will result.  Immediately
    * before dumping, @msg@ will be displayed.
    * Related Functions: @exit_jed@, @quit_jed@, @message@, @error@
    */
     MAKE_INTRINSIC(".get_last_macro", get_last_macro, VOID_TYPE, 0),
   /* Prototype: String get_last_macro ();
    * This function returns characters composing the last keyboard macro.  The
    * charactors that make up the macro are encoded as themselves except the
    * following characters:
    * @ '\n'    ---->   \J
    * @ null    ---->   \@
    * @  \      ---->   \\
    * @  '"'    ---->   \"
    */
#ifdef __os2__
     MAKE_INTRINSIC(".IsHPFSFileSystem", IsHPFSFileSystem, INT_TYPE, 1),
      /* Prototype: Integer IsHPFSFileSystem(String path);
       * Returns non-zero if drive of @path@ (possibly the default drive) is 
       * HPFS. 
       */
#endif
#ifdef pc_system
     MAKE_INTRINSIC(".msdos_fixup_dirspec", msdos_pinhead_fix_dir, STRING_TYPE, 1),
      /* Prototype: String msdos_fixup_dirspec (String dir);
       * The motivation behind this is that DOS does not like a trailing
       * backslash @\\@ except if it is for the root dir.  This function makes
       * @dir@ conform to that.
       */
#endif
     MAKE_VARIABLE(".WANT_SYNTAX_HIGHLIGHT", &Wants_Syntax_Highlight, INT_TYPE, 0),
     MAKE_INTRINSIC(".set_top_status_line", define_top_screen_line, VOID_TYPE, 1),
   /* Prototype: String set_top_status_line (String str);
    * This functions sets the string to be displayed at the top of the 
    * display. It returns the value of the line that was previously 
    * displayed.
    * 
    * Related Functions: @enable_top_status_line@
    */
     MAKE_INTRINSIC(".enable_top_status_line", enable_menu_bar, VOID_TYPE, 1),
   /* Prototype: Void enable_top_status_line (Integer x);
    * If x is non-zero, the top status line is enabled.  If x is zero, the
    * top status line is disabled and hidden.
    * Related Functions: @set_top_status_line@
    */
     MAKE_VARIABLE(".TOP_WINDOW_ROW", &Top_Window_Row, INT_TYPE, 1),
   /* This read-only variable gives the value of the starting row of the top
    * window.  
    */
     MAKE_VARIABLE(".DEFINING_MACRO", &Defining_Keyboard_Macro, INT_TYPE, 1),
     MAKE_VARIABLE(".EXECUTING_MACRO", &Executing_Keyboard_Macro, INT_TYPE, 1),
#if JED_HAS_LINE_MARKS
     MAKE_INTRINSIC(".create_line_mark", jed_create_line_mark, VOID_TYPE, 1),
   /* Prototype: User_Mark create_line_mark (Integer c);
    * The function @create_line_mark@ returns an object of the type
    * @User_Mark@.  This object contains information regarding the current
    * position and current buffer.  The parameter @c@ is used to specify the 
    * color to use when the line is displayed.
    * 
    * Related Functions: @create_user_mark@, @set_color_object@
    */
#endif
     MAKE_INTRINSIC(".create_user_mark", create_user_mark, VOID_TYPE, 0),
   /* Prototype: User_Mark create_user_mark ();
    * The function @create_user_mark@ returns an object of the type
    * @User_Mark@. This object contains information regarding the current
    * position and current buffer.
    * 
    * Related Functions: @move_user_mark@, @goto_user_mark@, @user_mark_buffer@
    */
     MAKE_INTRINSIC(".goto_user_mark", goto_user_mark, VOID_TYPE, 0),
   /* Prototype: Void goto_user_mark (User_Mark mark);
    * This function returns to the position of the User Mark @mark@.  Before 
    * this function may be called, the current buffer must be the buffer 
    * associated with the makr.
    * Related Functions: @move_user_mark@, @create_user_mark@, @user_mark_buffer@
    */
     MAKE_INTRINSIC(".move_user_mark", move_user_mark, VOID_TYPE, 0),
   /* Prototype: Void move_user_mark (User_Mark mark);
    * This function call takes a previously created User Mark, @mark@, and
    * moves it to the current position and buffer.  This means that if one
    * subsequently calls @goto_user_mark@ with this mark as an argument, the
    * the position will be set to what it is prior to the call to
    * @move_user_mark@.
    * Note: This function call is not equivalent to simply using
    * @ mark = create_user_mark ();
    * because independent copies of a User Mark are not created uponn
    * assignment.  That is, if one has
    * @ variable mark1, mark2;
    * @ setbuf ("first");
    * @ mark1 = create_user_mark ();
    * @ mark2 = mark1;
    * @ setbuf ("second");
    * and then calls 
    * @ move_user_mark (mark1);
    * both user marks, @mark1@ and @mark2@ will be moved since they refer to
    * the same mark.
    * Related Functions: @goto_user_mark@, @create_user_mark@, @user_mark_buffer@
    */
     
     MAKE_INTRINSIC(".user_mark_buffer", user_mark_buffer, STRING_TYPE, 0),
   /* Prototype: String user_mark_buffer (User_Mark m);
    * This function returns the name of the buffer associated with the 
    * User Mark specified by @m@.
    * Related Functions: @goto_user_mark@, @create_user_mark@, @move_user_mark@, @is_user_mark_in_narrow@
    */
     
   MAKE_INTRINSIC(".is_user_mark_in_narrow", jed_is_user_mark_in_narrow, INT_TYPE, 0),
   /* Prototype: Integer is_user_mark_in_narrow (User_Mark m);
    * This function returns non-zero if the user mark @m@ refers to a 
    * position that is within the current narrow restriction of the current
    * buffer.  It returns zero if the mark lies outside the restriction.
    * An error will be generated if @m@ does not represent a mark for the current 
    * buffer.
    * Related Functions: @goto_user_mark@, @move_user_mark@
    */

#if JED_HAS_ABBREVS
     
     MAKE_INTRINSIC(".list_abbrev_tables", list_abbrev_tables, INT_TYPE, 0),
   /* Prototype: Integer list_abbrev_tables ();
    * This function returns the names of all currently defined 
    * abbreviation tables.  The top item on the stack will be the number of
    * tables followed by the names of the tables.
    */
     MAKE_INTRINSIC(".use_abbrev_table", use_abbrev_table, VOID_TYPE, 1),
   /* Prototype: Void use_abbrev_table (String table);
    * Use the abbreviation table named @table@ as the abbreviation table for 
    * the current buffer.  By default, the "Global" table is used.
    */
     MAKE_INTRINSIC(".create_abbrev_table", create_abbrev_table, VOID_TYPE, 2),
    /* Prototype: Void create_abbrev_table (String name, String word);
     * Create an abbreviation table with name @name@.  The second parameter
     * @word@ is the list of characters used to represent a word for the 
     * table. If the empty string is passed for @word@, the characters that
     * currently constitute a word are used.
     */
     MAKE_INTRINSIC(".define_abbrev", define_abbrev, VOID_TYPE, 3),
   /* Prototype: Void define_abbrev (String tbl, String abbrv, String expans);
    * This function is used to define an abbreviation @abbrv@ that will be 
    * expanded to @expans@.  The definition will be placed in the table with
    * name @tbl@.
    */
     MAKE_INTRINSIC(".abbrev_table_p", abbrev_table_p, INT_TYPE, 1),
   /* Prototype: Integer abbrev_table_p (String name);
    * Returns non-zero if an abbreviation table with called @name@ exists. If
    * the table does not exist, it returns zero.
    */
     MAKE_INTRINSIC(".dump_abbrev_table", dump_abbrev_table, VOID_TYPE, 1),
   /* Prototype: Void dump_abbrev_table (String name);
    * This function inserts the contents of the abbreviation table called
    * @name@ into the current buffer.
    */
     MAKE_INTRINSIC(".what_abbrev_table", what_abbrev_table, VOID_TYPE, 0),
   /* Prototype: (String, String) what_abbrev_table ();
    * This functions returns both the name of the abbreviation table and the 
    * definition of the word for the table currently associated with the 
    * current buffer.  If none is defined it returns two empty strings.
    */
     MAKE_INTRINSIC(".delete_abbrev_table", delete_abbrev_table, VOID_TYPE, 1),
   /* Prototype: Void delete_abbrev_table (String name);
    * Delete the abbrev table specified by @name@.
    */
     
#endif
#if JED_HAS_COLOR_COLUMNS
     MAKE_INTRINSIC(".set_column_colors", set_column_colors, VOID_TYPE, 3),
   /* Prototype: Void set_column_colors (Integer color, Integer c0, Integer c1);
    * This function associates a color with columns @c0@ through @c1@ in the 
    * current buffer.  That is, if there is no syntax highlighting already
    * defined for the current buffer, when the current buffer is displayed, 
    * columns @c0@ through @c1@ will be displayed with the attributes of the
    * @color@ object.  The parameters @c0@ and @c1@ are restricted to the range
    * 1 through @SCREEN_WIDTH@.  Use the function @set_color_object@ to assign
    * attributes to the @color@ object.
    * Related Functions: @set_color_object@
    */
     MAKE_INTRINSIC(".set_color_object", set_color_object, VOID_TYPE, 3),
   /* Prototype: Void set_color_object (Integer obj, String fg, String bg);
    * Associate colors fg and bg with object obj.  Valid values for @obj@ 
    * are in the range 30 to 128.  All other values are reserved.  Values for
    * the strings @fg@ and @bg@ are as given by the description for @set_color@.
    * Related Functions: @set_column_colors@, @set_color@
    */
#endif
     MAKE_INTRINSIC(".translate_region", translate_region, VOID_TYPE, 0),
   /* Prototype: Void translate_region ();
    * This function uses the global character array @TRANSLATE_ARRAY@ to
    * modify the characters in a region based on the mapping defined by the
    * array.  The only restriction is that the newline character cannot be
    * mapped.  This example
    * @ define swap_a_and_b ()
    * @ {
    * @   variable i;
    * @   _for (0; 255, 1)
    * @     {
    * @        i = ();
    * @        TRANSLATE_ARRAY[i] = i;
    * @     }
    * @   TRANSLATE_ARRAY['a'] = 'b';
    * @   TRANSLATE_ARRAY['b'] = 'a';
    * @   bob (); push_mark (); eob ();
    * @   translate_region ();
    * @ }
    * uses @translate_region@ to swap the @'a'@ and @'b'@ characters in the 
    * current buffer.
    * Related Functions: @insert@, @delete@, @what_char@
    * Related Variables: @TRANSLATE_ARRAY@
    */
     
     MAKE_INTRINSIC(".set_current_kbd_command", set_current_kbd_command, VOID_TYPE, 1),
   /* Prototype: Void set_current_kbd_command (String s);
    */
     MAKE_VARIABLE(".LAST_KBD_COMMAND", Last_Kbd_Command_String, STRING_TYPE, 1),
     MAKE_VARIABLE(".CURRENT_KBD_COMMAND", Current_Kbd_Command_String, STRING_TYPE, 1),
     
     MAKE_INTRINSIC(".find_matching_delimiter", find_matching_delimiter, INT_TYPE, 1),
   /* Prototype: Integer find_matching_delimiter (Integer ch);
    * This function scans either forward or backward looking for the
    * delimiter that matches the character specified by @ch@.  The actual
    * direction depends upon the syntax of the character @ch@.  The
    * matching delimiter pair must be declared as such by a prior call to
    * @define_syntax@.  This function returns one of the following values:
    * @  1    Match found
    * @  0    Match not found
    * @ -1    A match was attempted from within a string.
    * @ -2    A match was attempted from within a comment
    * @  2    No information
    * In addition, the current point is left either at the match or is left
    * at the place where the routine either detected a mismatch or gave up.
    * In the case of a comment or a string (return values of -2 or -1), the 
    * current point is left at the beginning of a comment.
    * Note: If the of @ch@ is zero, the character at the current point will be 
    * used.
    * Related Functions: @blink_match@, @create_syntax_table@, @define_syntax@, @parse_to_point@
    */
     MAKE_INTRINSIC(".blink_match", blink_match, VOID_TYPE, 0),
   /* Prototype: Void blink_match ();
    * This function will attempt to blink the matching delimiter immediately 
    * before the editing point.
    * Related Functions: @find_matching_delimiter@, @define_syntax@
    */
     MAKE_INTRINSIC(".parse_to_point", parse_to_point, INT_TYPE, 0),
   /* Prototype: Integer parse_to_point ();
    * This function attempts to determine the syntactic context of the
    * current editing point.  That is, it tries to determine whether or not
    * the current point is in a comment, a string, or elsewhere.
    * It returns:
    * @ -2   In a comment
    * @ -1   In a string or a character
    * @  0   Neither of the above
    * Note: This routine is rather simplistic since it makes the assumption 
    * that the character at the beginning of the current line is not in a 
    * comment nor is in a string.
    * Related Functions: @define_syntax@, @find_matching_delimiter@
    */
     
     MAKE_INTRINSIC(".set_syntax_flags", set_syntax_flags, VOID_TYPE, 2),
   /* Prototype: Void set_syntax_flags (String table, Integer flag);
    * This function may be used to set the flags in the syntax table
    * specified by the @table@ parameter.  The @flag@ parameter may take
    * any of the following values or any combination bitwise or-ed together:
    * @ 1     Keywords are case insensitive
    * @ 2     Comments are Fortran-like
    * @ 4     Comments are C-like
    * @ 8     Keywords are TeX-like
    * A Fortran-like comment means that any line that begins with any
    * character that is not a digit is considered to be a comment.  A
    * C-like comment means that any line that starts with @"* "@ preceeded
    * by any ammount of whitespace is to be highlighted as a comment.
    * A TeX-like keyword is any word that follows the quote character.
    * Related Functions: @define_syntax@
    */
     MAKE_INTRINSIC(".define_syntax", define_syntax, VOID_TYPE, 2),
   /* Prototype: Void define_syntax (..., Integer type, String name);
    * This function adds a syntax entry to the table specified by the last
    * parameter @name@.  The actual number of parameters vary according to
    * the next to the last parameter @type@.
    * 
    * If @type@ is @'"'@ or @'\''@, a string or character delimiter syntax is
    * defined. In this case, @define_syntax@ only takes three parameters
    * where the first parameter is an integer that represents the character
    * for which the syntax is to be applied.
    * 
    * Similarly, if @type@ is @'\\'@, then a quote syntax is defined and
    * again @define_syntax@ only takes three parameters where the first
    * parameter is an integer that represents the character for which the
    * syntax is to be applied.  A quote character is one in which the
    * syntax of the following character is not treated as special.
    * 
    * If @type@ is @'('@, then @define_syntax@ takes four parameters where
    * the first two parameters are strings that represent a matching set of
    * delimiters.  The first string contains the set of opening delimiters
    * and the second string specifies the set of closing delimiters that
    * match the first set.  If a character from the closing set is entered
    * into the buffer, the corresponding delimiter from the opening set
    * will be blinked.  For example, if the C language syntax table is
    * called @"C"@, then one would use
    * @ define_syntax ("([{", ")]}", '(', "C");
    * to declare the matching delimiter set.  Note that the order of the
    * characters in the two strings must correspond.  That is, the above
    * example says that @'('@ matches @')'@ and so on.
    * 
    * If @type@ is @'%'@, a comment syntax is defined.  As in the
    * previous case, @define_syntax@ takes four parameters where there
    * first two parameters are strings that represent the begin and end
    * comment delimiters.  If the comment syntax is such that the comment
    * ends at the end of a line, the second string must either be the empty
    * string, @""@, or a newline @"\n"@.  In the current implementation, at
    * most the begin and end comment strings can consist of at most two
    * characters.
    * 
    * If @type@ is @'+'@, the first parameter is a string whose characters
    * are given the operator syntax.  If type is @','@, the first parameter
    * is a string composed of characters that are condered to be
    * delimiters.  If type is '0', the first parameter is a string composed
    * of characters that make up a number.
    * 
    * If @type@ is @<@, the first parameter is a string whose successive
    * characters form begin and end keyword highlight directives.
    * 
    * Finally, if @type@ is @'#'@, the first parameter is an integer whose
    * value corresponds to the character used to begin preprocessor lines.
    * 
    * As an example, imagine a language in which the dollar sign character
    * @$@ is used as a string delimiter, the backward quote character @`@
    * is used as a quote character, comments begin with a semi-colon and
    * end at the end of a line, and the characters @'<'@ and @'>'@ form 
    * matching delimiters.  The one might use
    * @ create_syntax_table ("strange");
    * @ define_syntax ('$',        '"',  "strange");
    * @ define_syntax ('`',        '\\', "strange");
    * @ define_syntax (";", "",    '%',  "strange");
    * @ define_syntax ("<", ">",   '(',  "strange");
    * to create a syntax table called @"strange"@ and define the
    * syntax entries for appropriate this example.
    * Related Functions: @create_syntax_table@, @use_syntax_table@, @find_matching_delimiter@
    * Related Variables: @BLINK@
    */
     MAKE_INTRINSIC(".use_syntax_table", use_syntax_table, VOID_TYPE, 1),
   /* Prototype: Void use_syntax_table (String n);
    * This function associates the current buffer with the syntax table
    * specified by the name @n@.  Until another syntax table is associated
    * with the buffer, the syntax table named @n@ will be used in all
    * operations that require a syntax.  This includes parenthesis matching,
    * indentation, etc.
    * Related Functions: @create_syntax_table@, @define_syntax@
    */
     MAKE_INTRINSIC(".create_syntax_table", create_syntax_table, VOID_TYPE, 1),
   /* Prototype: Void create_syntax_table (String name);
    * This the purpose of this function is to create a new syntax table
    * with the name specified by @name@.  If the table already exists, this
    * function does nothing.
    * Related Functions: @define_syntax@, @use_syntax_table@, @define_keywords@
    */
     MAKE_INTRINSIC(".define_keywords_n", define_keywords, VOID_TYPE, 4),
  /* Prototype: String define_keywords_n (String table, String kws, Integer len, Integer n);
   * This function is used to define a set of keywords that will be color
   * syntax highlighted in the keyword color associated with the table
   * specified by @n@.  The first parameter, @table@, specifies which
   * syntax table is to be used for the definition. The second parameter,
   * @kws@, is a string that is the concatenation of keywords of length
   * specified by the last parameter @len@.  The list of keywords specified
   * by @kws@ must be in alphabetic order.  The function returns the
   * previous list of keywords of length @len@. For example, C mode uses
   * the statement
   * @ () = define_keywords_n ("C", "asmforintnewtry", 3, 0);
   * to define the four three-letter keywords @asm@, @for@, @int@, @new@,
   * and @try@.  Note that in the above example, the return value is not used.
   * Related Functions: @define_syntax@, @set_color@
   * Related Variables: @WANT_SYNTAX_HIGHLIGHT@, @USE_ANSI_COLORS@
   */
#if JED_HAS_DFA_SYNTAX
     MAKE_INTRINSIC(".set_highlight_cache_dir", jed_set_dfa_cache_dir, VOID_TYPE, 0),
   /* Prototype: Void set_highlight_cache_dir (String dir);
    * This function sets the directory where the dfa syntax highlighting
    * cache files are located.
    * See also: @enable_highlight_cache@
    */
     MAKE_INTRINSIC(".define_highlight_rule", define_highlight_rule, VOID_TYPE, 3),
   /* Prototype: Void define_highlight_rule (String rule, String color, String n);
    * This function adds an enhanced highlighting rule to the
    * syntax table specified by the name @n@. The rule is described
    * as a regular expression by the string @rule@, and the
    * associated color is given by the string @color@, in the same
    * format as is passed to @set_color@. For example:
    * @ create_syntax_table ("demo");
    * @ define_highlight_rule ("[A-Za-z][A-Za-z0-9]*", "keyword", "demo");
    * @ define_highlight_rule ("//.*$", "comment", "demo");
    * @ build_highlight_table ("demo");
    * causes a syntax table to be defined in which any string of
    * alphanumeric characters beginning with an alphabetic is
    * highlighted in keyword color, and anything after "//" on a
    * line is highlighted in comment color.
    * 
    * The regular expression syntax understands character classes
    * like @[a-z]@ and @[^a-z0-9]@, parentheses, @+@, @*@, @?@, @|@
    * and @.@. Any metacharacter can be escaped using a backslash
    * so that it can be used as a normal character, but beware that
    * due to the syntax of S-Lang strings the backslash has to be
    * doubled when specified as a string constant. For example:
    * @ define_highlight_rule ("^[ \t]*\\*+[ \t].*$", "comment", "C");
    * defines any line beginning with optional whitespace, then one
    * or more asterisks, then more whitespace to be a comment. Note
    * the doubled backslash before the @*@.
    * 
    * Note also that @build_highlight_table@ must be called before
    * the syntax highlighting can take effect.
    * Related Functions: @create_syntax_table@, @use_syntax_table@, @build_highlight_table@
    * Related Variables: @WANT_SYNTAX_HIGHLIGHT@, @USE_ANSI_COLORS@
    */
     MAKE_INTRINSIC(".build_highlight_table", build_highlight_table, VOID_TYPE, 1),
   /* Prototype: Void build_highlight_table (String n);
    * This function builds a DFA table for the enhanced syntax
    * highlighting scheme specified for the syntax table specified
    * by the name @n@. This must be called before any syntax
    * highlighting will be done for that syntax table.
    * Related Functions: @create_syntax_table@, @use_syntax_table@, @define_highlight_rule@, @enable_highlight_cache@
    * Related Variables: @WANT_SYNTAX_HIGHLIGHT@, @USE_ANSI_COLORS@
    */
     MAKE_INTRINSIC(".enable_highlight_cache", enable_highlight_cache, VOID_TYPE, 2),
   /* Prototype: Void enable_highlight_cache (String file, String n);
    * This function enables caching of the DFA table for the
    * enhanced syntax highlighting scheme belonging to the syntax
    * table specified by the name @n@. This should be called before
    * any calls to @define_highlight_rule@ or to
    * @build_highlight_table@. The parameter @file@
    * specifies the name of the file (stored in the directory set by the
    * @set_highlight_cache_dir@ function) which should be used as a cache.
    * 
    * For example, in @cmode.sl@ one might write
    * @ enable_highlight_cache ("cmode.dfa", "C");
    * to enable caching of the DFA. If caching were not enabled for
    * C mode, the DFA would take possibly a couple of seconds to
    * compute every time Jed was started.
    *
    * Transferring cache files between different computers is
    * theoretically possible but not recommended. Transferring them
    * between different versions of Jed is not guaranteed to work.
    * 
    * Related Functions: @create_syntax_table@, @use_syntax_table@, @define_highlight_rule@, @build_highlight_table@
    * Related Variables: @WANT_SYNTAX_HIGHLIGHT@, @USE_ANSI_COLORS@
    */
#endif /* JED_HAS_DFA_SYNTAX */
     MAKE_VARIABLE(".DOLLAR_CHARACTER", &Jed_Dollar, INT_TYPE, 0),
  /* Prototype: Integer DOLLAR_CHARACTER = '$';
   * Character that is to be used to indicate that text extends to the left
   * or right of the window.  This character is traditionally a dollar sign.
   * The @"dollar"@ object may be used to assign it a color.
   * Related Functions: @set_color@
   */
     MAKE_VARIABLE("._jed_version", &Jed_Version_Number, INT_TYPE, 1),
  /* Prototype: Integer _jed_version;
   */
     MAKE_INTRINSIC(".set_expansion_hook", set_expansion_hook, VOID_TYPE, 1),
  /* Prototype: Void set_expansion_hook (String fname);
   * This function may be used to specify a function that will be called to
   * expand a filename upon TAB completion.  The function @fname@ must
   * already be defined.  When @fname@ is called, it is given a string to
   * be expanded. If it changes the string, it must return a non-zero value
   * and the modified string.  If the string is not modified, it must simply
   * return zero.
   */
#ifdef __unix__
#ifndef __GO32__
     MAKE_INTRINSIC(".get_passwd_info", get_passwd_info, VOID_TYPE, 1),
  /* Prototype: (dir, shell, pwd, uid, gid) = get_passwd_info (String username);
   * This function returns password information about the user with name
   * @username@.  The returned variables have the following meaning:
   * @ dir:     login directory
   * @ shell:   login shell
   * @ pwd:     encripted password
   * @ uid:     user identification number
   * @ gid:     group identification number
   * If the user does not exist, or the system call fails, the function
   * returns with @uid@ and @gid@ set to @-1@.
   */
     MAKE_INTRINSIC(".get_termcap_string", get_termcap_string, STRING_TYPE, 1),
  /* Prototype: String get_termcap_string (String cap);
   * This function may be used to extract the string associated with the
   * termcap capability associated with @cap@.
   * Note: This function is only available on Unix systems.
   */
#endif
#endif
     MAKE_INTRINSIC(".get_doc_string", get_doc_string, INT_TYPE, 2),
   /* Prototype: Integer get_doc_string (String obj, String filename);
    * This function may be used to extract the documentation for a variable
    * or function from a jed documentation file given by @filename@.
    * If successful, it returns non-zero as well as the documentation string.
    * It returns zero upon failure.  The first character of @obj@ determines 
    * whether @obj@ refers to a function or to a variable.  The rest of the 
    * characters specify the name of the object.
    */
#if JED_HAS_SUBPROCESSES
     MAKE_INTRINSIC(".kill_process", jed_close_process, VOID_TYPE, 1),
   /* Prototype: Void kill_process (Integer id);
    * Kill the subprocess specified by the process handle @id@.
    */
     MAKE_INTRINSIC(".send_process", jed_send_process, VOID_TYPE, 2),
   /* Prototype: Void send_process (Integer id, String s); */
     MAKE_INTRINSIC(".open_process", jed_open_process, INT_TYPE, 1),
    /* Prototype: Integer open_process (name, argv1, argv2, ..., argvN, N);
     * Returns id of process, -1 upon failure.
     */
     MAKE_INTRINSIC(".process_mark", jed_get_process_mark, VOID_TYPE, 1),
   /* Prototype: User_Mark process_mark (Integer id);
    * This function returns the user mark that contains the position of the 
    * last output by the process.
    */
     MAKE_INTRINSIC(".set_process", jed_set_process, VOID_TYPE, 3),
   /* Prototype: Void set_process (Integer pid, String what, String value);
    * @pid@ is the process hendle returned by @open_process@.  The second
    * parameter, @what@, specifies what to set.  It must be one of the
    * strings:
    * @ "signal" :  indicates that 'value' is the name of a function to call
    * @             when the process status changed.  The function specified
    * @             by 'value' must be declared to accept an argument list:
    * @             (pid, flags) where 'pid' has the same meaning as above and
    * @             flags is an integer with the meanings:
    * @               1: Process Running
    * @               2: Process Stopped
    * @               4: Process Exited Normally
    * @               8: Process Exited via Signal
    * @             To obtain more information about the process, e.g., exit_status,
    * @             use the function 'get_process_status'.
    * @             Note: when this function is called, the current buffer is
    * @             guaranteed to be the buffer associated with the process.
    * @ 
    * @ output" :   This parameter determines how output from the process is 
    * @             is processed.  If the 'value' is the empty string "", output
    * @             will go to the end of the buffer associated with the process 
    * @             and the point will be left there.
    * @             If value is ".", output will go at the current buffer position. 
    * @             If value is "@", output will go to the end of the buffer but
    * @             the point will not move.  Otherwise, 'value' is the name of 
    * @             a slang function with arguments: (pid, data) where pid has
    * @             the above meaning and data is the output from the process.
    */
     MAKE_INTRINSIC(".send_process_eof", jed_send_process_eof, VOID_TYPE, 1),
   /* Prototype: send_process_eof (Integer pid);
    * This function closes the @stdin@ of the process specified by the
    * handle @pid@.
    */
     MAKE_INTRINSIC(".get_process_input", get_process_input, VOID_TYPE, 1),
   /* Prototype: Void get_process_input (Integer tsecs);
    * Read all pending input by all subprocesses.  If no input is
    * available, this function will wait for input until @tsecs@ tenth of
    * seconds have expired.
    */
     MAKE_INTRINSIC(".signal_process", jed_signal_process, VOID_TYPE, 2),
   /* Prototype: Void signal_process (Integer pid, Integer signum);
    * This function may be used to send a signal to the process whose 
    * process handle is given by @pid@.  The @pid@ must be a valid handle
    * that was returned by @open_process@.
    * Related Functions: @open_process@, @kill_process@, @send_process_eof@
    */
#endif
     MAKE_INTRINSIC(".clear_message", clear_message, VOID_TYPE, 0),
     /* Prototype: Void clear_message ();
      * This function may be used to clear the message line of the display.
      * Related Functions: @message@, @update@, @error@, @flush@
      */
     MAKE_INTRINSIC(".flush_input", flush_input, VOID_TYPE, 0),
     /* Prototype: Void flush_input ();
      * This function may be used to remove all forms of queued input.
      * Related Functions: @input_pending@, @getkey@
      */
     SLANG_END_TABLE
};

/*}}}*/

static int jed_system (char *s) /*{{{*/
{
   if (Jed_Secure_Mode)
     {
	msg_error ("Access denied.");
	return -1;
     }
#ifdef pc_system
   return sys_System (s);
#else
   return system (s);
#endif
}

/*}}}*/

int init_jed_intrinsics (void) /*{{{*/
{
   int ret;
   
   ret = (SLang_add_table(Jed_Intrinsics, "Jed")
	  && SLang_add_table(Jed_Other_Intrinsics, "JedLine")
#if JED_HAS_LINE_ATTRIBUTES
	  && SLang_add_table(JedLine_Intrinsics, "JedLine")
#endif
	  );
   
   /* overload default slang version of system */
   SLadd_name ("system", (long) jed_system, SLANG_INTRINSIC, SLANG_MAKE_ARGS(INT_TYPE, 1));
#if JED_HAS_DISPLAY_TABLE
   SLang_add_array ("Display_Map",   /* slang name */
		    (long *) Output_Display_Table,   /* location of the array */
		    1,      /* number of dimensions */
		    256,    /* number of elements in X direction */
		    0, 0,   /* Number in Y and Z directions */
		    'c',    /* character array */
		    SLANG_IVARIABLE);   /* Read/Write array */
#endif
   
   
   SLang_add_array ("TRANSLATE_ARRAY",   /* slang name */
		    (long *) Translate_Region_Array,   /* location of the array */
		    1,      /* number of dimensions */
		    256,    /* number of elements in X direction */
		    0, 0,   /* Number in Y and Z directions */
		    'c',    /* character array */
		    SLANG_IVARIABLE);   /* Read/Write array */
   
#ifdef Sixteen_Bit_System
   SLdefine_for_ifdef("16_BIT_SYSTEM");
#endif
#if JED_HAS_SUBPROCESSES
   SLdefine_for_ifdef("SUBPROCESSES");
#endif
#if JED_HAS_LINE_ATTRIBUTES
   SLdefine_for_ifdef("HAS_LINE_ATTR");
#endif
#if JED_HAS_BUFFER_LOCAL_VARS
   SLdefine_for_ifdef("HAS_BLOCAL_VAR");
#endif
#if JED_HAS_DFA_SYNTAX
   SLdefine_for_ifdef("HAS_DFA_SYNTAX");
#endif
   return ret;
}

/*}}}*/


