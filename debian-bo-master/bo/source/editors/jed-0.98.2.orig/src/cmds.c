/* -*- mode: c; mode: fold -*- */
/*
 *  Copyright (c) 1992, 1996 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

/*{{{ #include files */

#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"

#include <string.h>

#include "buffer.h"
#include "screen.h"
#include "window.h"
#include "ins.h"
#include "ledit.h"
#include "cmds.h"
#include "line.h"
#include "paste.h"
#include "display.h"
#include "vterm.h"
#include "sysdep.h"
#include "text.h"
#include "file.h"
#include "misc.h"
#include "search.h"
#include "hooks.h"
#include "abbrev.h"
#if JED_HAS_SUBPROCESSES
# include "jprocess.h"
#endif

#if JED_HAS_LINE_ATTRIBUTES
# include "lineattr.h"
#endif

/*}}}*/

/*{{{ Global variables */

void (*X_Suspend_Hook)(void);
int Blink_Flag = 1;
int Indented_Text_Mode = 0;	       /* if non zero, intent line after wrap */
int Kill_Line_Feature = 1;	       /* non-zero means kill through eol if bolp  */
int Jed_Tab_Default = 8;
int Jed_Wrap_Column = 72;
int Jed_Suspension_Not_Allowed = 0;
int Jed_Use_Tabs = 1;

/*}}}*/

/*{{{ Static variables */

static char *Top_Of_Buffer_Error = "Top Of Buffer.";
static char *End_Of_Buffer_Error = "End Of Buffer.";

/*}}}*/
/*{{{ Static functions */

/* return 1 if line extends beyond the screen */
static int long_line(void)
{
   int p, col;
   
   col = Screen_Col;
   p = Point;
   Point = CLine->len - 1;
   if ((*(CLine->data + Point) != '\n') || (CBuf == MiniBuffer)) Point++;
   if (calculate_column() >= JWindow->width - 1)
     {
	Point = p;
	Screen_Col = col;
	return(1);
     }
   Screen_Col = col;
   Point = p;
   return(0);
}

/* check from point to end of line looking for tabs */
static int tabs_present(void)
{
   unsigned char *p, *pmax;
   
   if (!Buffer_Local.tab) return(0);
   pmax = CLine->data + CLine->len;
   p = CLine->data + Point;
   
   while (p < pmax) if (*p++ == '\t') return(1);
   return(0);
}

static int fast_not_ok (unsigned char ch)
{
   return ((Repeat_Factor != NULL) || JWindow->trashed
	   || (ch < 32) || (ch > 126) || Input_Buffer_Len
	   || Suspend_Screen_Update
	   || (Screen_Col >= JWindow->width - 2) || (JWindow->column > 1)
	   || Batch || (CLine->len == 0)
	   || ((CBuf->marks != NULL) && Wants_Attributes)
#if JED_HAS_LINE_ATTRIBUTES
	   || ((CLine->next != NULL) && (CLine->next->flags & JED_LINE_HIDDEN))
#endif	   
	   || input_pending(&Number_Zero) || Executing_Keyboard_Macro
	   || tabs_present()
	   || (Read_This_Character != NULL));
}

#if JED_HAS_LINE_ATTRIBUTES
static int check_line_attr_no_modify (Line *l)
{
   if (0 == (l->flags & JED_LINE_HIDDEN))
     return 0;
   
   msg_error ("You cannot edit this hidden line.");
   return -1;
}
#endif

static int next_visible_lines (int n)
{
#if JED_HAS_LINE_ATTRIBUTES
   int dn;
   int i;
   
   i = 0;
   while (i < n)
     {
	Line *l = CLine;
	
	dn = 0;
	do
	  {
	     l = l->next;
	     dn++;
	  }
	while ((l != NULL) && (l->flags & JED_LINE_HIDDEN));
	
	if (l == NULL) break;
	
	CLine = l;
	LineNum += dn;
	i++;
     }
   if (i) Point = 0;
   return i;
#else
   return nextline (&n);
#endif
}

static int prev_visible_lines (int n)
{
#if JED_HAS_LINE_ATTRIBUTES
   int i, dn;
   
   i = 0;
   while (i < n)
     {
	Line *l = CLine;
	
	dn = 0;
	do
	  {
	     l = l->prev;
	     dn++;
	  }
	while ((l != NULL) && (l->flags & JED_LINE_HIDDEN));
	
	if (l == NULL) break;
	
	CLine = l;
	LineNum -= dn;
	i++;
     }
   if (i) eol ();			       /* leave point at eol */
   return i;
#else
   return prevline (&n);
#endif
}

static int prev_visible_chars (int n)
{
#if JED_HAS_LINE_ATTRIBUTES
   push_mark ();
   n = backwchars (&n);
   if (n && (CLine->flags & JED_LINE_HIDDEN))
     {
	jed_skip_hidden_lines_backward (&Number_One);
	if (CLine->flags & JED_LINE_HIDDEN)
	  {
	     pop_mark (&Number_One);
	     return 0;
	  }
     }
   pop_mark (&Number_Zero);
   return n;
#else
   return backwchars (&n);
#endif
}

static int next_visible_chars (int n)
{
#if JED_HAS_LINE_ATTRIBUTES
   push_mark ();
   n = forwchars (&n);
   if (n && (CLine->flags & JED_LINE_HIDDEN))
     {
	jed_skip_hidden_lines_forward (&Number_One);
	if (CLine->flags & JED_LINE_HIDDEN)
	  {
	     pop_mark (&Number_One);
	     return 0;
	  }
     }
   pop_mark (&Number_Zero);
   return n;
#else
   return forwchars (&n);
#endif
}

static void check_last_key_function (FVOID_STAR f)
{
   /* For syntax highlighting */
   if ((Last_Key_Function != f)
       && ((Last_Key_Function == (FVOID_STAR) ins_char_cmd)
	   || (Last_Key_Function == (FVOID_STAR) eol_cmd)
	   || (Last_Key_Function == (FVOID_STAR) delete_char_cmd)
	   || (Last_Key_Function == (FVOID_STAR) backward_delete_char_cmd)
	   || (Last_Key_Function == (FVOID_STAR) backward_delete_char_untabify)))
     register_change(0);
}

   
static void next_line_prev_line_helper (int *gcp, int *ret, int dr, FVOID_STAR f)
{
   int gc;
   
   check_line();
   gc = calculate_column();
   if (Cursor_Motion <= 0) Goal_Column = gc;
   else if (Goal_Column < gc) Goal_Column = gc;
   *gcp = gc;
   
   Cursor_Motion = 2;
   
   check_last_key_function (f);
   
   *ret = (JWindow->trashed 
	   || (CLine == JScreen[JWindow->top + dr - 1].line)
#if JED_HAS_LINE_ATTRIBUTES
	   || (CLine->flags & JED_LINE_READONLY)
#endif
	   );
}

static void eob_bob_error (int f)
{   
   char *str;
   
   if (CBuf->bob_eob_error_hook != NULL)
     {
	SLang_push_integer (f);
	SLexecute_function (CBuf->bob_eob_error_hook);
	return;
     }
   
   if (f < 0)
     str = Top_Of_Buffer_Error;
   else 
     str = End_Of_Buffer_Error;
   
   msg_error (str);
}

/*}}}*/

/*{{{ interactive insert/delete functions */

/*{{{ Interactive char deletion functions */

int delete_char_cmd (void)
{
   int upd, n;
   char ch;
   
   CHECK_READ_ONLY
#if 0
     ;
#endif

#if JED_HAS_LINE_ATTRIBUTES
   if (check_line_attr_no_modify (CLine))
     return 0;
#endif

   if (eobp())
     {
	msg_error(End_Of_Buffer_Error);
	return(0);
     }
   
   ch = *(CLine->data + Point);
   
   if (fast_not_ok ((unsigned char) ch) || long_line () || *tt_Term_Cannot_Insert)
     {
	if ((Point == 0) && eolp()
#if JED_HAS_LINE_ATTRIBUTES
	    && ((CLine->prev != NULL)
		&& (0 == (CLine->prev->flags & JED_LINE_READONLY)))
#endif
	    ) n = 1;
	else n = 0;
	/* same effect, update looks better */
	n = backwchars(&n);
	del();
	forwchars(&n);
	upd = 1;
     }
   else
     {
	upd = ((CBuf->flags & BUFFER_TRASHED) == 0);
	fast_del();
	tt_delete_char ();
     }
   
   return(upd);
}

int backward_delete_char_cmd()
{
   char ch;
   int ret;
   
   CHECK_READ_ONLY
#if 0
     ;
#endif

   if (bobp())
     {
	msg_error(Top_Of_Buffer_Error);
	return(0);
     }
   
#if JED_HAS_LINE_ATTRIBUTES
   if (check_line_attr_no_modify (CLine))
     return 0;
#endif

   if (bolp())
     {
#if JED_HAS_LINE_ATTRIBUTES
	if (check_line_attr_no_modify (CLine->prev))
	  return 0;
#endif
	backwchars(&Number_One);
	return delete_char_cmd();
     }
   
   ch = *(CLine->data + (Point - 1));
   
   if (fast_not_ok ((unsigned char) ch) || long_line () || *tt_Term_Cannot_Insert)
     {
	backwchars(&Number_One);
	del();
	return(1);
     }
   
   ret = ((CBuf->flags & BUFFER_TRASHED) == 0);
   backwchars(&Number_One);
   tt_putchar('\b');
   Screen_Col--;
   fast_del();
   tt_delete_char();
   return(ret);
}

int backward_delete_char_untabify()
{
   unsigned char *p;
   int n;
   
   CHECK_READ_ONLY
#if 0
     ;
#endif
#if JED_HAS_LINE_ATTRIBUTES
   if (check_line_attr_no_modify (CLine))
     return 0;
#endif   
   p = CLine->data + (Point - 1);
   
   if (!Point || (*p != '\t') || !Buffer_Local.tab) 
     return backward_delete_char_cmd();
   
   n = calculate_column() - 1;
   backwchars(&Number_One);
   del();
   n = n - calculate_column();
   ins_char_n_times(' ', n);
   
   return(1);
}

/*}}}*/

/*{{{ newline_and_indent */
int newline_and_indent ()
{
   static int in_function;
   if (in_function) return -1;
   
#if JED_HAS_LINE_ATTRIBUTES
   if (check_line_attr_no_modify (CLine)
       || ((CLine->next != NULL) 
	   && eolp () && check_line_attr_no_modify (CLine->next)))
     return 0;
#endif
   
   if (CBuf->newline_indent_hook != NULL)
     {
	in_function = 1;
	SLexecute_function(CBuf->newline_indent_hook);
	in_function = 0;
	return 1;
     }
   newline();
   indent_line();
   return(1);
}

/*}}}*/
/*{{{ newline */

int newline (void)
{
   CHECK_READ_ONLY
#if 0
     ;
#endif

   if (CBuf == MiniBuffer) return exit_minibuffer();
   
   insert_string ("\n");
   return(1);
}

int newline_cmd (void)
{
#if JED_HAS_LINE_ATTRIBUTES
   if (check_line_attr_no_modify (CLine)
       || ((CLine->next != NULL) 
	   && eolp () && check_line_attr_no_modify (CLine->next)))
     return 0;
#endif
   return newline ();
}

/*}}}*/

/*{{{ ins_char_cmd */
int ins_char_cmd (void)
{
   char ch;
   
   int wrap = Jed_Wrap_Column;
   int ret, ll;
   
   CHECK_READ_ONLY
#if 0
     ;
#endif
#if JED_HAS_LINE_ATTRIBUTES
   if (check_line_attr_no_modify (CLine))
     return 0;
#endif
   
   ch = (char) SLang_Last_Key_Char;
   
   if (ch == '\n')
     {
	newline();
	return(1);
     }
   
#if JED_HAS_ABBREVS
   if (CBuf->flags & ABBREV_MODE)
     {
	expand_abbrev (ch);
     }
#endif
   
   if ((CBuf->flags & OVERWRITE_MODE) && !eolp()) del();
   
   if (((ch == ' ') || (Point >= wrap)) && (CBuf->modes & WRAP_MODE)
       && (calculate_column() > wrap)) /* JWindow->width */
     {
	ins(ch);
	wrap_line(0);			/* do not format--- just wrap */
	if (Indented_Text_Mode) indent_line ();
	if (CBuf->wrap_hook != NULL)
	  SLexecute_function(CBuf->wrap_hook);
	
	return(1);
     }
   
   if ((ch == ' ') || fast_not_ok ((unsigned char) ch) || (CBuf->flags & OVERWRITE_MODE)
       || (*tt_Term_Cannot_Insert && !eolp())
       )
     {
	ins(ch);
	if ((CBuf->syntax_table != NULL)
	    && (CBuf->syntax_table->char_syntax[(unsigned char) ch] & CLOSE_DELIM_SYNTAX)
	    && !input_pending(&Number_Zero)) blink_match (); /* (ch); */
	return(1);
     }
   
   
   ret = ((CBuf->flags & BUFFER_TRASHED) == 0) || (User_Prefers_Line_Numbers > 1);
   
   if (long_line ()) ll = 1; else ll = 0;
   ret = ret || ll;
   
   /* fast_ins goes first so that screen structure is updated now
    for following calls to use. */
   
   fast_ins(ch);
   if (!eolp() || ll)
     {
	tt_begin_insert();
	tt_putchar(ch);
	tt_end_insert();
	if (ll) register_change(0);
     }
   else tt_putchar(ch);
   Screen_Col++;
   if ((CBuf->syntax_table != NULL)
       && (CBuf->syntax_table->char_syntax[(unsigned char) ch] & CLOSE_DELIM_SYNTAX)
       && !input_pending(&Number_Zero)) blink_match (); /* (ch); */
   return(ret);
}

/*}}}*/
/*{{{ quoted_insert */
int quoted_insert()
{
   char ch;
   int lerr = SLang_Error;
   
   CHECK_READ_ONLY
     if (*Error_Buffer || SLKeyBoard_Quit) return(0);
   if (Repeat_Factor != NULL)
     {
	ch = *Repeat_Factor;
	Repeat_Factor = NULL;
     }
   else
     {
	SLang_Key_TimeOut_Flag = 1;
	ch = jed_getkey();
	SLang_Key_TimeOut_Flag = 0;
     }
   
   
   SLang_Error = lerr;  /* ^G may set it */
   
   if ((ch == '\n') && (CBuf == MiniBuffer))
     {
	ins('\n');
	/* msg_error("Not allowed!"); */
	return (1);
     }
   
   SLKeyBoard_Quit = 0;
   ins_char_n_times(ch, 1);
   
   
   if ((CBuf->syntax_table != NULL)
       && (CBuf->syntax_table->char_syntax[(unsigned char) ch] & CLOSE_DELIM_SYNTAX)
       && !input_pending(&Number_Zero)) blink_match (); /* (ch); */
   
   return(1);
}

/*}}}*/

/*{{{ kill_line */
int kill_line (void)
{
   int n, pnt, flag = 0;
   
   CHECK_READ_ONLY
     
     if (eobp())
     {
	msg_error(End_Of_Buffer_Error);
	return(0);
     }
   
   push_mark();
   push_spot();
   pnt = Point;
   eol();
   n = Point - pnt;
   if ((!pnt && Kill_Line_Feature) || !n)
     {
	
	/* Either of these (flag =0,1) have the same effect on the buffer.
	 *  However, the first sets the mark at the end of the line and moves
	 *  the point to the end of the previous line.  This way the current
	 *  line structure is deleted and the screen update looks better.
	 */
	if (!pnt && (CLine->prev != NULL) && (CLine->next != NULL))
	  {
	     flag = 1;
	     forwchars(&Number_One);
	  }
	else n += forwchars(&Number_One);
	
     }
   
   if ((Last_Key_Function == (FVOID_STAR) kill_line) && (Paste_Buffer != NULL))
     {
	copy_region_to_buffer(Paste_Buffer);
     }
   else copy_to_pastebuffer();
   pop_spot();
   if (flag) n += backwchars(&Number_One);
   generic_deln(&n);
   if (flag) forwchars(&Number_One);
   return(1);
}

/*}}}*/

/*}}}*/
/*{{{ interactive cursor movement functions */

/*{{{ Interactive char/line movement functions */

int previous_line_cmd (void)
{
   int ret, gc;
   
   next_line_prev_line_helper (&gc, &ret, 0, (FVOID_STAR) previous_line_cmd);
   
   if ((CLine == CBuf->beg)
       || (1 != prev_visible_lines (1)))
     {
	eob_bob_error (-1);
	return 1;
     }
   
   point_column(Goal_Column);
   return(ret);
}


int next_line_cmd (void)
{
   int ret, gc;
   
   next_line_prev_line_helper (&gc, &ret, JWindow->rows - 1, (FVOID_STAR) next_line_cmd);
   
   if ((CLine == CBuf->end)
       || (1 != next_visible_lines (1)))
     {
	eob_bob_error (1);
	return 1;
     }
   
   point_column(Goal_Column);
   return(ret);
}

int previous_char_cmd (void)
{
   int pnt;
   
   Cursor_Motion = 1;
   pnt = Point - 1;
   
   check_last_key_function ((FVOID_STAR) previous_char_cmd);
   
   if (1 != prev_visible_chars (1))
     {
	eob_bob_error (-2);
	return 1;
     }
   
   Goal_Column = calculate_column();
   return (pnt == -1) || JWindow->trashed;
}

int next_char_cmd ()
{
   Cursor_Motion = 1;
   
   check_last_key_function ((FVOID_STAR) next_char_cmd);
   
   if (1 != next_visible_chars (1))
     {
	eob_bob_error (2);
	return 1;
     }
   
   Goal_Column = calculate_column ();
   return !Point || JWindow->trashed;  /* Point = 0 ==> moved a line */
}

/*}}}*/

/*{{{ eol_cmd */
int eol_cmd (void)
{
   eol();
   if ((0 == (CBuf->flags & READ_ONLY))
#if JED_HAS_LINE_ATTRIBUTES
       && (0 == (CLine->flags & JED_LINE_READONLY))
#endif
       )
     trim_whitespace();
   return(1);
}

/*}}}*/

/*{{{ Scrolling/pageup/down functions */

int jed_buffer_visible (char *b)
{
   return buffer_visible (find_buffer(b));
}

static void scroll_completion (int dir)
{
   Window_Type *w;
   
   if (jed_buffer_visible (Completion_Buffer))
     {
	pop_to_buffer (Completion_Buffer);
   	if (dir > 0) pagedown_cmd (); else pageup_cmd ();
	while (!IS_MINIBUFFER) other_window ();
     }
   else
     {
	w = JWindow;
	other_window();
	if (!IS_MINIBUFFER)
	  {
	     if (dir > 0) pagedown_cmd (); else pageup_cmd ();
	  }
	while (JWindow != w) other_window ();
     }
}

int goto_bottom_of_window (void)
{
   int n;
   
   n = JWindow->rows - window_line ();

   return n == next_visible_lines (n);
}

void goto_top_of_window (void)
{
   int n;
   
   n = window_line () - 1;
   (void) prev_visible_lines (n);
}


static int Last_Page_Line;
static int Last_Page_Point;

/* the page up/down commands set cursor_motion to -1 because we do not
 want to use any goal column information */
int pagedown_cmd()
{
   int col, this_line, this_point;
   int n;
   
   Cursor_Motion = -1;
   if (IS_MINIBUFFER)
     {
	scroll_completion (1);
	return 1;
     }
   
   if (eobp())
     {
	eob_bob_error (3);
	return 1;
     }
   
   n = JWindow->rows;
   if ((CBuf != JWindow->buffer) || (n == 1))
     {
	return next_visible_lines (n);
     }
   
   if (JWindow->trashed)
     {
	update (NULL, 0, 0);
	if (JWindow->trashed) return next_visible_lines (n);
     }
   
   this_line = LineNum;
   this_point = Point;
   
   col = calculate_column ();
   if (goto_bottom_of_window ())
     {
	recenter (&Number_One);
     }
   
   goto_column1 (&col);
   
   if ((Last_Key_Function == (FVOID_STAR) pageup_cmd) && is_line_visible (Last_Page_Line))
     {
	goto_line (&Last_Page_Line);
	Point = Last_Page_Point;
     }
   else if (CLine->next == NULL) eol(); else Point = 0;
   
   Last_Page_Line = this_line;
   Last_Page_Point = this_point;
   
   return(1);
}

int pageup_cmd (void)
{
   int col, this_line, this_point;
   int n;
   
   Cursor_Motion = -1;
   
   if (IS_MINIBUFFER)
     {
	scroll_completion (-1);
	return 1;
     }
   
   if (bobp())
     {
	eob_bob_error (-3);
	return 1;
     }
   
   n = JWindow->rows;
   if ((CBuf != JWindow->buffer) || (n == 1))
     {
	return prev_visible_lines (n);
     }
   
   if (JWindow->trashed)
     {
	update (NULL, 0, 0);
	if (JWindow->trashed) return prev_visible_lines (n);
     }
   
   this_line = LineNum;
   this_point = Point;
   col = calculate_column ();
   goto_top_of_window ();
   (void) goto_column1(&col);
   recenter(&JWindow->rows);
   
   if ((Last_Key_Function == (FVOID_STAR) pagedown_cmd) && is_line_visible (Last_Page_Line))
     {
	goto_line (&Last_Page_Line);
	Point = Last_Page_Point;
     }
   else Point = 0; /* something like: Point = point_column(JWindow->column) better? */
   Last_Page_Line = this_line;
   Last_Page_Point = this_point;
   return(1);
}


int scroll_right()
{
   if (JWindow->column == 1) return(0);
   
   if ((JWindow->column = JWindow->column - JWindow->width / 2) < 1)
     {
	JWindow->column = 1;
     }
   
   touch_window();
   return(1);
}

int scroll_left ()
{
   JWindow->column = JWindow->column + JWindow->width / 2;
   touch_window();
   return(1);
}

/*}}}*/

/*}}}*/

/*{{{ Whitespace and Indention functions */

void insert_whitespace(int *n)
{
   int tab = Buffer_Local.tab;
   int c1, c2, i, k, nspace;
   
   if ((nspace = *n) <= 0) return;
   CHECK_READ_ONLY_VOID
     c1 = calculate_column() - 1;
   c2 = c1 + nspace;
   
   if (tab && Jed_Use_Tabs)
     {
	i = c1 / tab;
	k = c2 / tab - i;
	if (k) nspace = c2 - (i + k) * tab;
	ins_char_n_times('\t', k);
     }
   ins_char_n_times(' ', nspace);
}

/* get indent value of current line, n is the column */
unsigned char *get_current_indent(int *np)
{
   unsigned char *p, *pmax;
   int tab, n;
   
   tab = Buffer_Local.tab;
   p = CLine->data;
   pmax = CLine->data + CLine->len;
   n = 0;
   while((p < pmax) && ((*p == ' ') || (tab && (*p == '\t'))))
     {
	if (*p == '\t')
	  n = tab * (n / tab + 1);
	else n++;
	p++;
     }
   *np = n;
   return p;
}


int trim_whitespace ()
{
   register unsigned char *p, *pmax, ch;
   int n, save_point, len;
   
   CHECK_READ_ONLY
     len = CLine->len;
   if (len == 0) return(0);
   
   save_point = Point;
   if (Point == len) Point--;
   
   /* Note that we save the point before --ing it.  This way the comparison
    made below will effectively restore it if pointing at non whitespace */
   
   p = CLine->data + Point;
   if (Point && (*p == '\n') && (CBuf != MiniBuffer))
     {
	Point--;
	p--;
     }
   
   while ((Point > 0) && (((ch = *p) == '\t') || (ch == ' ')))
     {
	Point--;
	p--;
     }
   
   if (save_point != Point) if (!eolp() && ((*p != ' ') && (*p != '\t')))
     {
	Point++;
	p++;
     }
   
   n = 0;
   /* this needs to be inlined.  Actually for undo, I need del(n)! */
   pmax = CLine->data + CLine->len;
   while ((p < pmax) && ((ch = *p) != '\n')
	  && ((ch == ' ') || (ch == '\t'))) p++, n++;
   
   deln(&n);
   return(1);
}

/* indent line to column n */
void indent_to(int n)
{
   int m;
   
   get_current_indent(&m);
   
   if (n != m)
     {
	Point = 0;
	trim_whitespace();
	if (n >= 0) insert_whitespace(&n);
     }
}

int indent_line ()
{
   int n, n1;
   static int in_function;
   
   if (in_function) return -1;
   
   if (CBuf->indent_hook != NULL)
     {
	in_function = 1;
	SLexecute_function(CBuf->indent_hook);
	in_function = 0;
	return 1;
     }
   
   CHECK_READ_ONLY
     
     if (CLine == CBuf->beg) return(0);
   
   push_spot();
   CLine = CLine->prev;
   get_current_indent (&n);
   CLine = CLine->next;
   indent_to(n);
   pop_spot();
   
   /* This moves the cursor to the first non whitspace char if we are
    before it.  Otherwise leave it alone */
   
   n1 = calculate_column();
   get_current_indent(&n);
   if (n1 <= n) point_column(n + 1);
   return 1;
}

/*}}}*/

/*{{{ goto_column and goto_column1 */

/* goto to column c, returns actual column */
int goto_column1(int *cp)
{
   int c1, c = *cp;
   if (c <= 0) c = 1;
   eol();
   c1 = calculate_column();
   if (c1 > c)
     {
	point_column(c);
	c1 = calculate_column();
     }
   return(c1);
}

/* move cursor to column c adding spaces if necessary */
void goto_column(int *c)
{
   int c1 = *c;
   if (c1 <= 0) c1 = 1;
   c1 = c1 - goto_column1(&c1);
   insert_whitespace(&c1);
}

/*}}}*/
/*{{{ skip_whitespace */

/* does not leave current line */
unsigned int skip_whitespace()
{
   unsigned char *p, *pmax;
   
   if (CLine->len == 0) return('\n');
   
   p = CLine->data + Point;
   eol();
   pmax = CLine->data + Point;
   while(p < pmax)
     {
	if ((*p != ' ') && (*p != '\t')) break;
	p++;
     }
   Point = (int) (p - CLine->data);
   if (Point == CLine->len) return '\n';
   return(*p);
}

/*}}}*/
/*{{{ skip_chars1 */
void skip_chars1(char *range, int reverse)
{
   unsigned char lut[256];
   unsigned char *p, *pmax;
   Line *line;
   int n = 0;
   
   SLmake_lut(lut, (unsigned char *) range, (unsigned char) reverse);
   /* more flexibility is achieved with following line */
   if (reverse && lut['\n']) lut['\n'] = 0;
   
   line = CLine;
   while (line != NULL)
     {
	CLine = line;
	p = line->data + Point;
	pmax = line->data + line->len;
	
	while (p < pmax)
	  {
	     if (0 == lut[*p])
	       {
		  Point = (int)(p - CLine->data);
                  LineNum += n;
		  return;
	       }
	     p++;
	  }
	Point = 0;
	line = CLine->next;
        n++;
     }
   eob();
}

/*}}}*/
/*{{{ bskip_chars1 */

void bskip_chars1(char *range, int reverse)
{
   unsigned char lut[256];
   unsigned char *p;
   Line *line = CLine;
   int n = 0;
   
   SLmake_lut(lut, (unsigned char *) range, (unsigned char) reverse);
   /* more flexibility is achieved with following line */
   if (reverse && lut['\n']) lut['\n'] = 0;
   
   while (1)
     {
	if (Point == 0)
	  {
	     if (lut['\n'] == 0) return;
	     if (NULL == (line = CLine->prev)) break;
	     Point = line->len - 1;
             n++;
	  }
	
	CLine = line;
	
	p = line->data + (Point - 1);
	
	while (p >= line->data)
	  {
	     if (0 == lut[*p])
	       {
		  Point = 1 + (int)(p - line->data);
                  LineNum -= n;
		  return;
	       }
	     
	     p--;
	  }
	Point = 0;
     }
   bob();
}

/*}}}*/
/*{{{ skip_chars */
void skip_chars (char *range)
{
   if (*range == '^') skip_chars1(range + 1, 1);
   else
     {
	if (*range == '\\') range++;
	skip_chars1(range, 0);
     }
}

/*}}}*/
/*{{{ bskip_chars */
void bskip_chars (char *range)
{
   if (*range == '^') bskip_chars1(range + 1, 1);
   else
     {
	if (*range == '\\') range++;
	bskip_chars1(range, 0);
     }
}

/*}}}*/

/*{{{ looking_at */

/* returns < 0 if what is smaller than current thing, 0 if matched, pos otherwise */
int looking_at(char *what)
{
   register unsigned char *p, *pmax;
   register unsigned char *w = (unsigned char *) what, ch;
   Line *l = CLine;
   
   p = l->data + Point;
   while (1)
     {
	pmax = l->data + l->len;
	while (((ch = *w) != 0) && (p < pmax))
	  {
#define upcase(ch) (Case_Sensitive ? ch : UPPER_CASE(ch))
	     if ((upcase(ch) != upcase(*p))) return 0;
	     p++; w++;
	  }
	if (ch == 0) return 1;
	l = l->next;
	if (l == NULL) return 0;
	p = l->data;
     }
}

/*}}}*/

/*{{{ Exiting the editor */

/*{{{ sys_spawn_cmd */

int sys_spawn_cmd (void)
{
   if ((Jed_Secure_Mode)
       || (Jed_Suspension_Not_Allowed))
     {
	msg_error ("Access to shell denied.");
	return -1;
     }
   
   if (Batch)
     {
	sys_suspend ();
	return 0;
     }
   
   if (X_Suspend_Hook != NULL)
     {
	(*X_Suspend_Hook) ();
	return (0);
     }
   
   SLang_run_hooks("suspend_hook", NULL, NULL);
   if (SLang_Error) return(0);
   reset_display();
   reset_tty();
   sys_suspend();
   if (-1 == init_tty())
     {
	exit_error ("Unable to initialize terminal.", 0);
     }
   
   flush_input ();
   init_display(1);
   /* cls(); */
   SLang_run_hooks("resume_hook", NULL, NULL);
   check_buffers();
   return(1);
}

/*}}}*/
/*{{{ quit_jed */
int quit_jed(void)
{
   Buffer *b = CBuf;
   
   /* Any buffer marked with AUTO_SAVE_JUST_SAVE flag should be saved
    * if it has not already been.  That is what the flag is for and this
    * code fragment carries this out.
    */
   do
     {
	if ((b->flags & AUTO_SAVE_JUST_SAVE)
	    && (b->flags & BUFFER_TRASHED)
	    && (*b->file))
	  {
	     while (b->narrow != NULL) widen_buffer(b);
	     auto_save_buffer(b);      /* actually, it will save it */
	  }
	
#if defined(__WIN32__) && JED_HAS_SUBPROCESSES
 	if (b->subprocess)
 	  jed_kill_process(b->subprocess - 1);
#endif
	
	b = b->next;
     }
   while (b != CBuf);
   
   reset_display();
   reset_tty();
#ifdef VMS
   vms_cancel_exithandler();
#endif
#ifdef SLANG_STATS
   SLang_dump_stats("slang.dat");
#endif
#ifdef MALLOC_DEBUG
   SLmalloc_dump_statistics ();
#endif
   exit(0);
   return(1);
}


/*}}}*/
/*{{{ save_some_buffers */
/* I should try it like emacs--- if prefix argument, then save all without user
 intervention */
int save_some_buffers(void)
{
   Buffer *b, *tmp;
   int ans = 0;
   char msg[256];
   int err;
   
   b = CBuf;
   do
     {
	if ((b->flags & BUFFER_TRASHED)
	    && (*b->file))
	  {
	     if (b->flags & AUTO_SAVE_JUST_SAVE) ans = 'y';
	     else
	       {
		  sprintf(msg,"Buffer %s not saved. Save it? (y/n)", b->name);
		  flush_message(msg);
		  ans = my_getkey();
	       }

	     while (1)
	       {
		  if ((ans == 'y') || (ans == 'Y'))
		    {
		       tmp = CBuf;
		       switch_to_buffer(b);
		       while (b->narrow != NULL) widen_buffer(b);
		       err = write_file_with_backup(b->dir, b->file);
		       switch_to_buffer(tmp);
  		       if (err < 0) return(-1);
		       b->flags &= ~BUFFER_TRASHED;
		       b->flags |= AUTO_SAVE_BUFFER;
		       b->hits = 0;
		       break;
		    }
		  else if ((ans == 'n') || (ans == 'N'))
		    {
		       /* disabling this */
		       /* auto_save_buffer(b); */
		       break;
		    }
		  else if (SLKeyBoard_Quit)
		    {
		       /* warning--- bug here if user edits file at
			startup and forgets to save it then aborts. */
		       return(-1);
		    }
		  else
		    {
		       beep();
		       sprintf(msg,"Buffer %s not saved. Save it", b->name);
		       ans = get_yes_no(msg);
		       if (ans == 1) ans = 'y'; else if (!ans) ans = 'n';
		       else return(-1);
		    }
	       }
	  }
	
	b = b->next;
     }
   while (b != CBuf);

   clear_message ();
   return(1);
}

/*}}}*/
/*{{{ exit_jed */
int exit_jed (void)
{
   static int in_exit_jed = 0;
   
   if (!in_exit_jed)
     {
	in_exit_jed = 1;
	if (SLang_run_hooks("exit_hook", NULL, NULL))
	  {
	     in_exit_jed = 0;
	     return(1);
	  }
     }
   
   in_exit_jed = 0;
   if (SLang_Error) return(0);
   if (save_some_buffers() > 0) quit_jed();
   return(1);
}

/*}}}*/

/*}}}*/

