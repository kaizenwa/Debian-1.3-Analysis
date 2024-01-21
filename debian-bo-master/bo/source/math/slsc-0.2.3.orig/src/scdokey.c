#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <slang.h>

#include "jdmacros.h"

#include "sc.h"
#include "cmds.h"
#include "interp.h"
#include "lex.h"
#include "schelp.h"
#include "scdokey.h"

int Sc_Current_Mode = SC_ROW_MODE;

char *Sc_Modes[] = 
{
   "Row", "Column", "Block"
};

int Sc_Mark_Set;
static int Prefix_Arg = 1;
static int Direction_Set;

static int sc_begin_edit (char *, char *, int);
static int sc_write_menu_cmd (void);


void sc_redraw (void)
{
   SLsmg_cls ();
   FullUpdate = 1;
   sc_update (0);
   SLsmg_refresh ();
}


static unsigned char sc_get_response (char *prompt, char *ans)
{
   unsigned char ch;
   
   sc_message (prompt);
   SLsmg_refresh ();
   SLang_flush_input ();
   ch = SLang_getkey (); SLKeyBoard_Quit = 0;
   if ((ch >= 'A') && (ch <= 'Z')) ch |= 0x20;
   sc_clear_message ();
   
   while (*ans && ((unsigned char) *ans != ch)) ans++;
   if (*ans == 0)
     {
	slsc_error ("Invalid response!");
	return 0;
     }
   
   return ch;
}

static void sc_dup (void)
{
   if (Sc_Current_Mode == SC_ROW_MODE)
     duprow ();
   else if (Sc_Current_Mode == SC_COLUMN_MODE)
     dupcol ();
   else slsc_error ("Operation not allowed in Block Mode.");
}


static void sc_del (void)
{
   if (Sc_Current_Mode == SC_ROW_MODE)
     deleterow (Prefix_Arg);
   else if (Sc_Current_Mode == SC_COLUMN_MODE)
     deletecol (Prefix_Arg);
   else slsc_error ("Operation not allowed in Block Mode.");
}

static void sc_paste (void)
{
   if (Sc_Current_Mode == SC_ROW_MODE)
     pullcells ('r');
   else if (Sc_Current_Mode == SC_COLUMN_MODE)
     pullcells ('c');
   else pullcells ('m');
}


   
static void sc_show_cmd (void)
{
   register int r0, r1, c0, c1;
   unsigned char ch;
   
   r0 = 0; while ((r0 < MAXROWS) && (row_hidden[r0] == 0)) r0++;
   r1 = r0; while ((r1 < MAXROWS) && (row_hidden[r1] != 0)) r1++;
   
   c0 = 0; while ((c0 < MAXCOLS) && (col_hidden[c0] == 0)) c0++;
   c1 = c0; while ((c1 < MAXCOLS) && (col_hidden[c1] != 0)) c1++;
   
   r1--; c1--;
   
   if ((r0 < MAXROWS) && (c0 < MAXCOLS))
     {
	if (0 == (ch = sc_get_response ("Show row or column?  Enter 'r' or 'c':", "rc")))
	  {
	     return;
	  }
	if (ch == 'r') c0 = MAXCOLS;
     }
   
   if (c0 < MAXCOLS)
     {
	/* We have to do this in a 2 step process because the sc_coltoa
	 * returns a pointer to a static area.
	 */
	(void) sprintf(line,"%s:", sc_coltoa(c0));
	strcat (line, sc_coltoa(c1));
     }
   else if (r0 < MAXROWS)
     {
	(void) sprintf(line,"%d:%d", r0, r1);
     }
   else  
     {
	slsc_error ("There is nothing hidden to show.");
	return;
     }
   sc_begin_edit ("show ", line, 1);
}

int sc_get_region (int *r1, int *c1, int *dr1, int *dc1)
{
   int r, c, dr, dc;
   if (Sc_Mark_Set == 0) 
     {
	slsc_error ("Mark not set!");
	return 0;
     }
   
   r = savedrow;
   dr = currow - r;
   if (dr < 0)
     {
	dr = -dr;
	r = currow;
     }
   
   c = savedcol;
   dc = curcol - c;
   if (dc < 0)
     {
	dc = -dc;
	c = curcol;
     }
   
   *r1 = r; *dr1 = dr;
   *c1 = c; *dc1 = dc;
   return 1;
}

static void sc_set_mode (void)
{
   unsigned char ch;
   
   if (0 == (ch = sc_get_response ("Set mode: row, column, or block?  Enter 'r', 'c' or 'b'", "rcb")))
     {
	return;
     }
   if (ch == 'r') Sc_Current_Mode = SC_ROW_MODE;
   else if (ch == 'c') Sc_Current_Mode = SC_COLUMN_MODE;
   else Sc_Current_Mode = SC_BLOCK_MODE;
   FullUpdate = 1;
}

static void sc_delete_region (void)
{
   int r, c, dr, dc;

   if (!sc_get_region (&r, &c, &dr, &dc)) return;
   
   switch (Sc_Current_Mode)
     {
      case SC_ROW_MODE:
	currow = r;
	deleterow (dr + 1);
	break;
      case SC_COLUMN_MODE:
	curcol = c;
	deletecol (dc + 1);
	break;
      case SC_BLOCK_MODE:
	currow = r; curcol = c;
	sc_erase_block (r, c, r + dr, c + dc);
	/* currow = r + dr; curcol = c + dc; */
	break;
     }
   FullUpdate = 1;
   modflg = 1;
   Sc_Mark_Set = 0;
}

static void sc_hide_cmd (void)
{
   int r, c, dr, dc;

   if (!sc_get_region (&r, &c, &dr, &dc)) return;
   dr++; dc++;
   switch (Sc_Current_Mode)
     {
      case SC_ROW_MODE:
	currow = r;
	hiderow (dr);
	currow = r + dr;
	break;
      case SC_COLUMN_MODE:
	curcol = c;
	hidecol (dc);
	curcol = c + dc;
	break;
      case SC_BLOCK_MODE:
	/* This might be confusing so I am disabling it.
	 * currow = r1; curcol = c1;
	 * hiderow (r2 - r1);
	 * hidecol (c2 - c1);
	 */
	slsc_error ("Operation not permitted in Block Mode");
	return;
     }
   FullUpdate = 1;
   Sc_Mark_Set = 0;
   modflg++;
}



static void sc_delete_cell (void)
{
   struct ent **p, *q;
   
   p = &tbl[currow][curcol];
   q = *p;
   if (q != NULL) 
     {
	if ('y' != sc_get_response ("Delete this Cell? (y/n)", "yn")) return;
   
	q->flags |= IS_DELETED;
	*p = NULL;
	sync_refs();
	
	(void) sc_clearent(q);
	SLFREE((char *) q);
	modflg = 1;
	FullUpdate = 1;
     }
}



static void sc_sig_suspend (int sig)
{
#ifdef SIGTSTP
   sc_reset_display ();
   signal(SIGTSTP, SIG_DFL);
   (void) kill(getpid(),SIGTSTP);
   sc_init_display ();
   signal(SIGTSTP, sc_sig_suspend);
#endif
}

static void sc_eval_cmd (void)
{
   sc_eval_all ();
   FullUpdate++;
   Sc_Changed = 0;
}

static void sc_mark_cell (void)
{
   savedrow = currow;
   savedcol = curcol;
   sc_message ("Cell marked.");
   Sc_Mark_Set = 1;
}

#if 0
static void sc_copy_marked_cell (void)
{
   register struct ent *p = tbl[savedrow][savedcol];
   register c1;
   register struct ent *n;
   int arg = 1;
   
   if (p == NULL) return;
   
   FullUpdate++;
   Sc_Changed++;
   modflg++;
   for (c1 = curcol; arg-- && c1 < MAXCOLS; c1++) 
     {
	n = lookat (currow, c1);
	(void) sc_clearent(n);
	copyent( n, p, currow - savedrow, c1 - savedcol);
     }
   sc_message ("Cell copied.");
}
#endif

static void sc_suspend (void)
{
   sc_sig_suspend (0);
}


static void sc_up_cmd (void)
{
   backrow (1);
}

static void sc_down_cmd (void)
{
   forwrow (1);
}

static void sc_right_cmd (void)
{
   forwcol (1);
}

static void sc_left_cmd (void)
{
   backcol (1);
}

static void sc_pageup (void)
{
   backrow (SLtt_Screen_Rows - 4);
}

static void sc_pagedown (void)
{
   forwrow (SLtt_Screen_Rows - 4);
}

static void sc_pageright (void)
{
}

static void sc_pageleft (void)
{
}

static void sc_bol (void)
{
   doend (0, -1);
}

static void sc_eol (void)
{
   doend (0, 1);
}

static void sc_bob (void)
{
   doend (-1, 0);
}

static void sc_eob (void)
{
   doend (1, 0);
}


static void sc_exit_cmd (void)
{
   if (0 == modcheck(" before exiting"))
     {
	sc_quit (0);
     }
}

static void sc_exit_error (char *s)
{
   fprintf(stderr, "%s\n", s);
   sc_quit (-1);
}



static void sc_slrline_update (unsigned char *buf, int len, int col)
{
   SLsmg_gotorc (0, 0);
   SLsmg_write_nchars ((char *) buf, len);
   SLsmg_gotorc (0, col);
   SLsmg_refresh ();
}

static int tab_insert_range (void)
{
   int r, c, r1, c1;
   static char buf[20];
   
   if (Sc_Mark_Set) 
     {
	(void) sc_get_region (&r, &c, &r1, &c1);
	r1 += r; c1 += c;
	
	if (Sc_Current_Mode == SC_ROW_MODE)
	  {
	     c = 0;
	     c1 = MAXCOLS - 1;
	  }
	else if (Sc_Current_Mode == SC_COLUMN_MODE)
	  {
	     r = 0;
	     r1 = MAXROWS - 1;
	  }

	strcpy (buf, sc_r_name(r, c, r1, c1));
	
	FullUpdate = 1;
	Sc_Mark_Set = 0;
     }
   SLang_rline_insert (buf);
   return 1;
}

   
SLang_RLine_Info_Type *Sc_Keymap_RLI;

SLang_RLine_Info_Type  *sc_init_readline (void)
{
   unsigned char *buf = NULL;
   SLang_RLine_Info_Type *rli;
   
   if ((NULL == (rli = (SLang_RLine_Info_Type *) SLMALLOC (sizeof(SLang_RLine_Info_Type))))
       || (NULL == (buf = (unsigned char *) SLMALLOC (256))))
     {
	fprintf(stderr, "malloc error.\n");
	exit(-1);
     }
   
   SLMEMSET ((char *) rli, 0, sizeof (SLang_RLine_Info_Type));
   rli->buf = buf;
   rli->buf_len = 255;
   rli->tab = 8;
   rli->dhscroll = 20;
   rli->getkey = SLang_getkey;
   rli->tt_goto_column = NULL;
   rli->update_hook = sc_slrline_update;
   
   if (SLang_init_readline (rli) < 0)
     {
	fprintf(stderr, "Unable to initialize readline library.\n");
	exit (-1);
     }
   SLkm_define_key ("\t", (FVOID_STAR) tab_insert_range, rli->keymap);
   return rli;
}

static int sc_begin_edit (char *prompt, char *str, int use_prompt)
{
   int i;
   if (Sc_Keymap_RLI == NULL) 
     {
	Sc_Keymap_RLI = sc_init_readline ();
     }
   
   Sc_Keymap_RLI->edit_width = Sc_Indicator_Start_Column;
   Sc_Keymap_RLI->prompt = prompt;
   *Sc_Keymap_RLI->buf = 0;
   if (str != NULL) 
     {
	strcpy ((char *) Sc_Keymap_RLI->buf, str);
	Sc_Keymap_RLI->point = strlen (str);
     }
   
   i = SLang_read_line (Sc_Keymap_RLI);
   
   if ((i >= 0) && !SLang_Error && !SLKeyBoard_Quit)
     {
	SLang_rline_save_line (Sc_Keymap_RLI);
	*line = 0;
	if (use_prompt)	strcpy (line, prompt);
	
	strcpy (line + strlen(line), (char *) Sc_Keymap_RLI->buf);
	linelim = 0;
	(void) yyparse ();
     }
   linelim = -1;
   if (SLKeyBoard_Quit) i = -1;
   SLang_Error = SLKeyBoard_Quit = 0;
   return i;
}


static void sc_next_cell (void)
{
   int dr = 0, dc = 0;
   switch (Direction_Set)
     {
      default:
	if (Sc_Current_Mode == SC_ROW_MODE) dr = 1;
	else if (Sc_Current_Mode == SC_COLUMN_MODE) dc = 1;
	break;
	
      case 1: dr = 1; break;
      case -1: dr = -1; break;
      case -2: dc = -1; break;
      case 2: dc = 1; break;
     }
   forwcol (dc);
   forwrow (dr);
}

static void sc_edit_cell (void)
{
   struct ent *p;
   
   p = lookat (currow, curcol);
   if (p == NULL) return;
   
   if ((p->flags & IS_STREXPR) 
       || (p->label != NULL)) edits (currow, curcol);
   else editv (currow, curcol);
   /* the above calls simply fill out the 'line' buffer for the expression. */
   
   sc_begin_edit ("Edit> ", line, 0);
}

   
	
static void sc_define_cell (void)
{
   char this_line[80];
   
   (void) sprintf (this_line,"let %s = ",
		  sc_v_name(currow, curcol));
   sc_begin_edit (this_line, NULL, 1);
   sc_next_cell ();
}

static void sc_left_string (void)
{
   char the_line[80];
   
   (void) sprintf (the_line,"leftstring %s = \"",
		   sc_v_name(currow, curcol));
   sc_begin_edit (the_line, NULL, 1);
   
   sc_next_cell ();   
}

static void sc_get_file (void)
{
   if (*curfile)
     sc_message ("Default file is \"%s\"",curfile);
   sc_begin_edit ("get [\"source\"] \"", NULL, 1);
}

static void sc_right_string (void)
{
   char the_line[80];
   
   (void) sprintf(the_line,"rightstring %s = \"",
		  sc_v_name(currow, curcol));
   sc_begin_edit (the_line, NULL, 1);
   sc_next_cell ();
}

static void sc_center_string (void)
{
   char the_line[80];
   
   (void) sprintf (the_line,"label %s = \"",
		  sc_v_name(currow, curcol));
   sc_begin_edit (the_line, NULL, 1);
   sc_next_cell ();
}

static void sc_insert_cmd (void)
{
   if (Sc_Current_Mode == SC_ROW_MODE)
     {
	insertrow (Prefix_Arg);
     }
   else if (Sc_Current_Mode == SC_COLUMN_MODE)
     {
	insertcol (Prefix_Arg);
     }
   else 
     {
	switch (sc_get_response ("Insert row or column? (Press 'r' or 'c')", "rc"))
	  {
	   default:
	     return;
	     
	   case 'r':
	     insertrow (Prefix_Arg);
	     return;
	   case 'c':
	     insertcol (Prefix_Arg);
	     return;
	  }
     }
}


static void sc_self_insert_alpha (void)
{
   SLang_ungetkey ((unsigned char) SLang_Last_Key_Char);
   sc_right_string ();
}

static void sc_self_insert_number (void)
{
   SLang_ungetkey ((unsigned char) SLang_Last_Key_Char);
   sc_define_cell ();
}

static void sc_kbd_quit (void)
{
   Sc_Mark_Set = 0;
   slsc_error ("Quit!");
}

static void sc_slash_cmd (void);
static void sc_copy_region (void)
{
   sc_delete_region ();
   if (SLang_Error) return;
   sc_paste ();
   sc_message ("Region copied to pastebuffer.");
}



#define A_KEY(s, f)  {s, (int (*)(void)) f}

static SLKeymap_Function_Type Sc_Main_Functions [] =
{
   A_KEY("bol", sc_bol),
   A_KEY("eol", sc_eol),
   A_KEY("suspend", sc_suspend),
   A_KEY("insert_char", sc_self_insert_alpha),
   A_KEY("insert_number", sc_self_insert_number),
   A_KEY("left_string", sc_left_string),
   A_KEY("right_string", sc_right_string),
   A_KEY("center_string", sc_center_string),
   A_KEY("up", sc_up_cmd),
   A_KEY("right", sc_right_cmd),
   A_KEY("left", sc_left_cmd),
   A_KEY("down", sc_down_cmd),
   A_KEY("exit", sc_exit_cmd),
   A_KEY("define_cell", sc_define_cell),
   A_KEY("find_file", sc_get_file),
   A_KEY("paste", sc_paste),
   A_KEY("write_file", sc_write_menu_cmd),
   A_KEY("copy_region", sc_copy_region),
   A_KEY("delete_region", sc_delete_region),
   A_KEY("redraw", sc_redraw),
   A_KEY("set_mark",sc_mark_cell),
   {(char *) NULL, NULL}
};

SLKeyMap_List_Type *Sc_Main_Keymap;

int sc_setkey (int argc, SLcmd_Cmd_Table_Type *table)
{
   return SLang_define_key (table->string_args[2], table->string_args[1], Sc_Main_Keymap);
}

int sc_unsetkey (int argc, SLcmd_Cmd_Table_Type *table)
{
   SLang_undefine_key (table->string_args[1], Sc_Main_Keymap);
   return 0;
}


void sc_init_keymaps (void)
{
   char  *err = "Unable to create keymaps!";
   char single[2];
   int i;
   
   single[1] = 0;
   
   if (NULL == (Sc_Main_Keymap = SLang_create_keymap ("Sc_Main", NULL)))
     sc_exit_error (err);
   
   Sc_Main_Keymap->functions = Sc_Main_Functions;
   
   /* define alphabetic chars to right justify */
   for (i = 'A'; i <= 'Z'; i++)
     {
	single[0] = (char) i;
	SLkm_define_key (single, (FVOID_STAR) sc_self_insert_alpha, Sc_Main_Keymap);
	single[0] = (char) i + 32;
	SLkm_define_key (single, (FVOID_STAR) sc_self_insert_alpha, Sc_Main_Keymap);
     }
   
   /* define numeric chars to insert as numbers */
   for (i = '0'; i <= '9'; i++)
     {
	single[0] = (char) i;
	SLkm_define_key (single, (FVOID_STAR) sc_self_insert_number, Sc_Main_Keymap);
     }
   
   
   SLkm_define_key ("\033[A", (FVOID_STAR) sc_up_cmd, Sc_Main_Keymap);
   SLkm_define_key ("\033[B", (FVOID_STAR) sc_down_cmd, Sc_Main_Keymap);
   SLkm_define_key ("\033[C", (FVOID_STAR) sc_right_cmd, Sc_Main_Keymap);
   SLkm_define_key ("\033[D", (FVOID_STAR) sc_left_cmd, Sc_Main_Keymap);
   SLkm_define_key ("=", (FVOID_STAR) sc_define_cell, Sc_Main_Keymap);
   SLkm_define_key ("<", (FVOID_STAR) sc_left_string, Sc_Main_Keymap);
   SLkm_define_key ("/", (FVOID_STAR) sc_slash_cmd, Sc_Main_Keymap);
   SLkm_define_key (">", (FVOID_STAR) sc_right_string, Sc_Main_Keymap);
   SLkm_define_key ("\"", (FVOID_STAR) sc_center_string, Sc_Main_Keymap);
   SLkm_define_key ("^Z", (FVOID_STAR) sc_suspend, Sc_Main_Keymap);
   SLkm_define_key ("^B", (FVOID_STAR) sc_bol, Sc_Main_Keymap);
   SLkm_define_key ("^E", (FVOID_STAR) sc_eol, Sc_Main_Keymap);
   SLkm_define_key ("^G", (FVOID_STAR) sc_kbd_quit, Sc_Main_Keymap);
   SLkm_define_key ("^R", (FVOID_STAR) sc_redraw, Sc_Main_Keymap);
   SLkm_define_key ("\r", (FVOID_STAR) sc_next_cell, Sc_Main_Keymap);
   SLkm_define_key ("\t", (FVOID_STAR) sc_edit_cell, Sc_Main_Keymap);
   SLkm_define_key ("^?", (FVOID_STAR) sc_delete_cell, Sc_Main_Keymap);
   SLkm_define_key ("^K\033[A", (FVOID_STAR) sc_bob, Sc_Main_Keymap);
   SLkm_define_key ("^K\033[B", (FVOID_STAR) sc_eob, Sc_Main_Keymap);
   SLkm_define_key ("^K^W", (FVOID_STAR) sc_write_menu_cmd, Sc_Main_Keymap);
   SLkm_define_key ("^Kg", (FVOID_STAR) sc_get_file, Sc_Main_Keymap);
   SLkm_define_key ("^Ke", (FVOID_STAR) sc_exit_cmd, Sc_Main_Keymap);
   SLkm_define_key ("?", (FVOID_STAR) sc_help, Sc_Main_Keymap);
   SLkm_define_key ("^K\033[C", (FVOID_STAR) sc_pageright, Sc_Main_Keymap);
   SLkm_define_key ("^K\033[D", (FVOID_STAR) sc_pageleft, Sc_Main_Keymap);
   SLkm_define_key ("^U", (FVOID_STAR) sc_pageup, Sc_Main_Keymap);
   SLkm_define_key ("\033[5~", (FVOID_STAR) sc_pageup, Sc_Main_Keymap);
   SLkm_define_key ("^D", (FVOID_STAR) sc_pagedown, Sc_Main_Keymap);
   SLkm_define_key ("@", (FVOID_STAR) sc_eval_cmd, Sc_Main_Keymap);
   SLkm_define_key ("\033[6~", (FVOID_STAR) sc_pagedown, Sc_Main_Keymap);
   SLkm_define_key ("^K^B", (FVOID_STAR) sc_mark_cell, Sc_Main_Keymap);
   SLkm_define_key ("^K^P", (FVOID_STAR) sc_paste, Sc_Main_Keymap);
   SLkm_define_key ("\033e", (FVOID_STAR) sc_edit_cell, Sc_Main_Keymap);
   SLkm_define_key ("^V", (FVOID_STAR) sc_delete_cell, Sc_Main_Keymap);
   SLkm_define_key ("^Kh", (FVOID_STAR) sc_hide_cmd, Sc_Main_Keymap);
   SLkm_define_key ("^Ks", (FVOID_STAR) sc_show_cmd, Sc_Main_Keymap);
   SLkm_define_key ("^K^L", (FVOID_STAR) sc_dup, Sc_Main_Keymap);
   SLkm_define_key ("^L", (FVOID_STAR) sc_del, Sc_Main_Keymap);
   SLkm_define_key ("^K^V", (FVOID_STAR) sc_delete_region, Sc_Main_Keymap);
   SLkm_define_key ("^Kt", (FVOID_STAR) sc_set_mode, Sc_Main_Keymap);
}


void sc_dokey (void)
{
   SLang_Key_Type *key;
   
   key = SLang_do_key (Sc_Main_Keymap, (int (*)(void)) SLang_getkey);
   SLang_Error = SLKeyBoard_Quit = 0;
   
   /* Last_Char = SLang_Last_Key_Char; */
   sc_clear_message ();

   if ((key == NULL) || (key->f.f == NULL) || (key->type != SLKEY_F_INTRINSIC))
     {
	SLtt_beep ();
     }
   else (((void (*)(void))(key->f.f)) ());
}



/* ---------------------------------------------------------------------- */

typedef struct Menu_Type 
{
   char *name;
   char *hlp;
   void (*action)(void);
} Menu_Type;


void sc_do_menu (Menu_Type *, int, int);

char *Sc_Printer_String;

static void sc_print (void)
{
   if (Sc_Printer_String == NULL) 
     {
	slsc_error ("Printer not set!");
	return;
     }
   /* Stupid sc prints from 0 -> maxrow/col which might be a terrible
    * waste of space.  Lets fix that here.
    */
   
   sc_adjust_maxrow_col ();
   
   sprintf (line, "write \"%s\"", Sc_Printer_String);
   linelim = 0;
   yyparse ();
}

static void set_option (int opt)
{
   switch (opt)
     {
      case 'r':
	Sc_Current_Mode = SC_ROW_MODE;
	break;
      case 'b':
	Sc_Current_Mode = SC_BLOCK_MODE;
	break;
      case 'c':
	Sc_Current_Mode = SC_COLUMN_MODE;
	break;
      case 'e':
	Sc_Show_Exprs = !Sc_Show_Exprs;
	break;
     }
}

static void sc_write_native (void)
{
   if (*curfile) sc_message ("Default path is \"%s\"",curfile);
   sc_begin_edit ("put [\"dest\" range] \"", NULL, 1);
}

static void sc_write_printable (void)
{
   sc_begin_edit ("write [\"dest\" range] \"", NULL, 1);
}

static void sc_write_table_cmd (void)
{
   char *msg;
   switch (sc_get_response ("Table Format: Tex, Latex, tBl, or Frame?", "tblf"))
     {
      case 'b': tbl_style = TBL; msg = "TBL"; break;
      case 't': tbl_style = TEX; msg = "TEX"; break;
      case 'l': tbl_style = LATEX; msg = "LATEX"; break;
      case 'f': tbl_style = FRAME; msg = "FRAME"; break;
      default:
	return;
     }
   
   sc_message ("Using Table Format %s.", msg);
   sc_begin_edit ("tbl [\"dest\" range] \"", NULL, 1);
}

static Menu_Type Write_Menu [] =
{
     {
	"Spreadsheet",
	"Save to file in SLSC native format",
	sc_write_native
     },
     {
	"Printable",
	"Save to file in text format",
	sc_write_printable
     },
     {
	"Table",
	"TeX, LaTeX, and Frame formats",
	sc_write_table_cmd
     },
     {
	NULL, NULL, NULL
     }
};

static int sc_write_menu_cmd (void)
{
   sc_do_menu (Write_Menu, 0, 0);
   FullUpdate = 1;
   return 0;
}

   
static Menu_Type File_Menu [] =
{
     {
	"Save",
	"Save spreadsheet in various formats.",
	(void (*)(void)) sc_write_menu_cmd
     },
     {
	"Open",
	"Begin a new spreadsheet file.",
	sc_get_file
     },
     {
	"Print",
	"Send spreadsheet to the printer.",
	sc_print
     },
     {
	NULL, NULL, NULL
     }
};

   
	
   
static Menu_Type Toggle_Menu[] =
{
     {
	"Row",
	"Select ROW mode as the current mode.",
	(void (*)(void)) set_option
     },
     {
	"Column",
	"Select COLUMN mode as the current mode.",
	(void (*)(void)) set_option
     },
     {
	"Block",
	"Select BLOCK mode as the current mode.",
	(void (*)(void)) set_option
     },
     {
	"Expressions",
	"Toggle Display of expressions/values.",
	(void (*)(void)) set_option
     },
     {
	NULL, NULL, NULL
     }
};

Menu_Type Region_Menu [] =
{
     {
	"Copy",
	"Copy Region to pastebuffer.",
	sc_copy_region
     },
     {
	"Kill",
	"Kill region to pastebuffer.",
	sc_delete_region
     },
     {
	"Paste",
	"Insert Pastebuffer (Modal).",
	sc_paste
     },
     {
	"Insert",
	"Insert new rows or columns. (Modal)",
	sc_insert_cmd
     },
     {
	"Hide",
	"Hide region.  Use show to unhide it.",
	sc_hide_cmd
     },
     {
	"Show",
	"Show previously hidden rows or columns",
	sc_show_cmd
     },
     {
	NULL, NULL, NULL
     }
};


static void sc_toggle_menu (void)
{
   sc_do_menu (Toggle_Menu, 0, 1);
   FullUpdate = 1;
}

static void sc_region_menu (void)
{
   sc_do_menu (Region_Menu, 0, 0);
   FullUpdate = 1;
}

static void sc_file_menu_cmd (void)
{
   sc_do_menu (File_Menu, 0, 0);
   FullUpdate = 1;
}


Menu_Type Root_Menu[] = 
{
     {
	"File",
	"Save, Print, Get new spreadsheet, etc...",
	sc_file_menu_cmd
     },
     {
	"Edit",
	"Edit current cell.",
	sc_edit_cell
     },   
     {
	"Toggle",
	"Toggle modes (row, column, block), display, etc...",
	sc_toggle_menu
     },
     {
	"Mark",
	"Begin region definition.",
	sc_mark_cell
     },
     {
	"Region",
	"Region commands: paste, copy, kill, hide, etc...",
	sc_region_menu
     },
     {
	"Quit", 
	"Quit possibly saving changes first",
	sc_exit_cmd
     },
     {
	NULL, NULL, NULL
     }
};



void sc_do_menu (Menu_Type *menu, int field, int send_char)
{
   int n, max_n, i;
   char *hlp, ch;
   SLang_Key_Type *key;
   char options[30];
   void (*fvv)(void);
   void (*fvi)(int);
   static int menu_aborted;
   
   max_n = 0;
   
   top_level:
   
   while (1)
     {
	menu_aborted = 0;
	if ((SLang_Error) || SLKeyBoard_Quit) return;
	
	hlp = NULL;
	n = 0;
	SLsmg_gotorc (0, 0);
	SLsmg_erase_eol ();
	SLsmg_normal_video ();
	SLsmg_write_string ("Menu: ");
	
	while (menu[n].name != NULL)
	  {	     
	     if (field == n) 
	       {
		  SLsmg_write_char ('>');
		  SLsmg_set_color (SC_MENU_COLOR);
		  hlp = menu[n].hlp;
	       }
	     else SLsmg_write_char (' ');
	     options[n] = *menu[n].name | 0x20;
	     SLsmg_write_string (menu[n].name);
	     SLsmg_normal_video ();
	     
	     if (field == n) SLsmg_write_char ('<');
	     else SLsmg_write_char (' ');
	     n++;
	  }
	max_n = n;
	
	sc_update_indicators ();
	sc_message (hlp);
	SLsmg_refresh ();
	sc_clear_message ();
	SLsmg_gotorc (0, 0); SLsmg_erase_eol ();
	
	while (1)
	  {
	     key = SLang_do_key (Sc_Main_Keymap, (int (*)(void)) SLang_getkey);
	     if (key != NULL) break;
	     SLtt_beep ();
	     fflush (stdout);
	  }
	
	if ((void (*)(void)) key->f.f == sc_right_cmd)
	  {
	     field++;
	  }
	else if ((void (*)(void)) key->f.f == sc_left_cmd)
	  {
	     field--;
	  }
	else if ((SLang_Error) || SLKeyBoard_Quit) return;
	else 
	  {
	     
	     ch = SLang_Last_Key_Char;
	     if (ch == ' ') 
	       {
		  menu_aborted = 1;
		  return;
	       }
	     
	     if (ch == '\r')
	       {
		  fvv = menu[field].action;
		  fvi = (void (*)(int)) fvv;
		  if (send_char == 0) (*fvv)();
		  else (*fvi) ((int) options[field]);
		  
		  if (menu_aborted) goto top_level;
		  return;
	       }
	     
	     ch |= 0x20;
	     
	     i = 0; while (i < max_n)
	       {
		  if (ch == options[i])
		    {
		       fvv = menu[i].action;
		       fvi = (void (*)(int)) fvv;
		       if (send_char == 0) (*fvv)();
		       else (*fvi) ((int) ch);
		       
		       if (menu_aborted) goto top_level;
		       
		       return;
		    }
		  i++;
	       }
	     SLtt_beep ();
	  }
	     
	if (field < 0) field = max_n - 1;
	if (field == max_n)  field = 0;
     }
}

   
static void sc_slash_cmd (void)
{
   sc_do_menu (Root_Menu, 0, 0);
   FullUpdate = 1;
}
