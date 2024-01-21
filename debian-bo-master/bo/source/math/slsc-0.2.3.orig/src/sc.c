#define SC_VERSION_STRING "slsc 0.2.3,  Press '?' for help, '/' for menus."
/*	SC	A Spreadsheet Calculator
 *		Main driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *
 */


#include <signal.h>
#include <sys/ioctl.h>

#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <termios.h>

#ifdef M_UNIX
# include <sys/stream.h>	/* for ptem.h */
# include <sys/ptem.h>	/* for struct winsize */
#endif
 

#include "sc.h"
#include "cmds.h"
#include "interp.h"
#include "lex.h"
#include "range.h"
#include "slang.h"
#include "scdokey.h"

SLKeyMap_List_Type *Sc_Main_Keymap;

#ifndef DFLT_PAGER
#define	DFLT_PAGER "more"	/* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160	/* for ! command below */

/* Globals defined in sc.h */

double Sc_Epsilon = 1.0e-8;	       /* used for comparing doubles */

struct ent *tbl[MAXROWS][MAXCOLS];
static int Start_Row, Start_Col;
int currow, curcol;
int savedrow, savedcol;
int FullUpdate;
int maxrow, maxcol;
int Sc_Col_Width[MAXCOLS];
int precision[MAXCOLS];
char col_hidden[MAXCOLS];
char row_hidden[MAXROWS];
char line[1000];
int Sc_Changed;

struct ent *Sc_Row_Pastebuffer;
struct ent *Sc_Col_Pastebuffer;
struct ent *Sc_Block_Pastebuffer;

int modflg;
int numeric;
char *mdir;
int showsc, showsr;	/* Starting cell for highlighted range */

char curfile[1024];

int  linelim = -1;

int  showtop   = 1;	/* Causes current cell value display in top line  */
int  Highlight_Cell  = 1;	/* Causes current cell to be highlighted	  */

static int   showneed  = 0;	/* Causes cells needing values to be highlighted  */
int  Sc_Show_Exprs  = 0;	/* Causes cell exprs to be displayed, highlighted */

int  autocalc = 1 ;	/* 1 to calculate after each update */
int  calc_order = BYROWS;
int  tbl_style = 0;	/* headers for T command output */

static int  Last_Cell_Row = -1, Last_Cell_Col = -1;

int Sc_Indicator_Start_Column = 65;



/* static functions */
static void signals(void);




static int Sc_Error_Seen;

void yyerror (char *err)
{
   if (loading)
     {
	slsc_error ("%s: line %d: %s", curfile, Sc_File_Line_Number, err);
     }
   else slsc_error (err);
}

struct ent *lookat(int row, int col)
{
    register struct ent **p;
    if (row < 0)
	row = 0;
    else if (row > MAXROWS-1) 
	row = MAXROWS-1;
    if (col < 0) 
	col = 0;
    else if (col > MAXCOLS-1)
	col = MAXCOLS-1;
    p = &tbl[row][col];
    if (*p==0) {
	*p = (struct ent *) SLMALLOC ((unsigned)sizeof (struct ent));
	if (row>maxrow) maxrow = row;
	if (col>maxcol) maxcol = col;
	(*p)->label = 0;
	(*p)->flags = 0;
	(*p)->row = row;
	(*p)->col = col;
	(*p)->expr = 0;
	(*p)->v = (double) 0.0;
    }
    return *p;
}




static void suspend_sc (int sig)
{
   sc_reset_display ();
   (void) kill(getpid(),SIGTSTP);
   
   sc_init_display ();
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */

void sc_free_ent (register struct ent *p)
{
   if (Sc_Current_Mode == SC_ROW_MODE) 
     {
	p->next = Sc_Row_Pastebuffer;
	Sc_Row_Pastebuffer = p;
     }
   else if (Sc_Current_Mode == SC_COLUMN_MODE) 
     {
	p->next = Sc_Col_Pastebuffer;
	Sc_Col_Pastebuffer = p;
     }
   else
     {
	p->next = Sc_Block_Pastebuffer;
	Sc_Block_Pastebuffer = p;
     }
   p->flags |= IS_DELETED;
}

void sc_flush_saved (void)
{
    register struct ent *p;
    register struct ent *q;
    register struct ent **save;

   
   if (Sc_Current_Mode == SC_ROW_MODE) 
     {
	p = Sc_Row_Pastebuffer;
	save = &Sc_Row_Pastebuffer;
     }
   else if (Sc_Current_Mode == SC_COLUMN_MODE) 
     {
	p = Sc_Col_Pastebuffer;
	save = &Sc_Col_Pastebuffer;
     }
   else
     {
	p = Sc_Block_Pastebuffer;
	save = &Sc_Block_Pastebuffer;
     }

    if (p == NULL) return;
   
   while (p != NULL)
     {
	(void) sc_clearent(p);
	q = p->next;
	SLFREE((char *)p);
	p = q;
     }
   *save = NULL;
}

void sc_update_indicators (void)
{
   SLsmg_gotorc (0, Sc_Indicator_Start_Column);
   SLsmg_set_color (SC_MODE_COLOR);
   SLsmg_write_string (" Mode: ");
   SLsmg_write_string (Sc_Modes[Sc_Current_Mode]);
   SLsmg_erase_eol ();
   SLsmg_gotorc (2,0);
   SLsmg_set_color (SC_LABEL_COLOR);
   if (modflg) 
     {
	SLsmg_write_string ("-**-");
     }
   else	SLsmg_write_string ("----");

   SLsmg_normal_video ();
}


void sc_update (int show_version)
{
   register int   row, col;
   register struct ent **p;
   int     mxcol;
   int     mxrow;
   int     rows = 0;
   int     cols = 0;
   int     minsr = 0, minsc = 0, maxsr = 0, maxsc = 0;
   int cursor_row, cursor_col;
   register r;
   register i;

   /* You can't hide the last row or col */
   while (row_hidden[currow])  
     currow++;
   while (col_hidden[curcol])
     curcol++;
   /* First see if the last display still covers curcol */
   if (Start_Col <= curcol) 
     {
	for (i = Start_Col, cols = 0, col = RESCOL;
	     (col + Sc_Col_Width[i]) < SLtt_Screen_Cols-1 && i < MAXCOLS; i++) 
	  {
	     cols++;
	     if (col_hidden[i])
	       continue;
	     col += Sc_Col_Width[i];
	  }
     }

   while ((Start_Col + cols - 1 < curcol) || (curcol < Start_Col)) 
     {
	FullUpdate++;
	if (Start_Col - 1 == curcol) 
	  {    /* How about back one? */
	     Start_Col--;
	  } 
	else if (Start_Col + cols == curcol) 
	  {   /* Forward one? */
	     Start_Col++;
	  } 
	else 
	  {
	     /* Try to put the cursor in the center of the screen */
	     col = (SLtt_Screen_Cols - RESCOL - Sc_Col_Width[curcol]) / 2 + RESCOL; 
	     Start_Col = curcol;
	     for (i=curcol-1; i >= 0 && col-Sc_Col_Width[i] > RESCOL; i--) 
	       {
		  Start_Col--;
		  if (col_hidden[i])
		    continue;
		  col -= Sc_Col_Width[i];
	       }
	  }
	/* Now pick up the counts again */
	for (i = Start_Col, cols = 0, col = RESCOL;
	     (col + Sc_Col_Width[i]) < SLtt_Screen_Cols-1 && i < MAXCOLS; i++) 
	  {
	     cols++;
	     if (col_hidden[i])
	       continue;
	     col += Sc_Col_Width[i];
	  }
     }
   /* Now - same process on the rows */
   if (Start_Row <= currow) 
     {
	for (i = Start_Row, rows = 0, row=RESROW; (row < SLtt_Screen_Rows) && (i < MAXROWS); i++)
	  {
	     rows++;
	     if (row_hidden[i])
	       continue;
	     row++;
	  }
     }

   while (Start_Row + rows - 1 < currow || currow < Start_Row) 
     {
	FullUpdate++;
	if (Start_Row - 1 == currow) 
	  {    /* How about up one? */
	     Start_Row--;
	  } 
	else if (Start_Row + rows == currow) 
	  {   /* Down one? */
	     Start_Row++;
	  } 
	else 
	  {
	     /* Try to put the cursor in the center of the screen */
	     row = (SLtt_Screen_Rows - RESROW) / 2 + RESROW; 
	     Start_Row = currow;
	     for (i=currow-1; (i >= 0) && (row - 1 > RESROW); i--) 
	       {
		  Start_Row--;
		  if (row_hidden[i])
		    continue;
		  row--;
	       }
	  }
	/* Now pick up the counts again */
	for (i = Start_Row, rows = 0, row = RESROW; 
	     (row < SLtt_Screen_Rows) && (i < MAXROWS); i++) 
	  {
	     rows++;
	     if (row_hidden[i])
	       continue;
	     row++;
	  }
     }
   
   mxcol = Start_Col + cols - 1;
   mxrow = Start_Row + rows - 1;
   
   if (FullUpdate)		       /* print labels */
     {
	(void) SLsmg_gotorc (2, 0);
	SLsmg_erase_eos ();
	SLsmg_set_color (SC_LABEL_COLOR);
	row = RESROW;
	for (i = Start_Row; i <= mxrow; i++) 
	  {
	     if (row_hidden[i]) continue;
	     
	     SLsmg_gotorc (row, 0);
#if MAXROW < 1000
	     SLsmg_printf ("%-*d", RESCOL - 1, i);
#else
	     SLsmg_printf ("%-*d", RESCOL, i);
#endif
	     row++;
	  }
	
	SLsmg_gotorc (2, 0);
	SLsmg_printf("%*s", RESCOL, " ");
	
	col = RESCOL;
	for (i = Start_Col; i <= mxcol; i++)
	  {
	     register int k;
	     if (col_hidden[i]) continue;
	     SLsmg_gotorc (2, col);
	     k = Sc_Col_Width[i] / 2;
	     
	     if (k == 0)
	       SLsmg_printf("%1s", sc_coltoa(i));
	     else
	       (void) SLsmg_printf("%*s%-*s", k, " ", Sc_Col_Width[i] - k, sc_coltoa(i));
	     
	     col += Sc_Col_Width[i];
	  }
	
     }
   SLsmg_normal_video ();
   
   if (Sc_Mark_Set) 
     {
	sc_get_region (&minsr, &minsc, &maxsr, &maxsc);
	
	maxsr += minsr;
	maxsc += minsc;
	
	if (Sc_Current_Mode == SC_ROW_MODE)
	  {
	     minsc = 0;
	     maxsc = MAXCOLS - 1;
	  }
	else if (Sc_Current_Mode == SC_COLUMN_MODE)
	  {
	     minsr = 0;
	     maxsr = MAXROWS - 1;
	  }
	
	
	if (showtop) 
	  {
	     SLsmg_gotorc (1, 0);
	     SLsmg_erase_eol ();
	     SLsmg_printf("Default range:  %s",
			   sc_r_name (minsr, minsc, maxsr, maxsc));
	  }
	FullUpdate = 1;
     }
   
   /* Repaint the visible screen */
   for (row = Start_Row, r = RESROW; row <= mxrow; row++) 
     {
	register c = RESCOL;
	int do_stand = 0;
	int fieldlen;
	int nextcol;
	
	if (row_hidden[row]) continue;
	
	if (Last_Cell_Row == row)
	  {
	     SLsmg_gotorc (r, RESCOL - 1);
	     SLsmg_erase_eol ();
	  }
	

	col = Start_Col;
	for (p = &tbl[row][Start_Col]; col <= mxcol;
	         p += nextcol - col,  col = nextcol, c += fieldlen) 
	  {

	     nextcol = col + 1;
	     if (col_hidden[col]) 
	       {
		  fieldlen = 0;
		  continue;
	       }

	     fieldlen = Sc_Col_Width[col];
	     
	     /*
	      * Set standout if:
	      *
	      * - showing ranges, and not showing cells which need to be filled
	      *   in, and not showing cell expressions, and in a range, OR
	      *
	      * - if showing cells which need to be filled in and this one is
	      *   of that type (has a value and does not have an expression, or
	      *   it is a string expression), OR
              *
	      * - if showing cells which have expressions and this one does.
	      */

	     if (Sc_Mark_Set && (! showneed) && (! Sc_Show_Exprs)
		  && (row >= minsr) && (row <= maxsr)
		  && (col >= minsc) && (col <= maxsc))
	       {
		  do_stand = SC_REGION_COLOR;
	       }
	     else if (showneed && (*p) && ((*p)->flags & IS_VALID)
		       && (((*p)->flags & IS_STREXPR) || !((*p)->expr)))
	       {
		  do_stand = SC_NEEDS_COLOR;
	       }
	     else if (Sc_Show_Exprs && (*p) && ((*p)->expr))
	       {
		  do_stand = SC_EXPR_COLOR;
	       }
	     else
	       do_stand = 0;
	     
	     if ((*p && ((*p)->flags & IS_CHANGED))
		 || FullUpdate
		 || do_stand
		 || (Last_Cell_Row == row))
	       {
		  
		  (void) SLsmg_gotorc (r, c);
		  if (*p == NULL) *p = lookat(row, col);
		  
		  if (do_stand) 
		    {
		       (void) SLsmg_set_color (do_stand);
		       (*p) -> flags |= IS_CHANGED; 
		    } 
		  else 
		    {
		       (*p) -> flags &= ~IS_CHANGED;
		    }

		  /*
		   * Show expression; takes priority over other displays:
		   */

		  if (Sc_Show_Exprs && ((*p) -> expr))
		    {
		       linelim = 0;
		       editexp (row, col);		/* set line to expr */
		       linelim = -1;
		       showstring (line, 
				   /* leftflush = */ 1, 
				   /* hasvalue = */ 0,
				   row, col, & nextcol, mxcol, 
				   &fieldlen, r, c);
		    }
		  else 
		    {
		       /*
			* Show cell's numeric value:
                        */

		       if ((*p) -> flags & IS_VALID) 
			 {
			    char field[1024];
			    if (!do_stand) 
			      {
				 if ((*p) -> expr) do_stand = SC_FORMULA_COLOR;
				 else do_stand = SC_NUMBER_COLOR;
			      }
			    
			    SLsmg_set_color (do_stand);
			    (void)sprintf(field,"%*.*f", Sc_Col_Width[col], precision[col], (*p)->v);
			    if(strlen(field) > Sc_Col_Width[col]) 
			      {
				 for (i = 0; i < Sc_Col_Width[col]; i++) 
				   SLsmg_write_char('*');
			      } 
			    else 
			      {
				 (void)SLsmg_write_string(field);
			      }
			 }

		       /*
			* Show cell's label string:
			*/

		       if ((*p) -> label) 
			 {
			    if (!do_stand) do_stand = SC_STRING_COLOR;
			    SLsmg_set_color (do_stand);
			    showstring ((*p) -> label,
					(*p) -> flags & IS_LEFTFLUSH,
					(*p) -> flags & IS_VALID,
					row, col, & nextcol, mxcol,
					& fieldlen, r, c);
			 }

		       /*
			* repaint a blank cell:
			*/

		       if (!((*p)->flags & IS_VALID) && !(*p)->label) 
			 {
			    (void) SLsmg_printf ("%*s", Sc_Col_Width[col], " ");
			 }
		    } /* else */

		  if (do_stand) 
		    {
		       (void) SLsmg_normal_video ();
		       do_stand = 0;
		    }
	       }
	  }
	r++;
     }

   Last_Cell_Row = currow; Last_Cell_Col = curcol;
   
   /* Now show cursor.  This is accomplished by reading what is in the cell
    * and simply writing it out again with the proper attributes.
    */
   
   cursor_row = RESROW;
   for (row = Start_Row; row < currow; row++)
     {
	if (!row_hidden[row]) cursor_row++;
     }
   cursor_col = RESCOL;
   for (col = Start_Col; col < curcol; col++)
     {
	if (!col_hidden[col]) cursor_col += Sc_Col_Width[col];
     }
   
   SLsmg_set_color (SC_CELL_COLOR);
   SLsmg_gotorc (cursor_row, cursor_col);
   col = Sc_Col_Width [curcol];
   while (col-- > 0)
     {
	SLsmg_write_char (SLsmg_char_at () & 0xFF);
     }
   SLsmg_normal_video ();

   
   (void) SLsmg_gotorc (0, 0);
   (void) SLsmg_erase_eol ();

   if (show_version)
     {
	SLsmg_write_string (SC_VERSION_STRING);
     }
   else if (showtop) 
     {			/* show top line */
	register struct ent *p1;
	int printed = 0;		/* printed something? */
	
	(void) SLsmg_printf ("%s%d ", sc_coltoa (curcol), currow);
	
	if (0 != (p1 = tbl [currow] [curcol]))
	  {
	     if (p1 -> expr)		/* has expr of some type */
	       {
		  linelim = 0;
		  editexp (currow, curcol);	/* set line to expr */
		  linelim = -1;
	       }
	     
	     /*
	      * Display string part of cell:
	      */
	     
	     if ((p1 -> expr) && (p1 -> flags & IS_STREXPR))
	       {
		  (void) SLsmg_write_string ((p1 -> flags & IS_LEFTFLUSH) ? "<{" : ">{");
		  (void) SLsmg_write_string (line);
		  (void) SLsmg_write_string ("} ");	/* and this '}' is for vi % */
		  printed = 1;
	       }
	     else if (p1 -> label)		/* has constant label only */
	       {
		  (void) SLsmg_write_string ((p1 -> flags & IS_LEFTFLUSH) ? "<\"" : ">\"");
		  (void) SLsmg_write_string (p1 -> label);
		  (void) SLsmg_write_string ("\" ");
		  printed = 1;
	       }
	     
	     /*
	      * Display value part of cell:
	      */
	     
	     if (p1 -> flags & IS_VALID)	/* has value or num expr */
	       {
		  if ((! (p1 -> expr)) || (p1 -> flags & IS_STREXPR))
		    (void) sprintf (line, "%.15g", p1 -> v);
		  
		  (void) SLsmg_write_char ('[');
		  (void) SLsmg_write_string (line);
		  (void) SLsmg_write_char (']');
		  printed = 1;
	       }
	  }
	if (! printed) (void) SLsmg_write_string ("[]");
     }
   sc_update_indicators ();
   cursor_col += Sc_Col_Width[Last_Cell_Col];
   (void) SLsmg_gotorc (cursor_row, cursor_col);
   SLsmg_write_char ('<');
   (void) SLsmg_gotorc (cursor_row, cursor_col);
   FullUpdate = 0;
}


char *Progname;

static SLcmd_Cmd_Table_Type Sc_SLCmd_Table;
static SLcmd_Cmd_Table_Type Sc_SLCmd_Set_Table;

static int sc_parse_color (char *obj)
{
   int i;

   if (!strcmp ("normal", obj)) i = 0;
   else if (!strcmp ("label", obj)) i = SC_LABEL_COLOR;
   else if (!strcmp ("number", obj)) i = SC_NUMBER_COLOR;
   else if (!strcmp ("string", obj)) i = SC_STRING_COLOR;
   else if (!strcmp ("needs", obj)) i = SC_NEEDS_COLOR;
   else if (!strcmp ("region", obj)) i = SC_REGION_COLOR;
   else if (!strcmp ("expression", obj)) i = SC_EXPR_COLOR;
   else if (!strcmp ("formula", obj)) i = SC_FORMULA_COLOR;
   else if (!strcmp ("mode", obj)) i = SC_MODE_COLOR;
   else if (!strcmp ("cell", obj)) i = SC_CELL_COLOR;
   else if (!strcmp ("menu", obj)) i = SC_MENU_COLOR;
   else i = -1;
   return i;
}

static int sc_set_color (int argc, SLcmd_Cmd_Table_Type *table)
{
   char *obj = table->string_args[1];
   int i;
   
   i = sc_parse_color (obj);
   if (i >= 0)  SLtt_set_color (i,
				NULL,
				table->string_args[2],
				table->string_args[3]);
   else slsc_error ("set color: %s undefined", obj);
   return 0;
}


static int sc_set_printer (int argc, SLcmd_Cmd_Table_Type *table)
{
   if (Sc_Printer_String != NULL)
     {
	SLFREE (Sc_Printer_String);
     }
   Sc_Printer_String = table->string_args[1];
   table->string_args[1] = NULL;       /* stealing it */
   return 0;
}

static int sc_set_beep (int argc, SLcmd_Cmd_Table_Type *table)
{
   SLtt_Ignore_Beep = table->int_args[1];
   return 0;
}

/* This table is used by the set command */
static SLcmd_Cmd_Type Sc_SLCmd_Set_Cmds[] = 
{
   {sc_set_color, "color", "SSS"},
   {sc_set_color, "mono", "Sss"},
   {sc_set_printer, "printer", "S"},
   {sc_set_beep, "beep", "I"},
   
   {NULL, "", ""}
};

static int sc_set_cmd (int argc, SLcmd_Cmd_Table_Type *table)
{
   return SLcmd_execute_string (table->string_args[1], &Sc_SLCmd_Set_Table);
}

   
   
static SLcmd_Cmd_Type Sc_SLCmd_Cmds[] =
{
   {sc_set_cmd, "set", "V"},
   {sc_setkey, "setkey", "SS"},
   {sc_unsetkey, "unsetkey", "S"},
   {NULL, "", ""}
};


static void init_slcmd_table (void)
{
   Sc_SLCmd_Table.table = Sc_SLCmd_Cmds;
   Sc_SLCmd_Set_Table.table = Sc_SLCmd_Set_Cmds;
}

static void sc_read_init_file (void);

static void slang_sc_error (char *s)
{
   slsc_error (s);
}

char Slsc_Root_Dir[256];


static void slsc_fix_path (char *path)
{
#ifndef VMS
   int len;
   
   len = strlen (path);
   if (!len) return;
   if (path[len - 1] != '/')
     {
	path[len] = '/';
	path[len + 1] = 0;
     }
#endif
}

   
int main (int argc, char **argv)
{
   register int i;
   int first_time;
   char *slsc_root;
   
    /*
     * Keep command line options around until the file is read so the
     * command line overrides file options
     */

    int Mopt = 0;
    int Nopt = 0;
    int Copt = 0; 
    int Ropt = 0;
   
   Progname = argv[0];
   
    while (argc > 1 && argv[1][0] == '-') {
	argv++;
	argc--;
    	switch (argv[0][1]) {
	    case 'x':
#ifdef VMS
		    (void) fprintf(stderr, "Crypt not available for VMS\n");
		    exit(1);
#else 
		    Crypt = 1;
#endif
		    break;
	    case 'm':
		    Mopt = 1;
		    break;
	    case 'n':
		    Nopt = 1;
		    break;
	 case 'C':
	   SLtt_Use_Ansi_Colors = 1;
	   break;
	   
	    case 'c':
		    Copt = 1;
		    break;
	    case 'r':
		    Ropt = 1;
		    break;
	    default:
		    (void) fprintf(stderr,"%s: unrecognized option: \"%c\"\n",
			Progname,argv[0][1]);
		    exit(1);
	}
    }

   for (i = 0; i < MAXCOLS; i++) 
     {
	Sc_Col_Width[i] = DEFWIDTH;
	precision[i] = DEFPREC;
     }
   
   slsc_root = (char *) getenv ("SLSC_ROOT");
   
#ifdef SLSC_ROOT
   if (slsc_root == NULL)
     {
	slsc_root = SLSC_ROOT;
     }
#endif

   if (slsc_root != NULL) 
     {
	strcpy(Slsc_Root_Dir, slsc_root);
	slsc_fix_path (Slsc_Root_Dir);
     }

   signals();   

   SLtt_get_terminfo();
   
   sc_init_display ();
   sc_init_keymaps ();
   
   init_slcmd_table ();
   SLang_Error_Routine = slang_sc_error;
   
   for (i = 0; i < SC_REGION_COLOR; i++)
     SLtt_set_mono (i, NULL, 0);

   for (i = SC_REGION_COLOR; i <= SC_CELL_COLOR; i++)
     SLtt_set_mono (i, NULL, SLTT_REV_MASK);
   
   sc_read_init_file ();
   

    if (argc > 1) 
     {
	(void) strcpy(curfile,argv[1]);
	readfile (argv[1], 0);
     }

   if (Mopt)
     autocalc = 0;
   if (Nopt)
     numeric = 1;
   if (Copt)
     calc_order = BYCOLS;
   if (Ropt)
     calc_order = BYROWS;

   modflg = 0;
   
   FullUpdate++;
   Sc_Changed = 1;
   
   first_time = 1;
   while (1)
     { 
	if (autocalc && Sc_Changed)
	  {
	     sc_eval_all ();
	     Sc_Changed = 0;
	  }
	sc_update(first_time);
	first_time = 0;
	if (SLang_Error || !SLang_input_pending(0)) (void) SLsmg_refresh ();
	SLang_Error = 0;
	Sc_Error_Seen = 0;
	sc_dokey ();
     }				/*  while (inloop) */
}
/*
static void startshow (void)
{
    showrange = 1;
    showsr = currow;
    showsc = curcol;
}

static void showdr (void)
{
    int     minsr, minsc, maxsr, maxsc;

    minsr = showsr < currow ? showsr : currow;
    minsc = showsc < curcol ? showsc : curcol;
    maxsr = showsr > currow ? showsr : currow;
    maxsc = showsc > curcol ? showsc : curcol;
    (void) sprintf (line+linelim,"%s", r_name(minsr, minsc, maxsr, maxsc));
}
*/
void sc_set_order(int i)
{
   if ((i == BYROWS) || (i == BYCOLS))
     calc_order = i;
   else
     slsc_error("Not yet implemented");
}

void sc_set_auto (int i)
{
   autocalc = i;
}


static void sc_set_screen_size (int sig)
{
   int r = 0, c = 0;
   
#ifdef TIOCGWINSZ
   struct winsize wind_struct;
   
   if ((ioctl(1,TIOCGWINSZ,&wind_struct) == 0)
       || (ioctl(0, TIOCGWINSZ, &wind_struct) == 0)
       || (ioctl(2, TIOCGWINSZ, &wind_struct) == 0))
     {
	c = (int) wind_struct.ws_col;
	r = (int) wind_struct.ws_row;
     }

#endif
   
   if ((r <= 0) || (r > 200)) r = 24;
   if ((c <= 0) || (c > 250)) c = 80;
   SLtt_Screen_Rows = r;
   SLtt_Screen_Cols = c;
   
   Sc_Indicator_Start_Column = c - 15;

#ifdef SIGWINCH
   if (sig == SIGWINCH) 
     {
	SLsmg_init_smg ();
	sc_redraw ();
     }
   signal (SIGWINCH, sc_set_screen_size);
#endif
}


void sc_init_display (void)
{
   SLang_init_tty (7, 1, 0);
   SLang_set_abort_signal (NULL);
   sc_set_screen_size (0);
   SLsmg_init_smg ();
   signal(SIGTSTP, suspend_sc);
   FullUpdate++;
}

void sc_reset_display ()
{
   SLsmg_gotorc (SLtt_Screen_Rows - 1, 0);
   SLsmg_refresh (); 
   SLsmg_reset_smg ();
   SLang_reset_tty ();
   signal(SIGTSTP, SIG_DFL);
}

#ifdef SIGVOID
#define SIGTYPE void (*)(int)
#else
#define SIGTYPE int (*)(int)
#endif

static void signals(void)
{
#ifdef SIGWINCH
   signal (SIGWINCH, sc_set_screen_size);
#endif
   (void) signal(SIGQUIT,  sc_quit);
   (void) signal(SIGPIPE,  sc_quit);
   (void) signal(SIGTERM,  sc_quit);
   (void) signal(SIGFPE,  sc_quit);
   (void) signal(SIGBUS,  sc_quit);
   (void) signal(SIGTSTP,  suspend_sc);
}

void sc_quit (int sig)
{
   sc_reset_display ();
   putc('\n', stdout);
    exit(-sig);
}

int modcheck(char *endstr)
{
   if (modflg && curfile[0]) 
     {
	char ch, lin[100];

	(void) SLsmg_gotorc (0, 0);
	(void) SLsmg_erase_eol ();
	(void) sprintf (lin,"File \"%s\" is modified, save%s? ",curfile,endstr);
	(void) SLsmg_write_string (lin);
	(void) SLsmg_refresh ();
	ch = SLang_getkey ();
	if ( ch != 'y' && ch != 'Y' && ch != 'n' && ch != 'N' ) {
	   slsc_error("y or n response required");
	   return (1);
	}
 	if (ch != 'n' && ch != 'N') {
	   if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
	     return (1);
	} else if (ch == ctl('g') || ch == ESC) return(1);
     } else if (modflg) {
	char ch, lin[100];
	
	(void) SLsmg_gotorc (0, 0);
	(void) SLsmg_erase_eol ();
	(void) sprintf (lin,"Do you want a chance to save the data? ");
	(void) SLsmg_write_string (lin);
	(void) SLsmg_refresh ();
	ch = SLang_getkey ();
	if ( ch != 'y' && ch != 'Y' && ch != 'n' && ch != 'N' ) {
	   slsc_error("y or n response required");
	   return (1);
	}
	if (ch == 'n' || ch == 'N') return(0);
	else return(1);
     }
   return(0);
}


/* ------------------------------------------------------------------------ */

#include <stdarg.h>

void sc_clear_message (void)
{
   SLsmg_gotorc(1, 0);
   SLsmg_erase_eol ();
}

char *Sc_This_SL_File;
int Sc_This_SL_File_Line_Num;

void slsc_error (char *fmt, ...)
{
   va_list ap;
   
   SLtt_beep ();
   if (Sc_Error_Seen) return;
   Sc_Error_Seen = 1;
   sc_clear_message ();
   if (Sc_This_SL_File != NULL)
     {
	SLsmg_printf("Line %d:%s:", Sc_This_SL_File_Line_Num, Sc_This_SL_File);
     }
   
   va_start(ap, fmt);
   (void) SLsmg_vprintf(fmt, ap);
   va_end(ap);
   SLang_flush_input ();
   if (SLang_Error == 0) SLang_Error = INTRINSIC_ERROR;
}

void sc_message (char *fmt, ...)
{
   va_list ap;
   sc_clear_message ();
   va_start(ap, fmt);
   (void) SLsmg_vprintf(fmt, ap);
   va_end(ap);
}


static int sc_load_slfile (char *file)
{
   FILE *fp;
   char buf[256];
   char *save_file = Sc_This_SL_File;
   int save_n = Sc_This_SL_File_Line_Num;
   
   if (NULL == (fp = fopen (file, "r"))) return -1;
   
   Sc_This_SL_File_Line_Num = 0;
   Sc_This_SL_File = file;
   
   while (NULL != fgets (buf, 255, fp))
     {
	Sc_This_SL_File_Line_Num++;
	(void) SLcmd_execute_string (buf, &Sc_SLCmd_Table);
	if (SLang_Error) 
	  {
	     break;
	  }
     }
   fclose (fp);
   Sc_This_SL_File = save_file;
   Sc_This_SL_File_Line_Num = save_n;
   return 0;
}

static void sc_read_init_file (void)
{
   char file[1024];
   char *home;
   int len;
   
   sprintf (file, "%s%s", Slsc_Root_Dir, "slsc.rc");
   sc_load_slfile (file);
   if (SLang_Error) return;

   if (NULL != (home = getenv ("HOME")))
     {
	strcpy (file, home);
	len = strlen (file);
	if (len && (file[len - 1] != '/')) strcat (file + len, "/");
     }
   else *file = 0;
   
   strcat (file, ".slscrc");
   
   if (-1 == sc_load_slfile (file))
     {
	if (home != NULL) sc_load_slfile (".slscrc");
     }
}

   
   
