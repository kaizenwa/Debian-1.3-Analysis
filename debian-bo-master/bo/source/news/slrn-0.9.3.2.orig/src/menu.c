/* -*- mode: C; mode: fold; -*- */
#include "config.h"
#include "slrnfeat.h"

/*{{{ Include Files */

#include <stdio.h>
#include <string.h>


#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#include "slrn.h"
#include "menu.h"
#include "misc.h"

/*}}}*/

/*{{{ Menu Routines */

typedef struct /*{{{*/
{
   char *menu_name;
   char *function_name;
}

/*}}}*/
Menu_Type;

static Menu_Type Group_Mode_Menu [] = /*{{{*/
{
     {"Quit", "quit"},
     {"Refresh", "refresh_groups"},
     {"Top", "bob"},
     {"Bot", "eob"},
     {"Post", "post"},
     {"Help", "help"},
     {NULL, NULL}
};

/*}}}*/
	
static Menu_Type Article_Mode_Menu [] = /*{{{*/
{
     {"Quit", "quit"},
     {"Catchup", "catchup_all"},
     {"NextGrp", "skip_to_next_group"},
     {"NextArt", "next"},
     {"Top", "goto_beginning"},
     {"Bot", "goto_end"},
     {"Post", "post"},
     {"Reply", "reply"},
     {"Followup", "followup"},
     {"Help", "help"},
     {NULL, NULL}
};

/*}}}*/

static Menu_Type *Current_Menu;

static void update_menu (Menu_Type *m) /*{{{*/
{
   int col;
   
   Current_Menu = m;
   /* if (Slrn_Full_Screen_Update == 0) return; */
   SLsmg_gotorc (0, 0);
   slrn_set_color (MENU_COLOR);
   if (m != NULL) while (m->menu_name != NULL)
     {
	SLsmg_write_string (m->menu_name);
	SLsmg_write_string ("   ");
	m++;
     }
   
   SLsmg_erase_eol ();

   col = SLtt_Screen_Cols - 16;
   if (SLsmg_get_column () < col)
     SLsmg_gotorc (0, col);
   
   SLsmg_write_string ("slrn ");
   SLsmg_write_string (Slrn_Version);
   
   slrn_set_color (0);
}

/*}}}*/

int slrn_execute_menu (int want_col) /*{{{*/
{
   Menu_Type *m;
   int col;
   int color;
   
   if ((want_col < 0) || (want_col >= SLtt_Screen_Cols)) return -1;
   
   m = Current_Menu;
   if (m == NULL) return -1;
   
   col = -1;
   while (m->menu_name != NULL)
     {
	int dcol = 2 + strlen (m->menu_name);
	if ((want_col > col) 
	    && (want_col <= col + dcol))
	  break;
	col += dcol + 1;
	m++;
     }
   if (m->menu_name == NULL) return -1;
   
   slrn_push_suspension (0);
   /* redraw menu item so that user sees that it has been pressed */
   if (col == -1) col = 0;
   color = MENU_PRESS_COLOR;
   SLsmg_gotorc (0, col);
   while (1)
     {
	slrn_set_color (color);
	if (col) SLsmg_write_char (' ');
	SLsmg_write_string (m->menu_name);
	SLsmg_write_char (' ');
	SLsmg_gotorc (0, col);
	slrn_smg_refresh ();
	if (color == MENU_COLOR) break;
	(void) SLang_input_pending (1);	       /* 1/10 sec */
	color = MENU_COLOR;
     }
   slrn_set_color (0);
   slrn_pop_suspension ();

   slrn_call_command (m->function_name);

   return 0;
}

/*}}}*/
   
void slrn_update_article_menu (void) /*{{{*/
{
   update_menu (Article_Mode_Menu);
}

/*}}}*/

void slrn_update_group_menu (void) /*{{{*/
{
   update_menu (Group_Mode_Menu);
}

/*}}}*/

/*}}}*/

/*{{{ Selection Box Routines */

static char *Sort_Selections [] = /*{{{*/
{
   "No sorting",		       /* 000 */
     "Thread Headers",		       /* 001 */
     "Sort by subject",		       /* 010 */
     "Thread, then sort by subject.",  /* 011 */
     "Sort by scores.",		       /* 100 */
     "Thread, then sort by scores.",   /* 101 */
     "Sort by score and subject",      /* 110 */
     "Thread, then sort by score and subject",   /* 111 */
     "Sort by date (most recent first)",		       /* 1000 */
     "Thread, then sort by date (most recent first)",      /* 1001 */
     "Sort by date (most recent last)",      /* 1010 */
     "Thread, then Sort by date (most recent last)",      /* 1011 */
     NULL
};

/*}}}*/

static Slrn_Select_Box_Type Slrn_Sort_Select_Box = /*{{{*/
{
   "Sorting Method", Sort_Selections
};

/*}}}*/

static void center_string_column (char *title, int row, int col, int num_columns) /*{{{*/
{
   int c;
   int len = strlen (title);
   
   c = (num_columns - len) / 2;
   if (c < 0) c = 0;
   
   c += col;
   SLsmg_gotorc (row, c); 
   SLsmg_write_string (title);
}

/*}}}*/

static int draw_select_box (Slrn_Select_Box_Type *sb) /*{{{*/
{
   int num_selections, max_selection_len;
   int num_rows, num_columns;
   int len;
   int row, column, r, c;
   char **lines, *line, *title;
   static Slrn_Select_Box_Type *last_sb;
   
   if ((sb == NULL) && ((sb = last_sb) == NULL))
     return -1;
   
   last_sb = sb;
   
   lines = sb->lines;
   title = sb->title;

   slrn_push_suspension (0);
   
   if (title == NULL) title = "";
   max_selection_len = strlen (title);
   num_selections = 0;
   while ((line = *lines) != NULL)
     {	
	len = strlen (line);
	if (len > max_selection_len) max_selection_len = len;
	num_selections++;
	lines++;
     }

   /* allow room for title, blank line, top, bottom */
   num_rows = num_selections + 4 + 2;
   num_columns = max_selection_len + (3 + 3);
   
   row = (SLtt_Screen_Rows - num_rows) / 2;
   if (row < 0) row = 0;
   column = (SLtt_Screen_Cols - num_columns) / 2;
   if (column < 0) column = 0;

   slrn_set_color (0);
   SLsmg_fill_region (row, column, num_rows, num_columns, ' ');
   
   r = row + 1;
   center_string_column (title, r, column, num_columns);
   
   lines = sb->lines;
   
   
   num_selections = 0;
   c = column + 1;
   r += 1;
   while ((line = *lines) != NULL)
     {
	lines++;
	r++;
	SLsmg_gotorc (r, c);
	SLsmg_printf ("%2X %s", num_selections, line);
	num_selections++;
     }
   
   r += 2;
   center_string_column ("(Select One)", r, column, num_columns);
				      
   slrn_set_color (MENU_COLOR);
   SLsmg_draw_box (row, column, num_rows, num_columns);
   slrn_set_color (0);
   
   slrn_smg_refresh ();
   
   slrn_pop_suspension ();
   
   return num_selections;
}

/*}}}*/

static void (*Last_Redraw_Fun) (void);

static void select_box_redraw (void) /*{{{*/
{
   slrn_push_suspension (0);
   if (Last_Redraw_Fun != NULL) (*Last_Redraw_Fun) ();
   draw_select_box (NULL);
   Slrn_Full_Screen_Update = 1;
   slrn_pop_suspension ();
}

/*}}}*/

int slrn_select_box (Slrn_Select_Box_Type *sb) /*{{{*/
{
   int num_selections;
   int rsp;
   
   if (Slrn_Batch || (Slrn_Current_Mode == NULL))
     return -1;

   num_selections = draw_select_box (sb);
   
   Last_Redraw_Fun = Slrn_Current_Mode->redraw_fun;
   Slrn_Current_Mode->redraw_fun = select_box_redraw;
   
   Slrn_Full_Screen_Update = 1;
   rsp = SLang_getkey ();
   Slrn_Current_Mode->redraw_fun = Last_Redraw_Fun;
   
   if (rsp >= 'A')
     {
	rsp = 10 + ((rsp | 0x20) - 'a');
     }
   else rsp = rsp - '0';
   
   if ((rsp < 0) || (rsp >= num_selections))
     {
	slrn_error ("Cancelled.");

	while (SLang_input_pending (2))
	  (void) SLang_getkey ();
	
	return -1;
     }
   
   return rsp;
}

/*}}}*/

int slrn_sbox_sorting_method (void) /*{{{*/
{
   return slrn_select_box (&Slrn_Sort_Select_Box);
}

/*}}}*/

/*}}}*/
