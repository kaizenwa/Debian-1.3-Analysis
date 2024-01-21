/* This demo test some of the slsmg features. */
#include "config.h"

#include <stdio.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>

#include "demolib.c"

static void menu_loop (void);
static int select_menu_item (int i);
static void init_colors (void);

int main (int argc, char **argv)
{   
   if (-1 == demolib_init_terminal (1, 1))
     return 1;
   
   init_colors ();
   
   if (argc <= 1)
     menu_loop ();
   
   do
     {
	argc--;
	argv++;
	     
	if (-1 == select_menu_item (atoi (*argv)))
	  menu_loop ();
     }
   while (argc > 1);
	     
   demolib_exit (0);
   return 1;
}

static void quit (void)
{
   demolib_exit (0);
}

static void color_test (void);
static void alt_char_test (void);
static void esc_seq_test (void);

typedef struct
{
   char *name;
   void (*funct) (void);
}
Menu_Type;

static Menu_Type Root_Menu [] =
{
     {"Color Test", color_test},
     {"Alt charset test", alt_char_test},
     {"Key Escape Sequence Report", esc_seq_test},
     {"Quit", quit},
     {NULL, NULL}
};

Menu_Type *Current_Menu = Root_Menu;

static void print_menu (void)
{
   int i;
   int row;
   Menu_Type *menu;
   
   menu = Current_Menu;
   
   SLsig_block_signals ();
   
   SLsmg_cls ();
   
   row = 2;
   i = 1;
   while (menu->name != NULL)
     {
	SLsmg_gotorc (row, 3);
	SLsmg_printf ("%2d. %s", i, menu->name);
	menu++;
	row++;
	i++;
     }
   
   row = 0;
   SLsmg_gotorc (row, 1);
   SLsmg_write_string ("Choose number:");
   
   SLsmg_refresh ();
   SLsig_unblock_signals ();
}


static int select_menu_item (int num)
{
   int i = 1;
   Menu_Type *m = Current_Menu;
   
   while (m->name != NULL)
     {
	if (i == num)
	  {
	     (*m->funct) ();
	     return 0;
	  }
	i++;
	m++;
     }
   
   return -1;
}


static void menu_loop (void)
{
   int ch;
   
   print_menu ();
   
   while (1)
     {
	ch = SLkp_getkey () - '0';
	
	if (-1 == select_menu_item (ch))
	  {
	     SLtt_beep ();
	     continue;
	  }
	print_menu ();
     }
}

static void write_centered_string (char *s, int row)
{
   unsigned int len;
   int col;
   
   if (s == NULL)
     return;
   
   len = strlen (s);
   
   /* Want 2 * col + len == SLtt_Screen_Rows */
   if (len >= (unsigned int) SLtt_Screen_Cols) col = 0;
   else col = (SLtt_Screen_Cols - (int)len) / 2;
   
   SLsmg_gotorc (row, col);
   SLsmg_write_string (s);
}

static void pre_test (char *title)
{
   SLsig_block_signals ();
   SLsmg_cls ();
   write_centered_string (title, 0);
}

static void post_test (void)
{
   write_centered_string ("Press any key to return.", SLtt_Screen_Rows - 1);
   SLsmg_refresh ();
   SLsig_unblock_signals ();
   (void) SLkp_getkey ();
}

/* Various tests */

static char *Colors [] =
{
   "black",
   "red",
   "green",
   "brown",
   "blue",
   "magenta",
   "cyan",
   "lightgray",
   "gray",
   "brightred",
   "brightgreen",
   "yellow",
   "brightblue",
   "brightmagenta",
   "brightcyan",
   "white",
   NULL
};

static void init_colors (void)
{
   int i;
   char *fg, *bg;
   
   fg = "black";
   i = 0;
   while ((bg = Colors[i]) != NULL)
     {
	i++;
	SLtt_set_color (i, NULL, fg, bg);
     }
}

   
static void color_test (void)
{
   int color;
   int row;
   
   pre_test ("Color Test");
   
   row = 1;
   
   while (row < SLtt_Screen_Rows - 1)
     {
	color = 1 + (color % 16);

	SLsmg_gotorc (row, 0);
	SLsmg_set_color (color);
	SLsmg_erase_eol ();
	row++;
     }
   
   SLsmg_set_color (0);
   post_test ();
}

static void alt_char_test (void)
{
   int row, col;
   int ch;
   
   pre_test ("Alternate Charset Test");
   
   row = SLtt_Screen_Rows / 2 - 2;
   col = 0;
   for (ch = 32; ch < 128; ch++)
     {
	SLsmg_gotorc (row, col);
	SLsmg_write_char (ch);
	SLsmg_gotorc (row + 1, col);
	SLsmg_set_char_set (1);
	SLsmg_write_char (ch);
	SLsmg_set_char_set (0);
	col++;
	
	if (col > 40)
	  {
	     col = 0;
	     row += 4;
	  }
     }
   
   post_test ();
}

static void esc_seq_test (void)
{
   int row;
   unsigned char ch;
   unsigned char buf[80], *b;
   
   pre_test ("Escape Sequence Report");
   
   row = SLtt_Screen_Rows / 2;
   
   SLsmg_gotorc (row, 0);
   SLsmg_write_string ("Press key: ");
   SLsmg_refresh ();
   
   SLsmg_gotorc (row, 0);
   SLsmg_write_string ("Key returned \"");

   b = buf;
   do
     {
	ch = SLang_getkey ();
	if (ch < ' ')
	  {
	     *b++ = '^';
	     ch += '@';
	     *b++ = ch;
	  }
	else if (ch >= 127)
	  {
	     sprintf ((char *) b, "\\d%d", ch);
	     b += strlen ((char *) b);
	  }
	else if ((ch == '"') || (ch == '\\'))
	  {
	     *b++ = '\\';
	     *b++ = ch;
	  }
	else *b++ = ch;
     }
   while (SLang_input_pending (3) > 0);
   *b++ = '"';
   *b = 0;
   SLsmg_write_string ((char *) buf);
   post_test ();
}
