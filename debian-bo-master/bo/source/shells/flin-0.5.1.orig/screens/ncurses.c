/*
   Displays menu and takes input
   Copyright (C) 1995, 1996  Brian Cully

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 675 Mass
   Ave, Cambridge, MA 02139, USA.

   please send patches (w/ explanation) or advice to: `shmit@kublai.com'
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_NCURSES_NCURSES_H
#include <ncurses/ncurses.h>
#else
#include <curses.h>
#endif

#include <signal.h>
#include <unistd.h>
#include <termios.h>

#include "ncr_scr.h"
#include "../screen.h"
#include "../misc.h"
#include "../exec.h"
#include "../menu.h"

bool color;
char draw_box = 1;
extern struct menu *main_menu;
extern int noclobber;

static void setup_colors() {
   init_pair(MENU_TITLE + 1, TITLE_COL, BG_COL);
   init_pair(MENU_NOP + 1, NOP_COL, BG_COL);
   init_pair(MENU_EXEC + 1, EXEC_COL, BG_COL);
   init_pair(MENU_ARGS + 1, ARGS_COL, BG_COL);
   init_pair(MENU_SUB + 1, SUB_COL, BG_COL);
   init_pair(MENU_EXIT + 1, EXIT_COL, BG_COL);
   init_pair(MENU_QUIT + 1, QUIT_COL, BG_COL);
}

/* Interface to main module
   Initialize the screen, must be called before anything else */
void init_scr() {
   sigset_t newmask, oldmask;

   /*

      It looks as if initscr resets the default signal handlers. Block sigint, sigquit  
      and sigtstp until after we are through, then quickly ignore them and allow signals
      through again 
    */

   sigemptyset(&newmask);
   sigemptyset(&oldmask);

   sigaddset(&newmask, SIGINT);
   sigaddset(&newmask, SIGTSTP);
   sigaddset(&newmask, SIGQUIT);

   sigprocmask(SIG_BLOCK, &newmask, &oldmask);

   initscr();

   if ((color = has_colors())) {
      start_color();
      setup_colors();
   }
   cbreak();
   noecho();
   keypad(stdscr, TRUE);

   signal(SIGINT, SIG_IGN);
   signal(SIGTSTP, SIG_IGN);
   signal(SIGQUIT, SIG_IGN);

   sigprocmask(SIG_SETMASK, &oldmask, (sigset_t *) NULL);
}

/* Interface to main module
   Close the output, must be called when finished */
void close_scr() {
   endwin();
}

void write_str(char *s) {
   int row, col;

   getyx(stdscr, row, col);
   mvaddstr(row, col, s);
   refresh();
}

void clear_scr() {
   attrset(A_NORMAL);
   erase();
   refresh();
}

int readchar() {
   return getch();
}

static void draw_dialog(WINDOW ** b_win, WINDOW ** win, char *title, int len) {
   int i;

   *b_win = newwin(3, len + 2, 1, 0);
   box(*b_win, 0, 0);
   mvwaddstr(*b_win, 0, (len - strlen(title)) / 2 - 1, title);
   wrefresh(*b_win);

   *win = derwin(*b_win, 1, len, 1, 1);
   for (i = 0; i < len; i++)
      waddch(*win, ' ');
}

static void clear_wins(WINDOW **b_win, WINDOW **win) {
   if (!draw_box) {
      werase(*win);
      wrefresh(*win);
   }
   delwin(*win);
   if (draw_box)
      delwin(*b_win);
}

int get_args(char *args, char *title) {
   WINDOW *b_win, *win;
   struct termios term;
   int c;
   cc_t erase = 0, kill = 0;
   int i = 0;

   if (tcgetattr(0, &term) >= 0) {
      erase = term.c_cc[VERASE];
      kill = term.c_cc[VKILL];
   }
   draw_dialog(&b_win, &win, title, 78);
   waddch(win, ' ');
   mvwdelch(win, 0, 0);
   wmove(win, 0, 0);
   wrefresh(win);

   while ((c = wgetch(win)) != '\n' && c != ESCAPEKEY) {
      if ((c == erase) || (c == KEY_BACKSPACE)) {
	 if (i > 0) {
	    i--;
	    mvwdelch(win, 0, i);
	 }
      } else if (c == kill) {
	 wmove(win, 0, 0);
	 i = 0;
	 wclrtoeol(win);
      } else if (c == '\014')	/* ctrl-l */
	 wrefresh(curscr);
      else {
	 waddch(win, c);
	 *(args + i) = c;
	 i++;
      }
      wrefresh(win);
   }
   if (c == ESCAPEKEY)
      return 1;
   
   *(args + i) = '\0';

   clear_wins(&b_win, &win);
   return 0;
}

static void display_items(WINDOW * win, struct menu_items src, int cols, int hilite) {
   int lenstr, i;

   wattrset(win, hilite ? A_REVERSE : A_BOLD);
   if (color)
      wattron(win, COLOR_PAIR((src.type + 1)));

   switch (src.type) {
   case MENU_TITLE:
      for (i = 0; i < (cols - strlen(src.name)) / 2; i++)
	 waddch(win, ' ');
      waddstr(win, src.name);
      break;

   case MENU_SUB:
      waddch(win, '<');
      waddstr(win, src.name);
      waddch(win, '>');
      break;

   case MENU_NOP:
      wattroff(win, hilite ? A_REVERSE : A_BOLD);
      if (strlen(src.name) > 1)
	 waddstr(win, src.name);
      else
	 whline(win, ACS_HLINE, cols);
      break;

   default:
      waddstr(win, src.name);
      break;
   }

   lenstr = strlen(src.name);
   if (lenstr > 0) {
      lenstr += (src.type == MENU_TITLE) ? (cols - lenstr) / 2 : ((src.type == MENU_SUB) ? 2 : 0);
      for (i = cols; i > lenstr; i--)
	 waddch(win, ' ');
   }
}

static void draw_menu(WINDOW * win, int nlines, int ncols, struct menu_items *items, int *row) {
   int i;
   struct menu_items *p = items;

   for (i = 0; i <= nlines; i++) {
      if (p) {
	 wmove(win, i, 0);
	 display_items(win, *p, ncols, FALSE);
	 p = p->next;
      }
   }
}

static void draw_status_bar() {
   int i;

   attrset(A_REVERSE);
   mvaddstr(0, 0, TOP_BAR);
   for (i = strlen(TOP_BAR); i < COLS; i++)
      addch(' ');
   mvaddstr(LINES - 1, 0, BOT_BAR);
   for (i = strlen(BOT_BAR); i < COLS; i++)
      addch(' ');
}

/* Go down at least one row, skipping comments (MENU_NOP) and titles (MENU_TITLE)
   *row == a pointer to the current row
   **p == menu item list to sort through and display */
static void down_hilite(WINDOW * win, int *row, int cols, struct menu_items **p) {
   int back_row = *row;
   struct menu_items *back_item = *p;
   bool quit = FALSE;

   /* redisplay the first row */
   wmove(win, *row, 0);
   display_items(win, **p, cols, FALSE);

   /* advance until past titles and menus or end of menus */
   while ((*p)->next && !quit) {
      *p = (*p)->next;
      (*row)++;
      switch ((*p)->type) {
      case MENU_TITLE:
      case MENU_NOP:
	 break;
      default:
	 quit = TRUE;
	 break;
      }
   }

   /* if menu end was hit without finding a hilitable item, go back to original row */
   if (!quit) {
      *row = back_row;
      *p = back_item;
   }
   wmove(win, *row, 0);
   display_items(win, **p, cols, TRUE);
   wrefresh(win);
}

/* see down_hilite and reverse the direction */
static void up_hilite(WINDOW * win, int *row, int cols, struct menu_items **p) {
   int back_row = *row;
   struct menu_items *back_item = *p;
   bool quit = FALSE;

   wmove(win, *row, 0);
   display_items(win, **p, cols, FALSE);

   while ((*p)->prev && !quit) {
      *p = (*p)->prev;
      (*row)--;
      switch ((*p)->type) {
      case MENU_TITLE:
      case MENU_NOP:
	 break;
      default:
	 quit = TRUE;
	 break;
      }
   }

   if (!quit) {
      *row = back_row;
      *p = back_item;
   }
   wmove(win, *row, 0);
   display_items(win, **p, cols, TRUE);
   wrefresh(win);
}

static void init_list(WINDOW ** win, WINDOW ** bwin, struct menu_items **items, int *nitems,
		      int *nlines, int *ncols, int *row) {
   struct menu_items *p = *items, *q;

   *nitems = 0;
   *ncols = 0;

   draw_status_bar();
   refresh();

   while (p->prev)
      p = p->prev;

   q = p;

   while (q) {
      int i;

      if ((i = strlen(q->name) +
	   ((q->type == MENU_SUB) ? 2 : ((q->type == MENU_TITLE) ? 2 : 0))) > *ncols)
	 *ncols = i;
      (*nitems)++;
      q = q->next;
   }

   *nlines = (*nitems < LINES) ? (*nitems) : LINES;
   *ncols = (*ncols < COLS) ? (*ncols) : COLS;

   if (draw_box) {
      if ((*bwin = newwin((*nlines)+2, (*ncols)+2,
			  (LINES - *nlines)/2 - 1,
			  (COLS - *ncols)/2 - 1)) == NULL)
      error("Couldn't create border window", 0);

      box(*bwin, 0, 0);
      wrefresh(*bwin);
  
      if ((*win = derwin(*bwin, *nlines, *ncols, 1, 1)) == NULL)
	 error("Couldn't create main window", 0);
   } else {
      if ((*win = newwin(*nlines, *ncols, (LINES - *nlines)/2, (COLS - *ncols)/2)) == NULL)
	 error("Couldn't create main window", 0);
   }

   draw_menu(*win, *nlines, *ncols, p, row);

   if (((*items)->type == MENU_TITLE) || ((*items)->type == MENU_NOP))
      down_hilite(*win, row, *ncols, items);
   else {
      wmove(*win, *row, 0);
      display_items(*win, **items, *ncols, TRUE);
   }

   wrefresh(*win);
}

void display_list(struct menu *menu_list) {
   struct menu_items *p = menu_list->data;
   int win_lines, win_cols, n_items, row = 0, quit = 0, dontclobber = 0;
   WINDOW *win, *b_win;

   if (!(menu_list))
      return;

   dontclobber = ((noclobber) && (menu_list == main_menu));

   init_list(&win, &b_win, &p, &n_items, &win_lines, &win_cols, &row);
   refresh();

   while (!quit) {
      switch (getch()) {
      case KEY_LEFT:
      case 2:
	 if (!dontclobber) {
	    attrset(A_NORMAL);
	    erase();
	    refresh();
	    quit = TRUE;
	 }
	 break;

      case KEY_RIGHT:
      case '\n':
      case 6:
	 switch (p->type) {
	 case MENU_EXIT:
	    attrset(A_NORMAL);
	    erase();
	    refresh();
	    quit = TRUE;
	    break;

	 case MENU_QUIT:
	    clear_wins(&b_win, &win);
	    attrset(A_NORMAL);
	    erase();
	    refresh();
	    close_scr();
	    exit(0);

	 default:
	    clear_wins(&b_win, &win);
	    exec_item(*p, &menu_list);
	    init_list(&win, &b_win, &p, &n_items, &win_lines, &win_cols, &row);
	    break;
	 }
	 break;

      case KEY_DOWN:
      case 14:
	 down_hilite(win, &row, win_cols, &p);
	 break;

      case KEY_UP:
      case 16:
	 up_hilite(win, &row, win_cols, &p);
	 break;

      case 'g':
      case 'G':
	 clear_wins(&b_win, &win);
	 display_file(PATH_TO_GPL);
	 init_list(&win, &b_win, &p, &n_items, &win_lines, &win_cols, &row);
	 break;

      case 'h':
      case 'H':
	 clear_wins(&b_win, &win);
	 display_file(PATH_TO_HELP);
	 init_list(&win, &b_win, &p, &n_items, &win_lines, &win_cols, &row);
	 break;
      case 'F':
      case 'f':
      case 12:
	 wrefresh(curscr);
	 break;
      }
   }
   clear_wins(&b_win, &win);
}
