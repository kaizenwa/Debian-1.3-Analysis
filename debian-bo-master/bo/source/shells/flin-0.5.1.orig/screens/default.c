/*
Displays menu and takes input
Copyright (C) 1995  Brian Cully

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
#   include <config.h>
#endif

#ifdef HAVE_NCURSES_NCURSES_H
#   include <ncurses/ncurses.h>
#else
#   error This program requires ncurses
#endif
#include <string.h>
#include <stdio.h>

#include "../screen.h"
#include "misc.h"
#include "menu.h"

/* Length of NOP bar */
#define NOP_LINE_S 25

/* Output a menu item to the screen
   src == menu item to display
   attr == attribute */
static void display_items(struct menu_items src, int attr) {
   switch (src.type) {
   case MENU_TITLE:
      attrset(A_NORMAL);
      addch('[');
      attrset(attr);
      addstr(src.name);
      attrset(A_NORMAL);
      addch(']');
      break;
   case MENU_SUB:
      attrset(A_NORMAL);
      addch('<');
      attrset(attr);
      addstr(src.name);
      attrset(A_NORMAL);
      addstr(">");
      break;
   case MENU_NOP:
      attrset(A_NORMAL);
      if (strlen(src.name) > 1) {
	 addstr(src.name);
      } else {
	 int x, y;

	 hline(ACS_HLINE, NOP_LINE_S);
	 getyx(stdscr, y, x);
	 move(y, NOP_LINE_S);
      }
      break;

   default: 
      attrset(attr);
      addstr(src.name);
      break;
   }

   addstr("\n");
}

/* Go down at least one row, skipping comments (MENU_NOP) and titles (MENU_TITLE)
   *row == a pointer to the current row
   **p == menu item list to sort through and display */
static void down_hilite(int *row, struct menu_items **p) {
   int back_row = *row;
   struct menu_items *back_item = *p;
   bool quit=FALSE;
   
   /* redisplay the first row */
   move(*row, 0);
   display_items(**p, A_BOLD);

   /* advance until past titles and menus or end of menus */
   while ((*p)->next && !quit) {
      *p = (*p)->next; (*row)++;
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

   move(*row, 0);
   display_items(**p, A_REVERSE);
}

/* see down_hilite and reverse the direction */
static void up_hilite(int *row, struct menu_items **p) {
   int back_row = *row;
   struct menu_items *back_item = *p;
   bool quit=FALSE;
   
   move(*row, 0);
   display_items(**p, A_BOLD);

   while ((*p)->prev && !quit) {
      *p = (*p)->prev; (*row)--;
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

   move(*row, 0);
   display_items(**p, A_REVERSE);
}

/* display the menu list on the screen
   **p == menu list to display
   *row == pointer to current row */
static void init_list(struct menu_items **p, int *row) {
   struct menu_items *q = *p;
   
   attrset(A_NORMAL);
   erase();

   while (q->prev)
      q = q->prev;
   
   while (q) {
      display_items(*q, A_BOLD);
      q = q->next;
   }

   if (((*p)->type == MENU_TITLE) || ((*p)->type == MENU_NOP))
      down_hilite(row, p);
   else {
      move(*row, 0);
      display_items(**p, A_REVERSE);
   }

   refresh();
}

/* Interface to main module
   Initialize the screen, must be called before anything else */
void init_scr() {
   initscr();
   cbreak();
   noecho();
   keypad(stdscr, TRUE);
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
   attrset(A_NORMAL); erase(); refresh();
}

int readchar() {
   return getch();
}

/* Interface to main module
   Display menu list,
   *menu_list == menu list to display */
void display_list(struct menu *menu_list) {
   bool quit = FALSE;
   struct menu_items *p = menu_list->data;
   int row=0;

   if (menu_list != NULL) {
      init_list(&p, &row);

      while (!quit) {
	 switch (getch()) {
	 case KEY_LEFT:
	    erase();
	    quit = TRUE;
	    break;

	 case '\n':
	 case KEY_RIGHT:
	    switch (p->type) {
	    case MENU_EXIT:
	       quit = TRUE;
	       break;

	    case MENU_QUIT:
	       exit(0);

	    default:
	       exec_item(*p, &menu_list);
	       init_list(&p, &row);
	       break;
	    }
	    break;

	 case KEY_DOWN:
	    down_hilite(&row, &p);
	    break;

	 case KEY_UP:
	    up_hilite(&row, &p);
	    break;
	 }
      }
   }
}
