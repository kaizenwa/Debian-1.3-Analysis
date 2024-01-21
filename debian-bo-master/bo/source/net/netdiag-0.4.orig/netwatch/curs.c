/*  curs.c is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */

#include <ncurses.h>
#include <string.h>
#include <stdarg.h>
#include "curs.h"
extern int colour;

void
init_curses ()
{
  initscr ();
  nodelay (stdscr, TRUE);
  cbreak ();
  noecho ();
}


void
cleanup_curses ()
{
  endwin ();
}

void
clrportion (int y1, int x1, int y2, int x2)
{
  int i, j;

  j = x2 - x1;
  for (i = y1; i < y2; i++)
    mvprintw (i, x1, "%*c", j, ' ');
  return;
}

void
clrscr ()
{
  int i;
  clear ();
  if (colour)
    {
/*      bkgdset(COLOR_PAIR(4));   */
      attrset (COLOR_PAIR (4));
      for (i = 0; i < LINES; i++)
	mvchgat (i, 0, COLS, COLOR_PAIR (4), 0, NULL);
    }
  border (0, 0, 0, 0, 0, 0, 0, 0);
  refresh ();
}
