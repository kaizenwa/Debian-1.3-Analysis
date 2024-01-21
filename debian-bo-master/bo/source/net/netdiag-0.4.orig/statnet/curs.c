/*  curs.c is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */

#include <ncurses.h>
#include <string.h>
#include <stdarg.h>
#include "curs.h"

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
clrscr ()
{
  erase ();
  refresh ();
}
