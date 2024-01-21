/*
 * help		Print a help window.
 *
 *		This file is part of the minicom communications package,
 *		Copyright 1991-1995 Miquel van Smoorenburg.
 *
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "port.h"
#include "minicom.h"

/* Draw a help screen and return the keypress code. */
int help()
{
  WIN *w;
  int c;
  int i;
  int x1, x2;

#if HISTORY
  i = 1;
#else
  i = 0;
#endif
  x1 = (COLS / 2) - 34;
  x2 = (COLS / 2) + 32;
  w = wopen(x1, 3, x2, 19 + i, BDOUBLE, stdattr, mfcolor, mbcolor, 0, 0, 1);
  
  wlocate(w, 21, 0);
  wputs(w, "Minicom Command Summary");
  wlocate(w, 10, 2);

  wprintf(w, "Commands can be called by %s<key>", esc_key());

  wlocate(w, 15, 4);
  wputs(w, "Main Functions");
  wlocate(w, 47, 4);
  wputs(w, "Other Functions");
  wlocate(w, 0, 6);
  wputs(w, " Dialing directory..D  run script (Go)....G \263 Clear Screen.......C\n");
  wputs(w, " Send files.........S  Receive files......R \263 cOnfigure Minicom..O\n");
  wputs(w, " comm Parameters....P  Add linefeed.......A \263 ");
#ifdef SIGTSTP
  wputs(w, "Suspend minicom....J\n");
#else
  wputs(w, "Jump to a shell....J\n");
#endif
  wputs(w, " Capture on/off.....L  Hangup.............H \263 eXit and reset.....X\n");
  wputs(w, " send break.........F  initialize Modem...M \263 Quit with no reset.Q\n");
  wputs(w, " Terminal settings..T  run Kermit.........K \263 Cursor key mode....I\n");
  wputs(w, " lineWrap on/off....W");
#ifdef _SELECT
  wputs(w, "  local Echo on/off..E \263 Help screen........Z");
#else
  wputs(w, "                       \263 Help screen........Z");
#endif
#if HISTORY
  wlocate(w, 44, 13);
  wputs(w, "\263 scroll Back........B");
#endif

  wlocate(w, 13, 16 + i);
  wputs(w, "Written by Miquel van Smoorenburg 1991-1995");
  wlocate(w, 6, 14 + i);
  wputs(w, "Select function or press Enter for none.");
  wredraw(w, 1);

  c = wxgetch();
  wclose(w, 1);
  return(c);
}
