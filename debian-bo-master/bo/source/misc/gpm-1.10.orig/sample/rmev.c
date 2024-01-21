/*
 * rmev.c - reduced mev.
 *
 * Copyright 1994,1995   rubini@ipvvis.unipv.it (Alessandro Rubini)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ********/

/*
 * This program is a demo for gpm-0.18. It is a client which
 * uses the gpm services (on both the console and xterm) if libgpm.a is there,
 * and reverts to xterm-only mouse management if libgpm.a is not available.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

/*
 * Now, use the symbol HAVE_LIBGPM to know if we can use the
 * full featured mouse support of libgpm or the xterm-only reduced support.
 * The symbol is defined to be "1" or "0" from autoconf.
 */

#ifdef HAVE_LIBGPM       /* libgpm.a */
#  include <gpm.h>
#else                    /* xterm-only */
#  include "gpm-xterm.h"
#endif

#ifdef HAVE_NCURSES_H
#  include <ncurses.h>
#else
#  include <curses.h>
#endif

#define RMEV_NAME "Mev"
#define RMEV_RELEASE "1.10"
#define RMEV_DATE "July 1996"


/*===================================================================*/
int user_handler(Gpm_Event *event, void *data)
{
  do
    {
    printf("mouse: %s, at %2i,%2i (delta %2i,%2i), butt %i, mod %02X\r\n",
	   event->type&GPM_DOWN ? "press  " 
	   : (event->type&GPM_UP ? "release" : "motion "),
	   event->x, event->y,
	   event->dx, event->dy,
	   event->buttons, event->modifiers);
    fflush(stdout);
    
    event->dx=event->dy=0; /* prepare for repetitions */
    if (event->type&GPM_DOWN) event->type=GPM_DRAG;
    }
  while((event->type&GPM_DRAG) && Gpm_Repeat(200));
return 0;

}

/*===================================================================*/
int usage(name)
{
  printf( "(" RMEV_NAME ") " RMEV_RELEASE ", " RMEV_DATE "\n"
        "Usage: %s\n",name);
  return 1;
}

/*===================================================================*/
int main(int argc, char **argv)
{
Gpm_Connect conn;
int c,d,flags;
char s[8];
char cmd[128];

  if (argc>1) exit(usage(argv[0]));

  /* build a catch-all connectionData */
  conn.eventMask=~0;
  conn.defaultMask=0;
  conn.maxMod=~0;
  conn.minMod=0;
/*....................................... Init */

  initscr(); refresh();
  raw(); nonl();
  noecho();


  if (Gpm_Open(&conn,0)==-1)
    fprintf(stderr,"%s: Can't open mouse connection\r\n",argv[0]);

  gpm_handler = user_handler;

  fprintf(stderr,"This program prints on stdout any event it "
	  "gets from stdin and from the mouse\r\n");

  while ((c=Gpm_Getc(stdin))!=EOF)
    {
    if (isatty(0) && (c==4)) break; /* ctl-D to exit */
    strcpy(s, c&0x80 ? "M-" : "");
    c &= 0x7F; d=c;
    strcat(s, c<' ' ? "C-" : "");
    if (c<' ') d+=0x40;
    if (c==0x7F) d='?';
    printf("key: %02X ('%s%c')\r\n",c,s,d);
    fflush(stdout);
    }

  noraw();
  echo();
  endwin();

/*....................................... Done */

  Gpm_Close(); /* close the mouse */
  exit(0);
}






