/*
 * libcurses.c - client library - curses level (gpm-Linux)
 *
 * Copyright 1994,1995   rubini@ipvvis.unipv.it (Alessandro Rubini)
 * 
 * xterm management is mostly by Miguel de Icaza
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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>        /* select(); */
#include <sys/time.h>      /* timeval */
#include <sys/types.h>     /* socket() */

#include "gpmInt.h"
#include <curses.h> /* it is right? */

#define GET(win) ((win) ? wgetch(win) : getch())

int Gpm_Wgetch(WINDOW *win)
{
fd_set selSet;
int max, flag, result;
int fd=STDIN_FILENO;
static Gpm_Event ev;

  if (!gpm_flag || gpm_fd==-1) return GET(win);
  if (gpm_morekeys) return (*gpm_handler)(&ev,gpm_data);
  gpm_hflag=0;

  max = (gpm_fd>fd) ? gpm_fd : fd;

/*...................................................................*/
  if (gpm_fd>=0)                                            /* linux */
    while(1)
      {
      if (gpm_visiblepointer) GPM_DRAWPOINTER(&ev);
      do
	{
	FD_ZERO(&selSet);
	FD_SET(fd,&selSet);
	FD_SET(gpm_fd,&selSet);
	gpm_timeout.tv_sec=SELECT_TIME;
	flag=select(max+1,&selSet,(fd_set *)NULL,(fd_set *)NULL,&gpm_timeout);
	}
      while (!flag);

      if (FD_ISSET(fd,&selSet))
	return GET(win);
      
      if (flag==-1)
	continue;
      
      if (Gpm_GetEvent(&ev) && gpm_handler
	  && (result=(*gpm_handler)(&ev,gpm_data)))
	{
	gpm_hflag=1;
	return result;
	}
      }
  else
/*...................................................................*/
  if (gpm_fd==-2)                                           /* xterm */
    {
#define DELAY_MS 100
    static struct timeval to={0,DELAY_MS*1000};
    static fd_set selSet;
    static int prevchar=EOF;
    extern gpm_convert_event(char *data, Gpm_Event *event);
    int c; char mdata[4];

    if ((c=prevchar)!=EOF)  /* if ungetc() didn't suffice... */
      {
      prevchar=EOF;
      return c;
      }

    while(1)
      {
      do 
	{
	FD_ZERO(&selSet); FD_SET(fd,&selSet);
	gpm_timeout.tv_sec=SELECT_TIME;
	flag=select(fd+1,&selSet,(fd_set *)NULL,(fd_set *)NULL,&gpm_timeout);
	}
      while (!flag);

      if ((c=GET(win))!=0x1b) return c;

      /* escape: go on */
      FD_ZERO(&selSet); FD_SET(fd,&selSet); to.tv_usec=DELAY_MS*1000;
      if ((flag=select(fd+1,&selSet,(fd_set *)NULL,(fd_set *)NULL,&to))==0)
	return c;
      if ((c=GET(win))!='[')
	{ungetc(c,stdin); return 0x1B;}

      /* '[': go on */
      FD_ZERO(&selSet); FD_SET(fd,&selSet); to.tv_usec=DELAY_MS*1000;
      if ((flag=select(fd+1,&selSet,(fd_set *)NULL,(fd_set *)NULL,&to))==0)
	{ungetc(c,stdin); return 0x1B;}
      if ((c=GET(win))!='M')
	{ungetc(c,stdin);prevchar='['; return 0x1B;}

      /* now, it surely is a mouse event */

      for (c=0;c<3;c++) mdata[c]=GET(win);
      gpm_convert_event(mdata,&ev);

      if (gpm_handler && (result=(*gpm_handler)(&ev,gpm_data)))
	{
	gpm_hflag=1;
	return result;
	}
      } /* while(1) */
    }
  return 0;
}
