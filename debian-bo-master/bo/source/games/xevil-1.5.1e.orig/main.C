// "main.C"

/*    Copyright (C) 1994,1995,1996  Steve Hardt

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Steve Hardt 
    hardts@mit.edu (valid until Nov. 1996)
    hardts@netscape.com
    hardts@alum.mit.edu
    http://graphics.lcs.mit.edu/~hardts/xevil.html
*/

// Include Files
#include "utils.h"

extern "C" {
#include <sys/socket.h> 
#include <sys/types.h>
#include <sys/time.h>

#include <X11/Xutil.h>
#include <X11/Xos.h>
}

#ifdef USE_SELECT_H
extern "C" {
#include <sys/select.h>
}
#endif

#include <iostream.h>

#ifdef SELECT_NEEDS_PROTOTYPES
extern "C" {
int select(int,
	   fd_set *,
	   fd_set *,
	   fd_set *,
	   struct timeval *);
}
#endif

#include "game.h"



// Functions
main(int argc, char **argv)
{
#ifdef USE_RANDOM
  srandom((int)time(NULL));
#else
  srand((unsigned int)time(NULL));
#endif

  GameP game = new Game(&argc,argv);

  assert (clock() != -1);

  long total = 0;
  long events = 0;

  while (True)
    {
      Quanta quanta = game->get_quanta(); 
      clock_t startTime = clock();

      // Clock the game
      total++;

      for (int dpyNum = 0; dpyNum < game->get_dpy_max(); dpyNum++)
	{
	  int eventsNum;
	  if (eventsNum = XEventsQueued(game->get_dpy(dpyNum),QueuedAfterReading))
	    for (int m = 0; m < eventsNum; m++)
	      {
		XEvent event;
		XNextEvent(game->get_dpy(dpyNum),&event);
		game->process_event(dpyNum,&event);
		events++;
	      }
	}
      
      game->clock();
      if (game->show_stats() && !(total % Game::REPORT_TIME))
	cout << "total:" << total << " events:" << events << 
	  " percent:" << ((float)events / (float) total) << endl; 
      

      struct timeval waitTime;
      waitTime.tv_sec = 0;
      clock_t diff = clock() - startTime;
      if (diff > 0)
	waitTime.tv_usec = 1000 * (quanta - (long)(MSEC_PER_CLOCK * diff));
      else
	waitTime.tv_usec = 1000 * quanta;

      if (waitTime.tv_usec > 0)
	if (select(0,NULL,NULL,NULL,&waitTime) < 0)
	  {
	    cerr << "Error with select." << endl;
	    exit(1);
	  }
    } 
}

