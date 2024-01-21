/* Mouse managing
   Copyright (C) 1994 Miguel de Icaza.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Events received by clients of this library have their coordinates 0 */
/* based */

/* "$Id: mouse.c,v 1.8 1995/02/21 19:06:52 miguel Exp $" */

#include <config.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#include <signal.h>	/* For kill() and SIGQUIT */
#include <fcntl.h>
#include <termios.h>
#include <malloc.h>
#include <stdio.h>

#include "mad.h"
#include "mouse.h"
#include "global.h"		/* ESC_STR */
#include "util.h"		/* xmalloc */
#include "key.h"		/* define sequence */

int xmouse_flag = 0;

#ifdef HAVE_LIBGPM
static int mouse_d;		/* Handle to the mouse server */
#endif

#ifdef DEBUGMOUSE
/* Only used for debugging */
static int top_event = 0;
FILE *log;
#endif

#ifdef HAVE_LIBGPM

void show_mouse_pointer (int x, int y)
{
#ifdef HAVE_LIBGPM
    if (use_mouse_p == GPM_MOUSE){
	Gpm_DrawPointer (x, y, gpm_consolefd);
    }
#endif
}

#endif /* HAVE_LIBGPM */
#if 0
int mouse_handler (Gpm_Event *gpm_event)
{
    MouseEvent *event = mouse_events;
    int x = last_x = gpm_event->x;
    int y = last_y = gpm_event->y;
    int redo = 0;
    
/*    DEBUGM ((log, "Mouse [%d, %d]\n", x, y)); */

    /* Call any registered event handlers */
    for (; event; event = (MouseEvent *) event->next){
	if ((event->x1 <= x) && (x <= event->x2)
	    && (event->y1 <= y) && (y <= event->y2)){
	    gpm_event->x -= event->x1;
	    gpm_event->y -= event->y1;
	    last_mouse_event = event;
	    redo = (*(event->mouse_callback))(gpm_event, event->data);
	    gpm_event->x += event->x1;
	    gpm_event->y += event->y1;
	    break;
	}
    }
    return redo;
}

int redo_mouse (Gpm_Event *event)
{
    if (last_mouse_event){
    	int result;
    	event->x -= last_mouse_event->x1;
    	event->y -= last_mouse_event->y1;
	result = (*(last_mouse_event->mouse_callback))
	         (event,last_mouse_event->data);
    	event->x += last_mouse_event->x1;
    	event->y += last_mouse_event->y1;
    	return result;
    }
    return MOU_NORMAL;
}
#endif

void init_mouse (void)
{
    switch (use_mouse_p)
    {
#ifdef HAVE_LIBGPM
      case GPM_MOUSE:
	{
	    Gpm_Connect conn;

	    conn.eventMask   = ~GPM_MOVE;
	    conn.defaultMask = GPM_MOVE;
	    conn.minMod      = 0;
	    conn.maxMod      = 0;

	    if ((mouse_d = Gpm_Open (&conn, 0)) == -1)
	        return;

#ifdef DEBUGMOUSE
	    log = fopen ("mouse.log", "w");
#endif
	}
	break;
#endif /* HAVE_LIBGPM */
	case XTERM_MOUSE:
	    if (!xmouse_flag) {

		/* save old highlight mouse tracking */
		printf("%c[?1001s",27);

		/* enable mouse tracking */
		printf("%c[?1000h",27);

		fflush (stdout);
		/* turn on */
		xmouse_flag = 1; 
		define_sequence (0, ESC_STR "[M", MCKEY_NOACTION);
	    }
	    break;
	default:
	    /* nothing */
	break;
    } /* switch (use_mouse_p) */ 
}

void shut_mouse (void)
{
    switch (use_mouse_p){
#ifdef HAVE_LIBGPM
      case GPM_MOUSE:
	Gpm_Close ();
	break;
#endif
      case XTERM_MOUSE:
	if (xmouse_flag) {

	    /* disable mouse tracking */
	    /* Changed the 1 for an 'l' below: */
	    printf("%c[?1000l",27);

	    /* restore old highlight mouse tracking */
	    printf("%c[?1001r",27);

	    fflush (stdout);
	    /* off */
	    xmouse_flag = 0;
	}
	break;
      default:
	/* nothing */
	break;
    }
}

#ifdef DEBUGMOUSE
void mouse_log (char *function, char *file, int line)
{
    fprintf (log, "%s called from %s:%d\n", function, file, line);
}
#endif

