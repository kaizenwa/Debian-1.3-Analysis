#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mainevnt.c (Main Event)
 * Purpose:	Main Event Loop and Dispatcher
 * Subroutine:	control_event_loop()		returns: void
 * Xlib calls:	XPending(), XNextEvent()
 * UNIX calls:	select()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{1} Jay Travisano (STScI) VMS,IMTOOL changes      10 Nov 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <sys/time.h>		/* get struct timeval */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/define.h"	/* YES, NO, MIN, MAX and more */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/cgraph.h"	/* color graph structures */
#include "hfiles/extern.h"	/* extern main parameter structures */
extern struct cgraphRec cgraph;	/* get access to subwindow ID's */
extern struct windowRec desktop;
extern Display *display;	/* server connection */

#if VMS && IMTOOL
void XZ_ast();
int  XZ_efn = 10;
#endif

/*
 * Subroutine:	control_event_loop
 * Purpose:	Main event loop and dispatcher
 * Xlib calls:	XNextEvent();
 */
void control_event_loop ( )
{
#ifdef SYSV
  int mask[2];
#else
  int mask[4];
#endif
  GC set_gc_with_background();
  int *response, *control_buttonbox();
  void map_windows(), dispatch_select(), redraw_window();
  void control_cursor(), control_cgraph(), control_pan(), control_color();
  void magnify_disp(), magnify_pan(), map_graphbox(), close_input_pipe();
  void adjust_desktop(), adjust_graphbox(), key_response(), control_blink();
#ifdef VMS
#ifdef IMTOOL
  /* Set asynch notification for all Xwindow events and Imtool pipe
   * (`Z') events.  Note that buttonbox event notification is set
   * separately (see buttonlib/makebox.c and buttonlib/remotectrl.c).
   * The same goes for some other dynamic window/displays (see 
   * editctrl.c and grphcinit.c).
   */
  {
    extern Window root;
    int	all_events = 0xFFFFFFFF;

    sys$clref (XZ_efn);

    XSelectAsyncInput (display, root, all_events, XZ_ast, XZ_efn);
  }
#endif
#endif
  /* the event loop */
  control.priority = 0;
  /* set mask to screen events */
  while( 1 ) {
    if( control.remote_connected ) {
#ifdef IMTOOL
#ifndef VMS 
      /*  AIX is the only UNIX we have seen which can't handle the select!!  */
      if( XPending(display) ) {
	XNextEvent(display, &control.event);
      } else {
	mask[0] = control.select_mask[0];
	mask[1] = control.select_mask[1];
	/* check for either pipe or X server event */
	if( select(control.select_size, (fd_set *)mask,
		   (fd_set *)0, (fd_set *)0, (struct timeval *)0) <0 ) {
	  perror("select error");
	  continue;
	}
	if( (mask[0] & control.Xserver.mask[0]) ||
	    (mask[1] & control.Xserver.mask[1]) ) {
	  XNextEvent(display, &control.event);
	} else {
	  /* call routine to read from pipe */
	  respond_to_connection(mask);
	  continue;
	}
      }
#else
     /* For VMS, do the equivalent of Unix select() by 
      * using ASTs and event flags.
      */
      if( XPending(display) ) {
	XNextEvent(display, &control.event);
      } else {
	int not_found, i;
	struct connectRec *port = control.Xserver.next;

	not_found = 1;
        for( i=0; (port != NULL) && (i<control.remote_connected); i++ ) {
          if( ZPending(port->fd) ) {
	    respond_to_connection(&port->fd);
	    /* reset AST on connections for which events can be pending */
	    ZSelectAsyncInput(port->fd, XZ_ast, XZ_efn);
	    port = NULL;
	    not_found = 0;
	  } else {
	    port = port->next;
	  }
	}
        if( not_found ) {
	  /* Wait for event flag.  When set, clear it for the next go around
	   * and jump back to the while loop and process all the events that
	   * are pending.
	   */
	  sys$waitfr (XZ_efn);
	  sys$clref  (XZ_efn);
	  continue;
	} else {
	  continue;
	}
      }
#endif
#endif
    } else {
      XNextEvent(display, &control.event);
    }
    control.completed = 0;	
    /* priority event loop invoked by certain eventloop subroutines */
    /* usual application is to track mouse while buttons held down */
    if( control.event.type & control.priority ) {
      switch( control.mode ) {
      case COP:
	control_cursor();
	break;
      case VOP:
	control_color();
	break;
      case GOP:
	control_cgraph();
	break;
      case ZOP:
	control_pan();
	break;
      case SOP:
	control_blink();
	break;
      default:
	(void)fprintf(stderr, "do-nothing mode\n");
      }
    }
    /* priority events can speed up by returning to the while(1) */
    /* from here - by setting event processing completed flag */
    /* if a priority event has not claimed an exclusive */
    if( control.completed ) continue;
    /* check for buttonbox events */
    /* set the colors for the button menu bitmaps, in case it draws */
    (void)set_gc_with_background(&color.gcset.menu,
				 color.gcset.menu.background);
    if( (response = control_buttonbox(&control.event)) != 0 ) {
      /* respond to the buttonbox event (uses extern.h) */
      dispatch_select (response);
      /* ok, go directly for the next event */
      continue;
    }

    /* check the type of event */
    switch( control.event.type ) {
      /* if all or part of the window is newly exposed */
    case Expose:
      /* redraw entire window once (not parts of windows) */
      if( control.event.xexpose.count == 0 )
	redraw_window(control.event.xexpose.window);
      break;
    case MotionNotify:
      /* track with magnibox if in picturbox w/o shift key */
      if( control.event.xmotion.window == dispbox.ID ) {
	int shift;
	shift = ((control.event.xmotion.state & (ShiftMask | LockMask)) !=0);
	/* if magni_track or shift, but not both */
	if( control.magni_track != shift ) {
	  magnify_disp(&control.event, 1, control.coord_track || shift);
	} else if( control.coord_track && (!shift) )
	  /* else if coord_track alone, just show coords */
	  magnify_disp(&control.event, 0, 1);
      } else if( (control.event.xmotion.window == panbox.ID) &&
		 (control.event.xmotion.state & ShiftMask) ) {
	  magnify_pan(&control.event);
      }
      break;
    case ButtonPress:
      /* if a mouse button is pressed */
      if( control.event.xbutton.window == dispbox.ID ) {
	/* avoid duplication with same response under priority */
	if( !control.priority ) {
	  /* simple button pressed response depends on mode */
	  switch( control.mode ) {
	  case COP:
	    control_cursor();
	    break;
	  case VOP:
	    control_color();
	    break;
	  case ZOP:
	    control_pan();
	    break;
	  case SOP:
	    control_blink();
	    break;
	  default:
	    (void)fprintf(stderr, "do-nothing mode\n");
	  }
	}
      } else if( control.event.xbutton.window == cgraph.graph.ID ) {
	/* button pressed in color graph window */
	/* avoid duplication with priority response to same */
	if( !control.priority ) {
	  control_cgraph();
	}
      } else if( control.event.xbutton.window == panbox.ID ) {
	/* button pressed in pan window */
	/* avoid duplication with priority response to same */
	if( !control.priority ) {
	  control_pan();
	}
      } else if( control.event.xbutton.window == colorbox.ID ) {
	/* button pressed in color bar window */
	/* this window is not used with halftone */
	if( color.ncolors > 1 )
	  map_graphbox();
      }
      break;
    case KeyPress:
      /* if a key is pressed */
      key_response();
      break;
    case ConfigureNotify:
      /* if the main window is reconfigured */
      if( control.event.xconfigure.window == desktop.ID )
	adjust_desktop(&control.event.xconfigure);
      else if( control.event.xconfigure.window == graphbox.ID )
	adjust_graphbox(&control.event.xconfigure);
      break;
    default:
      break;
    }
  }
}


#ifdef VMS
#ifdef IMTOOL

/* XZ_ast - Asynchronous System Trap routine for X window and Z events.
 *	Simply set the appropriate event flag.
 */

void XZ_ast (int efn)
{
	sys$setef (efn);
}

#endif
#endif
                              
                                                               
                                                               
                                                               
                                                               
                                                               
                                                              

                                                               
                                 
