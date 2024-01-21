#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wndwcre.c (Window Create)
 * Purpose:	Create a window with proper X11 protocol
 * Subroutine:	create_window()			returns: void
 * Subroutine:	get_window_dimensions()		returns: void
 * Xlib calls:	XCreateWindow(), XMapWindow()
 * Xlib calls:	XSetStandardProperties(), XSetWMHints()
 * Xlib calls:	XWindowEvent(), XIfEvent(), XGetGeometry()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		30 April 1989
 *		{1} David Muchmore (astro.umontreal.ca) for SGI  23 June 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* window manager hint stuff */
#include "hfiles/window.h"

#ifdef VMS
#ifdef IMTOOL
/* get the VMS event flag number that will be used to detect events
 * from all windows and mailbox connections */
extern int XZ_efn;
#endif
#endif

/*
 * Subroutine:	create_window
 * Purpose:	Send window request to X server or window manager
 * Xlib calls:	XCreateWindow(), XMapWindow()
 * Xlib calls:	XSetStandardProperties(), XSetWMHints()
 * PostState:	The first created window (dispbox) becomes the group leader
 */
void create_window ( window, title, argc, argv, set, map )
     struct windowRec *window;	/* i: window info record */
     char *title;		/* i: name for use by window manager */
     int argc;			/* i: command line arg count param */
     char **argv;		/* i: command line args */
     int set;			/* i: adopt the parameters untested */
     int map;			/* i: map window immediately */
{
  XWMHints wmhints;
  void exit_errmsg();

  /* Create the window */
  window->ID =
    XCreateWindow(window->display, window->parent, window->hints.x,
		  window->hints.y, window->hints.width, window->hints.height,
		  window->bdrwidth, CopyFromParent, InputOutput,
		  window->visual, window->valuemask, &window->attrs);
/* Old code (prior to SGI version which reserves its own colormap     *
 *		  CopyFromParent, window->valuemask, &window->attrs); */

  if( !window->ID ) {
    if( title != NULL )
      (void)fprintf(stderr, "Window: %s - ", title);
    exit_errmsg("XCreateWindow window failed");
  }
  /* Hint desired dimensions or adopt them immediately */
  if( window->hints.flags )
    XSetStandardProperties(window->display, window->ID, title, NULL, None,
			   argv, argc, &window->hints);

  if( window->attrs.event_mask & KeyPressMask ) {
    /* Tell window manager to handle keyboard focus for us */
    wmhints.input = True;
    wmhints.flags = InputHint;
    XSetWMHints(window->display, window->ID, &wmhints);
  }

  if( set ) {
    window->x = window->hints.x;
    window->y = window->hints.y;
    window->width = window->hints.width;
    window->height = window->hints.height;
    window->xwidth = window->width - (window->xzero + window->xzero);
    window->yheight = window->height - (window->yzero + window->yzero);
    window->rightx = window->x + window->width + window->bdrtotal;
    window->lowery = window->y + window->height + window->bdrtotal;
  }
  if( map )
    XMapWindow(window->display, window->ID);

#ifdef VMS
#ifdef IMTOOL
  {
    int	all_events = 0xFFFFFFFF;
    void XZ_ast();
    XSelectAsyncInput(window->display, window->ID, all_events, XZ_ast, XZ_efn);
  }
#endif
#endif
}

/*
 * Subroutine:	check_expose
 * Purpose:	Function callable by XIfEvent from within xlib to check if
 *		event is Expose for given window.
 * Note:	Calling sequence is fixed, no xlib calls may be made.
 */
Bool check_expose ( display, event, arg )
     Display *display;		/* i: server ID */
     XEvent *event;		/* i: pointer to xlib event */
     char *arg;			/* i: generic pointer passed to XIfEvent */
{
  struct windowRec *window;	/* i: arg is actually a struct pointer */

  window = (struct windowRec *)arg;
  if( (display == window->display) &&
      (event->type == Expose) &&
      (event->xexpose.window == window->ID) )
    return( True );
  else
    return( False );
}

/*
 * Subroutine:	get_window_dimensions
 * Purpose:	Get window dimensions after window is first mapped
 * PreState:	Window's existence announced by Expose event.
 * PostState:	Window struct's dimensions filled in.
 * Xlib calls:	XWindowEvent(), XIfEvent(), XGetGeometry()
 * Method:	If disposing the Expose events, we dispose of all of the
 *		window,s expose events (they count down to 0).
 */
void get_window_dimensions ( window, wait, discard )
     struct windowRec *window;
     int wait;		/* i: look for an expose event for this window */
     int discard;	/* i: dispose of the expose event */
{
  Window parent;	/* l: returned parent of this window */
  XEvent event;		/* l: event
  Bool check_expose();	/* l: function to identify window's expose event

  /* if waiting for an Expose event before querying window */
  if( wait ) {
    if( discard ) {
      do {
	XWindowEvent(window->display, window->ID, ExposureMask, &event);
      } while( event.xexpose.count != 0 );
    } else {
      XIfEvent(window->display, &event, check_expose, window);
    }
  }
  (void)XGetGeometry(window->display, window->ID, &parent,
		     &window->x, &window->y, &window->width, &window->height,
		     &window->bdrwidth, &window->depth);
  window->bdrtotal = 2 * window->bdrwidth;
  window->xwidth = window->width - (window->xzero + window->xzero);
  window->yheight = window->height - (window->yzero + window->yzero);
}
