#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	wndwinit.c (Window Initialize)
 * Purpose:	Create (and raise) the various windows used by ximage.
 * Declared:	int screen_width, screen_height;
 * Subroutine:  init_windows1()			returns: void
 * Subroutine:  init_windows2()			returns: void
 * Subroutine:	create_graphbox()		returns: void
 * Subroutine:	redraw_window()			returns: void
 * Subroutine:	raise_windows()			returns: void
 * Xlib calls:	DisplayWidth(), DisplayHeight(), XMapWindow(), XRaiseWindow()
 * Xlib calls:	XSetTransientForHint(), XSelectInput()
 * Xlib calls:	XCreateImage(), XDestroyImage()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	      28 January 1989
 *		{1} MVH			Revised for X11		26 April 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/constant.h"		/* define codes */
#include "hfiles/define.h"		/* YES, NO, MIN, MAX and more */
#include "hfiles/struct.h"		/* all struct record types */
#include "hfiles/extern.h"		/* major declared structs */

extern struct windowRec desktop;

/* Root window info used by many routines */
Window root;
int screen_width, screen_height;
int border_color = -1;
char *border_color_name = NULL;
static int cmd_argc;		/* i: command line arg count param */
static char **cmd_argv;		/* i: command line args */

/*
 * Subroutine:	init_windows1
 * Purpose:	Send request for desktop area to screen's window manager
 * Uses:	init_desktop() in WndwDesktop.c
 * Uses:	set_configuration() in WndwConfig.c
 * Uses:	create_window() in WndwCreate.c
 * Xlib calls:	DisplayWidth(), DisplayHeight();
 */
void init_windows1 ( argc, argv )
     int argc;			/* i: command line arg count param */
     char **argv;		/* i: command line args */
{
  static void set_window_basics();
  void init_desktop(), create_window();

  /* set display screen parameters for all to see */
  root = DefaultRootWindow(desktop.display);
  screen_width = DisplayWidth(desktop.display, desktop.screen);
  screen_height = DisplayHeight(desktop.display, desktop.screen);
  cmd_argc = argc;
  cmd_argv = argv;
  /* determine how much of the screen to use up and get default border color */
  init_desktop();
  set_window_basics(&desktop, root,
		    (int)color.hard.std_black, (int)color.hard.std_white,
		    NULL);
  create_window(&desktop, "SAOimage", argc, argv, 0, 1);
}

/*
 * Subroutine:	init_windows2
 * Purpose:	Create the actual windows used by SAOimage.
 * PreState:	Recieved expose event for desktop window, colors known
 * Uses:	configure_windows() in WndwConfig.c
 * Uses:	get_window_dimensions(), create_window() in WndwCreate.c
 */
void init_windows2 ( )
{
  static void init_window_basics(), set_border_color();
  void get_window_dimensions(), configure_windowgroup(), create_window();

  /* install runtime environment parameters for all windows */
  if( border_color_name != NULL )
    set_border_color(border_color_name);
  if( border_color < 0 )
    border_color = (int)color.hard.std_black;
  init_window_basics(border_color);
  /* wait for expose event of desktop */
  get_window_dimensions(&desktop, 1, 1);
  /* set up the window configurations */
  configure_windowgroup (1);
  /* create the windows */
  /* create the main display window (must be first to be group leader) */
  create_window(&dispbox, "display", cmd_argc, cmd_argv, 1, 1);
  /* create the color bar window */
  create_window(&colorbox, "color", cmd_argc, cmd_argv, 1, 1);
  /* create the magnifier window */
  create_window(&magnibox, "magnifier", cmd_argc, cmd_argv, 1, 1);
  /* create the pan window */
  create_window(&panbox, "pan", cmd_argc, cmd_argv, 1, 1);
  /* create the buttonbox window */
  create_window(&btnbox, "buttons", cmd_argc, cmd_argv, 1, 1);
}

/*
 * Subroutine:	create_graphbox
 * Purpose:	Create the graphbox window (when needed, i.e. well after init)
 * Xlib calls:	XSetTransientForHint(), XSelectInput()
 */
void create_graphbox ( )
{
  void configure_graphbox(), create_window();

  /* set location of graphbox in relation to colorbox */
  configure_graphbox();
  /* create the color graph window (not mapped) and connect to desktop */
  create_window(&graphbox, "RGBgraph", cmd_argc, cmd_argv, 1, 0);
}

/*
 * Subroutine:	redraw_window
 * Purpose:	Redraw the specified window.
 * Called by:	control_event_loop in MainEvent.c on an Expose event.
 * Note:	The btnbox handles its own expose events for the buttons.
 */
void redraw_window ( wndwID )
     Window wndwID;
{
  void disp_dispbox(), redraw_magnifier(), draw_colorbar(), label_colorbar();
  void disp_panbox(), draw_cgraph(), label_color_graph(), show_filename();
  void display_graphbox();

  if( wndwID == dispbox.ID ) {
    disp_dispbox();
  } else if( wndwID == magnibox.ID ) {
    redraw_magnifier();
  } else if( wndwID == colorbox.ID ) {
    draw_colorbar(0);
  } else if( wndwID == graphbox.ID ) {
    /* window may be mapped by window manager */
    if( graphbox.active == 0 )
      display_graphbox(0, 0);
    else {
      draw_cgraph(0, 0);
      label_color_graph();
      draw_colorbar(1);
      label_colorbar();
    }
  } else if( wndwID == panbox.ID ) {
    disp_panbox();
  } else if( wndwID == desktop.ID )
    show_filename();
}

/*
 * Subroutine:	raise_windows
 * Purpose:	Raise the window should they have been obscured
 * Xlib calls:	XRaiseWindow()
 */
void raise_windows ( )
{
  XRaiseWindow(desktop.display, desktop.ID);
  if( graphbox.active )
    XRaiseWindow(graphbox.display, graphbox.ID);
}

/*
 * Subroutine:	init_window_basics, set_window_basics
 * Purpose:	Set the runtime environment parameters
 */
static void init_window_basics ( border_pixel )
     int border_pixel;
{
  XImage *ximage;
  static void set_window_basics();

  ximage = XCreateImage(desktop.display, color.visual, color.screen_depth,
			dispbox.image.format, 0, malloc(4), 2, 2,
			dispbox.image.bitmap_pad, (unsigned)2);
  set_window_basics(&dispbox, desktop.ID, border_pixel,
		    (int)desktop.attrs.background_pixel, ximage);
  set_window_basics(&panbox, desktop.ID, border_pixel,
		    (int)desktop.attrs.background_pixel, ximage);
  set_window_basics(&magnibox, desktop.ID, border_pixel,
		    (int)desktop.attrs.background_pixel, ximage);
  set_window_basics(&btnbox, desktop.ID, border_pixel,
		    (int)desktop.attrs.background_pixel, ximage);
  set_window_basics(&colorbox, desktop.ID, border_pixel,
		    (int)desktop.attrs.background_pixel, ximage);
  set_window_basics(&graphbox, root, (int)desktop.attrs.border_pixel,
		    (int)desktop.attrs.background_pixel, ximage);
  (void)XDestroyImage(ximage);
}
static void set_window_basics ( window, parent, border_pixel, background,
			        template )
     struct windowRec *window;
     Window parent;
     int border_pixel;
     int background;
     XImage *template;
{
  window->display = color.display;
  window->screen = color.screen;
  window->parent = parent;
  window->depth = color.screen_depth;
  window->image.depth = color.screen_depth;
  window->visual = color.visual;
  window->attrs.colormap = color.colormap;
  if( color.map.private_used )
    window->valuemask |= CWColormap;
  if( color.colormap_mode != VOP_Halftone )
    window->image.format = ZPixmap;
  else
    window->image.format = XYBitmap;
  if( background >= 0 ) {
    window->attrs.background_pixel = (unsigned long)background;
    window->valuemask |= CWBackPixel;
  }
  if( border_pixel >= 0 ) {
    window->attrs.border_pixel = (unsigned long)border_pixel;
    window->valuemask |= CWBorderPixel;
  }
  if( template != NULL )
    window->image.f = template->f;
}

/*
 * Subroutine:	set_border_color
 */
static void set_border_color ( color_name )
     char *color_name;
{
  XColor colordef, ideal;

  /* set up default border color for all windows */
  if( color_name != NULL ) {
    if( (color.screen_depth > 2) &&
      XAllocNamedColor(desktop.display, color.colormap,
		       color_name, &colordef, &ideal) ) {
      border_color = (int)colordef.pixel;
    } else {
      if( (strcmp(color_name, "white") == 0) )
	border_color = (int)WhitePixel(desktop.display, desktop.screen);
      else if( (strcmp(color_name, "black") == 0) )
	border_color = (int)BlackPixel(desktop.display, desktop.screen);
    }
  }
}
