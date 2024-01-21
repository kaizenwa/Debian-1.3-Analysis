#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphctrl.c (Color Graph Control)
 * Purpose:	Manipulate the color graph
 * Subroutine:	map_graphbox()			returns: void
 * Subroutine:	display_graphbox()			returns: void
 * Subroutine:	control_cgraph()		returns: void
 * Xlib calls:	XUnmapWindow(), XMapWindow()
 * Xlib calls:	XSync(), XCheckWindowEvent(), XStoreColors()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 11 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/cgraph.h"

#define BUTTONMASK (Button1Mask|Button2Mask|Button3Mask)
#define EVENTMASK (ButtonPressMask|ButtonReleaseMask|ButtonMotionMask)

#include "defs/cgraph.def"	/* declare the cgraph struct */

/*
 * Subroutine:	map_graphbox
 * Purpose:	Varify size and get everything set when mapping the graphbox
 * Xlib calls:	XUnmapWindow(), XMapWindow()
 */
void map_graphbox ( )
{
  int init;
  void create_graphbox(), display_graphbox();

  if( graphbox.active ) {
    XUnmapWindow(graphbox.display, graphbox.ID);
    graphbox.active = 0;
    cgraph.inactive = 1;
  } else {
    if( graphbox.ID == NULL ) {
      init = 1;
      create_graphbox();
    } else
      init = 0;
    XMapWindow(graphbox.display, graphbox.ID);
    display_graphbox(init, 1);
  }
}

/*
 * Subroutine:	display_graphbox
 * Purpose:	Put up the graphbox when it is mapped or reexposed
 */
void display_graphbox ( init, wait )
     int init;	/* i: initialize graphbox parameters */
     int wait;	/* i: wait for result of mapping before getting dimensions */
{
  void draw_colorbar(), draw_cgraph(), label_colorbar();
  void get_window_dimensions(), adjust_color_graph(), init_color_graph_label();
  void init_graph_colorbar(), adjust_graph_colorbar(), label_color_graph();

  get_window_dimensions(&graphbox, wait, 1);
  graphbox.active = 1;
  cgraph.inactive = 0;
  if( init ) {
    init_color_graph_label();
    adjust_color_graph();
    init_graph_colorbar();
  } else {
    adjust_color_graph();
    adjust_graph_colorbar();
  }
  draw_cgraph(1, 0);
  label_color_graph();
  label_colorbar();
  draw_colorbar(1);
}

/*
 * Subroutine:	control_cgraph
 * Purpose:	Mouse controlled interaction to alter color table.
 * Called by:	control_event_loop in MainEvent.c
 * Xlib calls:	XSync(), XCheckWindowEvent(), XStoreColors()
 */
void control_cgraph ( )
{
  static int not_active = 1;
  static int oldmode = MOP;
  int drop_cgraph_vertex();
  void draw_cgraph(), move_cgraph_vertices(), get_cgraph_vertex();
  void highlight_active_cgraph_vertex(), make_cellstore_from_tables();
  void replace_draw_queue_end();

  /* make sure this an event which we want */
  if( (control.event.xbutton.window != cgraph.graph.ID) &&
      (control.priority == 0) )
    return;
  /* we assume this event was meant to be fielded exclusively here */
  control.completed = 1;
  switch( control.event.type ) {
  case MotionNotify:
    /* get only the most recent move */
    XSync(dispbox.display, 0);
    while( XCheckWindowEvent(cgraph.graph.display, cgraph.graph.ID,
			     ButtonMotionMask, &control.event) );
    if( (control.event.xmotion.state & BUTTONMASK) == 0 ) {
      /* if no buttons are down, we must be done */
      control.priority = 0;
      control.mode = oldmode;
      return;
    }
    /* alter table(s) and graph */
    move_cgraph_vertices(control.event.xmotion.x, control.event.xmotion.y);
    /* make color cellmap and send colors to the display */
    make_cellstore_from_tables(&color);
    XStoreColors(color.display, color.colormap,
		 color.cellstore, color.ncolors);
    /* shift will cause the graph to track the changes as well */
    if( ((control.event.xmotion.state & (ShiftMask | LockMask)) !=0) ^
    	(control.tracking !=0) )
      draw_cgraph (0, 1);
    break;
  case ButtonPress:
    /* if one of the three mouse buttons was pressed */
    if( (control.event.xbutton.state & BUTTONMASK) == 0 ) {
      /* if no mouse button was previously down */
      if( control.event.xbutton.state & ControlMask ) {
	/* lone button with control key means delete vertex */
	if( drop_cgraph_vertex(&control.event) == 0 )
	  /* if nothing is deleted, do nothing */
	  return;
      } else if( (control.event.xbutton.button == Button1) ||
		 (control.event.xbutton.button == Button2) ||
		 (control.event.xbutton.button == Button3) ) {
	/* first button to grab a vertex starts priority mode */
	oldmode = control.mode;
	control.mode = GOP;
	/* events to send through here in any case */
	control.priority = EVENTMASK;
      } else {
	return;
      }
    } else if( not_active || (control.event.xbutton.state & ControlMask) )
      /* don't respond if things don't look right */
      return;
    /* if this was not a delete command, it is a grab vertex command */
    if( (control.event.xbutton.state & ControlMask) == 0 ) {
      not_active = 0;
      get_cgraph_vertex(&control.event);
    }
    /* make color cellmap and send colors to the display */
    make_cellstore_from_tables(&color);
    XStoreColors(color.display, color.colormap,
		 color.cellstore, color.ncolors);
    /* redraw the graph with newly grabbed vertex */
    draw_cgraph(0, 1);
    highlight_active_cgraph_vertex();
    break;
  case ButtonRelease:
    /* draw the color graph when any button is released */
    draw_cgraph(0, 0);
    switch( control.event.xbutton.button ) {
    case Button1:
      cgraph.red.active = 0;
      if( (control.event.xbutton.state & BUTTONMASK) == Button1Mask )
	not_active = 1;
      else if( cgraph.queue[2] == &cgraph.red )
	replace_draw_queue_end();
      break;
    case Button2:
      cgraph.green.active = 0;
      if( (control.event.xbutton.state & BUTTONMASK) == Button2Mask )
	not_active = 1;
      else if( cgraph.queue[2] == &cgraph.green )
	replace_draw_queue_end();
      break;
    case Button3:
      cgraph.blue.active = 0;
      if( (control.event.xbutton.state & BUTTONMASK) == Button3Mask )
	not_active = 1;
      else if( cgraph.queue[2] == &cgraph.blue )
	replace_draw_queue_end();
      break;
    }
    if( not_active ) {
      /* if the last button was just released */
      control.priority = 0;
      control.mode = oldmode;
    } else
      /* if there are still active vertices, rehighlight them */
      highlight_active_cgraph_vertex();
    break;
  default:
    /* this event was not meant for us */
    control.completed = 0;
    break;
  }
}
