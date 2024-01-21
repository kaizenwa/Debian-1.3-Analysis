#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphtext.c (Color Graph Text)
 * Purpose:	Initialize or reset color bar and graph
 * Subroutine:	init_colorbar_label()		returns: void
 * Subroutine:	init_color_graph_label()	returns: void
 * Subroutine:	create_cgraph_box()		returns: Window
 * Xlib calls:	XMoveWindow(), XResizeWindow(), XMapWindow(), XCreateWindow()
 * Xlib calls:	XTextWidth()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 22 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/cgraph.h"

extern struct cgraphRec cgraph;

/*
 * Subroutine:	init_color_graph_label
 * Purpose:	Set labeling pieces according to the graph window size
 * Xlib calls:	XMoveWindow(), XResizeWindow(), XMapWindow()
 */
void init_color_graph_label ( )
{
  int new = 0;
  int move = 0;
  int resize = 0;
  Window create_cgraph_box();
  void init_color_graph_label();
  static void init_colorbar_label();

  if( cgraph.graphlabel.active == 0 )
    return;
  if( cgraph.vertical ) {
    if( (cgraph.graphlabel.gamma_ID != NULL) &&
        (cgraph.graphlabel.width != graphbox.width) )
      resize = 1;
    cgraph.graphlabel.width = graphbox.width - cgraph.barlabel.width;
    /* place boxes together (side-by-side) and centered */
    if( graphbox.width >= cgraph.graphlabel.four_limit ) {
      cgraph.graphlabel.geq_x =
	2 + ((graphbox.width - cgraph.graphlabel.four_limit) / 2);
      cgraph.graphlabel.red_x =
	cgraph.graphlabel.geq_x + cgraph.graphlabel.geq_width + 3;
      if( (cgraph.graphlabel.gamma_ID != NULL) &&
	  (cgraph.graphlabel.geq_active == 0) ) {
	/* prior state existed without use of geq window */
	move = 1;
	if( cgraph.graphlabel.geq_ID == NULL ) {
	  cgraph.graphlabel.geq_ID =
	    create_cgraph_box(cgraph.graphlabel.geq_x,
			      cgraph.graphlabel.geq_y,
			      cgraph.graphlabel.geq_width,
			      cgraph.graphlabel.geq_height, graphbox.display,
			      cgraph.graphlabel.gamma_ID, -1, CenterGravity);
	} else {
	  /* geq window exists but isn't mapped */
	  XMoveWindow(cgraph.graph.display, cgraph.graphlabel.geq_ID,
		      cgraph.graphlabel.geq_x, cgraph.graphlabel.geq_y);
	  XMapWindow(cgraph.graph.display, cgraph.graphlabel.geq_ID);
	}
      }
      cgraph.graphlabel.geq_active = 1;
    } else {
      cgraph.graphlabel.red_x = 
	((graphbox.width - cgraph.graphlabel.three_limit) / 2);
      if( cgraph.graphlabel.geq_ID && cgraph.graphlabel.active ) {
	XUnmapWindow(cgraph.graph.display, cgraph.graphlabel.geq_ID);
	move = 1;
      }
      cgraph.graphlabel.geq_active = 0;
    }
    cgraph.graphlabel.green_x =
      cgraph.graphlabel.red_x + cgraph.graphlabel.box_width + 3;
    cgraph.graphlabel.blue_x =
      cgraph.graphlabel.green_x + cgraph.graphlabel.box_width + 3;
    /* offset "min" and "max" from bottom, offset "max" from right */
    cgraph.graphlabel.min_y = graphbox.height - cgraph.graphlabel.minmax_yoff;
    cgraph.graphlabel.max_y = cgraph.graphlabel.min_y;
    cgraph.graphlabel.max_x = graphbox.width - cgraph.graphlabel.minmax_xoff;
    if( cgraph.graphlabel.gamma_ID == NULL ) {
      cgraph.graphlabel.gamma_ID =
	create_cgraph_box(cgraph.barlabel.width, 0,
			  cgraph.graphlabel.width, cgraph.graphlabel.height,
			  graphbox.display, graphbox.ID, -1, NorthGravity);
      cgraph.graphlabel.minmax_ID = graphbox.ID;
      new = 1;
    }
  } else {
    if( (cgraph.graphlabel.gamma_ID != NULL) &&
        (cgraph.graphlabel.height != graphbox.height) )
      resize = 1;
    cgraph.graphlabel.height = graphbox.height;
    /* place boxes together (one_above-the-other) and centered */
    if( cgraph.graphlabel.height >= cgraph.graphlabel.four_limit ) {
      cgraph.graphlabel.geq_y =
	(1 + cgraph.graphlabel.height - cgraph.graphlabel.four_limit) / 2;
      cgraph.graphlabel.red_y =
	cgraph.graphlabel.geq_y + cgraph.graphlabel.box_height + 3;
      if( (cgraph.graphlabel.gamma_ID != NULL) &&
	  (cgraph.graphlabel.geq_active == 0) ) {
	/* prior state existed without use of geq window */
	move = 1;
	if( cgraph.graphlabel.geq_ID == NULL )
	  cgraph.graphlabel.geq_ID =
	    create_cgraph_box(cgraph.graphlabel.geq_x,
			      cgraph.graphlabel.geq_y,
			      cgraph.graphlabel.geq_width,
			      cgraph.graphlabel.geq_height, graphbox.display,
			      cgraph.graphlabel.gamma_ID, -1, CenterGravity);
	else
	  /* geq window exists but isn't mapped */
	  XMoveWindow(cgraph.graph.display, cgraph.graphlabel.geq_ID,
		      cgraph.graphlabel.geq_x, cgraph.graphlabel.geq_y);
	XMapWindow(cgraph.graph.display, cgraph.graphlabel.geq_ID);
      }
      cgraph.graphlabel.geq_active = 1;
    } else {
      cgraph.graphlabel.red_y =
	1 + ((cgraph.graphlabel.height - cgraph.graphlabel.three_limit) / 2);
      if( cgraph.graphlabel.geq_ID && cgraph.graphlabel.active ) {
	XUnmapWindow(cgraph.graph.display, cgraph.graphlabel.geq_ID);
	move = 1;
      }
      cgraph.graphlabel.geq_active = 0;
    }      
    cgraph.graphlabel.green_y =
      cgraph.graphlabel.red_y + cgraph.graphlabel.box_height + 3;
    cgraph.graphlabel.blue_y =
      cgraph.graphlabel.green_y + cgraph.graphlabel.box_height + 3;
    /* offset "min" from bottom */
    cgraph.graphlabel.min_y = graphbox.height - cgraph.graphlabel.minmax_yoff;
    if( cgraph.graphlabel.gamma_ID == NULL ) {
      cgraph.graphlabel.gamma_ID =
	create_cgraph_box((int)graphbox.width - cgraph.graphlabel.width, 0,
			  cgraph.graphlabel.width, cgraph.graphlabel.height,
			  graphbox.display, graphbox.ID, -1, EastGravity);
      cgraph.graphlabel.minmax_ID = graphbox.ID;
      new = 1;
    }
  }
  if( new ) {
    if( cgraph.graphlabel.geq_active )
      cgraph.graphlabel.geq_ID =
	create_cgraph_box(cgraph.graphlabel.geq_x, cgraph.graphlabel.geq_y,
			  cgraph.graphlabel.geq_width,
			  cgraph.graphlabel.geq_height, graphbox.display,
			  cgraph.graphlabel.gamma_ID, -1, CenterGravity);
    cgraph.graphlabel.red_ID =
      create_cgraph_box(cgraph.graphlabel.red_x, cgraph.graphlabel.red_y,
			cgraph.graphlabel.box_width,
			cgraph.graphlabel.box_height,
			graphbox.display, cgraph.graphlabel.gamma_ID,
			color.hard.red, CenterGravity);
    cgraph.graphlabel.green_ID =
      create_cgraph_box(cgraph.graphlabel.green_x, cgraph.graphlabel.green_y,
			cgraph.graphlabel.box_width,
			cgraph.graphlabel.box_height,
			graphbox.display, cgraph.graphlabel.gamma_ID,
			color.hard.green, CenterGravity);
    cgraph.graphlabel.blue_ID =
      create_cgraph_box(cgraph.graphlabel.blue_x, cgraph.graphlabel.blue_y,
			cgraph.graphlabel.box_width,
			cgraph.graphlabel.box_height,
			graphbox.display, cgraph.graphlabel.gamma_ID,
			color.hard.blue, CenterGravity);
    init_colorbar_label();
    XMapSubwindows(graphbox.display, cgraph.graphlabel.gamma_ID);
  } else {
    if( resize ) {
      XResizeWindow(graphbox.display, cgraph.graphlabel.gamma_ID,
		    cgraph.graphlabel.width, cgraph.graphlabel.height);
    }
    if( move ) {
      XMoveWindow(graphbox.display, cgraph.graphlabel.red_ID,
		  cgraph.graphlabel.red_x, cgraph.graphlabel.red_y);
      XMoveWindow(graphbox.display, cgraph.graphlabel.green_ID,
		  cgraph.graphlabel.green_x, cgraph.graphlabel.green_y);
      XMoveWindow(graphbox.display, cgraph.graphlabel.blue_ID,
		  cgraph.graphlabel.blue_x, cgraph.graphlabel.blue_y);
    }
  }
}

/*
 * Subroutine:	init_colorbar_label
 * Xlib calls:	XResizeWindow()
 */
static void init_colorbar_label ( )
{
  Window create_cgraph_box();
  if( (cgraph.graphlabel.active == 0) || (cgraph.barlabel.max_ID != NULL) )
    return;
  if( cgraph.vertical ) {
    cgraph.barlabel.max_ID =
      create_cgraph_box(0, 0, cgraph.barlabel.width, cgraph.barlabel.height,
			graphbox.display, graphbox.ID, -1, NorthWestGravity);
    cgraph.barlabel.min_ID =
      create_cgraph_box(0, (int)graphbox.height - cgraph.barlabel.height,
			cgraph.barlabel.width, cgraph.barlabel.height,
			graphbox.display, graphbox.ID, -1, SouthWestGravity);
    /* 3 digits are centered, "0" uses same right justification */
    cgraph.barlabel.max_x = cgraph.graphlabel.box_x +
      ((3 + cgraph.barlabel.width - cgraph.barlabel.base_width) / 2);
    cgraph.barlabel.min_x = cgraph.barlabel.max_x +
      XTextWidth(cgraph.fontstruct, "20", 2);
  } else {
    cgraph.barlabel.min_ID =
      create_cgraph_box(0, (int)graphbox.height - cgraph.barlabel.height,
			cgraph.barlabel.width, cgraph.barlabel.height,
			graphbox.display, graphbox.ID, -1, SouthWestGravity);
    cgraph.barlabel.max_ID =
      create_cgraph_box((int)graphbox.width - cgraph.graphlabel.width,
			(int)graphbox.height - cgraph.barlabel.height,
			cgraph.barlabel.width, cgraph.barlabel.height,
			graphbox.display, graphbox.ID, -1, SouthEastGravity);
    cgraph.barlabel.max_y = cgraph.graphlabel.box_y +
      ((cgraph.barlabel.height - cgraph.graphlabel.box_height) / 2);
    cgraph.barlabel.min_y = cgraph.barlabel.max_y;
  }
}

/*
 * Subroutine:	create_cgraph_box
 * Purpose:	Create window with window gravity and optional colored border
 * Xlib call:	XCreateWindow()
 */
Window create_cgraph_box ( x, y, width, height, display, parent,
			   border_color, window_gravity )
     int x, y, width, height;
     Display *display;
     Window parent;
     int border_color;
     int window_gravity;
{
  XSetWindowAttributes attributes;
  Window wndw;

  attributes.win_gravity = window_gravity;
  attributes.background_pixel = color.hard.std_white;
  attributes.event_mask = ExposureMask; 
  if( border_color < 0 ) {
    wndw = XCreateWindow(display, parent, x, y, (unsigned int)width,
			 (unsigned int)height, (unsigned int)0,
			 CopyFromParent, CopyFromParent, CopyFromParent,
			 CWBackPixel | CWWinGravity | CWEventMask,
			 &attributes);
  } else {
    unsigned long valuemask;
    attributes.border_pixel = (unsigned long)border_color;
    valuemask = CWBackPixel | CWWinGravity | CWEventMask | CWBorderPixel;
    wndw = XCreateWindow(display, parent, x, y, (unsigned int)width,
			 (unsigned int)height, (unsigned int)1,
			 CopyFromParent, CopyFromParent, CopyFromParent,
			 valuemask, &attributes);
  }
  return( wndw );
}
