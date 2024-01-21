#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clrctrl.c (Color Control)
 * Purpose:	Control colors
 * Subroutine:	select_color()			returns: void
 * Subroutine:	control_color()			returns: void
 * Subroutine:	invert_rgb()			returns: void
 * Subroutine:	reinit_color()			returns: void
 * Xlib call:	XStoreColors()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define NULL */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/cgraph.h"

extern struct cgraphRec cgraph;

#define BUTTONMASK (Button1Mask|Button2Mask|Button3Mask)
#define EVENTMASK (ButtonPressMask|ButtonReleaseMask|ButtonMotionMask)

/*
 * Subroutine:	select_color
 * Purpose:	Things to do when a buttonbox color menu button is selected
 */
void select_color ( )
{
  static int overlay = 0;
  static int cells = 0;
  static int mode = VOP_PseudoColor;
  void invert_rgb(), reinit_color();
  static void new_color_table();

  if( control.response[0] == VOP ) {
    switch( control.response[1] ) {
    case 0:
      /* 0 is main menu, change only main mode */
      break;
    case VOP_Invert:
      /* invert is a toggle */
      invert_rgb();
      /* change records to show reverse of previous mode */
      color.inverse ^= 1;
      break;
    case VOP_ContBias:
    case VOP_ThreshSat:
    case VOP_gamma:
      color.control_mode = control.response[1];
      break;
    case VOP_Overlay:
      if( color.cells.overlay ) {
	color.cells.wanted = (color.cells.wanted + 2) * 2;
	color.cells.overlay = 0;
      } else {
	color.cells.wanted = (color.cells.wanted / 2) - 2;
	color.cells.overlay = 1;
      }
      reinit_color (1);
      break;
    case VOP_Halftone:
      if( color.colormap_mode == VOP_Halftone ) {
	color.colormap_mode = mode;
	if( cells <= 1 )
	  color.cells.wanted = 200;
	else
	  color.cells.wanted = cells;
	color.cells.overlay = overlay;
      } else {
	mode = color.colormap_mode;
	overlay = color.cells.overlay;
	cells = color.cells.wanted;
	color.colormap_mode = VOP_Halftone;
	color.cells.wanted = 1;
	color.cells.overlay = 0;
      }
      reinit_color(1);
      break;
    default:
      break;
    }
    control.mode = VOP;
  } else if( control.response[0] == MOP ) {
    if( (control.response[1] != 0) &&
	(control.response[1] != MOP_SignOff) ) {
      /* install a new color set */
      new_color_table(control.response[1]);
    }
  }
}

/*
 * Subroutine:	control_color
 * Purpose:	Alter color based on mouse events
 */
void control_color ( )
{
  static int oldmode =VOP;
  static int track;
  static long motionmask = 0;
  int released_button_mask;
  Bool specificmotion_in_disp();	/* Function for XCheckIfEvent */
  void vary_colors(), draw_cgraph();

  if( color.ncolors <= 1 )
    /* trap halftone mode until we have something to do */
    return;
  switch( control.event.type ) {
  case ButtonPress:
    if( (control.event.xbutton.state & BUTTONMASK) == 0 ) {
      /* if this is the beginning of a colorful relationship */
      /* store current mode */
      oldmode = control.mode;
      control.mode = VOP;
      /* events to send through here in any case */
      control.priority = EVENTMASK;
    }
    if( control.event.xbutton.button == Button1 ) {
      motionmask |= Button1Mask;
    } else if( control.event.xbutton.button == Button2 ) {
      motionmask |= Button2Mask;
    } else if( control.event.xbutton.button == Button3 ) {
      motionmask |= Button3Mask;
    } else
      return;
  case MotionNotify:
    control.completed = 1;
    /* shift xor track will cause the graph to track the changes */
    if( ((control.event.xmotion.state & (ShiftMask | LockMask)) !=0) ^
    	(control.tracking !=0) )
      track = 1;
    else
      track = 0;
    /* get only the most recent move */
    XSync(control.event.xmotion.display, 0);
    while( XCheckIfEvent(control.event.xmotion.display, &control.event,
			 specificmotion_in_disp, (char *)(&motionmask)) );
    vary_colors(&control.event, color.control_mode, track,
		control.event.xmotion.x, control.event.xmotion.y,
		(int)dispbox.width, (int)dispbox.height);
    break;
  case ButtonRelease:
    /* this event could only have been meant for us */
    control.completed = 1;
    if( !track )
      /* update the color graph if any button is released and not tracking*/
      draw_cgraph(0, 0);
    /* which buttons were being held before? */
    switch( control.event.xbutton.button ) {
    case Button1:
      released_button_mask = Button1Mask;
      break;
    case Button2:
      released_button_mask = Button2Mask;
      break;
    case Button3:
      released_button_mask = Button3Mask;
      break;
    default:
      return;
    }
    motionmask ^= released_button_mask;
    /* if that was the last button, we are done */
    if( (control.event.xbutton.state & BUTTONMASK) == released_button_mask ) {
      control.priority = 0;
      control.mode = oldmode;
      motionmask = 0;
    }
    break;
  default:
    break;
  }
}

/*
 * Subroutine:	new_color_table
 * Purpose:	Install a new pre-defined color table
 */
static void new_color_table ( map_code )
     int map_code;
{
  int fetch_colortable();
  void make_cellstore_from_tables(), draw_cgraph(), label_gamma();
  void set_submenu_toggle();

  if( fetch_colortable(&color, map_code, img.filename) ) {
    /* unset the inverse flag if it is set */
    if( color.inverse ) {
      set_submenu_toggle(VOP, VOP_Invert, 0);
      color.inverse = 0;
    }
    make_cellstore_from_tables(&color);
    /* send colors to the display */
    XStoreColors(color.display, color.colormap,
		 color.cellstore, color.ncolors);
    /* draw the graph of the colors, and relabel the gamma values */
    if( graphbox.active ) {
      draw_cgraph(1, 0);
      label_gamma(1, 1, 1);
    }
  }
}

/*
 * Subroutine:	invert_rgb
 * Purpose:	Reverse all colors at once (respond to invert button)
 * Xlib call:	XStoreColors()
 */
void invert_rgb ( )
{
  void make_cellstore_from_cellmaps(), draw_cgraph();
  static void invert_table();

  /* invert color tables and remake storemap */
  invert_table(&color.ctable.red);
  invert_table(&color.ctable.green);
  invert_table(&color.ctable.blue);
  make_cellstore_from_cellmaps (&color);  
  XStoreColors(color.display, color.colormap,
	       color.cellstore, color.ncolors);
  /* redraw the color graph */
  if( graphbox.active )
    draw_cgraph(1, 0);
}

static void invert_table ( table )
     struct subtableRec *table;
{
  double *intensity;		/* l: intensity by table vertex or cell */
  int i, cnt;

  intensity = table->intensity;
  cnt = table->vertex_cnt;
  for( i=0; i<cnt; i++ )
    intensity[i] = 1.0 - intensity[i];
  intensity = table->cellmap;
  cnt = table->map_sz;
  for( i=0; i<cnt; i++ )
    intensity[i] = 1.0 - intensity[i];
}

/*
 * Subroutine:	reinit_color
 * Purpose:	Make necessary adjustment after a request to change use of
 *		color resources
 */
void reinit_color ( disp )
     int disp;
{
  void init_color(), draw_colorbar(), new_scalemap(), adjust_color_graph();
  void map_panbox(), map_dispbox(), disp_panbox(), redraw_magnifier();
  void adjust_main_colorbar(), label_colorbar(), disp_dispbox(), draw_cgraph();
  void adjust_graph_colorbar(), clear_blink(), map_graphbox(), set_magnifier();

  init_color(&color, 0);
  cursor.overwrites_image_data = (!color.cursor_overlay);
  set_magnifier();
  if( disp ) {
    /* make the new lookup table */
    new_scalemap();
    /* refill display buffers with rescaled values */
    redraw_magnifier();
    map_panbox();
    map_dispbox();
    disp_panbox();
    disp_dispbox();
  }
  adjust_main_colorbar();
  draw_colorbar(0);
  /* force the blinking parameters to be rechecked */
  clear_blink();
  if( cgraph.graph.ID != NULL ) {
    if( !color.single_plane ) {
      /* sometimes redundant: it also happens when the window is mapped */
      adjust_color_graph();
      adjust_graph_colorbar();
    }
    if( graphbox.active ) {
      if( (color.ncolors <= 1) || color.monochrome )
	/* unmap the graphbox */
	map_graphbox();
      else {
	draw_cgraph(1, 0);
	draw_colorbar(1);
	label_colorbar();
      }
    }
  }
}
