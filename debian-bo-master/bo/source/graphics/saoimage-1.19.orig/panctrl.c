#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	panctrl.c (Pan Control)
 * Purpose:	Respond to inputs which control pan and zoom
 * Subroutine:	control_pan()			returns: void
 * Subroutine:	select_pan()			returns: void
 * Xlib calls:	XSync();
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 31 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/define.h"	/* define MIN, MAX, etc */
#include "hfiles/struct.h"	/* all struct record types */
#include "hfiles/extern.h"	/* major declared structs */

/*
 * Subroutine:	control_pan
 * Purpose:	Respond to mouse commands in pan mode
 */
void control_pan ( )
{
  static int moved = 0;
  static int buttons = 0;
  static int downbutton = -1;
  static int oldmode = ZOP;
  float bufx, bufy;
  Bool statedmotion_in_disp();		/* Function for XCheckIfEvent */
  void i_transform(), d_transform(), draw_scope(), set_tdisp(), reset_tdisp();
  void panedge_zoom(), new_pancursor(), new_display(), show_dispcoords();
  void print_table(), draw_magnifier();

  /* if mouse button pressed (and no others), start tracking */
  switch( control.event.type ) {
  case ButtonPress:
    /* check for patch request */
    if( control.event.xbutton.button == Button3 ) {
      print_table();
      if( control.event.xbutton.state & ShiftMask ) {
	i_transform(&coord.pantoimg, control.event.xbutton.x,
		    control.event.xbutton.y, &bufx, &bufy);
	d_transform(&coord.imgtobuf, (double)bufx, (double)bufy,
		    &bufx, &bufy);
	draw_magnifier((double)bufx, (double)bufy);
      }
      return;
    } else if( (control.event.xbutton.button != Button1) &&
	       (control.event.xbutton.button != Button2) )
      return;
#ifdef DEBUG
    if( (buttons <0) || (buttons > 1) ) {
      (void)fprintf(stderr, "Lost track of panning buttons: %d\n",buttons);
      buttons = 1;
    }
#endif
    buttons++;
    /* set up variables for interactive mode */
    downbutton = control.event.xbutton.button;
    /* if second button, no added set-up, don't respond till move */
    if( buttons > 1 ) {
      return;
    }
    /* store mode (may be called from other mode) */
    oldmode = control.mode;
    control.mode = ZOP;
    /* set filter for events we will track on priority basis */
    control.priority =
      MotionNotify | ButtonPress | ButtonRelease | LeaveNotify;
    /* continue into moved type response */
  case MotionNotify:
    /* we must be tracking a button to do the this */
    if( downbutton < 0 )
      return;
    /* must be mouse moved - call relevant cursor activity */
    if( control.event.xmotion.window == dispbox.ID ) {
      /* get only the most recent move */
      XSync(control.event.xmotion.display, 0);
      while( XCheckIfEvent(control.event.xmotion.display, &control.event,
			   statedmotion_in_disp,
			   (char *)(&control.event.xmotion.state)) );
      /* if moved outside the window, cancel tracking */
      if( (control.event.xmotion.x < 0) ||
	  (control.event.xmotion.x >= dispbox.width) ||
	  (control.event.xmotion.y < 0) ||
	  (control.event.xmotion.y >= dispbox.height) ) {
	/* clear image change mark */
	moved = 0;
	buttons = 0;
	downbutton = -1;
	reset_tdisp(&coord);
	new_pancursor(1);
	/* unset tracking priority and mode */
	control.mode =  oldmode;
	control.priority = 0;
	return;
      }
      switch( downbutton ) {
      case Button1:
	/* case pan - transform center to img coordinates */
	i_transform(&coord.disptoimg,
		    control.event.xmotion.x, control.event.xmotion.y,
		    &coord.tid.cenX, &coord.tid.cenY);
	moved = 1;
	break;
      case Button2:
	/* case zoom - transform edge to img coordinates and choose a zoom */
	panedge_zoom(&coord, &coord.disptoimg,
		     control.event.xmotion.x, control.event.xmotion.y);
	moved = 1;
	break;
      default:
	break;
      }
    } else if( control.event.xbutton.window == panbox.ID ) {
      /* get only the most recent move */
      while( XCheckWindowEvent(panbox.display, panbox.ID, ButtonMotionMask,
			       &control.event) );
      switch( downbutton ) {
      case Button1:
	/* case pan - transform center to image coordinates */
	i_transform(&coord.pantoimg,
		    control.event.xmotion.x, control.event.xmotion.y,
		    &coord.tid.cenX, &coord.tid.cenY);
	moved = 1;
	break;
      case Button2:
	/* case zoom - transform edge to img coordinates and choose a zoom */
	panedge_zoom(&coord, &coord.pantoimg,
		     control.event.xmotion.x, control.event.xmotion.y);
	moved = 1;
	break;
      default:
	break;
      }
      if( control.event.xbutton.state & ShiftMask ) {
	i_transform(&coord.pantoimg, control.event.xmotion.x,
		    control.event.xmotion.y, &bufx, &bufy);
	d_transform(&coord.imgtobuf, (double)bufx, (double)bufy,
		    &bufx, &bufy);
	draw_magnifier((double)bufx, (double)bufy);
      }
    }
    /* keep the center coords within the image */
    if( coord.tid.cenX < coord.img.X1 )
      coord.tid.cenX = coord.img.X1 + 0.001;
    else if( coord.tid.cenX > coord.img.X2 )
      coord.tid.cenX = coord.img.X2 - 0.001;
    if( coord.tid.cenY < coord.img.Y1 )
      coord.tid.cenY = coord.img.Y1 + 0.001;
    else if( coord.tid.cenY > coord.img.Y2 )
      coord.tid.cenY = coord.img.Y2 - 0.001;
    /* set needed parameters for proposed display parameters */
    set_tdisp(&coord);
    /* draw panbox cursor */
    new_pancursor(1);
    break;
  case ButtonRelease:
    /* act only if this button is the last trackable button */
    if( control.event.xbutton.button  == Button3 )
      return;
    if( (buttons <= 0) || (--buttons > 0) )
      return;
    downbutton = -1;
    /* redraw the cursor correctly if we have been Xor'ing */
    if( color.cursor_overlay == 0 )
      new_pancursor(0);
    if( moved == 0 )
      return;
    /* prepare and display new image */
    new_display(1, 0, 0, 0);
    if( control.verbose ) {
      /* report disp coordinates to user */
      show_dispcoords();
    }
    /* clear image change mark */
    moved = 0;
    /* unset tracking priority */
    control.mode =  oldmode;
    control.priority = 0;
    break;
  case LeaveNotify:
    /* leaving the window undo's any selection activity */
    if( downbutton != -1 ) {
      buttons = 0;
      downbutton = -1;
      reset_tdisp(&coord);
      new_pancursor(0);
    }
    /* clear image change mark */
    moved = 0;
    /* unset tracking priority and mode */
    control.mode =  oldmode;
    control.priority = 0;
    break;
  }
}

/*
 * Subroutine:	select_pan
 * Purpose:	Respond to a menu selection in the pan and zoom menu
 * Xlib calls:	XSync();
 */
void select_pan ( )
{
  float zoom;
  void reset_tdisp(), set_tdisp(), new_pancursor(), new_display();
  void show_dispcoords();

  if( control.response[0] != ZOP )
    return;
  control.mode = ZOP;
  switch( control.response[1] ) {
    /* main menu selections have 0, change only main mode */
  case 0:
    return;
  case ZOP_ZPan14:
    zoom = coord.id.zoom * 0.25;
    break;
  case ZOP_ZPan12:
    zoom = coord.id.zoom * 0.5;
    break;
  case ZOP_ZPan1:
    zoom = 1;
    break;
  case ZOP_ZPan2:
    zoom = coord.id.zoom * 2.0;
    break;
  case ZOP_ZPan4:
    zoom = coord.id.zoom * 4.0;
    break;
  case ZOP_Center:
    /* center on image (same zoom) */
    coord.tid.cenX = coord.img.cenX;
    coord.tid.cenY = coord.img.cenY;
    zoom = coord.id.zoom;
    break;
  default:
    (void)fprintf(stderr,"Unknown menu zoom factor!\n");
    return;
  }
  /* check against the limits before proceeding */
  if( (0.01 > zoom) || (zoom > 400.0) ) {
    (void)fprintf(stderr, "WARNING: Attempt to exceed zooming limits!\n");
    reset_tdisp(&coord);
  }
  /* tid.zoom = zoom but force exact integer alignment (int or 1/int) */
  if( zoom > 0.75 )
    coord.tid.zoom = (double)((int)(zoom + 0.5));
  else
    coord.tid.zoom = 1.0 / (double)((int)((1.0 / zoom) + 0.5));
  /* calculate new display coordinates (in CoordTemp.c) */
  set_tdisp(&coord);
  /* create and draw new panbox cursor */
  new_pancursor(0);
  /* make sure this stuff appears before doing anything else */
  XSync(panbox.display, 0);
  /* prepare and display new image */
  new_display(1, 0, 0, 0);
  /* update record and report status */
  if( control.verbose ) {
    /* report disp coordinates to user */
    show_dispcoords();
  }
}
