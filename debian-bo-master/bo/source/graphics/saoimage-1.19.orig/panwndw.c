#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	panwndw.c (Pan Window)
 * Purpose:	Size the pan window to fit the image area and set it up
 * Subroutine:	init_panbox_dimensions()	returns: void
 * Subroutine:	init_panbox_coords()		returns: void
 * Subroutine:	new_panbox()			returns: void
 * Extern:	screen_width, screen_height in WndwInit.c
 * Extern:	button_left, button_middle, button_hmin in WndwConfig.c
 * Xlib calls:	none
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	    21 December 1988
 *		{1} Doug Mink, CfA	keep hints > 0      20  January 1995
 *		{2} Doug Mink, CfA	fix  bug            18  October 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* window manager stuff */
#include "hfiles/define.h"		/* define MIN, MAX, etc */
#include "hfiles/struct.h"		/* all struct record types */
#include "hfiles/extern.h"		/* major declared structs */

/* root window dimensions in wndwinit.c */
extern int screen_width, screen_height;

static double ip_zoomX, ip_zoomY;
static int pan_xwidth, pan_yheight;
/*
 * Subroutine:	init_panbox_dimensions
 * Purpose:	Set size of the panbox to fit exact area of image img specs.
 * Called by:	new_panbox() below
 * Called by:	configure_windowgroup() in WndwConfig.c to init or regroup.
 */
void init_panbox_dimensions ( )
{
  int xzoom, yzoom;
  double ip_zoom;

  /* Calculate the drawing area for the given window size */
  pan_xwidth = panbox.hints.max_width - (2 * panbox.xzero);
  pan_yheight = panbox.hints.max_height - (2 * panbox.yzero);

  /* Select a zoom factor which fits image within pan window */
  xzoom = (coord.img.width - 1) / pan_xwidth + 1;
  yzoom = (coord.img.height - 1) / pan_yheight + 1;

  /* Big image rule: zoom for which one dimension is window size or less */
  if( (xzoom > 1) || (yzoom > 1) ) {
    ip_zoom = 1.0 / (double)MAX(xzoom,yzoom);

  /* Small image rule: zoom for which one side is greater than 1/2 window */
  } else {
    xzoom = pan_xwidth / coord.img.width;
    yzoom = pan_yheight / coord.img.height;
    ip_zoom = (double)MIN(xzoom,yzoom);
  }

  /* Specify pan window size for perfect fit to zoomed img */
  pan_xwidth = (int)((double)coord.img.width * ip_zoom);
  if (pan_xwidth < 1) {
    pan_xwidth = coord.img.width;
    ip_zoomX = 1.0;
    }
  else
    ip_zoomX = ip_zoom;

  pan_yheight = (int)((double)coord.img.height * ip_zoom);
  if (pan_yheight < 1) {
    pan_yheight = coord.img.height;
    ip_zoomY = 1.0;
    }
  else
    ip_zoomY = ip_zoom;
  panbox.hints.width = pan_xwidth + (2 * panbox.xzero);
  panbox.hints.height = pan_yheight + (2 * panbox.yzero);

  /* Aspect ratio may put one side below minimum */
  if (panbox.hints.width > 0 &&
      panbox.hints.min_width > panbox.hints.width )
    panbox.hints.min_width = panbox.hints.width;
  if (panbox.hints.height > 0 &&
      panbox.hints.min_height > panbox.hints.height )
    panbox.hints.min_height = panbox.hints.height;

  /* set sizing guidlines to maintain image aspect ratio */
  panbox.hints.min_aspect.x = coord.img.width;
  panbox.hints.max_aspect.x = coord.img.width;
  panbox.hints.min_aspect.y = coord.img.height;
  panbox.hints.max_aspect.y = coord.img.height;
}

/*
 * Subroutine:	init_panbox_coords
 * Purpose:	Set the panbox coordinate system for the actual window.
 * Note:	Drawing area must be as decided above in init_panbox1.
 */
void init_panbox_coords ( )
{
  void get_window_dimensions();
  void set_coordsys(), set_transform(), combine_transform(), set_edges();

  panbox.xzero = MAX(0, ((int)panbox.width - pan_xwidth)/2);
  panbox.yzero = MAX(0, ((int)panbox.height - pan_yheight)/2);
  panbox.xwidth = pan_xwidth;
  panbox.yheight = pan_yheight;

  /* fill out pan window coordinate system parameters */
  set_coordsys(&coord.pan, 0.5, (int)panbox.width, (int)panbox.height,
	       panbox.xzero, panbox.yzero, panbox.xzero + panbox.xwidth - 1,
	       panbox.yzero + panbox.yheight - 1);

  /* compute coordinate conversions between pan and img systems */
  set_transform (coord.img.cenX, coord.img.cenY, ip_zoomX, ip_zoomY,
		&coord.pan, &coord.img, &coord.pantoimg, &coord.imgtopan);
  combine_transform(&coord.pantofile, &coord.pantoimg, &coord.imgtofile);
  set_edges(&coord.pantofile, &coord.file, &coord.pan, &coord.fp);
}

/*
 * Subroutine:	new_panbox
 * Purpose:	Create or readjust panbox (coord.img must be set).
 * Called by:	imtool_response()
 */
void new_panbox ( newdata )
     int newdata;		/* boolean to force loading new image */
{
  int diff;
  int changed = 0;
  void init_panbox_dimensions(), init_panbox_coords(), init_panbuf();
  void new_panimage(), new_pancursor();

  /* choose size to fit image aspect ratio for blocked pan image */
  init_panbox_dimensions();
  /* if window was changed */
  if( (panbox.hints.width != panbox.width) ||
      (panbox.hints.height != panbox.height) ) {
    /* resize the window (this causes an expose event) */
    diff = panbox.width - panbox.hints.width;
    panbox.width = panbox.hints.width;
    panbox.height = panbox.hints.height;
    if( diff ) {
      panbox.x += diff;
      XMoveResizeWindow(panbox.display, panbox.ID, panbox.x, panbox.y,
			panbox.width, panbox.height);
    } else
      XResizeWindow(panbox.display, panbox.ID, panbox.width, panbox.height);
    changed = 1;
  }
  init_panbox_coords();
  /* get bigger buffer if needed */
  init_panbuf();
  if( newdata || changed ) {
    /* create a new image to fit the panbox */
    new_panimage();
    /* set up the new panbox display area cursor */
    new_pancursor(0);
  }
}
