#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	pancrsr.c (Pan Cursor)
 * Purpose:	Handle the pan window cursor
 * Subroutine:	draw_pancursor()		returns: void
 * Subroutine:	new_pancursor()			returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 29 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/define.h"	/* define MIN */

static int pancur_x, pancur_y;
static unsigned int pancur_width, pancur_height;
static int pancur_present = 0;

/*
 * Subroutine:	draw_pancursor
 * Purpose:	Draw the pan window cursor
 */
void draw_pancursor ( )
{
  GC gc, set_gc();

  if( pancur_present ) {
    gc = set_gc(&color.gcset.draw);
    XDrawRectangle(panbox.display, panbox.ID, gc,
		   pancur_x, pancur_y, pancur_width, pancur_height);
  }
}

/*
 * Subroutine:	new_pancursor
 * Purpose:	Create and draw panbox cursor given image coordinates
 */
void new_pancursor ( track )
     int track;
{
  float panX1, panX2, panY1, panY2;
  GC gc, set_gc();
  void disp_window(), i_transform();
  static void set_pancursor();

  if( pancur_present ) {
    /* erase the old box if it was visible */
    if( (color.cursor_overlay) || track ) {
      gc = set_gc(&color.gcset.undraw);
      XDrawRectangle(panbox.display, panbox.ID, gc,
		     pancur_x, pancur_y, pancur_width, pancur_height);
    } else {
      /* draw a new image to erase old cursor */
      disp_window(&panbox);
    }
  }
  /* calculate display corners in pan coords */
  i_transform(&coord.imgtopan,
	      coord.tid.srcX1, coord.tid.srcY1, &panX1, &panY1);
  i_transform(&coord.imgtopan,
	      coord.tid.srcX2, coord.tid.srcY2, &panX2, &panY2);
  set_pancursor(&coord.pan, (int)panX1, (int)panX2, (int)panY1, (int)panY2);
  /* draw the new box if it is visible */
  if( pancur_present ) {
    if( track )
      gc = set_gc(&color.gcset.track);
    else
      gc = set_gc(&color.gcset.draw);
    XDrawRectangle(panbox.display, panbox.ID, gc,
		   pancur_x, pancur_y, pancur_width, pancur_height);
  }
}

/*
 * Subroutine:	set_pancursor
 * Purpose:	Set points in a panbox cursor
 */
static void set_pancursor ( pan, left_x, right_x, top_y, low_y )
     Coordsys *pan;
     int left_x, right_x, top_y, low_y;
{
  /* limit pan cursor to the panbox dimensions */
  if( left_x < pan->X1i )
    pancur_x = pan->X1i;
  else
    pancur_x = left_x;
  if( right_x > pan->X2i )
    pancur_width = pan->X2i - pancur_x;
  else
    pancur_width = right_x - pancur_x;
  if( top_y < pan->Y1i )
    pancur_y = pan->Y1i;
  else
    pancur_y = top_y;
  if( low_y > pan->Y2i )
    pancur_height = pan->Y2i - pancur_y;
  else
    pancur_height = low_y - pancur_y;
  if( (pancur_width > 0) && (pancur_height > 0) )
    pancur_present = 1;
  else
    pancur_present = 0;
}
