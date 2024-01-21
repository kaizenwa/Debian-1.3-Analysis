#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrdraw.c (Cursor Draw)
 * Purpose:	Display and draw the software cursor
 * Subroutine:	disp_cursor()			returns: void
 * Subroutine:	erase_cursor()			returns: void
 * Subroutine:	draw_cursor()			returns: void
 * Subroutine:	draw_annuli()			returns: void
 * Subroutine:	draw_point()
 * Subroutine:	erase_point()
 * Xlib calls:	XDrawLines(), XDrawRectangles()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 June 1989
 *		{1} MVH added text cursor support		   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/*  stderr, NULL, etc.  */
#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include "hfiles/color.h"	/*  Cursor colors needed by Cursor.h  */
#include "hfiles/cursor.h"	/*  Define cursor parameter structures  */
#include "hfiles/constant.h"	/*  Define codes  */

extern struct colorRec color;	/*  Need to know color.gcset  */


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		disp_cursor(	struct cursorRec *cursor);
void		erase_cursor(	struct cursorRec *cursor);
void		draw_cursor(	struct cursorRec *cursor, GCspec *draw);
void		draw_annuli(	struct cursorRec *cursor, GCspec *draw);

#else

  GC set_gc();
  void draw_annuli(), draw_cursor(), draw_point(), erase_point();
  void draw_textcursor();

#endif


/*  Subroutine:	disp_cursor
 *  Purpose:	Draw the software cursor, if it is to be visible
 */
#ifdef ANSIC
void disp_cursor ( struct cursorRec *cursor )
#else
void disp_cursor ( cursor )
     struct cursorRec *cursor;
#endif
{
  /*  If there are annuli, draw them  */
  if( cursor->annuli ) {
    draw_annuli(cursor, cursor->draw);
  } else {
    if( cursor->point_cnt || cursor->rectangle_cnt )
      draw_cursor(cursor, cursor->draw);
  }
}


/*  Subroutine:	erase_cursor
 *  Purpose:	Undraw the cursor
 */
#ifdef ANSIC
void erase_cursor ( struct cursorRec *cursor )
#else
void erase_cursor ( cursor )
     struct cursorRec *cursor;
#endif
{
  if( cursor->point_cnt || cursor->rectangle_cnt )
    draw_cursor(cursor, &color.gcset.undraw);
}


/*  Subroutine:	draw_cursor
 *  Purpose:	Draw a cursor as indicated
 */
#ifdef ANSIC
void draw_cursor ( struct cursorRec *cursor, GCspec *draw )
#else
void draw_cursor ( cursor, draw )
     struct cursorRec *cursor;
     GCspec *draw;
#endif
{
  GC gc;

  if( cursor->type == COP_Text ) {
    draw_textcursor(cursor, draw);
    return;
  }
  gc = set_gc(draw);
  /*  Draw the cursor  */
  if( cursor->point_cnt > 0 )
    XDrawLines(cursor->win.display, cursor->win.ID, gc,
	       cursor->points, cursor->point_cnt, CoordModeOrigin);
  else if( cursor->point_cnt < 0 )
    XDrawSegments(cursor->win.display, cursor->win.ID, gc,
		  (XSegment *)cursor->points, -cursor->point_cnt);
  if( cursor->rectangle_cnt )
    XDrawRectangles(cursor->win.display, cursor->win.ID, gc,
		    cursor->rectangles, cursor->rectangle_cnt);
}


/*  Subroutine:	draw_annuli
 *  Purpose:	Draw all annuli in specified color and function
 */
#ifdef ANSIC
void draw_annuli ( struct cursorRec *cursor, GCspec *draw )
#else
void draw_annuli ( cursor, draw )
     struct cursorRec *cursor;
     GCspec *draw;
#endif
{
  struct cursorRec *annulus;
  GC gc;

  gc = set_gc(draw);
  /*  Draw the cursor  */
  annulus = cursor->next_annulus;
  while( annulus != NULL ) {
    if( annulus->point_cnt )
      XDrawLines(annulus->win.display, annulus->win.ID, gc,
		 annulus->points, annulus->point_cnt, CoordModeOrigin);
    annulus = annulus->next_annulus;
  }
}
