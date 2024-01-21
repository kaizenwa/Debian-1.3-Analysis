#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrmove.c (Cursor Move)
 * Purpose:	Move, resize, or rotate the software cursor
 * Subroutine:	move_cursor()			returns: void
 * Subroutine:	size_cursor()			returns: void
 * Subroutine:	angle_cursor()			returns: void
 * Subroutine:  start_size()			returns: int
 * Called by:	control_cursor() in CursorCtrl.c
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 June 1989
 *		{1} MVH don't try to set angle if offset is 0,0	  22 Feb 1990
 *		{2} MVH added arrow and text cursors		   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <math.h>		/* get trig functions and sqrt */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* cursor colors needed by Cursor.h */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/cursor.h"	/* define cursor parameter structures */
#include "hfiles/define.h"	/* SMALL_NUMBER, LARGE_NUMBER and more */

extern struct colorRec color;	/* need to know color.gcset */

#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		move_cursor(	struct cursorRec *cursor, XEvent *event,
				int erase);
void		size_cursor(	struct cursorRec *cursor, XEvent *event,
				int erase);
void		angle_cursor(	struct cursorRec *cursor, XEvent *event);
int		start_size(	struct cursorRec *cursor, XEvent *event);

#else

  void draw_cursor(), make_cursor(), angle_cursor();
  void size_polygon(), set_polygon_hashmarks();

#endif

/*  Subroutine:	move_cursor
 *  Purpose:	Change cursor location cursor
 */
#ifdef ANSIC
void move_cursor ( struct cursorRec *cursor, XEvent *event, int erase )
#else
void move_cursor ( cursor, event, erase )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XMotionEvent for location of mouse */
     int erase;			/* i: erase-before-moving */
#endif
{
  int dxm, dym;
  int cnt;
  register XPoint *point;
  register int i;

  if( erase )
    /*  Erase existing cursor  */
    draw_cursor(cursor, &color.gcset.undraw);
  /*  How much did we move?  */
  dxm = event->xmotion.x - cursor->win.x;
  dym = event->xmotion.y - cursor->win.y;
  if( cursor->type == COP_Text ) {
    /*  Move text drawing parameters  */
    move_textcursor(dxm, dym);
  } else {
    /*  Move all points (or segments)  */
    if( (cnt = cursor->point_cnt) < 0 )
      cnt *= -2;
    point = cursor->points;
    for( i=0; i < cnt; i++ ) {
      point[i].x += dxm;
      point[i].y += dym;
    }
    if( cursor->type == COP_Polygon )
      set_polygon_hashmarks(cursor);
  }
  /*  Update record of center  */
  cursor->win.x = event->xmotion.x;
  cursor->win.y = event->xmotion.y;
  cursor->win.X = (double)cursor->win.x + 0.5;
  cursor->win.Y = (double)cursor->win.y + 0.5;
  /*  Draw it  */
  draw_cursor(cursor, &color.gcset.track);
}


/*  Subroutine:	size_cursor
 *  Purpose:	Change size of cursor to intersect current mouse cursor
 *		on side or at corner depending on side argument
 */
#ifdef ANSIC
void size_cursor ( struct cursorRec *cursor, XEvent *event, int erase )
#else
void size_cursor ( cursor, event, erase )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XMotionEvent for location of mouse */
     int erase;			/* i: erase-before-resizing */
#endif
{
  double rayX, rayY;
  int side;

  if( cursor->type == COP_Text ) {
    return;
  } else if( cursor->type == COP_Arrow ) {
    angle_cursor(cursor, event);
    return;
  }
  /*  Compute distance from center  */
  rayX = ((double)event->xmotion.x + 0.5) - cursor->win.X;
  rayY = ((double)event->xmotion.y + 0.5) - cursor->win.Y;
  side = cursor->ctrl.active_side;
  if( erase )
    /*  Erase existing cursor  */
    draw_cursor(cursor, &color.gcset.undraw);
  switch( cursor->type ) {
  case COP_Box:
    if( cursor->rot.angle == 0.0 ) {
      /*  Change specified side to share X and/or Y coordinate with mouse  */
      if( (side == XYCORNER) || (side == XSIDE) )
	cursor->win.rayX = fabs(rayX);
      if( (side == XYCORNER) || (side == YSIDE) )
	cursor->win.rayY = fabs(rayY);
      break;
    }
  case COP_Ellipse:
    /*  Change specified side to share X and/or Y coordinate with mouse  */
    /*  Ellipse is drawn within rotated box defined by mouse */
    if( (side == XYCORNER) || (side == XSIDE) )
      cursor->win.rayX =
	fabs(rayX * cursor->rot.cos + rayY * cursor->rot.sin);
    if( (side == XYCORNER) || (side == YSIDE) )
      cursor->win.rayY =
	fabs(rayY * cursor->rot.cos - rayX * cursor->rot.sin);
    break;
  case COP_Circle:
    /*  Circle is drawn to place edge at mouse position  */
    cursor->win.rayX = sqrt((rayX * rayX) + (rayY * rayY));
    cursor->win.rayY = cursor->win.rayX;
    break;
  case COP_Polygon:
    /*  Cursor sizing is unique, involving specified point  */
    size_polygon(cursor, event->xmotion.x, event->xmotion.y);
    draw_cursor(cursor, &color.gcset.track);
    return;
  case COP_Point:
  default:
    (void)fprintf(stderr, "cursor type error\n");
    return;
  }
  /*  Make cursor with new size  */
  make_cursor(cursor);
  /*  Draw it  */
  draw_cursor(cursor, &color.gcset.track);
}


/*  Subroutine:	angle_cursor
 *  Purpose:	Change angle of cursor to line up with current mouse cursor
 */
#ifdef ANSIC
void angle_cursor ( struct cursorRec *cursor, XEvent *event )
#else
void angle_cursor ( cursor, event )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XMotionEvent for location of mouse */
#endif
{
  double angle;

  /*  Do nothing if click is on cursor center  */
  if( (event->xmotion.x == cursor->win.x) &&
      (event->xmotion.y == cursor->win.y) )
    return;
  /*  Compute distance from center  */
  angle = atan2((cursor->win.X - ((double)(event->xmotion.x) + 0.5)),
		(((double)(event->xmotion.y) + 0.5) - cursor->win.Y));
  /*  Erase existing cursor  */
  draw_cursor(cursor, &color.gcset.undraw);
  switch( cursor->type ) {
  case COP_Box:
  case COP_Ellipse:
  case COP_Arrow:
    /*  Update cursor record  */
    /*  We reversed the sign of Y before, so ...  */
    cursor->rot.cos = cos(angle);
    cursor->rot.sin = sin(angle);
    /*  The angle variable is for priinted feedback only, we make it go counter
	clockwise from x=0, 0 to 2pi, as per astronomical convention  */
    cursor->rot.angle = PI - angle;
    if( cursor->type == COP_Arrow ) {
      double rayX, rayY;

      rayX = ((double)event->xmotion.x + 0.5) - cursor->win.X;
      rayY = ((double)event->xmotion.y + 0.5) - cursor->win.Y;
      cursor->win.rayX = sqrt((rayX * rayX) + (rayY * rayY));
    }
    break;
  default:
    (void)fprintf(stderr, "cursor type error\n");
    return;
  }
  /*  Set new drawing line coords  */
  make_cursor(cursor);
  /*  Draw it  */
  draw_cursor(cursor, &color.gcset.track);
}


/*  Subroutine:	start_size
 *  Purpose:	Calculate restrictions applied to sizing restrictions can apply
 *		where x and y dimensions are independent zones are defines
 *		by 2 rays from center through rayX/2,rayY and rayX,rayY/2
 *  Returns:	Code to indicate kind of sizing to do
 */
#ifdef ANSIC
int start_size ( struct cursorRec *cursor, XEvent *event )
#else
int start_size ( cursor, event )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XButtonEvent for location of mouse */
#endif
{
  double rayX, rayY;
  double mouse_ratio;
  int size_side;

  if( (cursor->type == COP_Box) ||
      (cursor->type == COP_Ellipse) ) {
    /*  Calculate relation between x and y (slope through corner)  */
    if( cursor->win.rayX < SMALL_NUMBER ) {
      if( cursor->win.rayY < SMALL_NUMBER ) {
	/*  Object has no size, adjust both sides  */
	cursor->ctrl.axis_ratio = 1.0;
	return( XYCORNER );
      } else
	cursor->ctrl.axis_ratio = LARGE_NUMBER;
    } else if( cursor->win.rayY < SMALL_NUMBER ) {
      cursor->ctrl.axis_ratio = SMALL_NUMBER;
    } else
      cursor->ctrl.axis_ratio = cursor->win.rayY / cursor->win.rayX;
    /*  For annuli, we need the ratio, but not the restriction choice  */
    if( cursor->annuli )
      return( XYCORNER );
    /*  Get positive rays in zero centered, orthogonal space  */
    rayX = ((double)event->xbutton.x + 0.5) - cursor->win.X;
    rayY = ((double)event->xbutton.y + 0.5) - cursor->win.Y;
    if( cursor->type != COP_Box ) {
      double temp;
      temp = (rayX * cursor->rot.cos) + (rayY * cursor->rot.sin);
      rayY = (rayY * cursor->rot.cos) - (rayX * cursor->rot.sin);
      rayX = temp;
    }
    if( rayX < 0.0 )
      rayX = -rayX;
    if( rayY < 0.0 )
      rayY = -rayY;
    /*  Determine slope of ray to mouse  */
    if( rayX < SMALL_NUMBER ) {
      if( rayY < SMALL_NUMBER )
	/*  Mouse is on center, adjust both sides  */
	return( XYCORNER );
      else
	mouse_ratio = LARGE_NUMBER;
    } else
      mouse_ratio = rayY / rayX;
    /*  Determine which of the three restriction zones ray occupies  */
    if( (mouse_ratio / 4.0) > cursor->ctrl.axis_ratio ) {
      size_side = YSIDE;
    } else if( (mouse_ratio * 4.0) < cursor->ctrl.axis_ratio) {
      size_side = XSIDE;
    } else
      size_side = XYCORNER;
    return( size_side );
  } else
    return( XYCORNER );
}
