#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrgrab.c (Cursor Grab)
 * Purpose:	Surgically resize or delete annular cursors
 * Subroutine:	size_annuli()			returns: void
 * Subroutine:	delete_annulus()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 June 1989
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

#define GRAB_RANGE 5.0
#define ANN_INSIDE 2
#define ANN_ON 1
#define ANN_BETWEEN 8
#define ANN_OUTSIDE 4

/*
 * Subroutine:	size_annuli
 * Purpose:	Change size of cursor to intersect current mouse cursor
 *		while keeping aspect ratio of cursor constant
 */
void size_annuli ( cursor, event )
     struct cursorRec *cursor;
     XEvent *event;		/* i: event for location of mouse */
{
  void draw_cursor(), make_cursor();
  static void size_annulus(), grab_annulus();

  /* if this event is initiating tracking, don't erase the cursor */
  if( event->type == MotionNotify )
    /* erase existing cursor */
    draw_cursor(cursor, &color.gcset.undraw);
  size_annulus(cursor, event);
  /* if this event is initiating tracking, decide which annulus to grab */
  if( event->type == ButtonPress )
    grab_annulus (cursor);
  /* make new point list */
  make_cursor(cursor);
  /* draw it */
  draw_cursor(cursor, &color.gcset.track);
}

/*
 * Subroutine:	delete_annulus
 * Purpose:	Respond to a mouse request to delete an annulus
 *
 */
void delete_annulus ( cursor, event )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XMotionEvent or XButtonEvent */
{
  struct cursorRec *parent;
  static int on_annulus();
  static void size_annulus(), remove_annulus();

  /* if there are two or more annuli */
  if( (cursor->next_annulus != 0) &&
      (cursor->next_annulus->next_annulus != 0) ) {
    /* size the cursor so we can compare its size with the annuli */
    size_annulus(cursor, event);
    if( on_annulus(cursor, &parent) != ANN_BETWEEN )
      /* remove if ON. INSIDE, or OUTSIDE */
      remove_annulus(parent);
  }
}

/*
 * Subroutine:	size_annulus
 * Purpose:	Change size of cursor to intersect current mouse cursor
 *		while keeping aspect ratio of cursor constant
 */
static void size_annulus ( cursor, event )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XMotionEvent for location of mouse */
{
  double rayX, rayY;
  double axis_ratio;

  /* compute distance from center */
  rayX = ((double)event->xmotion.x + 0.5) - cursor->win.X;
  rayY = ((double)event->xmotion.y + 0.5) - cursor->win.Y;
  if( (cursor->type == COP_Box) || (cursor->type == COP_Ellipse) ) {
    double temp;
    temp = (rayX * cursor->rot.cos) + (rayY * cursor->rot.sin);
    rayY = (rayY * cursor->rot.cos) - (rayX * cursor->rot.sin);
    rayX = temp;
  }
  if( rayX < 0.0 )
    rayX = -rayX;
  if( rayY < 0.0 )
    rayY = -rayY;
  axis_ratio = cursor->ctrl.axis_ratio;
  switch( cursor->type ) {
  case COP_Box:
    /* size on the side which will touch the mouse position */
    if( (rayX < SMALL_NUMBER) || ((rayY / rayX) > axis_ratio) ) {
      cursor->win.rayY = rayY;
      cursor->win.rayX = rayY / axis_ratio;
    } else {
      cursor->win.rayX = rayX;
      cursor->win.rayY = rayX * axis_ratio;
    }
    break;
  case COP_Ellipse:
    /* this version of sizing tracks the ellipses edge */
    rayX *= axis_ratio;
    cursor->win.rayY = sqrt((rayX * rayX) + (rayY * rayY));
    cursor->win.rayX = cursor->win.rayY / axis_ratio;
    break;
  case COP_Circle:
    cursor->win.rayX = sqrt((rayX * rayX) + (rayY * rayY));
    cursor->win.rayY = cursor->win.rayX;
    break;
  case COP_Polygon:
  case COP_Point:
  default:
    (void)fprintf(stderr, "cursor type error\n");
  }
}

/*
 * Subroutine:	remove_annulus
 * Purpose:	Remove an annular ring from annulus linked list and screen
 */
static void remove_annulus ( parent )
     struct cursorRec *parent;	/* i: annulus just before one to be removed */
{
  int index;
  struct cursorRec *annulus;
  void draw_cursor(), free_cursor();

  annulus = parent->next_annulus;
  index = annulus->index;
  /* remove annulus from linked list */
  parent->next_annulus = annulus->next_annulus;
  /* erase the grabbed annulus */
  draw_cursor(annulus, &color.gcset.undraw);
  /* free the grabbed annulus */
  free_cursor(annulus);
  /* reset index of annuli which were moved down */
  annulus = parent->next_annulus;
  while( annulus != 0 ) {
    annulus->index = index++;
    annulus = annulus->next_annulus;
  }
}

/*
 * Subroutine:	grab_annulus
 * Purpose:	Set up cursor to for a size annulus interaction
 * Called by:	size_annuli()
 */
static void grab_annulus ( cursor )
     struct cursorRec *cursor;
{
  double inc;
  int code;
  struct cursorRec *parent, *annulus;
  static int on_annulus();
  static void remove_annulus();

  code = on_annulus(cursor, &parent);
  /* decide what to draw */
  /* if grab, erase grabbed */
  if( code == ANN_ON ) {
    remove_annulus(parent);
  } else if( code == ANN_INSIDE ) {
    /* if inside all annuli, use next inc inward */
    if( ((parent = parent->next_annulus) != 0) &&
        ((annulus = parent->next_annulus) != 0) &&
        ((inc = annulus->win.rayX - parent->win.rayX) < parent->win.rayX) ) {
      cursor->win.rayX = parent->win.rayX - inc;
      cursor->win.rayY = cursor->win.rayX * cursor->ctrl.axis_ratio;
    }
  } else if( code == ANN_OUTSIDE ) {
    /* if outside all annuli, use smaller of next inc or mouse position */
    if( ((annulus = parent->next_annulus) != 0) ) {
      double rayX;
      if( parent == cursor )
	rayX = annulus->win.rayX + annulus->win.rayX;
      else
	rayX = annulus->win.rayX + annulus->win.rayX - parent->win.rayX;
      if( rayX < cursor->win.rayX ) {
	cursor->win.rayX = rayX;
	cursor->win.rayY = cursor->win.rayX * cursor->ctrl.axis_ratio;
      }
    }
  }
}

/*
 * Subroutine:	on_annulus
 * Purpose:	Compare size of cursor with annuli
 * Returns:	Code describing relation to annuli
 * PostState:	Sets pointer to nearest annulus inside of cursor
 */
static int on_annulus ( cursor, pointer )
     struct cursorRec *cursor;
     struct cursorRec **pointer;
{
  double grabmax, grabmin;
  struct cursorRec *last_ann, *annulus;
  int code;

  last_ann = cursor;
  /* is this the first annulus (probably an illegal state) */
  if( cursor->next_annulus == 0 ) {
    code = ANN_OUTSIDE;
  } else {
    /* set the capture limits */
    grabmax = cursor->win.rayX + GRAB_RANGE;
    grabmin = cursor->win.rayX - GRAB_RANGE;
    annulus = cursor->next_annulus;
    /* loop check for on or inside each successive ring */
    code = 0;
    do {
      if( annulus->win.rayX > grabmax ) {
	/* add inside this annulus */
	if( last_ann == cursor )
	  code = ANN_INSIDE;
	else
	  code = ANN_BETWEEN;
      } else if( annulus->win.rayX > grabmin ) {
	/* grab this annulus */
	code = ANN_ON;
      } else if( annulus->next_annulus == 0 ) {
	/* add outside all annuli */
	code = ANN_OUTSIDE;
      } else {
	last_ann = annulus;
	annulus = annulus->next_annulus;
      }
    } while( code == 0 );
  }
  *pointer = last_ann;
  return( code );
}
