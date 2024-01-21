#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrarea.c (Cursor Area)
 * Purpose:	Calculate areas of cursors
 * Subroutine:	cursor_area();				returns: double
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* cursor colors needed by Cursor.h */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/cursor.h"	/* define cursor parameter structures */

#define PI 3.14159265358979323846

/*
 * Subroutine:	cursor_area
 * Purpose:	Calculate area, in float units file pixels, enclosed by cursor
 * Method:	Use mathematical formula appropriate to cursor type
 */
double cursor_area ( cursor, user_info )
     struct cursorRec *cursor;
     int user_info;		/* flag, data is for user info */
{
  double area;
  int i, j;
  static int test_cross();

  switch( cursor->type ) {
  case COP_Circle:
    area = PI * cursor->file.Xdim * cursor->file.Xdim;
    break;
  case COP_Ellipse:
    area = PI * cursor->file.Xdim * cursor->file.Ydim;
    break;
  case COP_Box:
    area = cursor->file.Xdim * cursor->file.Ydim * 4.0;
    break;
  case COP_Point:
    area = 0.0;
    break;
  case COP_Polygon:
    /* polygon area is sum of signed areas between each edge and x axis */
    /* (except when polygon edges cross one another) */
    area = 0;
    if( user_info &&
	test_cross(cursor->poly, cursor->poly_cnt, cursor->poly_cnt - 1, 0) )
      return( -1.0 );
    j = cursor->poly_cnt - 1;
    /* each area is (x1-x2)*(y1+y2)/2, /2 will be done once at end */
    for( i=0; i<cursor->poly_cnt; i++ ) {
      area += (cursor->poly[i].fileX - cursor->poly[j].fileX) *
	(cursor->poly[i].fileY + cursor->poly[j].fileY);
      j = i;
    }
    /* sign of area depends on clockwise or counter-clockwise ordering */
    if( area < 0.0 ) area *= -0.5;
    else area *= 0.5;
  default:
    break;
  }
  return( area );
}

/* end points of a polygon edge against which to test others for crossing */
static double x11, y11, x12, y12;

/*
 * Subroutine:	test_cross
 * Purpose:	Test the edges of a polygon to see if any cross each other
 * method:	Starting with last edge, test each edge against subsequent
 *		edges.  Routine recurses with for next edge.  Adjacent edges
 *		are not tested since neighbors cannot cross but neighbors do
 *		share a common point.
 */
static int test_cross ( pt, cnt, j, i )
     PolyPoint *pt;	/* list of vertices */
     int cnt;		/* number of vertices */
     int j, i;		/* two vertices at ends of edge being tested */
{
  int k;
  static int intercept();

  if( (i+2) >= cnt ) {
    return( 0 );
  } else {
    x11 = pt[j].fileX;
    y11 = pt[j].fileY;
    x12 = pt[i].fileX;
    y12 = pt[i].fileY;
    /* test each subsequent edge against this edge (but not adjacent edges) */
    for( k=i+2; k<cnt; k++ ) {
      if( ((k+1) != j) &&
          intercept(pt[k-1].fileX, pt[k-1].fileY, pt[k].fileX, pt[k].fileY) )
	return(1);
    }
    /* recurse on to test next edge */
    return( test_cross(pt, cnt, i, i+1) );
  }
}

/*
 * Subroutine:	intercept
 * Purpose:	Test this edge against the reference edge
 * Method:	A) Minmax test if two edges have overlapping coordinates.
 *		B) Calculate the intercept of the two lines.
 *		C) Test if intercept is within both edges.
 */
static int intercept ( x21, y21, x22, y22 )
     double x21, y21, x22, y22;
{
  double xpt, ypt, m1, m2;

  /* try minmax test */
  if( x11 > x12 ) {
    if( ((x12 >= x21) && (x12 >= x22)) || ((x11 <= x21) && (x11 <= x22)) )
      return( 0 );
  } else {
    if( ((x11 >= x21) && (x11 >= x22)) || ((x12 <= x21) && (x12 <= x22)) )
      return( 0 );
  }
  if( y11 > y21 ) {
    if( ((y12 >= y21) && (y12 >= y22)) || ((y11 <= y21) && (y11 <= y22)) )
      return( 0 );
  } else {
    if( ((y11 >= y21) && (y11 >= y22)) || ((y12 <= y21) && (y12 <= y22)) )
      return( 0 );
  }
  /* compute intercept of line */
  if( x12 == x11 ) {
    xpt = x12;
    ypt = y21 + ((xpt - x21) * (y22 - y21) / (x22 - x21));
  } else if( x22 == x21 ) {
    xpt = x21;
    ypt = y11 + ((xpt - x11) * (y12 - y11) / (x12 - x11));
  } else {
    m1 = (y12 - y11) / (x12 - x11);
    m2 = (y22 - y21) / (x22 - x21);
    xpt = ((x11 * m1) - (x21 * m2) + (y21 - y11)) / (m1 - m2);
    ypt = y11 + (m1 * (xpt - x11));
  }
  /* check for being on both edges */
  /* intercept x within x's of reference edge */
  if( x11 > x12 ) {
    if( (x12 > xpt) || (xpt > x11) )
      return( 0 );
  } else {
    if( (x12 < xpt) || (xpt < x11) )
      return( 0 );
  }
  /* intercept y within y's of reference edge */
  if( y11 > y12 ) {
    if( (y12 > ypt) || (ypt > y11) )
      return( 0 );
  } else {
    if( (y12 < ypt) || (ypt < y11) )
      return( 0 );
  }
  /* intercept x within x's of test edge */
  if( x21 > x22 ) {
    if( (x22 > xpt) || (xpt > x21) )
      return( 0 );
  } else {
    if( (x22 < xpt) || (xpt < x21) )
      return( 0 );
  }
  /* intercept y within y's of test edge */
  if( y21 > y22 ) {
    if( (y22 > ypt) || (ypt > y21) )
      return( 0 );
  } else {
    if( (y22 < ypt) || (ypt < y21) )
      return( 0 );
  }
  return( 1 );
}
