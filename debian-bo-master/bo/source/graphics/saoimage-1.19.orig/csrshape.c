#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrshape.c (Cursor Shape)
 * Purpose:	Make basic software cursors
 * Subroutine:	make_cursor()			returns: void
 * Subroutine:	change_circle_granularity()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{1} MVH added arrow cursor			   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/*  stderr, NULL, etc.  */
#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include "hfiles/color.h"	/*  Cursor colors needed by Cursor.h  */
#include "hfiles/constant.h"	/*  Define codes  */
#include "hfiles/cursor.h"	/*  Define cursor parameter structures  */

#include "defs/circle.def"	/*  Define Unit_circles  */


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		make_cursor(	struct cursorRec *cursor);
static int	make_arrow(	double xtip, double ytip, double xray,
				double rotsin, double rotcos, XPoint *points);
static int	make_boxcur(	double xcen, double ycen,
				double xray, double yray,
				double rotsin, double rotcos, XPoint *points);
static int	make_circur(	double xcen, double ycen, double radius,
				XPoint *points )
static int	make_ellipse(	double xcen, double ycen,
				double xradius, double yradius,
				double rotsin, double rotcos, XPoint *points);

#else

  static int make_arrow(), make_boxcur(), make_circur(), make_ellipse();

#endif


/*  Declare and initialize drawing tools  */
double *UnitCircleX = Circle48X;
double *UnitCircleY = Circle48Y;
double *UnitEllipseX = Circle64X;
double *UnitEllipseY = Circle64Y;
int CircleCnt = 48;
int EllipseCnt = 64;
/*  Specify size and shape of arrow head (in pixels, regardless of zoom)  */
int ArrowHeadLength = 8;
int ArrowHeadWidth = 5;


/*  Subroutine	make_cursor
 *  Purpose:	Remake the current cursor polygon list
 */
#ifdef ANSIC
void make_cursor ( struct cursorRec *cursor )
#else
void make_cursor ( cursor )
     struct cursorRec *cursor;
#endif
{
  switch( cursor->type ) {
  case COP_Circle:
    cursor->point_cnt =
      make_circur(cursor->win.X, cursor->win.Y, cursor->win.rayX,
		  cursor->points);
    break;
  case COP_Ellipse:
    cursor->point_cnt = make_ellipse(cursor->win.X, cursor->win.Y,
				     cursor->win.rayX, cursor->win.rayY,
				     cursor->rot.sin, cursor->rot.cos,
				     cursor->points);
    break;
  case COP_Box:
    cursor->point_cnt = make_boxcur(cursor->win.X, cursor->win.Y,
				    cursor->win.rayX, cursor->win.rayY,
				    cursor->rot.sin, cursor->rot.cos,
				    cursor->points);
    break;
  case COP_Arrow:
    cursor->point_cnt = make_arrow(cursor->win.X, cursor->win.Y,
				   cursor->win.rayX, cursor->rot.sin,
				   cursor->rot.cos, cursor->points);
    break;
  case COP_Point:
    cursor->point_cnt = 0;
  case COP_Polygon:
  default:
    break;
  }
}


/*  Subroutine:	make_arrow
 *  Purpose:	Given tip and tail, make line including arrow head
 *  Returns:	Number of points needed to draw arrow
 */
#ifdef ANSIC
static int make_arrow ( double xtip, double ytip, double xray,
		        double rotsin, double rotcos, XPoint *points )
#else
static int make_arrow ( xtip, ytip, xray, rotsin, rotcos, points )
     double xtip, ytip;		/*  Coords of tip  */
     double xray;		/*  Length of shaft  */
     double rotsin, rotcos;	/*  Trig values for angle  */
     XPoint *points;
#endif
{
  double head_xcos, head_ycos, head_xsin, head_ysin;

  /*  Compute offset_from_tip partials to draw arrow head  */
  head_xcos = ArrowHeadWidth * rotcos;
  head_ycos = ArrowHeadLength * rotcos;
  head_xsin = ArrowHeadWidth * rotsin;
  head_ysin = ArrowHeadLength * rotsin;
  /*  Line drawn as 3 segments, all starting at tip  */
  /*  Statements with like factors grouped to help compiler optimization */
  points[5].x = xtip - (xray * rotsin);
  points[5].y = ytip + (xray * rotcos);
  points[1].x = xtip + (head_xcos - head_ysin);
  points[1].y = ytip + (head_xsin + head_ycos);
  points[3].x = xtip - (head_xcos + head_ysin);
  points[3].y = ytip - (head_xsin - head_ycos);
  points[0].x = xtip;
  points[0].y = ytip;
  points[2].x = xtip;
  points[2].y = ytip;
  points[4].x = xtip;
  points[4].y = ytip;
  return( -3 );
}


/*  Subroutine:	make_boxcur
 *  Purpose:	Set the corner coordinates for the rotating box cursor
 *  Returns:	Number of points needed to draw box
 */
#ifdef ANSIC
static int make_boxcur ( double xcen, double ycen, double xray, double yray,
			 double rotsin, double rotcos, XPoint *points )
#else
static int make_boxcur ( xcen, ycen, xray, yray, rotsin, rotcos, points )
     double xcen, ycen;
     double xray, yray;
     double rotsin, rotcos;
     XPoint *points;
#endif
{
  double x1, y1, x2, y2;

  /*  Calculate offset of two corners on the right  */
  x1 = (xray * rotcos) - (yray * rotsin);
  y1 = (xray * rotsin) + (yray * rotcos);
  x2 = (xray * rotcos) + (yray * rotsin);
  y2 = (xray * rotsin) - (yray * rotcos);
  /*  Define box corners  */
  points[0].x = xcen + x1;
  points[0].y = ycen + y1;
  points[1].x = xcen + x2;
  points[1].y = ycen + y2;
  points[2].x = xcen - x1;
  points[2].y = ycen - y1;
  points[3].x = xcen - x2;
  points[3].y = ycen - y2;
  points[4].x = xcen + x1;
  points[4].y = ycen + y1;
  return( 5 );
}


/*  Subroutine:	make_circur
 *  Purpose:	Create a new circle cursor with given center and radius
 *  Returns:	Number of points needed to draw circle
 */
#ifdef ANSIC
static int make_circur ( double xcen, double ycen, double radius,
			 XPoint *points )
#else
static int make_circur ( xcen, ycen, radius, points )
     double xcen, ycen, radius;
     XPoint *points;
#endif
{
  int loop;
  register int i, xoff, yoff;
  register int a1, a2, a3, b1, b2, b3, b4;

  /*  Set up array indexes for 8 fold symetry  */
  /*  8 indexes radiating both ways from each of the four axes  */
  a1 = CircleCnt / 4;		/*  (1 * cnt) / 4  */
  a2 = a1 + a1;			/*  (2 * cnt) / 4  */
  a3 = a2 + a1;			/*  (3 * cnt) / 4  */
  b1 = a1 - 1;
  b2 = a2 - 1;
  b3 = a3 - 1;
  b4 = a3 + a1 - 1;		/*  (4 * cnt) / 4 - 1  */
  /*  Calculate points on circumference for 1/8th of circle  */
  /*  Apply to each of 8 pairs  */
  loop = CircleCnt / 8;
  for( i=0; i <= loop; i++ ) {
    xoff = UnitCircleX[i] * radius;
    yoff = UnitCircleY[i] * radius;
    points[i].x = xcen + xoff;
    points[i].y = ycen + yoff;
    points[a1+i].x = xcen + yoff;
    points[a1+i].y = ycen - xoff;
    points[a2+i].x = xcen - xoff;
    points[a2+i].y = ycen - yoff;
    points[a3+i].x = xcen - yoff;
    points[a3+i].y = ycen + xoff;
    points[b1-i].x = xcen + yoff;
    points[b1-i].y = ycen + xoff;
    points[b2-i].x = xcen + xoff;
    points[b2-i].y = ycen - yoff;
    points[b3-i].x = xcen - yoff;
    points[b3-i].y = ycen - xoff;
    points[b4-i].x = xcen - xoff;
    points[b4-i].y = ycen + yoff;
  }
  /*  Close the circle (end point same as starting point)  */
  points[CircleCnt].x = points[0].x;
  points[CircleCnt].y = points[0].y;
  return( CircleCnt + 1 );
}


/*  Subroutine:	make_ellipse
 *  Purpose:	Create an ellipticle cursor with given center and radii
 *  Returns:	Number of points needed to draw ellipse
 */
#ifdef ANSIC
static int make_ellipse ( double xcen, double ycen,
			  double xradius, double yradius,
			  double rotsin, double rotcos, XPoint *points )
#else
static int make_ellipse ( xcen, ycen, xradius, yradius,
			  rotsin, rotcos, points )
     double xcen, ycen;
     double xradius, yradius;
     double rotsin, rotcos;
     XPoint *points;
#endif
{
  int loop;
  float xoff, yoff;
  register int i;
  register int x1, x2, y1, y2;
  register int a2, b1, b3;

  /*  Set up array indexes for 8 fold symetry  */
  /*  4 indexes radiating both ways from each of the two axis rays  */
  a2 = EllipseCnt / 2;
  b1 = a2 - 1;
  b3 = EllipseCnt - 1;
  /*  Recalculate points on circumference  */
  /*  Apply to each of 4 pairs  */
  loop = EllipseCnt / 4; 
  for( i=0; i < loop; i++ ) {
    /*  Calculate ray lengths for orthogonal case  */
    xoff = UnitEllipseX[i] * xradius;
    yoff = UnitEllipseY[i] * yradius;
    /*  Calculate offset of two points on the right (rotate)  */
    x2 = (xoff * rotcos) + (yoff * rotsin);
    y2 = (xoff * rotsin) - (yoff * rotcos);
    x1 = (xoff * rotcos) - (yoff * rotsin);
    y1 = (xoff * rotsin) + (yoff * rotcos);
    points[i].x = xcen + x1;
    points[i].y = ycen + y1;
    points[a2+i].x = xcen - x1;
    points[a2+i].y = ycen - y1;
    points[b1-i].x = xcen + x2;
    points[b1-i].y = ycen + y2;
    points[b3-i].x = xcen - x2;
    points[b3-i].y = ycen - y2;
  }
  /*  Close the circle (end point same as starting point)  */
  points[EllipseCnt].x = points[0].x;
  points[EllipseCnt].y = points[0].y;
  return( EllipseCnt + 1 );
}


#ifdef NOTNEEDED /* %% not yet needed */
/*  Subroutine:	change_circle_granularity
 *  Purpose:	Change number of points used to draw circles and ellipses
 */
void change_circle_granularity ( cursor, size, type )
     struct cursorRec *cursor;
     int size;			/* i: exact size (48, 64, or 80 */
     int type;			/* i: code either COP_Circle or COP_Ellipse */
{
  double *X, *Y;
  switch( size ) {
  case 48:
    X = Circle48X;
    Y = Circle48Y;
    break;
  case 64:
    X = Circle64X;
    Y = Circle64Y;
    break;
  case 80:
    X = Circle80X;
    Y = Circle80Y;
    break;
  default:
    (void)fprintf(stderr, "WARNING: Circle choices are 48, 64, or 80!\n");
    return;
  }
  if( type == COP_Circle ) {
    UnitCircleX = X;
    UnitCircleY = Y;
    CircleCnt = size;
  } else if( type = COP_Ellipse ) {
    UnitEllipseX = X;
    UnitEllipseY = Y;
    EllipseCnt = size;
  } else
    return;
  if( cursor->type == type ) {
    make_cursor (cursor);
  }
}
#endif
                         
                                                               
                                                               
                                                               
                                                               
                                                               
                         
