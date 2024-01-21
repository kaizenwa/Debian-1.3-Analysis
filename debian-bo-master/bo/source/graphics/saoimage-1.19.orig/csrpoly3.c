#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrpoly3.c (Cursor Polygon Line)
 * Purpose:	Manipulate vertex lists for drawing polygon cursor
 * Subroutine:	closest_polygon_line()		returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		30 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <math.h>		/* define sqrt() */
#include <X11/Xlib.h>		/* X window stuff */

#define ABS(a) ((a) < 0 ? (-(a)) : (a))
#define SQR(a) ((a) * (a))

/*
 * Subroutine:	closest_polygon_line
 * Purpose:	Test for the closest polygon segment
 * Note:	in case of tie chose segment forming smallest angle with
 *		vector from pointer to closest point on segment
 * Method:	top down search
 */
int closest_polygon_line ( x, y, vertex, cnt )
     int x, y;
     XPoint *vertex;
     int cnt;
{
  double min_distance, min_cosine;
  double new_distance, new_cosine;
  int min_endpoint, endpoint;
  int min_j;
  int i, j;
  static double distance_from_segment(), cos_to_segment();

  min_distance = 1.0E30;
  min_j = 0;
  for( j=cnt, i=j-1; i>=0; i--, j-- ) {
    new_distance = distance_from_segment(x, y, vertex[i].x, vertex[i].y,
					 vertex[j].x, vertex[j].y, &endpoint);
    if( new_distance <= min_distance ) {
      if( new_distance < min_distance ) {
	min_j = j;
	min_distance = new_distance;
	min_cosine = -1.0;
	min_endpoint = endpoint;
      } else {
	if( min_cosine < 0.0 )
	  min_cosine =
	    cos_to_segment(x, y, vertex[min_j-1].x, vertex[min_j-1].y,
			   vertex[min_j].x, vertex[min_j].y, min_endpoint);
	new_cosine = cos_to_segment(x, y, vertex[i].x, vertex[i].y,
				    vertex[j].x, vertex[j].y, endpoint);
	if( new_cosine < min_cosine ) {
	  min_j = j;
	  min_cosine = new_cosine;
	  min_endpoint = endpoint;
	}
      }
    }
  }
  return( min_j );
}

/*
 * Subroutine:	distance_from_segment
 * Returns:	square of shortest distance from reference to line segment
 * Method:	Determine closest point on line, if it is in segment, return
 *		its distance, else return distance to  closest endpoint
 * Method:      Equation of the line of the segement: ax + b = y
 *		  a = (y2-y1)/(x2-x1), b = y1 - (a * x1)
 *		Perpendicular line passing through x, y: aax + bb = y
 *		  aa = -1/a, bb = y_ref - (aa * x_ref)
 *		Intersection of two lines: x_norm, y_norm
 *		  x_norm = (b - bb) / (a - aa), y_norm = (a * x_norm) + b
 */
static double distance_from_segment ( x_ref, y_ref, x1, y1, x2, y2, endpoint )
     int x_ref, y_ref;		/* i: coordinates of reference point */
     int x1, y1, x2, y2;	/* i: coordinates of segment endpoints */
     int *endpoint;		/* o: closest pt end1=1 end2=2 between=0 */
{
  double x_norm, y_norm;
  int orthogonal;

  /* determine closest point of line to reference point */
  if( x2 == x1 ) {
    /* line runs vertical or not at all */
    x_norm = (double)x1;
    y_norm = (double)y_ref;
    orthogonal = 1;
  } else if( y2 == y1 ) {
    /* line runs horizontal */
    x_norm = (double)x_ref;
    y_norm = (double)y1;
    orthogonal = -1;
  } else {
    register double a, b;
    register double aa, bb;

    orthogonal = 0;
    a = (double)(y2 - y1) / (double)(x2 - x1);
    aa = -1.0 / a;
    b = (double)y1 - (a * (double)x1);
    bb = (double)y_ref - (aa * (double)x_ref);
    x_norm = (b - bb) / (aa - a);
    y_norm = (a * x_norm) + b;
  }
  *endpoint = 3;
  /* determine if point is in segment */
  if( orthogonal == 1 ) {
    /* vertical line, better to check y coords (all x the same) */
    if( y1 > y2 ) {
      if( (y_norm <= (double)y1) && (y_norm >= (double)y2) )
	*endpoint = 0;
    } else {
      if( (y_norm >= (double)y1) && (y_norm <= (double)y2) )
	*endpoint = 0;
    }
  } else {
    /* determine if point is in segment */
    if( x1 > x2 ) {
      if( (x_norm <= (double)x1) && (x_norm >= (double)x2) )
	*endpoint = 0;
    } else {
      if( (x_norm >= (double)x1) && (x_norm <= (double)x2) )
	*endpoint = 0;
    }
  }
  if( *endpoint == 0 ) {
    return( SQR((double)x_ref - x_norm) + SQR((double)y_ref - y_norm) );
  } else {
    int d1, d2;

    d1 = SQR(x_ref - x1) + SQR(y_ref - y1);
    d2 = SQR(x_ref - x2) + SQR(y_ref - y2);
    if( d1 < d2 ) {
      *endpoint = 1;
      return( (double)d1 );
    } else {
      *endpoint = 2;
      return( (double)d2 );
    }
  }
}

/*
 * Subroutine:	cos_to_segment
 * Returns:	cosine squared of angle between vector from reference to
 *		closest point on line and vector along line away from reference
 * Method:	cos = vector scalar (cross) product / product of lengths
 * Note:	Cosine-squared uses one multiply where cosine needs 2 sqrts
 */
static double cos_to_segment ( x_ref, y_ref, x1, y1, x2, y2, endpoint )
     int x_ref, y_ref;		/* i: coordinates of reference point */
     int x1, y1, x2, y2;	/* i: coordinates of segment endpoints */
     int endpoint;		/* i: closest pt end1=-1 end2=1 between=0 */
{
  int segment_i, segment_j;
  int ray_i, ray_j;
  int segment_len;

  if( endpoint == 0 )
    /* closest point was on segment, angle is 90 deg, cos = 0 */
    return( 0.0 );
  segment_i = x2 - x1;
  segment_j = y2 - y1;
  if( (segment_len = SQR(segment_i) + SQR(segment_j)) == 0 )
    /* if segment has no length, make it a sure loser for insertion */
    return( 1.1 );
  if( endpoint == 1 ) {
    ray_i = x1 - x_ref;
    ray_j = y1 - y_ref;
  } else {
    /* reverse sense since segment is also going other way */
    ray_i = x_ref - x2;
    ray_j = y_ref - y2;
  }
  /*
  return( ((double)SQR((segment_i * ray_i) + (segment_j * ray_j)) /
	   (double)(segment_len * (SQR(ray_i) + SQR(ray_j))))) );
  */
  segment_i *= ray_i;
  segment_j *= ray_j;
  segment_i += segment_j;
  return( (double)(segment_i * segment_i) /
	  (double)(segment_len * (SQR(ray_i) + SQR(ray_j))) );
}
