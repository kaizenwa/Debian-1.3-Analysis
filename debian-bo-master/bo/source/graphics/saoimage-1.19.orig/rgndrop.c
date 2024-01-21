#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgndrop.c (Region Drop)
 * Purpose:	Select and remove a region no longer wanted
 * Subroutine:	unsave_region()			returns: void
 * Subroutine:	region_indicated_by_pointer()	returns: struct cursorRec *
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		27 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <math.h>		/* declare sqrt */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/color.h"	/* cursor colors needed by Cursor.h */
#include "hfiles/cursor.h"	/* define cursor parameter structures */
#include "hfiles/define.h"	/* define SQR, SMALL_NUMBER */

extern struct cursorRec *cycle_region;

/*
 * Subroutine:	unsave_region
 * Purpose:	Remove and discard region from region list
 */
void unsave_region ( cursor, dead_meat )
     struct cursorRec *cursor;		/* i: main cursor record */
     struct cursorRec *dead_meat;	/* i: ptr to region being deleted */
{
  struct cursorRec *region;
  void free_cursor();

  if( (cursor->next_region == NULL) || (dead_meat == NULL) )
    return;
  region = cursor;
  while( region->next_region != dead_meat ) {
    region = region->next_region;
    region->index--;
  }
  region->next_region = dead_meat->next_region;
  /* if deleting region pointed at by cycle region, change cycle region */
  if( cycle_region == dead_meat )
    cycle_region = region;
  /* free deleted region and any annuli it might have */
  do {
    region = dead_meat->next_annulus;
    free_cursor (dead_meat);
    dead_meat = region;
  } while( dead_meat != NULL );
}

/*
 * Subroutine:	region_indicated_by_pointer
 * Purpose:	Identify which region if any is indicated by the pointer
 * Method:	Chooses smallest region which encloses pointer.  (Point
 *		cursors enclose one or two digit label but have 0 area).
 *		In case of equal areas (i.e. points), closer center is used.
 */
struct cursorRec *region_indicated_by_pointer ( cursor, x, y, point_only )
     struct cursorRec *cursor;
     int x, y;
     int point_only;
{
  struct cursorRec *region, *chosen;
  static int pointer_is_inside_region();
  static int closer_to_center();

  chosen = NULL;
  region = cursor->next_region;
  /* erase smallest region which encloses the mouse */
  while( region != NULL ) {
    if( !point_only || region->type == COP_Point ) {
      if( pointer_is_inside_region(x, y, region) ) {
	if( (chosen == NULL) || (region->file.area < chosen->file.area) ) {
	  chosen = region;
	} else if( (region->file.area == chosen->file.area) &&
		   closer_to_center(region, chosen, x, y) )
	  /* compare distance from center */
	  chosen = region;
      }
    }
    region = region->next_region;
  }
  return( chosen );
}

/*
 * Subroutine:	closer_to_center
 */
static int closer_to_center ( challenger, champ, x, y )
     struct cursorRec *challenger, *champ;
     int x, y;
{
  double X, Y, Xn, Yn;

  X = (double)x + 0.5 - champ->win.X;
  Y = (double)y + 0.5 - champ->win.Y;
  Xn = (double)x + 0.5 - challenger->win.X;
  Yn = (double)y + 0.5 - challenger->win.Y;
  if( (SQR(Xn) + SQR(Yn)) > (SQR(X) + SQR(Y)) )
    return( 1 );
  else
    return( 0 );
}

/*
 * Subroutine:	is_inside_region
 * Purpose:	Determine if mouse is within a region
 */
static int pointer_is_inside_region ( x, y, region )
     int x, y;
     struct cursorRec *region;
{
  double rayX, rayY, ray;
  int inside;
  static int pointer_is_inside_polygon();

  if( region->type == COP_Polygon )
    return( pointer_is_inside_polygon((double)x, (double)y,
				      region->poly, region->poly_cnt) );
  rayX = (double)x + 0.5 - region->win.X;
  rayY = (double)y + 0.5 - region->win.Y;
  inside = 0;
  switch( region->type ) {
  case COP_Circle:
    if( (SQR(rayX) + SQR(rayY)) <
        (region->win.rayX * region->win.rayY) )
      inside = 1;
    break;
  case COP_Box:
    if( region->rot.angle != 0.0 ) {
      /* rotate mouse coords into the rotated space */
      ray = (rayX * region->rot.cos) + (rayY * region->rot.sin);
      rayY = (rayY * region->rot.cos) - (rayX * region->rot.sin);
      rayX = ray;
    }
    if( (rayX < region->win.rayX) && (rayX > -region->win.rayX) &&
        (rayY < region->win.rayY) && (rayY > -region->win.rayY) )
      inside = 1;
    break;
  case COP_Point:
    /* cursor is over point index marker (one or two 6x13 characters) */
    if( (rayX >= -2) && (rayX <= 6) && (rayY <= 2) && (rayY >= (-11)) )
      inside = 1;
    break;
  case COP_Ellipse:
    rayX *= region->ctrl.axis_ratio;
    ray = sqrt(SQR(rayX) + SQR(rayY));
    if( (ray < region->win.rayY) &&
        ((ray / region->ctrl.axis_ratio) < region->win.rayX) )
      inside = 1;
    break;
  default:
    break;
  }
  return( inside );
}

/*
 * Subroutine:	pointer_is_inside_polygon
 * Purpose:	Determine if mouse is within a polygon
 */
static int pointer_is_inside_polygon ( x, y, polypt, poly_cnt )
     double x, y;
     PolyPoint *polypt;
     int poly_cnt;
{
  double yi, yj, xi, xj;
  int i, j, crossings;

  crossings = 0;
  j = poly_cnt - 1;
  for( i=0; i<poly_cnt; i++ ) {
    if( (polypt[i].winX >= x) != (polypt[j].winX > x) ) {
      yi = polypt[i].winY;
      yj = polypt[j].winY;
      if( (yi < y) || (yj < y) ) {
	if( (yi < y) && (yj < y) )
	  crossings++;
	else {
	  xi = polypt[i].winX;
	  xj = polypt[j].winX;
	  if( xj != xi ) {
	    if( ((x - xj) * (yi-yj)/(xi-xj) + yj) < y )
	      crossings++;
	  }
	}
      }
    }
  }
  if( crossings & 1 )
    return(1);
  else
    return(0);
}
