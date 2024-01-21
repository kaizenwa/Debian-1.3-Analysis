#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrpoly2.c (Cursor Polygon Points)
 * Purpose:	Maintain the vertex lists for drawing a polygon cursor
 * Subroutine:	start_polygon()				returns: void
 * Subroutine:	collapse_polygon()			returns: void
 * Subroutine:	add_polygon_vertex()			returns: void
 * Subroutine:	delete_polygon_vertex()			returns: void
 * Subroutine:	set_polygon_hashmarks()			returns: void
 * Subroutine:	set_active_polygon_hashmark()		returns: void
 * Subroutine:	copy_polygon_region_to_cursor()		returns: void
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
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* cursor colors needed by Cursor.h */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/coord.h"	/* coord structs */
#include "hfiles/cursor.h"	/* define cursor parameter structures */

/* define parameters for hash marks (size, number of vertices available */
#define HASH_RAY 2
#define HASH_SIDE 4

static PolyPoint *poly;
static XRectangle *rectangles;
static polysz = 0;

/*
 * Subroutine:	collapse_polygon
 * Purpose:	Reset polygon to the last manipulated point
 */
void collapse_polygon ( cursor )
     struct cursorRec *cursor;
{
  int active;
  active = cursor->ctrl.active_vertex;
  bcopy((char *)&cursor->poly[active], (char *)cursor->poly,
	sizeof(PolyPoint));
  cursor->win.X = cursor->poly[0].winX;
  cursor->win.Y = cursor->poly[0].winY;
  cursor->win.x = (int)cursor->win.X;
  cursor->win.x = (int)cursor->win.X;
  cursor->file.X = cursor->poly[0].fileX;
  cursor->file.Y = cursor->poly[0].fileY;
  cursor->points[0].x = cursor->points[active].x;
  cursor->points[0].y = cursor->points[active].y;
  cursor->points[1].x = cursor->points[0].x;
  cursor->points[1].y = cursor->points[0].y;
  cursor->rectangles[0].x = cursor->rectangles[active].x;
  cursor->rectangles[0].y = cursor->rectangles[active].y;
  /* set the counters */
  cursor->point_cnt = 2;
  cursor->poly_cnt = 1;
  cursor->rectangle_cnt = 1;
  cursor->ctrl.active_vertex = 0;
}

/*
 * Subroutine:	start_polygon
 * Purpose:	Install polygon drawing stuff for the active cursor
 */
void start_polygon ( cursor, coord )
     struct cursorRec *cursor;
     struct coordRec *coord;
{
  void set_polygon_from_file_coords();
  static void init_polygon();

  if( polysz == 0 )
    init_polygon();
  cursor->poly_cnt = 1;
  cursor->rectangle_cnt = 1;
  cursor->point_cnt = 2;
  /* install the local fields */
  cursor->poly = poly;
  cursor->rectangles = rectangles;
  /* update to cursor coords */
  poly[0].fileX = cursor->file.X;
  poly[0].fileY = cursor->file.Y;
  set_polygon_from_file_coords (cursor, &coord->filetodisp, 1);
  cursor->ctrl.active_vertex = 0;
}
static void init_polygon ( )
{
  int i;
  char *calloc_errchk();
  polysz = CURSOR_MAX;
  poly = (PolyPoint *)
    calloc_errchk(polysz, sizeof(PolyPoint), "poloygon records");
  rectangles = (XRectangle *)
    calloc_errchk(polysz, sizeof(XRectangle), "polygon hash marks");
  for( i=0; i<polysz; i++ ) {
    rectangles[i].width = HASH_SIDE;
    rectangles[i].height = HASH_SIDE;
  }
}

/*
 * Subroutine:	add_polygon_vertex
 */
void add_polygon_vertex ( cursor, index, x, y )
     struct cursorRec *cursor;
     int index;
     int x, y;
{
  register XPoint *points;
  register int i, j;

  if( (cursor->poly_cnt + 1) >= polysz ) {
    (void)fprintf(stderr, "WARNING: vertex buffer full\n");
    return;
  }
  /* shift the succeeding points up */
  points = cursor->points;
  i = cursor->poly_cnt;
  /* line list has first point repeated at top */
  points[i+1].x = points[i].x;
  points[i+1].y = points[i].y;
  for( j=i-1; j>=index; j-- ) {
    points[i].x = points[j].x;
    points[i].y = points[j].y;
    rectangles[i].x = rectangles[j].x;
    rectangles[i].y = rectangles[j].y;
    if( !(poly[i].unset = poly[j].unset) ) {
      poly[i].winX = poly[j].winX;
      poly[i].winY = poly[j].winY;
      poly[i].fileX = poly[j].fileX;
      poly[i].fileY = poly[j].fileY;
    }
    i = j;
  }
  /* install new values (we never add base point) */
  points[index].x = x;
  points[index].y = y;
  rectangles[index].x = x - HASH_RAY;
  rectangles[index].y = y - HASH_RAY;
  poly[index].unset = 1;
  /* updates the counts */
  cursor->point_cnt++;
  cursor->poly_cnt++;
  cursor->rectangle_cnt++;
  cursor->ctrl.active_vertex = index;
}

/*
 * Subroutine:	delete_polygon_vertex
 */
void delete_polygon_vertex ( cursor, index )
     struct cursorRec *cursor;
     int index;
{
  register XPoint *points;
  register int i, j;

  if( cursor->poly_cnt <= 1 )
    return;
  points = cursor->points;
  i = index;
  for( j=i+1; j<cursor->poly_cnt; j++ ) {
    points[i].x = points[j].x;
    points[i].y = points[j].y;
    rectangles[i].x = rectangles[j].x;
    rectangles[i].y = rectangles[j].y;
    if( !(poly[i].unset = poly[j].unset) ) {
      poly[i].winX = poly[j].winX;
      poly[i].winY = poly[j].winY;
      poly[i].fileX = poly[j].fileX;
      poly[i].fileY = poly[j].fileY;
    }
    i = j;
  }
  /* the first point is repeated at top for drawing */
  points[i].x = points->x;
  points[i].y = points->y;
  /* updates the counts */
  cursor->point_cnt--;
  cursor->poly_cnt--;
  cursor->rectangle_cnt--;
  /* active stays with its vertex, or if deleted, the preivous vertex */
  if( cursor->ctrl.active_vertex >= index ) {
    if( --(cursor->ctrl.active_vertex) < 0 )
      cursor->ctrl.active_vertex = cursor->poly_cnt - 1;
  }
}

/*
 * Subroutine:	set_polygon_hashmarks
 */
void set_polygon_hashmarks ( cursor )
     struct cursorRec *cursor;
{
  XPoint *points;
  int i;

  points = cursor->points;
  for( i=0; i<cursor->poly_cnt; i++ ) {
    rectangles[i].x = points[i].x - HASH_RAY;
    rectangles[i].y = points[i].y - HASH_RAY;
  }
}

/*
 * Subroutine:	set_active_polygon_hashmark
 */
void set_active_polygon_hashmark ( cursor )
     struct cursorRec *cursor;
{
  int active;

  active = cursor->ctrl.active_vertex;
  rectangles[active].x = cursor->points[active].x - HASH_RAY;
  rectangles[active].y = cursor->points[active].y - HASH_RAY;
}

/*
 * Subroutine:	copy_polygon_region_to_cursor
 * Purpose:	Install stuff to making a polygon copy the active cursor
 * Note:	Must be called by copy_cursor()
 */
void copy_polygon_region_to_cursor ( region, cursor )
     struct cursorRec *region;
     struct cursorRec *cursor;
{
  XPoint *points;
  int i;

  points = cursor->points;
  cursor->rectangles = rectangles;
  cursor->poly = poly;
  cursor->rectangle_cnt = cursor->poly_cnt;
  bcopy((char *)region->points, (char *)points,
	region->point_cnt * sizeof(XPoint));
  bcopy((char *)region->poly, (char *)poly,
	region->poly_cnt * sizeof(PolyPoint));
  for( i=0; i<cursor->poly_cnt; i++ ) {
    rectangles[i].x = points[i].x - HASH_RAY;
    rectangles[i].y = points[i].y - HASH_RAY;
  }
}
