#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrpoly1.c (Cursor Polygon)
 * Purpose:	Manipulate vertex lists for drawing polygon cursor
 * Subroutine:	grab_polygon_vertex()			returns: void
 * Subroutine:	size_polygon()				returns: void
 * Subroutine:	request_delete_polygon_vertex()		returns: int
 * Subroutine:	size_polygon_from_file_coords()		returns: void
 * Subroutine:	set_polygon_file_coords()		returns: void
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
#include "hfiles/coord.h"	/* coord structs */
#include "hfiles/cursor.h"	/* define cursor parameter structures */
#include "hfiles/define.h"	/* define SQR, etc */

extern struct colorRec color;	/* need to know color.gcset */

#define GRAB_RANGE 8

/*
 * Subroutine:	grab_polygon_vertex
 * Purpose:	Identify or create polygon vertex for manipulation
 *		Used when initiating a move or size action
 * Called by:	control_cursor() in CursorCtrl.c
 */
void grab_polygon_vertex ( cursor, event, size )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XbuttonEvent (ButtonPress) for x & y */
     int size;			/* i: distinguish size action from move */
{
  int x, y;			/* l: coordinates of event */
  int pt;			/* l: index of polygon vertex */
  int closest_polygon_line();
  void draw_cursor(), add_polygon_vertex();
  static int on_polygon_vertex();

  /* erase existing cursor */
  draw_cursor(cursor, &color.gcset.undraw);
  x = event->xbutton.x;
  y = event->xbutton.y;
  /* close enough to grab and existing vertex? */
  pt = on_polygon_vertex(x, y, cursor->points, cursor->poly_cnt);
  if( pt >= 0 ) {
    cursor->ctrl.active_vertex = pt;
  } else {
    if( size ) {
      /* decide in which line to add new point */
      cursor->ctrl.active_vertex =
	closest_polygon_line(x, y, cursor->points, cursor->poly_cnt);
      add_polygon_vertex(cursor, cursor->ctrl.active_vertex, x, y);
    }
  }
  if( size ) {
    /* size displaces one vertex */
    cursor->poly[cursor->ctrl.active_vertex].unset = 1;
  } else {
    /* move displaces all vertices */
    for( pt=0; pt<cursor->poly_cnt; pt++ )
      cursor->poly[pt].unset = 1;
  }
  /* make cursor coordinates those of the active vertex */
  cursor->win.x = cursor->points[cursor->ctrl.active_vertex].x;
  cursor->win.y = cursor->points[cursor->ctrl.active_vertex].y;
  cursor->win.X = (float)cursor->win.x + 0.5;
  cursor->win.Y = (float)cursor->win.y + 0.5;
}

/*
 * Subroutine:	request_delete_polygon_vertex
 * Purpose:	Remove a polygon vertex if mouse is close enough to grab it
 * Returns:	1 if image must be redrawn, else 0
 * Called by:	control_cursor() in CursorCtrl.c
 */
int request_delete_polygon_vertex ( cursor, event )
     struct cursorRec *cursor;
     XEvent *event;		/* i: XbuttonEvent (ButtonPress) for x & y */
{
  int i;
  void delete_polygon_vertex(), draw_cursor();
  static int on_polygon_vertex();

  i = on_polygon_vertex((int)event->xbutton.x, (int)event->xbutton.y,
			cursor->points, cursor->poly_cnt);
  if( i >= 0 ) {
    /* if changing cursor messes up image, request repainted image */ 
    if( cursor->overwrites_image_data ) {
      delete_polygon_vertex(cursor, i);
      return( 1 );
    }
    /* erase existing cursor */
    draw_cursor(cursor, &color.gcset.undraw);
    delete_polygon_vertex(cursor, i);
    /* draw new cursor */
    draw_cursor(cursor, cursor->draw);
  }
  return( 0 );
}

/*
 * Subroutine:	size_polygon
 * Purpose:	Move the tracked vertex of the polygon
 * Called by:	size_cursor() in CursorMove.c
 */
void size_polygon ( cursor, x, y )
     struct cursorRec *cursor;
     int x, y;
{
  void set_active_polygon_hashmark();

  cursor->points[cursor->ctrl.active_vertex].x = x;
  cursor->points[cursor->ctrl.active_vertex].y = y;
  if( cursor->ctrl.active_vertex == 0 ) {
    /* if moving first point, don't forget that last point returns here */
    cursor->points[cursor->poly_cnt].x = x;
    cursor->points[cursor->poly_cnt].y = y;
  }
  set_active_polygon_hashmark(cursor);
}

/*
 * Subroutine:	set_polygon_file_coords
 * Purpose:	Update all display coordinates and then the file coordinates
 *		for vertices
 * Called by:	control_cursor() in CursorCtrl.c
 */
void set_polygon_file_coords ( cursor, disptofile )
     struct cursorRec *cursor;
     Transform *disptofile;
{
  int i;
  PolyPoint *poly;
  void d_transform();

  /* update the cursor file coords */
  d_transform(disptofile, cursor->win.X, cursor->win.Y,
	      &cursor->file.X, &cursor->file.Y);
  /* update poly vertex file coords */
  for( i=0; i<cursor->poly_cnt; i++ ) {
    poly = &cursor->poly[i];
    /* update any poly coords if they have changed since last noted */
    if( poly->unset ) {
      poly->winX = 0.5 + (float)cursor->points[i].x;
      poly->winY = 0.5 + (float)cursor->points[i].y;
      poly->unset = 0;
    }
    d_transform(disptofile, (double)poly->winX, (double)poly->winY,
		&poly->fileX, &poly->fileY);
  }
}

/*
 * Subroutine:	set_polygon_from_file_coords
 * Purpose:	Update polygon coordinates from file coordinates for
 *		current display window
 * Called by:	adjust_cursor() in CursorCoord.c
 * Called by:	start_polygon() in CursorPolyPt.c
 * Called by:	make_polygon() in RegionMake.c
 */
void set_polygon_from_file_coords ( cursor, filetodisp, hash )
     struct cursorRec *cursor;
     Transform *filetodisp;
     int hash;
{
  int i;
  PolyPoint *poly;
  float X, Y;
  void d_transform(), set_polygon_hashmarks();

  for( i=0; i<cursor->poly_cnt; i++ ) {
    poly = &cursor->poly[i];
    d_transform(filetodisp, (double)poly->fileX, (double)poly->fileY,
		&poly->winX, &poly->winY);
    cursor->points[i].x = (short)poly->winX;
    cursor->points[i].y = (short)poly->winY;
    poly->unset = 0;
  }
  cursor->points[i].x = cursor->points[0].x;
  cursor->points[i].y = cursor->points[0].y;
  /* update vertex hash mark centers */
  if( hash )
    set_polygon_hashmarks(cursor);
  /* update the cursor coords as well */
  d_transform(filetodisp, (double)cursor->file.X, (double)cursor->file.Y,
	      &X, &Y);
  cursor->win.X = X;
  cursor->win.Y = Y;
  cursor->win.x = (int)X;
  cursor->win.y = (int)Y;
}

/*
 * Subroutine:	on_polygon_vertex
 * Purpose:	Test for being on a polygon point, return its index
 * Note:	If more than one vertex are within range, choose closest
 * Note:	(if ambiguity, favor point towards end - top down search)
 */
static int on_polygon_vertex ( x, y, vertex, cnt )
     int x, y;
     XPoint *vertex;
     int cnt;
{
  register int i;
  int match = -1;

  for( i=cnt-1; i>=0; i-- ) {
    if( (abs(x - vertex[i].x) < GRAB_RANGE) &&
        (abs(y - vertex[i].y) < GRAB_RANGE) ) {
      if( match >= 0 ) {
	/* if not only point within range, compare */
	if( (SQR(vertex[match].x - x) + SQR(vertex[match].y - y)) >
	    (SQR(vertex[i].x - x) + SQR(vertex[i].y - y)) )
	  match = i;
      } else
	match = i;
    }
  }
  return( match );
}
