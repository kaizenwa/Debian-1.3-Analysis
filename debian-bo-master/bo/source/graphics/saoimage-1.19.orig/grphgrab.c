#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphgrab.c (Color Graph Grab Vertex)
 * Purpose:	Get or drop a color graph vertex as per a mouse button event
 * Subroutine:	grab_cgraph_vertex()		returns: void
 * Subroutine:	drop_cgraph_vertex()		returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 11 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* color structs */
#include "hfiles/cgraph.h"
#include "hfiles/define.h"	/* YES, NO, MIN, MAX and more */

extern struct cgraphRec cgraph;

#define GRAB_RANGE 8	/* radius in screen pixels within which to grab mark */

/*
 * Subroutine:	grab_cgraph_vertex
 * Purpose:	When a mouse button is pressed, assign a vertex to that button
 */
void get_cgraph_vertex ( event )
     XEvent *event;
{
  static void get_color_vertex();

  switch( event->xbutton.button ) {
  case Button1:
    /* note: Sun compiler sometimes muffs these pointers */
    get_color_vertex(event->xbutton.x, event->xbutton.y, &(cgraph.red));
    cgraph.red.active = 1;
    break;
  case Button2:
    get_color_vertex(event->xbutton.x, event->xbutton.y, &(cgraph.green));
    cgraph.green.active = 1;
    break;
  case Button3:
    get_color_vertex(event->xbutton.x, event->xbutton.y, &(cgraph.blue));
    cgraph.blue.active = 1;
    break;
  }
}

/*
 * Subroutine:	drop_cgraph_vertex
 * Purpose:	Delete a vertex if one is under the mouse pointer for drop
 *		event
 * Returns:	1 if a vertex was dropped, else 0
 */
int drop_cgraph_vertex ( event )
     XEvent *event;
{
  static int drop_color_vertex();

  switch( event->xbutton.button ) {
  case Button1:
    return( drop_color_vertex(&cgraph.red,
			      event->xbutton.x, event->xbutton.y) );
  case Button2:
    return( drop_color_vertex(&cgraph.green,
			      event->xbutton.x, event->xbutton.y) );
  case Button3:
    return( drop_color_vertex(&cgraph.blue,
			      event->xbutton.x, event->xbutton.y) );
  default:
    return( 0 );
  }
}

/*
 * Subroutine:	get_color_vertex
 * Purpose:	Handle grab vertex event for single color 
 * Method:	Grab old vertex or make new vertex - under mouse pointer
 */
static void get_color_vertex ( x, y, col )
     int x, y;
     struct colgRec *col;
{
  void install_draw_queue_end();
  static int grab_old_color_vertex();
  static void install_new_color_vertex();

  /* clip against limits */
  if( x < cgraph.graph.xzero )
    x = cgraph.graph.xzero;
  else if( x > cgraph.graph.xmax )
    x = cgraph.graph.xmax;
  if( y < cgraph.graph.yzero )
    y = cgraph.graph.yzero;
  else if( y > cgraph.graph.ymax )
    y = cgraph.graph.ymax;
  if( grab_old_color_vertex(x, y, col) == 0 )
    install_new_color_vertex(x, y, col);
  install_draw_queue_end(col);
}

/*
 * Subroutine:	grab_old_color_vertex
 * Purpose:	Check to see if position is close to an existing hash mark.
 *		If so, set active markers.
 * Returns:	1 if a vertex was grabbed, else 0
 */
static int grab_old_color_vertex ( x, y, col )
     int x, y;
     struct colgRec *col;
{
  int i, match;
  XRectangle *hash;

  hash = col->hash;
  if( col->hash_cnt <= 0 )
    return( 0 );
  /* hash mark positions are upper left corner of hash box */
  x -= HASH_RAY;
  y -= HASH_RAY;
  match = -1;
  /* loop to look for close vertices */
  for( i = 0; i < col->hash_cnt; i++) {
    if( (abs(hash[i].x - x) < GRAB_RANGE) &&
	(abs(hash[i].y - y) < GRAB_RANGE) ) {
      if( match < 0 ) {
	match = i;
      } else if( (SQR(hash[match].x - x) + SQR(hash[match].y - y)) >
		 (SQR(hash[i].x - x) + SQR(hash[i].y - y)) ) {
	/* if not only point within range, compare */
	match = i;
      }
    }
  }
  if( match >= 0 ) {
    col->active_hash = match;
    /* hash marks may be a contiguous subset of vertexes, use offset */
    col->active_vertex = match + col->hash_0;
    return( 1 );
  } else
    return( 0 );
}

/*
 * Subroutine:	install_new_color_vertex
 * Purpose:	Given hash position, install a new vertex, and make active
 */
static void install_new_color_vertex ( x, y, col )
     int x, y;			/* i: position of ULcorner of new hash mark */
     struct colgRec *col;	/* i: color info structure for one color */
{
  double intensity, cell_level;
  int match;
  int select_best_hash_position();
  static void add_color_vertex(), add_color_vertex_hash();

  col->active_hash = select_best_hash_position(x, y, col->hash, col->hash_cnt,
					       &match, cgraph.vertical);
  col->active_vertex = col->active_hash + col->hash_0;
  if( match >= 0 ) {
    /* new vertex must have same level as an existing one */
    cell_level = col->table->cell_level[match + col->hash_0];
  } else {
    if( cgraph.vertical )
      cell_level = (cgraph.hash.Ymax - (double)(y - HASH_RAY)) /
	cgraph.hash.Yheight;
    else
      cell_level = ((double)(x - HASH_RAY) - cgraph.hash.Xzero) /
	cgraph.hash.Xwidth;
    if( cell_level > 1.0 )
      cell_level = 1.0;
    else if( cell_level < 0.0 )
      cell_level = 0.0;
  }
  if( cgraph.vertical )
    intensity = ((double)(x - HASH_RAY) - cgraph.hash.Xzero) /
      cgraph.hash.Xwidth;
  else
    intensity = (cgraph.hash.Ymax - (double)(y - HASH_RAY)) /
      cgraph.hash.Yheight;
  if( intensity > 1.0 )
    intensity = 1.0;
  else if( intensity < 0.0 )
    intensity = 0.0;
  add_color_vertex(col->table, col->active_vertex, cell_level, intensity);
  add_color_vertex_hash(col, col->active_hash, x, y);
  col->unset = 1;
}

/*
 * Subroutine:	add_color_vertex
 * Purpose:	Install new vertex in a color subtableRec.
 */
static void add_color_vertex ( table, index, new_cell_level, new_intensity )
     struct subtableRec *table;	/* i/o: table structure for one color */
     int index;			/* i: index of new vertex */
     double new_cell_level;	/* i: cell_level and intensity of new vertex */
     double new_intensity;
{
  register int i, j;
  int count = table->vertex_cnt;
  double *cell_level = table->cell_level;
  double *base_level = table->base_level;
  double *intensity = table->intensity;

  /* make room for the new entry */
  for( i = count, j=i-1; i > index; i=j, j-- ) {
    cell_level[i] = cell_level[j];
    base_level[i] = base_level[j];
    intensity[i] = intensity[j];
  }
  /* install the new entry */
  cell_level[i] = new_cell_level;
  base_level[i] = (new_cell_level - table->bias) / table->contrast;
  intensity[i] = new_intensity;
  ++(table->vertex_cnt);
}

/*
 * Subroutine:	add_color_vertex_hash
 * Purpose:	Add a new hash mark
 */
static void add_color_vertex_hash ( col, index, x, y )
     struct colgRec *col;
     int index;
     int x, y;
{
  register int i, j;
  XRectangle *hash;
  int hash_cnt;

  hash_cnt = col->hash_cnt;
  hash = col->hash;
  for( i = hash_cnt, j = i - 1; i > index; i = j, j-- ) {
    hash[i].x = hash[j].x;
    hash[i].y = hash[j].y;
  }
  hash[i].x = x - HASH_RAY;
  hash[i].y = y - HASH_RAY;
  ++(col->hash_cnt);
}

/*
 * Subroutine:	drop_color_vertex
 * Purpose:	Delete a color table vertex if one is under the mouse position
 * Returns:	1 if vertex was dropped, else 0
 */
static int drop_color_vertex ( col, x, y )
     struct colgRec *col;
     int x, y;
{
  static int grab_old_color_vertex();

  /* hash mark positions are upper left corner of hash box */
  x -= HASH_RAY;
  y -= HASH_RAY;
  if( grab_old_color_vertex(x, y, col)  ) {
    register int i, j;
    {
      int count = col->table->vertex_cnt;
      double *cell_level = col->table->cell_level;
      double *base_level = col->table->base_level;
      double *intensity = col->table->intensity;
      for( i = col->active_vertex, j=i+1; j < count; i=j, j++ ) {
	cell_level[i] = cell_level[j];
	base_level[i] = base_level[j];
	intensity[i] = intensity[j];
      }
      --(col->table->vertex_cnt);
    }
    {
      int count = col->hash_cnt;
      XRectangle *hash = col->hash;
      for( i = col->active_hash, j=i+1; j < count; i=j, j++ ) {
	hash[i].x = hash[j].x;
	hash[i].y = hash[j].y;
      }
      --(col->hash_cnt);
    }
    col->unset = 1;
    return( 1 );
  } else
    return( 0 );
}
