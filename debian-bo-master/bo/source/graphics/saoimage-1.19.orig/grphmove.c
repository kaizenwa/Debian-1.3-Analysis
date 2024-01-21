#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphmove.c (Color Graph Move)
 * Subroutine:	move_cgraph_vertices()		returns: void
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

extern struct cgraphRec cgraph;

/*
 * Subroutine:	move_cgraph_vertices
 * Purpose:	Change values (and position) of designated vertices in
 *		response to mouse movement
 */
void move_cgraph_vertices ( x, y )
     int x, y;
{
  double cell_level, intensity;
  int hash_x, hash_y;
  static void move_color_vertex();

  /* clip against limits */
  if( x < cgraph.graph.xzero )
    x = cgraph.graph.xzero;
  else if( x > cgraph.graph.xmax )
    x = cgraph.graph.xmax;
  if( y < cgraph.graph.yzero )
    y = cgraph.graph.yzero;
  else if( y > cgraph.graph.ymax )
    y = cgraph.graph.ymax;
  hash_x = x - HASH_RAY;
  hash_y = y - HASH_RAY;
  if( cgraph.vertical ) {
    cell_level = (cgraph.hash.Ymax - (double)hash_y) / cgraph.hash.Yheight;
    intensity = ((double)hash_x - cgraph.hash.Xzero) / cgraph.hash.Xwidth;
  } else {
    cell_level = ((double)hash_x - cgraph.hash.Xzero) / cgraph.hash.Xwidth;
    intensity = (cgraph.hash.Ymax - (double)hash_y) / cgraph.hash.Yheight;
  }
  /* clip against limits */
  if( cell_level < 0.0 )
    cell_level = 0.0;
  else if( cell_level > 1.0 )
    cell_level = 1.0;
  if( intensity < 0.0 )
    intensity = 0.0;
  else if( intensity > 1.0 )
    intensity = 1.0;
  /* move all active vertices */
  if( cgraph.red.active )
    move_color_vertex (&cgraph.red, hash_x, hash_y, cell_level, intensity);
  if( cgraph.green.active )
    move_color_vertex (&cgraph.green, hash_x, hash_y, cell_level, intensity);
  if( cgraph.blue.active )
    move_color_vertex (&cgraph.blue, hash_x, hash_y, cell_level, intensity);
}
 
/*
 * Subroutine:	move_color_vertex
 * Purpose:	Move one ccolor graph vertex
 */
static void move_color_vertex ( col, x, y, new_cell_level, new_intensity )
     struct colgRec *col;
     int x, y;
     double new_cell_level;
     double new_intensity;
{
  int active_hash;
  XRectangle *hash;

  active_hash = col->active_hash;
  hash = col->hash;
  if( cgraph.vertical ) {
    if( hash[active_hash].y == y ) {
      /* no change in y (cell_level) */
      if( hash[active_hash].x != x ) {
	hash[active_hash].x = x;
	col->table->intensity[col->active_vertex] = new_intensity;
      }
      return;
    } else {
      /* both x and y changed */
      double *cell_level = col->table->cell_level;
      double *base_level = col->table->base_level;
      double *intensity = col->table->intensity;
      int active_vertex = col->active_vertex;
      int vertex_cnt = col->table->vertex_cnt;
      if( y < hash[active_hash].y ) {
	/* moved up */
	if( (active_vertex < (vertex_cnt - 1)) &&
	    (cell_level[active_vertex + 1] < new_cell_level) ) {
	  /* position in list must change */
	  register int i, j;
	  for( i = active_vertex, j = i + 1;
	       ((j < vertex_cnt) && (cell_level[j] < new_cell_level));
	       i = j, j++ ) {
	    cell_level[i] = cell_level[j];
	    base_level[i] = base_level[j];
	    intensity[i] = intensity[j];
	    hash[active_hash].x = hash[active_hash+1].x;
	    hash[active_hash].y = hash[active_hash+1].y;
	    active_hash++;
	  }
	  active_vertex = i;
	  col->active_vertex = i;
	  col->active_hash = active_hash;
	}
      } else {
	/* moved down */
	if( (active_vertex > 0) &&
	    (cell_level[active_vertex - 1] > new_cell_level) ) {
	  /* position in list must change */
	  register int i, j;
	  for( i = active_vertex, j = i - 1;
	       ((i > 0) && (cell_level[j] > new_cell_level));
	       i = j, j-- ) {
	    cell_level[i] = cell_level[j];
	    base_level[i] = base_level[j];
	    intensity[i] = intensity[j];
	    hash[active_hash].x = hash[active_hash+1].x;
	    hash[active_hash].y = hash[active_hash+1].y;
	    active_hash--;
	  }
	  active_vertex = i;
	  col->active_vertex = i;
	  col->active_hash = active_hash;
	}
      }
      cell_level[active_vertex] = new_cell_level;
      base_level[active_vertex] =
	(new_cell_level - col->table->bias) / col->table->contrast;
      col->table->intensity[active_vertex] = new_intensity;
      hash[active_hash].x = x;
      hash[active_hash].y = y;
    }
  } else {
    /* horizontal */
    if( hash[active_hash].x == x ) {
      if( hash[active_hash].y != y ) {
	hash[active_hash].y = y;
	col->table->intensity[col->active_vertex] = new_intensity;
      }
      return;
    } else {
      /* cell_level changed */
      double *cell_level = col->table->cell_level;
      double *base_level = col->table->base_level;
      double *intensity = col->table->intensity;
      int active_vertex = col->active_vertex;
      int vertex_cnt = col->table->vertex_cnt;
      if( x > hash[active_hash].x ) {
	/* moved up */
	if( (active_vertex < (vertex_cnt - 1)) &&
	    (cell_level[active_vertex + 1] < new_cell_level) ) {
	  /* position in list must change */
	  register int i, j;
	  for( i = active_vertex, j = i + 1;
	       ((j < vertex_cnt) && (cell_level[j] < new_cell_level));
	       i = j, j++ ) {
	    cell_level[i] = cell_level[j];
	    base_level[i] = base_level[j];
	    intensity[i] = intensity[j];
	    hash[active_hash].x = hash[active_hash+1].x;
	    hash[active_hash].y = hash[active_hash+1].y;
	    active_hash++;
	  }
	  active_vertex = i;
	  col->active_vertex = i;
	  col->active_hash = active_hash;
	}
      } else {
	/* moved down */
	if( (active_vertex > 0) &&
	    (cell_level[active_vertex - 1] > new_cell_level) ) {
	  /* position in list must change */
	  register int i, j;
	  for( i = active_vertex, j = i - 1;
	       ((i > 0) && (cell_level[j] > new_cell_level));
	       i = j, j-- ) {
	    cell_level[i] = cell_level[j];
	    base_level[i] = base_level[j];
	    intensity[i] = intensity[j];
	    hash[active_hash].x = hash[active_hash+1].x;
	    hash[active_hash].y = hash[active_hash+1].y;
	    active_hash--;
	  }
	  active_vertex = i;
	  col->active_vertex = i;
	  col->active_hash = active_hash;
	}
      }
      cell_level[active_vertex] = new_cell_level;
      base_level[active_vertex] =
	(new_cell_level - col->table->bias) / col->table->contrast;
      col->table->intensity[active_vertex] = new_intensity;
      hash[active_hash].x = x;
      hash[active_hash].y = y;
    }
  }
  col->unset = 1;
}
