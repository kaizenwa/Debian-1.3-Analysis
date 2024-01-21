#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grpholap.c (Color Graph Overlap)
 * Purpose:	Draw color graph lines where they overlap
 * Subroutine:	mark_colorline_overlap()		returns: void
 * Xlib calls:	XDrawSegments(), XDrawRectangles()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 11 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* color structs */
#include "hfiles/cgraph.h"	/* color graph structs */

extern struct cgraphRec cgraph;

/*
 * Subroutine:	mark_colorline_overlap
 * Purpose:	Make line representing more than one color black (or white)
 */
void mark_colorline_overlap ( )
{
  int i, j;
  int x, y;
  int x1, y1;
  XSegment line[256];
  static void mark_hashmark_overlap();

  j = 0;
  if( cgraph.vertical ) {
    /* if vertical */
    x1 = -1;
    y1 = 0;	/* not really needed, but lint will complain */
    for( i = 0; i < cgraph.point_cnt; i++ ) {
      if( ((x = cgraph.red.line[i].x) == cgraph.green.line[i].x) ||
	  ((x = cgraph.green.line[i].x) == cgraph.blue.line[i].x) ||
	  ((x = cgraph.blue.line[i].x) == cgraph.red.line[i].x) ) {
	if( x1 >= 0 ) {
	  /* start overlap section */
	  line[j].x1 = x1;
	  line[j].y1 = y1;
	  line[j].x2 = x;
	  line[j].y2 = cgraph.red.line[i].y;
	  j++;
	}
	x1 = x;
	y1 = cgraph.red.line[i].y;
      } else
	x1 = -1;
    }
  } else {
    /* if horizontal */
    x1 = 0;	/* not really needed, but lint will complain */
    y1 = -1;
    for( i = 0; i < cgraph.point_cnt; i++ ) {
      if( ((y = cgraph.red.line[i].y) == cgraph.green.line[i].y) ||
	  ((y = cgraph.green.line[i].y) == cgraph.blue.line[i].y) ||
	  ((y = cgraph.blue.line[i].y) == cgraph.red.line[i].y) ) {
	if( y1 >= 0 ) {
	  /* start overlap section */
	  line[j].x1 = x1;
	  line[j].y1 = y1;
	  line[j].x2 = cgraph.red.line[i].x;
	  line[j].y2 = y;
	  j++;
	}
	x1 = cgraph.red.line[i].x;
	y1 = y;
      } else
	y1 = -1;
    }
  }
  if( j > 0 ) {
    GC gc, set_gc();
    gc = set_gc(cgraph.black);
    XDrawSegments(cgraph.graph.display, cgraph.graph.ID, gc, line, j);
    mark_hashmark_overlap();
  }
}
	  
/*
 * Subroutine:	mark_hashmark_overlap
 * Purpose:	Make hashmarks which represent more than one color black
 */
static void mark_hashmark_overlap ( )
{
  int ri, bi, gi, j;
  int red_cnt, blue_cnt, green_cnt;
  int x, y;
  XRectangle hash[256];

  bi = 0;
  gi = 0;
  j = 0;
  red_cnt = cgraph.red.hash_cnt;
  blue_cnt = cgraph.blue.hash_cnt;
  green_cnt = cgraph.green.hash_cnt;
  if( cgraph.vertical ) {
    /* catch matches between red and another */
    for( ri = 0; ri < red_cnt; ri++ ) {
      x = cgraph.red.hash[ri].x;
      y = cgraph.red.hash[ri].y;
      while( (bi < blue_cnt) && (cgraph.blue.hash[bi].y > y) )
	bi++;
      if( (x == cgraph.blue.hash[bi].x) && (y == cgraph.blue.hash[bi].y) ) {
	hash[j].x = x;
	hash[j].y = y;
	hash[j].width = HASH_SZ;
	hash[j].height = HASH_SZ;
	j++;
      } else {
	while( (gi < green_cnt) && (cgraph.green.hash[gi].y > y) )
	  gi++;
	if( (x == cgraph.green.hash[gi].x) &&
	    (y == cgraph.green.hash[gi].y) ) {
	  hash[j].x = x;
	  hash[j].y = y;
	  hash[j].width = HASH_SZ;
	  hash[j].height = HASH_SZ;
	  j++;
	}
      }
    }
    gi = 0;
    ri = 0;
    /* catch matches between blue and green and not red */
    for( bi = 0; bi < blue_cnt; bi++ ) {
      x = cgraph.blue.hash[bi].x;
      y = cgraph.blue.hash[bi].y;
      while( (gi < green_cnt) && (cgraph.green.hash[gi].y > y) )
	gi++;
      if( (x == cgraph.green.hash[gi].x) &&
	  (y == cgraph.green.hash[gi].y) ) {
	while( (ri < red_cnt) && (cgraph.red.hash[ri].y > y) )
	  ri++;
	if( (x != cgraph.red.hash[ri].x) || (y == cgraph.red.hash[ri].y) ) {
	  hash[j].x = x;
	  hash[j].y = y;
	  hash[j].width = HASH_SZ;
	  hash[j].height = HASH_SZ;
	  j++;
	}
      }
    }
  } else {
    /* catch matches between red and another */
    for( ri = 0; ri < red_cnt; ri++ ) {
      x = cgraph.red.hash[ri].x;
      y = cgraph.red.hash[ri].y;
      while( (bi < blue_cnt) && (cgraph.blue.hash[bi].x < x) )
	bi++;
      if( (x == cgraph.blue.hash[bi].x) && (y == cgraph.blue.hash[bi].y) ) {
	hash[j].x = x;
	hash[j].y = y;
	hash[j].width = HASH_SZ;
	hash[j].height = HASH_SZ;
	j++;
      } else {
	while( (gi < green_cnt) && (cgraph.green.hash[gi].x < x) )
	  gi++;
	if( (x == cgraph.green.hash[gi].x) &&
	    (y == cgraph.green.hash[gi].y) ) {
	  hash[j].x = x;
	  hash[j].y = y;
	  hash[j].width = HASH_SZ;
	  hash[j].height = HASH_SZ;
	  j++;
	}
      }
    }
    gi = 0;
    ri = 0;
    /* catch matches between blue and green and not red */
    for( bi = 0; bi < blue_cnt; bi++ ) {
      x = cgraph.blue.hash[bi].x;
      y = cgraph.blue.hash[bi].y;
      while( (gi < green_cnt) && (cgraph.green.hash[gi].x < x) )
	gi++;
      if( (x == cgraph.green.hash[gi].x) &&
	  (y == cgraph.green.hash[gi].y) ) {
	while( (ri < red_cnt) && (cgraph.red.hash[ri].x < x) )
	  ri++;
	if( (x != cgraph.red.hash[ri].x) || (y == cgraph.red.hash[ri].y) ) {
	  hash[j].x = x;
	  hash[j].y = y;
	  hash[j].width = HASH_SZ;
	  hash[j].height = HASH_SZ;
	  j++;
	}
      }
    }
  }
  if( j > 0 ) {
    GC gc, set_gc();
    gc = set_gc(cgraph.black);
    XDrawRectangles(cgraph.graph.display, cgraph.graph.ID, gc, hash, j);
  }
}
