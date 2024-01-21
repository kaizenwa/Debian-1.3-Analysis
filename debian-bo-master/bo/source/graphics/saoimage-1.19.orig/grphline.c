#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphline.c (Color Graph Line)
 * Purpose:	Assemble lines and hash marks for the color graph
 * Subroutine:	init_cgraph_lines()			returns: void
 * Subroutine:	set_cgraph_line()			returns: void
 * Subroutine:	init_cgraph_bars()			returns: void
 * Subroutine:	set_cgraph_bar()			returns: void
 * Subroutine:	init_cgraph_hash()			returns: void
 * Subroutine:	set_cgraph_hash()			returns: void
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
#include "hfiles/cgraph.h"	/* color graph structs */

extern struct cgraphRec cgraph;

/*
 * Subroutine:	init_cgraph_lines
 * Purpose:	Set the fixed color_linegraph coordinates (aligning segments
 *		with the corresponding colorbar stripe)
 */
void init_cgraph_lines ( rline, gline, bline )
     XPoint rline[];
     XPoint gline[];
     XPoint bline[];
{
  double inc;		/* l: ideal increment from one point to the next */
  register int i;

  if( cgraph.vertical ) {
    double Y;
    /* vertical */
    inc = cgraph.graph.Yinc;
    Y = (double)cgraph.graph.yzero + (inc / 2.0);
    for( i=0; i<cgraph.graph.ncolors; i++ ) {
      register int y = (int)Y;
      rline[i].y = y;
      gline[i].y = y;
      bline[i].y = y;
      Y += inc;
    }
  } else {
    double X;
    /* horizontal */
    inc = cgraph.graph.Xinc;
    X = (double)cgraph.graph.xzero + (inc / 2.0);
    for( i=0; i<cgraph.graph.ncolors; i++ ) {
      register int x = (int)X;
      rline[i].x = x;
      gline[i].x = x;
      bline[i].x = x;
      X += inc;
    }
  }
}

/*
 * Subroutine:	set_cgraph_line
 * Purpose:	Set the variable colorline coordinates representing intensity
 * PreState:	Must be preceded by init_colorhash whenever window size or
 *		color allocation is changed
 */
void set_cgraph_line ( cellmap, line )
     double cellmap[];		/* i: values being graphed (range 0.0-1.0) */
     XPoint line[];		/* i/o: line being initialized */
{
  int i, j;

  if( cgraph.vertical ) {
    double Xwidth = cgraph.graph.Xwidth;
    int xzero = cgraph.graph.xzero;
    /* vertical */
    for( i=cgraph.graph.ncolors-1, j=0; i>=0; i--, j++ )
      line[i].x = xzero + (int)(cellmap[j] * Xwidth);
  } else {
    double Yheight = cgraph.graph.Yheight;
    int ymax = cgraph.graph.ymax;
    /* horizontal */
    for( i=0; i<cgraph.graph.ncolors; i++ )
      line[i].y = ymax - (int)(cellmap[i] * Yheight);
  }
}

#ifdef NOTNEEDED /* %% not yet needed */
/*
 * Subroutine:	init_cgraph_bars
 * Purpose:	Set the fixed colorline coordinates (aligning segment with
 *		the corresponding colorbar stripe)
 * Params:	xzero, yzero, xwidth, yheight describe the graph drawing area
 *		within its (possibly larger) window
 * Method:	Graph starts at _zero edge, evenly spacing the segments to the
 *		far edge.  (count may be less than xwidth or yheight)
 */
void init_cgraph_bars ( rline, bline, gline )
     XPoint rline[];		/* i/o: red line being initialized */
     XPoint gline[];		/* i/o: green line being initialized */
     XPoint bline[];		/* i/o: blue line being initialized */
{
  double inc;		/* l: increment for even spacing */
  int npoints;		/* l: loop end for loop by 2 */
  int i;

  npoints = cgraph.graph.ncolors + cgraph.graph.ncolors;
  if( cgraph.vertical ) {
    double Y;		/* l: ideal position */
    int y;		/* l: realizable position */
    /* vertical */
    inc = cgraph.graph.Yinc;
    y = cgraph.graph.yzero;
    Y = (double)y;
    for( i=0; i<npoints; i += 2 ) {
      rline[i].y = y;
      gline[i].y = y;
      bline[i].y = y;
      Y += inc;
      y = (int)Y;
      rline[i+1].y = y;
      gline[i+1].y = y;
      bline[i+1].y = y;
    }
  } else {
    double X;		/* l: ideal position */
    int x;		/* l: realizable position */
    /* horizontal */
    inc = cgraph.graph.Xinc;
    x = cgraph.graph.xzero;
    X = (double)x;
    for( i=0; i<npoints; i += 2 ) {
      rline[i].x = x;
      gline[i].x = x;
      bline[i].x = x;
      X += inc;
      x = (int)X;
      rline[i+1].x = x;
      gline[i+1].x = x;
      bline[i+1].x = x;
    }
  }
}

/*
 * Subroutine:	set_cgraph_bar
 * Purpose:	Set the variable colorline coordinates representing intensity
 * Params:	xzero, yzero, xwidth, yheight describe the graph drawing area
 *		within its (possibly larger) window
 */
void set_cgraph_bar ( cellmap, line )
     double cellmap[];		/* i: values being graphed (range 0.0-1.0) */
     XPoint line[];		/* i/o: line being initialized */
{
  int i, j;

  if( cgraph.vertical ) {
    double Xwidth = cgraph.graph.Xwidth;
    int xzero = cgraph.graph.yzero;
    register int x;
    /* vertical */
    for( i=cgraph.graph.ncolors-1, j=i*2; i>=0; i--, j-=2 ) {
      x = xzero + (int)(cellmap[i] * Xwidth);
      line[j].x = x;
      line[j+1].x = x;
    }
  } else {
    double Yheight = cgraph.graph.Yheight;
    int ymax = cgraph.graph.ymax;
    register int y;
    /* horizontal */
    for( i=0, j=0; i<cgraph.graph.ncolors; i++, j+=2 ) {
      y = ymax - (int)(cellmap[i] * Yheight);
      line[j].y = y;
      line[j+1].y = y;
    }
  }
}
#endif

/*
 * Subroutine:	init_cgraph_hash
 * Purpose:	Initialize record parameters used to place the hash marks
 */
void init_cgraph_hash ( )
{
  if( cgraph.vertical ) {
    cgraph.hash.Xzero = (double)(cgraph.graph.xzero - HASH_RAY);
    cgraph.hash.Yzero = (double)(cgraph.graph.yzero - HASH_RAY) +
      (cgraph.graph.Yinc / 2.0);
    cgraph.hash.Xwidth = cgraph.graph.Xwidth;
    cgraph.hash.Yheight = cgraph.graph.Yheight - cgraph.graph.Yinc;
    cgraph.hash.Ymax = cgraph.hash.Yzero + cgraph.hash.Yheight;
  } else {
    cgraph.hash.Xzero = (double)(cgraph.graph.xzero - HASH_RAY) +
      (cgraph.graph.Xinc / 2.0);
    cgraph.hash.Yzero = (double)(cgraph.graph.yzero - HASH_RAY);
    cgraph.hash.Xwidth = cgraph.graph.Xwidth - cgraph.graph.Xinc;
    cgraph.hash.Yheight = cgraph.graph.Yheight;
    cgraph.hash.Ymax = cgraph.hash.Yzero + cgraph.hash.Yheight;
  }
}

/*
 * Subroutine:	set_cgraph_hash
 * Purpose:	Set rectangle positions to mark table vertexes for one color
 * PreState:	Must be preceded by init_colorhash whenever window size or
 *		color allocation is changed
 * Returns:	Table index of hash[0] (or where hypothetical hash[0] would be)
 */
int set_cgraph_hash ( table, hash, hash_cnt )
     struct subtableRec *table;	/* i: pseudocolor table structure */
     XRectangle hash[];		/* i/o: hash mark list */
     int *hash_cnt;		/* o: number of hash mark rectangles */
{
  double level;
  double *cell_level;		/* l: normalized cell level (range 0.0-1.0) */
  double *intensity;		/* l: normalized cell intensity */
  int vertex_cnt;		/* l: number of cell_level entries */
  int i, k;
  int hash_0 = 0;

  k = 0;
  cell_level = table->cell_level;
  intensity = table->intensity;
  vertex_cnt = table->vertex_cnt;
  for( i=0; i<vertex_cnt; i++ ) {
    level = cell_level[i];
    if( level < 0.0 ) {
      /* advance the table index of hash[0] ahead of sub-zero vertexes */
      hash_0 = i+1;
    } else if( level <= 1.0 ) {
      if( cgraph.vertical ) {
	hash[k].y = (int)(cgraph.hash.Ymax - (level * cgraph.hash.Yheight));
	hash[k].x = (int)
	  (cgraph.hash.Xzero + (intensity[i] * cgraph.hash.Xwidth));
      } else {
	hash[k].x = (int)(cgraph.hash.Xzero + (level * cgraph.hash.Xwidth));
	hash[k].y = (int)
	  (cgraph.hash.Ymax - (intensity[i] * cgraph.hash.Yheight));
      }
      k++;
    }
  }
  *hash_cnt = k;
  return( hash_0 );
}
