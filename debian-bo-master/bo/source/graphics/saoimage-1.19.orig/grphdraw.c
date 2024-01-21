#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphdraw.c (Color Graph Draw)
 * Purpose:	Draw color graph lines
 * Subroutine:	draw_colorbar()				returns: void
 * Subroutine:	draw_cgraph()				returns: void
 * Subroutine:	highlight_active_cgraph_vertex()	returns: void
 * Subroutine:	install_draw_queue_end()		returns: void
 * Subroutine:	replace_draw_queue_end()		returns: void
 * Xlib calls:	XPutImage(), XDrawLines(), XDrawRectangles(), XFillRectangles()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 11 June 1989
 *		{1} MVH added graphbox colorbar to draw_colorbar  21 Oct 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* color structs */
#include "hfiles/cgraph.h"	/* color graph structs */

extern struct cgraphRec cgraph;
extern struct colbarRec colorbar;

/*
 * Subroutine:	draw_colorbar
 * Purpose:	Put the color bar on the screen
 * Xlib calls:	XPutImage()
 */
void draw_colorbar ( graph )
     int graph;		/* i: graphbox colorbar, else colorbox colorbar */
{
  GC gc, set_gc(), set_gc_with_background();
  if( cgraph.ncolors > 1 )
    gc = set_gc(cgraph.disp);
  else
    gc = set_gc_with_background(cgraph.disp, cgraph.disp->background);
  if( graph )
    XPutImage(cgraph.bar.display, cgraph.bar.ID, gc, cgraph.bar.image,
	      0, 0, 0, 0, cgraph.bar.width, cgraph.bar.height);
  else
    XPutImage(colorbar.display, colorbar.ID, gc, colorbar.image,
	      0, 0, 0, 0, colorbar.width, colorbar.height);
}
  
/*
 * Subroutine:	draw_cgraph
 * Purpose:	Draw the cgraph graph lines and hash marks
 * Xlib calls:	XDrawLines(), XDrawRectangles()
 */
void draw_cgraph ( redo, highlight )
     int redo;		/* i: recalculate-all-line-coordinates */
     int highlight;	/* i: highlight-vertex-hash-being-manipulated */
{
  int i;
  GC gc, set_gc();
  int set_cgraph_hash();
  void mark_colorline_overlap();
  void highlight_active_cgraph_vertex(), set_cgraph_line();

  if( cgraph.inactive )
    return;
  XClearWindow(cgraph.graph.display, cgraph.graph.ID);
  for( i=0; i<cgraph.queue_cnt; i++ ) {
    if( redo || cgraph.queue[i]->unset ) {
      if( cgraph.queue[i]->table->do_gamma )
	set_cgraph_line(cgraph.queue[i]->table->gammamap,
		        cgraph.queue[i]->line);
      else
	set_cgraph_line(cgraph.queue[i]->table->cellmap,
			cgraph.queue[i]->line);
      cgraph.queue[i]->hash_0 =
	set_cgraph_hash(cgraph.queue[i]->table,
			cgraph.queue[i]->hash, &cgraph.queue[i]->hash_cnt);
      cgraph.queue[i]->unset = 0;
    }
    gc = set_gc(cgraph.queue[i]->draw);
    XDrawLines(cgraph.graph.display, cgraph.graph.ID, gc,
	       cgraph.queue[i]->line, cgraph.point_cnt, CoordModeOrigin);
  }
  /* draw the hash marks */
  for( i=0; i<cgraph.queue_cnt; i++ ) {
    if( cgraph.queue[i]->hash_cnt ) {
      gc = set_gc(cgraph.queue[i]->draw);
      XDrawRectangles(cgraph.graph.display, cgraph.graph.ID, gc,
		      cgraph.queue[i]->hash, cgraph.queue[i]->hash_cnt);
    }
  }
  if( highlight )
    highlight_active_cgraph_vertex();
  else
    mark_colorline_overlap();
}

/*
 * Subroutine:	highlight_active_cgraph_vertex
 * Purpose:	Draw active hash mark as filled box
 * Xlib calls:	XFillRectangles()
 */
void highlight_active_cgraph_vertex ( )
{
  GCspec *draw;
  XRectangle *hash = NULL;
  if( cgraph.red.active ) {
    hash = &cgraph.red.hash[cgraph.red.active_hash];
    draw = cgraph.red.draw;
  }
  if( cgraph.green.active ) {
    if( hash == NULL ) {
      hash = &cgraph.green.hash[cgraph.green.active_hash];
      draw = cgraph.green.draw;
    } else
      draw = cgraph.black;
  }
  if( cgraph.blue.active ) {
    if( hash == NULL ) {
      hash = &cgraph.blue.hash[cgraph.blue.active_hash];
      draw = cgraph.blue.draw;
    } else
      draw = cgraph.black;
  }
  if( hash != NULL ) {
    GC gc, set_gc();
    gc = set_gc(draw);
    XFillRectangles(cgraph.graph.display, cgraph.graph.ID, gc, hash, 1);
  }
}
      
/*
 * Subroutine:	install_draw_queue_end
 * Purpose:	Installed newly active line to be last drawn
 */
void install_draw_queue_end ( col )
     struct colgRec *col;
{
  if( cgraph.queue_cnt != 3 )
    return;
  if( cgraph.queue[2] == col ) {
    return;
  } else if( cgraph.queue[1] == col ) {
    cgraph.queue[1] = cgraph.queue[2];
    cgraph.queue[2] = col;
  } else if( cgraph.queue[0] == col ) {
    cgraph.queue[0] = cgraph.queue[1];
    cgraph.queue[1] = cgraph.queue[2];
    cgraph.queue[2] = col;
  }
}

/*
 * Subroutine:	replace_draw_queue_end
 * Purpose:	Replace end of the draw queue when end color is no longer
 *		active
 */
void replace_draw_queue_end ( )
{
  struct colgRec *col;
  if( cgraph.queue_cnt != 3 )
    return;
  if( cgraph.queue[1]->active ) {
    col = cgraph.queue[2];
    cgraph.queue[2] = cgraph.queue[1];
    cgraph.queue[1] = cgraph.queue[0];
    cgraph.queue[0] = col;
  } else if( cgraph.queue[0]->active ) {
    col = cgraph.queue[2];
    cgraph.queue[2] = cgraph.queue[0];
    cgraph.queue[0] = col;
  }
}
