#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphlbl.c (Color Graph Label)
 * Purpose:	Initialize or reset color bar and graph
 * Subroutine:	label_colorbar()		returns: void
 * Subroutine:	label_color_graph()		returns: void
 * Subroutine:	label_gamma()			returns: void
 * Subroutine:	init_graph_text()		returns: void
 * Extern:	normform, minform in WndwDesktop.c
 * Extern:	cgraph in CgraphCtrl.c (from Cgraph.def)
 * Xlib call:	XDrawImageString(), XTextExtents(), XTextWidth()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 22 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define MIN, etc. */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/cgraph.h"

extern struct cgraphRec cgraph;

/*
 * Subroutine:	label_colorbar
 * Purpose:	Label the maximum color level by that end of the color bar
 * Xlib call:	XDrawImageString()
 */
void label_colorbar ( )
{
  char max[4];
  GC gc, set_edit_gc();

  if( cgraph.graphlabel.active ) {
    gc = set_edit_gc(cgraph.font, cgraph.menu->foreground,
		     cgraph.menu->background);
    XDrawImageString(cgraph.bar.display, cgraph.barlabel.min_ID, gc,
		     cgraph.barlabel.min_x, cgraph.barlabel.min_y, "0", 1);
    if( color.ncolors > 1 )
      sprintf(max, "%3d", color.ncolors - 1);
    else
      sprintf(max, "255");
    XDrawImageString(cgraph.bar.display, cgraph.barlabel.max_ID, gc,
		     cgraph.barlabel.max_x, cgraph.barlabel.max_y, max, 3);
  }
}

/*
 * Subroutine:	label_color_graph
 * Purpose:	Draw entire graph label
 */
void label_color_graph ( )
{
  GC gc, set_edit_gc();
  void label_gamma();

  if( cgraph.graphlabel.active && graphbox.active ) {
    gc = set_edit_gc(cgraph.font, cgraph.menu->foreground,
		     cgraph.menu->background);
    if( cgraph.graphlabel.geq_active )
      XDrawImageString(cgraph.graph.display, cgraph.graphlabel.geq_ID, gc,
		       cgraph.graphlabel.geq_text_x,
		       cgraph.graphlabel.geq_text_y, "g=", 2);
    XDrawImageString(cgraph.graph.display, cgraph.graphlabel.minmax_ID, gc,
		     cgraph.graphlabel.min_x, cgraph.graphlabel.min_y,
		     "min", 3);
    XDrawImageString(cgraph.graph.display, cgraph.graphlabel.minmax_ID, gc,
		     cgraph.graphlabel.max_x, cgraph.graphlabel.max_y,
		     "max", 3);
    label_gamma(1, 1, 1);
  }
}

/*
 * Subroutine:	label_gamma
 * Purpose:	Label color gamma levels
 * Xlib call:	XDrawImageString()
 */
void label_gamma ( red, green, blue )
     int red, green, blue;
{
  double gamma;
  char string[8];
  GC gc, set_edit_gc();

  if( cgraph.graphlabel.active && graphbox.active ) {
    gc = set_edit_gc(cgraph.font, cgraph.menu->foreground,
		     cgraph.menu->background);
    if( red ) {
      gamma = MIN(color.ctable.red.gamma, 9.99);
      sprintf(string, "%3.2f", gamma);
      XDrawImageString(cgraph.graph.display, cgraph.graphlabel.red_ID, gc,
		       cgraph.graphlabel.box_x, cgraph.graphlabel.box_y,
		       string, 4);
    }
    if( green ) {
      if( color.ctable.green.gamma != gamma ) {
	gamma = MIN(color.ctable.green.gamma, 9.99);
	sprintf(string, "%3.2f", gamma);
      }
      XDrawImageString(cgraph.graph.display, cgraph.graphlabel.green_ID, gc,
		       cgraph.graphlabel.box_x, cgraph.graphlabel.box_y,
		       string, 4);
    }
    if( blue ) {
      if( color.ctable.blue.gamma != gamma ) {
	gamma = MIN(color.ctable.blue.gamma, 9.99);
	sprintf(string, "%3.2f", gamma);
      }
      XDrawImageString(cgraph.graph.display, cgraph.graphlabel.blue_ID, gc,
		       cgraph.graphlabel.box_x, cgraph.graphlabel.box_y,
		       string, 4);
    }
  }
}

/*
 * Subroutine:	init_cgraph_text
 * Purpose:	Set basic text parameters for color bar and graph labels
 * Xlib calls:	XTextExtents(), XTextWidth()
 * Note:	To be called before the color and graph windows are created
 */
void init_cgraph_text ( )
{
  int dir, ascent, descent;	/* l: character image bitmap extents */
  XCharStruct charstruct;	/* l: character bitmap pattern extents */
  XFontStruct *get_fontstruct();

  /* determine if colorbar will be vertical or horizontal */
  if( graphbox.hints.height > graphbox.hints.width )
    cgraph.vertical = 1;
  else
    cgraph.vertical = 0;
  /* get measurements of string using mini (0) font (check for failure) */
  if( (cgraph.fontstruct = get_fontstruct(0)) == NULL ) {
    cgraph.graphlabel.active = 0;
    return;
  }
  cgraph.graphlabel.active = 1;
  cgraph.font = cgraph.fontstruct->fid;
  XTextExtents(cgraph.fontstruct, "2.00", 4,
	       &dir, &ascent, &descent, &charstruct);
  /* define a box which nicely encloses text bitmap for "2.00" */
  cgraph.graphlabel.box_x = descent - (charstruct.lbearing);
  cgraph.graphlabel.box_width =
    descent + descent + (charstruct.rbearing - charstruct.lbearing);
  if( charstruct.descent > 1 )
    /* character's y position is defined high (above baseline) */
    cgraph.graphlabel.box_y = descent + charstruct.ascent - 1;
  else
    cgraph.graphlabel.box_y = descent + charstruct.ascent;
  cgraph.graphlabel.box_height = cgraph.graphlabel.box_y + descent;
  cgraph.graphlabel.geq_height = cgraph.graphlabel.box_height;
  if( cgraph.vertical ) {
    /* all boxes leave one pixel between border and top and bottom of window */
    cgraph.graphlabel.geq_y = 1;
    cgraph.graphlabel.red_y = 1;
    cgraph.graphlabel.green_y = 1;
    cgraph.graphlabel.blue_y = 1;
    cgraph.graphlabel.height = cgraph.graphlabel.box_height + 4;
    /* barlabel area has same height, minimum width */
    cgraph.barlabel.height = cgraph.graphlabel.height;
    cgraph.barlabel.base_width = 2 + cgraph.graphlabel.box_width -
      XTextWidth(cgraph.fontstruct, ".", 1);
    cgraph.barlabel.width = cgraph.barlabel.base_width;
    cgraph.barlabel.max_y = cgraph.graphlabel.box_y + 2;
    cgraph.barlabel.min_y = cgraph.barlabel.max_y;
    /* determine placement of "g=" */
    cgraph.graphlabel.geq_width = XTextWidth(cgraph.fontstruct, "g=", 2) + 5;
    cgraph.graphlabel.geq_text_x = 3;
    cgraph.graphlabel.geq_text_y = cgraph.graphlabel.box_y;
    /* limits are in x dimension (for labels and the color bar) */
    cgraph.graphlabel.three_limit = (3 * cgraph.graphlabel.box_width) + 9 +
      cgraph.barlabel.width;
    cgraph.graphlabel.four_limit =
      cgraph.graphlabel.three_limit + cgraph.graphlabel.geq_width + 4;
    /* "min" and "max" y is offset from bottom, "max" is offset from right */
    cgraph.graphlabel.minmax_xoff = XTextWidth(cgraph.fontstruct,"max", 3) + 3;
    cgraph.graphlabel.minmax_yoff =
      cgraph.graphlabel.height - (cgraph.graphlabel.box_y + 1);
    cgraph.graphlabel.min_x = 4 - charstruct.lbearing + cgraph.barlabel.width;
    /* max_x depends on the graphbox width and must be set later */
    /* set window parameters tied to the text size */
    graphbox.yzero = cgraph.graphlabel.height + BDRWDTH;
    /* set recommended graphbox sizes in structure used do determine size */
    graphbox.hints.min_width = cgraph.graphlabel.three_limit;
    graphbox.hints.width = cgraph.graphlabel.four_limit;
  } else {
    /* all boxes leave one pixel between border and sides of subwindow */
    cgraph.graphlabel.geq_x = 1;
    cgraph.graphlabel.red_x = 1;
    cgraph.graphlabel.green_x = 1;
    cgraph.graphlabel.blue_x = 1;
    cgraph.graphlabel.width = cgraph.graphlabel.box_width + 4;
    /* barlabel area has same width, minimum height */
    cgraph.barlabel.width = cgraph.graphlabel.width;
    cgraph.barlabel.height = cgraph.graphlabel.box_height;
    /* determine placement of "g=" */
    cgraph.graphlabel.geq_width = cgraph.graphlabel.box_width;
    cgraph.graphlabel.geq_text_x =
      (cgraph.graphlabel.box_width -
       XTextWidth(cgraph.fontstruct, "g=", 2)) / 2;
    cgraph.graphlabel.geq_text_y = cgraph.graphlabel.box_y;
    /* limits are in y dimension (enough for labels and the color bar) */
    cgraph.graphlabel.three_limit = (3 * cgraph.graphlabel.box_height) + 10 +
      cgraph.barlabel.height;
    cgraph.graphlabel.four_limit = (4 * cgraph.graphlabel.box_height) + 12 +
      cgraph.barlabel.height;
    /* "min" and "max" x is offset from right, min y is offset from bottom */
    cgraph.graphlabel.minmax_xoff =
      XTextWidth(cgraph.fontstruct, "max", 3) + 3;
    cgraph.graphlabel.minmax_yoff = descent + 3 + cgraph.barlabel.height;
    cgraph.graphlabel.max_y = ascent + 2;
    /* graphlabel.min_y is dependent on height and must be set later */
    /* width is fixed so x's can be determined now */
    cgraph.graphlabel.min_x =
      cgraph.graphlabel.width - cgraph.graphlabel.minmax_xoff;
    cgraph.graphlabel.max_x = cgraph.graphlabel.min_x;
    /* 0 and ncolors-1 are placed near side toward colorbar */
    cgraph.barlabel.max_x = 4 + charstruct.lbearing;
    cgraph.barlabel.min_x =
      cgraph.barlabel.width - (XTextWidth(cgraph.fontstruct, "0", 1) + 4);
    /* set window parameters tied to the text size */
    graphbox.xzero = cgraph.graphlabel.width + BDRWDTH;
    graphbox.hints.min_height = cgraph.graphlabel.three_limit;
    graphbox.hints.height = cgraph.graphlabel.four_limit;
  }
}
