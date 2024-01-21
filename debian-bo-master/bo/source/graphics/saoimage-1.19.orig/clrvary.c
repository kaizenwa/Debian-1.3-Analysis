#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clrvary.c (Color Vary)
 * Purpose:	Interactive color manipulation keyed to mouse position
 * Subroutine:	vary_colors()				returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 15 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define mode codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/cgraph.h"	/* color graph structs */
#include "hfiles/define.h"	/* define SQR */

extern struct cgraphRec cgraph;

/*
 * Subroutine:	vary_colors
 * Purpose:	Modify colors as per mode and the mouse position
 */
void vary_colors ( event, mode, track, x, y, width, height )
     XEvent *event;	/* i: details of event to check button states */
     int mode;		/* i: code for type of varying */
     int track;		/* i: update graphbox displays */
     int x, y;		/* i: mouse coords (could be adjusted) */
     int width, height;	/* i: dimensions bby which to judge control */
{
  double threshold, saturation;
  double contrast, bias;
  double gamma;
  int button1, button2, button3;
  void make_cellmap_from_table(), make_cellstore_from_cellmaps();
  void vary_contrast_and_bias(), draw_cgraph(), label_gamma();

  if( event->xbutton.state & ControlMask ) {
    button1 = (event->xbutton.state & Button1Mask);
    button2 = (event->xbutton.state & Button2Mask);
    button3 = (event->xbutton.state & Button3Mask);
    if( event->type == ButtonPress ) {
      button1 |= (event->xbutton.button == Button1);
      button2 |= (event->xbutton.button == Button2);
      button3 |= (event->xbutton.button == Button3);
    }
  } else {
    button1 = 1;
    button2 = 1;
    button3 = 1;
  }
  /* gamma is horizontal */
  if( mode == VOP_gamma ) {
    /* ranges from 0.0 to 4.0, with 1.0 in the middle */
    gamma = SQR((double)(2 * x) / (double)width);
    if( button1 ) {
      color.ctable.red.do_gamma = 1;
      color.ctable.red.gamma = gamma;
      make_cellmap_from_table(&color.ctable.red);
      cgraph.red.unset = 1;
    }
    if( button2 ) {
      color.ctable.green.do_gamma = 1;
      color.ctable.green.gamma = gamma;
      make_cellmap_from_table(&color.ctable.green);
      cgraph.green.unset = 1;
    }
    if( button3 ) {
      color.ctable.blue.do_gamma = 1;
      color.ctable.blue.gamma = gamma;
      make_cellmap_from_table(&color.ctable.blue);
      cgraph.blue.unset = 1;
    }
    if( graphbox.active )
      label_gamma(button1, button2, button3 );
  } else {
    /* make y increase from the bottom */
    y = height - y;
    if( mode == VOP_ThreshSat ) {
      /* threshold is parallel to the color bar, saturation is perpendicular */
      /* threshold varies from -1 to 1, saturation varies from 0 to 2 */
      threshold = ((double)(x * 2) / (double)width) - 1.0;
      saturation = (double)(y * 2) / (double)height;
      contrast = saturation - threshold;
      bias = (threshold + saturation) / 2;
    } else if( mode == VOP_ContBias ) {
      /* make x parallel to color bar */
      /* bias is parallel to the color bar, contrast is perpendicular */
      /* contrast varies from 0 to 2 */
      contrast = (double)(2 * y) / (double)height;
      /* bias ranges from (0.0 - contrast/2) to (1.0 + contrast/2) */
      bias = (double)x / (double)width;
      /* adjust bias to control full range for any contrast */
      bias += ((bias - 0.5) * contrast);
    } else
      return;
    if( button1 ) {
      vary_contrast_and_bias(&color.ctable.red, contrast, bias);
      make_cellmap_from_table(&color.ctable.red);
      cgraph.red.unset = 1;
    }
    if( button2 ) {
      vary_contrast_and_bias(&color.ctable.green, contrast, bias);
      make_cellmap_from_table(&color.ctable.green);
      cgraph.green.unset = 1;
    }
    if( button3 ) {
      vary_contrast_and_bias(&color.ctable.blue, contrast, bias);
      make_cellmap_from_table(&color.ctable.blue);
      cgraph.blue.unset = 1;
    }
  }
  /* update the server colormap */
  make_cellstore_from_cellmaps(&color);
  XStoreColors(color.display, color.colormap, color.cellstore, color.ncolors);
  if( track && graphbox.active )
    /* update the color graph */
    draw_cgraph(0, 0);
}

/*
 * Subroutine:	vary_contrast_and_bias
 * Purpose:	change color table cell_levels as per new contrast and bias
 */
void vary_contrast_and_bias ( table, contrast, bias )
     struct subtableRec *table;
     double contrast;		/* i: normal = 1.0 */
     double bias;		/* i: normal = 0.5 */
{
  double *base_level, *cell_level;
  int vertex_cnt;
  static void invert_table();

  base_level = table->base_level;
  cell_level = table->cell_level;
  vertex_cnt = table->vertex_cnt;
  if( contrast < 0.0 ) {
    int i, j;
    if( table->invert_order == 0 )
      invert_table(table);
    for( i=0, j=vertex_cnt - 1; i<vertex_cnt; i++, j-- )
      cell_level[i] = (base_level[j] * contrast) + bias;
  } else {
    int i;
    if( table->invert_order )
      invert_table(table);
    for( i=0; i<vertex_cnt; i++ )
      cell_level[i] = (base_level[i] * contrast) + bias;
  }
  table->bias = bias;
  table->contrast = contrast;
}

static void invert_table ( table )
     struct subtableRec *table;
{
  int i, j;
  double *intensity;
  intensity = table->intensity;
  for( i = 0, j = table->vertex_cnt - 1; i < j; i++, j-- ) {
    double temp;
    temp = intensity[i];
    intensity[i] = intensity[j];
    intensity[j] = temp;
  }
  table->invert_order ^= 1;
}
