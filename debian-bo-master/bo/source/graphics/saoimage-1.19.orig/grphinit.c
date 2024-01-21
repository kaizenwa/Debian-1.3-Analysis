#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphinit.c (Color Graph Initialize)
 * Purpose:	Initialize or reset color bar and graph
 * Subroutine:	init_colorbox()			returns: void
 * Subroutine:	init_main_colorbar()		returns: void
 * Subroutine:	adjust_main_colorbar()		returns: void
 * Subroutine:	init_graph_colorbar()		returns: void
 * Subroutine:	adjust_graph_colorbar()		returns: void
 * Subroutine:	map_halftone_colorbar()		returns: void
 * Subroutine:	adjust_color_graph()		returns: void
 * Extern:	cgraph in CgraphCtrl.c (from Cgraph.def)
 * Xlib calls:	XCreateSimpleWindow(), XSelectInput(), XMapSubwindows()
 * Xlib calls:	XUnmapWindow(), XMapWindow()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 11 June 1989
 *		{1} Jay Travisano (STScI)  VMS,IMTOOL changes    10 Nov  1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/cgraph.h"

extern struct cgraphRec cgraph;
extern struct colbarRec colorbar;
/* flag to set color graph background black, else white */
int black_graph_background = 0;

#if VMS && IMTOOL
extern void XZ_ast();
extern int  XZ_efn;
#endif

/*
 * Subroutine:	init_colorbox
 * Purpose:	Handle program init-time color bar and graph initialization
 */
void init_colorbox ( )
{
  static void init_main_colorbar();

  cgraph.ncolors = color.ncolors;
  cgraph.red.table = &color.ctable.red;
  cgraph.green.table = &color.ctable.green;
  cgraph.blue.table = &color.ctable.blue;
  cgraph.red.draw = &color.gcset.red;
  cgraph.green.draw = &color.gcset.green;
  cgraph.blue.draw = &color.gcset.blue;
  cgraph.queue[cgraph.red.queue_index] = &cgraph.red;
  cgraph.queue[cgraph.green.queue_index] = &cgraph.green;
  cgraph.queue[cgraph.blue.queue_index] = &cgraph.blue;
  init_main_colorbar();
}

/*
 * Subroutine:	init_main_colorbar
 * Purpose:	Initialize color bar and related params
 * Xlib calls:	XCreateSimpleWindow(), XMapSubwindows()
 */
static void init_main_colorbar ( )
{
  char *calloc_errchk();
  static void set_colorbar_image(), set_colorbar_params();

  colorbar.display = colorbox.display;
  colorbar.ref_width = colorbox.width;
  colorbar.ref_height = colorbox.height;
  colorbar.image = &colorbox.image;
  set_colorbar_params(&colorbar, colorbox.xwidth, colorbox.yheight);
  colorbar.ID =
   XCreateSimpleWindow(colorbar.display, colorbox.ID,
		       colorbox.xzero - BDRWDTH, colorbox.yzero - BDRWDTH,
		       colorbar.width, colorbar.height, BDRWDTH,
		       color.gcset.menu.foreground,
		       color.gcset.menu.background);
  XMapWindow(colorbox.display, colorbar.ID);
  set_colorbar_image(&colorbar, 0);
}

/*  
 * Subroutine:	adjust_main_colorbar
 * Purpose:	Adjust color bar size params and/or color data if needed
 */
void adjust_main_colorbar ( )
{
  char *calloc_errchk();
  static void set_colorbar_image(), set_colorbar_params();

  if( (colorbox.width != colorbar.ref_width) ||
      (colorbox.height != colorbar.ref_height) ) {
    set_colorbar_params(&colorbar, colorbox.xwidth, colorbox.yheight);
    /* set flag in case somebody wants to coordinate */
    XResizeWindow(colorbar.display, colorbar.ID, colorbar.width,
		  colorbar.height);
    set_colorbar_image(&colorbar, 0);
  } else if( color.ncolors != colorbar.ncolors )
    set_colorbar_image(&colorbar, 0);
}

/*
 * Subroutine:	init_graph_colorbar
 * Purpose:	Initialize color bar and related params
 * Xlib calls:	XMapSubwindows()
 */
void init_graph_colorbar ( )
{
  Window create_cgraph_box();
  char *calloc_errchk();
  static void set_colorbar_image(), set_colorbar_params();

  cgraph.bar.display = graphbox.display;
  cgraph.bar.ref_width = graphbox.width;
  cgraph.bar.ref_height = graphbox.height;
  cgraph.bar.image = &graphbox.image;
  if( cgraph.vertical ) {
    set_colorbar_params(&cgraph.bar,
			cgraph.barlabel.width - 4, cgraph.graph.height);
    cgraph.bar.ID =
      create_cgraph_box(1, graphbox.yzero - 1,
			cgraph.bar.width, cgraph.bar.height,
			graphbox.display, graphbox.ID,
			color.hard.true_black, NorthWestGravity);
  } else {
    set_colorbar_params(&cgraph.bar,
			cgraph.graph.width, cgraph.barlabel.height - 4);
    cgraph.bar.ID =
      create_cgraph_box(graphbox.xzero - 1,
			graphbox.height - cgraph.barlabel.height,
			cgraph.bar.width, cgraph.bar.height,
			graphbox.display, graphbox.ID,
			color.hard.true_black, SouthWestGravity);
  }
  XMapWindow(graphbox.display, cgraph.bar.ID);
  set_colorbar_image(&cgraph.bar, cgraph.vertical);
}

/*  
 * Subroutine:	adjust_graph_colorbar
 * Purpose:	Adjust color bar size params and/or color data if needed
 */
void adjust_graph_colorbar ( )
{
  int resize = 0;
  char *calloc_errchk();
  static void set_colorbar_image(), set_colorbar_params();

  if( cgraph.vertical ) {
    if( graphbox.height != cgraph.bar.ref_height ) {
      set_colorbar_params(&cgraph.bar,
			  cgraph.barlabel.width - 5, cgraph.graph.height);
      resize = 1;
    }
  } else {
    if( graphbox.width != cgraph.bar.ref_width ) {
      set_colorbar_params(&cgraph.bar,
			  cgraph.graph.width, cgraph.barlabel.height - 3);
      resize = 1;
    }
  }
  if( resize )
    XResizeWindow(cgraph.bar.display, cgraph.bar.ID, cgraph.bar.width,
		  cgraph.bar.height);
  /* if size was or the number of colors were changed */
  if( resize || (color.ncolors != cgraph.bar.ncolors) )
    set_colorbar_image(&cgraph.bar, cgraph.vertical);
}

/*
 * Subroutine:	map_halftone_colorbar
 * Purpose:	Create and draw the color bar
 */
void map_halftone_colorbar ( disp, graph )
     int disp;
     int graph;	/* graphbox colorbar, else colorbox colorbar */
{
  struct colbarRec *bar;
  void make_halftone_colorbar(), draw_colorbar();

  /* distinguish between two possible colorbars */
  if( graph )
    bar = &cgraph.bar;
  else
    bar = &colorbar;
  make_halftone_colorbar((unsigned char *)bar->byte_data,
			 (unsigned char *)bar->bit_data,
			 (int)bar->width, (int)bar->height,
			 bar->bytes_per_bit_line);
  if( disp )
    draw_colorbar(graph);
}

static void set_colorbar_params ( bar, width, height )
     struct colbarRec *bar;
     int width, height;
{
  int data_sz;
  char *calloc_errchk();

  bar->width = width;
  bar->height = height;
  bar->bytes_per_bit_line = (bar->width + 7) / 8;
  bar->image->width = bar->width;
  bar->image->height = bar->height;
  data_sz = bar->width * (bar->height + bar->bytes_per_bit_line);
  if( data_sz > bar->data_size ) {
    if( bar->byte_data != NULL )
      free(bar->byte_data);
    bar->byte_data = calloc_errchk(data_sz, sizeof(char), "color bar");
    bar->bit_data = &bar->byte_data[bar->width * bar->height];
    bar->data_size = data_sz;
  }
}

/*
 * Subroutine:	set_colorbar_image
 * Purpose:	Set the color bar for display of current color set
 */
static void set_colorbar_image ( bar, vertical )
     struct colbarRec *bar;
     int vertical;
{
  void fill_colorbar(), make_halftone_colorbar();

  cgraph.disp = &(color.gcset.disp);
  cgraph.menu = &(color.gcset.menu);
  bar->ncolors = color.ncolors;
  if( color.ncolors > 1 ) {
    fill_colorbar((unsigned char *)bar->byte_data,
		  (int)bar->width, (int)bar->height, 0, bar->ncolors - 1,
		  vertical, vertical, color.pixvalmap);
    bar->image->data = bar->byte_data;
    bar->image->bytes_per_line = bar->width;
    bar->image->format = ZPixmap;
    bar->image->depth = color.screen_depth;
  } else {
    fill_colorbar((unsigned char *)bar->byte_data,
		  (int)bar->width, (int)bar->height, 0, 255,
		  vertical, vertical, color.pixvalmap);
    make_halftone_colorbar((unsigned char *)bar->byte_data,
			   (unsigned char *)bar->bit_data,
			   (int)bar->width, (int)bar->height,
			   bar->bytes_per_bit_line);
    bar->image->data = bar->bit_data;
    bar->image->bytes_per_line = bar->bytes_per_bit_line;
    bar->image->format = XYBitmap;
    bar->image->depth = 1;
  }
}

/*
 * Subroutine:	adjust_color_graph
 * Purpose:	Adjust color graph size params and/or graph if needed
 * Xlib calls:	XCreateSimpleWindow(), XSelectInput(), XMapSubwindows()
 */
void adjust_color_graph ( )
{
  int resize, bkgd, graph_x;
  void init_cgraph_lines(), init_cgraph_hash();

  if( (cgraph.graph.ref_width != graphbox.width) ||
      (cgraph.graph.ref_height != graphbox.height) ) {
    cgraph.graph.ref_width = graphbox.width;
    cgraph.graph.ref_height = graphbox.height;
    if( cgraph.vertical ) {
      cgraph.graph.width = graphbox.xwidth - (cgraph.barlabel.width + 1);
      cgraph.graph.height = graphbox.yheight;
      graph_x = cgraph.barlabel.width;
    } else {
      cgraph.graph.width = graphbox.xwidth;
      cgraph.graph.height = graphbox.yheight - cgraph.barlabel.height;
      graph_x = graphbox.xzero - BDRWDTH;
    }
    /* server call placed as soon as possible to reduce delay */
    if( cgraph.graph.ID == NULL ) {
      cgraph.graph.display = graphbox.display;
      /* black used to show where cgraph lines overlap (white if black bkgd) */
      if( black_graph_background ) {
	bkgd = color.hard.true_black;
	cgraph.black = &(color.gcset.white);
      } else {
	bkgd = color.hard.std_white;
	cgraph.black = &(color.gcset.black);
      }
      /* graph window takes defaults NorthWestGravity, and spectial events */
      cgraph.graph.ID =
	XCreateSimpleWindow(graphbox.display, graphbox.ID,
			    graph_x, graphbox.yzero - BDRWDTH,
			    cgraph.graph.width, cgraph.graph.height, BDRWDTH,
			    color.hard.true_black, bkgd);
#if VMS && IMTOOL
      XSelectAsyncInput (cgraph.graph.display, cgraph.graph.ID,
		    ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    XZ_ast, XZ_efn);
#endif
      XSelectInput(cgraph.graph.display, cgraph.graph.ID,
		   ButtonPressMask | ButtonMotionMask | ButtonReleaseMask);
      XMapSubwindows(graphbox.display, graphbox.ID);
    } else {
      XResizeWindow(cgraph.graph.display, cgraph.graph.ID,
		    cgraph.graph.width, cgraph.graph.height);
    }
    /* offset "min" from bottom */
    cgraph.graphlabel.min_y = graphbox.height - cgraph.graphlabel.minmax_yoff;
    if( cgraph.vertical ) {
      cgraph.graph.xzero = INDENT;
      cgraph.graph.xwidth = cgraph.graph.width - (INDENT + INDENT);
      cgraph.graph.yzero = 0;
      cgraph.graph.yheight = cgraph.graph.height;
      cgraph.graphlabel.max_x = graphbox.width - cgraph.graphlabel.minmax_xoff;
      cgraph.graphlabel.max_y = cgraph.graphlabel.min_y;
    } else {
      cgraph.graph.xzero = 0;
      cgraph.graph.xwidth = cgraph.graph.width;
      cgraph.graph.yzero = INDENT;
      cgraph.graph.yheight = cgraph.graph.height - (INDENT + INDENT);
    }
    cgraph.graph.Xwidth = (double)cgraph.graph.xwidth;
    cgraph.graph.Yheight = (double)cgraph.graph.yheight;
    cgraph.graph.xmax = cgraph.graph.xzero + cgraph.graph.xwidth;
    cgraph.graph.ymax = cgraph.graph.yzero + cgraph.graph.yheight;
    resize = 1;
  } else
    resize = 0;
  if( resize || (cgraph.graph.ncolors != color.ncolors) ) {
    cgraph.graph.ncolors = color.ncolors;
    cgraph.point_cnt = cgraph.graph.ncolors;
    cgraph.graph.Xinc = cgraph.graph.Xwidth/(double)cgraph.graph.ncolors;
    cgraph.graph.Yinc = cgraph.graph.Yheight/(double)cgraph.graph.ncolors;
    init_cgraph_lines(cgraph.red.line, cgraph.green.line, cgraph.blue.line);
    init_cgraph_hash();
  }
}
                                  

                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                           
