#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clralloc.c (Color Alloc)
 * Purpose:	Allocate and free colors
 * Subroutine:	alloc_color()			returns: int
 * Subroutine:	free_color_cells()		returns: void
 * Xlib calls:	XAllocColorCells(), XFreeColors()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{1} David Muchmore	private colormap	  20 May 1990
 *		{2} MVH Allow 4&5 plane machines to use color	   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/define.h"		/* YES, NO, MIN, MAX and more */
#include "hfiles/color.h"		/* color structs */

#define COPIES 20  /* max number of default colors to copy to private map */

static unsigned int _npixels;		/* for XAllocColorCells */
static unsigned int _nplanes;		/* for XAllocColorCells */
static unsigned long overlay_mask;	/* for XAllocColorCells */

/*
 * Subroutine:	alloc_colors
 * Purpose:	Allocate color cells with and overlay plane if possible
 * Returns:	1 if allocated an acceptable number of cells, else 0
 * Method:	Try for at least min cells in default colormap, if failed
 *		try to create a private colormap.
 */
int alloc_colors( color )
     struct colorRec *color;
{
  int verify_pseudocolor();
  static Colormap create_colormap();
  static int alloc_color_cells();

  if( (color->map.default_permit != NO) &&
      (color->map.default_enable == YES) &&
      (alloc_color_cells(color, color->map.default_colormap,
			 color->map.default_vinfo)) ) {
    /* If the default colormap can satisfy our desires */
    color->colormap = color->map.default_colormap;
    return 1;
  } else if( color->map.private_permit != NO ) {
    /* If the existance of a pseudocolor visual has not yet been tested */
    if( (color->map.private_enable == UNKNOWN) &&
        (verify_pseudocolor(color, 4, 16) == 0) ) {
      return 0;
    }
    if( color->map.private_enable == YES ) {
      if( color->map.private_used == UNKNOWN ) {
        if( (color->map.private_colormap =
	     create_colormap(color)) == 0 ) {
	  return 0;
	}
        color->colormap = color->map.private_colormap;
	/* Redo the cursor color allocation */
	init_hard_colors(color, color->colormap);
	lookup_cursor_colors(color, color->colormap, 1);
        color->map.private_used = 1;
	XInstallColormap(color->display, color->colormap);
        color->map.private_installed = 1;
        color->visual = color->map.private_vinfo->visual;
      }
      if (alloc_color_cells(color, color->map.private_colormap,
			 color->map.private_vinfo) ) {
	return 1;
      }
      else {
	return 0;
      }
    }
  }
  return 0;
}


/* Subroutine:	alloc_color_cells
 * Returns:	1 if alloc's at least min cells, else 0
 * PostState:
 * Xlib calls:	XAllocColorCells()
 * Method:	Grab as many cells as possible with overlay as specified
 */
static int alloc_color_cells ( color, colormap, vinfo )
     struct colorRec *color;	/* i: color info and handling struct */
     Colormap colormap;		/* i: colormap in which to allocate */
     XVisualInfo *vinfo;
{
  void free_color_cells();

  int contig = 0;
  /*  For those with few colors, let them see what they can do  */
  if( (vinfo->colormap_size < 100) && (vinfo->colormap_size < 8) ) {
    /*  Drop from original default (hopefully user didn't specify this)  */
    if( color->cells.min == 20 )
      color->cells.min = 4;
    /*  Reserve cells for image and 1 overlay  */
    if( color->cells.overlay ) {
      _nplanes = 1;
      /*  Allow for 8 standard colors plus 2 overlayable fixed colors  */
      _npixels = MIN(color->cells.wanted + 2, (vinfo->colormap_size) / 2);
    } else {
      _nplanes = 0;
      _npixels = MIN(color->cells.wanted, vinfo->colormap_size);
    }
  } else {
       /*  Reserve cells for image and 1 overlay  */
    if( color->cells.overlay ) {
      _nplanes = 1;
      /*  Allow for 8 standard colors plus 2 overlayable fixed colors  */
      _npixels = MIN(color->cells.wanted + 2, (vinfo->colormap_size - 8) / 2);
    } else {
      _nplanes = 0;
      _npixels = MIN(color->cells.wanted, vinfo->colormap_size - 8);
    }
  }
  while( (_npixels >= color->cells.min) &&
	 (XAllocColorCells(color->display, colormap, contig, &overlay_mask,
			   _nplanes, color->pixvalmap, _npixels) == NULL) )
    /* Try again with fewer if request failed (down to mincells) */
    _npixels--;
  if( _npixels < color->cells.min )
    return( 0 );
  color->colormap = colormap;
  color->ncolors = _npixels;
  if( color->cells.overlay )
    color->overlay_mask = overlay_mask;
  else
    color->overlay_mask = 0;
  color->colors_alloced = 1;
  return 1;
}

/*
 * Subroutine:	free_color_cells
 * Purpose:	un-alloc alloc'd color map cells
 * Xlib calls:	XFreeColors()
 */
void free_color_cells ( color, colormap )
     struct colorRec *color;	/* i: color info and handling struct */
     Colormap colormap;
{  
  XFreeColors (color->display, colormap, color->pixvalmap, _npixels, _nplanes);
  bzero((char *)color->pixvalmap, COLMAPSZ * sizeof(int));
  color->ncolors = 0;
  color->colors_alloced = 0;
}
  
/*
 * Subroutine:	create_colormap
 * Purpose:	Create a private colormap with some of the default map's colors
 *		copied into it.  The copied colors are static (reducing the
 *		number of available colors should the user attempt to increase
 *		them later on.
 */
static Colormap create_colormap ( color )
     struct colorRec *color;
{
  Colormap private_colormap;
  XColor cdef[COPIES];
  int color_levels, copy_levels, i;
  void exit_errmsg();

  /* Get the colormap */
  private_colormap =
    XCreateColormap(color->display, RootWindow(color->display, color->screen),
		    color->map.private_vinfo->visual, AllocNone);
  if( private_colormap ) {
    /* Copy all of the default map's colors into the new map */
    color_levels = DisplayCells(color->display, color->screen);
    copy_levels = MIN(COPIES, (color_levels - (color->cells.wanted + 10)));
    if( copy_levels > 0 ) {
      for( i=0; i<copy_levels; i++ ) {
	cdef[i].pixel = i;
	cdef[i].flags = DoRed|DoGreen|DoBlue;
      }
      XQueryColors(color->display,
		   DefaultColormap(color->display, color->screen),
		   cdef, copy_levels);
      /* We trust that the manger allocates starting at 0 */
      for( i=0; i<copy_levels; i++ )
	XAllocColor(color->display, private_colormap, &(cdef[i]));
    }
  } else
    exit_errmsg("Unable to create new colormap");
  /* use special distribution plan for allocation. */
  return private_colormap;
}
