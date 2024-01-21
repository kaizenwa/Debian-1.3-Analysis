#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module: 	clrsetup.c (Color Setup)
 * Purpose:	Set parameters for the current color mode
 * Subroutine:	init_overlay_color()		returns: void
 * Subroutine:	init_cell_color()		returns: void
 * Subroutine:	init_halftone_color()		returns: void
 * Xlib calls:	XStoreColors()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{1} MVH initialize flags in init_cellstore        15 Oct 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/color.h"		/* color structs */

/*
 * Subroutine:	init_overlay_color
 * Purpose:	Set up color parameters unique to overlay type usage
 * Xlib calls:	XStoreColors()
 */
void init_overlay_color ( color )
     struct colorRec *color;
{
  int ncolors;
  void make_cellstore_from_tables();
  static void set_cursor_overlay_color(), copy_xcolor_rgb(), init_cellstore();

  init_cellstore(color->cellstore, color->pixvalmap, color->ncolors);
  /* color in the cursor range of the color map */
  set_cursor_overlay_color(color, &color->cellstore[color->ncolors]);
  /* set aside two pixel values for region maps */
  color->ncolors -= 2;
  ncolors = color->ncolors;
  color->cur.color_one.pixel = color->pixvalmap[ncolors]; 
  color->cur.color_two.pixel = color->pixvalmap[ncolors + 1];
  color->image_plane_mask = ~color->overlay_mask;
  copy_xcolor_rgb(&(color->cur.color_one), &color->cellstore[ncolors]);
  copy_xcolor_rgb(&(color->cur.color_two), &color->cellstore[ncolors+1]);
  /* set up the cursor drawing parameters */
  color->gcset.draw.foreground = color->overlay_mask;
  color->gcset.draw.mask = color->overlay_mask;
  color->gcset.track.foreground = color->overlay_mask;
  color->gcset.track.mask = color->overlay_mask;
  color->gcset.track.func = GXcopy;
  color->gcset.undraw.foreground = 0;
  color->gcset.undraw.func = GXcopy;
  color->gcset.undraw.mask = color->overlay_mask;
  color->gcset.incl.foreground = color->cur.color_one.pixel;
  color->gcset.excl.foreground = color->cur.color_two.pixel;
  color->gcset.incl.background = color->pixvalmap[0];
  color->gcset.excl.background = color->gcset.incl.background;
  /* make or remake the color table */
  color->ctable.red.map_sz = ncolors;
  color->ctable.green.map_sz = ncolors;
  color->ctable.blue.map_sz = ncolors;
  make_cellstore_from_tables(color);
  /* store all the colors at the same time */
  XStoreColors(color->display, color->colormap,
	       color->cellstore, 2 * (ncolors + 2));
}

/*
 * Subroutine:	init_cell_color
 * Purpose:	Set up color parameters unique to cell usage without overlay
 * Xlib calls:	XStoreColors()
 */
void init_cell_color ( color )
     struct colorRec *color;
{
  void make_cellstore_from_tables();
  int alloc_cursor_cell_color();
  static void init_cellstore();

  init_cellstore(color->cellstore, color->pixvalmap, color->ncolors);
  /* Not cursor plane */
  color->image_plane_mask = AllPlanes;
  /* Set up the cursor drawing parameters */
  color->gcset.draw.foreground =
    alloc_cursor_cell_color(color, color->colormap);
  color->gcset.draw.mask = AllPlanes;
  color->gcset.track.foreground = AllPlanes;
  color->gcset.track.func = GXxor;
  color->gcset.track.mask = AllPlanes;
  color->gcset.undraw.foreground = AllPlanes;
  color->gcset.undraw.func = GXxor;
  color->gcset.undraw.mask = AllPlanes;
  color->gcset.incl.foreground = color->cur.disp_one;
  color->gcset.excl.foreground = color->cur.disp_two;
  color->gcset.incl.background = color->hard.true_black;
  color->gcset.excl.background = color->hard.true_black;
  /* Make or remake the color table */
  color->ctable.red.map_sz = color->ncolors;
  color->ctable.green.map_sz = color->ncolors;
  color->ctable.blue.map_sz = color->ncolors;
  make_cellstore_from_tables(color);
  /* Store all the colors at the same time */
  XStoreColors(color->display, color->colormap,
	       color->cellstore, color->ncolors);
}

/*
 * Subroutine:	init_halftone_color
 * Purpose:	Initialize color parameters for halftone mode
 */
void init_halftone_color ( color )
     struct colorRec *color;
{
  color->ncolors = 1;
  color->cursor_overlay = 0;
  color->gcset.disp.foreground = color->hard.std_black;
  color->gcset.disp.background = color->hard.std_white;
  color->gcset.draw.foreground = color->hard.std_black;
  color->gcset.draw.mask = AllPlanes;
  color->gcset.track.foreground = 1;
  color->gcset.track.func = GXxor;
  color->gcset.track.mask = 1;
  color->gcset.undraw.foreground = 1;
  color->gcset.undraw.func = GXxor;
  color->gcset.undraw.mask = 1;
  color->gcset.incl.foreground = 0;
  color->gcset.excl.foreground = 1;
  color->gcset.incl.background = 1;
  color->gcset.excl.background = 0;
  color->cur.disp_one = 0;
  color->cur.disp_two = 1;
  {
    /* set up a remapping table in the pixvalmap space (not used much yet) */
    register int i;
    register unsigned long *pixels;

    pixels = color->pixvalmap;
    for( i=0; i<256; i++ ) {
      *pixels = i;
      ++pixels;
    }
  }
}

/*
 * Subroutine:	init_cellstore
 * Purpose:	Set the pixel values in the color cellstore
 */
static void init_cellstore ( cellstore, pixvalmap, ncolors )
     XColor *cellstore;
     unsigned long *pixvalmap;
     int ncolors;
{
  int i;

  for( i=0; i<ncolors; i++ ) {
    cellstore[i].pixel = pixvalmap[i];
    cellstore[i].flags = DoRed | DoBlue | DoGreen;
  }
}

/*
 * Subroutine:	copy_xcolor_rgb
 * Purpose:	Copy the red, green, and blue valuesto another XColor
 */
static void copy_xcolor_rgb ( orig, dup )
     XColor *orig, *dup;
{
  dup->red = orig->red;
  dup->green = orig->green;
  dup->blue = orig->blue;
}

/*
 * Subroutine:	set_cursor_overlay_color
 * Purpose:	Set the cellstore entries for an overlay cursor
 */
static void set_cursor_overlay_color ( color, cellstore )
     struct colorRec *color;
     XColor *cellstore;
{
  unsigned long *pixvalmap, overlay_bit;
  unsigned short red, green, blue;
  int i;

  pixvalmap = color->pixvalmap;
  red = color->cur.color_cur.red;
  green = color->cur.color_cur.green;
  blue = color->cur.color_cur.blue;
  overlay_bit = (int)color->overlay_mask;
  for( i=0; i<color->ncolors; i++ ) {
    cellstore[i].pixel = pixvalmap[i] + overlay_bit;
    cellstore[i].red = red;
    cellstore[i].green = green;
    cellstore[i].blue = blue;
  }
}
