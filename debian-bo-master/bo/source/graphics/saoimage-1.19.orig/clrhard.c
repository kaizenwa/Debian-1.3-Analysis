#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clrhard.c (Color Hard)
 * Purpose:	Get common named colors
 * Subroutine:	init_hard_colors()		returns: void
 *		lookup_cursor_colors()		returns: void
 *		alloc_cursor_cell_color()	returns: int
 *		free_cursor_cell_color()	returns: void
 * Xlib calls:	XAllocNamedColor(), XAllocColor()
 * Xlib calls:	XFreeColors(), XLookupColor()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/color.h"		/* color structs */

/*
 * Subroutine:	init_hard_colors
 * Purpose:	Set up basic hardware colors
 */
void init_hard_colors ( color, colormap )
     struct colorRec *color;
     Colormap colormap;
{
  static int get_hard_color();

  color->hard.red =
    get_hard_color(color->display, colormap, "red", 63000, 0, 0);
  color->gcset.red.foreground = color->hard.red;
  color->hard.green =
    get_hard_color(color->display, colormap, "green", 0, 60000, 0);
  color->gcset.green.foreground = color->hard.green;
  color->hard.blue =
    get_hard_color(color->display, colormap, "blue", 0, 0, 65535);
  color->gcset.blue.foreground = color->hard.blue;
  color->hard.yellow =
    get_hard_color(color->display, colormap, "yellow", 65535, 65535, 0);
  color->hard.true_black =
    get_hard_color(color->display, colormap, "black", 0, 0, 0);
  color->gcset.black.foreground = color->hard.true_black;
  color->hard.true_white =
    get_hard_color(color->display, colormap, "white", 65535, 65535, 65535);
  color->gcset.white.foreground = color->hard.std_white;
}

/*
 * Subroutine:	lookup_cursor_colors
 * Purpose:	Get color parameters from named cursor colors
 */
void lookup_cursor_colors ( color, colormap, init )
     struct colorRec *color;
     Colormap colormap;
     int init;
{
  static void lookup_color();

  /* update xcolor structs if needed */
  if( init || (color->cur.desired_cur != NULL) ) {
    lookup_color(color->display, colormap, &(color->cur.color_cur),
		  color->cur.desired_cur, color->cur.default_cur);
    color->cur.desired_cur = NULL;
  }
  if( init || (color->cur.desired_one != NULL) ) {
    lookup_color(color->display, colormap, &(color->cur.color_one),
		  color->cur.desired_one, color->cur.default_one);
    color->cur.desired_one = NULL;
  }
  if( init || (color->cur.desired_two != NULL) ) {
    lookup_color(color->display, colormap, &(color->cur.color_two),
		  color->cur.desired_two, color->cur.default_two);
    color->cur.desired_two = NULL;
  }
}

/*
 * Subroutine:	alloc_cursor_cell_color
 * Purpose:	Allocate defined colors for cursors and overlays
 * Returns:	Pixel value for the cursor color
 * Pre-state:	XColor for cur, one, and two, initialized
 * Post-state:	Pixel values for one and two set, that for cur returned.
 */
int alloc_cursor_cell_color ( color, colormap )
     struct colorRec *color;
     Colormap colormap;
{
  int val;
  static int alloc_hard_color();

  if( (color->cur.disp_one =
      alloc_hard_color(color->display, colormap, &color->cur.color_one)) < 0 )
    color->cur.disp_one = color->hard.blue;
  if( (color->cur.disp_two =
      alloc_hard_color(color->display, colormap, &color->cur.color_two)) < 0 )
    color->cur.disp_two = color->hard.red;
  if( (val =
      alloc_hard_color(color->display, colormap, &color->cur.color_cur)) < 0 )
    val = color->hard.green;
  return( val );
}

/*
 * Subroutine:	free_cursor_cell_color
 * Purpose:	Free cell color overlay colors
 */
void free_cursor_cell_color ( color )
     struct colorRec *color;
{
  static void free_readonly_color();

  free_readonly_color(color, (int)color->cur.color_one.pixel);
  free_readonly_color(color, (int)color->cur.color_two.pixel);
  free_readonly_color(color, (int)color->cur.color_cur.pixel);
}

/*
 * Subroutine:	free_readonly_color
 * Purpose:	Free cursor color if it is not one of the essential ones
 * Xlib calls:	XFreeColors()
 */
static void free_readonly_color ( color, pixel_value )
     struct colorRec *color;
     int pixel_value;
{
  if( (pixel_value != color->hard.red) &&
      (pixel_value != color->hard.green) &&
      (pixel_value != color->hard.blue) &&
      (pixel_value != color->hard.true_black) &&
      (pixel_value != color->hard.true_white) &&
      (pixel_value != color->hard.std_black) &&
      (pixel_value != color->hard.std_white) )
    XFreeColors(color->display, color->colormap,
		(unsigned long *)&pixel_value, 1, 0);
}

/*
 * Subroutine:	get_hard_color
 * Purpose:	Alloc a read-only color cell with a common color
 * Xlib calls:	XAllocNamedColor(), XAllocColor()
 * Method:	Get a color by name if possible, else reserve it by value
 */
static int get_hard_color ( display, colormap, name, red, green, blue )
     Display *display;
     Colormap colormap;
     char *name;
     int red, green, blue;
{
  XColor actual, ideal;

  /* see if we can get a standard named color */
  if( XAllocNamedColor(display, colormap, name, &actual, &ideal) == 0 ) {
    /* else reserve our own */
    actual.pixel = 0;
    actual.red = red;
    actual.green = green;
    actual.blue = blue;
    if( XAllocColor(display, colormap, &actual) == 0 ) {
      (void)fprintf(stderr, "WARNING: could not get harware color.\n");
      return( 0 );
    }
  }
  return( (int)actual.pixel );
}

/*
 * Subroutine:	lookup_color
 * Purpose:	Set up the rgb values in the Xcolor
 * Xlib calls:	XLookupColor
 * Method:	Use the desired color if given and known to Xlib, else use
 *		the defaut color.
 */
static void lookup_color ( display, colormap, xcolor,
			   desired_name, default_name )
     Display *display;
     Colormap colormap;
     XColor *xcolor;
     char *desired_name;
     char *default_name;
{
  XColor ideal;

  if( desired_name != NULL ) {
    if( XLookupColor(display, colormap, desired_name, &ideal, xcolor) ) {
      return;
    } else {
      (void)fprintf(stderr,"WARNING: Could not find color: %s, using: %s\n",
		    desired_name, default_name);
    }
  }
  XLookupColor(display, colormap, default_name, &ideal, xcolor);
}

/*
 * Subroutine:	alloc_hard_color
 * Purpose:	Given rgb values, set the pixel
 * Xlib calls:	XAllocColor()
 * Returns:	Pixel value used or -1 if failed
 */
static int alloc_hard_color ( display, colormap, xcolor )
     Display *display;
     Colormap colormap;
     XColor *xcolor;
{
  if( XAllocColor(display, colormap, xcolor) )
    return( (int)xcolor->pixel );
  else
    return( -1 );
}
