/*
   colors.c
   Some simple functions needed for the color handling.
*/

#include <stdio.h>
#include <X11/Xlib.h>

#include "global.h"
#include "colors.h"

/*-----------------------------------------------------------------------*/
/* Allocate a color by name (e.g. "red" or "#FF0030"). Sets pixel to     */
/* the pixel value of the requested color. Returns 1 if a color could    */
/* be allocated, 0 if not.                                               */
/*-----------------------------------------------------------------------*/
int alloc_named_color (Colormap cmap, char *colorname, unsigned long *pixel) {
  XColor scr_color, exact_color;
  int state;
  
  state = XAllocNamedColor(display,cmap,colorname,&scr_color,&exact_color);
  *pixel = scr_color.pixel;
  return(state?1:0);
} 


int init_cmap (void) {
/* get display defaults for colormap and black & white pixels */
  cmap = DefaultColormap(display,screen);
  black = BlackPixel(display,screen);
  white = WhitePixel(display,screen);
  return(1);
}
