/* 
   colors.h
   Header file for color handling functions contained in colors.c
*/

#ifndef _COLORS_H
#define _COLORS_H

/* Usually pixel colors are specified, which are of type unsigned long */
#define PIXEL unsigned long 

/* Standard pixels defaults for the display */
PIXEL black, white;

Colormap cmap;
PIXEL window_bg, led_on, led_off;

extern int alloc_named_color (Colormap cmap, char *colorname, 
                              unsigned long *pixel);
extern int init_cmap (void);

#endif
