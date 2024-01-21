/* Definitions of image families for the X11 interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Structure for X11-specific data. */

/* Note that this structure has an implicit display and therefore player,
   because pixmaps and fonts always have a particular associated display. */

#include <X11/Intrinsic.h>

typedef struct a_x11_image {
    Image *generic;		/* pointer to generic image */
    char *monodata;
    char *colrdata;
    char *maskdata;
    Pixmap mono;
    Pixmap colr;
    Pixmap mask;
    XFontStruct *font;
    char monochar;
    char maskchar;
    Pixel *colpix;
} X11Image;

extern X11Image *init_x11_image PARAMS ((Image *img));
extern X11Image *get_x11_image PARAMS ((Image *img));
extern void x11_load_imf PARAMS ((Display *dpy, Window rw, ImageFamily *imf));
extern void x11_interp_imf PARAMS ((Display *dpy, Window rw, ImageFamily *imf, int force));
extern void reverse_bit_endianness PARAMS ((char *rawdata, int numbytes));
extern void make_generic_image_data PARAMS ((ImageFamily *imf));
