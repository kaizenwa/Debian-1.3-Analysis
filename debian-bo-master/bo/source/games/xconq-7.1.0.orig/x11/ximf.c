/* X11-specific functions for image families in Xconq.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "config.h"
#include "misc.h"
#include "lisp.h"
#include "module.h"
#include "system.h"
#include "imf.h"
extern int smallest_image PARAMS ((ImageFamily *imf, int *wp, int *hp));

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>

#include "ximf.h"

extern char *imflib;

static Pixmap x11_load_bitmap PARAMS ((Display *dpy, Window rootwin,
				      char *name, char *ext, int *w, int *h));
static void x11_make_color_pixmap PARAMS ((Display *dpy, Window rootwin,
					  Image *img));

X11Image *
init_x11_image(img)
Image *img;
{
    X11Image *ximg;

    ximg = (X11Image *) xmalloc(sizeof(X11Image));
    ximg->mono = None;
    ximg->mask = None;
    /* Point to the generic image. */
    ximg->generic = img;
    ximg->colpix = NULL;
    return ximg;
}

X11Image *
get_x11_image(img)
Image *img;
{
    X11Image *ximg;

    if (img->hook)
      return (X11Image *) img->hook;
    ximg = init_x11_image(img);
    img->hook = (char *) ximg;
    return ximg;
}

/* This tries to fill in the given image family by looking for and loading
   standard X11 bitmap files. */

void
x11_load_imf(dpy, rootwin, imf)
Display *dpy;
Window rootwin;
ImageFamily *imf;
{
    int w, h;
    Pixmap pic;
    Image *img;
    X11Image *ximg;

    /* If no imf or no name, don't even try. */
    if (imf == NULL || imf->name == NULL)
      return;
    if (strcmp(imf->name, "none") == 0)
      return;
    if (0 /* found an imf file */) {
    } else {
	img = NULL;
	ximg = NULL;
	/* Just grab up plausibly-named bitmaps. */
	pic = x11_load_bitmap(dpy, rootwin, imf->name, "b", &w, &h);
	if (pic != None) {
	    img = get_img(imf, w, h);
	    ximg = get_x11_image(img);
	    ximg->mono = pic;
	}
	pic = x11_load_bitmap(dpy, rootwin, imf->name, "m", &w, &h);
	if (pic != None) {
	    img = get_img(imf, w, h);
	    ximg = get_x11_image(img);
	    ximg->mask = pic;
	}
	pic = x11_load_bitmap(dpy, rootwin, imf->name, "8.b", &w, &h);
	if (pic != None) {
	    img = get_img(imf, w, h);
	    ximg = get_x11_image(img);
	    ximg->mono = pic;
	}
	pic = x11_load_bitmap(dpy, rootwin, imf->name, "8.m", &w, &h);
	if (pic != None) {
	    img = get_img(imf, w, h);
	    ximg = get_x11_image(img);
	    ximg->mask = pic;
	}
	pic = x11_load_bitmap(dpy, rootwin, imf->name, "32.b", &w, &h);
	if (pic != None) {
	    img = get_img(imf, w, h);
	    ximg = get_x11_image(img);
	    ximg->mono = pic;
	}
	pic = x11_load_bitmap(dpy, rootwin, imf->name, "32.m", &w, &h);
	if (pic != None) {
	    img = get_img(imf, w, h);
	    ximg = get_x11_image(img);
	    ximg->mask = pic;
	}
#if 0
	if (imf->numsizes == 0) {
	    /* (should not complain unless no colors either -
	       check mac rules) */
	    fprintf(stderr,
		    "Image named \"%s\" not found here or in \"%s\"!\n",
		    imf->name, imflib);
	}
#endif
	if (img && ximg)
	  img->hook = (char *) ximg;
    }
}

/* Try to load a bitmap of the given name, looking in both the current dir
   and the library dir.  Warns about failure, but returns None, so the rest
   of the code will continue to function. */

static Pixmap
x11_load_bitmap(dpy, rootwin, name, ext, wp, hp)
Display *dpy;
Window rootwin;
char *name, *ext;
int *wp, *hp;
{
    int hotx, hoty;
    unsigned int w, h;
    Pixmap rslt;
    static char sbuf[1000];

    if (ext != NULL) {
	make_pathname(NULL, name, ext, sbuf);
	if (XReadBitmapFile(dpy, rootwin, sbuf,
			    &w, &h, &rslt, &hotx, &hoty) == BitmapSuccess) {
	    DGprintf("Loaded bitmap \"%s\"\n", sbuf);
	    *wp = w;  *hp = h;
	    return rslt;
	}
	make_pathname(imflib, name, ext, sbuf);
	if (XReadBitmapFile(dpy, rootwin, sbuf,
			    &w, &h, &rslt, &hotx, &hoty) == BitmapSuccess) {
	    DGprintf("Loaded bitmap \"%s\"\n", sbuf);
	    *wp = w;  *hp = h;
	    return rslt;
	}
    }
    return None;
}

/* if (force): prefer data over rawdata; always re-create pixmaps */

void
x11_interp_imf(dpy, rootwin, imf, force)
Display *dpy;
Window rootwin;
ImageFamily *imf;
int force;
{
    int w, h, rowbytes, numbytes;
    Image *img;
    X11Image *ximg;

    for (img = imf->images; img != NULL; img = img->next) {
	w = img->w;  h = img->h;
	ximg = get_x11_image(img);
	if (img->monodata != lispnil && (img->rawmonodata == NULL || force)) {
	    rowbytes = (w + 7) / 8;
	    numbytes = h * rowbytes;
	    img->rawmonodata = xmalloc(numbytes);
	    interp_bytes(img->monodata, numbytes, img->rawmonodata, 0);
	    reverse_bit_endianness(img->rawmonodata, numbytes);
	}
	if (img->rawmonodata && (ximg->mono == None || force)) {
	    ximg->mono =
	      XCreateBitmapFromData(dpy, rootwin, img->rawmonodata, w, h);
	}
	if (img->maskdata != lispnil && (img->rawmaskdata == NULL || force)) {
	    rowbytes = (w + 7) / 8;
	    numbytes = h * rowbytes;
	    img->rawmaskdata = xmalloc(numbytes);
	    interp_bytes(img->maskdata, numbytes, img->rawmaskdata, 0);
	    reverse_bit_endianness(img->rawmaskdata, numbytes);
	}
	if (img->rawmaskdata && (ximg->mask == None || force)) {
	    ximg->mask =
	      XCreateBitmapFromData(dpy, rootwin, img->rawmaskdata, w, h);
	}
	if (img->colrdata != lispnil && (img->rawcolrdata == NULL || force)) {
	    rowbytes = (w * img->pixelsize + 7) / 8;
	    numbytes = h * rowbytes;
	    img->rawcolrdata = xmalloc(numbytes);
	    interp_bytes(img->colrdata, numbytes, img->rawcolrdata, 0);
	}
	if (img->rawcolrdata && (ximg->colr == None || force)) {
	    x11_make_color_pixmap(dpy, rootwin, img);
	}
    }
}

/* X11 bitmaps are always in little-endian bit order, while IMF images
   are always big-endian in bit order, so we must reverse the bits
   in each byte individually. */

void
reverse_bit_endianness(rawdata, numbytes)
char *rawdata;
int numbytes;
{
    int i, j, byte, byte2;

    for (i = 0; i < numbytes; ++i) {
	byte = rawdata[i];
	byte2 = 0;
	for (j = 0; j < 8; ++j) {
	    byte2 = (byte2 << 1) | (byte & 1);
	    byte >>= 1;
	}
	rawdata[i] = byte2;
    }
}

static void
x11_make_color_pixmap(dpy, rootwin, img)
Display *dpy;
Window rootwin;
Image *img;
{
    unsigned short ipal[4][256];
    Pixel idx[256], pixel;
    int r, ri, rc, depth, c, ln, screen, rsize;
    int rowbytesize, bytesize, rmask;
    XColor col;
    char buf[BUFSIZE], *dp, *rp, *data;
    Obj *pal, *color;
    Colormap cmap;
    Pixmap pixmap;
    XImage *ximage;
    GC gc;
    X11Image *ximg = (X11Image *) img->hook;

    if (ximg == NULL || img->rawcolrdata == NULL)
      return;
    /* Can't make color pixmaps if we don't have any colors. */
    if (img->palette == lispnil && img->rawpalette == NULL)
      return;

    if (!img->rawpalette) {
	/* Parse the palette. */
	c = 0;
	for (pal = img->palette; pal != lispnil; pal = cdr(pal)) {
	    color = car(pal);
	    ipal[0][c] = c_number(car(color));
	    ipal[1][c] = c_number(car(cdr(color)));
	    ipal[2][c] = c_number(car(cdr(cdr(color))));
	    ipal[3][c] = c_number(car(cdr(cdr(cdr(color)))));
	    c++;
	}
	img->numcolors = c;
	if (c == 0)
	  return;
  
	/* store palette */
	img->rawpalette = (int *) xmalloc(img->numcolors * 4 * sizeof(int));
	for (c = 0; c < img->numcolors; c++) {
	    for (ln = 0; ln < 4; ln++) {
		img->rawpalette[4 * c + ln] = ipal[ln][c];
	    }
	}
    }

    screen = DefaultScreen(dpy);
    cmap = XDefaultColormap(dpy, screen);
    depth = DefaultDepth(dpy, screen);
    if (depth % 8)
      return;

    if (img->numcolors <= 0) {
	run_warning("No colors?");
	return;
    }

    /* allocate colors */
    ximg->colpix = (Pixel *) xmalloc(img->numcolors * sizeof(Pixel));
    for (c = 0; c < img->numcolors; c++) {
	col.red   = img->rawpalette[4 * c + 1];
	col.green = img->rawpalette[4 * c + 2];
	col.blue  = img->rawpalette[4 * c + 3];
	col.flags = DoRed | DoGreen | DoBlue;
	if (XAllocColor(dpy, cmap, &col)) {
	    ximg->colpix[c] = col.pixel;
	} else {
	    sprintf(buf, "%2.2x%2.2x%2.2x",
		    img->rawpalette[4*c+1],
		    img->rawpalette[4*c+2],
		    img->rawpalette[4*c+3]);
	    init_warning("Cannot alloc color #%s, will leave black", buf);
	    ximg->colpix[c] = XBlackPixel(dpy,screen);
	}
    }

    /* find reverse index->pixel mapping */
    for (c = 0; c < 256; c++) {
	idx[c] = XBlackPixel(dpy, screen);
    }
    for (c = 0; c < img->numcolors; c++) {
	idx[img->rawpalette[4 * c + 0]] = ximg->colpix[c];
    }

    /* make color data */
    rsize = img->pixelsize;
    rmask = (1 << img->pixelsize) - 1;
    rowbytesize = img->w * depth / 8;
    bytesize = rowbytesize * img->h;
    data = xmalloc(bytesize * sizeof(char));
    dp = data;
    rp = img->rawcolrdata;
    for (r = 0; r < img->h; r++) {
	ri = 8 - img->pixelsize;
	for (c = 0; c < img->w; c++) {
	    rc = ((int) (*rp >> ri)) & rmask;
	    if (ri) {
		ri -= img->pixelsize;
	    } else {
		ri = 8 - img->pixelsize;
		rp++;
	    }
	    pixel = idx[rc];
	    for (ln = depth - 8; ln >= 0; ln -= 8) {
		*dp = (pixel >> ln) & 0xff;
		dp++;
	    }
	}
	if ((img->pixelsize * img->w) % 8) {
	    rp++;
	}
    }

    /* convert to XImage */
    ximage = XCreateImage(dpy, DefaultVisual(dpy, screen), depth,
			  ZPixmap, 0, data, img->w, img->h,
			  8, rowbytesize);
    if (!ximage) {
	free(data);
	return;
    }
    ximage->byte_order = MSBFirst;
    ximage->bitmap_bit_order = MSBFirst;
    
    /* and finally to Pixmap */
    pixmap = XCreatePixmap(dpy, rootwin, img->w, img->h, depth);
    if (!pixmap)
      pixmap = None;
    if (pixmap == None) {
	XDestroyImage(ximage);
	/* XDestroyImage also frees data */
	return;
    }
    gc = XCreateGC(dpy, pixmap, 0, NULL);
    XPutImage(dpy, pixmap, gc, ximage, 0, 0, 0, 0,
	      ximage->width, ximage->height);
    XFreeGC(dpy, gc);
    XDestroyImage(ximage);
    /* XDestroyImage also frees data */

    ximg->colr = pixmap;
}

/* (should be a generic routine) */
int
smallest_image(imf, wp, hp)
ImageFamily *imf;
int *wp, *hp;
{
    Image *img, *smallest = NULL;

    if (imf == NULL)
      return FALSE;
    for (img = imf->images; img != NULL; img = img->next) {
	if (smallest == NULL || (img->w < smallest->w && img->h < smallest->h))
	  smallest = img;
    }
    if (smallest != NULL) {
	*wp = smallest->w;  *hp = smallest->h;
	return TRUE;
    }
    return FALSE;
}

void
make_generic_image_data(imf)
ImageFamily *imf;
{
}
