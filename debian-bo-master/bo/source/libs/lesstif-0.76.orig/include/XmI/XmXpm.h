/**
 *
 * $Id: XmXpm.h,v 1.2 1996/12/17 03:23:52 miers Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **************************************************************************
 * This is based on xpm 3.4.7
 *
 * This code comes from the Xpm library, which is distributed with the
 * following copyright:
 **************************************************************************
 *
 * Copyright (C) 1989-95 GROUPE BULL
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of GROUPE BULL shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from GROUPE BULL.
 */

#ifndef XmXPM_h
#define XmXPM_h

#ifdef __cplusplus
extern "C" {
#endif

/* the following is defined in X11R6 but not in previous versions */
#ifdef __alpha
#ifndef LONG64
#define LONG64
#endif
#endif

/* Return ErrorStatus codes:
 * null     if full success
 * positive if partial success
 * negative if failure
 */

#define XmXpmColorError    1
#define XmXpmSuccess       0
#define XmXpmOpenFailed   -1
#define XmXpmFileInvalid  -2
#define XmXpmNoMemory     -3
#define XmXpmColorFailed  -4

typedef struct {
    char *name;			/* Symbolic color name */
    char *value;		/* Color value */
    Pixel pixel;		/* Color pixel */
} XmXpmColorSymbol;

typedef struct {
    char *name;			/* name of the extension */
    unsigned int nlines;	/* number of lines in this extension */
    char **lines;		/* pointer to the extension array of strings */
} XmXpmExtension;

typedef struct {
    char *string;		/* characters string */
    char *symbolic;		/* symbolic name */
    char *m_color;		/* monochrom default */
    char *g4_color;		/* 4 level grayscale default */
    char *g_color;		/* other level grayscale default */
    char *c_color;		/* color default */
} XmXpmColor;

typedef struct {
    unsigned int width;		/* image width */
    unsigned int height;	/* image height */
    unsigned int cpp;		/* number of characters per pixel */
    unsigned int ncolors;	/* number of colors */
    XmXpmColor *colorTable;	/* list of related colors */
    unsigned int *data;		/* image data */
} XmXpmImage;

typedef struct {
    unsigned long valuemask;	/* Specifies which attributes are defined */
    char *hints_cmt;		/* Comment of the hints section */
    char *colors_cmt;		/* Comment of the colors section */
    char *pixels_cmt;		/* Comment of the pixels section */
    unsigned int x_hotspot;	/* Returns the x hotspot's coordinate */
    unsigned int y_hotspot;	/* Returns the y hotspot's coordinate */
    unsigned int nextensions;	/* number of extensions */
    XmXpmExtension *extensions;	/* pointer to array of extensions */
} XmXpmInfo;

typedef struct {
    unsigned long valuemask;		/* Specifies which attributes are
					 * defined */

    Visual *visual;			/* Specifies the visual to use */
    Colormap colormap;			/* Specifies the colormap to use */
    unsigned int depth;			/* Specifies the depth */
    unsigned int width;			/* Returns the width of the created
					 * pixmap */
    unsigned int height;		/* Returns the height of the created
					 * pixmap */
    unsigned int x_hotspot;		/* Returns the x hotspot's
					 * coordinate */
    unsigned int y_hotspot;		/* Returns the y hotspot's
					 * coordinate */
    unsigned int cpp;			/* Specifies the number of char per
					 * pixel */
    Pixel *pixels;			/* List of used color pixels */
    unsigned int npixels;		/* Number of used pixels */
    XmXpmColorSymbol *colorsymbols;	/* Array of color symbols to
					 * override */
    unsigned int numsymbols;		/* Number of symbols */
    char *rgb_fname;			/* RGB text file name */
    unsigned int nextensions;		/* number of extensions */
    XmXpmExtension *extensions;		/* pointer to array of extensions */

    unsigned int ncolors;               /* Number of colors */
    XmXpmColor *colorTable;               /* Color table pointer */
    /* 3.2 backward compatibility code */
    char *hints_cmt;                    /* Comment of the hints section */
    char *colors_cmt;                   /* Comment of the colors section */
    char *pixels_cmt;                   /* Comment of the pixels section */
    /* end 3.2 bc */
    unsigned int mask_pixel;            /* Transparent pixel's color table
                                         * index */

    /* Color Allocation Directives */
    unsigned int exactColors;		/* Only use exact colors for visual */
    unsigned int closeness;		/* Allowable RGB deviation */
    unsigned int red_closeness;		/* Allowable red deviation */
    unsigned int green_closeness;	/* Allowable green deviation */
    unsigned int blue_closeness;	/* Allowable blue deviation */
    int color_key;			/* Use colors from this color set */

    Pixel *alloc_pixels;		/* Returns the list of alloc'ed color
					   pixels */
    unsigned int nalloc_pixels;		/* Returns the number of alloc'ed
					   color pixels */

} XmXpmAttributes;

/* XpmAttributes value masks bits */
#define XmXpmVisual		(1L<<0)
#define XmXpmColormap		(1L<<1)
#define XmXpmDepth		(1L<<2)
#define XmXpmSize		(1L<<3)	/* width & height */
#define XmXpmHotspot		(1L<<4)	/* x_hotspot & y_hotspot */
#define XmXpmCharsPerPixel	(1L<<5)
#define XmXpmColorSymbols	(1L<<6)
#define XmXpmRgbFilename	(1L<<7)
/* 3.2 backward compatibility code */
#define XmXpmInfos		(1L<<8)
#define XmXpmReturnInfos	XmXpmInfos
/* end 3.2 bc */
#define XmXpmReturnPixels	(1L<<9)
#define XmXpmExtensions		(1L<<10)
#define XmXpmReturnExtensions	XmXpmExtensions

#define XmXpmExactColors	(1L<<11)
#define XmXpmCloseness		(1L<<12)
#define XmXpmRGBCloseness	(1L<<13)
#define XmXpmColorKey		(1L<<14)

#define XmXpmColorTable		(1L<<15)
#define XmXpmReturnColorTable	XmXpmColorTable

#define XmXpmReturnAllocPixels	(1L<<16)

/* XmXpmInfo value masks bits */
#define XmXpmComments		XmXpmInfos
#define XmXpmReturnComments	XmXpmComments

/* XmXpmAttributes mask_pixel value when there is no mask */
#define XmXpmUndefPixel		0x80000000

/*
 * color keys for visual type, they must fit along with the number key of
 * each related element in xpmColorKeys[] defined in xpmP.h
 */
#define XPM_MONO	2
#define XPM_GREY4	3
#define XPM_GRAY4	3
#define XPM_GREY 	4
#define XPM_GRAY 	4
#define XPM_COLOR	5

/*
 * prototypes
 */
extern int	_XmXpmCreateDataFromImage(Display *display,
					  char ***data_return,
					  XImage *image,
					  XImage *shapeimage,
					  XmXpmAttributes *attributes);

extern int	_XmXpmCreateImageFromData(Display *display,
					  char **data,
					  XImage **image_return,
					  XImage **shapemask_return,
					  XmXpmAttributes *attributes);

extern void	_XmXpmFreeAttributes(XmXpmAttributes *attributes);

extern int	_XmXpmReadFileToImage(Display *display,
				      char *filename,
				      XImage **image_return,
				      XImage **shapeimage_return,
				      XmXpmAttributes *attributes);

extern int	_XmXpmCreateDataFromPixmap(Display *display,
					   char ***data_return,
					   Pixmap pixmap,
					   Pixmap shapemask,
					   XmXpmAttributes *attributes);

extern int	_XmXpmReadFileToPixmap(Display *display,
				       Drawable d,
				       char *filename,
				       Pixmap *pixmap_return,
				       Pixmap *shapemask_return,
				       XmXpmAttributes *attributes);

#ifdef __cplusplus
}
#endif

#endif
