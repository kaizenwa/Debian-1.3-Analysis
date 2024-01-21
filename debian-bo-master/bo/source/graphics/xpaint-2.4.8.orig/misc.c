/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)            | */
/* | Copyright 1995, 1996 Torsten Martinsen (bullestock@dk-online.dk)  | */
/* |                                                                   | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.  There is no           | */
/* | representations about the suitability of this software for        | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.                                              | */
/* |                                                                   | */
/* +-------------------------------------------------------------------+ */

/* $Id: misc.c,v 1.11 1996/05/09 07:11:53 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/Label.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "bitmaps/background.xbm"

#ifndef XtNcursor
#define XtNcursor	 "cursor"
#endif

#include "xpaint.h"
#include "misc.h"
#include "PaintP.h"
#include "palette.h"
#include "protocol.h"


Widget
GetToplevel(Widget w)
{
    Widget p = w;

    while (w != None) {
	p = w;
	w = XtParent(w);
    }
    return p;
}

Widget
GetShell(Widget w)
{
    while (!XtIsShell(w))
	w = XtParent(w);
    return w;
}

void 
SetIBeamCursor(Widget w)
{
    XtVaSetValues(w, XtVaTypedArg,
		  XtNcursor, XtRString, "xterm", sizeof(Cursor), NULL);
}

void 
SetCrossHairCursor(Widget w)
{
    XtVaSetValues(w, XtVaTypedArg,
		XtNcursor, XtRString, "crosshair", sizeof(Cursor), NULL);
}
void 
SetPencilCursor(Widget w)
{
    XtVaSetValues(w, XtVaTypedArg,
		  XtNcursor, XtRString, "pencil", sizeof(Cursor), NULL);
}


/*
**  Some useful XRectangle computation code.
 */
XRectangle *
RectUnion(XRectangle * a, XRectangle * b)
{
    static XRectangle out;
    int sx, ex, sy, ey;

    sx = MIN(a->x, b->x);
    sy = MIN(a->y, b->y);
    ex = MAX(a->x + a->width, b->x + b->width);
    ey = MAX(a->y + a->height, b->y + b->height);

    XYtoRECT(sx, sy, ex, ey, &out);

    return &out;
}

XRectangle *
RectIntersect(XRectangle * a, XRectangle * b)
{
    static XRectangle out;
    int w, h;

    if (a == NULL || b == NULL)
	return NULL;

    out.x = MAX(a->x, b->x);
    out.y = MAX(a->y, b->y);
    w = MIN(a->x + a->width, b->x + b->width) - out.x;
    h = MIN(a->y + a->height, b->y + b->height) - out.y;

    if (w <= 0 || h <= 0)
	return NULL;

    out.width = w;
    out.height = h;

    return &out;
}

/*
**
 */
void 
GetPixmapWHD(Display * dpy, Drawable d, int *wth, int *hth, int *dth)
{
    Window root;
    int x, y;
    unsigned int width, height, bw, depth;

    XGetGeometry(dpy, d, &root, &x, &y, &width, &height, &bw, &depth);

    if (wth != NULL)
	*wth = width;
    if (hth != NULL)
	*hth = height;
    if (dth != NULL)
	*dth = depth;
}

/*
**  Two useful functions to "cache" both a tiled background
**    and a Xor gc
 */
typedef struct bgList_s {
    int depth;
    Pixmap pixmap;
    struct bgList_s *next;
} BackgroundList;

static BackgroundList *bgList = NULL;

Pixmap
GetBackgroundPixmap(Widget w)
{
    Widget p, n;
    Display *dpy = XtDisplay(w);
    int depth;
    Pixmap pix;
    Pixel fg, bg;
    BackgroundList *cur;
    Widget tw;

    for (n = XtParent(p = w); !XtIsShell(n); n = XtParent(p = n));

    XtVaGetValues(p, XtNdepth, &depth, NULL);

    for (cur = bgList; bgList != NULL; bgList = bgList->next)
	if (cur->depth == depth)
	    return cur->pixmap;

    tw = XtVaCreateWidget("junkWidget", labelWidgetClass, w,
			  XtNwidth, 1,
			  XtNheight, 1,
			  NULL);
    XtVaGetValues(tw, XtNforeground, &fg,
		  XtNbackground, &bg,
		  NULL);

    XtDestroyWidget(tw);

    pix = XCreatePixmapFromBitmapData(dpy, DefaultRootWindow(dpy),
				      (char *) background_bits,
				      background_width, background_height,
				      bg, fg, depth);

    cur = XtNew(BackgroundList);
    cur->next = bgList;
    bgList = cur;
    cur->depth = depth;
    cur->pixmap = pix;
    return pix;
}

typedef struct gcList_s {
    Widget widget;
    int depth;
    GC gc;
    struct gcList_s *next;
} GCXList;

static GCXList *gcList = NULL;

GC
GetGCX(Widget w)
{
    Widget n = GetShell(w);
    int depth;
    GCXList *cur;
    XGCValues values;

    XtVaGetValues(n, XtNdepth, &depth, NULL);

    for (cur = gcList; gcList != NULL; gcList = gcList->next)
	if (cur->depth == depth)
	    return cur->gc;

    values.function = GXxor;
    values.foreground = ~0;

    cur = XtNew(GCXList);
    cur->next = gcList;
    gcList = cur;
    cur->depth = depth;
    cur->gc = XtGetGC(n, GCFunction | GCForeground, &values);
    return cur->gc;
}

/*
**  Argv parsing routines
 */

static char *
nextArg(char *str, char **start)
{
    char *cp;
    int flg;

    while (isspace(*str))
	str++;
    if (start != NULL)
	*start = str;
    if (*str == '\0')
	return NULL;
    if (*str == '\'' || *str == '"') {
	char delim = *str;
	if (start != NULL)
	    (*start)++;
	cp = ++str;
	while (*str != '\0' && *str != delim) {
	    if (*str == '\\') {
		if (*++str == '\0')
		    continue;
	    }
	    *cp++ = *str++;
	}
	*cp = '\0';
    } else {
	while (!isspace(*str) && *str != '\0')
	    str++;
    }

    flg = (*str != '\0');
    *str = '\0';
    return str + (flg ? 1 : 0);
}

void 
StrToArgv(char *str, int *argc, char **argv)
{
    int t;
    char *cp;

    if (argc == NULL)
	argc = &t;

    *argc = 0;
    for (cp = str; cp != NULL; (*argc)++)
	cp = nextArg(cp, argv == NULL ? NULL : &argv[*argc]);

    if (argv != NULL)
	argv[--(*argc)] = NULL;
}

/*
**  Create a XImage
 */
XImage *
NewXImage(Display * dpy, Visual * visual, int depth, int width, int height)
{
    XImage *xim;
    int pad;

    if (depth > 16)
	pad = 32;
    else if (depth > 8)
	pad = 16;
    else
	pad = 8;

    xim = XCreateImage(dpy, visual, depth, ZPixmap, 0, NULL,
		       width, height, pad, 0);

    if (xim == NULL)
	return NULL;

    xim->data = (char *) XtMalloc(xim->bytes_per_line * height);

    if (xim->data == NULL) {
	XDestroyImage(xim);
	xim = NULL;
    }
    return xim;
}

/*
 * Return a Gaussian (aka normal) random variable.
 *
 * Adapted from ppmforge.c, which is part of PBMPLUS.
 * The algorithm comes from:
 * 'The Science Of Fractal Images'. Peitgen, H.-O., and Saupe, D. eds.
 * Springer Verlag, New York, 1988.
 */
double 
gauss(void)
{
    int i;
    double sum = 0.0;

    for (i = 0; i < 4; i++)
	sum += RANDOMI() & 0x7FFF;

    return sum * 5.28596089837e-5 - 3.46410161514;
}

/*
 * Return a Gaussian (aka normal) random variable, multiplied by 'range'
 * and clamped to [-range, range]. Note: integer return.
 */
int 
gaussclamp(int range)
{
    /* The '2.5' is a fudge factor; this value IMHO gives the best result */
    int g = gauss() * range / 2.5;

    if (g < -range)
	g = -range;
    else if (g > range)
	g = range;
    return g;
}

void *
xmalloc(size_t n)
{
    void *p;

    if ((p = malloc(n)) != NULL)
	return p;
    fprintf(stderr, "Out of memory\n");
    exit(1);
}

/*
 * Remove same-coloured borders from image.
 * Portions of this code taken from pnmcrop.c, which is part of the PBMPLUS
 * package, and carries the following copyright:
 */
/*
** Copyright (C) 1988 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
 */
void 
AutoCrop(Widget paint)
{
    PaintWidget pw = (PaintWidget) paint;
    int i;
    int x, y, width, height, top, bottom, left, right;
    Pixmap pix;
    Pixel corner[4], bg;
    XImage *xim;
    Colormap cmap;
    Palette *palette;
    Display *dpy = XtDisplay(pw);
    XRectangle rect;


    StateSetBusy(True);
    PwRegionFinish(paint, True);
    width = pw->paint.drawWidth;
    height = pw->paint.drawHeight;

    /*
     * First try to guess the border colour.
     */
    XtVaGetValues(paint, XtNcolormap, &cmap, NULL);
    palette = PaletteFind(paint, cmap);

    /* Find the RGB values for the four corners. */
    pix = XCreatePixmap(dpy, XtWindow(pw), width, height, pw->core.depth);
    xim = NewXImage(dpy, NULL, pw->core.depth, width, height);
    XGetSubImage(dpy, GET_PIXMAP(pw), 0, 0, width, height,
		 AllPlanes, ZPixmap, xim, 0, 0);
    for (y = 0, i = 0; y < height; y += height - 1)
	for (x = 0; x < width; x += width - 1)
	    corner[i++] = XGetPixel(xim, x, y);

    if ((corner[0] == corner[1]) || (corner[0] == corner[2]) ||
	(corner[0] == corner[3]))
	bg = corner[0];
    else if ((corner[1] == corner[2]) || (corner[1] == corner[3]))
	bg = corner[1];
    else if (corner[2] == corner[3])
	bg = corner[2];
    else {
	XDestroyImage(xim);
	StateSetBusy(False);
	return;			/* No two corners have the same colour */
    }

    /* Find first non-background line. */
    for (top = 0; top < height; top++)
	for (x = 0; x < width; x++)
	    if (XGetPixel(xim, x, top) != bg)
		goto gottop;
  gottop:

    /* Find last non-background line. */
    for (bottom = height - 1; bottom >= top; bottom--)
	for (x = 0; x < width; x++)
	    if (XGetPixel(xim, x, bottom) != bg)
		goto gotbottom;
  gotbottom:

    /* Find first non-background column. */
    left = width - 1;
    for (y = top; y <= bottom; y++)
	for (x = 0; x < left; x++)
	    if (XGetPixel(xim, x, y) != bg) {
		left = x;
		break;
	    }
    /* Find last non-background column. */
    right = left + 1;
    for (y = top; y <= bottom; y++)
	for (x = width - 1; x > right; x--)
	    if (XGetPixel(xim, x, y) != bg) {
		right = x;
		break;
	    }
    XDestroyImage(xim);

    rect.x = left;
    rect.y = top;
    rect.width = right - left + 1;
    rect.height = bottom - top + 1;

    if (((left == 0) && (right == width - 1) &&
	 (top == 0) && (bottom == height - 1))
	|| (rect.width < 1) || (rect.height < 1)) {
	StateSetBusy(False);
	return;
    }
    PwRegionSet(paint, &rect, None, None);
    PwRegionGet(paint, &pix, None);
    PwRegionFinish(paint, True);

    XtVaSetValues((Widget) paint, XtNpixmap, pix,
	     XtNdrawWidth, rect.width, XtNdrawHeight, rect.height, NULL);

    pw->paint.dirty = True;
    PwUpdateDrawable((Widget) paint, XtWindow(paint), NULL);
    StateSetBusy(False);
}
