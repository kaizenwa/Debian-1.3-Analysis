/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)            | */
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

/* Polygon selection code and other portions */
/* Copyright 1995, 1996 Torsten Martinsen    */

/* $Id: selectOp.c,v 1.6 1996/05/31 06:24:05 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Command.h>
#include <X11/Shell.h>
#include <X11/keysym.h>
#include <stdio.h>
#include "xpaint.h"
#include "Paint.h"
#include "protocol.h"
#include "palette.h"
#include "color.h"
#include "misc.h"
#include "operation.h"
#include "ops.h"

/*
**  It is now historical, that the Box and Area selection
**    operations are contained in the same file.
 */

static int cutMode = 0;
static int shapeMode = 0;

typedef struct {
    int areaType;		/* 0: box/ellipse, 1: lasso */
    Boolean lastType;		/* True: square/circle */
    Boolean tracking, first;

    Widget w;
    Drawable drawable;

    Boolean drawn;

    int wx, wy;

    /*
    **  The point region for lasso and polygon
     */
    int size, npoint;
    XPoint *real_p, *window_p;

    /*
     * Additional info for polygon
     */
    int startX, endX, startY, endY, button;

    /*
    **  The rectangles for a box (also used for ellipse)
     */
    int lastX, lastY;
    XRectangle rect;

    GC gcx;

    /*
    **  The interesting stuff, for the active region
    **    the important part of pixBox is the width, height
    **    the x,y is where it is presently located on the screen.
     */
    XRectangle pixBox;
} LocalInfo;

#define MKRECT(rect, sx, sy, ex, ey, typeFlag) do {			\
	if (typeFlag) {		/* square */				\
		(rect)->width  = MIN(ABS(sx - ex),ABS(sy - ey));	\
		(rect)->height = (rect)->width;				\
		(rect)->x = (ex - sx < 0) ? sx - (rect)->width : sx;	\
		(rect)->y = (ey - sy < 0) ? sy - (rect)->height : sy;	\
	} else {		/* no constraints */			\
		(rect)->x      = MIN(sx, ex);				\
		(rect)->y      = MIN(sy, ey);				\
		(rect)->width  = MAX(sx, ex) - (rect)->x;		\
		(rect)->height = MAX(sy, ey) - (rect)->y;		\
	}								\
} while (0)

static Boolean chromaCut(Widget w, LocalInfo * l, Pixmap * mask);

/*
**  press
 */
static void 
press(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    /*
    **  Check to make sure all buttons are up, before doing this
     */
    if ((event->state & AllButtonsMask) != 0)
	return;

    if (info->surface == opWindow) {
	l->w = w;

	l->wx = event->x;
	l->wy = event->y;

	l->drawn = False;

	return;
    }
    /*
    **  Must be on the surface == pixmap now
     */

    l->drawable = info->drawable;

    l->real_p[0].x = info->x;
    l->real_p[0].y = info->y;
    l->window_p[0].x = l->wx - info->zoom / 2;
    l->window_p[0].y = l->wy - info->zoom / 2;
    l->npoint = 1;
}

static void 
keyPress(Widget w, LocalInfo * l, XKeyEvent * event, OpInfo * info)
{
    KeySym keysym;
    int len, i, dx, dy;
    char buf[21];

    len = XLookupString(event, buf, sizeof(buf) - 1, &keysym, NULL);

    dx = dy = 0;
    switch (keysym) {
    case XK_Up:
	dy = -1;
	break;
    case XK_Down:
	dy = 1;
	break;
    case XK_Left:
	dx = -1;
	break;
    case XK_Right:
	dx = 1;
	break;
    default:
	break;
    }
    if (event->state & ControlMask) {
	dx *= 5;
	dy *= 5;
    }
    if (dx || dy) {
	RegionMove((PaintWidget) w, dx, dy);
	return;
    }
    /*
    **  Look for either backspace or delete and remove region
     */
    for (i = 0; i < len; i++) {
	if (buf[i] == 0x08 || buf[i] == 0x7f) {
	    PwRegionClear(w);
	    return;
	}
    }
}

static void 
finishPolyBand(Widget w, LocalInfo * l, OpInfo * info)
{
    int xmin, xmax, ymin, ymax, i, width, height;
    Pixmap mask;
    GC gc;
    XPoint tmp;

    if (!l->tracking)
	return;

    l->tracking = False;

    if (l->drawn)
	XDrawLines(XtDisplay(w), XtWindow(w), l->gcx,
		   l->window_p, l->npoint, CoordModeOrigin);
    PwUpdateDrawable(w, info->drawable, NULL);
    if (l->npoint <= 2)
	return;
    l->window_p[l->npoint] = l->window_p[0];
    l->real_p[l->npoint] = l->real_p[0];
    l->npoint++;

    xmin = xmax = l->real_p[0].x;
    ymin = ymax = l->real_p[0].y;
    for (i = l->npoint - 1; i > 0; i--) {
	xmin = MIN(xmin, l->real_p[i].x);
	ymin = MIN(ymin, l->real_p[i].y);
	xmax = MAX(xmax, l->real_p[i].x);
	ymax = MAX(ymax, l->real_p[i].y);

	l->window_p[i].x = l->window_p[i].x - l->window_p[i - 1].x;
	l->window_p[i].y = l->window_p[i].y - l->window_p[i - 1].y;
	l->real_p[i].x = l->real_p[i].x - l->real_p[i - 1].x;
	l->real_p[i].y = l->real_p[i].y - l->real_p[i - 1].y;
    }

    XYtoRECT(xmin, ymin, xmax, ymax, &l->pixBox);
    XtVaGetValues(w, XtNdrawWidth, &width, XtNdrawHeight, &height, NULL);
    if (l->pixBox.x + l->pixBox.width > width)
	l->pixBox.width = width - l->pixBox.x;
    if (l->pixBox.y + l->pixBox.height > height)
	l->pixBox.height = height - l->pixBox.y;
    if (l->pixBox.x < 0) {
	l->pixBox.width += l->pixBox.x;
	l->pixBox.x = 0;
    }
    if (l->pixBox.y < 0) {
	l->pixBox.height += l->pixBox.y;
	l->pixBox.y = 0;
    }
    mask = XCreatePixmap(XtDisplay(w), XtWindow(w),
			 l->pixBox.width, l->pixBox.height, 1);
    gc = XCreateGC(XtDisplay(w), mask, 0, 0);
    XSetFunction(XtDisplay(w), gc, GXclear);
    XFillRectangle(XtDisplay(w), mask, gc, 0, 0,
		   l->pixBox.width, l->pixBox.height);
    XSetFunction(XtDisplay(w), gc, GXset);
    tmp = l->real_p[0];
    l->real_p[0].x = l->real_p[0].x - xmin;
    l->real_p[0].y = l->real_p[0].y - ymin;
    XFillPolygon(XtDisplay(w), mask, gc, l->real_p, l->npoint,
		 Complex, CoordModePrevious);
    l->real_p[0] = tmp;

    XFreeGC(XtDisplay(w), gc);

    if (l->pixBox.width <= 1 || l->pixBox.height <= 1) {
	PwRegionFinish(w, True);
	return;
    }
    if (cutMode != 0 && !chromaCut(w, l, &mask)) {
	PwRegionFinish(w, True);
	return;
    }
    PwRegionSet(w, &l->pixBox, None, mask);
}

static void 
pressPolyBand(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    if (!l->tracking && (event->button == Button1)) {
	l->endX = l->startX = event->x;
	l->endY = l->startY = event->y;

	l->button = event->button;

	l->window_p[0].x = event->x;
	l->window_p[0].y = event->y;
	l->real_p[0].x = info->x;
	l->real_p[0].y = info->y;
	l->npoint = 1;

	l->drawn = False;
	l->tracking = True;
	l->first = True;
    } else if ((event->button == Button2) && l->tracking) {
	finishPolyBand(w, l, info);
	return;
    }
}

/*
**  motion
 */
static void 
motionBoxBand(Widget w, LocalInfo * l, XMotionEvent * event, OpInfo * info)
{
    XRectangle rect;

    if (l->drawn) {
	MKRECT(&rect, l->window_p[0].x, l->window_p[0].y,
	       l->lastX, l->lastY, l->lastType);
	if (shapeMode == 0)
	    XDrawRectangles(XtDisplay(w), XtWindow(w), l->gcx, &rect, 1);
	else
	    XDrawArc(XtDisplay(w), XtWindow(w), l->gcx, rect.x, rect.y,
		     rect.width, rect.height, 0, 360 * 64);
    }
    l->lastX = event->x - info->zoom / 2;
    l->lastY = event->y - info->zoom / 2;
    l->lastType = (event->state & ShiftMask);

    if ((l->drawn =
       (l->window_p[0].x != l->lastX && l->window_p[0].y != l->lastY))) {
	MKRECT(&rect, l->window_p[0].x, l->window_p[0].y,
	       l->lastX, l->lastY, l->lastType);
	if (shapeMode == 0)
	    XDrawRectangles(XtDisplay(w), XtWindow(w), l->gcx, &rect, 1);
	else
	    XDrawArc(XtDisplay(w), XtWindow(w), l->gcx, rect.x, rect.y,
		     rect.width, rect.height, 0, 360 * 64);
    }
}

static void 
motionLassoBand(Widget w, LocalInfo * l, XMotionEvent * event, OpInfo * info)
{
    if (l->npoint != 0 && (l->real_p[l->npoint - 1].x == info->x) &&
	(l->real_p[l->npoint - 1].y == info->y))
	return;

    l->window_p[l->npoint].x = event->x;
    l->window_p[l->npoint].y = event->y;
    l->real_p[l->npoint].x = info->x;
    l->real_p[l->npoint].y = info->y;

    XDrawLine(XtDisplay(w), XtWindow(w), l->gcx,
	      l->window_p[l->npoint - 1].x,
	      l->window_p[l->npoint - 1].y,
	      l->window_p[l->npoint].x,
	      l->window_p[l->npoint].y);
    XDrawPoint(XtDisplay(w), XtWindow(w), l->gcx,
	       l->window_p[l->npoint].x,
	       l->window_p[l->npoint].y);


    l->npoint++;

    if (l->npoint > l->size - 3) {
	l->size += 256;
	l->real_p = (XPoint *) XtRealloc((XtPointer) l->real_p,
					 l->size * sizeof(XPoint));
	l->window_p = (XPoint *) XtRealloc((XtPointer) l->window_p,
					   l->size * sizeof(XPoint));
    }
}

static void 
motionPolyBand(Widget w, LocalInfo * l, XMotionEvent * event, OpInfo * info)
{
    if (!l->tracking)
	return;

    if (l->drawn)
	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);

    l->endX = event->x;
    l->endY = event->y;

    if ((l->drawn = ((l->startX != l->endX) || (l->startY != l->endY))))
	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);
}

static void 
motion(Widget w, LocalInfo * l, XMotionEvent * event, OpInfo * info)
{
    if (!event->state)
	return;

    if (l->areaType == 0)
	motionBoxBand(w, l, event, info);
    else
	motionLassoBand(w, l, event, info);
}

static Boolean
chromaCut(Widget w, LocalInfo * l, Pixmap * mask)
{
    GC gc;
    XImage *src, *mimg;
    XRectangle *rect = PwScaleRectangle(w, &l->pixBox);
    int x, y, count = 0;
    Pixel p;
    XColor *xcol;
    int br, bg, bb;
    int vr, vg, vb;
    int pr, pg, pb;
    Boolean mode = (cutMode == 1);
    Palette *map;
    Colormap cmap;
    int step;

    src = PwGetImage(w, rect);
    XtVaGetValues(GetShell(w), XtNcolormap, &cmap, NULL);
    map = PaletteFind(w, cmap);

    if (*mask != None) {
	mimg = XGetImage(XtDisplay(w), *mask, 0, 0,
			 rect->width, rect->height, AllPlanes, ZPixmap);
    } else {
	mimg = NewXImage(XtDisplay(w), DefaultVisualOfScreen(XtScreen(w)),
			 1, rect->width, rect->height);

	memset(mimg->data, ~0, rect->height * mimg->bytes_per_line);
    }

    GetChromaBackground(&br, &bg, &bb);
    GetChromaDelta(&vr, &vg, &vb);

    if (rect->height * rect->width > 2048)
	StateSetBusy(True);
    step = 256 * 64 / rect->width;

    for (y = 0; y < rect->height; y++) {
	for (x = 0; x < rect->width; x++) {
	    if (!XGetPixel(mimg, x, y))
		continue;

	    p = XGetPixel(src, x + rect->x, y + rect->y);
	    xcol = PaletteLookup(map, p);
	    pr = (xcol->red >> 8) & 0xff;
	    pg = (xcol->green >> 8) & 0xff;
	    pb = (xcol->blue >> 8) & 0xff;

	    if (((br - vr) <= pr && pr <= (br + vr) &&
		 (bg - vg) <= pg && pg <= (bg + vg) &&
		 (bb - vb) <= pb && pb <= (bb + vb)) == mode)
		XPutPixel(mimg, x, y, False);
	    else
		count++;
	}
	if (y % step == 0)
	    StateTimeStep();
    }

    if (count != 0) {
	/*
	**  Not a solid region.
	 */

	if (*mask == None)
	    *mask = XCreatePixmap(XtDisplay(w), XtWindow(w),
				  rect->width, rect->height, 1);
	gc = XCreateGC(XtDisplay(w), *mask, 0, 0);
	XPutImage(XtDisplay(w), *mask, gc, mimg, 0, 0, 0, 0,
		  rect->width, rect->height);
	XFreeGC(XtDisplay(w), gc);
    }
    XDestroyImage(mimg);

    if (rect->height * rect->width > 2048)
	StateSetBusy(False);


    return count != 0;
}

static void 
releaseBoxBand(Widget w, LocalInfo * l, XButtonEvent * event,
	       OpInfo * info, Pixmap * mask)
{
    XRectangle rect;
    int width, height;
    GC gc;
    if (l->drawn) {
	MKRECT(&rect, l->window_p[0].x, l->window_p[0].y,
	       l->lastX, l->lastY, l->lastType);
	if (shapeMode == 0)
	    XDrawRectangles(XtDisplay(w), XtWindow(w), l->gcx, &rect, 1);
	else
	    XDrawArc(XtDisplay(w), XtWindow(w), l->gcx, rect.x, rect.y,
		     rect.width, rect.height, 0, 360 * 64);
    }
    MKRECT(&l->pixBox, l->real_p[0].x, l->real_p[0].y,
	   info->x, info->y, l->lastType);
    XtVaGetValues(w, XtNdrawWidth, &width, XtNdrawHeight, &height, NULL);

    if (l->pixBox.x + l->pixBox.width > width)
	l->pixBox.width = width - l->pixBox.x;
    if (l->pixBox.y + l->pixBox.height > height)
	l->pixBox.height = height - l->pixBox.y;
    if (l->pixBox.x < 0) {
	l->pixBox.width += l->pixBox.x;
	l->pixBox.x = 0;
    }
    if (l->pixBox.y < 0) {
	l->pixBox.height += l->pixBox.y;
	l->pixBox.y = 0;
    }
    if (shapeMode == 0)
	return;

    /* to avoid crash when selecting a single point as ellipse - ACZ */
    if ((l->pixBox.width <= 1) || (l->pixBox.height <= 1))
 	return;

    *mask = XCreatePixmap(XtDisplay(w), XtWindow(w),
			  l->pixBox.width, l->pixBox.height, 1);
    gc = XCreateGC(XtDisplay(w), *mask, 0, 0);
    XSetFunction(XtDisplay(w), gc, GXclear);
    XFillRectangle(XtDisplay(w), *mask, gc, 0, 0,
		   l->pixBox.width, l->pixBox.height);
    XSetFunction(XtDisplay(w), gc, GXset);
    XFillArc(XtDisplay(w), *mask, gc, 0, 0,
	     l->pixBox.width, l->pixBox.height, 0, 360 * 64);

    XFreeGC(XtDisplay(w), gc);
}

static Boolean
releaseLassoBand(Widget w, LocalInfo * l, XButtonEvent * event,
		 OpInfo * info, Pixmap * mask)
{
    int xmax, xmin, ymax, ymin;
    int width, height, i;
    GC gc;
    XPoint tmp;

    XDrawLines(XtDisplay(w), XtWindow(w), l->gcx,
	       l->window_p, l->npoint, CoordModeOrigin);

    if (l->npoint <= 2)
	return False;

    l->window_p[l->npoint] = l->window_p[0];
    l->real_p[l->npoint] = l->real_p[0];
    l->npoint++;

    xmin = xmax = l->real_p[0].x;
    ymin = ymax = l->real_p[0].y;
    for (i = l->npoint - 1; i > 0; i--) {
	xmin = MIN(xmin, l->real_p[i].x);
	ymin = MIN(ymin, l->real_p[i].y);
	xmax = MAX(xmax, l->real_p[i].x);
	ymax = MAX(ymax, l->real_p[i].y);

	l->window_p[i].x = l->window_p[i].x - l->window_p[i - 1].x;
	l->window_p[i].y = l->window_p[i].y - l->window_p[i - 1].y;
	l->real_p[i].x = l->real_p[i].x - l->real_p[i - 1].x;
	l->real_p[i].y = l->real_p[i].y - l->real_p[i - 1].y;
    }

    XYtoRECT(xmin, ymin, xmax, ymax, &l->pixBox);
    XtVaGetValues(w, XtNdrawWidth, &width, XtNdrawHeight, &height, NULL);
    if (l->pixBox.x + l->pixBox.width > width)
	l->pixBox.width = width - l->pixBox.x;
    if (l->pixBox.y + l->pixBox.height > height)
	l->pixBox.height = height - l->pixBox.y;
    if (l->pixBox.x < 0) {
	l->pixBox.width += l->pixBox.x;
	l->pixBox.x = 0;
    }
    if (l->pixBox.y < 0) {
	l->pixBox.height += l->pixBox.y;
	l->pixBox.y = 0;
    }
    *mask = XCreatePixmap(XtDisplay(w), XtWindow(w),
			  l->pixBox.width, l->pixBox.height, 1);
    gc = XCreateGC(XtDisplay(w), *mask, 0, 0);
    XSetFunction(XtDisplay(w), gc, GXclear);
    XFillRectangle(XtDisplay(w), *mask, gc, 0, 0,
		   l->pixBox.width, l->pixBox.height);
    XSetFunction(XtDisplay(w), gc, GXset);
    tmp = l->real_p[0];
    l->real_p[0].x = l->real_p[0].x - xmin;
    l->real_p[0].y = l->real_p[0].y - ymin;
    XFillPolygon(XtDisplay(w), *mask, gc, l->real_p, l->npoint,
		 Complex, CoordModePrevious);
    l->real_p[0] = tmp;

    XFreeGC(XtDisplay(w), gc);

    return True;
}

static void 
releasePolyBand(Widget w, LocalInfo * l, XButtonEvent * event,
		OpInfo * info, Pixmap * mask)
{
    if (l->first) {
	l->first = False;
	return;
    } else {
	l->endX = event->x;
	l->endY = event->y;
	l->startX = event->x;
	l->startY = event->y;
    }

    if (l->npoint > l->size - 3) {
	l->size += 32;
	l->real_p = (XPoint *) XtRealloc((XtPointer) l->real_p,
					 l->size * sizeof(XPoint));
	l->window_p = (XPoint *) XtRealloc((XtPointer) l->window_p,
					   l->size * sizeof(XPoint));
    }
    l->window_p[l->npoint].x = event->x;
    l->window_p[l->npoint].y = event->y;
    l->real_p[l->npoint].x = info->x;
    l->real_p[l->npoint].y = info->y;
    l->npoint++;
}


static void 
release(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    int mask;
    Pixmap mpix;

    /*
    **  Check to make sure all buttons are up, before doing this
     */
    mask = AllButtonsMask;
    switch (event->button) {
    case Button1:
	mask ^= Button1Mask;
	break;
    case Button2:
	mask ^= Button2Mask;
	break;
    case Button3:
	mask ^= Button3Mask;
	break;
    case Button4:
	mask ^= Button4Mask;
	break;
    case Button5:
	mask ^= Button5Mask;
	break;
    }
    if ((event->state & mask) != 0)
	return;

    mpix = None;

    if (l->areaType == 0)
	releaseBoxBand(w, l, event, info, &mpix);
    else if (releaseLassoBand(w, l, event, info, &mpix) == False) {
	PwRegionFinish(w, True);
	return;
    }
    if (l->pixBox.width <= 1 || l->pixBox.height <= 1) {
	PwRegionFinish(w, True);
	return;
    }
    if (cutMode != 0 && !chromaCut(w, l, &mpix)) {
	PwRegionFinish(w, True);
	return;
    }
    PwRegionSet(w, &l->pixBox, None, mpix);
}

/*
**  Those public functions
 */
void *
SelectBoxAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->areaType = 0;
    l->lastType = 0;
    l->gcx = GetGCX(w);

    l->size = 8;
    l->real_p = (XPoint *) XtCalloc(sizeof(XPoint), l->size);
    l->window_p = (XPoint *) XtCalloc(sizeof(XPoint), l->size);

    XtVaSetValues(w, XtNcompress, True, NULL);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opWindow, ButtonMotionMask, FALSE,
		      (OpEventProc) motion, l);
    OpAddEventHandler(w, opWindow, ButtonReleaseMask, FALSE,
		      (OpEventProc) release, l);
    OpAddEventHandler(w, opWindow, KeyPressMask, FALSE,
		      (OpEventProc) keyPress, l);
    SetCrossHairCursor(w);

    return l;
}

void 
SelectBoxRemove(Widget w, void *p)
{
    LocalInfo *l = (LocalInfo *) p;

    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, ButtonMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);
    OpRemoveEventHandler(w, opWindow, KeyPressMask, FALSE,
			 (OpEventProc) keyPress, l);
    PwRegionFinish(w, False);

    XtFree((XtPointer) l->window_p);
    XtFree((XtPointer) l->real_p);
    XtFree((XtPointer) l);
}

void *
LassoAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->areaType = 1;
    l->gcx = GetGCX(w);

    XtVaSetValues(w, XtNcompress, False, NULL);

    l->size = 256;
    l->real_p = (XPoint *) XtCalloc(sizeof(XPoint), l->size);
    l->window_p = (XPoint *) XtCalloc(sizeof(XPoint), l->size);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opWindow, ButtonMotionMask, FALSE,
		      (OpEventProc) motion, l);
    OpAddEventHandler(w, opWindow, ButtonReleaseMask, FALSE,
		      (OpEventProc) release, l);
    OpAddEventHandler(w, opWindow, KeyPressMask, FALSE,
		      (OpEventProc) keyPress, l);

    SetPencilCursor(w);

    return l;
}

void 
LassoRemove(Widget w, void *p)
{
    LocalInfo *l = (LocalInfo *) p;

    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, ButtonMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);
    OpRemoveEventHandler(w, opWindow, KeyPressMask, FALSE,
			 (OpEventProc) keyPress, l);
    PwRegionFinish(w, False);

    XtFree((XtPointer) l->window_p);
    XtFree((XtPointer) l->real_p);
    XtFree((XtPointer) l);
}


void *
SelectPolyAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->areaType = 2;
    l->drawn = False;
    l->first = True;
    l->tracking = False;
    l->gcx = GetGCX(w);

    XtVaSetValues(w, XtNcompress, False, NULL);

    l->size = 256;
    l->real_p = (XPoint *) XtCalloc(sizeof(XPoint), l->size);
    l->window_p = (XPoint *) XtCalloc(sizeof(XPoint), l->size);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
		      (OpEventProc) pressPolyBand, l);
    OpAddEventHandler(w, opWindow, PointerMotionMask, FALSE,
		      (OpEventProc) motionPolyBand, l);
    OpAddEventHandler(w, opWindow, ButtonReleaseMask, FALSE,
		      (OpEventProc) releasePolyBand, l);
    OpAddEventHandler(w, opWindow, KeyPressMask, FALSE,
		      (OpEventProc) keyPress, l);

    SetPencilCursor(w);

    return l;
}

void 
SelectPolyRemove(Widget w, void *p)
{
    LocalInfo *l = (LocalInfo *) p;

    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
			 (OpEventProc) pressPolyBand, l);
    OpRemoveEventHandler(w, opWindow, PointerMotionMask, FALSE,
			 (OpEventProc) motionPolyBand, l);
    OpRemoveEventHandler(w, opWindow, ButtonReleaseMask, FALSE,
			 (OpEventProc) releasePolyBand, l);
    OpRemoveEventHandler(w, opWindow, KeyPressMask, FALSE,
			 (OpEventProc) keyPress, l);
    PwRegionFinish(w, False);

    XtFree((XtPointer) l->window_p);
    XtFree((XtPointer) l->real_p);
    XtFree((XtPointer) l);
}

void 
SelectSetShapeMode(int value)
{
    shapeMode = value;
}

void 
SelectSetCutMode(int value)
{
    cutMode = value;
#if RANGEBUTTONS
    ChromaSetCutMode(value);
#endif
}

int 
SelectGetCutMode(void)
{
    return cutMode;
}
