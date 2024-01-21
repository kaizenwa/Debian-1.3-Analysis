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

/* Portions copyright 1995, 1995 Torsten Martinsen */

/* $Id: PaintRegion.c,v 1.6 1996/05/12 08:17:01 torsten Exp $ */

/*
** PaintRegion.c -- Hopefully all of the routines to get, set and  
**   manipulate the selection region.
**
**  Not part of the "selection" operation, since this really
**   need to know lots of hidden information (why?)
 */

#include <stdio.h>
#include <math.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/Xaw/Grip.h>
#include "PaintP.h"
#include "protocol.h"

#define SHAPE

#ifdef SHAPE
#include <X11/extensions/shape.h>
#endif

static void regionExpose(Widget, PaintWidget, XEvent *, Boolean *);

#define regionRedraw(pw)  regionExpose(pw->paint.region.child, pw, NULL, NULL)

#define BoolStr(flg)	((flg) ? "True" : "False")

#undef INTERACTIVE

/*
**  Border Width of child widget
 */
#define BW	0

/*
**  2x2 matrix stuff
**
 */

#define XFORM(x,y,mat,nx,ny)	nx = mat[0][0] * x + mat[0][1] * y; \
				ny = mat[1][0] * x + mat[1][1] * y
#define COPY_MAT(s,d)	d[0][0] = s[0][0]; d[0][1] = s[0][1]; \
			d[1][0] = s[1][0]; d[1][1] = s[1][1]

#define INVERT_MAT(mat, inv) do {			\
		float _d = 1.0 / (mat[0][0] * mat[1][1] \
			      - mat[0][1] * mat[1][0]);	\
		(inv)[0][0] =  (mat)[1][1] * _d;	\
		(inv)[1][1] =  (mat)[0][0] * _d;	\
		(inv)[0][1] = -(mat)[0][1] * _d;	\
		(inv)[1][0] = -(mat)[1][0] * _d;	\
	} while (0)

#define ZERO(v)		(((v) > -1e-5) && ((v) < 1e-5))

#ifndef MIN
#define MIN(a,b)        (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a,b)        (((a) > (b)) ? (a) : (b))
#endif
#ifndef SIGN
#define SIGN(a)		(((a) < 0) ? -1 : 1)
#endif
#ifndef ABS
#define ABS(a)          ((a > 0) ? (a) : 0 - (a))
#endif

#define MKMAT(pw) do {							\
		pwMatrix	m;					\
		m[0][0] = pw->paint.region.scaleX;			\
		m[1][1] = pw->paint.region.scaleY;			\
		m[0][1] = m[1][0] = 0.0;				\
		mm(pw->paint.region.rotMat, m, pw->paint.region.mat);	\
	} while (0)

static pwMatrix matIdentity =
{
    {1, 0},
    {0, 1}
};

static void 
doCallbacks(PaintWidget pw, int flag)
{
    PaintWidget tpw = (pw->paint.paint == None) ?
    pw : (PaintWidget) pw->paint.paint;
    int i;

    XtCallCallbackList((Widget) tpw, tpw->paint.regionCalls, (XtPointer) flag);
    for (i = 0; i < tpw->paint.paintChildrenSize; i++) {
	PaintWidget p = (PaintWidget) tpw->paint.paintChildren[i];
	XtCallCallbackList((Widget) p, p->paint.regionCalls, (XtPointer) flag);
    }
}

/*
 * Multiply matrices a and b and store result in r.
 */
static void 
mm(pwMatrix a, pwMatrix b, pwMatrix r)
{
    float t00, t10, t01, t11;

    t00 = a[0][0] * b[0][0] + a[0][1] * b[1][0];
    t01 = a[0][0] * b[0][1] + a[0][1] * b[1][1];
    t10 = a[1][0] * b[0][0] + a[1][1] * b[1][0];
    t11 = a[1][0] * b[0][1] + a[1][1] * b[1][1];

    r[0][0] = t00;
    r[1][0] = t10;
    r[0][1] = t01;
    r[1][1] = t11;
}

/*
**   PwRegionSet -- set the active image region
**     add handles and other useful things
**     if the pix == None, use the current paint info in rect
**     else use the pixmap.
**
 */
static void 
buildSources(PaintWidget pw)
{
    if (pw->paint.region.sourceImg == NULL) {
	pw->paint.region.sourceImg = XGetImage(XtDisplay(pw),
					   pw->paint.region.source, 0, 0,
					     pw->paint.region.orig.width,
		       pw->paint.region.orig.height, AllPlanes, ZPixmap);
    }
    if (pw->paint.region.mask == None)
	return;

    if (pw->paint.region.maskImg == NULL) {
	pw->paint.region.maskImg = XGetImage(XtDisplay(pw),
					     pw->paint.region.mask, 0, 0,
					     pw->paint.region.orig.width,
		       pw->paint.region.orig.height, AllPlanes, ZPixmap);
    }
}

static void 
resizeImg(PaintWidget pw, pwMatrix inv, Pixmap * pix, XImage * pixSrc,
	  Pixmap * msk, XImage * mskSrc, Pixmap * shape)
{
    int width, height, depth;
    int x, y;
    XImage *pixDst, *mskDst, *shapeDst = NULL;
    int sourceW, sourceH;
    int ix, iy, fx, fy, cx, cy;
    float dx, dy, sx, sy;

    if (pixSrc == NULL || pix == NULL)
	return;
    if (pw->paint.region.rect.width == 0 || pw->paint.region.rect.height == 0)
	return;

    /*
    **  Construct the dest pixmap.
     */

    if (*pix != None)
	XFreePixmap(XtDisplay(pw), *pix);
    if (*msk != None && mskSrc != NULL)
	XFreePixmap(XtDisplay(pw), *msk);

    depth = pixSrc->depth;

    width = pw->paint.region.rect.width / GET_ZOOM(pw);
    height = pw->paint.region.rect.height / GET_ZOOM(pw);

    *pix = XCreatePixmap(XtDisplay(pw), XtWindow(pw), width, height, depth);
    pixDst = XCreateImage(XtDisplay(pw), pw->paint.visual,
			  depth, ZPixmap, 0, NULL, width, height, 32, 0);
    pixDst->data = (char *) XtMalloc(height * pixDst->bytes_per_line);

    if (mskSrc != NULL) {
	*msk = XCreatePixmap(XtDisplay(pw), XtWindow(pw), width, height, 1);
	mskDst = XCreateImage(XtDisplay(pw), pw->paint.visual,
			      1, ZPixmap, 0, NULL, width, height, 32, 0);
	mskDst->data = (char *) XtMalloc(height * mskDst->bytes_per_line);
#ifdef SHAPE
	if (shape != NULL) {
	    *shape = XCreatePixmap(XtDisplay(pw), XtWindow(pw), width, height, 1);
	    shapeDst = XCreateImage(XtDisplay(pw), pw->paint.visual,
			      1, ZPixmap, 0, NULL, width, height, 32, 0);
	    shapeDst->data = (char *) XtMalloc(height * shapeDst->bytes_per_line);
	}
#endif
    }
    cx = pw->paint.region.orig.width / 2;
    cy = pw->paint.region.orig.height / 2;
    fx = (int) (-width / 2);
    fy = (int) (-height / 2);
    sourceW = pixSrc->width;
    sourceH = pixSrc->height;
    for (y = 0, dy = fy; y < height; y++, dy++) {
	for (x = 0, dx = fx; x < width; x++, dx++) {
	    XFORM(dx, dy, inv, sx, sy);
	    ix = (sx + cx);
	    iy = (sy + cy);
	    if (ix >= 0 && ix < sourceW && iy >= 0 && iy < sourceH) {
		xxPutPixel(pixDst, x, y, xxGetPixel(pixSrc, ix, iy));
		if (mskSrc != NULL)
		    XPutPixel(mskDst, x, y, XGetPixel(mskSrc, ix, iy));
#ifdef SHAPE
		if (shapeDst != NULL)
		    XPutPixel(shapeDst, x, y, True);
#endif
	    } else if (mskSrc != NULL) {
		XPutPixel(mskDst, x, y, False);
#ifdef SHAPE
		if (shapeDst != NULL)
		    XPutPixel(shapeDst, x, y, False);
#endif
	    }
	}
    }

    XPutImage(XtDisplay(pw), *pix, pw->paint.tgc, pixDst,
	      0, 0, 0, 0, width, height);
    XDestroyImage(pixDst);

    if (mskSrc != NULL) {
	XPutImage(XtDisplay(pw), *msk, pw->paint.mgc, mskDst,
		  0, 0, 0, 0, width, height);
	XDestroyImage(mskDst);
    }
#ifdef SHAPE
    if (shapeDst != NULL) {
	XPutImage(XtDisplay(pw), *shape, pw->paint.mgc, shapeDst,
		  0, 0, 0, 0, width, height);
	XDestroyImage(shapeDst);
    }
#endif
}

static void 
regionCreateNotMask(PaintWidget pw)
{
    if (pw->paint.region.rect.width == 0 || pw->paint.region.rect.height == 0)
	return;

    if (pw->paint.region.mask == None)
	return;

    if (pw->paint.region.notMask != None)
	XFreePixmap(XtDisplay(pw), pw->paint.region.notMask);

    pw->paint.region.notMask = XCreatePixmap(XtDisplay(pw), XtWindow(pw),
	   pw->paint.region.rect.width, pw->paint.region.rect.height, 1);
    XSetFunction(XtDisplay(pw), pw->paint.mgc, GXcopyInverted);
    XCopyArea(XtDisplay(pw), pw->paint.region.mask, pw->paint.region.notMask,
	      pw->paint.mgc, 0, 0,
	pw->paint.region.rect.width, pw->paint.region.rect.height, 0, 0);
    XSetFunction(XtDisplay(pw), pw->paint.mgc, GXcopy);

    XSetClipMask(XtDisplay(pw), pw->paint.region.bg_gc,
		 pw->paint.region.notMask);
    XSetClipMask(XtDisplay(pw), pw->paint.region.fg_gc, pw->paint.region.mask);
}

static void 
createVtxPts(PaintWidget pw, float vtx[9][2], Boolean flag, Boolean useZoom)
{
    int zoom;
    int i;
    int x0, x1, y0, y1;
    int width = pw->paint.region.orig.width;
    int height = pw->paint.region.orig.height;

    if (useZoom)
	zoom = GET_ZOOM(pw);
    else
	zoom = 1;

    x0 = (-width / 2);
    x1 = (width + x0);
    y0 = (-height / 2);
    y1 = (height + y0);
    x0 *= zoom;
    x1 *= zoom;
    y0 *= zoom;
    y1 *= zoom;

    /*
    **  Watch out, these are points 0,1, and _3__
     */
    XFORM(x0, y0, pw->paint.region.mat, vtx[0][0], vtx[0][1]);
    XFORM(x1, y0, pw->paint.region.mat, vtx[1][0], vtx[1][1]);
    XFORM(x0, y1, pw->paint.region.mat, vtx[3][0], vtx[3][1]);

    if (flag) {
	XFORM(x1, y1, pw->paint.region.mat, vtx[2][0], vtx[2][1]);

	for (i = 0; i < 4; i++) {
	    vtx[i][0] += pw->paint.region.centerX * zoom;
	    vtx[i][1] += pw->paint.region.centerY * zoom;
	}
    } else {
	/*
	**  sort the points, so that point 0,0 is top left corner
	 */
	if (vtx[0][0] > vtx[1][0]) {
	    float t = x0;
	    x0 = x1;
	    x1 = t;
	}
	if (vtx[0][1] > vtx[3][1]) {
	    float t = y0;
	    y0 = y1;
	    y1 = t;
	}
	XFORM(x0, y0, pw->paint.region.mat, vtx[0][0], vtx[0][1]);
	XFORM(0, y0, pw->paint.region.mat, vtx[1][0], vtx[1][1]);
	XFORM(x1, y0, pw->paint.region.mat, vtx[2][0], vtx[2][1]);

	XFORM(x0, 0, pw->paint.region.mat, vtx[3][0], vtx[3][1]);
	XFORM(0, 0, pw->paint.region.mat, vtx[4][0], vtx[4][1]);
	XFORM(x1, 0, pw->paint.region.mat, vtx[5][0], vtx[5][1]);

	XFORM(x0, y1, pw->paint.region.mat, vtx[6][0], vtx[6][1]);
	XFORM(0, y1, pw->paint.region.mat, vtx[7][0], vtx[7][1]);
	XFORM(x1, y1, pw->paint.region.mat, vtx[8][0], vtx[8][1]);
    }
}

static void 
doResize(PaintWidget pw)
{
    pwMatrix inv;
    Pixmap shape, *shp = &shape;

    buildSources(pw);

    /*
    **  First find out the bounding extent of the transformed
    **   area, then scale it to fit inside of the "region box"
     */

    if (pw->paint.region.maskImg == NULL) {
	Boolean needMask = False;
	float vtx[9][2];
	float minX, minY, maxX, maxY;
	int x, cmin, cmax;

	createVtxPts(pw, vtx, True, False);

	minX = MIN(vtx[0][0], MIN(vtx[1][0], MIN(vtx[2][0], vtx[3][0])));
	minY = MIN(vtx[0][1], MIN(vtx[1][1], MIN(vtx[2][1], vtx[3][1])));
	maxX = MAX(vtx[0][0], MAX(vtx[1][0], MAX(vtx[2][0], vtx[3][0])));
	maxY = MAX(vtx[0][1], MAX(vtx[1][1], MAX(vtx[2][1], vtx[3][1])));

	/*
	**  After computing min, max see if there are points
	**   on all vertices, if so then set the correct return code
	 */
	for (cmin = cmax = x = 0; x < 4; x++) {
	    if ((int) vtx[x][0] == (int) minX)
		cmin++;
	    else if ((int) vtx[x][0] == (int) maxX)
		cmax++;
	}
	needMask |= (cmin != 2 || cmax != 2);
	for (cmin = cmax = x = 0; x < 4; x++) {
	    if ((int) vtx[x][1] == (int) minY)
		cmin++;
	    else if ((int) vtx[x][1] == (int) maxY)
		cmax++;
	}
	needMask |= (cmin != 2 || cmax != 2);

	if (needMask) {
	    Pixmap mask;
	    GC mgc;

	    /*
	    **  If the image we just transformed needs a mask
	    **  and one doesn't exist, construct one.
	     */
	    mask = pw->paint.region.mask =
		XCreatePixmap(XtDisplay(pw), XtWindow(pw),
			      pw->paint.region.orig.width,
			      pw->paint.region.orig.height, 1);
	    mgc = GET_MGC(pw, mask);
	    XSetFunction(XtDisplay(pw), mgc, GXset);
	    XFillRectangle(XtDisplay(pw), mask, mgc, 0, 0,
			   pw->paint.region.orig.width,
			   pw->paint.region.orig.height);
	    XSetFunction(XtDisplay(pw), mgc, GXcopy);

	    buildSources(pw);

	    if (pw->paint.region.fg_gc == None) {
		pw->paint.region.fg_gc = XCreateGC(XtDisplay(pw), XtWindow(pw),
						   0, 0);
		pw->paint.region.bg_gc = XCreateGC(XtDisplay(pw), XtWindow(pw),
						   0, 0);
	    }
	}
    }
    INVERT_MAT(pw->paint.region.mat, inv);
#ifdef SHAPE
    if (pw->paint.region.maskImg == NULL || GET_ZOOM(pw) != 1)
	shp = NULL;
#else
    shp = NULL;
#endif
    resizeImg(pw, inv, &pw->paint.region.source, pw->paint.region.sourceImg,
	      &pw->paint.region.mask, pw->paint.region.maskImg, shp);
    regionCreateNotMask(pw);

#ifdef SHAPE
    if (shp != NULL) {
	XShapeCombineMask(XtDisplay(pw), XtWindow(pw->paint.region.child),
			  ShapeBounding, 0, 0, shape, ShapeSet);
	XFreePixmap(XtDisplay(pw), shape);
    }
#endif

    pw->paint.region.needResize = False;
}

static void 
drawRegionBox(PaintWidget pw, Boolean flag)
{
    static XPoint xvtxLast[5];
    XPoint xvtx[5];
    float vtx[9][2];
    Window window = XtWindow(pw);
    int i;

    createVtxPts(pw, vtx, True, True);

    xvtx[0].x = vtx[0][0];
    xvtx[0].y = vtx[0][1];
    xvtx[1].x = vtx[1][0];
    xvtx[1].y = vtx[1][1];
    xvtx[2].x = vtx[2][0];
    xvtx[2].y = vtx[2][1];
    xvtx[3].x = vtx[3][0];
    xvtx[3].y = vtx[3][1];
    xvtx[4].x = vtx[0][0];
    xvtx[4].y = vtx[0][1];

    for (i = 0; i < 5; i++) {
	xvtx[i].x += pw->paint.region.child->core.x;
	xvtx[i].y += pw->paint.region.child->core.y;
    }

    if (pw->paint.region.isDrawn) {
	XDrawLines(XtDisplay(pw), window, pw->paint.xgc, xvtxLast, 5,
		   CoordModeOrigin);
	pw->paint.region.isDrawn = False;
    }
    if (flag) {
	XDrawLines(XtDisplay(pw), window, pw->paint.xgc, xvtx, 5, CoordModeOrigin);
	memcpy(xvtxLast, xvtx, sizeof(xvtxLast));
	pw->paint.region.isDrawn = True;
    }
}

/*
**
 */

static void 
regionResizeWindow(PaintWidget pw, Boolean sameCenter)
{
    int zoom = GET_ZOOM(pw);
    int minX, minY, maxX, maxY;
    int width, height, dx, dy, nx, ny;
    int newX, newY;
    int i;
    float vtx[9][2];

#ifndef INTERACTIVE
    if (pw->paint.region.isTracking)
	return;
#endif

    createVtxPts(pw, vtx, False, False);

    minX = MIN(vtx[0][0], vtx[6][0]);
    maxX = MAX(vtx[2][0], vtx[8][0]);
    minY = MIN(vtx[0][1], vtx[2][1]);
    maxY = MAX(vtx[6][1], vtx[8][1]);

    width = maxX - minX + 0.5;
    height = maxY - minY + 0.5;

    newX = pw->paint.region.centerX - (width / 2);
    newY = pw->paint.region.centerY - (height / 2);

    newX += pw->paint.region.rect.x;
    newY += pw->paint.region.rect.y;
    if (!sameCenter) {
	pw->paint.region.centerX = width / 2;
	pw->paint.region.centerY = height / 2;
    } else {
	pw->paint.region.centerX = width / 2;
	pw->paint.region.centerY = height / 2;
    }

    if ((width *= zoom) < 10)
	width = 10;
    if ((height *= zoom) < 10)
	height = 10;
    XtResizeWidget(pw->paint.region.child, width, height, BW);
    nx = (newX - pw->paint.zoomX) * zoom;
    ny = (newY - pw->paint.zoomY) * zoom;
    XtMoveWidget(pw->paint.region.child, nx - BW, ny - BW);

    pw->paint.region.rect.x = newX;
    pw->paint.region.rect.y = newY;
    pw->paint.region.rect.width = width;
    pw->paint.region.rect.height = height;

    /*
    **  Now place all the grips.
     */
    createVtxPts(pw, vtx, False, True);
    width = pw->paint.region.grip[0]->core.width;
    height = pw->paint.region.grip[0]->core.height;
    dx = pw->paint.region.centerX * zoom - width / 2;
    dy = pw->paint.region.centerY * zoom - height / 2;
    for (i = 0; i < 9; i++) {
	int x, y;

	if (i == 4)
	    continue;

	x = vtx[i][0] + dx;
	y = vtx[i][1] + dy;

	if (x < 0)
	    x = 0;
	if (y < 0)
	    y = 0;
	if (x + width > pw->paint.region.rect.width)
	    x = pw->paint.region.rect.width - width;
	if (y + height > pw->paint.region.rect.height)
	    y = pw->paint.region.rect.height - height;

	XtMoveWidget(pw->paint.region.grip[i], x, y);
    }
}

/*
**
 */

static void 
moveGrips(PaintWidget pw)
{
    int width, height;
    int i, gx, gy;
    Dimension w, h;
    Widget widget = pw->paint.region.grip[0];

    w = widget->core.width;
    h = widget->core.height;
    width = widget->core.parent->core.width;
    height = widget->core.parent->core.height;

    for (i = 0; i < 9; i++) {
	if (i == 4)
	    continue;

	switch (i % 3) {
	case 0:
	    gx = 0;
	    break;
	case 1:
	    gx = width / 2 - w / 2;
	    break;
	case 2:
	    gx = width - w;
	    break;
	}
	switch (i / 3) {
	case 0:
	    gy = 0;
	    break;
	case 1:
	    gy = height / 2 - h / 2;
	    break;
	case 2:
	    gy = height - h;
	    break;
	}

	XtMoveWidget(pw->paint.region.grip[i], gx, gy);
    }
}

static void 
gripPress(Widget w, PaintWidget pw, XButtonEvent * event, Boolean * junk)
{
    static int fixedPoint[] =
    {8, 8, 6, 8, -1, 6, 2, 2, 0};
    float vtx[9][2], fvtx[9][2];
    float x0, x1, x2, y0, y1, y2, t1, t2, l;
    int index, i;

    pw->paint.region.offX = event->x;
    pw->paint.region.offY = event->y;
    pw->paint.region.baseX = event->x_root - w->core.x;
    pw->paint.region.baseY = event->y_root - w->core.y;

    pw->paint.region.isTracking = True;

    /*
    **  Compute which grip was grabbed, to determine constrain line.
     */
    for (index = 0; index < 9 && pw->paint.region.grip[index] != w; index++);

    createVtxPts(pw, vtx, False, False);
    createVtxPts(pw, fvtx, True, False);

    x0 = vtx[0][0];
    y0 = vtx[0][1];
    x1 = vtx[2][0];
    y1 = vtx[2][1];
    x2 = vtx[6][0];
    y2 = vtx[6][1];

    pw->paint.region.lineBase[0] = 0;
    pw->paint.region.lineBase[1] = 0;

    t1 = x1 - x0;
    t2 = y1 - y0;
    l = sqrt(t1 * t1 + t2 * t2);
    pw->paint.region.lineDelta[0] = t1 / l;
    pw->paint.region.lineDelta[1] = t2 / l;

    t1 = x2 - x0;
    t2 = y2 - y0;
    l = sqrt(t1 * t1 + t2 * t2);
    pw->paint.region.lineDelta[2] = t1 / l;
    pw->paint.region.lineDelta[3] = t2 / l;

    pw->paint.region.startScaleX = pw->paint.region.scaleX;
    pw->paint.region.startScaleY = pw->paint.region.scaleY;

    /*
    **  Now compute which corner of the 4 cornered box doesn't move
    **    as the object is resized.
     */
    for (i = 0; i < 4; i++) {
	float fx = vtx[fixedPoint[index]][0];
	float fy = vtx[fixedPoint[index]][1];
	float px = fvtx[i][0] - pw->paint.region.centerX;
	float py = fvtx[i][1] - pw->paint.region.centerY;

	if (ZERO(fx - px) && ZERO(fy - py))
	    break;
    }
    pw->paint.region.fixedPoint = i;
}

static void 
regionButtonPress(Widget w, PaintWidget pw, XButtonEvent * event, Boolean * junk)
{
    pw->paint.region.isRotate = event->button == Button2;

    pw->paint.region.offX = event->x;
    pw->paint.region.offY = event->y;
    pw->paint.region.baseX = event->x_root - w->core.x;
    pw->paint.region.baseY = event->y_root - w->core.y;

    pw->paint.region.lastX = event->x_root;
    pw->paint.region.lastY = event->y_root;

    if (pw->paint.region.isRotate) {
	XDefineCursor(XtDisplay(w), XtWindow(pw->paint.region.child),
		      XCreateFontCursor(XtDisplay(w), XC_exchange));
	pw->paint.region.lastAngle = 0.0;
    }
    /*
    **  Only draw the interactive box when we are rotating.
     */
    pw->paint.region.isTracking = pw->paint.region.isRotate;
}
static void 
gripRelease(Widget w, PaintWidget pw, XButtonEvent * event, Boolean * junk)
{
    pw->paint.region.isTracking = False;
    drawRegionBox(pw, False);
#ifndef INTERACTIVE
    if (pw->paint.region.needResize) {
	regionResizeWindow(pw, False);
	regionRedraw(pw);
    }
#endif
}
static void 
regionButtonRelease(Widget w, PaintWidget pw, XButtonEvent * event,
		    Boolean * junk)
{
    pw->paint.region.isTracking = False;
    drawRegionBox(pw, False);

    if (!pw->paint.region.isRotate)
	return;

#ifndef INTERACTIVE
    if (pw->paint.region.needResize) {
	regionResizeWindow(pw, False);
	regionRedraw(pw);
    }
#endif

    XDefineCursor(XtDisplay(w), XtWindow(pw->paint.region.child),
		  XCreateFontCursor(XtDisplay(w), XC_fleur));
}
static void 
regionGrab(Widget w, PaintWidget pw, XMotionEvent * event, Boolean * junk)
{
    int dx, dy, nx, ny;

    while (XCheckTypedWindowEvent(XtDisplay(w), XtWindow(w),
				  MotionNotify, (XEvent *) event));

    PwRegionTear((Widget) pw);

    if (pw->paint.region.isRotate) {
	double da, na;
	pwMatrix m;

	dx = event->x - pw->paint.region.rect.width / 2;
	dy = event->y - pw->paint.region.rect.height / 2;
	na = atan2((double) dy, (double) dx);
	/*
	 * If Shift is pressed, constrain rotation to multiples of 15 degrees.
	 */
	if (event->state & ShiftMask)
	    na = ((int) (na / (15.0 / 180.0 * M_PI))) * (15.0 / 180.0 * M_PI);
	da = na - pw->paint.region.lastAngle;
	pw->paint.region.lastAngle = na;

	m[0][0] = cos(da);
	m[0][1] = -sin(da);
	m[1][0] = sin(da);
	m[1][1] = cos(da);

	PwRegionAppendMatrix((Widget) pw, m);
    } else {
	int zoom = GET_ZOOM(pw);

	nx = event->x_root - pw->paint.region.baseX;
	ny = event->y_root - pw->paint.region.baseY;

	/*
	 * If Shift is pressed, constrain movement to horizontal or vertical
	 */
	if (event->state & ShiftMask)
	    if (ABS(event->x_root - pw->paint.region.lastX) >
		ABS(event->y_root - pw->paint.region.lastY))
		ny = pw->paint.region.lastY - pw->paint.region.baseY;
	    else
		nx = pw->paint.region.lastX - pw->paint.region.baseX;

	dx = (nx - w->core.x) / zoom;
	dy = (ny - w->core.y) / zoom;

	if (dx == 0 && dy == 0)
	    return;


	pw->paint.region.rect.x += dx;
	pw->paint.region.rect.y += dy;

	nx = (pw->paint.region.rect.x - pw->paint.zoomX) * zoom;
	ny = (pw->paint.region.rect.y - pw->paint.zoomY) * zoom;

	XtMoveWidget(pw->paint.region.child, nx - BW, ny - BW);
    }
}

static void 
regionExpose(Widget w, PaintWidget pw, XEvent * event, Boolean * junk)
{
    XImage *xim = NULL;
    XRectangle rect, nrect;
    int isExpose;
    int zoom = GET_ZOOM(pw);

    if (!pw->paint.region.isVisible)
	return;

    if (event == NULL) {
	isExpose = True;
	rect.x = 0;
	rect.y = 0;
	rect.width = w->core.width;
	rect.height = w->core.height;
    } else if (event->xany.type == Expose) {
	rect.x = event->xexpose.x;
	rect.y = event->xexpose.y;
	rect.width = event->xexpose.width;
	rect.height = event->xexpose.height;
	isExpose = True;
    } else if (event->xany.type == ConfigureNotify) {
	isExpose = False;
	rect.x = 0;
	rect.y = 0;
	rect.width = w->core.width;
	rect.height = w->core.height;
    } else {
	return;
    }

#ifndef INTERACTIVE
    if (pw->paint.region.isTracking) {
	drawRegionBox(pw, True);
	return;
    }
#endif
    if (pw->paint.region.needResize)
	doResize(pw);

    if (zoom != 1) {
	int width, height;
	int pixW, pixH;

	nrect.x = rect.x / zoom;
	nrect.y = rect.y / zoom;
	width = (rect.width + zoom - 1) / zoom;
	height = (rect.height + zoom - 1) / zoom;
	/*
	**  It is possible for this to happen, when resizes and 
	**    redraws are slightly out of sync.
	 */
	pixW = pw->paint.region.rect.width / zoom;
	pixH = pw->paint.region.rect.height / zoom;
	if (width + nrect.x > pixW)
	    width = pixW - nrect.x;
	if (height + nrect.y > pixH)
	    height = pixH - nrect.y;
	if (width <= 0 || height <= 0)
	    return;
	nrect.width = width;
	nrect.height = height;
    }
    if (isExpose) {
	if (zoom == 1) {
	    XCopyArea(XtDisplay(w), pw->paint.region.source, XtWindow(w),
		      pw->paint.region.fg_gc == None ?
		      pw->paint.tgc : pw->paint.region.fg_gc,
		rect.x, rect.y, rect.width, rect.height, rect.x, rect.y);
	} else {
	    XImage *src, *msk = NULL;
	    XRectangle tr;

	    tr = nrect;
	    tr.x = 0;
	    tr.y = 0;

	    src = XGetImage(XtDisplay(pw), pw->paint.region.source,
			    nrect.x, nrect.y, nrect.width, nrect.height,
			    AllPlanes, ZPixmap);
	    if (pw->paint.region.mask != None)
		msk = XGetImage(XtDisplay(pw), pw->paint.region.mask,
			     nrect.x, nrect.y, nrect.width, nrect.height,
				AllPlanes, ZPixmap);
	    _PwZoomDraw(pw, w, pw->paint.tgc, src, msk,
			False, nrect.x, nrect.y, zoom, &tr);
	    XDestroyImage(src);
	    if (msk != NULL)
		XDestroyImage(msk);
	}
    }
    /*
    **  XXX -- This should merge, and do fun things.. but.
     */
    if (isExpose && (event != NULL && event->xexpose.count != 0))
	return;

    if (pw->paint.region.mask != None) {
	int x = w->core.x + w->core.border_width;
	int y = w->core.y + w->core.border_width;

	/*
	**  Copy in the background picture
	 */
	if (GET_ZOOM(pw) != 1) {
	    XRectangle tr;
	    XImage *nmsk;
	    int dx = 0, dy = 0;
	    PaintWidget tpw = (pw->paint.paint == None) ?
	    pw : (PaintWidget) pw->paint.paint;
	    int width, height;

	    tr.x = (nrect.x + x) / zoom + pw->paint.zoomX;
	    tr.y = (nrect.y + y) / zoom + pw->paint.zoomY;
	    width = nrect.width;
	    height = nrect.height;

	    /*
	    **  We could use PwGetImage, but it returns
	    **    the original image, not the sub-region
	     */
	    if (tr.x < 0) {
		width += tr.x;
		dx = -tr.x;
		tr.x = 0;
	    }
	    if (tr.y < 0) {
		height += tr.y;
		dy = -tr.y;
		tr.y = 0;
	    }
	    if (tr.x + width > tpw->paint.drawWidth)
		width = tpw->paint.drawWidth - tr.x;
	    if (tr.y + height > tpw->paint.drawHeight)
		height = tpw->paint.drawHeight - tr.y;
	    if (width > 0 && height > 0) {
		tr.width = width;
		tr.height = height;
		xim = XGetImage(XtDisplay(w), GET_PIXMAP(pw),
				tr.x, tr.y, tr.width, tr.height,
				AllPlanes, ZPixmap);
		tr.x = tr.y = 0;
		nmsk = XGetImage(XtDisplay(pw), pw->paint.region.notMask,
			 nrect.x + dx, nrect.y + dy, tr.width, tr.height,
				 AllPlanes, ZPixmap);

		_PwZoomDraw(pw, w, pw->paint.tgc, xim, nmsk, False,
			    dx, dy, zoom, &tr);
		XDestroyImage(nmsk);
		XDestroyImage(xim);
	    }
	} else {
	    XCopyArea(XtDisplay(w), GET_PIXMAP(pw), XtWindow(w),
		      pw->paint.region.bg_gc,
		      x, y, w->core.width, w->core.height, 0, 0);
	}
    }
}
static void 
regionMove(Widget w, PaintWidget pw, XEvent * event, Boolean * junk)
{
    if (pw->paint.region.mask == None)
	return;
    /*
    **  For some reason, I had to generate an extra function.. 
     */
    regionExpose(w, pw, event, junk);
}


static void 
regionSetGripCursors(PaintWidget pw)
{
    static int cursors[9] =
    {
	XC_top_left_corner,
	XC_top_side,
	XC_top_right_corner,
	XC_left_side,
	0,
	XC_right_side,
	XC_bottom_left_corner,
	XC_bottom_side,
	XC_bottom_right_corner
    };
    static int list[9] =
    {None, None, None, None, None, None, None, None, None};
    int i;

    if (list[0] == None) {
	for (i = 0; i < 9; i++) {
	    if (i != 4)
		list[i] = XCreateFontCursor(XtDisplay(pw), cursors[i]);
	}
    }
    for (i = 0; i < 9; i++)
	if (i != 4)
	    XDefineCursor(XtDisplay(pw), XtWindow(pw->paint.region.grip[i]), list[i]);
}

static void 
gripGrab(Widget w, PaintWidget pw, XMotionEvent * event, Boolean * junk)
{
    static Boolean isLeftEdge[] =
    {True, False, False,
     True, False, False,
     True, False, False};
    static Boolean isTopEdge[] =
    {True, True, True,
     False, False, False,
     False, False, False};
    static Boolean isMiddle[] =
    {False, True, False,
     True, False, True,
     False, True, False};
    Boolean sameScale;
    int index, i, fp;
    int zoom = GET_ZOOM(pw);
    float v[2];
    int width, height;
    float ovtx[9][2], nvtx[9][2];
    float dx, dy;

    for (index = 0; index < 9 && pw->paint.region.grip[index] != w; index++);

    /*
    **  Find the intersection point
     */
    for (i = 0; i < 2; i++) {
	float x0 = pw->paint.region.lineBase[0];
	float y0 = pw->paint.region.lineBase[1];
	float xm = event->x;
	float ym = event->y;

	dx = pw->paint.region.lineDelta[0 + i * 2];
	dy = pw->paint.region.lineDelta[1 + i * 2];
	v[i] = dx * (xm - x0) - dy * (y0 - ym);
    }

    if (pw->paint.region.startScaleX < 0)
	v[0] = -v[0];
    if (pw->paint.region.startScaleY < 0)
	v[1] = -v[1];
    if (isLeftEdge[index])
	v[0] = -v[0];
    if (isTopEdge[index])
	v[1] = -v[1];

    v[0] /= (float) zoom;
    v[1] /= (float) zoom;

    PwRegionTear((Widget) pw);

    sameScale = False;
    if (isMiddle[index] || (event->state & ShiftMask) != 0) {
	/*
	**  Apply the constraint
	 */
	if (index == 1 || index == 7)
	    v[0] = 0;
	else if (index == 3 || index == 5)
	    v[1] = 0;
	else
	    sameScale = True;
    }
    width = pw->paint.region.startScaleX * pw->paint.region.orig.width;
    height = pw->paint.region.startScaleY * pw->paint.region.orig.height;

    createVtxPts(pw, ovtx, True, False);

    pw->paint.region.scaleX =
	(float) ((int) (width + v[0])) / pw->paint.region.orig.width;
    pw->paint.region.scaleY =
	(float) ((int) (height + v[1])) / pw->paint.region.orig.height;

    if (sameScale) {
	float sx = ABS(pw->paint.region.scaleX);
	float sy = ABS(pw->paint.region.scaleY);

	sx = MIN(sx, sy);

	pw->paint.region.scaleX = SIGN(pw->paint.region.scaleX) * sx;
	pw->paint.region.scaleY = SIGN(pw->paint.region.scaleY) * sx;
    }
    MKMAT(pw);
    createVtxPts(pw, nvtx, True, False);

    fp = pw->paint.region.fixedPoint;
    dx = ovtx[fp][0] - nvtx[fp][0];
    dy = ovtx[fp][1] - nvtx[fp][1];

    pw->paint.region.centerX += dx;
    pw->paint.region.centerY += dy;

    pw->paint.region.needResize = True;
    regionRedraw(pw);
}

/*
**
**
 */
static void 
writeRegion(PaintWidget pw)
{
    GC gc;
    Pixmap pix, src;
    XRectangle nr;
    int zoom = GET_ZOOM(pw);

    if (!pw->paint.region.isVisible)
	return;
    /*
    **  No need to write it, it's still on the drawable.
     */
    if (pw->paint.region.isAttached)
	return;

    nr.x = pw->paint.region.rect.x;
    nr.y = pw->paint.region.rect.y;
    nr.width = pw->paint.region.rect.width / zoom;
    nr.height = pw->paint.region.rect.height / zoom;

    /*
    **  If we've already modified the background (aka ripped the
    **    image up)  then don't get a new undo buffer.
    **    The second case of the if is if you've done an undo inbetween.
     */
    if (pw->paint.region.undoPixmap == None ||
	pw->paint.region.undoPixmap != GET_PIXMAP(pw))
	pix = PwUndoStart((Widget) pw, &nr);
    else
	pix = pw->paint.region.undoPixmap;

    PwUndoAddRectangle((Widget) pw, &nr);

    if (pw->paint.region.fg_gc != None) {
	gc = pw->paint.region.fg_gc;
	XSetClipOrigin(XtDisplay(pw), gc, nr.x, nr.y);
    } else {
	gc = pw->paint.tgc;
    }

    if (pw->paint.region.proc != NULL) {
	XImage *sim = pw->paint.region.sourceImg;
	Boolean made = False;

	if (sim == NULL) {
	    sim = XGetImage(XtDisplay(pw),
			    pw->paint.region.source, 0, 0,
			    pw->paint.region.orig.width,
		       pw->paint.region.orig.height, AllPlanes, ZPixmap);
	    made = True;
	}
	src = (*pw->paint.region.proc) ((Widget) pw, sim, pw->paint.region.mat);
	if (made)
	    XDestroyImage(sim);
    } else {
	src = pw->paint.region.source;
    }

    XCopyArea(XtDisplay(pw), src, pix, gc,
	      0, 0,
	      nr.width, nr.height,
	      nr.x, nr.y);

    if (pw->paint.region.fg_gc != None)
	XSetClipOrigin(XtDisplay(pw), gc, 0, 0);

    PwUpdate((Widget) pw, &nr, False);
}

/*
**  Called when the parent widgets zoom factor changes
 */
static void 
zoomValueChanged(Widget w, XtPointer junk1, XtPointer junk2)
{
    PaintWidget pw = (PaintWidget) w;
    int zoom = GET_ZOOM(pw);
    int nx, ny;
    int nw, nh;

    if (zoom == pw->paint.region.curZoom)
	return;

    if (!pw->paint.region.isVisible)
	return;

    nx = (pw->paint.region.rect.x - pw->paint.zoomX) * zoom;
    ny = (pw->paint.region.rect.y - pw->paint.zoomY) * zoom;
    nw = (pw->paint.region.rect.width / pw->paint.region.curZoom) * zoom;
    pw->paint.region.rect.width = nw;
    nh = (pw->paint.region.rect.height / pw->paint.region.curZoom) * zoom;
    pw->paint.region.rect.height = nh;
    pw->paint.region.curZoom = zoom;

    XtMoveWidget(pw->paint.region.child, nx - BW, ny - BW);
    XtResizeWidget(pw->paint.region.child, nw, nh, BW);

    pw->paint.region.needResize = True;
    moveGrips(pw);
    regionRedraw(pw);
}

void 
pwRegionZoomPosChanged(PaintWidget pw)
{
    int nx, ny;
    int zoom;

    if (!pw->paint.region.isVisible)
	return;

    zoom = GET_ZOOM(pw);

    nx = (pw->paint.region.rect.x - pw->paint.zoomX) * zoom;
    ny = (pw->paint.region.rect.y - pw->paint.zoomY) * zoom;

    XtMoveWidget(pw->paint.region.child, nx - BW, ny - BW);
}


static void 
writeCleanRegion(PaintWidget pw, Boolean flag, Boolean write)
{
    if (!pw->paint.region.isVisible)
	return;

    if (write)
	writeRegion(pw);

    /*
    **  Free up temporary images
     */
    if (pw->paint.region.sourceImg != NULL)
	XDestroyImage(pw->paint.region.sourceImg);
    if (pw->paint.region.maskImg != NULL)
	XDestroyImage(pw->paint.region.maskImg);

    pw->paint.region.sourceImg = NULL;
    pw->paint.region.maskImg = NULL;
    if (pw->paint.region.source != None) {
	XFreePixmap(XtDisplay(pw), pw->paint.region.source);
	pw->paint.region.source = None;
    }
    if (pw->paint.region.mask != None) {
	XFreePixmap(XtDisplay(pw), pw->paint.region.mask);
	pw->paint.region.mask = None;
    }
    if (pw->paint.region.notMask != None) {
	XFreePixmap(XtDisplay(pw), pw->paint.region.notMask);
	pw->paint.region.notMask = None;
    }
    pw->paint.region.undoPixmap = None;

    if (flag) {
	if (pw->paint.region.child != None)
	    XtUnmapWidget(pw->paint.region.child);
	pw->paint.region.isVisible = False;
    }
}

/*  Turn off the selected region after writing it to the background.
 *  flag == True for all widgets
 */
void 
PwRegionFinish(Widget w, Boolean flag)
{
    PaintWidget pw = (PaintWidget) w;
    PaintWidget pp = (PaintWidget) pw->paint.paint;
    PaintWidget tpw;
    int i;

    if (flag) {
	tpw = (pp == None) ? pw : pp;
	writeCleanRegion(tpw, True, True);
	for (i = 0; i < tpw->paint.paintChildrenSize; i++)
	    writeCleanRegion((PaintWidget) tpw->paint.paintChildren[i], True, True);
	doCallbacks(pw, False);
    } else {
	writeCleanRegion(pw, True, True);
    }
}

/*
 * Turn off the selected region, but do not write it to the background.
 * Return False if no region, else True.
 *  flag == True for all widgets
 */
Boolean
PwRegionOff(Widget w, Boolean flag)
{
    PaintWidget pw = (PaintWidget) w;
    PaintWidget pp = (PaintWidget) pw->paint.paint;
    PaintWidget tpw;
    int i;

    tpw = pw;
    if (flag)
	tpw = (pp == None) ? pw : pp;

    if (!tpw->paint.region.isVisible)
	return False;

    writeCleanRegion(tpw, True, False);
    if (flag) {
	for (i = 0; i < tpw->paint.paintChildrenSize; i++)
	    writeCleanRegion((PaintWidget) tpw->paint.paintChildren[i], True, False);
	doCallbacks(pw, False);
    }
    return True;
}

/*  Set the region pixmap, and mask */
void 
PwRegionSet(Widget w, XRectangle * rect, Pixmap pix, Pixmap mask)
{
    PaintWidget pw = (PaintWidget) w;
    int i;
    int nx, ny, x, y, width, height;
    int zoom = GET_ZOOM(pw);
    Boolean setIsAttached = False;

    /*
    **  If there is an image, write it
    **     rect == NULL, then this is just a "write" & "unmap" request
     */
    PwRegionFinish(w, True);
    if (rect == NULL)
	return;

    pw->paint.region.curZoom = zoom;

    x = (rect->x + pw->paint.zoomX) * zoom;
    y = (rect->y + pw->paint.zoomY) * zoom;
    width = rect->width * zoom;
    height = rect->height * zoom;

    /*
    **  A little "initializing"
     */
    pw->paint.region.isDrawn = False;
    pw->paint.region.isTracking = False;
    pw->paint.region.needResize = False;

    pw->paint.region.rect = *rect;

    if (pix == None) {
	setIsAttached = True;

	if (rect->x < 0) {
	    rect->width += rect->x;
	    rect->x = 0;
	}
	if (rect->y < 0) {
	    rect->height += rect->y;
	    rect->y = 0;
	}
	if (rect->width > pw->paint.drawWidth)
	    rect->width = pw->paint.drawWidth;
	if (rect->height > pw->paint.drawHeight)
	    rect->height = pw->paint.drawHeight;

	pw->paint.region.source = XCreatePixmap(XtDisplay(pw), XtWindow(pw),
						rect->width, rect->height,
						pw->core.depth);
	XCopyArea(XtDisplay(pw), GET_PIXMAP(pw), pw->paint.region.source,
		  pw->paint.gc,
		  rect->x, rect->y,
		  rect->width, rect->height,
		  0, 0);
    } else {
	pw->paint.region.source = pix;
    }

    pw->paint.region.mask = mask;
    pw->paint.region.orig = *rect;

    /*
    **  If there is a clipping mask, create a fg and bg GC with clip-masks
    **    to draw through.
     */
    if (mask != None) {

	if (pw->paint.region.fg_gc == None) {
	    pw->paint.region.fg_gc = XCreateGC(XtDisplay(w), XtWindow(w), 0, 0);
	    pw->paint.region.bg_gc = XCreateGC(XtDisplay(w), XtWindow(w), 0, 0);
	}
	/*
	**  Make sure the Mask GC is built.
	 */
	GET_MGC(pw, mask);
	regionCreateNotMask(pw);
    } else {
	/*
	**  No clip mask, make sure we aren't using one.
	 */
	if (pw->paint.region.fg_gc != None) {
	    XSetClipMask(XtDisplay(w), pw->paint.region.fg_gc, None);
	    XSetClipMask(XtDisplay(w), pw->paint.region.bg_gc, None);
	}
    }

    if (pw->paint.region.child == None) {
	pw->paint.region.child = XtVaCreateWidget("region",
						  compositeWidgetClass, w,
						  XtNborderWidth, BW,
						  NULL);
	XtAddEventHandler(pw->paint.region.child, ButtonPressMask,
			  False,
			  (XtEventHandler) regionButtonPress,
			  (XtPointer) pw);
	XtAddEventHandler(pw->paint.region.child, ButtonReleaseMask,
			  False,
			  (XtEventHandler) regionButtonRelease,
			  (XtPointer) pw);
	XtAddEventHandler(pw->paint.region.child, ButtonMotionMask,
			  False,
			  (XtEventHandler) regionGrab,
			  (XtPointer) pw);
	XtAddEventHandler(pw->paint.region.child, ExposureMask,
			  False,
			  (XtEventHandler) regionExpose,
			  (XtPointer) pw);
	XtAddEventHandler(pw->paint.region.child, StructureNotifyMask,
			  False,
			  (XtEventHandler) regionMove,
			  (XtPointer) pw);
	XtAddCallback((Widget) pw, XtNsizeChanged,
		    (XtCallbackProc) zoomValueChanged, (XtPointer) NULL);
	XtVaSetValues(pw->paint.region.child, XtNx, x, XtNy, y,
		      XtNwidth, width, XtNheight, height, NULL);
	XtManageChild(pw->paint.region.child);
	XDefineCursor(XtDisplay(w), XtWindow(pw->paint.region.child),
		      XCreateFontCursor(XtDisplay(w), XC_fleur));

	for (i = 0; i < 9; i++) {
	    if (i == 4)
		continue;

	    pw->paint.region.grip[i] =
		XtVaCreateManagedWidget("grip",
				 gripWidgetClass, pw->paint.region.child,
					XtNwidth, 6, XtNheight, 6, NULL);

	    XtAddEventHandler(pw->paint.region.grip[i], ButtonPressMask,
			      False,
			      (XtEventHandler) gripPress,
			      (XtPointer) pw);
	    XtAddEventHandler(pw->paint.region.grip[i], ButtonMotionMask,
			      False,
			      (XtEventHandler) gripGrab,
			      (XtPointer) pw);
	    XtAddEventHandler(pw->paint.region.grip[i], ButtonReleaseMask,
			      False,
			      (XtEventHandler) gripRelease,
			      (XtPointer) pw);
	}
	regionSetGripCursors(pw);
	pw->paint.region.isVisible = True;
    } else {
	XClearArea(XtDisplay(pw), XtWindow(pw->paint.region.child),
		   0, 0, 0, 0, True);
    }

    if (setIsAttached) {
	nx = rect->x;
	ny = rect->y;
    } else {
	nx = pw->paint.downX;
	ny = pw->paint.downY;
    }
    pw->paint.region.rect.x = nx;
    pw->paint.region.rect.y = ny;
    nx -= pw->paint.zoomX;
    ny -= pw->paint.zoomY;
    XtVaSetValues(pw->paint.region.child, XtNx, nx * zoom - BW,
		  XtNy, ny * zoom - BW,
		  XtNwidth, width,
		  XtNheight, height,
		  NULL);

    pw->paint.region.scaleX = 1.0;
    pw->paint.region.scaleY = 1.0;
    pw->paint.region.centerX = pw->paint.region.orig.width / 2;
    pw->paint.region.centerY = pw->paint.region.orig.height / 2;
    COPY_MAT(matIdentity, pw->paint.region.rotMat);
    MKMAT(pw);

    if (zoom != 1) {
	pw->paint.region.rect.width *= zoom;
	pw->paint.region.rect.height *= zoom;
    }
    moveGrips(pw);

    if (!pw->paint.region.isVisible) {
	XtMapWidget(pw->paint.region.child);
	pw->paint.region.isVisible = True;
    }
#ifdef SHAPE
    XShapeCombineMask(XtDisplay(pw), XtWindow(pw->paint.region.child),
		      ShapeBounding, 0, 0, None, ShapeSet);
#endif

    pw->paint.region.isAttached = setIsAttached;
    doCallbacks(pw, True);
}

static PaintWidget
getActiveRegion(PaintWidget pw)
{
    PaintWidget pp = (PaintWidget) pw->paint.paint;
    PaintWidget tpw = (pp == None) ? pw : pp;
    int i;

    if (pw->paint.region.isVisible && pw->paint.region.source != None)
	return pw;

    if (tpw->paint.region.isVisible && tpw->paint.region.source != None)
	return tpw;

    for (i = 0; i < tpw->paint.paintChildrenSize; i++) {
	PaintWidget p = (PaintWidget) tpw->paint.paintChildren[i];
	if (p->paint.region.source != None)
	    return p;
    }

    return None;
}

/*  Set the foreground pixmap, changing it in place */
void 
PwRegionSetRawPixmap(Widget w, Pixmap pix)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);

    if (pw == None)
	return;

    XFreePixmap(XtDisplay(pw), pw->paint.region.source);

    if (pw->paint.region.sourceImg != NULL) {
	XDestroyImage(pw->paint.region.sourceImg);
	pw->paint.region.sourceImg = NULL;
    }
    pw->paint.region.source = pix;

    doResize(pw);
    regionRedraw(pw);
}

/*  Get a copy of the current image & mask, True if exist */
Boolean
PwRegionGet(Widget w, Pixmap * pix, Pixmap * mask)
{
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    PaintWidget pw = getActiveRegion((PaintWidget) w);
    Pixmap myMask = None, notMask = None;
    int zoom;
    int width, height;

    if (pw == None)
	return False;
    zoom = GET_ZOOM(pw);
    width = pw->paint.region.orig.width;
    height = pw->paint.region.orig.height;

    if (pix)
	*pix = None;
    if (mask)
	*mask = None;

    if (pw->paint.region.source != None && pix != NULL) {
	*pix = XCreatePixmap(dpy, win, width, height, pw->core.depth);
	if (pw->paint.region.sourceImg != NULL) {
	    XPutImage(dpy, *pix, pw->paint.tgc,
		      pw->paint.region.sourceImg,
		      0, 0, 0, 0, width, height);
	} else {
	    XCopyArea(dpy, pw->paint.region.source,
		      *pix, pw->paint.tgc,
		      0, 0, width, height, 0, 0);
	}
    }
    if (pw->paint.region.mask != None) {
	myMask = XCreatePixmap(dpy, win, width, height, 1);
	notMask = XCreatePixmap(dpy, win, width, height, 1);

	if (pw->paint.region.maskImg != NULL) {
	    XPutImage(dpy, myMask, pw->paint.mgc,
		      pw->paint.region.maskImg,
		      0, 0, 0, 0, width, height);
	} else {
	    XCopyArea(dpy, pw->paint.region.mask,
		      myMask, pw->paint.mgc,
		      0, 0, width, height, 0, 0);
	}

	XSetFunction(dpy, pw->paint.mgc, GXcopyInverted);
	XCopyArea(dpy, myMask, notMask, pw->paint.mgc, 0, 0,
		  width, height, 0, 0);
	XSetFunction(dpy, pw->paint.mgc, GXcopy);

	if (mask == NULL)
	    XFreePixmap(dpy, myMask);
	else
	    *mask = myMask;
    }
    if (notMask != None && pix != NULL) {
	XSetClipOrigin(dpy, pw->paint.igc, 0, 0);
	XSetClipMask(XtDisplay(pw), pw->paint.igc, notMask);
	XFillRectangle(XtDisplay(pw), *pix, pw->paint.igc, 0, 0,
		       width, height);
	XSetClipMask(XtDisplay(pw), pw->paint.igc, None);

    }
    if (notMask != None)
	XFreePixmap(dpy, notMask);

    return True;
}

/*  Clear the region to the current background color */
void 
PwRegionClear(Widget w)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);

    if (pw == None)
	return;

    PwRegionTear(w);

    pw->paint.region.isVisible = False;
    if (pw->paint.region.child != None)
	XtUnmapWidget(pw->paint.region.child);
    doCallbacks(pw, False);
}

/*
**  Make sure that the current region is not connected to the
**    source pixmap.
 */
void 
PwRegionTear(Widget w)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);
    XRectangle nr;

    if (pw == None)
	return;
    if (!pw->paint.region.isAttached || !pw->paint.region.isVisible)
	return;

    nr = pw->paint.region.rect;
    nr.width /= GET_ZOOM(pw);
    nr.height /= GET_ZOOM(pw);

    pw->paint.region.undoPixmap = PwUndoStart((Widget) pw, &nr);

    if (pw->paint.region.mask != None) {
	XSetClipOrigin(XtDisplay(pw), pw->paint.igc, nr.x, nr.y);
	XSetClipMask(XtDisplay(pw), pw->paint.igc, pw->paint.region.mask);
    }
    XFillRectangles(XtDisplay(pw), pw->paint.region.undoPixmap,
		    pw->paint.igc, &nr, 1);
    PwUpdate((Widget) pw, &nr, False);
    if (pw->paint.region.mask != None) {
	XSetClipOrigin(XtDisplay(pw), pw->paint.igc, 0, 0);
	XSetClipMask(XtDisplay(pw), pw->paint.igc, None);
    }
    pw->paint.region.isAttached = False;
}

/*  Append a transformation matrix to the current transform */
void 
PwRegionAppendMatrix(Widget w, pwMatrix mat)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);

    if (pw == None)
	return;

    PwRegionTear((Widget) pw);

    mm(pw->paint.region.rotMat, mat, pw->paint.region.rotMat);
    MKMAT(pw);

    pw->paint.region.needResize = True;
    regionResizeWindow(pw, False);
    regionRedraw(pw);
}

/*  Set the current transformation matrix */
void 
PwRegionSetMatrix(Widget w, pwMatrix mat)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);

    if (pw == None)
	return;

    PwRegionTear((Widget) pw);

    COPY_MAT(mat, pw->paint.region.rotMat);
    MKMAT(pw);

    pw->paint.region.needResize = True;
    regionResizeWindow(pw, False);
    regionRedraw(pw);
}

/* Append the current values to the scale */
void 
PwRegionAddScale(Widget w, float *xs, float *ys)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);

    if (pw == None)
	return;

    PwRegionTear((Widget) pw);

    if (xs != NULL)
	pw->paint.region.scaleX *= *xs;
    if (ys != NULL)
	pw->paint.region.scaleY *= *ys;

    MKMAT(pw);

    pw->paint.region.needResize = True;
    regionResizeWindow(pw, False);
    regionRedraw(pw);
}

/* Set the current X & Y scale values */
void 
PwRegionSetScale(Widget w, float *xs, float *ys)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);

    if (pw == None)
	return;

    if (xs != NULL)
	pw->paint.region.scaleX = *xs;
    if (ys != NULL)
	pw->paint.region.scaleY = *ys;

    MKMAT(pw);

    pw->paint.region.needResize = True;
    regionResizeWindow(pw, False);
    regionRedraw(pw);
}

/* Reset both the rotation and scale back to identity */
void 
PwRegionReset(Widget w, Boolean flag)
{
    PaintWidget pw = getActiveRegion((PaintWidget) w);
    pwMatrix mat;

    if (pw == None)
	return;

    PwRegionTear((Widget) pw);

    mat[0][0] = mat[1][1] = 1;
    mat[1][0] = mat[0][1] = 0;

    COPY_MAT(mat, pw->paint.region.rotMat);
    pw->paint.region.scaleY = 1.0;
    pw->paint.region.scaleX = 1.0;
    MKMAT(pw);

    pw->paint.region.needResize = True;
    regionResizeWindow(pw, False);
    regionRedraw(pw);

    /* XXX flag should reset X & Y position as well */
}

/*
 * Crop to region: replaces the image with the region.
 */
void 
RegionCrop(PaintWidget paint)
{
    PaintWidget pw = getActiveRegion(paint);
    Pixmap pix;


    StateSetBusy(True);

    if (!PwRegionGet((Widget) paint, &pix, None)) {
	StateSetBusy(False);
	return;			/* No region selected */
    }
    pw->paint.dirty = True;

    /* Make the region inactive */
    PwRegionFinish((Widget) paint, True);

    XtVaSetValues((Widget) paint,
		  XtNpixmap, pix,
		  XtNdrawWidth, pw->paint.region.orig.width,
		  XtNdrawHeight, pw->paint.region.orig.height,
		  NULL);

    PwUpdateDrawable((Widget) paint, XtWindow(paint), NULL);
    StateSetBusy(False);
}

void 
RegionMove(PaintWidget pw, int dx, int dy)
{
    int nx, ny;
    int zoom = GET_ZOOM(pw);

    if (getActiveRegion(pw) == None)
	return;
    PwRegionTear((Widget) pw);

    pw->paint.region.rect.x += dx;
    pw->paint.region.rect.y += dy;
    nx = (pw->paint.region.rect.x - pw->paint.zoomX) * zoom;
    ny = (pw->paint.region.rect.y - pw->paint.zoomY) * zoom;

    XtMoveWidget(pw->paint.region.child, nx - BW, ny - BW);

    drawRegionBox(pw, False);
}
