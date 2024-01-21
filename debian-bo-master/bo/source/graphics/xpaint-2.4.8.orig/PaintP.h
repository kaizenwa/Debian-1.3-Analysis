#ifndef _PaintP_h
#define _PaintP_h

/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)	       | */
/* | Copyright 1995, 1996 Torsten Martinsen (bullestock@dk-online.dk)  | */
/* |								       | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.	 There is no	       | */
/* | representations about the suitability of this software for	       | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.					       | */
/* |								       | */
/* +-------------------------------------------------------------------+ */

/* $Id: PaintP.h,v 1.8 1996/04/19 09:03:59 torsten Exp $ */

#include "Paint.h"

#include <X11/IntrinsicP.h>	/* necessary for CoreP.h */
/* include superclass private header file */
#include <X11/CoreP.h>
#include <X11/CompositeP.h>
#include <X11/Xutil.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRPaintResource "PaintResource"

typedef struct {
    int empty;
} PaintClassPart;

typedef struct _PaintClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    PaintClassPart paint_class;
} PaintClassRec;

extern PaintClassRec paintClassRec;

typedef struct s_undoStack {
    XRectangle box;
    Pixmap pixmap;
    Boolean pushed;
    struct s_undoStack *next;
} UndoStack;

typedef struct {
    int drawWidth, drawHeight;
    int fillRule, lineFillRule;
    GC gc, igc, fgc, sgc;
    Pixmap sourcePixmap;
    Pixmap pattern, linePattern;
    int lineWidth;
    XFontStruct *fontInfo;
    Pixel foreground, background, lineForeground;
    int undoSize;		/* number of undo stacks */

    /*
    **	The undo system
     */
    Pixmap current;		/* The currently displayed pixmap */
    UndoStack * undo, * head;
    Boolean dirty;
    /* The undo/redo stacks: */
    int nlevels;		/* Maximum stack depth */
    Pixmap * undostack, * redostack;
    int undobot, undoitems, redobot, redoitems;

    Boolean snapOn, grid;
    int snap;
    void *eventList;
    XtCallbackList fatcalls;
    XtCallbackList sizecalls;
    XtCallbackList regionCalls;
    int zoomX, zoomY;
    int zoom;
    GC tgc;			/* scratch GC, for stuffing clip masks into */
    GC mgc;			/* region mask GC, 1 bit deep */
    GC xgc;			/* XOR (invert) GC */
    Widget paint;
    int paintChildrenSize;
    Widget *paintChildren;
    Pixel linePixel;
    Boolean compress;
    Cursor cursor;

    /*
    **	Region info
     */
    struct {
	Widget child;
	Widget grip[9];
	Position offX, offY;
	Position baseX, baseY;
	XRectangle rect, orig;
	GC fg_gc, bg_gc;

	Boolean isTracking, isDrawn, isRotate;
	float lastAngle;
	Position lastX, lastY;
	int fixedPoint;
	float lineDelta[4], lineBase[2];
	float startScaleX, startScaleY;

	Pixmap source, mask, notMask;
	XImage *sourceImg, *maskImg, *notMaskImg;

	Boolean isAttached, isVisible, needResize;
	Pixmap undoPixmap;
	/*
	**  The scale{X,Y} are combined with the rotMat
	**    to produce mat.
	 */
	float centerX, centerY;
	float scaleX, scaleY;
	pwMatrix rotMat, mat;

	int curZoom;
	pwRegionDoneProc *proc;
    } region;

    Boolean invalidateRegion;
    Region imageRegion;
    XImage *image;

    Position downX, downY;	/* Last button down X & Y pos */

    /*
    **	Taken from our parent shell
     */
    Visual *visual;

    /*
     * Used by the revert function
     */
    char *filename;

    WidgetList menuwidgets;
} PaintPart;

#define PwPixmap(w)	((w)->paint.active->pixmap)

typedef struct _PaintRec {
    CorePart core;
    CompositePart composite;
    PaintPart paint;
} PaintRec;


#define _GET_PIXMAP(pw) ((pw)->paint.current)
#define GET_PIXMAP(pw) (pw->paint.paint ? \
		_GET_PIXMAP((PaintWidget)pw->paint.paint) : _GET_PIXMAP(pw))

#define _GET_ZOOM(pw)	pw->paint.zoom
#define GET_ZOOM(pw) ((pw->paint.zoom == PwZoomParent) ? \
		_GET_ZOOM(((PaintWidget)pw->paint.paint)) : _GET_ZOOM(pw))

#define GET_MGC(pw, msk)	((pw)->paint.mgc == None ? \
	(pw->paint.mgc = XCreateGC(XtDisplay(pw), msk, 0, 0)) : pw->paint.mgc)

void pwRegionZoomPosChanged(PaintWidget);
void _PwZoomDraw(PaintWidget, Widget, GC, XImage *, XImage *,
		 Boolean, int, int, int, XRectangle *);

/*
**  Try to optimize XGetPixel and XPutPixel
**
 */
#define ZINDEX8(x, y, img)  ((y) * img->bytes_per_line) + (x)
#define xxGetPixel(img, x, y)					\
		((img->bits_per_pixel == 8) ?			\
			img->data[ZINDEX8(x, y, img)] :		\
			XGetPixel(img, x, y))
#define xxPutPixel(img, x, y, p) do {				\
		if (img->bits_per_pixel == 8)			\
			img->data[ZINDEX8(x, y, img)] = p;	\
		else						\
			XPutPixel(img, x, y, p);		\
	} while (0)

#endif				/* _PaintP_h */
