
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

/* $Id: polyOp.c,v 1.3 1996/04/19 08:51:50 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include "xpaint.h"
#include "misc.h"
#include "Paint.h"
#include "ops.h"

#define	FILL		0x2
#define	POLY		0x1
#define IsPoly(x)	(x & POLY)
#define	IsFill(x)	(x & FILL)

#define MAXP	100

typedef struct {
    int flag;
    int startX, startY, endX, endY, button;
    int drawn, first, tracking, go;
    int npoints;
    XPoint points[MAXP + 1];
    XPoint real[MAXP + 1];
    /*
    **  Borrowed from my info structure.
     */
    GC fgc, lgc, gcx;
    Pixmap pixmap;
    Widget widget;
    Boolean isFat;
} LocalInfo;

static void 
finish(Widget w, LocalInfo * l, Boolean flag)
{
    if (!l->tracking)
	return;

    l->tracking = False;

    if (l->drawn)
	XDrawLines(XtDisplay(w), XtWindow(w), l->gcx,
		   l->points, l->npoints, CoordModeOrigin);
    if (flag && l->drawn)
	XDrawLine(XtDisplay(w), XtWindow(w), l->gcx,
		  l->points[l->npoints - 1].x,
		  l->points[l->npoints - 1].y,
		  l->endX, l->endY);

    if (IsFill(l->flag)) {
	if (!l->isFat)
	    XFillPolygon(XtDisplay(w), XtWindow(w), l->fgc,
			 l->real, l->npoints, Complex, CoordModeOrigin);
	XFillPolygon(XtDisplay(w), l->pixmap, l->fgc,
		     l->real, l->npoints, Complex, CoordModeOrigin);
    }
    if (IsPoly(l->flag)) {
	l->real[l->npoints].x = l->real[0].x;
	l->real[l->npoints].y = l->real[0].y;
	l->npoints++;
    }
    if (!l->isFat)
	XDrawLines(XtDisplay(w), XtWindow(w), l->lgc,
		   l->real, l->npoints, CoordModeOrigin);
    XDrawLines(XtDisplay(w), l->pixmap, l->lgc,
	       l->real, l->npoints, CoordModeOrigin);

    PwUpdate(w, NULL, False);
}

static void 
press(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    if (!l->tracking && event->button == Button1) {
	l->endX = l->startX = event->x;
	l->endY = l->startY = event->y;

	l->button = event->button;

	l->npoints = 0;

	l->points[l->npoints].x = event->x;
	l->points[l->npoints].y = event->y;
	l->real[l->npoints].x = info->x;
	l->real[l->npoints].y = info->y;
	l->npoints++;

	l->drawn = False;
	l->tracking = True;
	l->first = True;

	l->isFat = info->isFat;
	l->fgc = info->second_gc;
	l->lgc = info->first_gc;
    } else if (event->button == Button2 && l->tracking) {
	finish(w, l, True);
	return;
    }
    if (l->first && info->surface == opPixmap) {
	UndoStartPoint(w, info, info->x, info->y);
	l->pixmap = info->drawable;
	l->widget = w;
    }
}

static void 
release(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    if (l->npoints == MAXP - 1)
	return;

    if (info->surface == opWindow) {
	if (!l->first) {
	    l->endX = l->points[l->npoints].x = event->x;
	    l->endY = l->points[l->npoints].y = event->y;
	    l->startX = event->x;
	    l->startY = event->y;
	}
	return;
    }
    if (l->first) {
	l->first = False;
	return;
    }
    /*
    **  else on the pixmap.
     */
    l->real[l->npoints].x = info->x;
    l->real[l->npoints].y = info->y;
    l->npoints++;

    UndoGrow(w, info->x, info->y);
}

static void 
motion(Widget w, LocalInfo * l, XMotionEvent * event, OpInfo * info)
{
    /*
    **  Haven't done the first button press
     */
    if (!l->tracking || l->first)
	return;

    if (l->drawn)
	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);

    l->endX = event->x;
    l->endY = event->y;

    if ((l->drawn = (l->startX != l->endX && l->startY != l->endY)))
	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);
}


/*
**  Those public functions
 */
void *
PolyAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->flag = POLY;
    l->drawn = False;
    l->first = True;
    l->tracking = False;
    l->gcx = GetGCX(w);

    XtVaSetValues(w, XtNcompress, True, NULL);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opWindow, PointerMotionMask, FALSE,
		      (OpEventProc) motion, l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
		      (OpEventProc) release, l);
    SetCrossHairCursor(w);

    return l;
}

void 
PolyRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, PointerMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);

    finish(w, (LocalInfo *) l, True);

    XtFree((XtPointer) l);
}

void *
FPolyAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->flag = POLY | FILL;
    l->drawn = False;
    l->first = True;
    l->tracking = False;
    l->gcx = GetGCX(w);

    XtVaSetValues(w, XtNcompress, True, NULL);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opWindow, PointerMotionMask, FALSE,
		      (OpEventProc) motion, l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
		      (OpEventProc) release, l);
    SetCrossHairCursor(w);

    return l;
}

void 
FPolyRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, PointerMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);

    finish(w, (LocalInfo *) l, True);
    XtFree((XtPointer) l);
}

void *
CLineAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->flag = 0;
    l->drawn = False;
    l->first = True;
    l->tracking = False;
    l->gcx = GetGCX(w);

    XtVaSetValues(w, XtNcompress, True, NULL);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opWindow, PointerMotionMask, FALSE,
		      (OpEventProc) motion, l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
		      (OpEventProc) release, l);
    SetCrossHairCursor(w);

    return l;
}

void 
CLineRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, PointerMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);

    finish(w, (LocalInfo *) l, True);
    XtFree((XtPointer) l);
}
