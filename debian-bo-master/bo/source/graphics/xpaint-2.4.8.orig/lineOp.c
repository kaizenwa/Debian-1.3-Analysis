/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)	       | */
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

/* $Id: lineOp.c,v 1.3 1996/04/19 09:09:01 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <math.h>
#include "xpaint.h"
#include "misc.h"
#include "Paint.h"
#include "ops.h"

typedef struct {
    int isRay;
    int type;
    Boolean didUndo, didSet, tracking;
    int rx, ry;
    int startX, startY, endX, endY;
    int drawn;
    Drawable drawable;
    GC gcx;
} LocalInfo;

static void 
press(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    /*
    **	Check to make sure all buttons are up, before doing this
     */
    if ((event->state & AllButtonsMask) != 0)
	return;

    if (event->button == Button1 && ((!l->tracking && l->isRay) || !l->isRay)) {
	l->rx = info->x;
	l->ry = info->y;
	l->startX = event->x;
	l->startY = event->y;

	l->drawable = info->drawable;
	l->drawn = False;
	l->didUndo = False;
	l->didSet = False;
	l->tracking = True;
    } else if (event->button == Button2) {
	if (l->drawn)
	    XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		      l->startX, l->startY, l->endX, l->endY);
	l->tracking = False;
	return;
    }
    if (l->tracking) {
	if (l->drawn)
	    XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		      l->startX, l->startY, l->endX, l->endY);

	l->endX = event->x;
	l->endY = event->y;

	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);
	l->drawn = True;
    }
}

static void 
motion(Widget w, LocalInfo * l, XMotionEvent * event, OpInfo * info)
{
    if (!l->tracking)
	return;

    if (l->drawn) {
	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);
    }
    l->type = (event->state & ShiftMask);

    if (l->type) {
	int ax = ABS(event->x - l->startX);
	int ay = ABS(event->y - l->startY);
	int dx = MIN(ax, ay);
	int v, v1 = dx - ax, v2 = dx - ay;
	int addX, addY;

	v = v1 * v1 + v2 * v2;

	if (ay * ay < v) {
	    addX = event->x - l->startX;
	    addY = 0;
	} else if (ax * ax < v) {
	    addX = 0;
	    addY = event->y - l->startY;
	} else {
	    if (ax < ay) {
		addX = event->x - l->startX;
		addY = SIGN(event->y - l->startY) * ax;
	    } else {
		addX = SIGN(event->x - l->startX) * ay;
		addY = event->y - l->startY;
	    }
	}

	l->endX = l->startX + addX;
	l->endY = l->startY + addY;
    } else {
	l->endX = event->x;
	l->endY = event->y;
    }

    /*
    **	Really set this flag in the if statement
     */
    if ((l->drawn = (l->startX != l->endX || l->startY != l->endY))) {
	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);
    }
}

static void 
release(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    XRectangle undo;
    int mask;

    if (!l->tracking)
	return;

    /*
    **	Check to make sure all buttons are up, before doing this
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

    if (l->drawn && info->surface == opWindow) {
	XDrawLine(XtDisplay(w), info->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);
	l->drawn = False;
    }
    if (info->surface == opWindow && info->isFat)
	return;

    if (!l->didUndo && info->surface == opPixmap) {
	UndoStart(w, info);
	l->didUndo = True;
    }
    if (l->type) {
	int ax = ABS(event->x - l->rx);
	int ay = ABS(event->y - l->ry);
	int dx = MIN(ax, ay);
	int v, v1 = dx - ax, v2 = dx - ay;
	int addX, addY;

	v = v1 * v1 + v2 * v2;

	if (ay * ay < v) {
	    addX = event->x - l->rx;
	    addY = 0;
	} else if (ax * ax < v) {
	    addX = 0;
	    addY = event->y - l->ry;
	} else {
	    if (ax < ay) {
		addX = event->x - l->rx;
		addY = SIGN(event->y - l->ry) * ax;
	    } else {
		addX = SIGN(event->x - l->rx) * ay;
		addY = event->y - l->ry;
	    }
	}

	l->endX = l->rx + addX;
	l->endY = l->ry + addY;
    } else {
	l->endX = event->x;
	l->endY = event->y;
    }

    XDrawLine(XtDisplay(w), info->drawable, info->first_gc,
	      l->rx, l->ry, l->endX, l->endY);

    if (info->surface == opPixmap) {
	XYtoRECT(l->rx, l->ry, l->endX, l->endY, &undo);

	if (l->didSet) {
	    UndoGrow(w, l->endY, l->endY);
	} else {
	    UndoSetRectangle(w, &undo);
	    l->didSet = True;
	}

	PwUpdate(w, &undo, False);
    }
}

/*
**  Those public functions
 */
void *
LineAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->isRay = False;
    l->tracking = False;
    l->gcx = GetGCX(w);
    XtVaSetValues(w, XtNcompress, True, NULL);

    OpAddEventHandler(w, opWindow, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opWindow, ButtonMotionMask, FALSE,
		      (OpEventProc) motion, l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
		      (OpEventProc) release, l);
    SetCrossHairCursor(w);
    return l;
}

void 
LineRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, ButtonMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);
    XtFree((XtPointer) l);
}

void *
RayAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->isRay = True;
    l->tracking = False;
    l->gcx = GetGCX(w);
    XtVaSetValues(w, XtNcompress, True, NULL);

    OpAddEventHandler(w, opWindow, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opWindow, PointerMotionMask, FALSE,
		      (OpEventProc) motion, l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
		      (OpEventProc) release, l);
    SetCrossHairCursor(w);
    return l;
}

void 
RayRemove(Widget w, void *p)
{
    LocalInfo *l = (LocalInfo *) p;

    OpRemoveEventHandler(w, opWindow, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, PointerMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);

    if (l->tracking && l->drawn)
	XDrawLine(XtDisplay(w), l->drawable, l->gcx,
		  l->startX, l->startY, l->endX, l->endY);

    XtFree((XtPointer) l);
}
