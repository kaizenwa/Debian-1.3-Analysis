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

/* $Id: boxOp.c,v 1.3 1996/04/19 09:05:53 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "xpaint.h"
#include "misc.h"
#include "Paint.h"
#include "ops.h"

typedef struct {
    int rx, ry;
    int startX, startY, endX, endY;
    int drawn;
    Boolean type;		/* 0: rectangle, 1: square,
				   2: centered rectangle, 3: centered square */
    Boolean fill;		/* Filled rectangle? */
    Boolean swap;
    GC gcx;
} LocalInfo;

#define MKRECT(rect, sx, sy, ex, ey, typeFlag) do {			\
	if (typeFlag == 1) {	/* square */				\
		(rect)->width  = MAX(ABS(sx - ex),ABS(sy - ey));	\
		(rect)->height = (rect)->width;				\
		(rect)->x = (ex - sx < 0) ? sx - (rect)->width : sx;	\
		(rect)->y = (ey - sy < 0) ? sy - (rect)->height : sy;	\
	} else if (typeFlag == 2) {	/* center */			\
		(rect)->x      = sx - ABS(sx - ex);			\
		(rect)->y      = sy - ABS(sy - ey);			\
		(rect)->width  = 2*ABS(sx - ex);			\
		(rect)->height = 2*ABS(sy - ey);			\
	} else if (typeFlag == 3) {	/* center, square */		\
		(rect)->width  = MAX(ABS(sx - ex),ABS(sy - ey));	\
		(rect)->x      = sx - (rect)->width;			\
		(rect)->y      = sy - (rect)->width;			\
		(rect)->width  *= 2;					\
		(rect)->height = (rect)->width;				\
	} else {		/* no constraints */			\
		(rect)->x      = MIN(sx, ex);				\
		(rect)->y      = MIN(sy, ey);				\
		(rect)->width  = MAX(sx, ex) - (rect)->x;		\
		(rect)->height = MAX(sy, ey) - (rect)->y;		\
	}								\
} while (0)

static int boxStyle = 0;

static void 
press(Widget w, LocalInfo * l,
      XButtonEvent * event, OpInfo * info)
{
    /*
    **	Check to make sure all buttons are up, before doing this
     */
    if ((event->state & AllButtonsMask) != 0)
	return;

    l->swap = (event->button == Button2);

    l->rx = info->x;
    l->ry = info->y;
    l->endX = l->startX = event->x;
    l->endY = l->startY = event->y;

    l->type = 2 * boxStyle + ((event->state & ShiftMask) ? 1 : 0);
    l->drawn = False;
}

static void 
motion(Widget w, LocalInfo * l,
       XMotionEvent * event, OpInfo * info)
{
    XRectangle rect;

    if (l->drawn) {
	MKRECT(&rect, l->startX, l->startY, l->endX, l->endY, l->type);

	XDrawRectangles(XtDisplay(w), XtWindow(w), l->gcx, &rect, 1);
    }
    l->endX = event->x;
    l->endY = event->y;

    l->type = 2 * boxStyle + ((event->state & ShiftMask) ? 1 : 0);

    if ((l->drawn = (l->startX != l->endX || l->startY != l->endY))) {
	MKRECT(&rect, l->startX, l->startY, l->endX, l->endY, l->type);

	XDrawRectangles(XtDisplay(w), XtWindow(w), l->gcx, &rect, 1);
    }
}

static void 
release(Widget w, LocalInfo * l,
	XButtonEvent * event, OpInfo * info)
{
    XRectangle rect;
    int mask;
    GC fgc, lgc;

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
	MKRECT(&rect, l->startX, l->startY, l->endX, l->endY, l->type);

	XDrawRectangles(XtDisplay(w), XtWindow(w), l->gcx, &rect, 1);
    }
    if (info->isFat && info->surface == opWindow)
	return;

    MKRECT(&rect, l->rx, l->ry, event->x, event->y, l->type);

    UndoStart(w, info);

    if (l->swap) {
	fgc = info->first_gc;
	lgc = info->second_gc;
    } else {
	fgc = info->second_gc;
	lgc = info->first_gc;
    }

    if (l->fill)
	XFillRectangles(XtDisplay(w), info->drawable, fgc, &rect, 1);
    XDrawRectangles(XtDisplay(w), info->drawable, lgc, &rect, 1);

    if (info->surface == opPixmap) {
	rect.width++;
	rect.height++;
	UndoSetRectangle(w, &rect);
	PwUpdate(w, &rect, False);
    }
}

/*
**  Public routines
 */

void *
BoxAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->fill = False;
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
BoxRemove(Widget w, void *l)
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
FBoxAdd(Widget w)
{
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    l->fill = True;
    l->gcx = GetGCX(w);

    XtVaSetValues(w, XtNcompress, False, NULL);

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
FBoxRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow, ButtonPressMask, FALSE,
			 (OpEventProc) press, l);
    OpRemoveEventHandler(w, opWindow, ButtonMotionMask, FALSE,
			 (OpEventProc) motion, l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask, FALSE,
			 (OpEventProc) release, l);

    XtFree((XtPointer) l);
}

void 
BoxSetStyle(Boolean mode)
{
    boxStyle = mode ? 1 : 0;
}

Boolean
BoxGetStyle(void)
{
    return boxStyle == 0 ? False : True;
}
