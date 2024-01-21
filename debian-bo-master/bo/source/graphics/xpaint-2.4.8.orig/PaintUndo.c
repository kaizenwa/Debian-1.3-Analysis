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
/* | any purpose.  This software is provided "as is" without express   | */
/* | or implied warranty.                                              | */
/* |                                                                   | */
/* +-------------------------------------------------------------------+ */

/* $Id: PaintUndo.c,v 1.8 1996/05/08 08:38:31 torsten Exp $ */

#include <X11/IntrinsicP.h>
#include <stdlib.h>
#include "xpaint.h"
#include "misc.h"
#include "PaintP.h"

#define	GET_PW(w)	((PaintWidget)(((PaintWidget)w)->paint.paint == None ? \
				       w : ((PaintWidget)w)->paint.paint))

static void RedoClear(PaintWidget pw);
static void UndoPush(PaintWidget pw, Pixmap p);
static Pixmap UndoPop(PaintWidget pw);
static void RedoPush(PaintWidget pw, Pixmap p);
static Pixmap RedoPop(PaintWidget pw);

void
UndoInitialize(PaintWidget pw, int n)
{
    pw->paint.nlevels = n;
    pw->paint.undostack = n ? malloc(n*sizeof(Pixmap)) : NULL;
    pw->paint.redostack = n ? malloc(n*sizeof(Pixmap)) : NULL;
    pw->paint.undobot = pw->paint.undoitems = 0;
    pw->paint.redobot = pw->paint.redoitems = 0;
}

static void
RedoClear(PaintWidget pw)
{
    Pixmap p;
    
    while ((p = RedoPop(pw)) != None)
	;
}

static void
UndoPush(PaintWidget pw, Pixmap p)
{
    UndoStack * s, * h;

    if (pw->paint.nlevels == 0)
	return;
    
    h = pw->paint.head;
    s = h;
    do {
	if (s->pixmap == p) {
	    s->pushed = True;
	    pw->paint.undo = s;
	    break;
	}
	s = s->next;
    } while (s != h);
    if (pw->paint.undoitems < pw->paint.nlevels)
	pw->paint.undostack[pw->paint.undoitems++] = p;
    else {
	int bot = pw->paint.undobot;
	Pixmap bp;

	bp = pw->paint.undostack[bot];
	s = h;
	do {
	    if (s->pixmap == bp) {
		s->pushed = False;
		break;
	    }
	    s = s->next;
	} while (s != h);
	pw->paint.undostack[bot++] = p;
	pw->paint.undobot = bot % pw->paint.nlevels;
    }
}

static Pixmap
UndoPop(PaintWidget pw)
{
    Pixmap p;
    UndoStack * s, * h;
    
    if (pw->paint.undoitems == 0)
        return None;
    --pw->paint.undoitems;
    p = pw->paint.undostack[(pw->paint.undobot + pw->paint.undoitems)
			   % pw->paint.nlevels];
    h = pw->paint.head;
    s = h;
    do {
	if (s->pixmap == p) {
	    s->pushed = False;
	    break;
	}
	s = s->next;
    } while (s != h);
    return p;
}

static void
RedoPush(PaintWidget pw, Pixmap p)
{
    UndoStack * s, * h;

    if (pw->paint.nlevels == 0)
	return;
    h = pw->paint.head;
    s = h;
    do {
	if (s->pixmap == p) {
	    s->pushed = True;
	    break;
	}
	s = s->next;
    } while (s != h);
    if (pw->paint.redoitems < pw->paint.nlevels)
	pw->paint.redostack[pw->paint.redoitems++] = p;
    else {
	int bot = pw->paint.redobot;
	Pixmap bp;

	bp = pw->paint.redostack[bot];
	s = h;
	do {
	    if (s->pixmap == bp) {
		s->pushed = False;
		break;
	    }
	    s = s->next;
	} while (s != h);
	
	pw->paint.redostack[bot++] = p;
	pw->paint.redobot = bot % pw->paint.nlevels;
    }
}

static Pixmap
RedoPop(PaintWidget pw)
{
    Pixmap p;
    UndoStack * s, * h;
    
    if (pw->paint.redoitems == 0)
	return None;
    --pw->paint.redoitems;
    p = pw->paint.redostack[(pw->paint.redobot + pw->paint.redoitems)
			   % pw->paint.nlevels];
    h = pw->paint.head;
    s = h;
    do {
	if (s->pixmap == p) {
	    s->pushed = False;
	    break;
	}
	s = s->next;
    } while (s != h);
    return p;
}

void 
Undo(Widget w)
{
    PaintWidget pw = GET_PW(w);
    Pixmap popped;

    popped = UndoPop(pw);
    if (popped == None) {
#ifdef ERRORBEEP
	XBell(XtDisplay(w), 100);
#endif
	return;
    }
    RedoPush(pw, pw->paint.current);
    pw->paint.current = popped;
    PwUpdate(w, NULL, True);
}

void 
Redo(Widget w)
{
    PaintWidget pw = GET_PW(w);
    Pixmap popped;

    popped = RedoPop(pw);
    if (popped == None) {
#ifdef ERRORBEEP
	XBell(XtDisplay(w), 100);
#endif
	return;
    }
    UndoPush(pw, pw->paint.current);
    pw->paint.current = popped;
    PwUpdate(w, NULL, True);
}

/*
 * Called when starting a drawing operation.
 */
Pixmap
PwUndoStart(Widget wid, XRectangle * rect)
{
    PaintWidget pw = GET_PW(wid);
    UndoStack * s;
    Pixmap cur;
    int x, y, w, h;

    pw->paint.dirty = True;

    cur = GET_PIXMAP(pw);
    UndoPush(pw, cur);
    RedoClear(pw);
    
    if (rect) {
	x = rect->x;
	y = rect->y;
	w = rect->width;
	h = rect->height;
    } else {
	x = y = 0;
	w = pw->paint.drawWidth;
	h = pw->paint.drawHeight;
    }

    for (s = pw->paint.head; s->next != pw->paint.head; s = s->next) {
	if (s->pushed == False)
	    break;
    }
	
    s->box.x = x;
    s->box.y = y;
    s->box.width = w;
    s->box.height = h;

    XCopyArea(XtDisplay(pw), cur, s->pixmap,
    pw->paint.gc, 0, 0, pw->paint.drawWidth, pw->paint.drawHeight, 0, 0);

    pw->paint.current = s->pixmap;
    return s->pixmap;
}


void 
UndoStart(Widget w, OpInfo * info)
{
    if (info->surface != opPixmap)
	return;

    info->drawable = PwUndoStart(w, NULL);
}


void 
UndoStartPoint(Widget w, OpInfo * info, int x, int y)
{
    PaintWidget pw = GET_PW(w);

    if (info->surface != opPixmap)
	return;

    UndoStart(w, info);

    if (pw->paint.undo == NULL)
	return;

    pw->paint.undo->box.x = x - pw->paint.lineWidth;
    pw->paint.undo->box.y = y - pw->paint.lineWidth;
    pw->paint.undo->box.width = 1 + pw->paint.lineWidth * 2;
    pw->paint.undo->box.height = 1 + pw->paint.lineWidth * 2;
}

void 
UndoGrow(Widget w, int x, int y)
{
    PaintWidget pw = GET_PW(w);
    XRectangle *rect;
    int dx, dy;

    if (pw->paint.undo == NULL)
	return;

    rect = &pw->paint.undo->box;

    rect->x += pw->paint.lineWidth;
    rect->y += pw->paint.lineWidth;
    rect->width -= pw->paint.lineWidth * 2;
    rect->height -= pw->paint.lineWidth * 2;

    dx = x - rect->x;
    dy = y - rect->y;

    if (dx > 0) {
	rect->width = MAX(rect->width, dx);
    } else {
	rect->width = (int) rect->width - dx + 1;
	rect->x = x;
    }

    if (dy > 0) {
	rect->height = MAX(rect->height, dy) + 1;
    } else {
	rect->height = (int) rect->height - dy + 1;
	rect->y = y;
    }

    rect->x -= pw->paint.lineWidth;
    rect->y -= pw->paint.lineWidth;
    rect->width += pw->paint.lineWidth * 2;
    rect->height += pw->paint.lineWidth * 2;
}

void 
PwUndoSetRectangle(Widget w, XRectangle * rect)
{
    PaintWidget pw = GET_PW(w);

    if (pw->paint.undo == NULL)
	return;

    pw->paint.undo->box = *rect;
}
void 
PwUndoAddRectangle(Widget w, XRectangle * rect)
{
    PaintWidget pw = GET_PW(w);

    if (pw->paint.undo == NULL)
	return;

    pw->paint.undo->box = *RectUnion(rect, &pw->paint.undo->box);
}

void 
UndoSetRectangle(Widget w, XRectangle * rect)
{
    PaintWidget pw = GET_PW(w);

    if (pw->paint.undo == NULL)
	return;

    PwUndoSetRectangle(w, rect);

    pw->paint.undo->box.x -= pw->paint.lineWidth;
    pw->paint.undo->box.y -= pw->paint.lineWidth;
    pw->paint.undo->box.width += pw->paint.lineWidth * 2 + 1;
    pw->paint.undo->box.height += pw->paint.lineWidth * 2 + 1;
}

void 
UndoStartRectangle(Widget w, OpInfo * info, XRectangle * rect)
{
    UndoStart(w, info);
    UndoSetRectangle(w, rect);
}
