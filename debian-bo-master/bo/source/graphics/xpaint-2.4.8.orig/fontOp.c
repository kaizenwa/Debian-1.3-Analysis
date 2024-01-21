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

/* $Id: fontOp.c,v 1.3 1996/04/19 08:56:26 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include "xpaint.h"
#include "Paint.h"
#include "graphic.h"
#include "misc.h"
#include "ops.h"

static Atom targetAtom, selectionAtom;

#define	DELAY	500

typedef struct {
    XtIntervalId id;
    Boolean typing;
    Boolean state;
    int startX, startY, curX, curY, height;
    XFontStruct *fi;
    int insertX, insertY, insertSX, insertSY, insertH;
    int zoom;
    Widget w;
    char *sptr;
    int maxStrLen;
    char minPos;
    char *str;
    Drawable drawable;
    GC gc, gcx;
} LocalInfo;

static void addString(Widget, LocalInfo *, int, char *);

static void 
cursor(LocalInfo * l, Boolean flag)
{
    if (l->w == None)
	return;
    if (flag) {
	XDrawLine(XtDisplay(l->w), XtWindow(l->w), l->gcx,
	    l->insertX, l->insertY, l->insertX, l->insertY + l->insertH);
	l->state = !l->state;
    } else if (l->state) {
	XDrawLine(XtDisplay(l->w), XtWindow(l->w), l->gcx,
	    l->insertX, l->insertY, l->insertX, l->insertY + l->insertH);
	l->state = False;
    }
}

static void 
flash(LocalInfo * l)
{
    cursor(l, True);
    l->id = XtAppAddTimeOut(XtWidgetToApplicationContext(l->w),
		      DELAY, (XtTimerCallbackProc) flash, (XtPointer) l);
}

static void 
gotSelection(Widget w, LocalInfo * l, Atom * selection, Atom * type,
	     XtPointer value, unsigned long *len, int *format)
{
    if (!l->typing)
	return;

    if (len == 0 || value == NULL)
	return;

    switch (*type) {
    case XA_STRING:
	addString(w, l, *len, (char *) value);
	break;
    }
    XtFree((XtPointer) value);
}

static void 
press(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    if (event->button == Button1) {
	cursor(l, False);

	XtVaGetValues(w, XtNfont, &l->fi, NULL);
	if (l->fi == NULL)
	    return;

	l->w = w;
	l->height = l->fi->ascent + l->fi->descent;
	l->startX = l->curX = info->x;
	l->startY = l->curY = info->y;
	l->curY = l->curY - l->height / 2;
	l->typing = True;

	if (info->surface == opWindow) {
	    l->insertH = l->height * info->zoom;
	    l->insertY = event->y - l->insertH / 2;
	    l->insertX = event->x;
	    l->insertSX = event->x;
	}
	l->sptr = l->str;

	if (l->id == (XtIntervalId) NULL)
	    flash(l);

	l->zoom = info->zoom;
	l->gc = info->first_gc;
	l->minPos = 0;

	UndoStartPoint(w, info, l->curX, l->curY);
	if (info->surface == opPixmap)
	    l->drawable = info->drawable;
    } else if (event->button == Button2) {
	XtGetSelectionValue(w, selectionAtom, targetAtom,
			    (XtSelectionCallbackProc) gotSelection, (XtPointer) l, event->time);
    }
}

static void 
key(Widget w, LocalInfo * l, XKeyEvent * event, OpInfo * info)
{
    char buf[21];
    KeySym keysym;
    int len;

    if (l->w == None)
	return;

    if ((len = XLookupString(event, buf, sizeof(buf) - 1, &keysym, NULL)) == 0)
	return;

    l->zoom = info->zoom;
    l->gc = info->first_gc;
    l->drawable = info->drawable;
    addString(w, l, len, buf);
}

static void 
addString(Widget w, LocalInfo * l, int len, char *buf)
{
    int i, width;
    XRectangle rect;

    if (len != 0)
	cursor(l, False);
    for (i = 0; i < len; i++) {
	if (l->sptr == l->str + l->maxStrLen - 5) {
	    int delta = l->sptr - l->str;
	    l->maxStrLen += 128;
	    l->str = (char *) XtRealloc((XtPointer) l->str, l->maxStrLen);
	    l->sptr = l->str + delta;
	}
	if (buf[i] == '\n' || buf[i] == '\r') {
	    l->curX = l->startX;
	    l->curY += l->height + 2;
	    l->insertX = l->insertSX;
	    l->insertY += l->insertH + 2 * l->zoom;

	    *l->sptr++ = '\n';
	} else if (buf[i] == 0x08 || buf[i] == 0x7f) {
	    if (l->sptr - l->str > l->minPos) {
		l->sptr--;
		if (*l->sptr == '\n') {
		    char *cp;
		    *l->sptr = '\0';
		    if ((cp = strrchr(l->str, '\n')) == NULL)
			cp = l->str;
		    else
			cp++;
		    width = XTextWidth(l->fi, cp, strlen(cp));
		    l->curX = l->startX + width;
		    l->curY -= l->height + 2;
		    l->insertX = (l->startX + width) * l->zoom;
		    l->insertY -= l->insertH + 2 * l->zoom;
		} else {
		    width = XTextWidth(l->fi, l->sptr, 1);
		    l->curX -= width;
		    l->insertX -= width * l->zoom;

		    /*
		    **  Now undraw the character.
		     */
		    XYtoRECT(l->curX, l->curY, l->curX + width, l->curY + l->height, &rect);
		    PwUpdateFromLast(w, &rect);
		}
	    }
	} else {
	    if (l->zoom == 1)
		XDrawString(XtDisplay(w), XtWindow(w), l->gc,
			    l->curX, l->curY + l->fi->ascent, &buf[i], 1);
	    XDrawString(XtDisplay(w), l->drawable, l->gc,
			l->curX, l->curY + l->fi->ascent, &buf[i], 1);
	    width = XTextWidth(l->fi, &buf[i], 1);

	    XYtoRECT(l->curX, l->curY, l->curX + width, l->curY + l->height, &rect);
	    PwUpdate(w, &rect, False);

	    l->curX += width;
	    l->insertX += width * l->zoom;

	    UndoGrow(w, l->curX, l->curY + l->height);
	    UndoGrow(w, l->curX, l->curY + l->height);

	    *l->sptr++ = buf[i];
	}
    }
}

/*
**  Those public functions
 */
void *
FontAdd(Widget w)
{
    static int inited = False;
    LocalInfo *l = (LocalInfo *) XtMalloc(sizeof(LocalInfo));

    if (!inited) {
	selectionAtom = XA_PRIMARY;
	targetAtom = XA_STRING;
    }
    l->id = (XtIntervalId) NULL;
    l->w = None;
    l->typing = False;
    l->state = False;

    l->maxStrLen = 128;
    l->str = (char *) XtMalloc(l->maxStrLen);
    l->gcx = GetGCX(w);

    OpAddEventHandler(w, opPixmap | opWindow, ButtonPressMask, FALSE,
		      (OpEventProc) press, l);
    OpAddEventHandler(w, opPixmap, KeyPressMask, FALSE, (OpEventProc) key, l);

    SetIBeamCursor(w);

    return l;
}
void 
FontRemove(Widget w, void *p)
{
    LocalInfo *l = (LocalInfo *) p;

    OpRemoveEventHandler(w, opPixmap | opWindow, ButtonPressMask, FALSE,
			 (OpEventProc) press, p);
    OpRemoveEventHandler(w, opPixmap, KeyPressMask, FALSE, (OpEventProc) key, p);

    if (l->id != (XtIntervalId) NULL)
	XtRemoveTimeOut(l->id);

    cursor(l, False);

    XtFree((XtPointer) l->str);
    XtFree((XtPointer) l);
}
void 
FontChanged(Widget w)
{
    LocalInfo *l;

    if (CurrentOp->add != FontAdd)
	return;

    l = (LocalInfo *) GraphicGetData(w);

    XtVaGetValues(w, XtNfont, &l->fi, NULL);
    if (l->fi == NULL)
	return;

    cursor(l, False);
    l->height = l->fi->ascent + l->fi->descent;

    l->minPos = l->sptr - l->str;
    l->insertH = l->height * l->zoom;
}
