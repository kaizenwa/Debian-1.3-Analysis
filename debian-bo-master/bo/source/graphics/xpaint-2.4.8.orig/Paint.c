
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

/* $Id: Paint.c,v 1.17 1996/06/19 19:02:29 torsten Exp $ */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/ShellP.h>
#include <X11/Xatom.h>
#include <stdio.h>
#include <stdlib.h>

#include "PaintP.h"
#include "xpaint.h"
#include "misc.h"

/*
**  Value at which zoom starts putting in "white" lines
**    between pixels
 */
#define ZOOM_THRESH 5


typedef void (*func_t) (Widget, void *, XEvent *, Boolean);
typedef struct data_s {
    func_t func;
    void *data;
    int mask, flag;
    struct data_s *next;
} data_t;

void PwAddChild(Widget, Widget);

static void pwSetPixmap(PaintWidget w, Pixmap pix, int flag);
static void fatCallback(Widget, XtPointer, XtPointer);
static void realExposeProc(PaintWidget, XExposeEvent *, Boolean);
static void drawVisibleGrid(PaintWidget, Widget, Boolean,
			    int, int, int, int, int);

static XtResource resources[] =
{
#define offset(field) XtOffset(PaintWidget, paint.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    {XtNcompress, XtCCompress, XtRBoolean, sizeof(Boolean),
     offset(compress), XtRImmediate, (XtPointer) False},
    {XtNreadOnly, XtCReadOnly, XtRBoolean, sizeof(Boolean),
     offset(compress), XtRImmediate, (XtPointer) False},
    {XtNdrawWidth, XtCDrawWidth, XtRInt, sizeof(int),
     offset(drawWidth), XtRImmediate, (XtPointer) 512},
    {XtNdrawHeight, XtCDrawHeight, XtRInt, sizeof(int),
     offset(drawHeight), XtRImmediate, (XtPointer) 512},
    {XtNlineWidth, XtCLineWidth, XtRInt, sizeof(int),
     offset(lineWidth), XtRImmediate, (XtPointer) 0},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(fontInfo), XtRString, XtDefaultFont},
    {XtNpattern, XtCPattern, XtRPixmap, sizeof(Pixmap),
     offset(pattern), XtRPixmap, (XtPointer) None},
    {XtNlinePattern, XtCLinePattern, XtRPixmap, sizeof(Pixmap),
     offset(linePattern), XtRPixmap, (XtPointer) None},
    {XtNpixmap, XtCPixmap, XtRPixmap, sizeof(Pixmap),
     offset(sourcePixmap), XtRPixmap, (XtPointer) NULL},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground), XtRString, XtDefaultForeground},
    {XtNlineForeground, XtCLineForeground, XtRPixel, sizeof(Pixel),
     offset(lineForeground), XtRString, XtDefaultForeground},
    {XtNsnap, XtCSnap, XtRInt, sizeof(int),
     offset(snap), XtRImmediate, (XtPointer) 10},
    {XtNregionCallback, XtCRegionCallback, XtRCallback, sizeof(XtPointer),
     offset(regionCalls), XtRCallback, (XtPointer) None},
    {XtNsizeChanged, XtCSizeChanged, XtRCallback, sizeof(XtPointer),
     offset(sizecalls), XtRCallback, (XtPointer) None},
    {XtNfatBack, XtCFatBack, XtRCallback, sizeof(XtPointer),
     offset(fatcalls), XtRCallback, (XtPointer) None},
    {XtNundoSize, XtCUndoSize, XtRInt, sizeof(int),
     offset(undoSize), XtRImmediate, (XtPointer) 1},
    {XtNzoom, XtCZoom, XtRInt, sizeof(int),
     offset(zoom), XtRImmediate, (XtPointer) 1},
    {XtNzoomX, XtCZoomX, XtRInt, sizeof(int),
     offset(zoomX), XtRImmediate, (XtPointer) 0},
    {XtNzoomY, XtCZoomY, XtRInt, sizeof(int),
     offset(zoomY), XtRImmediate, (XtPointer) 0},
    {XtNgrid, XtCGrid, XtRBoolean, sizeof(Boolean),
     offset(grid), XtRImmediate, (XtPointer) False},
    {XtNsnapOn, XtCSnapOn, XtRBoolean, sizeof(Boolean),
     offset(snapOn), XtRImmediate, (XtPointer) False},
    {XtNpaint, XtCPaint, XtRWidget, sizeof(Widget),
     offset(paint), XtRImmediate, (XtPointer) None},
    {XtNdirty, XtCDirty, XtRBoolean, sizeof(Boolean),
     offset(dirty), XtRImmediate, (XtPointer) False},
    {XtNfillRule, XtCFillRule, XtRInt, sizeof(int),
     offset(fillRule), XtRImmediate, (XtPointer) FillSolid},
    {XtNlineFillRule, XtCLineFillRule, XtRInt, sizeof(int),
     offset(lineFillRule), XtRImmediate, (XtPointer) FillSolid},
    {XtNdownX, XtCDownX, XtRPosition, sizeof(Position),
     offset(downX), XtRImmediate, (XtPointer) 0},
    {XtNdownY, XtCDownY, XtRPosition, sizeof(Position),
     offset(downY), XtRImmediate, (XtPointer) 0},
    {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(cursor), XtRString, (XtPointer) "watch"},
    {XtNfilename, XtCfilename, XtRString, sizeof(String),
     offset(filename), XtRString, (XtPointer) 0},
    {XtNmenuwidgets, XtCmenuwidgets, XtRWidgetList, sizeof(WidgetList),
     offset(menuwidgets), XtRWidgetList, (XtPointer) 0}
#undef offset
};

/*
**  Some forward defines.
 */
static void InitializeProc(Widget, Widget);
static void DestroyProc(Widget);
static void ResizeProc(Widget);
static void ExposeProc(Widget, XExposeEvent *);
static void RealizeProc(Widget, Mask *, XSetWindowAttributes *);
static Boolean SetValuesProc(Widget, Widget, Widget);
static void GetValuesHook(Widget, ArgList, Cardinal *);
static int QueryGeometryProc(Widget w, XtWidgetGeometry * proposed,
			     XtWidgetGeometry * reply);

static XtGeometryResult
GeometryManager(Widget w, XtWidgetGeometry * request, XtWidgetGeometry * reply)
{
    *reply = *request;

    if ((request->request_mode & CWX) != 0)
	w->core.x = request->x;
    if ((request->request_mode & CWY) != 0)
	w->core.y = request->y;
    if ((request->request_mode & CWWidth) != 0)
	w->core.width = request->width;
    if ((request->request_mode & CWHeight) != 0)
	w->core.height = request->height;
    if ((request->request_mode & CWBorderWidth) != 0)
	w->core.border_width = request->border_width;

    return XtGeometryYes;
}


PaintClassRec paintClassRec =
{
    {				/* core fields */
    /* superclass		*/ (WidgetClass) & compositeClassRec,
    /* class_name		*/ "Paint",
    /* widget_size		*/ sizeof(PaintRec),
    /* class_initialize		*/ NULL,
    /* class_part_initialize	*/ NULL,
    /* class_inited		*/ FALSE,
    /* initialize		*/ (XtInitProc) InitializeProc,
    /* initialize_hook		*/ NULL,
    /* realize			*/ RealizeProc,
    /* actions			*/ NULL,
    /* num_actions		*/ 0,
    /* resources		*/ resources,
    /* num_resources		*/ XtNumber(resources),
    /* xrm_class		*/ NULLQUARK,
    /* compress_motion		*/ FALSE,
    /* compress_exposure	*/ TRUE,
    /* compress_enterleave	*/ TRUE,
    /* visible_interest		*/ FALSE,
    /* destroy			*/ DestroyProc,
    /* resize			*/ ResizeProc,
    /* expose			*/ (XtExposeProc) ExposeProc,
    /* set_values		*/ (XtSetValuesFunc) SetValuesProc,
    /* set_values_hook		*/ NULL,
    /* set_values_almost	*/ XtInheritSetValuesAlmost,
    /* get_values_hook		*/ GetValuesHook,
    /* accept_focus		*/ NULL,
    /* version			*/ XtVersion,
    /* callback_private		*/ NULL,
    /* tm_table			*/ NULL,
    /* query_geometry		*/ (XtGeometryHandler) QueryGeometryProc,
    /* display_accelerator	*/ XtInheritDisplayAccelerator,
    /* extension		*/ NULL
    },
    {				/* composite fields	    */
    /* geometry_manager		*/ GeometryManager,
    /* change_managed		*/ XtInheritChangeManaged,
    /* insert_child		*/ XtInheritInsertChild,
    /* delete_childion		*/ XtInheritDeleteChild,
    /* extension		*/ NULL,
    },
    {				/* paint fields */
	0
    }
};

WidgetClass paintWidgetClass = (WidgetClass) & paintClassRec;

/*
 * Attach a new undo buffer to the head of the undo list for this widget.
 */
static UndoStack *
undoBuffer(PaintWidget w)
{
    UndoStack *new = (UndoStack *) XtMalloc(sizeof(UndoStack));

    if (new == NULL)
	return NULL;

    new->box.x = 0;
    new->box.y = 0;
    new->box.width = 0;
    new->box.height = 0;
    new->pixmap = None;
    new->next = w->paint.head;
    new->pushed = False;
    w->paint.head = new;

    return new;
}

static void
InitializeProc(Widget w, Widget newArg)
{
    PaintWidget new = (PaintWidget) newArg;
    int i;
    int zoom = new->paint.zoom;
    UndoStack * cur;

    if (zoom == PwZoomParent && new->paint.paint == None)
	zoom = new->paint.zoom = 1;

    if (zoom == PwZoomParent)
	zoom = 1;
    else if (zoom < 0)
	zoom = new->paint.zoom = 1;

    if (new->paint.sourcePixmap != None) {
	Window root;
	int x, y;
	unsigned int width, height, bw, depth;

	XGetGeometry(XtDisplay(w), new->paint.sourcePixmap,
		     &root, &x, &y, &width, &height, &bw, &depth);

	new->paint.drawWidth = width;
	new->paint.drawHeight = height;
    } else if (new->paint.paint != None) {
	PaintWidget parent = (PaintWidget) new->paint.paint;

	new->paint.drawWidth = parent->paint.drawWidth;
	new->paint.drawHeight = parent->paint.drawHeight;
    }
    if (new->core.width == 0)
	new->core.width = new->paint.drawWidth * zoom;
    if (new->core.height == 0)
	new->core.height = new->paint.drawHeight * zoom;

    new->paint.foreground = BlackPixelOfScreen(XtScreen(new));
    new->paint.background = WhitePixelOfScreen(XtScreen(new));

    new->paint.gc = None;
    new->paint.igc = None;
    new->paint.sgc = None;
    new->paint.fgc = None;
    new->paint.mgc = None;
    new->paint.xgc = None;
    new->paint.tgc = None;

    /*
    **	Init the undo stuff
     */
    new->paint.head = NULL;
    if (new->paint.paint == None) {
	cur = undoBuffer(new);
	for (i = 0; i <= new->paint.undoSize; i++)
	    undoBuffer(new);
	cur->next = new->paint.head;
	UndoInitialize(new, new->paint.undoSize);
	new->paint.undo = new->paint.head;
	new->paint.current = new->paint.head->pixmap;
    } else {
	new->paint.undo = NULL;
	new->paint.current = None;
    }

    /*
    **	The region information
     */
    new->paint.region.isVisible = False;
    new->paint.region.mask = None;
    new->paint.region.source = None;
    new->paint.region.notMask = None;
    new->paint.region.fg_gc = None;
    new->paint.region.bg_gc = None;
    new->paint.region.child = None;
    new->paint.region.sourceImg = NULL;
    new->paint.region.maskImg = NULL;
    new->paint.region.notMaskImg = NULL;
    new->paint.region.proc = NULL;

    for (i = 0; i < XtNumber(new->paint.region.grip); i++)
	new->paint.region.grip[i] = None;

    /*
    **	Now the backing store image
     */
    new->paint.image = NULL;

    /*
    **
     */
    new->paint.paintChildrenSize = 0;
    new->paint.paintChildren = NULL;
    new->paint.filename = NULL;
    new->paint.menuwidgets = NULL;
}

static XRectangle *
GetRectangle(PaintWidget w)
{
    static XRectangle rect;
    XRectangle *rp = &rect, newRect;
    Widget cur;
    Widget prev = (Widget) w;
    int dx, dy;

    rect.x = dx = 0;
    rect.y = dy = 0;
    rect.width = w->core.width - dx;
    rect.height = w->core.height - dy;

    /*
    **	Climb the tree to find the visible area.
    **
     */
    for (cur = XtParent((Widget) w); !XtIsShell(cur); cur = XtParent(cur)) {
	dx += -prev->core.x;
	dy += -prev->core.y;
	newRect.x = dx;
	newRect.y = dy;
	newRect.width = cur->core.width;
	newRect.height = cur->core.height;

	rp = RectIntersect(&newRect, rp);

	prev = cur;
    }

    if (rp != NULL)
	rect = *rp;
    else
	rect.x = rect.y = rect.width = rect.height = 0;

    return &rect;
}

static void
RealizeProc(Widget w, Mask * valueMask,
	    XSetWindowAttributes * attributes)
{
    XWindowAttributes wattr;
    PaintWidget pw = (PaintWidget) w;
    PaintWidget pp = (PaintWidget) pw->paint.paint;
    UndoStack *cur;
    Window root;
    int x, y, i;
    unsigned int width, height, bw, depth;
    XGCValues values;

    XtCreateWindow(w, InputOutput, CopyFromParent, *valueMask, attributes);

    XGetGeometry(XtDisplay(w), XtWindow(w),
		 &root, &x, &y, &width, &height, &bw, &depth);

    values.foreground = pw->paint.foreground;
    values.background = pw->core.background_pixel;
    pw->paint.gc = XtGetGC(w, GCForeground | GCBackground, &values);

    if (pp == None) {
	/*
	**  Inverse of the base colors.
	 */
	values.foreground = pw->core.background_pixel;
	values.background = pw->paint.foreground;
	pw->paint.igc = XCreateGC(XtDisplay(w), XtWindow(w),
				  GCForeground | GCBackground, &values);
	pw->paint.tgc = XCreateGC(XtDisplay(w), XtWindow(w), 0, 0);

	values.function = GXxor;
	values.foreground = ~0;
	values.subwindow_mode = IncludeInferiors;
	pw->paint.xgc = XCreateGC(XtDisplay(w), XtWindow(w),
		   GCForeground | GCFunction | GCSubwindowMode, &values);
    } else {
	pw->paint.igc = pp->paint.igc;
	pw->paint.tgc = pp->paint.tgc;
	pw->paint.xgc = pp->paint.xgc;

	pw->paint.lineWidth = pp->paint.lineWidth;
    }

    /*
    **	Create the filled gc information
     */
    if (pw->paint.paint == None) {
	pw->paint.sgc = XCreateGC(XtDisplay(w), XtWindow(w), 0, 0);
	pw->paint.fgc = XCreateGC(XtDisplay(w), XtWindow(w), 0, 0);
	XSetForeground(XtDisplay(w), pw->paint.fgc, pw->paint.foreground);
	XSetForeground(XtDisplay(w), pw->paint.sgc, pw->paint.lineForeground);

	/*
	**  First the fill GC
	 */
	XSetFillStyle(XtDisplay(w), pw->paint.fgc, pw->paint.fillRule);
	XSetLineAttributes(XtDisplay(w), pw->paint.fgc,
			   pw->paint.lineWidth, LineSolid,
			   CapRound, JoinMiter);
	if (pw->paint.pattern != None)
	    XSetTile(XtDisplay(w), pw->paint.fgc, pw->paint.pattern);
	if (pw->paint.fontInfo != NULL && pw->paint.fontInfo->fid != None)
	    XSetFont(XtDisplay(w), pw->paint.fgc, pw->paint.fontInfo->fid);

	/*
	**  Then the line GC
	 */
	XSetFillStyle(XtDisplay(w), pw->paint.sgc, pw->paint.lineFillRule);
	XSetLineAttributes(XtDisplay(w), pw->paint.sgc,
			   pw->paint.lineWidth, LineSolid,
			   CapRound, JoinMiter);
	if (pw->paint.linePattern != None)
	    XSetTile(XtDisplay(w), pw->paint.sgc, pw->paint.linePattern);
    } else if (pw->paint.paint != None) {
	pw->paint.sgc = pp->paint.sgc;
	pw->paint.fgc = pp->paint.fgc;
    }
    /*
    **	Parent realized after children, odd..
     */
    if (pp == None && pw->paint.paintChildrenSize != 0)
	for (i = 0; i < pw->paint.paintChildrenSize; i++) {
	    PaintWidget tp = (PaintWidget) pw->paint.paintChildren[i];
	    tp->paint.sgc = pw->paint.sgc;
	    tp->paint.fgc = pw->paint.fgc;
	    tp->paint.igc = pw->paint.igc;
	    tp->paint.xgc = pw->paint.xgc;
	    tp->paint.tgc = pw->paint.tgc;
	}
    /*
    **	Create the backing undo buffers
     */
    for (cur = pw->paint.head; cur && (cur->pixmap == None); cur = cur->next)
	cur->pixmap = XCreatePixmap(XtDisplay(w), XtWindow(w),
			       pw->paint.drawWidth, pw->paint.drawHeight,
				    depth);

    /*
    **	If this paint widget is the child of another paint
    **	 widget then either create our own unique information
    **	 or just use the information from our parent.
     */
    if (pw->paint.paint == None) {
	pw->paint.current = pw->paint.head->pixmap;

	if (pw->paint.sourcePixmap != None)
	    pwSetPixmap(pw, pw->paint.sourcePixmap, FALSE);
	else
	    XFillRectangle(XtDisplay(w), pw->paint.current,
			   pw->paint.igc, 0, 0,
			   pw->paint.drawWidth, pw->paint.drawHeight);
    } else
	PwAddChild(pw->paint.paint, w);

    XGetWindowAttributes(XtDisplay(w), XtWindow(w), &wattr);
    pw->paint.visual = wattr.visual;

    if (pw->paint.cursor != None)
	XDefineCursor(XtDisplay(pw), XtWindow(pw), pw->paint.cursor);
}

static void
DestroyProc(Widget w)
{
    PaintWidget pw = (PaintWidget) w;
    UndoStack *cur, *nxt;

    if (pw->paint.mgc != None)
	XFreeGC(XtDisplay(pw), pw->paint.mgc);
    if (pw->paint.gc != None)
	XtReleaseGC(w, pw->paint.gc);

    if (pw->paint.region.source != None)
	XFreePixmap(XtDisplay(pw), pw->paint.region.source);
    if (pw->paint.region.mask != None)
	XFreePixmap(XtDisplay(pw), pw->paint.region.mask);
    if (pw->paint.region.notMask != None)
	XFreePixmap(XtDisplay(pw), pw->paint.region.notMask);
    if (pw->paint.region.sourceImg != NULL)
	XDestroyImage(pw->paint.region.sourceImg);
    if (pw->paint.region.maskImg != NULL)
	XDestroyImage(pw->paint.region.maskImg);
    if (pw->paint.region.notMaskImg != NULL)
	XDestroyImage(pw->paint.region.notMaskImg);

    if (pw->paint.paint != None)
	return;

    /*	WARNING: the rest of this callback doesn't execute for children */

    if (pw->paint.filename != NULL)
	free(pw->paint.filename);
    if (pw->paint.menuwidgets != NULL)
	free(pw->paint.menuwidgets);

    if (pw->paint.tgc != None)
	XFreeGC(XtDisplay(pw), pw->paint.tgc);
    if (pw->paint.xgc != None)
	XFreeGC(XtDisplay(pw), pw->paint.xgc);
    if (pw->paint.igc != None)
	XFreeGC(XtDisplay(pw), pw->paint.igc);
    if (pw->paint.fgc != None)
	XFreeGC(XtDisplay(pw), pw->paint.fgc);
    if (pw->paint.sgc != None)
	XFreeGC(XtDisplay(pw), pw->paint.sgc);

    cur = pw->paint.head;
    do {
	nxt = cur->next;
	if (cur->pixmap != None)
	    XFreePixmap(XtDisplay(pw), cur->pixmap);
	XtFree((XtPointer) cur);
	cur = nxt;
    }
    while (cur != pw->paint.head);

    if (pw->paint.image != NULL) {
	XDestroyRegion(pw->paint.imageRegion);
	XDestroyImage(pw->paint.image);
    }
    if (pw->paint.paintChildren != NULL)
	XtFree((XtPointer) pw->paint.paintChildren);
}

static void
ResizeProc(Widget w)
{

}

static void
drawVisibleGrid(PaintWidget pw, Widget w, Boolean flag,
		int zoom, int sx, int sy, int ex, int ey)
{
    static GC tgc = None;
    GC gc;
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    int i;

    if (zoom <= 1)
	return;

    if (flag) {
	if (tgc == None) {
	    XGCValues values;

	    values.foreground = BlackPixelOfScreen(XtScreen(w));
	    tgc = XtGetGC(w, GCForeground, &values);
	}
	gc = tgc;
    } else {
	/*
	**  Turning off visible grid, and we need to clear
	**    to no space between pixels...
	 */
	if (zoom <= ZOOM_THRESH) {
	    XClearArea(dpy, win, sx, sy, ex - sx, ey - sy, True);
	    return;
	}
	gc = pw->paint.igc;
    }

    for (i = sx; i < ex; i += zoom)
	XDrawLine(dpy, win, gc, i, sy, i, ey);
    for (i = sy; i < ey; i += zoom)
	XDrawLine(dpy, win, gc, sx, i, ex, i);
}

/*
**  Update the zoomed region as specified by rect.
**    rect is in screen coordinates.
**
**  There are two drawers, the first draws using rectangles
**   the second draws using XImages.
**
 */

static void
zoomDrawRects(PaintWidget pw, Widget w, GC gc, XImage * xim, XImage * mask,
	      Boolean isExpose, int xstart, int ystart, int zoom,
	      XRectangle * rect)
{
    static Boolean *flags = NULL;
    static int flagsSize = 0;
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    int width = rect->width;
    int count = rect->width * rect->height;
    XRectangle rects[256];
    int index, i;

    if (flagsSize < count) {
	int allocSize = (count + 4) * sizeof(Boolean);
	if (flags == NULL)
	    flags = (Boolean *) XtMalloc(allocSize);
	else
	    flags = (Boolean *) XtRealloc((XtPointer) flags, allocSize);
	flagsSize = count;
    }
    memset(flags, 0, flagsSize * sizeof(Boolean));
    if (mask != NULL) {
	int x, y;

	i = 0;
	for (y = rect->y; y < rect->height + rect->y; y++) {
	    for (x = rect->x; x < rect->width + rect->x; x++, i++) {
		if (!XGetPixel(mask, x, y))
		    flags[i] = True;
	    }
	}
    }
    for (i = 0; i < XtNumber(rects) && i < count; i++)
	if (zoom > ZOOM_THRESH)
	    rects[i].width = rects[i].height = zoom - 1;
	else
	    rects[i].width = rects[i].height = zoom;

    for (index = 0; index != count; index++) {
	if (!flags[index]) {
	    int xindex = index % width + rect->x;
	    int yindex = index / width + rect->y;
	    int xindexEnd = rect->x + rect->width;
	    Pixel p = xxGetPixel(xim, xindex, yindex);
	    int xpos = (index % width + xstart) * zoom;
	    int ypos = (index / width + ystart) * zoom;
	    int pos, flag;

	    flag = (isExpose && p == w->core.background_pixel);

	    XSetForeground(dpy, gc, p);
	    for (pos = index, i = 0; pos != count;
		 pos++, xindex++, xpos += zoom) {
		if (xindex == xindexEnd) {
		    xindex = rect->x;
		    xpos = xstart * zoom;
		    yindex++;
		    ypos += zoom;
		}
		if (flags[pos] || xxGetPixel(xim, xindex, yindex) != p)
		    continue;
		flags[pos] = True;

		if (flag)
		    continue;

		rects[i].x = xpos;
		rects[i].y = ypos;
		i++;

		if (i == XtNumber(rects)) {
		    XFillRectangles(dpy, win, gc, rects, i);
		    i = 0;
		}
	    }
	    if (i > 0)
		XFillRectangles(dpy, win, gc, rects, i);
	}
    }
}

static void
zoomDrawImage(PaintWidget pw, Widget w, GC gc, XImage * xim, XImage * mask,
	      Boolean isExpose, int xstart, int ystart, int zoom,
	      XRectangle * rect)
{
    Display *dpy = XtDisplay(w);
    int width = rect->width;
    int height = rect->height;
    XImage *dst;
    XImage *dstMsk = NULL;
    int x, y, x1, y1, dx, dy;
    int numDraw = zoom - ((zoom > ZOOM_THRESH) ? 1 : 0);
    int endX = rect->x + rect->width;
    int endY = rect->y + rect->height;

    dst = XCreateImage(dpy, pw->paint.visual, xim->depth, xim->format,
		       0, NULL, width * zoom, height * zoom, 32, 0);
    dst->data = (char *) XtMalloc(height * zoom * dst->bytes_per_line);
    if (mask != NULL) {
	dstMsk = XCreateImage(dpy, pw->paint.visual,
			      mask->depth, mask->format,
			      0, NULL, width * zoom, height * zoom, 8, 0);
	dstMsk->data = (char *) XtMalloc(height * zoom * dstMsk->bytes_per_line);
	memset(dstMsk->data, 0, height * zoom * dstMsk->bytes_per_line);
    }
    if (mask == NULL) {
	for (dy = 0, y = rect->y; y < endY; y++, dy += zoom) {
	    for (dx = 0, x = rect->x; x < endX; x++, dx += zoom) {
		Pixel p;

		p = xxGetPixel(xim, x, y);

		for (y1 = 0; y1 < numDraw; y1++) {
		    for (x1 = 0; x1 < numDraw; x1++) {
			xxPutPixel(dst, dx + x1, dy + y1, p);
		    }
		    if (numDraw != zoom)
			xxPutPixel(dst, dx + x1, dy + y1,
				   w->core.background_pixel);
		}
		if (numDraw != zoom)
		    for (x1 = 0; x1 < zoom; x1++)
			xxPutPixel(dst, dx + x1, dy + y1,
				   w->core.background_pixel);
	    }
	}
    } else {
	for (dy = 0, y = rect->y; y < endY; y++, dy += zoom) {
	    for (dx = 0, x = rect->x; x < endX; x++, dx += zoom) {
		Pixel p;

		if (!xxGetPixel(mask, x, y))
		    continue;

		p = xxGetPixel(xim, x, y);

		for (y1 = 0; y1 < numDraw; y1++) {
		    for (x1 = 0; x1 < numDraw; x1++) {
			xxPutPixel(dst, dx + x1, dy + y1, p);
			xxPutPixel(dstMsk, dx + x1, dy + y1, True);
		    }
		}
	    }
	}
    }

    if (mask != NULL) {
	Pixmap pix = XCreatePixmap(XtDisplay(w), XtWindow(w),
				   width * zoom, height * zoom, 1);

	XPutImage(XtDisplay(w), pix, pw->paint.mgc, dstMsk,
		  0, 0, 0, 0, width * zoom, height * zoom);

	XSetClipMask(XtDisplay(w), gc, pix);
	XSetClipOrigin(XtDisplay(w), gc, xstart * zoom, ystart * zoom);
	XPutImage(XtDisplay(w), XtWindow(w), gc, dst, 0, 0,
		  xstart * zoom, ystart * zoom,
		  width * zoom, height * zoom);
	XSetClipMask(XtDisplay(w), gc, None);

	XFreePixmap(XtDisplay(w), pix);
    } else {
	XPutImage(XtDisplay(w), XtWindow(w), gc, dst, 0, 0,
		  xstart * zoom, ystart * zoom,
		  width * zoom, height * zoom);
    }

    XDestroyImage(dst);
    if (dstMsk != NULL)
	XDestroyImage(dstMsk);
}

static void
zoomUpdate(PaintWidget pw, Boolean isExpose, XRectangle * rect)
{
    int zoom = GET_ZOOM(pw);
    int sx, sy, w, h, zsx, zsy;
    XImage *xim;
    XRectangle *isec, *tmp;
    XRectangle real, grab;

    if ((isec = RectIntersect(rect, GetRectangle(pw))) == NULL)
	return;

    zsx = sx = isec->x / zoom;
    zsy = sy = isec->y / zoom;
    w = (isec->width + (isec->x % zoom) + zoom - 1) / zoom;
    h = (isec->height + (isec->y % zoom) + zoom - 1) / zoom;

    grab.x = sx + pw->paint.zoomX;
    grab.y = sy + pw->paint.zoomY;
    grab.width = w;
    grab.height = h;
    real.x = 0;
    real.y = 0;
    real.width = pw->paint.drawWidth;
    real.height = pw->paint.drawHeight;

    if ((tmp = RectIntersect(&grab, &real)) == NULL)
	return;

    xim = PwGetImage((Widget) pw, tmp);

    real.x = tmp->x;
    real.y = tmp->y;
    real.width = tmp->width;
    real.height = tmp->height;

    _PwZoomDraw(pw, (Widget) pw, pw->paint.tgc, xim, NULL,
		isExpose, sx, sy, zoom, &real);
}

/*
**  pw	= parent paint widget
**  w	= widget who's window will be updated
**  gc	= gc to use (read/written)
**  src = source XImage to use
**  mask = mask XImage to use
**  flag = is this an expose event, so we don't have to
**	   update pixels == window background
**  sx,sy = start x, y for window drawing (non-zoomed)
**  zoom = zoom value to apply
**  rect = region of the input ximages to use to draw
**	   a start x,y with the specified width,height
 */
void
_PwZoomDraw(PaintWidget pw, Widget w, GC gc, XImage * src, XImage * mask,
	    Boolean flag, int sx, int sy, int zoom, XRectangle * rect)
{
    if (rect->width * rect->height < 1024 + 256)
	zoomDrawRects(pw, w, gc, src, mask, flag, sx, sy, zoom, rect);
    else
	zoomDrawImage(pw, w, gc, src, mask, flag, sx, sy, zoom, rect);

    if (pw->paint.grid) {
	int xstart, xend, ystart, yend;

	xstart = sx * zoom - 1;
	ystart = sy * zoom - 1;
	xend = xstart + rect->width * zoom + zoom - 1;
	yend = ystart + rect->height * zoom + zoom - 1;

	drawVisibleGrid(pw, w, True, zoom, xstart, ystart, xend, yend);
    }
}


void
PwUpdateDrawable(Widget w, Drawable draw, XRectangle * rect)
{
    PaintWidget pw = (PaintWidget) w;
    XRectangle all;
    Pixmap pix = GET_PIXMAP(pw);

    if (rect == NULL || rect->width == 0 || rect->height == 0) {
	rect = &all;
	all.x = all.y = 0;
	all.width = pw->core.width;
	all.height = pw->core.height;
    }
    XCopyArea(XtDisplay(w), pix, draw, pw->paint.gc,
	      rect->x + pw->paint.zoomX,
	      rect->y + pw->paint.zoomY,
	      rect->width, rect->height,
	      rect->x, rect->y);
}

static void
realExposeProc(PaintWidget pw, XExposeEvent * event, Boolean flag)
{
    int x, y, width, height;
    XRectangle rect;

    if (!XtIsRealized(((Widget) pw)))
	return;

    /*
    **	Clean up the event, since I do simulate them
     */
    x = event->x;
    y = event->y;
    width = event->width;
    height = event->height;

    if (x < 0) {
	width += x;
	x = 0;
    }
    if (y < 0) {
	height += y;
	y = 0;
    }
    if (width < 0 || height < 0)
	return;
    if (x > pw->core.width || y > pw->core.height)
	return;
    if (x + width > pw->core.width)
	width = pw->core.width - x;
    if (y + height > pw->core.height)
	height = pw->core.height - y;

    rect.x = x;
    rect.y = y;
    rect.width = width;
    rect.height = height;

    if (GET_ZOOM(pw) <= 1)
	PwUpdateDrawable((Widget) pw, XtWindow((Widget) pw), &rect);
    else
	zoomUpdate(pw, flag, &rect);
}

static void
ExposeProc(Widget w, XExposeEvent * event)
{
    realExposeProc((PaintWidget) w, event, True);
}

static void
resizePixmap(PaintWidget w, Pixmap * pixmap)
{
    Pixmap n;

    if (pixmap == NULL || *pixmap == None)
	return;

    n = XCreatePixmap(XtDisplay(w), XtWindow(w),
		      w->paint.drawWidth, w->paint.drawHeight,
		      w->core.depth);
    XFillRectangle(XtDisplay(w), n,
		   w->paint.igc, 0, 0,
		   w->paint.drawWidth, w->paint.drawHeight);
    XCopyArea(XtDisplay(w), *pixmap, n,
	      w->paint.gc, 0, 0,
	      w->paint.drawWidth, w->paint.drawHeight, 0, 0);
    XFreePixmap(XtDisplay(w), *pixmap);
    *pixmap = n;
}

static Boolean
SetValuesProc(Widget curArg, Widget request,
	      Widget newArg)
{
    PaintWidget cur = (PaintWidget) curArg;
    PaintWidget new = (PaintWidget) newArg;
    int ret = False;
    int sizeChanged = False;
    int i;

    if (cur->paint.sourcePixmap != new->paint.sourcePixmap)
	pwSetPixmap(new, new->paint.sourcePixmap, True);

    if (cur->paint.compress != new->paint.compress)
	new->core.widget_class->core_class.compress_motion = new->paint.compress;

    if (!XtIsRealized((Widget) new))
	return ret;

    if (cur->paint.fillRule != new->paint.fillRule)
	XSetFillStyle(XtDisplay(new), new->paint.fgc, new->paint.fillRule);
    if (cur->paint.foreground != new->paint.foreground)
	XSetForeground(XtDisplay(new), new->paint.fgc, new->paint.foreground);
    if (cur->paint.pattern != new->paint.pattern)
	XSetTile(XtDisplay(new), new->paint.fgc, new->paint.pattern);

    if (cur->paint.lineFillRule != new->paint.lineFillRule)
	XSetFillStyle(XtDisplay(new), new->paint.sgc, new->paint.lineFillRule);
    if (cur->paint.lineForeground != new->paint.lineForeground)
	XSetForeground(XtDisplay(new),
		       new->paint.sgc, new->paint.lineForeground);
    if (cur->paint.linePattern != new->paint.linePattern)
	XSetTile(XtDisplay(new), new->paint.sgc, new->paint.linePattern);

    if (cur->paint.lineWidth != new->paint.lineWidth) {
	XSetLineAttributes(XtDisplay(new), new->paint.fgc,
			   new->paint.lineWidth, LineSolid,
			   CapRound, JoinMiter);
	XSetLineAttributes(XtDisplay(new), new->paint.sgc,
			   new->paint.lineWidth, LineSolid,
			   CapRound, JoinMiter);
    }
    if (cur->paint.fontInfo->fid != new->paint.fontInfo->fid) {
	XSetFont(XtDisplay(new), new->paint.fgc,
		 new->paint.fontInfo->fid);
    }
    if (cur->paint.drawWidth != new->paint.drawWidth ||
	cur->paint.drawHeight != new->paint.drawHeight) {
	UndoStack *cur;
	int gotit = 0;

	/*
	**  Resize the undo buffers
	 */
	if (new->paint.paint == None) {
	    cur = new->paint.head;
	    do {
		if (cur->pixmap == new->paint.current)
		    gotit = 1;
		resizePixmap(new, &cur->pixmap);
		if (gotit) {
		    new->paint.current = cur->pixmap;
		    gotit = 0;
		}
	    } while ((cur = cur->next) != new->paint.head);
	    new->paint.undobot = new->paint.undoitems = 0;
	    new->paint.redobot = new->paint.redoitems = 0;
	}
	/*
	**  Destroy the XImage that is cached
	 */
	if (new->paint.image != NULL) {
	    XDestroyImage(new->paint.image);
	    XDestroyRegion(new->paint.imageRegion);
	    new->paint.image = NULL;
	}
	if (XtMakeResizeRequest((Widget) new,
				new->paint.drawWidth * new->paint.zoom,
				new->paint.drawHeight * new->paint.zoom,
				NULL, NULL) == XtGeometryDone) {
	    ret = True;
	    sizeChanged = True;
	}
	for (i = 0; i < new->paint.paintChildrenSize; i++) {
	    XtVaSetValues(new->paint.paintChildren[i],
			  XtNdrawWidth, new->paint.drawWidth,
			  XtNdrawHeight, new->paint.drawHeight,
			  NULL);
	}
    }
    if (cur->paint.zoom != new->paint.zoom) {
	ret = True;

	XtMakeResizeRequest((Widget) new,
			    new->paint.drawWidth * new->paint.zoom,
			    new->paint.drawHeight * new->paint.zoom,
			    NULL, NULL);
	sizeChanged = True;
    }
    if (cur->paint.zoomX != new->paint.zoomX ||
	cur->paint.zoomY != new->paint.zoomY) {
	XExposeEvent event;

	event.x = 0;
	event.y = 0;
	event.width = new->core.width;
	event.height = new->core.height;

	realExposeProc(new, &event, False);
	pwRegionZoomPosChanged(new);
    }
    if (cur->paint.grid != new->paint.grid) {
	XRectangle *rect = GetRectangle(new);

	rect->x -= rect->x % new->paint.zoom + 1;
	rect->y -= rect->y % new->paint.zoom + 1;

	drawVisibleGrid(new, (Widget) new,
			new->paint.grid, GET_ZOOM(new),
			rect->x, rect->y,
			rect->x + rect->width, rect->y + rect->height);

	for (i = 0; i < new->paint.paintChildrenSize; i++) {
	    XtVaSetValues(new->paint.paintChildren[i],
			  XtNgrid, new->paint.grid,
			  NULL);
	}
    }
    if (cur->core.background_pixel != new->core.background_pixel)
	XSetForeground(XtDisplay(new), new->paint.igc,
		       new->core.background_pixel);

    if (cur->paint.cursor != new->paint.cursor)
	XDefineCursor(XtDisplay(new), XtWindow(new), new->paint.cursor);

    /*
     * Number of undo levels changed.
     * Free old undo buffers and undo/redo stacks,
     * and allocate new ones. Catch: we need to preserve
     * the currently used undo buffer, otherwise we'll
     * lose everything on the canvas.
     */
    if (cur->paint.undoSize != new->paint.undoSize) {
	UndoStack * this, * nxt, * first;
	
	this = cur->paint.head;
	do {
	    nxt = this->next;
	    if (this->pixmap != cur->paint.current)
		XFreePixmap(XtDisplay(new), this->pixmap);
	    free(this);
	    this = nxt;
	} while (this != cur->paint.head);
	new->paint.head = NULL;
	for (i = 0; i <= new->paint.undoSize; i++) {
	    this = undoBuffer(new);
	    if (i == 0) {
		first = this;
		this->pixmap = cur->paint.current;
	    } else {
		this->pixmap = XCreatePixmap(XtDisplay(new), XtWindow(new),
					     new->paint.drawWidth,
					     new->paint.drawHeight,
					 DefaultDepthOfScreen(XtScreen(new)));
		XFillRectangle(XtDisplay(new), this->pixmap,
			       new->paint.igc, 0, 0,
			       new->paint.drawWidth, new->paint.drawHeight);
	    }
	}
	first->next = new->paint.head;
	new->paint.current = cur->paint.current;
	new->paint.undo = new->paint.head;
	free(cur->paint.undostack);
	free(cur->paint.redostack);
	UndoInitialize(new, new->paint.undoSize);
    }	

    if (sizeChanged)
	XtCallCallbackList((Widget) new, new->paint.sizecalls, NULL);

    return ret;
}

static void
GetValuesHook(Widget w, ArgList args, Cardinal * nargs)
{
    PaintWidget pw = (PaintWidget) w;
    PaintWidget pp = (PaintWidget) pw->paint.paint;
    Arg a;
    int i;

    if (pp == None)
	return;

    for (i = 0; i < *nargs; i++) {
	char *nm = args[i].name;

	if (strcmp(nm, XtNlineWidth) == 0)
	    XtSetArg(a, XtNlineWidth, args[i].value);
	else if (strcmp(nm, XtNlineFillRule) == 0)
	    XtSetArg(a, XtNlineFillRule, args[i].value);
	else if (strcmp(nm, XtNfillRule) == 0)
	    XtSetArg(a, XtNfillRule, args[i].value);
	else if (strcmp(nm, XtNdrawWidth) == 0)
	    XtSetArg(a, XtNdrawWidth, args[i].value);
	else if (strcmp(nm, XtNdrawHeight) == 0)
	    XtSetArg(a, XtNdrawHeight, args[i].value);
	else
	    continue;
	XtGetValues((Widget) pp, &a, 1);
    }
}

static int
QueryGeometryProc(Widget w, XtWidgetGeometry * proposed,
		  XtWidgetGeometry * reply)
{

/*
**  Use the fact that the return values are ordered by "importance"
 */
#define SETRET(ret,x)	ret = (x > ret) ? x : ret
    PaintWidget pw = (PaintWidget) w;
    int width = pw->paint.drawWidth * pw->paint.zoom;
    int height = pw->paint.drawHeight * pw->paint.zoom;

    reply->request_mode = CWWidth | CWHeight;
    reply->width = width;
    reply->height = height;

    if (((proposed->request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight))
	&& (proposed->width == reply->width)
	&& (proposed->height == reply->height))
	return XtGeometryNo;
    else
	return XtGeometryAlmost;
}

/*
 * Convenience routine (with caching) to get an XImage of an area
 */
XImage *
PwGetImage(Widget w, XRectangle * rect)
{
    PaintWidget pw = (PaintWidget) w;
    Pixmap pix = GET_PIXMAP(pw);
    Region r;
    XRectangle clean;

    if (pw->paint.paint != None)
	pw = (PaintWidget) pw->paint.paint;


    if (rect != NULL) {
	if (rect->x < 0) {
	    clean.x = 0;
	    clean.width = rect->width + rect->x;
	} else {
	    clean.x = rect->x;
	    clean.width = rect->width;
	}
	if (rect->y < 0) {
	    clean.y = 0;
	    clean.height = rect->height + rect->y;
	} else {
	    clean.y = rect->y;
	    clean.height = rect->height;
	}
	if (clean.width > pw->paint.drawWidth)
	    clean.width = pw->paint.drawWidth;
	if (clean.height > pw->paint.drawHeight)
	    clean.height = pw->paint.drawHeight;
    } else {
	clean.x = 0;
	clean.y = 0;
	clean.width = pw->paint.drawWidth;
	clean.height = pw->paint.drawHeight;
    }

    if (pw->paint.image == NULL) {
	pw->paint.image = XCreateImage(XtDisplay(w), pw->paint.visual,
				       pw->core.depth, ZPixmap, 0, NULL,
				       pw->paint.drawWidth, pw->paint.drawHeight,
				       32, 0);
	pw->paint.image->data = (char *)
	    XtMalloc(pw->paint.drawHeight * pw->paint.image->bytes_per_line);
	pw->paint.imageRegion = XCreateRegion();
	pw->paint.invalidateRegion = False;
    }
    if (pw->paint.invalidateRegion) {
	XDestroyRegion(pw->paint.imageRegion);
	pw->paint.imageRegion = XCreateRegion();
	pw->paint.invalidateRegion = False;
    }
    if (XRectInRegion(pw->paint.imageRegion, clean.x, clean.y,
		      clean.width, clean.height) == RectangleIn)
	return pw->paint.image;

    r = XCreateRegion();
    XUnionRectWithRegion(&clean, r, r);
    XSubtractRegion(r, pw->paint.imageRegion, r);

    XClipBox(r, &clean);

    /*
    **	Since this is "caching" grow the image slightly over the
    **	  requested size.
     */
    if ((clean.x -= 8) < 0)
	clean.x = 0;
    if ((clean.y -= 8) < 0)
	clean.y = 0;
    if ((clean.width += 16) > pw->paint.drawWidth - clean.x)
	clean.width = pw->paint.drawWidth - clean.x;
    if ((clean.height += 16) > pw->paint.drawHeight - clean.y)
	clean.height = pw->paint.drawHeight - clean.y;

    XGetSubImage(XtDisplay(pw), pix, clean.x, clean.y,
		 clean.width, clean.height, AllPlanes, ZPixmap,
		 pw->paint.image, clean.x, clean.y);
    XUnionRectWithRegion(&clean, pw->paint.imageRegion,
			 pw->paint.imageRegion);

    XDestroyRegion(r);

    return pw->paint.image;
}

void
PwGetPixmap(Widget w, Pixmap * pix, int *width, int *height)
{
    PaintWidget pw = (PaintWidget) w;
    *pix = None;

    if (pw->paint.paint != None) {
	PwGetPixmap((Widget) pw->paint.paint, pix, width, height);
	return;
    }
    if (width != NULL)
	*width = pw->paint.drawWidth;
    if (height != NULL)
	*height = pw->paint.drawHeight;

    *pix = XCreatePixmap(XtDisplay(w), XtWindow(w),
			 pw->paint.drawWidth, pw->paint.drawHeight,
			 pw->core.depth);
    PwUpdateDrawable(w, *pix, NULL);
}

/*  Get the pixmap id of the current region image */
Pixmap
PwGetRawPixmap(Widget w)
{
    PaintWidget pw = (PaintWidget) w;

    if (pw->paint.paint != None)
	return PwGetRawPixmap((Widget) pw->paint.paint);

    return GET_PIXMAP(pw);
}

static void
pwSetPixmap(PaintWidget w, Pixmap pix, int flag)
{
    Window root;
    int x, y;
    unsigned int width, height, bw, depth;

    XGetGeometry(XtDisplay(w), pix,
		 &root, &x, &y, &width, &height, &bw, &depth);

    XCopyArea(XtDisplay(w), pix, GET_PIXMAP(w),
	      w->paint.gc, 0, 0, width, height, 0, 0);
}

static void
fatCallback(Widget parent, XtPointer w, XtPointer rectArg)
{
    XRectangle *rect = (XRectangle *) rectArg;
    PaintWidget pw = (PaintWidget) w;
    XExposeEvent event;
    int zoom = GET_ZOOM(pw);

    /*
    **	Make this look like an expose event on the fatbits region
     */
    event.x = (rect->x - pw->paint.zoomX) * zoom;
    event.y = (rect->y - pw->paint.zoomY) * zoom;
    event.width = rect->width * zoom;
    event.height = rect->height * zoom;

    realExposeProc(pw, &event, False);
}

/*
 * If rect is NULL, use undo box size.
 */
void
PwUpdate(Widget w, XRectangle * rect, Boolean force)
{
    PaintWidget pw = (PaintWidget) w;
    PaintWidget parent = (PaintWidget) pw->paint.paint;
    PaintWidget usePW = (parent == None) ? pw : parent;
    XRectangle all;

    if (rect == NULL) {
	if (pw->paint.undo == NULL) {
	    all.x = 0;
	    all.y = 0;
	    all.width = pw->core.width;
	    all.height = pw->core.height;
	    rect = &all;
	} else {
	    rect = &pw->paint.undo->box;
	}
    } else {
	all.x = rect->x - pw->paint.lineWidth;
	all.y = rect->y - pw->paint.lineWidth;
	all.width = rect->width + pw->paint.lineWidth * 2;
	all.height = rect->height + pw->paint.lineWidth * 2;
	rect = &all;
    }

    /*
    **	If we have a caching image
     */
    if (usePW->paint.image != NULL && !usePW->paint.invalidateRegion)
	usePW->paint.invalidateRegion = True;

    XtCallCallbackList((Widget) usePW, usePW->paint.fatcalls,
		       (XtPointer) rect);

    if (force || (pw->paint.zoom > 1) || parent != None)
	fatCallback((Widget) usePW, (XtPointer) usePW, (XtPointer) rect);
}

/*
**  Update the current visual buffer with the contents
**   of the undo area, thus a pseudo-real time undo.
**   (it's not undo at all)
**  Called from fontOp.c.
 */
void
PwUpdateFromLast(Widget w, XRectangle * rect)
{
    Pixmap pix;
    PaintWidget pw = (PaintWidget) w;
    Pixmap draw = GET_PIXMAP(pw);

    if (pw->paint.paint != None)
	pw = (PaintWidget) pw->paint.paint;

    pix = pw->paint.undo->pixmap;

    XCopyArea(XtDisplay(w), pix, draw, pw->paint.gc,
	      rect->x, rect->y, rect->width, rect->height,
	      rect->x, rect->y);
    PwUpdate(w, rect, True);
}

/*
**  Bogus XXX
 */
XRectangle *
PwScaleRectangle(Widget w, XRectangle * src)
{
    return src;
}

void
PwPutPixmap(Widget w, Pixmap pix)
{
    PaintWidget pw = (PaintWidget) w;
    GC gc = pw->paint.gc;
    Pixmap dst;
    Window root;
    int x, y;
    unsigned int width, height, bw, depth;
    XRectangle rect;

    XGetGeometry(XtDisplay(w), pix, &root, &x, &y, &width, &height, &bw, &depth);
    rect.x = 0;
    rect.y = 0;
    rect.width = width;
    rect.height = height;
    dst = PwUndoStart(w, &rect);
    XCopyArea(XtDisplay(pw), pix, dst, gc, 0, 0,
	      width, height, 0, 0);
    PwUpdate(w, &rect, True);
}

static void
removeChild(Widget child, Widget w)
{
    PaintWidget pw = (PaintWidget) w;
    int i;

    for (i = 0; i < pw->paint.paintChildrenSize; i++)
	if (pw->paint.paintChildren[i] == child)
	    break;
    for (i++; i < pw->paint.paintChildrenSize; i++)
	pw->paint.paintChildren[i - 1] = pw->paint.paintChildren[i];

    pw->paint.paintChildrenSize--;

    XtRemoveCallback((Widget) w, XtNfatBack, fatCallback, (XtPointer) child);
}

void
PwAddChild(Widget w, Widget child)
{
    PaintWidget pw = (PaintWidget) w;

    if (pw->paint.paintChildren == NULL)
	pw->paint.paintChildren = (Widget *)
	    XtMalloc(sizeof(Widget) * (pw->paint.paintChildrenSize + 2));
    else
	pw->paint.paintChildren = (Widget *)
	    XtRealloc((XtPointer) pw->paint.paintChildren,
		      sizeof(Widget) * (pw->paint.paintChildrenSize + 2));
    pw->paint.paintChildren[pw->paint.paintChildrenSize++] = child;
    XtAddCallback(child, XtNdestroyCallback, (XtCallbackProc) removeChild,
		  (XtPointer) w);
    XtAddCallback(w, XtNfatBack, fatCallback, (XtPointer) child);
}
