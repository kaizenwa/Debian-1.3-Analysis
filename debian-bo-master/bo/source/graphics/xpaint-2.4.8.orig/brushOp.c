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

/* $Id: brushOp.c,v 1.11 1996/11/01 09:37:12 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <stdlib.h>
#include <xpm.h>

#include "xpaint.h"
#include "misc.h"
#include "Paint.h"
#include "PaintP.h"
#include "palette.h"
#include "graphic.h"
#include "protocol.h"
#include "image.h"
#include "ops.h"

#include "bitmaps/paintA.xpm"
#include "bitmaps/paintB.xpm"
#include "bitmaps/paintC.xpm"
#include "bitmaps/paintD.xpm"
#include "bitmaps/paintE.xpm"
#include "bitmaps/paintF.xpm"
#include "bitmaps/paintG.xpm"
#include "bitmaps/paintH.xpm"
#include "bitmaps/paintI.xpm"
#include "bitmaps/paintJ.xpm"
#include "bitmaps/paintK.xpm"
#include "bitmaps/paintL.xpm"
#include "bitmaps/paintM.xpm"
#include "bitmaps/paintN.xpm"
#include "bitmaps/paintO.xpm"
#include "bitmaps/paintP.xpm"
#include "bitmaps/paintQ.xpm"
#include "bitmaps/paintR.xpm"
#include "bitmaps/paintS.xpm"
#include "bitmaps/paintT.xpm"

#define BRUSH(name) (char **) CONCAT(name, _xpm)

#define BLEND(a, b, x)	((x)*(b) + (1.0-(x))*(a))

typedef struct {
    Pixmap pixmap;
    Cursor cursor;
    char **bits;
    int width, height;
    char *brushbits;
    int numpixels;		/* Total number of set pixels */
} BrushItem;

static BrushItem brushList[] =
{
    {None, None, BRUSH(paintA)},
    {None, None, BRUSH(paintB)},
    {None, None, BRUSH(paintC)},
    {None, None, BRUSH(paintD)},
    {None, None, BRUSH(paintE)},
    {None, None, BRUSH(paintF)},
    {None, None, BRUSH(paintG)},
    {None, None, BRUSH(paintH)},
    {None, None, BRUSH(paintI)},
    {None, None, BRUSH(paintJ)},
    {None, None, BRUSH(paintK)},
    {None, None, BRUSH(paintL)},
    {None, None, BRUSH(paintM)},
    {None, None, BRUSH(paintN)},
    {None, None, BRUSH(paintO)},
    {None, None, BRUSH(paintP)},
    {None, None, BRUSH(paintQ)},
    {None, None, BRUSH(paintR)},
    {None, None, BRUSH(paintS)},
    {None, None, BRUSH(paintT)}
};

static XpmColorSymbol monoColorSymbols[5] =
{
    {"A", NULL, 0},
    {"B", NULL, 1},
    {"C", NULL, 1},
    {"D", NULL, 1},
    {"E", NULL, 1}
};

typedef enum {
    ERASE, SMEAR, PLAIN
} BrushType;

typedef struct {
    Boolean useSecond;
    BrushType brushtype;
    Pixmap pixmap;
    int width, height;
    Palette *brushPalette;
} LocalInfo;

static BrushItem *currentBrush = NULL;
static Boolean eraseMode = True;
static Boolean transparentMode = False;
static float brushOpacity = 0.2;
static XImage *brushImage;

static void smear(Widget w, OpInfo * info, GC gc, LocalInfo * l, int sx, int sy);
static void wbrush(Widget w, OpInfo * info, GC gc,
		   LocalInfo * l, int sx, int sy);

static void 
draw(Widget w, OpInfo * info, LocalInfo * l, int x, int y)
{
    XRectangle undo;
    int sx = x - l->width / 2;
    int sy = y - l->height / 2;
    GC gc;

    if (l->brushtype == ERASE)
	gc = info->base_gc;
    else
	gc = l->useSecond ? info->second_gc : info->first_gc;

    XSetClipOrigin(XtDisplay(w), gc, sx, sy);

    if ((l->brushtype == ERASE) && eraseMode && (info->base != None)) {
	XCopyArea(XtDisplay(w), info->base, info->drawable,
		  gc, sx, sy, l->width, l->height, sx, sy);
    } else if (l->brushtype == SMEAR) {
	smear(w, info, gc, l, sx, sy);
    } else if (transparentMode) {
	wbrush(w, info, gc, l, sx, sy);
    } else {			/* plain opaque brush */
	XFillRectangle(XtDisplay(w), info->drawable,
		       gc, sx, sy, l->width, l->height);
    }

    if (info->surface == opPixmap) {
	XYtoRECT(sx, sy, sx + l->width, sy + l->height, &undo);
	UndoGrow(w, sx, sy);
	UndoGrow(w, sx + l->width, sy + l->height);
	PwUpdate(w, &undo, False);
    }
}

static void 
press(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    /*
    **  Check to make sure all buttons are up, before doing this
     */
    if ((event->state & (Button1Mask | Button2Mask | Button3Mask |
			 Button4Mask | Button5Mask)) != 0)
	return;

    if (info->surface == opWindow && info->isFat)
	return;

    l->useSecond = event->button == Button2;
    l->width = currentBrush->width;
    l->height = currentBrush->height;

    XSetClipMask(XtDisplay(w), info->first_gc, l->pixmap);
    XSetClipMask(XtDisplay(w), info->second_gc, l->pixmap);
    XSetClipMask(XtDisplay(w), info->base_gc, l->pixmap);

    UndoStart(w, info);

    draw(w, info, l, event->x, event->y);
}

static void 
motion(Widget w, LocalInfo * l, XMotionEvent * event, OpInfo * info)
{
    if (!event->state || ((info->surface == opWindow) && info->isFat))
	return;
    draw(w, info, l, event->x, event->y);
}

static void 
release(Widget w, LocalInfo * l, XButtonEvent * event, OpInfo * info)
{
    int mask;
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

    XSetClipMask(XtDisplay(w), info->first_gc, None);
    XSetClipMask(XtDisplay(w), info->second_gc, None);
    XSetClipMask(XtDisplay(w), info->base_gc, None);
}

/*
** drawing routine for smear operator
 */
static void 
smear(Widget wid, OpInfo * info, GC gc, LocalInfo * l, int sx, int sy)
{
    int x, y, n, xx, yy, w, h, d, m;
    unsigned long r, g, b;
    unsigned char *brushbits;
    Pixel p;
    Display *dpy = XtDisplay(wid);
    XColor *col, newcol;

    /*
     * Perform manual clipping to avoid XPutImage crashing on us
     */
    xx = yy = 0;
    w = l->width;
    h = l->height;
    if (sx < 0) {
	xx = -sx;
	w += sx;
	sx = 0;
    } else if ((d = (sx + w - ((PaintWidget) wid)->paint.drawWidth)) > 0)
	w -= d;
    if (sy < 0) {
	yy = -sy;
	h += sy;
	sy = 0;
    } else if ((d = (sy + h - ((PaintWidget) wid)->paint.drawHeight)) > 0)
	h -= d;
    if ((w <= 0) || (h <= 0))
	return;


    /* copy portion of image under brush into brushImage */
    XGetSubImage(dpy, info->drawable, sx, sy, w, h,
		 AllPlanes, ZPixmap, brushImage, xx, yy);

    /* compute average of pixels inside brush */
    r = g = b = 0;
    brushbits = (unsigned char *) currentBrush->brushbits;
    for (y = 0; y < l->height; ++y)
	for (x = 0; x < l->width; ++x)
	    if (*brushbits++) {
		p = XGetPixel(brushImage, x, y);
		col = PaletteLookup(l->brushPalette, p);
		r += col->red;
		g += col->green;
		b += col->blue;
	    }
    n = currentBrush->numpixels;
    r = r / 256 / n;
    g = g / 256 / n;
    b = b / 256 / n;

    /* now blend each surface pixel with average */
    brushbits = (unsigned char *) currentBrush->brushbits;
    for (y = 0; y < l->height; ++y)
	for (x = 0; x < l->width; ++x) {
	    if ((m = *brushbits++) != 0) {
		float mix = m / 5.0;

		p = XGetPixel(brushImage, x, y);
		col = PaletteLookup(l->brushPalette, p);
		newcol.red = 256 * BLEND(col->red / 256, r, mix);
		newcol.green = 256 * BLEND(col->green / 256, g, mix);
		newcol.blue = 256 * BLEND(col->blue / 256, b, mix);
		p = PaletteAlloc(l->brushPalette, &newcol);
		XPutPixel(brushImage, x, y, p);
	    }
	}
    XPutImage(dpy, info->drawable, gc, brushImage, xx, yy, sx, sy, w, h);
}


/*
** drawing routine for transparent brush
 */
static void 
wbrush(Widget wid, OpInfo * info, GC gc, LocalInfo * l, int sx, int sy)
{
    int x, y, xx, yy, m, w, h, d;
    unsigned long r, g, b;
    unsigned char *brushbits;
    Pixel p;
    Display *dpy = XtDisplay(wid);
    XColor *col, newcol;
    XGCValues gcval;


    /*
     * Perform manual clipping to avoid XPutImage crashing on us
     */
    xx = yy = 0;
    w = l->width;
    h = l->height;
    if (sx < 0) {
	xx = -sx;
	w += sx;
	sx = 0;
    } else if ((d = (sx + w - ((PaintWidget) wid)->paint.drawWidth)) > 0)
	w -= d;
    if (sy < 0) {
	yy = -sy;
	h += sy;
	sy = 0;
    } else if ((d = (sy + h - ((PaintWidget) wid)->paint.drawHeight)) > 0)
	h -= d;
    if ((w <= 0) || (h <= 0))
	return;

    /* copy portion of image under brush into brushImage */
    XGetSubImage(dpy, info->drawable, sx, sy, w, h,
		 AllPlanes, ZPixmap, brushImage, xx, yy);

    /* get current colour */
    XGetGCValues(dpy, gc, GCForeground, &gcval);
    col = PaletteLookup(l->brushPalette, gcval.foreground);
    r = col->red / 256;
    g = col->green / 256;
    b = col->blue / 256;
    brushbits = (unsigned char *) currentBrush->brushbits;
    for (y = 0; y < l->height; ++y)
	for (x = 0; x < l->width; ++x)
	    if ((m = *brushbits++) != 0) {
		float mix = m / 5.0 * brushOpacity;

		p = XGetPixel(brushImage, x, y);
		col = PaletteLookup(l->brushPalette, p);
		newcol.red = 256 * BLEND(col->red / 256, r, mix);
		newcol.green = 256 * BLEND(col->green / 256, g, mix);
		newcol.blue = 256 * BLEND(col->blue / 256, b, mix);
		p = PaletteAlloc(l->brushPalette, &newcol);
		XPutPixel(brushImage, x, y, p);
	    }
    XPutImage(dpy, info->drawable, gc, brushImage, xx, yy, sx, sy, w, h);
}


static void 
setPixmap(Widget w, void *brushArg)
{
    BrushItem *brush = (BrushItem *) brushArg;
    LocalInfo *l = (LocalInfo *) GraphicGetData(w);

    l->pixmap = brush->pixmap;
}

static void 
setCursor(Widget wid, void *brushArg)
{
    static Boolean inited = False;
    static XColor xcols[2];
    BrushItem *brush = (BrushItem *) brushArg;
    Display *dpy = XtDisplay(wid);
    PaintWidget paint = (PaintWidget) wid;

    if (!inited) {
	Colormap map;
	Screen *screen = XtScreen(wid);

	inited = True;
	xcols[0].pixel = WhitePixelOfScreen(screen);
	xcols[1].pixel = BlackPixelOfScreen(screen);

	XtVaGetValues(wid, XtNcolormap, &map, NULL);

	XQueryColors(dpy, map, xcols, XtNumber(xcols));
    }
    if (brush->cursor == None) {
	Pixmap source, mask;
	XImage *src, *msk;
	GC gc;
	int x, y, w, h, ow, oh, n;
	unsigned char *brushbits;
	XpmAttributes xpmAttr;
	static XpmColorSymbol colorsymbols[5] =
	{
	    {"A", NULL, 0},
	    {"B", NULL, 1},
	    {"C", NULL, 2},
	    {"D", NULL, 3},
	    {"E", NULL, 4}
	};

	ow = brush->width;
	oh = brush->height;
	w = ow + 2;
	h = oh + 2;		/* add 1 pixel border for mask */

	/* get full depth pixmap */
	xpmAttr.valuemask = XpmColorSymbols;
	xpmAttr.numsymbols = 5;
	xpmAttr.colorsymbols = colorsymbols;
	XpmCreatePixmapFromData(dpy, RootWindowOfScreen(XtScreen(wid)),
				brush->bits, &source, NULL, &xpmAttr);
	GetPixmapWHD(dpy, source, NULL, NULL, (int *) &xpmAttr.depth);
	src = NewXImage(dpy, NULL, xpmAttr.depth, w, h);
	memset(src->data, 0, src->bytes_per_line * h);

	/* copy colour pixmap to center of XImage */
	XGetSubImage(dpy, source, 0, 0, ow, oh, AllPlanes, ZPixmap, src, 1, 1);
	msk = NewXImage(dpy, NULL, 1, w, h);
	brushbits = (unsigned char *) xmalloc(ow * oh);
	brush->brushbits = (char *) brushbits;
	
	n = 0;
	for (y = 0; y < h; y++)
	    for (x = 0; x < w; x++) {
		Pixel p = XGetPixel(src, x, y);

		if ((y != 0) && (y != h - 1) && (x != 0) && (x != w - 1))
		    *brushbits++ = p;

		if (p)
		    ++n;

		if (!p && x > 0)
		    p = XGetPixel(src, x - 1, y);
		if (!p && x < w - 1)
		    p = XGetPixel(src, x + 1, y);
		if (!p && y > 0)
		    p = XGetPixel(src, x, y - 1);
		if (!p && y < h - 1)
		    p = XGetPixel(src, x, y + 1);
		XPutPixel(msk, x, y, p ? 1 : 0);
	    }
	XFreePixmap(dpy, source);
	XDestroyImage(src);
	brush->numpixels = n;

	source = XCreatePixmap(dpy, brush->pixmap, w, h, 1);
	mask = XCreatePixmap(dpy, brush->pixmap, w, h, 1);
	gc = XCreateGC(dpy, mask, 0, 0);
	XSetForeground(dpy, gc, 0);
	XFillRectangle(dpy, source, gc, 0, 0, w, h);
	XCopyArea(dpy, brush->pixmap, source, gc, 0, 0, ow, oh, 1, 1);
	XPutImage(dpy, mask, gc, msk, 0, 0, 0, 0, w, h);
	XDestroyImage(msk);
	XFreeGC(dpy, gc);

	brush->cursor = XCreatePixmapCursor(dpy, source, mask,
					    &xcols[1], &xcols[0],
					    w / 2, h / 2);
	XFreePixmap(dpy, source);
	XFreePixmap(dpy, mask);
    }
    FatCursorSet(wid, brush->pixmap);

    /* don't set cursor to brush shape if zoom is larger than 1 */
    if (paint->paint.zoom <= 1)
	XtVaSetValues(wid, XtNcursor, brush->cursor, NULL);
    else {
	SetCrossHairCursor(wid);
	FatCursorAddZoom(paint->paint.zoom, wid);
	XtAddCallback(wid, XtNdestroyCallback, FatCursorDestroyCallback, wid);
    }
}

/*
**  Those public functions
 */

Boolean
EraseGetMode(void)
{
    return eraseMode;
}

void
EraseSetMode(Boolean mode)
{
    eraseMode = mode;
}

void
BrushSetMode(Boolean mode)
{
    transparentMode = mode;
}

void
BrushSetParameters(float opacity)
{
    brushOpacity = opacity;
}

void *
BrushAdd(Widget w)
{
    static LocalInfo *l;
    Colormap cmap;

    l = XtNew(LocalInfo);

    XtVaSetValues(w, XtNcompress, False, NULL);

    l->brushtype = PLAIN;
    l->pixmap = currentBrush->pixmap;
    XtVaGetValues(w, XtNcolormap, &cmap, NULL);
    l->brushPalette = PaletteFind(w, cmap);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask,
		      FALSE, (OpEventProc) press, (XtPointer) l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonMotionMask,
		      FALSE, (OpEventProc) motion, (XtPointer) l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask,
		      FALSE, (OpEventProc) release, (XtPointer) l);

    setCursor(w, (void *) currentBrush);

    return l;
}

void
BrushRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask,
			 FALSE, (OpEventProc) press, (XtPointer) l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonMotionMask,
			 FALSE, (OpEventProc) motion, (XtPointer) l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask,
			 FALSE, (OpEventProc) release, (XtPointer) l);

    XtFree((XtPointer) l);
    FatCursorOff(w);
}

void *
EraseAdd(Widget w)
{
    LocalInfo *l = XtNew(LocalInfo);

    XtVaSetValues(w, XtNcompress, False, NULL);

    l->brushtype = ERASE;
    l->pixmap = currentBrush->pixmap;

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask,
		      FALSE, (OpEventProc) press, (XtPointer) l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonMotionMask,
		      FALSE, (OpEventProc) motion, (XtPointer) l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask,
		      FALSE, (OpEventProc) release, (XtPointer) l);

    setCursor(w, (void *) currentBrush);

    return l;
}

void
EraseRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask,
			 FALSE, (OpEventProc) press, (XtPointer) l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonMotionMask,
			 FALSE, (OpEventProc) motion, (XtPointer) l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask,
			 FALSE, (OpEventProc) release, (XtPointer) l);

    XtFree((XtPointer) l);
    FatCursorOff(w);
}

void *
SmearAdd(Widget w)
{
    LocalInfo *l = XtNew(LocalInfo);
    Colormap cmap;


    XtVaSetValues(w, XtNcompress, False, NULL);

    l->brushtype = SMEAR;
    l->pixmap = currentBrush->pixmap;
    XtVaGetValues(w, XtNcolormap, &cmap, NULL);
    l->brushPalette = PaletteFind(w, cmap);

    OpAddEventHandler(w, opWindow | opPixmap, ButtonPressMask,
		      FALSE, (OpEventProc) press, (XtPointer) l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonMotionMask,
		      FALSE, (OpEventProc) motion, (XtPointer) l);
    OpAddEventHandler(w, opWindow | opPixmap, ButtonReleaseMask,
		      FALSE, (OpEventProc) release, (XtPointer) l);

    setCursor(w, (void *) currentBrush);

    return l;
}

void
SmearRemove(Widget w, void *l)
{
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonPressMask,
			 FALSE, (OpEventProc) press, (XtPointer) l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonMotionMask,
			 FALSE, (OpEventProc) motion, (XtPointer) l);
    OpRemoveEventHandler(w, opWindow | opPixmap, ButtonReleaseMask,
			 FALSE, (OpEventProc) release, (XtPointer) l);

    XtFree((XtPointer) l);
    FatCursorOff(w);
}

/*
**  Initializer to create a default brush
 */
void
BrushInit(Widget toplevel)
{
    XpmAttributes xpmAttr;

    currentBrush = &brushList[0];

    /* force depth of one */
    xpmAttr.depth = 1;
    xpmAttr.colorsymbols = monoColorSymbols;
    xpmAttr.valuemask = XpmDepth | XpmColorSymbols;
    XpmCreatePixmapFromData(XtDisplay(toplevel),
			    RootWindowOfScreen(XtScreen(toplevel)),
			    currentBrush->bits, &currentBrush->pixmap,
			    NULL, &xpmAttr);
    currentBrush->width = xpmAttr.width;
    currentBrush->height = xpmAttr.height;

    brushImage = NewXImage(XtDisplay(toplevel), NULL,
			   DefaultDepthOfScreen(XtScreen(toplevel)),
			   currentBrush->width, currentBrush->height);
}

/*
**  The brush selection dialog
 */

static void
closePopup(Widget button, Widget shell)
{
    XtPopdown(shell);
}

static void
selectBrush(Widget shell, BrushItem * nc)
{
    currentBrush = nc;

    if ((CurrentOp->add == BrushAdd) ||
	(CurrentOp->add == EraseAdd) ||
	(CurrentOp->add == SmearAdd)) {
	GraphicAll(setCursor, (void *) currentBrush);
	GraphicAll(setPixmap, (void *) currentBrush);
    }
    if (brushImage != NULL)
	XDestroyImage(brushImage);
    brushImage = NewXImage(XtDisplay(shell), NULL,
			   DefaultDepthOfScreen(XtScreen(shell)),
			   currentBrush->width, currentBrush->height);
}

static Widget
createDialog(Widget w)
{
    Widget shell, form, box, icon, firstIcon = 0, close;
    GC gc;
    XGCValues values;
    int i;
    Pixel fg, bg;
    Pixmap pix;
    int nw, nh, ox, oy;
    XpmAttributes xpmAttr;

    shell = XtVaCreatePopupShell("brush",
				 topLevelShellWidgetClass, w,
				 NULL);

    form = XtVaCreateManagedWidget(NULL,
				   formWidgetClass, shell,
				   NULL);

    box = XtVaCreateManagedWidget("box",
				  boxWidgetClass, form,
				  NULL);

    values.foreground = WhitePixelOfScreen(XtScreen(w));
    values.background = BlackPixelOfScreen(XtScreen(w));

    gc = XCreateGC(XtDisplay(w),
		   RootWindowOfScreen(XtScreen(w)),
		   GCForeground | GCBackground, &values);

    values.background = WhitePixelOfScreen(XtScreen(w));
    values.foreground = BlackPixelOfScreen(XtScreen(w));

    for (i = 0; i < XtNumber(brushList); i++) {
	if (brushList[i].pixmap == None) {
	    /* force depth of one */
	    xpmAttr.depth = 1;
	    xpmAttr.colorsymbols = monoColorSymbols;
	    xpmAttr.valuemask = XpmDepth | XpmColorSymbols;
	    XpmCreatePixmapFromData(XtDisplay(box),
				    RootWindowOfScreen(XtScreen(box)),
				 brushList[i].bits, &brushList[i].pixmap,
				    NULL, &xpmAttr);
	    brushList[i].width = xpmAttr.width;
	    brushList[i].height = xpmAttr.height;
	}
	icon = XtVaCreateManagedWidget("icon",
				       toggleWidgetClass, box,
				       XtNradioGroup, firstIcon,
				       NULL);
	nw = brushList[i].width;
	nh = brushList[i].height;
	ox = oy = 0;
	if (nw < 16) {
	    ox = (16 - nw) / 2;
	    nw = 16;
	}
	if (nh < 16) {
	    oy = (16 - nh) / 2;
	    nh = 16;
	}
	pix = XCreatePixmap(XtDisplay(box),
			    RootWindowOfScreen(XtScreen(box)),
			    nw, nh,
			    DefaultDepthOfScreen(XtScreen(box)));

	XtVaGetValues(icon, XtNforeground, &fg, XtNbackground, &bg,
		      NULL);
	/*
	**  Clear then draw the clipped rectangle in
	 */
	XSetClipMask(XtDisplay(w), gc, None);
	XSetForeground(XtDisplay(w), gc, bg);
	XFillRectangle(XtDisplay(w), pix, gc, 0, 0, nw, nh);
	XSetClipMask(XtDisplay(w), gc, brushList[i].pixmap);
	XSetClipOrigin(XtDisplay(w), gc, ox, oy);
	XSetForeground(XtDisplay(w), gc, fg);
	XFillRectangle(XtDisplay(w), pix, gc, 0, 0, nw, nh);

	XtVaSetValues(icon, XtNbitmap, pix, NULL);

	if (firstIcon == NULL) {
	    XtVaSetValues(icon, XtNstate, True, NULL);
	    firstIcon = icon;
	}
	XtAddCallback(icon, XtNcallback,
	       (XtCallbackProc) selectBrush, (XtPointer) & brushList[i]);
    }

    close = XtVaCreateManagedWidget("close",
				    commandWidgetClass, form,
				    XtNfromVert, box,
				    XtNtop, XtChainBottom,
				    NULL);

    XtAddCallback(close, XtNcallback, (XtCallbackProc) closePopup,
		  (XtPointer) shell);

    XFreeGC(XtDisplay(w), gc);

    AddDestroyCallback(shell, (DestroyCallbackFunc) closePopup, shell);

    return shell;
}

void 
BrushSelect(Widget w)
{
    static Widget popup = NULL;

    if (popup == NULL)
	popup = createDialog(GetToplevel(w));

    XtPopup(popup, XtGrabNone);
    XMapRaised(XtDisplay(popup), XtWindow(popup));
}
