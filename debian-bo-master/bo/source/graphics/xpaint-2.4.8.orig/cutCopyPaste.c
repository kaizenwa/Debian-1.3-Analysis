/* +-------------------------------------------------------------------+ */
/* | Copyright 1993, David Koblas (koblas@netcom.com)		       | */
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

/* $Id: cutCopyPaste.c,v 1.10 1997/01/27 06:45:46 torsten Exp $ */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include "Paint.h"
#include "PaintP.h"
#include "xpaint.h"
#include "misc.h"
#include "menu.h"
#include "palette.h"
#include "image.h"
#include "operation.h"
#include "graphic.h"
#include "protocol.h"
#include "cutCopyPaste.h"

static Boolean selectionOwner = False;

static Image *(*lastFilter) (Image *);

/*
**  This really should be a "local" or malloced variable
 */
typedef struct {
    Pixmap pixmap;
    int count;
    int width, height, depth;
} selectInfo;

static void 
setToSelectOp(void)
{
    static String names[2] =
    {"selectBox", "selectArea"};
    OperationSet(names, 2);
}

static void 
selectionLost(Widget w, Atom * selection)
{
    selectionOwner = False;
}
static void 
selectionDone(Widget w, Atom * selection, Atom * target)
{
    /* empty */
}
static Boolean
selectionConvert(Widget w, Atom * selection, Atom * target,
		 Atom * type, XtPointer * value, unsigned long *len,
		 int *format)
{
    Pixmap src = Global.region.pix;
    Pixmap *pix, np;
    GC gc;
    int wth, hth, dth;

    if (src == None)
	return False;

    if (*target != XA_PIXMAP && *target != XA_BITMAP)
	return False;

    pix = XtNew(Pixmap);

    GetPixmapWHD(XtDisplay(w), src, &wth, &hth, &dth);
    np = XCreatePixmap(XtDisplay(w), XtWindow(w), wth, hth, dth);
    gc = XCreateGC(XtDisplay(w), np, 0, 0);
    XCopyArea(XtDisplay(w), src, np, gc, 0, 0, wth, hth, 0, 0);
    XFreeGC(XtDisplay(w), gc);

    *pix = np;

    *type = XA_PIXMAP;
    *len = 1;
    *format = 32;
    *value = (XtPointer) pix;

    return True;
}

void 
StdCopyCallback(Widget w, XtPointer paintArg, String * nm, XEvent * event)
{
    Widget paint = (Widget) paintArg;
    Pixmap pix, mask;
    int width, height;

    if (!PwRegionGet(paint, &pix, &mask))
	return;

    GetPixmapWHD(XtDisplay(paint), pix, &width, &height, NULL);

    if (Global.region.pix != None)
	XFreePixmap(XtDisplay(paint), Global.region.pix);
    if (Global.region.mask != None)
	XFreePixmap(XtDisplay(paint), Global.region.mask);

    Global.region.pix = pix;
    Global.region.mask = mask;
    Global.region.width = width;
    Global.region.height = height;

    XtVaGetValues(paint, XtNcolormap, &Global.region.cmap, NULL);

    selectionOwner = XtOwnSelection(paint, XA_PRIMARY, Global.currentTime,
			 selectionConvert, selectionLost, selectionDone);
}

static void 
stdPasteCB(Widget paint, XtPointer infoArg, Atom * selection, Atom * type,
	   XtPointer value, unsigned long *len, int *format)
{
    selectInfo *info = (selectInfo *) infoArg;
    Display *dpy = XtDisplay(paint);
    XRectangle rect;
    Pixmap pix;
    Colormap cmap;
    Pixmap newMask = None;
    GC gc;

    if (type != NULL) {
	info->count--;
	if (*type == XA_BITMAP) {
	    int wth, hth, dth;
	    Pixmap pix = *(Pixmap *) value;

	    GetPixmapWHD(dpy, pix, &wth, &hth, &dth);
	    if (info->pixmap == None ||
		info->depth < dth) {
		info->pixmap = pix;
		info->width = wth;
		info->height = hth;
		info->depth = dth;
	    }
	} else if (*type == XA_PIXMAP) {
	    int wth, hth, dth;
	    Pixmap pix = *(Pixmap *) value;

	    GetPixmapWHD(dpy, pix, &wth, &hth, &dth);
	    if (info->pixmap == None ||
		info->depth < dth) {
		info->pixmap = pix;
		info->width = wth;
		info->height = hth;
		info->depth = dth;
	    }
	}
	/*
	**  Are there more possible selections comming?
	 */
	if (info->count != 0)
	    return;

	/*
	**  Now that we have gotten all of the selections
	**    use the best one.
	 */
	if (info->pixmap != None) {
	    Pixmap np;
	    GC gc;

	    if (Global.region.pix != None)
		XFreePixmap(dpy, Global.region.pix);
	    if (Global.region.mask != None)
		XFreePixmap(dpy, Global.region.mask);
	    if (Global.region.image != NULL)
		ImageDelete((Image *) Global.region.image);

	    Global.region.pix = None;
	    Global.region.mask = None;
	    Global.region.image = NULL;

	    np = XCreatePixmap(dpy, XtWindow(paint),
			       info->width, info->height, info->depth);
	    gc = XCreateGC(dpy, np, 0, 0);
	    XCopyArea(dpy, info->pixmap, np, gc,
		      0, 0,
		      info->width, info->height,
		      0, 0);

	    XFreeGC(dpy, gc);

	    Global.region.width = info->width;
	    Global.region.height = info->height;
	    Global.region.pix = np;

	    if (info->depth == 1)
		Global.region.cmap = -1;
	    else
		Global.region.cmap = DefaultColormapOfScreen(XtScreen(paint));
	}
	XtFree((XtPointer) info);
    }
    /*
    **	No valid selections anywhere or we own the selection.
     */
    if (Global.region.pix == None && Global.region.image == NULL)
	return;

    rect.x = 0;
    rect.y = 0;
    rect.width = Global.region.width;
    rect.height = Global.region.height;

    if (Global.region.mask != None) {
	newMask = XCreatePixmap(dpy, XtWindow(paint), rect.width, rect.height, 1);
	gc = XCreateGC(dpy, newMask, 0, 0);
	XCopyArea(dpy, Global.region.mask, newMask, gc,
		  0, 0, rect.width, rect.height, 0, 0);
	XFreeGC(dpy, gc);
    } else if (Global.region.image != NULL &&
	       ((Image *) Global.region.image)->maskData != NULL) {
	newMask = ImageMaskToPixmap(paint, (Image *) Global.region.image);
    }
    XtVaGetValues(paint, XtNcolormap, &cmap, NULL);
    if (cmap != Global.region.cmap) {
	Image *image;
	Pixmap npix = None;

	if (rect.width * rect.height > 1024)
	    StateSetBusy(True);

	if (Global.region.pix == None) {
	    image = (Image *) Global.region.image;
	    image->refCount++;
	} else {
	    image = PixmapToImage(paint, Global.region.pix, Global.region.cmap);
	}

	ImageToPixmapCmap(image, paint, &npix, cmap);

	PwRegionSet(paint, &rect, npix, newMask);

	if (rect.width * rect.height > 1024)
	    StateSetBusy(False);
    } else {
	int depth;

	XtVaGetValues(paint, XtNdepth, &depth, NULL);
	pix = XCreatePixmap(dpy, XtWindow(paint), rect.width, rect.height, depth);
	gc = XtGetGC(paint, 0, 0);
	XCopyArea(dpy, Global.region.pix, pix, gc,
		  0, 0, rect.width, rect.height, 0, 0);
	XtReleaseGC(paint, gc);

	PwRegionSet(paint, &rect, pix, newMask);
    }

    setToSelectOp();
}

void 
StdPasteCallback(Widget w, XtPointer paintArg, XtPointer junk)
{
    Widget paint = (Widget) paintArg;

    if (!selectionOwner) {
	static Atom targets[2] =
	{XA_PIXMAP, XA_BITMAP};
	XtPointer data[2];
	selectInfo *info = XtNew(selectInfo);

	info->count = XtNumber(targets);
	info->pixmap = None;

	data[0] = (XtPointer) info;
	data[1] = (XtPointer) info;

	XtGetSelectionValues(paint, XA_PRIMARY, targets, 2,
			     stdPasteCB, data, Global.currentTime);
    } else {
	stdPasteCB(paint, NULL, NULL, NULL, 0, NULL, NULL);
    }
}

void 
StdClearCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    PwRegionClear(paint);
}

void 
StdCutCallback(Widget w, XtPointer paintArg, String * nm, XEvent * event)
{
    StdCopyCallback(w, paintArg, nm, event);
    StdClearCallback(w, paintArg, NULL);
}

void 
StdDuplicateCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    XRectangle rect;
    Widget paint = (Widget) paintArg;
    Pixmap pix, mask;
    int width, height;

    if (!PwRegionGet(paint, &pix, &mask))
	return;

    GetPixmapWHD(XtDisplay(paint), pix, &width, &height, NULL);

    rect.x = 0;
    rect.y = 0;
    rect.width = width;
    rect.height = height;

    PwRegionSet(paint, &rect, pix, mask);
}

void 
StdSelectAllCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    XRectangle rect;
    int dw, dh;

    XtVaGetValues(paint, XtNdrawWidth, &dw, XtNdrawHeight, &dh, NULL);

    rect.x = 0;
    rect.y = 0;
    rect.width = dw;
    rect.height = dh;

    PwRegionSet(paint, &rect, None, None);
    setToSelectOp();
}

void 
StdUndoCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    PaintWidget pw = (PaintWidget) paintArg;
    int haveRegion = 0;
    XRectangle *o, *r;

    /* Only fiddle with the region if it has not been moved or resized */
    o = &pw->paint.region.orig;
    r = &pw->paint.region.rect;
    if ((o->x == r->x) && (o->y == r->y) &&
	(o->width == r->width) && (o->height == r->height))
	haveRegion = PwRegionOff((Widget) pw, True);

    Undo((Widget) pw);
    if (haveRegion)
	PwRegionSet((Widget) pw, &pw->paint.region.rect, None, None);
}

void 
StdRedoCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    PaintWidget pw = (PaintWidget) paintArg;
    int haveRegion = 0;
    XRectangle *o, *r;

    /* Only fiddle with the region if it has not been moved or resized */
    o = &pw->paint.region.orig;
    r = &pw->paint.region.rect;
    if ((o->x == r->x) && (o->y == r->y) &&
	(o->width == r->width) && (o->height == r->height))
	haveRegion = PwRegionOff((Widget) pw, True);

    Redo((Widget) pw);
    if (haveRegion)
	PwRegionSet((Widget) pw, &pw->paint.region.rect, None, None);
}

void 
ClipboardSetImage(Widget w, Image * image)
{
    if (Global.region.pix != None)
	XFreePixmap(XtDisplay(w), Global.region.pix);
    if (Global.region.mask != None)
	XFreePixmap(XtDisplay(w), Global.region.mask);
    if (Global.region.image != NULL)
	ImageDelete(Global.region.image);

    Global.region.pix = None;
    Global.region.mask = None;
    Global.region.cmap = None;
    Global.region.image = (void *) image;
    Global.region.width = image->width;
    Global.region.height = image->height;
}

/*
**  End of "edit" menu function, start region menu functions.
**
 */

void 
StdRegionFlipX(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    float v = -1.0;

    PwRegionAddScale(paint, &v, NULL);
}

void 
StdRegionFlipY(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    float v = -1.0;

    PwRegionAddScale(paint, NULL, &v);
}

/*
** This is the function that calls the actual image processing functions.
 */
static Pixmap ImgProcessPix;
static Colormap ImgProcessCmap;
static int ImgProcessFlag;
Image *
ImgProcessSetup(Widget paint)
{
    Image *in;
    Pixel bg;
    Palette *pal;
    extern struct imageprocessinfo ImgProcessInfo;


    StateSetBusy(True);

    ImgProcessFlag = 0;
    if (!PwRegionGet(paint, &ImgProcessPix, None)) {
#ifdef FILTERNEEDSSELECTION
	StateSetBusy(False);
	return NULL;			/* No region selected */
#else
	PaintWidget pw = (PaintWidget) paint;

	ImgProcessPix = GET_PIXMAP(pw);
	ImgProcessFlag = 1;
#endif
    }
    PwRegionTear(paint);
    XtVaGetValues(paint, XtNcolormap, &ImgProcessCmap, NULL);

    XtVaGetValues(paint, XtNbackground, &bg, NULL);
    pal = PaletteFind(paint, ImgProcessCmap);
    ImgProcessInfo.background = PaletteLookup(pal, bg);

    in = PixmapToImage(paint, ImgProcessPix, ImgProcessCmap);
    StateSetBusy(False);
    return in;
}

Image *
ImgProcessFinish(Widget paint, Image * in, Image * (*func) (Image *))
{
    Image *out;

    StateSetBusy(True);
    out = func(in);
    if (out != in)		/* delete input unless done in place */
	ImageDelete(in);

    ImageToPixmapCmap(out, paint, &ImgProcessPix, ImgProcessCmap);

    if (!ImgProcessFlag)
	PwRegionSetRawPixmap(paint, ImgProcessPix);
    else {
	PwUpdate(paint, NULL, True);
	XtVaSetValues(paint, XtNdirty, True, NULL);
    }

    StateSetBusy(False);

    return out;
}

static void 
stdImgProcess(Widget paint, Image * (*func) (Image *))
{
    Image *in, *out;

    lastFilter = func;
    EnableLast(paint);

    if ((in = ImgProcessSetup(paint)) == NULL)
	return;
    out = ImgProcessFinish(paint, in, func);
}

void 
StdLastImgProcess(Widget paint)
{
    if (lastFilter != NULL)
	stdImgProcess(paint, lastFilter);
}

void 
StdRegionInvert(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    stdImgProcess(paint, ImageInvert);
}

void 
StdRegionSharpen(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    stdImgProcess(paint, ImageSharpen);
}

void 
StdRegionSmooth(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    stdImgProcess(paint, ImageSmooth);
}

void 
StdRegionEdge(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    stdImgProcess(paint, ImageEdge);
}

void 
StdRegionEmbose(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    stdImgProcess(paint, ImageEmbose);
}

void 
StdRegionOilPaint(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    stdImgProcess(paint, ImageOilPaint);
}

void 
StdRegionAddNoise(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageAddNoise);
}

void 
StdRegionSpread(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageSpread);
}

void 
StdRegionBlend(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageBlend);
}

void 
StdRegionPixelize(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImagePixelize);
}

void 
StdRegionDespeckle(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageDespeckle);
}

void 
StdRegionNormContrast(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageNormContrast);
}

void 
StdRegionSolarize(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageSolarize);
}

void 
StdRegionQuantize(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageQuantize);
}

void 
StdRegionGrey(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageGrey);
}


void 
StdRegionDirFilt(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageDirectionalFilter);
}

#ifdef FEATURE_TILT
void 
StdRegionTilt(Widget w, XtPointer paintArg, XtPointer junk2)
{
    stdImgProcess((Widget) paintArg, ImageTilt);
}

#endif

/*
**  Start callback functions
 */
static void 
prCallback(Widget paint, Widget w, Boolean flag)
{
    XtVaSetValues(w, XtNsensitive, flag, NULL);
}

static void 
addFunction(Widget item, Widget paint, XtCallbackProc func)
{
    XtAddCallback(item, XtNcallback, func, (XtPointer) paint);
    XtAddCallback(paint, XtNregionCallback, (XtCallbackProc) prCallback,
		  (XtPointer) item);
    XtVaSetValues(item, XtNsensitive, (XtPointer) False, NULL);
}

void 
ccpAddUndo(Widget w, Widget paint)
{
    XtAddCallback(w, XtNcallback, StdUndoCallback, (XtPointer) paint);
}

void 
ccpAddRedo(Widget w, Widget paint)
{
    XtAddCallback(w, XtNcallback, StdRedoCallback, (XtPointer) paint);
}

void 
ccpAddCut(Widget w, Widget paint)
{
    addFunction(w, paint, (XtCallbackProc) StdCutCallback);
}

void 
ccpAddCopy(Widget w, Widget paint)
{
    addFunction(w, paint, (XtCallbackProc) StdCopyCallback);
}

void 
ccpAddPaste(Widget w, Widget paint)
{
    XtVaSetValues(w, XtNsensitive, True, NULL);
    XtAddCallback(w, XtNcallback,
		  (XtCallbackProc) StdPasteCallback, (XtPointer) paint);
}

void 
ccpAddClear(Widget w, Widget paint)
{
    addFunction(w, paint, (XtCallbackProc) StdClearCallback);
}

void 
ccpAddDuplicate(Widget w, Widget paint)
{
    addFunction(w, paint, (XtCallbackProc) StdDuplicateCallback);
}

/*
**  Region functions
 */
void 
ccpAddSaveRegion(Widget w, Widget paint)
{
    addFunction(w, paint, (XtCallbackProc) StdSaveRegionFile);
}

/*
**  Standard poup menu
 */
static PaintMenuItem popupMenu[] =
{
    MI_SEPERATOR(),
#define P_UNDO_ITEM	1
    MI_SIMPLE("undo"),
#define P_REDO_ITEM	2
    MI_SIMPLE("redo"),
    MI_SEPERATOR(),  /* 3 */
#define P_CUT_ITEM	4
    MI_SIMPLE("cut"),
#define P_COPY_ITEM	5
    MI_SIMPLE("copy"),
#define P_PASTE_ITEM	6
    MI_SIMPLE("paste"),
#define P_CLEAR_ITEM	7
    MI_SIMPLE("clear"),
};

void 
ccpAddStdPopup(Widget paint)
{
    MenuPopupCreate(XtParent(paint), XtNumber(popupMenu), popupMenu);

    ccpAddUndo(popupMenu[P_UNDO_ITEM].widget, paint);
    ccpAddRedo(popupMenu[P_REDO_ITEM].widget, paint);
    ccpAddCut(popupMenu[P_CUT_ITEM].widget, paint);
    ccpAddCopy(popupMenu[P_COPY_ITEM].widget, paint);
    ccpAddPaste(popupMenu[P_PASTE_ITEM].widget, paint);
    ccpAddClear(popupMenu[P_CLEAR_ITEM].widget, paint);
}
