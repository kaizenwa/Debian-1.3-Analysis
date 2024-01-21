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

/* $Id: graphic.c,v 1.21 1996/09/27 06:17:31 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Paned.h>
#ifdef HAVE_COLTOG
#include "ColToggle.h"
#endif
#include <stdio.h>
#include <math.h>
#include <limits.h>

#ifndef M_PI
#define M_PI		3.14159265358979323846
#endif

#ifndef NOSTDHDRS
#include <stdlib.h>
#include <unistd.h>
#endif

/*
 * Undefine this if you want the Lookup button to be
 * inside the palette area (not recommended)
 */
#define LOOKUP_OUTSIDE

#include "Paint.h"
#include "PaintP.h"

#include "xpaint.h"
#include "palette.h"
#include "misc.h"
#include "menu.h"
#include "text.h"
#include "graphic.h"
#include "image.h"
#include "cutCopyPaste.h"
#include "operation.h"
#include "rc.h"
#include "protocol.h"
#include "color.h"
#include "ops.h"

#include "rw/rwTable.h"

/* Global data */
struct imageprocessinfo ImgProcessInfo =
{
    7,				/* oilArea		*/
    20,				/* noiseDelta		*/
    2,				/* spreadDistance	*/
    4,				/* pixelizeXSize	*/
    4,				/* pixelizeYSize	*/
    3,				/* despeckleMask	*/
    3,				/* smoothMaskSize	*/
    20,				/* tilt			*/
    50,				/* solarizeThreshold	*/
    99,				/* contrastW		*/
    2,				/* contrastB		*/
    16,				/* quantizeColors      */
    0, 0, 0, 0
};

#define PATTERN(name)	   \
	(char *)CONCAT(name,_bits), CONCAT(name,_width), CONCAT(name,_height)

struct paintWindows {
    Widget paint;
    struct paintWindows *next;
    void *ldata;
    Boolean done;
};

typedef struct {
    Widget paint;
    Palette *map;
    Widget primaryBox, secondaryBox;
    Widget primaryList, secondaryList;
    Widget first, second;
} LocalInfo;

/*
 *  Forward references
 */
static void makePaletteArea(LocalInfo *, RCInfo *);
static void editPatternCB(Widget, Widget);
static void deletePatternCB(Widget, Widget);

/*
 *  Begin menus
 */

static PaintMenuItem fileMenu[] =
{
#define SAVE_ITEM 0
    MI_SIMPLE("save"),
#define SAVEAS_ITEM 1
    MI_SIMPLE("saveas"),
#define SAVER_ITEM 2
    MI_SIMPLE("saveregion"),
#define SAVE_CONFIG 3
    MI_SIMPLE("saveconfig"),
#define LOAD_CONFIG 4
    MI_SIMPLE("loadconfig"),
#define REVERT_ITEM 5
    MI_SIMPLE("revert"),
#define CLOSE_ITEM 6
    MI_SIMPLE("close"),
};

static PaintMenuItem editMenu[] =
{
#define UNDO_ITEM	0
    MI_SIMPLE("undo"),
#define REDO_ITEM	1
    MI_SIMPLE("redo"),
    MI_SEPERATOR(),
#define CUT_ITEM	3
    MI_SIMPLE("cut"),
#define COPY_ITEM	4
    MI_SIMPLE("copy"),
#define PASTE_ITEM	5
    MI_SIMPLE("paste"),
#define CLEAR_ITEM	6
    MI_SIMPLE("clear"),
    MI_SEPERATOR(),
#define DUP_ITEM	8
    MI_SIMPLE("dup"),
#define SELECT_ALL_ITEM	9
    MI_SIMPLE("all"),
#if 0
    {"lock", NULL, NULL, MF_NONE, 0, NULL},
    {"unlock", NULL, NULL, MF_NONE, 0, NULL},
#endif
};

static PaintMenuItem rotateMenu[] =
{
    MI_SEPERATOR(),
    MI_SIMPLE("rotate1"),
    MI_SIMPLE("rotate2"),
    MI_SIMPLE("rotate3"),
    MI_SIMPLE("rotate4"),
    MI_SIMPLE("rotate5"),
};

static PaintMenuItem regionMenu[] =
{
#define RG_FLIPX_ITEM	0
    MI_SIMPLE("flipX"),
#define RG_FLIPY_ITEM	1
    MI_SIMPLE("flipY"),
    MI_SEPERATOR(),
#define RG_ROTATETO	3
    MI_RIGHT("rotateTo", XtNumber(rotateMenu), rotateMenu),
#define RG_ROTATE	4
    MI_SIMPLE("rotate"),
    MI_SEPERATOR(),		/* 5 */
#define RG_CROP		6
    MI_SIMPLE("crop"),
    MI_SEPERATOR(),		/* 7 */
#ifdef FEATURE_TILT
#define RG_TILT		8
    MI_SIMPLE("tilt"),
    MI_SEPERATOR(),		/* 9 */
#define RG_RESET	10
#else
#define RG_RESET	8
#endif
    MI_SIMPLE("reset"),
};

static PaintMenuItem filterMenu[] =
{
#define RG_LAST_ITEM	0
    MI_SIMPLE("last"),
    MI_SEPERATOR(),		/* 1 */
#define RG_INVERT_ITEM	2
    MI_SIMPLE("invert"),
#define RG_SHARPEN_ITEM	3
    MI_SIMPLE("sharpen"),
  /* remove noise */
    MI_SEPERATOR(),		/* 4 */
#define RG_SMOOTH_ITEM	5
    MI_SIMPLE("smooth"),
#define RG_DIRFILT_ITEM 6
    MI_SIMPLE("dirfilt"),
#define RG_DESPECKLE_ITEM 7
    MI_SIMPLE("despeckle"),
    MI_SEPERATOR(),		/* 8 */
#define RG_EDGE_ITEM	9
    MI_SIMPLE("edge"),
#define RG_EMBOSE_ITEM	10
    MI_SIMPLE("emboss"),
#define RG_OIL_ITEM	11
    MI_SIMPLE("oil"),
#define RG_NOISE_ITEM	12
    MI_SIMPLE("noise"),
#define RG_SPREAD_ITEM	13
    MI_SIMPLE("spread"),
#define RG_PIXELIZE_ITEM  14
    MI_SIMPLE("pixelize"),
  /* special fx */
#define RG_BLEND_ITEM	15
    MI_SIMPLE("blend"),
#define RG_SOLARIZE_ITEM 16
    MI_SIMPLE("solarize"),
  /* colour manipulation */
    MI_SEPERATOR(),		/* 17 */
#define RG_CONTRAST_ITEM 18
    MI_SIMPLE("contrast"),
#define RG_QUANTIZE_ITEM 19
    MI_SIMPLE("quantize"),
#define RG_TOGREY_ITEM 20
    MI_SIMPLE("togrey"),
};

static PaintMenuItem otherMenu[] =
{
#define	FAT_ITEM	0
    MI_SIMPLE("fatbits"),
#define	GRID_ITEM	1
    MI_FLAG("grid", MF_CHECK),
#define	SNAP_ITEM	2
    MI_FLAG("snap", MF_CHECK),
#define	CHSNAP_ITEM	3
    MI_SIMPLE("snapSpacing"),
    MI_SEPERATOR(),		/* 4 */
#define EDIT_BACKGROUND	5
    MI_SIMPLE("background"),
    MI_SEPERATOR(),		/* 6 */
#define	CHSIZE_ITEM	7
    MI_SIMPLE("size"),
#define UNDOSZ_ITEM	8
    MI_SIMPLE("undosize"),
#define	AUTOCROP_ITEM	9
    MI_SIMPLE("autocrop"),
#define	CHZOOM_ITEM	10
    MI_SIMPLE("zoom"),
};

static PaintMenuItem helpMenu[] =
{
    MI_SIMPLECB("help", HelpDialog, "canvas"),
};

static PaintMenuBar menuBar[] =
{
    {None, "file", XtNumber(fileMenu), fileMenu},
    {None, "edit", XtNumber(editMenu), editMenu},
    {None, "region", XtNumber(regionMenu), regionMenu},
    {None, "filter", XtNumber(filterMenu), filterMenu},
    {None, "other", XtNumber(otherMenu), otherMenu},
    {None, "help", XtNumber(helpMenu), helpMenu},
};

static PaintMenuItem paletteMenu[] =
{
    MI_SEPERATOR(),
    MI_SIMPLECB("edit", editPatternCB, NULL),
    MI_SIMPLECB("remove", deletePatternCB, NULL),
    MI_SEPERATOR(),
    MI_SIMPLECB("help", HelpDialog, "canvas.patBox"),
};


/*
 *  End of menus
 */

static struct paintWindows *head = NULL;

void
GraphicRemove(Widget paint, XtPointer junk, XtPointer junk2)
{
    struct paintWindows *cur = head, **prev = &head;

    while (cur != NULL && cur->paint != paint) {
	prev = &cur->next;
	cur = cur->next;
    }

    if (cur == NULL)
	return;

    *prev = cur->next;

    if (cur->done)
	CurrentOp->remove(cur->paint, cur->ldata);
    XtFree((XtPointer) cur);
}

static void
destroyCallback(Widget paint, XtPointer data, XtPointer junk2)
{
    LocalInfo *info = (LocalInfo *) data;
    Colormap cmap;
    Palette *map;

    XtVaGetValues(paint, XtNcolormap, &cmap, NULL);

    if ((map = PaletteFind(paint, cmap)) != NULL)
	PaletteDelete(map);
    XtFree((XtPointer) info);
}

static void
realize(Widget paint, XtPointer ldataArg, XEvent * event, Boolean * junk)
{
    struct paintWindows *cur = (struct paintWindows *) ldataArg;

    if (event->type == MapNotify) {
	XtRemoveEventHandler(paint, StructureNotifyMask, False, realize, ldataArg);
	if (CurrentOp != NULL && CurrentOp->add != NULL)
	    cur->ldata = CurrentOp->add(paint);
	cur->done = True;
    }
}

void
GraphicAdd(Widget paint)
{
    struct paintWindows *new = XtNew(struct paintWindows);

    new->next = head;
    head = new;
    new->paint = paint;
    new->ldata = NULL;
    new->done = False;

    if (XtIsRealized(paint)) {
	if (CurrentOp != NULL && CurrentOp->add != NULL)
	    new->ldata = CurrentOp->add(paint);
	new->done = True;
    } else
	XtAddEventHandler(paint, StructureNotifyMask, False,
			  realize, (XtPointer) new);

    XtAddCallback(paint, XtNdestroyCallback, GraphicRemove, NULL);
}


void
GraphicAll(GraphicAllProc func, void *data)
{
    struct paintWindows *cur;

    for (cur = head; cur != NULL; cur = cur->next) {
	if (!cur->done)
	    continue;
	func(cur->paint, data);
    }
}

void
GraphicSetOp(void (*stop) (Widget, void *), void *(*start) (Widget))
{
    struct paintWindows *cur;

    for (cur = head; cur != NULL; cur = cur->next) {
	if (stop != NULL)
	    stop(cur->paint, cur->ldata);
	if (start != NULL)
	    cur->ldata = start(cur->paint);
    }
}

void *
GraphicGetData(Widget w)
{
    struct paintWindows *cur;

    for (cur = head; cur != NULL; cur = cur->next)
	if (cur->paint == w)
	    return cur->ldata;
    return NULL;
}

/*
 *  Pattern creation and changing, utilities.
 */
#define PAT_MIN_WH	24

static int pattern_min_wh = 0;

typedef struct {
    Widget paint;
    Widget widget, widget2;
    int width, height;
    Pixmap pixmap, shownPixmap;
    Pixel pixel;
    LocalInfo *li;
} PatternInfo;

static void
patternUpdate(PatternInfo * info, Boolean isFirst)
{
    char *w_fr, *w_fg, *w_pat;

    if (isFirst) {
	w_fr = XtNfillRule;
	w_fg = XtNforeground;
	w_pat = XtNpattern;
    } else {
	w_fr = XtNlineFillRule;
	w_fg = XtNlineForeground;
	w_pat = XtNlinePattern;
    }

    if (info->pixmap == None) {
	XtVaSetValues(info->paint, w_fg, info->pixel,
		      w_fr, FillSolid,
		      NULL);
    } else {
	XtVaSetValues(info->paint, w_pat, info->pixmap,
		      w_fr, FillTiled,
		      NULL);
    }
}

static void
setPatternCallback(Widget icon, PatternInfo * info, XtPointer junk2)
{
    Widget rg;
    Boolean state;

    XtVaGetValues(icon, XtNstate, &state, XtNradioGroup, &rg, NULL);

    if (state == False)
	return;

    if (rg == info->li->primaryList || info->li->primaryList == icon)
	patternUpdate(info, True);
    else
	patternUpdate(info, False);
}

static void
freePatternInfo(Widget icon, PatternInfo * info)
{
    if (info->pixmap != info->shownPixmap)
	XFreePixmap(XtDisplay(icon), info->shownPixmap);
    if (info->pixmap != None)
	XFreePixmap(XtDisplay(icon), info->pixmap);

    XtFree((XtPointer) info);
}

static void
installPatternPixmap(PatternInfo * info, Pixmap pix)
{
    XtPointer curPrimary, curSecondary;

    curPrimary = XawToggleGetCurrent(info->li->primaryList);
    curSecondary = XawToggleGetCurrent(info->li->secondaryList);

    if (curPrimary == (XtPointer) info)
	XawToggleUnsetCurrent(info->li->primaryList);
    if (curSecondary == (XtPointer) info)
	XawToggleUnsetCurrent(info->li->secondaryList);

    XtVaSetValues(info->widget, XtNbitmap, pix, NULL);
    XtVaSetValues(info->widget2, XtNbitmap, pix, NULL);

    if (curPrimary == (XtPointer) info)
	XawToggleSetCurrent(info->li->primaryList, curPrimary);
    if (curSecondary == (XtPointer) info)
	XawToggleSetCurrent(info->li->secondaryList, curSecondary);
}

static void
editSolidOk(Widget paint, PatternInfo * info, XColor * col)
{
    LocalInfo *l = info->li;

    StateShellBusy(paint, False);

    if (col == NULL)
	return;

    if (!PaletteSetPixel(l->map, info->pixel, col)) {
	/*
	 *  This will fail for because we are on a static screen
	 *   if it is a TrueColor screen change the "icon"
	 */
	if (!l->map->isMapped) {
	    /*
	     *	TrueColor
	     */
	    Pixel p = PaletteAlloc(l->map, col);
	    Display *dpy = XtDisplay(paint);
	    GC gc = XCreateGC(dpy, XtWindow(paint), 0, 0);
	    Pixmap pix;
	    int depth;

	    XtVaGetValues(info->widget, XtNdepth, &depth, NULL);

	    pix = XCreatePixmap(dpy, DefaultRootWindow(dpy),
				pattern_min_wh, pattern_min_wh, depth);
	    XSetForeground(dpy, gc, p);
	    XFillRectangle(dpy, pix, gc, 0, 0, pattern_min_wh, pattern_min_wh);

	    info->pixel = p;
	    installPatternPixmap(info, pix);

	    XFreePixmap(dpy, info->shownPixmap);
	    info->shownPixmap = pix;

	    XFreeGC(dpy, gc);
	}
    }
}

static void
editPatternAction(Widget w, XEvent * event)
{
    PatternInfo *info;

    XtVaGetValues(w, XtNradioData, &info, NULL);

    if (info->pixmap == None) {
	LocalInfo *l = (LocalInfo *) info->li;

	if (!l->map->readonly || !l->map->isMapped) {
	    StateShellBusy(l->paint, True);

	    ColorEditor(l->paint, info->pixel, l->map, True,
			(XtCallbackProc) editSolidOk, (XtPointer) info);
	}
    } else {
	void PatternEdit(Widget, Pixmap, Widget);

	PatternEdit(info->paint, info->pixmap, w);
    }
}

static void
deletePatternCB(Widget mb, Widget w)
{
    LocalInfo *li;
    PatternInfo *info, *ainfo;
    Widget parent;
    WidgetList children;
    int nchild;
    Boolean setFirst = False;
    Boolean actPrimary = False, actSecondary = False;

    XtVaGetValues(w, XtNradioData, &info, NULL);

    /*
     *	Check to see if it is active
     */
    ainfo = (PatternInfo *) XawToggleGetCurrent(info->widget);
    if (info == ainfo) {
	XawToggleUnsetCurrent(info->widget);
	actPrimary = True;
    }
    ainfo = (PatternInfo *) XawToggleGetCurrent(info->widget2);
    if (info == ainfo) {
	XawToggleUnsetCurrent(info->widget2);
	actSecondary = True;
    }
    /*	
     *	Make sure there are enough widgets
     */
    XtVaGetValues(parent = XtParent(info->widget),
		  XtNnumChildren, &nchild,
		  XtNchildren, &children,
		  NULL);
    if (nchild == 1) {
	Notice(w, "You must have at least one entry");
	return;
    }
    li = info->li;
    if (info->widget == li->primaryList)
	setFirst = True;

    XtDestroyWidget(info->widget);
    XtDestroyWidget(info->widget2);

    if (setFirst) {
	XtVaGetValues(parent, XtNchildren, &children, NULL);
	if (children[0] == info->widget)
	    li->primaryList = children[1];
	else
	    li->primaryList = children[0];
	XtVaGetValues(li->primaryList, XtNradioData, &info, NULL);
	li->secondaryList = info->widget2;
    }
    if (actPrimary) {
	XtVaGetValues(li->primaryList, XtNradioData, &info, NULL);
	XawToggleSetCurrent(li->primaryList, (XtPointer) info);
    }
    if (actSecondary) {
	XtVaGetValues(li->secondaryList, XtNradioData, &info, NULL);
	XawToggleSetCurrent(li->secondaryList, (XtPointer) info);
    }
}

static void
editPatternCB(Widget mb, Widget w)
{
    editPatternAction(w, NULL);
}


/*
 *   Special trick so that during creation of the pattern box
 *    initialization can happen
 */
static LocalInfo *hiddenLocalInfo;

Widget
AddPattern(Widget group, Widget paint, Pixmap pix, Pixel pxl)
{
    static XtTranslations trans = None;
    static GC gc = None;
    static Boolean added = False;
    Widget parent, iconA, iconB, box;
    PatternInfo *info = XtNew(PatternInfo);
    Display *dpy = XtDisplay(group);
    int depth;
    char *nm = "pattern";
    char *str;
    XrmValue val;
    
    if (pattern_min_wh == 0)
	if ((XrmGetResource(XtDatabase(XtDisplay(paint)), "Xpaint.patternsize",
			    "XPaint", &str, &val) != 1) ||
	    (sscanf((char *) val.addr, "%d", &pattern_min_wh) != 1) ||
	    (pattern_min_wh < 4) || (pattern_min_wh > 64))
	    pattern_min_wh = PAT_MIN_WH;
    
    if (!added) {
	static XtActionsRec v =
	{"editPattern", (XtActionProc) editPatternAction};
	XtAppAddActions(XtWidgetToApplicationContext(paint), &v, 1);
	added = True;
    }
    if (trans == None)
	trans = XtParseTranslationTable
	    ("<BtnDown>,<BtnUp>: set() notify()\n<BtnDown>(2): editPattern()");

    if (XtClass(group) == toggleWidgetClass) {
	PatternInfo *tpi;

	parent = XtParent(group);
	XtVaGetValues(group, XtNradioData, &tpi, NULL);
	info->li = tpi->li;
#ifdef HAVE_COLTOG
    } else if (XtClass(group) == colToggleWidgetClass) {
	PatternInfo *tpi;

	parent = XtParent(group);
	XtVaGetValues(group, XtNradioData, &tpi, NULL);
	info->li = tpi->li;
#endif
    } else {
	parent = group;
	group = None;
	info->li = hiddenLocalInfo;
    }

    box = parent;

    info->paint = paint;

    if (pix != None) {
	GetPixmapWHD(dpy, pix, &info->width, &info->height, &depth);
	info->pixmap = pix;
	if (info->width == 1 && info->height == 1) {
	    XImage *xim = XGetImage(XtDisplay(paint), pix, 0, 0, 1, 1,
				    AllPlanes, ZPixmap);
	    info->pixel = XGetPixel(xim, 0, 0);
	    XDestroyImage(xim);

	    info->pixmap = None;
	    pix = None;
	}
    } else {
	info->pixel = pxl;
	info->pixmap = None;
	XtVaGetValues(parent, XtNdepth, &depth, NULL);
    }
    if (info->pixmap == None)
	nm = "solid";

    if (info->pixmap == None) {
	/* XXX Getting a read only GC, and writing to it */
	if (gc == None)
	    gc = XtGetGC(paint, 0, 0);

	pix = XCreatePixmap(dpy, DefaultRootWindow(dpy),
			    pattern_min_wh, pattern_min_wh, depth);
	XSetForeground(dpy, gc, info->pixel);
	XFillRectangle(dpy, pix, gc, 0, 0, pattern_min_wh, pattern_min_wh);
    } else if (info->width < pattern_min_wh || info->height < pattern_min_wh) {
	int nw = (info->width < pattern_min_wh) ? pattern_min_wh : info->width;
	int nh = (info->height < pattern_min_wh) ? pattern_min_wh : info->height;
	int x, y;

	if (gc == None)
	    gc = XtGetGC(paint, 0, 0);

	pix = XCreatePixmap(dpy, DefaultRootWindow(dpy), nw, nh, depth);

	for (y = 0; y < nh; y += info->height)
	    for (x = 0; x < nw; x += info->width)
		XCopyArea(dpy, info->pixmap, pix, gc,
			  0, 0, info->width, info->height, x, y);
    }
#ifdef HAVE_COLTOG
    if (info->pixmap == None) {
	iconA = XtVaCreateManagedWidget(nm, colToggleWidgetClass,
					info->li->primaryBox,
					XtNforeground, info->pixel,
				    XtNradioGroup, info->li->primaryList,
					XtNtranslations, trans,
					XtNradioData, info,
					XtNisSolid, True,
					NULL);
	iconB = XtVaCreateManagedWidget(nm, colToggleWidgetClass,
					info->li->secondaryBox,
					XtNforeground, info->pixel,
				  XtNradioGroup, info->li->secondaryList,
					XtNtranslations, trans,
					XtNradioData, info,
					XtNisSolid, True,
					NULL);
    } else {
#endif
	iconA = XtVaCreateManagedWidget(nm, toggleWidgetClass,
					info->li->primaryBox,
					XtNbitmap, pix,
				    XtNradioGroup, info->li->primaryList,
					XtNtranslations, trans,
					XtNradioData, info,
					NULL);
	iconB = XtVaCreateManagedWidget(nm, toggleWidgetClass,
					info->li->secondaryBox,
					XtNbitmap, pix,
				  XtNradioGroup, info->li->secondaryList,
					XtNtranslations, trans,
					XtNradioData, info,
					NULL);
#ifdef HAVE_COLTOG
    }
#endif

    if (info->li->primaryList == None)
	info->li->primaryList = iconA;
    if (info->li->secondaryList == None)
	info->li->secondaryList = iconB;

    XtAddCallback(iconA, XtNcallback, (XtCallbackProc) setPatternCallback,
		  (XtPointer) info);
    XtAddCallback(iconB, XtNcallback, (XtCallbackProc) setPatternCallback,
		  (XtPointer) info);

    paletteMenu[1].data = (void *) iconA;
    paletteMenu[2].data = (void *) iconA;
    MenuPopupCreate(iconA, XtNumber(paletteMenu), paletteMenu);
    paletteMenu[1].data = (void *) iconB;
    paletteMenu[2].data = (void *) iconB;
    MenuPopupCreate(iconB, XtNumber(paletteMenu), paletteMenu);

    info->widget = iconA;
    info->widget2 = iconB;

    info->shownPixmap = pix;
    XtAddCallback(iconA, XtNdestroyCallback, (XtCallbackProc) freePatternInfo,
		  (XtPointer) info);

    return iconA;
}

Widget
AddPatternInfo(void *piArg, Pixmap pix, Pixel pxl)
{
    PatternInfo *pi = (PatternInfo *) piArg;

    return AddPattern(pi->widget, pi->paint, pix, pxl);
}

void
ChangePattern(void *iArg, Pixmap pix)
{
    PatternInfo *info = (PatternInfo *) iArg;
    int depth;
    Display *dpy = XtDisplay(info->paint);
    Pixmap pfree1 = None, pfree2 = None;

    GetPixmapWHD(dpy, pix, &info->width, &info->height, &depth);

    if (info->pixmap != info->shownPixmap)
	pfree1 = info->shownPixmap;
    if (info->pixmap != None)
	pfree2 = info->pixmap;

    info->pixmap = pix;

    if (info->width < pattern_min_wh || info->height < pattern_min_wh) {
	int nw = (info->width < pattern_min_wh) ? pattern_min_wh : info->width;
	int nh = (info->height < pattern_min_wh) ? pattern_min_wh : info->height;
	int x, y;
	GC gc = XtGetGC(info->paint, 0, 0);

	pix = XCreatePixmap(dpy, DefaultRootWindow(dpy), nw, nh, depth);

	for (y = 0; y < nh; y += info->height)
	    for (x = 0; x < nw; x += info->width)
		XCopyArea(dpy, info->pixmap, pix, gc,
			  0, 0, info->width, info->height, x, y);
	XtReleaseGC(info->paint, gc);
    }
    installPatternPixmap(info, pix);

    info->shownPixmap = pix;

    if (pfree1 != None)
	XFreePixmap(dpy, pfree1);
    if (pfree2 != None)
	XFreePixmap(dpy, pfree2);
}

/*
 *  Save configuration 
 */
static void
saveConfigOkCallback(Widget il, LocalInfo * info, char *file)
{
    Widget parent = XtParent(info->primaryList);
    WidgetList children;
    int nchildren;
    int i;
    FILE *fd;
    Colormap cmap;

    XtVaGetValues(parent, XtNchildren, &children,
		  XtNnumChildren, &nchildren, NULL);
    XtVaGetValues(GetShell(parent), XtNcolormap, &cmap, NULL);

    if (nchildren == 0) {
	Notice(parent, "No information to save");
	return;
    }
    StateSetBusyWatch(True);

    if ((fd = fopen(file, "w")) == NULL) {
	Notice(parent, "Unable to open file '%s' for writing", file);
	return;
    }
    fprintf(fd, "reset\n\n");

    for (i = 0; i < nchildren; i++) {
	PatternInfo *pi;

	pi = NULL;
	XtVaGetValues(children[i], XtNradioData, &pi, NULL);

	if (pi == NULL)
	    continue;

	if (pi->pixmap == None) {
	    XColor col;
	    int r, g, b;

	    col.pixel = pi->pixel;
	    col.flags = DoRed | DoGreen | DoBlue;
	    XQueryColor(XtDisplay(parent), cmap, &col);
	    r = (col.red >> 8) & 0xff;
	    g = (col.green >> 8) & 0xff;
	    b = (col.blue >> 8) & 0xff;
	    fprintf(fd, "solid #%02x%02x%02x\n", r, g, b);
	} else {
	    Image *image;

	    fprintf(fd, "pattern BeginData\n");
	    image = PixmapToImage(info->paint, pi->pixmap, cmap);
	    WriteAsciiPNMfd(fd, image);
	    ImageDelete(image);
	    fprintf(fd, "\nEndData\n");
	}
    }

    fclose(fd);

    StateSetBusyWatch(False);
}

static void
saveConfigCallback(Widget w, LocalInfo * info, XtPointer junk)
{
    GetFileName(info->paint, 2, ".XPaintrc",
		(XtCallbackProc) saveConfigOkCallback, (XtPointer) info);
}

static void
loadConfigOkCallback(Widget il, LocalInfo * info, char *file)
{
    RCInfo *rcInfo = ReadRC(file);

    if (rcInfo == NULL) {
	Notice(info->paint, "Unable to open file %s", file);
	return;
    }
    makePaletteArea(info, rcInfo);

    FreeRC(rcInfo);
}

static void
loadConfigCallback(Widget w, LocalInfo * info, XtPointer junk)
{
    GetFileName(info->paint, 3, ".XPaintrc",
		(XtCallbackProc) loadConfigOkCallback, (XtPointer) info);
}

/*
 *  Pattern edit/add callback
 */
static void
addSolidOk(Widget w, LocalInfo * info, XColor * col)
{
    StateShellBusy(w, False);

    if (col != NULL) {
	Pixel pix = PaletteAlloc(info->map, col);

	AddPattern(info->primaryList, info->paint, None, pix);
    }
}

static void
addSolidCallback(Widget w, XtPointer wlArg, XtPointer junk)
{
    LocalInfo *info = (LocalInfo *) wlArg;
    Widget paint = info->paint;
    Pixel bg;

    StateShellBusy(paint, True);

    XtVaGetValues(paint, XtNbackground, &bg, NULL);

    ColorEditor(paint, bg, info->map, False,
		(XtCallbackProc) addSolidOk, (XtPointer) info);
}

static void
addPatternCallback(Widget w, XtPointer wlArg, XtPointer junk)
{
    void PatternEdit(Widget, Pixmap, Widget);
    LocalInfo *info = (LocalInfo *) wlArg;

    PatternEdit(info->paint, None, info->primaryList);
}

static void
lookupPatternCallback(Widget w, XtPointer infoArg, XtPointer junk2)
{
    LocalInfo *info = (LocalInfo *) infoArg;
    int nchildren, i;
    WidgetList children;
    Widget icon, list;
    Colormap cmap;
    Pixel p;
    PatternInfo *pi;

    DoGrabPixel(w, &p, &cmap);

    if (cmap != info->map->cmap) {
	XColor col;

	col.pixel = p;
	col.flags = DoRed | DoGreen | DoBlue;
	XQueryColor(XtDisplay(w), cmap, &col);
	if (!PaletteLookupColor(info->map, &col, &p))
	    p = PaletteAlloc(info->map, &col);
    }
#ifndef LOOKUP_OUTSIDE
    if (XtParent(w) == info->primaryBox)
	list = info->primaryList;
    else
	list = info->secondaryList;
#else
    if (XtNameToWidget(XtParent(w), "viewport2.patternRack") == info->primaryBox)
	list = info->primaryList;
    else
	list = info->secondaryList;
#endif

    XtVaGetValues(XtParent(list),
		  XtNnumChildren, &nchildren,
		  XtNchildren, &children,
		  NULL);

    for (i = 0; i < nchildren; i++) {
	pi = NULL;
	XtVaGetValues(children[i], XtNradioData, &pi, NULL);
	if (pi == NULL || pi->pixmap != None)
	    continue;

	if (pi->pixel == p) {
	    XawToggleSetCurrent(list, (XtPointer) pi);
	    return;
	}
    }

    icon = AddPattern(info->primaryList, info->paint, None, p);
    XtVaGetValues(icon, XtNradioData, &pi, NULL);
    XawToggleSetCurrent(list, (XtPointer) pi);
}

/*
 *  Convert RC file information into the pattern box info
 */
static void
makePaletteArea(LocalInfo * info, RCInfo * rcInfo)
{
    Widget firstIcon, icon;
    Widget pattern = info->secondaryBox;
    XColor col, rgb;
    int i, j;

    /*
     *	Allocate the color entries
     */
    rcInfo->colorFlags = (Boolean *) XtCalloc(sizeof(Boolean),
					      rcInfo->ncolors);
    rcInfo->colorPixels = (Pixel *) XtCalloc(sizeof(Pixel),
					     rcInfo->ncolors);
    for (i = 0; i < rcInfo->ncolors; i++)
	rcInfo->colorFlags[i] = False;

    for (i = 0; i < rcInfo->ncolors; i++) {
	if (XLookupColor(XtDisplay(info->paint), info->map->cmap,
			 rcInfo->colors[i], &col, &rgb) ||
	    XParseColor(XtDisplay(info->paint), info->map->cmap,
			rcInfo->colors[i], &col)) {
	    rcInfo->colorPixels[i] = PaletteAlloc(info->map, &col);
	    rcInfo->colorFlags[i] = True;
	    for (j = 0; j < i; j++)
		if (rcInfo->colorPixels[i] == rcInfo->colorPixels[j])
		    rcInfo->colorFlags[i] = False;
	}
    }

    /*
     *	Sneaky pass to AddPattern()
     */
    hiddenLocalInfo = info;

    firstIcon = pattern;

    for (i = 0; i < rcInfo->ncolors; i++) {
	if (!rcInfo->colorFlags[i])
	    continue;

	icon = AddPattern(firstIcon, info->paint, None, rcInfo->colorPixels[i]);

	if (firstIcon == pattern)
	    firstIcon = icon;
    }

    for (i = 0; i < rcInfo->nimages; i++) {
	Pixmap pix;

	rcInfo->images[i]->refCount++;
	pix = None;
	ImageToPixmapCmap(rcInfo->images[i], info->paint, &pix, info->map->cmap);

	icon = AddPattern(firstIcon, info->paint, pix, 0);

	if (firstIcon == pattern)
	    firstIcon = icon;
    }
}

/*
 *  First level menu callbacks.
 */

static void
closeOkCallback(Widget shell, XtPointer junk, XtPointer junk2)
{
    XtDestroyWidget(shell);
}

static void
genericCancelCallback(Widget shell, XtPointer junk, XtPointer junk2)
{
}

static void
closeCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    Boolean flg;
    XtVaGetValues(paint, XtNdirty, &flg, NULL);

    if (flg)
	AlertBox(GetShell(paint), "Unsaved changes!\nDo you really wish to close?",
		 closeOkCallback, genericCancelCallback, NULL);
    else
	XtDestroyWidget(GetShell(paint));
}

/*
 * Reload the image from the last saved file.
 */

/*
 * We need to use a work procedure, otherwise X gets confused.
 * Simplify a bit by assuming that there will never be more than
 * one revert process going on at any one time.
 */
static XtWorkProcId workProcId;
static int workProcDone;

static Boolean
workProc(XtPointer w)
{
    if (workProcDone)
	return True;

    /* this is a kluge, but it is necessary */
    workProcDone = 1;

    XtRemoveWorkProc(workProcId);

    XtDestroyWidget(GetShell((Widget) w));
    return True;
}


static void
doRevert(Widget w)
{
    void *v;
    char *file;
    Widget paint, top;
    int zoom, snap;
    Boolean snapon;
    Pixel background;
    WidgetList wlist;


    XtVaGetValues(w,
		  XtNzoom, &zoom, XtNsnap, &snap, XtNsnapOn, &snapon,
		  XtNbackground, &background, XtNmenuwidgets, &wlist,
		  XtNfilename, &file,
		  NULL);
    if ((file == NULL) || (*file == 0))
	return;

    StateSetBusy(True);

    top = GetToplevel(w);
    workProcDone = 0;
    workProcId = XtAppAddWorkProc(Global.appContext, workProc, (XtPointer) w);

    if ((v = ReadMagic(file)) != NULL)
	paint = GraphicOpenFileZoom(top, file, v, zoom);
    else {
	StateSetBusy(False);
	Notice(top, "Unable to open input file \"%s\"\n	%s",
	       file, RWGetMsg());
	return;
    }

    XtVaSetValues(paint, XtNsnapOn, snapon, XtNsnap, snap,
		  XtNbackground, background, XtNdirty, False, NULL);

    XtVaGetValues(paint, XtNmenuwidgets, &wlist, NULL);
    MenuCheckItem(wlist[2], snapon);

    StateSetBusy(False);
}

static void
revertOkCallback(Widget shell, XtPointer arg, XtPointer junk2)
{
    doRevert((Widget) arg);
}

static void
revertCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    Boolean flg;

    XtVaGetValues(paint, XtNdirty, &flg, NULL);
    if (flg)
	AlertBox(GetShell(paint), "Unsaved changes!\nDo you really wish to revert?",
		 revertOkCallback, genericCancelCallback, paint);
}

/*
 * Pop up the fatbits window.
 */
static void
fatCallback(Widget w, XtPointer paint, XtPointer junk2)
{
    FatbitsEdit((Widget) paint);
}

/*
 * Toggle the 'grid' menu item.
 */
static void
gridCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    Boolean v;

    XtVaGetValues(paint, XtNgrid, &v, NULL);
    v = !v;
    XtVaSetValues(paint, XtNgrid, v, NULL);

    MenuCheckItem(w, v);
}

/*
 * Toggle the 'snap' menu item.
 */
static void
snapCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    Boolean v;

    XtVaGetValues(paint, XtNsnapOn, &v, NULL);
    v = !v;
    XtVaSetValues(paint, XtNsnapOn, v, NULL);

    MenuCheckItem(w, v);
}

/*
 *  Callbacks for setting snap spacing.
 */
static int snapSpacing = 10;

static void
changeSnapOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int v = atoi(info->prompts[0].rstr);

    if (v < 1 || v > 100) {
	Notice(paint, "Bad snap spacing.\nShould be between 2 and 100");
	return;
    }
    snapSpacing = v;
    XtVaSetValues(paint, XtNsnap, v, NULL);
}

static void
changeSnapCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];

    sprintf(buf, "%d", snapSpacing);

    value[0].prompt = "Spacing:";
    value[0].str = buf;
    value[0].len = 4;
    info.prompts = value;
    info.title = "Enter desired snap spacing";
    info.nprompt = 1;

    TextPrompt(paint, "linewidth", &info, changeSnapOkCallback, NULL, NULL);
}

/*
 *  Callback for changing the image size.
 */
static void
sizeCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    SizeSelect(GetShell(paint), paint, NULL);
}

static void
sureCallback(Widget w, XtPointer argArg, XtPointer junk)
{
    AutoCrop((Widget) argArg);
}

static void
undosizeOkCallback(Widget paint, void * junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int n = atoi(info->prompts[0].rstr);

    if (n < 0 || n > 20) {
	Notice(paint, "Bad number of undo levels.\nShould be between 0 and 20");
	return;
    }
    XtVaSetValues(paint, XtNundoSize, n, NULL);
}

static void
undosizeCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[5];
    int undosize;

    XtVaGetValues(paint, XtNundoSize, &undosize, NULL);
    sprintf(buf, "%d", undosize);

    value[0].prompt = "Levels (0-20):";
    value[0].str = buf;
    value[0].len = 3;
    info.prompts = value;
    info.title = "Number of undo levels";
    info.nprompt = 1;

    TextPrompt(paint, "undolevels", &info, undosizeOkCallback, NULL, NULL);
}


static void
autocropCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    AlertBox(GetShell(paintArg),
       "Autocrop\n\nWarning: this operation cannot be undone\nContinue?",
	     sureCallback, genericCancelCallback, paintArg);
}

/*
 * Callback functions for changing zoom
 */
static void
zoomAddChild(Widget paint, int zoom)
{
    Cardinal nc;
    Widget t, box = XtParent(paint);
    WidgetList children;
    int dw, dh;

    /*
     *	1 child == just paint widget
     *	2 children paint widget + normal size view
     */
    XtVaGetValues(box, XtNchildren, &children, XtNnumChildren, &nc, NULL);
    XtVaGetValues(paint, XtNdrawWidth, &dw, XtNdrawHeight, &dh, NULL);
    if (nc == 1 && zoom > 1 && dw < 256 && dh < 256) {
	/*
	 * Add child
	 */
	t = XtVaCreateManagedWidget("norm", paintWidgetClass, box,
				    XtNpaint, paint,
				    XtNzoom, 1,
				    NULL);
	GraphicAdd(t);
    } else if (nc != 1 && zoom <= 1) {
	/*
	 * Remove child
	 */
	t = children[(children[0] == paint) ? 1 : 0];
	XtDestroyWidget(t);
    }
}

static void
zoomOkCallback(Widget w, XtPointer paintArg, XtPointer infoArg)
{
    Widget paint = (Widget) paintArg;
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int zoom = atoi(info->prompts[0].rstr);

    if (zoom < 1) {
	Notice(paint, "Invalid zoom");
    } else {
	XtVaSetValues(paint, XtNzoom, zoom, NULL);
	zoomAddChild(paint, zoom);
	if ((CurrentOp->add == BrushAdd) ||
	    (CurrentOp->add == EraseAdd) ||
	    (CurrentOp->add == SmearAdd))
	    if (zoom > 1) {
		SetCrossHairCursor(paint);
		FatCursorAddZoom(zoom, paint);
	    } else
		FatCursorRemoveZoom(paint);
	FatbitsUpdate(paint, zoom);
    }
}

void
zoomCallback(Widget w, XtPointer paintArg, XtPointer junk2)
{
    static TextPromptInfo info;
    static struct textPromptInfo values[2];
    char buf[80];
    int zoom;
    Widget paint = (Widget) paintArg;

    info.nprompt = 1;
    info.prompts = values;
    info.title = "Change zoom factor for image";
    values[0].prompt = "Zoom: ";
    values[0].len = 4;
    values[0].str = buf;

    XtVaGetValues(paint, XtNzoom, &zoom, NULL);
    sprintf(buf, "%d", (int) zoom);

    TextPrompt(GetShell(paint), "zoomselect", &info,
	       zoomOkCallback, NULL, paint);
}

/*
 * Callback functions for Region menu
 */
static void
rotateTo(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    float t;
    pwMatrix m;
    String lbl;

    XtVaGetValues(w, XtNlabel, &lbl, NULL);
    t = atof(lbl);
    if (t == 0.0)
	return;

    t *= M_PI / 180.0;

    m[0][0] = cos(t);
    m[0][1] = -sin(t);
    m[1][0] = sin(t);
    m[1][1] = cos(t);
    PwRegionAppendMatrix(paint, m);
}

static int rotateAngle = 0;

static void
rotateOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    float t = atof(info->prompts[0].rstr) * M_PI / 180.0;
    pwMatrix m;

    m[0][0] = cos(t);
    m[0][1] = -sin(t);
    m[1][0] = sin(t);
    m[1][1] = cos(t);
    PwRegionAppendMatrix(paint, m);
    rotateAngle = (int) t;
}

static void
rotate(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];

    sprintf(buf, "%d", rotateAngle);

    value[0].prompt = "Angle (in Degrees):";
    value[0].str = buf;
    value[0].len = 4;
    info.prompts = value;
    info.title = "Enter desired rotation";
    info.nprompt = 1;

    TextPrompt(paint, "rotation", &info, rotateOkCallback, NULL, NULL);
}

static void
resetMat(Widget w, XtPointer paintArg, XtPointer junk2)
{
    PwRegionReset((Widget) paintArg, True);
}

static void
cropToRegionOkCallback(Widget w, PaintWidget paint, XtPointer infoArg)
{
    RegionCrop(paint);
}

static void
cropToRegion(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    AlertBox(GetShell(paint),
    "Are you sure you want to crop the\nimage to the size of the region?",
	     (XtCallbackProc) cropToRegionOkCallback,
	     genericCancelCallback, paint);
}

#ifdef FEATURE_TILT
static void
tiltRegionOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;

    ImgProcessInfo.tiltX1 = atoi(info->prompts[0].rstr);
    ImgProcessInfo.tiltY1 = atoi(info->prompts[1].rstr);
    ImgProcessInfo.tiltX2 = atoi(info->prompts[2].rstr);
    ImgProcessInfo.tiltY2 = atoi(info->prompts[3].rstr);
    StdRegionTilt(paint, paint, NULL);
}

static void
tiltRegion(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[4];
    static char buf1[10], buf2[10], buf3[10], buf4[10];

    sprintf(buf1, "%d", ImgProcessInfo.tiltX1);
    sprintf(buf2, "%d", ImgProcessInfo.tiltY1);
    sprintf(buf3, "%d", ImgProcessInfo.tiltX2);
    sprintf(buf4, "%d", ImgProcessInfo.tiltY2);

    value[0].prompt = "X1:";
    value[0].str = buf1;
    value[0].len = 4;
    value[1].prompt = "Y1:";
    value[1].str = buf2;
    value[1].len = 4;
    value[2].prompt = "X2:";
    value[2].str = buf3;
    value[2].len = 4;
    value[3].prompt = "Y2:";
    value[3].str = buf4;
    value[3].len = 4;
    info.prompts = value;
    info.title = "Enter points";
    info.nprompt = 4;

    TextPrompt(paint, "tilt", &info, tiltRegionOkCallback, NULL, NULL);
}

#endif


/*
 * Callback functions for Filter menu
 */

static void
oilPaintOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t;

    t = atoi(info->prompts[0].rstr);
    if ((t < 3) || ((t & 1) == 0)) {
	Notice(paint, "Invalid mask size");
	return;
    }
    ImgProcessInfo.oilArea = t;
    StdRegionOilPaint(paint, paint, NULL);
}

static void
oilPaint(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];


    sprintf(buf, "%d", ImgProcessInfo.oilArea);

    info.prompts = value;
    info.title = "Enter mask size for oil paint effect";
    info.nprompt = 1;
    value[0].prompt = "(must be odd):";
    value[0].str = buf;
    value[0].len = 3;

    TextPrompt(paint, "mask", &info, oilPaintOkCallback, NULL, NULL);
}

static void
SmoothOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t;

    t = atoi(info->prompts[0].rstr);
    if ((t < 3) || ((t & 1) == 0)) {
	Notice(paint, "Invalid mask size");
	return;
    }
    ImgProcessInfo.smoothMaskSize = t;
    StdRegionSmooth(paint, paint, NULL);
}

static void
doSmooth(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];


    sprintf(buf, "%d", ImgProcessInfo.smoothMaskSize);

    info.prompts = value;
    info.title = "Enter mask size for smoothing effect";
    info.nprompt = 1;
    value[0].prompt = "(must be odd):";
    value[0].str = buf;
    value[0].len = 3;

    TextPrompt(paint, "mask", &info, SmoothOkCallback, NULL, NULL);
}

static void
addNoiseOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t;

    t = atoi(info->prompts[0].rstr);
    if (t < 1) {
	Notice(paint, "Invalid noise variance");
	return;
    }
    ImgProcessInfo.noiseDelta = t;
    StdRegionAddNoise(paint, paint, NULL);
}

static void
addNoise(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];


    sprintf(buf, "%d", ImgProcessInfo.noiseDelta);

    value[0].prompt = "(0-255):";
    value[0].str = buf;
    value[0].len = 3;
    info.prompts = value;
    info.title = "Enter desired noise variance";
    info.nprompt = 1;

    TextPrompt(paint, "delta", &info, addNoiseOkCallback, NULL, NULL);
}

static void
doSpreadOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t;

    t = atoi(info->prompts[0].rstr);
    if (t < 1) {
	Notice(paint, "Invalid spread distance");
	return;
    }
    ImgProcessInfo.spreadDistance = t;
    StdRegionSpread(paint, paint, NULL);
}

static void
doSpread(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf1[10];


    sprintf(buf1, "%d", ImgProcessInfo.spreadDistance);

    value[0].prompt = "Distance (pixels):";
    value[0].str = buf1;
    value[0].len = 3;
    info.prompts = value;
    info.title = "Enter the desired spread distance";
    info.nprompt = 1;

    TextPrompt(paint, "distance", &info, doSpreadOkCallback, NULL, NULL);
}

static void
doPixelizeOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    char *s = info->prompts[0].rstr;
    int e = 0, tx, ty;


    if (strchr(s, 'x')) {
	if (sscanf(s, "%d x %d", &tx, &ty) != 2)
	    ++e;
    } else {
	if (sscanf(s, "%d", &tx) != 1)
	    ++e;
	ty = tx;
    }

    if (e || (tx < 1) || (ty < 1)) {
	Notice(paint, "Invalid pixel size");
	return;
    }
    ImgProcessInfo.pixelizeXSize = tx;
    ImgProcessInfo.pixelizeYSize = ty;
    StdRegionPixelize(paint, paint, NULL);
}

static void
doPixelize(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];


    if (ImgProcessInfo.pixelizeXSize != ImgProcessInfo.pixelizeYSize)
	sprintf(buf, "%dx%d", ImgProcessInfo.pixelizeXSize,
		ImgProcessInfo.pixelizeYSize);
    else
	sprintf(buf, "%d", ImgProcessInfo.pixelizeXSize);

    value[0].prompt = "(w x h, or single number):";
    value[0].str = buf;
    value[0].len = 3;
    info.prompts = value;
    info.title = "Enter desired megapixel size";
    info.nprompt = 1;

    TextPrompt(paint, "size", &info, doPixelizeOkCallback, NULL, NULL);
}

static void
despeckleOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t;

    t = atoi(info->prompts[0].rstr);
    if ((t < 3) || ((t & 1) == 0)) {
	Notice(paint, "Invalid mask size");
	return;
    }
    ImgProcessInfo.despeckleMask = t;
    StdRegionDespeckle(paint, paint, NULL);
}

static void
doDespeckle(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];


    sprintf(buf, "%d", ImgProcessInfo.despeckleMask);

    info.prompts = value;
    info.title = "Enter mask size for despeckle filter";
    info.nprompt = 1;
    value[0].prompt = "(must be odd):";
    value[0].str = buf;
    value[0].len = 3;

    TextPrompt(paint, "despeckle", &info, despeckleOkCallback, NULL, NULL);
}

static void
contrastOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t1, t2;

    t1 = atoi(info->prompts[0].rstr);
    if ((t1 < 0) || (t1 > 100)) {
	Notice(paint, "Invalid white level");
	return;
    }
    t2 = atoi(info->prompts[1].rstr);
    if ((t2 < 0) || (t2 > 100)) {
	Notice(paint, "Invalid black level");
	return;
    }
    ImgProcessInfo.contrastB = t1;
    ImgProcessInfo.contrastW = t2;
    StdRegionNormContrast(paint, paint, NULL);
}


static void
doContrast(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[2];
    static char buf1[10], buf2[10];


    sprintf(buf1, "%d", ImgProcessInfo.contrastB);
    sprintf(buf2, "%d", ImgProcessInfo.contrastW);

    info.prompts = value;
    info.title = "Enter levels for contrast adjustment";
    info.nprompt = 2;
    value[0].prompt = "Black level (%):";
    value[0].str = buf1;
    value[0].len = 3;
    value[1].prompt = "White level (%):";
    value[1].str = buf2;
    value[1].len = 3;
    TextPrompt(paint, "contrast", &info, contrastOkCallback, NULL, NULL);
}

static void
solarizeOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t;

    t = atoi(info->prompts[0].rstr);
    if ((t < 1) || (t > 99)) {
	Notice(paint, "Invalid solarization threshold");
	return;
    }
    ImgProcessInfo.solarizeThreshold = t;
    StdRegionSolarize(paint, paint, NULL);
}

static void
doSolarize(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];


    sprintf(buf, "%d", ImgProcessInfo.solarizeThreshold);

    info.prompts = value;
    info.title = "Enter threshold for solarize filter";
    info.nprompt = 1;
    value[0].prompt = "(%):";
    value[0].str = buf;
    value[0].len = 3;

    TextPrompt(paint, "mask", &info, solarizeOkCallback, NULL, NULL);
}

static void
quantizeOkCallback(Widget paint, void *junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int t;

    t = atoi(info->prompts[0].rstr);
    if ((t < 2) || (t > 256)) {
	Notice(paint, "Invalid number of colors");
	return;
    }
    ImgProcessInfo.quantizeColors = t;
    StdRegionQuantize(paint, paint, NULL);
}

static void
doQuantize(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    static TextPromptInfo info;
    static struct textPromptInfo value[1];
    static char buf[10];


    sprintf(buf, "%d", ImgProcessInfo.quantizeColors);

    info.prompts = value;
    info.title = "Enter desired number of colors";
    info.nprompt = 1;
    value[0].prompt = "(2-256):";
    value[0].str = buf;
    value[0].len = 3;

    TextPrompt(paint, "mask", &info, quantizeOkCallback, NULL, NULL);
}

static void
doLast(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;

    StdLastImgProcess(paint);
}

static void
prCallback(Widget paint, Widget w, Boolean flag)
{
    XtVaSetValues(w, XtNsensitive, flag, NULL);
}

void
EnableRevert(Widget paint)
{
    WidgetList wlist;

    XtVaGetValues(paint, XtNmenuwidgets, &wlist, NULL);
    XtVaSetValues(wlist[0], XtNsensitive, 1, NULL);
}

void
EnableLast(Widget paint)
{
    WidgetList wlist;

    XtVaGetValues(paint, XtNmenuwidgets, &wlist, NULL);
    XtVaSetValues(wlist[1], XtNsensitive, 1, NULL);
}

/*
 *  Background changer
 */
static void
changeBgOk(Widget w, Palette * map, XColor * col)
{
    StateShellBusy(w, False);

    if (col != NULL) {
	Pixel pix = PaletteAlloc(map, col);
	XtVaSetValues(w, XtNbackground, pix, NULL);
    }
}

static void
changeBackground(Widget w, XtPointer paintArg, XtPointer junk2)
{
    Widget paint = (Widget) paintArg;
    Colormap cmap;
    Pixel bg;
    Palette *map;

    StateShellBusy(paint, True);

    XtVaGetValues(GetShell(paint), XtNcolormap, &cmap, NULL);
    XtVaGetValues(paint, XtNbackground, &bg, NULL);
    map = PaletteFind(paint, cmap);

    ColorEditor(paint, bg, map,
		False, (XtCallbackProc) changeBgOk, (XtPointer) map);
}

static void
selectColorRange(Widget w, XtPointer wlArg, XtPointer junk)
{
    LocalInfo *info = (LocalInfo *) wlArg;
    Widget paint = info->paint;

    ChromaDialog(paint, info->map);
}


/*
 *  Start of graphic window creation routines
 */

Widget
makeGraphicShell(Widget wid)
{
    Arg args[2];
    int nargs = 0;
    Widget shell;

    XtSetArg(args[nargs], XtNtitle, DEFAULT_TITLE);
    nargs++;
    XtSetArg(args[nargs], XtNiconName, DEFAULT_TITLE);
    nargs++;

    shell = XtAppCreateShell("Canvas", "Canvas",
		   topLevelShellWidgetClass, XtDisplay(GetToplevel(wid)),
			     args, nargs);

    return shell;
}

static Widget
mkPatternArea(Widget parent, Widget left, char *name,
	      Widget * pform, LocalInfo * info)
{
    Widget form, label, pattern, vport, lookup;

    form = XtVaCreateManagedWidget("patternRackForm",
				   formWidgetClass, parent,
				   XtNborderWidth, 0,
				   XtNfromHoriz, left,
				   XtNleft, XtChainLeft,
				   XtNright, XtChainLeft,
				   NULL);
    if (pform != NULL)
	*pform = form;
    label = XtVaCreateManagedWidget(name, labelWidgetClass, form,
				    XtNborderWidth, 0,
				    NULL);
#ifdef LOOKUP_OUTSIDE
    lookup = XtVaCreateManagedWidget("lookup",
				     commandWidgetClass, form,
				     XtNfromHoriz, label,
				     NULL);
#else
    lookup = label;
#endif

    vport = XtVaCreateManagedWidget("viewport2",
				    viewportWidgetClass, form,
				    XtNallowVert, True,
				    XtNuseBottom, True,
				    XtNuseRight, True,
				    XtNfromVert, lookup,
				    NULL);
    pattern = XtVaCreateWidget("patternRack",
			       boxWidgetClass, vport,
			       NULL);
#ifndef LOOKUP_OUTSIDE
    lookup = XtVaCreateManagedWidget("lookup",
				     commandWidgetClass, pattern,
				     NULL);
#endif

    XtAddCallback(lookup, XtNcallback, lookupPatternCallback, (XtPointer) info);

    return pattern;
}

#define ADDCALLBACK(menu, item, pw, func) \
  XtAddCallback(menu[item].widget, XtNcallback, (XtCallbackProc) func, \
		(XtPointer) pw);

/*
 * This assumes that you either
 *  - specify a pixmap, in which case width and height are taken from that, or
 *  - specify width and height, in which case the pixmap is not needed.
 * Returns the created PaintWidget.
 */
Widget
graphicCreate(Widget shell, int width, int height, int zoom,
	      Pixmap pix, Colormap cmap)
{
    Widget form, viewport, pattern, bar;
    Widget pane, pane1, pform;
    Widget paint;
    Widget add, edit;
    WidgetList child, wlist;
    int nchild;
    RCInfo *rcInfo;
    Palette *map;
    int i;
    int depth;
    LocalInfo *info = XtNew(LocalInfo);
    PatternInfo *pData;

    /*
     *	Menu Bar
     */
    if (cmap == None) {
	map = PaletteCreate(shell);
	cmap = map->cmap;
    } else {
	map = PaletteFind(shell, cmap);
    }
    depth = map->depth;
    XtVaSetValues(shell, XtNcolormap, cmap, NULL);

    PaletteAddUser(map, shell);

    info->map = map;

    pane = XtVaCreateManagedWidget("pane",
				   panedWidgetClass, shell,
				   NULL);
    rcInfo = ReadDefaultRC();

    /*
     *	Menu area
     */
    form = XtVaCreateManagedWidget("form1",
				   formWidgetClass, pane,
				   NULL);
    bar = MenuBarCreate(form, XtNumber(menuBar), menuBar);
    XtVaSetValues(form, XtNshowGrip, False, NULL);

    /*
     *	Drawing Area
     */
    viewport = XtVaCreateWidget("viewport",
				viewportWidgetClass, pane,
				XtNfromVert, bar,
				XtNallowVert, True,
				XtNallowHoriz, True,
				XtNuseBottom, True,
				XtNuseRight, True,
				XtNtop, XtChainTop,
				NULL);

    /*
     *	Custom Drawing Widget here
     */
    pane1 = XtVaCreateWidget("paintBox",
			     boxWidgetClass, viewport,
		      XtNbackgroundPixmap, GetBackgroundPixmap(viewport),
			     NULL);
    /*
     *	Try and do something nice for the user
     */
    if (pix != None)
	GetPixmapWHD(XtDisplay(pane1), pix, &width, &height, NULL);
    if (zoom == -1 && width <= 64 && height <= 64)
	zoom = 6;

    paint = XtVaCreateManagedWidget("paint",
				    paintWidgetClass, pane1,
				    XtNdrawWidth, width,
				    XtNdrawHeight, height,
				    XtNzoom, zoom,
				    XtNpixmap, pix,
				    XtNcolormap, cmap,
				    XtNallowResize, True,
				    XtNshowGrip, False,
				    XtNundoSize, 4,
				    NULL);
    XtSetKeyboardFocus(pane, paint);

    zoomAddChild(paint, zoom);

    info->paint = paint;
    OperationSetPaint(paint);
    ccpAddStdPopup(paint);

    XtManageChild(pane1);
    XtManageChild(viewport);

    ADDCALLBACK(fileMenu, SAVE_CONFIG, info, saveConfigCallback);
    ADDCALLBACK(fileMenu, LOAD_CONFIG, info, loadConfigCallback);

    ccpAddUndo(editMenu[UNDO_ITEM].widget, paint);
    ccpAddRedo(editMenu[REDO_ITEM].widget, paint);
    ccpAddCut(editMenu[CUT_ITEM].widget, paint);
    ccpAddCopy(editMenu[COPY_ITEM].widget, paint);
    ccpAddPaste(editMenu[PASTE_ITEM].widget, paint);
    ccpAddClear(editMenu[CLEAR_ITEM].widget, paint);
    ccpAddDuplicate(editMenu[DUP_ITEM].widget, paint);

    ADDCALLBACK(editMenu, SELECT_ALL_ITEM, paint, StdSelectAllCallback);
    ADDCALLBACK(fileMenu, SAVEAS_ITEM, paint, StdSaveAsFile);
    ADDCALLBACK(fileMenu, SAVE_ITEM, paint, StdSaveFile);

    ccpAddSaveRegion(fileMenu[SAVER_ITEM].widget, paint);

    ADDCALLBACK(fileMenu, REVERT_ITEM, paint, revertCallback);
    prCallback(paint, fileMenu[REVERT_ITEM].widget, False);

    ADDCALLBACK(fileMenu, CLOSE_ITEM, paint, closeCallback);
    ADDCALLBACK(otherMenu, GRID_ITEM, paint, gridCallback);
    ADDCALLBACK(otherMenu, SNAP_ITEM, paint, snapCallback);
    ADDCALLBACK(otherMenu, CHSNAP_ITEM, paint, changeSnapCallback);
    ADDCALLBACK(otherMenu, FAT_ITEM, paint, fatCallback);
    ADDCALLBACK(otherMenu, CHSIZE_ITEM, paint, sizeCallback);
    ADDCALLBACK(otherMenu, UNDOSZ_ITEM, paint, undosizeCallback);
    ADDCALLBACK(otherMenu, AUTOCROP_ITEM, paint, autocropCallback);
    ADDCALLBACK(otherMenu, CHZOOM_ITEM, paint, zoomCallback);
    ADDCALLBACK(otherMenu, EDIT_BACKGROUND, paint, changeBackground);

    ADDCALLBACK(regionMenu, RG_FLIPX_ITEM, paint, StdRegionFlipX);
    ADDCALLBACK(regionMenu, RG_FLIPY_ITEM, paint, StdRegionFlipY);
    ADDCALLBACK(filterMenu, RG_INVERT_ITEM, paint, StdRegionInvert);
    ADDCALLBACK(filterMenu, RG_SHARPEN_ITEM, paint, StdRegionSharpen);
    ADDCALLBACK(filterMenu, RG_EDGE_ITEM, paint, StdRegionEdge);
    ADDCALLBACK(filterMenu, RG_EMBOSE_ITEM, paint, StdRegionEmbose);
    ADDCALLBACK(filterMenu, RG_BLEND_ITEM, paint, StdRegionBlend);

    for (i = 0; i < XtNumber(rotateMenu); i++) {
	if (rotateMenu[i].name[0] == '\0')
	    continue;

	ADDCALLBACK(rotateMenu, i, paint, rotateTo);
	XtAddCallback(paint, XtNregionCallback, (XtCallbackProc) prCallback,
		      (XtPointer) rotateMenu[i].widget);
	prCallback(paint, rotateMenu[i].widget, False);
    }

    ADDCALLBACK(regionMenu, RG_ROTATE, paint, rotate);
    XtAddCallback(paint, XtNregionCallback, (XtCallbackProc) prCallback,
		  (XtPointer) regionMenu[RG_ROTATE].widget);
    prCallback(paint, regionMenu[RG_ROTATE].widget, False);

    ADDCALLBACK(filterMenu, RG_SMOOTH_ITEM, paint, doSmooth);
    ADDCALLBACK(filterMenu, RG_OIL_ITEM, paint, oilPaint);
    ADDCALLBACK(filterMenu, RG_NOISE_ITEM, paint, addNoise);
    ADDCALLBACK(filterMenu, RG_SPREAD_ITEM, paint, doSpread);
    ADDCALLBACK(filterMenu, RG_PIXELIZE_ITEM, paint, doPixelize);
    ADDCALLBACK(filterMenu, RG_DESPECKLE_ITEM, paint, doDespeckle);
    ADDCALLBACK(filterMenu, RG_CONTRAST_ITEM, paint, doContrast);
    ADDCALLBACK(filterMenu, RG_SOLARIZE_ITEM, paint, doSolarize);
    ADDCALLBACK(filterMenu, RG_QUANTIZE_ITEM, paint, doQuantize);
    ADDCALLBACK(filterMenu, RG_TOGREY_ITEM, paint, StdRegionGrey);
    ADDCALLBACK(filterMenu, RG_DIRFILT_ITEM, paint, StdRegionDirFilt);
    ADDCALLBACK(filterMenu, RG_LAST_ITEM, paint, doLast);
    prCallback(paint, filterMenu[RG_LAST_ITEM].widget, False);

    ADDCALLBACK(regionMenu, RG_CROP, paint, cropToRegion);
    XtAddCallback(paint, XtNregionCallback, (XtCallbackProc) prCallback,
		  (XtPointer) regionMenu[RG_CROP].widget);
    prCallback(paint, regionMenu[RG_CROP].widget, False);

#ifdef FEATURE_TILT
    ADDCALLBACK(regionMenu, RG_TILT, paint, tiltRegion);
#endif
    ADDCALLBACK(regionMenu, RG_RESET, paint, resetMat);

    XtAddCallback(paint, XtNregionCallback, (XtCallbackProc) prCallback,
		  (XtPointer) regionMenu[RG_RESET].widget);
    prCallback(paint, regionMenu[RG_RESET].widget, False);

    /* Store menu widgets for later reference */
    wlist = (WidgetList) XtMalloc(NMENUWIDGETS * sizeof(Widget));
    wlist[0] = fileMenu[REVERT_ITEM].widget;
    wlist[1] = filterMenu[RG_LAST_ITEM].widget;
    wlist[2] = otherMenu[SNAP_ITEM].widget;
    XtVaSetValues(paint, XtNmenuwidgets, wlist, NULL);

    form = XtVaCreateManagedWidget("form2",
				   formWidgetClass, pane,
				   XtNskipAdjust, True,
				   NULL);

    pattern = mkPatternArea(form, None, "primary", &pform, info);
    info->primaryBox = pattern;

    pattern = mkPatternArea(form, pform, "secondary", &pform, info);
    info->secondaryBox = pattern;

    info->primaryList = None;
    info->secondaryList = None;

    /*
     *	Now construct the palette area and set the
     *	  primary and secondary selections correctly
     */

    makePaletteArea(info, rcInfo);

    XtVaGetValues(info->primaryBox, XtNchildren, &child,
		  XtNnumChildren, &nchild,
		  NULL);
    XtVaGetValues(child[0], XtNradioData, &pData, NULL);
    XawToggleSetCurrent(info->primaryList, (XtPointer) pData);

    XtVaGetValues(info->secondaryBox, XtNchildren, &child,
		  XtNnumChildren, &nchild,
		  NULL);
    if (nchild == 1)
	XtVaGetValues(child[0], XtNradioData, &pData, NULL);
    else
	XtVaGetValues(child[1], XtNradioData, &pData, NULL);
    XawToggleSetCurrent(info->secondaryList, (XtPointer) pData);

    XtManageChild(info->primaryBox);
    XtManageChild(info->secondaryBox);

    /*
     *	A few buttons for help...
     */
    add = XtVaCreateManagedWidget("addPattern",
				  commandWidgetClass, form,
				  XtNfromHoriz, pform,
				  XtNfromVert, None,
				  XtNleft, XtChainRight,
				  XtNright, XtChainRight,
				  XtNtop, XtChainTop,
				  XtNbottom, XtChainTop,
				  NULL);
    /*
     *	If we are on a small static colormap, we shouldn't be able to do an add
     */
    edit = XtVaCreateManagedWidget("addSolid",
				   commandWidgetClass, form,
				   XtNfromHoriz, pform,
				   XtNfromVert, add,
				   XtNleft, XtChainRight,
				   XtNright, XtChainRight,
				   XtNtop, XtChainTop,
				   XtNbottom, XtChainTop,
				   NULL);

    if (edit != None)
	XtAddCallback(edit, XtNcallback, addSolidCallback, (XtPointer) info);
    XtAddCallback(add, XtNcallback, addPatternCallback, (XtPointer) info);
    XtAddCallback(paint, XtNdestroyCallback, destroyCallback, (XtPointer) info);

    edit = XtVaCreateManagedWidget("selectRange",
				   commandWidgetClass, form,
				   XtNfromHoriz, pform,
				   XtNfromVert, edit,
				   XtNleft, XtChainRight,
				   XtNright, XtChainRight,
				   XtNtop, XtChainTop,
				   XtNbottom, XtChainTop,
				   NULL);

    XtAddCallback(edit, XtNcallback, selectColorRange, (XtPointer) info);

    AddDestroyCallback(shell,
		       (DestroyCallbackFunc) closeCallback, (void *) paint);
    SetIconImage(shell);

    XtRealizeWidget(shell);
    GraphicAdd(paint);
    return paint;
}

typedef struct cwi_s {
    Widget paint;
    void *id;
    struct cwi_s *next;
} CanvasWriteInfo;

static CanvasWriteInfo *cwiHead = NULL;

static void
removeCWI(Widget w, CanvasWriteInfo * ci, XtPointer junk)
{
    CanvasWriteInfo *cur = cwiHead, **pp = &cwiHead;

    while (cur != NULL && cur != ci) {
	pp = &cur->next;
	cur = cur->next;
    }

    if (cur == NULL)
	return;
    *pp = cur->next;
    XtFree((XtPointer) ci);
}

void *
GraphicGetReaderId(Widget paint)
{
    CanvasWriteInfo *cur;

    paint = GetShell(paint);

    for (cur = cwiHead; cur != NULL && cur->paint != paint; cur = cur->next);

    if (cur == NULL)
	return NULL;

    return cur->id;
}

Widget
GraphicOpenFileZoom(Widget w, char *file, XtPointer imageArg, int zoom)
{
    Image *image = (Image *) imageArg;
    Colormap cmap;
    Pixmap pix;
    Widget shell = makeGraphicShell(w);
    CanvasWriteInfo *ci = XtNew(CanvasWriteInfo);
    PaintWidget paint;

    ci->next = cwiHead;
    cwiHead = ci;
    ci->paint = shell;
    ci->id = GetFileNameGetLastId();

    XtAddCallback(shell, XtNdestroyCallback,
		  (XtCallbackProc) removeCWI, (XtPointer) ci);

    if (ImageToPixmap(image, shell, &pix, &cmap)) {
	/*
	 * If mask != None, set the mask region color to the BG color of the Canvas
	 */
	if ((paint = (PaintWidget)
	     graphicCreate(shell, 0, 0, zoom, pix, cmap)) != None) {
	    char *cp = strrchr(file, '/');
	    if (cp == NULL)
		cp = file;
	    else
		cp++;

	    XtVaSetValues(shell, XtNiconName, cp, XtNtitle, file, NULL);
	    cp = xmalloc(strlen(file) + 1);
	    strcpy(cp, file);
	    paint->paint.filename = cp;
	    EnableRevert((Widget) paint);
	    return (Widget) paint;
	} else {
	    XtDestroyWidget(shell);
	}
    } else {
	Notice(w, "Unable to create paint window with image");
	XtDestroyWidget(shell);
    }
    return NULL;
}

void
GraphicOpenFile(Widget w, XtPointer fileArg, XtPointer imageArg)
{
    GraphicOpenFileZoom(w, (char *) fileArg, imageArg, -1);
}

static void
doCreate(Widget wid, int width, int height, int zoom)
{
    graphicCreate(makeGraphicShell(wid), width, height, zoom, None, None);
}

/*
 * 0: Create new (blank) canvas
 * 1: Open a file
 * 2: Create new (blank) canvas, querying for size
 */
void
GraphicCreate(Widget wid, int value)
{
    int width, height;

    switch (value) {
    case 0:
	GetDefaultWH(&width, &height);
	graphicCreate(makeGraphicShell(wid), width, height, -1, None, None);
	break;
    case 1:
	GetFileName(GetToplevel(wid), 0, NULL, GraphicOpenFile, NULL);
	break;
    case 2:
	SizeSelect(wid, None, doCreate);
	break;
    }
}
