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

/* $Id: pattern.c,v 1.6 1996/04/19 08:53:22 torsten Exp $ */

#include <X11/IntrinsicP.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Command.h>
#include <X11/CoreP.h>
#include <X11/Xaw/ToggleP.h>
#include <X11/cursorfont.h>
#include <stdio.h>
#include "Colormap.h"
#include "Paint.h"
#include "palette.h"
#include "xpaint.h"
#include "menu.h"
#include "image.h"
#include "misc.h"
#include "cutCopyPaste.h"
#include "text.h"
#include "graphic.h"
#include "operation.h"
#include "color.h"
#include "protocol.h"

#ifndef NOSTDHDRS
#include <stdlib.h>
#include <unistd.h>
#endif

static PaintMenuItem fileMenu[] =
{
    MI_SIMPLE("read"),
    MI_SIMPLE("save"),
    MI_SIMPLE("close"),
};
static PaintMenuItem editMenu[] =
{
    MI_SIMPLE("undo"),
    MI_SEPERATOR(),
    MI_SIMPLE("cut"),
    MI_SIMPLE("copy"),
    MI_SIMPLE("paste"),
    MI_SIMPLE("clear"),
    MI_SEPERATOR(),
    MI_SIMPLE("dup"),
    MI_SIMPLE("all"),
};
static PaintMenuItem sizeMenu[] =
{
#define SZ_N1	0
    MI_FLAG("16x16", MF_CHECK | MF_GROUP1),
#define SZ_N2	1
    MI_FLAG("24x24", MF_CHECK | MF_GROUP1),
#define SZ_N3	2
    MI_FLAG("32x32", MF_CHECK | MF_GROUP1),
#define SZ_N4	3
    MI_FLAG("48x48", MF_CHECK | MF_GROUP1),
#define SZ_N5	4
    MI_FLAG("64x64", MF_CHECK | MF_GROUP1),
    MI_SEPERATOR(),
#define SZ_N6	6
    MI_FLAG("other", MF_CHECK | MF_GROUP1),
};
static PaintMenuItem imageMenu[] =
{
    MI_FLAG("grid", MF_CHECK),
};
static PaintMenuItem helpMenu[] =
{
    MI_SIMPLECB("help", HelpDialog, "canvas.patBox.pattern"),
};
static PaintMenuBar menuBar[] =
{
    {None, "file", XtNumber(fileMenu), fileMenu},
    {None, "edit", XtNumber(editMenu), editMenu},
    {None, "size", XtNumber(sizeMenu), sizeMenu},
    {None, "image", XtNumber(imageMenu), imageMenu},
    {None, "help", XtNumber(helpMenu), helpMenu},
};

#define	RED	1
#define	GREEN	2
#define	BLUE	3

Widget ColorPickerPalette(Widget, Palette *, Pixel *);

typedef struct {
    int add;
    Pixmap pix;
    void *iconList;
    Widget fat, norm, paint;
    Widget cpick, color;
    Widget curCheck;
    Widget sizeChecks[XtNumber(sizeMenu) - 1];
    Palette *map;
} LocalInfo;

static void 
activePixel(LocalInfo * l, Pixel p)
{
    if (l->color != None)
	XtVaSetValues(l->color, XtNcolor, p, NULL);
    XtVaSetValues(l->fat, XtNforeground, p,
		  XtNlineForeground, p,
		  NULL);
    if (l->cpick != None)
	ColorPickerSetPixel(l->cpick, p);
    PaletteSetInvalid(l->map, p);
}

static void 
cpickCallback(Widget w, LocalInfo * l, Pixel p)
{
    activePixel(l, p);
}

static void 
colorCallback(Widget w, LocalInfo * l, XColor * color)
{
    activePixel(l, color->pixel);
}

static void 
matchCallback(Widget w, LocalInfo * l, XtPointer junk)
{
    Colormap cmap;
    Pixel p;

    DoGrabPixel(w, &p, &cmap);

    if (l->map->cmap == cmap) {
	activePixel(l, p);
    } else {
	XColor col;

	col.flags = DoRed | DoGreen | DoBlue;
	col.pixel = p;
	XQueryColor(XtDisplay(w), cmap, &col);
	if (PaletteLookupColor(l->map, &col, &p)) {
	    activePixel(l, p);
	} else {
	    Notice(w, "Selected color is from a different colormap,\n"
		   "with no matching color on this colormap");
	}
    }
}


static void 
grabCallback(Widget w, XtPointer infoArg, XtPointer junk2)
{
    LocalInfo *info = (LocalInfo *) infoArg;
    Image *image;
    Colormap cmap;
    Pixmap pix;
    int grabW, grabH;

    XtVaGetValues(info->fat, XtNdrawWidth, &grabW,
		  XtNdrawHeight, &grabH,
		  NULL);

    image = DoGrabImage(w, grabW, grabH);

    XtVaGetValues(info->paint, XtNcolormap, &cmap, NULL);
    pix = None;
    if (ImageToPixmapCmap(image, info->fat, &pix, cmap))
	PwPutPixmap(info->fat, pix);
}

static void 
closeCallback(Widget w, XtPointer infoArg, XtPointer junk2)
{
    LocalInfo *info = (LocalInfo *) infoArg;

    XtDestroyWidget(GetShell(info->fat));
    StateShellBusy(info->paint, False);
    XtFree((XtPointer) info);
}
static void 
buttonCallback(Widget w, XtPointer infoArg, XtPointer junk2)
{
    LocalInfo *info = (LocalInfo *) infoArg;
    Pixmap pix;
    Widget icon;
#if 0
    XtPointer data;
#endif

    if (info != NULL) {
	void *pi = XawToggleGetCurrent(info->iconList);
	PwRegionFinish(info->fat, True);
	PwGetPixmap(info->fat, &pix, NULL, NULL);

	if (info->add) {
	    icon = AddPatternInfo(pi, pix, 0);

#if 0
	    XtVaGetValues(icon, XtNradioData, &data, NULL);

	    XawToggleSetCurrent(icon, data);
#endif
	} else {
	    ChangePattern(pi, pix);
	}

#if 0
	XtVaSetValues(info->paint, XtNpattern, pix, NULL);
#endif
	StateShellBusy(info->paint, False);
	XtFree((XtPointer) info);
    }
    XtDestroyWidget(GetShell(w));
}
static void 
readFileCallback(Widget paint, XtPointer fileArg, XtPointer imageArg)
{
    Image *image = (Image *) imageArg;
    Pixmap pix;
    Colormap cmap;

    /*  XXX - allocating a new colormap! */
    if (ImageToPixmap(image, paint, &pix, &cmap)) {
	XtVaSetValues(paint, XtNcolormap, cmap, NULL);
	PwPutPixmap(paint, pix);
	return;
    }
}
static void 
readCallback(Widget w, XtPointer paint, XtPointer junk)
{
    GetFileName((Widget) paint, False, NULL, readFileCallback, NULL);
}

static void 
cac(LocalInfo * info, int inW, int inH)
{
    int width, height, i;
    String lbl;

    for (i = 0; i < XtNumber(info->sizeChecks); i++) {
	Widget w = info->sizeChecks[i];
	XtVaGetValues(w, XtNlabel, &lbl, NULL);
	width = -1;
	height = -1;
	sscanf(lbl, "%dx%d", &width, &height);
	if (width <= 0 || height <= 0 || (width == inW && height == inH)) {
	    MenuCheckItem(info->curCheck = w, True);
	    break;
	}
    }
}
static void 
sizeOkChoiceCallback(Widget w, LocalInfo * l, TextPromptInfo * info)
{
    int width = atoi(info->prompts[0].rstr);
    int height = atoi(info->prompts[1].rstr);

    if (width <= 0 || height <= 0) {
	Notice(w, "Invalid width or height,\nmust be greater than 0");
    } else if (width > 128 || height > 128) {
	Notice(w, "Invalid width or height,\nmust be less than 129");
    } else {
	XtVaSetValues(l->fat, XtNdrawWidth, width,
		      XtNdrawHeight, height,
		      NULL);
	MenuCheckItem(l->curCheck, False);
	l->curCheck = None;
	cac(l, width, height);
    }
}
static void 
sizeChoiceCallback(Widget w, XtPointer larg, XtPointer junk)
{
    static TextPromptInfo info;
    static struct textPromptInfo values[3];
    char bufA[16], bufB[16];
    int width, height;
    LocalInfo *l = (LocalInfo *) larg;

    XtVaGetValues(l->fat, XtNdrawWidth, &width,
		  XtNdrawHeight, &height,
		  NULL);

    info.prompts = values;
    info.nprompt = 2;
    info.title = "Enter the desired pattern size:";

    values[0].prompt = "Width:";
    values[0].str = bufA;
    values[0].len = 4;
    values[1].prompt = "Height:";
    values[1].str = bufB;
    values[1].len = 4;

    sprintf(bufA, "%d", width);
    sprintf(bufB, "%d", height);

    TextPrompt(w, "sizeselect", &info, (XtCallbackProc) sizeOkChoiceCallback,
	       NULL, larg);
}
static void 
sizeCallback(Widget w, XtPointer larg, XtPointer junk)
{
    LocalInfo *l = (LocalInfo *) larg;
    int width, height;
    String lbl;

    if (l->curCheck == w)
	return;

    XtVaGetValues(w, XtNlabel, &lbl, NULL);
    width = -1;
    height = -1;
    sscanf(lbl, "%dx%d", &width, &height);
    if (width <= 0 || height <= 0 || width >= 256 || height >= 256) {
	Notice(w, "Invalid width/height specification must be between 0..256");
	return;
    }
    MenuCheckItem(l->curCheck = w, True);

    /*
    **  Just change the size, no OK
     */
    XtVaSetValues(l->fat, XtNdrawWidth, width,
		  XtNdrawHeight, height,
		  NULL);
    XtVaSetValues(l->norm, XtNdrawWidth, width,
		  XtNdrawHeight, height,
		  NULL);
}
static void 
gridCallback(Widget w, XtPointer larg, XtPointer junk)
{
    LocalInfo *l = (LocalInfo *) larg;
    Boolean v;

    XtVaGetValues(l->fat, XtNgrid, &v, NULL);
    v = !v;
    XtVaSetValues(l->fat, XtNgrid, v, NULL);

    MenuCheckItem(w, v);
}

void 
PatternEdit(Widget w, Pixmap pix, Widget button)
{
    Widget shell, form, topf, norm, box, vp;
    Widget color, fat, bar, cpick;
    Widget okButton, cancelButton, grabButton, matchButton;
    Colormap cmap;
    Pixel pval = WhitePixelOfScreen(XtScreen(w));
    LocalInfo *info = (LocalInfo *) XtMalloc(sizeof(LocalInfo));
    Palette *map;
    int width, height;
    Position x, y;

    info->paint = w;
    info->add = (pix == None);
    info->iconList = button;

    StateShellBusy(w, True);
    XtVaGetValues(GetShell(w), XtNcolormap, &cmap, XtNx, &x, XtNy, &y, NULL);
    info->map = map = PaletteFind(w, cmap);

    shell = XtVaCreatePopupShell("pattern", transientShellWidgetClass,
				 GetShell(w),
				 XtNcolormap, cmap,
				 XtNx, x + 24,
				 XtNy, y + 24,
				 NULL);
    PaletteAddUser(map, shell);

    topf = XtVaCreateManagedWidget("form", formWidgetClass, shell,
				   NULL);

    bar = MenuBarCreate(topf, XtNumber(menuBar), menuBar);

    form = XtVaCreateManagedWidget("box", formWidgetClass, topf,
				   XtNfromVert, bar,
				   XtNtop, XtChainTop,
				   XtNbottom, XtChainBottom,
				   NULL);
    vp = XtVaCreateManagedWidget("viewport", viewportWidgetClass, form,
				 XtNfromVert, bar,
				 XtNallowVert, True,
				 XtNallowHoriz, True,
				 XtNuseBottom, True,
				 XtNuseRight, True,
				 XtNleft, XtChainLeft,
				 XtNright, XtChainRight,
				 NULL);
    box = XtVaCreateManagedWidget("patternBox", boxWidgetClass, vp,
			    XtNbackgroundPixmap, GetBackgroundPixmap(vp),
				  XtNorientation, XtorientHorizontal,
				  NULL);

    if (pix == None)
	fat = XtVaCreateManagedWidget("paint", paintWidgetClass, box,
				      XtNpixmap, pix,
				      XtNdrawWidth, 24,
				      XtNdrawHeight, 24,
				      XtNfillRule, FillSolid,
				      NULL);
    else
	fat = XtVaCreateManagedWidget("paint", paintWidgetClass, box,
				      XtNpixmap, pix,
				      XtNfillRule, FillSolid,
				      NULL);
    norm = XtVaCreateManagedWidget("norm", paintWidgetClass, box,
				   XtNpaint, fat,
				   XtNfillRule, FillSolid,
				   XtNzoom, 1,
				   NULL);
    OperationSetPaint(fat);
    ccpAddStdPopup(fat);


    cpick = None;
    if (!map->isMapped || !map->readonly || map->ncolors > 256)
	cpick = ColorPickerPalette(form, map, &pval);

    if (map->isMapped) {
	color = XtVaCreateManagedWidget("cedit", colormapWidgetClass, form,
					XtNwidth, 256,
					XtNheight, 256,
					XtNcolormap, cmap,
					XtNfromHoriz, vp,
					XtNleft, XtChainRight,
					XtNright, XtChainRight,
					NULL);
	if (cpick != None)
	    XtVaSetValues(cpick, XtNfromHoriz, color,
			  XtNleft, XtChainRight,
			  NULL);
	XtAddCallback(color, XtNcallback,
		      (XtCallbackProc) colorCallback, (XtPointer) info);
    } else {
	color = None;
	if (cpick != None) {
	    XtVaSetValues(cpick, XtNfromHoriz, vp,
			  XtNleft, XtChainRight,
			  XtNright, XtChainRight,
			  NULL);
	    ColorPickerSetFunction(cpick,
		       (XtCallbackProc) cpickCallback, (XtPointer) info);
	}
    }

    info->cpick = cpick;
    info->color = color;
    info->fat = fat;
    info->norm = norm;

    okButton = XtVaCreateManagedWidget("ok",
				       commandWidgetClass, topf,
				       XtNfromVert, form,
				       XtNtop, XtChainBottom,
				       XtNbottom, XtChainBottom,
				       XtNleft, XtChainLeft,
				       XtNright, XtChainLeft,
				       NULL);

    cancelButton = XtVaCreateManagedWidget("cancel",
					   commandWidgetClass, topf,
					   XtNfromVert, form,
					   XtNfromHoriz, okButton,
					   XtNtop, XtChainBottom,
					   XtNbottom, XtChainBottom,
					   XtNleft, XtChainLeft,
					   XtNright, XtChainLeft,
					   NULL);
    grabButton = XtVaCreateManagedWidget("grab",
					 commandWidgetClass, topf,
					 XtNfromVert, form,
					 XtNfromHoriz, cancelButton,
					 XtNtop, XtChainBottom,
					 XtNbottom, XtChainBottom,
					 XtNleft, XtChainLeft,
					 XtNright, XtChainLeft,
					 NULL);
    matchButton = XtVaCreateManagedWidget("lookup",
					  commandWidgetClass, topf,
					  XtNfromVert, form,
					  XtNfromHoriz, grabButton,
					  XtNtop, XtChainBottom,
					  XtNbottom, XtChainBottom,
					  XtNleft, XtChainLeft,
					  XtNright, XtChainLeft,
					  NULL);

    ccpAddUndo(XtNameToWidget(bar, "edit.editMenu.undo"), fat);
    ccpAddCut(XtNameToWidget(bar, "edit.editMenu.cut"), fat);
    ccpAddCopy(XtNameToWidget(bar, "edit.editMenu.copy"), fat);
    ccpAddPaste(XtNameToWidget(bar, "edit.editMenu.paste"), fat);
    ccpAddClear(XtNameToWidget(bar, "edit.editMenu.clear"), fat);
    ccpAddDuplicate(XtNameToWidget(bar, "edit.editMenu.dup"), fat);

    XtAddCallback(XtNameToWidget(bar, "edit.editMenu.all"),
		  XtNcallback, StdSelectAllCallback, (XtPointer) fat);

    XtAddCallback(XtNameToWidget(bar, "file.fileMenu.close"),
		  XtNcallback, closeCallback, (XtPointer) info);
    XtAddCallback(XtNameToWidget(bar, "file.fileMenu.save"),
		  XtNcallback, StdSaveFile, (XtPointer) fat);
    XtAddCallback(XtNameToWidget(bar, "file.fileMenu.read"),
		  XtNcallback, readCallback, (XtPointer) fat);

    XtAddCallback(sizeMenu[SZ_N1].widget,
		  XtNcallback, sizeCallback, (XtPointer) info);
    XtAddCallback(sizeMenu[SZ_N2].widget,
		  XtNcallback, sizeCallback, (XtPointer) info);
    XtAddCallback(sizeMenu[SZ_N3].widget,
		  XtNcallback, sizeCallback, (XtPointer) info);
    XtAddCallback(sizeMenu[SZ_N4].widget,
		  XtNcallback, sizeCallback, (XtPointer) info);
    XtAddCallback(sizeMenu[SZ_N5].widget,
		  XtNcallback, sizeCallback, (XtPointer) info);
    XtAddCallback(sizeMenu[SZ_N6].widget,
		  XtNcallback, sizeChoiceCallback, (XtPointer) info);
    XtAddCallback(imageMenu[0].widget,
		  XtNcallback, gridCallback, (XtPointer) info);

    info->sizeChecks[0] = sizeMenu[SZ_N1].widget;
    info->sizeChecks[1] = sizeMenu[SZ_N2].widget;
    info->sizeChecks[2] = sizeMenu[SZ_N3].widget;
    info->sizeChecks[3] = sizeMenu[SZ_N4].widget;
    info->sizeChecks[4] = sizeMenu[SZ_N5].widget;
    info->sizeChecks[5] = sizeMenu[SZ_N6].widget;

    XtVaGetValues(info->fat, XtNdrawWidth, &width, XtNdrawHeight, &height, NULL);
    cac(info, width, height);

    XtAddCallback(okButton, XtNcallback,
		  (XtCallbackProc) buttonCallback, (XtPointer) info);
    XtAddCallback(cancelButton, XtNcallback,
		  (XtCallbackProc) closeCallback, (XtPointer) info);
    AddDestroyCallback(shell, (DestroyCallbackFunc) closeCallback, info);
    XtAddCallback(grabButton, XtNcallback,
		  (XtCallbackProc) grabCallback, (XtPointer) info);
    XtAddCallback(matchButton, XtNcallback,
		  (XtCallbackProc) matchCallback, (XtPointer) info);

    XtPopup(shell, XtGrabNone);

    activePixel(info, 0);

    GraphicAdd(fat);
    GraphicAdd(norm);
}
