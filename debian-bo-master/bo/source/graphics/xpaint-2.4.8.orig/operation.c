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

/* $Id: operation.c,v 1.17 1996/11/01 09:37:46 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>
#include <X11/cursorfont.h>
#include <xpm.h>
#include <stdio.h>
#ifndef NOSTDHDRS
#include <stdlib.h>
#include <unistd.h>
#endif

#include "xpaint.h"
#include "misc.h"
#include "menu.h"
#include "Paint.h"
#include "text.h"
#include "ops.h"
#include "graphic.h"
#include "image.h"
#include "operation.h"
#include "protocol.h"
#include "cutCopyPaste.h"

/* Pixmaps for toolbox icons */
#include "brushOp.xpm"
#include "eraseOp.xpm"
#include "sprayOp.xpm"
#include "pencilOp.xpm"
#include "dotPenOp.xpm"
#include "lineOp.xpm"
#include "arcOp.xpm"
#include "fillOp.xpm"
#include "tfillOp.xpm"
#ifdef FEATURE_FRACTAL
#include "ffillOp.xpm"
#endif
#include "smearOp.xpm"
#include "textOp.xpm"
#include "selectOp.xpm"
#include "boxOp.xpm"
#include "rayOp.xpm"
#include "fboxOp.xpm"
#include "ovalOp.xpm"
#include "fovalOp.xpm"
#include "lassoOp.xpm"
#include "clineOp.xpm"
#include "polyOp.xpm"
#include "fpolyOp.xpm"
#include "freehandOp.xpm"
#include "ffreehandOp.xpm"
#include "selpolyOp.xpm"



static void changeLineAction(Widget, XEvent *);
static void changeSprayAction(Widget, XEvent *);
static void changeTFillAction(Widget, XEvent *);
static void changeBrushAction(Widget, XEvent *);
static void changeBrushParmAction(Widget, XEvent *);
static void changeFontAction(Widget, XEvent *);

static void 
setOperation(Widget w, XtPointer opArg, XtPointer junk2)
{
    OperationPair *op = (OperationPair *) opArg;
    OperationAddProc start;
    OperationRemoveProc stop;

    if (op == CurrentOp)
	return;

    start = op ? op->add : NULL;
    stop = CurrentOp ? CurrentOp->remove : NULL;

    GraphicSetOp((OperationRemoveProc) stop, (OperationAddProc) start);

    CurrentOp = op;
}

/*
**  The set line width callback pair
 */
static char currentLineWidth[10] = "1";

static void 
setWidth(Widget w, void *width)
{
    XtVaSetValues(w, XtNlineWidth, (int) width, NULL);
}
static void 
okLineCallback(Widget w, XtPointer junk, XtPointer infoArg)
{
    Arg arg;
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int width = atoi(info->prompts[0].rstr);

    if (width < 1 || width > 1000) {
	Notice(w, "Invalid width, must be greater than zero and less than 1000");
	return;
    }
    sprintf(currentLineWidth, "%d", width);

    MenuCheckItem(w, True);

    if (width == 1)
	width = 0;
    GraphicAll(setWidth, (void *) width);
    XtSetArg(arg, XtNlineWidth, width);
    OperationAddArg(arg);
}

static void 
lineWidth(Widget w, int width)
{
    String lbl;

    XtVaGetValues(w, XtNlabel, &lbl, NULL);
    width = atoi(lbl);
    if (width <= 0) {
	static TextPromptInfo info;
	static struct textPromptInfo value[1];

	value[0].prompt = "Width:";
	value[0].str = currentLineWidth;
	value[0].len = 4;
	info.prompts = value;
	info.title = "Enter desired line width";
	info.nprompt = 1;

	TextPrompt(w, "linewidth", &info, okLineCallback, NULL, NULL);
    } else {
	Arg arg;

	if (width == 1)
	    width = 0;
	MenuCheckItem(w, True);
	GraphicAll(setWidth, (void *) width);

	XtSetArg(arg, XtNlineWidth, width);
	OperationAddArg(arg);
    }
}

/*
** Set brush parameters callbacks
 */
static char brushOpacityStr[10] = "20";
static void 
brushOkCallback(Widget w, XtPointer junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int opacity = atoi(info->prompts[0].rstr);

    if (opacity < 0 || opacity > 100) {
	Notice(w, "Opacity should be between 0 and 100 %");
	return;
    }
    BrushSetParameters(opacity / 100.0);

    sprintf(brushOpacityStr, "%d", opacity);
}

static void 
brushMenuCallback(Widget w)
{
    static TextPromptInfo info;
    static struct textPromptInfo value[1];

    value[0].prompt = "Opacity (%):";
    value[0].str = brushOpacityStr;
    value[0].len = 4;
    info.prompts = value;
    info.title = "Enter the desired brush parameters";
    info.nprompt = 1;

    TextPrompt(w, "brushparams", &info, brushOkCallback, NULL, NULL);
}

/*
** Set spray parameters callbacks
 */
static char sprayDensityStr[10] = "10";
static char sprayRadiusStr[10] = "10";
static char sprayRateStr[10] = "10";
static void 
sprayOkCallback(Widget w, XtPointer junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int radius = atoi(info->prompts[0].rstr);
    int density = atoi(info->prompts[1].rstr);
    int rate = atoi(info->prompts[2].rstr);

    if (radius < 1 || radius > 100) {
	Notice(w, "Radius should be between 1 and 100");
	return;
    }
    if (density < 1 || density > 500) {
	Notice(w, "Density should be between 1 and 500");
	return;
    }
    if (rate < 1 || rate > 500) {
	Notice(w, "Rate should be between 1 and 500");
	return;
    }
    SpraySetParameters(radius, density, rate);

    sprintf(sprayDensityStr, "%d", density);
    sprintf(sprayRadiusStr, "%d", radius);
    sprintf(sprayRateStr, "%d", rate);
}

static void 
sprayMenuCallback(Widget w)
{
    static TextPromptInfo info;
    static struct textPromptInfo value[3];

    value[0].prompt = "Radius:";
    value[0].str = sprayRadiusStr;
    value[0].len = 4;
    value[1].prompt = "Density:";
    value[1].str = sprayDensityStr;
    value[1].len = 4;
    value[2].prompt = "Rate:";
    value[2].str = sprayRateStr;
    value[2].len = 4;
    info.prompts = value;
    info.title = "Enter the desired spray parameters";
    info.nprompt = 3;

    TextPrompt(w, "sprayparams", &info, sprayOkCallback, NULL, NULL);
}

/*
** Gradient fill set parameters callbacks
 */
static char tfillAngleStr[10] = "0";
static char tfillPadStr[10] = "0";
static char tfillHOffsetStr[10] = "0";
static char tfillVOffsetStr[10] = "0";
static char tfillStepsStr[10] = "25";

static void 
tfillOkCallback(Widget w, XtPointer junk, XtPointer infoArg)
{
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int HOffset, VOffset, Pad, Angle, Steps;


    Angle = atoi(info->prompts[0].rstr);
    Pad = atoi(info->prompts[1].rstr);
    HOffset = atoi(info->prompts[2].rstr);
    VOffset = atoi(info->prompts[3].rstr);
    Steps = atoi(info->prompts[4].rstr);
    if (HOffset < -100 || HOffset > 100) {
	Notice(w, "Horizontal offset should be between -100 and 100 %");
	return;
    }
    if (VOffset < -100 || VOffset > 100) {
	Notice(w, "Vertical offset should be between -100 and 100 %");
	return;
    }
    if (Pad < -49 || Pad > 49) {
	Notice(w, "Pad should be between -49 and 49 %");
	return;
    }
    if (Angle < -360 || Angle > 360) {
	Notice(w, "Angle should be between -360 and 360 degrees");
	return;
    }
    if (Steps < 1 || Steps > 300) {
	Notice(w, "Number of steps should be between 1 and 300");
	return;
    }
    TfillSetParameters(VOffset/100.0, HOffset/100.0, Pad/100.0, Angle, Steps);

    sprintf(tfillVOffsetStr, "%3d", VOffset);
    sprintf(tfillHOffsetStr, "%3d", HOffset);
    sprintf(tfillPadStr, "%3d", Pad);
    sprintf(tfillAngleStr, "%3d", Angle);
    sprintf(tfillStepsStr, "%3d", Steps);
}

static void 
tfillMenuCallback(Widget w)
{
    static TextPromptInfo info;
    static struct textPromptInfo value[5];

    value[0].prompt = "Angle (degrees):";
    value[0].str = tfillAngleStr;
    value[0].len = 4;
    value[1].prompt = "Pad (%):";
    value[1].str = tfillPadStr;
    value[1].len = 4;
    value[2].prompt = "Horizontal offset (%):";
    value[2].str = tfillHOffsetStr;
    value[2].len = 4;
    value[3].prompt = "Vertical offset (%):";
    value[3].str = tfillVOffsetStr;
    value[3].len = 4;
    value[4].prompt = "Steps:";
    value[4].str = tfillStepsStr;
    value[4].len = 3;
    info.prompts = value;
    info.title = "Enter the desired gradient fill parameters";
    info.nprompt = 5;

    TextPrompt(w, "tfillparams", &info, tfillOkCallback, NULL, NULL);
}

/*
**  Font menu callbacks.
 */
static void 
fontSetCallback(Widget paint, void *info)
{
    XtVaSetValues(paint, XtNfont, (XFontStruct *) info, NULL);
    FontChanged(paint);
}
static void 
fontSet(Widget w, char *name)
{
    XFontStruct *info;
    Arg arg;

    if (name == NULL) {
	FontSelect(w, None);
    } else {
	if ((info = XLoadQueryFont(XtDisplay(GetShell(w)), name)) == NULL) {
	    XtVaSetValues(w, XtNsensitive, False, NULL);
	    Notice(w, "Unable to load requested font");
	    return;
	}
	GraphicAll(fontSetCallback, (void *) info);
	XtSetArg(arg, XtNfont, info);
	OperationAddArg(arg);
    }
    MenuCheckItem(w, True);
}

/*
**  Exit callback (simple)
 */
static void 
exitOkCallback(Widget w, XtPointer junk, XtPointer junk2)
{
#if 0
    XtDestroyWidget(GetToplevel(w));
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
    Global.timeToDie = True;
#else
    exit(0);
#endif
}
static void 
exitCancelCallback(Widget paint, XtPointer junk, XtPointer junk2)
{
}
static void 
exitPaintCheck(Widget paint, void *sumArg)
{
    int *sum = (int *) sumArg;
    Boolean flg;
    XtVaGetValues(paint, XtNdirty, &flg, NULL);
    *sum += flg ? 1 : 0;
}
static void 
exitPaint(Widget w, XtPointer junk, XtPointer junk2)
{
    int total = 0;

    GraphicAll(exitPaintCheck, (void *) &total);

    if (total == 0) {
	exitOkCallback(w, NULL, NULL);
	return;
    }
    AlertBox(w, "There are unsaved changes,\nare you sure you wish to quit?",
	     exitOkCallback, exitCancelCallback, NULL);
}

/*
**
 */
static void 
loadClipboardCB(Widget w, char *file, void *image)
{
    ClipboardSetImage(w, image);
}
static void 
loadClipboard(Widget w, XtPointer junk, XtPointer junk2)
{
    GetFileName(GetShell(w), 0, NULL, (XtCallbackProc) loadClipboardCB, NULL);
}


/*
**  Button popups 
 */
static void brushPopupCB(Widget w, XtPointer junk, XtPointer junk2);
static void erasePopupCB(Widget w, XtPointer junk, XtPointer junk2);
static void fillPopupCB(Widget w, XtPointer junk, XtPointer junk2);
static void tfillPopupCB2(Widget w, XtPointer junk, XtPointer junk2);
#ifdef FEATURE_FRACTAL
static void ffillPopupCB2(Widget w, XtPointer junk, XtPointer junk2);
#endif
static void selectPopupCB(Widget w, XtPointer junk, XtPointer junk2);
static void selectShapePopupCB(Widget w, XtPointer junk, XtPointer junk2);
static void sprayPopupCB(Widget w, XtPointer junk, XtPointer junk2);
static void ovalPopupCB(Widget w, XtPointer junk, XtPointer junk2);
static void boxPopupCB(Widget w, XtPointer junk, XtPointer junk2);

#define GENERATE_HELP(name, hlpstr)				\
		static PaintMenuItem name [] = {		\
			MI_SEPERATOR(),				\
			MI_SIMPLECB("help", HelpDialog, hlpstr)	\
		};

static PaintMenuItem brushPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("opaque", MF_CHECKON | MF_GROUP1, brushPopupCB, 0),
    MI_FLAGCB("transparent", MF_CHECK | MF_GROUP1, brushPopupCB, 1),
    MI_SIMPLECB("select", BrushSelect, NULL),
    MI_SIMPLECB("parms", changeBrushParmAction, NULL),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.brush"),
};
static PaintMenuItem erasePopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("original", MF_CHECKON, erasePopupCB, 0),
    MI_SIMPLECB("select", changeBrushAction, NULL),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.erase"),
};

static PaintMenuItem sprayPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("gauss", MF_CHECKON, sprayPopupCB, 0),
    MI_SIMPLECB("select", changeSprayAction, NULL),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.spray"),
};

static PaintMenuItem smearPopup[] =
{
    MI_SEPERATOR(),
    MI_SIMPLECB("select", BrushSelect, NULL),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.smear"),
};

GENERATE_HELP(pencilPopup, "toolbox.tools.pencil")
GENERATE_HELP(dotPencilPopup, "toolbox.tools.dotPencil")
GENERATE_HELP(linePopup, "toolbox.tools.line")
GENERATE_HELP(rayPopup, "toolbox.tools.ray")
GENERATE_HELP(arcPopup, "toolbox.tools.arc")
static PaintMenuItem fillPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("fill", MF_CHECKON | MF_GROUP1, fillPopupCB, 0),
    MI_FLAGCB("change", MF_CHECK | MF_GROUP1, fillPopupCB, 1),
    MI_FLAGCB("fill_range", MF_CHECK | MF_GROUP1, fillPopupCB, 2),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.fill"),
};
static PaintMenuItem tfillPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("fill", MF_CHECKON | MF_GROUP1, fillPopupCB, 0),
    MI_FLAGCB("change", MF_CHECK | MF_GROUP1, fillPopupCB, 1),
    MI_FLAGCB("fill_range", MF_CHECK | MF_GROUP1, fillPopupCB, 2),
    MI_SEPERATOR(),
MI_FLAGCB("radial", MF_CHECKON | MF_GROUP2, tfillPopupCB2, TFILL_RADIAL),
  MI_FLAGCB("linear", MF_CHECK | MF_GROUP2, tfillPopupCB2, TFILL_LINEAR),
    MI_FLAGCB("conical", MF_CHECK | MF_GROUP2, tfillPopupCB2, TFILL_CONE),
  MI_FLAGCB("square", MF_CHECK | MF_GROUP2, tfillPopupCB2, TFILL_SQUARE),
    MI_SIMPLECB("parms", changeTFillAction, NULL),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.tfill"),
};
#ifdef FEATURE_FRACTAL
static PaintMenuItem ffillPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("fill", MF_CHECKON | MF_GROUP1, fillPopupCB, 0),
    MI_FLAGCB("change", MF_CHECK | MF_GROUP1, fillPopupCB, 1),
    MI_FLAGCB("fill_range", MF_CHECK | MF_GROUP1, fillPopupCB, 2),
    MI_SEPERATOR(),
    MI_FLAGCB("plasma", MF_CHECKON | MF_GROUP2, ffillPopupCB2, 0),
    MI_FLAGCB("clouds", MF_CHECK | MF_GROUP2, ffillPopupCB2, 1),
    MI_FLAGCB("landscape", MF_CHECK | MF_GROUP2, ffillPopupCB2, 2),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.ffill"),
};
#endif
static PaintMenuItem textPopup[] =
{
    MI_SEPERATOR(),
    MI_SIMPLECB("select", changeFontAction, NULL),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.text"),
};
GENERATE_HELP(polyPopup, "toolbox.tools.poly")
GENERATE_HELP(freehandPopup, "toolbox.tools.blob")
GENERATE_HELP(clinePopup, "toolbox.tools.cline")
static PaintMenuItem selectBPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("shape", MF_CHECKON | MF_GROUP1, selectPopupCB, 0),
    MI_FLAGCB("not_color", MF_CHECK | MF_GROUP1, selectPopupCB, 1),
    MI_FLAGCB("only_color", MF_CHECK | MF_GROUP1, selectPopupCB, 2),
    MI_SEPERATOR(),
    MI_FLAGCB("rectangle", MF_CHECKON | MF_GROUP2, selectShapePopupCB, 0),
    MI_FLAGCB("ellipse", MF_CHECK | MF_GROUP2, selectShapePopupCB, 1),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.select"),
};
static PaintMenuItem lassoPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("shape", MF_CHECKON | MF_GROUP1, selectPopupCB, 0),
    MI_FLAGCB("not_color", MF_CHECK | MF_GROUP1, selectPopupCB, 1),
    MI_FLAGCB("only_color", MF_CHECK | MF_GROUP1, selectPopupCB, 2),
    MI_SEPERATOR(),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.select"),
};
static PaintMenuItem selectAPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("shape", MF_CHECKON | MF_GROUP1, selectPopupCB, 0),
    MI_FLAGCB("not_color", MF_CHECK | MF_GROUP1, selectPopupCB, 1),
    MI_FLAGCB("only_color", MF_CHECK | MF_GROUP1, selectPopupCB, 2),
    MI_SEPERATOR(),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.select"),
};
static PaintMenuItem boxPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("center", MF_CHECK, boxPopupCB, 0),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.box"),
};
static PaintMenuItem fboxPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("center", MF_CHECK, boxPopupCB, 0),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.box"),
};
static PaintMenuItem ovalPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("center", MF_CHECK, ovalPopupCB, 0),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.oval"),
};
static PaintMenuItem fovalPopup[] =
{
    MI_SEPERATOR(),
    MI_FLAGCB("center", MF_CHECK, ovalPopupCB, 0),
    MI_SIMPLECB("help", HelpDialog, "toolbox.tools.oval"),
};

static void 
brushPopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    int nm = (int) junk;

    BrushSetMode(nm);
    MenuCheckItem(brushPopup[nm + 1].widget, True);
}

static void 
ovalPopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    Boolean m = !CircleGetStyle();

    CircleSetStyle(m);
    MenuCheckItem(ovalPopup[1].widget, m);
    MenuCheckItem(fovalPopup[1].widget, m);
}
static void 
boxPopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    Boolean m = !BoxGetStyle();

    BoxSetStyle(m);
    MenuCheckItem(boxPopup[1].widget, m);
    MenuCheckItem(fboxPopup[1].widget, m);
}
static void 
fillPopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    int nm = (int) junk;

    FillSetMode(nm);
    MenuCheckItem(w, True);
}
static void 
tfillPopupCB2(Widget w, XtPointer junk, XtPointer junk2)
{
    int nm = (int) junk;

    TFillSetMode(nm);
    MenuCheckItem(w, True);
}

#ifdef FEATURE_FRACTAL
static void 
ffillPopupCB2(Widget w, XtPointer junk, XtPointer junk2)
{
    int nm = (int) junk;

    FFillSetMode(nm);
    MenuCheckItem(w, True);
}

#endif
static void 
selectPopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    int n = (int) junk;

    MenuCheckItem(selectAPopup[n + 1].widget, True);
    MenuCheckItem(selectBPopup[n + 1].widget, True);
    MenuCheckItem(lassoPopup[n + 1].widget, True);
    SelectSetCutMode(n);
}
static void 
selectShapePopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    int n = (int) junk;

    MenuCheckItem(selectBPopup[n + 1 + 4].widget, True);
    SelectSetShapeMode(n);
}

void 
OperationSelectCallAcross(int n)
{
    MenuCheckItem(selectAPopup[n + 1].widget, True);
    MenuCheckItem(selectBPopup[n + 1].widget, True);
    MenuCheckItem(lassoPopup[n + 1].widget, True);
}
static void 
sprayPopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    Boolean m = !SprayGetStyle();

    SpraySetStyle(m);
    MenuCheckItem(w, m);
}
static void 
erasePopupCB(Widget w, XtPointer junk, XtPointer junk2)
{
    Boolean m = !EraseGetMode();

    EraseSetMode(m);
    MenuCheckItem(w, m);
}

/*
**  Done with operation popup menus
 */

#define GENERATE_OP(name)				\
	static OperationPair  CONCAT(name,Op) = {	\
	  CONCAT(name,Add), CONCAT(name,Remove)		\
	};

GENERATE_OP(DotPencil)
GENERATE_OP(Pencil)
GENERATE_OP(Box)
GENERATE_OP(FBox)
GENERATE_OP(Line)
GENERATE_OP(Ray)
GENERATE_OP(Arc)
GENERATE_OP(Oval)
GENERATE_OP(FOval)
GENERATE_OP(Erase)
GENERATE_OP(Brush)
GENERATE_OP(Font)
GENERATE_OP(Smear)
GENERATE_OP(Spray)
GENERATE_OP(Poly)
GENERATE_OP(FPoly)
GENERATE_OP(Freehand)
GENERATE_OP(FFreehand)
GENERATE_OP(CLine)
GENERATE_OP(Fill)
GENERATE_OP(TFill)
#ifdef FEATURE_FRACTAL
GENERATE_OP(FFill)
#endif
GENERATE_OP(SelectBox)
GENERATE_OP(Lasso)
GENERATE_OP(SelectPoly)
#define	XPMMAP(name)	(char **) CONCAT(name,Op_xpm)
#define MENU(name)	XtNumber(CONCAT(name,Popup)), CONCAT(name,Popup)

typedef struct {
    char *name;
    char **xpmmap;
    OperationPair *data;
    char *translations;
    int nitems;
    PaintMenuItem *popupMenu;
    Pixmap icon;
} IconListItem;

static Widget iconListWidget;
static IconListItem iconList[] =
{
    {"brush", XPMMAP(brush), &BrushOp,
     "<BtnDown>(2): changeBrush()", MENU(brush), None},
    {"erase", XPMMAP(erase), &EraseOp,
     "<BtnDown>(2): changeBrush()", MENU(erase), None},
    {"pencil", XPMMAP(pencil), &PencilOp,
     NULL, MENU(pencil), None},
    {"dotPencil", XPMMAP(dotPen), &DotPencilOp,
     NULL, MENU(dotPencil), None},
    {"selectBox", XPMMAP(select), &SelectBoxOp,
     NULL, MENU(selectB), None},
    {"lasso", XPMMAP(lasso), &LassoOp,
     NULL, MENU(lasso), None},
    {"selectPoly", XPMMAP(selpoly), &SelectPolyOp,
     NULL, MENU(selectA), None},
    {"spray", XPMMAP(spray), &SprayOp,
     "<BtnDown>(2): changeSpray()", MENU(spray), None},
    {"fill", XPMMAP(fill), &FillOp,
     NULL, MENU(fill), None},
    {"tfill", XPMMAP(tfill), &TFillOp,
     "<BtnDown>(2): changeTFillParms()", MENU(tfill), None},
#ifdef FEATURE_FRACTAL
    {"ffill", XPMMAP(ffill), &FFillOp,
     NULL, MENU(ffill), None},
#endif
    {"smear", XPMMAP(smear), &SmearOp,
     "<BtnDown>(2): changeBrush()", MENU(smear), None},
    {"text", XPMMAP(text), &FontOp,
     "<BtnDown>(2): changeFont()", MENU(text), None},
    {"line", XPMMAP(line), &LineOp,
     NULL, MENU(line), None},
    {"ray", XPMMAP(ray), &RayOp,
     NULL, MENU(ray), None},
    {"arc", XPMMAP(arc), &ArcOp,
     NULL, MENU(arc), None},
    {"cLine", XPMMAP(cline), &CLineOp,
     NULL, MENU(cline), None},
    {"box", XPMMAP(box), &BoxOp,
     NULL, MENU(box), None},
    {"fBox", XPMMAP(fbox), &FBoxOp,
     NULL, MENU(fbox), None},
    {"oval", XPMMAP(oval), &OvalOp,
     NULL, MENU(oval), None},
    {"fOval", XPMMAP(foval), &FOvalOp,
     NULL, MENU(foval), None},
    {"polygon", XPMMAP(poly), &PolyOp,
     NULL, MENU(poly), None},
    {"fPolygon", XPMMAP(fpoly), &FPolyOp,
     NULL, MENU(poly), None},
    {"freehand", XPMMAP(freehand), &FreehandOp,
     NULL, MENU(freehand), None},
    {"fFreehand", XPMMAP(ffreehand), &FFreehandOp,
     NULL, MENU(freehand), None},
};

void 
OperationSet(String names[], int num)
{
    IconListItem *match = NULL;
    int i, j;

    for (i = 0; i < XtNumber(iconList); i++) {
	for (j = 0; j < num; j++) {
	    if (strcmp(names[j], iconList[i].name) == 0) {
		if (match == NULL)
		    match = &iconList[i];
		if (CurrentOp == iconList[i].data)
		    return;
	    }
	}
    }
    if (match != NULL)
	XawToggleSetCurrent(iconListWidget, (XtPointer) match->name);
}

static PaintMenuItem lineMenu[] =
{
#if 0
    MI_FLAGCB("0", MF_CHECK | MF_GROUP1, lineWidth, NULL),
#endif
    MI_FLAGCB("1", MF_CHECK | MF_GROUP1, lineWidth, NULL),
    MI_FLAGCB("2", MF_CHECK | MF_GROUP1, lineWidth, NULL),
    MI_FLAGCB("4", MF_CHECK | MF_GROUP1, lineWidth, NULL),
    MI_FLAGCB("6", MF_CHECK | MF_GROUP1, lineWidth, NULL),
    MI_FLAGCB("8", MF_CHECK | MF_GROUP1, lineWidth, NULL),
#define LW_SELECT	5
    MI_FLAGCB("select", MF_CHECK | MF_GROUP1, lineWidth, NULL),
};

static PaintMenuItem fontMenu[] =
{
    MI_FLAGCB("Times 8", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-times-medium-r-normal-*-*-80-*-*-p-*-*-*"),
    MI_FLAGCB("Times 12", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-times-medium-r-normal-*-*-120-*-*-p-*-*-*"),
    MI_FLAGCB("Times 18", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-times-medium-r-normal-*-*-180-*-*-p-*-*-*"),
    MI_FLAGCB("Times Bold 12", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-times-bold-r-normal-*-*-120-*-*-p-*-*-*"),
    MI_FLAGCB("Times Italic 12", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-times-bold-i-normal-*-*-120-*-*-p-*-*-*"),
    MI_FLAGCB("Lucida 12", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-lucida-medium-r-normal-*-*-120-*-*-p-*-*-*"),
    MI_FLAGCB("Helvetica 12", MF_CHECK | MF_GROUP1,
	    fontSet, "-*-helvetica-medium-r-normal-*-*-120-*-*-p-*-*-*"),
    MI_FLAGCB("Helvetica Bold 12", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-helvetica-bold-r-normal-*-*-120-*-*-p-*-*-*"),
    MI_FLAGCB("Fixed 12", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-fixed-medium-r-normal-*-*-120-*-*-m-*-*-*"),
    MI_FLAGCB("Courier 12", MF_CHECK | MF_GROUP1,
	      fontSet, "-*-courier-medium-r-normal-*-*-120-*-*-m-*-*-*"),
    MI_SEPERATOR(),
#define FM_SELECT	11
    MI_FLAGCB("select", MF_CHECK | MF_GROUP1, fontSet, NULL),
};

#if 0
static PaintMenuItem otherMenu[] =
{
    MI_SIMPLECB("brushSelect", BrushSelect, NULL),
#define SP_SELECT	1
    MI_SIMPLECB("sprayEdit", sprayMenuCallback, NULL),
};
#endif

static PaintMenuItem fileMenu[] =
{
    MI_SIMPLECB("new", GraphicCreate, 0),
    MI_SIMPLECB("new-size", GraphicCreate, 2),
    MI_SIMPLECB("open", GraphicCreate, 1),
    MI_SIMPLECB("load-clip", loadClipboard, NULL),
    MI_SEPERATOR(),
    MI_SIMPLECB("quit", exitPaint, NULL),
};

static PaintMenuItem helpMenu[] =
{
    MI_SIMPLECB("about", HelpDialog, "about"),
    MI_SEPERATOR(),
    MI_SIMPLECB("help", HelpDialog, "toolbox.toolbox"),
};

static PaintMenuBar menuBar[] =
{
    {None, "file", XtNumber(fileMenu), fileMenu},
#if 0
    {None, "other", XtNumber(otherMenu), otherMenu},
#endif
    {None, "line", XtNumber(lineMenu), lineMenu},
    {None, "font", XtNumber(fontMenu), fontMenu},
    {None, "help", XtNumber(helpMenu), helpMenu},
};

/*
**  Now for the callback functions
 */
static int argListLen = 0;
static Arg *argList;

void 
OperationSetPaint(Widget paint)
{
    XtSetValues(paint, argList, argListLen);
}

void 
OperationAddArg(Arg arg)
{
    int i;

    for (i = 0; i < argListLen; i++) {
	if (strcmp(argList[i].name, arg.name) == 0) {
	    argList[i].value = arg.value;
	    return;
	}
    }

    if (argListLen == 0)
	argList = (Arg *) XtMalloc(sizeof(Arg) * 2);
    else
	argList = (Arg *) XtRealloc((XtPointer) argList,
				    sizeof(Arg) * (argListLen + 2));

    argList[argListLen++] = arg;
}

/*
**  Double click action callback functions.
 */
static void 
changeLineAction(Widget w, XEvent * event)
{
    lineWidth(lineMenu[LW_SELECT].widget, (int) lineMenu[LW_SELECT].data);
}
static void 
changeSprayAction(Widget w, XEvent * event)
{
    sprayMenuCallback(w);
}
static void 
changeTFillAction(Widget w, XEvent * event)
{
    tfillMenuCallback(w);
}
static void 
changeBrushAction(Widget w, XEvent * event)
{
    BrushSelect(w);
}
static void 
changeBrushParmAction(Widget w, XEvent * event)
{
    brushMenuCallback(w);
}

static void 
changeFontAction(Widget w, XEvent * event)
{
    fontSet(fontMenu[FM_SELECT].widget, (char *) fontMenu[FM_SELECT].data);
}

/*
**  The real init function
 */
void 
OperationInit(Widget toplevel)
{
    static XtActionsRec acts[] =
    {
	{"changeLine", (XtActionProc) changeLineAction},
	{"changeBrush", (XtActionProc) changeBrushAction},
	{"changeFont", (XtActionProc) changeFontAction},
	{"changeSpray", (XtActionProc) changeSprayAction},
	{"changeTFillParms", (XtActionProc) changeTFillAction},
    };
    int i;
    Pixmap pix;
    Widget vport, form, box, icon, bar, firstIcon = None;
    char *defTrans = "<BtnDown>,<BtnUp>: set() notify()\n";
    XtTranslations trans = XtParseTranslationTable(defTrans);
    IconListItem *cur;
    XpmAttributes attributes;

    
    form = XtVaCreateManagedWidget("toolbox",
				   formWidgetClass, toplevel,
				   XtNborderWidth, 0,
				   XtNwidth, 300,
				   NULL);
    XtAppAddActions(XtWidgetToApplicationContext(toplevel),
		    acts, XtNumber(acts));

    /*
    **  Create the menu bar
     */
    bar = MenuBarCreate(form, XtNumber(menuBar), menuBar);

    /*
    **  Create the operation icon list
     */
    vport = XtVaCreateManagedWidget("vport",
				    viewportWidgetClass, form,
				    XtNallowVert, True,
				    XtNuseRight, True,
				    XtNfromVert, bar,
				    NULL);
    box = XtVaCreateManagedWidget("box",
				  boxWidgetClass, vport,
				  XtNwidth, 32 * 8,
				  XtNtop, XtChainTop,
				  NULL);

    for (i = 0; i < XtNumber(iconList); i++) {
	cur = &iconList[i];

	icon = XtVaCreateManagedWidget(cur->name,
				       toggleWidgetClass, box,
				       XtNtranslations, trans,
				       XtNradioGroup, firstIcon,
				       XtNradioData, cur->name,
				       NULL);

	if (cur->xpmmap != NULL) {
	    attributes.valuemask = XpmCloseness;
            attributes.closeness =  40000;
	    XpmCreatePixmapFromData(XtDisplay(box),
				    DefaultRootWindow(XtDisplay(box)),
				    cur->xpmmap,
				    &pix, NULL, &attributes);
	} else {
	    pix = None;
	}

	if (pix == None) {
	    printf("Unable to allocate sufficient color entries.\n");
	    printf("Try exiting other color intensive applications.\n");
	    exit(1);
	}
	
	cur->icon = pix;

	if (pix != None)
	    XtVaSetValues(icon, XtNbitmap, pix, NULL);
	if (cur->translations != NULL) {
	    XtTranslations moreTrans;

	    moreTrans = XtParseTranslationTable(cur->translations);
	    XtAugmentTranslations(icon, moreTrans);
	}
	if (firstIcon == NULL) {
	    XtVaSetValues(icon, XtNstate, True, NULL);
	    setOperation(icon, cur->data, NULL);

	    firstIcon = icon;
	    iconListWidget = icon;
	}
	XtAddCallback(icon, XtNcallback,
		      setOperation, cur->data);

	if (cur->nitems != 0 && cur->popupMenu != NULL)
	    MenuPopupCreate(icon, cur->nitems, cur->popupMenu);
    }

    lineWidth(lineMenu[0].widget, (int) lineMenu[0].data);
    fontSet(fontMenu[1].widget, fontMenu[1].data);

    AddDestroyCallback(toplevel, (DestroyCallbackFunc) exitPaint, NULL);
}

Image *
OperationIconImage(Widget w, char *name)
{
    int i;

    for (i = 0; i < XtNumber(iconList); i++)
	if (strcmp(name, iconList[i].name) == 0)
	    break;
    if (i == XtNumber(iconList) || iconList[i].icon == None)
	return NULL;

    return PixmapToImage(w, iconList[i].icon, None);
}
