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

/* $Id: size.c,v 1.4 1996/05/31 06:25:37 torsten Exp $ */

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Text.h>
#include <stdio.h>

#ifndef NOSTDHDRS
#include <stdlib.h>
#include <unistd.h>
#endif

#include "Paint.h"
#include "misc.h"
#include "text.h"


typedef struct {
    Widget widget, paint;
    int w, h;
    int z;
    void (*func) (Widget, int, int, int);
} arg_t;

static void 
cancelSizeCallback(Widget w, XtPointer arg, XtPointer junk)
{
    XtFree((XtPointer) arg);
}

static void 
sureCallback(Widget w, XtPointer argArg, XtPointer junk)
{
    arg_t *arg = (arg_t *) argArg;
    XtVaSetValues(arg->paint, XtNdrawWidth, arg->w, XtNdrawHeight, arg->h,
		  XtNdirty, True, NULL);
    FatbitsUpdate(arg->paint, -1);
    XtFree((XtPointer) arg);
}

static void 
okSizeCallback(Widget w, XtPointer argArg, XtPointer infoArg)
{
    arg_t *arg = (arg_t *) argArg;
    TextPromptInfo *info = (TextPromptInfo *) infoArg;
    int width, height;

    arg->w = atoi(info->prompts[0].rstr);
    arg->h = atoi(info->prompts[1].rstr);
    if (arg->paint == None)
	arg->z = atoi(info->prompts[2].rstr);

    if (arg->paint != None)
	XtVaGetValues(arg->paint, XtNdrawWidth, &width,
		      XtNdrawHeight, &height,
		      NULL);

    if (arg->w <= 0) {
	Notice(w, "Invalid width");
    } else if (arg->h <= 0) {
	Notice(w, "Invalid height");
    } else if (arg->paint == None) {
	arg->func(arg->widget, arg->w, arg->h, arg->z);
    } else if (arg->w != width || arg->h != height) {
	AlertBox(GetShell(arg->paint),
		 "Warning: this operation cannot be undone\nContinue?",
		 sureCallback, cancelSizeCallback, arg);
	/* don't free */
	return;
    }
    XtFree((XtPointer) arg);
}

void 
SizeSelect(Widget w, Widget paint, void (*func) (Widget, int, int, int))
{
    static TextPromptInfo info;
    static struct textPromptInfo values[4];
    int width, height, zoom;
    arg_t *arg = XtNew(arg_t);
    char bufA[16], bufB[16], bufC[16];

    info.prompts = values;
    info.nprompt = (paint == None) ? 3 : 2;
    info.title = "Enter the desired image size:";

    values[0].prompt = "Width:";
    values[0].str = bufA;
    values[0].len = 5;
    values[1].prompt = "Height:";
    values[1].str = bufB;
    values[1].len = 5;
    values[2].prompt = "Zoom:";
    values[2].str = bufC;
    values[2].len = 5;

    if (paint != None) {
	XtVaGetValues(paint, XtNdrawWidth, &width,
		      XtNdrawHeight, &height,
		      XtNzoom, &zoom,
		      NULL);
    } else {
	GetDefaultWH(&width, &height);
	zoom = 1;
    }

    sprintf(bufA, "%d", (int) width);
    sprintf(bufB, "%d", (int) height);
    sprintf(bufC, "%d", (int) zoom);

    arg->widget = w;
    arg->paint = paint;
    arg->func = func;

    TextPrompt(w, "sizeselect", &info, okSizeCallback, cancelSizeCallback, arg);
}
