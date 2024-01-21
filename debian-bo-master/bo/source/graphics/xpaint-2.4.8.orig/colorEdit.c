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

/* $Id: colorEdit.c,v 1.3 1996/04/19 09:06:29 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include "misc.h"
#include "palette.h"
#include "color.h"
#include "protocol.h"

typedef struct {
    XtCallbackProc okProc;
    XtPointer closure;
    Widget wIn;

    Widget shell, pick;
    Palette *map;
    Boolean allowWrite;
    XColor origColor;
} LocalInfo;

static void 
commonCB(LocalInfo * info, XColor * col)
{
    Widget cw = info->wIn;
    XtCallbackProc proc = info->okProc;
    XtPointer closure = info->closure;

    XtDestroyWidget(info->shell);
    XtFree((XtPointer) info);

    proc(cw, closure, (XtPointer) col);
}

static void 
changeBgCancel(Widget w, LocalInfo * info, XtPointer junk2)
{
    if (info->allowWrite)
	PaletteSetPixel(info->map, ColorPickerGetPixel(info->pick),
			&info->origColor);
    commonCB(info, NULL);
}
static void 
changeBgOk(Widget w, LocalInfo * info, XtPointer junk2)
{
    XColor xcol;

    xcol = *ColorPickerGetXColor(info->pick);

    commonCB(info, &xcol);
}

void 
ColorEditor(Widget w, Pixel pixel, Palette * map, Boolean allowWrite,
	    XtCallbackProc okProc, XtPointer closure)
{
    Position x, y;
    Widget shell, form, ok, cancel, pick;
    Pixel pix;
    LocalInfo *info = XtNew(LocalInfo);

    info->okProc = okProc;
    info->closure = closure;
    info->wIn = w;
    info->allowWrite = allowWrite;

    XtVaGetValues(GetShell(w), XtNx, &x, XtNy, &y, NULL);

    shell = XtVaCreatePopupShell("colorEditDialog",
				 transientShellWidgetClass, GetShell(w),
				 XtNx, x + 24,
				 XtNy, y + 24,
				 XtNcolormap, map->cmap,
				 NULL);
    PaletteAddUser(map, shell);

    form = XtVaCreateManagedWidget("form", formWidgetClass, shell,
				   NULL);

    if (allowWrite) {
	pix = pixel;
    } else {
	pix = PaletteGetUnused(map);
    }
    pick = ColorPickerPalette(form, map, &pix);
    info->origColor = *PaletteLookup(map, pixel);
    ColorPickerSetXColor(pick, &info->origColor);

    ok = XtVaCreateManagedWidget("ok",
				 commandWidgetClass, form,
				 XtNfromVert, pick,
				 XtNtop, XtChainBottom,
				 XtNbottom, XtChainBottom,
				 XtNleft, XtChainLeft,
				 XtNright, XtChainLeft,
				 NULL);

    cancel = XtVaCreateManagedWidget("cancel",
				     commandWidgetClass, form,
				     XtNfromVert, pick,
				     XtNfromHoriz, ok,
				     XtNtop, XtChainBottom,
				     XtNbottom, XtChainBottom,
				     XtNleft, XtChainLeft,
				     XtNright, XtChainLeft,
				     NULL);
    XtAddCallback(cancel, XtNcallback,
		  (XtCallbackProc) changeBgCancel, (XtPointer) info);
    XtAddCallback(ok, XtNcallback,
		  (XtCallbackProc) changeBgOk, (XtPointer) info);
    AddDestroyCallback(shell,
		       (DestroyCallbackFunc) changeBgCancel, (XtPointer) info);

    info->shell = shell;
    info->pick = pick;
    info->map = map;

    XtPopup(shell, XtGrabNone);
}
