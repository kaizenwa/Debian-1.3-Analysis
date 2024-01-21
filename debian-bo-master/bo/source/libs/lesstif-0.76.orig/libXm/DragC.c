/**
 *
 * $Id: DragC.c,v 1.9 1997/01/14 04:16:55 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

/* This code owes a lot to Daniel Dardailler, who wrote up an
   initial implementation of the dynamic drag protocol for an X/Open
   contract while still with the X consortium.
   
   Here's his copyright notice:

   Copyright 1996 Daniel Dardailler.  
   Permission to use, copy, modify, distribute, and sell this software
   for any purpose is hereby granted without fee, provided that the above
   copyright notice appear in all copies and that both that copyright
   notice and this permission notice appear in supporting documentation,
   and that the name of Daniel Dardailler not be used in advertising or
   publicity pertaining to distribution of the software without specific,
   written prior permission.  Daniel Dardailler makes no representations
   about the suitability of this software for any purpose.  It is
   provided "as is" without express or implied warranty.
   */

static char rcsid[] = "$Id: DragC.c,v 1.9 1997/01/14 04:16:55 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/DragCP.h>
#include <Xm/AtomMgr.h>
#include <Xm/MenuUtilP.h> /* for the _XmGrab stuff */
#include <X11/cursorfont.h> /* ### CT */

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);

static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

static void _XmDCStartDrag(XmDragContext dc,
			   Widget srcW,
			   XEvent *event);
static void _XmDCCancelDrag(XmDragContext dc);

/* static? */ unsigned char _XmByteOrder(void);
static void swallow_button(Widget w, XtPointer clientData, XEvent *event, Boolean *cont);

#define Offset(field) XtOffsetOf(XmDragContextRec, drag.field)

/* Resources for the pushButton class */

static XtResource resources[] = {
    {
	XmNsourceWidget, XmCSourceWidget, XmRWidget,
	sizeof(Widget), Offset(sourceWidget),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNexportTargets, XmCExportTargets, XmRAtomList,
	sizeof(Atom *), Offset(exportTargets),
	XmRImmediate, NULL
    },
    {
	XmNnumExportTargets, XmCNumExportTargets, XmRInt,
	sizeof(Cardinal), Offset(numExportTargets),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNconvertProc, XmCConvertProc, XmRFunction,
	sizeof(XmConvertSelectionRec), Offset(convertProc),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNclientData, XmCClientData, XtRPointer,
	sizeof(XtPointer), Offset(clientData),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNincremental, XmCIncremental, XmRBoolean,
	sizeof(Boolean), Offset(incremental),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdragOperations, XmCDragOperations, XmRUnsignedChar,
	sizeof(unsigned char), Offset(dragOperations),
	XmRImmediate, (XtPointer)(XmDROP_COPY | XmDROP_MOVE)
    },
    {
	XmNsourceCursorIcon, XmCSourceCursorIcon, XmRWidget,
	sizeof(Widget), Offset(sourceCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNsourcePixmapIcon, XmCSourcePixmapIcon, XmRWidget,
	sizeof(Widget), Offset(sourcePixmapIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNstateCursorIcon, XmCStateCursorIcon, XmRWidget,
	sizeof(Widget), Offset(stateCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNoperationCursorIcon, XmCOperationCursorIcon, XmRWidget,
	sizeof(Widget), Offset(operationCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNcursorBackground, XmCCursorBackground, XmRPixel,
	sizeof(Pixel), Offset(cursorBackground),
	XmRCallProc, (XtPointer)NULL
    },
    {
	XmNcursorForeground, XmCCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(cursorForeground),
	XmRCallProc, (XtPointer)NULL
    },
    {
	XmNvalidCursorForeground, XmCValidCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(validCursorForeground),
	XtRCallProc, NULL
    },
    {
	XmNinvalidCursorForeground, XmCInvalidCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(invalidCursorForeground),
	XmRCallProc, NULL
    },
    {
	XmNnoneCursorForeground, XmCNoneCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(noneCursorForeground),
	XmRCallProc, NULL
    },
    {
	XmNdropSiteEnterCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(siteEnterCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropSiteLeaveCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(siteLeaveCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtopLevelEnterCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(topLevelEnterCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdragMotionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dragMotionCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtopLevelLeaveCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(topLevelLeaveCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropStartCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dropStartCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdragDropFinishCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dragDropFinishCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropFinishCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dropFinishCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNoperationChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(operationChangedCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNblendModel, XmCBlendModel, XmRBlendModel,
	sizeof(unsigned char), Offset(blendModel),
	XtRImmediate, (XtPointer)XmBLEND_ALL
    },
    {
	XmNsourceIsExternal, XmCSourceIsExternal, XmRBoolean,
	sizeof(Boolean), Offset(sourceIsExternal),
	XtRImmediate, (XtPointer)False
    },
    {
	XmNsourceWindow, XmCSourceWindow, XmRWindow,
	sizeof(Window), Offset(srcWindow),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNstartTime, XmCStartTime, XmRInt,
	sizeof(Time), Offset(dragStartTime),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNiccHandle, XmCICCHandle, XmRAtom,
	sizeof(Atom), Offset(iccHandle),
	XtRImmediate, (XtPointer)0
    }
};

static void _XmDCCancel(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void _XmDCMotion(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void _XmDCFinish(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void _XmDCHelp(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmDCTranslations[] =
   "<Btn2Motion>:         DragMotion()\n\
    <Btn2Up>:             FinishDrag()\n\
    <Key>osfCancel:       CancelDrag()\n\
    <Key>osfHelp:         HelpDrag()";

static XtActionsRec actions[] = {
    {"CancelDrag", _XmDCCancel},
    {"DragMotion", _XmDCMotion},
    {"FinishDrag", _XmDCFinish},
    {"HelpDrag", _XmDCHelp}
};

static XmBaseClassExtRec _XmDragCCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL, /* FIXME */
    /* set_values_prehook        */ NULL, /* FIXME */
    /* initialize_posthook       */ NULL, /* FIXME */
    /* set_values_posthook       */ NULL, /* FIXME */
    /* secondary_object_class    */ NULL, /* FIXME */
    /* secondary_object_create   */ NULL, /* FIXME */
    /* get_secondary_resources   */ NULL, /* FIXME */
    /* fast_subclass             */ { 0 }, /* FIXME */
    /* get_values_prehook        */ NULL, /* FIXME */
    /* get_values_posthook       */ NULL, /* FIXME */
    /* class_part_init_prehook   */ NULL, /* FIXME */
    /* class_part_init_posthook  */ NULL, /* FIXME */
    /* ext_resources             */ NULL, /* FIXME */
    /* compiled_ext_resources    */ NULL, /* FIXME */
    /* num_ext_resources         */ 0, /* FIXME */
    /* use_sub_resources         */ FALSE, /* FIXME */
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmDragContextClassRec xmDragContextClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &coreClassRec,
        /* class_name            */ "XmDragContext",
	/* widget_size           */ sizeof(XmDragContextRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmDCTranslations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmDragCCoreClassExtRec
    },
    /* DragContext Class part */
    {
        /* start     */ _XmDCStartDrag,
        /* cancel    */ _XmDCCancelDrag,
        /* extension */ NULL,
    }
};

WidgetClass xmDragContextClass = (WidgetClass)&xmDragContextClassRec;

static void
class_initialize()
{
    _XmDragCCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDRAG_CONTEXT_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "Initialize\n");

    XtWidth(new_w) = XtHeight(new_w) = 1;
    XtX(new_w) = XtY(new_w) = -100;

    XtManageChild(new_w);

    XdbDebug(__FILE__, new_w, "Initialize: Parent %s window 0x%X us 0x%X\n", 
	XtName(XtParent(new_w)), XtWindow(XtParent(new_w)), XtWindow(new_w));
}

static void
destroy(Widget w)
{
    XdbDebug(__FILE__, w, "Destroy\n");
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "SetValues\n");
    return False;
}

static void 
_XmDCStartDrag(XmDragContext dc,
	       Widget srcW,
	       XEvent *event)
{
    XdbDebug(__FILE__, srcW, "_XmDCStartDrag\n");

    DC_SrcWidget(dc) = srcW;
    DC_SrcWindow(dc) = XtWindow(srcW);

    DC_DragStartTime(dc) = event->xbutton.time;
    DC_LastChangeTime(dc) = DC_DragStartTime(dc);

    XGrabPointer(XtDisplay(dc), DefaultRootWindow(XtDisplay(dc)), True,
		 ButtonPressMask | ButtonMotionMask | ButtonReleaseMask |
		 EnterWindowMask | LeaveWindowMask,
		 GrabModeSync, GrabModeAsync,
		 None, XCreateFontCursor(XtDisplay(dc), XC_pirate), /* ### CT */
		 DC_DragStartTime(dc));

    XtAddEventHandler((Widget)dc, ButtonPressMask, False, swallow_button, NULL);

    XtAddGrab((Widget)dc, True, True);

    XAllowEvents(XtDisplay(dc), SyncPointer, DC_LastChangeTime(dc));
}

static void 
_XmDCCancelDrag(XmDragContext dc)
{
    XdbDebug(__FILE__, NULL, "_XmDCCancelDrag\n");
    XUngrabPointer(XtDisplay(dc), DC_LastChangeTime(dc));
    XtRemoveGrab((Widget)dc);
}

static void
_XmDCCancel(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmDCCancel\n");

    DC_LastChangeTime(w) = event->xbutton.time;

    XUngrabPointer(XtDisplay(w), DC_LastChangeTime(w));
    XtRemoveGrab((Widget)w);
}

static void 
_XmDCMotion(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmDCMotion\n");

    DC_LastChangeTime(w) = event->xbutton.time;
}

static void 
_XmDCFinish(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmDCFinish\n");

    DC_DragFinishTime(w) = event->xbutton.time;
    DC_LastChangeTime(w) = DC_DragFinishTime(w);

    XUngrabPointer(XtDisplay(w), DC_LastChangeTime(w));
    XtRemoveGrab(w);
}

static void 
_XmDCHelp(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "_XmDCHelp\n");

    DC_LastChangeTime(w) = event->xbutton.time;
}

XmDragReceiverInfo
_XmAllocReceiverInfo(XmDragContext dc)
{
    XdbDebug(__FILE__, NULL, "_XmAllocReceiverInfo\n");
    return NULL;
}

unsigned char
_XmGetActiveProtocolStyle(Widget w)
{
    XdbDebug(__FILE__, w, "_XmGetActiveProtocolStyle\n");
    return DC_ActiveProtocolStyle(w);
}

unsigned char 
_XmByteOrder(void)
{
  static unsigned char byte_order = 0;
  
  if (!byte_order) {
    unsigned int endian = 1;
    byte_order = (*((char *)&endian))?'l':'B';
  }
  return byte_order ;
}

/* 
 * We have to allowevents here or else if someone is dragging with
 * button two down and hits button one, we freeze.
 */
static void
swallow_button(Widget w, XtPointer clientData, XEvent *event, Boolean *cont)
{
  XAllowEvents(XtDisplay(w), SyncPointer, event->xbutton.time);
}
