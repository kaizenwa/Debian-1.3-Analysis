/**
 *
 * $Id: DrawnB.c,v 1.7 1996/12/18 00:45:18 miers Exp $
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

static char rcsid[] = "$Id: DrawnB.c,v 1.7 1996/12/18 00:45:18 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/DrawnBP.h>
#include <Xm/TransltnsP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static void resize(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void button_release(Widget w, XtPointer client_data, XEvent *event,
                           Boolean *continue_dispatch);


/*
 * Resources for the pushButton class
 */
#define Offset(field) XtOffsetOf(XmDrawnButtonRec, drawnbutton.field)
static XtResource resources[] = {
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)XmMULTICLICK_KEEP
    },
    {
	XmNpushButtonEnabled, XmCPushButtonEnabled, XmRBoolean,
	sizeof(Boolean), Offset(pushbutton_enabled),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNshadowType, XmCShadowType, XmRShadowType,
	sizeof(unsigned char), Offset(shadow_type),
	XmRImmediate, (XtPointer)XmSHADOW_ETCHED_IN
    },
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNarmCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(arm_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdisarmCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(disarm_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNexposeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(expose_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNresizeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(resize_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmDrawnButtonRec, primitive.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), XtOffsetOf(XmDrawnButtonRec, label._label),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmDrawnButtonRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmDrawnButtonRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)2
    }
};

static void Arm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Activate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmDrawnB_defaultTranslations[] = 
   "<EnterWindow>:        EnterWindow()\n\
    <LeaveWindow>:        LeaveWindow()\n\
    <Btn1Down>:           Arm()\n\
    <Btn1Down>,<Btn1Up>:  Activate() Disarm()\n\
    <Btn1Up>:             Activate() Disarm()\n\
    <Btn1Down>(2+):       MultiArm()\n\
    <Btn1Up>(2+):         MultiActivate()\n\
    <Key>osfActivate:     PrimitiveParentActivate()\n\
    <Key>osfCancel:       PrimitiveParentCancel()\n\
    <Key>osfSelect:       ArmAndActivate()\n\
    <Key>osfHelp:         Help()\n\
    ~s ~m ~a <Key>Return: PrimitiveParentActivate()\n\
    ~s ~m ~a <Key>space:  ArmAndActivate()";

static XtActionsRec actions[] = {
    {"Arm", Arm},
    {"Activate", Activate},
    {"Disarm", Disarm},
    {"ArmAndActivate", ArmAndActivate},
    {"Help", Help},
    {"EnterWindow", EnterWindow},
    {"LeaveWindow", LeaveWindow},
    {"MultiArm", MultiArm},
    {"MultiActivate", MultiActivate}
};

static XmBaseClassExtRec _XmDrawnBCoreClassExtRec = {
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
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmDrawnBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmDrawnButtonClassRec xmDrawnButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
        /* class_name            */ "XmDrawnButton",
	/* widget_size           */ sizeof(XmDrawnButtonRec),
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
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmDrawnB_defaultTranslations,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmDrawnBCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
       	/* border_unhighlight    */ XmInheritBorderUnhighlight,
       	/* translations          */ _XmDrawnB_defaultTranslations,
       	/* arm_and_activate_proc */ XmInheritArmAndActivate,
       	/* Synthetic Resources   */ NULL, 
        /* num syn res           */ 0,
	/* extension             */ (XtPointer)&_XmDrawnBPrimClassExtRec
    },
    /* Label Class part */
    {
        /* setOverrideCallback */ XmInheritSetOverrideCallback,
        /* menuProcs           */ XmInheritMenuProc,
        /* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* DrawnButton Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmDrawnButtonWidgetClass = (WidgetClass)&xmDrawnButtonClassRec;

static void
class_initialize()
{
    _XmDrawnBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDRAWN_BUTTON_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    if (!Lab_Font(new_w))
	Lab_Font(new_w) = _XmGetDefaultFontList(new_w,
					      XmBUTTON_FONTLIST);

    if (Lab_Label(new_w)) 
    {
       _XmStringFree(Lab_Label(new_w));
       Lab_Label(new_w) = _XmStringCreate(XmStringCreateSimple("\0"));
    }

    DB_Armed(new_w) = False;

    XtAddEventHandler(new_w, ButtonReleaseMask, False, button_release, NULL);

}

static void
destroy(Widget w)
{
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = True;

    XdbDebug(__FILE__, new_w, "XmDrawnButton %s SetValues\n", XtName(new_w));

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    XmDrawnButtonCallbackStruct cbs;

#define superclass (&xmLabelClassRec)
    (*superclass->core_class.expose)(w, event, region);
#undef superclass

    if (DB_PushButtonEnabled(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w), 
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       (XtSensitive(w) && DB_Armed(w)) ? XmSHADOW_IN : XmSHADOW_OUT);
    }
    else {
	_XmDrawShadows(XtDisplay(w), XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w), 
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       DB_ShadowType(w));
    }

    if (DB_ExposeCallback(w)) {
	cbs.reason = XmCR_EXPOSE;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

        XtCallCallbackList(w,
			   DB_ExposeCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
resize(Widget w)
{
    XmDrawnButtonCallbackStruct cbs;

    if (!XtIsRealized(w))
	return;

    if (DB_PushButtonEnabled(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w), 
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       (XtSensitive(w) && DB_Armed(w)) ? XmSHADOW_IN : XmSHADOW_OUT);
    }
    else {
	_XmDrawShadows(XtDisplay(w), XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w), 
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       DB_ShadowType(w));
    }
    if (DB_ResizeCallback(w)) {
	cbs.reason = XmCR_RESIZE;
	cbs.event = NULL;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   DB_ResizeCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
button_release(Widget w,
               XtPointer client_data,
               XEvent *event,
               Boolean *continue_dispatch)
{
    if (event->xany.type == ButtonRelease)
    {
        /* check to see if the event happened outside the
           widget.  The action routines will handle it
           if it was inside */

        if (event->xbutton.x < 0
            || event->xbutton.x >= XtWidth(w)
            || event->xbutton.y < 0
            || event->xbutton.y >= XtHeight(w))
        {
            DB_Armed(w) = False;
            XtCallActionProc(w, "Disarm", event, NULL, 0);
            *continue_dispatch = False;
        }
    }
}

static void
Arm(Widget w,
    XEvent *event,
    String *params,
    Cardinal *num_params)
{
    XmDrawnButtonCallbackStruct cbs;
    XtExposeProc exp;

    DB_Armed(w) = True;

    exp = XtClass(w)->core_class.expose;
    (*exp)(w, event, (Region)NULL);

    if (DB_ArmCallback(w)) {
	cbs.reason = XmCR_ARM;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   DB_ArmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void 
Activate(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    XmDrawnButtonCallbackStruct cbs;
    XtExposeProc exp;

    DB_ClickCount(w) = 1;

    DB_Armed(w) = False;

    exp = XtClass(w)->core_class.expose;
    (*exp)(w, event, (Region)NULL);

    if (DB_ActivateCallback(w)) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   DB_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
}

static void 
Disarm(Widget w,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XmDrawnButtonCallbackStruct cbs;
    XtExposeProc exp;

    if (DB_Armed(w)) {
	exp = XtClass(w)->core_class.expose;
	(*exp)(w, event, (Region)NULL);
    }

    if (DB_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   DB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
ArmAndActivate(Widget w,
	       XEvent *event,
	       String *params,
	       Cardinal *num_params)
{
    Arm(w, event, params, num_params);
    Activate(w, event, params, num_params);
    Disarm(w, event, params, num_params);
}

static void 
Help(Widget w,
     XEvent *event,
     String *params,
     Cardinal *num_params)
{
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

static void
EnterWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    _XmPrimitiveEnter(w, event, NULL, NULL);

    if (DB_PushButtonEnabled(w) && XtSensitive(w) && DB_Armed(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w), 
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       XmSHADOW_IN);
    }
}

static void
LeaveWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    _XmPrimitiveLeave(w, event, NULL, NULL);

    if (DB_PushButtonEnabled(w) && XtSensitive(w) && DB_Armed(w)) {
	_XmDrawShadows(XtDisplay(w), XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w), 
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       XmSHADOW_OUT);
    }
}

static void
MultiArm(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "MultiArm\n");

    if (DB_MultiClick(w) == XmMULTICLICK_KEEP)
	Arm(w, event, NULL, NULL);
}

static void
MultiActivate(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    XmDrawnButtonCallbackStruct cbs;
    XtExposeProc exp;

    XdbDebug(__FILE__, w, "DrawnB: MultiClick\n");

    if (DB_MultiClick(w) == XmMULTICLICK_KEEP) {
	Time mctime = XtGetMultiClickTime(XtDisplay(w));

	if ((event->xbutton.time - DB_ArmTimeStamp(w)) < mctime)
	    DB_ClickCount(w)++;
	else
	    DB_ClickCount(w) = 1;

	DB_Armed(w) = False;

	exp = XtClass(w)->core_class.expose;
	(*exp)(w, event, NULL);

	if (ev->type == KeyPress || ev->type == KeyRelease
	    || ((ev->x >= 0 && ev->x < XtWidth(w))
	    && (ev->y >= 0 && ev->y < XtHeight(w))))
	{
	    if (DB_MultiClick(w) == XmMULTICLICK_DISCARD &&
		DB_ClickCount(w) > 1)
		return;

	    if (DB_ActivateCallback(w)) {
		cbs.reason = XmCR_ACTIVATE;
		cbs.event = event;
		cbs.click_count = DB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   DB_ActivateCallback(w),
				   (XtPointer)&cbs);
	    }
	}
	Disarm(w, event, params, num_params);
    }
}

Widget
XmCreateDrawnButton(Widget parent,
		   char *name,
		   Arg *arglist,
		   Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmDrawnButtonWidgetClass,
			  parent,
			  arglist,
			  argcount);
}
