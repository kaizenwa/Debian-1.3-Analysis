/**
 *
 * $Id: ArrowB.c,v 1.10 1996/12/18 02:35:18 miers Exp $
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

static char rcsid[] = "$Id: ArrowB.c,v 1.10 1996/12/18 02:35:18 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ArrowBP.h>
#include <Xm/TransltnsP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

/*
 * Resources for the ArrowButton class
 */
#define Offset(field) XtOffsetOf(XmArrowButtonRec, arrowbutton.field)
static XtResource resources[] = {
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)XmMULTICLICK_KEEP
    },
    {
	XmNarrowDirection, XmCArrowDirection, XmRArrowDirection,
	sizeof(unsigned char), Offset(direction),
	XmRImmediate, (XtPointer)XmARROW_UP
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
    }
};


static void Activate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Arm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmArrowB_defaultTranslations[] =
    "<Btn1Down>:            Arm()\n\
     <Btn1Down>,<Btn1Up>:   Activate() Disarm()\n\
     <Btn1Down>(2+):        MultiArm()\n\
     <Btn1Up>(2+):          MultiActivate()\n\
     <Btn1Up>:              Activate() Disarm()\n\
     <EnterWindow>:         Enter()\n\
     <LeaveWindow>:         Leave()\n\
     :<Key>osfActivate:     PrimitiveParentActivate()\n\
     :<Key>osfCancel:       PrimitiveParentCancel()\n\
     :<Key>osfSelect:       ArmAndActivate()\n\
     :<Key>osfHelp:         Help()\n\
     ~s ~m ~a <Key>Return:  PrimitiveParentActivate()\n\
     ~s ~m ~a <Key>space:   ArmAndActivate()";

static XtActionsRec actions[] = {
    {"Activate",                Activate},
    {"Arm",                     Arm},
    {"ArmAndActivate",          ArmAndActivate},
    {"Disarm",                  Disarm},
    {"Enter",                   EnterWindow},
    {"Help",                    Help},
    {"Leave",                   LeaveWindow},
    {"MultiActivate",           MultiActivate},
    {"MultiArm",                MultiArm},
    {"PrimitiveParentActivate", _XmPrimitiveParentActivate},
    {"PrimitiveParentCancel",   _XmPrimitiveParentCancel}
};

static XmBaseClassExtRec _XmArrowBCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmArrowBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmArrowButtonClassRec xmArrowButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name            */ "XmArrowButton",
	/* widget_size           */ sizeof(XmArrowButtonRec),
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
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmArrowB_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmArrowBCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ _XmArrowB_defaultTranslations,
        /* arm_and_activate_proc */ XmInheritArmAndActivate,
        /* synthetic resources   */ NULL,
	/* num syn res           */ 0,
        /* extension             */ (XtPointer)&_XmArrowBPrimClassExtRec,
    },
    /* ArrowButton Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmArrowButtonWidgetClass = (WidgetClass)&xmArrowButtonClassRec;

static void
class_initialize()
{
    _XmArrowBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmARROW_BUTTON_BIT);
}

static void
CreateArrowGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;

    AB_ArrowGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateInsensitiveGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction | GCStipple |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask |
		GCTileStipXOrigin | GCTileStipYOrigin;

    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple = XmGetPixmapByDepth(XtScreen(w),
					XmEVEN_STIPPLE_IMAGE,
					WhitePixelOfScreen(XtScreen(w)),
					BlackPixelOfScreen(XtScreen(w)),
					1);

    AB_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    if (XtWidth(request) == (Dimension)0)
	XtWidth(new_w) = 20;
    if (XtHeight(request) == (Dimension)0)
	XtHeight(new_w) = 20;

    AB_Armed(new_w) = False;

    CreateArrowGC(new_w);
    CreateInsensitiveGC(new_w);
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, AB_ArrowGC(w));
    XtReleaseGC(w, AB_InsensitiveGC(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmArrowButtonWidget lw = (XmArrowButtonWidget) new_w;
    XmArrowButtonWidget ow = (XmArrowButtonWidget) old;
    Boolean refresh_needed = False;

    XdbDebug(__FILE__, new_w, "ArrowB set_values\n");
    if (lw->arrowbutton.direction != ow->arrowbutton.direction)
	refresh_needed = True;

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    Boolean State;
    int x,y;
    int xsize;
    int ysize;
    int margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) + 1;
    GC myGC;
    

    x = margin;
    y = margin;
    xsize = XtWidth(w) - margin * 2;
    ysize = XtHeight(w) - margin * 2;

    if (XtSensitive(w))
	myGC = AB_ArrowGC(w);
    else
	myGC = AB_InsensitiveGC(w);

    _XmDrawShadows(XtDisplay(w), 
		   XtWindow(w), 
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   Prim_HighlightThickness(w),
		   Prim_HighlightThickness(w),
		   XtWidth(w) - 2 * Prim_HighlightThickness(w), 
		   XtHeight(w) - 2 * Prim_HighlightThickness(w),
		   Prim_ShadowThickness(w),
		   XmSHADOW_OUT);
		   
    State = AB_Armed(w);
    if (State) {
	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_TopShadowGC(w),
		     Prim_BottomShadowGC(w),
		     myGC,
		     x, y,
		     xsize, ysize,
		     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
		        this
		     Prim_ShadowThickness(w),
		      */
		     1,
		     AB_Direction(w));
    }
    else {
	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_BottomShadowGC(w),
		     Prim_TopShadowGC(w),
		     myGC,
		     x, y,
		     xsize, ysize,
		     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
		        this
		     Prim_ShadowThickness(w),
		      */
		     1,
		     AB_Direction(w));
    }
}

static XtGeometryResult 
query_geometry(Widget w, 
	       XtWidgetGeometry *proposed, 
	       XtWidgetGeometry *answer)
{
    XmArrowButtonWidget aw = (XmArrowButtonWidget)w;
    answer->request_mode = CWWidth | CWHeight;

    answer->width = XtWidth(w);

    answer->height = XtHeight(w);
    
    if (((proposed->request_mode & (CWWidth | CWHeight))
	 == (CWWidth | CWHeight)) &&
	proposed->width == answer->width &&
	proposed->height == answer->height)

	return XtGeometryYes;
    else if (answer->width == XtWidth(aw) &&
	     answer->height == XtHeight(aw))
	return XtGeometryNo;
    else 
	return XtGeometryAlmost;    
}


static void
Arm(Widget w,
    XEvent *event,
    String *params,
    Cardinal *num_params)
{
    XmArrowButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "Arm\n");

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    AB_Armed(w) = True;
    AB_ArmTimeStamp(w) = event->xbutton.time;

    (*exp)(w, event, (Region)NULL);

    if (AB_ArmCallback(w)) {
	cbs.reason = XmCR_ARM;
	cbs.event = event;
	cbs.click_count = AB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   AB_ArmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void 
Activate(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    XmArrowButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "Activate 000\n");

    (*exp)(w, event, NULL);

    XFlush(XtDisplay(w));

    AB_ClickCount(w) = 1;

    if ((ev->x >= 0 && ev->x < XtWidth(w)) &&
	(ev->y >= 0 && ev->y < XtHeight(w))) {

	if (AB_ActivateCallback(w)) {
	    cbs.reason = XmCR_ACTIVATE;
	    cbs.event = event;
	    cbs.click_count = AB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       AB_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }
}

static void 
Disarm(Widget w,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XmArrowButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;
    
    XdbDebug(__FILE__, w, "Disarm\n");

    if (AB_Armed(w)) {
	AB_Armed(w) = False;
	(*exp)(w, event, NULL);
    }

    if (AB_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = AB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   AB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    XmArrowButtonCallbackStruct cbs;
    Widget w = (Widget)data;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    if (AB_Armed(w) == False)
	return;

    AB_Armed(w) = False;

    (*exp)(w, NULL, NULL);

    if (AB_ActivateCallback(w)) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = NULL;
	cbs.click_count = AB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   AB_ActivateCallback(w),
			   (XtPointer)&cbs);
    }

    Disarm(w, NULL, NULL, 0);
}

static void
ArmAndActivate(Widget w,
	       XEvent *event,
	       String *params,
	       Cardinal *num_params)
{
    Arm(w, event, params, num_params);

    AB_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				  ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
}

static void
EnterWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    int margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) + 1;
    int xsize = XtWidth(w) - margin * 2;
    int ysize = XtHeight(w) - margin * 2;

    XdbDebug(__FILE__, w, "EnterWindow\n");

    if (AB_Armed(w)) {
	_XmPrimitiveLeave(w, event, NULL, NULL);

        _XmDrawArrow(XtDisplay(w),
                     XtWindow(w),
                     Prim_TopShadowGC(w),
                     Prim_BottomShadowGC(w),
		     AB_ArrowGC(w),
                     margin, margin,
                     xsize, ysize,
                     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
                        this
                     Prim_ShadowThickness(w),
                      */
                     1,
                     AB_Direction(w));
    }
}

static void
LeaveWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    int margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) + 1;
    int xsize = XtWidth(w) - margin * 2;
    int ysize = XtHeight(w) - margin * 2;

    XdbDebug(__FILE__, w, "LeaveWindow\n");

    if (AB_Armed(w)) {
	_XmPrimitiveLeave(w, event, params, num_params);

	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_BottomShadowGC(w),
		     Prim_TopShadowGC(w),
		     AB_ArrowGC(w),
		     margin, margin,
		     xsize, ysize,
		     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
		        this
		     Prim_ShadowThickness(w),
		      */
		     1,
		     AB_Direction(w));
    }
}

static void
MultiArm(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "MultiArm\n");

    if (AB_MultiClick(w) == XmMULTICLICK_KEEP)
	Arm(w, event, NULL, NULL);
}

static void
MultiActivate(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    XmPushButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "PushB: MultiClick\n");

    if (AB_MultiClick(w) == XmMULTICLICK_KEEP) {
	Time mctime = XtGetMultiClickTime(XtDisplay(w));

	if ((event->xbutton.time - AB_ArmTimeStamp(w)) < mctime)
	    AB_ClickCount(w)++;
	else
	    AB_ClickCount(w) = 1;

	AB_Armed(w) = False;

	(*exp)(w, event, NULL);

	if (ev->type == KeyPress || ev->type == KeyRelease
	    || ((ev->x >= 0 && ev->x < XtWidth(w))
	    && (ev->y >= 0 && ev->y < XtHeight(w))))
	{
	    if (AB_MultiClick(w) == XmMULTICLICK_DISCARD &&
		AB_ClickCount(w) > 1)
		return;

	    if (AB_ActivateCallback(w)) {
		cbs.reason = XmCR_ACTIVATE;
		cbs.event = event;
		cbs.click_count = AB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   AB_ActivateCallback(w),
				   (XtPointer)&cbs);
	    }
	}
	Disarm(w, event, params, num_params);
    }
}

static void
Help(Widget w,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

Widget
XmCreateArrowButton(Widget parent,
	            char *name,
                    Arg *arglist,
                    Cardinal argcount)
{
  return XtCreateWidget(name,
                        xmArrowButtonWidgetClass,
                        parent, arglist, argcount);
}
