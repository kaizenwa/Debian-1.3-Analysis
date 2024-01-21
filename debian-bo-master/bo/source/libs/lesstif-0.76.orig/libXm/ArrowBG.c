/**
 *
 * $Id: ArrowBG.c,v 1.9 1997/01/11 02:19:40 miers Exp $
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

static char rcsid[] = "$Id: ArrowBG.c,v 1.9 1997/01/11 02:19:40 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ArrowBGP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */


static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget w_new, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);

static void Arm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Activate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);

#define Offset(field) XtOffsetOf(XmArrowButtonGadgetRec, arrowbutton.field)

/* Resources for the arrowbuttongadget class */
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

static XmBaseClassExtRec _XmArrowBGRectClassExtRec = {
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

static XmGadgetClassExtRec _XmArrowBGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ NULL, /* FIXME */
    /* display_rect_proc         */ NULL, /* FIXME */
};

XmArrowButtonGadgetClassRec xmArrowButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmGadgetClassRec,
	/* class_name            */ "XmArrowButtonGadget",
	/* widget_size           */ sizeof(XmArrowButtonGadgetRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ FALSE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ FALSE,
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
	/* tm_table              */ NULL,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmArrowBGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, 
	/* border_unhighlight */ XmInheritBorderUnhighlight,
	/* arm_and_activate   */ ArmAndActivate,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL, /* fix me */
	/* syn_resources      */ NULL,
	/* num_syn_resources  */ 0,
	/* cache_part         */ NULL,
	/* extension          */ (XtPointer)&_XmArrowBGadgetClassExtRec
    },
    /* XmArrowButtonGadget part */
    {
	/* extension */ NULL
    },
};

WidgetClass xmArrowButtonGadgetClass = (WidgetClass)&xmArrowButtonGadgetClassRec;

#define ABG_DEFAULT_HEIGHT	20
#define ABG_DEFAULT_WIDTH	20

static void
class_initialize()
{
    _XmArrowBGRectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmARROW_BUTTON_GADGET_BIT);
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
    values.foreground = XmParentForeground(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillSolid;

    ABG_ArrowGC(w) = XtGetGC(w, mask, &values);
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
    values.foreground = XmParentForeground(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple = XmGetPixmapByDepth(XtScreen(w),
					XmEVEN_STIPPLE_IMAGE,
					WhitePixelOfScreen(XtScreen(w)),
					BlackPixelOfScreen(XtScreen(w)),
					1);

    ABG_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    if (! XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
	_XmError(new_w, "parent should be manager.");

    if (XtWidth(new_w) == 0)
	XtWidth(new_w) = ABG_DEFAULT_WIDTH;
    
    if (XtHeight(new_w) == 0)
	XtHeight(new_w) = ABG_DEFAULT_HEIGHT;

    /* Gadget override */
    G_HighlightOnEnter(new_w) = True;

    ABG_Armed(new_w) = False;

    CreateArrowGC(new_w);
    CreateInsensitiveGC(new_w);

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT | XmENTER_EVENT |
			     XmLEAVE_EVENT | XmFOCUS_IN_EVENT |
			     XmFOCUS_OUT_EVENT | XmMULTI_ARM_EVENT |
			     XmMULTI_ACTIVATE_EVENT | XmHELP_EVENT;
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, ABG_ArrowGC(w));
    XtReleaseGC(w, ABG_InsensitiveGC(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{

    XmArrowButtonGadget abg = (XmArrowButtonGadget) new_w;
    XmArrowButtonGadget og = (XmArrowButtonGadget) old;
    Boolean refresh_needed = False;

    if (XmParentBackground(new_w) != XmParentBackground(old)) {
	XtReleaseGC(new_w, ABG_ArrowGC(new_w));
	XtReleaseGC(new_w, ABG_InsensitiveGC(new_w));
	CreateArrowGC(new_w);
	CreateInsensitiveGC(new_w);
	refresh_needed = True;
    }
    if (ABG_Direction(abg) != ABG_Direction(og)) 
	refresh_needed = True;

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    Dimension x, y, xsize, ysize;
    Dimension margin=G_ShadowThickness(w) + G_HighlightThickness(w) + 1;
    Boolean State;
    GC myGC;

    x = XtX(w) + margin;
    y = XtY(w) + margin;

    xsize = XtWidth(w) - margin * 2;
    ysize = XtHeight(w) - margin * 2;

    /* use the right GC */
    if (XtIsSensitive(w))
	myGC = ABG_ArrowGC(w);
    else
	myGC = ABG_InsensitiveGC(w);

    _XmDrawShadows(XtDisplayOfObject(w), 
		   XtWindowOfObject(w), 
		   XmParentTopShadowGC(w),
		   XmParentBottomShadowGC(w),
 		   XtX(w) + G_HighlightThickness(w),
		   XtY(w) + G_HighlightThickness(w),
		   XtWidth(w) - 2 * G_HighlightThickness(w),
		   XtHeight(w) - 2 * G_HighlightThickness(w),
		   G_ShadowThickness(w),
		   XmSHADOW_OUT);
		   
    State = ABG_Armed(w);
    if (State) {
	_XmDrawArrow(XtDisplayOfObject(w),
		     XtWindowOfObject(w),
		     XmParentTopShadowGC(w),
		     XmParentBottomShadowGC(w),
		     myGC,
		     x, y,
		     xsize, ysize,
		     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
			this
		     G_ShadowThickness(w),
		      */
		     1,
		     ABG_Direction(w));
    }
    else {
	_XmDrawArrow(XtDisplayOfObject(w),
		     XtWindowOfObject(w),
		     XmParentBottomShadowGC(w),
		     XmParentTopShadowGC(w),
		     myGC,
		     x, y,
		     xsize, ysize,
		     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
			this
		     G_ShadowThickness(w),
		      */
		     1,
		     ABG_Direction(w));
    }
}

static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    answer->request_mode = CWWidth | CWHeight;

    answer->width = XtWidth(w);

    answer->height = XtHeight(w);
    
    if (((proposed->request_mode & (CWWidth | CWHeight))
	 == (CWWidth | CWHeight)) &&
	proposed->width == answer->width &&
	proposed->height == answer->height)

	return XtGeometryYes;
    else if (answer->width == XtWidth(w) &&
	     answer->height == XtHeight(w))
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

    ABG_Armed(w) = True;

    (*exp)(w, NULL, (Region)NULL);

    if (ABG_ArmCallback(w)) {
	cbs.reason = XmCR_ARM;
	cbs.event = event;
	cbs.click_count = ABG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   ABG_ArmCallback(w),
			   (XtPointer)&cbs);
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

    if (ABG_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = ABG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   ABG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
    ABG_Armed(w) = False;
    (*exp)(w, NULL, (Region)NULL);
}

static void 
Activate(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    XmArrowButtonCallbackStruct cbs;

    if ((ev->x > XtX(w) && ev->x < XtX(w) + XtWidth(w)) &&
	(ev->y > XtY(w) && ev->y < XtY(w) + XtHeight(w))) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = ABG_ClickCount(w);

	if (ABG_ActivateCallback(w)) {

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       ABG_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }

    Disarm(w,event,params,num_params);
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    XmArrowButtonCallbackStruct cbs;
    Widget w = (Widget)data;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    if (ABG_Armed(w) == False)
	return;

    ABG_Armed(w) = False;

    (*exp)(w, NULL, NULL);

    if (ABG_ActivateCallback(w)) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = NULL;
	cbs.click_count = ABG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   ABG_ActivateCallback(w),
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

    ABG_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				   ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
}

static void
EnterWindow(Widget w,
      XEvent *event,
      String *params,
      Cardinal *num_params)
{
    int margin = G_ShadowThickness(w) + G_HighlightThickness(w) + 1;
    int xsize = XtWidth(w) - margin * 2;
    int ysize = XtHeight(w) - margin * 2;

    if (ABG_Armed(w)) 
        _XmDrawArrow(XtDisplayOfObject(w),
                     XtWindowOfObject(w),
                     XmParentTopShadowGC(w),
                     XmParentBottomShadowGC(w),
		     ABG_ArrowGC(w),
                     XtX(w) + margin, XtY(w) + margin,
                     xsize, ysize,
                     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
                        this
                     Prim_ShadowThickness(w),
                      */
                     1,
                     ABG_Direction(w));
}

static void
LeaveWindow(Widget w,
      XEvent *event,
      String *params,
      Cardinal *num_params)
{
    int margin = G_ShadowThickness(w) + G_HighlightThickness(w) + 1;
    int xsize = XtWidth(w) - margin * 2;
    int ysize = XtHeight(w) - margin * 2;

    if (ABG_Armed(w)) 
        _XmDrawArrow(XtDisplayOfObject(w),
                     XtWindowOfObject(w),
                     XmParentBottomShadowGC(w),
                     XmParentTopShadowGC(w),
		     ABG_ArrowGC(w),
                     XtX(w) + margin, XtY(w) + margin,
                     xsize, ysize,
                     /* Per tegla@katalin.csoma.elte.hu, M*tif ignores
                        this
                     Prim_ShadowThickness(w),
                      */
                     1,
                     ABG_Direction(w));
}

static void
Help(Widget w,
     XEvent *event,
     String *params,
     Cardinal *num_params)
{
    Widget cur = w;
    XmAnyCallbackStruct cbs;

    cbs.reason = XmCR_HELP;
    cbs.event = event;

    while (cur != NULL) {
	if (XtHasCallbacks(w, XmNhelpCallback) == XtCallbackHasSome) {
	    XtCallCallbacks(w, XmNhelpCallback, (XtPointer)&cbs);
	    return;
	}
	cur = XtParent(cur);
    }
}

static void
input_dispatch(Widget gadget, 
	       XEvent *event, 
	       Mask event_mask) 
{
    Cardinal num_params = 0;

    switch (event_mask)
    {
    case XmARM_EVENT:
	XdbDebug(__FILE__, gadget, "ArrowButtonGadget got arm event\n");
	Arm(gadget, event, NULL, &num_params);
	break;

    case XmACTIVATE_EVENT:
	XdbDebug(__FILE__, gadget, "ArrowButtonGadget got activate event\n");
	ABG_ClickCount(gadget) = 1;
	Activate(gadget, event, NULL, &num_params);
	break;

    case XmENTER_EVENT:
	XdbDebug(__FILE__, gadget, "ArrowButtonGadget got enter event\n");
	EnterWindow(gadget, event, NULL, &num_params);
	break;

    case XmLEAVE_EVENT:
	XdbDebug(__FILE__, gadget, "ArrowButtonGadget got leave event\n");
	LeaveWindow(gadget, event, NULL, &num_params);
	break;

    case XmFOCUS_IN_EVENT:
	_XmFocusInGadget(gadget, event, NULL, &num_params);
	break;

    case XmFOCUS_OUT_EVENT:
	_XmFocusOutGadget(gadget, event, NULL, &num_params);
	break;

    case XmHELP_EVENT:
	Help(gadget, event, NULL, &num_params);
	break;

    case XmMULTI_ARM_EVENT:
	if (ABG_MultiClick(gadget) == XmMULTICLICK_KEEP)
	    Arm(gadget, event, NULL, &num_params);
	break;

    case XmMULTI_ACTIVATE_EVENT:
	if (ABG_MultiClick(gadget) == XmMULTICLICK_KEEP) {
	    ABG_ClickCount(gadget)++;
	    Activate(gadget, event, NULL, &num_params);
	}
	break;

    default:
	_XmError(gadget, "Unexpected event in ArrowButton gadget\n");
	break;
    }
}

Widget
XmCreateArrowButtonGadget(Widget parent,
			  char *name,
			  Arg *arglist,
			  Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmArrowButtonGadgetClass,
			  parent,
			  arglist,
			  argcount);
}
