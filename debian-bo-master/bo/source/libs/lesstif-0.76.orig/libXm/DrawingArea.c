/**
 *
 * $Id: DrawingArea.c,v 1.9 1996/12/30 07:36:25 u27113 Exp $
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

static char rcsid[] = "$Id: DrawingArea.c,v 1.9 1996/12/30 07:36:25 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/DrawingAP.h>
#include <Xm/TransltnsP.h>
#include <Xm/ScrolledWP.h>
#include <XmI/MacrosI.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static XtGeometryResult GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void changed_managed(Widget w);
static void constraint_initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);

/*
 * Resources for the Drawing Area class
 */
#define Offset(field) XtOffsetOf(XmDrawingAreaRec, drawing_area.field)
static XtResource resources[] = {
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNresizeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(resize_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNexposeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(expose_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNinputCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(input_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNresizePolicy, XmCResizePolicy, XmRResizePolicy,
	sizeof(unsigned char), Offset(resize_policy),
	XmRImmediate, (XtPointer)XmRESIZE_ANY
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

static void PreferedSize(Widget w, Dimension *wid, Dimension *hei, Widget instigator);

void _XmDrawingAreaInput(Widget w, XEvent *event, String *parems, Cardinal *num_params);

char _XmDrawingA_defaultTranslations[] = 
   "<BtnMotion>:          ManagerGadgetButtonMotion()\n\
    <Btn1Down>:           DrawingAreaInput() ManagerGadgetArm() \n\
    <Btn1Down>,<Btn1Up>:  DrawingAreaInput() ManagerGadgetActivate()\n\
    <Btn1Up>:             DrawingAreaInput() ManagerGadgetActivate()\n\
    <Btn1Down>(2+):       DrawingAreaInput() ManagerGadgetMultiArm()\n\
    <Btn1Up>(2+):         DrawingAreaInput() ManagerGadgetMultiActivate()\n\
    <Btn2Down>:           DrawingAreaInput() ManagerGadgetDrag()\n\
    <BtnDown>:            DrawingAreaInput()\n\
    <BtnUp>:              DrawingAreaInput()\n\
    <Key>osfActivate:     DrawingAreaInput() ManagerParentActivate()\n\
    <Key>osfCancel:       DrawingAreaInput() ManagerParentCancel()\n\
    <Key>osfHelp:         DrawingAreaInput() ManagerGadgetHelp()\n\
    <Key>osfSelect:       DrawingAreaInput() ManagerGadgetSelect()\n\
    ~s ~m ~a <Key>Return: DrawingAreaInput() ManagerParentActivate()\n\
    <Key>Return:          DrawingAreaInput() ManagerGadgetSelect()\n\
    <Key>space:           DrawingAreaInput() ManagerGadgetSelect()\n\
    <KeyDown>:            DrawingAreaInput() ManagerGadgetKeyInput()\n\
    <KeyUp>:              DrawingAreaInput()";

char _XmDrawingA_traversalTranslations[] =
   "<EnterWindow>:        ManagerEnter()\n\
    <LeaveWindow>:        ManagerLeave()\n\
    <FocusOut>:           ManagerFocusOut()\n\
    <FocusIn>:            ManagerFocusIn()\n\
    <Key>osfUp:           DrawingAreaInput() ManagerGadgetTraverseUp()\n\
    <Key>osfDown:         DrawingAreaInput() ManagerGadgetTraverseDown()\n\
    <Key>osfLeft:         DrawingAreaInput() ManagerGadgetTraverseLeft()\n\
    <Key>osfRight:        DrawingAreaInput() ManagerGadgetTraverseRight()\n\
    <Key>osfBeginLine:    DrawingAreaInput() ManagerGadgetTraverseHome()\n\
    s <Key>Tab:           DrawingAreaInput() ManagerGadgetPrevTabGroup()\n\
    ~s <Key>Tab:          DrawingAreaInput() ManagerGadgetNextTabGroup()";

static XtActionsRec actions[] = {
    {"DrawingAreaInput", _XmDrawingAreaInput},
};

static XmBaseClassExtRec _XmDrawingACoreClassExtRec = {
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
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static CompositeClassExtensionRec daCompositeExt = 
{
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmManagerClassExtRec _XmDrawingAMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmDrawingAreaClassRec xmDrawingAreaClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmDrawingArea",
	/* widget_size           */ sizeof(XmDrawingAreaRec),
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
	/* compress_exposure     */ XtExposeCompressMultiple,
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
	/* tm_table              */ _XmDrawingA_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmDrawingACoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ GeometryManager, 
        /* change_managed   */ changed_managed, 
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ (XtPointer)&daCompositeExt
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,  
        /* subresource_count */ 0,     
        /* constraint_size   */ 0,     
        /* initialize        */ constraint_initialize,
        /* destroy           */ NULL,  
        /* set_values        */ NULL,  
        /* extension         */ NULL,  
    },
    /* XmManager class part */
    {
	/* translations                 */ _XmDrawingA_traversalTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmDrawingAMClassExtRec
    },
    /* XmDrawing Area part */
    {
	/* extension */ NULL,
    },
};

WidgetClass xmDrawingAreaWidgetClass = (WidgetClass)&xmDrawingAreaClassRec;

static void 
class_initialize()
{
    _XmDrawingACoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmDrawingAreaWidgetClass daclass = (XmDrawingAreaWidgetClass)widget_class;
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension*)_XmGetClassExtensionPtr(
		(XmGenericClassExt*)&(daclass->composite_class.extension),
		NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = daclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    daclass->composite_class.extension = (XtPointer) ext;
	}
    }    
    _XmFastSubclassInit(widget_class, XmDRAWING_AREA_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    if (XtWidth(request) == 0)
	XtWidth(new_w) = 2 * DA_MarginWidth(new_w);

    if (XtHeight(request) == 0)
	XtHeight(new_w) = 2 * DA_MarginHeight(new_w);
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
    Boolean need_refresh = False;

    XdbDebug(__FILE__, new_w, "setvalues()\n");

#define NE(x)	(x(old) != x(new_w))

    if (NE(DA_MarginHeight) ||
	NE(DA_MarginWidth) ||
	NE(DA_ResizePolicy)) {
	need_refresh = True;
    }

    return need_refresh;
}

static void 
expose(Widget w, 
       XEvent *event, 
       Region region)
{
    XmDrawingAreaCallbackStruct cb;

    cb.reason = XmCR_EXPOSE;
    cb.event = event;
    cb.window = XtWindow(w);
    
    XtCallCallbackList(w,
		       DA_ExposeCallback(w),
		       (XtPointer)&cb);
}

static void
resize(Widget w)
{
    XmDrawingAreaCallbackStruct cb;

    cb.reason = XmCR_RESIZE;
    cb.event = NULL;
    cb.window = XtWindow(w);

    XtCallCallbackList(w,
		       DA_ResizeCallback(w),
		       (XtPointer)&cb);
}

static void 
constraint_initialize(Widget request, 
		      Widget new_w, 
		      ArgList args, 
		      Cardinal *num_args)
{
    /* we keep the children from being within the margin,
       by changing their X/Y to the margin width/height
       if they are */

    Widget da = XtParent(new_w);

    if (XtX(new_w) < (Position)DA_MarginWidth(da))
	XtX(new_w) = DA_MarginWidth(da);

    if (XtY(new_w) < (Position)DA_MarginHeight(da))
	XtY(new_w) = DA_MarginHeight(da);
}

static void
PreferedSize(Widget w, 
	     Dimension *wid,
	     Dimension *hei,
	     Widget instigator)
{
    int i;
    XmDrawingAreaWidget dw = (XmDrawingAreaWidget)w;

    *wid = 0;
    *hei = 0;

    for (i=0; i < dw->composite.num_children; i++)
    {
	Widget child = dw->composite.children[i];
	XtWidgetGeometry child_geometry;

	if (!XtIsManaged(child))
	    continue;

	/* we assume that the routines calling this one fill in the
	   width and height of the child responsible for the resize
	   request with their prefered size. */
	if (instigator == child)
	{
	    child_geometry.width = XtWidth(child);
	    child_geometry.height = XtHeight(child);
	}
	else
	    XtQueryGeometry(child, NULL, &child_geometry);

	XdbDebug2(__FILE__, w, child,
		  "Child wants %d %d\n",
		  child_geometry.width, child_geometry.height);

	if (XtX(child) + child_geometry.width > *wid)
	    *wid = XtX(child) + child_geometry.width;

	if (XtY(child) + child_geometry.height > *hei)
	    *hei = XtY(child) + child_geometry.height;
    }

    /* we assume that the X/Y coordinates of all the children have the
       drawing area's margin width/height added to them.  But now we
       increment the total width and height of the drawing area to
       include the margins on the other side. */

    *wid += DA_MarginWidth(w);
    *hei += DA_MarginHeight(w);
}

static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    Dimension wid, hei;

#define	Wants(x)	((proposed->request_mode & x) == x)

    XdbDebug(__FILE__, w, "QueryGeometry()\n");

    if (proposed != answer)
	    *answer = *proposed;

    answer->request_mode = 0;

    PreferedSize(w, &wid, &hei, NULL);

    if (Wants(CWWidth)) {
	answer->request_mode |= CWWidth;
	answer->width = wid;
    }
    if (Wants(CWHeight)) {
	answer->request_mode |= CWHeight;
	answer->height = hei;
    }
    if (Wants(CWHeight) && (XtHeight(w) != proposed->height))
	return XtGeometryNo;
    if (Wants(CWWidth) && (XtWidth(w) != proposed->width))
	return XtGeometryNo;
    return XtGeometryYes;
}

static XtGeometryResult
GeometryManager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    Widget dw = XtParent(w), sw = XtParent(dw);
    XtGeometryResult result;
    Dimension reply_height = 0, reply_width = 0;
    Dimension wid = 0, hei = 0;
    Dimension child_w = 0, child_h = 0;
    XtWidgetGeometry req;

    XdbDebug2(__FILE__, dw, w, "GeometryManager(%s)\n",
	(DA_ResizePolicy(dw) == XmRESIZE_NONE) ? "XmRESIZE_NONE" :
	(DA_ResizePolicy(dw) == XmRESIZE_GROW) ? "XmRESIZE_GROW" :
	(DA_ResizePolicy(dw) == XmRESIZE_ANY) ? "XmRESIZE_ANY" : "???" );

    if (XtIsSubclass(sw, xmScrolledWindowWidgetClass)
		&& SW_ScrollPolicy(sw) == XmAUTOMATIC) {
	XdbDebug2(__FILE__, dw, w,
		"GeometryManager: in ScrolledWindow (%s) => YES\n",
		XdbWidgetGeometry2String(request));
	*reply = *request;
	reply->request_mode &= (CWWidth|CWHeight);

	if ((reply->request_mode & CWWidth) == 0)
	    reply->width = XtWidth(w);
	if ((reply->request_mode & CWHeight) == 0)
	    reply->height = XtHeight(w);
	if ((reply->request_mode & CWBorderWidth) == 0)
	    reply->border_width = XtBorderWidth(w);
#if 1
	/* Resize the widget as requested */
/* FIX ME : ScrolledWindow should do this for us */
	XtResizeWidget(w, reply->width, reply->height, reply->border_width);
#endif
	/* Trigger the ScrolledWindow's geometry algorithms so it can
	 * handle scrollbars and such more.
	 *
	 * Not using _XmMakeGeometryRequest here since this is a special case.
	 */
	XdbDebug(__FILE__, dw, "XtMakeResizeRequest (phony) %d %d\n",
		reply->width, reply->height);
	XtMakeResizeRequest(dw, reply->width, reply->height, NULL, NULL);

	return XtGeometryYes;
    }

    if (request->request_mode & CWWidth)
    {
	child_w = XtWidth(w);
	XtWidth(w) = request->width;
    }

    if (request->request_mode & CWHeight)
    {
	child_h = XtHeight(w);
	XtHeight(w) = request->height;
    }

    PreferedSize(dw, &wid, &hei, w);

    XtWidth(w) = child_w;
    XtHeight(w) = child_h;

    switch (DA_ResizePolicy(dw))
    {
    case XmRESIZE_NONE:
	if ((wid > XtWidth(dw)) || (hei > XtHeight(dw)))
	    return XtGeometryNo;
	else
	    /* the resize request doesn't require us to grow, accept it */
	    return XtGeometryYes;
	break;
    case XmRESIZE_GROW:
	if ((wid > XtWidth(dw)) || (hei > XtHeight(dw)))
	{
	    /* attempt to resize */
	    req.request_mode = (CWWidth|CWHeight);
	    req.width = wid;
	    req.height = hei;
	    result = _XmMakeGeometryRequest(w, &req);
	    reply_width = req.width;
	    reply_height = req.height;

	    if (result == XtGeometryYes)
	    {
		XtWidth(dw) = reply_width;
		XtHeight(dw) = reply_height;
		
		return XtGeometryYes;
	    }
	    else if (result == XtGeometryNo)
		return XtGeometryNo;
	}
	else
	    /* the resize request doesn't require us to grow, accept it */
	    return XtGeometryYes;
	break;
    case XmRESIZE_ANY:
	if ((wid != XtWidth(dw)) || (hei != XtHeight(dw))) {

	    req.request_mode = (CWWidth|CWHeight);
	    req.width = wid;
	    req.height = hei;
	    result = _XmMakeGeometryRequest(w, &req);
	    reply_width = req.width;
	    reply_height = req.height;

	    if (result == XtGeometryYes) {
		XtWidth(dw) = reply_width;
		XtHeight(dw) = reply_height;
		
		return XtGeometryYes;
	    } else if (result == XtGeometryNo) {
		return XtGeometryNo;
	    }
	} else
	    /* the resize request doesn't require us to alter our geometry, accept it */
	    return XtGeometryYes;
	break;
    }
    return XtGeometryNo;
}

static void
changed_managed(Widget w)
{
    Dimension wid, hei;
    Dimension reply_wid, reply_hei;
    XtGeometryResult result;
    XtWidgetGeometry req;

    if (DA_ResizePolicy(w) != XmRESIZE_NONE)
    {
	PreferedSize(w, &wid, &hei, NULL);
	if (wid > XtWidth(w) || hei > XtHeight(w))
	{

	    req.request_mode = (CWWidth|CWHeight);
	    req.width = wid;
	    req.height = hei;
	    result = _XmMakeGeometryRequest(w, &req);
	    reply_wid = req.width;
	    reply_hei = req.height;

	    if (result == XtGeometryYes)
	    {
		XtWidth(w) = reply_wid;
		XtHeight(w) = reply_hei;
	    }
	}
	/* else we're not getting any bigger... */
	else if (DA_ResizePolicy(w) == XmRESIZE_ANY)
	{
	    req.request_mode = (CWWidth|CWHeight);
	    req.width = wid;
	    req.height = hei;
	    result = _XmMakeGeometryRequest(w, &req);
	    reply_wid = req.width;
	    reply_hei = req.height;

	    if (result == XtGeometryYes)
	    {
		XtWidth(w) = reply_wid;
		XtHeight(w) = reply_hei;
	    }	    
	}
    }
}

void 
_XmDrawingAreaInput(Widget w,
		    XEvent *event,
		    String *parems,
		    Cardinal *num_params)
{
    XmDrawingAreaCallbackStruct cb;

    cb.reason = XmCR_INPUT;
    cb.event = event;
    cb.window = XtWindow(w);

    XdbDebug(__FILE__, w, "_XmDrawingAreaInput\n");

    XtCallCallbackList(w,
		       DA_InputCallback(w),
		       (XtPointer)&cb);
}

Widget
XmCreateDrawingArea(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmDrawingAreaWidgetClass,
			  parent,
			  arglist,
			  argcount);
}
