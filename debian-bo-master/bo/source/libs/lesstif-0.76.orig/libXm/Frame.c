/** 
 *
 * $Id: Frame.c,v 1.13 1997/01/13 07:08:52 u27113 Exp $
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
 
static char rcsid[] = "$Id: Frame.c,v 1.13 1997/01/13 07:08:52 u27113 Exp $";

#define OPTIMISTIC_COMPUTE_SIZE
#undef	STRICT_GEOMETRY_RULES

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/FrameP.h>
#include <Xm/TransltnsP.h>
#include <Xm/RepType.h>

#include <XmI/DebugUtil.h>

#define _XmMax(a,b) ((a) > (b) ? (a) : (b))
#define _XmMin(a,b) ((a) < (b) ? (a) : (b))

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

/*
 * Forward Declarations
 */
/* core */
static void class_initialize(void);
static void class_part_initialize(WidgetClass widget_class);
static void initialize(Widget request,
		       Widget new_w,
		       ArgList args,
		       Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w,
		    XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void resize(Widget w);
static Boolean set_values(Widget current,
			  Widget request,
			  Widget new_w,
			  ArgList args,
			  Cardinal *num_args);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *request,
				       XtWidgetGeometry *reply);
/* composite */
static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void change_managed(Widget w);
/* constraint */
static void constraint_initialize(Widget request,
				  Widget new_w,
				  ArgList args,
				  Cardinal *num_args);
static Boolean constraint_set_values(Widget current, 
				     Widget request,
				     Widget new_w,
				     ArgList args,
				     Cardinal *num_args);
/* helper */
static void _XmFrameComputeSize(Widget w, Widget instig,
				XtWidgetGeometry *desired,
				Dimension *wd, Dimension *ht);
static void _XmFrameConfigureChildren(Widget w, Widget instig,
				      XtWidgetGeometry *desired,
				      Dimension wd, Dimension ht);

/* actions */
static void Enter(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void GetFocus(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Arm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Activate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ManagerGadgetDrag(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ManagerParentActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ManagerParentCancel(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmFrame_defaultTranslations[] = 
   "<EnterWindow>:        Enter()\n\
    <FocusIn>:            FocusIn()\n\
    <Btn1Down>:           Arm()\n\
    <Btn1Up>:             Activate()\n\
    <Btn2Down>:           ManagerGadgetDrag()\n\
    <Key>osfActivate:     ManagerParentActivate()\n\
    <Key>osfCancel:       ManagerParentCancel()\n\
    ~s ~m ~a <Key>Return: ManagerParentActivate()";

static XtActionsRec actions[] = {
    {"Enter",			Enter},
    {"FocusIn",			GetFocus},
    {"Arm",			Arm},
    {"Activate",		Activate},
    {"ManagerGadgetDrag",	ManagerGadgetDrag},
    {"ManagerParentActivate",	ManagerParentActivate},
    {"ManagerParentCancel",	ManagerParentCancel}
};

/*
 * resources for the frame class
 */
#define Offset(field) XtOffsetOf(XmFrameRec, frame.field)
static XtResource resources[] = {
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNshadowType, XmCShadowType, XmRShadowType,
	sizeof(unsigned char), Offset(shadow_type),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    /* manager override */
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmFrameRec, manager.shadow_thickness),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
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

/*
 * Constraint Resources for frame's children
 */
#undef Offset
#define Offset(field) XtOffsetOf(XmFrameConstraintRec, frame.field)

static XtResource frameConstraintResources[] = {
    {
	XmNchildType, XmCChildType, XmRChildType,
	sizeof(unsigned char), Offset(child_type),
	XtRImmediate, (XtPointer)XmFRAME_WORKAREA_CHILD
    },
    {
	XmNchildHorizontalAlignment, XmCChildHorizontalAlignment, XmRChildHorizontalAlignment,
	sizeof(unsigned char), Offset(child_h_alignment),
	XtRImmediate, (XtPointer)XmALIGNMENT_BEGINNING
    },
    {
	XmNchildVerticalAlignment, XmCChildVerticalAlignment, XmRChildVerticalAlignment,
	sizeof(unsigned char), Offset(child_v_alignment),
	XtRImmediate, (XtPointer)XmALIGNMENT_CENTER
    },
    {
	XmNchildHorizontalSpacing, XmCChildHorizontalSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(child_h_spacing),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    }
};

static XmSyntheticResource constraint_syn_resources[] = {
    {
	XmNchildHorizontalSpacing,
	sizeof(Dimension), Offset(child_h_spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

static XmBaseClassExtRec _XmFrameCoreClassExtRec = {
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

#undef Offset
static CompositeClassExtensionRec frameCompositeExt = {
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmManagerClassExtRec _XmFrameMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmFrameClassRec xmFrameClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmFrame",
	/* widget_size           */ sizeof(XmFrameRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
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
	/* tm_table              */ _XmFrame_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmFrameCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ (XtPointer)&frameCompositeExt,
    },
    /* Constraint class part */
    {
	/* subresources      */ frameConstraintResources,
        /* subresource_count */ XtNumber(frameConstraintResources), 
        /* constraint_size   */ sizeof(XmFrameConstraintRec),
        /* initialize        */ constraint_initialize,
        /* destroy           */ NULL,
        /* set_values        */ constraint_set_values,
        /* extension         */ NULL,  
    },
    /* XmManager class part */
    {
	/* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ constraint_syn_resources,
        /* num_syn_constraint_resources */ XtNumber(constraint_syn_resources),
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmFrameMClassExtRec
    },
    /* XmFrame Area part */
    {
	/* extension */ NULL,
    },
};

WidgetClass xmFrameWidgetClass = (WidgetClass)&xmFrameClassRec;

/* core methods */

static void 
class_initialize()
{
    _XmFrameCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    XmFrameWidgetClass fclass = (XmFrameWidgetClass)widget_class;

    extptr = (CompositeClassExtension*)_XmGetClassExtensionPtr((XmGenericClassExt*)&(fclass->composite_class.extension),
							       NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = fclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    fclass->composite_class.extension = (XtPointer) ext;
	}
    }
    _XmFastSubclassInit(widget_class, XmFRAME_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Frame_TitleArea(new_w) = NULL;
    Frame_WorkArea(new_w) = NULL;

    if (Frame_ShadowType(new_w) == (unsigned char)XmUNSPECIFIED)
    {
	if (XtIsShell(XtParent(new_w)))
	    Frame_ShadowType(new_w) = XmSHADOW_OUT;
	else
	    Frame_ShadowType(new_w) = XmSHADOW_ETCHED_IN;
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRShadowType),
			     Frame_ShadowType(new_w),
			     new_w))
	Frame_ShadowType(new_w) = XmSHADOW_ETCHED_IN;

    if (MGR_ShadowThickness(new_w) == XmINVALID_DIMENSION)
    {
	if (XtIsShell(XtParent(new_w)))
	    MGR_ShadowThickness(new_w) = 1;
	else
	    MGR_ShadowThickness(new_w) = 2;
    }

    Frame_OldShadowX(new_w) = Frame_OldShadowY(new_w) = 0;
    Frame_OldShadowThickness(new_w) = MGR_ShadowThickness(new_w);
    Frame_OldWidth(new_w) = XtWidth(new_w);
    Frame_OldHeight(new_w) = XtHeight(new_w);
    Frame_ProcessingConstraints(new_w) = False;
}

static void
destroy(Widget w)
{
}

static Boolean
set_values(Widget current,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean redisplay = False;
    Dimension wd, ht;
    XtWidgetGeometry req;

    if (Frame_MarginWidth(new_w) != Frame_MarginWidth(current) ||
	Frame_MarginHeight(new_w) != Frame_MarginHeight(current) ||
	Frame_ShadowType(new_w) != Frame_ShadowType(current))
	redisplay = True;

    if (redisplay)
    {
	_XmFrameComputeSize(new_w, NULL, NULL, &wd, &ht);
	if (wd > Frame_OldWidth(new_w) || ht > Frame_OldHeight(new_w))
	{
	    req.request_mode = (CWWidth|CWHeight);
	    req.width = wd;
	    req.height = ht;
	    _XmMakeGeometryRequest(new_w, &req);
	    wd = req.width;
	    ht = req.height;

	    _XmFrameConfigureChildren(new_w, NULL, NULL, wd, ht);
	}
    }

    Frame_OldShadowX(new_w) = 0;
    if (Frame_TitleArea(new_w) && XtIsManaged(Frame_TitleArea(new_w)))
	Frame_OldShadowY(new_w) = XtHeight(Frame_TitleArea(new_w)) +
				MGR_ShadowThickness(new_w) +
				Frame_MarginHeight(new_w);
    else
	Frame_OldShadowY(new_w) = MGR_ShadowThickness(new_w) +
				Frame_MarginHeight(new_w);

    Frame_OldShadowThickness(new_w) = MGR_ShadowThickness(current);
    Frame_OldWidth(new_w) = XtWidth(new_w);
    Frame_OldHeight(new_w) = XtHeight(new_w);

    return redisplay;
}

static void
realize(Widget w, 
	XtValueMask *value_mask, 
	XSetWindowAttributes *attributes)
{
#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

    _XmFrameConfigureChildren(w, NULL, NULL, XtWidth(w), XtHeight(w));

    XdbDebug(__FILE__, w, "Frame Realize\n");
}

static void
resize(Widget w)
{
    XdbDebug(__FILE__, w, "Frame resize (%d %d)\n", XtWidth(w), XtHeight(w));

    _XmFrameConfigureChildren(w, NULL, NULL, XtWidth(w), XtHeight(w));
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    Position topline_y = 0;

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	switch (FrameC_ChildVAlignment(Frame_TitleArea(w)))
	{
	case XmALIGNMENT_CENTER:
	    topline_y = XtHeight(Frame_TitleArea(w)) / 2;
	    break;
	case XmALIGNMENT_BASELINE_TOP:
	    /* change this to work with XmWidgetBaseline */
	    topline_y = XtHeight(w);
	    break;
	case XmALIGNMENT_WIDGET_TOP:
	    topline_y = XtHeight(Frame_TitleArea(w));
	    break;
	case XmALIGNMENT_WIDGET_BOTTOM:
	    topline_y = 0;
	    break;
	}
    }

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   MGR_TopShadowGC(w),
		   MGR_BottomShadowGC(w),
		   0, 
		   topline_y, 
		   XtWidth(w),
		   XtHeight(w) - topline_y,
		   MGR_ShadowThickness(w),
		   Frame_ShadowType(w));

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	XFillRectangle(XtDisplay(w), XtWindow(w), MGR_BackgroundGC(w),
		       XtX(Frame_TitleArea(w)), XtY(Frame_TitleArea(w)),
		       XtWidth(Frame_TitleArea(w)),
		       XtHeight(Frame_TitleArea(w)));
    }

    /* redisplay our gadget children -- if any */
    _XmRedisplayGadgets(w, event, region);
}

static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
    Dimension		wd, ht;
    XtGeometryResult	r;
    XtWidgetGeometry	rr = *request;

    _XmFrameComputeSize(w, w, NULL, &wd, &ht);

    reply->width = wd;
    reply->height = ht;
    reply->request_mode = CWWidth|CWHeight;

    if ((request->request_mode & (CWWidth|CWHeight)) == (CWWidth|CWHeight) &&
	reply->width == request->width && reply->height == request->height) {
	r =  XtGeometryYes;
    } else if (((request->request_mode & CWWidth) && (request->width == XtWidth(w))) &&
	     ((request->request_mode & CWHeight) && (request->height == XtHeight(w)))) {
	r =  XtGeometryNo;
    } else {
	r =  XtGeometryAlmost;
    }

    XdbDebug(__FILE__, w, "QueryGeometry [%s] => %s [%s]\n",
	XdbWidgetGeometry2String(&rr), XdbGeometryResult2String(r),
	XdbWidgetGeometry2String(reply));

    return r;
}

/*
 * This GeometryManager is a weirdo compared to many others in LessTif.
 *
 * Frame can have two children - a title and a work area.
 * Geometry Manager replies differently depending on which one asks a question.
 *
 * Implementation before October 1996 didn't do that :-(
 */
static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    Dimension wd, ht;
    Widget	fw = XtParent(w);

    XdbDebug2(__FILE__, fw, w, "Frame GeometryManager [%s]\n",
	XdbWidgetGeometry2String(request));

    if (FrameC_ChildType(w) != XmFRAME_WORKAREA_CHILD &&
	FrameC_ChildType(w) != XmFRAME_TITLE_CHILD) {
	XdbDebug(__FILE__, w, "Frame GeometryManager => No\n");
	return XtGeometryNo;
    }
#ifdef	STRICT_GEOMETRY_RULES
    if (request->request_mode & (CWX|CWY)) {
	XdbDebug2(__FILE__, fw, w, "Frame GeometryManager (asked X or Y) => No\n");
	return XtGeometryNo;
    }
#endif
    if (request->request_mode & XtCWQueryOnly) {
	abort();
    }

/* Calculate desired Frame geometry in wd/ht from requested child geometry in "request" */
    _XmFrameComputeSize(fw, w, request, &wd, &ht);

    if (wd != XtWidth(fw) || ht != XtHeight(fw)) {
	XtWidgetGeometry	geo;

	geo.width = wd;
	geo.height = ht;
	geo.request_mode = CWWidth | CWHeight;

	if (_XmMakeGeometryRequest(fw, &geo) == XtGeometryYes) {
	    XtWidth(fw) = geo.width;
	    XtHeight(fw) = geo.height;
	}

	_XmFrameConfigureChildren(fw, w, reply, XtWidth(fw), XtHeight(fw));

/* FIXME : Does _XmFrameConfigureChildren just return a value in "reply" here ? */
	XdbDebug2(__FILE__, fw, w, "GeometryManager : _XmFrameConfigureChildren => %s\n",
		XdbWidgetGeometry2String(reply));
    } else {
	if (w == Frame_TitleArea(fw)) {
	    reply->width = XtWidth(fw)
		- 2 * (MGR_ShadowThickness(fw) + Frame_MarginHeight(fw) + FrameC_ChildHSpacing(w));
	    reply->height = XtHeight(fw) - 2 * (MGR_ShadowThickness(fw) + Frame_MarginWidth(fw));
	    reply->request_mode = CWWidth | CWHeight;
	} else {	/* Work Area */
	    reply->width = XtWidth(fw) - 2 * (MGR_ShadowThickness(fw) + Frame_MarginWidth(fw));
	    reply->height = XtHeight(fw) - 2 * (MGR_ShadowThickness(fw) + Frame_MarginHeight(fw));
	    reply->request_mode = CWWidth | CWHeight;
	}
	if (reply->width < 1)
	    reply->width = 1;
	if (reply->height < 1)
	    reply->height = 1;

	XdbDebug2(__FILE__, fw, w, "GeometryManager determined child size at %d [%s]\n",
		__LINE__, XdbWidgetGeometry2String(reply));
    }

    Frame_OldShadowX(w) = 0;
    if (Frame_TitleArea(fw) && XtIsManaged(Frame_TitleArea(fw)))
	Frame_OldShadowY(fw) = XtHeight(Frame_TitleArea(fw)) +
				MGR_ShadowThickness(fw) +
				Frame_MarginHeight(fw);
    else
	Frame_OldShadowY(fw) = MGR_ShadowThickness(fw) +
				Frame_MarginHeight(fw);

    Frame_OldWidth(fw) = XtWidth(fw);
    Frame_OldHeight(fw) = XtHeight(fw);

    if ((request->request_mode & (CWWidth|CWHeight)) == (CWWidth|CWHeight) &&
	reply->width == request->width && reply->height == request->height) {
	XdbDebug2(__FILE__, fw, w, "Frame GeometryManager => Yes (%d %d)\n",
	    reply->width, reply->height);
	return XtGeometryYes;
    } else if (request->width == XtWidth(w) && request->height == XtHeight(w)) {
	XdbDebug2(__FILE__, fw, w, "Frame GeometryManager => No\n");
	return XtGeometryNo;
    } else {
	XdbDebug2(__FILE__, fw, w, "Frame GeometryManager => Almost (%d %d)\n",
	    reply->width, reply->height);
	return XtGeometryAlmost;
    }
}

static void
change_managed(Widget w)
{
    int i;
    Widget child;
    Dimension wd, ht;
    XtWidgetGeometry request;

    Frame_TitleArea(w) = NULL;
    Frame_WorkArea(w) = NULL;

    for (i=0; i < ((XmFrameWidget)w)->composite.num_children; i++)
    {
	child = ((XmFrameWidget)w)->composite.children[i];

	if (!XtIsManaged(child))
	    continue;
	else if (FrameC_ChildType(child) != XmFRAME_GENERIC_CHILD)
	{
	    if (FrameC_ChildType(child) == XmFRAME_TITLE_CHILD)
	    {
		if (!Frame_TitleArea(w))
		    Frame_TitleArea(w) = child;
	    }
	    else if (FrameC_ChildType(child) == XmFRAME_WORKAREA_CHILD)
	    {
		if (!Frame_WorkArea(w))
		    Frame_WorkArea(w) = child;
	    }
	    else {
		XdbDebug(__FILE__, w, "XmFrame illegal child type resource\n");
	    }
	}
    }
 
    _XmFrameComputeSize(w, NULL, NULL, &wd, &ht);

    if (wd > XtWidth(w) || ht > XtHeight(w))
    {
	request.request_mode = (CWWidth|CWHeight);
	request.width = wd;
	request.height = ht;
	_XmMakeGeometryRequest(w, &request);
	wd = request.width;
	ht = request.height;

	_XmFrameConfigureChildren(w, NULL, NULL, wd, ht);
    }

    Frame_OldShadowX(w) = 0;
    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
	Frame_OldShadowY(w) = XtHeight(Frame_TitleArea(w)) +
				MGR_ShadowThickness(w) +
				Frame_MarginHeight(w);
    else
	Frame_OldShadowY(w) = MGR_ShadowThickness(w) +
				Frame_MarginHeight(w);

    Frame_OldWidth(w) = XtWidth(w);
    Frame_OldHeight(w) = XtHeight(w);

    _XmNavigChangeManaged(w);
}

/* Constraint methods */

static void 
constraint_initialize(Widget request, 
		      Widget new_w, 
		      ArgList args, 
		      Cardinal *num_args)
{
    if (FrameC_ChildHSpacing(new_w) == XmINVALID_DIMENSION)
	FrameC_ChildHSpacing(new_w) = Frame_MarginWidth(XtParent(new_w)) + 10;
}

static Boolean
constraint_set_values(Widget current, 
		      Widget request, 
		      Widget new_w, 
		      ArgList args, 
		      Cardinal *num_args)
{
    Boolean redisplay = False;

    if (FrameC_ChildType(current) != FrameC_ChildType(new_w) ||
	FrameC_ChildHAlignment(current) != FrameC_ChildHAlignment(new_w) ||
	FrameC_ChildHSpacing(current) != FrameC_ChildHSpacing(new_w) ||
	FrameC_ChildVAlignment(current) != FrameC_ChildVAlignment(new_w))
	redisplay = True;

    return redisplay;
}

/*
 * Danny 11/1/1997 - adding some risk to the implementation.
 *
 * This function is only called with an instigator and a desired
 * geometry from GeometryManager.
 *
 * The problem is that the result of this function depends highly on
 * whether you pass an instigator (and geometry) or not.
 *
 * Since the only place from which this happens is one where we assume
 * that we're actually doing this geometry, we will now not go wrong in
 * setting the instigator widget's geometry to the desired one.
 *
 * The above assumption is true because in all of LessTif we're not
 * implementing QueryOnly in the Geometry Managers.
 *
 * You can disable this with #undef OPTIMISTIC_COMPUTE_SIZE above.
 */
static void
_XmFrameComputeSize(Widget w, Widget instig, XtWidgetGeometry *desired,
		    Dimension *wd, Dimension *ht)
{
    Dimension curw, curh;

    XdbDebug(__FILE__, w, "_XmFrameComputeSize");
    if (instig != w && instig)
	XdbDebug0(__FILE__, w, " Instig %s (desired geo %s)",
		XtName(instig), XdbWidgetGeometry2String(desired));
    if (Frame_TitleArea(w))
	XdbDebug0(__FILE__, w, " Title %s wid %d ht %d",
		XtName(Frame_TitleArea(w)), XtWidth(Frame_TitleArea(w)),
		XtHeight(Frame_TitleArea(w)));
    if (Frame_WorkArea(w))
	XdbDebug0(__FILE__, w, " Work %s wid %d ht %d",
		XtName(Frame_WorkArea(w)), XtWidth(Frame_WorkArea(w)),
		XtHeight(Frame_WorkArea(w)));
    XdbDebug0(__FILE__, w, "\n");

    curw = curh = 0;

#ifdef	OPTIMISTIC_COMPUTE_SIZE
    if (Frame_TitleArea(w) && instig == Frame_TitleArea(w)) {
	if (desired->request_mode & CWWidth)
	    XtWidth(Frame_TitleArea(w)) = desired->width;
	if (desired->request_mode & CWHeight)
	    XtHeight(Frame_TitleArea(w)) = desired->height;
    }
    if (Frame_WorkArea(w) && instig == Frame_WorkArea(w)) {
	if (desired->request_mode & CWWidth)
	    XtWidth(Frame_WorkArea(w)) = desired->width;
	if (desired->request_mode & CWHeight)
	    XtHeight(Frame_WorkArea(w)) = desired->height;
    }
#endif

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	if (instig == Frame_TitleArea(w) && desired->request_mode & CWHeight)
	    curh += desired->height;
	else
	    curh += XtHeight(Frame_TitleArea(w));

	if (instig == Frame_TitleArea(w) && desired->request_mode & CWWidth)
	    curw = desired->width + 2 * FrameC_ChildHSpacing(Frame_TitleArea(w));
	else
	    curw = XtWidth(Frame_TitleArea(w)) + 2 * FrameC_ChildHSpacing(Frame_TitleArea(w));
    }

    if (Frame_WorkArea(w) && XtIsManaged(Frame_WorkArea(w)))
    {
	if (instig == Frame_WorkArea(w) && desired->request_mode & CWHeight)
	    curh += desired->height;
	else
	    curh += XtHeight(Frame_WorkArea(w));

	if (instig == Frame_WorkArea(w) && desired->request_mode & CWWidth)
	    curw = desired->width;
	else
	    curw = _XmMax(curw, XtWidth(Frame_WorkArea(w)));
    }

    curh += 2 * Frame_MarginHeight(w) + 2 * MGR_ShadowThickness(w);
    curw += 2 * Frame_MarginWidth(w) + 2 * MGR_ShadowThickness(w);

    *wd = curw;
    *ht = curh;

    XdbDebug2(__FILE__, w, instig, "_XmFrameComputeSize (%s) => %d %d\n",
	XdbWidgetGeometry2String(desired), curw, curh);
}

static void
_XmFrameConfigureChildren(Widget w, Widget instig, XtWidgetGeometry *desired,
			  Dimension curw, Dimension curh)
{
    int title_x = 0, title_y = 0;
    int workarea_x = 0, workarea_y = 0;

    XdbDebug2(__FILE__, w, instig,
	"_XmFrameConfigureChildren (child wants %s, Frame geo %d %d)\n",
	XdbWidgetGeometry2String(desired), curw, curh);

    workarea_y = MGR_ShadowThickness(w) + Frame_MarginHeight(w);

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	switch (FrameC_ChildVAlignment(Frame_TitleArea(w)))
	{
	case XmALIGNMENT_CENTER:
	    title_y = 0;
	    workarea_y = XtHeight(Frame_TitleArea(w)) + Frame_MarginHeight(w);
	    break;
	case XmALIGNMENT_BASELINE_BOTTOM:
	    _XmWarning(w, "_XmFrameConfigureChildren doesn't implement"
			"XmALIGNMENT_BASELINE_BOTTOM yet\n");
	    /* FIX ME - fill this out */
	    break;
	case XmALIGNMENT_BASELINE_TOP:
	    title_y = 0;
	    workarea_y = MGR_ShadowThickness(w)
			 + Frame_MarginHeight(w);
	    break;
	case XmALIGNMENT_WIDGET_TOP:
	    title_y = 0;
	    workarea_y = XtHeight(Frame_TitleArea(w)) + MGR_ShadowThickness(w)
			 + Frame_MarginHeight(w);
	    break;
	case XmALIGNMENT_WIDGET_BOTTOM:
	    title_y = MGR_ShadowThickness(w) + Frame_MarginHeight(w);
	    workarea_y = MGR_ShadowThickness(w) + Frame_MarginHeight(w);
	    break;
	}

	title_x = MGR_ShadowThickness(w) +
		  FrameC_ChildHSpacing(Frame_TitleArea(w));

	switch (FrameC_ChildHAlignment(Frame_TitleArea(w))) {
	case XmALIGNMENT_BEGINNING:
	    if (MGR_StringDirection(w) != XmSTRING_DIRECTION_L_TO_R)
	    {
		title_x = XtWidth(w) - XtWidth(Frame_TitleArea(w)) - title_x -
			  2 * XtBorderWidth(Frame_TitleArea(w));
	    }
	    break;
	case XmALIGNMENT_CENTER:
	    title_x = (curw >> 1) - (XtWidth(Frame_TitleArea(w)) >> 1);
	    break;
	case XmALIGNMENT_END:
	default:
	    if (MGR_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	    {
		title_x = XtWidth(w) - XtWidth(Frame_TitleArea(w)) - title_x -
			  2 * XtBorderWidth(Frame_TitleArea(w));
	    }
	    break;
	}

	if (Frame_TitleArea(w) == instig)
	{
	    desired->request_mode = CWBorderWidth|CWWidth|CWHeight|CWX|CWY;
	    desired->x = title_x;
	    desired->y = title_y;
	    desired->width =
		 _XmMin(XtWidth(Frame_TitleArea(w)),
			curw - 2 * MGR_ShadowThickness(w)
			- 2 * Frame_MarginWidth(w)
			- 2 * FrameC_ChildHSpacing(Frame_TitleArea(w)));
	    desired->height = XtHeight(Frame_TitleArea(w));
	    desired->border_width = XtBorderWidth(Frame_TitleArea(w));
	}
	else
	{
	    _XmConfigureObject(Frame_TitleArea(w),
			       title_x, title_y,
			       _XmMin(XtWidth(Frame_TitleArea(w)),
					curw - 2 * MGR_ShadowThickness(w)
					- 2 * Frame_MarginWidth(w)
					- 2 * FrameC_ChildHSpacing(Frame_TitleArea(w))),
			       XtHeight(Frame_TitleArea(w)),
			       XtBorderWidth(Frame_TitleArea(w)));
	}
    }
	
    if (Frame_WorkArea(w) && XtIsManaged(Frame_WorkArea(w)))
    {
	workarea_x = MGR_ShadowThickness(w) + Frame_MarginWidth(w);

	if (Frame_WorkArea(w) == instig)
	{
	    desired->request_mode = CWBorderWidth|CWWidth|CWHeight|CWX|CWY;
	    desired->x = workarea_x;
	    desired->y = workarea_y;
	    desired->border_width = XtBorderWidth(Frame_WorkArea(w));

/*
 * This is the place that makes Xinvest-2.1 fail.
 *
 * In a call with instig vs. one without it, we get different geometries for the height
 * of the workarea child.
 *
 * Danny 4/10/1996
 */
#if 1
	    desired->width = curw - 2 * MGR_ShadowThickness(w)
			- 2 * Frame_MarginWidth(w);
	    desired->height = curh - XtY(Frame_WorkArea(w))
			- Frame_MarginHeight(w) - MGR_ShadowThickness(w);
#else
	    Dimension	wd, ht;

	    wd = curw - 2 * MGR_ShadowThickness(w) - 2 * Frame_MarginWidth(w);
	    ht = curh - XtY(Frame_WorkArea(w))
		- Frame_MarginHeight(w) - MGR_ShadowThickness(w);

	    if (wd > desired->width)
		desired->width = wd;
	    if (ht > desired->height)
		desired->height = ht;
#endif
	}
	else
	{
#if 0
/* xmcd breakage here ... */
	    _XmConfigureObject(Frame_WorkArea(w),
			       workarea_x, workarea_y,
			       curw - 2 * MGR_ShadowThickness(w)
				       - 2 * Frame_MarginWidth(w),
			       curh - XtY(Frame_WorkArea(w))
				        - Frame_MarginHeight(w)
				        - MGR_ShadowThickness(w),
			       XtBorderWidth(Frame_WorkArea(w)));
#else
	    _XmConfigureObject(Frame_WorkArea(w),
			       workarea_x, workarea_y,
			       curw - 2 * MGR_ShadowThickness(w)
				       - 2 * Frame_MarginWidth(w),
			       curh - workarea_y
				        - Frame_MarginHeight(w)
				        - MGR_ShadowThickness(w),
			       XtBorderWidth(Frame_WorkArea(w)));
#endif
	}
    }
}

static void
Enter(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

static void
GetFocus(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

static void
Arm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

static void
Activate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

static void
ManagerGadgetDrag(Widget w, XEvent *event,
		  String *params, Cardinal *num_params)
{
}

static void
ManagerParentActivate(Widget w, XEvent *event,
		      String *params, Cardinal *num_params)
{
}

static void
ManagerParentCancel(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
}

Widget 
XmCreateFrame(Widget parent,
	      char *name,
	      Arg *arglist,
	      Cardinal argCount)
{
    return XtCreateManagedWidget(name,
		                 xmFrameWidgetClass,
				 parent,
				 arglist,
				 argCount);
}
