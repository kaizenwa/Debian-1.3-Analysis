/**
 *
 * $Id: ToggleBG.c,v 1.10 1997/01/11 02:19:53 miers Exp $
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

static char rcsid[] = "$Id: ToggleBG.c,v 1.10 1997/01/11 02:19:53 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuShell.h>
#include <Xm/MenuUtilP.h>
#include <Xm/CacheP.h>
#include <X11/Xfuncs.h>
#include <X11/ShellP.h>
#include <Xm/ToggleBP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args);
static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);

static void Arm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Select(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonUp(Widget w, XEvent *event, String *params, Cardinal *num_params);

static void secondary_object_create(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_prehook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_posthook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args);
static Cardinal get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data);

/*
 * resources
 */
#define Offset(field) XtOffsetOf(XmToggleButtonGCacheObjRec, toggle_cache.field)
static XtResource cache_resources[] = {
    {
	XmNindicatorSize, XmCIndicatorSize, XmRVerticalDimension,
	sizeof(Dimension), Offset(indicator_dim),
	XmRImmediate,(XtPointer) XmINVALID_DIMENSION
    },
    {
	XmNindicatorType, XmCIndicatorType, XmRIndicatorType,
	sizeof(unsigned char), Offset(ind_type),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNvisibleWhenOff, XmCVisibleWhenOff, XmRBoolean,
	sizeof(Boolean), Offset(visible),
	XmRImmediate,(XtPointer)85 /* if anybody knows why, I'd like to know */
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(spacing),
	XmRImmediate, (XtPointer)4
    },
    {
	XmNselectPixmap, XmCSelectPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(on_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNselectInsensitivePixmap, XmCSelectInsensitivePixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(insen_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNindicatorOn, XmCIndicatorOn, XmRBoolean,
	sizeof(Boolean), Offset(ind_on),
	XmRImmediate,(XtPointer)True
    },
    {
	XmNfillOnSelect, XmCFillOnSelect, XmRBoolean,
	sizeof(Boolean), Offset(fill_on_select),
	XmRImmediate, (XtPointer)85
    },
    {
	XmNselectColor, XmCSelectColor, XmRPixel,
	sizeof(Pixel), Offset(select_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
    }
};

static XmSyntheticResource cache_syn_resources[] = {
    {
	XmNspacing,
	sizeof(Dimension), Offset(spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNindicatorSize,
	sizeof(Dimension), Offset(spacing),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

XmToggleButtonGCacheObjClassRec xmToggleButtonGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGCacheObjClassRec,
        /* class_name            */ "XmToggleButtonGCacheObjClass",
	/* widget_size           */ sizeof(XmToggleButtonGCacheObjRec),
	/* class_initialize      */ NULL,
	/* class_part_initialize */ NULL,
	/* class_inited          */ FALSE,
	/* initialize            */ NULL,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ cache_resources,
	/* num_resources         */ XtNumber(cache_resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ NULL,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ NULL,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ cache_syn_resources,
        /* num_syn_resources  */ XtNumber(cache_syn_resources),
        /* extension          */ NULL
    },
    /* LabelGCacheObj part */
    {
	/* foo                */ 0
    },
    /* ToggleButtonGCacheObj part */
    {
	/* foo                */ 0
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmToggleButtonGadgetRec, toggle.field)

/* Resources for the togglebutton class */
static XtResource resources[] = {
    {
	XmNset, XmCSet, XmRBoolean,
	sizeof(Boolean), Offset(set),
	XmRImmediate,(XtPointer)False
    },
    {
	XmNvalueChangedCallback, XmCValueChangedCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_CB),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNarmCallback, XmCArmCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(arm_CB),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdisarmCallback, XmCDisarmCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(disarm_CB),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmToggleButtonGadgetRec, gadget.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmToggleButtonGadgetRec,gadget.highlight_thickness),
	XmRImmediate, (XtPointer)2
    }
};

static XmBaseClassExtRec _XmToggleBGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (XtPointer)&xmToggleButtonGCacheObjClassRec,
    /* secondary_object_create   */ secondary_object_create,
    /* get_secondary_resources   */ get_sec_res_data,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ get_values_prehook,
    /* get_values_posthook       */ get_values_posthook,
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

static XmCacheClassPart cache_part = {
    /* cache head part */
    {
	/* next         */ NULL,
	/* prev         */ NULL,
	/* ref_count    */ 0
    },
    _XmCacheCopy,
    _XmCacheDelete,
    _XmToggleBCacheCompare
};

static XmGadgetClassExtRec _XmToggleBGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ XmInheritBaselineProc,
    /* display_rect_proc         */ XmInheritDisplayRectProc,
};

XmToggleButtonGadgetClassRec xmToggleButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGadgetClassRec,
	/* class_name            */ "XmToggleButtonGadget",
	/* widget_size           */ sizeof(XmToggleButtonGadgetRec),
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
	/* compress_exposure     */ XtExposeNoCompress,
	/* compress_enterleave   */ FALSE,
 	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ get_values_hook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmToggleBGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, 
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* fix me */
	/* arm_and_activate   */ ArmAndActivate,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL, /* fix me */
	/* syn_resources      */ NULL,
	/* num_syn_resources  */ 0,
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmToggleBGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
        /* setOverrideCallback */ NULL, /* fix me */
        /* menuProcs           */ NULL, /* fix me */
	/* extension           */ NULL
    },
    /* XmToggleButtonGadget part */
    {
	/* extension */ NULL
    },
};

WidgetClass xmToggleButtonGadgetClass = (WidgetClass)&xmToggleButtonGadgetClassRec;

/* 
 *  Some #defines to make the code below more readable
 */

#define IN_MENU(w) (LabG_MenuType(w) == XmMENU_POPUP || \
                    LabG_MenuType(w) == XmMENU_PULLDOWN)

/******************************* CACHE PART *********************************/
static void
secondary_object_create(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XtPointer nsec, rsec;
    XmWidgetExtData ed;
    int size;

    XdbDebug(__FILE__, new_w, "ToggleButtonGCacheRec %s being initialized.\n",
	     XtName(new_w));

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);

    size = (*bce)->secondaryObjectClass->core_class.widget_size;
    nsec = _XmExtObjAlloc(size);
    rsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent(new_w);
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    XtGetSubresources(new_w, nsec, NULL, NULL,
                      (*bce)->secondaryObjectClass->core_class.resources,
                      (*bce)->secondaryObjectClass->core_class.num_resources,
                      args, *num_args);

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;
    ed->reqWidget = (Widget)rsec;

    bcopy(nsec, rsec, size);
    ((XmExtRec *)rsec)->object.self = (Widget)rsec;

    _XmPushWidgetExtData(new_w, ed, XmCACHE_EXTENSION);

    LabG_Cache(new_w) = &(((XmLabelGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmLabelGCacheObject)rsec)->label_cache);

    TBG_Cache(new_w) = &(((XmToggleButtonGCacheObject)nsec)->toggle_cache);
    TBG_Cache(request) = &(((XmToggleButtonGCacheObject)rsec)->toggle_cache);
}

int
_XmToggleBCacheCompare(XtPointer A, XtPointer B)
{
    return !bcmp(((XmToggleButtonGCacheObjPart *)A),
		 ((XmToggleButtonGCacheObjPart *)B),
		 sizeof(XmToggleButtonGCacheObjPart));
}

/******************************* CACHE PART *********************************/
static void
class_initialize()
{
    XtResourceList combined, labels;
    int ncom; 
    Cardinal nlabels;

    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(TBG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(TBG_ClassCachePart(NULL));
    ClassCacheHead(TBG_ClassCachePart(NULL)).next =
	&ClassCacheHead(TBG_ClassCachePart(NULL));

    _XmToggleBGRectClassExtRec.record_type = XmQmotif;

    /*
     * Label subclasses (ToggleBG, PushBG, CascadeBG) have a problem.  Since
     * we do all the subpart manipulation in the pre- and post- hooks, and
     * since those hooks aren't chained, we have to either make multiple
     * calls to XtGetSubresources/Xt[Get|Set]Subvalues, or merge the resource
     * lists.  Since I just wrote _XmTransformSubresources, seems like a
     * waste not to use it.
     */
    ncom = XtNumber(cache_resources) +
           xmLabelGCacheObjClassRec.object_class.num_resources;

    _XmTransformSubResources(xmLabelGCacheObjClassRec.object_class.resources,
                             xmLabelGCacheObjClassRec.object_class.num_resources,
                             &labels, &nlabels);

    combined = (XtResourceList)XtMalloc(sizeof(XtResource) * ncom);
    bcopy(labels, combined, nlabels * sizeof(XtResource));
    bcopy(cache_resources,
	  &combined[nlabels],
	  XtNumber(cache_resources) * sizeof(XtResource));
    XtFree((char *)labels);

    xmToggleButtonGCacheObjClassRec.object_class.resources = combined;
    xmToggleButtonGCacheObjClassRec.object_class.num_resources = ncom;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmTOGGLE_BUTTON_GADGET_BIT);
}

static void
CreateSelectGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = TBG_SelectColor(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillSolid;

    TBG_SelectGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateBackgroundGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XmParentBackground(w);
    values.background = XmParentForeground(w);
    values.fill_style = FillSolid;
    TBG_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "ToggleBG InitializePrehook\n");
}

static void
initialize_posthook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XmWidgetExtData ext;

    XdbDebug(__FILE__, new_w, "ToggleBG InitializePosthook\n");

    /* don't let the null fool you */
    LabG_Cache(new_w) = _XmCachePart(LabG_ClassCachePart(NULL),
                                   (XtPointer)LabG_Cache(new_w),
                                   sizeof(XmLabelGCacheObjPart));
    TBG_Cache(new_w) = _XmCachePart(TBG_ClassCachePart(NULL),
                                  (XtPointer)TBG_Cache(new_w),
                                  sizeof(XmToggleButtonGCacheObjPart));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);
    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);
    XtFree((char *)ext);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    TBG_Armed(new_w) = False;

    TBG_VisualSet(new_w) = TBG_IndicatorSet(new_w) = TBG_Set(new_w);

    if (TBG_IndOn(new_w))
	TBG_IndicatorDim(new_w) = 10;
    else
	TBG_IndicatorDim(new_w) = 0;

    CreateSelectGC(new_w);
    CreateBackgroundGC(new_w);

    /*
     * have to check request since new may have been polluted by a
     * superclass
     */
    if (XtWidth(request) == (Dimension)0 || XtHeight(request) == (Dimension)0) {
	/* call this to determine the real dimensions of the label 
	 * since we don't want to use 0
	 */
	XtWidth(new_w) = 0;
	XtHeight(new_w) = 0;

	_XmCalcLabelGDimensions(new_w);

	(*xmLabelGadgetClassRec.rect_class.resize)(new_w);
    }

    if (TBG_IndType(new_w) == (unsigned char)XmUNSPECIFIED) {
	if (XmIsRowColumn(XtParent(new_w)) && RC_RadioBehavior(XtParent(new_w)))
	    TBG_IndType(new_w) = XmONE_OF_MANY;
	else
	    TBG_IndType(new_w) = XmN_OF_MANY;
    }

    if (!XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
        _XmError(new_w, "parent should be manager.");


    LabG_MarginLeft(new_w) = TBG_IndicatorDim(new_w) + (TBG_IndOn(new_w)
				? 2 * TBG_Spacing(new_w)
				: 0);

    if (LabG_MarginLeft(new_w) != LabG_MarginLeft(request)) {

	if (XtWidth(request) == 0)
	    XtWidth(new_w) = 0;
	if (XtHeight(request) == 0)
	    XtHeight(new_w) = 0;

	_XmCalcLabelGDimensions(new_w);

	(*xmLabelGadgetClassRec.rect_class.resize)(new_w);
    }

    if (IN_MENU(new_w)) {
	LabG_Highlight(new_w) = 0;
	if (G_ShadowThickness(new_w) == 0)
	    G_ShadowThickness(new_w) = 2;
	TBG_Visible(new_w) = False;
    }

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT | XmENTER_EVENT |
			XmLEAVE_EVENT | XmMOTION_EVENT | XmFOCUS_IN_EVENT |
			XmFOCUS_OUT_EVENT | XmHELP_EVENT;

}

static void
destroy(Widget w)
{
    XtReleaseGC(w, TBG_SelectGC(w));
    XtReleaseGC(w, TBG_BackgroundGC(w));
    _XmCacheDelete(TBG_Cache(w));
}

static Boolean
set_values_prehook(Widget old,
                   Widget request,
                   Widget new_w,
                   ArgList args,
                   Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    int size;
    XtPointer nsec, rsec;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);
    rsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent(new_w);
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    bcopy(LabG_Cache(new_w),
          &((XmLabelGCacheObject)nsec)->label_cache,
          sizeof(XmLabelGCacheObjPart));
    bcopy(TBG_Cache(new_w),
          &((XmToggleButtonGCacheObject)nsec)->toggle_cache,
          sizeof(XmToggleButtonGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;
    ed->reqWidget = (Widget)rsec;

    _XmPushWidgetExtData(new_w, ed, XmCACHE_EXTENSION);

    _XmGadgetImportSecondaryArgs(new_w, args, num_args);

    XtSetSubvalues((XtPointer)nsec,
                    (*bce)->secondaryObjectClass->core_class.resources,
                    (*bce)->secondaryObjectClass->core_class.num_resources,
                    args, *num_args);

    bcopy(nsec, rsec, size);

    LabG_Cache(new_w) = &(((XmToggleButtonGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmToggleButtonGCacheObject)rsec)->label_cache);
    TBG_Cache(new_w) = &(((XmToggleButtonGCacheObject)nsec)->toggle_cache);
    TBG_Cache(request) = &(((XmToggleButtonGCacheObject)rsec)->toggle_cache);

    _XmExtImportArgs((Widget)nsec, args, num_args);

    return FALSE;
}

static Boolean
set_values_posthook(Widget old,
                   Widget request,
                   Widget new_w,
                   ArgList args,
                   Cardinal *num_args)
{
    XmWidgetExtData ext;

    if (!_XmLabelCacheCompare(LabG_Cache(new_w), LabG_Cache(old))) {

	_XmCacheDelete((XtPointer)LabG_Cache(old));

	LabG_Cache(new_w) = _XmCachePart(LabG_ClassCachePart(NULL),
				       (XtPointer)LabG_Cache(new_w),
				       sizeof(XmLabelGCacheObjPart));
    }
    else
	LabG_Cache(new_w) = LabG_Cache(old);

    if (!_XmToggleBCacheCompare(TBG_Cache(new_w), TBG_Cache(old))) {

	_XmCacheDelete((XtPointer)TBG_Cache(old));

	TBG_Cache(new_w) = _XmCachePart(TBG_ClassCachePart(NULL),
				      (XtPointer)TBG_Cache(new_w),
				      sizeof(XmToggleButtonGCacheObjPart));
    }
    else
	TBG_Cache(new_w) = TBG_Cache(old);

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);

    XtFree( (char *) ext);

    return FALSE;
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = False;

    if (TBG_SelectColor(new_w) != TBG_SelectColor(old)) {
	XtReleaseGC(new_w, TBG_SelectGC(new_w));
	CreateSelectGC(new_w);
        refresh_needed = True;
    }
    if (XmParentBackground(new_w) != XmParentBackground(old)) {
	XtReleaseGC(new_w, TBG_BackgroundGC(new_w));
	CreateBackgroundGC(new_w);
	refresh_needed = True;
    }

#if 0
/* Huh - I thought we were past this type of madness.
 * Why is this here ???
 */
    if (XtWidth(request) != XtWidth(new_w)
	   || XtHeight(request) != XtHeight(new_w))
    {
	  XtWidth(new_w) = XtWidth(request);
	  XtHeight(new_w) = XtHeight(request);
	  refresh_needed = False;
    }
#endif

    TBG_VisualSet(new_w) = TBG_IndicatorSet(new_w) = TBG_Set(new_w);

    if (TBG_Set(request) != TBG_Set(new_w))
	refresh_needed = True;

    if (TBG_IndType(request) != TBG_IndType(new_w))
    {
	refresh_needed = True;
	if ((TBG_IndType(new_w) != XmN_OF_MANY) &&
	    (TBG_IndType(new_w) != XmONE_OF_MANY))
	{
	    TBG_IndType(new_w) = TBG_IndType(request);
	}
    }

    return refresh_needed;
}

static void
get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    int size;
    XtPointer nsec;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    bcopy(LabG_Cache(new_w),
          &((XmLabelGCacheObject)nsec)->label_cache,
          sizeof(XmLabelGCacheObjPart));
    bcopy(TBG_Cache(new_w),
          &((XmToggleButtonGCacheObject)nsec)->toggle_cache,
          sizeof(XmToggleButtonGCacheObjPart));

    /*
     * don't do this and ResInd will blow up.
     */
    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent(new_w);
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData(new_w, ed, XmCACHE_EXTENSION);

    XtGetSubvalues((XtPointer)nsec,
                    (*bce)->secondaryObjectClass->core_class.resources,
                    (*bce)->secondaryObjectClass->core_class.num_resources,
                    args, *num_args);

    _XmExtGetValuesHook((Widget)nsec, args, num_args);
}

static void
get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args)
{
     XmWidgetExtData ext;

     _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

     _XmExtObjFree((XtPointer)ext->widget);

     XtFree( (char *) ext);
}

static void
get_values_hook(Widget w, ArgList args, Cardinal *num_args)
{
    XdbDebug(__FILE__, w, "ToggleButtonGadget: GetValuesHook\n");
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    Boolean   State = TBG_VisualSet(w);
    Pixmap tmp_pix = XmUNSPECIFIED_PIXMAP, tmp2_pix = XmUNSPECIFIED_PIXMAP;

    if (!TBG_IndOn(w)) {
	if (TBG_FillOnSelect(w)) {
	    XFillRectangle(XtDisplayOfObject(w), XtWindowOfObject(w),
			   State ? TBG_SelectGC(w) : TBG_BackgroundGC(w),
			   XtX(w) + LabG_Highlight(w) + LabG_Shadow(w),
			   XtY(w) + LabG_Highlight(w) + LabG_Shadow(w),
			   XtWidth(w) -
				2 * (LabG_Highlight(w) + LabG_Shadow(w)),
			   XtHeight(w) -
				2 * (LabG_Highlight(w) + LabG_Shadow(w)));
	}

	if (LabG_IsPixmap(w) && State &&
	    TBG_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    tmp_pix = LabG_Pixmap(w);
	    tmp2_pix = LabG_PixmapInsensitive(w);
	    LabG_Pixmap(w) = TBG_OnPixmap(w);
	}
#define superclass (&xmLabelGadgetClassRec)
	(*superclass->rect_class.expose)(w, event, region);
#undef superclass
	if (LabG_IsPixmap(w) && State &&
	    TBG_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    LabG_Pixmap(w) = tmp_pix;
	    LabG_PixmapInsensitive(w) = tmp2_pix;
	}

	if (!IN_MENU(w)) {
	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w), 
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   LabG_Highlight(w) + XtX(w),
			   LabG_Highlight(w) + XtY(w),
			   XtWidth(w) - 2 * LabG_Highlight(w),
			   XtHeight(w) - 2 * LabG_Highlight(w),
			   LabG_Shadow(w),
 			   State ? XmSHADOW_IN : XmSHADOW_OUT);
	}
	else {
	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w), 
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   XtX(w), XtY(w),
			   XtWidth(w), XtHeight(w),
			   LabG_Shadow(w),
 			   TBG_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
	}
    }
    else if (TBG_IndType(w) == XmN_OF_MANY){
	if (LabG_IsPixmap(w) && State &&
	    TBG_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    tmp_pix = LabG_Pixmap(w);
	    tmp2_pix = LabG_PixmapInsensitive(w);
	    LabG_Pixmap(w) = TBG_OnPixmap(w);
	}
#define superclass (&xmLabelGadgetClassRec)
	(*superclass->rect_class.expose)(w, event, region);
#undef superclass
	if (LabG_IsPixmap(w) && State &&
	    TBG_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    LabG_Pixmap(w) = tmp_pix;
	    LabG_PixmapInsensitive(w) = tmp2_pix;
	}

	XFillRectangle(XtDisplayOfObject(w), XtWindowOfObject(w),
		       State ? TBG_SelectGC(w) : TBG_BackgroundGC(w),
		       TBG_Spacing(w) + LabG_Highlight(w)
			 + LabG_Shadow(w) + XtX(w),
		       (XtHeight(w) - TBG_IndicatorDim(w)) / 2 + XtY(w),
		       TBG_IndicatorDim(w),
		       TBG_IndicatorDim(w));
	if (TBG_Visible(w) || State) {
	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w),
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   TBG_Spacing(w) + LabG_Highlight(w)
				+ LabG_Shadow(w) + XtX(w),
			   (XtHeight(w) - TBG_IndicatorDim(w)) / 2 + XtY(w),
			   TBG_IndicatorDim(w), TBG_IndicatorDim(w),
			   2,
			   State ? XmSHADOW_IN : XmSHADOW_OUT);
	}
	if (!IN_MENU(w)) {
	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w),
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   G_HighlightThickness(w) + XtX(w),
			   G_HighlightThickness(w) + XtY(w),
			   XtWidth(w) - 2 * G_HighlightThickness(w),
			   XtHeight(w) - 2 * G_HighlightThickness(w),
			   G_ShadowThickness(w),
			   XmSHADOW_OUT);
	}
	else {
	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w), 
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   XtX(w), XtY(w),
			   XtWidth(w), XtHeight(w),
			   LabG_Shadow(w),
 			   TBG_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
	}
    }
    else 
    {
	if (LabG_IsPixmap(w) && State &&
	    TBG_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    tmp_pix = LabG_Pixmap(w);
	    tmp2_pix = LabG_PixmapInsensitive(w);
	    LabG_Pixmap(w) = TBG_OnPixmap(w);
	}
#define superclass (&xmLabelGadgetClassRec)
	(*superclass->rect_class.expose)(w, event, region);
#undef superclass
	if (LabG_IsPixmap(w) && State &&
	    TBG_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    LabG_Pixmap(w) = tmp_pix;
	    LabG_PixmapInsensitive(w) = tmp2_pix;
	}

	if (TBG_Visible(w) || State) {
	    _XmDrawDiamond(XtDisplayOfObject(w), XtWindowOfObject(w),
			   State
				? XmParentTopShadowGC(w)
				: XmParentBottomShadowGC(w),
			   State
				? XmParentBottomShadowGC(w)
				: XmParentTopShadowGC(w),
			   State ? TBG_SelectGC(w) : TBG_BackgroundGC(w),
			   TBG_Spacing(w) + LabG_Highlight(w) + XtX(w),
			   (XtHeight(w) - TBG_IndicatorDim(w)) / 2 + XtY(w),
			   TBG_IndicatorDim(w), TBG_IndicatorDim(w),
			   2,
			   True);
	}
	if (!IN_MENU(w)) {
	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w),
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   G_HighlightThickness(w) + XtX(w),
			   G_HighlightThickness(w) + XtY(w),
			   XtWidth(w) - 2 * G_HighlightThickness(w),
			   XtHeight(w) - 2 * G_HighlightThickness(w),
			   G_ShadowThickness(w),
			   XmSHADOW_OUT);
	}
	else {
	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w), 
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   XtX(w), XtY(w),
			   XtWidth(w), XtHeight(w),
			   LabG_Shadow(w),
 			   TBG_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
	}
    }
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIXME */

    return _XmSecondaryResourceData(&_XmToggleBGRectClassExtRec,
                                    data, NULL, NULL, NULL, NULL);
}

static void
Arm(Widget w,
    XEvent *event,
    String *params,
    Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    TBG_Armed(w) = True;
    TBG_VisualSet(w) = TBG_IndicatorSet(w) = !TBG_Set(w);

    (*exp)(w, event, (Region)NULL);

    if (TBG_ArmCallback(w)) {
	cbs.reason = XmCR_ARM;
	cbs.event = event;
	cbs.set = TBG_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   TBG_ArmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
Select(Widget w,
    XEvent *event,
    String *params,
    Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;

    if (ev->type == KeyPress || ev->type == KeyRelease ||
	((ev->x > XtX(w) && ev->x < XtX(w) + XtWidth(w)) &&
	 (ev->y > XtY(w) && ev->y < XtY(w) + XtHeight(w)))) {

#if 0
      /* why is this here?  We're not in a menu here. -- Chris */
	if (XmIsRowColumn(XtParent(w)))
	    (*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_CALLBACK,
							   XtParent(w), False,
							   w, &cbs);
#endif

	if (TBG_VisualSet(w) == TBG_IndicatorSet(w)) {

	    TBG_Set(w) = TBG_VisualSet(w);

	    if (!LabG_SkipCallback(w) && TBG_ValueChangedCallback(w)) {
		cbs.reason = XmCR_VALUE_CHANGED;
		cbs.event = event;
		cbs.set = TBG_Set(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   TBG_ValueChangedCallback(w),
				   (XtPointer)&cbs);
	    }
	}
	else
	    TBG_IndicatorSet(w) = TBG_Set(w);
    }
}


static void 
Disarm(Widget w,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    /* expose(w, NULL, (Region)NULL); */

    TBG_Set(w) = TBG_IndicatorSet(w) = TBG_VisualSet(w);

    if (TBG_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TBG_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   TBG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    TBG_Armed(w) = False;
}

static void
ArmAndActivate(Widget w,
	       XEvent *event,
	       String *params,
	       Cardinal *num_params)
{
    Arm(w, event, params, num_params);
    Select(w, event, params, num_params);
    Disarm(w, event, params, num_params);
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
ButtonUp(Widget w, 
	 XEvent *event, 
	 String *params, 
	 Cardinal *num_params)
{
    Widget parent,shell;
    Boolean validButton, poppedUp;
    XmToggleButtonCallbackStruct cbs;

    parent = XtParent(w);

    XAllowEvents(XtDisplayOfObject(w), SyncPointer, CurrentTime);

    shell = parent;
    while (!XtIsShell(shell))
	shell = XtParent(shell);

    if (event && (event->type == ButtonRelease))
	(*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_BUTTON, parent,
						       NULL, event, &validButton);

    if (!validButton)
	return;

    if (IN_MENU(w) && !XmIsMenuShell(shell))
	(*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_POPDOWN, w,
						       NULL, event, &poppedUp);
    else
	(*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_BUTTON_POPDOWN, w,
						       NULL, event, &poppedUp);

    _XmRecordEvent(event);
    
    if (poppedUp)
	return;

    TBG_Armed(w) = False;

    TBG_Set(w) = TBG_VisualSet(w);

    if (!LabG_SkipCallback(w) && TBG_ValueChangedCallback(w)) {
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.event = event;
	cbs.set = TBG_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w, 
			   TBG_ValueChangedCallback(w), 
			   (XtPointer)&cbs);
    }
    if (TBG_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TBG_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   TBG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    _XmSetInDragMode(w, False);
}


static void 
ButtonDown(Widget w, 
	   XEvent *event, 
	   String *params, 
	   Cardinal *num_params)
{
    ShellWidget popupShell;
    int validButton;
    Widget child;
    XtExposeProc exp = XtClass(w)->core_class.expose;
    
    XAllowEvents(XtDisplayOfObject(w), SyncPointer, CurrentTime);
    
    if (event && (event->type == ButtonPress))
    {
	(* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_BUTTON, XtParent(w), NULL, event, &validButton);
	if (!validButton)
	    return;
    }

    _XmSetInDragMode(w, TRUE);

    TBG_Armed(w) = True;
    TBG_VisualSet(w) = TBG_IndicatorSet(w) = !TBG_Set(w);
    (*exp)(w, event, NULL);

    popupShell = (ShellWidget)_XmGetRC_PopupPosted(XtParent(w));
    if  (popupShell)
    {
	if (popupShell->shell.popped_up)
	    (*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN,
							   (Widget)popupShell,
							   NULL, event, NULL);
	
	child = ((XmManagerWidget)XtParent(w))->manager.active_child;
	if (child && (XmIsCascadeButton(child) || XmIsCascadeButtonGadget(child)))
	    XmCascadeButtonHighlight (child, FALSE);
    }

    _XmSetInDragMode(w, FALSE);

    _XmRecordEvent(event);
}

static void
EnterWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    if (!IN_MENU(w)) {
	_XmEnterGadget(w, event, NULL, NULL);
	if (TBG_Armed(w)) {
	    TBG_VisualSet(w) = TBG_IndicatorSet(w);
	    (*exp)(w, event, (Region)NULL);
	}
    }
    else {
	if (_XmGetInDragMode(w))
	{
	    ShellWidget popupShell;

	    popupShell = (ShellWidget)_XmGetRC_PopupPosted(XtParent(w));
	    if  (popupShell)
	    {
		if (popupShell->shell.popped_up)
		    (*xmLabelClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN,
							     (Widget)popupShell,
							     NULL, 
							     event, 
							     NULL);
	    }

	    TBG_Armed(w) = True; 

	    TBG_VisualSet(w) = TBG_IndicatorSet(w) = !TBG_Set(w);

	    (*exp)(w, event, NULL);

	    if (TBG_ArmCallback(w)) {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.set = TBG_Set(w);

		XFlush(XtDisplay(w));

                XtCallCallbackList(w,
				   TBG_ArmCallback(w),
				   (XtPointer)&cbs);
            }
        }
    }
}

static void
LeaveWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    if (!IN_MENU(w))
    {
	_XmLeaveGadget(w, event, NULL, NULL);
	if (TBG_Armed(w)) {
	    TBG_VisualSet(w) = TBG_Set(w);
	    (*exp)(w, event, (Region)NULL);
	}
    }
    else
    {
	if (_XmGetInDragMode(w))
	{
	    TBG_VisualSet(w) = TBG_Set(w);

	    TBG_Armed(w) = False;

	    (*exp)(w, event, NULL);

	    if (TBG_DisarmCallback(w)) {
		cbs.reason = XmCR_DISARM;
		cbs.event = event;
		cbs.set = TBG_Set(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   TBG_DisarmCallback(w),
				   (XtPointer)&cbs);
            }
        }
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
	XdbDebug(__FILE__, gadget, "ToggleButtonGadget got arm event\n");
	if (XmIsRowColumn(XtParent(gadget)) 
	    && (RC_Type(XtParent(gadget)) == XmMENU_PULLDOWN))
	    ButtonDown(gadget, event, NULL, &num_params);
	else
	    Arm(gadget, event, NULL, &num_params);
	break;

    case XmACTIVATE_EVENT:
	XdbDebug(__FILE__, gadget, "ToggleButtonGadget got activate event\n");
	if (XmIsRowColumn(XtParent(gadget)) 
	    && (RC_Type(XtParent(gadget)) == XmMENU_PULLDOWN))
	    ButtonUp(gadget, event, NULL, &num_params);
	else
        {
	    Select(gadget, event, NULL, &num_params);
	    Disarm(gadget, event, NULL, &num_params);
        }
	break;

    case XmENTER_EVENT:
	XdbDebug(__FILE__, gadget, "ToggleButtonGadget enter window\n");
	EnterWindow(gadget, event, NULL, &num_params);
	break;

    case XmLEAVE_EVENT:
	XdbDebug(__FILE__, gadget, "ToggleButtonGadget leave window\n");
	LeaveWindow(gadget, event, NULL, &num_params);
	break;

    case XmMOTION_EVENT:
	XdbDebug(__FILE__, gadget, "ToggleButtonGadget motion event\n");
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

    default:
	_XmWarning(gadget, "ToggleButtonGadget got unknown event\n");
    }
}

Widget
XmCreateToggleButtonGadget(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmToggleButtonGadgetClass,
			  parent,
			  arglist,
			  argcount);
}

Boolean
XmToggleButtonGadgetGetState(Widget widget)
{
    if (XmIsToggleButtonGadget(widget))
	return TBG_Set(widget);
    else if (XmIsToggleButton(widget))
	return XmToggleButtonGetState(widget);
    return False;
}

void
XmToggleButtonGadgetSetState(Widget widget, 
			     Boolean state,
			     Boolean notify)
{
    XmToggleButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(widget)->core_class.expose;

    if (XtIsWidget(widget)) {
	XmToggleButtonSetState(widget, state, notify);
	return;
    }
    if (!XmIsToggleButtonGadget(widget))
	return;
    if ((TBG_Set(widget) && !state) || (!TBG_Set(widget) && state)) {
	TBG_VisualSet(widget) = TBG_Set(widget) = state ? True : False;
	if (notify && TBG_ValueChangedCallback(widget)) {
	    cbs.reason = XmCR_VALUE_CHANGED;
	    cbs.event = NULL;
	    cbs.set = state;

	    XFlush(XtDisplay(widget));

	    XtCallCallbackList(widget,
			       TBG_ValueChangedCallback(widget),
			       (XtPointer)&cbs);
	}
	if (XtIsRealized(widget))
	    (*exp)(widget, NULL, (Region)NULL);
    }
}
