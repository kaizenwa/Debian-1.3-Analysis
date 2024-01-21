/**
 *
 * $Id: PushBG.c,v 1.11 1997/01/11 02:19:51 miers Exp $
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

static char rcsid[] = "$Id: PushBG.c,v 1.11 1997/01/11 02:19:51 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CacheP.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShell.h>
#include <Xm/MenuUtilP.h>
#include <Xm/PushBGP.h>
#include <Xm/RowColumnP.h>
#include <X11/Xfuncs.h>
#include <X11/ShellP.h>

#include <XmI/DebugUtil.h>

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

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
static void Activate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonUp(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params);

static void secondary_object_create(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_prehook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_posthook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args);
static Cardinal get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data);

int _XmPushBCacheCompare(XtPointer A, XtPointer B);

/*
 * resources
 */
#define Offset(field) XtOffsetOf(XmPushButtonGCacheObjRec, pushbutton_cache.field)
static XtResource cache_resources[] = {
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNdefaultButtonShadowThickness, XmCDefaultButtonShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNfillOnArm, XmCFillOnArm, XmRBoolean,
	sizeof(Boolean), Offset(fill_on_arm),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNarmColor, XmCArmColor, XmRPixel,
	sizeof(Pixel), Offset(arm_color),
        XmRCallProc, (XtPointer)_XmSelectColorDefault
    },
    {
	XmNarmPixmap, XmCArmPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(arm_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    }
};

static XmSyntheticResource cache_syn_resources[] = {
    {
	XmNdefaultButtonShadowThickness,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

XmPushButtonGCacheObjClassRec xmPushButtonGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGCacheObjClassRec,
        /* class_name            */ "XmPushButtonGCacheObjClass",
	/* widget_size           */ sizeof(XmPushButtonGCacheObjRec),
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
    /* PushButtonGCacheObj part */
    {
	/* foo                */ 0
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmPushButtonGadgetRec, pushbutton.field)

/* Resources for the pushbutton class */
static XtResource resources[] = {
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
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPushButtonGadgetRec, gadget.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmPushButtonGadgetRec, gadget.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPushButtonGadgetRec,gadget.highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNshowAsDefault, XmCShowAsDefault, XmRBooleanDimension,
	sizeof(Dimension), Offset(show_as_default),
	XmRImmediate, 0
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNshowAsDefault,
	sizeof(Dimension), Offset(show_as_default),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNhighlightThickness,
	sizeof(Dimension), XtOffsetOf(XmPushButtonGadgetRec, gadget.highlight_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
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
    _XmPushBCacheCompare
};

static XmBaseClassExtRec _XmPushBGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmPushButtonGCacheObjClassRec,
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

static XmGadgetClassExtRec _XmPushBGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ XmInheritBaselineProc,
    /* display_rect_proc         */ XmInheritDisplayRectProc,
};

XmPushButtonGadgetClassRec xmPushButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGadgetClassRec,
	/* class_name            */ "XmPushButtonGadget",
	/* widget_size           */ sizeof(XmPushButtonGadgetRec),
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
	/* resize                */ XtInheritResize,
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
	/* extension             */ (XtPointer)&_XmPushBGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, 
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* fix me */
	/* arm_and_activate   */ ArmAndActivate,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL, /* fix me */
	/* syn_resources      */ syn_resources,
	/* num_syn_resources  */ XtNumber(syn_resources),
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmPushBGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
        /* setOverrideCallback */ NULL, /* fix me */
        /* menuProcs           */ XmInheritMenuProc,
	/* extension           */ NULL
    },
    /* XmPushButtonGadget part */
    {
	/* extension          */ NULL, 
    },
};

WidgetClass xmPushButtonGadgetClass = (WidgetClass)&xmPushButtonGadgetClassRec;

/* 
 * Some #defines to make the code below more readable
 */
#define IN_MENU(w) (LabG_MenuType(w) == XmMENU_POPUP || \
                    LabG_MenuType(w) == XmMENU_PULLDOWN)

/******************************* GADGET PART *********************************/
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

    XdbDebug(__FILE__, new_w, "PushBGCacheRec %s being initialized.\n", XtName(new_w));

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

    PBG_Cache(new_w) = &(((XmPushButtonGCacheObject)nsec)->pushbutton_cache);
    PBG_Cache(request) = &(((XmPushButtonGCacheObject)rsec)->pushbutton_cache);
}

int
_XmPushBCacheCompare(XtPointer A, XtPointer B)
{
    return !bcmp(((XmPushButtonGCacheObjPart *)A),
		 ((XmPushButtonGCacheObjPart *)B),
		 sizeof(XmPushButtonGCacheObjPart));
}

/******************************* GADGET PART *********************************/
static void
class_initialize()
{
    XtResourceList combined, labels;
    int ncom;
    Cardinal nlabels;

    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(PBG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(PBG_ClassCachePart(NULL));
    ClassCacheHead(PBG_ClassCachePart(NULL)).next =
	&ClassCacheHead(PBG_ClassCachePart(NULL));

    _XmPushBGRectClassExtRec.record_type = XmQmotif;

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

    xmPushButtonGCacheObjClassRec.object_class.resources = combined;
    xmPushButtonGCacheObjClassRec.object_class.num_resources = ncom;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmPUSH_BUTTON_GADGET_BIT);
}

static void
CreateFillGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = PBG_ArmColor(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillSolid;

    PBG_FillGC(w) = XtGetGC(w, mask, &values);
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

    PBG_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "PushBG InitializePrehook\n");
}

static void
initialize_posthook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XmWidgetExtData ext;

    XdbDebug(__FILE__, new_w, "PushBG InitializePosthook\n");

    /* don't let the null fool you */
    LabG_Cache(new_w) = _XmCachePart(LabG_ClassCachePart(NULL),
                                   (XtPointer)LabG_Cache(new_w),
                                   sizeof(XmLabelGCacheObjPart));
    PBG_Cache(new_w) = _XmCachePart(PBG_ClassCachePart(NULL),
                                  (XtPointer)PBG_Cache(new_w),
                                  sizeof(XmPushButtonGCacheObjPart));

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
    Dimension margin, margin_extra;

    CreateFillGC(new_w);
    CreateBackgroundGC(new_w);

    if (!XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
        _XmError(new_w, "parent should be manager.");

    if (!LabG_Font(new_w))
	LabG_Font(new_w) = _XmGetDefaultFontList(new_w,
					       XmBUTTON_FONTLIST);

    PBG_Armed(new_w) = False;

    if (LabG_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	PBG_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP) {

	LabG_Pixmap(new_w) = PBG_ArmPixmap(new_w);

	if (XtWidth(request) == 0)
	    XtWidth(new_w) = 0;
	if (XtHeight(request) == 0)
	    XtHeight(new_w) = 0;

	_XmCalcLabelGDimensions(new_w);

	(*xmLabelGadgetClassRec.rect_class.resize)(new_w);
    }

    PBG_UnarmPixmap(new_w) = LabG_Pixmap(new_w);

    if (IN_MENU(new_w))
    {
	LabG_Highlight(new_w) = 0;
/* FIX ME - I just added the line below because I think it belongs here;
 * not sure if it's right though. Danny 18/5/1996 */
	G_TraversalOn(new_w) = True;
    }
    else {
	/* take care of the default button shadow stuff */
	/*
	 * This new code adjusts button size in two cases : when ShowAsDefault
	 * is non-zero, and when DefaultButtonShadow is non-zero.
	 */
	/*
	 * This really new code finally figures out what that damned
	 * compatible flag does.  Back in the days of 1.1 and earlier,
	 * DefaultButtonShadow didn't exist.  Lesstif has come full
	 * circle, in the same way and for the same reason I suspect
	 * Motif did -- to avoid unnecessary geometry negotiation.
	 * What our original code did caused geometry negotiation to happen
	 * in the set_values method when we changed ShowAsDefault -- even
	 * though we weren't actually changing the geometry of the widget...
	 * See the tail end of the set_values method for changing ShowAsDefault
	 * MLM
	 */

	if (PBG_DefaultButtonShadow(new_w) > 0)
	    PBG_Compatible(new_w) = False;
	else
	    PBG_Compatible(new_w) = True;

	if (PBG_Compatible(new_w))
	    PBG_DefaultButtonShadow(new_w) = PBG_ShowAsDefault(new_w);

        if (PBG_DefaultButtonShadow(new_w)) {
            margin = 2 * PBG_DefaultButtonShadow(new_w) + LabG_Shadow(new_w);
	    margin_extra = Xm3D_ENHANCE_PIXEL;

            LabG_MarginLeft(new_w) = margin + margin_extra;
	    LabG_MarginRight(new_w) = margin + margin_extra;
	    LabG_MarginTop(new_w) = margin + margin_extra;
	    LabG_MarginBottom(new_w) = margin + margin_extra;

	    XtWidth(new_w) += (margin + margin_extra) * 2;
	    XtHeight(new_w) += 2 * margin + margin_extra;

	    (*xmLabelGadgetClassRec.rect_class.resize)(new_w);
	}
    }

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT | XmENTER_EVENT |
		       XmLEAVE_EVENT | XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT |
		       XmMULTI_ARM_EVENT | XmMULTI_ACTIVATE_EVENT |
		       XmHELP_EVENT;
}

static void
destroy(Widget w)
{
    _XmCacheDelete(PBG_Cache(w));
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
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    bcopy(LabG_Cache(new_w),
          &((XmLabelGCacheObject)nsec)->label_cache,
          sizeof(XmLabelGCacheObjPart));
    bcopy(PBG_Cache(new_w),
          &((XmPushButtonGCacheObject)nsec)->pushbutton_cache,
          sizeof(XmPushButtonGCacheObjPart));

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

    LabG_Cache(new_w) = &(((XmPushButtonGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmPushButtonGCacheObject)rsec)->label_cache);
    PBG_Cache(new_w) = &(((XmPushButtonGCacheObject)nsec)->pushbutton_cache);
    PBG_Cache(request) = &(((XmPushButtonGCacheObject)rsec)->pushbutton_cache);

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

    /* Gaaaa.  Forgot the CachePart copy!!! */
    if (!_XmLabelCacheCompare(LabG_Cache(new_w), LabG_Cache(old))) {
	_XmCacheDelete((XtPointer)LabG_Cache(old));

	LabG_Cache(new_w) = _XmCachePart(LabG_ClassCachePart(NULL),
				       (XtPointer)LabG_Cache(new_w),
				       sizeof(XmLabelGCacheObjPart));
    }
    else
	LabG_Cache(new_w) = LabG_Cache(old);

    if (!_XmPushBCacheCompare(PBG_Cache(new_w), PBG_Cache(old))) {

	_XmCacheDelete((XtPointer)PBG_Cache(old));

	PBG_Cache(new_w) = _XmCachePart(PBG_ClassCachePart(NULL),
				      (XtPointer)PBG_Cache(new_w),
				      sizeof(XmPushButtonGCacheObjPart));
    }
    else
	PBG_Cache(new_w) = PBG_Cache(old);

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
    Dimension margin, margin_extra;

    XdbDebug(__FILE__, new_w, "SetValues\n");

    if (PBG_ArmColor(new_w) != PBG_ArmColor(old)) {
	XtReleaseGC(new_w, PBG_FillGC(new_w));
	CreateFillGC(new_w);
	refresh_needed = True;
    }
    if (XmParentBackground(new_w) != XmParentBackground(old)) {
	XtReleaseGC(new_w, PBG_BackgroundGC(new_w));
	CreateBackgroundGC(new_w);
	refresh_needed = True;
    }

    if (!IN_MENU(new_w)) {
	if (PBG_DefaultButtonShadow(new_w) != PBG_DefaultButtonShadow(old))
	    PBG_Compatible(new_w) = False;

	if (PBG_Compatible(new_w))
	    PBG_DefaultButtonShadow(new_w) = PBG_ShowAsDefault(new_w);

	if (PBG_DefaultButtonShadow(new_w) != PBG_DefaultButtonShadow(old))
	{
	    margin = 2 * PBG_DefaultButtonShadow(new_w) + LabG_Shadow(new_w);
	    margin_extra = Xm3D_ENHANCE_PIXEL;

	    LabG_MarginLeft(new_w) = margin + margin_extra;
	    LabG_MarginRight(new_w) = margin + margin_extra;
	    LabG_MarginTop(new_w) = margin + margin_extra;
	    LabG_MarginBottom(new_w) = margin + margin_extra;

	    XtWidth(new_w) += 2 * (margin + margin_extra);
	    XtHeight(new_w) += 2 * margin + margin_extra;

	    (*xmLabelGadgetClassRec.rect_class.resize)(new_w);

 	    refresh_needed = True;
	}
    }

    if (PBG_ArmPixmap(new_w) != PBG_ArmPixmap(old) &&
        LabG_IsPixmap(new_w) && PBG_Armed(new_w)) {
        refresh_needed = True;
    }

    if (LabG_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	PBG_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP) {

	LabG_Pixmap(new_w) = PBG_ArmPixmap(new_w);

	if (LabG_RecomputeSize(new_w) && XtWidth(new_w) == XtWidth(old))
	    XtWidth(new_w) = 0;
	if (LabG_RecomputeSize(new_w) && XtHeight(new_w) == XtHeight(old))
	    XtHeight(new_w) = 0;

	_XmCalcLabelGDimensions(new_w);

	(*xmLabelGadgetClassRec.rect_class.resize)(new_w);
    }

    if (LabG_Pixmap(new_w) != LabG_Pixmap(old)) {
	PBG_UnarmPixmap(new_w) = LabG_Pixmap(new_w);
	if (LabG_IsPixmap(new_w) && !PBG_Armed(new_w))
	    refresh_needed = True;
    }

    if (LabG_IsPixmap(new_w) && PBG_Armed(new_w) &&
        PBG_ArmPixmap(new_w) != PBG_ArmPixmap(old)) {
        if (LabG_RecomputeSize(new_w) && XtWidth(new_w) == XtWidth(old))
            XtWidth(new_w) = 0;
        if (LabG_RecomputeSize(new_w) && XtHeight(new_w) == XtHeight(old))
            XtHeight(new_w) = 0;
        _XmCalcLabelGDimensions(new_w);
        (*xmLabelGadgetClassRec.rect_class.resize)(new_w);
        refresh_needed = True;
    }

    if (PBG_FillOnArm(new_w) != PBG_FillOnArm(old) && PBG_Armed(new_w))
        refresh_needed = True;

    if (XtIsRealized(new_w) && !refresh_needed)
    {
	Position normal_shadow_x, normal_shadow_y;
	Dimension normal_shadow_width, normal_shadow_height, shad;

	normal_shadow_x = LabG_Highlight(new_w) + LabG_MarginLeft(new_w);
	normal_shadow_y = LabG_Highlight(new_w) + LabG_MarginTop(new_w);
    
	normal_shadow_width = XtWidth(new_w) - 2 * LabG_Highlight(new_w) -
				LabG_MarginLeft(new_w) - LabG_MarginRight(new_w);
	normal_shadow_height = XtHeight(new_w) - 2 * LabG_Highlight(new_w) -
				LabG_MarginTop(new_w) - LabG_MarginBottom(new_w);

	shad = PBG_DefaultButtonShadow(new_w);

	if (PBG_ShowAsDefault(new_w) && !PBG_ShowAsDefault(old))
	{
  	    _XmDrawShadows(XtDisplay(new_w), XtWindow(new_w),
  			   XmParentTopShadowGC(new_w),
  			   XmParentBottomShadowGC(new_w),
 			   XtX(new_w) + normal_shadow_x -
				 (2 * shad + LabG_Shadow(new_w)),
 			   XtY(new_w) + normal_shadow_y -
				 (2 * shad + LabG_Shadow(new_w)),
 			   normal_shadow_width + 2 *
				 (2 * shad + LabG_Shadow(new_w)),
			   normal_shadow_height + 2 *
				 (2 * shad + LabG_Shadow(new_w)),
 			   shad,
			   XmSHADOW_IN);
	}
	else if (!PBG_ShowAsDefault(new_w) && PBG_ShowAsDefault(old))
	{
  	    _XmClearBorder(XtDisplay(new_w), XtWindow(new_w),
 			   XtX(new_w) + normal_shadow_x -
				 (2 * shad + LabG_Shadow(new_w)),
 			   XtY(new_w) + normal_shadow_y -
				 (2 * shad + LabG_Shadow(new_w)),
 			   normal_shadow_width + 2 *
				 (2 * shad + LabG_Shadow(new_w)),
			   normal_shadow_height + 2 *
				 (2 * shad + LabG_Shadow(new_w)),
 			   shad);
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
    bcopy(PBG_Cache(new_w),
          &((XmPushButtonGCacheObject)nsec)->pushbutton_cache,
          sizeof(XmPushButtonGCacheObjPart));

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
    XdbDebug(__FILE__, w, "GetValuesHook\n");
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
     /*
      * these make it easier to deal with the default button stuff
      * Nah.  All drawing is easier:)
      */
    Position normal_shadow_x, normal_shadow_y;
    Dimension normal_shadow_width, normal_shadow_height;
    XmGadgetClass gwc = (XmGadgetClass)XtClass(w);

    XdbDebug(__FILE__, w, "PBG expose: XY %d %d\n", XtX(w), XtY(w));

    if (PBG_DefaultButtonShadow(w) || PBG_ShowAsDefault(w)) {
	normal_shadow_x = LabG_Highlight(w) + LabG_MarginLeft(w);
	normal_shadow_y = LabG_Highlight(w) + LabG_MarginTop(w);
    
	normal_shadow_width = XtWidth(w) - 2 * LabG_Highlight(w) -
				LabG_MarginLeft(w) - LabG_MarginRight(w);
	normal_shadow_height = XtHeight(w) - 2 * LabG_Highlight(w) -
				LabG_MarginTop(w) - LabG_MarginBottom(w);
    }
    else {
	normal_shadow_x = LabG_Highlight(w);
	normal_shadow_y = LabG_Highlight(w);
    
	normal_shadow_width = XtWidth(w) - 2 * LabG_Highlight(w);
	normal_shadow_height = XtHeight(w) - 2 * LabG_Highlight(w);
    }

    if (!IN_MENU(w))
    {
	if (!PBG_Armed(w))
	    XFillRectangle(XtDisplay(w), 
			   XtWindow(w),
			   PBG_BackgroundGC(w),
			   XtX(w), XtY(w), XtWidth(w), XtHeight(w));

	if (PBG_Armed(w) && PBG_FillOnArm(w) && !LabG_IsPixmap(w))
	{
	    XFillRectangle(XtDisplay(w), 
			   XtWindow(w),
			   PBG_FillGC(w),
			   XtX(w) + normal_shadow_x + LabG_Shadow(w),
			   XtY(w) + normal_shadow_y + LabG_Shadow(w),
			   normal_shadow_width - 2 * LabG_Shadow(w),
			   normal_shadow_height - 2 * LabG_Shadow(w));
	}

	if (LabG_IsPixmap(w)) {
	    if (PBG_Armed(w) && PBG_ArmPixmap(w) != XmUNSPECIFIED_PIXMAP)
		LabG_Pixmap(w) = PBG_ArmPixmap(w);
	    else
		LabG_Pixmap(w) = PBG_UnarmPixmap(w);
	}
#define superclass (&xmLabelGadgetClassRec)
	(*superclass->rect_class.expose)(w, event, region);
#undef superclass

	/* now draw the normal shadow */
	_XmDrawShadows(XtDisplayOfObject(w), 
		       XtWindowOfObject(w), 
		       XmParentTopShadowGC(w),
		       XmParentBottomShadowGC(w),
		       XtX(w) + normal_shadow_x,
		       XtY(w) + normal_shadow_y,
		       normal_shadow_width,
		       normal_shadow_height,
		       LabG_Shadow(w),
		       PBG_Armed(w) ? XmSHADOW_IN : XmSHADOW_OUT);

	/*
	 * If PBG_ShowAsDefault is 0 (= False), then no shadow is drawn !!
	 */
 	if (PBG_ShowAsDefault(w) > 0)
	{
 	    Dimension shad;
	    
 	    shad = PBG_DefaultButtonShadow(w);
	    
  	    _XmDrawShadows(XtDisplayOfObject(w), XtWindowOfObject(w),
  			   XmParentTopShadowGC(w),
  			   XmParentBottomShadowGC(w),
 			   XtX(w) + normal_shadow_x -
				 (2 * shad + LabG_Shadow(w)),
 			   XtY(w) + normal_shadow_y -
				 (2 * shad + LabG_Shadow(w)),
 			   normal_shadow_width + 2 *
				 (2 * shad + LabG_Shadow(w)),
			   normal_shadow_height + 2 *
				 (2 * shad + LabG_Shadow(w)),
 			   shad,
			   XmSHADOW_IN);
	}

	if (G_Highlighted(w))
	    (*gwc->gadget_class.border_highlight)(w);
	else
	    (*gwc->gadget_class.border_unhighlight)(w);
    }
    else {
	if (LabG_IsPixmap(w)) {
	    if (PBG_Armed(w) && PBG_ArmPixmap(w) != XmUNSPECIFIED_PIXMAP)
		LabG_Pixmap(w) = PBG_ArmPixmap(w);
	    else
		LabG_Pixmap(w) = PBG_UnarmPixmap(w);
	}
#define superclass (&xmLabelGadgetClassRec)
	(*superclass->rect_class.expose)(w, event, region);
#undef superclass

	_XmDrawShadows(XtDisplayOfObject(w), 
		       XtWindowOfObject(w), 
		       XmParentTopShadowGC(w),
		       XmParentBottomShadowGC(w),
		       XtX(w),  /* G_Highlight should be 0 in a menu */
		       XtY(w),
		       XtWidth(w),
		       XtHeight(w),
		       LabG_Shadow(w),
		       PBG_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
    }
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIXME */

    return _XmSecondaryResourceData(&_XmPushBGRectClassExtRec,
                                    data, NULL, NULL, NULL, NULL);
}

static void
Arm(Widget w,
    XEvent *event,
    String *params,
    Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    PBG_Armed(w) = True;

    (*exp)(w, event, NULL);

    if (PBG_ArmCallback(w)) {
	cbs.reason = XmCR_ARM;
	cbs.event = event;
	cbs.click_count = PBG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PBG_ArmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void 
Disarm(Widget w,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    if (PBG_Armed(w)) {
	PBG_Armed(w) = False;
	(*exp)(w, event, NULL);
    }

    if (PBG_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PBG_ClickCount(w);
 
	XFlush(XtDisplay(w));
	XtCallCallbackList(w,
			   PBG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void 
Activate(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "Activate\n");

    if (PBG_Armed(w) == False)
	return;

    PBG_Armed(w) = False;

    (*exp)(w, event, NULL);

    /*
     * Having this test breaks accelerators. Danny
     * MLM: Not having means PB's break if the button is released outside
     * the widget after it is armed.
     *
     * Test refined so it doesn't fail for accelerators. -- Danny
     */
    if (ev->type == KeyPress || ev->type == KeyRelease
	|| ((ev->x > XtX(w) && ev->x < XtX(w) + XtWidth(w))
	&& (ev->y > XtY(w) && ev->y < XtY(w) + XtHeight(w))))
    {

	if (!LabG_SkipCallback(w) && PBG_ActivateCallback(w)) {
	    cbs.reason = XmCR_ACTIVATE;
	    cbs.event = event;
	    cbs.click_count = PBG_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PBG_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }

    Disarm(w,event,params,num_params);
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    XmPushButtonCallbackStruct cbs;
    Widget w = (Widget)data;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "Activate\n");

    if (PBG_Armed(w) == False)
	return;

    PBG_Armed(w) = False;

    (*exp)(w, NULL, NULL);

    if (!LabG_SkipCallback(w) && PBG_ActivateCallback(w)) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = NULL;
	cbs.click_count = PBG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PBG_ActivateCallback(w),
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
    XdbDebug(__FILE__, w, "ArmAndActivate\n");

    Arm(w, event, params, num_params);

    PBG_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				   ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
}

static void 
EnterWindow(Widget w, 
	    XEvent *event, 
	    String *params, 
	    Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "EnterWindow()\n");

    if (!IN_MENU(w))
    {
	_XmEnterGadget(w, event, NULL, NULL);
	if (PBG_Armed(w))
	{
	    (*exp)(w, event, NULL);
	}
    }
    else /* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    ShellWidget popupShell;

	    popupShell = (ShellWidget)_XmGetRC_PopupPosted(XtParent(w));
	    if  (popupShell)
	    {
		if (popupShell->shell.popped_up)
		    (*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN, 
								   (Widget)popupShell, 
								   NULL, 
								   event, 
								   NULL);
	    }

	    PBG_Armed(w) = True; 

	    (*exp)(w, event, NULL);

	    MGR_SelectedGadget(XtParent(w)) = (XmGadget)w;

	    if (PBG_ArmCallback(w)) {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.click_count = PBG_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PBG_ArmCallback(w),
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
    XmPushButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;
 
    if (!IN_MENU(w))
    {
	_XmLeaveGadget(w, event, NULL, NULL);
	if (PBG_Armed(w))
	{
	    (*exp)(w, event, NULL);
	}
    }
    else /* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    PBG_Armed(w) = False;

	    (*exp)(w, event, NULL);

	    MGR_SelectedGadget(XtParent(w)) = NULL;

	    if (PBG_DisarmCallback(w)) {
		cbs.reason = XmCR_DISARM;
		cbs.event = event;
		cbs.click_count = PBG_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PBG_DisarmCallback(w),
				   (XtPointer)&cbs);
	    }
	}
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
    XmPushButtonCallbackStruct cbs;

    XdbDebug(__FILE__, w, "ButtonUp()\n");

    /* queue events until the next button event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    parent = XtParent(w);

    shell = parent;
    while (!XtIsShell(shell))
	shell = XtParent(shell);

    if (event && (event->type == ButtonRelease))
      (*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_BUTTON, parent,
						     NULL, event, &validButton);

    if (!validButton)
      return;

    if (!PBG_Armed(w))
	return;

    PBG_Armed(w) = False;

    if (IN_MENU(w) && !XmIsMenuShell(shell))
	(*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_POPDOWN, w,
						       NULL, event, &poppedUp);
    else
	(*xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_BUTTON_POPDOWN, w,
						       NULL, event, &poppedUp);

    _XmRecordEvent(event);
    
    if (poppedUp)
	return;

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   XmParentTopShadowGC(w),
		   XmParentBottomShadowGC(w),
		   LabG_Highlight(w),
		   LabG_Highlight(w),
		   XtWidth(w) - 2 * LabG_Highlight(w),
		   XtHeight(w) - 2 * LabG_Highlight(w),
		   LabG_Shadow(w),
		   XmNO_LINE);

    if (!LabG_SkipCallback(w) && PBG_ActivateCallback(w)) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = PBG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PBG_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
    if (PBG_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PBG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PBG_DisarmCallback(w),
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
    /* modified from the MegaButton widget */
    ShellWidget popupShell;
    int validButton;
    XmPushButtonCallbackStruct cbs;
    
    /* queue events until the next button event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);
    
    if (event && (event->type == ButtonPress))
    {
	(* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_BUTTON, 
							 XtParent(w), 
							 NULL, 
							 event, 
							 &validButton);
	if (!validButton)
	    return;
    }

    _XmSetInDragMode(w, True);

    MGR_SelectedGadget(XtParent(w)) = (XmGadget)w;

    popupShell = (ShellWidget)_XmGetRC_PopupPosted(XtParent(w));
    if  (popupShell)
    {
	if (popupShell->shell.popped_up)
	    (* xmLabelGadgetClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN, 
							    (Widget)popupShell, 
							    NULL, 
							    event, 
							    NULL);
    }

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   XmParentTopShadowGC(w),
		   XmParentBottomShadowGC(w),
		   LabG_Highlight(w),
		   LabG_Highlight(w),
		   XtWidth(w) - 2 * LabG_Highlight(w),
		   XtHeight(w) - 2 * LabG_Highlight(w),
		   LabG_Shadow(w),
		   XmSHADOW_OUT);

    if (!PBG_Armed(w)) {
	PBG_Armed(w) = True;
	if (PBG_ArmCallback(w)) {
	    cbs.reason = XmCR_DISARM;
	    cbs.event = event;
	    cbs.click_count = PBG_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PBG_DisarmCallback(w),
			       (XtPointer)&cbs);
	}
    }

    _XmRecordEvent(event);
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
	XdbDebug(__FILE__, gadget, "got arm event\n");
	if (IN_MENU(gadget))
	    ButtonDown(gadget, event, NULL, &num_params);
	else
	    Arm(gadget, event, NULL, &num_params);
	break;

    case XmMULTI_ARM_EVENT:
	if (PBG_MultiClick(gadget) == XmMULTICLICK_KEEP) {
	    if (IN_MENU(gadget))
		ButtonDown(gadget, event, NULL, &num_params);
	    else
		Arm(gadget, event, NULL, &num_params);
	}
	break;

    case XmACTIVATE_EVENT:
	XdbDebug(__FILE__, gadget, "got activate event\n");
	/*
	 * The test below is expanded so it also checks for the Shell.
	 * In tearoff menus, the shell will be a TransientShell, thus we'll
	 * trigger the Activate event.
	 */
	PBG_ClickCount(gadget) = 1;
	if (LabG_MenuType(gadget) == XmMENU_PULLDOWN &&
	    XtIsSubclass(XtParent(XtParent(gadget)), xmMenuShellWidgetClass))
	    ButtonUp(gadget, event, NULL, &num_params);
	else
	    Activate(gadget, event, NULL, &num_params);
	break;

    case XmMULTI_ACTIVATE_EVENT:
	if (PBG_MultiClick(gadget) == XmMULTICLICK_KEEP) {
	    PBG_ClickCount(gadget)++;
	    if (LabG_MenuType(gadget) == XmMENU_PULLDOWN &&
		XtIsSubclass(XtParent(XtParent(gadget)), xmMenuShellWidgetClass))
		ButtonUp(gadget, event, NULL, &num_params);
	    else
		Activate(gadget, event, NULL, &num_params);
	}
	break;

    case XmENTER_EVENT:
	XdbDebug(__FILE__, gadget, "got an enter window\n");
	EnterWindow(gadget, event, NULL, &num_params);
	break;

    case XmLEAVE_EVENT:
	XdbDebug(__FILE__, gadget, "got a leave window\n");
	LeaveWindow(gadget, event, NULL, &num_params);
	break;

    case XmMOTION_EVENT:
	XdbDebug(__FILE__, gadget, "got a motion event\n");
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
	_XmWarning(gadget, "PushButtonGadget got unknown event\n");
    }
}

void
_XmClearBGCompatibility(Widget pbg)
{
}

Widget
XmCreatePushButtonGadget(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmPushButtonGadgetClass,
			  parent,
			  arglist,
			  argcount);
}
