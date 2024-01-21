/**
 *
 * $Id: CascadeBG.c,v 1.11 1997/01/11 02:19:43 miers Exp $
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

static char rcsid[] = "$Id: CascadeBG.c,v 1.11 1997/01/11 02:19:43 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CacheP.h>
#include <Xm/CascadeB.h> /* For XmCascadeButtonHighlight */
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShell.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScreenP.h>
#include <Xm/MenuUtilP.h>
#include <X11/Xfuncs.h>

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

static void secondary_object_create(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_prehook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_posthook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args);
static Cardinal get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data);

extern int _XmCascadeBCacheCompare(XtPointer A, XtPointer B);



/*
 * cache resources
 */
#define Offset(field) XtOffsetOf(XmCascadeButtonGCacheObjRec, cascade_button_cache.field)
static XtResource cache_resources[] = {
    {
	XmNcascadePixmap, XmCPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(cascade_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNmappingDelay, XmCMappingDelay, XmRInt,
	sizeof(int), Offset(map_delay),
	XmRImmediate, (XtPointer)180
    }
};

XmCascadeButtonGCacheObjClassRec xmCascadeButtonGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGCacheObjClassRec,
        /* class_name            */ "XmCascadeButtonGCacheObjClass",
	/* widget_size           */ sizeof(XmCascadeButtonGCacheObjRec),
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
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    },
    /* LabelGCacheObj part */
    {
	/* foo                */ 0
    },
    /* CascadeButtonGCacheObj part */
    {
	/* foo                */ 0
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmCascadeButtonGadgetRec, cascade_button.field)

/* Resources for the cascadebutton class */
static XtResource resources[] = {
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNcascadingCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(cascade_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNsubMenuId, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(submenu),
	XmRMenuWidget, (XtPointer)NULL
    },
    /* resources we override from XmGadget */
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmCascadeButtonGadgetRec, gadget.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmCascadeButtonGadgetRec, gadget.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmCascadeButtonGadgetRec, gadget.highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
};

static XmBaseClassExtRec _XmCascadeBGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmCascadeButtonGCacheObjClassRec,
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
    _XmCascadeBCacheCompare
};

static XmGadgetClassExtRec _XmCascadeBGGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ XmInheritBaselineProc,
    /* display_rect_proc         */ XmInheritDisplayRectProc,
};

static void DelayedArm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void CheckDisarm(Widget w, XEvent *event, String *params, Cardinal *num_params);

XmCascadeButtonGadgetClassRec xmCascadeButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGadgetClassRec,
	/* class_name            */ "XmCascadeButtonGadget",
	/* widget_size           */ sizeof(XmCascadeButtonGadgetRec),
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
	/* extension             */ (XtPointer)&_XmCascadeBGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, 
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* fix me */
	/* arm_and_activate   */ NULL, /* fix me */
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL, /* fix me */
	/* syn_resources      */ NULL,
	/* num_syn_resources  */ 0,
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmCascadeBGGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
        /* setOverrideCallback */ NULL, /* fix me */
        /* menuProcs           */ NULL, /* fix me */
	/* extension           */ NULL
    },
    /* XmCascadeButtonGadget part */
    {
	/* extension */ NULL
    },
};

WidgetClass xmCascadeButtonGadgetClass = (WidgetClass)&xmCascadeButtonGadgetClassRec;

/********************************* CACHE PART *******************************/
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

    XdbDebug(__FILE__, new_w, "CascadeButtonGCacheRec %s being initialized.\n",
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

    CBG_Cache(new_w) =
	&(((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache);
    CBG_Cache(request) =
	&(((XmCascadeButtonGCacheObject)rsec)->cascade_button_cache);
}

int
_XmCascadeBCacheCompare(XtPointer A, XtPointer B)
{
    return !bcmp(((XmCascadeButtonGCacheObjPart *)A),
		 ((XmCascadeButtonGCacheObjPart *)B),
		 sizeof(XmCascadeButtonGCacheObjPart));
}

int
_XmArrowPixmapCacheCompare(XtPointer A, XtPointer B)
{
    return 0;
}

void
_XmArrowPixmapCacheDelete(XtPointer data)
{
}

/*
 * Assumes that the existing pixmaps are freed.
 */
void
_XmCreateArrowPixmaps(Widget w)
{
	Pixmap		apm, pm;
	GC		bgc, tgc, fgc;
	Display		*dpy;
	int		depth;
	Dimension	ww, hh, st;
	Screen		*scr;
	Window		win;
	Pixel		tsc, bsc, fc, hc;
	XGCValues	values;
	XtGCMask	mask, dynamic, dontcare;

	tsc = XmParentTopShadowColor(w);	/* top shadow color */
	bsc = XmParentBottomShadowColor(w);	/* bottom shadow color */
	fc = XmParentForeground(w);		/* fill color */
	hc = XmParentHighlightColor(w);		/* highlight color */

	values.foreground = bsc;
	values.background = XmParentBackground(w);
	mask = GCForeground | GCBackground;
	dynamic = GCClipMask | GCClipXOrigin | GCClipYOrigin;
	dontcare = ~(mask | dynamic);
	bgc = XtAllocateGC(w, 0, mask, &values, dynamic, dontcare);

	values.foreground = tsc;
	values.background = XmParentBackground(w);
	mask = GCForeground | GCBackground;
	dynamic = GCClipMask | GCClipXOrigin | GCClipYOrigin;
	dontcare = ~(mask | dynamic);
	tgc = XtAllocateGC(w, 0, mask, &values, dynamic, dontcare);

	values.foreground = values.background = XmParentBackground(w);
	mask = GCForeground | GCBackground;
	dynamic = GCForeground | GCClipMask | GCClipXOrigin | GCClipYOrigin;
	dontcare = ~(mask | dynamic);
	fgc = XtAllocateGC(w, 0, mask, &values, dynamic, dontcare);

	dpy = XtDisplay(w);
	scr = XtScreen(w);
	depth = DefaultDepthOfScreen(scr);

	ww = 2 * XtHeight(w) / 3;
	hh = XtHeight(w) / 2;
	st = 2;

	win = RootWindowOfScreen(scr);

	/* Create pixmaps, fill them up with _XmDrawArrow */
	pm = XCreatePixmap(dpy, win, ww, hh, depth);
	apm = XCreatePixmap(dpy, win, ww, hh, depth);

	XdbDebug(__FILE__, w, "_XmCreateArrowPixmaps -> CascadePixmap 0x%X ArmedPixmap 0x%X\n",
		pm, apm);

	XFillRectangle(dpy, pm, fgc, 0, 0, ww, hh);
	XFillRectangle(dpy, apm, fgc, 0, 0, ww, hh);

	_XmDrawArrow(dpy, pm,
		/* GC's */		bgc, tgc, fgc,
		/* x, y, w, h */	0, 0, ww, hh,
		/* shadowthickness */	st,
		/* direction */		XmARROW_RIGHT);

	_XmDrawArrow(dpy, apm,
		/* GC's */		tgc, bgc, fgc,
		/* x, y, w, h */	0, 0, ww, hh,
		/* shadowthickness */	st,
		/* direction */		XmARROW_RIGHT);

	/* Copy them into the widget/gadget */
	if (XmIsGadget(w)) {
		CBG_ArmedPixmap(w) = apm;
		CBG_CascadePixmap(w) = pm;
	} else {
		CB_ArmedPixmap(w) = apm;
		CB_CascadePixmap(w) = pm;
	}

	/* Caching ?? */
	XtReleaseGC(w, bgc);
	XtReleaseGC(w, fgc);
	XtReleaseGC(w, tgc);
}

/******************************** GADGET PART *******************************/
static void
class_initialize()
{
    XtResourceList combined, labels;
    int ncom;
    Cardinal nlabels;

    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(CBG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(CBG_ClassCachePart(NULL));
    ClassCacheHead(CBG_ClassCachePart(NULL)).next =
	&ClassCacheHead(CBG_ClassCachePart(NULL));

    _XmCascadeBGRectClassExtRec.record_type = XmQmotif;

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

    xmCascadeButtonGCacheObjClassRec.object_class.resources = combined;
    xmCascadeButtonGCacheObjClassRec.object_class.num_resources = ncom;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmCASCADE_BUTTON_GADGET_BIT);
}

static void
initialize_prehook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "CascadeBG InitializePrehook\n");
}

static void
initialize_posthook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XmWidgetExtData ext;

    XdbDebug(__FILE__, new_w, "CascadeBG InitializePosthook\n");

    /* don't let the null fool you */
    LabG_Cache(new_w) = _XmCachePart(LabG_ClassCachePart(NULL),
                                   (XtPointer)LabG_Cache(new_w),
                                   sizeof(XmLabelGCacheObjPart));
    CBG_Cache(new_w) = _XmCachePart(CBG_ClassCachePart(NULL),
                                  (XtPointer)CBG_Cache(new_w),
                                  sizeof(XmCascadeButtonGCacheObjPart));

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
    Widget rc;

    if (! XmIsManager(XtParent(new_w)))
	_XmError(new_w, "parent should be manager.");

    CBG_SetArmed(new_w, False);

    rc = XtParent(new_w);

    if (!XmIsRowColumn(rc))
	_XmError(new_w, "Cascade gadget parent must be a RowColumn.");

    if (RC_Type(rc) != XmMENU_BAR      && RC_Type(rc) != XmMENU_POPUP &&
	RC_Type(rc) != XmMENU_PULLDOWN && RC_Type(rc) != XmMENU_OPTION)
	_XmError(new_w, "Cascade gadget parent is incorrect type.");

    if (RC_Type(rc) != XmMENU_BAR)	/* ??? */
	_XmCreateArrowPixmaps(new_w);

    LabG_Highlight(new_w) = 0;

    CBG_Timer(new_w) = 0;

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT |
			XmENTER_EVENT | XmLEAVE_EVENT |
			XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT |
			XmHELP_EVENT;

    G_TraversalOn(new_w) = True;	/* Danny 18/5/1996 - FIX ME (not sure) */

}

static void
destroy(Widget w)
{
    _XmCacheDelete(CBG_Cache(w));
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
    bcopy(CBG_Cache(new_w),
          &((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache,
          sizeof(XmCascadeButtonGCacheObjPart));

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

    LabG_Cache(new_w) = &(((XmCascadeButtonGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmCascadeButtonGCacheObject)rsec)->label_cache);
    CBG_Cache(new_w) =
	&(((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache);
    CBG_Cache(request) =
	&(((XmCascadeButtonGCacheObject)rsec)->cascade_button_cache);

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

    if (!_XmCascadeBCacheCompare(CBG_Cache(new_w), CBG_Cache(old))) {

	_XmCacheDelete((XtPointer)CBG_Cache(old));

	CBG_Cache(new_w) = _XmCachePart(CBG_ClassCachePart(NULL),
				      (XtPointer)CBG_Cache(new_w),
				      sizeof(XmCascadeButtonGCacheObjPart));
    }
    else
	CBG_Cache(new_w) = CBG_Cache(old);

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

    XdbDebug(__FILE__, new_w, "set_values()\n");

    if (CBG_CascadePixmap(old) != CBG_CascadePixmap(new_w))
    {
        _XmFreeScratchPixmap((XmScreen)XmGetXmScreen(XtScreenOfObject(old)), CBG_CascadePixmap(old));
        _XmFreeScratchPixmap((XmScreen)XmGetXmScreen(XtScreenOfObject(old)), CBG_ArmedPixmap(old));

	_XmCreateArrowPixmaps(new_w);

	refresh_needed = True;
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
    bcopy(CBG_Cache(new_w),
          &((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache,
          sizeof(XmCascadeButtonGCacheObjPart));

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
    XdbDebug(__FILE__, w, "get_values_hook()\n");
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
#define superclass (&xmLabelGadgetClassRec)
    (*superclass->rect_class.expose)(w, event, region);
#undef superclass

    XdbDebug(__FILE__, w, "expose() Armed(%d)\n", CBG_IsArmed(w));

    if (CBG_IsArmed(w) || LabG_MenuType(w) == XmMENU_OPTION)
	XmCascadeButtonHighlight(w, True);
    else
	XmCascadeButtonHighlight(w, False);
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIXME */

    return _XmSecondaryResourceData(&_XmCascadeBGRectClassExtRec,
                                    data, NULL, NULL, NULL, NULL);
}

static void
DoSelect(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    Widget submenu;

    submenu = CBG_Submenu(w);

    XdbDebug(__FILE__, w, "DoSelect()\n");

    /* quene events until the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    /* if we have a submenu attached, call the cascading callback, 
       post the submenu and allow keyboard traversal of it. */

    if (submenu) 
    {
	/* Turn on keyboard traversal */
	_XmSetInDragMode(w, False);

	XdbDebug0(__FILE__, submenu, "RC_CascadeBtn(%s)-> %s\n", 
		XtName(submenu), XtName(w));

	RC_CascadeBtn(submenu) = w;
	RC_PopupPosted(XtParent(w)) = submenu;

	XdbDebug0(__FILE__, w, "RC_PopupPosted(%s) set to %s\n",
		XtName(XtParent(w)),
		submenu ? XtName(submenu) : "(null)");
    }

    CBG_SetArmed(w, False);
}

static void
StartDrag(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    XmCascadeButtonGadget cw = (XmCascadeButtonGadget) w;

    XdbDebug(__FILE__, w, "StartDrag()\n");

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    /* If the submenu is already active, disable keyboard traversal
       and set it to mouse traversal */

    if (CBG_Submenu(cw) == NULL) {
	XdbDebug(__FILE__, w, "StartDrag: no CBG_Submenu\n");
	return;
    }

    if (CBG_Submenu(w) &&
	XmIsMenuShell(XtParent(CBG_Submenu(cw))))
    {
        XmAnyCallbackStruct cbs;
	Position my_x, my_y;

	/* FIX ME: check for identity */
	if (RC_PopupPosted(XtParent(w)) != NULL)
	{
	    /* Unpost the current menu */
	    /* FIX ME: cb might be a gadget */
	    Widget cb = RC_CascadeBtn(RC_PopupPosted(XtParent(w)));

	    XtPopdown(XtParent(RC_PopupPosted(XtParent(w))));

	    CB_SetArmed(cb, False);

	    XmCascadeButtonHighlight(cb, False);
	}

	XtTranslateCoords(w, 0, 0, &my_x, &my_y);

	XtMoveWidget(XtParent(CBG_Submenu(cw)),
                     my_x + XtWidth(w) - LabG_Shadow(cw),
                     my_y);

	cbs.reason=XmCR_CASCADING;
	cbs.event=event;
	XtCallCallbackList(w,
			   CBG_CascadeCall(w),
			   &cbs);


	XtManageChild(CBG_Submenu(cw));

	/* FIX ME */
	XdbDebug(__FILE__, CBG_Submenu(w), "RC_CascadeBtn(%s) -> %s\n",
		XtName(CBG_Submenu(w)), XtName(w));
	RC_CascadeBtn(CBG_Submenu(w)) = w;
	RC_PopupPosted(XtParent(w)) = CBG_Submenu(w);

	RC_LastSelectToplevel(CBG_Submenu(w)) = RC_LastSelectToplevel(XtParent(w));

	XtPopup(XtParent(CBG_Submenu(cw)), XtGrabNonexclusive);
    }
    else
    {
	_XmWarning(w, 
                   "CascadeButton's popup must be a subclass of XmMenuShell\n");
        return;
    }

    CBG_SetArmed(cw, True);

    expose(w, NULL, (Region)NULL);
}

static void 
CascadePopupHandler(XtPointer clientData, XtIntervalId *id)
{
    Widget w = (Widget)clientData;

    CBG_Timer(w) = 0;

    if (CBG_Submenu(w) && XmIsMenuShell(XtParent(CBG_Submenu(w))))
    {
        XmAnyCallbackStruct cbs;
	Position my_x, my_y;
	
	/* make sure the menu is popped down */

	/* FIX ME: This seems to have problems */	
	/* XtUnmapWidget(CBG_Submenu(w));*/
	
	/* position the row column inside the menushell*/
	XtMoveWidget(CBG_Submenu(w),
		     0,0);

	/* now move the menushell */
	XtTranslateCoords(w, 0, 0, &my_x, &my_y);
	XtMoveWidget(XtParent(CBG_Submenu(w)),
                     my_x + XtWidth(w) - LabG_Shadow(w),
                     my_y);

	cbs.reason=XmCR_CASCADING;
	cbs.event=NULL; /* Uhm... maybe this works :) */
	XtCallCallbackList(w,
			   CBG_CascadeCall(w),
			   &cbs);

	RC_CascadeBtn(CBG_Submenu(w)) = w;
	XdbDebug0(__FILE__, CBG_Submenu(w), "RC_CascadeBtn(%s) -> %s\n",
		XtName(CBG_Submenu(w)), XtName(w));

	RC_PopupPosted(XtParent(w)) = CBG_Submenu(w);

	RC_LastSelectToplevel(CBG_Submenu(w)) = RC_LastSelectToplevel(XtParent(w));

	XtPopup(XtParent(CBG_Submenu(w)), XtGrabNonexclusive);

	XtMapWidget(CBG_Submenu(w));
    }
    else if (CBG_Submenu(w))
    {
	_XmWarning(w,
                   "CascadeButton's popup must be a subclass of XmMenuShell\n");
        return;
    }
}


static void 
DelayedArm(Widget w, 
           XEvent *event, 
           String *params, 
           Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "DelayedArm()\n");

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    if (_XmGetInDragMode(w)) 
    {
	CBG_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				       CBG_MapDelay(w),
				       CascadePopupHandler,
				       (XtPointer)w);

	CBG_SetArmed(w, True);

	XmCascadeButtonHighlight(w, True);
	
	MGR_SelectedGadget(XtParent(w)) = (XmGadget)w;
    }
}

static void 
CheckDisarm(Widget w, 
	    XEvent *event, 
	    String *params, 
	    Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "CheckDisarm()\n");

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    if (_XmGetInDragMode(w)) 
    {
	if (CBG_Timer(w))
	{
	    XtRemoveTimeOut(CBG_Timer(w));
	    CBG_Timer(w) = 0;
	}
	
        /* FIX ME: check for edges */
        if (CBG_Submenu(w)) {
	    XtPopdown(XtParent(CBG_Submenu(w)));
	    /* FIX ME */
	}

	CBG_SetArmed(w, False);

	XmCascadeButtonHighlight(w, False);
    }
}

static void
MenuBarSelect(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    Widget	menu, shell, mb;
    Display	*dpy;
    Position my_x, my_y;

    /* first thing we do is free up the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    dpy = XtDisplay(w);

    mb = XtParent(w);
    menu = CBG_Submenu(w);
    shell = menu ? XtParent(menu) : NULL;

    if (!menu)
      return;

    /*
     * If this button is already armed, remove only any cascaded sub-menus and then
     * simply enable drag mode.
     */

    /*
     * Check for torn-off menu
     */
#ifdef RESTORE_TEAR_OFFS
    if (RC_TearOffModel(menu) == XmTEAR_OFF_ENABLED)
    {
        XdbDebug(__FILE__, w, 
		 "MenuBarSelect: restoring tear off menu to menu shell\n");
        _XmRestoreTearOffToMenuShell(menu, event);

        shell = XtParent(menu);
    }
#else
    if (RC_TornOff(menu)) 
    {
        XdbDebug(__FILE__, w, 
		 "MenuBarSelect: menu was torn off; can't cope with that.\n");
        return;
    }
#endif


    /*
     * Check if our menu is already up
     */
    if (((ShellWidget)shell)->shell.popped_up)
    {
        XdbDebug(__FILE__, w, 
		 "MenuBarSelect: Menu is already up (Shell %s, Menu Pane %s)\n",
		 shell ? XtName(shell) : "(null)",
		 menu ? XtName(menu) : "(null)");

	XdbDebug(__FILE__, w, "MenuBarSelect: shell popped up %d, armed CB %d\n",
		 ((ShellWidget)shell)->shell.popped_up, CBG_IsArmed(w));


	/* Popdown any sub menu levels */

	XtCallActionProc(shell, "MenuShellPopdownDone", event, NULL, 0);
    }
    else 
    {
        XmAnyCallbackStruct cbs;
        ShellWidget popupShell;
	Widget popupPosted = _XmGetRC_PopupPosted(XtParent(w));

	/* popup any submenus (besides ours) that are already popped up. */
	if (popupPosted)
        {
	  popupShell = (ShellWidget)XtParent(popupPosted);

	  (*xmLabelClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN, 
						   (Widget)popupShell, 
						   NULL, 
						   event, 
						   NULL);
	  /* This is an educated guess at best, but is seems to solve
	   * the problem seen in xephem: popups refusing to take focus sometimes.
	   * PvH 10/22/96
	   */
	  XtRemoveGrab((Widget)popupShell);
          XtRemoveGrab(mb);
	}

        XdbDebug(__FILE__, w, 
		 "MenuBarSelect : need to pop up a menu (Shell %s, Menu Pane %s)\n",
		 shell ? XtName(shell) : "(null)",
		 menu ? XtName(menu) : "(null)");

	/* grab the keyboard and freeze everything so that we can be sure that
	   the next event goes to the right place. */
	XdbDebug(__FILE__, w, "    Doing _XmGrabKeyboard\n");
	
	_XmGrabKeyboard(mb,
			/* owner_events */	True,
			/* Pointer mode */	GrabModeSync,
			/* Keyboard mode */	GrabModeSync,
			/* Timestamp */		CurrentTime);

	/* get the window that previously had the focus
	   before popping up the menu so we can revert 
	   to it after we're done. */
	_XmMenuFocus(w, XmMENU_FOCUS_SAVE, CurrentTime);
	
	_XmMenuFocus(mb, XmMENU_FOCUS_SET, CurrentTime);

	/* We can unfreeze the keyboard events, since the input focus has been set */
	XdbDebug(__FILE__, w, "    Doing XAllowEvents(AsyncKeyboard)\n");
	XAllowEvents(dpy,
		     AsyncKeyboard,
		     CurrentTime);

	/* Set drag mode */
	_XmSetInDragMode(w, True);

	CBG_SetArmed(w, True);

	/* highlight the cascade button. */
	XmCascadeButtonHighlight(w, True);

	    
	/* Register inside the RC that this cascadebutton is the one that 
	   triggered the menu */
	RC_CascadeBtn(menu) = w;
      XdbDebug0(__FILE__, menu, "RC_CascadeBtn(%s)-> %s\n", XtName(menu), XtName(w));
	
	XtUnmapWidget(menu);
	
	/* position the row column inside the menushell*/
	_XmMoveObject(menu, 0,0);
	
	/* now move the menushell */
	XtTranslateCoords(w, 0, 0, &my_x, &my_y);

	_XmMoveObject(shell, my_x, my_y + XtHeight(w));
	XdbDebug(__FILE__, shell, "_XmMoveObject -> %d %d\n", my_x, my_y + XtHeight(w));
	
	cbs.reason=XmCR_CASCADING;
	cbs.event=event;
	XtCallCallbackList(w, CBG_CascadeCall(w), &cbs);
	
	RC_PopupPosted(mb) = menu;
	XdbDebug0(__FILE__, mb, "RC_PopupMenuPosted(%s) set to %s\n",
		  mb ? XtName(mb) : "(null)",
		  shell ? XtName(shell) : "(null)");
	
	RC_LastSelectToplevel(menu) = menu;
	
	/*
	 * Add the menu bar to the TOP of the modal cascade, as an exclusive grab.
	 * This causes all pointer events which occur outside the menu
	 * cascade to be delivered to the menu bar widget.  It is 
	 * essential to have at least one widget in the cascade own an 
	 * exclusive grab, because otherwise external events would be
	 * suppressed by the Intrinsics.  This would prevent us from
	 * releasing the next pointer event in the server, and hence the
	 * system would lock.
	 */
	XdbDebug(__FILE__, mb, "    Doing XtAddGrab()\n");
	XtAddGrab(mb, True, True);
	
	/*
	 * Popup the menu shell, and append it to the modal cascade.
	 * Must be done after adding grab for menu bar, so that the menu
	 * bar is the first widget in the modal cascade.
	 */
	XdbDebug(__FILE__, menu, "XtMapWidget\n");
	XtMapWidget(menu);
	XdbDebug(__FILE__, shell, "XMapRaised(0x%X) geo %d %d %dx%d\n",	
		XtWindow(shell), XtX(shell), XtY(shell), XtWidth(shell), XtHeight(shell));
	XMapRaised(XtDisplay(shell), XtWindow(shell));

	/*
	 * The XtMapWidget and XMapRaised above don't always seem to work.
	 * The "Other" menu in testXm/cascadebutton/test1 is an example.
	 * Danny 7/8/96.
	 */
	XtManageChild(menu);
	
	XdbDebug(__FILE__, shell, "XtAddGrab()\n");
	XtAddGrab(shell, False, False);
	
	/* in xscope, after the call to MapWindow and before the
	   call to _XmGrabPointer, it seems like all the children
	   of the menu shell are being exposed -- you see all these
	   things like PolyText8 and CopyArea and PolySegment
	   flying around.  The server hasn't generated an Expose
	   event yet, so the client side code must be walking through 
	   the widget tree calling the widgets' expose methods
	   itself -- faking an expose event. */
	/*	_XmFakeExpose(shell);*/
	
	/* Initiate a new pointer grab for the menu bar */
	/* Changing owner_events to False is a bad idea - Danny 12/5 */
	/* Pointer mode will change to Sync anyway with the 
	   XAllowEvents. It's also set here, according to xscope - Chris 6/23 */
	
	XdbDebug(__FILE__, menu, "_XmGrabPointer()\n");

	_XmGrabPointer(mb,
		       /* owner_events */	True,
		       /* Event Mask */	(ButtonPressMask 
					 | ButtonReleaseMask 
					 | EnterWindowMask 
					 | LeaveWindowMask),
		       /* Pointer Mode */	GrabModeSync,
		       /* Keyboard Mode */	GrabModeAsync,
		       /* Confine_to */	None,
		       /* Cursor */		XmGetMenuCursor(dpy),
		       /* Timestamp */		CurrentTime);

	_XmMenuFocus(shell, XmMENU_FOCUS_SET, CurrentTime);

	/* queue events up until the next button event. */
	XAllowEvents(dpy, SyncPointer, CurrentTime);
    }
    
    /* arm the menu bar */
    RC_SetArmed(mb, 1);
}

static void
MenuBarEnter(Widget w, 
	    XEvent *event, 
	    String *params, 
	    Cardinal *num_params)
{
    /* if we're not dragging, do nothing. */
    if (!_XmGetInDragMode(w))
      return;

    /* if we're already armed, do nothing */
    if (CBG_IsArmed(w))
      return;

    MenuBarSelect(w, event, NULL, num_params);
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
	if (LabG_MenuType(gadget) == XmMENU_BAR)
	     MenuBarSelect(gadget, event, NULL, &num_params);
	else
	    StartDrag(gadget, event, NULL, &num_params);
	break;
    case XmACTIVATE_EVENT:
	DoSelect(gadget, event, NULL, &num_params);
	break;
    case XmENTER_EVENT:
	if (LabG_MenuType(gadget) == XmMENU_PULLDOWN)
	    DelayedArm(gadget, event, NULL, &num_params);
	else if (LabG_MenuType(gadget) == XmMENU_BAR)
	    MenuBarEnter(gadget, event, NULL, &num_params);
	break;
    case XmLEAVE_EVENT:
	if (LabG_MenuType(gadget) == XmMENU_PULLDOWN)
	    CheckDisarm(gadget, event, NULL, &num_params);
#if 0
	/* FIX ME */
	else if (LabG_MenuType(gadget) == XmMENU_BAR)
	    MenuBarLeave(gadget, event, NULL, &num_params);
#endif
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
    }
}

void
XmCascadeButtonGadgetHighlight(Widget cb, 
			       Boolean highlight)
{
    XdbDebug(__FILE__, cb, "XmCascadeButtonGadgetHighlight(hl %d, armed %d, apm 0x%X, cpm 0x%X)\n",
	highlight,
	XmIsPrimitive(cb) ? CB_IsArmed(cb) : CBG_IsArmed(cb),
	XmIsPrimitive(cb) ? CB_ArmedPixmap(cb) : CBG_ArmedPixmap(cb),
	XmIsPrimitive(cb) ? CB_CascadePixmap(cb) : CBG_CascadePixmap(cb));

    if (! XtIsRealized(cb))
	return;

    if (XmIsPrimitive(cb))
	XmCascadeButtonHighlight(cb, highlight);
    else if (XmIsGadget(cb)) {
#ifdef	DO_SANITY
      if (CBG_CascadePixmap(cb) == 0) {
	XdbDebug(__FILE__, cb, "CascadePixmap has NULL value\n");
#if 0
	_XmWarning(cb, "CascasdePixmap has NULL value\n");
#endif
      }
      if (CBG_ArmedPixmap(cb) == 0) {
	XdbDebug(__FILE__, cb, "ArmedPixmap has NULL value\n");
#if 0
	_XmWarning(cb, "ArmedPixmap has NULL value\n");
#endif
      }
#endif
      _XmDrawShadows(XtDisplayOfObject(cb),
		     XtWindowOfObject(cb),
		     XmParentTopShadowGC(cb),
		     XmParentBottomShadowGC(cb),
		     XtX(cb),
		     XtY(cb),
		     XtWidth(cb),
		     XtHeight(cb),
		     LabG_Shadow(cb),
		     highlight ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
      
      /* now draw the pixmap */
      if (LabG_MenuType(cb) == XmMENU_PULLDOWN)
	if (CBG_IsArmed(cb)) {
	  if (CBG_ArmedPixmap(cb) != XmUNSPECIFIED_PIXMAP 
	      && CBG_ArmedPixmap(cb) != 0) {
	    XdbDebug(__FILE__, cb, "XCopyArea 0x%X -> 0x%X geo %d %d %dx%d\n",
		     CBG_ArmedPixmap(cb), XtWindow(XtParent(cb)),
		     2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
		     XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4);

	    XCopyArea(XtDisplay(cb), CBG_ArmedPixmap(cb), 
		      XtWindow(cb),
		      LabG_NormalGC(cb),
		      0, 0,
		      2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
		      XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4 + XtY(cb));
	  }
	} 
	else if (CBG_CascadePixmap(cb) != XmUNSPECIFIED_PIXMAP 
		 && CBG_CascadePixmap(cb) != 0) {
	  XdbDebug(__FILE__, cb, "XCopyArea 0x%X -> 0x%X geo %d %d %dx%d\n",
		   CBG_CascadePixmap(cb), XtWindow(XtParent(cb)),
		   2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
		   XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4);

	  XCopyArea(XtDisplay(cb), CBG_CascadePixmap(cb), 
		    XtWindow(XtParent(cb)),
		    LabG_NormalGC(cb),
		    0, 0,
		    2 * XtHeight(cb) / 3, XtHeight(cb) / 2,
		    XtWidth(cb) - 2 * XtHeight(cb) / 3, XtHeight(cb) / 4 + XtY(cb));
	}
    } 
    else
      _XmError(cb, "XmCascadeButtonHighlight called with non-cascade button widget");
}

Widget
XmCreateCascadeButtonGadget(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmCascadeButtonGadgetClass,
			  parent,
			  arglist,
			  argcount);
}
