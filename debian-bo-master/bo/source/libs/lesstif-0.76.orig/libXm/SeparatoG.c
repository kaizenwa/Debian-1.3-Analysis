/**
 *
 * $Id: SeparatoG.c,v 1.4 1996/11/28 09:21:56 u27113 Exp $
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

static char rcsid[] = "$Id: SeparatoG.c,v 1.4 1996/11/28 09:21:56 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/CacheP.h>
#include <X11/Xfuncs.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);

static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);

static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args);

static void secondary_object_create(Widget request, Widget new_w, ArgList args, Cardinal *num_args);

static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_prehook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_posthook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args);
static Cardinal get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data);

/*
 * cache resources
 */
#define Offset(field) XtOffsetOf(XmSeparatorGCacheObjRec, separator_cache.field)
static XtResource cache_resources[] = {
    {
	XmNseparatorType, XmCSeparatorType, XmRSeparatorType,
	sizeof(unsigned char), Offset(separator_type),
	XmRImmediate, (XtPointer)XmSHADOW_ETCHED_IN
    },
    {
	XmNmargin, XmCMargin, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmHORIZONTAL
    }
};

static XmSyntheticResource cache_syn_resources[] = {
    {
	XmNmargin,
	sizeof(Dimension), Offset(margin),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

XmSeparatorGCacheObjClassRec xmSeparatorGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmExtClassRec,
        /* class_name            */ "XmSeparatorGadget",
	/* widget_size           */ sizeof(XmSeparatorGCacheObjRec),
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
    /* SeparatorGCache part */
    {
	/* foo                */ 0
    }
};

#undef GOffset
#define GOffset(field) XtOffsetOf(XmSeparatorGadgetRec, gadget.field)
static XtResource resources[] = {
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), GOffset(traversal_on),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), GOffset(highlight_thickness),
	XmRImmediate, (XtPointer)0
    },
};

static XmBaseClassExtRec _XmSeparatoGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmSeparatorGCacheObjClassRec,
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
	/* next		*/ NULL,
	/* prev		*/ NULL,
	/* ref_count	*/ 0
    },
    _XmCacheCopy,
    _XmCacheDelete,
    _XmSeparatorCacheCompare
};

static XmGadgetClassExtRec SeparatorGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ NULL, /* FIXME */
    /* display_rect_proc         */ NULL, /* FIXME */
};

XmSeparatorGadgetClassRec xmSeparatorGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmGadgetClassRec,
	/* class_name            */ "XmSeparatorGadget",
	/* widget_size           */ sizeof(XmSeparatorGadgetRec),
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
	/* get_values_hook       */ get_values_hook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmSeparatoGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, 
	/* border_unhighlight */ XmInheritBorderUnhighlight,
	/* arm_and_activate   */ NULL,
	/* input_dispatch     */ NULL,
	/* visual_change      */ NULL,
	/* syn_resources      */ NULL,
	/* num_syn_resources  */ 0,
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&SeparatorGadgetClassExtRec,
    },
    /* XmSeparatorGadget part */
    {
	/* extension */ NULL
    },
};

WidgetClass xmSeparatorGadgetClass = (WidgetClass)&xmSeparatorGadgetClassRec;


/***************************** CACHE PART ********************************/

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

    XdbDebug(__FILE__, new_w, "SeparatorGCacheRec %s being initialized.\n",
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

    SEPG_Cache(new_w) = &(((XmSeparatorGCacheObject)nsec)->separator_cache);
    SEPG_Cache(request) = &(((XmSeparatorGCacheObject)rsec)->separator_cache);
}

int
_XmSeparatorCacheCompare(XtPointer A, XtPointer B)
{
    return !bcmp(((XmSeparatorGCacheObjPart *)A),
		 ((XmSeparatorGCacheObjPart *)B),
		 sizeof(XmSeparatorGCacheObjPart));
}

/***************************** GADGET PART *******************************/

static void
class_initialize()
{
    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(SEPG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(SEPG_ClassCachePart(NULL));
    ClassCacheHead(SEPG_ClassCachePart(NULL)).next =
	&ClassCacheHead(SEPG_ClassCachePart(NULL));

    _XmSeparatoGRectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSEPARATOR_GADGET_BIT);
}

static void
initialize_prehook(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "SeparatoG InitializePrehook\n");

    /*
     * We should allocate secondary object here, using _XmExtObjAlloc (to create
     * the request/new objects), run them throught XtGetSubresources (?).  The
     * superclass will actually allocate the object (see Gadget.c:initialize),
     * but NOT use XtCreateWidget on ExtObject.  If you do that, you *will*
     * dump core, because some of the things done by _XmBuildGadgetResources
     * are also done by _XmBuildExtResources (this was the key concept that was
     * NOT obvious.  Trace seems to indicate that the XmPushWidgetExtData/
     * XmPopWidgetExtData functions are used to communicate these allocated
     * objects the the initialize method and the posthook.  The Cache comes
     * into play during the initialize method (?).
     */

}

static void
initialize_posthook(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmWidgetExtData ext;

    XdbDebug(__FILE__, new_w, "SeparatoG InitializePosthook\n");

    /* don't let the null fool you */
    SEPG_Cache(new_w) = _XmCachePart(SEPG_ClassCachePart(NULL),
				   (XtPointer)SEPG_Cache(new_w),
				   sizeof(XmSeparatorGCacheObjPart));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);
    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);
    XtFree((char *)ext);
}

static void
CreateSeparatorGC(Widget w)
{
    XtGCMask valueMask;
    XGCValues values;

    valueMask =  GCBackground | GCForeground | GCLineStyle | GCJoinStyle |
			GCCapStyle;

    switch (SEPG_SeparatorType(w)) {
    case XmSINGLE_LINE:
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = XmParentForeground(w);
	values.background = XmParentBackground(w);
	break;

    case XmSINGLE_DASHED_LINE:
	values.line_style = LineDoubleDash;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = XmParentForeground(w);
	values.background = XmParentBackground(w);
	break;

    case XmDOUBLE_LINE:
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = XmParentForeground(w);
	values.background = XmParentBackground(w);
	break;

    case XmDOUBLE_DASHED_LINE:
	values.line_style = LineDoubleDash;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = XmParentForeground(w);
	values.background = XmParentBackground(w);
	break;

    case XmSHADOW_ETCHED_IN:
	/* doesn't really matter */
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = XmParentForeground(w);
	values.background = XmParentBackground(w);
	break;

    case XmSHADOW_ETCHED_OUT:
	/* doesn't really matter */
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = XmParentForeground(w);
	values.background = XmParentBackground(w);
	break;

    case XmSHADOW_ETCHED_IN_DASH:
    case XmSHADOW_ETCHED_OUT_DASH:
    default:
	SEPG_SeparatorType(w) = XmSHADOW_ETCHED_IN;
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = XmParentForeground(w);
	values.background = XmParentBackground(w);
	break;
    }

    SEPG_SeparatorGC(w) = XtGetGC(w, valueMask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    if (! XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
        _XmError(new_w, "parent should be manager.");

    if (SEPG_Orientation(new_w) == XmVERTICAL) {
	XtWidth(new_w) = G_ShadowThickness(new_w);
    }
    if (SEPG_Orientation(new_w) == XmHORIZONTAL) {
	XtHeight(new_w) = G_ShadowThickness(new_w);
    }

    CreateSeparatorGC(new_w);

    G_EventMask(new_w) = 0;
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, SEPG_SeparatorGC(w));

    _XmCacheDelete(SEPG_Cache(w));
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

    bcopy(SEPG_Cache(new_w),
	  &((XmSeparatorGCacheObject)nsec)->separator_cache,
	  sizeof(XmSeparatorGCacheObjPart));

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
    ((XmExtRec *)rsec)->object.self = (Widget)rsec;

    SEPG_Cache(new_w) = &(((XmSeparatorGCacheObject)nsec)->separator_cache);
    SEPG_Cache(request) = &(((XmSeparatorGCacheObject)rsec)->separator_cache);

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

    if (!_XmSeparatorCacheCompare(SEPG_Cache(new_w), SEPG_Cache(old))) {

	_XmCacheDelete((XtPointer)SEPG_Cache(old));

	SEPG_Cache(new_w) = _XmCachePart(SEPG_ClassCachePart(NULL),
				       (XtPointer)SEPG_Cache(new_w),
				       sizeof(XmSeparatorGCacheObjPart));
    }
    else
	SEPG_Cache(new_w) = SEPG_Cache(old);

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

    if (XmParentForeground(new_w) != XmParentForeground(old) ||
	SEPG_SeparatorType(new_w) != SEPG_SeparatorType(old)) {
	XtReleaseGC(new_w, SEPG_SeparatorGC(new_w));
	CreateSeparatorGC(new_w);
	refresh_needed = False;
    }

    return refresh_needed;
}

static void
get_values_hook(Widget w, ArgList args, Cardinal *num_args)
{
    XdbDebug(__FILE__, w, "SepG: GetValuesHook\n");
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

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent(new_w);
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    bcopy(SEPG_Cache(new_w),
	  &((XmSeparatorGCacheObject)nsec)->separator_cache,
	  sizeof(XmSeparatorGCacheObjPart));

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
expose(Widget w,
       XEvent *event,
       Region region)
{
    XdbDebug(__FILE__, w, "XmSeparatorGadget %s Expose (shadowThickness %d)\n",
	     XtName(w), G_ShadowThickness(w));

    _XmDrawSeparator(XtDisplayOfObject(w), 
		     XtWindowOfObject(w),
		     XmParentTopShadowGC(w), XmParentBottomShadowGC(w),
		     SEPG_SeparatorGC(w),
		     XtX(w), XtY(w),
		     XtWidth(w), XtHeight(w),
		     G_ShadowThickness(w),
		     SEPG_Margin(w),
		     SEPG_Orientation(w),
		     SEPG_SeparatorType(w));
}

static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    XmSeparatorGadget sw = (XmSeparatorGadget)w;

    answer->request_mode = CWWidth | CWHeight;

    switch(SEPG_Orientation(sw))
    {
    case XmHORIZONTAL:
	answer->width = 0; /* this is just so the manager will reset it,
			    * and to avoid the XmUNSPECIFIED */
	answer->height = G_ShadowThickness(sw);
	break;
    case XmVERTICAL:
	answer->height = 0; /* this is just so the manager will reset it,
			     * and to avoid the XmUNSPECIFIED */
	answer->width = G_ShadowThickness(sw);
	break;
    }

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

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIXME */

    return _XmSecondaryResourceData(&_XmSeparatoGRectClassExtRec,
				    data, NULL, NULL, NULL, NULL);
}

Widget
XmCreateSeparatorGadget(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmSeparatorGadgetClass,
			  parent,
			  arglist,
			  argcount);
}
