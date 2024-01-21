/**
 *
 * $Id: LabelG.c,v 1.16 1996/11/28 09:21:23 u27113 Exp $
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

static char rcsid[] = "$Id: LabelG.c,v 1.16 1996/11/28 09:21:23 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/LabelGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuShell.h>
#include <Xm/CacheP.h>
#include <Xm/XmosP.h>
#include <X11/Xfuncs.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <XmI/DebugUtil.h>

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args);

static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void labelg_menu_procs(int function, Widget menushell_parent, ...);

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
#define Offset(field) XtOffsetOf(XmLabelGCacheObjRec, label_cache.field)
static XtResource cache_resources[] = {
    {
	XmNlabelType, XmCLabelType, XmRLabelType,
	sizeof(unsigned char), Offset(label_type),
	XmRImmediate, (XtPointer)XmSTRING
    },
    {
	XmNalignment, XmCAlignment, XmRAlignment,
	sizeof(unsigned char), Offset(alignment),
	XmRImmediate, (XtPointer)XmALIGNMENT_CENTER
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginLeft, XmCMarginLeft, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_left),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginRight, XmCMarginRight, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_right),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginTop, XmCMarginTop, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_top),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginBottom, XmCMarginBottom, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_bottom),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNrecomputeSize, XmCRecomputeSize, XmRBoolean,
	sizeof(Boolean), Offset(recompute_size),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNstringDirection, XmCStringDirection, XmRStringDirection,
	sizeof(XmStringDirection), Offset(string_direction),
	XmRImmediate, (XtPointer)((XmStringDirection)XmUNSPECIFIED)
    }
};

static XmSyntheticResource cache_syn_resources[] = {
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNmarginLeft,
	sizeof(Dimension), Offset(margin_left),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginRight,
	sizeof(Dimension), Offset(margin_right),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginTop,
	sizeof(Dimension), Offset(margin_top),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNmarginBottom,
	sizeof(Dimension), Offset(margin_bottom),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
};

XmLabelGCacheObjClassRec xmLabelGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmExtClassRec,
        /* class_name            */ "XmLabelGCacheObjClass",
	/* widget_size           */ sizeof(XmLabelGCacheObjRec),
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
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmLabelGadgetRec, label.field)

/* Resources for the label class */
static XtResource resources[] = {
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelGadgetRec, gadget.shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNlabelPixmap, XmCLabelPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelInsensitivePixmap, XmCLabelInsensitivePixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(pixmap_insen),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(_label),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmnemonic, XmCMnemonic, XmRKeySym,
	sizeof(KeySym), Offset(mnemonic),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmnemonicCharSet, XmCMnemonicCharSet, XmRString,
	sizeof(String), Offset(mnemonicCharset),
	XmRImmediate, (XtPointer)XmFONTLIST_DEFAULT_TAG
    },
    {
	XmNaccelerator, XmCAccelerator, XmRString,
	sizeof(String), Offset(accelerator),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNacceleratorText, XmCAcceleratorText, XmRXmString,
	sizeof(XmString), Offset(_acc_text),
	XmRImmediate, (XtPointer)NULL
    },
    /* Things we override from Gadget */
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmLabelGadgetRec,gadget.traversal_on),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelGadgetRec,gadget.highlight_thickness),
	XmRImmediate, (XtPointer)0
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNlabelString,
	sizeof(XmString), Offset(_label),
	_XmExportLabelString, NULL
    },
    {
	XmNaccelerator,
	sizeof(String), Offset(accelerator),
	_XmExportString, NULL
    },
    {
	XmNacceleratorText,
	sizeof(XmString), Offset(_acc_text),
	_XmExportLabelString, NULL
    },
    {
	XmNmnemonicCharSet,
	sizeof(String), Offset(mnemonicCharset),
	_XmExportString, NULL
    }
};

static XmBaseClassExtRec _XmLabelGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmLabelGCacheObjClassRec,
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
    _XmLabelCacheCompare
};

static XmGadgetClassExtRec _XmLabelGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ NULL, /* FIXME */
    /* display_rect_proc         */ NULL, /* FIXME */
};

XmLabelGadgetClassRec xmLabelGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmGadgetClassRec,
	/* class_name            */ "XmLabelGadget",
	/* widget_size           */ sizeof(XmLabelGadgetRec),
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
	/* resize                */ resize,
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
	/* extension             */ (XtPointer)&_XmLabelGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, 
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* fix me */
	/* arm_and_activate   */ NULL, /* fix me */
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ XmInheritVisualChange, /* fix me */
	/* syn_resources      */ syn_resources,
	/* num_syn_resources  */ XtNumber(syn_resources),
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmLabelGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
        /* setOverrideCallback */ NULL, /* fix me */
        /* menuProcs           */ labelg_menu_procs, 
	/* extension           */ NULL
    },
};

WidgetClass xmLabelGadgetClass = (WidgetClass)&xmLabelGadgetClassRec;

extern XmFontList _XmFontListCreateDefault(Display *);

/******************************** CACHE PART *********************************/
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

    XdbDebug(__FILE__, new_w, "LabelGCacheRec %s being initialized.\n", XtName(new_w));

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
}

int
_XmLabelCacheCompare(XtPointer A, XtPointer B)
{
    return !bcmp(((XmLabelGCacheObjPart *)A),
		 ((XmLabelGCacheObjPart *)B),
		 sizeof(XmLabelGCacheObjPart));
}

void
_XmCalcLabelGDimensions(Widget w)
{
    Dimension width, height;

    if (LabG_IsText(w))
    {
	_XmStringExtent(LabG_Font(w),
		       LabG_Label(w),
		       &width,
		       &height);
    }
    else	/* pixmap */
    {
	_XmLabelGetPixmapSize(w, LabG_Pixmap(w), &width, &height);
    }

    width = (LabG_Highlight(w)
	   + LabG_Shadow(w)
	   + LabG_MarginLeft(w)
	   + LabG_MarginWidth(w)
	   + width
	   + LabG_MarginWidth(w)
	   + LabG_MarginRight(w)
	   + LabG_Shadow(w)
	   + LabG_Highlight(w));
	
    height = (LabG_Highlight(w)
	   + LabG_Shadow(w)
	   + LabG_MarginTop(w)
	   + LabG_MarginHeight(w)
	   + height
	   + LabG_MarginHeight(w)
	   + LabG_MarginBottom(w)
	   + LabG_Shadow(w)
	   + LabG_Highlight(w));

    if (XtWidth(w) == 0)
	XtWidth(w) = width;

    if (XtHeight(w) == 0)
	XtHeight(w) = height;
}

void
_XmReCacheLabG(Widget w)
{
}

void
_XmAssignLabG_MarginHeight(XmLabelGadget lw, Dimension value)
{
}

void
_XmAssignLabG_MarginWidth(XmLabelGadget lw, Dimension value)
{
}

void
_XmAssignLabG_MarginLeft(XmLabelGadget lw, Dimension value)
{
}

void
_XmAssignLabG_MarginRight(XmLabelGadget lw, Dimension value)
{
}

void
_XmAssignLabG_MarginTop(XmLabelGadget lw, Dimension value)
{
}

void
_XmAssignLabG_MarginBottom(XmLabelGadget lw, Dimension value)
{
}

void
_XmProcessDrag(Widget w, XEvent *event,
	       String *params, Cardinal *num_params)
{
}

/******************************* GADGET PART *********************************/
static void
class_initialize()
{
    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(LabG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(LabG_ClassCachePart(NULL));
    ClassCacheHead(LabG_ClassCachePart(NULL)).next =
	&ClassCacheHead(LabG_ClassCachePart(NULL));

    _XmLabelGRectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmLabelGadgetClass lc = (XmLabelGadgetClass)widget_class;
    XmLabelGadgetClass sc = (XmLabelGadgetClass)(widget_class->core_class.superclass);
    XmGadgetClassExt ext, *extptr, *sextptr;

    if (lc->label_class.menuProcs == XmInheritMenuProc)
	lc->label_class.menuProcs =
		sc->label_class.menuProcs;

    extptr = (XmGadgetClassExt*)_XmGetClassExtensionPtr(
		(XmGenericClassExt*)&(lc->gadget_class.extension),
		NULLQUARK);
    sextptr = (XmGadgetClassExt*)_XmGetClassExtensionPtr(
		(XmGenericClassExt*)&(sc->gadget_class.extension),
		NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (XmGadgetClassExt)XtNew(XmGadgetClassExtRec);
	if (ext != NULL) {
	    ext->next_extension = lc->gadget_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XmGadgetClassExtVersion;
	    ext->record_size = sizeof(XmGadgetClassExtRec);
	    lc->gadget_class.extension = (XtPointer)ext;
	}
    }
    else
	ext = *extptr;
    if (sextptr && *sextptr) {
	if (ext->widget_baseline == XmInheritBaselineProc)
	    ext->widget_baseline = (*sextptr)->widget_baseline;
	if (ext->widget_display_rect == XmInheritDisplayRectProc)
	    ext->widget_display_rect = (*sextptr)->widget_display_rect;
    }

    _XmFastSubclassInit(widget_class, XmLABEL_GADGET_BIT);
}

static void
CreateNormalGC(Widget w)
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

    LabG_NormalGC(w) = XtGetGC(w, mask, &values);
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
    if ((LabG_TextRect_x(w) & 1) ^ (LabG_TextRect_y(w) & 1))
	values.stipple = XmGetPixmapByDepth(XtScreen(w),
					    XmODD_STIPPLE_IMAGE,
					    WhitePixelOfScreen(XtScreen(w)),
					    BlackPixelOfScreen(XtScreen(w)),
					    1);
    else
	values.stipple = XmGetPixmapByDepth(XtScreen(w),
					    XmEVEN_STIPPLE_IMAGE,
					    WhitePixelOfScreen(XtScreen(w)),
					    BlackPixelOfScreen(XtScreen(w)),
					    1);

    LabG_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "LabelG InitializePrehook\n");
}

static void
initialize_posthook(Widget request,
           Widget new_w,
           ArgList args,
           Cardinal *num_args)
{
    XmWidgetExtData ext;

    XdbDebug(__FILE__, new_w, "LabelG InitializePosthook\n");

    /* don't let the null fool you */
    LabG_Cache(new_w) = _XmCachePart(LabG_ClassCachePart(NULL),
                                   (XtPointer)LabG_Cache(new_w),
                                   sizeof(XmLabelGCacheObjPart));

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
    XdbDebug(__FILE__, new_w, "LabelGadget initialize w,h before: %d %d\n",
	 XtWidth(new_w), XtHeight(new_w));

    /* get the default fontlist if the label was created without one. */
    if (LabG_Font(new_w) == (XmFontList)XmUNSPECIFIED || LabG_Font(new_w) == NULL)
	LabG_Font(new_w) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);

    /* If the label was not initialized with the resource labelString set,
       use its name -- the follow _XmString code comes from MegaButton */
    if (LabG_Label(new_w) == (_XmString)XmUNSPECIFIED
	|| LabG_Label(new_w) == (_XmString)0) { /* Shouldn't be necessary but is */
	XmString xmstring;

	xmstring = _XmOSGetLocalizedString((char *) NULL,
					   (Widget)new_w,
					   XmNlabelString,
					   XtName(new_w));

	LabG_Label(new_w) = _XmStringCreate(xmstring);
    }

    if (_XmStringIsXmString((XmString)LabG_Label(new_w)))
	LabG_Label(new_w) = _XmStringCreate((XmString)LabG_Label(new_w));

    if (LabG_AcceleratorText(new_w) != NULL)
	LabG_AcceleratorText(new_w) = _XmStringCreate((XmString)LabG_AcceleratorText(new_w));
    else
	LabG_AcceleratorText(new_w) = _XmStringCreate(XmStringCreateLocalized(""));

    /* have to check request since new_w may have been polluted by a
     * superclass 
     */
    if (XtWidth(request) == (Dimension)0 || XtHeight(request) == (Dimension)0) {
	/* call this to determine the real dimensions of the label 
	 * since we don't want to use 0
	 */
	XtWidth(new_w) = 0;
	XtHeight(new_w) = 0;
	_XmCalcLabelGDimensions(new_w);
	resize(new_w);
    }

    CreateNormalGC(new_w);
    CreateInsensitiveGC(new_w);

    /* if the parent is a row column, set the menu_type to
       it's type.  Otherwise, XmNONE  (Is this right?) FIX ME */

    if (XmIsRowColumn(XtParent(new_w)))
	LabG_MenuType(new_w) = RC_Type(XtParent(new_w));
    else
	LabG_MenuType(new_w) = XmNONE;

    /* Force the traversal and highlight on enter resources if
       in an popup, pulldown, and option menus */

    if (LabG_MenuType(new_w) == XmMENU_POPUP 
	|| LabG_MenuType(new_w) == XmMENU_PULLDOWN
	|| LabG_MenuType(new_w) == XmMENU_OPTION)
    {
	G_TraversalOn(new_w) = False;
	G_HighlightOnEnter(new_w) = False;
    }

    if (! XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
        _XmError(new_w, "parent should be manager.");

    if (LabG_MnemonicCharset(new_w) != NULL)
	LabG_MnemonicCharset(new_w) = XtNewString(LabG_MnemonicCharset(new_w));
    else
	LabG_MnemonicCharset(new_w) = XtNewString("");

    if (_XmStringIsXmString((XmString)LabG_Accelerator(new_w)))
	XmStringGetLtoR((XmString)LabG_Accelerator(new_w),
			XmFONTLIST_DEFAULT_TAG,
			&LabG_Accelerator(new_w));

    if (LabG_Accelerator(new_w)) {
	LabG_Accelerator(new_w) = XtNewString(LabG_Accelerator(new_w));
	_XmManagerInstallAccelerator(XtParent(new_w), new_w, LabG_Accelerator(new_w));
    }
    if (LabG_Mnemonic(new_w))
	_XmManagerInstallMnemonic(XtParent(new_w), new_w, LabG_Mnemonic(new_w));
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, LabG_NormalGC(w));
    XtReleaseGC(w, LabG_InsensitiveGC(w));
    _XmManagerUninstallAccelerator(XtParent(w), w);
    _XmManagerUninstallMnemonic(XtParent(w), w);

    _XmCacheDelete(LabG_Cache(w));
}

static void
resize(Widget w)
{
    Dimension width, height;
    Dimension pix_width, pix_height;
    Boolean	showAcc;
    unsigned char beforex, beforey, afterx, aftery;

    /* NB. For gadgets, the TextRect is relative to the XtX and XtY
       positions, as the Xt[X|Y] and border_widths are added onto
       them before the text is drawn. */

    if (!XmIsLabelGadget(w))
	return;

    beforex = LabG_TextRect_x(w) & 1;
    beforey = LabG_TextRect_y(w) & 1;

    showAcc = _XmLabelShowsAccelerators(w);

    _XmLabelAccTextSize(w);

    /* set the label's size so the pixmap/string fits */
    if (LabG_IsText(w))
        _XmStringExtent(LabG_Font(w),
		       LabG_Label(w),
		       &width,
		       &height);
    else /* LabG_IsPixmap(w) */
    {
	_XmLabelGetPixmapSize(w,LabG_Pixmap(w), &pix_width, &pix_height);	
	width = pix_width;
	height = pix_height;
    }
    
    LabG_TextRect_width(w) = width;
    LabG_TextRect_height(w) = height;
    
    switch (LabG_Alignment(w))
    {
    case XmALIGNMENT_END:
	LabG_TextRect_x(w) = (XtWidth(w)  
			  - LabG_Highlight(w)
			  - LabG_Shadow(w)
			  - LabG_MarginWidth(w)
			  - LabG_MarginRight(w)
			  - LabG_TextRect_width(w));
        break;
    case XmALIGNMENT_BEGINNING:
	LabG_TextRect_x(w) = (LabG_Highlight(w)
			  + LabG_Shadow(w)
			  + LabG_MarginWidth(w)
			  + LabG_MarginLeft(w));
	break;
    case XmALIGNMENT_CENTER:
    default:
	LabG_TextRect_x(w) = (XtWidth(w) -
				(LabG_MarginLeft(w) +
				 LabG_MarginRight(w)) -
				width) / 2 + LabG_MarginLeft(w);
	break;
    }
    
    LabG_TextRect_y(w) = (XtHeight(w) -
				(LabG_MarginTop(w) +
				 LabG_MarginBottom(w)) -
				height) / 2 +
				LabG_MarginTop(w);

    if (showAcc) {
    	LabG_AccTextRect(w).x = XtWidth(w) - LabG_MarginRight(w); /* FIX ME */
    	LabG_AccTextRect(w).y = LabG_TextRect_y(w);
    }

    afterx = LabG_TextRect_x(w) & 1;
    aftery = LabG_TextRect_y(w) & 1;
    if (beforex ^ afterx || beforey ^ aftery) {
	XtReleaseGC(w, LabG_InsensitiveGC(w));
	CreateInsensitiveGC(w);
    }
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

    LabG_Cache(new_w) = &(((XmLabelGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmLabelGCacheObject)rsec)->label_cache);

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

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);

    XtFree( (char *) ext);

    return FALSE;
}

/*
 * Manipulated to have the layout algorithms be called only once per call to
 * set_values, instead of three or four times... Danny 28/8/96
 */
static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean	refresh_needed = False,
		relayout_needed = False;

    XdbDebug(__FILE__, new_w, "LabelGadget SetValues\n");

    /* This is a Primitive resource but we have the GC's for it... */
    if (XmParentForeground(new_w) != XmParentForeground(old)) {
	XtReleaseGC(new_w, LabG_NormalGC(new_w));
	XtReleaseGC(new_w, LabG_InsensitiveGC(new_w));
	CreateNormalGC(new_w);
	CreateInsensitiveGC(new_w);
	refresh_needed = True;
    }

    if (LabG_AcceleratorText(new_w) != LabG_AcceleratorText(old)) {
	LabG_AcceleratorText(new_w) = _XmStringCreate((XmString)LabG_AcceleratorText(new_w));
	_XmStringFree(LabG_AcceleratorText(old));
	refresh_needed = True;
    }
    if (LabG_MnemonicCharset(new_w) != LabG_MnemonicCharset(old)) {
	LabG_MnemonicCharset(new_w) = XtNewString(LabG_MnemonicCharset(new_w));
	XtFree(LabG_MnemonicCharset(old));
	refresh_needed = True;
    }

    /* if labelString is still NULL, it was set that way by the
     * user: NULL labelStrings get the widget name
     */
    if (LabG_Label(new_w) == NULL) {
	LabG_Label(new_w) = _XmStringCreate(XmStringCreateSimple(XtName(new_w)));
	relayout_needed = True;
    }
    /*
     * The "else" was inserted here to make sure we don't do
     * _XmStringCreate twice
     */
    else if (LabG_Label(new_w) != LabG_Label(old)) {
	if (_XmStringIsXmString((XmString)LabG_Label(new_w)))
	    LabG_Label(new_w) = _XmStringCreate((XmString)LabG_Label(new_w));

	if (LabG_Label(old))
	    _XmStringFree(LabG_Label(old));

	relayout_needed = True;
    }

    if (LabG_Alignment(new_w) != LabG_Alignment(old))
	refresh_needed = True;

    if (LabG_Font(new_w) != LabG_Font(old)
	|| LabG_MarginTop(new_w) != LabG_MarginTop(old)
	|| LabG_MarginBottom(new_w) != LabG_MarginBottom(old)
	|| LabG_MarginLeft(new_w) != LabG_MarginLeft(old)
	|| LabG_MarginRight(new_w) != LabG_MarginRight(old)
	|| LabG_MarginWidth(new_w) != LabG_MarginWidth(old)
	|| LabG_MarginHeight(new_w) != LabG_MarginHeight(old)
	|| LabG_Mnemonic(new_w) != LabG_Mnemonic(old)
	|| LabG_StringDirection(new_w) != LabG_StringDirection(old))
    {
	relayout_needed = True;
    }

    /* check for change in insensitive pixmap */
    if ((LabG_PixmapInsensitive(new_w) != LabG_PixmapInsensitive(old)) 
	&& !XtSensitive(new_w) && LabG_IsPixmap(new_w))
	relayout_needed = True;

    /* check for change in pixmap */
    if (LabG_Pixmap(new_w) != LabG_Pixmap(old)) 
    {
	/* if changed pixmap to UNSPECIFIED, automatically configure to a
	 * string
	 */
	if (LabG_IsPixmap(new_w)
	    && LabG_Pixmap(new_w) == (Pixmap)XmUNSPECIFIED_PIXMAP)
	{
	    LabG_LabelType(new_w) = XmSTRING;
	}

	relayout_needed = True;
    }

    /* did the label change types? */
    if (LabG_LabelType(new_w) != LabG_LabelType(old))
	relayout_needed = True;

    if (LabG_Accelerator(new_w) != LabG_Accelerator(old)) {

        if (_XmStringIsXmString((XmString)LabG_Accelerator(new_w)))
            XmStringGetLtoR((XmString)LabG_Accelerator(new_w),
                             XmFONTLIST_DEFAULT_TAG,
                             &LabG_Accelerator(new_w));
	else if (LabG_Accelerator(new_w))
	    LabG_Accelerator(new_w) = XtNewString(LabG_Accelerator(new_w));
	_XmManagerUninstallAccelerator(XtParent(new_w), new_w);
	_XmManagerInstallAccelerator(XtParent(new_w), new_w, LabG_Accelerator(new_w));
	refresh_needed = True;
    }
    if (LabG_Mnemonic(new_w) != LabG_Mnemonic(old)) {
	_XmManagerUninstallMnemonic(XtParent(new_w), new_w);
	_XmManagerInstallMnemonic(XtParent(new_w), new_w, LabG_Mnemonic(new_w));
	refresh_needed = True;
    }

    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0)
	relayout_needed = True;

    if (relayout_needed) {
	if (LabG_RecomputeSize(new_w) && XtWidth(new_w) == XtWidth(old))
	    XtWidth(new_w) = 0;
	if (LabG_RecomputeSize(new_w) && XtHeight(new_w) == XtHeight(old))
	    XtHeight(new_w) = 0;

	_XmCalcLabelGDimensions(new_w);

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
    XdbDebug(__FILE__, w, "LabelG: GetValuesHook\n");
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    XRectangle cliprect;
    int width, height;
    GC myGC;

   /*
    * I'm getting paranoid - Danny - see testXm/filesb/test3
    */
    if (! XtIsRealized(w))
	return;

    /* if the internals of the widget haven't been recomputed yet,
     * recompute them now (happens if resize isn't called after set_values)
     */
    resize(w);

    /* use the right GC */
    if (XtSensitive(w))
        myGC = LabG_NormalGC(w);
    else
        myGC = LabG_InsensitiveGC(w);

    /* Set a clip rectangle for the GC - ensure we don't overwrite shadows */
    width = XtWidth(w);
    height = XtHeight(w);

    if(width <= 0)
	width = 1;
    if(height <= 0)
	height = 1;
    cliprect.x = XtX(w) + LabG_MarginLeft(w) + G_HighlightThickness(w) +
			G_ShadowThickness(w);
    cliprect.y = XtY(w) + LabG_MarginTop(w) + G_HighlightThickness(w) +
			G_ShadowThickness(w);
    cliprect.width = XtX(w) + XtWidth(w) - (LabG_MarginRight(w) + cliprect.x +
			G_ShadowThickness(w) + G_HighlightThickness(w));
    cliprect.height = XtY(w) + XtHeight(w) - (LabG_MarginBottom(w) + cliprect.y +
			G_ShadowThickness(w) + G_HighlightThickness(w));
    cliprect.width = width;
    cliprect.height = height;

    XSetClipRectangles(XtDisplay(w), myGC, 0, 0, &cliprect, 1, Unsorted);
    
    if(LabG_IsText(w))
    {
	if (_XmLabelShowsMnemonic(w) && LabG_Mnemonic(w)) {
	    char        m[2];

	    m[0] = LabG_Mnemonic(w);
	    m[1] = '\0';

	    _XmStringDrawMnemonic(XtDisplay(w), XtWindow(w),
		LabG_Font(w), LabG_Label(w), myGC,
		XtX(w) + LabG_TextRect_x(w),
		XtY(w) +  LabG_TextRect_y(w),
		LabG_TextRect_width(w),
		LabG_Alignment(w),
		0,
		NULL,
		m, LabG_MnemonicCharset(w));
	}
	else {
	    _XmStringDraw(XtDisplayOfObject(w),
			 XtWindowOfObject(w),
			 LabG_Font(w),
			 LabG_Label(w),
			 myGC,
			 XtX(w) + LabG_TextRect_x(w),
			 XtY(w) + LabG_TextRect_y(w),
			 LabG_TextRect_width(w),
			 LabG_Alignment(w),
			 0,
			 NULL);
	}
        /* AcceleratorText */
	if (_XmLabelShowsAccelerators(w)) {
	    _XmStringDraw(XtDisplay(w),
			 XtWindow(w),
			 LabG_Font(w),
			 LabG_AcceleratorText(w),
			 myGC,
			 XtX(w) + LabG_AccTextRect(w).x,
			 XtY(w) + LabG_AccTextRect(w).y,
			 LabG_AccTextRect(w).width,
			 XmALIGNMENT_BEGINNING,
			 0,
			 NULL);
	}
    } 
    else if (XtSensitive(w) &&
	     LabG_Pixmap(w) != XmUNSPECIFIED_PIXMAP)
    {
	XdbDebug(__FILE__, w,
		 "XmLabelGadget %s XCopyArea Pixmap 0x%X on win 0x%X\n",
		 XtName(w), LabG_Pixmap(w), XtWindowOfObject(w));

	XCopyArea(XtDisplay(w),
		  LabG_Pixmap(w),
		  XtWindow(w),
		  myGC,
		  0,
		  0,
		  LabG_TextRect_width(w),
		  LabG_TextRect_height(w), 
		  XtX(w) + XtBorderWidth(w) + LabG_TextRect_x(w), 
		  XtY(w) + XtBorderWidth(w) + LabG_TextRect_y(w));
    }
    else if (!XtSensitive(w) &&
	     LabG_PixmapInsensitive(w) != XmUNSPECIFIED_PIXMAP) {
	XdbDebug(__FILE__, w,
		 "XmLabelGadget %s XCopyArea Pixmap 0x%X on win 0x%X\n",
		 XtName(w), LabG_Pixmap(w), XtWindowOfObject(w));

	XCopyArea(XtDisplay(w),
		  LabG_PixmapInsensitive(w),
		  XtWindow(w),
		  myGC,
		  0,
		  0,
		  LabG_TextRect_width(w),
		  LabG_TextRect_height(w), 
		  XtX(w) + XtBorderWidth(w) + LabG_TextRect_x(w), 
		  XtY(w) + XtBorderWidth(w) + LabG_TextRect_y(w));
    }

    XSetClipMask(XtDisplay(w), myGC, None);
}


static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    XtWidgetGeometry	a;	/* Standin for answer if NULL parameter */
    Dimension wd, ht;

#define	Wants(x)	(proposed->request_mode & x)

    if (proposed->request_mode != 0) {/* NULL case should not yet end here ! */
	if ((! (Wants(CWWidth))) && (! Wants(CWHeight))) {
	    /*
	     * If they don't ask width/height, let them have whatever they
	     * like
	     */
	    if (answer)
		*answer = *proposed;
	    return XtGeometryYes;
	}
    }

#if 1
/* 
 * this should be active, but RowColumn can't handle it very well
 */
    wd = XtWidth(w);
    ht = XtHeight(w);
    XtWidth(w) = 0;
    XtHeight(w) = 0;
    _XmCalcLabelGDimensions(w);

    a.width = XtWidth(w);
    a.height = XtHeight(w);
    a.request_mode = CWWidth | CWHeight;
    XdbDebug(__FILE__, w,
	     "LabelGadget queried for size: reporting %d %d %08x\n",
	     XtWidth(w), XtHeight(w), answer);

    XtWidth(w) = wd;
    XtHeight(w) = ht;
#else
    a.width = XtWidth(w);
    a.height = XtHeight(w);
    a.request_mode = CWWidth | CWHeight;
#endif

    if (answer)
	*answer = a;

    if (proposed->request_mode == 0)	/* NULL proposed -> return Width+Height */
	return XtGeometryAlmost;

    if (proposed->width >= answer->width && proposed->height >= answer->height) 
	return XtGeometryYes;
    else if (answer->width == XtWidth(w) && answer->height == XtHeight(w)) {
	if (answer)
	    answer->request_mode = 0;
	return XtGeometryNo;
    } else 
	return XtGeometryAlmost;
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIXME */

    return _XmSecondaryResourceData(&_XmLabelGRectClassExtRec,
                                    data, NULL, NULL, NULL, NULL);
}

static void
input_dispatch(Widget gadget, 
	       XEvent *event, 
	       Mask event_mask) 
{
    switch (event_mask)
    {
    case XmHELP_EVENT:
	XdbDebug(__FILE__, gadget, "LabelGadget got help event\n");
	break;
    case XmBDRAG_EVENT:
	XdbDebug(__FILE__, gadget, "LabelGadget got bdrag event\n");
	break;
    }
}

static void 
#ifdef __STDC__
labelg_menu_procs(int function, Widget widget, ...)
{
    va_list arg_list;
    XtPointer foo;
    XEvent *event; 
    XtPointer returnData;

    va_start(arg_list, widget);

#else
labelg_menu_procs(function, widget, va_alist)
    int function;
    Widget widget;
    va_dcl
{
    va_list arglist;
    XtPointer foo;
    XEvent *event; 
    XtPointer returnData;

    va_start(arg_list);

#endif

    foo = va_arg(arg_list, XtPointer);
    event = va_arg(arg_list, XEvent *);
    returnData = va_arg(arg_list, XtPointer);

    XdbDebug(__FILE__, widget, "LabelG_Menu_Procs(%s, ...)\n", XdbMenuEnum2String(function));

    switch (function)
    {
    case XmMENU_BUTTON: 
	{
	    /* There was a button press in the menu.  
	       Check to see if it was the valid button
	       for that type of menu */
	    XButtonEvent *xbe = (XButtonEvent*)event;

	    if (!XmIsRowColumn(widget)) {
		va_end(arg_list);
		return;
	    }

	    switch (RC_Type(widget))
	    {
	    case XmMENU_BAR:
	    case XmMENU_PULLDOWN:
	    case XmMENU_OPTION:
		*(Boolean*)returnData = (xbe->button == 1);
		break;
	    case XmMENU_POPUP:
		/* the third mouse button also works in popups */
		*(Boolean*)returnData = ((xbe->button == 1) || (xbe->button == 3));		
		break;
	    default:
		*(Boolean*)returnData = False; /* ? */
		break;
	    }
	    break;
	}
    case XmMENU_POPDOWN:
	break;
    case XmMENU_BUTTON_POPDOWN:
	{
	    Widget mb, shell = XtParent(XtParent(widget));

	    if (LabG_MenuType(widget) == XmMENU_POPUP)
		*(Boolean*)returnData = True;
	    else
		*(Boolean*)returnData = False;

	    if (LabG_MenuType(widget) == XmMENU_BAR) 
            {
		XdbDebug2(__FILE__, XtParent(widget), widget, "XtRemoveGrab(parent)\n");
		XtRemoveGrab(XtParent(widget));
	    } 

	    XdbDebug(__FILE__, widget, "Calling MenuShellPopdownDone\n");
	    XtCallActionProc(shell, "MenuShellPopdownDone", event, NULL, 0);
#if 0
/*
 * This produces a nice
 *    Error: Couldn't find per display information
 * and the last action inside MenuShellPopdownDone (see above) seems to be
 *    _XmUngrabPointer();
 * so we might as well skip this one...
 */
	    XdbDebug(__FILE__, widget, "XtUngrabPointer\n");
	    XtUngrabPointer(widget, CurrentTime);
#endif
            for (mb = widget; mb; mb = XtParent(mb))
                if (XmIsRowColumn(mb) && RC_Type(mb) == XmMENU_BAR)
                    break;
            if (mb) {
                XdbDebug(__FILE__, mb, "XtRemoveGrab\n");
                XtRemoveGrab(mb);
            }
	    break;
	}
    case XmMENU_SHELL_POPDOWN:
	{
	    Widget shell = XtParent(XtParent(widget));

	    if (!shell)
	      return;

	    XtCallActionProc(shell, "MenuShellPopdownOne", event, NULL, 0);
	    
	    break;
	}
    }
    va_end(arg_list);
}

Widget
XmCreateLabelGadget(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmLabelGadgetClass,
			  parent,
			  arglist,
			  argcount);
}
