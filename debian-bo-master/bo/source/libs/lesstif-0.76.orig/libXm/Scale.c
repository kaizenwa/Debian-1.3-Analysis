/**
 *
 * $Id: Scale.c,v 1.8 1996/12/19 06:57:24 u27113 Exp $
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

static char rcsid[] = "$Id: Scale.c,v 1.8 1996/12/19 06:57:24 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ScaleP.h>
#include <Xm/LabelGP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/BulletinBP.h>
#include <Xm/MenuShellP.h>
#include <Xm/VendorSP.h>
#include <Xm/VendorSEP.h>

#include <stdio.h>
#include <limits.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

#define CHILD_LAB	0
#define CHILD_SB	1

/* FIX ME/BUGS
 */

/*
 * these values come from behavior observation of M*tif.  Change at your own
 * risk
 *
 * Hmm, I've observed HSLIDERSIZE to be 30 (M*tif 1.2.2) -- PvH
 */
#define SCB_MAX		1000000000
#define SCB_MIN		0
#define HSLIDERSIZE	/*40*/30
#define VSLIDERSIZE	12
#define MIN_SLIDE	4

#define SB_TRANSVERSAL_SIZE	15
#define SB_LONGITUDAL_SIZE	100

#define _XmMax(a,b) ((a)>(b)?(a):(b))
#define Scale_ShadowThickness(x) MGR_ShadowThickness(x)

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal * num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal * num_args);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry * request, XtWidgetGeometry * reply);
static XtGeometryResult geometry_manager(Widget w, XtWidgetGeometry * request, XtWidgetGeometry * reply);
static void expose(Widget w, XEvent * event, Region region);
static void resize(Widget w);
static void change_managed(Widget w);
static void insert_child(Widget w);

static void _XmScaleLayout(Widget scale, Boolean ParentResize,
                           Widget child, Boolean TestMode,
			   XtWidgetGeometry *childgeom);

static int _XmScaleConvertWidthToSliderSize(Widget w);
static int _XmScaleConvertSCBValueToScaleValue(Widget w, int value);
static int _XmScaleConvertScaleValueToSCBValue(Widget w);
static void _ScaleValueChanged(Widget sb, XtPointer cd, XtPointer data);
static void _ScaleDrag(Widget sb, XtPointer cd, XtPointer data);
static void computeValueSize(Widget w);
static void showValue(Widget w, int scb_value, int scale_value);
  
static void GetFocus(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LoseFocus(Widget w, XEvent *event, String *params, Cardinal *num_params);
  
/*
 * Resources for the scale class 
 */
#define Offset(field) XtOffsetOf(XmScaleRec, scale.field)
static XtResource resources[] =
{
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmScaleRec, manager.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNvalue, XmCValue, XmRInt,
	sizeof(int), Offset(value),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNmaximum, XmCMaximum, XmRInt,
	sizeof(int), Offset(maximum),
	XmRImmediate, (XtPointer) 100
    },
    {
	XmNminimum, XmCMinimum, XmRInt,
	sizeof(int), Offset(minimum),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmVERTICAL
    },
    {
	XmNprocessingDirection, XmCProcessingDirection, XmRProcessingDirection,
	sizeof(unsigned char), Offset(processing_direction),
	XmRCallProc, (XtPointer)_XmScaleProcessingDirectionDefault
    },
    {
	XmNtitleString, XmCTitleString, XmRXmString,
	sizeof(XmString), Offset(title),
	XtRImmediate, (XtPointer) NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font_list),
	XtRImmediate, (XtPointer)NULL
    },
    {
	XmNshowValue, XmCShowValue, XmRBoolean,
	sizeof(Boolean), Offset(show_value),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdecimalPoints, XmCDecimalPoints, XmRShort,
	sizeof(short), Offset(decimal_points),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNscaleWidth, XmCScaleWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(scale_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNscaleHeight, XmCScaleHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(scale_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(highlight_thickness),
	XmRImmediate, (XtPointer) 2
    },
    {
	XmNhighlightOnEnter, XmCHighlightOnEnter, XmRBoolean,
	sizeof(Boolean), Offset(highlight_on_enter),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNvalueChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_callback),
	XmRCallback, (XtPointer) NULL
    },
    {
	XmNdragCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(drag_callback),
	XmRCallback, (XtPointer) NULL
    },
    {
	XmNscaleMultiple, XmCScaleMultiple, XmRInt,
	sizeof(int), Offset(scale_multiple),
	XmRImmediate, (XtPointer)0
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNtitleString,
	sizeof(XmString), Offset(title),
	_XmExportXmString, NULL
    },
    {
	XmNscaleWidth,
	sizeof(Dimension), Offset(scale_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNscaleHeight,
	sizeof(Dimension), Offset(scale_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
};

static char defaultTranslations[] =
    "<FocusIn>: FocusIn() \n\
     <FocusOut>: FocusOut()";

static XtActionsRec actions[] = {
    {"FocusIn", GetFocus},
    {"FocusOut", LoseFocus}
};

static XmBaseClassExtRec _XmScaleCoreClassExtRec = {
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

static XmManagerClassExtRec _XmScaleMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmScaleClassRec xmScaleClassRec =
{
    /* Core class part */
 {
	/* superclass            */ (WidgetClass) & xmManagerClassRec,
	/* class_name            */ "XmScale",
	/* widget_size           */ sizeof(XmScaleRec),
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
	/* tm_table              */ defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmScaleCoreClassExtRec
 },
    /* Composite class part */
 {
	/* geometry manager */ geometry_manager,
	/* change_managed   */ change_managed,
	/* insert_child     */ insert_child,
	/* delete_child     */ XtInheritDeleteChild,
	/* extension        */ NULL,
 },
    /* Constraint class part */
 {
	/* subresources      */ NULL,
	/* subresource_count */ 0,
	/* constraint_size   */ 0,
	/* initialize        */ NULL,
	/* destroy           */ NULL,
	/* set_values        */ NULL,
	/* extension         */ NULL,
 },
    /* XmManager class part */
 {
        /* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
        /* extension                    */ (XtPointer)&_XmScaleMClassExtRec
 },
    /* XmScale part */
 {
	/* extension */ NULL,
 },
};

WidgetClass xmScaleWidgetClass = (WidgetClass) & xmScaleClassRec;

static void
class_initialize()
{
    _XmScaleCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmScaleWidgetClass scclass = (XmScaleWidgetClass)widget_class;
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension*)_XmGetClassExtensionPtr((XmGenericClassExt*)&(scclass->composite_class.extension),
							       NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
        ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
        if (ext != NULL)
        {
            ext->next_extension = scclass->composite_class.extension;
            ext->record_type = NULLQUARK;
            ext->version = XtCompositeExtensionVersion;
            ext->record_size = sizeof(CompositeClassExtensionRec);
            ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
            ext->allows_change_managed_set = True;
#endif
            scclass->composite_class.extension = (XtPointer) ext;
        }
    }    
    _XmFastSubclassInit(widget_class, XmSCALE_BIT);
}

static void
CreateForegroundGC(Widget w)
{
    XtGCMask mask;
    XGCValues values;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction | GCFont |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = MGR_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;
    values.font = Scale_FontStruct(w)->fid;

    Scale_ForegroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Widget title;
    Widget sb;
    Arg argl[10];
    int argc;

    if (Scale_Value(new_w) == INT_MAX) {
	if (Scale_Minimum(new_w) >= 0)
	    Scale_Value(new_w) = Scale_Minimum(new_w);
	else 
	    Scale_Value(new_w) = 0;
    }
    
    if (Scale_ScaleMultiple(new_w) == 0) {
	Scale_ScaleMultiple(new_w) = (Scale_Maximum(new_w) - Scale_Minimum(new_w)) / 10;
    }

    Scale_LastValue(new_w) = Scale_Value(new_w);

    if (Scale_FontList(new_w) == NULL)
	Scale_FontList(new_w) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);

    /* MLM - FIXME -- Should the titleString get the FontList from the scale? */
    argc = 0;
    if (Scale_Title(new_w) != NULL)
	Scale_Title(new_w) = XmStringCopy(Scale_Title(new_w));
    XtSetArg(argl[argc], XmNlabelString, Scale_Title(new_w)); argc++;
    XtSetArg(argl[argc], XmNalignment, XmALIGNMENT_BEGINNING); argc++;
    title = XmCreateLabelGadget(new_w, "Title", argl, argc);
    if (Scale_Title(new_w) != NULL)
	XtManageChild(title);

    argc = 0;
    XtSetArg(argl[argc], XmNshowArrows, FALSE); argc++;
    XtSetArg(argl[argc], XmNmaximum, SCB_MAX); argc++;
    XtSetArg(argl[argc], XmNminimum, SCB_MIN); argc++;
    XtSetArg(argl[argc], XmNorientation, Scale_Orientation(new_w)); argc++;
    XtSetArg(argl[argc], XmNprocessingDirection, Scale_ProcessingDirection(new_w)); argc++;
    XtSetArg(argl[argc], XmNwidth, Scale_ScaleWidth(new_w)); argc++;
    XtSetArg(argl[argc], XmNheight, Scale_ScaleHeight(new_w)); argc++;
    XtSetArg(argl[argc], XmNshadowThickness, Scale_ShadowThickness(new_w)); argc++;
    XtSetArg(argl[argc], XmNhighlightThickness, Scale_HighlightThickness(new_w)); argc++;
    XtSetArg(argl[argc], XmNhighlightOnEnter, Scale_HighlightOnEnter(new_w)); argc++;
    sb = XmCreateScrollBar(new_w, "Scrollbar", argl, argc);
    _XmSetEtchedSlider((XmScrollBarWidget)sb);
    XtAddCallback(sb, XmNdragCallback, _ScaleDrag, NULL);
    if (Scale_Value(new_w) == INT_MAX)
	Scale_Value(new_w) = Scale_Minimum(new_w);
    XtAddCallback(sb, XmNvalueChangedCallback, _ScaleValueChanged, NULL);
    XtManageChild(sb);

    _XmFontListGetDefaultFont(Scale_FontList(new_w), &Scale_FontStruct(new_w));

    if (Scale_FontStruct(new_w) == NULL) {
      Scale_FontList(new_w) = _XmFontListCreateDefault(XtDisplay(new_w));
      _XmFontListGetDefaultFont(Scale_FontList(new_w), &Scale_FontStruct(new_w));
    }

    CreateForegroundGC(new_w);

    /* M*tif doesn't do this */
/*
    _XmScaleLayout(new_w, True, NULL, False, NULL);
*/
}

static void
destroy(Widget w)
{
    XtDestroyGC(Scale_ForegroundGC(w));
}

/*
 * called when the user changes a resource
 */
static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal * num_args)
{
    Boolean		refresh = False;
    Arg argl[8];
    int argc;
    Widget sb, lab;

    sb = ((XmScaleWidget)new_w)->composite.children[CHILD_SB];
    lab = ((XmScaleWidget)new_w)->composite.children[CHILD_LAB];

    if (MGR_Foreground(new_w) != MGR_Foreground(old) ||
	XtBackground(new_w) != XtBackground(old) ||
	Scale_FontList(new_w) != Scale_FontList(old)) {
	_XmFontListGetDefaultFont(Scale_FontList(new_w), &Scale_FontStruct(new_w));

	if (Scale_FontStruct(new_w) == NULL) {
	    Scale_FontList(new_w) = _XmFontListCreateDefault(XtDisplay(new_w));
	    _XmFontListGetDefaultFont(Scale_FontList(new_w),
				      &Scale_FontStruct(new_w));
	}
	XtReleaseGC(new_w, Scale_ForegroundGC(new_w));
	CreateForegroundGC(new_w);
	refresh = True;
    }
	
    if (Scale_Title(new_w) != Scale_Title(old)) {
	argc = 0;
	if (Scale_Title(old))
	    XmStringFree(Scale_Title(old));
	Scale_Title(new_w) = XmStringCopy(Scale_Title(new_w));
	XtSetArg(argl[argc], XmNlabelString, Scale_Title(new_w)); argc++;
	XtSetValues(lab, argl, argc);
	refresh = True;
    }

/*
 * This is taken out of the (large) if statement below,
 * because somehow taking all of the values to scrollbar
 * breaks scrollbar.
 * Therefore we treat XmNvalue separately here.
 * Yes this is a workaround in case you didn't know by now :-(
 */
    if (Scale_Value(new_w) != Scale_Value(old)) {
	int	newval;
	newval = _XmScaleConvertScaleValueToSCBValue(new_w);
	argc = 0;
	XtSetArg(argl[argc], XmNvalue, newval); argc++;
	XtSetValues(sb, argl, argc);

	refresh = True;
    }

    if (Scale_Maximum(new_w) != Scale_Maximum(old) ||
        Scale_Minimum(new_w) != Scale_Minimum(old) ||
        Scale_Orientation(new_w) != Scale_Orientation(old) ||
        Scale_ProcessingDirection(new_w) != Scale_ProcessingDirection(old) ||
        Scale_ScaleWidth(new_w) != Scale_ScaleWidth(old) ||
        Scale_ScaleHeight(new_w) != Scale_ScaleHeight(old) ||
        Scale_ScaleMultiple(new_w) != Scale_ScaleMultiple(old) ||
        Scale_HighlightOnEnter(new_w) != Scale_HighlightOnEnter(old) ||
        Scale_HighlightThickness(new_w) != Scale_HighlightThickness(old)) {

	int	newval;

	argc = 0;
	XtSetArg(argl[argc], XmNorientation, Scale_Orientation(new_w)); argc++;
	XtSetArg(argl[argc], XmNprocessingDirection, Scale_ProcessingDirection(new_w)); argc++;
	XtSetArg(argl[argc], XmNwidth, Scale_ScaleWidth(new_w)); argc++;
	XtSetArg(argl[argc], XmNheight, Scale_ScaleHeight(new_w)); argc++;
#if 0
	newval = _XmScaleConvertScaleValueToSCBValue(new_w);
	XtSetArg(argl[argc], XmNvalue, newval); argc++;
#endif
	XtSetArg(argl[argc], XmNshadowThickness, Scale_ShadowThickness(new_w)); argc++;
	XtSetArg(argl[argc], XmNhighlightThickness, Scale_HighlightThickness(new_w)); argc++;
	XtSetArg(argl[argc], XmNhighlightOnEnter, Scale_HighlightOnEnter(new_w)); argc++;
	XtSetValues(sb, argl, argc);

	refresh = True;
    }

    if (Scale_DecimalPoints(new_w) != Scale_DecimalPoints(old) ||
	Scale_FontList(new_w) != Scale_FontList(old) ||
	Scale_ShowValue(new_w) != Scale_ShowValue(old) ||
	Scale_Value(new_w) != Scale_Value(old))
	refresh = True;

    _XmScaleLayout(new_w, True, NULL, False, NULL);

    return refresh;
}

static void
expose(Widget w,
       XEvent * event,
       Region region)
{
    _XmRedisplayGadgets(w, event, region);

    if (Scale_ShowValue(w))
	showValue(w,
		  _XmScaleConvertScaleValueToSCBValue(w),
		  Scale_Value(w));
}

/*
 * called when our parent wants to find out how we would like to look.  It
 * doesn't have to honor our preference, though.
 */
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry * request, XtWidgetGeometry * reply)
{
#define Wants(x)	((request->request_mode & x) == x)

    XdbDebug(__FILE__, w, "XmScale %s QueryGeometry\n", XtName(w));

    if (request != reply)
	*reply = *request;

    reply->request_mode = 0;

    if (Wants(CWWidth)) {
	reply->request_mode |= CWWidth;
	reply->width = XtWidth(w);
    }
    if (Wants(CWHeight)) {
	reply->request_mode |= CWHeight;
	reply->height = XtHeight(w);
    }

    _XmScaleLayout(w, False, w, False, reply);

    if (Wants(CWHeight) && (request->height < reply->height))
	return XtGeometryNo;
    else if (Wants(CWHeight) && (request->height > reply->height))
	return XtGeometryAlmost;

    if (Wants(CWWidth) && (request->width < reply->width))
	return XtGeometryNo;
    else if (Wants(CWWidth) && (request->width > reply->width))
	return XtGeometryAlmost;

    return XtGeometryYes;
#undef Wants
}

/*
 * called when our parent is resizing us.  We have no choice but to obey.
 */
static void
resize(Widget w)
{
    XdbDebug(__FILE__, w, "Scale %s Resize: x %d y %d w %d h %d\n",
	     XtName(w), XtX(w), XtY(w), XtWidth(w), XtHeight(w));

    Scale_SliderSize(w) = _XmScaleConvertWidthToSliderSize(w);

    _XmScaleLayout(w, False, NULL, False, NULL);
}

/*
 * called when one of our children wants to change.  We control whether
 * the child can, or not
 * Shamelessly ripped off from the Form
 */
#define       Wants(x)        (request->request_mode & x)
static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry * request, XtWidgetGeometry * reply)
{
    XtWidgetGeometry want;
    int good = 0, ask = 0;

    if ((Wants(CWX) && request->x != XtX(w)) ||
	(Wants(CWY) && request->y != XtY(w)))
	return XtGeometryNo;

    want = *request;

    _XmScaleLayout(XtParent(w), False, w, True, &want);

    if (Wants(CWWidth)) {
	ask++;
	if ((want.request_mode & CWWidth) && want.width == request->width)
	    good++;
    }
    if (Wants(CWHeight)) {
	ask++;
	if ((want.request_mode & CWHeight) && want.height == request->height)
	    good++;
    }

    if (reply)
	*reply = want;

    if (ask == good)
	return XtGeometryYes;
    if (good == 0)
	return XtGeometryNo;
    else
	return XtGeometryAlmost;
#undef Wants
}

/*
 * called when our Window is allocated.  We may want to change our layout,
 * and we can ask our parent to allow this
 */
static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes) {
#define superclass (&xmManagerClassRec)
        (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

    /* Redundant? XtRealizeWidget calls changed_managed ... -- PvH */
    _XmScaleLayout(w, True, NULL, False, NULL);
}

/*
 * one of our children has become managed or unmanaged.  We need to relayout
 */
static void
change_managed(Widget w)
{
    _XmScaleLayout(w, True, NULL, False, NULL);

    _XmNavigChangeManaged(w);
}

/*
 * a child has been added
 */
static void
insert_child(Widget w)
{
#define superclass      (&xmManagerClassRec)
    (*superclass->composite_class.insert_child)(w);
#undef  superclass

    XdbDebug(__FILE__, w,
	     "Scale %s: insert child %s\n", XtName(XtParent(w)), XtName(w));
}

Widget
XmCreateScale(Widget parent,
	      char *name,
	      Arg * arglist,
	      Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmScaleWidgetClass,
			  parent,
			  arglist,
			  argcount);
}

void
XmScaleGetValue(Widget widget,
		int *value_return)
{
    *value_return = Scale_Value(widget);
}

void
XmScaleSetValue(Widget widget,
		int value)
{
    Widget sb;
    int newval;

    sb = ((XmScaleWidget)widget)->composite.children[CHILD_SB];
    if (value < Scale_Minimum(widget) || value > Scale_Maximum(widget))
	return;
    Scale_Value(widget) = value;
    newval = _XmScaleConvertScaleValueToSCBValue(widget);
    XtVaSetValues((Widget)sb,
		  XmNvalue, newval,
		  NULL);
    if (Scale_ShowValue(widget))
	showValue(widget, newval, Scale_Value(widget));
}

static void
valueToString(Widget w, char *buf) {
    int base, i;
    char buf2[17]; /* anybody got larger than 64 bit ints? */

    if (Scale_DecimalPoints(w) <= 0)
	sprintf(buf, "%d", Scale_Value(w));
    else {
	/* I'd like to use %d.%*d here, but not everyone has that */
        base = 10;
        for (i = 1; i < Scale_DecimalPoints(w); i++)
            base *= 10;
        sprintf(buf, "%d.", Scale_Value(w)/base);
	for (i = 1; i <= Scale_DecimalPoints(w); i++)
	    strcat(buf, "0");
	sprintf(buf2, "%d", Scale_Value(w)%base);
	strcpy(&buf[strlen(buf) - strlen(buf2)], buf2);
    }
}

/*
 * layout rules
 *
 * scrollbar stays at rightmost or bottommost position, when no label
 * ScaleWidth/ScaleHeight are hints.  If it is set, pay attention to it; otherwise,
 * obey the parent.
 * FIXME - currently, we always allow a widget to resize.
 */
static void
_XmScaleLayout(Widget scale, Boolean ParentResize,
	       Widget child, Boolean TestMode, XtWidgetGeometry *childgeom) {
    Position curx, cury;
    Position *child_x, *child_y;
    Dimension *child_w, *child_h;
    Dimension curw, curh;
    Dimension incw, inch;
    int maxw, maxh;
    XtWidgetGeometry request;

    Widget *children;
    int num_children;
    XtGeometryResult result;
    int i;
    int inc;
    int p_inc;

    /* FIX ME: signs are some of the XtIsManaged tests will have to go. */
    /* FIX ME: if scaleWidth/Height is specified  scrollbar_width/height can
     * get arbitrary small. If width/height is specified slider_width/height
     * is bound to a minimum.
     */
    /* FIX ME: tick mark x/y seems to have a minimum value of -4 */

    /*
     * layout is normal processing as per the M*tif docs
     * (Hmm.. tick marks and title get laid out on opposite sides of SB -- PvH)
     *  per me, this is as follows (FIXME -- this is not complete):
     *    if (orientation is HORIZONTAL) then
     *      layout title (if managed), and other children (if present)
     *        along the bottom edge.  This initially defines our preferred
     *        width, and the tallest child defines the slider's bottom edge.
     *        if (TestMode) then
     *          if the child widget argument is the same as the current child
     *             and the geometry request is a match for the calculated
     *             geometry, the return XtGeometryYes.
     *          else
     *             return XtGeometryNo
     *          endif
     *        endif
     *      layout the slider above the tallest managed child other than the
     *        scrollbar (if any).
     *	      if (ScaleHeight hint is set) then
     *	        we must use that as the slider height
     *        else
     *	        use the default slider height
     *        endif
     *        if (ScaleWidth hint is set)
     *          we must use it as the slider width
     *        else
     *          use our width as the slider width
     *        endif
     *        if (TestMode) then
     *          if the child widget argument is the same as the current child
     *             and the geometry request is a match for the calculated
     *             geometry, the return XtGeometryYes.
     *          else
     *             return XtGeometryNo
     *          endif
     *        endif
     *        layout the value (if ShowValue is true) to be from the top of the
     *          slider. The value should not affect the width calculation.
     *    else if (orientation is VERTICAL) then
     *      layout title (if managed), and other children (if present)
     *        along the right edge.  This initially defines our preferred
     *        height, and the tallest child defines the slider's right edge.
     *        if (TestMode) then
     *          if the child widget argument is the same as the current child
     *             and the geometry request is a match for the calculated
     *             geometry, the return XtGeometryYes.
     *          else
     *             return XtGeometryNo
     *          endif
     *        endif
     *      layout the slider left of the tallest managed child other than the
     *        scrollbar (if any).
     *	      if (ScaleWidth hint is set) then
     *	        we must use that as the slider width
     *        else
     *	        use the default slider width
     *        endif
     *        if (ScaleHeight hint is set)
     *          we must use it as the slider height
     *        else
     *          use our height as the slider height
     *        endif
     *        if (TestMode) then
     *          if the child widget argument is the same as the current child
     *             and the geometry request is a match for the calculated
     *             geometry, the return XtGeometryYes.
     *          else
     *             return XtGeometryNo
     *          endif
     *        endif
     *      layout the value (if ShowValue is true) to be from the right of the
     *        slider. The value should not affect the height calculation.
     *    endif
     *
     *    we have now computed our preferred width and height, or returned.
     *    if (ParentResizeMode) then
     *      do
     *        request size change
     *      while (change not allowed, but close)
     *      our width = new width
     *      our height = new height
     *    else
     *      our width = XtWidth
     *      our height = XtHeight
     *    endif
     *
     *    if (our width < preferred width) then
     *      compute layouts that maximize the display of the scrollbar and
     *      the value (if ShowValue is true)
     *    else
     *      compute proportional layouts
     *    endif
     *    if (our height < preferred height) then
     *      compute layouts that maximize the display of the scrollbar and
     *      the value (if ShowValue is true)
     *    else
     *      compute proportional layouts
     *    endif
     *
     *    for each widget
     *      XmConfigureWidget
     *    endfor
     *
     *  end algorithm
     *
     * My additions:
     * never allow a child to request an XY change. (is this correct?)
     * the ScrollBar shall always be children[1]
     * the LabelGadget shall always be children[0]
     * This is per M*tif!
     */
    curx = cury = 0;
    curw = curh = 0;
    incw = inch = 0;
    maxw = maxh = 0;

    num_children = ((XmScaleWidget)scale)->composite.num_children;
    children = ((XmScaleWidget)scale)->composite.children;

    child_x = (Position*)XtMalloc(sizeof(Position) * num_children);
    child_y = (Position*)XtMalloc(sizeof(Position) * num_children);
    child_w = (Dimension*)XtMalloc(sizeof(Dimension) * num_children);
    child_h = (Dimension*)XtMalloc(sizeof(Dimension) * num_children);

#define	FREE \
	{XtFree((XtPointer)child_x); XtFree((XtPointer)child_y); \
	 XtFree((XtPointer)child_w); XtFree((XtPointer)child_h);}
  
    for (i = 2; i < num_children; i++) {
	if (XtIsManaged(children[i])) {
	    if (maxw < XtWidth(children[i]))
		maxw = XtWidth(children[i]);
	    if (maxh < XtHeight(children[i]))
		maxh = XtHeight(children[i]);

	    child_w[i] = XtWidth(children[i]);
	    child_h[i] = XtHeight(children[i]);
	}
    }

    if (Scale_Orientation(scale) == XmHORIZONTAL) {

	curh = maxh;
	cury = maxh;

	/*
	 * do the value, if there is one
	 */
	if (Scale_ShowValue(scale)) {

	    computeValueSize(scale);

	    incw = Scale_ShowValueWidth(scale);
	    inch = Scale_ShowValueHeight(scale);

	    if (incw > curw)
		curw = incw;
	    curh += inch;

	    cury += inch;
	}

	/*
	 * then do the ScrollBar
	 */
	if (XtIsManaged(children[CHILD_SB])) {

	    child_x[CHILD_SB] = curx;
	    child_y[CHILD_SB] = cury;

	    inch = Scale_ScaleHeight(scale);

	    if (inch == 0)
		inch = SB_TRANSVERSAL_SIZE + 2 * Scale_HighlightThickness(scale);

	    incw = Scale_ScaleWidth(scale);

	    if (incw == 0)
		incw = XtWidth(scale);

	    if (incw == 0 && num_children >= 3) {
		if (num_children == 3)
		    incw = XtWidth(children[2]);
		else
		    incw = HSLIDERSIZE +
			   2 * (Prim_HighlightThickness(children[CHILD_SB]) +
				Prim_ShadowThickness(children[CHILD_SB])) +
			   maxw * (num_children - 3);
	    }

	    if (incw == 0)
		incw = SB_LONGITUDAL_SIZE + 2 * Scale_HighlightThickness(scale);

	    child_w[CHILD_SB] = incw;
	    child_h[CHILD_SB] = inch;

	    if (children[CHILD_SB] == child) {
		if (childgeom->request_mode & CWWidth)
		    childgeom->width = child_w[CHILD_SB];
		if (childgeom->request_mode & CWHeight)
		    childgeom->height = child_h[CHILD_SB];
		FREE;
		return;
	    }

	    if (incw > curw)
		curw = incw;

	    curh += inch;
	    cury += inch;
	}

	/* lay out the tick marks */
	if (num_children > 2) {
	    /* centres are nicely aligned */

	    if (num_children > 3) {
		int spacing;

		if (curw == 0)
		    spacing = maxw * (num_children - 3);
		else
		    spacing = (curw - 
			       (HSLIDERSIZE +
				2 * (Prim_HighlightThickness(children[CHILD_SB]) +
				     Prim_ShadowThickness(children[CHILD_SB]))));

		for (i = 2; i < num_children; i++) {
		    if (XtIsManaged(children[i])) {
			child_y[i] = (maxh - XtHeight(children[i])) / 2; 
			child_x[i] = (HSLIDERSIZE - XtWidth(children[i])) / 2 + 
				     Prim_HighlightThickness(children[CHILD_SB]) +
				     Prim_ShadowThickness(children[CHILD_SB]) +
				     (i - 2) * spacing / (num_children - 3);

			child_h[i] = XtHeight(children[i]);
			child_w[i] = XtWidth(children[i]);

			if (children[i] == child) {
			    if (childgeom->request_mode & CWWidth)
				childgeom->width = child_w[i];
			    if (childgeom->request_mode & CWHeight)
				childgeom->height = child_h[i];
			    FREE;
			    return;
			}
		    }
		}
	    }
	    else {
		if (XtIsManaged(children[2])) {
		    child_y[2] = 0;
		    child_x[2] = (curw - XtWidth(children[2])) / 2;

		    child_h[2] = XtHeight(children[2]);
		    child_w[2] = XtWidth(children[2]);

		    if (children[2] == child) {
			if (childgeom->request_mode & CWWidth)
			    childgeom->width = child_w[2];
			if (childgeom->request_mode & CWHeight)
			    childgeom->height = child_h[2];
			FREE;
			return;
		    }
		}
	    }
	}

	/*
	 * then the title, if there is one
	 */
	if (Scale_Title(scale) &&
	    XtIsManaged(children[CHILD_LAB])) {

	    child_x[CHILD_LAB] = curx;
	    child_y[CHILD_LAB] = cury;

	    _XmCalcLabelGDimensions(children[CHILD_LAB]);
	    incw = XtWidth(children[CHILD_LAB]);
	    inch = XtHeight(children[CHILD_LAB]);

	    child_w[CHILD_LAB] = incw;
	    child_h[CHILD_LAB] = inch;

	    if (children[CHILD_LAB] == child) {
		if (childgeom->request_mode & CWWidth)
		    childgeom->width = child_w[CHILD_LAB];
		if (childgeom->request_mode & CWHeight)
		    childgeom->height = child_h[CHILD_LAB];
		FREE;
		return;
	    }

	    if (incw > curw)
		curw = incw;
	    curh += inch;

	    curx += incw;
	}
    }
    /* XmNorientation == XmVERTICAL */
    else {

	curw = maxw;
	curx = maxw;

	/*
	 * do the value, if there is one
	 */
	if (Scale_ShowValue(scale)) {

	    computeValueSize(scale);

	    inch = Scale_ShowValueHeight(scale);
	    incw = Scale_ShowValueWidth(scale);

	    if (inch > curh)
		curh = inch;
	    curw += incw;

	    curx += incw;
	}

	/*
	 * then do the ScrollBar
	 */
	if (XtIsManaged(children[CHILD_SB])) {

	    child_x[CHILD_SB] = curx;
	    child_y[CHILD_SB] = cury;

	    incw = Scale_ScaleWidth(scale);

	    if (incw == 0)
		incw = SB_TRANSVERSAL_SIZE + 2 * Scale_HighlightThickness(scale);

	    inch = Scale_ScaleHeight(scale);

	    if (inch == 0)
		inch = XtHeight(scale);

	    if (inch == 0 && num_children >= 3) {
		if (num_children == 3)
		    inch = XtHeight(children[2]);
		else
		    /* FIX ME: SrollBar's or Scale's? */
		    inch = HSLIDERSIZE +
			   2 * (Prim_HighlightThickness(children[CHILD_SB]) +
				Prim_ShadowThickness(children[CHILD_SB])) +
			   maxh * (num_children - 3);
	    }

	    if (inch == 0)
		inch = SB_LONGITUDAL_SIZE + 2 * Scale_HighlightThickness(scale);

	    child_w[CHILD_SB] = incw;
	    child_h[CHILD_SB] = inch;

	    if (children[CHILD_SB] == child) {
		if (childgeom->request_mode & CWWidth)
		    childgeom->width = child_w[CHILD_SB];
		if (childgeom->request_mode & CWHeight)
		    childgeom->height = child_h[CHILD_SB];
		FREE;
		return;
	    }

	    if (inch > curh)
		curh = inch;

	    curw += incw;
	    curx += incw;
	}

	/* lay out the tick marks */
	if (num_children > 2) {
	    /* centres are nicely aligned */

	    if (num_children > 3) {
		int spacing;

		if (curh == 0)
		    spacing = maxh * (num_children - 3);
		else
		    spacing = (curh - 
			       (HSLIDERSIZE +
				2 * (Prim_HighlightThickness(children[CHILD_SB]) +
				     Prim_ShadowThickness(children[CHILD_SB]))));

		for (i = 2; i < num_children; i++) {
		    if (XtIsManaged(children[i])) {
			child_x[i] = (maxw - XtWidth(children[i])) / 2; 
			child_y[i] = (HSLIDERSIZE - XtHeight(children[i])) / 2 + 
				     Prim_HighlightThickness(children[CHILD_SB]) +
				     Prim_ShadowThickness(children[CHILD_SB]) +
				     (i - 2) * spacing / (num_children - 3);

			child_w[i] = XtWidth(children[i]);
			child_h[i] = XtHeight(children[i]);

			if (children[i] == child) {
			    if (childgeom->request_mode & CWWidth)
				childgeom->width = child_w[i];
			    if (childgeom->request_mode & CWHeight)
				childgeom->height = child_h[i];
			    FREE;
			    return;
			}
		    }
		}
	    }
	    else {
		if (XtIsManaged(children[2])) {
		    child_x[2] = 0;
		    child_y[2] = (curh - XtHeight(children[2])) / 2;

		    child_w[2] = XtWidth(children[2]);
		    child_h[2] = XtHeight(children[2]);

		    if (children[2] == child) {
			if (childgeom->request_mode & CWWidth)
			    childgeom->width = child_w[2];
			if (childgeom->request_mode & CWHeight)
			    childgeom->height = child_h[2];
			FREE;
			return;
		    }
		}
	    }
	}

	/*
	 * then the title, if there is one
	 */
	if (Scale_Title(scale) &&
	    XtIsManaged(children[CHILD_LAB])) {

	    child_x[CHILD_LAB] = curx;
	    child_y[CHILD_LAB] = cury;


	    _XmCalcLabelGDimensions(children[CHILD_LAB]);
	    incw = XtWidth(children[CHILD_LAB]);
	    inch = XtHeight(children[CHILD_LAB]);

	    child_w[CHILD_LAB] = incw;
	    child_h[CHILD_LAB] = inch;

	    if (children[CHILD_LAB] == child) {
		if (childgeom->request_mode & CWWidth)
		    childgeom->width = child_w[CHILD_LAB];
		if (childgeom->request_mode & CWHeight)
		    childgeom->height = child_h[CHILD_LAB];
		FREE;
		return;
	    }

	    if (inch > curh)
		curh = inch;

	    curw += incw;
	    cury += inch;
	}
    }

    /*
     * if we're being queried for our geometry, return what we've got
     */
    if (child == scale) {
	childgeom->width = curw;
	childgeom->height = curh;
	FREE;
	return;
    }

    /*
     * if we can, change our size to match
     */
    if (ParentResize) {

	request.request_mode = (CWWidth|CWHeight);
	request.width = curw;
	request.height = curh;
	result = _XmMakeGeometryRequest(scale, &request);
	curw = request.width;
	curh = request.height;

	if (result == XtGeometryYes) {
	    XtWidth(scale) = curw;
	    XtHeight(scale) = curh;
	}

	XdbDebug(__FILE__, scale, "_XmScaleLayout %s (%d %d) => %s\n",
		 XtName(scale), curw, curh, XdbGeometryResult2String(result));
    }

    /*
     * do the layout
     */
    if (Scale_Orientation(scale) == XmHORIZONTAL) {
#if 0
	if (Scale_ScaleWidth(scale) == 0)
	    child_w[CHILD_SB] = XtWidth(scale);

	if (child_w[CHILD_SB] < (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale))) {
	    child_w[CHILD_SB] = (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale));
	    child_x[CHILD_SB] = (XtWidth(scale) - child_w[CHILD_SB]) / 2;
	}

	if (num_children == 2) {
	    child_w[CHILD_LAB] = child_w[CHILD_SB];
	    child_x[CHILD_LAB] = child_x[CHILD_SB];
	}

	if (XtHeight(scale) < (child_y[CHILD_SB] + VSLIDERSIZE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale))) {
	    child_y[CHILD_SB] -= (child_y[CHILD_SB] + VSLIDERSIZE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale)) -
				 XtHeight(scale) + 1;
	}

	if (XtWidth(scale) > curw) {
	    for (i = 2; i < num_children; i++){
		child_w[i] = (Dimension)((float)child_w[i] / (float)curw *
					 (float)XtWidth(scale));
	    }
	}
#endif
	if (XtHeight(scale) > curh) {
	    for (i = 0; i < 2; i++)
		child_y[i] = XtHeight(scale) - (curh - child_y[i]);
	}
    }
    else if (Scale_Orientation(scale) == XmVERTICAL) {
#if 0
	if (Scale_ScaleHeight(scale) == 0)
	    child_h[CHILD_SB] = XtHeight(scale);

	if (child_h[CHILD_SB] < (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale))) {
	    child_h[CHILD_SB] = (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale));
	    child_y[CHILD_SB] = (XtHeight(scale) - child_h[CHILD_SB]) / 2;
	}

	if (num_children == 2) {
	    child_h[CHILD_LAB] = child_h[CHILD_SB];
	    child_y[CHILD_LAB] = child_y[CHILD_SB];
	}

	if (XtWidth(scale) < (child_w[CHILD_SB] + VSLIDERSIZE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale))) {
	    child_x[CHILD_SB] -= (child_x[CHILD_SB] + VSLIDERSIZE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale)) -
				 XtWidth(scale) + 1;
	}

	if (XtWidth(scale) > curw) {
	    for (i = 2; i < num_children; i++)
		child_h[i] = (Dimension)((float)child_h[i] / (float)curw *
					 (float)XtWidth(scale));
	}
#endif

	if (XtWidth(scale) > curw) {
	    for (i = 0; i < 2; i++)
		child_x[i] = XtWidth(scale) - (curw - child_x[i]);
	}
    }

    /*
     * if we've gotten here, layout the children
     */
    for (i = 0; i < num_children; i++) {
	if (XtIsManaged(children[i])) {
	    _XmConfigureObject(children[i],
			       child_x[i], child_y[i],
			       child_w[i], child_h[i],
			       0);
	}
    }

    Scale_SliderSize(scale) = _XmScaleConvertWidthToSliderSize(scale);
    inc = (int)((float)(SCB_MAX - SCB_MIN - Scale_SliderSize(scale)) /
		(float)(Scale_Maximum(scale) - Scale_Minimum(scale)) +
		0.5);
    inc = _XmMax(inc, 1); /* FIX ME */
    p_inc = inc * Scale_ScaleMultiple(scale);

    XmScrollBarSetValues(children[CHILD_SB],
			 _XmScaleConvertScaleValueToSCBValue(scale),
			 Scale_SliderSize(scale),
			 inc, 
			 p_inc,
			 False);

    FREE;
}
  
static void
computeValueSize(Widget w) {
    int maxlen, maxval;

    for (maxlen = 1, maxval = Scale_Maximum(w); maxval >= 10; maxlen++)
	maxval /= 10;

    if (Scale_DecimalPoints(w))
	maxlen++;
    Scale_ShowValueWidth(w) = Scale_FontStruct(w)->max_bounds.width *
			      maxlen;
    Scale_ShowValueHeight(w) = Scale_FontStruct(w)->max_bounds.ascent +
			       Scale_FontStruct(w)->max_bounds.descent;
}

static void
showValue(Widget w, int scb_value, int scale_value) {
    char buf[256];
    Widget sb;

    sb = ((XmScaleWidget)w)->composite.children[CHILD_SB];
    valueToString(w, buf);

#if 1
/* Fix from John Richardson <jrichard@zko.dec.com> 1/7/1996 */
    if (XtIsRealized(w)) {
	XFillRectangle(XtDisplay(w), XtWindow(w), MGR_BackgroundGC(w),
		       Scale_ShowValueX(w), Scale_ShowValueY(w),
		       Scale_ShowValueWidth(w), Scale_ShowValueHeight(w));
    }
#else
    if (XtIsRealized(w)) {
	XFillRectangle(XtDisplay(w), XtWindow(w), MGR_BackgroundGC(w),
		       Scale_ShowValueX(w), Scale_ShowValueY(w) - Scale_FontStruct(w)->descent,
		       Scale_ShowValueWidth(w), Scale_ShowValueHeight(w));
    }
#endif

    if (Scale_Orientation(w) == XmHORIZONTAL) {
	Scale_ShowValueX(w) = _XmScrollBarValueToPos(sb, scb_value) +
			      (_XmScrollBarSliderPixSize(sb) / 2) -
			      (Scale_ShowValueWidth(w) / 2) + XtX(sb);
	Scale_ShowValueY(w) = XtY(sb) - Scale_ShowValueHeight(w);
    }
    else if (Scale_Orientation(w) == XmVERTICAL) {
	Scale_ShowValueY(w) = _XmScrollBarValueToPos(sb, scb_value) +
			      (_XmScrollBarSliderPixSize(sb) / 2) -
			      (Scale_ShowValueHeight(w) / 2) + XtY(sb);
	Scale_ShowValueX(w) = XtX(sb) - Scale_ShowValueWidth(w);
    }
    else
	_XmError(w, "Scale Orientation wrong.");

    if (XtIsRealized(w)) {
	int offset;

	offset = (Scale_ShowValueWidth(w) -
		  XTextWidth(Scale_FontStruct(w), buf, strlen(buf))) / 2;
	if (Scale_Orientation(w) == XmHORIZONTAL) {
	    XDrawString(XtDisplay(w), XtWindow(w),
			Scale_ForegroundGC(w),
			Scale_ShowValueX(w) + offset,
			Scale_ShowValueY(w) + Scale_FontStruct(w)->ascent,
			buf, strlen(buf));
	}
	else if (Scale_Orientation(w) == XmVERTICAL) {
	    XDrawString(XtDisplay(w), XtWindow(w),
			Scale_ForegroundGC(w),
			Scale_ShowValueX(w) + offset,
			Scale_ShowValueY(w) + Scale_FontStruct(w)->ascent,
			buf, strlen(buf));
	}
	else
	    _XmError(w, "Scale Orientation wrong");
    }
}

static void
_ScaleValueChanged(Widget sb, XtPointer cd, XtPointer data) {
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)data;
    XmScaleCallbackStruct scbs;
    int scale_value;

    Scale_LastValue(XtParent(sb)) = Scale_Value(sb);
    scale_value = _XmScaleConvertSCBValueToScaleValue(XtParent(sb), cbs->value);
    Scale_Value(XtParent(sb)) = scale_value;
    if (Scale_ShowValue(XtParent(sb)))
	showValue(XtParent(sb), cbs->value, scale_value);

    scbs.reason = cbs->reason;
    scbs.event = cbs->event;
    scbs.value = scale_value;

    if (Scale_ValueChangedCallback(XtParent(sb)))
	XtCallCallbackList(XtParent(sb), Scale_ValueChangedCallback(XtParent(sb)), &scbs);
}
  
static void
_ScaleDrag(Widget sb, XtPointer cd, XtPointer data) {
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)data;
    XmScaleCallbackStruct scbs;
    int scale_value;
  
    Scale_LastValue(XtParent(sb)) = Scale_Value(sb);
    scale_value = _XmScaleConvertSCBValueToScaleValue(XtParent(sb), cbs->value);
    Scale_Value(XtParent(sb)) = scale_value;
    if (Scale_ShowValue(XtParent(sb)))
	showValue(XtParent(sb), cbs->value, scale_value);

    scbs.reason = cbs->reason;
    scbs.event = cbs->event;
    scbs.value = scale_value;
    if (Scale_DragCallback(XtParent(sb)))
        XtCallCallbackList(XtParent(sb), Scale_DragCallback(XtParent(sb)), &scbs);
}

static void
GetFocus(Widget w, XEvent *event, String *params, Cardinal *num_params) {
    XmScrollBarWidget sb;

    sb = (XmScrollBarWidget)(((XmScaleWidget)w)->composite.children[CHILD_SB]);
	XtCallActionProc((Widget)sb, "PrimitiveFocusIn", event, params, *num_params);
}

static void
LoseFocus(Widget w, XEvent *event, String *params, Cardinal *num_params) {
    XmScrollBarWidget sb;

    sb = (XmScrollBarWidget)(((XmScaleWidget)w)->composite.children[CHILD_SB]);
	XtCallActionProc((Widget)sb, "PrimitiveFocusOut", event, params, *num_params);
}

static int
_XmScaleConvertWidthToSliderSize(Widget w) {
    int ret, last_ret;
    XmScrollBarWidget sb;
    Dimension ht;
    Dimension st;

    /* FIX ME: the next two statements are a kludge */
    sb = (XmScrollBarWidget)(((XmScaleWidget)w)->composite.children[CHILD_SB]);

    /* FIX ME: Can't we use scale's values here? I.o.w. what shouls scale do
     * when these values are set explicitly and thus differ from scale's ?
     */
    XtVaGetValues((Widget)sb,
		  XmNhighlightThickness, &ht,
		  XmNshadowThickness, &st,
		  NULL);

    ret = Scale_SliderSize(w);
    do {
        /* FIX ME: rework the if's */
	last_ret = ret;
	if (Scale_Orientation(w) == XmHORIZONTAL) {
    	    if (XtWidth(sb) == 2 * (ht + st) ) {
		/* Note: M*tif 1.2.2 SIGFPE's on this -- PvH */
	        ret = (int)((float)(SCB_MAX - SCB_MIN) /
			    (float)(Scale_Maximum(w) - Scale_Minimum(w)) *
			    (float)HSLIDERSIZE);
	    }
	    else if (XtWidth(sb) < HSLIDERSIZE) {
	        ret = SCB_MAX - SCB_MIN;
	    }
	    else {
		/* Note: M*tif doesn't use float arithmetic here -- PvH */
	        ret = (int)((SCB_MAX - SCB_MIN) /
			    (XtWidth(sb) - 2 * (ht + st)) *
			    HSLIDERSIZE);
	    }
	}
	else {
	    if (XtHeight(sb) == 2 * (ht + st) ) {
		ret = (int)((float)(SCB_MAX - SCB_MIN) /
			    (float)(Scale_Maximum(w) - Scale_Minimum(w)) *
			    (float)HSLIDERSIZE);
	    }
	    else if (XtHeight(sb) < HSLIDERSIZE) {
		ret = SCB_MAX - SCB_MIN;
	    }
	    else {
		/* Note: M*tif doesn't use float arithmetic here -- PvH */
		ret = (int)((SCB_MAX - SCB_MIN) /
			    (XtHeight(sb) - 2 * (ht + st)) *
			    HSLIDERSIZE);
	    }
	}
    } while (last_ret != ret);

    return ret;
}

static int
_XmScaleConvertSCBValueToScaleValue(Widget w, int value) {
    if ((SCB_MAX - SCB_MIN - Scale_SliderSize(w)) == 0)
	return Scale_Minimum(w);
    return (int)((float)(Scale_Maximum(w) - Scale_Minimum(w)) /
		 (float)(SCB_MAX - SCB_MIN - Scale_SliderSize(w)) *
		 (float)value + (float)Scale_Minimum(w) + 0.5);
}

static int
_XmScaleConvertScaleValueToSCBValue(Widget w) {
    return (int)((float)(Scale_Value(w) - Scale_Minimum(w)) /
		 (float)(Scale_Maximum(w) - Scale_Minimum(w)) *
		 (float)(SCB_MAX - SCB_MIN - Scale_SliderSize(w)));
}
