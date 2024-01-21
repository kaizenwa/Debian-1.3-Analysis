/**
 *
 * $Id: Separator.c,v 1.6 1996/12/18 02:35:22 miers Exp $
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

static char rcsid[] = "$Id: Separator.c,v 1.6 1996/12/18 02:35:22 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/SeparatorP.h>

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
 * Resources for the separator class
 */
#define Offset(field) XtOffsetOf(XmSeparatorRec, separator.field)
static XtResource resources[] = {
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
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmSeparatorRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmSeparatorRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)0
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNmargin,
	sizeof(Dimension), Offset(margin),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);

static char defaultTranslations[] = 
    "<Key>osfHelp:  Help() ";

static XtActionsRec actions[] = {
    {"Help", Help},
};

static XmBaseClassExtRec _XmSeparatorCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmSeparatorPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmSeparatorClassRec xmSeparatorClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name            */ "XmSeparator",
	/* widget_size           */ sizeof(XmSeparatorRec),
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
	/* resize                */ NULL,
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
	/* extension             */ (XtPointer)&_XmSeparatorCoreClassExtRec
    },
    /* Primitive Class part */
    {
        /* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ XtInheritTranslations,
        /* arm_and_activate_proc */ XmInheritArmAndActivate,
        /* Synthetic Resources   */ syn_resources, 
        /* num syn res           */ XtNumber(syn_resources),
        /* extension             */ (XtPointer)&_XmSeparatorPrimClassExtRec
    },
    /* Separator Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmSeparatorWidgetClass = (WidgetClass)&xmSeparatorClassRec;

static void
class_initialize()
{
    _XmSeparatorCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSEPARATOR_BIT);
}

static void
CreateSeparatorGC(Widget w)
{
    XGCValues values;
    XtGCMask  valueMask;

    valueMask =  GCBackground | GCLineStyle | GCForeground | GCJoinStyle |
			GCCapStyle;

    switch (SEP_SeparatorType(w)) {
    case XmSINGLE_LINE:
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmSINGLE_DASHED_LINE:
	values.line_style = LineDoubleDash;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmDOUBLE_LINE:
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmDOUBLE_DASHED_LINE:
	values.line_style = LineDoubleDash;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmSHADOW_ETCHED_IN:
	/* doesn't really matter */
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmSHADOW_ETCHED_OUT:
	/* doesn't really matter */
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmSHADOW_ETCHED_IN_DASH:
	/* doesn't really matter */
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmSHADOW_ETCHED_OUT_DASH:
	/* doesn't really matter */
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    default:
	SEP_SeparatorType(w) = XmSINGLE_LINE;
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;
    }

    SEP_SeparatorGC(w) = XtGetGC(w, valueMask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    if (SEP_Orientation(new_w) == XmVERTICAL)
    {
	XtWidth(new_w) = Prim_ShadowThickness(new_w);
    }
    if (SEP_Orientation(new_w) == XmHORIZONTAL)
    {
	XtHeight(new_w) = Prim_ShadowThickness(new_w);
    }

    CreateSeparatorGC(new_w);
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, SEP_SeparatorGC(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = False;

    if (Prim_Foreground(new_w) != Prim_Foreground(old) ||
	XtBackground(new_w) != XtBackground(old) ||
	SEP_SeparatorType(new_w) != SEP_SeparatorType(old)) {
	XtReleaseGC(new_w, SEP_SeparatorGC(new_w));
	CreateSeparatorGC(new_w);
	refresh_needed = True;
    }
    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    _XmDrawSeparator(XtDisplay(w), XtWindow(w),
		     Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
		     SEP_SeparatorGC(w),
		     0,0,
		     XtWidth(w), XtHeight(w),
		     Prim_ShadowThickness(w),
		     SEP_Margin(w),
		     SEP_Orientation(w),
		     SEP_SeparatorType(w));
}

static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    answer->request_mode = CWWidth | CWHeight;

    switch (SEP_Orientation(w))
    {
    case XmHORIZONTAL:
	answer->width = XtWidth(w);
	answer->height = Prim_ShadowThickness(w);
	break;
    case XmVERTICAL:
	answer->height = XtHeight(w);
	answer->width = Prim_ShadowThickness(w);
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

static void 
Help(Widget w,
     XEvent *event,
     String *params,
     Cardinal *num_params)
{
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

Widget 
XmCreateSeparator(Widget parent, 
		  char *name, 
		  Arg *arglist, 
		  Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmSeparatorWidgetClass,
			  parent,
			  arglist,
			  argcount);
}
