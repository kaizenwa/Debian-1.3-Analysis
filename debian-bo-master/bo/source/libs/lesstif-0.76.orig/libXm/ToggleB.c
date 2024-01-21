/**
 *
 * $Id: ToggleB.c,v 1.10 1996/12/18 00:45:27 miers Exp $
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

static char rcsid[] = "$Id: ToggleB.c,v 1.10 1996/12/18 00:45:27 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ToggleBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <X11/ShellP.h>

#include <XmI/DebugUtil.h>

#include <Xm/ToggleBGP.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

/*
 * Resources for the toggleButton class
 */
#define Offset(field) XtOffsetOf(XmToggleButtonRec, toggle.field)
static XtResource resources[] = {
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
	XmRImmediate,(XtPointer)85 /* don't ask me */
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(spacing),
	XtRImmediate, (XtPointer)4
    },
    {
	XmNselectPixmap, XmCSelectPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(on_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNselectInsensitivePixmap, XmCSelectInsensitivePixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(insen_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNset, XmCSet, XmRBoolean,
	sizeof(Boolean), Offset(set),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNindicatorOn, XmCIndicatorOn, XmRBoolean,
	sizeof(Boolean), Offset(ind_on),
	XtRImmediate,(XtPointer)True
    },
    {
	XmNfillOnSelect, XmCFillOnSelect, XmRBoolean,
	sizeof(Boolean), Offset(fill_on_select),
	XtRImmediate, (XtPointer)85 /* I don't know why */
    },
    {
	XmNselectColor, XmCSelectColor, XmRPixel,
	sizeof(Pixel), Offset(select_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
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
	sizeof(Boolean), XtOffsetOf(XmToggleButtonRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmToggleButtonRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)2
    }
};

static XmSyntheticResource syn_resources[] = {
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

static void Arm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Select(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonUp(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmToggleB_defaultTranslations[] =
    "<EnterWindow>:Enter()\n\
     <LeaveWindow>:Leave()\n\
     <Btn1Down>:Arm()\n\
     <Btn1Up>:Select() Disarm()\n\
     <Btn2Down>:ProcessDrag()\n\
     :<Key>osfActivate:PrimitiveParentActivate()\n\
     :<Key>osfCancel:PrimitiveParentCancel()\n\
     :<Key>osfSelect:ArmAndActivate()\n\
     :<Key>osfHelp:Help()\n\
     ~s ~m ~a <Key>Return:PrimitiveParentActivate()\n\
     ~s ~m ~a <Key>space:ArmAndActivate()";

char _XmToggleB_menuTranslations[] =
    "<EnterWindow>:Enter()\n\
     <LeaveWindow>:Leave()\n\
     <BtnDown>:BtnDown()\n\
     <BtnUp>:BtnUp()\n\
     :<Key>osfSelect:ArmAndActivate()\n\
     :<Key>osfActivate:ArmAndActivate()\n\
     :<Key>osfHelp:Help()\n\
     :<Key>osfCancel:MenuEscape()\n\
     ~s ~m ~a <Key>Return:ArmAndActivate()\n\
     ~s ~m ~a <Key>space:ArmAndActivate()";

static XtTranslations default_trans = NULL;
static XtTranslations menu_trans = NULL;

static XtActionsRec actions[] = {
    {"Arm", Arm},
    {"ArmAndActivate", ArmAndActivate},
    {"Disarm", Disarm},
    {"Select", Select},
    {"Help", Help},
    {"BtnDown", ButtonDown},
    {"BtnUp", ButtonUp},
    {"Enter", EnterWindow},
    {"Leave", LeaveWindow},
};

static XmBaseClassExtRec _XmToggleBCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook,
    /* secondary_object_class    */ XmInheritClass,
    /* secondary_object_create   */ XmInheritSecObjectCreate,
    /* get_secondary_resources   */ XmInheritGetSecResData,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ XmInheritGetValuesPrehook,
    /* get_values_posthook       */ XmInheritGetValuesPosthook,
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

XmPrimitiveClassExtRec _XmToggleBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmToggleButtonClassRec xmToggleButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
        /* class_name            */ "XmToggleButton",
	/* widget_size           */ sizeof(XmToggleButtonRec),
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
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ XtInheritResize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmToggleBCoreClassExtRec
    },
    /* Primitive Class part */
    {
        /* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ XtInheritTranslations,
        /* arm_and_activate_proc */ XmInheritArmAndActivate,
        /* synthetic resources   */ syn_resources,
        /* num syn res           */ XtNumber(syn_resources),
        /* extension             */ (XtPointer)&_XmToggleBPrimClassExtRec
    },
    /* Label Class part */
    {
        /* setOverrideCallback */ XmInheritSetOverrideCallback,
        /* menuProcs           */ XmInheritMenuProc,
        /* translations        */ XtInheritTranslations,
        /* extension           */ NULL
    },
    /* ToggleButton Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmToggleButtonWidgetClass = (WidgetClass)&xmToggleButtonClassRec;


/* 
  Some #defines to make the code below more readable
 */

#define IN_MENU(w) (Lab_MenuType(w) == XmMENU_POPUP || \
                    Lab_MenuType(w) == XmMENU_PULLDOWN)

static void
class_initialize()
{
    menu_trans = XtParseTranslationTable(_XmToggleB_menuTranslations);
    default_trans = XtParseTranslationTable(_XmToggleB_defaultTranslations);

    _XmToggleBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmTOGGLE_BUTTON_BIT);
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
    values.foreground = TB_SelectColor(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;

    TB_SelectGC(w) = XtGetGC(w, mask, &values);
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
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);
    values.fill_style = FillSolid;
    TB_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request,
		   Widget new_w,
		   ArgList args,
		   Cardinal *num_args)
{
    _XmSaveCoreClassTranslations(new_w);

    if (XmIsRowColumn(XtParent(new_w)) &&
	(RC_Type(XtParent(new_w)) == XmMENU_PULLDOWN ||
	 RC_Type(XtParent(new_w)) == XmMENU_POPUP))
	new_w->core.widget_class->core_class.tm_table = (String)menu_trans;
    else 
	new_w->core.widget_class->core_class.tm_table = (String)default_trans;
}

static void
initialize_posthook(Widget request,
		    Widget new_w,
		    ArgList args,
		    Cardinal *num_args)
{
    _XmRestoreCoreClassTranslations(new_w);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    TB_Armed(new_w) = False;

    TB_VisualSet(new_w) = TB_IndicatorSet(new_w) = TB_Set(new_w);

    if (TB_IndOn(new_w))
        TB_IndicatorDim(new_w) = XmDEFAULT_INDICATOR_DIM;
    else
        TB_IndicatorDim(new_w) = 0;
 
    CreateSelectGC(new_w);
    CreateBackgroundGC(new_w);

    if (TB_IndType(new_w) == (unsigned char)XmUNSPECIFIED) {
	if (XmIsRowColumn(XtParent(new_w)) && RC_RadioBehavior(XtParent(new_w)))
	    TB_IndType(new_w) = XmONE_OF_MANY;
	else
	    TB_IndType(new_w) = XmN_OF_MANY;
    }

    /* allocate the normal and insensitive GC's */
    Lab_MarginLeft(new_w) = TB_IndicatorDim(new_w) + (TB_IndOn(new_w)
                               ? 2 * TB_Spacing(new_w)
                               : 0);

    if (Lab_MarginLeft(new_w) != Lab_MarginLeft(request)) {

	if (XtWidth(request) == 0)
	    XtWidth(new_w) = 0;
	if (XtHeight(request) == 0)
	    XtHeight(new_w) = 0;

	_XmCalcLabelDimensions(new_w);

	(*xmLabelClassRec.core_class.resize)(new_w);
    }

    if (IN_MENU(new_w)) {
	Lab_Highlight(new_w) = 0;
	if (Prim_ShadowThickness(new_w) == 0)
	    Prim_ShadowThickness(new_w) = 2;
	TB_Visible(new_w) = False;
    }
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, TB_SelectGC(w));
    XtReleaseGC(w, TB_BackgroundGC(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = False;

    if (TB_SelectColor(new_w) != TB_SelectColor(old)) {
	XtReleaseGC(new_w, TB_SelectGC(new_w));
	CreateSelectGC(new_w);
	refresh_needed = True;
    }
    if (XtBackground(new_w) != XtBackground(old)) {
	XtReleaseGC(new_w, TB_BackgroundGC(new_w));
	CreateBackgroundGC(new_w);
	refresh_needed = True;
    }

    TB_VisualSet(new_w) = TB_IndicatorSet(new_w) = TB_Set(new_w);
    if (TB_Set(old) != TB_Set(new_w))
   	refresh_needed = True;

    if (TB_IndType(old) != TB_IndType(new_w))
    {
      if ((TB_IndType(new_w) != XmN_OF_MANY) && (TB_IndType(new_w) != XmONE_OF_MANY))
      {
         TB_IndType(new_w) = TB_IndType(old);
      }
    }

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    Boolean   State = TB_VisualSet(w);
    Pixmap tmp_pix = XmUNSPECIFIED_PIXMAP, tmp2_pix = XmUNSPECIFIED_PIXMAP;
    
    if (!TB_IndOn(w)) {
	if (TB_FillOnSelect(w)) {
	    XFillRectangle(XtDisplay(w), XtWindow(w),
                           State ? TB_SelectGC(w) : TB_BackgroundGC(w),
                           Lab_Highlight(w) + Lab_Shadow(w),
                           Lab_Highlight(w) + Lab_Shadow(w),
                           XtWidth(w)
				 - 2 * (Lab_Highlight(w) + Lab_Shadow(w)),
                           XtHeight(w)
				 - 2 * (Lab_Highlight(w) + Lab_Shadow(w)));
	}

	if (Lab_IsPixmap(w) && State &&
	    TB_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    tmp_pix = Lab_Pixmap(w);
	    tmp2_pix = Lab_PixmapInsensitive(w);
	    Lab_Pixmap(w) = TB_OnPixmap(w);
	}
#define superclass (&xmLabelClassRec)
	(*superclass->core_class.expose)(w, event, region);
#undef superclass
	if (Lab_IsPixmap(w) && State &&
	    TB_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    Lab_Pixmap(w) = tmp_pix;
	    Lab_PixmapInsensitive(w) = tmp2_pix;
	}

	if (!IN_MENU(w)) {
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   Prim_TopShadowGC(w),
			   Prim_BottomShadowGC(w),
			   Lab_Highlight(w), Lab_Highlight(w),
			   XtWidth(w) - 2 * Lab_Highlight(w),
			   XtHeight(w) - 2 * Lab_Highlight(w),
			   Lab_Shadow(w),
			   State ? XmSHADOW_IN : XmSHADOW_OUT);
	}
	else {
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   Prim_TopShadowGC(w),
			   Prim_BottomShadowGC(w),
			   0, 0,
			   XtWidth(w), XtHeight(w),
			   Lab_Shadow(w),
			   TB_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
	}
    }
    else if (TB_IndType(w) == XmN_OF_MANY){
	if (Lab_IsPixmap(w) && State &&
	    TB_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    tmp_pix = Lab_Pixmap(w);
	    tmp2_pix = Lab_PixmapInsensitive(w);
	    Lab_Pixmap(w) = TB_OnPixmap(w);
	}
#define superclass (&xmLabelClassRec)
	(*superclass->core_class.expose)(w, event, region);
#undef superclass
	if (Lab_IsPixmap(w) && State &&
	    TB_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    Lab_Pixmap(w) = tmp_pix;
	    Lab_PixmapInsensitive(w) = tmp2_pix;
	}

        XFillRectangle(XtDisplay(w), XtWindow(w),
                       State ? TB_SelectGC(w) : TB_BackgroundGC(w),
                       TB_Spacing(w) + Lab_Highlight(w) + Lab_Shadow(w),
                       (XtHeight(w) - TB_IndicatorDim(w)) / 2,
                       TB_IndicatorDim(w),
                       TB_IndicatorDim(w));

	if (TB_Visible(w) || State) {
	    _XmDrawShadows(XtDisplay(w),XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   TB_Spacing(w) + Lab_Highlight(w) + Lab_Shadow(w),
		   (XtHeight(w) - TB_IndicatorDim(w)) / 2,
		   TB_IndicatorDim(w), TB_IndicatorDim(w),
		   2, State ? XmSHADOW_IN : XmSHADOW_OUT);
	}
	if (!IN_MENU(w)) {
	    _XmDrawShadows(XtDisplay(w),XtWindow(w),
			   Prim_TopShadowGC(w),
			   Prim_BottomShadowGC(w),
			   Prim_HighlightThickness(w),
			   Prim_HighlightThickness(w),
			   XtWidth(w) - 2 * Prim_HighlightThickness(w),
			   XtHeight(w) - 2 * Prim_HighlightThickness(w),
			   Lab_Shadow(w),
			   XmSHADOW_OUT);
	}
	else {
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   Prim_TopShadowGC(w),
			   Prim_BottomShadowGC(w),
			   0, 0,
			   XtWidth(w), XtHeight(w),
			   Lab_Shadow(w),
			   TB_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
	}
    }
    else 
    {
	if (Lab_IsPixmap(w) && State &&
	    TB_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    tmp_pix = Lab_Pixmap(w);
	    tmp2_pix = Lab_PixmapInsensitive(w);
	    Lab_Pixmap(w) = TB_OnPixmap(w);
	}
#define superclass (&xmLabelClassRec)
	(*superclass->core_class.expose)(w, event, region);
#undef superclass
	if (Lab_IsPixmap(w) && State &&
	    TB_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
	    Lab_Pixmap(w) = tmp_pix;
	    Lab_PixmapInsensitive(w) = tmp2_pix;
	}

	if (TB_Visible(w) || State) {
	    _XmDrawDiamond(XtDisplay(w), XtWindow(w),
	                   State
				? Prim_TopShadowGC(w)
				: Prim_BottomShadowGC(w),
			   State
				? Prim_BottomShadowGC(w)
				: Prim_TopShadowGC(w),
	                   State ? TB_SelectGC(w) : TB_BackgroundGC(w),
			   TB_Spacing(w) + Lab_Highlight(w),
			   (XtHeight(w) - TB_IndicatorDim(w)) / 2,
			   TB_IndicatorDim(w), TB_IndicatorDim(w),
			   2, True);
	}
	if (!IN_MENU(w)) {
	    _XmDrawShadows(XtDisplay(w),XtWindow(w),
			   Prim_TopShadowGC(w),
			   Prim_BottomShadowGC(w),
			   Prim_HighlightThickness(w),
			   Prim_HighlightThickness(w),
			   XtWidth(w) - 2 * Prim_HighlightThickness(w),
			   XtHeight(w) - 2 * Prim_HighlightThickness(w),
			   Lab_Shadow(w),
			   XmSHADOW_OUT);
	}
	else {
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   Prim_TopShadowGC(w),
			   Prim_BottomShadowGC(w),
			   0, 0,
			   XtWidth(w), XtHeight(w),
			   Lab_Shadow(w),
			   TB_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);
	}
    }
}

static void
Arm(Widget w,
    XEvent *event,
    String *params,
    Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    TB_Armed(w) = True;
    TB_VisualSet(w) = TB_IndicatorSet(w) = !TB_Set(w);

    (exp)(w, event, (Region)NULL);

    if (TB_ArmCallback(w)) {
	cbs.reason = XmCR_ARM;
	cbs.event = event;
	cbs.set = TB_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   TB_ArmCallback(w),
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
	((ev->x >= 0 && ev->x < XtWidth(w)) &&
	 (ev->y >= 0 && ev->y < XtHeight(w)))) {

#if 0
      /* Why is this here?  We're not in a menu here. -- Chris */
	if (XmIsRowColumn(XtParent(w)))
            (*xmLabelClassRec.label_class.menuProcs)(XmMENU_CALLBACK,
                                                     XtParent(w), False,
                                                     w, &cbs);
#endif

	if (TB_VisualSet(w) == TB_IndicatorSet(w)) {

	    TB_Set(w) = TB_VisualSet(w);

	    if (!Lab_SkipCallback(w) && TB_ValueChangedCallback(w)) {
		cbs.reason = XmCR_VALUE_CHANGED;
		cbs.event = event;
		cbs.set = TB_Set(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   TB_ValueChangedCallback(w),
				   (XtPointer)&cbs);
	    }
	}
	else
	    TB_IndicatorSet(w) = TB_Set(w);
    }
}


static void 
Disarm(Widget w,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    TB_Set(w) = TB_IndicatorSet(w) = TB_VisualSet(w);

    if (TB_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TB_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   TB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    TB_Armed(w) = False;
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
ButtonUp(Widget w, 
	 XEvent *event, 
	 String *params, 
	 Cardinal *num_params)
{
    Widget parent,shell;
    Boolean validButton, poppedUp;
    XmToggleButtonCallbackStruct cbs;

    XdbDebug(__FILE__, w, "ToggleB ButtonUp()\n");

    /* queue events until the next button event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    parent = XtParent(w);

    shell = parent;
    while (!XtIsShell(shell))
	shell = XtParent(shell);

    if (event && (event->type == ButtonRelease))
	(*xmLabelClassRec.label_class.menuProcs)(XmMENU_BUTTON, parent,
						 NULL, event, &validButton);

    if (!validButton)
	return;

    if (IN_MENU(w) && !XmIsMenuShell(shell))
	(*xmLabelClassRec.label_class.menuProcs)(XmMENU_POPDOWN, w,
						 NULL, event, &poppedUp);
    else
	(*xmLabelClassRec.label_class.menuProcs)(XmMENU_BUTTON_POPDOWN, w,
						 NULL, event, &poppedUp);

    _XmRecordEvent(event);
    
    if (poppedUp)
	return;

    TB_Armed(w) = False;

    TB_Set(w) = TB_VisualSet(w);

    if (!Lab_SkipCallback(w) && TB_ValueChangedCallback(w)) {
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.event = event;
	cbs.set = TB_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w, 
			   TB_ValueChangedCallback(w), 
			   (XtPointer)&cbs);
    }
    if (TB_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TB_Set(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   TB_DisarmCallback(w),
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
    
    /* queue events until the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    XdbDebug(__FILE__, w, "ToggleB ButtonDown()\n");

    if (event && (event->type == ButtonPress))
    {
	(*xmLabelClassRec.label_class.menuProcs)(XmMENU_BUTTON, XtParent(w),
						 NULL, event, &validButton);
	if (!validButton)
	    return;
    }

    _XmSetInDragMode(w, TRUE);

    TB_Armed(w) = True;
    TB_VisualSet(w) = TB_IndicatorSet(w) = !TB_Set(w);
    (*exp)(w, event, NULL);

    popupShell = (ShellWidget)_XmGetRC_PopupPosted(XtParent(w));
    if  (popupShell)
    {
	if (popupShell->shell.popped_up)
	    (*xmLabelClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN,
						     (Widget)popupShell,
						     NULL, event, NULL);
	
	child = ((XmManagerWidget)XtParent(w))->manager.active_child;
	if (child && (XmIsCascadeButton(child) || XmIsCascadeButtonGadget(child)))
	    XmCascadeButtonHighlight(child, FALSE);
    }
    
    _XmSetInDragMode(w, FALSE);

    _XmRecordEvent(event);
}

static void 
Help(Widget w,
     XEvent *event,
     String *params,
     Cardinal *num_params)
{
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

static void
EnterWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "ToggleB Enter\n");

    if (!IN_MENU(w))
    {
	_XmPrimitiveLeave(w, event, NULL, NULL);
	if (TB_Armed(w))
	{
            TB_VisualSet(w) = TB_IndicatorSet(w);
	    (*exp)(w, NULL, (Region)NULL);
	}
    }
    else /* In menu */
    {
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

	    TB_Armed(w) = True; 

	    TB_VisualSet(w) = TB_VisualSet(w) = !TB_Set(w);

	    (*exp)(w, event, NULL);

	    if (TB_ArmCallback(w)) {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.set = TB_Set(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   TB_ArmCallback(w),
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
	_XmPrimitiveLeave(w, event, NULL, NULL);
	if (TB_Armed(w))
	{
            TB_VisualSet(w) = TB_Set(w);
	    (*exp)(w, NULL, (Region)NULL);
	}
    }
    else /* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
            TB_VisualSet(w) = TB_Set(w);

	    TB_Armed(w) = False;

	    (*exp)(w, event, NULL);

	    if (TB_DisarmCallback(w)) {
		cbs.reason = XmCR_DISARM;
		cbs.event = event;
		cbs.set = TB_Set(w);

		XFlush(XtDisplay(w));
    
		XtCallCallbackList(w,
				   TB_DisarmCallback(w),
				   (XtPointer)&cbs);
	    }
	}
    }
}

Widget
XmCreateToggleButton(Widget parent,
		     char *name,
		     Arg *arglist,
		     Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmToggleButtonWidgetClass,
			  parent,
			  arglist,
			  argcount);
}

Boolean
XmToggleButtonGetState(Widget widget)
{
    if (XmIsToggleButtonGadget(widget))
	return XmToggleButtonGadgetGetState(widget);
    else if (XmIsToggleButton(widget))
	return TB_Set(widget);
    return False;
}

void
XmToggleButtonSetState(Widget widget, 
		       Boolean state,
		       Boolean notify)
{
    XmToggleButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(widget)->core_class.expose;

    if (XmIsGadget(widget)) {
	XmToggleButtonGadgetSetState(widget, state, notify);
	return;
    }
    if (!XmIsToggleButton(widget))
	return;

    if ((TB_Set(widget) && !state) || (!TB_Set(widget) && state)) 
    {
       TB_VisualSet(widget) = TB_Set(widget) = state ? True : False;
        if (notify && TB_ValueChangedCallback(widget)) 
        {
	    cbs.reason = XmCR_VALUE_CHANGED;
	    cbs.event = NULL;
	    cbs.set = state;

	    XFlush(XtDisplay(widget));

	    XtCallCallbackList(widget,
			       TB_ValueChangedCallback(widget),
			       (XtPointer)&cbs);
        }

	if (XtIsRealized(widget))
	    (*exp)(widget, NULL, (Region)NULL);
    }
}
