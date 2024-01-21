/**
 *
 * $Id: PushB.c,v 1.13 1997/01/06 06:50:18 u27113 Exp $
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

static char rcsid[] = "$Id: PushB.c,v 1.13 1997/01/06 06:50:18 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/PushBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/TransltnsP.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>

#include <XmI/DebugUtil.h>

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
 * Resources for the pushButton class
 */
#define Offset(field) XtOffsetOf(XmPushButtonRec, pushbutton.field)
static XtResource resources[] = {
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNfillOnArm, XmCFillOnArm, XmRBoolean,
	sizeof(unsigned char), Offset(fill_on_arm),
	XtRImmediate, (XtPointer)True
    },
    {
	XmNarmColor, XmCArmColor, XmRPixel,
	sizeof(Pixel), Offset(arm_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
    },
    {
	XmNarmPixmap, XmCArmPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(arm_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNshowAsDefault, XmCShowAsDefault, XmRBooleanDimension,
	sizeof(Dimension), Offset(show_as_default),
	XtRImmediate, (XtPointer)0
    },
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
	sizeof(Dimension), XtOffsetOf(XmPushButtonRec, primitive.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNdefaultButtonShadowThickness, XmCDefaultButtonShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    /* Resources redefined from Primitive/Label */
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmPushButtonRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPushButtonRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNshowAsDefault,
	sizeof(Dimension), Offset(show_as_default),
	NULL /* FIXME */, NULL /* FIXME */
    },
    {
	XmNdefaultButtonShadowThickness,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNhighlightThickness,
	sizeof(Dimension), XtOffsetOf(XmPushButtonRec, primitive.highlight_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

static void Arm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Activate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Help(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonUp(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void MultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmPushB_defaultTranslations[] = 
   "<Key>osfSelect:       ArmAndActivate()\n\
    <Key>osfHelp:         Help()\n\
    <Key>osfActivate:     PrimitiveParentActivate()\n\
    <EnterWindow>:        Enter()\n\
    <LeaveWindow>:        Leave()\n\
    <Btn1Down>:           Arm()\n\
    <Btn1Down>,<Btn1Up>:  Activate() Disarm()\n\
    <Btn1Down>(2+):       MultiArm()\n\
    <Btn1Up>(2+):         MultiActivate()\n\
    <Btn1Up>:             Activate() Disarm()\n\
    <Btn2Down>:           ProcessDrag()\n\
    ~s ~m ~a <Key>Return: PrimitiveParentActivate()\n\
    ~s ~m ~a <Key>space:  ArmAndActivate()";

char _XmPushB_menuTranslations[] =
   "<EnterWindow>:        Enter()\n\
    <LeaveWindow>:        Leave()\n\
    <BtnDown>:            BtnDown()\n\
    <BtnUp>:              BtnUp()\n\
    <Key>osfSelect:       ArmAndActivate()\n\
    <Key>osfActivate:     ArmAndActivate()\n\
    <Key>osfCancel:       MenuEscape()\n\
    <Key>osfHelp:         Help()\n\
    ~s ~m ~a <Key>Return: ArmAndActivate()\n\
    ~s ~m ~a <Key>space:  ArmAndActivate()";

static XtTranslations default_trans = NULL;
static XtTranslations menu_trans = NULL;

static XtActionsRec actions[] = {
    {"Arm", Arm},
    {"MultiArm", MultiArm},
    {"Activate", Activate},
    {"MultiActivate", MultiActivate},
    {"ArmAndActivate", ArmAndActivate},
    {"Disarm", Disarm},
    {"BtnDown", ButtonDown},
    {"BtnUp", ButtonUp},
    {"Enter", EnterWindow},
    {"Leave", LeaveWindow},
    {"Help", Help},
};

static XmBaseClassExtRec _XmPushBCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmPushBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmPushButtonClassRec xmPushButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
        /* class_name            */ "XmPushButton",
	/* widget_size           */ sizeof(XmPushButtonRec),
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
	/* extension             */ (XtPointer)&_XmPushBCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
       	/* border_unhighlight    */ XmInheritBorderUnhighlight,
       	/* translations          */ XtInheritTranslations,
       	/* arm_and_activate_proc */ ArmAndActivate,
       	/* synthetic resources   */ syn_resources, 
        /* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmPushBPrimClassExtRec
    },
    /* Label Class part */
    {
        /* setOverrideCallback */ XmInheritSetOverrideCallback,
        /* menuProcs           */ XmInheritMenuProc,
        /* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* PushButton Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmPushButtonWidgetClass = (WidgetClass)&xmPushButtonClassRec;

/* 
  Some #defines to make the code below more readable
 */

#define IN_MENU(w) (Lab_MenuType(w) == XmMENU_POPUP || \
		    Lab_MenuType(w) == XmMENU_PULLDOWN)

static void
class_initialize()
{
    menu_trans = XtParseTranslationTable(_XmPushB_menuTranslations);
    default_trans = XtParseTranslationTable(_XmPushB_defaultTranslations);

    _XmPushBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmPUSH_BUTTON_BIT);
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
    values.foreground = PB_ArmColor(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;

    PB_FillGC(w) = XtGetGC(w, mask, &values);
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

    PB_BackgroundGC(w) = XtGetGC(w, mask, &values);
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
    int		margin, margin_extra;
    
    if (!Lab_Font(new_w))
	Lab_Font(new_w) = _XmGetDefaultFontList(new_w,
					      XmBUTTON_FONTLIST);
    
    PB_Armed(new_w) = False;

    CreateFillGC(new_w);
    CreateBackgroundGC(new_w);

    if (Lab_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	PB_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP) {

	Lab_Pixmap(new_w) = PB_ArmPixmap(new_w);

	if (XtWidth(request) == 0)
	    XtWidth(new_w) = 0;
	if (XtHeight(request) == 0)
	    XtHeight(new_w) = 0;

	_XmCalcLabelDimensions(new_w);

	(*xmLabelClassRec.core_class.resize)(new_w);
    }

    PB_UnarmPixmap(new_w) = Lab_Pixmap(new_w);

    if (IN_MENU(new_w))
    {
	Lab_Highlight(new_w) = 0;
    }
    else
    {
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
	if (PB_DefaultButtonShadow(new_w) > 0)
	    PB_Compatible(new_w) = False;
	else
	    PB_Compatible(new_w) = True;

	if (PB_Compatible(new_w))
	    PB_DefaultButtonShadow(new_w) = PB_ShowAsDefault(new_w);

	if (PB_DefaultButtonShadow(new_w)) {
	    margin = 2 * PB_DefaultButtonShadow(new_w) + Lab_Shadow(new_w);
	    margin_extra = Xm3D_ENHANCE_PIXEL;

	    Lab_MarginLeft(new_w) = margin + margin_extra;
	    Lab_MarginRight(new_w) = margin + margin_extra;
	    Lab_MarginTop(new_w) = margin + margin_extra;
	    Lab_MarginBottom(new_w) = margin + margin_extra;

	    XtWidth(new_w) += (margin + margin_extra) * 2;
	    XtHeight(new_w) += 2 * margin + margin_extra;

	    (*xmLabelClassRec.core_class.resize)(new_w);
	}
    }
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, PB_FillGC(w));
    XtReleaseGC(w, PB_BackgroundGC(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = True;
    Dimension margin, margin_extra;

    XdbDebug(__FILE__, new_w, "set_values()\n");

    if (PB_ArmColor(new_w) != PB_ArmColor(old)) {
	XtReleaseGC(new_w, PB_FillGC(new_w));
	CreateFillGC(new_w);
	refresh_needed = True;
    }
    if (XtBackground(new_w) != XtBackground(old)) {
	XtReleaseGC(new_w, PB_BackgroundGC(new_w));
	CreateBackgroundGC(new_w);
	refresh_needed = True;
    }

    if (!IN_MENU(new_w)) {
	if (PB_DefaultButtonShadow(new_w) != PB_DefaultButtonShadow(old))
	    PB_Compatible(new_w) = False;

	if (PB_Compatible(new_w))
	    PB_DefaultButtonShadow(new_w) = PB_ShowAsDefault(new_w);

	if (PB_DefaultButtonShadow(new_w) != PB_DefaultButtonShadow(old)) {
	    margin = 2 * PB_DefaultButtonShadow(new_w) + Lab_Shadow(new_w);
	    margin_extra = Xm3D_ENHANCE_PIXEL;

	    Lab_MarginLeft(new_w) = margin + margin_extra;
	    Lab_MarginRight(new_w) = margin + margin_extra;
	    Lab_MarginTop(new_w) = margin + margin_extra;
	    Lab_MarginBottom(new_w) = margin + margin_extra;

	    XtWidth(new_w) += (margin + margin_extra) * 2;
	    XtHeight(new_w) += 2 * margin + margin_extra;

	    (*xmLabelClassRec.core_class.resize)(new_w);

	    refresh_needed = True;
	}
    }

    if (PB_ArmPixmap(new_w) != PB_ArmPixmap(old) &&
	Lab_IsPixmap(new_w) && PB_Armed(new_w)) {
	refresh_needed = True;
    }

    if (Lab_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
		PB_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP) {

	Lab_Pixmap(new_w) = PB_ArmPixmap(new_w);

	if (Lab_RecomputeSize(new_w) && XtWidth(request) == 0)
	    XtWidth(new_w) = 0;
	if (Lab_RecomputeSize(new_w) && XtHeight(request) == 0)
	    XtHeight(new_w) = 0;

	_XmCalcLabelDimensions(new_w);

	(*xmLabelClassRec.core_class.resize)(new_w);

	refresh_needed = True;
    }

    if (Lab_Pixmap(new_w) != Lab_Pixmap(old)) {
	PB_UnarmPixmap(new_w) = Lab_Pixmap(new_w);
	if (Lab_IsPixmap(new_w) && !PB_Armed(new_w))
	    refresh_needed = True;
    }

    if (Lab_IsPixmap(new_w) && PB_Armed(new_w) &&
		PB_ArmPixmap(new_w) != PB_ArmPixmap(old)) {

	if (Lab_RecomputeSize(new_w) && XtWidth(request) == 0)
	    XtWidth(new_w) = 0;
	if (Lab_RecomputeSize(new_w) && XtHeight(request) == 0)
	    XtHeight(new_w) = 0;

	_XmCalcLabelDimensions(new_w);

	(*xmLabelClassRec.core_class.resize)(new_w);

	refresh_needed = True;
    }

    if (PB_FillOnArm(new_w) != PB_FillOnArm(old) && PB_Armed(new_w))
	refresh_needed = True;

    if (XtIsRealized(new_w) && !refresh_needed)
    {
	Position normal_shadow_x, normal_shadow_y;
	Dimension normal_shadow_width, normal_shadow_height, shad;

	normal_shadow_x = Lab_Highlight(new_w) + Lab_MarginLeft(new_w);
	normal_shadow_y = Lab_Highlight(new_w) + Lab_MarginTop(new_w);
    
	normal_shadow_width = XtWidth(new_w) - 2 * Lab_Highlight(new_w) -
				Lab_MarginLeft(new_w) - Lab_MarginRight(new_w);
	normal_shadow_height = XtHeight(new_w) - 2 * Lab_Highlight(new_w) - 
				Lab_MarginTop(new_w) - Lab_MarginBottom(new_w);

	shad = PB_DefaultButtonShadow(new_w);

	if (PB_ShowAsDefault(new_w) && !PB_ShowAsDefault(old))
	{
	    _XmDrawShadows(XtDisplay(new_w), XtWindow(new_w),
			   Prim_TopShadowGC(new_w),
			   Prim_BottomShadowGC(new_w),
			   normal_shadow_x - (2 * shad + Lab_Shadow(new_w)),
			   normal_shadow_y - (2 * shad + Lab_Shadow(new_w)),
			   normal_shadow_width + 2 * (2 * shad + Lab_Shadow(new_w)),
			   normal_shadow_height + 2 * (2 * shad + Lab_Shadow(new_w)),
			   shad,
			   XmSHADOW_IN);
	}
	else
	{
	    _XmClearBorder(XtDisplay(new_w), XtWindow(new_w),
			   normal_shadow_x - (2 * shad + Lab_Shadow(new_w)),
			   normal_shadow_y - (2 * shad + Lab_Shadow(new_w)),
			   normal_shadow_width + 2 * (2 * shad + Lab_Shadow(new_w)),
			   normal_shadow_height + 2 * (2 * shad + Lab_Shadow(new_w)),
			   shad);
	}
    }

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    /* these make it easier to deal with the default button stuff */
    Dimension normal_shadow_x, normal_shadow_y;
    Dimension normal_shadow_width, normal_shadow_height;
    XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(w);

    if (!XtIsRealized(w))
	return;

    XdbDebug(__FILE__, w, "PB expose\n");

    if (PB_DefaultButtonShadow(w) || PB_ShowAsDefault(w)) {
	normal_shadow_x = Lab_Highlight(w) + Lab_MarginLeft(w);
	normal_shadow_y = Lab_Highlight(w) + Lab_MarginTop(w);
    
	normal_shadow_width = XtWidth(w) - 2 * Lab_Highlight(w) -
				Lab_MarginLeft(w) - Lab_MarginRight(w);
	normal_shadow_height = XtHeight(w) - 2 * Lab_Highlight(w) - 
				Lab_MarginTop(w) - Lab_MarginBottom(w);
    }
    else {
	normal_shadow_x = Lab_Highlight(w);
	normal_shadow_y = Lab_Highlight(w);
    
	normal_shadow_width = XtWidth(w) - 2 * Lab_Highlight(w);
	normal_shadow_height = XtHeight(w) - 2 * Lab_Highlight(w);
    }

    XdbDebug(__FILE__, w,
	     "x %d y %d w %d h %d b %d\n", normal_shadow_x, normal_shadow_y,
	     normal_shadow_width, normal_shadow_height, XtBorderWidth(w));

    if (!IN_MENU(w))
    {
	/*
	 * this was badly wrong, and relied on label to _NOT_ overwrite
	 * PB shadows.  The correct order is: background, label, shadows.
	 *
	 * Chris:  No.  If you look at the output of xscope, you'll see 
	 * that the label's expose routine is called after the pushbuttons.
	 * In general this is the way it's done -- you call your expose
	 * routine, and then call your superclass's.
	 */
	if (!PB_Armed(w))
	    XFillRectangle(XtDisplay(w), XtWindow(w),
			   PB_BackgroundGC(w),
			   0, 0, XtWidth(w), XtHeight(w));

	if (PB_Armed(w) && PB_FillOnArm(w) && !Lab_IsPixmap(w))
	{
	    XFillRectangle(XtDisplay(w), XtWindow(w),
			   PB_FillGC(w),
			   normal_shadow_x + Prim_ShadowThickness(w),
			   normal_shadow_y + Prim_ShadowThickness(w),
			   normal_shadow_width - 2 * Prim_ShadowThickness(w),
			   normal_shadow_height - 2 * Prim_ShadowThickness(w));
	}

	if (Lab_IsPixmap(w)) {
	    if (PB_Armed(w) && PB_ArmPixmap(w) != XmUNSPECIFIED_PIXMAP)
		Lab_Pixmap(w) = PB_ArmPixmap(w);
	    else
		Lab_Pixmap(w) = PB_UnarmPixmap(w);
	}

	/* now draw the normal shadow */
	_XmDrawShadows(XtDisplay(w), XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       normal_shadow_x,
		       normal_shadow_y,
		       normal_shadow_width,
		       normal_shadow_height,
		       Lab_Shadow(w),
		       PB_Armed(w) ? XmSHADOW_IN : XmSHADOW_OUT);

	/* take care of the default button stuff */
	if (PB_ShowAsDefault(w) > 0)
	{
	    Dimension shad;

 	    shad = PB_DefaultButtonShadow(w);

	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   Prim_TopShadowGC(w),
			   Prim_BottomShadowGC(w),
			   normal_shadow_x - (2 * shad + Lab_Shadow(w)),
			   normal_shadow_y - (2 * shad + Lab_Shadow(w)),
			   normal_shadow_width + 2 * (2 * shad + Lab_Shadow(w)),
			   normal_shadow_height + 2 * (2 * shad + Lab_Shadow(w)),
			   shad,
			   XmSHADOW_IN);
	}

#define superclass (&xmLabelClassRec)
	(*superclass->core_class.expose)(w, event, region);
#undef superclass

	if (Prim_Highlighted(w))
	    (*pwc->primitive_class.border_highlight)(w);
	else
	    (*pwc->primitive_class.border_unhighlight)(w);
    }
    else
    {
	if (Lab_IsPixmap(w)) {
	    if (PB_Armed(w) && PB_ArmPixmap(w) != XmUNSPECIFIED_PIXMAP)
		Lab_Pixmap(w) = PB_ArmPixmap(w);
	    else
		Lab_Pixmap(w) = PB_UnarmPixmap(w);
	}
	_XmDrawShadows(XtDisplay(w), 
		       XtWindow(w), 
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       0,
		       0,
		       XtWidth(w),
		       XtHeight(w),
		       Lab_Shadow(w),
		       PB_Armed(w) ? (int)XmSHADOW_OUT : (int)XmNO_LINE);

#define superclass (&xmLabelClassRec)
	(*superclass->core_class.expose)(w, event, region);
#undef superclass
    }
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

    PB_Armed(w) = True;
    PB_ArmTimeStamp(w) = event->xbutton.time;

    (*exp)(w, event, NULL);

    if (PB_ArmCallback(w)) {
	cbs.reason = XmCR_ARM;
	cbs.event = event;
	cbs.click_count = PB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_ArmCallback(w),
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

    XdbDebug(__FILE__, w, "Activate()\n");

    /*
     * This test also broke Accelerators. Refined as well.
     * Guys, please watch out !!
     * Danny 23/5/1996
     */
    if ((ev->type == ButtonPress || ev->type == ButtonRelease) &&
	PB_Armed(w) == False)
	return;

    PB_ClickCount(w) = 1;
    PB_Armed(w) = False;

    (*exp)(w, event, NULL);

    /*
     * This test should not be necessary.
     * It happens to break accelerators that trigger a button.
     * Danny 5/4/96
     * MLM: Not having means PB's break if the button is released outside
     * the widget after it is armed. Check testXm/pushbutton/test1.  Arm
     * (click) on the button, move outside the button, and release.  If this
     * isn't here, the arm callback will be executed (and it shouldn't be).
     *
     * Test refined so it doesn't fail for accelerators. -- Danny
     */
    if (ev->type == KeyPress || ev->type == KeyRelease
	|| ((ev->x >= 0 && ev->x < XtWidth(w))
	&& (ev->y >= 0 && ev->y < XtHeight(w))))
    {
	if (!Lab_SkipCallback(w) && PB_ActivateCallback(w)) {
	    cbs.reason = XmCR_ACTIVATE;
	    cbs.event = event;
	    cbs.click_count = PB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PB_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
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

    if (PB_Armed(w)) {
	PB_Armed(w) = False;
	(*exp)(w, event, NULL);
    }

    if (PB_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PB_ClickCount(w);
 
	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    XmPushButtonCallbackStruct cbs;
    Widget w = (Widget)data;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "Activate\n");

    if (PB_Armed(w) == False)
	return;

    PB_Armed(w) = False;

    (*exp)(w, NULL, NULL);

    if (!Lab_SkipCallback(w) && PB_ActivateCallback(w)) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = NULL;
	cbs.click_count = PB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_ActivateCallback(w),
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
    Arm(w, event, params, num_params);

    PB_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				  ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
}

static void 
Help(Widget w,
     XEvent *event,
     String *params,
     Cardinal *num_params)
{
  /* unpost menus */

  /* restore focus */

  /* invoke help callbacks */
  XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

static void
EnterWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "PushB Enter\n");

    if (!IN_MENU(w))
    {
        _XmPrimitiveEnter(w, event, NULL, NULL);    

	if (PB_Armed(w))
	{
	    (*exp)(w, event, NULL);
	}
    }
    else /* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    Widget popupShell;
	    Widget popupPosted = _XmGetRC_PopupPosted(XtParent(w));

	    if (popupPosted)
            {
  	        popupShell = XtParent(popupPosted);
	      
		(*xmLabelClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN, 
							 (Widget)popupShell, 
							 NULL, 
							 event, 
							 NULL);
	    }

	    PB_Armed(w) = True; 

	    (*exp)(w, event, NULL);

	    if (PB_ArmCallback(w)) {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.click_count = PB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PB_ArmCallback(w),
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
	_XmPrimitiveLeave(w, event, NULL, NULL);    
	if (PB_Armed(w))
	{
	    (*exp)(w, event, NULL);
	}
    }
    else /* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    PB_Armed(w) = False;

	    (*exp)(w, event, NULL);

	    if (PB_DisarmCallback(w)) {
		cbs.reason = XmCR_DISARM;
		cbs.event = event;
		cbs.click_count = PB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PB_DisarmCallback(w),
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

    if (!PB_Armed(w))
	return;

    PB_Armed(w) = False;

    XdbDebug(__FILE__, w, "Lab_MenuType(w) = %d\n", Lab_MenuType(w));

    if (IN_MENU(w) && !XmIsMenuShell(shell)) 
    {
      /* If it's not a menu shell then it could be a tearoff */
        Widget menu = XtParent(w);
	
	if (XmIsRowColumn(menu) 
	    && RC_ParentShell(menu) 
	    && RC_ParentShell(menu) != shell) 
	{
	    /* What is there to do here ? FIX ME */
	    poppedUp = False;
	} 
	else
	  (*xmLabelClassRec.label_class.menuProcs)(XmMENU_POPDOWN,
						   w, NULL, event, &poppedUp);
    } 
    else 
    {	/* In a menu, and the shell is a menu shell */
        (*xmLabelClassRec.label_class.menuProcs)(XmMENU_BUTTON_POPDOWN,
						 w, NULL, event, &poppedUp);
    }

    _XmRecordEvent(event);
    
    if (poppedUp) {
	XdbDebug(__FILE__, w, "ButtonUp: was not popped up\n");
#if 0
/* Why ?? */
	return;
#endif
    }

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   Lab_Highlight(w),
		   Lab_Highlight(w),
		   XtWidth(w) - 2 * Lab_Highlight(w),
		   XtHeight(w) - 2 * Lab_Highlight(w),
		   Lab_Shadow(w),
		   XmNO_LINE);

    if (!Lab_SkipCallback(w) && PB_ActivateCallback(w)) {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = PB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_ActivateCallback(w),
			   &cbs);
    }
    if (PB_DisarmCallback(w)) {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_DisarmCallback(w),
			   &cbs);
    }

    _XmSetInDragMode(w, False);

    /* Try to do something about option menus */
    if (Lab_MenuType(w) == XmMENU_PULLDOWN) {
#if 0
/*
 * Don't understand what's going on here.
 * In option menus, RC_CascadeBtn seems to be NULL.
 * Oh well, nothing we can do here before fixing *that*.
 * Danny 5/1/1997
 */
	Widget	cb = RC_CascadeBtn(XtParent(w));
	Widget	mb = XtParent(cb);

	XdbDebug(__FILE__, w, "ButtonUp: CB %s MB %s\n",
		XtName(cb), XtName(mb));

	if (RC_Type(mb) == XmMENU_OPTION) {
	    /*
	     * Because of this, RowColumn will set the labelString on
	     * the cascade button.
	     */
	    XtVaSetValues(mb, XmNmenuHistory, w, NULL);
	}
#endif
    }
}

static void 
ButtonDown(Widget w, 
	   XEvent *event, 
	   String *params, 
	   Cardinal *num_params)
{
    /* modified from the MegaButton widget */
    Widget popupShell;
    Widget popupPosted = _XmGetRC_PopupPosted(XtParent(w));
    int validButton;
    XmPushButtonCallbackStruct cbs;
    
    /* queue events until the next button event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);
    
    if (event && (event->type == ButtonPress))
    {
	(*xmLabelClassRec.label_class.menuProcs)(XmMENU_BUTTON, 
						 XtParent(w), 
						 NULL, 
						 event, 
						 &validButton);
	if (!validButton)
	    return;
    }

    _XmSetInDragMode(w, True);

    if (popupPosted)
    {
        popupShell = XtParent(popupPosted);
      
	(*xmLabelClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN, 
						 (Widget)popupShell, 
						 NULL, 
						 event, 
						 NULL);
    }
    
    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   Lab_Highlight(w),
		   Lab_Highlight(w),
		   XtWidth(w) - 2 * Lab_Highlight(w),
		   XtHeight(w) - 2 * Lab_Highlight(w),
		   Lab_Shadow(w),
		   XmSHADOW_OUT);

    if (!PB_Armed(w)) {
	PB_Armed(w) = True;
	if (PB_ArmCallback(w)) {
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.click_count = PB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PB_ArmCallback(w),
			       (XtPointer)&cbs);
	}
    }

    _XmRecordEvent(event);
}

static void 
MultiActivate(Widget w, 
              XEvent *event, 
              String *params, 
              Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    XmPushButtonCallbackStruct cbs;
    XtExposeProc exp = XtClass(w)->core_class.expose;

    XdbDebug(__FILE__, w, "PushB: MultiClick\n");

    if (PB_MultiClick(w) == XmMULTICLICK_KEEP) {
	Time mctime = XtGetMultiClickTime(XtDisplay(w));

	if ((event->xbutton.time - PB_ArmTimeStamp(w)) < mctime)
	    PB_ClickCount(w)++;
	else
	    PB_ClickCount(w) = 1;

	PB_Armed(w) = False;

	(*exp)(w, event, NULL);

	if (ev->type == KeyPress || ev->type == KeyRelease
	    || ((ev->x >= 0 && ev->x < XtWidth(w))
	    && (ev->y >= 0 && ev->y < XtHeight(w))))
	{
	    if (PB_MultiClick(w) == XmMULTICLICK_DISCARD &&
		PB_ClickCount(w) > 1)
		return;

	    if (!Lab_SkipCallback(w) && PB_ActivateCallback(w)) {
		cbs.reason = XmCR_ACTIVATE;
		cbs.event = event;
		cbs.click_count = PB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PB_ActivateCallback(w),
				   (XtPointer)&cbs);
	    }
	}
	Disarm(w, event, params, num_params);
    }
}

static void 
MultiArm(Widget w, 
         XEvent *event, 
         String *params, 
         Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "PushB: MultiArm\n");

    if (PB_MultiClick(w) == XmMULTICLICK_KEEP)
	Arm(w, event, NULL, NULL);
}

void
_XmClearBCompatibility(Widget pb)
{
}


Widget
XmCreatePushButton(Widget parent,
		   char *name,
		   Arg *arglist,
		   Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmPushButtonWidgetClass,
			  parent,
			  arglist,
			  argcount);
}
