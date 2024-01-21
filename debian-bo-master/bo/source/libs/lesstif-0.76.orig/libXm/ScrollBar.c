/**
 *
 * $Id: ScrollBar.c,v 1.5 1996/12/30 07:36:31 u27113 Exp $
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

static char rcsid[] = "$Id: ScrollBar.c,v 1.5 1996/12/30 07:36:31 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/ScaleP.h>
#include <limits.h>
#include <Xm/ScreenP.h>
#include <stdlib.h>

#include <XmI/DebugUtil.h>

#define _XmMin(a,b) (((a)<(b))?(a):(b))
#define _XmMax(a,b) (((a)>(b))?(a):(b))

#define DEF_MIN			0
#define DEF_MAX			100
#define DEF_INC			1
#define DEF_PAGE_INC		10
#define DEF_INIT_DELAY		250
#define DEF_REP_DELAY		50
#define DEF_SB_DIM		11
#define DEF_SB_LEN		100
#define ARROW_SPACING		1

/* BUGS/FIX ME:
 *
 * M*tif 1.2.2 handles initial_x and initial_y differently.
 *
 * Arghhhh... in M*tif 1.2.2 value (and hence saved_value, I guess) private
 * vars are independent of processing direction.
 */

/*
 * rules
 *
 * SliderAreaWidth is the width of the trough; this is the width of the trough
 *   area
 * SliderAreaHeight is the height of the trough;  this is the height of the
 *   trough area
 * SliderAreaX is the rightmost starting location of the trough after taking
 *   into account the orientation and the arrows (if any)
 * SliderAreaY is the topmost starting location of the trough after taking
 *   into account the orientation and the arrows (if any)
 * SliderX is the leftmost position of the slider.  Note that some SliderX values
 *   make for impossible Values (rounding errors when orientation is
 *   XmHORIZONTAL).  This is due to the physical size being insufficient to
 *   represent all possible Values.
 * SliderY is the topmost position of the slider.  Note that some SliderY values
 *   make for impossible Values (rounding errors when orientation is
 *   XmVERTICAL).  This is due to the physical size being insufficient to
 *   represent all possible Values.
 * if (orientation is horizontal) then
 *    SliderHeight is the same as the SliderAreaHeight
 *    SliderWidth is the relative to the area width
 * else if (orientation is vertical) then
 *    SliderWidth is the same as the SliderAreaWidth
 *    SliderHeight is the relative to the area height
 * else
 *    error
 * SliderSize (in pixels) is defined as a proportion:
 *   if (orientation is XmHORIZONTAL)
 *     SliderSize is (Maximum - Minimum) / SliderAreaWidth * .1
 *   else if (orientation is XmVERTICAL)
 *     SliderSize is (Maximum - Minimum) / SliderAreaHeight * .1
 *   else
 *     error
 * end
 * Value is defined by pos, and vice-versa:
 *   if (orientation is XmHORIZONTAL)
 *     SliderX = (Value - Minimum) * ((SliderAreaWidth - SliderSize) / (Maximum - Minimum)) + SliderAreaX
 *     Value = ((pos - SliderAreaX) / (SliderAreaWidth - SliderSize) * (Maximum - Minimum)) + Minimum
 *   else if (orientation is XmVERTICAL)
 *     SliderY = (Value - Minimum) * ((SliderAreaHeight - SliderSize) / (Maximum - Minimum)) + SliderAreaY
 *     Value = ((pos - SliderAreaY) / (SliderAreaHeight - SliderSize) * (Maximum - Minimum)) + Minimum
 *   else
 *     error
 *
 * 101295 -- Sigh.  HighlightThickness goes back in.  M*tif behavior
 * observation says * it's there, it just doesn't get drawn -- unless the
 * parent is a Scale.
 * 102395 -- Sigh again.  There is always a one pixel separation between the
 * arrow and the slider, regardless of size.
 */

/*
 * pick a GC, any GC
 */
#define SCB_GC(w)	(XtSensitive(w) \
				? SCB_ForegroundGC(w) : SCB_UnavailableGC(w))
/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void expose(Widget w, XEvent *event, Region region);
static void resize(Widget w);
static void realize(Widget w, XtValueMask *values, XSetWindowAttributes *attributes);
static void DoLayout(Widget w, Boolean first, Boolean resize);
static void scrollbar_highlight(Widget w);
static void scrollbar_unhighlight(Widget w);

/* timer callbacks for holding the button in to scroll */

static void buttonTimer(XtPointer clientData, XtIntervalId *id);
static void troughIncTimer(XtPointer clientData, XtIntervalId *id);
static void troughDecTimer(XtPointer clientData, XtIntervalId *id);

/* functions that actually move the slider, and call callbacks */

static void increment(Widget sw, XEvent *event);
static void decrement(Widget sw, XEvent *event);
static void incrementByPage(Widget sw, XEvent *event);
static void decrementByPage(Widget sw, XEvent *event);

/* misc scrollbar routines */

static int _XmScrollBarArrowWidth(Widget sw);
static int _XmScrollBarArrowHeight(Widget sw);
static int _XmScrollBarPosToValue(Widget sw, Position pos);

static void draw_slider(Widget sw);
static void erase_slider(Widget sw);
static void redraw_arrows(Widget sw);
static void redraw_inset(Widget sw);

static void check_pixel_constraints(Widget sw);
static void check_constraints(Widget sw, Boolean);

/*
 * Resources for the scrollbar class
 */
#define Offset(field) XtOffsetOf(XmScrollBarRec, scrollBar.field)
#define Prim_Offset(field) XtOffsetOf(XmScrollBarRec, primitive.field)
static XtResource resources[] = {
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), XtOffsetOf(XmScrollBarRec, primitive.navigation_type),
	XmRImmediate, (XtPointer)XmSTICKY_TAB_GROUP
    },
    {
	XmNforeground, XmCForeground, XmRPixel,
	sizeof(Pixel), XtOffsetOf(XmScrollBarRec, primitive.foreground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault /* FIXME: NEED correct PROC HERE */
    },
    {
	XmNbackground, XmCBackground, XmRPixel,
	sizeof(Pixel), XtOffsetOf(XmScrollBarRec, core.background_pixel),
	XmRCallProc, (XtPointer)_XmBackgroundColorDefault /* FIXME: NEED correct PROC HERE */
    },
    {
	XmNtroughColor, XmCTroughColor, XmRPixel,
	sizeof(Pixel), Offset(trough_color),
	XmRCallProc, (XtPointer)_XmScrollBarTroughColorDefault
    },
    {
	XmNvalue, XmCValue, XmRInt,
	sizeof(int), Offset(value),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNminimum, XmCMinimum, XmRInt,
	sizeof(int), Offset(minimum),
	XtRImmediate, (XtPointer)DEF_MIN
    },
    {
	XmNmaximum, XmCMaximum, XmRInt,
	sizeof(int), Offset(maximum),
	XtRImmediate, (XtPointer)DEF_MAX
    },
    {
	XmNsliderSize, XmCSliderSize, XmRInt,
	sizeof(int), Offset(slider_size),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNshowArrows, XmCShowArrows, XmRBoolean,
	sizeof(Boolean), Offset(show_arrows),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmVERTICAL
    },
    {
	XmNprocessingDirection, XmCProcessingDirection, XmRProcessingDirection,
	sizeof(unsigned char), Offset(processing_direction),
	XmRCallProc, (XtPointer)_XmScrollBarProcessingDirectionDefault
    },
    {
	XmNincrement, XmCIncrement, XmRInt,
	sizeof(int), Offset(increment),
	XmRImmediate, (XtPointer)DEF_INC
    },
    {
	XmNpageIncrement, XmCPageIncrement, XmRInt,
	sizeof(int), Offset(page_increment),
	XmRImmediate, (XtPointer)DEF_PAGE_INC
    },
    {
	XmNinitialDelay, XmCInitialDelay, XmRInt,
	sizeof(int), Offset(initial_delay),
	XmRImmediate, (XtPointer)DEF_INIT_DELAY
    },
    {
	XmNrepeatDelay, XmCRepeatDelay, XmRInt,
	sizeof(int), Offset(repeat_delay),
	XmRImmediate, (XtPointer)DEF_REP_DELAY
    },
    {
	XmNvalueChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNincrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(increment_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdecrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(decrement_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNpageIncrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(page_increment_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNpageDecrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(page_decrement_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtoTopCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(to_top_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtoBottomCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(to_bottom_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdragCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(drag_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), Prim_Offset(traversal_on),
	XmRCallProc, (XtPointer)_XmScrollBarTraversalOnDefault,
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), Prim_Offset(highlight_thickness),
	XmRImmediate, (XtPointer)0
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNvalue,
	sizeof(int), Offset(value),
	NULL /* FIXME */, NULL /* FIXME */
    }
};

static void Select(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Release(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void Moved(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void TopOrBottom(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void CancelDrag(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void PageDownOrRight(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void PageUpOrLeft(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void IncrementDownOrRight(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void IncrementUpOrLeft(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmScrollBar_defaultTranslations[] = 
   "<Unmap>:                  PrimitiveUnmap()\n\
    <Enter>:                  PrimitiveEnter()\n\
    <Leave>:                  PrimitiveLeave()\n\
    <FocusIn>:                PrimitiveFocusIn()\n\
    <FocusOut>:               PrimitiveFocusOut()\n\
    <Key>osfActivate:         PrimitiveParentActivate()\n\
    <Key>osfCancel:           CancelDrag()\n\
    <Key>osfBeginLine:        TopOrBottom()\n\
    <Key>osfEndLine:          TopOrBottom()\n\
    <Key>osfPageLeft:         PageUpOrLeft(1)\n\
    c <Key>osfPageUp:         PageUpOrLeft(1)\n\
    <Key>osfPageUp:           PageUpOrLeft(0)\n\
    <Key>osfPageRight:        PageDownOrRight(1)\n\
    c <Key>osfPageDown:       PageDownOrRight(1)\n\
    <Key>osfPageDown:         PageDownOrRight(0)\n\
    <Key>osfHelp:             PrimitiveHelp()\n\
    c <Key>osfUp:             PageUpOrLeft(0)\n\
    c <Key>osfDown:           PageDownOrRight(0)\n\
    c <Key>osfLeft:           PageUpOrLeft(1)\n\
    c <Key>osfRight:          PageDownOrRight(1)\n\
    <Key>osfUp:               IncrementUpOrLeft(0)\n\
    <Key>osfDown:             IncrementDownOrRight(0)\n\
    <Key>osfLeft:             IncrementUpOrLeft(1)\n\
    <Key>osfRight:            IncrementDownOrRight(1)\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    s ~m ~a <Key>Tab:         PrimitivePrevTabGroup()\n\
    ~m ~a <Key>Tab:           PrimitiveNextTabGroup()\n\
    ~m ~a ~c ~s <Btn1Down>:   Select() \n\
    ~m ~a ~c ~s <Btn1Up>:     Release() \n\
    ~m ~a ~c ~s Button1<PtrMoved>: Moved() \n\
    ~m ~a ~c ~s <Btn2Down>:   Select() \n\
    ~m ~a ~c ~s Button2<PtrMoved>: Moved() \n\
    ~m ~a ~c ~s <Btn2Up>:     Release() \n\
    ~m ~a  c ~s <Btn1Down>:   TopOrBottom() \n\
    ~m ~a  c ~s <Btn1Up>:     Release()";

static XtActionsRec actions[] = {
    {"Select", Select},
    {"Release", Release},
    {"Moved", Moved},
    {"TopOrBottom", TopOrBottom},
    {"CancelDrag", CancelDrag},
    {"PageDownOrRight", PageDownOrRight},
    {"PageUpOrLeft", PageUpOrLeft},
    {"IncrementDownOrRight", IncrementDownOrRight},
    {"IncrementUpOrLeft", IncrementUpOrLeft}
};

static XmBaseClassExtRec _XmScrollBarCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmScrollBarPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmScrollBarClassRec xmScrollBarClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name            */ "XmScrollBar",
	/* widget_size           */ sizeof(XmScrollBarRec),
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
	/* compress_exposure     */ XtExposeCompressMaximal,
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
	/* tm_table              */ _XmScrollBar_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmScrollBarCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ scrollbar_highlight,
       	/* border_unhighlight    */ scrollbar_unhighlight,
       	/* translations          */ NULL,
       	/* arm_and_activate_proc */ NULL,
       	/* synthetic resources   */ syn_resources, 
        /* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmScrollBarPrimClassExtRec
    },
    /* ScrollBar Class part */
    {
	/* extension */ NULL
    },
};

WidgetClass xmScrollBarWidgetClass = (WidgetClass)&xmScrollBarClassRec;


static char *warnings[] = {
    "Maximum value is less than or equal to minimum value.",
    "Specified slider size is less than 1.",
    "Specified slider size is greater than maximum value minus minimum value.",
    "Specified value is less than minimum value.",
    "Specified value is greater than maximum value minus slider size.",
    "Specified increment is less than 1.",
    "Specified page increment is less than 1.",
    "Specified initial delay is less than 1.",
    "Specified repeat delay is less than 1.",
    "Incorrect processing direction."
};

#define MSG_MAX_LTE_MIN		warnings[0]
#define MSG_SLSZ_LT_1		warnings[1]
#define MSG_SLSZ_GT_MAXMIN	warnings[2]
#define MSG_VAL_LT_MIN		warnings[3]
#define MSG_VAL_GT_MAXSLSZ	warnings[4]
#define MSG_INC_LT_1		warnings[5]
#define MSG_PGINC_LT_1		warnings[6]
#define MSG_IDLAY_LT_1		warnings[7]
#define MSG_RDLAY_LT_1		warnings[8]
#define MSG_PROCDIR		warnings[9]

/*
 * things I've learned testing with M*tif in the flags field
 * number bits, right to left, starting with 1
 *
 * BIT 8 (value 128) -- set when dragging. Should this be BIT 7 (value 64)?
 *
 * BIT 1 (value 1) -- set when first clicking in trough, but cleared when
 *   repeat kicks in (otherwise not cleared until repeat is performed).
 *
 * I was wrong about the flag for PARENT.  Seems that is indicated by having
 * bit 8 set in the change_type field (see below).
 * There doesn't seem to be flags for where the clicking occurred, but bit
 * 5 and 6 seem to always be set in a callback.
 */
#define FLG_FIRST_PAGE_MOVE	0x01
#define FLG_IN_SLIDER		0x02
#define FLG_IN_TROUGH		0x04
#define FLG_DRAG		0x40

/*
 * change type values
 *
 * bit 8 seems to be set if the ScrollBar is in a Scale.  Bits 5 and 6
 * seem to always be set in callbacks.  I can't figure the lower 4 bits yet.
 */
#define CHANGE_TYPE_SCALE	0x80

static void
class_initialize()
{
    _XmScrollBarCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSCROLL_BAR_BIT);
}

static void
CreateForegroundGC(Widget w)
{
    XtGCMask mask;
    XGCValues values;

    mask = GCFunction | GCPlaneMask | GCSubwindowMode | GCGraphicsExposures |
	   GCForeground | GCBackground | GCFillStyle;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);
    values.fill_style = FillStippled;

    SCB_ForegroundGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateUnavailableGC(Widget w)
{
    XtGCMask mask;
    XGCValues values;

    mask = GCFunction | GCPlaneMask | GCSubwindowMode | GCGraphicsExposures |
	   GCForeground | GCBackground | GCStipple | GCFillStyle |
	   GCTileStipXOrigin | GCTileStipYOrigin;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple = XmGetPixmapByDepth(XtScreen(w),
					XmEVEN_STIPPLE_IMAGE,
					WhitePixelOfScreen(XtScreen(w)),
					BlackPixelOfScreen(XtScreen(w)),
					1);

    SCB_UnavailableGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    /*
     * we override the Primitive width/height if the user didn't specify one
     */

    if (SCB_Orientation(new_w) == XmHORIZONTAL)
    {
	if (XtWidth(request) == 0) {
	    XtWidth(new_w) = DEF_SB_LEN +
			2 * Prim_ShadowThickness(new_w) +
			2 * Prim_HighlightThickness(new_w);
	}

	if (XtHeight(request) == 0) {
	    XtHeight(new_w) = DEF_SB_DIM +
			2 * Prim_ShadowThickness(new_w) +
			2 * Prim_HighlightThickness(new_w);
	}

	SCB_Arrow1Orientation(new_w) = XmARROW_LEFT;
	SCB_Arrow2Orientation(new_w) = XmARROW_RIGHT;
    }
    else
    {
	if (XtHeight(request) == 0) {
	    XtHeight(new_w) = DEF_SB_LEN +
			2 * Prim_ShadowThickness(new_w) +
			2 * Prim_HighlightThickness(new_w);
	}

	if (XtWidth(request) == 0) {
	    XtWidth(new_w) = DEF_SB_DIM +
			2 * Prim_ShadowThickness(new_w) +
			2 * Prim_HighlightThickness(new_w);
	}

	SCB_Arrow1Orientation(new_w) = XmARROW_UP;
	SCB_Arrow2Orientation(new_w) = XmARROW_DOWN;
    }

    SCB_Arrow1Selected(new_w) = False;
    SCB_Arrow2Selected(new_w) = False;
    SCB_Flags(new_w) = 0;
    SCB_EtchedSlider(new_w) = False;

    SCB_Pixmap(new_w) = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(XtScreen(new_w)),
					    DefaultDepthOfScreen(XtScreen(new_w)),
					    XtWidth(new_w), XtHeight(new_w));

    SCB_Timer(new_w) = 0;

    CreateForegroundGC(new_w);
    CreateUnavailableGC(new_w);

    if (XtIsSubclass(XtParent(new_w), xmScaleWidgetClass))
	SCB_ChangeType(new_w) |= CHANGE_TYPE_SCALE;

    check_constraints(new_w, True);

    SCB_SavedValue(new_w) = SCB_Value(new_w);

    DoLayout((Widget)new_w, True, False);
}

/* FIX ME: split this up into two functions, one for initialize and one for
 * SetValues behaviour.
 * Values for processing direction need a closer look, the others are per
 * Motif 1.2.2.
 */
static void 
check_constraints(Widget sw, Boolean init)
{
    XdbDebug(__FILE__, sw, "Specified scrollbar values: min=%d, max=%d, val=%d, sl_size=%d, inc=%d, p_inc=%d, p_dir=%d\n", SCB_Minimum(sw), SCB_Maximum(sw), SCB_Value(sw), SCB_SliderSize(sw), SCB_Increment(sw), SCB_PageIncrement(sw), (int)SCB_ProcessingDirection(sw));

    if (SCB_Maximum(sw) <= SCB_Minimum(sw))
    {
	_XmWarning(sw, MSG_MAX_LTE_MIN);
	SCB_Minimum(sw) = DEF_MIN;
	SCB_Maximum(sw) = DEF_MAX;
    }

    if (init)
    {
	if (SCB_SliderSize(sw) == INT_MAX)
	{
	    SCB_SliderSize(sw) = (SCB_Maximum(sw) - SCB_Minimum(sw)) / 10;
	}

	if (SCB_SliderSize(sw) < 1)
	{
	    _XmWarning(sw, MSG_SLSZ_LT_1);
	    SCB_SliderSize(sw) = 1;
	}

	if (SCB_SliderSize(sw) > SCB_Maximum(sw) - SCB_Minimum(sw)
	 && SCB_Maximum(sw) != SCB_Minimum(sw))
	{
	    _XmWarning(sw, MSG_SLSZ_GT_MAXMIN);
	    SCB_SliderSize(sw) = SCB_Maximum(sw) - SCB_Minimum(sw);
	}

	if (SCB_Value(sw) == INT_MAX)
	{
	    SCB_Value(sw) = SCB_Minimum(sw);
	}

	if (SCB_Value(sw) < SCB_Minimum(sw))
	{
	    _XmWarning(sw, MSG_VAL_LT_MIN);
	    SCB_Value(sw) = SCB_Minimum(sw);
	}

	if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw)
	 && SCB_Maximum(sw) != SCB_SliderSize(sw))
	{
	    _XmWarning(sw, MSG_VAL_GT_MAXSLSZ);
	    SCB_Value(sw) = SCB_Minimum(sw);
	}
    }
    else /* !init */
    {
	if (SCB_SliderSize(sw) < 1)
	{
	    _XmWarning(sw, MSG_SLSZ_LT_1);
	    SCB_SliderSize(sw) = (SCB_Maximum(sw) - SCB_Minimum(sw)) / 10;
	}

	if (SCB_SliderSize(sw) > SCB_Maximum(sw) - SCB_Minimum(sw)
	 && SCB_Maximum(sw) != SCB_Minimum(sw))
	{
	    _XmWarning(sw, MSG_SLSZ_GT_MAXMIN);
	    SCB_SliderSize(sw) = (SCB_Maximum(sw) - SCB_Minimum(sw)) / 10;
	}

	if (SCB_Value(sw) < SCB_Minimum(sw))
	{
	    _XmWarning(sw, MSG_VAL_LT_MIN);
	    SCB_Value(sw) = SCB_Minimum(sw);
	}

	if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw)
	 && SCB_Maximum(sw) != SCB_SliderSize(sw))
	{
	    _XmWarning(sw, MSG_VAL_GT_MAXSLSZ);
	    SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);
	}
    }

    if (SCB_Increment(sw) < 1)
    {
	_XmWarning(sw, MSG_INC_LT_1);
	SCB_Increment(sw) = DEF_INC;
    }

    if (SCB_PageIncrement(sw) < 1)
    {
	_XmWarning(sw, MSG_PGINC_LT_1);
	SCB_PageIncrement(sw) = DEF_PAGE_INC;
    }

    if (SCB_InitialDelay(sw) < 1)
    {
	_XmWarning(sw, MSG_IDLAY_LT_1);
	SCB_InitialDelay(sw) = DEF_INIT_DELAY;
    }

    if (SCB_RepeatDelay(sw) < 1)
    {
	_XmWarning(sw, MSG_RDLAY_LT_1);
	SCB_RepeatDelay(sw) = init ? 75 : DEF_REP_DELAY; /* Yes, 75 is not equal to the default. */
    }

    if ((SCB_Orientation(sw) == XmHORIZONTAL
	 && SCB_ProcessingDirection(sw) != XmMAX_ON_LEFT
	 && SCB_ProcessingDirection(sw) != XmMAX_ON_RIGHT)
	||
	(SCB_Orientation(sw) == XmVERTICAL
	 && SCB_ProcessingDirection(sw) != XmMAX_ON_TOP
	 && SCB_ProcessingDirection(sw) != XmMAX_ON_BOTTOM))
    {
	int dummy_offset = 0;
	XrmValue dummy_val;

	_XmWarning(sw, MSG_PROCDIR);
	_XmScrollBarProcessingDirectionDefault(sw, dummy_offset, &dummy_val); /* FIX ME: right? */
    }

    XdbDebug(__FILE__, sw, "Got scrollbar values: min=%d, max=%d, val=%d, sl_size=%d, inc=%d, p_inc=%d, p_dir=%d\n", SCB_Minimum(sw), SCB_Maximum(sw), SCB_Value(sw), SCB_SliderSize(sw), SCB_Increment(sw), SCB_PageIncrement(sw), (int)SCB_ProcessingDirection(sw));
}
 

static void
destroy(Widget w)
{
    if (SCB_Pixmap(w) != None)
	_XmFreeScratchPixmap((XmScreen)XmGetXmScreen(XtScreen(w)),
			     SCB_Pixmap(w));
    XtReleaseGC(w, SCB_UnavailableGC(w));
    XtReleaseGC(w, SCB_ForegroundGC(w));
}

static Boolean 
set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    Boolean		r = False;

#define	CNE(x)	(x(new_w) != x(current))

    if (SCB_TroughColor(new_w) != SCB_TroughColor(current) ||
        Prim_Foreground(new_w) != Prim_Foreground(current)) {
	XtReleaseGC(new_w, SCB_ForegroundGC(new_w));
	XtReleaseGC(new_w, SCB_UnavailableGC(new_w));
	CreateUnavailableGC(new_w);
	CreateForegroundGC(new_w);
	r = True;
    }

    /* FIX ME: test too restrictive? */
    if (CNE(SCB_Orientation) || CNE(SCB_Minimum) || CNE(SCB_Maximum) || CNE(SCB_Value)
			|| CNE(XtX) || CNE(XtY) || CNE(SCB_SliderSize)) {
	r = True;
	/* Ensure slidersize and value are within constraints */
	check_constraints(new_w, False);

	erase_slider(new_w);

	DoLayout(new_w, False, True);
    }

    return r;
}

static void
redraw_inset(Widget sw)
{
    XSetForeground(XtDisplay(sw), SCB_GC(sw), SCB_TroughColor(sw));

    XFillRectangle(XtDisplay(sw),
		   SCB_Pixmap(sw),
		   SCB_GC(sw),
		   0, 0,
		   XtWidth(sw), XtHeight(sw));
    XSetForeground(XtDisplay(sw), SCB_GC(sw), XtBackground(sw));

    _XmDrawShadows(XtDisplay((Widget)sw),
		   (Window)SCB_Pixmap(sw),
		   Prim_TopShadowGC(sw),
		   Prim_BottomShadowGC(sw),
		   Prim_HighlightThickness(sw),
		   Prim_HighlightThickness(sw),
		   XtWidth(sw) - 2 * Prim_HighlightThickness(sw),
		   XtHeight(sw) - 2 * Prim_HighlightThickness(sw),
		   Prim_ShadowThickness(sw),
		   XmSHADOW_IN);
}

static void
redraw_arrows(Widget sw)
{
    if (!SCB_ShowArrows(sw))
	return;

    if (SCB_Arrow1Selected(sw))
    {
	_XmDrawArrow(XtDisplay(sw),
		     (Window)SCB_Pixmap(sw),
		     Prim_TopShadowGC(sw),
		     Prim_BottomShadowGC(sw),
		     SCB_GC(sw),
		     SCB_Arrow1X(sw),
		     SCB_Arrow1Y(sw),
		     SCB_ArrowWidth(sw),
		     SCB_ArrowHeight(sw),
		     Prim_ShadowThickness(sw) == 0 ? 0 : 2,
		     SCB_Arrow1Orientation(sw));
    }
    else
    {
	_XmDrawArrow(XtDisplay(sw),
		     (Window)SCB_Pixmap(sw),
		     Prim_BottomShadowGC(sw),
		     Prim_TopShadowGC(sw),
		     SCB_GC(sw),
		     SCB_Arrow1X(sw),
		     SCB_Arrow1Y(sw),
		     SCB_ArrowWidth(sw),
		     SCB_ArrowHeight(sw),
		     Prim_ShadowThickness(sw) == 0 ? 0 : 2,
		     SCB_Arrow1Orientation(sw));
    }

    if (SCB_Arrow2Selected(sw))
    {
	_XmDrawArrow(XtDisplay(sw),
		     (Window)SCB_Pixmap(sw),
		     Prim_TopShadowGC(sw),
		     Prim_BottomShadowGC(sw),
		     SCB_GC(sw),
		     SCB_Arrow2X(sw),
		     SCB_Arrow2Y(sw),
		     SCB_ArrowWidth(sw),
		     SCB_ArrowHeight(sw),
		     Prim_ShadowThickness(sw) == 0 ? 0 : 2,
		     SCB_Arrow2Orientation(sw));
    }
    else
    {
	_XmDrawArrow(XtDisplay(sw),
		     (Window)SCB_Pixmap(sw),
		     Prim_BottomShadowGC(sw),
		     Prim_TopShadowGC(sw),
		     SCB_GC(sw),
		     SCB_Arrow2X(sw),
		     SCB_Arrow2Y(sw),
		     SCB_ArrowWidth(sw),
		     SCB_ArrowHeight(sw),
		     Prim_ShadowThickness(sw) == 0 ? 0 : 2,
		     SCB_Arrow2Orientation(sw));
    }
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    if (XtIsRealized(w))

	XCopyArea(XtDisplay(w),
		  SCB_Pixmap(w),
		  XtWindow(w),
		  SCB_GC(w),
		  0,0,
		  XtWidth(w),
		  XtHeight(w),
		  0,0);
}

static void 
resize(Widget w)
{
    DoLayout(w, False, True);

    if (!XtIsRealized(w))
	return;

    redraw_inset(w);
    draw_slider(w);
    redraw_arrows(w);
}

static void
realize(Widget w, XtValueMask *values, XSetWindowAttributes *attributes) {
    DoLayout(w, False, False);

#define superclass (&xmPrimitiveClassRec)
    (*superclass->core_class.realize)(w, values, attributes);
#undef superclass
}

static void 
check_pixel_constraints(Widget sw)
{
    if(SCB_Orientation(sw) == XmHORIZONTAL)
    {
	/* We must ensure that the slider x pixel value
	   will not cause overlap of the arrow */
	if(SCB_SliderX(sw) + SCB_SliderWidth(sw) > SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw))
	    SCB_SliderX(sw) = SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw) - SCB_SliderWidth(sw);
    }
    else
    {
	/* We must ensure that the slider y pixel value
	   will not cause overlap of the arrow */
	if(SCB_SliderY(sw) + SCB_SliderHeight(sw) > SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw))
	    SCB_SliderY(sw) = SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw) - SCB_SliderHeight(sw);
    }
}
 

static void 
DoLayout(Widget sw, Boolean first, Boolean resize)
{
    int arrow_width;
    int arrow_height;

    if (resize) {
	_XmFreeScratchPixmap((XmScreen)XmGetXmScreen(XtScreen(sw)),
			     SCB_Pixmap(sw));

	SCB_Pixmap(sw) = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(XtScreen(sw)),
					       DefaultDepthOfScreen(XtScreen(sw)),
					       XtWidth(sw) ? XtWidth(sw) : 1,
					       XtHeight(sw) ? XtHeight(sw) : 1);
    }

    arrow_width = _XmScrollBarArrowWidth(sw);
    arrow_height = _XmScrollBarArrowHeight(sw);

    SCB_ArrowWidth(sw) = (Dimension) _XmMax(1, arrow_width);
    SCB_ArrowHeight(sw) = (Dimension) _XmMax(1, arrow_height);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
    {
	if (SCB_ShowArrows(sw)) {
	    SCB_SliderAreaX(sw) = arrow_width +
				  Prim_ShadowThickness(sw) +
				  Prim_HighlightThickness(sw) +
				  ARROW_SPACING;
	    SCB_SliderAreaY(sw) = (XtHeight(sw) - SCB_ArrowHeight(sw) + 1) / 2;

	    SCB_SliderAreaWidth(sw) = XtWidth(sw) -
					    2 * (arrow_width +
						 Prim_ShadowThickness(sw) +
						 Prim_HighlightThickness(sw) +
						 ARROW_SPACING);
	    SCB_SliderAreaHeight(sw) = SCB_ArrowHeight(sw);

	    SCB_Arrow1Y(sw) = SCB_Arrow2Y(sw) = (XtHeight(sw) - arrow_height) / 2;
	    SCB_Arrow1X(sw) = Prim_ShadowThickness(sw) +
			      Prim_HighlightThickness(sw);

	    SCB_Arrow2X(sw) = XtWidth(sw) -
				    (Prim_ShadowThickness(sw) +
				     Prim_HighlightThickness(sw) +
				     arrow_width);
        }
	else /* ! showArrows */ {
	    SCB_ArrowWidth(sw) = 0;
	    SCB_ArrowHeight(sw) = 0;

	    SCB_SliderAreaX(sw) = Prim_HighlightThickness(sw) +
				  Prim_ShadowThickness(sw);
	    SCB_SliderAreaY(sw) = (XtHeight(sw) - SCB_SliderAreaHeight(sw) + 1) / 2;

	    SCB_SliderAreaWidth(sw) = _XmMax(1, XtWidth(sw) -
					2 * (Prim_ShadowThickness(sw) +
					     Prim_HighlightThickness(sw)));
	    SCB_SliderAreaHeight(sw) = _XmMax(1, XtHeight(sw) -
					2 * (Prim_ShadowThickness(sw) +
					     Prim_HighlightThickness(sw)));

	    SCB_Arrow1X(sw) = 0;
	    SCB_Arrow1Y(sw) = 0;
	    SCB_Arrow2X(sw) = 0;
	    SCB_Arrow2Y(sw) = 0;
	}

	SCB_SliderHeight(sw) = SCB_SliderAreaHeight(sw);
	SCB_SliderWidth(sw) = _XmScrollBarSliderPixSize(sw);

	if (SCB_SliderWidth(sw) < MIN_SLIDER_LENGTH)
	    SCB_SliderWidth(sw) = MIN_SLIDER_LENGTH;

	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	SCB_SliderY(sw) = SCB_SliderAreaY(sw);
    }
    else /* vertical */
    {
	if (SCB_ShowArrows(sw)) {
	    SCB_SliderAreaY(sw) = arrow_height +
				  Prim_ShadowThickness(sw) +
				  Prim_HighlightThickness(sw) +
				  ARROW_SPACING;
	    SCB_SliderAreaX(sw) = (XtWidth(sw) - SCB_ArrowWidth(sw) + 1) / 2;

	    SCB_SliderAreaHeight(sw) = XtHeight(sw) -
					    2 * (arrow_height +
						 Prim_ShadowThickness(sw) +
						 Prim_HighlightThickness(sw) +
						 ARROW_SPACING);
	    SCB_SliderAreaWidth(sw) = SCB_ArrowWidth(sw);

	    SCB_Arrow1X(sw) = SCB_Arrow2X(sw) = (XtWidth(sw) - arrow_width) / 2;
	    SCB_Arrow1Y(sw) = Prim_ShadowThickness(sw) +
			      Prim_HighlightThickness(sw);
	    SCB_Arrow2Y(sw) = XtHeight(sw) -
				    (Prim_ShadowThickness(sw) +
				     Prim_HighlightThickness(sw) +
				     arrow_height);

        }
	else { /* ! showArrows */
	    SCB_ArrowHeight(sw) = 0;
	    SCB_ArrowWidth(sw) = 0;

	    SCB_SliderAreaY(sw) = Prim_HighlightThickness(sw) +
				  Prim_ShadowThickness(sw);
	    SCB_SliderAreaX(sw) = (XtWidth(sw) - SCB_SliderAreaWidth(sw) + 1) / 2;

	    SCB_SliderAreaHeight(sw) = _XmMax(1, XtHeight(sw) -
					2 * (Prim_ShadowThickness(sw) +
					     Prim_HighlightThickness(sw)));
	    SCB_SliderAreaWidth(sw) = _XmMax(1, XtWidth(sw) -
					2 * (Prim_ShadowThickness(sw) +
					     Prim_HighlightThickness(sw)));

	    SCB_Arrow1Y(sw) = 0;
	    SCB_Arrow1X(sw) = 0;
	    SCB_Arrow2Y(sw) = 0;
	    SCB_Arrow2X(sw) = 0;
	}

	SCB_SliderWidth(sw) = SCB_SliderAreaWidth(sw);
	SCB_SliderHeight(sw) = _XmScrollBarSliderPixSize(sw);

	if (SCB_SliderHeight(sw) < MIN_SLIDER_LENGTH)	
	    SCB_SliderHeight(sw) = MIN_SLIDER_LENGTH;

	SCB_SliderX(sw) = SCB_SliderAreaX(sw);
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    }

    check_pixel_constraints(sw);
    redraw_inset(sw);
    draw_slider(sw);
    redraw_arrows(sw);
}

static XtGeometryResult
query_geometry(Widget w,
	       XtWidgetGeometry *request,
	       XtWidgetGeometry *reply)
{
    XtGeometryResult result = XtGeometryYes;
    Dimension width = XtWidth(w);
    Dimension height = XtHeight(w);
    
    request->request_mode &= CWWidth | CWHeight;

    if (request->request_mode == 0)
	return result;

    if (request->request_mode & CWWidth) {
	if (request->width < width){
	    result = XtGeometryAlmost;
	    reply->width = width;
	    reply->request_mode |= CWWidth;
	}
    }
    if (request->request_mode & CWHeight) {
	if (request->height < height){
	    result = XtGeometryAlmost;
	    reply->height = height;
	    reply->request_mode |= CWHeight;
	}
    }

    return result;
}

static void
scrollbar_unhighlight(Widget w) {
    if (!(SCB_ChangeType(w) & CHANGE_TYPE_SCALE))
	return;

#define superclass (&xmPrimitiveClassRec)
    (*superclass->primitive_class.border_unhighlight)(w);
#undef superclass
}

static void
scrollbar_highlight(Widget w) {
    if (!(SCB_ChangeType(w) & CHANGE_TYPE_SCALE))
	return;

#define superclass (&xmPrimitiveClassRec)
    (*superclass->primitive_class.border_highlight)(w);
#undef superclass
}

Widget
XmCreateScrollBar(Widget parent,
		  char *name,
		  Arg *arglist,
		  Cardinal argCount)
{
    return XtCreateWidget(name,
			  xmScrollBarWidgetClass,
			  parent,
			  arglist,
			  argCount);
}

void 
XmScrollBarGetValues(Widget w,
		     int *value_return,
		     int *slider_size_return,
		     int *increment_return,
		     int *page_increment_return)
{
    XmScrollBarWidget sw = (XmScrollBarWidget) w;

    *value_return = SCB_Value(sw);
    *slider_size_return = SCB_SliderSize(sw);
    *increment_return = SCB_Increment(sw);
    *page_increment_return = SCB_PageIncrement(sw);
}

void
XmScrollBarSetValues(Widget sw,
		     int value,
		     int slider_size,
		     int increment,
		     int page_increment,
		     Boolean notify)
{
    int prevValue = SCB_Value(sw);

    SCB_Value(sw) = value;
    SCB_SliderSize(sw) = slider_size;
    SCB_Increment(sw) = increment;
    SCB_PageIncrement(sw) = page_increment;

    check_constraints(sw, False);

    if (notify && SCB_Value(sw) != prevValue)
    {
	XmScrollBarCallbackStruct cbs;

	cbs.event = NULL;
	cbs.value = SCB_Value(sw);
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.pixel = 0;

	XtCallCallbackList(sw, SCB_ValueChangedCallback(sw), &cbs);
    }

    SCB_Arrow1Selected(sw) = SCB_Arrow2Selected(sw) = False;
    SCB_Flags(sw) |= FLG_IN_SLIDER;

    DoLayout(sw, False, False);

    draw_slider(sw);
    redraw_arrows(sw);

    expose(sw, NULL, (Region)NULL);
}

static void
erase_slider(Widget sw)
{
    XSetForeground(XtDisplay(sw), SCB_GC(sw), SCB_TroughColor(sw));

    XFillRectangle(XtDisplay(sw),
		   SCB_Pixmap(sw),
		   SCB_GC(sw),
		   SCB_SliderX(sw), 
		   SCB_SliderY(sw), 
		   SCB_SliderWidth(sw),
		   SCB_SliderHeight(sw));

    XSetForeground(XtDisplay(sw), SCB_GC(sw), XtBackground(sw));
}

static void
draw_slider(Widget sw)
{
    XFillRectangle(XtDisplay(sw),
		   SCB_Pixmap(sw),
		   SCB_GC(sw),
		   SCB_SliderX(sw),
		   SCB_SliderY(sw), 
		   SCB_SliderWidth(sw) , 
		   SCB_SliderHeight(sw));

    _XmDrawShadows(XtDisplay((Widget)sw),
		   (Window)SCB_Pixmap(sw),
		   Prim_TopShadowGC(sw),
		   Prim_BottomShadowGC(sw),
		   SCB_SliderX(sw),
		   SCB_SliderY(sw),
		   SCB_SliderWidth(sw),
		   SCB_SliderHeight(sw),
		   _XmMin(Prim_ShadowThickness(sw),
			  _XmMin(SCB_SliderWidth(sw) / 2,
				 SCB_SliderHeight(sw) / 2)),
		   XmSHADOW_OUT);

    if(SCB_EtchedSlider(sw)) {
	Dimension sep_x;
	Dimension sep_y;

	if (SCB_Orientation(sw) == XmHORIZONTAL) {
	    sep_x = (SCB_SliderX(sw) +
		     SCB_SliderWidth(sw) / 2 -
		     Prim_ShadowThickness(sw) / 2);
	    sep_y = SCB_SliderY(sw) + Prim_ShadowThickness(sw) / 2;
	    _XmDrawSeparator(XtDisplay(sw), (Window)SCB_Pixmap(sw),
			     Prim_TopShadowGC(sw), Prim_BottomShadowGC(sw),
			     SCB_GC(sw),
			     sep_x, sep_y,
			     Prim_ShadowThickness(sw),
			     SCB_SliderHeight(sw) - Prim_ShadowThickness(sw),
			     Prim_ShadowThickness(sw), 0,
			     XmVERTICAL, XmSHADOW_ETCHED_IN);
	}
	else if (SCB_Orientation(sw) == XmVERTICAL) {
	    sep_y = (SCB_SliderY(sw) +
		     SCB_SliderHeight(sw) / 2 -
		     Prim_ShadowThickness(sw) / 2);
	    sep_x = SCB_SliderX(sw) + Prim_ShadowThickness(sw) / 2;
	    _XmDrawSeparator(XtDisplay(sw), (Window)SCB_Pixmap(sw),
			     Prim_TopShadowGC(sw), Prim_BottomShadowGC(sw),
			     SCB_GC(sw),
			     sep_x, sep_y,
			     SCB_SliderWidth(sw) - Prim_ShadowThickness(sw),
			     Prim_ShadowThickness(sw),
			     Prim_ShadowThickness(sw), 0,
			     XmHORIZONTAL, XmSHADOW_ETCHED_IN);
	}
    }
}

static void
Select(Widget sw,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    int eventPos;
    XmScrollBarCallbackStruct cbs;

    if (ev->x < Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) ||
	ev->y < Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) ||
	ev->x >= XtWidth(sw) - Prim_ShadowThickness(sw) - Prim_HighlightThickness(sw) ||
	ev->y >= XtHeight(sw) - Prim_ShadowThickness(sw) - Prim_HighlightThickness(sw))
        return;

    eventPos = (SCB_Orientation(sw) == XmHORIZONTAL) ? ev->x : ev->y;

    /* Yes... CancelDrag cancels a lot more then just dragging... */
    SCB_Flags(sw) |= FLG_DRAG;
    SCB_SavedValue(sw) = SCB_Value(sw);

    if ((SCB_Orientation(sw) == XmHORIZONTAL && eventPos < SCB_SliderAreaX(sw)) ||
	(SCB_Orientation(sw) == XmVERTICAL && eventPos < SCB_SliderAreaY(sw)))
    {
	/* left/top button */

	if (
	    ((SCB_ProcessingDirection(sw) == XmMAX_ON_LEFT 
	      || SCB_ProcessingDirection(sw) == XmMAX_ON_TOP)
	     && SCB_Value(sw) != SCB_Maximum(sw) - SCB_SliderSize(sw))
	    || 
	    ((SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT
	      || SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM)
	     && SCB_Value(sw) != SCB_Minimum(sw))
	    )
	{

	    SCB_Arrow1Selected(sw) = True;

	    /* first, clear the slider, before we move it. */
	    
	    erase_slider(sw);
	    
	    if ((SCB_Orientation(sw) == XmHORIZONTAL &&
	         SCB_ProcessingDirection(sw) == XmMAX_ON_LEFT) ||
	        (SCB_Orientation(sw) == XmVERTICAL &&
		 SCB_ProcessingDirection(sw) == XmMAX_ON_TOP))
		increment(sw, event);
	    else
		decrement(sw, event);

	    if (SCB_Timer(sw))
		XtRemoveTimeOut(SCB_Timer(sw));
	
	    SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw), 
					    SCB_InitialDelay(sw), 
					    buttonTimer, 
					    (XtPointer)sw);
	}
    }
    else if ((SCB_Orientation(sw) == XmHORIZONTAL 
	      && eventPos > SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw))
	     || (SCB_Orientation(sw) == XmVERTICAL 
		 && eventPos > SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw)))
    {
	/* right/bottom button */

	if (((SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT ||
	      SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM) &&
	     SCB_Value(sw) != SCB_Maximum(sw) - SCB_SliderSize(sw)) || 
	    ((SCB_ProcessingDirection(sw) == XmMAX_ON_LEFT ||
	      SCB_ProcessingDirection(sw) == XmMAX_ON_TOP) &&
	     SCB_Value(sw) != SCB_Minimum(sw))) {	
	    SCB_Arrow2Selected(sw) = True;
	    
	    /* first, clear the slider, before we move it. */

	    erase_slider(sw);
	    
	    if ((SCB_Orientation(sw) == XmHORIZONTAL &&
		 SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT) ||
		(SCB_Orientation(sw) == XmVERTICAL &&
		 SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM))
		increment(sw, event);
	    else
		decrement(sw, event);
	    
	    if (SCB_Timer(sw))
		XtRemoveTimeOut(SCB_Timer(sw));
	    
	    SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw), 
					    SCB_InitialDelay(sw), 
					    buttonTimer, 
					    (XtPointer)sw);
	}
    }

    /* Testing for the button defeats the translation scheme, yet 
       this is how Motif seems to do it.  -- PvH 
     */

    else if (ev->button != 1)
    {
	if ((SCB_Orientation(sw) == XmHORIZONTAL && 
	     (eventPos >= SCB_SliderAreaX(sw))  &&
	     (eventPos < SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw))) ||
	    (SCB_Orientation(sw) == XmVERTICAL &&
	     (eventPos >= SCB_SliderAreaY(sw)) &&
	     (eventPos < SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw)))) {
	    int prevValue;

	    prevValue = SCB_Value(sw);

	    erase_slider(sw);

	    if (SCB_Orientation(sw) == XmHORIZONTAL)
	    {
		SCB_SeparationX(sw) = SCB_SliderWidth(sw) / 2;
		SCB_SeparationY(sw) = ev->y - SCB_SliderY(sw); /* pretty useless, but per Motif 1.2.2 */

		SCB_SliderX(sw) = eventPos - SCB_SeparationX(sw);
 
		if (SCB_SliderX(sw) < SCB_SliderAreaX(sw))
		{
		    SCB_SliderX(sw) = SCB_SliderAreaX(sw);
		}
		if (SCB_SliderX(sw) > (SCB_SliderAreaX(sw) +
				       SCB_SliderAreaWidth(sw) -
				       SCB_SliderWidth(sw)))
		{
		    SCB_SliderX(sw) = SCB_SliderAreaX(sw) +
					SCB_SliderAreaWidth(sw) -
					SCB_SliderWidth(sw);
		}
		SCB_Value(sw) = _XmScrollBarPosToValue(sw, SCB_SliderX(sw));
	    }
	    else
	    {
		SCB_SeparationY(sw) = SCB_SliderHeight(sw) / 2;
		SCB_SeparationX(sw) = ev->x - SCB_SliderX(sw); /* pretty useless, but per Motif 1.2.2 */

		SCB_SliderY(sw) = eventPos - SCB_SeparationY(sw);

		if (SCB_SliderY(sw) < SCB_SliderAreaY(sw))
		{
		    SCB_SliderY(sw) = SCB_SliderAreaY(sw);
		}
		if (SCB_SliderY(sw) > (SCB_SliderAreaY(sw) +
				       SCB_SliderAreaHeight(sw) -
				       SCB_SliderHeight(sw)))
		{
		    SCB_SliderY(sw) = SCB_SliderAreaY(sw) +
					SCB_SliderAreaHeight(sw) -
					SCB_SliderHeight(sw);
		}
		SCB_Value(sw) = _XmScrollBarPosToValue(sw, SCB_SliderY(sw));
	    }

	    /* if value changed call drag_callback */

	    if (prevValue != SCB_Value(sw))
	    {
		cbs.event = event;
		cbs.value = SCB_Value(sw);
		cbs.reason = XmCR_DRAG;
		cbs.pixel = eventPos;
		
		XtCallCallbackList((Widget)sw, SCB_DragCallback(sw), &cbs);
	    }
	    SCB_Flags(sw) |= FLG_IN_SLIDER;
	}
    }
    else if (((SCB_Orientation(sw) == XmHORIZONTAL) && (eventPos >= SCB_SliderX(sw)) && (eventPos < (SCB_SliderX(sw) + SCB_SliderWidth(sw))))
	     || ((SCB_Orientation(sw) == XmVERTICAL) &&  (eventPos >= SCB_SliderY(sw)) && (eventPos < (SCB_SliderY(sw) + SCB_SliderHeight(sw)))))
    {
	/* the slider */
	SCB_Flags(sw) |= FLG_IN_SLIDER;
	SCB_SeparationX(sw) = ev->x - SCB_SliderX(sw);
	SCB_SeparationY(sw) = ev->y - SCB_SliderY(sw);
    }
    else /* the trough */
    {
	SCB_Flags(sw) |= FLG_IN_TROUGH;
	
	/* check to see what side of the slider we're on, and call increment/decrement by page accordingly */
	
	/* first, clear the slider, before we move it. */
	
	erase_slider(sw);
	
	if (SCB_Timer(sw))
	    XtRemoveTimeOut(SCB_Timer(sw));

	if ((SCB_Orientation(sw) == XmHORIZONTAL && eventPos < SCB_SliderX(sw))
	    || (SCB_Orientation(sw) == XmVERTICAL && eventPos < SCB_SliderY(sw)))
	    /* we're on the left/top side */
	    if ((SCB_Orientation(sw) == XmHORIZONTAL &&
		 SCB_ProcessingDirection(sw) == XmMAX_ON_LEFT) ||
		(SCB_Orientation(sw) == XmVERTICAL &&
		 SCB_ProcessingDirection(sw) == XmMAX_ON_TOP)) {

		incrementByPage(sw, event);
		
		SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw), 
					        SCB_InitialDelay(sw), 
					        troughIncTimer, 
					        (XtPointer)sw);
	    }
	    else
	    {
		decrementByPage(sw, event);
		SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw), 
						SCB_InitialDelay(sw), 
						troughDecTimer, 
						(XtPointer)sw);
	    }
	else /* we're on the right/bottom side */
	    if ((SCB_Orientation(sw) == XmHORIZONTAL &&
		 SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT) ||
		(SCB_Orientation(sw) == XmVERTICAL &&
		 SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM)) {

		incrementByPage(sw, event);

		SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw), 
					        SCB_InitialDelay(sw), 
					        troughIncTimer, 
					        (XtPointer)sw);
	    }
	    else
	    {
		decrementByPage(sw, event);
		SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw), 
						SCB_InitialDelay(sw), 
						      troughDecTimer, 
						      (XtPointer)sw);
	    }
    }

    check_pixel_constraints(sw);
    draw_slider(sw);
    redraw_arrows(sw);

    expose(sw, NULL, (Region)NULL);
}

static void
Release(Widget sw,
	XEvent *event,
	String *params,
	Cardinal *num_params)
{
    XmScrollBarCallbackStruct cbs;

    SCB_Flags(sw) &= ~FLG_DRAG;

    SCB_SavedValue(sw) = SCB_Value(sw);

    if (SCB_Flags(sw) & FLG_IN_SLIDER)
    {
	XButtonEvent *ev = (XButtonEvent *)event; /* FIX ME: safe to assume it's a button event? */

	/* send a value changed callback */
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.event = event;
	cbs.value = SCB_Value(sw);
	cbs.pixel = (SCB_Orientation(sw) == XmHORIZONTAL ? ev->x : ev->y);

	XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);
    }
    
    if (SCB_Arrow1Selected(sw) || SCB_Arrow2Selected(sw))
    {
	SCB_Arrow1Selected(sw) = SCB_Arrow2Selected(sw) = False;

	redraw_arrows(sw);
	expose(sw, NULL, (Region)NULL);
    }

    SCB_Flags(sw) &= ~(FLG_IN_TROUGH | FLG_IN_SLIDER);

    if (SCB_Timer(sw))
    {
	XtRemoveTimeOut(SCB_Timer(sw));
	SCB_Timer(sw) = 0;
    }
}

static void
Moved(Widget sw,
      XEvent *event,
      String *params,
      Cardinal *num_params)
{
    XMotionEvent *ev = (XMotionEvent *)event;
    int newval;
    XmScrollBarCallbackStruct cbs;

    if (!(SCB_Flags(sw) & FLG_IN_SLIDER))
	return;

    if (!(SCB_Flags(sw) & FLG_DRAG))
	return;

    erase_slider(sw);

    if (ev != NULL)
    {

	if (SCB_Orientation(sw) == XmHORIZONTAL)
	{
	    SCB_SliderX(sw) = ev->x - SCB_SeparationX(sw);
	    if (SCB_SliderX(sw) < SCB_SliderAreaX(sw))
	    {
		SCB_SliderX(sw) = SCB_SliderAreaX(sw);
	    }
	    if (SCB_SliderX(sw) > (SCB_SliderAreaX(sw) +
				   SCB_SliderAreaWidth(sw) -
					SCB_SliderWidth(sw)))
	    {
		SCB_SliderX(sw) = SCB_SliderAreaX(sw) +
					SCB_SliderAreaWidth(sw) -
					SCB_SliderWidth(sw);
	    }
	    newval = _XmScrollBarPosToValue(sw, SCB_SliderX(sw));
        }
	else
	{
	    SCB_SliderY(sw) = ev->y - SCB_SeparationY(sw);
	    if (SCB_SliderY(sw) < SCB_SliderAreaY(sw))
	    {
		SCB_SliderY(sw) = SCB_SliderAreaY(sw);
	    }
	    if (SCB_SliderY(sw) > (SCB_SliderAreaY(sw) +
				   SCB_SliderAreaHeight(sw) -
				   SCB_SliderHeight(sw)))
	    {
		SCB_SliderY(sw) = SCB_SliderAreaY(sw) +
					SCB_SliderAreaHeight(sw) -
					SCB_SliderHeight(sw);
	    }
	    newval = _XmScrollBarPosToValue(sw, SCB_SliderY(sw));
	}

	/* only call dragcallback if value really changed */
	if (SCB_Value(sw) != newval)
	{
	    SCB_Value(sw) = newval;

	    cbs.pixel = (SCB_Orientation(sw) == XmHORIZONTAL) ? ev->x : ev->y;
	    cbs.event = event;
	    cbs.value = SCB_Value(sw);
	    cbs.reason = XmCR_DRAG;
	    XtCallCallbackList((Widget)sw, SCB_DragCallback(sw), &cbs);
	}
    }

    draw_slider(sw);

    expose(sw, NULL, (Region)NULL);
}

static void
TopOrBottom(Widget sw,
           XEvent *event,
           String *params,
           Cardinal *num_params)
{
    XmScrollBarCallbackStruct cbs;
    Boolean toTopCallback = False;

    erase_slider(sw);

    cbs.event = event;

    if (event->type == KeyPress)
    {
	XKeyPressedEvent *ev = (XKeyPressedEvent *)event;
	Modifiers modifiers_return;
	KeySym keysym;

	cbs.pixel = (SCB_Orientation(sw) == XmHORIZONTAL) ? ev->x : ev->y;

        keysym = XtGetActionKeysym(event, &modifiers_return);

	/* This is per Motif: osfBeginLine results in a toTopCallback
	 * whereas all oher keys bound to TopOrBottom() result in a
	 * toBottomCallback -- PvH
	 */

	toTopCallback = (keysym == XStringToKeysym(&_XmStrings[12304]));
    }
    else
    {
	XButtonEvent *ev = (XButtonEvent *)event;
	int eventPos;

	/* Note: don't exclude the arrow regions, even have them selected ... */

	if (SCB_Orientation(sw) == XmHORIZONTAL)
	{
	    cbs.pixel = eventPos = ev->x;

	    if (eventPos >= Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) &&
		eventPos < SCB_SliderX(sw))
	    {
		/* left of the slider */

		if (eventPos < SCB_SliderAreaX(sw))
		{
		    SCB_Arrow1Selected(sw) = True;
		    redraw_arrows(sw);
		}

		toTopCallback = (SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT);
	    }
	    else if (eventPos < XtWidth(sw) - Prim_ShadowThickness(sw) -  Prim_HighlightThickness(sw) &&
		     eventPos >= SCB_SliderX(sw) + SCB_SliderWidth(sw))
	    {
		/* right of slider */
		if (eventPos > SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw))

		{
		    SCB_Arrow2Selected(sw) = True;
		    redraw_arrows(sw);
		}

		toTopCallback = (SCB_ProcessingDirection(sw) != XmMAX_ON_RIGHT);
	    }
	}
	else /* SCB_Orientation(sw) == XmVERTICAL */
	{
	    cbs.pixel = eventPos = ev->y;

	    if (eventPos >= Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) &&
		eventPos < SCB_SliderY(sw))
	    {
		/* up the slider */

		if (eventPos < SCB_SliderAreaY(sw))
		{
		    SCB_Arrow1Selected(sw) = True;
		    redraw_arrows(sw);
		}

		toTopCallback = (SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM);
	    }
	    else if (eventPos < XtHeight(sw) - Prim_ShadowThickness(sw) - Prim_HighlightThickness(sw) &&
		     eventPos >= SCB_SliderY(sw) + SCB_SliderHeight(sw))
	    {
		/* down the slider */

		if (eventPos > SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw))
		{
		    SCB_Arrow2Selected(sw) = True;
		    redraw_arrows(sw);
		}

		toTopCallback = (SCB_ProcessingDirection(sw) != XmMAX_ON_BOTTOM);
	    }
	}
    }

    SCB_Value(sw) = (toTopCallback) ? SCB_Minimum(sw) : SCB_Maximum(sw) - SCB_SliderSize(sw);
    if (SCB_Orientation(sw) == XmHORIZONTAL)
    {
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    }
    else
    {
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    }

    cbs.value = SCB_Value(sw);
    if (toTopCallback)
    {
        if (SCB_ToTopCallback(sw)) 
        {
	    cbs.reason = XmCR_TO_TOP;
	    XtCallCallbackList((Widget)sw, SCB_ToTopCallback(sw), &cbs);
	}
	else
	{
	    cbs.reason = XmCR_VALUE_CHANGED;
	    XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);
        }
    }
    else
    {
	if (SCB_ToBottomCallback(sw))
	{
	    cbs.reason = XmCR_TO_BOTTOM;
	    XtCallCallbackList((Widget)sw, SCB_ToBottomCallback(sw), &cbs);
	}
	else
	{
	    cbs.reason = XmCR_VALUE_CHANGED;
	    XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);
	}
    }

    check_pixel_constraints(sw); /* FIX ME: NEEDED? */
    draw_slider(sw);
    expose((Widget)sw, NULL, (Region)NULL);
}

static void
CancelDrag(Widget sw,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    XmScrollBarCallbackStruct cbs;

    if (!(SCB_Flags(sw) & FLG_DRAG))
    {
	_XmPrimitiveParentActivate(sw, event, params, num_params); /* Per O'Reilly 6B, I guess all the params are passed on, so FIX ME */
        return;
    }

    SCB_Flags(sw) &= ~FLG_DRAG;

    erase_slider(sw);

    SCB_Value(sw) = SCB_SavedValue(sw);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
    {
        SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	cbs.pixel = SCB_SliderX(sw); /* per Motif */
    }
    else
    {
        SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	cbs.pixel = SCB_SliderY(sw); /* per Motif */
    }

    cbs.event = event;
    cbs.value = SCB_Value(sw);
    cbs.reason = XmCR_VALUE_CHANGED;

    XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);

    draw_slider(sw);

    expose((Widget)sw, NULL, (Region)NULL);
}

/*
 * FIXME - MLM -- I don't know if the next four procs handle params right.
 */
static void
PageDownOrRight(Widget w,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    int which;

    if (*num_params != 1) {
	_XmWarning(w, "PageDownOrRight: num_params wrong for widget");
	which = 0;
    }
    else
	which = atoi(params[0]);

    erase_slider(w);

    if (which == 0) {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_TOP)
	    decrementByPage(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_BOTTOM)
	    incrementByPage(w, event);
    }
    else {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_RIGHT)
	    incrementByPage(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_LEFT)
	    decrementByPage(w, event);
    }

    draw_slider(w);

    expose(w, NULL, (Region)NULL);
}

static void
PageUpOrLeft(Widget w,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    int which;

    if (*num_params != 1) {
	_XmWarning(w, "PageDownOrRight: num_params wrong for widget");
	which = 0;
    }
    else
	which = atoi(params[0]);

    erase_slider(w);

    if (which == 0) {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_TOP)
	    decrementByPage(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_BOTTOM)
	    decrementByPage(w, event);
    }
    else {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_RIGHT)
	    decrementByPage(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_LEFT)
	    incrementByPage(w, event);
    }

    draw_slider(w);

    expose(w, NULL, (Region)NULL);
}

static void
IncrementDownOrRight(Widget w,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    int which;

    if (*num_params != 1) {
	_XmWarning(w, "PageDownOrRight: num_params wrong for widget");
	which = 0;
    }
    else
	which = atoi(params[0]);

    erase_slider(w);

    if (which == 0) {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_TOP)
	    decrement(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_BOTTOM)
	    increment(w, event);
    }
    else {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_RIGHT)
	    increment(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_LEFT)
	    decrement(w, event);
    }

    draw_slider(w);

    expose(w, NULL, (Region)NULL);
}

static void
IncrementUpOrLeft(Widget w,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    int which;

    if (*num_params != 1) {
	_XmWarning(w, "PageDownOrRight: num_params wrong for widget");
	which = 0;
    }
    else
	which = atoi(params[0]);

    erase_slider(w);

    if (which == 0) {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_TOP)
	    increment(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_BOTTOM)
	    decrement(w, event);
    }
    else {
	if (SCB_ProcessingDirection(w) == XmMAX_ON_RIGHT)
	    decrement(w, event);
	if (SCB_ProcessingDirection(w) == XmMAX_ON_LEFT)
	    increment(w, event);
    }

    draw_slider(w);

    expose(w, NULL, (Region)NULL);
}

/* FIX ME: Should timers be canceled if (page)?(inc|dec)rementing has reached 
 * (ceiling|floor) ?
 */

/* timer callbacks for moving by holding down the mouse button */

/* The FLG_DRAG test lines in the next functions were introduced to make
 * CancelDrag work. They prevent key (page)?(inc/dec)rementing to
 * work. Well, they seem to be redundant anyway because of the canceled() test
 * in the button timer callbacks. -- PvH
 */
static void
increment(Widget sw, XEvent *event)
{
    XmScrollBarCallbackStruct cbs;

    if (SCB_Value(sw) == SCB_Maximum(sw) - SCB_SliderSize(sw))
	return;

/*
    if (!(SCB_Flags(sw) & FLG_DRAG))
        return;
*/

    SCB_Value(sw) += SCB_Increment(sw);

    if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw))
	SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    else
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));

    check_pixel_constraints(sw);

    cbs.event = event;
    cbs.value = SCB_Value(sw);
    cbs.pixel = 0;

    if (SCB_IncrementCallback(sw))
    {
	cbs.reason = XmCR_INCREMENT;
	XtCallCallbackList((Widget)sw, SCB_IncrementCallback(sw), &cbs);
    }
    else
    {
	cbs.reason = XmCR_VALUE_CHANGED;
	XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);
    }
}

static void
decrement(Widget sw, XEvent *event)
{
    XmScrollBarCallbackStruct cbs;

    if (SCB_Value(sw) == SCB_Minimum(sw))
	return;
/*
    if (!(SCB_Flags(sw) & FLG_DRAG))
        return;
*/

    SCB_Value(sw) -= SCB_Increment(sw);

    if (SCB_Value(sw) < SCB_Minimum(sw))
	SCB_Value(sw) = SCB_Minimum(sw);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    else
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));

    check_pixel_constraints(sw);

    cbs.event = event;
    cbs.value = SCB_Value(sw);
    cbs.pixel = 0;

    if (SCB_DecrementCallback(sw))
    {
	cbs.reason = XmCR_DECREMENT;
	XtCallCallbackList((Widget)sw, SCB_DecrementCallback(sw), &cbs);
    }
    else
    {
	cbs.reason = XmCR_VALUE_CHANGED;
	XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);
    }
}

static void
incrementByPage(Widget sw, XEvent *event)
{
    XmScrollBarCallbackStruct cbs;

    if (SCB_Value(sw) == SCB_Maximum(sw) - SCB_SliderSize(sw))
	return;
/*
    if (!(SCB_Flags(sw) & FLG_DRAG))
        return;
*/

    SCB_Value(sw) += SCB_PageIncrement(sw);

    if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw))
	SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    else
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));

    check_pixel_constraints(sw);

    cbs.event = event;
    cbs.value = SCB_Value(sw);
    cbs.pixel = 0;

    if (SCB_PageIncrementCallback(sw))
    {
	cbs.reason = XmCR_PAGE_INCREMENT;
	XtCallCallbackList((Widget)sw, SCB_PageIncrementCallback(sw), &cbs);
    }
    else
    {
	cbs.reason = XmCR_VALUE_CHANGED;
	XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);
    }
}

static void
decrementByPage(Widget sw, XEvent *event)
{
    XmScrollBarCallbackStruct cbs;

    if (SCB_Value(sw) == SCB_Minimum(sw))
	return;
/*
    if (!(SCB_Flags(sw) & FLG_DRAG))
        return;
*/

    SCB_Value(sw) -= SCB_PageIncrement(sw);

    if (SCB_Value(sw) < SCB_Minimum(sw))
	SCB_Value(sw) = SCB_Minimum(sw);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    else
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));

    check_pixel_constraints(sw);

    cbs.event = event;
    cbs.value = SCB_Value(sw);
    cbs.pixel = 0;

    if (SCB_PageDecrementCallback(sw))
    {
	cbs.reason = XmCR_PAGE_DECREMENT;
	XtCallCallbackList((Widget)sw, SCB_PageDecrementCallback(sw), &cbs);
    }
    else
    {
	cbs.reason = XmCR_VALUE_CHANGED;
	XtCallCallbackList((Widget)sw, SCB_ValueChangedCallback(sw), &cbs);
    }
}


static int
canceled(Widget sw)
{
    if (!(SCB_Flags(sw) & FLG_DRAG))
    {
        if (SCB_Timer(sw))
	{
	  XtRemoveTimeOut(SCB_Timer(sw));
	  SCB_Timer(sw) = 0;
	}

	return 1;
    }
    return 0;
}

static void
buttonTimer(XtPointer clientData,
	    XtIntervalId *id)
{
    Widget sw = (Widget)clientData;

    if (canceled(sw)) return;

    if (!(SCB_Arrow1Selected(sw) || SCB_Arrow2Selected(sw)))
	return;

    /* first, clear the slider, before we move it. */

    erase_slider(sw);

    if ((SCB_Arrow1Selected(sw) && 
	 (SCB_ProcessingDirection(sw) == XmMAX_ON_TOP ||
	  SCB_ProcessingDirection(sw) == XmMAX_ON_LEFT)) ||
	(SCB_Arrow2Selected(sw) &&
	 (SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM ||
	  SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT)))
	increment(sw, NULL);
    else
	decrement(sw, NULL);

    draw_slider(sw);
    expose((Widget)sw, NULL, (Region)NULL);

    if (SCB_Timer(sw))
	XtRemoveTimeOut(SCB_Timer(sw));

    SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)sw),
				    SCB_RepeatDelay(sw), 
					  buttonTimer, 
					  (XtPointer)sw);
}

static void
troughIncTimer(XtPointer clientData,
	       XtIntervalId *id)
{
    Widget sw = (Widget)clientData;

    if (canceled(sw)) return;

    /* first, clear the slider, before we move it. */

    if (!(SCB_Flags(sw) & FLG_IN_TROUGH))
	return;

    erase_slider(sw);

    incrementByPage(sw, NULL);

    draw_slider(sw);

    expose((Widget)sw, NULL, (Region)NULL);

    if (SCB_Timer(sw))
	XtRemoveTimeOut(SCB_Timer(sw));

    SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)sw), 
				    SCB_RepeatDelay(sw), 
				    troughIncTimer, 
				    (XtPointer)sw);
}

static void
troughDecTimer(XtPointer clientData,
	       XtIntervalId *id)
{
    Widget sw = (Widget)clientData;

    if (!(SCB_Flags(sw) & FLG_IN_TROUGH))
	return;

    if (canceled(sw)) return;

    /* first, clear the slider, before we move it. */

    erase_slider(sw);

    decrementByPage(sw, NULL);

    draw_slider(sw);

    expose((Widget)sw, NULL, (Region)NULL);

    if (SCB_Timer(sw))
	XtRemoveTimeOut(SCB_Timer(sw));

    SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)sw), 
				    SCB_RepeatDelay(sw),
				    troughDecTimer,
				    (XtPointer)sw);
}

void
_XmSetEtchedSlider(XmScrollBarWidget sw)
{
    /* FIX ME: Guess this is what it is supposed to do */
    SCB_EtchedSlider(sw) = True;
}

Position
_XmScrollBarValueToPos(Widget sw, int value)
{
    if (SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM ||
	SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT) {
	switch (SCB_Orientation(sw)) {
	case XmHORIZONTAL:
	    if ((SCB_Maximum(sw) - SCB_Minimum(sw) - SCB_SliderSize(sw)) == 0)
		return SCB_SliderAreaX(sw);
	    return (int)((float)(value - SCB_Minimum(sw)) /
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw)
				 - SCB_SliderSize(sw)) *
			 (float) (SCB_SliderAreaWidth(sw) -
				  SCB_SliderWidth(sw)) + SCB_SliderAreaX(sw));
	case XmVERTICAL:
	    if ((SCB_Maximum(sw) - SCB_Minimum(sw) - SCB_SliderSize(sw)) == 0)
		return SCB_SliderAreaY(sw);
	    return (int)((float)(value - SCB_Minimum(sw)) /
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw)
				- SCB_SliderSize(sw)) *
			 (float)(SCB_SliderAreaHeight(sw) -
				 SCB_SliderHeight(sw)) + SCB_SliderAreaY(sw));
	default:
	    _XmError((Widget)sw,
		     "Orientation is neither VERTICAL nor HORIZONTAL in ScrollBar: %s\n",
		     XtName(sw));
	}
    }
    else if (SCB_ProcessingDirection(sw) == XmMAX_ON_TOP ||
	     SCB_ProcessingDirection(sw) == XmMAX_ON_LEFT) {
	switch (SCB_Orientation(sw)) {
	case XmHORIZONTAL:
	    if ((SCB_Maximum(sw) - SCB_Minimum(sw) - SCB_SliderSize(sw)) == 0)
		return SCB_SliderAreaX(sw);
	    return (SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw)) -
	    	   (int)((float)(value - SCB_Minimum(sw)) /
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw)
				 - SCB_SliderSize(sw)) *
			 (float) (SCB_SliderAreaWidth(sw) -
				  SCB_SliderWidth(sw))) -
		   SCB_SliderWidth(sw);
	case XmVERTICAL:
	    if ((SCB_Maximum(sw) - SCB_Minimum(sw) - SCB_SliderSize(sw)) == 0)
		return SCB_SliderAreaY(sw);
	    return (SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw)) -
	    	   (int)((float)(value - SCB_Minimum(sw)) /
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw)
				- SCB_SliderSize(sw)) *
			 (float)(SCB_SliderAreaHeight(sw) -
				 SCB_SliderHeight(sw))) -
		   SCB_SliderHeight(sw);
	    break;
	default:
	    _XmError((Widget)sw,
		     "Orientation is neither VERTICAL nor HORIZONTAL in ScrollBar: %s\n",
		     XtName(sw));
	}
    }
    else
	_XmError((Widget)sw,
		 "Processing direction invalid for widget %s", XtName(sw));
    return 0;
}

static int
_XmScrollBarPosToValue(Widget sw, Position pos)
{
    if (SCB_ProcessingDirection(sw) == XmMAX_ON_BOTTOM ||
	SCB_ProcessingDirection(sw) == XmMAX_ON_RIGHT) {
	switch (SCB_Orientation(sw)) {
	case XmHORIZONTAL:
	    if (SCB_SliderAreaWidth(sw) <= SCB_SliderWidth(sw))
		return SCB_Maximum(sw) - SCB_SliderSize(sw); /* FIX ME: return what? == instead of <= ?? */

	    return (int)((float)(pos - SCB_SliderAreaX(sw)) /
			 (float)(SCB_SliderAreaWidth(sw) -
				  SCB_SliderWidth(sw)) *
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw) -
				 SCB_SliderSize (sw)) +
			 (float)SCB_Minimum(sw) + 0.5);
	case XmVERTICAL:
	    if (SCB_SliderAreaHeight(sw) <= SCB_SliderHeight(sw))
		return SCB_Maximum(sw) - SCB_SliderSize(sw); /* FIX ME: return what? == instead of <= ?? */

	    return (int)((float)(pos - SCB_SliderAreaY(sw)) /
			 (float)(SCB_SliderAreaHeight(sw) -
				 SCB_SliderHeight(sw)) *
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw) -
				 SCB_SliderSize(sw)) +
			 (float) SCB_Minimum(sw) + 0.5);
	default:
	    _XmError((Widget)sw,
		     "Orientation is neither VERTICAL nor HORIZONTAL in ScrollBar: %s\n",
		     XtName(sw));
	}
    }
    else if (SCB_ProcessingDirection(sw) == XmMAX_ON_TOP ||
	     SCB_ProcessingDirection(sw) == XmMAX_ON_LEFT) {
	switch (SCB_Orientation(sw)) {
	case XmHORIZONTAL:
	    if ((SCB_Maximum(sw) - SCB_Minimum(sw) - SCB_SliderSize(sw)) == 0)
		return 0;
	    return (SCB_Maximum(sw) + SCB_Minimum(sw) - SCB_SliderSize(sw)) -
		   (int)((float)(pos - SCB_SliderAreaX(sw)) /
			 (float)(SCB_SliderAreaWidth(sw) -
				  SCB_SliderWidth(sw)) *
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw) -
				 SCB_SliderSize(sw)) +
			 (float)SCB_Minimum(sw) + 0.5);
	case XmVERTICAL:
	    if ((SCB_Maximum(sw) - SCB_Minimum(sw) - SCB_SliderSize(sw)) == 0)
		return 0;
	    return (SCB_Maximum(sw) + SCB_Minimum(sw) - SCB_SliderSize(sw)) -
		   (int)((float)(pos - SCB_SliderAreaY(sw)) /
			 (float)(SCB_SliderAreaHeight(sw) -
				 SCB_SliderHeight(sw)) *
			 (float)(SCB_Maximum(sw) - SCB_Minimum(sw) -
				 SCB_SliderSize(sw)) +
			 (float) SCB_Minimum(sw) + 0.5);
	    break;
	default:
	    _XmError((Widget)sw,
		     "Orientation is neither VERTICAL nor HORIZONTAL in ScrollBar: %s\n",
		     XtName(sw));
	}
    }
    else
	_XmError((Widget)sw,
		 "Processing direction invalid for widget %s", XtName(sw));
    return 0;
}

Dimension
_XmScrollBarSliderPixSize(Widget sw) {
    Dimension ret;
    int minmax;

    minmax = SCB_Maximum(sw) - SCB_Minimum(sw);
    switch (SCB_Orientation(sw)) {
    case XmHORIZONTAL:
	if ((minmax - SCB_SliderSize(sw)) == 0)
	    return SCB_SliderAreaWidth(sw);
	ret = (Dimension)(((float)SCB_SliderSize(sw) / (float)minmax) *
			   (float)SCB_SliderAreaWidth(sw) +
			  0.5);
	return ret;
    case XmVERTICAL:
	if ((minmax - SCB_SliderSize(sw)) == 0)
	    return SCB_SliderAreaHeight(sw);
	ret = (Dimension)(((float)SCB_SliderSize(sw) / (float)minmax) *
			   (float)SCB_SliderAreaHeight(sw) +
			  0.5);
	return ret;
    default:
	_XmError((Widget)sw,
		 "Orientation is neither VERTICAL nor HORIZONTAL in ScrollBar: %s\n",
		 XtName(sw));
    }
    return 0;
}

static int
_XmScrollBarArrowWidth(Widget w)
{
    int size = 0, hor_size;

    if (SCB_Orientation(w) == XmHORIZONTAL) {
	if (XtHeight(w) == 0)
	    return DEF_SB_DIM;

	/*
	 * Don't change these three lines lightly. Remember that a + b/2 need
	 * not equal (2*a + b)/2, depending on the rounding in case of negative
	 * dividends. -- PvH
	 */
	size = (int)XtHeight(w) -
		2 * (int)(Prim_ShadowThickness(w) + Prim_HighlightThickness(w));

	hor_size = (int)(XtWidth(w) -
			(int)(MIN_SLIDER_LENGTH + 
			      2 * ARROW_SPACING +
			      2 * Prim_ShadowThickness(w) +
			      2 * Prim_HighlightThickness(w))) / 2;

	if (hor_size < size)
	    size = hor_size;
    }
    else if (SCB_Orientation(w) == XmVERTICAL) {
	if (XtWidth(w) == 0)
	    return DEF_SB_DIM;

	size = (int)XtWidth(w) -
	        2 * (int)(Prim_ShadowThickness(w) + Prim_HighlightThickness(w));
    }
    else
	_XmError((Widget)w, "ScrollBar Orientation is incorrect");

    return size;
}

static int
_XmScrollBarArrowHeight(Widget w)
{
    int size = 0, ver_size;

    if (SCB_Orientation(w) == XmHORIZONTAL) {
	if (XtHeight(w) == 0)
	    return DEF_SB_DIM;

	size = (int)XtHeight(w) -
		2 * (int)(Prim_ShadowThickness(w) + Prim_HighlightThickness(w));
    }
    else if (SCB_Orientation(w) == XmVERTICAL) {
	if (XtWidth(w) == 0)
	    return DEF_SB_DIM;

	/*
	 * Don't change these three lines lightly. Remember that a + b/2 need
	 * not equal (2*a + b)/2, depending on the rounding in case of negative
	 * dividends. -- PvH
	 */
	size = (int)XtWidth(w) -
		2 * (int)(Prim_ShadowThickness(w) + Prim_HighlightThickness(w));

	ver_size = (int)(XtHeight(w) -
			(int)(MIN_SLIDER_LENGTH + 
			      2 * ARROW_SPACING +
			      2 * Prim_ShadowThickness(w) +
			      2 * Prim_HighlightThickness(w))) / 2;

	if (ver_size < size)
	    size = ver_size;
    }
    else
	_XmError((Widget)w, "ScrollBar Orientation is incorrect");

    return size;
}
