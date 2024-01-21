/**
 *
 * $Id: TearOffB.c,v 1.7 1996/11/28 09:22:05 u27113 Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
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

static char rcsid[] = "$Id: TearOffB.c,v 1.7 1996/11/28 09:22:05 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/MenuShell.h>
#include <Xm/MenuUtilP.h>
#include <Xm/TearOffBP.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <X11/cursorfont.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);


/* 
 * Resources for the tearoffbutton class 
 */
#define Offset(field) XtOffsetOf(XmTearOffButtonRec, tear_off_button.field)
static XtResource resources[] = {
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmHORIZONTAL
    },
    {
	XmNseparatorType, XmCSeparatorType, XmRSeparatorType,
	sizeof(unsigned char), Offset(separator_type),
	XmRImmediate, (XtPointer)XmSHADOW_ETCHED_OUT_DASH
    },
    {
	XmNmargin, XmCMargin, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmTearOffButtonRec, core.height),
	XmRImmediate, (XtPointer)1
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNmargin,
	sizeof(Dimension), Offset(margin),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};


static void BDrag(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void BActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void KActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmTearOffB_overrideTranslations[] = 
   "<Btn2Down>:           BDrag()\n\
    <BtnUp>:              BActivate()\n\
    :<Key>osfSelect:      KActivate()\n\
    :<Key>osfActivate:    KActivate()\n\
    ~s ~m ~a <Key>Return: KActivate()\n\
    ~s ~m ~a <Key>space:  KActivate()";

static XtActionsRec actions[] = {
    {"BDrag", BDrag},
    {"BActivate", BActivate},
    {"KActivate", KActivate}
};

static XmBaseClassExtRec _XmTearOffBCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmTearOffBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmTearOffButtonClassRec xmTearOffButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPushButtonClassRec,
        /* class_name            */ "XmTearOffButton",
	/* widget_size           */ sizeof(XmTearOffButtonRec),
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
	/* resize                */ resize, 
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmTearOffBCoreClassExtRec
    },
    /* Primitive Class part */
    {
        /* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ XtInheritTranslations,
        /* arm_and_activate_proc */ XmInheritArmAndActivate,
        /* Synthetic Resources   */ syn_resources,
        /* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmTearOffBPrimClassExtRec
    },
    /* Label Class part */
    {
        /* setOverrideCallback */ XmInheritSetOverrideCallback,
        /* menuProcs           */ XmInheritMenuProc,
        /* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* Push Button part */
    {
        /* extension           */ NULL
    },
    /* Tear Off Button part */
    {
       /* translations         */ _XmTearOffB_overrideTranslations,
    },
};

WidgetClass xmTearOffButtonWidgetClass = (WidgetClass)&xmTearOffButtonClassRec;

static void
class_initialize()
{
    _XmTearOffBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmTearOffButtonWidgetClass tobc = (XmTearOffButtonWidgetClass)widget_class;

    /* Handle tear off button class part inheritance */

    if (tobc->tearoffbutton_class.translations == XtInheritTranslations)
	tobc->tearoffbutton_class.translations = _XmTearOffB_overrideTranslations;

    _XmFastSubclassInit(widget_class, XmTEAROFF_BUTTON_BIT);
}

static void
CreateBottomShadowGC(Widget pw)
{
    XGCValues values;
    unsigned long mask;

    if (Prim_BottomShadowPixmap(pw) != None 
	&& Prim_BottomShadowPixmap(pw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed bottom shadow*/
	mask = GCTile | GCFillStyle;
	
	values.tile = Prim_BottomShadowPixmap(pw); 
	values.fill_style = FillTiled;
    }
    else
    {
	mask = GCForeground | GCBackground;

	values.foreground = Prim_BottomShadowColor(pw);
	values.background = XtBackground(pw);
    }

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineDoubleDash;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_BottomShadowGC(pw) = XtGetGC((Widget)pw, mask, &values);    
}

static void
CreateTopShadowGC(Widget pw)
{
    XGCValues values;
    unsigned long mask;

    if (Prim_TopShadowPixmap(pw) != None 
	&& Prim_TopShadowPixmap(pw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed top shadow*/
	mask = GCTile | GCFillStyle;
	
	values.tile = Prim_TopShadowPixmap(pw); 
	values.fill_style = FillTiled;
    }
    else
    {
	mask = GCForeground | GCBackground;

	values.foreground = Prim_TopShadowColor(pw);
	values.background = XtBackground(pw);
    }

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle;
    values.line_width = 1;
    values.line_style = LineDoubleDash;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_TopShadowGC(pw) = XtGetGC((Widget)pw, mask, &values);
}

static void 
CreateSeparatorGC(Widget w)
{
    XGCValues values;
    long value_mask;

    value_mask = GCBackground | GCForeground | GCLineStyle | GCCapStyle |
			GCJoinStyle;

    switch (TOB_SeparatorType(w)) {
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
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    case XmSHADOW_ETCHED_OUT_DASH:
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;

    default:
	TOB_SeparatorType(w) = XmSINGLE_LINE;
	values.line_style = LineSolid;
	values.join_style = JoinMiter;
	values.cap_style = CapButt;
	values.foreground = Prim_Foreground(w);
	values.background = XtBackground(w);
	break;
    }

    TOB_SeparatorGC(w) = XtGetGC(w, value_mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmTearOffButtonWidgetClass tobc = 
	(XmTearOffButtonWidgetClass)new_w->core.widget_class;

    XmString xmstr = XmStringCreateSimple("");

    XdbDebug(__FILE__, new_w, "TOB_initialize\n");
    
    XtWidth(new_w) = 2 * (Lab_Shadow(new_w) + Lab_Highlight(new_w));
    XtHeight(new_w) = 2 * (Lab_Shadow(new_w) + Lab_Highlight(new_w));

    TOB_Margin(new_w) = 5;

    if (TOB_Orientation(new_w) == XmVERTICAL)
	XtWidth(new_w) += TOB_Margin(new_w);
    if (TOB_Orientation(new_w) == XmHORIZONTAL)
	XtHeight(new_w) += TOB_Margin(new_w);

    CreateSeparatorGC(new_w);
    CreateTopShadowGC(new_w);
    CreateBottomShadowGC(new_w);
   
    XtOverrideTranslations(new_w, 
 			   XtParseTranslationTable(tobc->tearoffbutton_class.translations));


    _XmStringFree(Lab_Label(new_w));
    Lab_Label(new_w) = _XmStringCreate(xmstr);
    XmStringFree(xmstr);

    if (XtWidth(request) == 0 || XtHeight(request) == 0) {
	XtWidth(new_w) = 0;
	XtHeight(new_w) = 0;

	_XmCalcLabelDimensions(new_w);

	(*xmLabelClassRec.core_class.resize)(new_w);
    }
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, TOB_SeparatorGC(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = True;	/* FIX ME */

    XdbDebug(__FILE__, new_w, "TOB_SetValues()\n");

    return refresh_needed;
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    XdbDebug(__FILE__, w, "TearOffB expose\n");

    _XmDrawSeparator(XtDisplay(w),
		     XtWindow(w),
		     Prim_TopShadowGC(w),
		     Prim_BottomShadowGC(w),
		     TOB_SeparatorGC(w),
		     0, 0,
		     XtWidth(w), XtHeight(w),
		     /*Lab_Shadow(w) * 2*/ Lab_Shadow(w),
		     TOB_Margin(w) + Lab_Shadow(w),
		     TOB_Orientation(w),
		     TOB_SeparatorType(w));

/*
 * Why ??
 */
#define superclass (&xmPushButtonClassRec)
    (*superclass->core_class.expose)(w, event, region);
#undef superclass
}
   
static void
resize(Widget w)
{
}

static XtGeometryResult 
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    XtWidgetGeometry	a;	/* Standin for answer if NULL parameter */

    XdbDebug(__FILE__, w, "TOB_query_geometry\n");

#define	Wants(x)	(proposed->request_mode & x)

    if (proposed->request_mode != 0) {	/* NULL case should not yet end here ! */
	if ((! (Wants(CWWidth))) && (! Wants(CWHeight))) {
		/* If they don't ask width/height, let them have whatever they like */
	    if (answer)
		*answer = *proposed;
	    return XtGeometryYes;
	}
    }

    if (TOB_Orientation(w) == XmVERTICAL) 
    {
	a.width = TOB_Margin(w) + 2 * (Lab_Shadow(w) + Lab_Highlight(w));
	a.height = 2 * (Lab_Shadow(w) + Lab_Highlight(w));
    }
    else 
    {
	a.width = 2 * (Lab_Shadow(w) + Lab_Highlight(w));
	a.height = TOB_Margin(w) + 2 * (Lab_Shadow(w) + Lab_Highlight(w));
    }

    a.request_mode = CWWidth | CWHeight;

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

    if (proposed->width >= answer->width && proposed->height >= answer->height) 
	return XtGeometryYes;
    else if (answer->width == XtWidth(w) && answer->height == XtHeight(w)) {
	if (answer)
	    answer->request_mode = 0;
	return XtGeometryNo;
    } else 
	return XtGeometryAlmost;
}

static void 
BDrag(Widget w,
      XEvent *event, 
      String *params, 
      Cardinal *num_params)
{
  /* create a glyph cursor, do an asynchronouse grab of the pointer and
     keyboard, and select button events so that we can be notified when
     the user releases the drag. */
  Cursor dragCursor;

  XdbDebug(__FILE__, w, "BDrag\n");

  dragCursor = XCreateFontCursor(XtDisplay(w), XC_fleur);
/* FIX ME */
}

static void 
BActivate(Widget w, 
          XEvent *event, 
          String *params, 
          Cardinal *num_params)
{
    Widget parent,shell;
    Boolean validButton, poppedUp;

    XdbDebug(__FILE__, w, "BActivate\n");

    parent = XtParent(w);

    shell = parent;
    while (!XtIsShell(shell))
	shell = XtParent(shell);

    if (event && (event->type == ButtonRelease))
	(* xmLabelClassRec.label_class.menuProcs) (XmMENU_BUTTON, parent, NULL, event, &validButton);

    if (!validButton)
	return;

    if (!XmIsMenuShell(shell))
	(* xmLabelClassRec.label_class.menuProcs) (XmMENU_POPDOWN, w, NULL, event, &poppedUp);
    else
	(* xmLabelClassRec.label_class.menuProcs) (XmMENU_BUTTON_POPDOWN, w , NULL, event, &poppedUp);

    _XmRecordEvent(event);
    
    if (poppedUp)
	return;

    PB_Armed(w) = False;

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   Lab_Highlight(w),
		   Lab_Highlight(w),
		   XtWidth(w),
		   XtHeight(w),
		   Lab_Shadow(w),
		   XmNO_LINE);

    _XmSetInDragMode(w, False);

    _XmTearOffInitiate(w, event);
}

static void 
KActivate(Widget w, 
          XEvent *event, 
          String *params, 
          Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "KActivate\n");

    _XmTearOffInitiate(w, event);
}
