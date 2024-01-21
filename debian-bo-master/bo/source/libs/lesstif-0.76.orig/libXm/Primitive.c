/**
 *
 * $Id: Primitive.c,v 1.8 1997/01/11 02:19:49 miers Exp $
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

static char rcsid[] = "$Id: Primitive.c,v 1.8 1997/01/11 02:19:49 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/RepType.h>
#include <Xm/ManagerP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

static void focus_change(Widget w, XmFocusChange fevent);
static XmNavigability widget_navigable(Widget w);

static void primitive_border_unhighlight(Widget w);
static void primitive_border_highlight(Widget w);

#define Offset(field) XtOffsetOf(XmPrimitiveRec, primitive.field)

/* Resources for the primitive class */
static XtResource resources[] = {
    {
	XmNunitType, XmCUnitType, XmRUnitType,
	sizeof(unsigned char), Offset(unit_type),
	XmRCallProc, (XtPointer)_XmUnitTypeDefault
    },
    {
	XmNx, XmCPosition, XmRHorizontalPosition,
	sizeof(Position), XtOffsetOf(XmPrimitiveRec, core.x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNy, XmCPosition, XmRVerticalPosition,
	sizeof(Position), XtOffsetOf(XmPrimitiveRec, core.y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNwidth, XmCDimension, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNborderWidth, XmCBorderWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNforeground, XmCForeground, XmRPixel,
	sizeof(Pixel), Offset(foreground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNbackground, XmCBackground, XmRPixel,
	sizeof(Pixel), XtOffsetOf(XmPrimitiveRec, core.background_pixel),
	XmRCallProc, (XtPointer)_XmBackgroundColorDefault
    },
    {
	XmNbackgroundPixmap, XmCPixmap, XmRXmBackgroundPixmap,
	sizeof(Pixmap), XtOffsetOf(XmPrimitiveRec, core.background_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), Offset(traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightOnEnter, XmCHighlightOnEnter, XmRBoolean,
	sizeof(Boolean), Offset(highlight_on_enter),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), Offset(navigation_type),
	XmRImmediate, (XtPointer)XmNONE
    },  
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNhighlightColor, XmCHighlightColor, XmRPixel,
	sizeof(Pixel), Offset(highlight_color),
	XmRCallProc, (XtPointer)_XmHighlightColorDefault
    },
    {
	XmNhighlightPixmap, XmCHighlightPixmap, XmRHighlightPixmap,
	sizeof(Pixmap), Offset(highlight_pixmap),
	XmRCallProc, (XtPointer)NULL /* FIXME: NEED PROC HERE */
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtopShadowColor, XmCTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(top_shadow_color),
	XmRCallProc, (XtPointer)_XmTopShadowColorDefault
    },
    {
	XmNtopShadowPixmap, XmCTopShadowPixmap, XmRTopShadowPixmap,
	sizeof(Pixmap), Offset(top_shadow_pixmap),
	XmRCallProc, (XtPointer)NULL /* FIXME: NEED PROC HERE */
    },
    {
	XmNbottomShadowColor, XmCBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(bottom_shadow_color),
	XmRCallProc, (XtPointer)_XmBottomShadowColorDefault
    },
    {
	XmNbottomShadowPixmap, XmCBottomShadowPixmap, XmRBottomShadowPixmap,
	sizeof(Pixmap), Offset(bottom_shadow_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNhelpCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(help_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNuserData, XmCUserData, XmRPointer,
	sizeof(XtPointer), Offset(user_data),
	XmRImmediate, (XtPointer)NULL
    }
};

#define POffset(field)	XtOffset(XmPrimitiveWidget, primitive.field)
#define COffset(field)	XtOffset(XmPrimitiveWidget, core.field)
static XmSyntheticResource syn_resources[] = {
    /* core part */
    {
	XmNx, 
	sizeof(Position), COffset(x),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNy, 
	sizeof(Position), COffset(y),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNwidth, 
	sizeof(Dimension), COffset(width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNheight, 
	sizeof(Dimension), COffset(height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNborderWidth, 
	sizeof(Dimension), COffset(border_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    /* primitive */
    {
	XmNhighlightThickness, 
	sizeof(Dimension), POffset(highlight_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNshadowThickness, 
	sizeof(Dimension), POffset(shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
};

char _XmPrimitive_defaultTranslations[] = 
     "<Unmap>:              PrimitiveUnmap()\n\
      :<Key>osfUp:          PrimitiveTraverseUp()\n\
      :<Key>osfDown:        PrimitiveTraverseDown()\n\
      :<Key>osfLeft:        PrimitiveTraverseLeft()\n\
      :<Key>osfRight:       PrimitiveTraverseRight()\n\
      :<Key>osfBeginLine:   PrimitiveTraverseHome()\n\
      :<Key>osfNextField:   PrimitiveNextTabGroup()\n\
      :<Key>osfPrevField:   PrimitivePrevTabGroup()\n\
      :<Key>osfActivate:    PrimitiveParentActivate()\n\
      :<Key>osfCancel:      PrimitiveParentCancel()\n\
      :<Key>osfHelp:        PrimitiveHelp()\n\
      <FocusIn>:            PrimitiveFocusIn()\n\
      <FocusOut>:           PrimitiveFocusOut()\n\
      s ~m ~a <Key>Tab:     PrimitivePrevTabGroup()\n\
      ~s ~m <Key>Tab:       PrimitiveNextTabGroup()\n\
      ~s ~m ~a <Key>Return: PrimitiveParentActivate()\n";

static XtActionsRec actions[] = {
    {"PrimitiveLeave",          _XmPrimitiveLeave},
    {"PrimitiveEnter",          _XmPrimitiveEnter},
    {"PrimitiveTraverseUp",     _XmTraverseUp},
    {"PrimitiveTraverseDown",   _XmTraverseDown},
    {"PrimitiveTraverseLeft",   _XmTraverseLeft},
    {"PrimitiveTraverseRight",  _XmTraverseRight},
    {"PrimitiveTraverseHome",   _XmTraverseHome},
    {"PrimitiveNextTabGroup",   _XmTraverseNextTabGroup},
    {"PrimitivePrevTabGroup",   _XmTraversePrevTabGroup},
    {"PrimitiveParentActivate", _XmPrimitiveParentActivate},
    {"PrimitiveParentCancel",   _XmPrimitiveParentCancel},
    {"PrimitiveHelp",           _XmPrimitiveHelp},
    {"PrimitiveFocusIn",        _XmPrimitiveFocusIn},
    {"PrimitiveFocusOut",       _XmPrimitiveFocusOut},
    {"PrimitiveUnmap",          _XmPrimitiveUnmap},
    {"unmap",          _XmPrimitiveUnmap},
    {"Help",           _XmPrimitiveHelp},
    {"enter",          _XmPrimitiveLeave},
    {"leave",          _XmPrimitiveEnter},
    {"PrevTabGroup",   _XmTraverseNextTabGroup},
    {"NextTabGroup",   _XmTraversePrevTabGroup},
};

static XmBaseClassExtRec _XmPrimitiveCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ NULL,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ NULL,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ widget_navigable,
    /* focus_change              */ focus_change,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmPrimitiveClassRec xmPrimitiveClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &coreClassRec,
        /* class_name            */ "XmPrimitive",
	/* widget_size           */ sizeof(XmPrimitiveRec),
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
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ _XmPrimitiveGetValuesHook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmPrimitiveCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ primitive_border_highlight,
        /* border_unhighlight    */ primitive_border_unhighlight,
        /* translations          */ _XmPrimitive_defaultTranslations,
        /* arm_and_activate_proc */ NULL,
        /* Synthetic Resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
        /* extension             */ (XtPointer)&_XmPrimClassExtRec
    }
};

WidgetClass xmPrimitiveWidgetClass = (WidgetClass)&xmPrimitiveClassRec;

static void
class_initialize()
{
    _XmInitializeExtensions();
    _XmPrimitiveCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmPrimitiveWidgetClass pwc, swc;

    pwc = (XmPrimitiveWidgetClass)widget_class;
    swc = (XmPrimitiveWidgetClass)widget_class->core_class.superclass;

    /* Handle the Primitive Widget class's inheritance stuff */

    if (pwc->primitive_class.border_highlight == XmInheritBorderHighlight)
	pwc->primitive_class.border_highlight =
		swc->primitive_class.border_highlight;
    if (pwc->primitive_class.border_unhighlight == XmInheritBorderUnhighlight)
	pwc->primitive_class.border_unhighlight =
		swc->primitive_class.border_unhighlight;
    if (pwc->primitive_class.translations == XtInheritTranslations)
	pwc->primitive_class.translations =
		swc->primitive_class.translations;

    _XmBaseClassPartInitialize(widget_class);
    _XmFastSubclassInit(widget_class, XmPRIMITIVE_BIT);

    /* compile the resources */
    if (widget_class == xmPrimitiveWidgetClass) {
	_XmSortResourceList((XrmResource **)pwc->core_class.resources,
			    pwc->core_class.num_resources);
    }

    _XmInitializeSyntheticResources(pwc->primitive_class.syn_resources,
				    pwc->primitive_class.num_syn_resources);

    if (widget_class != xmPrimitiveWidgetClass) { 
 	XmPrimitiveWidgetClass psc = 
 	    (XmPrimitiveWidgetClass)pwc->core_class.superclass; 

	_XmBuildResources(&pwc->primitive_class.syn_resources,
			  &pwc->primitive_class.num_syn_resources,
			  psc->primitive_class.syn_resources,
			  psc->primitive_class.num_syn_resources);
    }
}

static void
CreateHighlightGC(XmPrimitiveWidget pw)
{    
    XGCValues values;
    unsigned long mask;

    if (Prim_HighlightPixmap(pw) != None 
	&& Prim_HighlightPixmap(pw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed highlight*/
	mask = GCTile | GCFillStyle;
	
	values.tile = Prim_HighlightPixmap(pw); 
	values.fill_style = FillTiled;
    }
    else
    {
	mask = GCForeground | GCBackground;

	values.foreground = Prim_HighlightColor(pw);
	values.background = XtBackground(pw);
    }

    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_HighlightGC(pw) = XtGetGC((Widget)pw, mask, &values);    
}

static void
CreateBottomShadowGC(XmPrimitiveWidget pw)
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
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_BottomShadowGC(pw) = XtGetGC((Widget)pw, mask, &values);    
}

static void
CreateTopShadowGC(XmPrimitiveWidget pw)
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
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_TopShadowGC(pw) = XtGetGC((Widget)pw, mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmPrimitiveWidget pw = (XmPrimitiveWidget) new_w;
    XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(new_w);

    /* merge the traversal translations into the regular widget translations */
    if (pwc->primitive_class.translations)
        XtOverrideTranslations(new_w, XtParseTranslationTable(pwc->primitive_class.translations));
    
    Prim_HaveTraversal(new_w) = 
	Prim_Highlighted(new_w) =
        Prim_HighlightDrawn(new_w) = False;

    /* validate the entries in XmNnavigationType and XmNunitType 
       Are these correct?  Or do we need to call a defaultProc? FIX ME*/

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     pw->primitive.navigation_type,
			     new_w))
	pw->primitive.navigation_type = XmNONE; 

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     pw->primitive.unit_type,
			     new_w))
	pw->primitive.unit_type = XmPIXELS;

    _XmNavigInitialize(request, new_w, args, num_args);

    if (XtWidth(request) == (Dimension)0)
	XtWidth(new_w) = (Prim_HighlightThickness(new_w) * 2
			+ Prim_ShadowThickness(new_w) * 2);
    if (XtHeight(request) == (Dimension)0)
	XtHeight(new_w) = (Prim_HighlightThickness(new_w) * 2
			 + Prim_ShadowThickness(new_w) * 2);

    _XmPrimitiveImportArgs(new_w, args, num_args);

    /* create all the initial GC's */
    CreateHighlightGC(pw);
    CreateBottomShadowGC(pw);
    CreateTopShadowGC(pw);
}

static void
destroy(Widget w)
{
    XmPrimitiveWidget pw = (XmPrimitiveWidget)w;

    XtReleaseGC(w, Prim_TopShadowGC(pw));
    XtReleaseGC(w, Prim_BottomShadowGC(pw));
    XtReleaseGC(w, Prim_HighlightGC(pw));

    _XmNavigDestroy(w);
}

static void 
realize(Widget w, 
	XtValueMask *value_mask, 
	XSetWindowAttributes *attributes)
{
   int i;
   
   XdbDebug(__FILE__, w, "Realize wid before create %d ht %d bw %d\n",
	    XtWidth(w), XtHeight(w), XtBorderWidth(w));

   XtCreateWindow(w, (unsigned int) InputOutput,
		  (Visual *)CopyFromParent, *value_mask, attributes);

   /* make sure our menushell children are realized */

   for (i=0; i < w->core.num_popups; i++)
     XtRealizeWidget(w->core.popup_list[i]);

   XdbDebug(__FILE__, w, "Realize wid %d ht %d bw %d\n",
	    XtWidth(w), XtHeight(w), XtBorderWidth(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean need_refresh = False;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     Prim_NavigationType(new_w),
			     new_w))
	Prim_NavigationType(new_w) = Prim_NavigationType(old); 

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     Prim_UnitType(new_w),
			     new_w))
	Prim_UnitType(new_w) = Prim_UnitType(old);

    need_refresh = _XmNavigSetValues(old, request, new_w, args, num_args);

    /* Debug print */
    if (Prim_Foreground(old) != Prim_Foreground(new_w)) {
	XdbDebug(__FILE__, new_w, "Primitive SetValues: Foreground changed\n");
	need_refresh = True;
    }

    if (Prim_ShadowThickness(old) != Prim_ShadowThickness(new_w) 
	|| Prim_HighlightThickness(old) != Prim_HighlightThickness(new_w)
	|| Prim_Foreground(old) != Prim_Foreground(new_w))
	need_refresh = True;

    if (Prim_HighlightColor(old) != Prim_HighlightColor(new_w)
	|| Prim_HighlightPixmap(old) != Prim_HighlightPixmap(new_w)) 
    {
	XtReleaseGC(new_w, Prim_HighlightGC(new_w));
	CreateHighlightGC((XmPrimitiveWidget)new_w);
	need_refresh = True;
    }

    if (Prim_TopShadowColor(old) != Prim_TopShadowColor(new_w)
	|| Prim_TopShadowPixmap(old) != Prim_TopShadowPixmap(new_w))
    {
	XtReleaseGC(new_w, Prim_TopShadowGC(new_w));
	CreateTopShadowGC((XmPrimitiveWidget)new_w);
	need_refresh = True;
    }

    if (Prim_BottomShadowColor(old) != Prim_BottomShadowColor(new_w)
	|| Prim_BottomShadowPixmap(old) != Prim_BottomShadowPixmap(new_w))
    {
	XtReleaseGC(new_w, Prim_BottomShadowGC(new_w));
	CreateBottomShadowGC((XmPrimitiveWidget)new_w);
	need_refresh = True;
    }

    if ((Prim_HighlightDrawn(new_w) || ! XtSensitive(new_w))
	&& _XmGetFocusPolicy(new_w) == XmPOINTER
	&& Prim_HighlightOnEnter(old) == True
	&& Prim_HighlightOnEnter(new_w) == False)
    {
	XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(new_w);
	
	(*pwc->primitive_class.border_unhighlight)(new_w);
    }

    _XmPrimitiveImportArgs(new_w, args, num_args);

    return need_refresh;
}

/*
 * ENTER/LEAVE should only happen when mwm is in pointer-follows-mouse mode.
 * FOCUS_IN/FOCUS_OUT otherwise.
 */
void
focus_change(Widget w, XmFocusChange change)
{
    XmPrimitiveWidgetClass pc = (XmPrimitiveWidgetClass)XtClass(w);

    switch(change) {
    case XmENTER:
	if (!Prim_HighlightOnEnter(w))
	    break;
	if (pc->primitive_class.border_highlight)
	    (pc->primitive_class.border_highlight)(w);
	break;

    case XmFOCUS_IN:
	Prim_HaveTraversal(w) = True;
	if (pc->primitive_class.border_highlight)
	    (pc->primitive_class.border_highlight)(w);
	break;

    case XmLEAVE:
	if (!Prim_HighlightOnEnter(w))
	    break;
	if (pc->primitive_class.border_unhighlight)
	    (pc->primitive_class.border_unhighlight)(w);
	break;

    case XmFOCUS_OUT:
	Prim_HaveTraversal(w) = True;
	if (pc->primitive_class.border_unhighlight)
	    (pc->primitive_class.border_unhighlight)(w);
	break;
    }
}

static XmNavigability
widget_navigable(Widget w)
{   
    if (XtSensitive(w) && Prim_TraversalOn(w))
    {
	if ((Prim_NavigationType(w) == XmSTICKY_TAB_GROUP ||
	     Prim_NavigationType(w) == XmEXCLUSIVE_TAB_GROUP ||
	     Prim_NavigationType(w) == XmTAB_GROUP) && !_XmShellIsExclusive(w))
	{
	    XdbDebug(__FILE__, w, "WidgetNavigable => XmTAB_NAVIGABLE\n");
	    return XmTAB_NAVIGABLE;
	}
	XdbDebug(__FILE__, w, "WidgetNavigable => XmCONTROL_NAVIGABLE\n");
	return XmCONTROL_NAVIGABLE;
    }
    XdbDebug(__FILE__, w, "WidgetNavigable => XmNOT_NAVIGABLE\n");
    return XmNOT_NAVIGABLE;
}

static void
primitive_border_unhighlight(Widget w)
{
    if (Prim_HighlightThickness(w) == 0) /* with zero width, we don't need this... */
	return;

    if (XmIsManager(XtParent(w)))
    {
	_XmDrawHighlight(XtDisplay(w), XtWindow(w), XmParentBackgroundGC(w),
			 0, 0, XtWidth(w), XtHeight(w),
			 Prim_HighlightThickness(w), LineSolid);
    }
    else
	_XmClearBorder(XtDisplay(w),
		       XtWindow(w),
		       0,0,
		       XtWidth(w), XtHeight(w),
		       Prim_HighlightThickness(w));

    Prim_Highlighted(w) = False;
    Prim_HighlightDrawn(w) = False;
}

static void
primitive_border_highlight(Widget w)
{
    if (Prim_HighlightThickness(w) == 0) /* with zero width, we don't need this... */
	return; 

    _XmDrawHighlight(XtDisplay(w), XtWindow(w), Prim_HighlightGC(w),
		     0, 0, XtWidth(w), XtHeight(w),
		     Prim_HighlightThickness(w), LineSolid);

    Prim_Highlighted(w) = True;
    Prim_HighlightDrawn(w) = True;
}

/* ACTION PROCS */

void 
_XmTraverseLeft(Widget w, 
		XEvent *event, 
		String *params, 
		Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_LEFT);
}

void 
_XmTraverseRight(Widget w, 
		 XEvent *event, 
		 String *params, 
		 Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "XmProcessTraversal(TRAVERSE_RIGHT)\n");
    XmProcessTraversal(w, XmTRAVERSE_RIGHT);
}

void 
_XmTraverseDown(Widget w, 
		XEvent *event, 
		String *params, 
		Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_DOWN);
}

void 
_XmTraverseUp(Widget w, 
	      XEvent *event, 
	      String *params, 
	      Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_UP);
}

void 
_XmTraverseHome(Widget w, 
		XEvent *event, 
		String *params, 
		Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_HOME);
}

void 
_XmTraverseNextTabGroup(Widget w, 
			XEvent *event, 
			String *params, 
			Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);
}

void 
_XmTraversePrevTabGroup(Widget w, 
			XEvent *event, 
			String *params, 
			Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_PREV_TAB_GROUP);
}

void 
_XmPrimitiveParentActivate(Widget w, 
			   XEvent *event, 
			   String *params, 
			   Cardinal *num_params)
{
    XmParentProcessDataRec data;
    Widget m = XtParent(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)m->core.widget_class;

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_ACTIVATE;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(m) && mwc->manager_class.parent_process)
	(*mwc->manager_class.parent_process)(m, &data);
}

void 
_XmPrimitiveParentCancel(Widget w, 
			 XEvent *event, 
			 String *params, 
			 Cardinal *num_params)
{
    XmParentProcessDataRec data;
    Widget m = XtParent(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)m->core.widget_class;

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_CANCEL;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(m) && mwc->manager_class.parent_process)
	(*mwc->manager_class.parent_process)(m, &data);
}

void 
_XmPrimitiveHelp(Widget w, 
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

Boolean
_XmDifferentBackground(Widget w, Widget parent)
{
    if (!w || !parent)
	return True;
    if (!XmIsPrimitive(w))
	return True;
    return (XtBackground(w) != XtBackground(parent));
}

/* Public Functions */

/*
 * The functions XmResolvePartOffsets and XmResolveAllPartOffsets were designed
 * to be used by widgets subclassing OSF/Motif widgets. Therefore, LessTif must
 * provide, but not (necessarily) use them.
 *
 * The big idea is that *binary* compatibility can be obtained for these subclasses,
 * even if the Motif (LessTif) revisions add fields to the instance parts of the
 * widgets.
 *
 * The functions below are used together with a macro called XmField which is in
 * LESSTIF/include/Xm/XmP.h .
 *
 * All this is described very thoroughly in an article called "Achieving Binary
 * Compatibility using the XmResolvePartOffset API" by Daniel DarDailler, who has
 * been in the Motif team at OSF for as long as there was a Motif team at OSF :-)
 *
 * Look for the article at http://www.x.org/people/daniel/xmresolv
 */
void
XmResolvePartOffsets(WidgetClass w_class, XmOffsetPtr *offset)
{
}

void
XmResolveAllPartOffsets(WidgetClass w_class, XmOffsetPtr *offset,
			XmOffsetPtr *constraint_offset)
{
}
