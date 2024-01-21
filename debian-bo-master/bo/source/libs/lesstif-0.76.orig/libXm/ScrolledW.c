/*
 *
 * $Id: ScrolledW.c,v 1.25 1997/01/13 07:08:57 u27113 Exp $
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

static char rcsid[] = "$Id: ScrolledW.c,v 1.25 1997/01/13 07:08:57 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <X11/Xfuncs.h>
#include <Xm/XmP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumnP.h>

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
static void realize(Widget w, Mask *value_mask, XSetWindowAttributes *attributes);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult QueryGeometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void ChangeManaged(Widget w);
static void insert_child(Widget w);
static XtGeometryResult GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);

static void RepositionScrolledWindow(Widget w, XtPointer client, XtPointer call);

/*
 * Resources for the ScrolledWindow class
 */
#define Offset(field) XtOffsetOf(XmScrolledWindowRec, swindow.field)
#define MGR_Offset(field) XtOffsetOf(XmScrolledWindowRec, manager.field)
static XtResource resources[] = {
    {
	XmNhorizontalScrollBar, XmCHorizontalScrollBar, XmRWidget,
	sizeof(Widget), Offset(hScrollBar),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNverticalScrollBar, XmCVerticalScrollBar, XmRWidget,
	sizeof(Widget), Offset(vScrollBar),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNworkWindow, XmCWorkWindow, XmRWidget,
	sizeof(Widget), Offset(WorkWindow),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNclipWindow, XmCClipWindow, XmRWidget,
	sizeof(Widget), Offset(ClipWindow),
	XtRImmediate, (XtPointer)NULL
    },
    {
	XmNscrollingPolicy, XmCScrollingPolicy, XmRScrollingPolicy,
	sizeof(unsigned char), Offset(ScrollPolicy),
	XmRImmediate, (XtPointer)XmAPPLICATION_DEFINED
    },
    {
	XmNvisualPolicy, XmCVisualPolicy, XmRVisualPolicy,
	sizeof(unsigned char), Offset(VisualPolicy),
	XmRImmediate, (XtPointer)XmVARIABLE
    },
    {
	XmNscrollBarDisplayPolicy, XmCScrollBarDisplayPolicy, XmRScrollBarDisplayPolicy,
	sizeof(unsigned char), Offset(ScrollBarPolicy),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNscrollBarPlacement, XmCScrollBarPlacement, XmRScrollBarPlacement,
	sizeof(unsigned char), Offset(Placement),
	XtRImmediate, (XtPointer)XmBOTTOM_RIGHT
    },
    {
	XmNscrolledWindowMarginWidth, XmCScrolledWindowMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(WidthPad),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNscrolledWindowMarginHeight, XmCScrolledWindowMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(HeightPad),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(pad),
	XtRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), MGR_Offset(shadow_thickness),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNtraverseObscuredCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(traverseObscuredCallback),
	XmRImmediate, NULL
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNscrolledWindowMarginWidth,
	sizeof(Dimension), Offset(WidthPad),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNscrolledWindowMarginHeight,
	sizeof(Dimension), Offset(HeightPad),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNspacing,
	sizeof(Dimension), Offset(pad),
	_XmFromVerticalPixels, NULL
    }
};

static void PageUp(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void PageDown(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void PageLeft(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void PageRight(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void BeginLine(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EndLine(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void BeginData(Widget w, XEvent *event, String *params, Cardinal *num_params);
static void EndData(Widget w, XEvent *event, String *params, Cardinal *num_params);

char _XmScrolledW_ScrolledWindowXlations[] =
   "<EnterWindow>:            ManagerEnter()\n\
    <FocusOut>:               ManagerFocusOut()\n\
    <FocusIn>:                ManagerFocusIn()\n\
    <Btn2Down>:               ManagerGadgetDrag()\n\
    <Key>osfActivate:         ManagerParentActivate()\n\
    <Key>osfCancel:           ManagerParentCancel()\n\
    c <Key>osfBeginLine:      SWTopLine()\n\
    <Key>osfBeginLine:        SWBeginLine()\n\
    c <Key>osfEndLine:        SWBottomLine()\n\
    <Key>osfEndLine:          SWEndLine()\n\
    <Key>osfPageLeft:         SWLeftPage()\n\
    c <Key>osfPageUp:         SWLeftPage()\n\
    <Key>osfPageUp:           SWUpPage()\n\
    <Key>osfPageRight:        SWRightPage()\n\
    c <Key>osfPageDown:       SWRightPage()\n\
    <Key>osfPageDown:         SWDownPage()\n\
    <Key>osfHelp:             ManagerGadgetHelp()\n\
    <Key>osfUp:               ManagerGadgetTraverseUp()\n\
    <Key>osfDown:             ManagerGadgetTraverseDown()\n\
    <Key>osfLeft:             ManagerGadgetTraverseLeft()\n\
    <Key>osfRight:            ManagerGadgetTraverseRight()\n\
    ~s ~m ~a <Key>Return:     ManagerParentActivate()\n\
    s ~m ~a <Key>Tab:         ManagerGadgetPrevTabGroup()\n\
    ~m ~a <Key>Tab:           ManagerGadgetNextTabGroup()";

/*
 * MLM: FIXME -- the following two translation tables need to be installed.
 * Without them, misc/test1.c doesn't compile.
 */
char _XmScrolledW_ClipWindowTranslationTable[] =
    "<MapNotify>:           SWNoop() \n\
     :c <Key>osfBeginLine:  SWTopLineGrab() \n\
     :<Key>osfBeginLine:    SWBeginLineGrab() \n\
     :c <Key>osfEndLine:    SWBottomLineGrab() \n\
     :<Key>osfEndLine:      SWEndLineGrab() \n\
     :<Key>osfPageLeft:     SWLeftPageGrab() \n\
     :c <Key>osfPageUp:     SWLeftPageGrab() \n\
     :<Key>osfPageUp:       SWUpPageGrab() \n\
     :<Key>osfPageRight:    SWRightPageGrab() \n\
     :c <Key>osfPageDown:   SWRightPageGrab() \n\
     :<Key>osfPageDown:     SWDownPageGrab()";

char _XmScrolledW_WorkWindowTranslationTable[] =
    ":c <Key>osfBeginLine:  SWTopLineWork() \n\
     :<Key>osfBeginLine:    SWBeginLineWork() \n\
     :c <Key>osfEndLine:    SWBottomLineWork() \n\
     :<Key>osfEndLine:      SWEndLineWork()"; 

static XtActionsRec actions[] = {
    {"SWUpPage", PageUp},
    {"SWDownPage", PageDown},
    {"SWLeftPage", PageLeft},
    {"SWRightPage", PageRight},
    {"SWBeginLine", BeginLine},
    {"SWEndLine", EndLine},
    {"SWTopLine", BeginData},
    {"SWBottomLine", EndData}
};

static XmBaseClassExtRec _XmScrolledWCoreClassExtRec = {
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

static XmManagerClassExtRec _XmScrolledWMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmScrolledWindowClassRec xmScrolledWindowClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmScrolledWindow",
	/* widget_size           */ sizeof(XmScrolledWindowRec),
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
	/* tm_table              */ _XmScrolledW_ScrolledWindowXlations,
	/* query_geometry        */ QueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmScrolledWCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ GeometryManager, 
        /* change_managed   */ ChangeManaged, 
        /* insert_child     */ insert_child,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,  /* FIX ME */
        /* subresource_count */ 0,     /* FIX ME */
        /* constraint_size   */ 0,     /* FIX ME */
        /* initialize        */ NULL,  /* FIX ME */
        /* destroy           */ NULL,  /* FIX ME */
        /* set_values        */ NULL,  /* FIX ME */
        /* extension         */ NULL,   /* FIX ME */
    },
    /* XmManager class part */
    {
        /* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
        /* extension                    */ (XtPointer)&_XmScrolledWMClassExtRec
    },
    /* XmScrolledWindow part */
    {
	/* extension */ NULL,
    },
};

WidgetClass xmScrolledWindowWidgetClass = (WidgetClass)&xmScrolledWindowClassRec;

static void 
class_initialize()
{
    _XmScrolledWCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmScrolledWindowWidgetClass swclass = (XmScrolledWindowWidgetClass) widget_class;
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension*)_XmGetClassExtensionPtr((XmGenericClassExt*)&(swclass->composite_class.extension),
							       NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = swclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    swclass->composite_class.extension = (XtPointer) ext;
	}
    }    
    _XmFastSubclassInit(widget_class, XmSCROLLED_WINDOW_BIT);
}

/*
 * Note
 * The OSF/Motif manual page for XmScrolledWindow describes a "clip" widget
 * and a "work" widget. The Clip widget is really the area that the user sees,
 * whereas the Work widget is the one that is put in the ScrolledWindow.
 * I.e. if you display a large label widget, then that label is the work widget,
 * and the clip widget is something magic that is private to scrolledwindow.
 */
static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    if (SW_ScrollBarPolicy(new_w) == (unsigned char)XmUNSPECIFIED) {
	if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
	    SW_ScrollBarPolicy(new_w) = XmAS_NEEDED;
	else
	    SW_ScrollBarPolicy(new_w) = XmSTATIC;
    }

    if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
	SW_VisualPolicy(new_w) = XmCONSTANT;
    else
	SW_VisualPolicy(new_w) = XmVARIABLE;

    if (SW_Spacing(new_w) == XmINVALID_DIMENSION)
	SW_Spacing(new_w) = 4;

    if (MGR_ShadowThickness(new_w) == XmINVALID_DIMENSION)
	MGR_ShadowThickness(new_w) = 0;

    if (SW_ScrollPolicy(new_w) == XmAUTOMATIC) {
	SW_VSB(new_w) = (XmScrollBarWidget)XtVaCreateManagedWidget("VertScrollBar",
		xmScrollBarWidgetClass, new_w,
		XmNorientation, XmVERTICAL,
		NULL);
	SW_HasVSB(new_w) = True;

	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNincrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNdecrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNpageIncrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNpageDecrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNdragCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNvalueChangedCallback,
		      RepositionScrolledWindow,
		      NULL);

	SW_HSB(new_w) = (XmScrollBarWidget)XtVaCreateManagedWidget("HorScrollBar",
		xmScrollBarWidgetClass, new_w,
		XmNorientation, XmHORIZONTAL,
		NULL);
	SW_HasHSB(new_w) = True;

	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNincrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNdecrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNpageIncrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNpageDecrementCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNdragCallback,
		      RepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNvalueChangedCallback,
		      RepositionScrolledWindow,
		      NULL);
    }
    else {
	SW_VSBMinimum(new_w) = 0;
	SW_VSBMaximum(new_w) = 0;
	SW_VSBValue(new_w) = 0;
	SW_VSBSliderSize(new_w) = 0;

	SW_HSBMinimum(new_w) = 0;
	SW_HSBMaximum(new_w) = 0;
	SW_HSBValue(new_w) = 0;
	SW_HSBSliderSize(new_w) = 0;

	SW_HSBX(new_w) = 0;
	SW_HSBY(new_w) = 0;
	SW_HSBWidth(new_w) = 0;
	SW_HSBHeight(new_w) = 0;

	SW_VSBX(new_w) = 0;
	SW_VSBY(new_w) = 0;
	SW_VSBWidth(new_w) = 0;
	SW_VSBHeight(new_w) = 0;

	SW_WorkWidth(new_w) = 0;
	SW_WorkHeight(new_w) = 0;

	SW_ClipWidth(new_w) = 0;
	SW_ClipHeight(new_w) = 0;
	SW_ClipX(new_w) = 0;
	SW_ClipY(new_w) = 0;

	SW_HasHSB(new_w) = False;
	SW_HasVSB(new_w) = False;
	SW_WorkWindow(new_w) = NULL;
	SW_ClipWindow(new_w) = NULL;
	SW_HSB(new_w) = NULL;
	SW_VSB(new_w) = NULL;

    }

    if (SW_VisualPolicy(new_w) == XmCONSTANT) {
	SW_ClipWindow(new_w) =
		(XmDrawingAreaWidget)XtCreateManagedWidget("clipWindow",
		xmDrawingAreaWidgetClass, new_w, NULL, 0);
    }
    else if (SW_VisualPolicy(new_w) != XmVARIABLE)
	SW_VisualPolicy(new_w) = XmVARIABLE;

    if (SW_VisualPolicy(new_w) == XmVARIABLE && SW_ScrollBarPolicy(new_w) != XmSTATIC)
	SW_ScrollBarPolicy(new_w) = XmSTATIC;

    SW_FromResize(new_w) = False;
    SW_InInit(new_w) = True;
    /* init chained from super to sub order */
    if (XtClass(new_w) == xmScrolledWindowWidgetClass)
	_XmScrolledWindowLayout(new_w, True, NULL, False, NULL,
				0, 0, XtWidth(new_w), XtHeight(new_w));
    SW_InInit(new_w) = False;

#if	1
    if (XtWidth(request) == 0) {
	XtWidth(new_w) = 100;
    }
    if (XtHeight(request) == 0) {
	XtHeight(new_w) = 100;
    }
#endif
}

static void
destroy(Widget w)
{
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean			r = False;	/* Must I be redisplayed ? */

    XdbDebug(__FILE__, new_w, "SetValues\n");

#define	NE(x)	(x(new_w) != x(old))

    if (NE(SW_ClipWindow)) {
	SW_ClipWindow(new_w) = SW_ClipWindow(old);
	_XmWarning(new_w,
		   "Attempted to change the clipWindow in scrolled window %s",
		   XtName(new_w));
    }
    if (NE(SW_VisualPolicy)) {
	SW_VisualPolicy(new_w) = SW_VisualPolicy(old);
	_XmWarning(new_w,
		   "Attempted to change the visualPolicy in scrolled window %s",
		   XtName(new_w));
    }
    if (NE(SW_ScrollPolicy)) {
	SW_ScrollPolicy(new_w) = SW_ScrollPolicy(old);
	_XmWarning(new_w,
		   "Attempted to change the scrollingPolicy in scrolled window %s",
		   XtName(new_w));
    }

    if (NE(SW_HSB)) {
	if (SW_HSB(new_w) && XtIsManaged(SW_HSB(new_w)))
	    SW_HasHSB(new_w) = True;
	else
	    SW_HasHSB(new_w) = False;
	r = True;
    }
    if (NE(SW_VSB)) {
	if (SW_VSB(new_w) && XtIsManaged(SW_VSB(new_w)))
	    SW_HasVSB(new_w) = True;
	else
	    SW_HasVSB(new_w) = False;
	r = True;
    }

    if (NE(SW_ScrollBarPolicy)
     || NE(SW_Placement)
     || NE(SW_MarginHeight)
     || NE(SW_MarginWidth)
     || NE(SW_Spacing)
     || NE(SW_TraverseObscuredCallback)
     || NE(SW_WorkWindow) ) {
	r = True;
    }

    SW_FromResize(new_w) = True;
    /* set values chained in super to sub order */
    if (XtClass(new_w) == xmScrolledWindowWidgetClass)
	_XmScrolledWindowLayout(new_w, True, NULL, False, NULL,
				0, 0, XtWidth(new_w), XtHeight(new_w));
    SW_FromResize(new_w) = False;

    return r;
}

static void
resize(Widget w)
{
    XdbDebug(__FILE__, w, "Resize: x %d y %d w %d h %d\n",
	     XtX(w), XtY(w), XtWidth(w), XtHeight(w));

    SW_FromResize(w) = True;
    /* resize not chained */
    _XmScrolledWindowLayout(w, False, NULL, False, NULL,
			    0, 0, XtWidth(w), XtHeight(w));
    SW_FromResize(w) = False;
}

static void
expose(Widget w, XEvent *event, Region region) 
{
    XdbDebug(__FILE__, w,
	"Expose: _XmDrawShadows(_, _, _, _, %d, %d, %d, %d, %d, XmSHADOW_IN)\n",
		   SW_ClipX(w) - MGR_ShadowThickness(w),
		   SW_ClipY(w) - MGR_ShadowThickness(w),
		   SW_ClipWidth(w) + 2 * MGR_ShadowThickness(w),
		   SW_ClipHeight(w) + 2 * MGR_ShadowThickness(w),
		   MGR_ShadowThickness(w));

    _XmDrawShadows(XtDisplay(w), XtWindow(w),
		   MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		   SW_ClipX(w) - MGR_ShadowThickness(w),
		   SW_ClipY(w) - MGR_ShadowThickness(w),
		   SW_ClipWidth(w) + 2 * MGR_ShadowThickness(w),
		   SW_ClipHeight(w) + 2 * MGR_ShadowThickness(w),
		   MGR_ShadowThickness(w), XmSHADOW_IN);

    _XmRedisplayGadgets(w, event, region);
}
      
static void 
realize(Widget w, Mask *value_mask, XSetWindowAttributes *attributes)
{

    XdbDebug(__FILE__, w, "Realize\n");

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

    SW_FromResize(w) = True;
    /* not chained */
    _XmScrolledWindowLayout(w, True, NULL, False, NULL,
			    0, 0, XtWidth(w), XtHeight(w));
    SW_FromResize(w) = False;
} 

/*
 * Allow other widgets to ask what the preferred geometry of this widget is.
 */
static XtGeometryResult
QueryGeometry(Widget w, XtWidgetGeometry *intended, XtWidgetGeometry *preferred)
{
    XdbDebug(__FILE__, w, "QueryGeometry(%s)\n",
	XdbWidgetGeometry2String(intended));

    /* not chained */
    _XmScrolledWindowLayout(w, False, w, True, preferred,
			    0, 0, XtWidth(w), XtHeight(w));

    XdbDebug(__FILE__, w, "QueryGeometry: I want %s\n",
	     XdbWidgetGeometry2String(preferred));

    if (intended == NULL) { /* This never happens */
	if (XtWidth(w) != preferred->width || XtHeight(w) != preferred->height)
	    return XtGeometryAlmost;		/* Something's different */
	return XtGeometryNo;			/* Everything is ok */
    }
    if ((intended->request_mode & CWWidth) &&
	preferred->width != intended->width) {
	XdbDebug(__FILE__, w, "QueryGeometry => ALMOST (w %d)\n",
		preferred->width);
	return XtGeometryAlmost;		/* Something's different */
    }
    if ((intended->request_mode & CWHeight)
		&& preferred->height != intended->height) {
	XdbDebug(__FILE__, w, "QueryGeometry => ALMOST (h %d)\n",
		preferred->height);
	return XtGeometryAlmost;		/* Something's different */
    }
    XdbDebug(__FILE__, w, "QueryGeometry => YES\n");
    return XtGeometryYes;			/* I'm happy */
}

static XtGeometryResult
GeometryManager(Widget w, XtWidgetGeometry *desired, XtWidgetGeometry *allowed)
{
    XtWidgetGeometry wants;
    Widget sw = XtParent(w);

    XdbDebug2(__FILE__, sw, w, "GeometryManager (request %s)\n",
	XdbWidgetGeometry2String(desired));

#define	Wants(flag)	(desired->request_mode & flag)

    if (desired != NULL && allowed != NULL && desired != allowed)
	wants = *desired;
    else
	bzero((void *)&wants, sizeof(XtWidgetGeometry));

    /* We control the XY of all children. Width/Height is all we care about */
    wants.request_mode &= CWWidth | CWHeight;

    /* Special case : Work Window trying to resize itself in XmAUTOMATIC */
    if (SW_ScrollPolicy(sw) == XmAUTOMATIC) {
	XdbDebug2(__FILE__, sw, w, "GeometryManager: resize WorkWindow\n");

	/*
	 * This works as follows.
	 * Remember the SW in automatic mode has a ClipWindow (SW's child)
	 * and a WorkWindow. When created, the workwindow is reparented to
	 * have ClipWindow as parent, meaning it becomes SW's grandchild.
	 *
	 * When WorkWindow tries to resize, it'll ask its parent. ClipWindow
	 * is a XmDrawingArea which has code to look up whether its parent
	 * is a SW in XmAUTOMATIC mode. If so, it'll trigger SW's Geometry-
	 * Manager by XtMakeResizeRequest (which is how we get here), whose
	 * result is not taken into account. Therefore we can get away with
	 * XtGeometryNo as reply.
	 *
	 * We now call _XmScrolledWindowLayout here to make it fulfill the
	 * request by resizing WorkWindow (even though it's not a child),
	 * and letting ClipWindow remain the same size.
	 */
	_XmScrolledWindowLayout(sw, True, w, False, &wants,
		0, 0, XtWidth(sw), XtHeight(sw));

	return XtGeometryNo;
    }

    /* not chained */
    _XmScrolledWindowLayout(sw, True, w, True, &wants,
			    0, 0, XtWidth(sw), XtHeight(sw));

    if (allowed)
	*allowed = wants;

    if (Wants(XtCWQueryOnly)) {
	_XmWarning(sw, "XmScrolledWindow: GeometryManager QueryOnly"
		"not implemented (child %s, class %s)",
		XtName(w), XtClass(w)->core_class.class_name);

	XdbDebug2(__FILE__, sw, w, "GeometryManager QueryOnly not implemented ! (=> YES)\n");
	return XtGeometryYes;
    }

    if (Wants(CWX) && Wants(CWY) && !(Wants(CWWidth) && Wants(CWHeight)))
	return XtGeometryNo;

    if (Wants(CWWidth) && wants.width == desired->width &&
	Wants(CWHeight) && wants.height == desired->height)
	return XtGeometryYes;

    else if ((Wants(CWWidth) && wants.width < desired->width) ||
	     (Wants(CWHeight) && wants.height < desired->height))
	return XtGeometryAlmost;

    return XtGeometryNo;
}

static void
ChangeManaged(Widget w)
{
    if (XtIsManaged(w)) {
	_XmScrolledWindowLayout(w, True, NULL, False, NULL,
			    0, 0, XtWidth(w), XtHeight(w));
	XdbDebug(__FILE__, w, "ChangeManaged: size %d %d\n",
		XtWidth(w), XtHeight(w));
    } else {
	XdbDebug(__FILE__, w, "ChangeManaged: unmanaging sb's\n");
	if (SW_HSB(w) && XtIsManaged((Widget)SW_HSB(w)))
		XtUnmanageChild((Widget)SW_HSB(w));
	if (SW_VSB(w) && XtIsManaged((Widget)SW_VSB(w)))
		XtUnmanageChild((Widget)SW_VSB(w));
    }
}

/*
 * In here we probably need some trickery to make things work.
 * One sensible thing to do is to make the work widget the
 * child of the clip widget. This is ugly, though. That's probably the
 * reason why the X Consortium never wanted to add XtReparentWidget().
 *
 * The advantage of reparenting the work widget is, all you have to do
 * to scroll is tell the work widget where it is. Through the magic of
 * the X window system having window hierarchies, things should work.
 */
static void
insert_child(Widget w)
{
    XmScrolledWindowWidget sw = (XmScrolledWindowWidget)XtParent(w);

#define superclass (&xmManagerClassRec)

    /* Special first case : these are not the work window */
    /* Note this is especially here for our subclass XmMainWindow */
    if (XmIsSeparator(w) || XmIsSeparatorGadget(w)
	|| (XmIsRowColumn(w) && RC_Type(w) == XmMENU_BAR))
	(*superclass->composite_class.insert_child)(w);
    else if (XmIsScrollBar(w)) {
	(*superclass->composite_class.insert_child)(w);

	switch (SCB_Orientation(w)) {
	case XmHORIZONTAL:
	    SW_HSB(sw) = (XmScrollBarWidget)w;
	    if (XtIsManaged(w))
		SW_HasHSB(w) = True;
	    else
		SW_HasHSB(w) = False;
	    break;
	case XmVERTICAL:
	    SW_VSB(sw) = (XmScrollBarWidget)w;
	    if (XtIsManaged(w))
		SW_HasVSB(w) = True;
	    else
		SW_HasVSB(w) = False;
	    break;
	default:
	    _XmWarning((Widget)sw,
		      "Can't determine ScrollBar orientation in ScrolledWindow %s: Not adding",
		      XtName(sw));
	    break;
	}
    }
    else if (XtIsShell(w)) {
	/* Shells cannot be the work area - so make sure we catch them here */
	(*superclass->composite_class.insert_child)(w);
    }
    /*
     * only we can add a clip window, and we know it's id
     */
    else if (SW_VisualPolicy(sw) == XmCONSTANT && SW_ClipWindow(sw) == NULL) {
	(*superclass->composite_class.insert_child)(w);
    }
    /*
     * THIS IS SOO UGLY
     *
     * if we're not XmCONSTANT, don't reparent
     *
     */
    else if (SW_VisualPolicy(sw) == XmCONSTANT) {
	XdbDebug2(__FILE__, w, (Widget)sw, "Reparented to %s\n",
		XtName(SW_ClipWindow(sw)));
	
	XtParent(w) = (Widget)SW_ClipWindow(sw);

	/*
	 * Now we're reparented. Go on inserting the child as if
	 * nothing happened
	 */
	(*superclass->composite_class.insert_child)(w);

        SW_WorkWindow(sw) = w;
    }
    else {
	XdbDebug2(__FILE__, (Widget)sw, w, "Child is Work Window\n");

	(*superclass->composite_class.insert_child)(w);

        SW_WorkWindow(sw) = w;
    }
#undef superclass
}

static void
RepositionScrolledWindow(Widget w, XtPointer client, XtPointer call)
{
    int h, v;
    Widget sw = XtParent(w);

    if (SW_VSB(sw)) {
	XtVaGetValues((Widget)SW_VSB(sw), XmNvalue, &v, NULL);
	SW_VSBValue(sw) = v;
    } else
	SW_VSBValue(sw) = 0;

    if (SW_HSB(sw)) {
	XtVaGetValues((Widget)SW_HSB(sw), XmNvalue, &h, NULL);
	SW_HSBValue(sw) = h;
    } else
	SW_HSBValue(sw) = 0;

    XdbDebug2(__FILE__, sw, w, "Hor %d Vert %d\n", h, v);

    /*
     * Position the work window
     */
    if (SW_WorkWindow(sw))
	XtMoveWidget(SW_WorkWindow(sw), -h, -v);
}

/*
 * to get here, we have to have gone through the layout function.  All our
 * instance variable should have reasonable values.
 */
static void
FixupScrollBars(Widget w)
{
    int	max, min, percent, n;
    Arg	args[4];

    if (!SW_ClipWindow(w)) {
	_XmWarning(w,
		   "Requested to do scrolling without a clip window: %s",
		   XtName(w));
	return;
    }

    /*
     * see if we have a work window.  If not, pick some reasonable defaults
     */
    if (!SW_WorkWindow(w)) {
	if (SW_HasHSB(w)) {
	    XtVaGetValues((Widget)SW_HSB(w),
			  XmNmaximum, &max,
			  XmNminimum, &min,
			  NULL);
	    XtVaSetValues((Widget)SW_HSB(w),
			  XmNsliderSize, max - min,
			  XmNvalue, 0,
			  NULL);
	}
	if (SW_HasVSB(w)) {
	    XtVaGetValues((Widget)SW_VSB(w),
			  XmNmaximum, &max,
			  XmNminimum, &min,
			  NULL);
	    XtVaSetValues((Widget)SW_VSB(w),
			  XmNsliderSize, max - min,
			  XmNvalue, 0,
			  NULL);
	}
	return;
    }

    /*
     * otherwise, fixup the scrollbars.
     */
    XdbDebug(__FILE__, w, "FixupScrollBars Widths : Work %d Clip %d\n",
	SW_WorkWidth(w), SW_ClipWidth(w));
    XdbDebug(__FILE__, w, "FixupScrollBars Heights : Work %d Clip %d\n",
	SW_WorkHeight(w), SW_ClipHeight(w));

    if (SW_HasHSB(w)) {
	if (SW_WorkWidth(w) <= SW_ClipWidth(w) || SW_WorkWidth(w) == 0) {
	    percent = 100;
	    max = 100;
	    min = 0;
	}
	else {
	    max = SW_WorkWidth(w);
	    min = 0;
	    percent = (int)(((float)SW_ClipWidth(w) /
			     (float)SW_WorkWidth(w)) *
			    (float)SW_WorkWidth(w));
	}

	n = 0;
	XtSetArg(args[n], XmNminimum, min); n++;
	XtSetArg(args[n], XmNmaximum, max); n++;
	XtSetArg(args[n], XmNsliderSize, percent); n++;
	if (SW_ClipWidth(w) > 0) {
		XtSetArg(args[n], XmNpageIncrement, SW_ClipWidth(w)); n++;
	}
	
	XtSetValues((Widget)SW_HSB(w), args, n);

	SW_HSBMinimum(w) = min;
	SW_HSBMaximum(w) = max;
	SW_HSBSliderSize(w) = percent;

	XdbDebug(__FILE__, w, "FixupScrollBars HSB min %d max %d percent %d\n",
		min, max, percent);
    }
    if (SW_HasVSB(w)) {
	if (SW_WorkHeight(w) <= SW_ClipHeight(w) || SW_WorkHeight(w) == 0) {
	    percent = 100;
	    max = 100;
	    min = 0;
	}
	else {
	    max = SW_WorkHeight(w);
	    min = 0;
	    percent = (int)(((float)SW_ClipHeight(w) /
			     (float)SW_WorkHeight(w)) *
			    (float)SW_WorkHeight(w));
	}

	if (percent < 1)
		percent = 1;

	n = 0;
	XtSetArg(args[n], XmNminimum, min); n++;
	XtSetArg(args[n], XmNmaximum, max); n++;
	XtSetArg(args[n], XmNsliderSize, percent); n++;
	if (SW_ClipHeight(w) > 0) {
	    if (SW_ClipHeight(w) > max) {
		XtSetArg(args[n], XmNpageIncrement, max); n++;
	    } else {
		XtSetArg(args[n], XmNpageIncrement, SW_ClipHeight(w)); n++;
	    }
	}
	
	XtSetValues((Widget)SW_VSB(w), args, n);

	SW_VSBMinimum(w) = min;
	SW_VSBMaximum(w) = max;
	SW_VSBSliderSize(w) = percent;

	XdbDebug(__FILE__, w, "FixupScrollBars VSB min %d max %d percent %d\n",
		min, max, percent);
    }
}

static void
SetMinimum(Widget w)
{
	int	m;

	XtVaGetValues(w, XmNminimum, &m, NULL);
	XtVaSetValues(w, XmNvalue, m, NULL);
}

/*
 * _XmScrolledWindowLayout
 *
 * The widget w is a ScrolledWindow widget.
 * When in XmAUTOMATIC mode, a XmDrawingArea widget has been created by
 *	XmScrolledWindow; this is the clip widget. A widget which is created
 *	as child of the XmScrolledWindow is reparented (see insert_child)
 *	so it really becomes child of the XmDrawingArea.
 * In XmAPPLICATION_DEFINED, the child widget has to take care of most
 *	scrolling all by itself. It can be expected that only two widgets
 *	(XmList and XmText) will ever really succeed in implementing this.
 *
 * To avoid confusion : the ParentResize parameter if False only when
 *	called from the resize() method, meaning that we cannot resize
 *	the XmScrolledWindow itself.
 *	Actually one exception : in QueryGeometry, we run this in test
 *	mode, so ParentResize doesn't really matter.
 */
void
_XmScrolledWindowLayout(Widget w, Boolean ParentResize,
			Widget child, Boolean TestMode,
			XtWidgetGeometry *childgeom,
			Position x, Position y,
			Dimension fw, Dimension fh)
{
/* Need variables for just about everything here to implement TestMode */
    Boolean		ShowVSB, ShowHSB, HasHSB, HasVSB;
    XtWidgetGeometry	clipgeo, workgeo, request, orig;
    XtGeometryResult	res;
    Position		HsbX, HsbY, VsbX, VsbY, ClipX, ClipY, WorkX, WorkY;
/*
 * Dimension variables (below) are typed as int because Dimension is
 * unsigned. However, we need to check whether they become negative, hence
 * the theoretically incorrect type.
 * We don't do GetValues on them so we should be ok.
 */
    int			HsbW, HsbH, VsbW, VsbH, ClipW, ClipH, WorkW, WorkH;
    int			needw, needh, SwW, SwH;

    if (TestMode)
	ParentResize = True;

    if (! XtIsManaged(w)) {
	XdbDebug(__FILE__, w, "XmScrolledWindowLayout (not managed) return\n");
	return;
    }
    if (! SW_WorkWindow(w)) {
	XdbDebug(__FILE__, w, "XmScrolledWindowLayout (no workarea) return\n");
	return;		/* Probably doesn't happen much */
    }
    if (childgeom && childgeom->request_mode)
	orig = *childgeom;

/* Print out how we're called */
    if (XdbInDebug(__FILE__, w)) {
      XdbDebug(__FILE__, w, "XmScrolledWindowLayout %s %s [size %d %d]\n",
	TestMode ? "TestMode" : "",
	ParentResize ? "ParentResize" : "",
	XtWidth(w), XtHeight(w));
      XdbDebug0(__FILE__, w, "... SW_VisualPolicy %s",
	(SW_VisualPolicy(w) == XmCONSTANT) ? "XmCONSTANT" :
	(SW_VisualPolicy(w) == XmVARIABLE) ? "XmVARIABLE" :
	(SW_VisualPolicy(w) == XmRESIZE_IF_POSSIBLE)
		? "XmRESIZE_IF_POSSIBLE" : "???");
      XdbDebug0(__FILE__, w, " SW_ScrollBarPolicy %s",
	(SW_ScrollBarPolicy(w) == XmSTATIC) ? "XmSTATIC" :
	(SW_ScrollBarPolicy(w) == XmAS_NEEDED) ? "XmAS_NEEDED" : "???");
      XdbDebug0(__FILE__, w, " SW_ScrollPolicy %s\n",
	(SW_ScrollPolicy(w) == XmAPPLICATION_DEFINED)
		? "XmAPPLICATION_DEFINED" :
	(SW_ScrollPolicy(w) == XmAUTOMATIC) ? "XmAUTOMATIC" : "???" );
    }

    if (childgeom) {
	childgeom->request_mode &= (CWWidth|CWHeight|XtCWQueryOnly);
    }

/* Initialize all local variables to reflect their widgets */
    VsbX = VsbY = HsbX = HsbY = 0;
    VsbW = VsbH = HsbW = HsbH = 0;
    ShowHSB = ShowVSB = HasHSB = HasVSB = False;

    if (SW_VSB(w)) {
	/*
	 * Don't be confused by HasHSB having similar name to SW_HasHSB().
	 * HasHSB is true if the widget exists.
	 * ShowHSB is used to manage/unmanage the scrollbar.
	 */
	HasVSB = True;
	ShowVSB = XtIsManaged(SW_VSB(w));
	VsbX = XtX(SW_VSB(w));
	VsbY = XtY(SW_VSB(w));
	VsbW = XtWidth(SW_VSB(w));
	VsbH = XtHeight(SW_VSB(w));
    }

    if (SW_HSB(w)) {
	HasHSB = True;
	ShowHSB = XtIsManaged(SW_HSB(w));
	HsbX = XtX(SW_HSB(w));
	HsbY = XtY(SW_HSB(w));
	HsbW = XtWidth(SW_HSB(w));
	HsbH = XtHeight(SW_HSB(w));
    }

    ClipX = ClipY = WorkX = WorkY = 0;
    ClipW = ClipH = WorkW = WorkH = 0;
    if (SW_WorkWindow(w)) {
#if 0
	WorkX = XtX(SW_WorkWindow(w));
	WorkY = XtY(SW_WorkWindow(w));
#else
	WorkX = x;
	WorkY = y;
#endif
	WorkW = XtWidth(SW_WorkWindow(w));
	WorkH = XtHeight(SW_WorkWindow(w));

	XdbDebug2(__FILE__, w, (Widget)SW_WorkWindow(w),
		"WorkWindow geo is W %d H %d X %d Y %d\n",
		WorkW, WorkH, WorkX, WorkY);
    }
    if (SW_ClipWindow(w)) {
	ClipX = XtX(SW_ClipWindow(w));
	ClipY = XtY(SW_ClipWindow(w));
	ClipW = XtWidth(SW_ClipWindow(w));
	ClipH = XtHeight(SW_ClipWindow(w));
    }

/*
 * Below, SwW/SwH are not initialised from XtWidth/XtHeight because we can
 * be called from MainWindow as well.
 *
 * XmMainWindow is a subclass of XmScrolledWindow.
 * In XmMainWindow, not all of the size managed by the widget is scrollable.
 * Therefore we're called with parameters indicating which part is.
 *
 * This also complicates the geometry request (resize ourselves) that we do.
 */
    SwW = fw;
    SwH = fh;

    if (childgeom) {
	if (child == w) {
	  if (childgeom->request_mode & CWWidth) SwW = childgeom->width;
	  if (childgeom->request_mode & CWHeight) SwH = childgeom->height;
	}
	if (child == (Widget)SW_WorkWindow(w)) {
	  if (childgeom->request_mode & CWWidth) WorkW = childgeom->width;
	  if (childgeom->request_mode & CWHeight) WorkH = childgeom->height;
	}
	if (child == (Widget)SW_ClipWindow(w)) {
	  if (childgeom->request_mode & CWWidth) ClipW = childgeom->width;
	  if (childgeom->request_mode & CWHeight) ClipH = childgeom->height;
	}
	if (child == (Widget)SW_HSB(w)) {
	  if (childgeom->request_mode & CWWidth) HsbW = childgeom->width;
	  if (childgeom->request_mode & CWHeight) HsbH = childgeom->height;
	}
	if (child == (Widget)SW_VSB(w)) {
	  if (childgeom->request_mode & CWWidth) VsbW = childgeom->width;
	  if (childgeom->request_mode & CWHeight) VsbH = childgeom->height;
	}
    }

/*
 * Rules for combinations of resource settings :
 *
 * Possible values :
 *	ParentResize (True, False) : depends on which method calls us
 *		can be anything in any of the cases described below.
 *	SW_VisualPolicy	(XmCONSTANT, XmVARIABLE, XmRESIZE_IF_POSSIBLE)
 *	SW_ScrollBarPolicy (XmAS_NEEDED, XmSTATIC)
 *	SW_ScrollPolicy (XmAPPLICATION_DEFINED, XmAUTOMATIC)
 *
 * Some combinations of the above are not possible.
 *
 * From the XmScrolledWindow(3) man page in OSF/Motif 2.0 :
 * - if XmNscrollBarDisplayPolicy == XmAS_NEEDED and
 *	XmNscrollingPolicy == XmAUTOMATIC then scrollbars are displayed only
 *	if the workspace exceeds the clip area
 * - if XmNscrollBarDisplayPolicy == XmSTATIC then scrollbars are always shown
 * - if XmNscrollingPolicy == XmAPPLICATION_DEFINED
 *	then XmNscrollBarDisplayPolicy == XmSTATIC
 * - if XmNscrollingPolicy == XmAUTOMATIC
 *	then default XmNscrollBarDisplayPolicy == XmAS_NEEDED,
 *	otherwise default XmNscrollBarDisplayPolicy == XmSTATIC
 * - if XmNscrollingPolicy == XmAUTOMATIC then XmNvisualPolicy == XmCONSTANT
 *	and automatically create scrollbars
 * - if XmNscrollingPolicy == XmAPPLICATION_DEFINED then client must
 *	create scrollbars, add callbacks for them, redisplay, etc.
 * - if visualPolicy == XmVARIABLE then XmSTATIC and allow the work area
 *	to grow & shrink as it wants, but clip window stays same size.
 *	Only resize if requested by parent.
 * - default visualPolicy == XmCONSTANT when scrollingPolicy == XmAUTOMATIC
 *	and visualPolicy == XmVARIABLE otherwise
 *
 * XmNscrollBarDisplayPolicy is CSG (create, set, get)
 * XmNscrollingPolicy is CG (cannot be altered after creation)
 * XmNvisualPolicy is CG (cannot be altered after creation)
 *
 * What do they mean ?
 * 1a. XmNvisualPolicy == XmCONSTANT
 *	The work area grows or shrinks as requested, but a clipping window
 *	forces the size of the visible portion to remain constant.
 *	The only time the viewing area can grow is in response to a resize
 *	from the ScrolledWindow's parent.
 * 1b. XmNvisualPolicy == XmVARIABLE
 *	Allows the work area to grow or shrink at any time and adjusts SW's
 *	layout to accommodate the new size.
 * 1c. XmNvisualPolicy == XmRESIZE_IF_POSSIBLE
 *	???	The symbol exists but nobody currently uses it.
 * 2a. SW_ScrollBarPolicy == XmAS_NEEDED
 *	Scrollbars are displayed only when needed.
 * 2b. SW_ScrollBarPolicy == XmSTATIC
 *	Scrollbars are always shown.
 * 3a. SW_ScrollPolicy == XmAPPLICATION_DEFINED
 *	Child widget or application does scrolling.
 * 3b. SW_ScrollPolicy == XmAUTOMATIC
 *	Scrolling is automatically handled by ScrolledWindow.
 */
    if (SW_ScrollPolicy(w) == XmAPPLICATION_DEFINED) {
	if (SW_ScrollBarPolicy(w) != XmSTATIC) {	/* Sanity check */
	  _XmWarning(w,
	    "_XmScrolledWindowLayout: XmAPPLICATION_DEFINED but not XmSTATIC");
	  SW_ScrollBarPolicy(w) = XmSTATIC;
	}

#if 0
/* Should not be set. See testXm/list/test6 : only has one SB */
/* However testXm/filesb/test1 itemsList extends under HSB without this */
	ShowHSB = ShowVSB = True;
#else
	if (WorkW > SwW)
		ShowHSB = True;
	if (WorkH > SwH)
		ShowVSB = True;
#endif

	/* If we're being resized (grown), make sure child follows */
	if (WorkW + 2 * SW_MarginWidth(w) + 2 * MGR_ShadowThickness(w) < SwW) {
	    Dimension	xx = WorkW;

	    WorkW = SwW - 2 * SW_MarginWidth(w) - 2 * MGR_ShadowThickness(w)
		- (ShowVSB ? (SW_Spacing(w) + VsbW) : 0);

	    XdbDebug2(__FILE__, w, SW_WorkWindow(w), "Grow WorkW %d -> %d\n",
		xx, WorkW);
	}
	if (WorkH + 2 * SW_MarginHeight(w) + 2 * MGR_ShadowThickness(w) < SwH) {
	    Dimension xx = WorkH;

	    WorkH = SwH - 2 * SW_MarginHeight(w) - 2 * MGR_ShadowThickness(w)
		- (ShowHSB ? (SW_Spacing(w) + HsbH) : 0);

	    XdbDebug2(__FILE__, w, SW_WorkWindow(w), "Grow WorkH %d -> %d\n",
		xx, WorkH);
	}

	/* If child is bigger than we are, grow (if we can) */
	if (ParentResize && SW_VisualPolicy(w) == XmVARIABLE) {
	  if (WorkW + 2 * (SW_MarginWidth(w) + MGR_ShadowThickness(w)) > SwW) {
	    Dimension xx = SwW;

	    SwW = WorkW + 2 * (SW_MarginWidth(w) + MGR_ShadowThickness(w))
		+ (ShowVSB ? (SW_Spacing(w) + VsbW) : 0);

	    XdbDebug2(__FILE__, w, SW_WorkWindow(w), "Grow SwW %d -> %d\n",
		xx, SwW);
	  }
	  if (WorkH + 2 * (SW_MarginHeight(w) + MGR_ShadowThickness(w)) > SwH) {
	    Dimension xx = SwH;

	    SwH = WorkH + 2 * (SW_MarginHeight(w) + MGR_ShadowThickness(w))
		+ (ShowHSB ? (SW_Spacing(w) + HsbH) : 0);

	    XdbDebug2(__FILE__, w, SW_WorkWindow(w), "Grow SwH %d -> %d\n",
		xx, SwH);
	  }
	}
    } else { /* SW_ScrollPolicy(w) == XmAUTOMATIC */
	/* We're in charge ! */
#if 0
	/* Find out how big work window wants to be */
	/* Note work window is not our child, but our grandchild */
	XtQueryGeometry(SW_WorkWindow(w), NULL, &workgeo);
#else
	workgeo.request_mode = CWWidth | CWHeight;
	workgeo.width = WorkW;
	workgeo.height = WorkH;
#endif
	if (workgeo.request_mode & CWWidth)
	    needw = workgeo.width + 2 * SW_MarginWidth(w)
		+ 2 * MGR_ShadowThickness(w);
	if (workgeo.request_mode & CWHeight)
	    needh = workgeo.height + 2 * SW_MarginHeight(w)
		+ 2 * MGR_ShadowThickness(w);

	if (SW_ScrollBarPolicy(w) == XmSTATIC) { /* Always show */
		ShowHSB = ShowVSB = True;
	} else {	/* do we need to display scrollbars ? */
	    ShowVSB = (needh > SwH);
	    ShowHSB = (needw > SwW);
	}
    }

/* What follows is code independent of XmAUTOMATIC/XmAPPLICATION_DEFINED */
/*
 * Try to resize ourselves if necessary (and if we're allowed to).
 */
#if 0
    if (SW_VisualPolicy(w) == XmVARIABLE) /* FIX ME - should we check ? */
#endif
    {
	if (SwW == 0 || SwH == 0 || (SwW == 1 && SwH == 1)) {
	    SwW = WorkW + 2 * SW_MarginWidth(w) + 2 * MGR_ShadowThickness(w);
	    SwH = WorkH + 2 * SW_MarginHeight(w) + 2 * MGR_ShadowThickness(w);
	}
    }

    if (! TestMode) {
	Boolean	mw = False;

	request.request_mode = CWWidth|CWHeight;
	request.width = SwW;
	request.height = SwH;

/*
 * Special treatment for XmMainWindow.
 * In all other cases (i.e. from within XmScrolledWindow),
 * _XmScrolledWindowLayout is called with fw,fh = XtWidth/XtHeight.
 */
	if (fw != XtWidth(w) || fh != XtHeight(w)) {
		mw = True;

		request.width += XtWidth(w) - fw;
		request.height += XtHeight(w) - fh;

		XdbDebug(__FILE__, w, "MW treatment : %d/%d instead of %d/%d\n",
			request.width, request.height, SwW, SwH);
	}

	if (request.width == XtWidth(w) && request.height == XtHeight(w)) {
	    XdbDebug(__FILE__, w, "Already have geo %s\n",
		XdbWidgetGeometry2String(&request));
	} else {
	    XdbDebug(__FILE__, w, "Request geo %s\n",
		XdbWidgetGeometry2String(&request));

	    res = _XmMakeGeometryRequest(w, &request);

	    XdbDebug(__FILE__, w, "==> Got %s %s\n",
		XdbGeometryResult2String(res),
		XdbWidgetGeometry2String(&request));

	    if (res == XtGeometryYes || res == XtGeometryDone) {
		if (mw) {	/* MainWindow */
		    /* ??? FIX ME - do we need to adapt something here ? */
		    SwW = XtWidth(w) = request.width;
		    SwH = XtHeight(w) = request.height;
		} else {
		    SwW = XtWidth(w) = request.width;
		    SwH = XtHeight(w) = request.height;
		}
	    }

	    /*
	     * If the geometry you got is different than requested, adapt
	     * the clip window's geometry to it.
	     *
	     * (Is treated in the switch statement below.)
	     */
	}
    }
/*
 * Figure out the layout, based on placement policy.
 * Note all modifications are done to local variables; they're only done
 * to the widgets later on in this function if not in test mode.
 */
    switch(SW_Placement(w)) {
    case XmTOP_RIGHT:
	ClipW = SwW - 2 * SW_MarginWidth(w) - 2 * MGR_ShadowThickness(w);
	ClipH = SwH - 2 * SW_MarginHeight(w) - 2 * MGR_ShadowThickness(w);
	ClipX = x + SW_MarginWidth(w) + MGR_ShadowThickness(w);
	ClipY = y + SW_MarginHeight(w) + MGR_ShadowThickness(w);

	if (ShowHSB && HasHSB) {
	    HsbX = SW_MarginWidth(w);
	    HsbY = SW_MarginHeight(w);
	    HsbW = SwW - 2 * SW_MarginWidth(w);
	    if (ShowVSB && HasVSB)
		HsbW -= VsbW + SW_Spacing(w);
	    ClipH -= SW_Spacing(w) + HsbH;
	    ClipY += SW_Spacing(w) + HsbH;
	} else {
	    if (HasHSB)
		SetMinimum((Widget)SW_HSB(w));
	}

	if (ShowVSB && HasVSB) {
	    VsbY = SW_MarginHeight(w);
	    VsbX = SwW - SW_MarginWidth(w) - VsbW;
	    VsbH = SwH /* ??? XtHeight(w) */ - 2 * SW_MarginHeight(w);
	    if (ShowHSB && HasHSB) {
		VsbY += HsbH + SW_Spacing(w);
		VsbH -= HsbH + SW_Spacing(w);
	    }
	    ClipW -= SW_Spacing(w) + VsbW;
	} else {
	    if (HasVSB)
		SetMinimum((Widget)SW_VSB(w));
	}
	break;

    case XmBOTTOM_LEFT:
	ClipW = SwW - 2 * SW_MarginWidth(w) - 2 * MGR_ShadowThickness(w);
	ClipH = SwH - 2 * SW_MarginHeight(w) - 2 * MGR_ShadowThickness(w);
	ClipX = x + SW_MarginWidth(w) + MGR_ShadowThickness(w);
	ClipY = y + SW_MarginHeight(w) + MGR_ShadowThickness(w);

	if (ShowHSB && HasHSB) {
	    HsbX = SW_MarginWidth(w);
	    HsbY = SwH - SW_MarginHeight(w) - HsbH;
	    HsbW = SwW - 2 * SW_MarginWidth(w);
	    if (ShowVSB && HasVSB) {
		HsbW -= VsbW + SW_Spacing(w);
		HsbX += VsbW + SW_Spacing(w);
	    }
	    ClipH -= SW_Spacing(w) + HsbH;
	} else {
	    if (HasHSB)
		SetMinimum((Widget)SW_HSB(w));
	}

	if (ShowVSB && HasVSB) {
	    VsbY = y + SW_MarginHeight(w);
	    VsbX = x + SW_MarginWidth(w);
	    VsbH = SwH - 2 * SW_MarginHeight(w);
	    if (ShowHSB && HasHSB)
		VsbH -= HsbH + SW_Spacing(w);
	    ClipW -= SW_Spacing(w) + VsbW;
	    ClipX += SW_Spacing(w) + VsbW;
	} else {
	    if (HasVSB)
		SetMinimum((Widget)SW_VSB(w));
	}
	break;

    case XmTOP_LEFT:
	ClipW = SwW - 2 * SW_MarginWidth(w) - 2 * MGR_ShadowThickness(w);
	ClipH = SwH - 2 * SW_MarginHeight(w) - 2 * MGR_ShadowThickness(w);
	ClipX = x + SW_MarginWidth(w) + MGR_ShadowThickness(w);
	ClipY = y + SW_MarginHeight(w) + MGR_ShadowThickness(w);

	if (ShowHSB && HasHSB) {
	    HsbX = x + SW_MarginWidth(w);
	    HsbY = y + SW_MarginHeight(w);
	    HsbW = SwW - 2 * SW_MarginWidth(w);
	    if (ShowVSB && HasVSB) {
		HsbX += VsbW + SW_Spacing(w);
		HsbW -= VsbW + SW_Spacing(w);
	    }
	    ClipH -= SW_Spacing(w) + HsbH;
	    ClipY += SW_Spacing(w) + HsbH;
	} else {
	    if (HasHSB)
		SetMinimum((Widget)SW_HSB(w));
	}

	if (ShowVSB && HasVSB) {
	    VsbY = y + SW_MarginHeight(w);
	    VsbX = x + SW_MarginWidth(w);
	    VsbH = SwH  - 2 * SW_MarginHeight(w);
	    if (ShowHSB && HasHSB) {
		VsbH -= HsbH + SW_Spacing(w);
		VsbY += HsbH + SW_Spacing(w);
	    }
	    ClipW -= SW_Spacing(w) + VsbW;
	    ClipX += SW_Spacing(w) + VsbW;
	} else {
	    if (HasVSB)
		SetMinimum((Widget)SW_VSB(w));
	}
	break;

    case XmBOTTOM_RIGHT:
    default:
	ClipW = SwW - 2 * SW_MarginWidth(w) - 2 * MGR_ShadowThickness(w);
	ClipH = SwH - 2 * SW_MarginHeight(w) - 2 * MGR_ShadowThickness(w);
	ClipX = x + SW_MarginWidth(w) + MGR_ShadowThickness(w);
	ClipY = y + SW_MarginHeight(w) + MGR_ShadowThickness(w);

	if (ShowHSB && HasHSB) {
	    HsbX = x + SW_MarginWidth(w);
	    HsbY = y + SwH - SW_MarginHeight(w) - HsbH;
	    HsbW = SwW - 2 * SW_MarginWidth(w);
	    if (ShowVSB && HasVSB)
		HsbW -= VsbW + SW_Spacing(w);
	    ClipH -= SW_Spacing(w) + HsbH;
	} else {
	    if (HasHSB)
		SetMinimum((Widget)SW_HSB(w));
	}

	if (ShowVSB && HasVSB) {
	    VsbY = y + SW_MarginHeight(w);
	    VsbX = x + SwW - SW_MarginWidth(w) - VsbW;
	    VsbH = SwH - 2 * SW_MarginHeight(w);
	    if (ShowHSB && HasHSB)
		VsbH -= HsbH + SW_Spacing(w);
	    ClipW -= SW_Spacing(w) + VsbW;
	} else {
	    if (HasVSB)
		SetMinimum((Widget)SW_VSB(w));
	}
	break;
    }

#if 1
    if (SW_ScrollPolicy(w) == XmAPPLICATION_DEFINED) {
	WorkW = ClipW;
	WorkH = ClipH;
	WorkX = ClipX;
	WorkY = ClipY;
    }
#endif

/*
 * In the code above (all of the switch statement really), some
 * variables can become negative where we don't want them to.
 * Fix that here by giving them their original value from the
 * widget resource they represent.
 */
    if (ClipH < 0)
	ClipH = SW_ClipWindow(w) ? XtHeight(SW_ClipWindow(w)) : 0;
    if (ClipW < 0)
	ClipW = SW_ClipWindow(w) ? XtWidth(SW_ClipWindow(w)) : 0;
    if (WorkH < 0)
	WorkH = SW_WorkWindow(w) ? XtHeight(SW_WorkWindow(w)) : 0;
    if (WorkW < 0)
	WorkW = SW_WorkWindow(w) ? XtWidth(SW_WorkWindow(w)) : 0;
    if (VsbH < 0)
	VsbH = SW_VSB(w) ? XtHeight(SW_VSB(w)) : 0;
    if (VsbW < 0)
	VsbW = SW_VSB(w) ? XtWidth(SW_VSB(w)) : 0;
    if (HsbH < 0)
	HsbH = SW_HSB(w) ? XtHeight(SW_HSB(w)) : 0;
    if (HsbW < 0)
	HsbW = SW_HSB(w) ? XtWidth(SW_HSB(w)) : 0;
/* End fixing of negative values */

    if (TestMode) {	/* TestMode : feed back feedback */
	if (child == (Widget)SW_WorkWindow(w)) {
	    childgeom->request_mode = CWX|CWY|CWHeight|CWWidth;
	    childgeom->x = WorkX;
	    childgeom->y = WorkY;
	    childgeom->width = WorkW;
	    childgeom->height = WorkH;
	}
	if (child == (Widget)SW_ClipWindow(w)) {
	    childgeom->request_mode = CWX|CWY|CWHeight|CWWidth;
	    childgeom->x = ClipX;
	    childgeom->y = ClipY;
	    childgeom->width = ClipW;
	    childgeom->height = ClipH;
	}
	if (child == (Widget)SW_HSB(w)) {
	    childgeom->request_mode = CWX|CWY|CWHeight|CWWidth;
	    childgeom->x = HsbX;
	    childgeom->y = HsbY;
	    childgeom->width = HsbW;
	    childgeom->height = HsbH;
	}
	if (child == (Widget)SW_VSB(w)) {
	    childgeom->request_mode = CWX|CWY|CWHeight|CWWidth;
	    childgeom->x = VsbX;
	    childgeom->y = VsbY;
	    childgeom->width = VsbW;
	    childgeom->height = VsbH;
	}
	if (child == w) {
	    childgeom->request_mode = CWHeight|CWWidth;
	    childgeom->width = SwW;
	    childgeom->height = SwH;
	}

	XdbDebug2(__FILE__, w, child, "TestMode => %s\n",
		XdbWidgetGeometry2String(childgeom));

	/* Nothing actually happens beyond this point */
    } else {	/* ~TestMode : Modify everyone's geometry. */

#if	1
    if (XdbInDebug(__FILE__, w)) {
	XdbDebug(__FILE__, w, "_XmScrolledWindowLayout results : Sw %dx%d\n",
		SwW, SwH);
	XdbDebug0(__FILE__, w, "\t\tX\tY\tW\tH\n\tWork\t%d\t%d\t%d\t%d\n",
		WorkX, WorkY, WorkW, WorkH);
	XdbDebug0(__FILE__, w, "\tClip\t%d\t%d\t%d\t%d\n",
		ClipX, ClipY, ClipW, ClipH);
	XdbDebug0(__FILE__, w, "\tHsb\t%d\t%d\t%d\t%d\t%3s managed\n",
		HsbX, HsbY, HsbW, HsbH,
		(ShowHSB && HasHSB) ? "" : "not");
	XdbDebug0(__FILE__, w, "\tVsb\t%d\t%d\t%d\t%d\t%3s managed\n",
		VsbX, VsbY, VsbW, VsbH,
		(ShowVSB && HasVSB) ? "" : "not");
    }
#endif

#if 0
      if (SW_ScrollPolicy(w) == XmAUTOMATIC)
#else
      if (1)
#endif
      {
/*
 * Not sure what we need to do here in the XmAPPLICATION_DEFINED case.
 * Need to manage/unmanage ?
 * Need to configure scrollbar geometry ?
 */
	if (HasVSB) {	/* Actions ordered to be as fast as possible */
      if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	    if (XtIsManaged(SW_VSB(w)) && !ShowVSB) {
		XtUnmanageChild((Widget)SW_VSB(w));
		SW_HasVSB(w) = False;
	    }

	    _XmConfigureObject((Widget)SW_VSB(w),
		VsbX, VsbY,
		VsbW ? VsbW : 1, VsbH ? VsbH : 1,
		XtBorderWidth(SW_VSB(w)));

      if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	    if (ShowVSB && !XtIsManaged(SW_VSB(w))) {
		XtManageChild((Widget)SW_VSB(w));
		SW_HasVSB(w) = True;
	    }
	}
	if (HasHSB) {	/* Actions ordered to be as fast as possible */
      if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	    if (XtIsManaged(SW_HSB(w)) && !ShowHSB) {
		XtUnmanageChild((Widget)SW_HSB(w));
		SW_HasHSB(w) = False;
	    }

	    _XmConfigureObject((Widget)SW_HSB(w),
		HsbX, HsbY,
		HsbW ? HsbW : 1, HsbH ? HsbH : 1,
		XtBorderWidth(SW_HSB(w)));

      if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	    if (ShowHSB && !XtIsManaged(SW_HSB(w))) {
		XtManageChild((Widget)SW_HSB(w));
		SW_HasHSB(w) = True;
	    }
	}
	if (SW_WorkWindow(w)) {
	    _XmConfigureObject(SW_WorkWindow(w),
		WorkX, WorkY, WorkW, WorkH, XtBorderWidth(SW_WorkWindow(w)));
	}
	if (SW_ClipWindow(w)) {
	    _XmConfigureObject((Widget)SW_ClipWindow(w),
		ClipX, ClipY, ClipW, ClipH, XtBorderWidth(SW_ClipWindow(w)));

	    SW_ClipWidth(w) = ClipW;
	    SW_ClipHeight(w) = ClipH;
	}

	SW_WorkWidth(w) = WorkW;
	SW_WorkHeight(w) = WorkH;

	if (SW_ScrollPolicy(w) == XmAUTOMATIC) {
	  FixupScrollBars(w);
	  RepositionScrolledWindow((Widget)SW_ClipWindow(w), NULL, NULL);
	}
      } else {	/* XmAPPLICATION_DEFINED */
      }

      SW_WorkWidth(w) = WorkW;
      SW_WorkHeight(w) = WorkH;
    }	/* ~TestMode */
}

static String moves[2] = {
    "0",
    "1"
};
#define PAGE_UP		&moves[0]
#define PAGE_DOWN	&moves[0]
#define PAGE_LEFT	&moves[1]
#define PAGE_RIGHT	&moves[1]

static void
PageUp(Widget w,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w))
	XtCallActionProc((Widget)SW_VSB(w), "PageUpOrLeft", event, PAGE_UP, 1);
}

static void
PageDown(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w))
	XtCallActionProc((Widget)SW_VSB(w), "PageDownOrRight", event, PAGE_DOWN, 1);
}

static void
PageLeft(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w))
	XtCallActionProc((Widget)SW_HSB(w), "PageUpOrLeft", event, PAGE_LEFT, 1);
}

static void
PageRight(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w))
	XtCallActionProc((Widget)SW_HSB(w), "PageDownOrRight", event, PAGE_RIGHT, 1);
}

static void
BeginLine(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w))
	XtCallActionProc((Widget)SW_HSB(w), "TopOrBottom", event, params, *num_params);
}

static void
EndLine(Widget w,
	XEvent *event,
	String *params,
	Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w))
	XtCallActionProc((Widget)SW_HSB(w), "TopOrBottom", event, params, *num_params);
}

static void
BeginData(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w))
	XtCallActionProc((Widget)SW_VSB(w), "TopOrBottom", event, params, *num_params);
}

static void
EndData(Widget w,
	XEvent *event,
	String *params,
	Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w))
	XtCallActionProc((Widget)SW_VSB(w), "TopOrBottom", event, params, *num_params);
}

Widget
XmCreateScrolledWindow(Widget parent, 
		       char *name,
		       Arg *argList,
		       Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmScrolledWindowWidgetClass,
			  parent,
			  argList, argcount);
}

void 
XmScrollVisible(Widget scrollw_widget,
		Widget widget,
		Dimension left_right_margin,
		Dimension top_bottom_margin)
{
}

void 
XmScrolledWindowSetAreas(Widget widget,
			 Widget h_scrollbar,
			 Widget v_scrollbar,
			 Widget work_region)
{
    XtVaSetValues(widget,
		  XmNhorizontalScrollBar, h_scrollbar,
		  XmNverticalScrollBar, v_scrollbar,
		  XmNworkWindow, work_region,
		  NULL);
}
