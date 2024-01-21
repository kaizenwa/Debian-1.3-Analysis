/**
 *
 * $Id: Manager.c,v 1.22 1997/01/11 05:48:39 miers Exp $
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

static char rcsid[] = "$Id: Manager.c,v 1.22 1997/01/11 05:48:39 miers Exp $";

#include <stdio.h>
#include <stdlib.h>

#include <LTconfig.h>
#include <XmI/XmI.h>
#include <XmI/TraversalI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/ManagerP.h>
#include <Xm/GadgetP.h>
#include <Xm/RepType.h>
#include <X11/keysym.h>

#include <XmI/DebugUtil.h>

/* For determining class type */
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/MenuUtilP.h>

/*
 * Yes I know this is crazy... Danny
 */
#include <Xm/RowColumnP.h>
#include <Xm/DialogS.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static XtGeometryResult geometry_manager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply);
static void changed_managed(Widget w);

static void constraint_initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);

static XmNavigability widget_navigable(Widget w);

static void _XmManagerInstallAccelerators(Widget w);
void _XmManagerInstallAccelerator(Widget m, Widget w, String s);

/* action routine prototypes */
void _XmGadgetButtonMotion(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetArm(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetMultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetMultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetDrag(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetSelect(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmManagerParentActivate(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmManagerParentCancel(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraversePrevTabGroup(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseNextTabGroup(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraversePrev(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseNext(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseUp(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseDown(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseLeft(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseRight(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetTraverseHome(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetHelp(Widget w, XEvent *event, String *params, Cardinal *num_params);
void _XmGadgetKeyInput(Widget w, XEvent *event, String *params, Cardinal *num_params);

static void _XmManagerActivateTranslation(Widget w, XEvent *event, String *params, Cardinal *num_params);

/* the event handler to handle pointer position related events for gadget children */
static void _XmManagerEventHandler(Widget w, XtPointer data, XEvent *event, Boolean *cont);

char _XmManager_defaultTranslations[] = 
   "<BtnMotion>:          ManagerGadgetButtonMotion()\n\
    <Btn1Down>:           ManagerGadgetArm()\n\
    <Btn1Down>,<Btn1Up>:  ManagerGadgetActivate()\n\
    <Btn1Up>:             ManagerGadgetActivate()\n\
    <Btn1Down>(2+):       ManagerGadgetMultiArm()\n\
    <Btn1Up>(2+):         ManagerGadgetMultiActivate()\n\
    <Btn2Down>:           ManagerGadgetDrag()\n\
    <Key>osfCancel:       ManagerParentCancel()\n\
    <Key>osfSelect:       ManagerGadgetSelect()\n\
    <Key>osfHelp:         ManagerGadgetHelp()\n\
    ~s ~m ~a <Key>Return: ManagerParentActivate()\n\
    ~s ~m ~a <Key>space:  ManagerGadgetSelect()\n\
    <Key>:                ManagerGadgetKeyInput()";

char _XmManager_managerTraversalTranslations[] =
   "<EnterWindow>:     ManagerEnter()\n\
    <LeaveWindow>:     ManagerLeave()\n\
    <FocusOut>:        ManagerFocusOut()\n\
    <FocusIn>:         ManagerFocusIn()\n\
    <Key>osfBeginLine: ManagerGadgetTraverseHome()\n\
    <Key>osfUp:        ManagerGadgetTraverseUp()\n\
    <Key>osfDown:      ManagerGadgetTraverseDown()\n\
    <Key>osfLeft:      ManagerGadgetTraverseLeft()\n\
    <Key>osfRight:     ManagerGadgetTraverseRight()\n\
    s ~m ~a <Key>Tab:  ManagerGadgetPrevTabGroup()\n\
    ~m ~a <Key>Tab:    ManagerGadgetNextTabGroup()";

static XtActionsRec actions[] = {
    {"ManagerGadgetButtonMotion", _XmGadgetButtonMotion},
    {"ManagerGadgetArm", _XmGadgetArm},
    {"ManagerGadgetActivate", _XmGadgetActivate},
    {"ManagerGadgetMultiArm", _XmGadgetMultiArm},
    {"ManagerGadgetDrag", _XmGadgetDrag},
    {"ManagerGadgetSelect", _XmGadgetSelect},
    {"ManagerGadgetHelp", _XmGadgetHelp},
    {"ManagerGadgetKeyInput", _XmGadgetKeyInput},
    {"ManagerGadgetMultiActivate", _XmGadgetMultiActivate},
    {"ManagerGadgetMultiArm", _XmGadgetMultiArm},
    {"ManagerGadgetNextTabGroup", _XmGadgetTraverseNextTabGroup},
    {"ManagerGadgetPrevTabGroup", _XmGadgetTraversePrevTabGroup},
    {"ManagerGadgetTraverseDown", _XmGadgetTraverseDown},
    {"ManagerGadgetTraverseUp", _XmGadgetTraverseUp},
    {"ManagerGadgetTraverseHome", _XmGadgetTraverseHome},
    {"ManagerGadgetTraverseLeft", _XmGadgetTraverseLeft},
    {"ManagerGadgetTraverseRight", _XmGadgetTraverseRight},
    {"ManagerGadgetTraverseNext", _XmGadgetTraverseNext},
    {"ManagerGadgetTraversePrev", _XmGadgetTraversePrev},
    {"ManagerParentActivate", _XmManagerParentActivate},
    {"ManagerParentCancel", _XmManagerParentCancel},
    {"ManagerEnter", _XmManagerEnter},
    {"ManagerLeave", _XmManagerLeave},
    {"ManagerFocusOut", _XmManagerFocusOut},
    {"ManagerFocusIn", _XmManagerFocusIn},
    {"ActivateTranslation", _XmManagerActivateTranslation},
    {"Enter", _XmManagerEnter},
    {"FocusIn", _XmManagerFocusIn},
    {"Help", _XmGadgetHelp},
    {"Arm", _XmGadgetArm},
    {"Activate", _XmGadgetActivate},
};

#define Offset(field) XtOffsetOf(XmManagerRec, manager.field)

/* Resources for the manager class */
static XtResource resources[] = {
    {
	XmNunitType, XmCUnitType, XmRUnitType,
	sizeof(unsigned char), Offset(unit_type),
	XmRCallProc, (XtPointer)_XmUnitTypeDefault
    },
    {
	XmNx, XmCPosition, XmRHorizontalPosition,
	sizeof(Position), XtOffsetOf(XmManagerRec, core.x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNy, XmCPosition, XmRVerticalPosition,
	sizeof(Position), XtOffsetOf(XmManagerRec, core.y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNwidth, XmCDimension, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmManagerRec, core.width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmManagerRec, core.height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNborderWidth, XmCBorderWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmManagerRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNforeground, XmCForeground, XmRPixel,
	sizeof(Pixel), Offset(foreground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNbackground, XmCBackground, XmRPixel,
	sizeof(Pixel), XtOffsetOf(XmManagerRec, core.background_pixel),
	XmRCallProc, (XtPointer)_XmBackgroundColorDefault
    },
    {
	XmNbackgroundPixmap, XmCPixmap, XmRXmBackgroundPixmap,
	sizeof(Pixmap), XtOffsetOf(XmManagerRec, core.background_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNhighlightColor, XmCHighlightColor, XmRPixel,
	sizeof(Pixel), Offset(highlight_color),
	XmRString, "Black"
	/* FIXME: should be: XmRCallProc, (XtPointer)something */
    },
    {
	XmNhighlightPixmap, XmCHighlightPixmap, XmRManHighlightPixmap,
	sizeof(Pixmap), Offset(highlight_pixmap),
	XmRCallProc, (XtPointer)NULL /* FIXME: NEEDS A PROC */
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), Offset(navigation_type),
	XmRImmediate, (XtPointer)XmTAB_GROUP
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNtopShadowColor, XmCTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(top_shadow_color),
	XmRCallProc, (XtPointer)_XmTopShadowColorDefault
    },
    {
	XmNtopShadowPixmap, XmCTopShadowPixmap, XmRManTopShadowPixmap,
	sizeof(Pixmap), Offset(top_shadow_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
	/* FIXME: should be XmRCallProc, (XtPointer)something */
    },
    {
	XmNbottomShadowColor, XmCBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(bottom_shadow_color),
	XmRCallProc, (XtPointer)_XmBottomShadowColorDefault
    },
    {
	XmNbottomShadowPixmap, XmCBottomShadowPixmap, XmRManBottomShadowPixmap,
	sizeof(Pixmap), Offset(bottom_shadow_pixmap),
	XtRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNhelpCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(help_callback),
	XtRPointer, (XtPointer)NULL
    },
    {
	XmNuserData, XmCUserData, XmRPointer,
	sizeof(XtPointer), Offset(user_data),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), Offset(traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNstringDirection, XmCStringDirection, XmRStringDirection,
	sizeof(XmStringDirection), Offset(string_direction),
	XmRImmediate, (XtPointer)((XmStringDirection)XmUNSPECIFIED)
    },
    {
	XmNinitialFocus, XmCInitialFocus, XmRWidget,
	sizeof(Widget), Offset(initial_focus),
	XmRImmediate, (XtPointer)NULL
    }
};

#define MOffset(field)	XtOffset(XmManagerWidget, manager.field)
#define COffset(field)	XtOffset(XmManagerWidget, core.field)
static XmSyntheticResource syn_resources[] = {
    /* core part */
    {
	XmNx, 
	sizeof(Position), COffset(x),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNy, 
	sizeof(Position), 
	COffset(y),
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
    /* manager */
    {
	XmNshadowThickness, 
	sizeof(Dimension), MOffset(shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
};

static XmBaseClassExtRec _XmManagerCoreClassExtRec = {
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
    /* widget_navigable          */ widget_navigable,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

static CompositeClassExtensionRec managerCompositeExt = 
{
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmManagerClassExtRec _XmManagerClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmManagerClassRec xmManagerClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &constraintClassRec,
        /* class_name            */ "XmManager",
	/* widget_size           */ sizeof(XmManagerRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ _XmManagerGetValuesHook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmManager_defaultTranslations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmManagerCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager,
        /* change_managed   */ changed_managed,
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ (XtPointer)&managerCompositeExt,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,  /* FIX ME */
        /* subresource_count */ 0,     /* FIX ME */
        /* constraint_size   */ 0,     /* FIX ME */
        /* initialize        */ constraint_initialize,
        /* destroy           */ NULL,  /* FIX ME */
        /* set_values        */ NULL,  /* FIX ME */
        /* extension         */ NULL,   /* FIX ME */
    },
    /* XmManager class part */
    {
	/* translations                 */ _XmManager_managerTraversalTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ _XmParentProcess,
	/* extension                    */ (XtPointer)&_XmManagerClassExtRec
    },
};

WidgetClass xmManagerWidgetClass = (WidgetClass)&xmManagerClassRec;

static void 
class_initialize()
{
    _XmInitializeExtensions();
    _XmManagerCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)widget_class;
    XmManagerWidgetClass swc = (XmManagerWidgetClass)widget_class->core_class.superclass;
    CompositeClassExtension ext, *extptr;

    if (mwc->manager_class.translations == XtInheritTranslations)
	mwc->manager_class.translations =
		swc->manager_class.translations;
    if (mwc->manager_class.parent_process == XmInheritParentProcess)
	mwc->manager_class.parent_process =
		swc->manager_class.parent_process;

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr(
		(XmGenericClassExt*)&(mwc->composite_class.extension),
		NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = mwc->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    mwc->composite_class.extension = (XtPointer) ext;
	}
    }    

    _XmBaseClassPartInitialize(widget_class);
    _XmFastSubclassInit(widget_class, XmMANAGER_BIT);

    /* compile the resources */
    if (widget_class == xmManagerWidgetClass) {
	_XmSortResourceList((XrmResource **)mwc->core_class.resources,
			    mwc->core_class.num_resources);
    }

    _XmInitializeSyntheticResources(mwc->manager_class.syn_resources,
				    mwc->manager_class.num_syn_resources);

    if (widget_class != xmManagerWidgetClass) {
	XmManagerWidgetClass msc =
	    (XmManagerWidgetClass)mwc->core_class.superclass;

	_XmBuildResources(&mwc->manager_class.syn_resources,
			  &mwc->manager_class.num_syn_resources,
			  msc->manager_class.syn_resources,
			  msc->manager_class.num_syn_resources);
    }
}

static void
CreateHighlightGC(Widget mw)
{    
    XGCValues values;
    unsigned long mask;

    if (MGR_HighlightPixmap(mw) != None 
	&& MGR_HighlightPixmap(mw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed highlight*/
	mask = GCTile | GCFillStyle;
	
	values.tile = MGR_HighlightPixmap(mw); 
	values.fill_style = FillTiled;
    }
    else
    {
	mask = GCForeground | GCBackground;

	values.foreground = MGR_HighlightColor(mw);
	values.background = XtBackground(mw);
    }

    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    MGR_HighlightGC(mw) = XtGetGC((Widget)mw, mask, &values);    
}

static void
CreateBottomShadowGC(Widget mw)
{
    XGCValues values;
    unsigned long mask;

    if (MGR_BottomShadowPixmap(mw) != None 
	&& MGR_BottomShadowPixmap(mw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed bottom shadow*/
	mask = GCTile | GCFillStyle;
	
	values.tile = MGR_BottomShadowPixmap(mw); 
	values.fill_style = FillTiled;
    }
    else
    {
	mask = GCForeground | GCBackground;

	values.foreground = MGR_BottomShadowColor(mw);
	values.background = XtBackground(mw);
    }

    mask |= GCLineWidth;
    values.line_width = 1;

    MGR_BottomShadowGC(mw) = XtGetGC((Widget)mw, mask, &values);    
}

static void
CreateTopShadowGC(Widget mw)
{
    XGCValues values;
    unsigned long mask;

    if (MGR_TopShadowPixmap(mw) != None 
	&& MGR_TopShadowPixmap(mw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed top shadow*/
	mask = GCTile | GCFillStyle;
	
	values.tile = MGR_TopShadowPixmap(mw); 
	values.fill_style = FillTiled;
    }
    else
    {
	mask = GCForeground | GCBackground;

	values.foreground = MGR_TopShadowColor(mw);
	values.background = XtBackground(mw);
    }

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    MGR_TopShadowGC(mw) = XtGetGC((Widget)mw, mask, &values);
}

static void
CreateBackgroundGC(Widget mw)
{
    XGCValues values;
    unsigned long mask;

    mask = GCForeground | GCBackground;
 
    values.foreground = XtBackground(mw);
    values.background = XtBackground(mw);

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    MGR_BackgroundGC(mw) = XtGetGC((Widget)mw, mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)new_w->core.widget_class;

    CreateTopShadowGC(new_w);
    CreateBottomShadowGC(new_w);
    CreateHighlightGC(new_w);
    CreateBackgroundGC(new_w);


    MGR_EventHandlerAdded(new_w) = False;
    MGR_HighlightedWidget(new_w) = NULL;
    MGR_SelectedGadget(new_w) = NULL;
    MGR_ActiveChild(new_w) = NULL;
    MGR_KeyboardList(new_w) = NULL;
    MGR_NumKeyboardEntries(new_w) = 0;
    MGR_SizeKeyboardList(new_w) = 0;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     MGR_UnitType(new_w),
			     new_w))
	MGR_UnitType(new_w) = XmPIXELS;


    /* initialize these to values that aren't possible */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     MGR_NavigationType(new_w),
			     new_w))
	MGR_UnitType(new_w) = XmTAB_GROUP;

    /* This is way too simple. FIXME */
    if (MGR_StringDirection(new_w) == (XmStringDirection)XmUNSPECIFIED)
	MGR_StringDirection(new_w) = XmSTRING_DIRECTION_L_TO_R;

    _XmNavigInitialize(request, new_w, args, num_args);

    _XmManagerImportArgs(new_w, args, num_args);

    if (mwc->manager_class.translations) {
	XtOverrideTranslations(new_w, 
			    XtParseTranslationTable(mwc->manager_class.translations));
    }
}

static void
destroy(Widget w)
{
    XtReleaseGC(w,MGR_TopShadowGC(w));
    XtReleaseGC(w,MGR_BottomShadowGC(w));
    XtReleaseGC(w,MGR_HighlightGC(w));
    XtReleaseGC(w,MGR_BackgroundGC(w));

    if (MGR_EventHandlerAdded(w))
	XtRemoveEventHandler(w, 
			     PointerMotionMask, 
			     False, 
			     _XmManagerEventHandler, 
			     NULL);
    _XmNavigDestroy(w);
}

static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
/*
 * MLM 960524 - if you get an XError: widget has zero width or height, try
 * enabling this code.  It is a hack, though, and will probably go away.
 */
#if 1
    if (XtWidth(w) == 0)
	XtWidth(w) = 1;
    if (XtHeight(w) == 0)
	XtHeight(w) = 1;
#endif

    XtCreateWindow(w, (unsigned int) InputOutput,
		  (Visual *)CopyFromParent, *value_mask, attributes);

    _XmManagerInstallAccelerators(w);
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean need_refresh = False;

    XdbDebug(__FILE__, new_w, "SetValues()\n");

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     MGR_UnitType(new_w),
			     new_w))
	MGR_UnitType(new_w) = MGR_UnitType(old);

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     MGR_NavigationType(new_w),
			     new_w))
	MGR_NavigationType(new_w) = MGR_NavigationType(old);

    need_refresh = _XmNavigSetValues(old, request, new_w, args, num_args);

    if (MGR_ShadowThickness(old) != MGR_ShadowThickness(new_w)
	|| MGR_Foreground(old) != MGR_Foreground(new_w))
	need_refresh = True;

    if (MGR_HighlightColor(old) != MGR_HighlightColor(new_w)
	|| MGR_HighlightPixmap(old) != MGR_HighlightPixmap(new_w)) 
    {
	XtReleaseGC(new_w, MGR_HighlightGC(new_w));
	CreateHighlightGC(new_w);
	need_refresh = True;
    }

    if (MGR_BottomShadowColor(old) != MGR_BottomShadowColor(new_w)
	|| MGR_BottomShadowPixmap(old) != MGR_BottomShadowPixmap(new_w)) 
    {
	XtReleaseGC(new_w, MGR_BottomShadowGC(new_w));
	CreateBottomShadowGC(new_w);
	
	need_refresh = True;
    }

    if (MGR_TopShadowColor(old) != MGR_TopShadowColor(new_w)
	|| MGR_TopShadowPixmap(old) != MGR_TopShadowPixmap(new_w)) 
    {
	XtReleaseGC(new_w, MGR_TopShadowGC(new_w));
	CreateTopShadowGC(new_w);

	need_refresh = True;
    }

    if (XtBackground(old) != XtBackground(new_w))
    {
	XtReleaseGC(new_w, MGR_BackgroundGC(new_w));
	CreateBackgroundGC(new_w);
	
	need_refresh = True;
    }
    _XmManagerImportArgs(new_w, args, num_args);

    return need_refresh;
}

static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    XdbDebug(__FILE__, w, "GeometryManager()\n");

    /* FIX ME */
    if (reply)
	*reply = *request;
    return XtGeometryYes;
}

static void
changed_managed(Widget w)
{
    /* FIX ME */
    XdbDebug(__FILE__, w, "ChangeManaged()\n");


    _XmNavigChangeManaged(w);
}

/* constraint class methods */

static void 
constraint_initialize(Widget request, 
		      Widget new_w, 
		      ArgList args, 
		      Cardinal *num_args)
{
    Widget manager = XtParent(new_w);
    /* check to see if the gadget's event mask specifies an event
       that requires us to install an event handler. */

    if (XmIsGadget(new_w))
	if (!MGR_EventHandlerAdded(manager))
	{
	    /* we only need to check if we haven't already added one. */
	    
	    if (G_EventMask(new_w) & XmMOTION_EVENT
		|| G_EventMask(new_w) & XmENTER_EVENT
		|| G_EventMask(new_w) & XmLEAVE_EVENT
		|| G_EventMask(new_w) & XmFOCUS_IN
		|| G_EventMask(new_w) & XmFOCUS_OUT)
	    {
		XdbDebug(__FILE__, manager, "adding event handler\n");

		XtAddEventHandler(manager, PointerMotionMask, True, 
				  (XtEventHandler)_XmManagerEventHandler, NULL);

		MGR_EventHandlerAdded(manager) = True;
	    }
	}

}

void
_XmGadgetArm(Widget w,
	     XEvent *event,
	     String *params,
	     Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget g = (Widget)_XmInputForGadget(w, ev->x, ev->y);

    XdbDebug(__FILE__, w, "Inside _XmGadgetArm()\n");

    if (_XmIsNavigable(g)) {
	if (g)
	    _XmDispatchGadgetInput(g,
			           event,
			           XmARM_EVENT);

	MGR_SelectedGadget(w) = (XmGadget)g;
    }
}

void
_XmGadgetActivate(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    Widget g = (Widget)MGR_SelectedGadget(w);

    XdbDebug(__FILE__, w, "Inside _XmGadgetActivate()\n");

    if (g)
	_XmDispatchGadgetInput((Widget)g,
			       event,
			       XmACTIVATE_EVENT);
    MGR_SelectedGadget(w) = NULL;
}

void
_XmGadgetMultiArm(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget g = (Widget)_XmInputForGadget(w, ev->x, ev->y);

    XdbDebug(__FILE__, w, "Inside _XmGadgetMultiArm()\n");

    if (g) {
	_XmDispatchGadgetInput(g,
			       event,
			       XmMULTI_ARM_EVENT);
	MGR_SelectedGadget(w) = (XmGadget)g;
    }
}

void
_XmGadgetMultiActivate(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params)
{
    Widget g = (Widget)MGR_SelectedGadget(w);

    XdbDebug(__FILE__, w, "Inside _XmGadgetMultiActivate()\n");

    if (g)
	_XmDispatchGadgetInput((Widget)g,
			       event,
			       XmMULTI_ACTIVATE_EVENT);
    MGR_SelectedGadget(w) = NULL;
}

void
_XmGadgetDrag(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget g = (Widget)_XmInputForGadget(w, ev->x, ev->y);

    XdbDebug(__FILE__, w, "Inside _XmGadgetDrag()\n");

    if (g)
	_XmDispatchGadgetInput((Widget)g,
			       event,
			       XmBDRAG_EVENT);
}

void
_XmGadgetSelect(Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XdbDebug2(__FILE__, w, g, "Inside _XmGadgetSelect()\n");

    if (g && XmIsGadget(g)) {
	XmGadgetClass gc = (XmGadgetClass)XtClass(g);

	if (gc->gadget_class.arm_and_activate)
	    (*gc->gadget_class.arm_and_activate)(g, event, params, num_params);
    }
}

void
_XmManagerParentActivate(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    XmParentProcessDataRec data;
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)w->core.widget_class;

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_ACTIVATE;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(w) && mwc->manager_class.parent_process)
    {
	if (!(*mwc->manager_class.parent_process)(w, &data))
	    /* propagate to parent? */;
    }
}

void
_XmGadgetTraversePrevTabGroup(Widget w,
			      XEvent *event,
			      String *params,
			      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_PREV_TAB_GROUP);
}

void
_XmGadgetTraverseNextTabGroup(Widget w,
			      XEvent *event,
			      String *params,
			      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_NEXT_TAB_GROUP);
}

void
_XmGadgetTraverseUp(Widget w,
		    XEvent *event,
		    String *params,
		    Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_UP);
}

void
_XmGadgetTraverseDown(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_DOWN);
}

void
_XmGadgetTraverseLeft(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_LEFT);
}

void
_XmGadgetTraverseRight(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XdbDebug(__FILE__, g, "Manager XmProcessTraversal(TRAVERSE_RIGHT)\n");
    XmProcessTraversal(g, XmTRAVERSE_RIGHT);
}

void
_XmGadgetTraverseHome(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_HOME);
}

void
_XmGadgetHelp(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);
    Widget cur;
    XmAnyCallbackStruct cbs;

    XdbDebug(__FILE__, w, "Inside _XmGadgetHelp()\n");

    if (g) {
	_XmDispatchGadgetInput(g,
			       event,
			       XmHELP_EVENT);
    }
    else {
	cur = w;

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
}

void
_XmGadgetKeyInput(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XdbDebug2(__FILE__, w, g, "Inside _XmGadgetKeyInput()\n");

    if (g)
	_XmDispatchGadgetInput(g,
			       event,
			       XmKEY_EVENT);
}

void 
_XmGadgetButtonMotion(Widget w, 
		      XEvent *event, 
		      String *params, 
		      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XdbDebug(__FILE__, w, "Inside _XmGadgetButtonMotion(), gadget %s\n",
	g ? XtName(g) : "(null)");

    if (g)
	_XmDispatchGadgetInput(g,
			       event,
			       XmMOTION_EVENT);
}

void 
_XmManagerParentCancel(Widget w, 
		XEvent *event, 
		String *params, 
		Cardinal *num_params)
{
    XmParentProcessDataRec data;
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)w->core.widget_class;

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_CANCEL;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(w) && mwc->manager_class.parent_process)
	(*mwc->manager_class.parent_process)(w, &data);
}

void 
_XmGadgetTraversePrev(Widget w, 
		      XEvent *event, 
		      String *params, 
		      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_PREV);
}


void 
_XmGadgetTraverseNext(Widget w, 
		      XEvent *event, 
		      String *params, 
		      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    XmProcessTraversal(g, XmTRAVERSE_NEXT);
}

/*
 * yeah, right.  Who came up with these names, anyway?
 */
void
_XmSocorro(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmAnyCallbackStruct cbs;

    XdbDebug(__FILE__, w, "Inside _XmSocorro()\n");

    cbs.reason = XmCR_HELP;
    cbs.event  = event;

    while (w != NULL) {
        if (XtHasCallbacks(w, XmNhelpCallback) == XtCallbackHasSome) {
	    XtCallCallbacks(w, XmNhelpCallback, (XtPointer)&cbs);
	    return;
	}
	w = XtParent(w);
    }
}

Boolean 
_XmParentProcess(Widget widget, 
		 XmParentProcessData data)
{
    /* we don't handle anything by default.  We leave that up to the other manager
       widgets, like bulletin board. Just pop up to our parent*/
    Widget p = XtParent(widget);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)p->core.widget_class;

    if (XmIsManager(p) && mwc->manager_class.parent_process)
	return ((*mwc->manager_class.parent_process)(p,data));

    return False;
}

static void
_XmManagerEventHandler(Widget w, 
		       XtPointer data, 
		       XEvent *event, 
		       Boolean *cont)
{
    XmGadget g = _XmInputForGadget(w, event->xmotion.x, event->xmotion.y);
    XmFocusChange foc;

    if (g == NULL)
	XdbDebug(__FILE__, w, "### _XmManagerEventHandler(gadget NULL)\n");
    else
	XdbDebug2(__FILE__, w, (Widget)g, "### _XmManagerEventHandler()\n");

    /* We've got to tie this into the focus policy.  If the focus mode is
     * explicit, we get focus in/out events.  If the focus mode is pointer,
     * we get enter/leave events.  Regardless, we get one or the other,
     * but not both.  You can see this in Motif/mwm, but not in LessTif/lmwm.
     * There's a lot of wacky stuff that Motif does in the BaseClass
     * extension procs.  This (at least for Gadgets) is one of them (the
     * FocusChange proc).
     * Started this, but currently it is a hack.  Focus in/out and Enter/Leave
     * look the same -- and they shouldn't.  The trouble I'm having is how
     * to distinguish the two cases.  It really needs to follow when the
     * Gadget gets the keyboard focus (er, well, really when the parent
     * gets the keyboard focus). MLM
     *
     * 960720 -- fixed this.  Now this event handler only does the work for
     * XmPOINTER.  XmEXPLICIT is handled via the focus mechanism in Traversal.c
     * MLM
     *
     * 961114 -- Sometimes I am slow.  POINTER or InDragMode, stupid.  MLM
     */
    foc = _XmGetFocusPolicy(w);

    if (g == NULL) {
	/* 
	 * we're now not in a gadget child, or the child isn't listening
	 * 
	 * if we weren't in a gadget before, do nothing.
	 * else
	 * if we were in a gadget before, send a leave event to the old gadget
	 */
	
	if (MGR_HighlightedWidget(w) != NULL) {
	    _XmDispatchGadgetInput(MGR_HighlightedWidget(w),
				   event,
				   XmLEAVE_EVENT);

	    if (foc == XmPOINTER || (XmIsRowColumn(w) && _XmGetInDragMode(w)))
		MGR_HighlightedWidget(w) = NULL;
	}
    }
    else {
	/* we are in a gadget child 
	 *
	 * if we weren't in a gadget before, send an enter event to the new gadget
         * else
         * if we were in a gadget before:
	 *    if it's the same gadget, send a motion event.
         *    else
         *    it it's not the same gadget, send a leave event to the old one and an enter
         *       event to the new one.
         */

	if (MGR_HighlightedWidget(w) == NULL) {
	    XdbDebug(__FILE__, w, "Sending ENTER event to gadget\n");

	    _XmDispatchGadgetInput((Widget)g,
				   event, 
				   XmENTER_EVENT);
	    if (foc == XmPOINTER || (XmIsRowColumn(w) && _XmGetInDragMode(w)))
		MGR_HighlightedWidget(w) = (Widget)g;
	}
	else {
	    /* the motion events are already propagated to the highlightedwidget */
	    if (MGR_HighlightedWidget(w) != (Widget)g) {

		XdbDebug(__FILE__, w, "Sending LEAVE event to old gadget\n");

		_XmDispatchGadgetInput(MGR_HighlightedWidget(w),
				       event,
				       XmLEAVE_EVENT);

		XdbDebug(__FILE__, w, "Sending ENTER event to new gadget\n");

		_XmDispatchGadgetInput((Widget) g,
				       event,
				       XmENTER_EVENT);
		if (foc == XmPOINTER ||
		    (XmIsRowColumn(w) && _XmGetInDragMode(w)))
		    MGR_HighlightedWidget(w) = (Widget)g;
	    }
	}
    }
}

void
_XmDestroyParentCallback (Widget w, XtPointer client, XtPointer call)
{
}

void
_XmClearShadowType(Widget w,
		   Dimension old_width, Dimension old_height,
		   Dimension old_shadow_thickness,
		   Dimension old_highlight_thickness)
{
    /* THIS IS A HACK.  FIXME
     * NOTE: If you do, make sure that testXm/filesb/test1 redisplays
     * properly after being resized.  Chances are it won't.
     */
    if (XtIsRealized(w))
	XClearWindow(XtDisplay(w), XtWindow(w));
}

/*
 * Accelerators and Mnemonics -  How they work.
 *
 * Accelerators are an issue to be dealt with on a per "window" basis -
 * if you take the user's perception of a window.
 * We collect the information on accelerators in the Menu Bar (a RowColumn
 * hence a subclass of Manager).
 *
 * Information on mnemonics is stored in the menu pane itself - also a
 * RowColumn.
 *
 * Both accelerators and mnemonics are eventually made to work with the
 *	ActivateTranslation(xx)
 * action, in which the parameter is the index of the translation in our table.
 */
static void
_XmManagerActivateTranslation(Widget w, XEvent *evp,
			      String *params, Cardinal *num_params)
{
    int     i;
    Widget	p;
    Boolean	ok;

    if (*num_params == 1)
	XdbDebug(__FILE__, w, "ActivateTranslation(%s)\n", params[0]);
    else {
	XdbDebug(__FILE__, w, "ActivateTranslation - no parameter\n");
	return;
    }

    i = atoi(params[0]);

    /* Find the manager widget responsible for all this */
    for (p=w, ok=False; XtParent(p); p = XtParent(p)) {
	if (XmIsManager(p) && MGR_NumKeyboardEntries(p)) {
	    ok = True;
	    break;
	}
    }

    if (!ok)
	return;

    if ( (i < 0) || (i >= MGR_SizeKeyboardList(p)) ||
         !MGR_KeyboardList(p)[i].component ) {
	XdbDebug(__FILE__, w,
		 "ActivateTranslation: didn't find suitable manager widget\n");
    }

    /* Call the action */
    /* If it's a mnemonic, go find the owner */
    if (MGR_KeyboardList(p)[i].isMnemonic) {
	XdbDebug2(__FILE__, MGR_KeyboardList(p)[i].component, w,
		  "Treat mnemonic\n");
	/*
	 * The "component" is the object that has the code -> trigger there.
	 */
	if (XtIsSubclass(MGR_KeyboardList(p)[i].component,
			 xmCascadeButtonWidgetClass) ||
	    XtIsSubclass(MGR_KeyboardList(p)[i].component,
			 xmCascadeButtonGadgetClass))
	{
	    XtCallActionProc(MGR_KeyboardList(p)[i].component,
			     "MenuBarSelect",
			     evp, NULL, 0);
	    return;
	}
	/* If it turned out not to be a cascade, hope it's a button
	 * and treat below */
    }

    /*
     * Find out if the object was in a menu; if it was then it needs to be
     * de-activated.
     *
     * Aren't mnemonics always in menus ? - The second law of OSF/Motif or so ;-)
     */
    if (1) {
	Widget	shell, mb, b = MGR_KeyboardList(p)[i].component;

#ifdef DO_KEYBOARD_GRABS
	XdbDebug(__FILE__, b, "XtUngrabKeyboard\n");
	XtUngrabKeyboard(b, CurrentTime);
#endif

	mb = b;
	while (mb && ! XmIsMenuBar(mb))
			mb = XtParent(mb);

#ifdef DO_GRABS
	if (mb) {
	    XdbDebug2(__FILE__, mb, b, "XtRemoveGrab(parent)\n");
	    XtRemoveGrab(mb);
	}
	else {
	    XdbDebug(__FILE__, b, "Help !! Can't find a menu bar !?\n");
	}
#endif

	for (shell=b; !XtIsShell(shell); shell = XtParent(shell))
	    ;

	XdbDebug(__FILE__, b, "Calling MenuShellPopdownDone\n");
		XtCallActionProc(shell, "MenuShellPopdownDone", evp, NULL, 0);

#ifdef	DO_GRABS
	XdbDebug(__FILE__, b, "XtUngrabPointer\n");
	XtUngrabPointer(b, CurrentTime);
#endif
    }

    if (XtIsWidget(MGR_KeyboardList(p)[i].component)) {
	XtCallActionProc(MGR_KeyboardList(p)[i].component,
			 "Activate",
			 evp, NULL, 0);
    }
    else if (XmIsGadget(MGR_KeyboardList(p)[i].component)) {
	XmGadget g = (XmGadget)MGR_KeyboardList(p)[i].component;

	((XmGadgetClassRec *)g->object.widget_class)->
		gadget_class.arm_and_activate((Widget)g, evp, NULL, NULL);
    }
    else {
	XdbDebug(__FILE__, MGR_KeyboardList(p)[i].component, "Not a widget/gadget\n");
    }
}

/*
 * Recursively apply translations to all child widgets
 */
static void
_XmManagerApplyTranslations(Widget w, XtTranslations t)
{
    if (w) {
	XdbDebug(__FILE__, w, "Overriding translations\n");
	XtOverrideTranslations(w, t);
    }

    if (XtIsSubclass(w, compositeWidgetClass)) {
	int	i;

	for (i=0; i<MGR_NumChildren(w); i++) {
	    if (XtIsWidget(MGR_Children(w)[i])) {
		_XmManagerApplyTranslations(MGR_Children(w)[i], t);
	    }
	}
    }
}

/*
 * Fix stuff that doesn't regularly fit into the
 * translation table syntax.
 *
 * --aldi: This had a bad memory leak whenever the accelerator string
 * contained a colon. The correction now is that we're remembering the
 * pointer to the malloc'ed new buffer containing "colon" instead of ":".
 * Also triple colons won't kill us anymore (security meniac).
 *
 * Someday the pthreads are surely going to kill us. I'm really sure.
 */
static char *
FixAccelerator(char *s)
{
    char        *p, *q;
    int         len;
    static char *r = NULL;
    static int   rLen = 0;
    Boolean      SkipColons;

    for (p=s; *p && *p != ':'; p++)
	;
    if (*p == '\0')
	return s;       /* no colon found */

    /*
     * NO, I didn't forget to count the trailing zero in the next code line.
     * Instead of giving a long explanation, here's an example:
     *  ":" needs 1 (+1) bytes of storage
     *  "colon" needs 5+1 bytes = strlen(":")+5 additional bytes of storage
     *                          = strlen(":")+(strlen("colon")-1)+1
     *                                                            ^
     *                                                            +- trailing zero
     */
    len = strlen(s) + 5;
    if ( len > rLen ) {
        rLen = len;
	r = XtRealloc(r, rLen);
    }

    SkipColons = False;
    for (p=s, q=r; *p; ) {
	if (*p != ':')
	    *(q++) = *(p++);
	else {
	    if ( !SkipColons ) {
	        *(q++) = 'c';
		*(q++) = 'o';
		*(q++) = 'l';
		*(q++) = 'o';
		*(q++) = 'n';
		/* Work around a xmgr bug and double check all... */
		SkipColons = True;
	    }
	    p++;
        }
    }

    *q = '\0';
    return r;
}

static char *
Keysym2String(KeySym k)
{
    char    *r;

    if (k == XK_colon)
	r =  "colon";
    else
	r = XKeysymToString(k);

    return r;
}

/*
 * This routine does much of the hard work :
 *      - assemble all the translations for all accelerators
 *        They all call the same Action with their index in the
 *        table as argument.
 *              Ctrl<Key>c: ActivateTranslation(2)\n\
 *      - parse the thing
 *      - install on the manager widget
 */
static void
_XmManagerInstallAccelerators(Widget w)
{
    int	i;
    char line[256], *tr;
    XtTranslations xtt;

    XdbDebug(__FILE__, w, "_XmManagerInstallAccelerators()\n");

    if (MGR_KeyboardList(w) == NULL || MGR_NumKeyboardEntries(w) == 0)
	return;

#if 0
/* This is not right ! */
    if (!XmIsRowColumn(w))
	return;
    if (RC_Type(w) != XmMENU_BAR)
	return;
#endif

    tr = XtMalloc(MGR_NumKeyboardEntries(w) * 256);
    tr[0] = '\0';

    for (i=0; i<MGR_NumKeyboardEntries(w); i++) {
	if (MGR_KeyboardList(w)[i].component) {
	    if (MGR_KeyboardList(w)[i].isMnemonic) {
		Widget	rc = XtParent(MGR_KeyboardList(w)[i].component);

		if (XmIsRowColumn(rc) && RC_Type(rc) == XmMENU_BAR) {
		    /* Mnemonic on the menu bar : Alt<Key>xx where xx is the
		     * converted keysym */
		    sprintf(line, "Alt<Key>%s: ActivateTranslation(%d)\n",
			    Keysym2String(MGR_KeyboardList(w)[i].keysym), i);
			strcat(tr, line);
		}
		else if (XmIsRowColumn(rc) && RC_Type(rc) == XmMENU_PULLDOWN) {
		    /* Mnemonic in menus : <Key>xx */
		    sprintf(line, "<Key>%s: ActivateTranslation(%d)\n",
			    Keysym2String(MGR_KeyboardList(w)[i].keysym), i);
		    strcat(tr, line);
		}
	    }
	    else { /* Accelerator */
		if (!strstr(MGR_KeyboardList(w)[i].accelerator, "<Key>"))
		    sprintf(line, "<Key>%s: ActivateTranslation(%d)\n",
			    FixAccelerator(MGR_KeyboardList(w)[i].accelerator), i);
		else
		    sprintf(line, "%s: ActivateTranslation(%d)\n",
			    FixAccelerator(MGR_KeyboardList(w)[i].accelerator), i);
		strcat(tr, line);
	    }
	}
    }

    XdbDebug(__FILE__, w, "_XmManagerInstallAccelerators(%s)\n", tr);

    xtt = XtParseTranslationTable(tr);
    _XmManagerApplyTranslations(w, xtt);

    XtFree(tr);
    /* Apparently no need to free xtt */
}

void
_XmManagerInstallAccelerator(Widget m, Widget w, String s)
{
    int i;
    Widget mw, sh;

    XdbDebug2(__FILE__, m, w, "_XmManagerInstallAccelerator(%s)\n", s);

    /* Do we really have an accelerator ? */
    if (s == NULL || strlen(s) == 0)
	return;

    /* Check whether we're in a menu system */
    mw = XtParent(w);
    if (!XmIsRowColumn(mw))
	return;
    if (! (RC_Type(mw) == XmMENU_PULLDOWN || RC_Type(mw) == XmMENU_OPTION))
	return;

    /* Find the manager widget just underneath the shell */
    for (mw = m; XtParent(mw); mw = sh) {
	sh = XtParent(mw);

	if (XtIsSubclass(sh, applicationShellWidgetClass) ||
	    XtIsSubclass(sh, xmDialogShellWidgetClass) ||
	    XtIsSubclass(sh, topLevelShellWidgetClass))
	    break;
    }

    XdbDebug(__FILE__, w,
	     "_XmManagerInstallAccelerator found manager %s\n", XtName(mw));

    /* Register with the manager widget */
    if (MGR_NumKeyboardEntries(mw) == 0 ||
	(MGR_NumKeyboardEntries(mw) == MGR_SizeKeyboardList(mw))) {
	i = MGR_SizeKeyboardList(mw);
	MGR_SizeKeyboardList(mw)++;

	MGR_KeyboardList(mw) =
	    (XmKeyboardData *)XtRealloc((XtPointer)MGR_KeyboardList(mw),
					sizeof(XmKeyboardData) *
					    MGR_SizeKeyboardList(mw));
	MGR_KeyboardList(mw)[i].component = NULL;
    }
    else {
	for (i=0; i<MGR_SizeKeyboardList(mw); i++)
	    if (MGR_KeyboardList(mw)[i].component == NULL)
		break;
    }

    if (MGR_KeyboardList(mw)[i].component != NULL) {
	_XmError(w, "_XmManagerInstallAccelerator: This should not happen\n");
    }

    /* Put the data in the entry */
    MGR_KeyboardList(mw)[i].component = w;
    MGR_KeyboardList(mw)[i].eventType = KeyPress;
    /* FIX ME: XtNewString probably solves nothing */
    MGR_KeyboardList(mw)[i].accelerator = XtNewString(s);
    MGR_KeyboardList(mw)[i].isMnemonic = False;
    MGR_KeyboardList(mw)[i].keysym = 0;		/* FIX ME */
    MGR_NumKeyboardEntries(mw)++;
#if 0
    /* Not implemented yet ... FIX ME !!! */
    MGR_KeyboardList(mw)[i].key = b;
    MGR_KeyboardList(mw)[i].modifiers = b;
    MGR_KeyboardList(mw)[i].needGrab = b;
#endif

    /* Make it work */
    if (XtIsRealized(mw))
	_XmManagerInstallAccelerators(mw);
}

void
_XmManagerUninstallAccelerator(Widget m, Widget w)
{
    int i;
    Widget mw, sh;

    /* Find the manager widget just underneath the shell */
    for (mw = m; XtParent(mw); mw = sh) {
	sh = XtParent(mw);
	if (XtIsSubclass(sh, applicationShellWidgetClass) ||
	    XtIsSubclass(sh, topLevelShellWidgetClass))
	    break;
    }

    if (!XmIsManager(mw))
	return;

    for (i=0; i<MGR_SizeKeyboardList(mw); i++) {
	if (MGR_KeyboardList(mw) &&
	    w == MGR_KeyboardList(mw)[i].component) {
	    MGR_KeyboardList(mw)[i].component = NULL;
	    MGR_NumKeyboardEntries(mw)--;
	    break;
	}
    }
#ifdef	notdef
    /* This crashes phone */
    if (XtIsRealized(mw))
	_XmManagerInstallAccelerators(mw);
#endif
}

void
_XmManagerInstallMnemonic(Widget m, Widget w, KeySym mn)
{
    int i;

    if (mn == 0 || mn == NoSymbol)
	return;

    XdbDebug2(__FILE__, m, w, "_XmManagerInstallMnemonic(%c)\n", mn);

    /* Check whether we're in a menu system */
    if (! XmIsRowColumn(m))
	return;
    if (RC_Type(m) != XmMENU_PULLDOWN && RC_Type(m) != XmMENU_BAR &&
	RC_Type(m) != XmMENU_OPTION)
	return;

    /* If we're dealing with the menu bar, store the stuff in the highest
     * manager widget that you can find (with all the accelerators) */
    if (RC_Type(m) == XmMENU_BAR) {
	Widget	mw, sh;

	for (mw = m; XtParent(mw); mw = sh) {
	    sh = XtParent(mw);
	    if (XtIsSubclass(sh, applicationShellWidgetClass) ||
		XtIsSubclass(sh, xmDialogShellWidgetClass) ||
		XtIsSubclass(sh, topLevelShellWidgetClass))
		break;
	}

	XdbDebug2(__FILE__, mw, w, "Store mnemonic info\n");
	m = mw;
    }

    /* Register with the manager widget */
    if (MGR_NumKeyboardEntries(m) == 0 ||
	(MGR_NumKeyboardEntries(m) == MGR_SizeKeyboardList(m))) {
	i = MGR_SizeKeyboardList(m);

	MGR_SizeKeyboardList(m)++;

	MGR_KeyboardList(m) =
	    (XmKeyboardData *)XtRealloc((XtPointer)MGR_KeyboardList(m),
					sizeof(XmKeyboardData) *
					    MGR_SizeKeyboardList(m));
	MGR_KeyboardList(m)[i].component = NULL;
    }
    else {
	for (i=0; i<MGR_SizeKeyboardList(m); i++) {
	    if (MGR_KeyboardList(m)[i].component == NULL)
		break;
	}
    }
    if (MGR_KeyboardList(m)[i].component != NULL) {
	_XmError(w, "_XmManagerInstallMnemonic: This should not happen %s %d\n",
		 __FILE__, __LINE__);
    }

    /* Put the data in the entry */
    MGR_KeyboardList(m)[i].component = w;
    MGR_KeyboardList(m)[i].eventType = KeyPress;
    MGR_KeyboardList(m)[i].accelerator = NULL;
    MGR_KeyboardList(m)[i].keysym = mn;
    MGR_KeyboardList(m)[i].isMnemonic = True;
#if 0
    /* Not implemented yet ... FIX ME !!! */
    MGR_KeyboardList(m)[i].key = b;
    MGR_KeyboardList(m)[i].modifiers = b;
    MGR_KeyboardList(m)[i].needGrab = b;
#endif
    MGR_NumKeyboardEntries(m)++;

    /* Make it work */
    /* FIX ME */
    if (RC_Type(m) == XmMENU_BAR) {
	/* Two cases :
	 * If one of the cascades in the menu bar has been activated, then
	 *	the mnemonic's key by itself should trigger the corresponding
	 *	cascade button.
	 * If the menu bar is not selected, then Alt+the mnemonic should work.
	 *
	 * Only the latter case is treated here - the other is treated by ???
	 * when the menu is activated, either through the mouse or the keyboard.
	 */
    }
    else if (RC_Type(m) == XmMENU_PULLDOWN) {
	/* If the menu is up and has the focus, then the mnemonic's key
	 * by itself should trigger the widget */
    }
    else if (RC_Type(m) == XmMENU_OPTION) {
	/* FIX ME -- don't know what this is supposed to do */
    }
}

void
_XmManagerUninstallMnemonic(Widget m, Widget w)
{
    return;

    if (m && w)
	XdbDebug2(__FILE__, m, w,
		  "_XmManagerUninstallMnemonic() is not implemented\n");
}

static XmNavigability
widget_navigable(Widget w)
{   
    if (XtSensitive(w) && MGR_TraversalOn(w))
    {
	if ((MGR_NavigationType(w) == XmSTICKY_TAB_GROUP ||
	     MGR_NavigationType(w) == XmEXCLUSIVE_TAB_GROUP ||
	     MGR_NavigationType(w) == XmTAB_GROUP) && !_XmShellIsExclusive(w))
	{
	    XdbDebug(__FILE__, w,
		     "WidgetNavigable => XmDESCENDANTS_TAB_NAVIGABLE\n");
	    return XmDESCENDANTS_TAB_NAVIGABLE;
	}
	XdbDebug(__FILE__, w, "WidgetNavigable => XmDESCENDANTS_NAVIGABLE\n");
	return XmDESCENDANTS_NAVIGABLE;
    }
    XdbDebug(__FILE__, w, "WidgetNavigable => XmNOT_NAVIGABLE\n");
    return XmNOT_NAVIGABLE;
}

