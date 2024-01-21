/**
 *
 * $Id: PanedW.c,v 1.20 1997/01/08 03:35:59 miers Exp $
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

static char rcsid[] = "$Id: PanedW.c,v 1.20 1997/01/08 03:35:59 miers Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/PanedWP.h>
#include <Xm/SashP.h>
#include <Xm/SeparatorP.h>
#include <X11/Xfuncs.h>
#include <stdlib.h>
#include <stdio.h>

#include <XmI/DebugUtil.h>

/*
 * INSTANCE VARIABLES
 *-------------------
 * Documented resources (public instance variables):
 *  XmNrefigureMode           (PW_RefigureMode)
 *    determines whether the panes' positions are recomputed and repositioned
 *    when programmatic changes are made to the PanedWindow.  Default (True)
 *    sets the pane children to their appropriate positions.
 *  XmNseparatorOn            (PW_SeparatorOn)
 *    determines whether a Separator is created and managed between multiple
 *    panes.  Default (True) is to create the separator.
 *  XmNmarginHeight           (PW_MarginHeight)
 *    distance between top and bottom edges of the PanedWindow and its
 *    children.  Default is 3.
 *  XmNmarginWidth            (PW_MarginWidth)
 *    distance between left and right edges of the PanedWindow and its
 *    children.  Default is 3.
 *  XmNspacing                (PW_Spacing)
 *    specifies the distance between panes.  Default is 8.
 *  XmNsashWidth              (PW_SashWidth)
 *    specifies the width of Sash children.  Default is 10.
 *  XmNsashHeight             (PW_SashHeight)
 *    specifies the height of Sash children.  Default is 10.
 *  XmNsashShadowThickness    (PW_SashShadowThickness)
 *    defined as (variable).  On color displays, the default is 2(?).
 *  XmNsashIndent             (PW_SashIndent)
 *    specifies the placement of Sash children from the left or right margins.
 *    Positive values are offset values from the left margin; negative values
 *    indicate that the sash should be offset from the right margin by the
 *    absolute value of the sash indent.  Default value is -10.
 *
 * Private Instance variables and what they do:
 *  PW_StartY
 *    mouse starting y position when moving a sash.
 *  PW_IncrementCount
 *    sash increment count (?)
 *  PW_PaneCount
 *    number of *managed* panes
 *  PW_NumSlots
 *    total number of slots in managed_children array
 *  PW_NumManagedChildren
 *    number of slots in use in managed_children array
 *  PW_ManagedChildren
 *    misleading.  This array is regenerated whenever change_managed() is
 *    invoked with a pane changing managed state; this leads to peculiar
 *    contents, sometimes.
 *  PW_RecursivelyCalled
 *    protection in change_managed() against children that aren't panes
 *    (sashes and separators)
 *  PW_ResizeAtRealize
 *    to be discovered.
 *  PW_TopPane
 *  PW_BottomPane
 *  PW_FlipGC
 *
 ***************************************************************************
 *
 * CONSTRAINT INSTANCE VARIABLES
 *------------------------------
 * Documented resources (public constraint instance variables)
 *  XmNpaneMinimum            (PWC_Min)
 *    minimum size of pane.  default is 1; can't be less than that.
 *  XmNpaneMaximum            (PWC_Max)
 *    maximum size of pane.  default is 1000; can't be less than paneMinimum.
 *  XmNallowResize            (PWC_AllowResize)
 *    allows an application to control whether a pane can request to be
 *    resized.  It is not effective until the PanedWindow and all pane
 *    children have been realized.  If false; pane geometry requests are
 *    denied (default); else pane geometry requests are honored.
 *  XmNskipAdjust             (PWC_SkipAdjust)
 *    PanedWindow should not (default:False) or should (True) automatically
 *    resize the pane.
 *  XmNpositionIndex          (PWC_PositionIndex)
 *    computed position in MGR_Children list.
 * NOTE: the above constraint resources are only relevant to panes, not to Sash
 *    or Separators.
 *
 * Private constraint instance varibles:
 *  PWC_Position
 *    pane position in paned window.  Recalculated when panes are managed
 *    or unmanaged.
 *  PWC_DHeight
 *    desired height of pane
 *  PWC_DY
 *    desired Y location of pane
 *  PWC_OldDY
 *    last computed value of Y location
 *  PWC_IsPane
 *    if the child that has these constraints is in fact a pane, this variable
 *    is true.  else false.
 *  PWC_Sash
 *    sash widget associated with a pane, if child is a pane.  If child is
 *    a sash, this is a back-pointer to the associated pane child.
 *  PWC_Separator
 *    separator widget associated with a pane, if the child is a pane.  If the
 *    child is a separator, this is a back pointer to the associated pane.
 * #ifdef ORIENTED_PANEDW
 *  PWC_Orientation   -- not used in 1.2
 * #endif
 *
 * Rules:
 *
 * Testing with Motif indicates the following:
 *  o every pane has a sash and a separator.  They aren't necessarily managed,
 *    but they do get created. (FIXED)
 *  o PW_NumManagedChildren:
 *     if no panes have been unmanaged, then
 *       PW_NumManagedChildren = PW_PaneCount + ((PW_PaneCount - 1) * 2);
 *     (i.e., PW_PaneCount = 4, PW_NumManagedChildren = 10;
 *            PW_PaneCount = 3, PW_NumManagedChildren = 7;
 *            PW_PaneCount = 2, PW_NumManagedChildren = 4;
 *            PW_PaneCount = 1, PW_NumManagedChildren = 1;
 *     if panes have been unmanaged, then
 *       PW_NumManagedChildren = PW_PaneCount + ((PW_PaneCount) * 2);
 *     (i.e., PW_PaneCount = 4, PW_NumManagedChildren = 12, PW_PaneCount was 5;
 *            PW_PaneCount = 3, PW_NumManagedChildren = 9, PW_PaneCount was 4;
 *            PW_PaneCount = 2, PW_NumManagedChildren = 6, PW_PaneCount was 3;
 *            PW_PaneCount = 1, PW_NumManagedChildren = 3, PW_PaneCount was 2;
 *     so obviously the sashes and separators aren't paid attention to if the
 *     PaneCount doesn't indicate they should be.
 *     NOTE: need to check whether or not they are *really* managed.
 *     UPDATE: We just bypass this, and never have anything but true panes
 *     in our managed_children list.
 *  o Contrary to the current implementation, the sashes and separators aren't
 *    passed around, but always stay associated with the pane they're created
 *    with. (FIXED).
 *  o The insertion_position routine returns PW_PaneCount if the child created
 *    isn't a pane (meaning that the oldest panes have the highest numbered
 *    sashes and seps -- they meet in the middle at PW_PaneCount). (FIXED)
 *  o The PWC_Sash() component of a sash child is a back pointer to the pane.
 *    (FIXED).
 *  o The PWC_Separator() component of a separator child is a back pointer to
 *    the pane. (FIXED: This helps in the SashAction routine with performance)
 *  o PWC_Position and PWC_PositionIndex: the PWC_PositionIndex is what the
 *    pane wants.  PWC_Position represents the reality of pane ordering in
 *    the list of PW_PaneCount managed panes.
 *
 * Sash Movement:
 *  o Given a middle pane with PaneMinimum = 10 and PaneMaximum = 20
 *    -- if you move the sash above this pane up, movement will cease when
 *       the pane reaches PaneMaximum in height, or until the pane above
 *       reaches PaneMinimum, whichever is first.
 *    -- if you move the sash above this pane down, and the pane below is
 *       not pane minimum size, *both* separators will animate, and both
 *       panes will be resized (at least until PaneMinimum is reached).
 *    -- the opposite is also True.  That is, if you move the sash below
 *       this pane down, movement will cease when the pane reaches PaneMaximum
 *       in height.  If you move the sash below this pane up, and the Pane
 *       below is not maximum size, *both separators will animate, and both
 *       panes will be resized approaching PaneMinimum.  Movement will stop
 *       if the pane below reaches PaneMaximum 
 */
/*
 * Forward Declarations
 */
/* core */
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request,
		       Widget new_w,
		       ArgList args,
		       Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current,
		 	  Widget request,
			  Widget new_w,
			  ArgList args,
			  Cardinal *num_args);
static void resize(Widget w);
static void expose(Widget w,
		   XEvent *event,
		   Region region);
static void realize(Widget w,
		    XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *desired);

/* composite */
static void insert_child(Widget w);
static void delete_child(Widget w);
static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void change_managed(Widget w);

/* constraint */
static void constraint_initialize(Widget request,
				  Widget new_w,	
				  ArgList args,
				  Cardinal *num_args);
static Boolean constraint_set_values(Widget current,
				     Widget request,	
				     Widget new_w,
				     ArgList args,
				     Cardinal *num_args);
Cardinal panedw_insert_position(Widget w);

/* helper */
static void  _XmPanedWLayout(Widget w, Boolean ParentResize,
			     Widget pane, Boolean TestMode,
			     XtWidgetGeometry *childgeom);

/* manager */
static Boolean traversal_children(Widget mw, Widget **children, Cardinal *num_children);

/*
 * Resources for the PanedWindow class
 */
#define Offset(field) XtOffsetOf(XmPanedWindowRec, paned_window.field)

static XtResource resources[] = {
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNspacing, XmCSpacing, XmRVerticalDimension,
	sizeof(Dimension), Offset(spacing),
	XmRImmediate, (XtPointer)8
    },
    {
	XmNrefigureMode, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(refiguremode),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNseparatorOn, XmCSeparatorOn, XmRBoolean,
	sizeof(Boolean), Offset(separator_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNsashIndent, XmCSashIndent, XmRHorizontalPosition,
	sizeof(Position), Offset(sash_indent),
	XmRImmediate, (XtPointer)-10
    },
    {
	XmNsashWidth, XmCSashWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(sash_width),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNsashHeight, XmCSashHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(sash_height),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNsashShadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(sash_shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNinsertPosition, XmCInsertPosition, XmRFunction,
	sizeof(XtOrderProc), XtOffsetOf(XmPanedWindowRec, composite.insert_position),
	XmRImmediate, (XtPointer)panedw_insert_position
    }
};

static XmSyntheticResource syn_resources[] = {
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
	XmNspacing,
	sizeof(Dimension), Offset(spacing),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNsashIndent,
	sizeof(Position), Offset(sash_indent),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNsashWidth,
	sizeof(Dimension), Offset(sash_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNsashHeight,
	sizeof(Dimension), Offset(sash_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNsashShadowThickness,
	sizeof(Dimension), Offset(sash_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};
#undef Offset

#define Offset(field) XtOffsetOf(XmPanedWindowConstraintRec, panedw.field)
static XtResource panedWindowConstraintResources[] = {
    {
	XmNallowResize, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(allow_resize),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpaneMinimum, XmCPaneMinimum, XmRVerticalDimension,
	sizeof(Dimension), Offset(min),
	XmRImmediate, (XtPointer)1
    }, 
    {
	XmNpaneMaximum, XmCPaneMaximum, XmRVerticalDimension,
	sizeof(Dimension), Offset(max),
	XmRImmediate, (XtPointer)1000
    },
    {
	XmNskipAdjust, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(skip_adjust),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpositionIndex, XmCPositionIndex, XmRShort,
	sizeof(short), Offset(position_index),
	XmRImmediate, (XtPointer)XmLAST_POSITION
    }
};

static XmSyntheticResource constraint_syn_resources[] = {
    {
	XmNpaneMinimum,
	sizeof(Dimension), Offset(min),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNpaneMaximum,
	sizeof(Dimension), Offset(max),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
};

static void SashAction(Widget w, XtPointer client_data, XtPointer call_data);

static CompositeClassExtensionRec panedWCompositeExt = {
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmBaseClassExtRec _XmPanedWCoreClassExtRec = {
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

static XmManagerClassExtRec _XmPanedWMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ traversal_children
};

XmPanedWindowClassRec xmPanedWindowClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmPanedWindow",
	/* widget_size           */ sizeof(XmPanedWindowRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
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
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmPanedWCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
        /* extension        */ &panedWCompositeExt,
    },
    /* Constraint class part */
    {
	/* subresources      */ panedWindowConstraintResources,  
        /* subresource_count */ XtNumber(panedWindowConstraintResources),
        /* constraint_size   */ sizeof(XmPanedWindowConstraintRec),
        /* initialize        */ constraint_initialize,
        /* destroy           */ NULL,  /* FIX ME */
        /* set_values        */ constraint_set_values,
        /* extension         */ NULL,   /* FIX ME */
    },
    /* XmManager class part */
    {
	/* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ constraint_syn_resources,
        /* num_syn_constraint_resources */ XtNumber(constraint_syn_resources),
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmPanedWMClassExtRec
    },
    /* XmPanedWindow part */
    {
	/* extension */ NULL,
    },
};

WidgetClass xmPanedWindowWidgetClass = (WidgetClass)&xmPanedWindowClassRec;

/*
 * handy defines
 */
#define _XmMin(a,b)  ((a) < (b)) ? (a) : (b)
#define _XmMax(a,b)  ((a) < (b)) ? (b) : (a)

/*
 * default number of slots.  8 seems to be a reasonable number.
 */
#define DEFAULT_NUM_SLOTS	8

static void
ValidatePaneMin(Widget w)
{
    if (PWC_PaneMinimum(w) < 1) {
	XtWarning("PanedWindow: XmNpaneMinimum must be greater than 0.");
	PWC_PaneMinimum(w) = 1;
    }
    if (PWC_PaneMaximum(w) < PWC_PaneMinimum(w)) {
	_XmWarning(XtParent(w),
		   "XmNpaneMinimum must be less than XmNpaneMaximum.");
	PWC_PaneMaximum(w) = PWC_PaneMinimum(w) + 1;
    }
    if (XtHeight(w) < PWC_PaneMinimum(w))
	XtHeight(w) = PWC_PaneMinimum(w);
}

static void
ValidatePaneMax(Widget w)
{
    if(PWC_PaneMaximum(w) < PWC_PaneMinimum(w)) {
	_XmWarning(XtParent(w),
		   "XmNpaneMaximum must be greater than XmNpaneMinimum.");
	PWC_PaneMinimum(w) = PWC_PaneMaximum(w) - 1;
    }
    
    if(XtHeight(w) > PWC_PaneMaximum(w))
	XtHeight(w) = PWC_PaneMaximum(w);
}

static Boolean
AllPanesRealized(Widget w)
{
    int i;

    if (!XtIsRealized(w))
	return False;

    for (i = 0; i < PW_PaneCount(w); i++) {
	if (!XtIsRealized(PW_ManagedChildren(w)[i]))
	    return False;
    }

    return True;
}


static void 
class_initialize()
{
    _XmPanedWCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    XmPanedWindowWidgetClass pclass = (XmPanedWindowWidgetClass)widget_class;

    extptr = (CompositeClassExtension*)_XmGetClassExtensionPtr((XmGenericClassExt*)&(pclass->composite_class.extension),
							       NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension) XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = pclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    pclass->composite_class.extension = (XtPointer) ext;
	}
    }
    _XmFastSubclassInit(widget_class, XmPANED_WINDOW_BIT);
}

static void
CreateFlipGC(Widget w)
{
    XtGCMask careabout;
    XGCValues gcv;

    careabout = GCForeground | GCLineWidth | GCSubwindowMode | GCFunction;
    gcv.function = GXxor;
    gcv.line_width = 0;
    gcv.foreground = (1UL << DefaultDepthOfScreen(XtScreen(w))) - 1;
    gcv.subwindow_mode = IncludeInferiors;

    PW_FlipGC(w) = XtGetGC(w, careabout, &gcv);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    PW_StartY(new_w) = 0;
    PW_PaneCount(new_w) = 0;
    PW_NumSlots(new_w) = DEFAULT_NUM_SLOTS;
    PW_NumManagedChildren(new_w) = 0;
    PW_RecursivelyCalled(new_w) = False;
    PW_ResizeAtRealize(new_w) = True;
    PW_TopPane(new_w) = NULL;
    PW_BottomPane(new_w) = NULL;

    CreateFlipGC(new_w);

    PW_ManagedChildren(new_w) = (WidgetList)XtMalloc(sizeof(Widget) *
							PW_NumSlots(new_w));

    _XmPanedWLayout(new_w, True, NULL, False, NULL);
}

static void 
constraint_initialize(Widget request, 
		      Widget new_w, 
		      ArgList args, 
		      Cardinal *num_args)
{
    XdbDebug(__FILE__, new_w, "PanedWindow constraint initialize\n");

    ValidatePaneMin(new_w);
    ValidatePaneMax(new_w);
    PWC_Position(new_w) = 0;
    PWC_DHeight(new_w) = 0;
    PWC_DY(new_w) = 0;
    PWC_OldDY(new_w) = 0;
    PWC_IsPane(new_w) = False;
    PWC_Sash(new_w) = NULL;
    PWC_Separator(new_w) = NULL;
}


static void
destroy(Widget w)
{
    XtReleaseGC(w, PW_FlipGC(w));
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    int i, argc;
    Boolean redisplay = False;
    Arg argl[3];
    
    if (PW_SeparatorOn(new_w) != PW_SeparatorOn(old)) {
	for(i = 1; i < PW_PaneCount(new_w); i++) {
	    if (PW_SeparatorOn(new_w))
		XtManageChild(PWC_Separator(PW_ManagedChildren(new_w)[i]));
	    else
		XtUnmanageChild(PWC_Separator(PW_ManagedChildren(new_w)[i]));
	}
    }

    if (PW_MarginHeight(new_w) != PW_MarginHeight(old) 
       || PW_MarginWidth(new_w) != PW_MarginWidth(old)
       || PW_Spacing(new_w) != PW_Spacing(old)
       || PW_SashIndent(new_w) != PW_SashIndent(old)) {
	    redisplay = True;
    }

    if (PW_SashWidth(new_w) != PW_SashWidth(old) ||
	PW_SashHeight(new_w) != PW_SashHeight(old) ||
	PW_SashShadowThickness(new_w) != PW_SashShadowThickness(old)) {

	argc = 0;
	if (PW_SashWidth(new_w) != PW_SashWidth(old))
	    XtSetArg(argl[argc], XmNwidth, PW_SashWidth(new_w)); argc++;
	if (PW_SashHeight(new_w) != PW_SashHeight(old))
	    XtSetArg(argl[argc], XmNheight, PW_SashHeight(new_w)); argc++;
	if (PW_SashShadowThickness(new_w) != PW_SashShadowThickness(old))
	    XtSetArg(argl[argc],
		     XmNshadowThickness,
		     PW_SashShadowThickness(new_w)); argc++;

	for(i = 0; i < MGR_NumChildren(new_w); i++) {

	    if (PWC_IsPane(MGR_Children(new_w)[i]))
		XtSetValues(PWC_Sash(MGR_Children(new_w)[i]), argl, argc);
	    else
		break;
	}

	redisplay = True;
    }

    if (redisplay == True && PW_RefigureMode(new_w))
	_XmPanedWLayout(new_w, True, NULL, False, NULL);
    
    return redisplay;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    int i;

    /*
     * make sure panes are lower than sashes and separators 
     * a bit kludgy.  MLM -- it should be ok to do this only on exposes.
     */
    if (XtIsRealized(w)) {
	for(i = 0; i < PW_PaneCount(w); i++) {
 	    XLowerWindow(XtDisplay(w), XtWindow(PW_ManagedChildren(w)[i]));
	}
	for(; i < MGR_NumChildren(w); i++) {
	    if (XtIsManaged(MGR_Children(w)[i]) && XmIsSash(MGR_Children(w)[i]))
 		XRaiseWindow(XtDisplay(w), XtWindow(MGR_Children(w)[i]));
	}
    }

    _XmRedisplayGadgets(w, event, region);
}

static Boolean 
constraint_set_values(Widget current, 
		      Widget request, 
		      Widget new_w, 
		      ArgList args, 
		      Cardinal *num_args)
{
    XmPanedWindowWidget pw = (XmPanedWindowWidget) XtParent(current);
    Boolean redisplay = False;
    int i;
    
    if (PWC_PaneMinimum(current) != PWC_PaneMinimum(new_w)) {
	redisplay = True;
	ValidatePaneMin(new_w);
    }

    if (PWC_PaneMaximum(current) != PWC_PaneMaximum(new_w)) {
	redisplay = True;
	ValidatePaneMax(new_w);
    }
 
    /*
     * NOTE: FIXME
     * This recomputation needs to think about whether the position index
     * is above the range of panes; I need to check what Motif does here if
     * somebody sets the position index to something wacky.
     * MLM - Dec10'96
     */
    if (PWC_PositionIndex(current) > PWC_PositionIndex(new_w)) {

	for (i = PWC_PositionIndex(current); i > PWC_PositionIndex(new_w); i--) {
	    MGR_Children(pw)[i] = MGR_Children(pw)[i-1];
	    PWC_PositionIndex(MGR_Children(pw)[i]) = i;
	}

	MGR_Children(pw)[i] = new_w;

	redisplay = True;
    }
    else if (PWC_PositionIndex(current) < PWC_PositionIndex(new_w)) {

	for(i = PWC_PositionIndex(current); i < PWC_PositionIndex(new_w); i++) {
	    MGR_Children(pw)[i] = MGR_Children(pw)[i+1];
	    PWC_PositionIndex(MGR_Children(pw)[i]) = i;
	}

        MGR_Children(pw)[i] = new_w;

	redisplay = True;
    }

    if (redisplay && PW_RefigureMode(XtParent(new_w)))
	_XmPanedWLayout(XtParent(new_w), True, NULL, False, NULL);

    return redisplay;
}

static void
resize(Widget w)
{
    XdbDebug(__FILE__, w, "PanedWindow Resize (%d %d)\n", XtWidth(w), XtHeight(w));

    _XmPanedWLayout(w, False, NULL, False, NULL);
}

static void
realize(Widget w, 
	XtValueMask *value_mask, 
	XSetWindowAttributes *attributes)
{
#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass

    _XmPanedWLayout(w, True, NULL, False, NULL);

    XdbDebug(__FILE__, w, "PanedWindow Realize\n");
}


/*
 * Insert panes before sashes and separators.
 */ 
Cardinal 
panedw_insert_position(Widget w)
{
    XmPanedWindowWidget pw = (XmPanedWindowWidget) XtParent(w);
    int cnt;

    for (cnt = 0; cnt < MGR_NumChildren(pw); cnt++) {
	if (!PWC_IsPane(MGR_Children(pw)[cnt]))
	    break;
    }

    if (PWC_PositionIndex(w) == XmLAST_POSITION || PWC_PositionIndex(w) < 0 ||
	PWC_PositionIndex(w) > cnt)
	    return cnt;

    return PWC_PositionIndex(w);
}

static void 
insert_child(Widget w)
{
    XmPanedWindowWidget pw = (XmPanedWindowWidget) XtParent(w);
    int i;

    if (PW_RecursivelyCalled(pw)) {

	PWC_IsPane(w) = False;
	PWC_Sash(w) = NULL;
	PWC_Separator(w) = NULL;

#define superclass (&xmManagerClassRec)
	(*superclass->composite_class.insert_child)(w);
#undef superclass

	for (i = 0; i < MGR_NumChildren(pw); i++)
	    PWC_PositionIndex(MGR_Children(pw)[i]) = i;

	return;
    }

    PWC_IsPane(w) = True;

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.insert_child)(w);
#undef superclass

    PW_RecursivelyCalled(pw) = True;

    /*
     * Create separator and sash for the pane.  Don't necessarily manage it,
     * though.
     */ 
    PWC_Separator(w) = XtVaCreateWidget("separator", xmSeparatorWidgetClass,
					(Widget)pw, 
					XmNshadowThickness, 2,
					XmNseparatorType,
					XmSHADOW_ETCHED_IN,
					NULL);
    PWC_Separator(PWC_Separator(w)) = w;
    PWC_Sash(PWC_Separator(w)) = NULL;

    PWC_Sash(w) = XtVaCreateWidget("sash", xmSashWidgetClass,
				   (Widget)pw, 
				   XmNshadowThickness,
				   PW_SashShadowThickness(pw),
				   XmNheight, PW_SashHeight(pw),
				   XmNwidth, PW_SashWidth(pw),
				   NULL);
    PWC_Sash(PWC_Sash(w)) = w;
    PWC_Separator(PWC_Sash(w)) = NULL;
	
    XtAddCallback(PWC_Sash(w),
		  XmNcallback,
		  SashAction,
		  pw);

    PW_RecursivelyCalled(pw) = False;
}

static void 
delete_child(Widget w)
{
    /* have to make sure we delete sash and sep children if child is deleted */
    if (PWC_IsPane(w)) {
	XtDestroyWidget(PWC_Sash(w));
	XtDestroyWidget(PWC_Separator(w));
    }

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.delete_child)(w);
#undef superclass
}

static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *desired)
{
    XdbDebug(__FILE__, w, "query_geometry\n");

    _XmPanedWLayout(w, False, w, True, desired);

    if ((proposed->request_mode & (CWWidth|CWHeight)) == (CWWidth|CWHeight) &&
	desired->width == XtWidth(w) && desired->height == XtHeight(w))
	return XtGeometryNo;

    if ((proposed->request_mode & (CWWidth|CWHeight)) == (CWWidth|CWHeight) &&
	desired->width == proposed->width && desired->height == proposed->height)
	return XtGeometryYes;

    desired->request_mode = 0;
    if (proposed->request_mode & CWWidth)
	desired->request_mode |= CWWidth;

    if (proposed->request_mode & CWHeight)
	desired->request_mode |= CWHeight;

    return XtGeometryAlmost;
}

static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    int			ask = 0, good = 0;
    XtWidgetGeometry	want;

    XdbDebug(__FILE__, w, "PanedW: geometry manager\n");

    if (AllPanesRealized(XtParent(w)) && !PWC_AllowResize(w))
	return XtGeometryNo;

#define Wants(x)	(request->request_mode & x)
    if ((Wants(CWX) && request->x != XtX(w)) ||
	(Wants(CWY) && request->y != XtY(w)))
	return XtGeometryNo;

    want = *request;

    _XmPanedWLayout(XtParent(w), True, w, True, &want);

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

    return XtGeometryNo;
}

static int
pi_sort(const void *a, const void *b)
{
    Widget wa = *(WidgetList)a;
    Widget wb = *(WidgetList)b;

    return PWC_PositionIndex(wa) - PWC_PositionIndex(wb);
}

static void
change_managed(Widget w)
{
    int i, j;

    XdbDebug(__FILE__, w, "PanedWindow change_managed\n");

    if (PW_RecursivelyCalled(w)) {
	XdbDebug(__FILE__, w, "ChangeManaged was recursively called\n");
	return;
    }

    /*
     * go through our managed list, checking to see if one of our managed
     * children has been unmanaged.
     */
    for (i = 0; i < PW_PaneCount(w); i++) {

	if (!XtIsManaged(PW_ManagedChildren(w)[i])) {

	    PW_RecursivelyCalled(w) = True;

	    XtUnmanageChild(PWC_Sash(PW_ManagedChildren(w)[i]));
	    XtUnmanageChild(PWC_Separator(PW_ManagedChildren(w)[i]));

	    PW_RecursivelyCalled(w) = False;

	    PW_NumManagedChildren(w)--;
	    PW_PaneCount(w)--;

	    if ((PW_NumManagedChildren(w) - i) > 0)
		bcopy(&PW_ManagedChildren(w)[i+1], &PW_ManagedChildren(w)[i],
		      sizeof(Widget) * (PW_NumManagedChildren(w) - i));
	}
    }

    /*
     * now go through the pane list and see if a previously unmanaged pane
     * has been managed.  Ugly, O(n**2) linear search, but I expect this
     * is exactly what the M*tif guys did.
     */
    for (i = 0; i < MGR_NumChildren(w); i++) {

	if (!PWC_IsPane(MGR_Children(w)[i]))
	    break;

	if (XtIsManaged(MGR_Children(w)[i])) {

	    for (j = 0; j < PW_PaneCount(w); j++) {
		if (MGR_Children(w)[i] == PW_ManagedChildren(w)[j])
		    break;
	    }

	    /* not in list if bound met */
	    if (j == PW_PaneCount(w)) {

		PW_PaneCount(w)++;
		PWC_DHeight(MGR_Children(w)[i]) = XtHeight(MGR_Children(w)[i]);

		if (PW_NumManagedChildren(w) >= PW_NumSlots(w)) {
		    PW_NumSlots(w) *= 2;
		    PW_ManagedChildren(w) =
			(WidgetList)XtRealloc((void *)PW_ManagedChildren(w),
					      sizeof(Widget) * PW_NumSlots(w));
		}

		PW_ManagedChildren(w)[PW_NumManagedChildren(w)] =
			MGR_Children(w)[i];
		PW_NumManagedChildren(w)++;
	    }
	}
    }

    /*
     * sort the managed children by position index
     */
    qsort((void *)PW_ManagedChildren(w), PW_NumManagedChildren(w),
	  sizeof(Widget), pi_sort);

    for (i = 0; i < PW_PaneCount(w); i++) {
	Widget pane;

	pane = PW_ManagedChildren(w)[i];

	PWC_Position(pane) = i;

	if (i != (PW_PaneCount(w) - 1) && !XtIsManaged(PWC_Sash(pane))) {

	    PW_RecursivelyCalled(w) = True;

	    XtManageChild(PWC_Sash(pane));

	    if (PW_SeparatorOn(w))
		XtManageChild(PWC_Separator(pane));

	    PW_RecursivelyCalled(w) = False;
	}
	else if (i == (PW_PaneCount(w) - 1) && XtIsManaged(PWC_Sash(pane))) {

	    PW_RecursivelyCalled(w) = True;

	    XtUnmanageChild(PWC_Sash(PW_ManagedChildren(w)[i]));
	    XtUnmanageChild(PWC_Separator(PW_ManagedChildren(w)[i]));

	    PW_RecursivelyCalled(w) = False;
	}
    }

    if (PW_NumManagedChildren(w)) {
	PW_TopPane(w) = CoreConstraints(PW_ManagedChildren(w)[0]);
	PW_BottomPane(w) =
	    CoreConstraints(PW_ManagedChildren(w)[PW_NumManagedChildren(w)-1]);
    }

    _XmPanedWLayout(w, True, NULL, False, NULL);

    _XmNavigChangeManaged(w);
}

static Boolean
traversal_children(Widget mw, Widget **children, Cardinal *num_children)
{
    int i, cnt;

    *num_children = PW_PaneCount(mw);
    if (PW_PaneCount(mw) > 1)
	*num_children += (PW_PaneCount(mw) - 1);

    if (*num_children == 0) {
	*children = NULL;
	return True;
    }

    /* note the + 1.  Laziness to avoid two loops */
    *children = (Widget *)XtMalloc((*num_children + 1) * sizeof(Widget));

    cnt = 0;
    for (i = 0; i < PW_PaneCount(mw); i++) {
	(*children)[cnt] = PW_ManagedChildren(mw)[i];
	cnt++;

	/* NOTE: we put in the last sash, but the count indicates that it
	 * should be skipped */
	(*children)[cnt] = PWC_Sash(PW_ManagedChildren(mw)[i]);
	cnt++;
    }

    return True;
}

static void 
_XmPanedWLayout(Widget w, Boolean ParentResize,
		Widget rchild, Boolean TestMode, XtWidgetGeometry *childgeom)
{
    Widget sash, separator; 
    Widget child;
    int i;
    Dimension height, nh, curw, curh;
    XtGeometryResult result;
    XtWidgetGeometry request;

    XdbDebug(__FILE__, w, "_XmPanedWLayout %s %s\n",
	ParentResize ? "ParentResize" : "", TestMode ? "TestMode" : "");

    /* Sanity check */
    if (TestMode)
	ParentResize = False;

    /* Find widest child and compute total height of PanedWindow */
    curw = curh  = 0;
    for (i = 0; i < PW_PaneCount(w); i++) {

	child = PW_ManagedChildren(w)[i];

	curw = curw >= XtWidth(child) ? curw : XtWidth(child);

	if (curh > 0) /* Add spacing between panes */
	    curh += PW_Spacing(w);

	curh += _XmMax(PWC_DHeight(child), PWC_PaneMinimum(child));
    }
    curh += 2 * PW_MarginHeight(w);
    curw += 2 * PW_MarginWidth(w);
  
    if (rchild != NULL && rchild == w && childgeom && TestMode) {
	childgeom->width = curw;
	childgeom->height = curh;
	return;
    }

    if (ParentResize) {

	request.request_mode = (CWWidth|CWHeight);
	request.width = curw;
	request.height = curh;
	result = _XmMakeGeometryRequest(w, &request);
	curw = request.width;
	curh = request.height;

	if (result == XtGeometryYes) {
	    XtWidth(w) = curw;
	    XtHeight(w) = curh;
	}
	if (result == XtGeometryNo) {
	    curw = XtWidth(w);
	    curh = XtHeight(w);
	}
	XdbDebug(__FILE__, w, "_XmPanedWLayout %s (%d %d) => %s\n",
		 XtName(w), curw, curh, XdbGeometryResult2String(result));
    }
    else {
	curw = XtWidth(w);
	curh = XtHeight(w);
    }

    /* now actually do the layout */
    height = PW_MarginHeight(w);

    for(i = 0; i < PW_PaneCount(w); i++) {
	child = PW_ManagedChildren(w)[i];

	if (child == rchild)
	    continue;	/* Don't resize the child being specified as parameter */

	XdbDebug2(__FILE__, w, child, "Layout child height %d\n", height);

	XdbDebug(__FILE__, w, "child %s height %d width %d border %d\n",
		 XtName(child), XtHeight(child), XtWidth(child),
		 XtBorderWidth(child));

	if (i == (PW_PaneCount(w) - 1)) {
	    nh = curh - height - PW_MarginHeight(w);

	    _XmConfigureObject(child, 
	    		       PW_MarginWidth(w), height,
			       curw - 2 * PW_MarginWidth(w), 
			       nh,
			       XtBorderWidth(child));
	    PWC_DY(child) = height;
	    PWC_DHeight(child) = nh;
	}
	else {
	    _XmConfigureObject(child, 
	    		       PW_MarginWidth(w), height,
			       curw - 2 * PW_MarginWidth(w), 
			       PWC_DHeight(child),
			       XtBorderWidth(child));
	    PWC_DY(child) = height;
	}

	height = height + PWC_DHeight(child);

	if (i != (PW_PaneCount(w) - 1)) {
	    height += PW_Spacing(w) / 2;

	    separator  = PWC_Separator(child);
	    if (separator) {
		_XmConfigureObject(separator,
				   0, height - XtHeight(separator)/2,
				   curw, XtHeight(separator),
				   XtBorderWidth(separator));
	    } 

	    sash = PWC_Sash(child);
	    if (sash) {
		XdbDebug(__FILE__, w, "moving sash to : %d %d w = %d h = %d\n", 
			 curw - PW_MarginWidth(w) +
			 PW_SashIndent(w) - PW_SashWidth(w),
			 height - (PW_SashHeight(w)/2),
			 PW_SashWidth(w),
			 PW_SashHeight(w));

		_XmConfigureObject(sash, 
				   curw + PW_SashIndent(w) - PW_SashWidth(w),
				   height - PW_SashHeight(w)/2,
				   PW_SashWidth(w), PW_SashHeight(w),
				   XtBorderWidth(sash));
	    }

	    height += PW_Spacing(w) / 2;
	}
    }
}

/* #define OLDWAY */
#ifdef OLDWAY

static int 
MoveBorderUp(Widget pane, int dy, int first)
{
    Widget prev, next=NULL;
    XmPanedWindowWidget pw = (XmPanedWindowWidget) XtParent(pane);
    int sy, my;

    if (PWC_Position(pane) == 0)
	prev = pane;
    else
	prev = MGR_Children(pw)[PWC_Position(pane) - 1];
    if (PWC_Position(pane) < PW_PaneCount(pw) - 1)
	next = MGR_Children(pw)[PWC_Position(pane) + 1];

    if (dy == 0) {
	if (first) {
	    PWC_AllowResize(pane) = False;
	}
	if (PWC_Position(prev) > 0)
	    MoveBorderUp(prev, 0, False);
	PWC_AllowResize(prev) = False;
	sy = my = 0;
    }
    else {
	if (first) /* Don't make current pane larger than pane_maximum */
	    dy = _XmMin(dy, PWC_PaneMaximum(pane) - XtHeight(pane));
	
    
	/* Shrink previous pane */	  
	sy = _XmMin(dy, XtHeight(prev) - PWC_PaneMinimum(prev));
	PWC_AllowResize(prev) = True;
	PWC_DHeight(prev) = XtHeight(prev) - sy;
	
	/* Move previous pane up */
	if (PWC_Position(prev) > 0) {
	    my = MoveBorderUp(prev, dy - sy, False);
	}
	else 
	    my = 0;
	
	if (first) {
	    PWC_AllowResize(pane) = True;
	    PWC_DHeight(pane) = XtHeight(pane)+ sy + my;
	}
    }
	
    return sy + my;
}


static int
MoveBorderDown(Widget pane, int dy, int first)
{
    Widget prev, next=NULL;
    XmPanedWindowWidget pw = (XmPanedWindowWidget) XtParent(pane);
    int sy, my, my_up = 0;

    if (PWC_Position(pane) == 0)
        prev = pane;
    else
	prev = MGR_Children(pw)[PWC_Position(pane) - 1];

    if (PWC_Position(pane) < PW_PaneCount(pw) - 1)
	next = MGR_Children(pw)[PWC_Position(pane) + 1];

    if (dy == 0) {
	if (first) {
	    PWC_AllowResize(pane) = False;
	}
	if (next)
	    PWC_AllowResize(next) = False;
	sy = my = 0;
    }
    else {
	if (first) /* Don't make previous pane larger than pane_maximum */
	    dy = _XmMin(dy, PWC_PaneMaximum(prev) - XtHeight(prev));
	
	/* Shrink current pane */
	sy = _XmMin(dy, XtHeight(pane) - PWC_PaneMinimum(pane));
	PWC_AllowResize(pane) = True;
	PWC_DHeight(pane) = XtHeight(pane) - sy;

	/* Move current pane down */
	if (next)
	    my = MoveBorderDown(next, dy - sy, False);
	else 
	    my = 0;

	if (my < dy - sy) {
	    /* Move previous border if we cannot move the lower 
	     * border all the way.
	     */
	    if (first && PWC_Position(prev) > 0) {
		my_up = MoveBorderUp(prev, dy - sy - my, True);
	    }
	}
	else if (first && PWC_Position(prev) > 0) {
	    /* Reset previous border to avoid possible incorrect 
	     * border placement as a result of rapid cursor movement.
	     */
	    MoveBorderUp(prev, 0, False); 
	}
	if (first) {
	    PWC_AllowResize(prev) = True;
	    PWC_DHeight(prev) = XtHeight(prev) + sy + my + my_up;
	}
    }

    return sy + my;
}

static void 
MoveBorder(Widget pane, int dy, int first)
{
    if (dy < 0) { 
	MoveBorderUp(pane, dy, first);
    }
    else if (dy > 0) {
	MoveBorderDown(pane, dy, first);
    }
}

static void 
SashAction(Widget w, XtPointer client_data, XtPointer call_data)
{
    int y, i;
    XmPanedWindowWidget pw = (XmPanedWindowWidget) client_data;
    XEvent *event = ((SashCallData)call_data)->event;
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget child;
    Widget pane = NULL;

    pane = PWC_Sash(w);

    switch(event->type) {
    case ButtonPress:
	PW_StartY(pw) = ev->y;
	PWC_DY(pane) = XtY(w) + XtHeight(w) / 2;
	break;
    case ButtonRelease:
	XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
		  0, PWC_DY(pane), XtWidth(pw), PWC_DY(pane));

	for(i = 0; i < PW_NumManagedChildren(pw); i++) {
	    child = PW_ManagedChildren(pw)[i];
	    if (PWC_AllowResize(child)) {
		_XmConfigureObject(child,
				   XtX(child), XtY(child),
				   XtWidth(child),
			           PWC_DHeight(child),
				   XtBorderWidth(child));
		PWC_AllowResize(child) = False;
	    }
	}
	_XmPanedWLayout((Widget)pw, False, NULL, False, NULL);
	break;
    case MotionNotify:
	PWC_OldDY(pane) = PWC_DY(pane);
	y = ((XMotionEvent *)event)->y - PW_StartY(pw);
	PWC_DY(pane) = XtY(w) + XtHeight(w)/2 + y;

	MoveBorder(pane,  y, True);

	if (PWC_DY(pane) != PWC_OldDY(pane)) {
	    XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
		      0, PWC_OldDY(pane), XtWidth(pw), PWC_OldDY(pane));

	    XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
		      0, PWC_DY(pane), XtWidth(pw), PWC_DY(pane));

	}

	break;
    }
}

#else

static void 
MoveBorder(Widget pw, Widget sash, int dy)
{
    Widget panea, paneb;
    XmPanedWindowConstraintPtr cons;
    CoreRec a, b;

    /*
     * because it's a sash, we know that we have a pane above (which is the
     * back link in the sash constraints [panea]), and a pane below (which
     * appears at the next consecutive spot in the managed_children list --
     * which is panea's position + 1.  The bottom most pane never has a
     * managed sash (so we can't get here).  The top pane's sash is always
     * below the top pane.  Therefore, the pointers must always be valid!
     * If they aren't, something is broken -- but somewhere else, not here.
     */
    panea = PWC_Sash(sash);
    paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
    CoreConstraints(&a) = panea->core.constraints;
    CoreConstraints(&b) = paneb->core.constraints;

    for (;;) {

	/* Move up? */
	if (dy < 0) {

	    XdbDebug(__FILE__, pw, "Move up\n");

	    if ((PWC_DHeight(paneb) - dy) < PWC_PaneMaximum(paneb)) {
	        /*
		 * can panea absorb the change?  If so, then adjust paneb's
		 * height up, it's dy up, and decrease panea's height
		 */
		if ((PWC_DHeight(panea) + dy) > PWC_PaneMinimum(panea)) {
		    cons = PW_BottomPane(pw);
		    cons->panedw.dheight -= dy;
		    cons->panedw.dy += dy;
		    cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position-1]);
		    while (cons != PW_TopPane(pw)) {
			cons->panedw.dy += dy;
			cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position-1]);
		    }
		    PWC_DHeight(panea) += dy;
		    return;
		}
		/*
		 * can panea absorb part of the change?  If so, then adjust
		 * paneb's height as much as possible, adjust his dy as much
		 * as possible, and minimize panea's height.  If possible,
		 * pass the rest of the delta on to the parent.
		 */
		else if (PWC_DHeight(panea) > PWC_PaneMinimum(panea)) {
		    dy += PWC_DHeight(panea) - PWC_PaneMinimum(panea);

#if 0
		    cons = PW_BottomPane(pw);
		    cons->panedw.dheight -= dy;
		    cons->panedw.dy += dy;
		    cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position-1]);
		    while (cons != PW_TopPane(pw)) {
			cons->panedw.dy += dy;
			cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position-1]);
		    }
#else
		    PWC_DHeight(paneb) += PWC_DHeight(panea) - PWC_PaneMinimum(panea);
		    PWC_DY(paneb) -= PWC_DHeight(panea) - PWC_PaneMinimum(panea);
#endif

		    PWC_DHeight(panea) = PWC_PaneMinimum(panea);

		    if (PWC_Position(panea) > 0) {
			sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea)-1]);
			panea = PWC_Sash(sash);
			paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
			PW_TopPane(pw) = CoreConstraints(panea);
		    }
	        }
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * higher pane
		 */
		else if (PWC_Position(panea) > 0) {
		    sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea)-1]);
		    panea = PWC_Sash(sash);
		    paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
		    PW_TopPane(pw) = CoreConstraints(panea);
		}
		/*
		 * no more higher panes, bail out.
		 */
		else
		    return;
	    }
	    /* paneb can't grow, bail out */
	    else
		return;
	}
	/* move down? */
	else if (dy > 0) {

	    XdbDebug(__FILE__, pw, "Move down: %d %d %d %d %d\n",
		     PWC_DHeight(panea), dy, PWC_PaneMaximum(panea),
		     PWC_DHeight(paneb), PWC_PaneMinimum(paneb));

	    /* can panea grow? */
	    if ((PWC_DHeight(panea) + dy) < PWC_PaneMaximum(panea)) {
		/*
		 * can paneb absorb the change?  If so, increate panea's
		 * height, increase paneb's dy, and decrease paneb's height.
		 */
		if ((PWC_DHeight(paneb) - dy) > PWC_PaneMinimum(paneb)) {
		    PWC_DHeight(paneb) -= dy;
		    PWC_DY(paneb) += dy;

		    cons = PW_TopPane(pw);
		    cons->panedw.dheight += dy;
		    cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position+1]);
		    while (cons != PW_BottomPane(pw)) {
			cons->panedw.dy += dy;
			cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position+1]);
		    }
		    return;
		}
		/*
		 * can paneb absorb part of the change? If so, increase panea's
		 * height as much as possible, increate paneb's dy as much
		 * as possible, and decrease paneb's height to the minimum.
		 * Pass the rest on to a lower pane.
		 */
		else if (PWC_DHeight(paneb) > PWC_PaneMinimum(paneb)) {
		    dy -= PWC_DHeight(paneb) - PWC_PaneMinimum(paneb);

#if 0
		    cons = PW_TopPane(pw);
		    cons->panedw.dheight += dy;
		    cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position+1]);
		    while (cons != PW_BottomPane(pw)) {
			cons->panedw.dy += dy;
			cons = CoreConstraints(PW_ManagedChildren(pw)[cons->panedw.position+1]);
		    }
#else
		    PWC_DHeight(panea) += PWC_DHeight(paneb) - PWC_PaneMinimum(paneb);
		    PWC_DY(paneb) += PWC_DHeight(paneb) - PWC_PaneMinimum(paneb);
#endif
		    PWC_DHeight(paneb) = PWC_PaneMinimum(paneb);

		    if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1)) {
			sash = PWC_Sash(paneb);
			panea = PWC_Sash(sash);
			paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
			PW_BottomPane(pw) = CoreConstraints(paneb);
		    }
		}
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * lower pane
		 */
		else if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1)) {
		    sash = PWC_Sash(paneb);
		    panea = PWC_Sash(sash);
		    paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
		    PW_BottomPane(pw) = CoreConstraints(paneb);
		}
		/*
		 * no more lower panes, bail out.
		 */
		else
		    return;
	    }
	    /* panea can't grow, bail out */
	    else
		return;
	}
	/* no motion, bail out */
	else {

	    XdbDebug(__FILE__, pw, "Move down\n");

	    return;
	}
    }
}

static void 
SashAction(Widget w, XtPointer client_data, XtPointer call_data)
{
    int y, i;
    Widget pw = (Widget)client_data;
    XEvent *event = ((SashCallData)call_data)->event;
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget child;

    switch(event->type) {
    case ButtonPress:
	PW_StartY(pw) = ev->y;
	PW_TopPane(pw) = CoreConstraints(PWC_Sash(w));
	PW_BottomPane(pw) = CoreConstraints(PW_ManagedChildren(pw)[PWC_Position(PWC_Sash(w))+1]);
	break;

    case ButtonRelease:
	for (i = 0; i < PW_PaneCount(pw); i++) {
	    child = PW_ManagedChildren(pw)[i];
	    if (PWC_DY(child) != PWC_OldDY(child)) {
		XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			  0, PWC_DY(child) - PW_Spacing(pw) / 2,
			  XtWidth(pw), PWC_DY(child) - PW_Spacing(pw) / 2);
	    }
	}

#if 0
	for (i = 0; i < PW_PaneCount(pw); i++) {
	    child = PW_ManagedChildren(pw)[i];
	    if (PWC_DHeight(child) != XtHeight(child)) {
		_XmConfigureObject(child,
				   XtX(child), PWC_DY(child),
				   XtWidth(child),
			           PWC_DHeight(child),
				   XtBorderWidth(child));
	    }
	    if (i != (PW_PaneCount(pw) - 1)) {
		height = PWC_DY(child) + PWC_DHeight(child) + PW_Spacing(pw) / 2;

		_XmConfigureObject(PWC_Separator(child),
				   0, height - XtHeight(PWC_Separator(child))/2,
				   XtWidth(pw), XtHeight(PWC_Separator(child)),
				   XtBorderWidth(PWC_Separator(child)));

		_XmConfigureObject(PWC_Sash(child), 
				   XtWidth(pw) + PW_SashIndent(pw)
					- PW_SashWidth(pw),
				   height - PW_SashHeight(pw)/2,
				   PW_SashWidth(pw), PW_SashHeight(pw),
				   XtBorderWidth(PWC_Sash(child)));
	    }
	}
#else
	_XmPanedWLayout((Widget)pw, False, NULL, False, NULL);
#endif

	break;

    case MotionNotify:

#if 1
	if ((((XMotionEvent *)event)->y + XtY(w)) < XtY(pw) ||
	    (((XMotionEvent *)event)->y + XtY(w)) > XtY(pw) + XtHeight(pw))
	    break;
#else
	if ((((XMotionEvent *)event)->y + XtY(w)) < PWC_DY(PWC_Sash(w)) ||
	    (((XMotionEvent *)event)->y + XtY(w)) > PWC_DY(PWC_Sash(w)))
	    break;
#endif

	for (i = 0; i < PW_PaneCount(pw); i++) {
	    PWC_OldDY(PW_ManagedChildren(pw)[i]) =
		PWC_DY(PW_ManagedChildren(pw)[i]);
	}

	y = ((XMotionEvent *)event)->y - PW_StartY(pw);
	PW_StartY(pw) = ((XMotionEvent *)event)->y;

	MoveBorder((Widget)pw, w, y);

	for (i = 0; i < PW_PaneCount(pw); i++) {
	    child = PW_ManagedChildren(pw)[i];
	    if (PWC_DY(child) != PWC_OldDY(child)) {
		XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			  0, PWC_OldDY(child) - PW_Spacing(pw) / 2,
			  XtWidth(pw), PWC_OldDY(child) - PW_Spacing(pw) / 2);

		XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			  0, PWC_DY(child) - PW_Spacing(pw) / 2,
			  XtWidth(pw), PWC_DY(child) - PW_Spacing(pw) / 2);
	    }
	}

	break;
    }
}

#endif

Widget
XmCreatePanedWindow(Widget parent, 
		    char *name,
		    Arg *argList,
		    Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmPanedWindowWidgetClass,
			  parent,
			  argList, argcount);
}

