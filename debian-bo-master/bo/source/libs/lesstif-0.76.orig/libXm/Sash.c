/**
 *
 * $Id: Sash.c,v 1.5 1996/11/28 09:21:48 u27113 Exp $
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

static char rcsid[] = "$Id: Sash.c,v 1.5 1996/11/28 09:21:48 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/SashP.h>
#include <X11/cursorfont.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize();
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args);

/*
 * Resources for the sash class
 */
#define Offset(field) XtOffsetOf(XmSashRec, sash.field)
static XtResource resources[] = {
    {
	XmNborderWidth, XmCBorderWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNcallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(sash_action),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), XtOffsetOf(XmPrimitiveRec, primitive.navigation_type),
	XmRImmediate, (XtPointer)XmSTICKY_TAB_GROUP
    },  
};

static void SashAction(Widget, XEvent *, String *, Cardinal *);
static void NextTabGroup(Widget, XEvent *, String *, Cardinal *);
static void PrevTabGroup(Widget, XEvent *, String *, Cardinal *);
static void enter(Widget, XEvent *, String *, Cardinal *);
static void leave(Widget, XEvent *, String *, Cardinal *);
static void SashFocusIn(Widget, XEvent *, String *, Cardinal *);
static void SashFocusOut(Widget, XEvent *, String *, Cardinal *);

char _XmSash_defTranslations[] = 
   "<Unmap>:                  PrimitiveUnmap()\n\
    <EnterWindow>:            enter()\n\
    <LeaveWindow>:            leave()\n\
    <FocusIn>:                SashFocusIn()\n\
    <FocusOut>:               SashFocusOut()\n\
    ~c ~s ~m ~a <Btn1Down>:   SashAction(Start)\n\
    ~c ~s ~m ~a <Btn1Motion>: SashAction(Move)\n\
    ~c ~s ~m ~a <Btn1Up>:     SashAction(Commit)\n\
    ~c ~s ~m ~a <Btn2Down>:   SashAction(Start)\n\
    ~c ~s ~m ~a <Btn2Motion>: SashAction(Move)\n\
    ~c ~s ~m ~a <Btn2Up>:     SashAction(Commit)\n\
    <Key>osfActivate:         PrimitiveParentActivate()\n\
    <Key>osfCancel:           PrimitiveParentCancel()\n\
    c <Key>osfUp:             SashAction(Key,LargeIncr,Up)\n\
    <Key>osfUp:               SashAction(Key,DefaultIncr,Up)\n\
    c <Key>osfDown:           SashAction(Key,LargeIncr,Down)\n\
    <Key>osfDown:             SashAction(Key,DefaultIncr,Down)\n\
    ~s ~m ~a <Key>Return:     PrimitiveParentActivate()\n\
    s ~m ~a <Key>Tab:         PrevTabGroup()\n\
    ~m ~a <Key>Tab:           NextTabGroup()";

static XtActionsRec actions[] = {
    {"SashAction", SashAction},
    {"enter", enter},
    {"leave", leave},
    {"SashFocusIn", SashFocusIn},
    {"SashFocusOut", SashFocusOut},
    {"NextTabGroup", NextTabGroup},
    {"PrevTabGroup", PrevTabGroup},
};

static XmBaseClassExtRec _XmSashCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmSashPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmSashClassRec xmSashClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name            */ "XmSash",
	/* widget_size           */ sizeof(XmSashRec),
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
	/* resize                */ NULL,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmSash_defTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ (XtPointer)&_XmSashCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ NULL,
        /* arm_and_activate_proc */ XmInheritArmAndActivate,
        /* synthetic resources   */ NULL,
	/* num syn res           */ 0,
        /* extension             */ (XtPointer)&_XmSashPrimClassExtRec,
    },
    /* Sash Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmSashWidgetClass = (WidgetClass)&xmSashClassRec;

static void
class_initialize()
{
    _XmSashCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSASH_BIT);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
   XdbDebug(__FILE__, new_w, "sash initialize\n");
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
    XdbDebug(__FILE__, new_w, "sash setvalues\n");

    return False;
}

static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    Cursor crosshair = XCreateFontCursor(XtDisplay(w), XC_crosshair);

    *value_mask |= CWCursor;
    attributes->cursor = crosshair;

#define superclass (&xmPrimitiveClassRec)
    (*superclass->core_class.realize)(w, value_mask, attributes);
#undef superclass
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    XdbDebug(__FILE__, w, "Sash expose\n");

    _XmDrawShadows(XtDisplay(w), 
		   XtWindow(w), 
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   0,0,
		   XtWidth(w),
		   XtHeight(w),
		   Prim_ShadowThickness(w),
		   XmSHADOW_OUT);

    if (!Prim_Highlighted(w)) {
	XFillRectangle(XtDisplay(w), XtWindow(w),
		       XmParentBackgroundGC(w),
		       Prim_ShadowThickness(w), Prim_ShadowThickness(w),
		       XtWidth(w) - 2 * Prim_ShadowThickness(w),
		       XtHeight(w) - 2 * Prim_ShadowThickness(w));
    }
}

static XtGeometryResult 
query_geometry(Widget w, 
	       XtWidgetGeometry *proposed, 
	       XtWidgetGeometry *answer)
{
    XmSashWidget sw = (XmSashWidget)w;
    answer->request_mode = CWWidth | CWHeight;

    answer->width = XtWidth(w) + 2 * Prim_ShadowThickness(sw);

    answer->height = XtHeight(w) + 2 * Prim_ShadowThickness(sw);
    
    if (((proposed->request_mode & (CWWidth | CWHeight))
	 == (CWWidth | CWHeight)) &&
	proposed->width == answer->width &&
	proposed->height == answer->height)

	return XtGeometryYes;
    else if (answer->width == XtWidth(sw) &&
	     answer->height == XtHeight(sw))
	return XtGeometryNo;
    else 
	return XtGeometryAlmost;    
}

static void 
SashAction(Widget w,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    XmSashWidget sw = (XmSashWidget) w;
    SashCallDataRec cd;

    cd.event = event;
    cd.params = params;
    cd.num_params = *num_params;

    XtCallCallbackList((Widget)sw, Sash_SashAction(sw), &cd);

    expose(w, NULL, NULL);
}


static void
enter(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

static void
leave(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

static void
SashFocusIn(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "Sash FocusIn\n");
}

static void
SashFocusOut(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XdbDebug(__FILE__, w, "SASH FOCUS OUT\n");
}

static void
NextTabGroup(Widget w,
	     XEvent *event,
	     String *params,
	     Cardinal *num_params)
{
}

static void 
PrevTabGroup(Widget w,
	     XEvent *event,
	     String *params,
	     Cardinal *num_params)
{
}
