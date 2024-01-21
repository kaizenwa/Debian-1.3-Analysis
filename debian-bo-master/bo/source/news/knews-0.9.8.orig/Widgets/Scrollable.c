/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include "Compat.h"
#include "Manager.h"
#include "ScrollableP.h"
#include "Util.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(ScrollableRec, scrollable.field)
    {XtNhBar, XtCScrBar, XtRWidget, sizeof(Widget),
     offset(h_bar), XtRImmediate, (XtPointer)NULL},
    {XtNvBar, XtCScrBar, XtRWidget, sizeof(Widget),
     offset(v_bar), XtRImmediate, (XtPointer)NULL},
#undef offset
};

static void	ClassPartInitialize(WidgetClass);
static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Resize(Widget);
static void	Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void	SetHPos(ScrollableWidget, long);
static void	SetVPos(ScrollableWidget, long);

ScrollableClassRec scrollableClassRec = {
    { /* core_class fields */
	(WidgetClass) &shadowClassRec,	/* superclass			*/
	"Scrollable",			/* class_name			*/
	sizeof(ScrollableRec),		/* widget_size			*/
	NULL,				/* class_initialize		*/
	ClassPartInitialize,		/* class_part_initialize	*/
	FALSE,				/* class_inited			*/
	Initialize,			/* initialize			*/
	NULL,				/* initialize_hook		*/
	Realize,			/* realize			*/
	NULL,				/* actions			*/
	0,				/* num_actions			*/
	resources,			/* resources			*/
	XtNumber(resources),		/* num_resources		*/
	NULLQUARK,			/* xrm_class			*/
	TRUE,				/* compress_motion		*/
	TRUE,				/* compress_exposure		*/
	TRUE,				/* compress_enterleave		*/
	FALSE,				/* visible_interest		*/
	NULL,				/* destroy			*/
	Resize,				/* resize			*/
	NULL,				/* expose			*/
	SetValues,			/* set_values			*/
	NULL,				/* set_values_hook		*/
	XtInheritSetValuesAlmost,	/* set_values_almost		*/
	NULL,				/* get_values_hook		*/
	NULL,				/* accept_focus			*/
	XtVersion,			/* version			*/
	NULL,				/* callback_private		*/
	NULL,				/* tm_table			*/
	XtInheritQueryGeometry,		/* query_geometry		*/
	XtInheritDisplayAccelerator,	/* display_accelerator		*/
	NULL				/* extension			*/
    },
    {					/* shadow fields		*/
	XtInheritPixelOffset,		/* pixel_offset */
	False,				/* use_arm_for_background	*/
	XtInheritAllocShadowColors,	/* alloc_shadow_colors		*/
	XtInheritAllocShadowPixmaps,	/* alloc_shadow_pixmaps		*/
	XtInheritAllocArmColor,		/* alloc_arm_color		*/
	XtInheritAllocArmPixmap,	/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    {					/* scrollable fields		*/
	SetHPos,			/* set_hpos			*/
	SetVPos,			/* set_vpos			*/
	NULL,				/* suspend_hook			*/
	NULL,				/* extension			*/
    }
};

WidgetClass scrollableWidgetClass = (WidgetClass)&scrollableClassRec;

/*************************************************************************/

static void hbar_callback(Widget    scr_bar,
			  XtPointer client_data,
			  XtPointer call_data)
{
    ScrollReport		*rep  = (ScrollReport *)call_data;
    ScrollableWidget		w     = (ScrollableWidget)client_data;
    ScrollableWidgetClass	class = (ScrollableWidgetClass)XtClass(w);
    long			n_pos, n_shown, n_size;

    if (!w->core.managed)
	return;

    class->scrollable_class.set_hpos(w, rep->pos);
    n_pos   = w->scrollable.pos_x;
    n_shown = w->scrollable.shown_x;
    n_size  = w->scrollable.width;
    if (rep->pos != n_pos || rep->shown != n_shown || rep->size != n_size)
	ScrBarSetLengthsAndPos(scr_bar, n_size, n_shown, n_pos);
}

static void vbar_callback(Widget    scr_bar,
			  XtPointer client_data,
			  XtPointer call_data)
{
    ScrollReport		*rep  = (ScrollReport *)call_data;
    ScrollableWidget		w     = (ScrollableWidget)client_data;
    ScrollableWidgetClass	class = (ScrollableWidgetClass)XtClass(w);
    long			n_pos, n_shown, n_size;

    if (!w->core.managed)
	return;

    class->scrollable_class.set_vpos(w, rep->pos);
    n_pos   = w->scrollable.pos_y;
    n_shown = w->scrollable.shown_y;
    n_size  = w->scrollable.height;
    if (rep->pos != n_pos || rep->shown != n_shown || rep->size != n_size)
	ScrBarSetLengthsAndPos(scr_bar, n_size, n_shown, n_pos);
}

static void remove_callback(ScrollableWidget w,
			    Widget           scr_bar,
			    XtCallbackProc   callback)
{
    if (scr_bar)
	XtRemoveCallback(scr_bar, XtNscrollCallback, callback, (XtPointer)w);
}

static void add_callback(ScrollableWidget w,
			 Widget           scr_bar,
			 XtCallbackProc   callback)
{
    if (scr_bar)
	XtAddCallback(scr_bar, XtNscrollCallback, callback, (XtPointer)w);
}

static void call_suspend_hook(ScrollableWidget w)
{
    ScrollableWidgetClass	class = (ScrollableWidgetClass)XtClass(w);

    if (class->scrollable_class.suspend_hook)
	class->scrollable_class.suspend_hook(w);
}

static void parent_resize_callback(Widget	parent,
				   XtPointer	client_data,
				   XtPointer	call_data)
{
    Widget	w = (Widget)client_data;

    if (XtIsManaged(w) && w->core.widget_class->core_class.resize)
	w->core.widget_class->core_class.resize(w);
}

/*************************************************************************/

static void ClassPartInitialize(WidgetClass gclass)
{
    ScrollableWidgetClass	class, super;

    class = (ScrollableWidgetClass)gclass;
    super = (ScrollableWidgetClass)class->core_class.superclass;

    if (class->scrollable_class.set_hpos == XtInheritScrollableSetPos)
	class->scrollable_class.set_hpos = super->scrollable_class.set_hpos;
    if (class->scrollable_class.set_vpos == XtInheritScrollableSetPos)
	class->scrollable_class.set_vpos = super->scrollable_class.set_vpos;
    if (class->scrollable_class.suspend_hook == XtInheritScrollableSuspendHook)
	class->scrollable_class.suspend_hook =
	    super->scrollable_class.suspend_hook;
}

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *num_args)
{
    ScrollableWidget	new    = (ScrollableWidget)gnew;
    Widget		parent = XtParent(new);

    new->scrollable.pos_x   = 0;
    new->scrollable.shown_x = 0;
    new->scrollable.width   = 0;
    new->scrollable.pos_y   = 0;
    new->scrollable.shown_y = 0;
    new->scrollable.height  = 0;
    new->scrollable.suspended = False;
    add_callback(new, new->scrollable.h_bar, hbar_callback);
    add_callback(new, new->scrollable.v_bar, vbar_callback);
    if (XtIsSubclass(parent, managerWidgetClass))
	XtAddCallback(parent, XtNresizeCallback,
		      parent_resize_callback, (XtPointer)new);
}

static void Resize(Widget gw)
{
    ScrollableWidget	w = (ScrollableWidget)gw;

    ScrollableHFromGeometry(w);
    ScrollableVFromGeometry(w);
    ScrollableFitHBar(w);
    ScrollableFitVBar(w);
}

static void SetHPos(ScrollableWidget w, long x)
{
    Arg		arg;

    XtSetArg(arg, XtNx, - x);
    XtSetValues((Widget)w, &arg, 1);
    ScrollableHFromGeometry(w);
}

static void SetVPos(ScrollableWidget w, long y)
{
    Arg		arg;

    XtSetArg(arg, XtNy, - y);
    XtSetValues((Widget)w, &arg, 1);
    ScrollableVFromGeometry(w);
}

static void Realize(Widget gw, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    ScrollableWidget	w = (ScrollableWidget)gw;

    ScrollableFitHBar(w);
    ScrollableFitVBar(w);
    shadowWidgetClass->core_class.realize((Widget)w, mask, attributes);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    ScrollableWidget	new     = (ScrollableWidget)gnew;
    ScrollableWidget	current = (ScrollableWidget)gcurrent;

    if (new->scrollable.h_bar != current->scrollable.h_bar) {
	remove_callback(new, current->scrollable.h_bar, hbar_callback);
	add_callback(new, new->scrollable.h_bar, hbar_callback);
	ScrollableFitHBar(new);
    }

    if (new->scrollable.v_bar != current->scrollable.v_bar) {
	remove_callback(new, current->scrollable.v_bar, vbar_callback);
	add_callback(new, new->scrollable.v_bar, vbar_callback);
	ScrollableFitVBar(new);
    }

    return False;
}

/*************************************************************************/

void ScrollableSetHBar(Widget gw, Widget h_bar)
{
    ScrollableWidget	w = (ScrollableWidget)gw;

    remove_callback(w, w->scrollable.h_bar, hbar_callback);
    w->scrollable.h_bar = h_bar;
    add_callback(w, w->scrollable.h_bar, hbar_callback);
    if (!w->scrollable.suspended)
	ScrollableFitHBar(w);
}

void ScrollableSetVBar(Widget gw, Widget v_bar)
{
    ScrollableWidget	w = (ScrollableWidget)gw;

    remove_callback(w, w->scrollable.v_bar, vbar_callback);
    w->scrollable.v_bar = v_bar;
    add_callback(w, w->scrollable.v_bar, vbar_callback);
    if (!w->scrollable.suspended)
	ScrollableFitVBar(w);
}

void ScrollableSuspend(Widget gw)
{
    ScrollableWidget	w = (ScrollableWidget)gw;

    w->scrollable.suspended = True;
    call_suspend_hook(w);
}

void ScrollableResume(Widget gw)
{
    ScrollableWidget	w = (ScrollableWidget)gw;

    w->scrollable.suspended = False;
    call_suspend_hook(w);
    ScrollableFitHBar(w);
    ScrollableFitVBar(w);
}

void ScrollableFitHBar(ScrollableWidget w)
{
    if (!w->scrollable.h_bar || w->scrollable.suspended)
	return;
    ScrBarSetLengthsAndPos(w->scrollable.h_bar,
			   w->scrollable.width,
			   w->scrollable.shown_x,
			   w->scrollable.pos_x);
}

void ScrollableFitVBar(ScrollableWidget w)
{
    if (!w->scrollable.v_bar || w->scrollable.suspended)
	return;
    ScrBarSetLengthsAndPos(w->scrollable.v_bar,
			   w->scrollable.height,
			   w->scrollable.shown_y,
			   w->scrollable.pos_y);
}

void ScrollableHFromGeometry(ScrollableWidget w)
{
    w->scrollable.pos_x   = -w->core.x;
    w->scrollable.shown_x = XtParent(w)->core.width;
    w->scrollable.width   = w->core.width;
}

void ScrollableVFromGeometry(ScrollableWidget w)
{
    w->scrollable.pos_y   = -w->core.y;
    w->scrollable.shown_y = XtParent(w)->core.height;
    w->scrollable.height  = w->core.height;
}

void ScrollableSetHPos(Widget gw, long pos_x)
{
    ScrollableWidget		w = (ScrollableWidget)gw;
    ScrollableWidgetClass	class = (ScrollableWidgetClass)XtClass(w);

    class->scrollable_class.set_hpos(w, pos_x);
    ScrollableFitHBar(w);
}

void ScrollableSetVPos(Widget gw, long pos_y)
{
    ScrollableWidget		w = (ScrollableWidget)gw;
    ScrollableWidgetClass	class = (ScrollableWidgetClass)XtClass(w);

    class->scrollable_class.set_vpos(w, pos_y);
    ScrollableFitVBar(w);
}

long ScrollableGetVPos(Widget gw)
{
    ScrollableWidget		w = (ScrollableWidget)gw;

    return w->scrollable.pos_y;
}

long ScrollableGetVSize(Widget gw)
{
    ScrollableWidget		w = (ScrollableWidget)gw;

    return w->scrollable.height;
}

long ScrollableGetVShown(Widget gw)
{
    ScrollableWidget		w = (ScrollableWidget)gw;

    return w->scrollable.shown_y;
}

void ScrollablePage(Widget gw, float amount)
{
    ScrollableWidget	w = (ScrollableWidget)gw;

    ScrollableSetVPos((Widget)w,
		      w->scrollable.pos_y + amount * w->scrollable.shown_y);
}
