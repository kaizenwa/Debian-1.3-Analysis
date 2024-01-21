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
#include "ToggleP.h"

static XtResource resources[] = {
    {XtNleftMargin, XtCLeftMargin, XtRDimension, sizeof(Dimension),
     XtOffsetOf(ToggleRec, knapp.left_margin), XtRImmediate, (XtPointer)24},
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(ToggleRec, core.border_width), XtRImmediate, (XtPointer)0},
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(ToggleRec, shadow.shadow_width), XtRImmediate, (XtPointer)1},
#define offset(field) XtOffsetOf(ToggleRec, toggle.field)
    {XtNtoggleSize, XtCToggleSize, XtRDimension, sizeof(Dimension),
     offset(toggle_size), XtRImmediate, (XtPointer)12},
    {XtNtoggleOffset, XtCToggleOffset, XtRDimension, sizeof(Dimension),
     offset(toggle_offset), XtRImmediate, (XtPointer)8},
    {XtNtoggleShadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     offset(toggle_shadow_width), XtRImmediate, (XtPointer)2},
    {XtNset, XtCSet, XtRBoolean, sizeof(Boolean),
     offset(set), XtRImmediate, (XtPointer)False},
#undef offset
};

static void	Redisplay(Widget, XEvent*, Region);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void	toggle(Widget, XEvent*, String*, Cardinal*);
static void	set(Widget, XEvent*, String*, Cardinal*);
static void	reset(Widget, XEvent*, String*, Cardinal*);
static void	notify(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"toggle",	toggle},
    {"set",	set},
    {"reset",	reset},
    {"notify",	notify},
};

static char translations[] =
"<BtnDown>:	notify() \n";

ToggleClassRec toggleClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &knappClassRec,   /* superclass                   */
        "Toggle",			/* class_name                   */
        sizeof(ToggleRec),		/* widget_size                  */
        NULL,				/* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        NULL,				/* initialize                   */
        NULL,                           /* initialize_hook              */
        XtInheritRealize,               /* realize                      */
        actions,                        /* actions                      */
        XtNumber(actions),              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
#if (XtSpecificationRelease < 4)
	True,				/* compress exposure		*/
#elif (XtSpecificationRelease < 6)
        XtExposeCompressMaximal,	/* compress_exposure		*/
#else
        XtExposeCompressMaximal | XtExposeNoRegion, /* compress_exposure*/
#endif
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        NULL,                           /* destroy                      */
        XtInheritResize,                /* resize                       */
        Redisplay,                      /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        translations,                   /* tm_table                     */
        XtInheritQueryGeometry,         /* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    {					/* shadow fields		*/
	XtInheritPixelOffset,		/* pixel_offset			*/
	False,				/* use_arm_for_background	*/
	XtInheritAllocShadowColors,	/* alloc_shadow_colors		*/
	XtInheritAllocShadowPixmaps,	/* alloc_shadow_pixmaps		*/
	XtInheritAllocArmColor,		/* alloc_arm_color		*/
	XtInheritAllocArmPixmap,	/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    {                                   /* knapp fields                 */
        NULL,                           /* extension                    */
    },
    {					/* toggle fields		*/
	NULL,				/* extension			*/
    }
};

WidgetClass toggleWidgetClass = (WidgetClass)&toggleClassRec;

/*************************************************************************/

static void draw_toggle(ToggleWidget w)
{
    Dimension	sw = w->shadow.shadow_width;
    int		x, y;

    x = w->toggle.toggle_offset + w->shadow.shadow_width;
    y = w->core.height - w->toggle.toggle_size;
    y /= 2;
    if (w->shadow.arm_gc != 0) {
	Display	*disp = XtDisplay(w);
	Window	win = XtWindow(w);

	if (w->toggle.set)
	    XFillRectangle(disp, win, w->shadow.arm_gc, x, y,
			   w->toggle.toggle_size, w->toggle.toggle_size);
	else
	    XClearArea(disp, win, x, y, w->toggle.toggle_size,
		       w->toggle.toggle_size, False);
    }

    w->shadow.shadow_width = w->toggle.toggle_shadow_width;
    ShadowDrawShadows((ShadowWidget)w, x, y, w->toggle.toggle_size,
		      w->toggle.toggle_size, w->toggle.set);
    w->shadow.shadow_width = sw;
}

static void call_callbacks(ToggleWidget w)
{
    XtCallbackList	c_list = w->knapp.callback;
    Boolean		set = w->toggle.set;

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)&set);
}

/*************************************************************************/

static void toggle(Widget gw, XEvent *event,
		   String *params, Cardinal *no_params)
{
    ToggleWidget	w = (ToggleWidget)gw;

    w->toggle.set = !w->toggle.set;
    draw_toggle(w);
    call_callbacks(w);
}

static void set(Widget gw, XEvent *event,
		String *params, Cardinal *no_params)
{
    ToggleWidget	w = (ToggleWidget)gw;

    w->toggle.set = True;
    draw_toggle(w);
    call_callbacks(w);
}

static void reset(Widget gw, XEvent *event,
		  String *params, Cardinal *no_params)
{
    ToggleWidget	w = (ToggleWidget)gw;

    w->toggle.set = False;
    draw_toggle(w);
    call_callbacks(w);
}

static void notify(Widget gw, XEvent *event,
		   String *params, Cardinal *no_params)
{
    ToggleWidget	w = (ToggleWidget)gw;
    XtCallbackList	c_list = w->knapp.callback;

    if (c_list) {
	Boolean	set = w->toggle.set;

	/* callbacks may change 'set' */
	XtCallCallbackList((Widget)w, c_list, (XtPointer)&w->toggle.set);
	if (set != w->toggle.set)
	    draw_toggle(w);
    }
}

/*************************************************************************/

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    ToggleWidget	w = (ToggleWidget)gw;

    knappWidgetClass->core_class.expose((Widget)w, NULL, NULL);

    if (w->toggle.toggle_shadow_width > 0)
	draw_toggle(w);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    ToggleWidget	new = (ToggleWidget)gnew;
    ToggleWidget	current = (ToggleWidget)gcurrent;
    Boolean		redisplay = False;

    if (new->toggle.set != current->toggle.set ||
	new->toggle.toggle_shadow_width !=
	current->toggle.toggle_shadow_width ||
	new->toggle.toggle_size != current->toggle.toggle_size ||
	new->toggle.toggle_offset != current->toggle.toggle_offset)
	redisplay = True;

    return redisplay;
}

/*************************************************************************/

void ToggleSet(Widget gw, int set)
{
    ToggleWidget	w = (ToggleWidget)gw;

    set = !!set;
    if (w->toggle.set == set)
	return;

    w->toggle.set = set;
    if (XtIsRealized((Widget)w))
	draw_toggle(w);
}

int ToggleGet(Widget gw)
{
    ToggleWidget	w = (ToggleWidget)gw;

    return w->toggle.set;
}
