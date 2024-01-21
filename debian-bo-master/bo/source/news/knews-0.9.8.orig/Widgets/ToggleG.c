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
#include "ToggleGP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(ToggleGadgetRec, toggle_g.field)
    {XtNtoggleSize, XtCToggleSize, XtRDimension, sizeof(Dimension),
     offset(toggle_size), XtRImmediate, (XtPointer)10},
    {XtNtoggleOffset, XtCToggleOffset, XtRDimension, sizeof(Dimension),
     offset(toggle_offset), XtRImmediate, (XtPointer)6},
    {XtNtoggleShadowWidth, XtCToggleShadowWidth,
     XtRDimension, sizeof(Dimension),
     offset(toggle_shadow_width), XtRImmediate, (XtPointer)2},
    {XtNset, XtCSet, XtRBoolean, sizeof(Boolean),
     offset(set), XtRImmediate, (XtPointer)False},
#undef offset
};

static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void	Redisplay(Widget, XEvent*, Region);
static int	Notify(MenuGadget);

ToggleGadgetClassRec toggleGadgetClassRec = {
    { /* rectObj fields */
        (WidgetClass) &stringGadgetClassRec, /* superclass                */
        "ToggleGadget",                 /* class_name                   */
        sizeof(ToggleGadgetRec),        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        NULL,				/* initialize                   */
        NULL,                           /* initialize_hook              */
        NULL,                           /* rect1                        */
        NULL,                           /* rect2                        */
        0,                              /* rect3                        */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        FALSE,                          /* rect4                        */
        FALSE,                          /* rect5                        */
        FALSE,                          /* rect6                        */
        FALSE,                          /* rect7                        */
        NULL,    			/* destroy                      */
        NULL,                           /* resize                       */
        Redisplay,                      /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* rect9                        */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        NULL,                           /* rect10                       */
        NULL,                           /* query_geometry               */
        NULL,                           /* rect11                       */
        NULL,                           /* extension                    */
    },
    { /* menu_g fields */
        XtInheritChangeHl,		/* change_hl			*/
	XtInheritPopdown,		/* popdown			*/
	Notify,				/* notify			*/
	XtInheritPostNotify,		/* post_notify			*/
	XtInheritSetActive,		/* set_active			*/
	False,				/* ignore_leave			*/
	NULL,				/* extension			*/
    },
    { /* string_g fields */
	NULL,				/* extension			*/
    },
    { /* toggle_g fields */
	NULL,				/* extension			*/
    },
};

WidgetClass toggleGadgetClass = (WidgetClass)&toggleGadgetClassRec;

/*************************************************************************/

static void draw_toggle(ToggleGadget g, int clear)
{
    ShadowWidget	parent = (ShadowWidget)g->object.parent;
    Display		*disp = XtDisplay(parent);
    Window		win = XtWindow(parent);
    int			sw = parent->shadow.shadow_width;
    int			x, y, size = g->toggle_g.toggle_size;

    x = g->rectangle.x + g->toggle_g.toggle_offset + g->string_g.shadow_width;
    y = g->rectangle.y + (int)(g->rectangle.height - size) / 2;

    if (clear)
	XClearArea(disp, win, x, y, size, size, False);

    if (g->toggle_g.set && parent->shadow.arm_gc != 0)
	XFillRectangle(disp, win, parent->shadow.arm_gc, x, y, size, size);

    parent->shadow.shadow_width = g->toggle_g.toggle_shadow_width;
    ShadowDrawShadows(parent, x, y, size, size, g->toggle_g.set);
    parent->shadow.shadow_width = sw;
}

/*************************************************************************/

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    ToggleGadget	g = (ToggleGadget)gw;

    stringGadgetClass->core_class.expose((Widget)g, event, region);

    draw_toggle(g, False);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean		redisplay = False;

    return redisplay;
}

static int Notify(MenuGadget m)
{
    ToggleGadget	g      = (ToggleGadget)m;
    XtCallbackList	c_list = g->menu_g.callback;
    Boolean		set    = !g->toggle_g.set;

    if (!g->menu_g.inside)
	return False;

    g->toggle_g.set = set;
    draw_toggle(g, !set);
    if (c_list)
	XtCallCallbackList((Widget)g, c_list, (XtPointer)&set);

    return True;
}

/*************************************************************************/

void ToggleGadgetSet(Widget w, int set)
{
    ToggleGadget	g = (ToggleGadget)w;
    Widget		parent = g->object.parent;

    if (g->toggle_g.set == set)
	return;

    g->toggle_g.set = set;
    if (XtIsRealized(parent) && parent->core.visible) {
	XClearArea(XtDisplay(parent), XtWindow(parent),
		   g->rectangle.x, g->rectangle.y,
		   g->rectangle.width, g->rectangle.height, False);
    }
}

int ToggleGadgetGet(Widget w)
{
    ToggleGadget	g = (ToggleGadget)w;

    return g->toggle_g.set;
}
