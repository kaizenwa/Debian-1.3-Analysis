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
#include "SeparatoGP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(SeparatorGadgetRec, separator_g.field)
    {XtNsize, XtCSize, XtRDimension, sizeof(Dimension),
     offset(size), XtRImmediate, (XtPointer)4},
    {XtNinternalWidth, XtCInternalWidth, XtRDimension, sizeof(Dimension),
     offset(internal_width), XtRImmediate, (XtPointer)2},
    {XtNinternalHeight, XtCInternalHeight, XtRDimension, sizeof(Dimension),
     offset(internal_height), XtRImmediate, (XtPointer)1},
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     offset(shadow_width), XtRImmediate, (XtPointer)1},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Redisplay(Widget, XEvent*, Region);

SeparatorGadgetClassRec separatorGadgetClassRec = {
    { /* rectObj fields */
        (WidgetClass) &menuGadgetClassRec, /* superclass                */
        "SeparatorGadget",              /* class_name                   */
        sizeof(SeparatorGadgetRec),	/* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,				/* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
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
        NULL,                           /* set_values                   */
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
        XtInheritChangeHl,		/* change_highlighted		*/
	XtInheritPopdown,		/* popdown			*/
	XtInheritNotify,		/* notify			*/
	XtInheritPostNotify,		/* post_notify			*/
	XtInheritSetActive,		/* set_active			*/
	True,				/* unhighlight_on_leave		*/
	NULL,				/* extension			*/
    }
};

WidgetClass separatorGadgetClass = (WidgetClass)&separatorGadgetClassRec;

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    SeparatorGadget	new = (SeparatorGadget)gnew;

    new->rectangle.height = new->separator_g.size;
    new->rectangle.width = 2 * new->separator_g.internal_width;
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    SeparatorGadget	g = (SeparatorGadget)gw;
    ShadowWidget	parent = (ShadowWidget)g->object.parent;
    int			x, width, y, height, sw, old_sw;

    x = g->rectangle.x + g->separator_g.internal_width;
    width = g->rectangle.width - 2 * g->separator_g.internal_width;
    y = g->rectangle.y + g->separator_g.internal_height;
    height = g->rectangle.height - 2 * g->separator_g.internal_height;
    sw = g->separator_g.shadow_width;

    if (width <= 0 || height <= 0)
	return;

    old_sw = parent->shadow.shadow_width;
    parent->shadow.shadow_width = sw;
    ShadowDrawShadows(parent, x, y, width, height, !parent->shadow.line_mode);
    parent->shadow.shadow_width = old_sw;

    width -= 2 * sw;
    height -= 2 * sw;
    if (width <= 0 || height <= 0 || parent->shadow.arm_gc == 0)
	return;

    XFillRectangle(XtDisplay(parent), XtWindow(parent), 
		   parent->shadow.arm_gc, x + sw, y + sw, width, height);
}
