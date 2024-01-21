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
#include "Util.h"
#include "StringGP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(StringGadgetRec, string_g.field)
    {XtNcommand, XtCCommand, XtRString, sizeof(String),
     offset(command), XtRImmediate, (XtPointer)NULL},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, XtDefaultFont},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, XtDefaultForeground},
    {XtNleftMargin, XtCMargin, XtRDimension, sizeof(Dimension),
     offset(left_margin), XtRImmediate, (XtPointer)24},
    {XtNrightMargin, XtCMargin, XtRDimension, sizeof(Dimension),
     offset(right_margin), XtRImmediate, (XtPointer)32},
    {XtNinternalHeight, XtCInternalHeight, XtRDimension, sizeof(Dimension),
     offset(internal_height), XtRImmediate, (XtPointer)2},
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     offset(shadow_width), XtRImmediate, (XtPointer)2},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void	Redisplay(Widget, XEvent*, Region);

StringGadgetClassRec stringGadgetClassRec = {
    { /* rectObj fields */
        (WidgetClass) &menuGadgetClassRec, /* superclass                */
        "StringGadget",                 /* class_name                   */
        sizeof(StringGadgetRec),        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
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
        Destroy,    			/* destroy                      */
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
	XtInheritNotify,		/* notify			*/
	XtInheritPostNotify,		/* post_notify			*/
	XtInheritSetActive,		/* set_actvive			*/
	False,				/* ignore_leave			*/
	NULL,				/* extension			*/
    },
    { /* string_g fields */
	NULL,				/* extension			*/
    },
};

WidgetClass stringGadgetClass = (WidgetClass)&stringGadgetClassRec;

/*************************************************************************/

static void set_preferred_size(StringGadget g)
{
    g->rectangle.width = g->string_g.left_margin +
	g->string_g.right_margin + 2 * g->string_g.shadow_width;

    g->rectangle.width +=
	XTextWidth(g->string_g.font, g->menu_g.label, strlen(g->menu_g.label));

    g->rectangle.height =
	2 * (g->string_g.internal_height + g->string_g.shadow_width) +
	g->string_g.font->ascent + g->string_g.font->descent;
}

static void init_gcs(StringGadget g)
{
    XGCValues	values;

    values.font = g->string_g.font->fid;
    values.foreground = g->string_g.foreground_pixel;
    values.stipple = g->string_g.stipple;
    values.fill_style =  FillStippled;
    g->string_g.default_gc =
	XtGetGC(g->object.parent, GCForeground | GCFont, &values);
    g->string_g.gray_gc =
	XtGetGC(g->object.parent,
		GCForeground | GCFont | GCFillStyle | GCStipple,
		&values);
}

static void free_gcs(StringGadget g)
{
    XtReleaseGC(g->object.parent, g->string_g.default_gc);
    XtReleaseGC(g->object.parent, g->string_g.gray_gc);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    StringGadget	new = (StringGadget)gnew;

    set_preferred_size(new);
    new->string_g.stipple = create_stipple(XtScreen(new->object.parent));
    init_gcs(new);
}

static void Destroy(Widget gw)
{
    StringGadget	g = (StringGadget)gw;

    release_stipple(XtScreen(g->object.parent), g->string_g.stipple);
    free_gcs(g);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    StringGadget	g = (StringGadget)gw;
    ShadowWidget	parent = (ShadowWidget)g->object.parent;
    Display		*disp = XtDisplay(parent);
    Window		win = XtWindow(parent);

    XDrawString(disp, win,
		XtIsSensitive((Widget)g) ?
		g->string_g.default_gc : g->string_g.gray_gc,
		g->rectangle.x + g->string_g.left_margin +
		g->string_g.shadow_width,
		g->rectangle.y + (int)(g->rectangle.height +
				       g->string_g.font->ascent -
				       g->string_g.font->descent) / 2,
		g->menu_g.label, strlen(g->menu_g.label));

    if (g->menu_g.hl) {
	int	old_sw = parent->shadow.shadow_width;

	parent->shadow.shadow_width = g->string_g.shadow_width;
	ShadowDrawShadows(parent, g->rectangle.x, g->rectangle.y,
			  g->rectangle.width, g->rectangle.height, False);
	parent->shadow.shadow_width = old_sw;
    }
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean		redisplay = False;
    StringGadget	new = (StringGadget)gnew;
    StringGadget	current = (StringGadget)gcurrent;

    if (new->string_g.font             != current->string_g.font ||
	new->string_g.foreground_pixel != current->string_g.foreground_pixel) {
	free_gcs(current);
	init_gcs(new);
	redisplay = True;
    }

    return redisplay;
}

/*********************************************************************/

char *StringGadgetCommand(Widget gw)
{
    StringGadget	w = (StringGadget)gw;

    return w->string_g.command;
}
