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
#include <stdio.h>

#include "Compat.h"
#include "MenuP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(MenuRec, menu.field)
    {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(cursor), XtRImmediate, (XtPointer)None},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static void	Resize(Widget);
static void	Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static void	Redisplay(Widget, XEvent*, Region);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);

static void	motion(Widget, XEvent*, String*, Cardinal*);
static void	leave(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"motion",	motion},
    {"leave",	leave},
};

static char translations[] =
"<Motion>:	motion() \n"
"<LeaveWindow>:	leave() \n";

MenuClassRec menuClassRec = {
    { /* core fields */
    	(WidgetClass) &shadowClassRec,		/* superclass		*/
    	"Menu",					/* class_name		*/
    	sizeof(MenuRec),			/* widget_size		*/
    	NULL,					/* class_initialize	*/
    	NULL,					/* class_part_initialize*/
    	FALSE,					/* class_inited		*/
    	Initialize,				/* initialize		*/
    	NULL,					/* initialize_hook	*/
    	Realize,				/* realize		*/
    	actions,				/* actions		*/
    	XtNumber(actions),			/* num_actions		*/
    	resources,				/* resources		*/
    	XtNumber(resources),			/* resource_count	*/
    	NULLQUARK,				/* xrm_class		*/
    	TRUE,					/* compress_motion	*/
#if (XtSpecificationRelease < 4)
        TRUE,					/* compress_exposure    */
#elif (XtSpecificationRelease < 6)
	XtExposeCompressMaximal,		/* compress_exposure	*/
#else
	XtExposeCompressMaximal | XtExposeNoRegion, /* compress_exposure*/
#endif
    	TRUE,					/* compress_enterleave	*/
    	TRUE,					/* visible_interest	*/
    	Destroy,				/* destroy		*/
    	Resize,					/* resize		*/
    	Redisplay,				/* expose		*/
    	SetValues,				/* set_values		*/
    	NULL,					/* set_values_hook	*/
    	XtInheritSetValuesAlmost,		/* set_values_almost	*/
    	NULL,					/* get_values_hook	*/
    	NULL,					/* accept_focus		*/
    	XtVersion,				/* version		*/
    	NULL,					/* callback_private	*/
    	translations,				/* tm_table		*/
    	QueryGeometry,				/* query_geometry       */
    	XtInheritDisplayAccelerator,		/* display_accelerator  */
    	NULL					/* extension            */
    },
    { /* shadow fields */
	XtInheritPixelOffset,		/* pixel offset			*/
	False,				/* use_arm_for_background	*/
	XtInheritAllocShadowColors,	/* alloc_shadow_colors		*/
	XtInheritAllocShadowPixmaps,	/* alloc_shadow_pixmaps		*/
	XtInheritAllocArmColor,		/* alloc_arm_color		*/
	XtInheritAllocArmPixmap,	/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    { /* menu fields */
	NULL					/* extension            */
    }
};

WidgetClass menuWidgetClass = (WidgetClass) &menuClassRec;

/*************************************************************************/

static MenuGadget y_to_gadget(MenuWidget w, int y)
{
    int	i;

    for (i = 0 ; i < w->menu.num_children ; i++) {
	MenuGadget	g = w->menu.children[i];

	if ((unsigned int)(y - g->rectangle.y) < g->rectangle.height)
	    return g;
    }

    return NULL;
}

static void change_hl(MenuWidget w, MenuGadget g, int hl)
{
    if (!XtIsSensitive((Widget)g))
	hl = False;
    g->menu_g.hl = hl;
    XClearArea(XtDisplay(w), XtWindow(w), g->rectangle.x, g->rectangle.y,
	       g->rectangle.width, g->rectangle.height, False);
    if (g->object.widget_class->core_class.expose)
	g->object.widget_class->core_class.expose((Widget)g, NULL, 0);
    MenuGadgetChangeHlProc(g)(g);
}

static void motion(Widget	 gw,
		   XEvent	*event,
		   String	*params,
		   Cardinal	*no_params)
{
    MenuWidget	w = (MenuWidget)gw;
    MenuGadget	old, new;

    if (!w->menu.active)
	return;

    if (event->type != MotionNotify) {
	fputs("Menu: Action highlight() only supported "
	      "for Motion events.\n", stderr);
	return;
    }

    old = w->menu.current;
    new = y_to_gadget(w, event->xmotion.y);
    if (new)
	new->menu_g.inside = True;
    if (old != new) {
	if (old)
	    change_hl(w, old, False);
	if (new)
	    change_hl(w, new, True);
	w->menu.current = new;
    }
}

static void leave(Widget	 gw,
		  XEvent	*event,
		  String	*params,
		  Cardinal	*no_params)
{
    MenuWidget	w = (MenuWidget)gw;

    if (!w->menu.current || !w->menu.active)
	return;

    w->menu.current->menu_g.inside = False;
    if (MenuGadgetIgnoreLeave(w->menu.current))
	return;

    change_hl(w, w->menu.current, False);
    w->menu.current = NULL;
}

static void layout(MenuWidget w)
{
    MenuGadget		*loop;
    int			n = w->menu.num_children;
    int			sw = w->shadow.shadow_width;
    int			i, y, width = 0;

    for (y = sw, i = 0, loop = w->menu.children ; i < n ; i++, loop++) {
	if (!XtIsSubclass((Widget)*loop, menuGadgetClass))
	    fputs("Menu: Child of MenuWidget must be "
		  "subclass of MenuGadgetClass.", stderr);

	y += (*loop)->rectangle.height;
	if (width < (int)(*loop)->rectangle.width)
	    width = (int)(*loop)->rectangle.width;
    }
    y += sw;
    w->menu.pref_width = (Dimension)(width + 2 * sw);
    w->menu.pref_height = (Dimension)y;
    (void)XtMakeResizeRequest((Widget)w, w->menu.pref_width,
			      w->menu.pref_height, NULL, NULL);

    for (y = sw, i = 0, loop = w->menu.children ; i < n ; i++, loop++) {
	XtConfigureWidget((Widget)*loop, sw, y,
			  width, (*loop)->rectangle.height, 0);
	y += (*loop)->rectangle.height;
    }
}

/*************************************************************************/

static void Initialize (Widget grequest, Widget gnew,
			ArgList args, Cardinal *num_args)
{
    MenuWidget	new = (MenuWidget)gnew;

    new->menu.children     = NULL;
    new->menu.num_children = 0;
    new->menu.num_slots    = 0;
    new->menu.current      = NULL;
    new->menu.active       = True;
    layout(new);
}

static void Destroy(Widget gw)
{
    MenuWidget	w = (MenuWidget)gw;
    int		i;

    for (i = 0 ; i < w->menu.num_children ; i++)
	XtDestroyWidget((Widget)w->menu.children[i]);
}

static void Resize (Widget gw)
{
    /* we never expect to be resized */
}

static void Redisplay(Widget gw, XEvent *event, Region reg)
{
    MenuWidget	w = (MenuWidget)gw;
    int		n = w->menu.num_children;
    int		i, y1, y2;

    if (event)
	switch (event->type) {
	case Expose:
	    y1 = event->xexpose.y;
	    y2 = y1 + event->xexpose.height;
	    break;
	case GraphicsExpose:
	    y1 = event->xgraphicsexpose.y;
	    y2 = y1 + event->xgraphicsexpose.height;
	    break;
	default:
	    return;
	}
    else {
	y1 = 0;
	y2 = w->menu.pref_height;
    }
    
    for (i = 0 ; i < n ; i++) {
	MenuGadget	g = w->menu.children[i];

	if (g->rectangle.y <= y2 &&
	    y1 <= g->rectangle.y + (int)g->rectangle.height &&
	    g->object.widget_class->core_class.expose)
	    g->object.widget_class->core_class.expose((Widget)g, event, reg);

    }

    ShadowDrawShadows((ShadowWidget)w, 0, 0,
		      w->menu.pref_width, w->menu.pref_height, False);
}

static void Realize(Widget gw, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    MenuWidget	w = (MenuWidget)gw;

    if (w->menu.cursor != None) {
	*mask |= CWCursor;
	attributes->cursor = w->menu.cursor;
    }

    shadowWidgetClass->core_class.realize((Widget)w, mask, attributes);
}

static Boolean SetValues(Widget gcurrent, Widget grequest, Widget gnew,
			 ArgList args, Cardinal *num_args)
{
    Boolean	redisplay = False;
    MenuWidget	new = (MenuWidget)gnew;
    MenuWidget	current = (MenuWidget)gcurrent;

    if (XtIsRealized((Widget)new) && new->menu.cursor != current->menu.cursor)
	XDefineCursor(XtDisplay(new), XtWindow(new), new->menu.cursor);

    return redisplay;
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    MenuWidget	w = (MenuWidget)gw;
    Dimension	intended_width, intended_height;

    if (intended->request_mode & CWWidth)
	intended_width = intended->width;
    else
	intended_width = w->core.width;
    if (intended->request_mode & CWHeight)
	intended_height = intended->height;
    else
	intended_height = w->core.height;

    preferred->request_mode = CWWidth | CWHeight;
    preferred->width = w->menu.pref_width;
    preferred->height = w->menu.pref_height;

    if (preferred->width == w->core.width &&
	preferred->height == w->core.height)
	return XtGeometryNo;
    if (preferred->width == intended_width &&
	preferred->height == intended_height)
	return XtGeometryYes;
    return XtGeometryAlmost;
}

/*************************************************************************/

Widget MenuCreateGadget(String		name,
			WidgetClass	class,
			Widget		parent,
			ArgList		args,
			Cardinal	n)
{
    MenuWidget	w = (MenuWidget)parent;
    MenuGadget	child;

    if (w->menu.num_children >= w->menu.num_slots) {
	w->menu.num_slots = 2 * (w->menu.num_slots + 1);
	w->menu.children =
	    (MenuGadget *)XtRealloc((char *)w->menu.children,
				    w->menu.num_slots * sizeof(MenuGadget));
    }

    child = (MenuGadget)XtCreateWidget(name, class, (Widget)w, args, n);
    if (!XtIsSubclass((Widget)child, menuGadgetClass)) {
	fputs("MenuWidgetClass accepts children "
	      "that are subclass of MenuGadgetClass only.", stderr);
	exit(1);
    }
    w->menu.children[w->menu.num_children] = child;
    w->menu.num_children++;
    layout(w);

    return (Widget)child;
}

void PopdownMenu(Widget gw)
{
    MenuWidget	w = (MenuWidget)gw;
    MenuGadget	g = w->menu.current;

    w->menu.active = True;
    if (!g)
	return;

    if (g->menu_g.hl) {
	g->menu_g.hl = False;
	MenuGadgetChangeHlProc(g)(g);
    }
    MenuGadgetPopdownProc(g)(g);
}

int NotifyMenu(Widget gw)
{
    MenuWidget	w = (MenuWidget)gw;

    w->menu.active = False;

    if (!w->menu.current)
	return False;

    return MenuGadgetNotifyProc(w->menu.current)(w->menu.current);
}

int PostNotifyMenu(Widget gw)
{
    MenuWidget	w = (MenuWidget)gw;
    int		tmp;

    if (!w->menu.current)
	return False;

    tmp = MenuGadgetPostNotifyProc(w->menu.current)(w->menu.current);
    w->menu.current = NULL;

    return tmp;
}

void SetActiveMenu(Widget gw, int active)
{
    MenuWidget	w = (MenuWidget)gw;
    int		i;

    w->menu.active = active;
    for (i = 0 ; i < w->menu.num_children ; i++) {
	MenuGadget	g = w->menu.children[i];

	MenuGadgetSetActiveProc(g)(g, active);
    }
}
