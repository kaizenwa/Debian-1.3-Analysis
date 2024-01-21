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

#include "MenuI.h"
#include "PullRightP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(PullRightGadgetRec, pull_right.field)
    {XtNmenuName, XtCMenuName, XtRString, sizeof(String),
     offset(menu_name), XtRImmediate, (XtPointer)NULL},
    {XtNarrowSize, XtCArrowSize, XtRDimension, sizeof(Dimension),
     offset(arrow_size), XtRImmediate, (XtPointer)6},
    {XtNarrowOffset, XtCArrowOffset, XtRDimension, sizeof(Dimension),
     offset(arrow_offset), XtRImmediate, (XtPointer)8},
    {XtNarrowShadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     offset(arrow_shadow_width), XtRImmediate, (XtPointer)1},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void	Redisplay(Widget, XEvent*, Region);
static void	ChangeHl(MenuGadget);
static void	Popdown(MenuGadget);
static int	Notify(MenuGadget);
static int	PostNotify(MenuGadget);
static void	SetActive(MenuGadget, int);

PullRightGadgetClassRec pullRightGadgetClassRec = {
    { /* rectObj fields */
        (WidgetClass) &stringGadgetClassRec, /* superclass              */
        "PullRightGadget",              /* class_name                   */
        sizeof(PullRightGadgetRec),     /* widget_size                  */
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
        ChangeHl,			/* change_highlighted		*/
	Popdown,			/* popdown			*/
	Notify,				/* notify			*/
	PostNotify,			/* post_notify			*/
	SetActive,			/* set_active			*/
	True,				/* ignore_leave			*/
	NULL,				/* extension			*/
    },
    { /* menu_str_g fields */
	NULL,				/* extension			*/
    },
    { /* pull_right fields */
	NULL,				/* extension			*/
    },
};

WidgetClass pullRightGadgetClass = (WidgetClass)&pullRightGadgetClassRec;

/*************************************************************************/

static void draw_arrow(PullRightGadget g)
{
    MenuWidget	parent = (MenuWidget)g->object.parent;
    Display	*disp = XtDisplay(parent);
    Window	win = XtWindow(parent);
    int		s = g->pull_right.arrow_size;
    int		x, y;

    if (s <= 0)
	return;

    x = g->rectangle.x + g->rectangle.width -
	g->pull_right.arrow_offset - parent->shadow.shadow_width;
    y = g->rectangle.y + g->rectangle.height/2;

    if (parent->shadow.line_mode) {
	short	sw = parent->shadow.shadow_width;
	XPoint	point[4];

	XClearArea(disp, win, x - s - sw, y - s/2 - sw,
		   s + 2 * sw, s + 2 * sw, False);

	if (!g->menu_g.hl)
	    sw = 0;
	else {
	    sw /= 2;
	    s -= sw;
	}

	point[0].x = point[3].x = x - sw/2;
	point[1].x = point[2].x = x - s;

	point[0].y = point[3].y = y;
	point[1].y = y - s/2;
	point[2].y = y + s/2;

	XDrawLines(disp, win,
		   !g->menu_g.hl ?
		   parent->shadow.light_gc : parent->shadow.dark_gc,
		   point, 4, CoordModeOrigin);
    } else {
	XPoint	point[6];
	short	sw = g->pull_right.arrow_shadow_width;
	GC	light_gc, dark_gc;

	if (g->menu_g.hl) {
	    light_gc = parent->shadow.dark_gc;
	    dark_gc = parent->shadow.light_gc;
	} else {
	    light_gc = parent->shadow.light_gc;
	    dark_gc = parent->shadow.dark_gc;
	}

	if (sw == 0)
	    return;
	else if (sw == 1) {
	    point[0].x = point[3].x = x;
	    point[1].x = point[2].x = x - s;

	    point[0].y = point[3].y = y;
	    point[1].y = y - s/2;
	    point[2].y = y + s/2;

	    if (!g->menu_g.hl && parent->shadow.arm_gc != 0)
		XFillPolygon(disp, win, parent->shadow.arm_gc,
			     point, 3, Convex, CoordModeOrigin);

	    XDrawLines(disp, win, dark_gc, point, 3, CoordModeOrigin);
	    XDrawLines(disp, win, light_gc, &point[2], 2, CoordModeOrigin);
	} else {
	    point[0].x = x;
	    point[1].x = point[2].x = x - s;
	    point[3].x = point[4].x = x - s + sw;
	    point[5].x = x - (9 * sw)/4;

	    point[0].y = point[5].y = y;
	    point[1].y = y - s/2;
	    point[2].y = y + s/2;
	    point[3].y = point[2].y - (3 * sw)/2;
	    point[4].y = point[1].y + (3 * sw)/2;

	    if (!g->menu_g.hl && parent->shadow.arm_gc != 0)
		XFillPolygon(disp, win, parent->shadow.arm_gc,
			     &point[3], 3, Convex, CoordModeOrigin);

	    XFillPolygon(disp, win, dark_gc,
			 point, 6, Nonconvex, CoordModeOrigin);
	    point[1] = point[0];
	    point[4] = point[5];
	    XFillPolygon(disp, win, light_gc,
			 &point[1], 4, Convex, CoordModeOrigin);
	}
    }
}

static Widget find_menu(PullRightGadget g)
{
    if (g->pull_right.menu_name) {
	Widget loop;

	for (loop = g->object.parent ; loop ; loop = XtParent(loop)) {
	    Widget	temp = XtNameToWidget(loop, g->pull_right.menu_name);

	    if (temp && XtIsShell(temp))
		return temp;
	}
    }

    return NULL;
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    PullRightGadget	g = (PullRightGadget)gnew;

    if (!g->pull_right.menu_name)
	g->pull_right.menu = NULL;
    else {
	g->pull_right.menu_name = XtNewString(g->pull_right.menu_name);
	g->pull_right.menu = find_menu(g);
    }
}

static void Destroy(Widget gw)
{
    PullRightGadget	w = (PullRightGadget)gw;

    XtFree(w->pull_right.menu_name);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    PullRightGadget	w = (PullRightGadget)gw;

    stringGadgetClass->core_class.expose((Widget)w, event, region);

    draw_arrow(w);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean		redisplay = False;
    PullRightGadget	new = (PullRightGadget)gnew;
    PullRightGadget	current = (PullRightGadget)gcurrent;

    if (new->pull_right.menu_name != current->pull_right.menu_name) {
	XtFree(current->pull_right.menu_name);
	if (!new->pull_right.menu_name)
	    new->pull_right.menu = NULL;
	else {
	    new->pull_right.menu_name = XtNewString(new->pull_right.menu_name);
	    new->pull_right.menu = find_menu(new);
	}
    }

    return redisplay;
}

static void ChangeHl(MenuGadget m)
{
    PullRightGadget	g      = (PullRightGadget)m;
    Widget		parent = g->object.parent;
    Widget		menu   = g->pull_right.menu;
    Screen		*screen;
    Arg			args[2];
    Position		x, y;

    Redisplay((Widget)g, NULL, NULL);

    if (!menu && !(menu = g->pull_right.menu = find_menu(g))) {
	fprintf(stderr, "PullRightGadget: Couldn't find menu: %s\n",
		g->pull_right.menu_name);
	return;
    }

    /*
     *  Unhighlight.
     */
    if (!g->menu_g.hl) {
	PopdownMenuShell(menu);
	return;
    }

    screen = XtScreen(menu);
    if (!XtIsRealized(menu))
	XtRealizeWidget(menu);
    XtTranslateCoords(parent, parent->core.width, g->rectangle.y, &x, &y);
    if (x < 0)
	x = 0;
    else {
	int	tmp = WidthOfScreen(screen) - menu->core.width;

	if (tmp >= 0 && x > tmp)
	    x = tmp;
    }
    if (y < 0)
	y = 0;
    else {
	int	tmp = HeightOfScreen(screen) - menu->core.height;

	if (tmp >= 0 && y > tmp)
	    y = tmp;
    }
    XtSetArg(args[0], XtNx, x);
    XtSetArg(args[1], XtNy, y);
    XtSetValues(menu, args, 2);
    XtPopup(menu, XtGrabNone/*xclusive*/);
    XtAddGrab(menu, False, False);
}

static void Popdown(MenuGadget m)
{
    PullRightGadget	g = (PullRightGadget)m;

    if (g->pull_right.menu)
	PopdownMenuShell(g->pull_right.menu);
}

static int Notify(MenuGadget m)
{
    PullRightGadget	g = (PullRightGadget)m;

    if (!g->pull_right.menu || g->menu_g.inside) {
	XtCallCallbackList((Widget)g, g->menu_g.callback, NULL);
	return True;
    }

    return NotifyMenuShell(g->pull_right.menu);
}

static int PostNotify(MenuGadget m)
{
    PullRightGadget	g = (PullRightGadget)m;

    if (!g->pull_right.menu || g->menu_g.inside) {
	XtCallCallbackList((Widget)g, g->menu_g.post_popdown_callback, NULL);
	return True;
    }

    return PostNotifyMenuShell(g->pull_right.menu);
}

static void SetActive(MenuGadget m, int active)
{
    PullRightGadget	g = (PullRightGadget)m;

    g->menu_g.active = active;
    if (g->pull_right.menu)
	SetActiveMenuShell(g->pull_right.menu, active);
}
