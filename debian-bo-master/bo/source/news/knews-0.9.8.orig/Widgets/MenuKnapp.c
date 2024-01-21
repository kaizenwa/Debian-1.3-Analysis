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
#include <X11/Shell.h>
#include <stdio.h>

#include "Compat.h"

#include "MenuI.h"
#include "MenuKnappP.h"
#include "MenuShell.h"

static XtResource resources[] = {
    {XtNrightMargin, XtCRightMargin, XtRDimension, sizeof(Dimension),
     XtOffsetOf(MenuKnappRec, knapp.right_margin),
     XtRImmediate, (XtPointer)24},
#define offset(field) XtOffsetOf(MenuKnappRec, menu_knapp.field)
    {XtNmenuName, XtCMenuName, XtRString, sizeof(String),
     offset(menu_name), XtRImmediate, (XtPointer)NULL},
    {XtNarrowSize, XtCArrowSize, XtRDimension, sizeof(Dimension),
     offset(arrow_size), XtRImmediate, (XtPointer)6},
    {XtNarrowOffset, XtCArrowOffset, XtRDimension, sizeof(Dimension),
     offset(arrow_offset), XtRImmediate, (XtPointer)8},
    {XtNarrowShadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     offset(arrow_shadow_width), XtRImmediate, (XtPointer)1},
    {XtNmultiClickTime, XtCMultiClickTime, XtRInt, sizeof(int),
     offset(multi_click_time), XtRImmediate, (XtPointer)200},
    {XtNpopdownTime, XtCMultiClickTime, XtRInt, sizeof(int),
     offset(popdown_time), XtRImmediate, (XtPointer)200},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Redisplay(Widget, XEvent*, Region);
static void	Destroy(Widget);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void	popup_and_grab(Widget, XEvent*, String*, Cardinal*);
static void	popdown_and_notify_if(Widget, XEvent*, String*, Cardinal*);
static void	set_unless(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"popup-and-grab",		popup_and_grab},
    {"popdown-and-notify-if",	popdown_and_notify_if},
    {"set-unless",		set_unless},
};

static char translations[] =
"<BtnDown>:	popup-and-grab() \n"
"<BtnUp>:	popdown-and-notify-if() \n";

MenuKnappClassRec menuKnappClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &knappClassRec,   /* superclass                   */
        "MenuKnapp",                    /* class_name                   */
        sizeof(MenuKnappRec),	        /* widget_size                  */
        NULL,		                /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
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
        Destroy,			/* destroy                      */
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
    {					/* menu_knapp fields		*/
	NULL,				/* extension			*/
    }
};

WidgetClass menuKnappWidgetClass = (WidgetClass)&menuKnappClassRec;

/*************************************************************************/

static void draw_arrow(MenuKnappWidget w, int inverted)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    int		s = w->menu_knapp.arrow_size;
    int		y = ((int)w->core.height - s) / 2;
    int		x;

    if (s <= 0)
	return;

    x = w->core.width + w->menu_knapp.arrow_offset -
	w->shadow.shadow_width - w->knapp.right_margin;

    if (w->shadow.line_mode) {
	short	sw = w->shadow.shadow_width;
	XPoint	point[4];

	XClearArea(disp, win, x - sw, y - sw,
		   s + 2 * sw, s + 2 * sw, False);

	if (inverted)
	    sw = 0;
	else
	    sw = (sw + 1)/2;

	point[0].x = point[3].x = x + s - sw;
	point[1].x = x + sw;
	point[2].x = x + s/2;

	point[0].y = point[1].y = point[3].y = y + sw;
	point[2].y = y + s - sw;

	XDrawLines(disp, win,
		   inverted ? w->shadow.light_gc : w->shadow.dark_gc,
		   point, 4, CoordModeOrigin);	    
    } else {
	XPoint	point[6];
	GC	light_gc, dark_gc;
	short	sw = w->menu_knapp.arrow_shadow_width;
	
	if (inverted) {
	    light_gc = w->shadow.dark_gc;
	    dark_gc = w->shadow.light_gc;
	} else {
	    light_gc = w->shadow.light_gc;
	    dark_gc = w->shadow.dark_gc;
	}

	if (sw == 0)
	    return;
	else if (sw == 1) {
	    point[0].x = point[3].x = x + s;
	    point[1].x = x;
	    point[2].x = (2 * x + s) / 2;

	    point[0].y = point[1].y = point[3].y = y;
	    point[2].y = y + s;
	    point[3].y--;

	    if (inverted && w->shadow.arm_gc != 0)
		XFillPolygon(disp, win, w->shadow.arm_gc, point, 3,
			     Convex, CoordModeOrigin);

	    XDrawLines(disp, win, light_gc, point, 3, CoordModeOrigin);
	    XDrawLines(disp, win, dark_gc, &point[2], 2, CoordModeOrigin);
	} else {
	    point[0].x = x + s;
	    point[1].x = x;
	    point[2].x = point[3].x = (2 * x + s) / 2;
	    point[4].x = x + (3 * sw) / 2;
	    point[5].x = x + s - (3 * sw) / 2;

	    point[0].y = point[1].y = y;
	    point[2].y = y + s;
	    point[3].y = y + s - (9 * sw) / 4;
	    point[4].y = point[5].y = y + sw;

	    if (inverted && w->shadow.arm_gc != 0)
		XFillPolygon(disp, win, w->shadow.arm_gc, &point[3], 3,
			     Convex, CoordModeOrigin);

	    XFillPolygon(disp, win, light_gc,
			 point, 6, Nonconvex, CoordModeOrigin);
	    point[4] = point[5];
	    point[5] = point[0];
	    XFillPolygon(disp, win, dark_gc,
			 &point[2], 4, Convex, CoordModeOrigin);
	}
    }
}

static Widget find_menu(MenuKnappWidget w)
{
    Widget loop;

    if (w->menu_knapp.menu_name)
	for (loop = (Widget)w ; loop ; loop = XtParent(loop)) {
	    Widget	temp = XtNameToWidget(loop, w->menu_knapp.menu_name);

	    if (temp)
		return temp;
	}

    return NULL;
}

static void set_unless(Widget gw, XEvent *event,
		       String *params, Cardinal *no_params)
{
    MenuKnappWidget	w = (MenuKnappWidget)gw;

    if (w->menu_knapp.menu_state == MenuStateDown)
	XtCallActionProc((Widget)w, "set", event, params, *no_params);
}

static void popup_and_grab(Widget gw, XEvent *event,
			   String *params, Cardinal *no_params)
{
    MenuKnappWidget	w = (MenuKnappWidget)gw;
    Widget		menu = w->menu_knapp.menu;
    Screen		*screen = XtScreen(w);
    Arg			args[2];
    Position		x, y;
    int			tmp;

    if (!w->knapp.active || event->type != ButtonPress ||
	w->menu_knapp.menu_state != MenuStateDown)
	return;

    if (!menu && !(menu = w->menu_knapp.menu = find_menu(w))) {
	fprintf(stderr, "MenuKnapp: Couldn't find popup menu widget: %s\n",
		w->menu_knapp.menu_name ? w->menu_knapp.menu_name : "(null)");
	return;
    }

    if (!XtIsSubclass(menu, menuShellWidgetClass)) {
	fprintf(stderr, "MenuKnapp: Widget %s is not a "
		"subclass of menuShell.\n", XtName(menu));
	return;
    }

    XtAddGrab((Widget)w, True, True);
    if (XtGrabPointer((Widget)w, True,
		      ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		      GrabModeAsync, GrabModeAsync, None,
		      None, event->xbutton.time) != GrabSuccess) {
	XtRemoveGrab((Widget)w);
	fputs("MenuKnapp: Failed to grab pointer.\n", stderr);
	return;
    }

    if (!XtIsRealized(menu))
	XtRealizeWidget(menu);

    SetActiveMenuShell(menu, True);

    XtTranslateCoords((Widget)w, 0, 0, &x, &y);
    y += w->core.height + w->core.border_width;
    if (x < 0)
	x = 0;
    else {
	tmp = WidthOfScreen(screen) - menu->core.width;
	if (tmp >= 0 && x > tmp)
	    x = tmp;
    }
    if (y < 0)
	y = 0;
    else {
	tmp = HeightOfScreen(screen) - menu->core.height;
	if (tmp >= 0 && y > tmp)
	    y = tmp;
    }

    XtSetArg(args[0], XtNx, x);
    XtSetArg(args[1], XtNy, y);
    XtSetValues(menu, args, 2);
    XtPopup(menu, XtGrabNone);
    XtAddGrab(menu, False, False);
    w->menu_knapp.menu_state = MenuStateUp;
    w->menu_knapp.start_time = event->xbutton.time;
    XClearWindow(XtDisplay(w), XtWindow(w));
    Redisplay((Widget)w, NULL, NULL);
}

static void popdown_timer(XtPointer client_data, XtIntervalId *id)
{
    MenuKnappWidget	w = (MenuKnappWidget)client_data;

    w->menu_knapp.timer = 0;
    w->menu_knapp.menu_state = MenuStateDown;
    PopdownMenuShell(w->menu_knapp.menu);
#if 0
    XtRemoveGrab((Widget)w);
#endif
    PostNotifyMenuShell(w->menu_knapp.menu);
    draw_arrow(w, True);
}

static void popdown_and_notify_if(Widget gw, XEvent *event,
				  String *params, Cardinal *no_params)
{
    MenuKnappWidget	w = (MenuKnappWidget)gw;
    unsigned int	x, y;
    Time		t;

    if (event->type != ButtonRelease ||
	w->menu_knapp.menu_state != MenuStateUp)
	return;

    x = event->xbutton.x;
    y = event->xbutton.y;
    t = event->xbutton.time;

    if (event->xbutton.window == XtWindow(w) &&
	x < w->core.width && y < w->core.height &&
	t <= w->menu_knapp.start_time + w->menu_knapp.multi_click_time)
	return;

    /*
     *  We wouldn't want an active grab when we're calling the callbacks...
     */
    XtUngrabPointer((Widget)w, event->xbutton.time);
    XtRemoveGrab(w->menu_knapp.menu);
    XtRemoveGrab((Widget)w);
    SetActiveMenuShell(w->menu_knapp.menu, False);
    XFlush(XtDisplay(w));
    if (!NotifyMenuShell(w->menu_knapp.menu)) {
	popdown_timer((XtPointer)w, 0);
	return;
    }

    w->menu_knapp.menu_state = MenuStateWaiting;
    w->menu_knapp.timer =
	XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w),
			w->menu_knapp.popdown_time, popdown_timer,
			(XtPointer)w);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    MenuKnappWidget	new = (MenuKnappWidget)gnew;

    new->menu_knapp.menu       = find_menu(new);
    new->menu_knapp.start_time = 0;
    new->menu_knapp.menu_state = MenuStateDown;
    new->menu_knapp.timer      = 0;
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    MenuKnappWidget	w = (MenuKnappWidget)gw;

    knappWidgetClass->core_class.expose((Widget)w, NULL, NULL);

    if (w->menu_knapp.arrow_shadow_width > 0)
	draw_arrow(w, w->menu_knapp.menu_state == MenuStateDown);
}

static void Destroy(Widget gw)
{
    MenuKnappWidget	w = (MenuKnappWidget)gw;

    if (w->menu_knapp.timer)
	XtRemoveTimeOut(w->menu_knapp.timer);
    w->menu_knapp.timer = 0;
    w->menu_knapp.menu_state = MenuStateDown;
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    MenuKnappWidget	new = (MenuKnappWidget)gnew;
    MenuKnappWidget	current = (MenuKnappWidget)gcurrent;
    Boolean		redisplay = False;

    if (new->menu_knapp.menu_name != current->menu_knapp.menu_name) {
	if (new->menu_knapp.menu_state == MenuStateWaiting) {
	    if (new->menu_knapp.timer)
		XtRemoveTimeOut(new->menu_knapp.timer);
	    popdown_timer((XtPointer)new, 0);
	}
	new->menu_knapp.menu = find_menu(new);
    }

    if (new->menu_knapp.arrow_shadow_width !=
	current->menu_knapp.arrow_shadow_width ||
	new->menu_knapp.arrow_size   != current->menu_knapp.arrow_size ||
	new->menu_knapp.arrow_offset != current->menu_knapp.arrow_offset)
	redisplay = True;

    return redisplay;
}
