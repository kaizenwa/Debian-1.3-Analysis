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
#include "ScrBarP.h"
#include "Util.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(ScrBarRec, scrbar.field)
    {XtNminimumThumb, XtCMinimumThumb, XtRDimension, sizeof(Dimension),
     offset(minimum_thumb), XtRImmediate, (XtPointer)8},
    {XtNpushThumb, XtCPushThumb, XtRBoolean, sizeof(Boolean),
     offset(push_thumb), XtRImmediate, (XtPointer)True},
    {XtNcanvasLength, XtCCanvasLength, XtRLong, sizeof(long),
     offset(canvas_length), XtRImmediate, (XtPointer)0},
    {XtNsliderLength, XtCSliderLength, XtRLong, sizeof(long),
     offset(slider_length), XtRImmediate, (XtPointer)1},
    {XtNsliderPosition, XtCSliderPosition, XtRLong, sizeof(long),
     offset(slider_position), XtRImmediate, (XtPointer)0},
    {XtNstepSize, XtCStepSize, XtRLong, sizeof(long),
     offset(step_size), XtRImmediate, (XtPointer)1},
    {XtNthickness, XtCThickness, XtRDimension, sizeof(Dimension),
     offset(thickness), XtRImmediate, (XtPointer)15},
    {XtNdecay, XtCDecay, XtRInt, sizeof(int),
     offset(decay), XtRImmediate, (XtPointer)20},
    {XtNinitialDelay, XtCDelay, XtRInt, sizeof(int),
     offset(initial_delay), XtRImmediate, (XtPointer)200},
    {XtNminimumDelay, XtCMinimumDelay, XtRInt, sizeof(int),
     offset(minimum_delay), XtRImmediate, (XtPointer)40},
    {XtNvertical, XtCVertical, XtRBoolean, sizeof(Boolean),
     offset(vertical), XtRImmediate, (XtPointer)True},
    {XtNallowOff, XtCAllocOff, XtRBoolean, sizeof(Boolean),
     offset(allow_off), XtRImmediate, (XtPointer)False},
    {XtNsyncScroll, XtCSyncScroll, XtRBoolean, sizeof(Boolean),
     offset(sync_scroll), XtRImmediate, (XtPointer)True},
    {XtNscrollCallback, XtCScrollCallback, XtRCallback, sizeof(XtCallbackList),
     offset(scroll_callback), XtRCallback, (XtPointer)NULL},
#undef offset
};

static void	ClassInitialize(void);
static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static void	Resize(Widget);
static void	Redisplay(Widget, XEvent*, Region);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);

static void move_thumb(Widget, XEvent*, String*, Cardinal*);
static void start_scroll(Widget, XEvent*, String*, Cardinal*);
static void cont_scroll(Widget, XEvent*, String*, Cardinal*);
static void stop_scroll(Widget, XEvent*, String*, Cardinal*);
static void page_scroll(Widget, XEvent*, String*, Cardinal*);
static void abs_scroll(Widget, XEvent*, String*, Cardinal*);
static void athena_scroll(Widget, XEvent*, String*, Cardinal*);
static void nop(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"move-thumb",	move_thumb},
    {"start-scroll",	start_scroll},
    {"cont-scroll",	cont_scroll},
    {"stop-scroll",	stop_scroll},
    {"page-scroll",	page_scroll},
    {"abs-scroll",	abs_scroll},
    {"athena-scroll",	athena_scroll},
    {"nop",		nop},
};

static char translations[] =
"<Btn1Down>:	start-scroll() \n"
"<Btn1Motion>:	cont-scroll() \n"
"<Btn1Up>:	stop-scroll() \n"
"<Btn2Down>:	move-thumb() \n"
"<Btn2Motion>:	move-thumb() \n";

ScrBarClassRec scrBarClassRec = {
    {                                   /* core fields                  */
        (WidgetClass) &shadowClassRec,  /* superclass           	*/
        "ScrBar",                       /* class_name                   */
        sizeof(ScrBarRec),              /* widget_size                  */
        ClassInitialize,                /* class_initialize             */
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
        TRUE,                           /* compress_exposure            */
#elif (XtSpecificationRelease < 6)
	XtExposeCompressMaximal,	/* compress_exposure		*/
#else
	XtExposeCompressMaximal | XtExposeNoRegion, /* compress_exposure*/
#endif
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        Destroy,                        /* destroy                      */
        Resize,                         /* resize                       */
        Redisplay,                      /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        translations,                   /* tm_table                     */
        QueryGeometry,                  /* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    {					/* shadow fields		*/
	XtInheritPixelOffset,		/* pixel_offset			*/
	True,				/* use_arm_for_background	*/
	XtInheritAllocShadowColors,	/* alloc_shadow_colors		*/
	XtInheritAllocShadowPixmaps,	/* alloc_shadow_pixmaps		*/
	XtInheritAllocArmColor,		/* alloc_arm_color		*/
	XtInheritAllocArmPixmap,	/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    {                                   /* scrbar fields		*/
        0                               /* empty                        */
    }
};

WidgetClass scrBarWidgetClass = (WidgetClass)&scrBarClassRec;

/*************************************************************************/

static void init_gcs(ScrBarWidget w)
{
    XGCValues	values;

    values.foreground = w->core.background_pixel;
    w->scrbar.bg_gc = XtGetGC((Widget)w, GCForeground, &values);
}

static void free_gcs(ScrBarWidget w)
{
    XtReleaseGC((Widget)w, w->scrbar.bg_gc);
}

/*************************************************************************/

static void calc_scrbar_coords(ScrBarWidget w)
{
    long	length, thick;
    long	c1, c2, c3, c4;

    if (w->scrbar.vertical) {
	length = w->core.height;
	thick  = w->core.width;
    } else {
	length = w->core.width;
	thick  = w->core.height;
    }

    c1 = thick;
    c4 = length - thick;

    if (c4 <= c1)
	c1 = c2 = c3 = c4 = length/2;
    else if (w->scrbar.canvas_length == 0) {
	c2 = c1;
	c3 = c4;
    } else {
	length -= 2 * thick;

	if (length > 0) {
	    c2 = c1 + (w->scrbar.slider_position * length) /
		w->scrbar.canvas_length;
	    c3 = c1 + (w->scrbar.slider_position +
		       w->scrbar.slider_length) * length /
		w->scrbar.canvas_length;
	    if (c3 > c4)
		c3 = c4;

	    if (c3 - c2 < (int)w->scrbar.minimum_thumb) {
		long	c = (c2 + c3)/2 - c1;
		long	dc = w->scrbar.minimum_thumb/2;

		if (dc > length/2)
		    dc = length/2;
		if (c - dc < 0)
		    c = dc;
		if (c + dc > length)
		    c = length - dc;

		c2 = c1 + c - dc;
		c3 = c1 + c + dc;
	    }
	} else {
	    c2 = c1;
	    c3 = c4;
	}
    }

    w->scrbar.c1 = c1;
    w->scrbar.c2 = c2;
    w->scrbar.c3 = c3;
    w->scrbar.c4 = c4;
}

static void draw_up_arrow(ScrBarWidget w)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    short	sw = w->shadow.shadow_width;

    if (w->shadow.line_mode) {
	XPoint	points[4];

	points[0].x = w->core.width/2;
	points[0].y = (3 * sw)/2 - 1;
	points[1].x = w->core.width - sw - 1;
	points[1].y = w->scrbar.c1 - 1;
	points[2].x = sw;
	points[2].y = w->scrbar.c1 - 1;
	points[3] = points[0];

	if (w->scrbar.mode == ScrBarModeBackwardLine)
	    XFillPolygon(disp, win, w->shadow.light_gc, points, 4,
			 Convex, CoordModeOrigin);
	else {
	    XFillPolygon(disp, win, w->scrbar.bg_gc, points, 4,
			 Convex, CoordModeOrigin);
	    XDrawLines(disp, win, w->shadow.light_gc,
		       points, 4, CoordModeOrigin);
	}
    } else {
	XPoint	points[6];

	points[0].x = w->core.width/2;
	points[0].y = sw - 1;
	points[1].x = w->core.width/2;
	points[1].y = sw - 1 + (9 * sw)/4;
	points[2].x = w->core.width - sw - (3 * sw)/2 + 1;
	points[2].y = w->scrbar.c1 - sw;
	points[3].x = sw + (3 * sw)/2 - 1;
	points[3].y = w->scrbar.c1 - sw;
	points[4].x = sw - 1;
	points[4].y = w->scrbar.c1;
	points[5].x = w->core.width - sw;
	points[5].y = w->scrbar.c1;

	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeBackwardLine) ?
		     w->shadow.light_gc : w->shadow.dark_gc,
		     points, 6, Complex, CoordModeOrigin);
	XFillPolygon(disp, win, w->scrbar.bg_gc,
		     &points[1], 3, Convex, CoordModeOrigin);

	points[2] = points[3];
	points[3] = points[4];

	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeBackwardLine) ?
		     w->shadow.dark_gc : w->shadow.light_gc,
		     points, 4, Complex, CoordModeOrigin);
    }
}

static void draw_down_arrow(ScrBarWidget w)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    short	sw = w->shadow.shadow_width;

    if (w->shadow.line_mode) {
	XPoint	points[4];

	points[0].x = w->core.width/2;
	points[0].y = w->core.height - (3 * sw)/2;
	points[1].x = w->core.width - sw - 1;
	points[1].y = w->scrbar.c4;
	points[2].x = sw;
	points[2].y = w->scrbar.c4;
	points[3] = points[0];

	if (w->scrbar.mode == ScrBarModeForwardLine)
	    XFillPolygon(disp, win, w->shadow.light_gc, points, 4,
			 Convex, CoordModeOrigin);
	else {
	    XFillPolygon(disp, win, w->scrbar.bg_gc, points, 4,
			 Convex, CoordModeOrigin);
	    XDrawLines(disp, win, w->shadow.light_gc,
		       points, 4, CoordModeOrigin);
	}
    } else {
	XPoint	points[6];

	points[0].x = w->core.width/2;
	points[0].y = w->core.height - sw;
	points[1].x = w->core.width/2;
	points[1].y = w->core.height - sw - (9 * sw)/4;
	points[2].x = sw + (3 * sw)/2 - 1;
	points[2].y = w->scrbar.c4 + sw;
	points[3].x = w->core.width - sw - (3 * sw)/2;
	points[3].y = w->scrbar.c4 + sw;
	points[4].x = w->core.width - sw;
	points[4].y = w->scrbar.c4;
	points[5].x = sw;
	points[5].y = w->scrbar.c4;

	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeForwardLine) ?
		     w->shadow.dark_gc : w->shadow.light_gc,
		     points, 6, Complex, CoordModeOrigin);
	XFillPolygon(disp, win, w->scrbar.bg_gc,
		     &points[1], 3, Convex, CoordModeOrigin);

	points[2] = points[3];
	points[3] = points[4];

	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeForwardLine) ?
		     w->shadow.light_gc : w->shadow.dark_gc,
		     points, 4, Complex, CoordModeOrigin);
    }
}

static void draw_left_arrow(ScrBarWidget w)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    short	sw = w->shadow.shadow_width;

    if (w->shadow.line_mode) {
	XPoint	points[4];

	points[0].x = (3 * sw)/2;
	points[0].y = w->core.height/2;
	points[1].x = w->scrbar.c1 - 1;
	points[1].y = w->core.height - sw - 1;
	points[2].x = w->scrbar.c1 - 1;
	points[2].y = sw;
	points[3] = points[0];

	if (w->scrbar.mode == ScrBarModeBackwardLine)
	    XFillPolygon(disp, win, w->shadow.light_gc, points, 4,
			 Convex, CoordModeOrigin);
	else {
	    XFillPolygon(disp, win, w->scrbar.bg_gc, points, 4,
			 Convex, CoordModeOrigin);
	    XDrawLines(disp, win, w->shadow.light_gc,
		       points, 4, CoordModeOrigin);
	}
    } else {
	XPoint	points[6];

	points[0].x = sw;
	points[0].y = w->core.height/2;
	points[1].x = sw + (9 * sw)/4;/**/
	points[1].y = w->core.height/2;
	points[2].x = w->scrbar.c1 - sw;
	points[2].y = w->core.height - sw - (3 * sw)/2;
	points[3].x = w->scrbar.c1 - sw;
	points[3].y = sw + (3 * sw)/2 - 1;
	points[4].x = w->scrbar.c1;
	points[4].y = sw - 1;
	points[5].x = w->scrbar.c1;
	points[5].y = w->core.height - sw;
	
	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeBackwardLine) ?
		     w->shadow.light_gc : w->shadow.dark_gc,
		     points, 6, Complex, CoordModeOrigin);
	XFillPolygon(disp, win, w->scrbar.bg_gc,
		     &points[1], 3, Convex, CoordModeOrigin);
	
	points[2] = points[3];
	points[3] = points[4];
	
	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeBackwardLine) ?
		     w->shadow.dark_gc : w->shadow.light_gc,
		     points, 4, Complex, CoordModeOrigin);
    }
}

static void draw_right_arrow(ScrBarWidget w)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    short	sw = w->shadow.shadow_width;

    if (w->shadow.line_mode) {
	XPoint	points[4];

	points[0].x = w->core.width - (3 * sw)/2;
	points[0].y = w->core.height/2;
	points[1].x = w->scrbar.c4;
	points[1].y = w->core.height - sw - 1;
	points[2].x = w->scrbar.c4;
	points[2].y = sw;
	points[3] = points[0];

	if (w->scrbar.mode == ScrBarModeForwardLine)
	    XFillPolygon(disp, win, w->shadow.light_gc, points, 4,
			 Convex, CoordModeOrigin);
	else {
	    XFillPolygon(disp, win, w->scrbar.bg_gc, points, 4,
			 Convex, CoordModeOrigin);
	    XDrawLines(disp, win, w->shadow.light_gc,
		       points, 4, CoordModeOrigin);
	}
    } else {
	XPoint	points[6];

	points[0].x = w->core.width - sw;
	points[0].y = w->core.height/2;
	points[1].x = w->core.width - sw - (9 * sw)/4;
	points[1].y = w->core.height/2;
	points[2].x = w->scrbar.c4 + sw;
	points[2].y = sw + (3 * sw)/2 - 1;
	points[3].x = w->scrbar.c4 + sw;
	points[3].y = w->core.height - sw - (3 * sw)/2 + 1;
	points[4].x = w->scrbar.c4;
	points[4].y = w->core.height - sw;
	points[5].x = w->scrbar.c4;
	points[5].y = sw - 1;

	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeForwardLine) ?
		     w->shadow.dark_gc : w->shadow.light_gc,
		     points, 6, Complex, CoordModeOrigin);
	XFillPolygon(disp, win, w->scrbar.bg_gc,
		     &points[1], 3, Convex, CoordModeOrigin);

	points[2] = points[3];
	points[3] = points[4];

	XFillPolygon(disp, win,
		     (w->scrbar.mode == ScrBarModeForwardLine) ?
		     w->shadow.light_gc : w->shadow.dark_gc,
		     points, 4, Complex, CoordModeOrigin);
    }
}

static void draw_thumb(ScrBarWidget w, Region region)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    int		sw = w->shadow.shadow_width;
    int		c1 = w->scrbar.c1;
    int		c2 = w->scrbar.c2;
    int		c3 = w->scrbar.c3;
    int		c4 = w->scrbar.c4;

    if (w->scrbar.vertical) {
	int	width = w->core.width - 2 * sw;
	
	if (c3 > c2 &&
	    (!region ||
	     XRectInRegion(region, sw, c2, width, c3 - c2) !=
	     RectangleOut)) {
	    ShadowDrawShadows((ShadowWidget)w, sw, c2, width, c3 - c2,
			      (w->scrbar.mode == ScrBarModeContinuous) &&
			      w->scrbar.push_thumb);
	    if (w->shadow.line_mode &&
		!(w->scrbar.mode == ScrBarModeContinuous &&
		  w->scrbar.push_thumb)) {
		if (c3 - c2 - 2 > 0)
		    XFillRectangle(disp, win, w->scrbar.bg_gc,
				   sw + 1, c2 + 1, width - 2, c3 - c2 - 2);
	    } else {
		if (c3 - c2 - 2 * sw > 0) 
		    XFillRectangle(disp, win, w->scrbar.bg_gc,
				   2 * sw, c2 + sw,
				   width - 2 * sw, c3 - c2 - 2 * sw);
	    }
	}

	if (c2 > c1 &&
	    (!region ||
	    XRectInRegion(region, sw, c1, width, c2 - c1) != RectangleOut))
	    XClearArea(disp, win, sw, c1, width, c2 - c1, False);

	if (c4 > c3 &&
	    (!region ||
	     XRectInRegion(region, sw, c3, width, c4 - c3) != RectangleOut))
	    XClearArea(disp, win, sw, c3, width, c4 - c3, False);
    } else {
	int	height = w->core.height - 2 * sw;

	if (!region ||
	    XRectInRegion(region, c2, sw, c3 - c2, height) != RectangleOut) {
	    ShadowDrawShadows((ShadowWidget)w, c2, sw, c3 - c2, height,
			      (w->scrbar.mode == ScrBarModeContinuous) &&
			      w->scrbar.push_thumb);
	    if (w->shadow.line_mode &&
		!(w->scrbar.mode == ScrBarModeContinuous &&
		  w->scrbar.push_thumb)) {
		if (c3 - c2 - 2 > 0)
		    XFillRectangle(disp, win, w->scrbar.bg_gc, c2 + 1, sw + 1,
				   c3 - c2 - 2, height - 2);
	    } else if (c3 -c2 - 2 * sw > 0)
		XFillRectangle(disp, win, w->scrbar.bg_gc, c2 + sw, 2 * sw,
			       c3 - c2 - 2 * sw, height - 2 * sw);
	}

	if (c2 > c1 &&
	    (!region ||
	     XRectInRegion(region, c1, sw, c2 - c1, height) != RectangleOut))
	    XClearArea(disp, win, c1, sw, c2 - c1, height, False);

	if (c4 > c3 &&
	    (!region ||
	     XRectInRegion(region, c3, sw, c4 - c3, height) != RectangleOut))
	    XClearArea(disp, win, c3, sw, c4 - c3, height, False);
    }
}

static void draw_arrows(ScrBarWidget w)
{
    if (w->scrbar.vertical) {
	draw_up_arrow(w);
	draw_down_arrow(w);
    } else {
	draw_left_arrow(w);
	draw_right_arrow(w);
    }
}

static void fit_scrbar(ScrBarWidget w)
{
    if (w->scrbar.slider_position < 0)
	w->scrbar.slider_position = 0;
    else if (w->scrbar.allow_off) {
	if (w->scrbar.slider_position >= w->scrbar.canvas_length) {
	    if (w->scrbar.canvas_length == 0)
		w->scrbar.slider_position = 0;
	    else
		w->scrbar.slider_position = w->scrbar.canvas_length - 1;
	}
    } else if (w->scrbar.slider_position + w->scrbar.slider_length >
	       w->scrbar.canvas_length) {
	w->scrbar.slider_position = w->scrbar.canvas_length -
	    w->scrbar.slider_length;
	if (w->scrbar.slider_position < 0)
	    w->scrbar.slider_position = 0;
    }
}

static void send_scroll_report(ScrBarWidget w)
{
    XtCallbackList	c_list = w->scrbar.scroll_callback;

    if (c_list) {
	ScrollReport	report;

	report.pos   = w->scrbar.slider_position;
	report.shown = w->scrbar.slider_length;
	report.size  = w->scrbar.canvas_length;
	XtCallCallbackList((Widget)w, c_list, (XtPointer)&report);
    }
}

/*************************************************************************/

static void scroll_timer_proc(XtPointer client_data, XtIntervalId *id)
{
    ScrBarWidget	w = (ScrBarWidget)client_data;

    w->scrbar.timer = 0;

    /*
     *  We get rid of a race by explicitly quering
     *  the pointer for the modifiers.
     */
    if (w->scrbar.sync_scroll) {
	unsigned int	mods;
	Window		w1, w2;
	int		x1, x2, y1, y2;

	XQueryPointer(XtDisplay(w), XtWindow(w),
		      &w1, &w2, &x1, &y1, &x2, &y2, &mods);
	if (!mods)
	    return;
    }

    switch (w->scrbar.mode) {
    case ScrBarModeForwardLine:
	w->scrbar.slider_position += w->scrbar.step_size;
	break;
    case ScrBarModeBackwardLine:
	w->scrbar.slider_position -= w->scrbar.step_size;
	break;
    case ScrBarModeForwardPage:
	w->scrbar.slider_position += w->scrbar.slider_length;
	break;
    case ScrBarModeBackwardPage:
	w->scrbar.slider_position -= w->scrbar.slider_length;
	break;
    default:
	XBell(XtDisplay(w), 0);
	return;
    }

    fit_scrbar(w);
    calc_scrbar_coords(w);
    draw_thumb(w, NULL);
    send_scroll_report(w);
    w->scrbar.this_delay -= w->scrbar.decay;
    if (w->scrbar.this_delay < w->scrbar.minimum_delay)
	w->scrbar.this_delay = w->scrbar.minimum_delay;
    w->scrbar.timer =
	XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w),
			(unsigned long)w->scrbar.this_delay,
			scroll_timer_proc, (XtPointer)w);
}

static void move_thumb(Widget	 gw,
		       XEvent	*event,
		       String	*params,
		       Cardinal	*no_params)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    int			x, y, c, length;

    if (!get_event_xy(event, &x, &y) || w->scrbar.mode != ScrBarModeNone) {
	XBell(XtDisplay(w), 0);
	return;
    }

    length = w->scrbar.c4 - w->scrbar.c1;
    if (length <= 0)
	return;

    c = (w->scrbar.vertical ?  y : x ) - w->scrbar.c1;
    w->scrbar.slider_position = c * w->scrbar.canvas_length / length;

    fit_scrbar(w);
    calc_scrbar_coords(w);
    draw_thumb(w, NULL);
    send_scroll_report(w);
}

static void cont_scroll(Widget	  gw,
			XEvent	 *event,
			String	 *params,
			Cardinal *no_params)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    int			x, y, dc, length;

    if (w->scrbar.mode != ScrBarModeContinuous)
	return;

    if (!get_event_xy(event, &x, &y)) {
	XBell(XtDisplay(w), 0);
	return;
    }

    length = w->scrbar.c4 - w->scrbar.c1;
    if (length <= 0)
	return;

    dc = (w->scrbar.vertical ?  y : x ) - w->scrbar.init_ptr_pos;
    w->scrbar.slider_position = w->scrbar.init_slider_pos +
	dc * w->scrbar.canvas_length / length;

    fit_scrbar(w);
    calc_scrbar_coords(w);
    draw_thumb(w, NULL);
    send_scroll_report(w);
}

static void start_scroll(Widget     gw,
			 XEvent    *event,
			 String    *params,
			 Cardinal  *no_params)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    int			c, x, y;
    ScrBarMode		mode;

    if (w->scrbar.mode == ScrBarModeContinuous)
	return;

    if (!get_event_xy(event, &x, &y)) {
	XBell(XtDisplay(w), 0);
	return;
    }
    
    c = w->scrbar.vertical ? y : x;

    if (c <= w->scrbar.c1) {
	mode = ScrBarModeBackwardLine;
	w->scrbar.slider_position -= w->scrbar.step_size;
    } else if (c < w->scrbar.c2) {
	mode = ScrBarModeBackwardPage;
	w->scrbar.slider_position -= w->scrbar.slider_length;
    } else if (c >= w->scrbar.c4) {
	mode = ScrBarModeForwardLine;
	w->scrbar.slider_position += w->scrbar.step_size;
    } else if (c >= w->scrbar.c3) {
	mode = ScrBarModeForwardPage;
	w->scrbar.slider_position += w->scrbar.slider_length;
    } else {
	mode = ScrBarModeContinuous;
	w->scrbar.init_ptr_pos = c;
	w->scrbar.init_slider_pos = w->scrbar.slider_position;
    }

    w->scrbar.mode = mode;
    fit_scrbar(w);
    calc_scrbar_coords(w);
    if (mode == ScrBarModeBackwardLine)
	if (w->scrbar.vertical)
	    draw_up_arrow(w);
	else
	    draw_left_arrow(w);
    else if (mode == ScrBarModeForwardLine)
	if (w->scrbar.vertical)
	    draw_down_arrow(w);
	else
	    draw_right_arrow(w);
    draw_thumb(w, NULL);
    if (mode != ScrBarModeContinuous && w->scrbar.initial_delay != 0) {
	w->scrbar.this_delay = w->scrbar.initial_delay;
	w->scrbar.timer =
	    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w),
			    (unsigned long)w->scrbar.this_delay,
			    scroll_timer_proc,
			    (XtPointer)w);
    }
    send_scroll_report(w);
}

static void stop_scroll(Widget     gw,
			XEvent    *event,
			String    *params,
			Cardinal  *no_params)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    ScrBarMode		mode = w->scrbar.mode;

    w->scrbar.mode = ScrBarModeNone;
    if (w->scrbar.timer) {
	XtRemoveTimeOut(w->scrbar.timer);
	w->scrbar.timer = 0;
    }
    if (mode == ScrBarModeForwardLine ||
	mode == ScrBarModeBackwardLine)
	draw_arrows(w);
    if (mode == ScrBarModeContinuous &&
	w->scrbar.push_thumb)
	draw_thumb(w, NULL);
}

static void page_scroll(Widget    gw,
			XEvent   *event,
			String   *params,
			Cardinal *no_params)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    float		amount;

    if (w->scrbar.mode != ScrBarModeNone ||
	*no_params != 1 || sscanf(params[0], "%f", &amount) != 1) {
	XBell(XtDisplay(w), 0);
	return;
    }

    w->scrbar.slider_position += amount * (float)w->scrbar.slider_length;
    fit_scrbar(w);
    calc_scrbar_coords(w);
    draw_thumb(w, NULL);
    send_scroll_report(w);
}

static void abs_scroll(Widget    gw,
		       XEvent   *event,
		       String   *params,
		       Cardinal *no_params)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    long		amount;

    if (w->scrbar.mode != ScrBarModeNone ||
	*no_params != 1 || sscanf(params[0], "%ld", &amount) != 1) {
	XBell(XtDisplay(w), 0);
	return;
    }

    w->scrbar.slider_position += amount;
    fit_scrbar(w);
    calc_scrbar_coords(w);
    draw_thumb(w, NULL);
    send_scroll_report(w);
}

static void athena_scroll(Widget    gw,
			  XEvent   *event,
			  String   *params,
			  Cardinal *no_params)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    int			x, y;
    float		fract;

    if (w->scrbar.mode != ScrBarModeNone || *no_params != 1 ||
	!get_event_xy(event, &x, &y))
	return;

    fract = (float)y / w->core.height;
    if (params[0][0] == 'u' || params[0][0] == 'U')
	fract = - fract;
    w->scrbar.slider_position += fract * w->scrbar.slider_length;
    fit_scrbar(w);
    calc_scrbar_coords(w);
    draw_thumb(w, NULL);
    send_scroll_report(w);
}

static void nop(Widget    gw,
		XEvent   *event,
		String   *params,
		Cardinal *no_params)
{
}

/*************************************************************************/

static void ClassInitialize(void)
{
    XtSetTypeConverter(XtRString, XtRLong, cvt_string_to_long,
		       NULL, 0, XtCacheAll, NULL);
}

static void Initialize(Widget greq, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    ScrBarWidget	new = (ScrBarWidget)gnew;

    if (new->core.width  == 0)
	new->core.width  = new->scrbar.vertical ? new->scrbar.thickness : 1;
    if (new->core.height == 0)
	new->core.height = new->scrbar.vertical ? 1 : new->scrbar.thickness;

    init_gcs(new);
    new->scrbar.mode = ScrBarModeNone;
    new->scrbar.timer = 0;
    new->scrbar.this_delay = 0;
    calc_scrbar_coords(new);
}

static void Destroy(Widget gw)
{
    ScrBarWidget	w = (ScrBarWidget)gw;

    free_gcs(w);
}

static void Resize(Widget gw)
{
    ScrBarWidget	w = (ScrBarWidget)gw;

    calc_scrbar_coords(w);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    ScrBarWidget	w = (ScrBarWidget)gw;

    if (!XtIsRealized((Widget)w)) return;

    ShadowDrawShadows((ShadowWidget)w, 0, 0, w->core.width,
		      w->core.height, !w->shadow.line_mode);

    if (w->scrbar.c1 == w->scrbar.c4)
	return;

    if (w->scrbar.vertical) {
	if (!region ||
	    XRectInRegion(region, 0, 0,
			  w->core.width, w->scrbar.c1) != RectangleOut)
	    draw_up_arrow(w);

	if (!region ||
	    XRectInRegion(region, 0, w->scrbar.c4, w->core.width,
			  w->core.height - w->scrbar.c4) != RectangleOut)
	    draw_down_arrow(w);
    } else {
	if (!region ||
	    XRectInRegion(region, 0, 0,
			  w->scrbar.c1, w->core.height) != RectangleOut)
	    draw_left_arrow(w);

	if (!region ||
	    XRectInRegion(region, w->scrbar.c4, 0,
			  w->core.width - w->scrbar.c4,
			  w->core.height) != RectangleOut)
	    draw_right_arrow(w);
    }

    draw_thumb(w, region);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *no_args)
{
    ScrBarWidget	new = (ScrBarWidget)gnew;
    ScrBarWidget	current = (ScrBarWidget)gcurrent;
    Boolean		redisplay = False;

    if (new->shadow.shadow_width != current->shadow.shadow_width ||
	new->scrbar.minimum_thumb != current->scrbar.minimum_thumb ||
	new->scrbar.vertical != current->scrbar.vertical ||
	new->scrbar.canvas_length != current->scrbar.canvas_length ||
	new->scrbar.slider_length != current->scrbar.slider_length ||
	new->scrbar.slider_position != current->scrbar.slider_position)
	redisplay = True;

    if (new->scrbar.thickness != current->scrbar.thickness) {
	if (new->scrbar.vertical)
	    new->core.width = new->scrbar.thickness;
	else
	    new->core.height = new->scrbar.thickness;
    }

    if (redisplay) {
	fit_scrbar(new);
	calc_scrbar_coords(new);
    }
    
    if (new->core.background_pixel != current->core.background_pixel) {
	free_gcs(new);
	init_gcs(new);
	redisplay = True;
    }

    return redisplay;
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    ScrBarWidget	w = (ScrBarWidget)gw;
    Dimension		thickness = w->scrbar.thickness;
    Dimension		curr_thick, intend_thick;
    int			mask;

    if (w->scrbar.vertical) {
	mask = CWWidth;
	preferred->width = thickness;
	curr_thick = w->core.width;
	if (intended->request_mode & CWWidth)
	    intend_thick = intended->width;
	else
	    intend_thick = curr_thick;
    } else {
	mask = CWHeight;
	preferred->height = thickness;
	curr_thick = w->core.height;
	if (intended->request_mode & CWHeight)
	    intend_thick = intended->height;
	else
	    intend_thick = curr_thick;
    }
    preferred->request_mode = mask;

    if (intended->request_mode & mask) {
	if (intend_thick == thickness)
	    return XtGeometryYes;
	else if (curr_thick == thickness)
	    return XtGeometryNo;
	else
	    return XtGeometryAlmost;
    }

    if (curr_thick == thickness)
	return XtGeometryYes;
    else
	return XtGeometryAlmost;
}

/*************************************************************************/

void ScrBarSetSliderPosition(Widget gw, long pos)
{
    ScrBarWidget	w = (ScrBarWidget)gw;

    w->scrbar.slider_position = pos;
    fit_scrbar(w);
    calc_scrbar_coords(w);
    if (XtIsRealized((Widget)w))
	draw_thumb(w, NULL);
}

void ScrBarSetLengthsAndPos(Widget gw, long c_l, long s_l, long s_p)
{
    ScrBarWidget	w = (ScrBarWidget)gw;

    w->scrbar.canvas_length = c_l;
    w->scrbar.slider_length = s_l;
    w->scrbar.slider_position = s_p;
    fit_scrbar(w);
    calc_scrbar_coords(w);
    if (XtIsRealized((Widget)w))
	draw_thumb(w, NULL);
}
