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
#include <X11/ShellP.h>
#include <string.h>

#include "Compat.h"
#include "ScrListP.h"
#include "Util.h"

#define FIRST(w)  ((w)->scrollable.pos_y)
#define SHOWN(w)  ((w)->scrollable.shown_y)
#define LINES(w)  ((w)->scrollable.height)

static void grey90_default_proc(Widget, int, XrmValue*);

static XtResource resources[] = {
#define offset(field) XtOffsetOf(ScrListRec, scrlist.field)
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(ShadowRec, shadow.shadow_width), XtRImmediate, (XtPointer)1},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, XtDefaultForeground},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, XtDefaultFont},
    {XtNhighlightColor, XtCHighlightColor, XtRPixel, sizeof(Pixel),
     offset(highlight_pixel), XtRCallProc, (XtPointer)grey90_default_proc},
    {XtNrowSpacing, XtCRowSpacing, XtRDimension, sizeof(Dimension),
     offset(row_spacing), XtRImmediate, (XtPointer)0},
    {XtNnAlloc, XtCNAlloc, XtRLong, sizeof(long),
     offset(n_alloc), XtRImmediate, (XtPointer)0},
    {XtNinternalWidth, XtCInternalWidth, XtRDimension, sizeof(Dimension),
     offset(internal_width), XtRImmediate, (XtPointer)8},
    {XtNinternalHeight, XtCInternalHeight, XtRDimension, sizeof(Dimension),
     offset(internal_height), XtRImmediate, (XtPointer)4},
    {XtNinternalItemWidth, XtCInternalItemWidth,
     XtRDimension, sizeof(Dimension),
     offset(internal_item_width), XtRImmediate, (XtPointer)8},
    {XtNinternalItemHeight, XtCInternalItemHeight,
     XtRDimension, sizeof(Dimension),
     offset(internal_item_height), XtRImmediate, (XtPointer)1},
    {XtNpixmapWidth, XtCPixmapWidth, XtRDimension, sizeof(Dimension),
     offset(pixmap_width), XtRImmediate, (XtPointer)0},
    {XtNpixmapHeight, XtCPixmapHeight, XtRDimension, sizeof(Dimension),
     offset(pixmap_height), XtRImmediate, (XtPointer)0},
    {XtNpixmapSpacing, XtCPixmapSpacing, XtRDimension, sizeof(Dimension),
     offset(pixmap_spacing), XtRImmediate, (XtPointer)8},
    {XtNpreferredLines, XtCPreferredLines, XtRDimension, sizeof(Dimension),
     offset(preferred_lines), XtRImmediate, (XtPointer)12},
    {XtNpreferredColumns, XtCPreferredColumns, XtRDimension, sizeof(Dimension),
     offset(preferred_columns), XtRImmediate, (XtPointer)80},
    {XtNdepthOne, XtCDepthOne, XtRBoolean, sizeof(Boolean),
     offset(depth_one), XtRImmediate, (XtPointer)True},
    {XtNatLeastOne, XtCAtLeastOne, XtRBoolean, sizeof(Boolean),
     offset(at_least_one), XtRImmediate, (XtPointer)False},
    {XtNatMostOne, XtCAtMostOne, XtRBoolean, sizeof(Boolean),
     offset(at_most_one), XtRImmediate, (XtPointer)True},
    {XtNallowDnd, XtCAllowDnd, XtRBoolean, sizeof(Boolean),
     offset(allow_dnd), XtRImmediate, (XtPointer)False},
    {XtNusePixmaps, XtCUsePixmaps, XtRBoolean, sizeof(Boolean),
     offset(use_pixmaps), XtRImmediate, (XtPointer)False},
    {XtNselectCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(select_callback), XtRCallback, (XtPointer)NULL},
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRCallback, (XtPointer)NULL},
    {XtNsecondCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(second_callback), XtRCallback, (XtPointer)NULL},
    {XtNdndCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(dnd_callback), XtRCallback, (XtPointer)NULL},
    {XtNdndCursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(dnd_cursor), XtRString, (XtPointer)"hand2"},
    {XtNmarginUp, XtCMargin, XtRDimension, sizeof(Dimension),
     offset(margin_up), XtRImmediate, (XtPointer)1},
    {XtNmarginDown, XtCMargin, XtRDimension, sizeof(Dimension),
     offset(margin_down), XtRImmediate, (XtPointer)1},
    {XtNpageUp, XtCPage, XtRBoolean, sizeof(Boolean),
     offset(page_up), XtRImmediate, (XtPointer)True},
    {XtNpageDown, XtCPage, XtRBoolean, sizeof(Boolean),
     offset(page_down), XtRImmediate, (XtPointer)True},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static void	Redisplay(Widget, XEvent*, Region);
static void	Resize(Widget);
static void	Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void	SetVPos(ScrollableWidget, long);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);

static void	notify(Widget, XEvent*, String*, Cardinal*);
static void	notify_second(Widget, XEvent*, String*, Cardinal*);
static void	select_action(Widget, XEvent*, String*, Cardinal*);
static void	drag_select(Widget, XEvent*, String*, Cardinal*);
static void	dnd_start(Widget, XEvent*, String*, Cardinal*);
static void	dnd_do(Widget, XEvent*, String*, Cardinal*);
static void	dnd_end(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"notify",		notify},
    {"notify-second",	notify_second},
    {"select",		select_action},
    {"drag-select",	drag_select},
    {"dnd-start",	dnd_start},
    {"dnd-do",		dnd_do},
    {"dnd-end",		dnd_end},
};

static char translations[] =
"<Btn1Down>:	select() \n"
"<Btn1Down>(2):	notify() \n";

ScrListClassRec scrListClassRec = {
    { /* core_class fields */
	(WidgetClass) &scrollableClassRec,	/* superclass		*/
	"ScrList",			/* class_name			*/
	sizeof(ScrListRec),		/* widget_size			*/
	NULL,				/* class_initialize		*/
	NULL,				/* class_part_initialize	*/
	FALSE,				/* class_inited			*/
	Initialize,			/* initialize			*/
	NULL,				/* initialize_hook		*/
	Realize,			/* realize			*/
	actions,			/* actions			*/
	XtNumber(actions),		/* num_actions			*/
	resources,			/* resources			*/
	XtNumber(resources),		/* num_resources		*/
	NULLQUARK,			/* xrm_class			*/
	TRUE,				/* compress_motion		*/
#if (XtSpecificationRelease < 4) /* It really needs GraphicsExpose ... */
	True,				/* compress_exposure		*/
#elif (XtSpecificationRelease < 6)
	XtExposeCompressMaximal | XtExposeGraphicsExposeMerged,
					/* compress_exposure		*/
#else
	XtExposeCompressMaximal | XtExposeGraphicsExposeMerged |
	XtExposeNoRegion,		/* compress_exposure		*/
#endif
	TRUE,				/* compress_enterleave		*/
	FALSE,				/* visible_interest		*/
	Destroy,			/* destroy			*/
	Resize,				/* resize			*/
	Redisplay,			/* expose			*/
	SetValues,			/* set_values			*/
	NULL,				/* set_values_hook		*/
	XtInheritSetValuesAlmost,	/* set_values_almost		*/
	NULL,				/* get_values_hook		*/
	NULL,				/* accept_focus			*/
	XtVersion,			/* version			*/
	NULL,				/* callback_private		*/
	translations,			/* tm_table			*/
	QueryGeometry,			/* query_geometry		*/
	XtInheritDisplayAccelerator,	/* display_accelerator		*/
	NULL				/* extension			*/
    },
    {					/* shadow fields		*/
	XtOffsetOf(ScrListRec, scrlist.highlight_pixel), /* pixel_offset */
	True,				/* use_arm_for_background	*/
	XtInheritAllocShadowColors,	/* alloc_shadow_colors		*/
	XtInheritAllocShadowPixmaps,	/* alloc_shadow_pixmaps		*/
	NULL,				/* alloc_arm_color		*/
	NULL,				/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    {					/* scrollable fields		*/
	XtInheritScrollableSetPos,	/* set_hpos			*/
	SetVPos,			/* set_vpos			*/
	NULL,				/* suspend_hook			*/
	NULL,				/* extension			*/
    },
    {					/* scrlist fields		*/
	NULL				/* extension			*/
    }
};

WidgetClass scrListWidgetClass = (WidgetClass)&scrListClassRec;

/*************************************************************************/

static void grey90_default_proc(Widget w, int offset, XrmValue *to)
{
    static Pixel	grey90;
    Display		*disp = XtDisplay(w);

    grey90 = w->core.background_pixel;

    if (w->core.depth != 1) {
	XColor		col;

	col.red = col.green = col.blue = 0xe5e5;

	if (XAllocColor(disp, w->core.colormap, &col))
	    if ((col.red   == 0 &&
		 col.green == 0 &&
		 col.blue  == 0) ||
		(col.red   == 0xffff &&
		 col.green == 0xffff &&
		 col.blue  == 0xffff))
		XFreeColors(disp, w->core.colormap, &col.pixel, 1, 0);
	    else
		grey90 = col.pixel;
    }

    to->addr = (XPointer)&grey90;
}

static void init_gcs(ScrListWidget w)
{
    XGCValues	values;

    values.foreground = w->scrlist.foreground_pixel;
    values.background = w->core.background_pixel;
    values.font = w->scrlist.font->fid;
    w->scrlist.default_gc =
	XtGetGC((Widget)w, GCForeground | GCBackground | GCFont, &values);
    values.background = w->scrlist.highlight_pixel;
    w->scrlist.selected_gc =
	XtGetGC((Widget)w, GCForeground | GCBackground, &values);
    if (w->shadow.line_mode)
	w->scrlist.highlight_gc = (GC)0;
    else {
	values.foreground = w->scrlist.highlight_pixel;
	w->scrlist.highlight_gc = XtGetGC((Widget)w, GCForeground, &values);
    }
}

static void free_gcs(ScrListWidget w)
{
    XtReleaseGC((Widget)w, w->scrlist.default_gc);
    XtReleaseGC((Widget)w, w->scrlist.highlight_gc);
    XtReleaseGC((Widget)w, w->scrlist.selected_gc);
}

static void alloc_list(ScrListWidget w, long n_alloc)
{
    long	n = w->scrlist.n_alloc;
    int		use_pixmaps = w->scrlist.use_pixmaps;

    if (n_alloc < LINES(w))
	n_alloc = LINES(w);

    w->scrlist.strings =
	(char **)XtRealloc((char *)w->scrlist.strings,
			   n_alloc * sizeof(char*));
    w->scrlist.selected =
	(Boolean *)XtRealloc((char *)w->scrlist.selected,
			     n_alloc * sizeof(Boolean));
    if (use_pixmaps)
	w->scrlist.pixmaps =
	    (Pixmap *)XtRealloc((char *)w->scrlist.pixmaps,
				n_alloc * sizeof(Pixmap));
    w->scrlist.n_alloc = n_alloc;

    while (n < n_alloc) {
	w->scrlist.strings[n] = NULL;
	w->scrlist.selected[n] = False;
	if (use_pixmaps)
	    w->scrlist.pixmaps[n] = None;
	n++;
    }
}

static void calc_shown(ScrListWidget w)
{
    int h, dh = 0;

    h  = w->core.height -
	2 * w->scrlist.internal_height +
	w->scrlist.row_spacing;
    dh = w->scrlist.font->ascent + w->scrlist.font->descent;
    if (w->scrlist.use_pixmaps && (int)w->scrlist.pixmap_height > dh)
	dh = w->scrlist.pixmap_height;
    dh += w->scrlist.row_spacing +
	2 * (w->scrlist.internal_item_height + w->shadow.shadow_width);

    SHOWN(w) = (dh <= 0) ? 0 : h/dh;
}

static void draw_list_item(ScrListWidget w, String *string,
			   Pixmap *pixmap, Boolean *selected,
			   int x, int y, int width, int height)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    int		sw = w->shadow.shadow_width;

    if (*selected) {
	ShadowDrawShadows((ShadowWidget)w, (Position)x, (Position)y,
			  (Dimension)width, (Dimension)height, False);
	if (!w->shadow.line_mode)
	    XFillRectangle(disp, win, w->scrlist.highlight_gc,
			   x + sw, y + sw, width - 2*sw, height - 2*sw);
    }

    x += w->scrlist.internal_item_width + w->shadow.shadow_width;
    if (pixmap) {
	if (*pixmap != None) {
	    if (w->scrlist.depth_one)
		XCopyPlane(disp, *pixmap, win,
			   *selected ? w->scrlist.selected_gc :
			   w->scrlist.default_gc,
			   0, 0, w->scrlist.pixmap_width,
			   w->scrlist.pixmap_height, x,
			   y + (height - (int)w->scrlist.pixmap_height) / 2,
			   1);
	    else
		XCopyArea(disp, *pixmap, win, w->scrlist.default_gc, 0, 0,
			  w->scrlist.pixmap_width, w->scrlist.pixmap_height,
			  x, y + (height - (int)w->scrlist.pixmap_height) / 2);
	}
	x += w->scrlist.pixmap_width + w->scrlist.pixmap_spacing;
    }

    if (string && *string != (String)NULL)
	XDrawString(disp, win, w->scrlist.default_gc, x,
		    y + (height + w->scrlist.font->ascent -
			 w->scrlist.font->descent)/2,
		    *string, strlen(*string));
}

static void redraw_items(ScrListWidget w, long start, long stop, Boolean clear)
{
    String	*string = w->scrlist.strings;
    Pixmap	*pixmap = w->scrlist.pixmaps;
    int		x = w->scrlist.internal_width;
    int		width = w->core.width - 2 * w->scrlist.internal_width;
    int		y = w->scrlist.internal_height;
    int		height = 0;

    if (width <= 0 || LINES(w) == 0)
	return;
    if (start < FIRST(w))
	start = FIRST(w);
    if (stop >= FIRST(w) + SHOWN(w))
	stop = FIRST(w) + SHOWN(w) - 1;
    if (stop >= LINES(w) - 1)
	stop = LINES(w) - 1;
    if (start > stop)
	return;

    if (string)
	string += start;
    if (pixmap)
	pixmap += start;
    if (string)
	height = w->scrlist.font->ascent + w->scrlist.font->descent;
    if (pixmap && (int)w->scrlist.pixmap_height > height)
	height = w->scrlist.pixmap_height;
    height += 2 * (w->scrlist.internal_item_height + w->shadow.shadow_width);
    if (width <= 0 || height == 0)
	return;
    y += (start - FIRST(w)) * (height + w->scrlist.row_spacing);

    if (clear) {
	int	h = (stop - start + 1) * (height + w->scrlist.row_spacing);

	XClearArea(XtDisplay(w), XtWindow(w), 0, y,
		   w->core.width, h, False);
    }

    while (start <= stop) {
	draw_list_item(w, string, pixmap,
		       &(w->scrlist.selected[start]),
		       x, y, width, height);
	y += height + w->scrlist.row_spacing;
	if (string)
	    string++;
	if (pixmap)
	    pixmap++;
	start++;
    }
}

static Dimension preferred_width(ScrListWidget w)
{
    int	ret;

    ret = w->scrlist.max_width;
    if (ret <= 0)
	ret = w->scrlist.preferred_columns * w->scrlist.font->max_bounds.width;

    ret += 2 * (w->shadow.shadow_width + w->scrlist.internal_width +
		w->scrlist.internal_item_width);

    if (w->scrlist.use_pixmaps)
	ret += w->scrlist.pixmap_width + w->scrlist.pixmap_spacing;

    return ret;
}

static Dimension preferred_height(ScrListWidget w)
{
    long	height = 0;
    long	n = w->scrlist.preferred_lines;

    if (w->scrlist.strings || !w->scrlist.pixmaps)
	height = w->scrlist.font->ascent + w->scrlist.font->descent;
    if (w->scrlist.pixmaps && height < (int)w->scrlist.pixmap_height)
	height = w->scrlist.pixmap_height;

    height += 2 * (w->scrlist.internal_item_height + w->shadow.shadow_width +
		   w->scrlist.row_spacing);
    height *= n;
    height += 2 * w->scrlist.internal_height - w->scrlist.row_spacing;

    if (height > 32767)
	height = 32767;
    else if (height <= 0)
	height = 1;

    return height;
}

static void make_visible(ScrListWidget w, long i, int may_page)
{
    long	first = FIRST(w);

    if (LINES(w) <= 0)
	return;

    if (i >= LINES(w))
	i = LINES(w) - 1;
    if (i < 0)
	i = 0;

    if (i < FIRST(w) + w->scrlist.margin_up)
	if (may_page && w->scrlist.page_up)
	    first = i - SHOWN(w) + w->scrlist.margin_down + 1;
	else
	    first = i - w->scrlist.margin_up;
    else if (i >= FIRST(w) + SHOWN(w) - w->scrlist.margin_down)
	if (may_page && w->scrlist.page_down)
	    first = i - w->scrlist.margin_up;
	else
	    first = i - SHOWN(w) + w->scrlist.margin_down + 1;

    if (i < first)
	i = first;
    else if (i >= first + SHOWN(w))
	first = i - SHOWN(w) + 1;
    if (first < 0)
	first = 0;

    if (first != FIRST(w))
	ScrollableSetVPos((Widget)w, first);
}

/*************************************************************************/

static long event_to_index(ScrListWidget w, XEvent *event)
{
    int		ex, ey;
    int		y, h = 0;
    long	i, n;

    if (!get_event_xy(event, &ex, &ey)) {
	XBell(XtDisplay(w), 0);
	return -1;
    }

    if (LINES(w) <= 0 || ex < (int)w->scrlist.internal_width ||
	ex > (int)(w->core.width - w->scrlist.internal_width))
	return -1;


    y = w->scrlist.internal_height;
    if (w->scrlist.strings)
	h = w->scrlist.font->ascent + w->scrlist.font->descent;
    if (w->scrlist.pixmaps && h < (int)w->scrlist.pixmap_height)
	h = w->scrlist.pixmap_height;
    h += 2 * (w->scrlist.internal_item_height + w->shadow.shadow_width);
    if (ey < y)
	return FIRST(w) > 0 ? FIRST(w) - 1 : 0;

    i = FIRST(w);
    n = FIRST(w) + SHOWN(w);
    if (n > LINES(w))
	n = LINES(w);

    while (i < n) {
	if (y <= ey && ey < y + h)
	    return i;
	y += h + w->scrlist.row_spacing;
	i++;
    }

    n = FIRST(w) + SHOWN(w);
    if (n >= LINES(w))
	n = LINES(w) - 1;

    return n;
}

static void select_action(Widget	 gw,
			  XEvent	*event,
			  String	*params,
			  Cardinal	*no_params)
{
    ScrListWidget	w = (ScrListWidget)gw;
    XtCallbackList	c_list = w->scrlist.select_callback;
    long		i;

    if (!w->scrlist.active)
	return;

    i = event_to_index(w, event);
    if (i < 0 || i >= LINES(w))
	return;

    ScrListSetSelected((Widget)w, i, !w->scrlist.selected[i]);

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)i);
}

static void notify(Widget	 gw,
		   XEvent	*event,
		   String	*params,
		   Cardinal	*no_params)
{
    ScrListWidget	w = (ScrListWidget)gw;
    XtCallbackList	c_list = w->scrlist.callback;
    long		i;

    if (!w->scrlist.active || !c_list)
	return;

    i = event_to_index(w, event);
    if (i < 0 || i >= LINES(w))
	return;

    XtCallCallbackList((Widget)w, c_list, (XtPointer)i);
}

static void notify_second(Widget	 gw,
			  XEvent	*event,
			  String	*params,
			  Cardinal	*no_params)
{
    ScrListWidget	w = (ScrListWidget)gw;
    XtCallbackList	c_list = w->scrlist.second_callback;
    long		i;

    if (!w->scrlist.active || !c_list)
	return;

    i = event_to_index(w, event);
    if (i < 0 || i >= LINES(w))
	return;

    XtCallCallbackList((Widget)w, c_list, (XtPointer)i);
}

static void drag_select(Widget		 gw,
			XEvent		*event,
			String		*params,
			Cardinal	*no_params)
{
    ScrListWidget	w = (ScrListWidget)gw;
    XtCallbackList	c_list = w->scrlist.select_callback;
    long		i;

    if (!w->scrlist.active)
	return;

    i = event_to_index(w, event);
    if (i < 0 || i >= LINES(w))
	return;

    if (!w->scrlist.selected[i]) {
	ScrListSetSelected((Widget)w, i, True);
	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)i);
    }
}

static void dnd_start(Widget	 gw,
		      XEvent	*event,
		      String	*params,
		      Cardinal	*no_params)
{
    ScrListWidget	w = (ScrListWidget)gw;
    long		i = event_to_index(w, event);

    if (!w->scrlist.allow_dnd || i < 0) {
	w->scrlist.dnd_start = -1;
	XBell(XtDisplay(w), 0);
	return;
    }

    w->scrlist.dnd_start = i;
    XDefineCursor(XtDisplay(w), XtWindow(w), w->scrlist.dnd_cursor);
}

static void dnd_do(Widget gw, XEvent *event,
		   String *params, Cardinal *no_params)
{
    ScrListWidget	w = (ScrListWidget)gw;
    long		i = event_to_index(w, event);

    if (w->scrlist.dnd_start < 0) {
	XBell(XtDisplay(w), 0);
	return;
    }

    if (LINES(w) <= 0)
	return;

    if (i >= LINES(w))
	i = LINES(w) - 1;
    if (i < 0)
	i = 0;

    make_visible(w, i, False);
}

static void dnd_end(Widget	 gw,
		    XEvent	*event,
		    String	*params,
		    Cardinal	*no_params)
{
    ScrListWidget	w = (ScrListWidget)gw;
    XtCallbackList	c_list = w->scrlist.dnd_callback;
    long		index[3];
    long		i;
    int			use_pixmaps = w->scrlist.use_pixmaps;
    char		*tempstr = NULL;
    Pixmap		temppix = None;
    Boolean		tempsel = False;

    XDefineCursor(XtDisplay(w), XtWindow(w), None);

    index[0] = w->scrlist.dnd_start;
    w->scrlist.dnd_start = -1;
    index[1] = event_to_index(w, event);
    if (!w->scrlist.allow_dnd || index[0] == index[1] ||
	index[0] < 0 || index[0] >= LINES(w) ||
	index[1] < 0 || index[1] >= LINES(w)) {
	XBell(XtDisplay((Widget)w), 0);
	return;
    }	      

    if (c_list) {
	index[2] = False;
	XtCallCallbackList((Widget)w, c_list, (XtPointer)index);
	if (!index[2])
	    return;
    }

    tempstr = w->scrlist.strings[index[0]];
    if (use_pixmaps)
	temppix = w->scrlist.pixmaps[index[0]];
    tempsel = w->scrlist.selected[index[0]];

    if (index[0] < index[1])
	for (i = index[0] ; i < index[1] ; i++) {
	    w->scrlist.strings[i] = w->scrlist.strings[i + 1];
	    if (use_pixmaps)
		w->scrlist.pixmaps[i] = w->scrlist.pixmaps[i + 1];
	    w->scrlist.selected[i] = w->scrlist.selected[i + 1];
	}
    else
	for (i = index[0] ; i > index[1] ; i--) {
	    w->scrlist.strings[i] = w->scrlist.strings[i - 1];
	    if (use_pixmaps)
		w->scrlist.pixmaps[i] = w->scrlist.pixmaps[i - 1];
	    w->scrlist.selected[i] = w->scrlist.selected[i - 1];
	}

    w->scrlist.strings[index[1]] = tempstr;
    if (use_pixmaps)
	w->scrlist.pixmaps[index[1]] = temppix;
    w->scrlist.selected[index[1]] = tempsel;

    if (index[0] < index[1])
	redraw_items(w, index[0], index[1], True);
    else
	redraw_items(w, index[1], index[0], True);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *num_args)
{
    ScrListWidget	new = (ScrListWidget)gnew;
    long		n_alloc = new->scrlist.n_alloc;

    new->scrlist.active = True;
    new->scrlist.strings = NULL;
    new->scrlist.pixmaps = NULL;
    new->scrlist.selected = NULL;
    new->scrlist.n_sel = 0;
    new->scrlist.n_alloc = 0;
    new->scrlist.max_width = 0;

    init_gcs(new);
    calc_shown(new);
    alloc_list(new, n_alloc > 0 ? n_alloc : 1);
    new->scrlist.dnd_start = -1;
    if (new->scrlist.at_least_one) {
	new->scrlist.selected[0] = True;
	new->scrlist.n_sel = 1;
    }
    if (new->core.width == 0)
	new->core.width = preferred_width(new);
    if (new->core.height == 0)
	new->core.height = preferred_height(new);
}

static void Destroy(Widget gw)
{
    ScrListWidget	w = (ScrListWidget)gw;

    XtFree(w->scrlist.selected);
    free_gcs(w);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    ScrListWidget	w = (ScrListWidget)gw;
    long		start, stop;
    int			height = 0;

    if (!XtIsRealized((Widget)w) || LINES(w) == 0)
	return;

    if (w->scrlist.strings)
	height = w->scrlist.font->ascent + w->scrlist.font->descent;
    if (w->scrlist.use_pixmaps && (int)w->scrlist.pixmap_height > height)
	height = w->scrlist.pixmap_height;
    height += 2 * (w->scrlist.internal_item_height + w->shadow.shadow_width) +
	w->scrlist.row_spacing;
    if (height == 0)
	return;

    if (event && event->type == Expose) {
	int	y = w->scrlist.internal_height;
	int	y1 = event->xexpose.y - y;
	int	y2 = y1 + event->xexpose.height;

	start = FIRST(w) + y1/height;
	stop = FIRST(w) + y2/height;
    } else if (event && event->type == GraphicsExpose) {
	int	y = w->scrlist.internal_height;
	int	y1 = event->xgraphicsexpose.y - y;
	int	y2 = y1 + event->xgraphicsexpose.height;

	start = FIRST(w) + y1/height;
	stop = FIRST(w) + y2/height;
    } else {
	start = FIRST(w);
	stop = start + SHOWN(w) - 1;
    }

    redraw_items(w, start, stop, False);
}

static void Resize(Widget gw)
{
    ScrListWidget	w = (ScrListWidget)gw;

    ScrollableHFromGeometry((ScrollableWidget)w);
    calc_shown(w);
    ScrollableFitHBar((ScrollableWidget)w);
    ScrollableFitVBar((ScrollableWidget)w);
}

static void Realize(Widget		   gw,
		    XtValueMask		  *mask,
		    XSetWindowAttributes  *attributes)
{
    ScrListWidget	w = (ScrListWidget)gw;

    ScrollableHFromGeometry((ScrollableWidget)w);
    calc_shown(w);
    scrollableWidgetClass->core_class.realize((Widget)w, mask, attributes);
}

static Boolean SetValues(Widget		 gcurrent,
			 Widget		 grequest,
			 Widget		 gnew,
			 ArgList	 args,
			 Cardinal	*num_args)
{
    ScrListWidget	new = (ScrListWidget)gnew;
    ScrListWidget	current = (ScrListWidget)gcurrent;
    Boolean		redisplay = False;

    new->scrlist.use_pixmaps = current->scrlist.use_pixmaps;

    if (new->core.background_pixel    != current->core.background_pixel    ||
	new->scrlist.foreground_pixel != current->scrlist.foreground_pixel ||
	new->scrlist.highlight_pixel  != current->scrlist.highlight_pixel  ||
	new->scrlist.font             != current->scrlist.font) {
	free_gcs(current);
	init_gcs(new);
	redisplay = True;
    }

    if (new->scrlist.n_alloc != current->scrlist.n_alloc) {
	long	n_alloc = new->scrlist.n_alloc;

	new->scrlist.n_alloc = current->scrlist.n_alloc;
	alloc_list(new, n_alloc);
    }

    if (new->scrlist.row_spacing     != current->scrlist.row_spacing     ||
	new->shadow.shadow_width     != current->shadow.shadow_width     ||
	new->scrlist.internal_width  != current->scrlist.internal_width  ||
	new->scrlist.internal_height != current->scrlist.internal_height ||
	new->scrlist.pixmap_width    != current->scrlist.pixmap_width    ||
	new->scrlist.pixmap_height   != current->scrlist.pixmap_height   ||
	new->scrlist.pixmap_spacing  != current->scrlist.pixmap_spacing  ||
	new->scrlist.depth_one       != current->scrlist.depth_one)
	redisplay = True;

    if (new->scrlist.internal_item_width !=
	current->scrlist.internal_item_width ||
	new->scrlist.internal_item_height !=
	current->scrlist.internal_item_height)
	redisplay = True;

    if (new->scrlist.at_least_one != current->scrlist.at_least_one &&
	new->scrlist.n_sel == 0 && LINES(new) > 0) {
	new->scrlist.selected[0] = True;
	redisplay = True;
    }

    if (new->scrlist.at_most_one != current->scrlist.at_most_one &&
	new->scrlist.n_sel > 1) {
	Boolean	*loop = new->scrlist.selected;
	int	n = LINES(new);
	    
	while (n-- > 0)
	    if (*loop++)
		break;

	if (n > 0)
	    memset(loop, 0, n);
    }

    if (new->scrlist.preferred_lines != current->scrlist.preferred_lines)
	(void)XtMakeResizeRequest((Widget)new, preferred_width(new),
				  preferred_height(new), NULL, NULL);

    if (redisplay)
	calc_shown(new);

    return redisplay;
}

static void SetVPos(ScrollableWidget gw, long pos_y)
{
    ScrListWidget	w = (ScrListWidget)gw;
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);
    long		old = FIRST(w);
    long		diff;
    long		n = SHOWN(w);
    int			y = w->scrlist.internal_height;
    int			dy = 0;

    if (LINES(w) <= 0)
	return;

    if (pos_y > LINES(w) - SHOWN(w))
	pos_y = LINES(w) - SHOWN(w);
    if (pos_y < 0)
	pos_y = 0;

    diff = pos_y - old;
    FIRST(w) = pos_y;
    if (w->scrlist.strings)
	dy = w->scrlist.font->ascent + w->scrlist.font->descent;
    if (w->scrlist.pixmaps && (int)w->scrlist.pixmap_height > dy)
	dy = w->scrlist.pixmap_height;
    dy += 2 * (w->scrlist.internal_item_height + w->shadow.shadow_width) +
	w->scrlist.row_spacing;
    if (dy == 0 || diff == 0 || !XtIsRealized((Widget)w))
	return;

    if (diff <= -n || diff >= n) {
	XClearWindow(disp, win);
	redraw_items(w, pos_y, pos_y + n - 1, False);
    } else if (diff < 0) {
	XCopyArea(disp, win, win, w->scrlist.default_gc,
		  0, y,
		  w->core.width, (n + diff) * dy,
		  0, y - diff * dy);
	XClearArea(disp, win, 0, 0, 0, y - diff * dy, False);
	redraw_items(w, pos_y, pos_y - diff - 1, False);
    } else if (diff > 0) {
	XCopyArea(disp, win, win, w->scrlist.default_gc,
		  0, y + diff * dy,
		  w->core.width, (n - diff) * dy,
		  0, y);
	XClearArea(disp, win, 0, y + (n - diff) * dy, 0, 0, False);
	redraw_items(w, pos_y + n - diff, pos_y + n - 1, False);
    }
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    ScrListWidget	w = (ScrListWidget)gw;
    Dimension		intended_width;
    Dimension		intended_height;

    preferred->request_mode = CWHeight | CWWidth;
    preferred->height = preferred_height(w);
    preferred->width = preferred_width(w);

    if (intended->request_mode & CWWidth)
	intended_width = intended->width;
    else
	intended_width = w->core.width;
    if (intended->request_mode & CWHeight)
	intended_height = intended->height;
    else
	intended_height = w->core.height;

    if (intended_width == preferred->width &&
	intended_height == preferred->height)
	return XtGeometryYes;
    if (preferred->width == w->core.width &&
	preferred->height == w->core.height)
	return XtGeometryNo;
    return XtGeometryAlmost;
}

/*************************************************************************/

void ScrListClearLines(Widget gw)
{
    ScrListWidget	w = (ScrListWidget)gw;
    long		i;

    for (i = 0 ; i < LINES(w) ; i++) {
	XtFree(w->scrlist.strings[i]);
	w->scrlist.strings[i] = NULL;
	w->scrlist.selected[i] = False;
	if (w->scrlist.pixmaps)
	    w->scrlist.pixmaps[i] = None;
    }

    XtFree((char *)w->scrlist.strings);
    w->scrlist.strings = NULL;
    XtFree((char *)w->scrlist.pixmaps);
    w->scrlist.pixmaps = NULL;
    XtFree((char *)w->scrlist.selected);
    w->scrlist.selected = NULL;
    w->scrlist.n_alloc = 0;

    LINES(w) = 0;
    FIRST(w) = 0;
    w->scrlist.dnd_start = -1;
    w->scrlist.n_sel = 0;
    w->scrlist.max_width = 0;

    if (XtIsRealized((Widget)w))
	XClearWindow(XtDisplay(w), XtWindow(w));

    ScrollableFitVBar((ScrollableWidget)w);
}

long ScrListAddLine(Widget gw, char *string, Pixmap pixmap)
{
    ScrListWidget	w = (ScrListWidget)gw;
    long		n = LINES(w)++;
    int			len, width;

    if (n + 3 > w->scrlist.n_alloc)
	alloc_list(w, 2 * (w->scrlist.n_alloc + 1));
    len = strlen(string);
    w->scrlist.strings[n] = strcpy(XtMalloc(len + 1), string);
    if (w->scrlist.use_pixmaps)
	w->scrlist.pixmaps[n] = pixmap;
    width = XTextWidth(w->scrlist.font, string, len);
    if (width > w->scrlist.max_width)
	w->scrlist.max_width = width;

    if (n == 0 && w->scrlist.at_least_one) {
	w->scrlist.selected[0] = True;
	w->scrlist.n_sel = 1;
    }

    if (XtIsRealized((Widget)w) &&
	n >= FIRST(w) && n < FIRST(w) + SHOWN(w))
	redraw_items(w, n, n, False);

    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);

    return n;
}

void ScrListSetLine(Widget gw, long row, char *string, Pixmap pixmap)
{
    ScrListWidget	w = (ScrListWidget)gw;
    int			len, width;

    if (row < 0 || row >= LINES(w))
	return;

    XtFree(w->scrlist.strings[row]);
    len = strlen(string);
    w->scrlist.strings[row] = strcpy(XtMalloc(len + 1), string);
    if (w->scrlist.use_pixmaps)
	w->scrlist.pixmaps[row] = pixmap;
    width = XTextWidth(w->scrlist.font, string, len);
    if (width > w->scrlist.max_width)
	w->scrlist.max_width = width;

    if (XtIsRealized((Widget)w))
	redraw_items(w, row, row, True);
}

void ScrListDeleteLine(Widget gw, long row)
{
    ScrListWidget	w = (ScrListWidget)gw;
    int			use_pixmaps = w->scrlist.use_pixmaps;
    int			was_sel;
    long		i, n;

    if (row < 0 || row >= LINES(w))
	return;

    was_sel = w->scrlist.selected[row];
    if (was_sel)
	w->scrlist.n_sel--;
    XtFree(w->scrlist.strings[row]);
    n = --LINES(w);
    for (i = row ; i < n ; i++) {
	w->scrlist.strings[i] = w->scrlist.strings[i + 1];
	if (use_pixmaps)
	    w->scrlist.pixmaps[i] = w->scrlist.pixmaps[i + 1];
	w->scrlist.selected[i] = w->scrlist.selected[i + 1];
    }
    w->scrlist.strings[n] = NULL;

    if (was_sel && w->scrlist.at_least_one &&
	w->scrlist.n_sel == 0 && LINES(w) > 0) {
	if (row < LINES(w) - 1)
	    w->scrlist.selected[row + 1] = True;
	else
	    w->scrlist.selected[LINES(w) - 1] = True;
	w->scrlist.n_sel = 1;
    }

    if (FIRST(w) >= LINES(w) && LINES(w) > 0)
	FIRST(w)--;

    if (XtIsRealized((Widget)w)) {
	XClearWindow(XtDisplay(w), XtWindow(w));
	Redisplay((Widget)w, NULL, NULL);
    }

    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

void ScrListSetSelected(Widget gw, long i, int select)
{
    ScrListWidget	w = (ScrListWidget)gw;

    if (i < 0 || i >= LINES(w))
	return;

    select = !!select;
    if (w->scrlist.selected[i] == select)
	return;

    if (select) {
	if (w->scrlist.at_most_one && w->scrlist.n_sel > 0) {
	    long	j;
	    Boolean	*sel = w->scrlist.selected;

	    for (j = 0 ; j < LINES(w) ; j++)
		if (sel[j]) {
		    sel[j] = False;
		    redraw_items(w, j, j, True);
		}
	    w->scrlist.n_sel = 0;
	}
	w->scrlist.n_sel++;
	w->scrlist.selected[i] = True;
	redraw_items(w, i, i, True);
    } else if (!w->scrlist.at_least_one || w->scrlist.n_sel > 1) {
	w->scrlist.n_sel--;
	w->scrlist.selected[i] = False;
	redraw_items(w, i, i, True);
    }
}

void ScrListMakeVisible(Widget gw, long i)
{
    make_visible((ScrListWidget)gw, i, True);
}

int ScrListGetSelected(Widget gw, long i)
{
    ScrListWidget w = (ScrListWidget)gw;

    return i >= 0 && i < LINES(w) && w->scrlist.selected[i];
}

long ScrListGetFirstSelected(Widget gw)
{
    return ScrListGetNextSelected(gw, -1);
}

long ScrListGetNextSelected(Widget gw, long i)
{
    ScrListWidget	w = (ScrListWidget)gw;
    Boolean		*loop = w->scrlist.selected;
    long		n = LINES(w);

    i++;
    if (i < 0)
	return -1;
    loop += i;
    while (i < n) {
	if (*loop)
	    return i;
	loop++;
	i++;
    }

    return -1;
}

char *ScrListGetString(Widget gw, long row)
{
    ScrListWidget	w = (ScrListWidget)gw;

    if (row < 0 || row >= LINES(w))
	return NULL;

    return w->scrlist.strings[row];
}

Pixmap ScrListGetPixmap(Widget gw, long row)
{
    ScrListWidget	w = (ScrListWidget)gw;

    if (!w->scrlist.use_pixmaps ||
	row < 0 || row >= LINES(w))
	return None;

    return w->scrlist.pixmaps[row];
}

void ScrListPurgePixmap(Widget gw, Pixmap pixmap)
{
    ScrListWidget	w = (ScrListWidget)gw;
    long		i;

    if (!w->scrlist.use_pixmaps)
	return;

    for (i = 0 ; i < LINES(w) ; i++) {
	if (w->scrlist.pixmaps[i] == pixmap) {
	    w->scrlist.pixmaps[i] = None;
	    redraw_items(w, i, i, True);
	}
    }
}

long ScrListEventToIndex(Widget gw, XEvent *event)
{
    ScrListWidget	w = (ScrListWidget)gw;

    return event_to_index(w, event);    
}

void ScrListSetActive(Widget gw, int active)
{
    ScrListWidget	w = (ScrListWidget)gw;

    w->scrlist.active = active;
}
