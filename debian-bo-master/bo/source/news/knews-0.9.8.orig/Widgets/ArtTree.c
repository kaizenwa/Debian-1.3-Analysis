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
#include "ArtTreeP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(ArtTreeRec, arttree.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, XtDefaultForeground},
    {XtNinnerColor, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(inner_pixel), XtRString, XtDefaultForeground},
    {XtNouterColor, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(outer_pixel), XtRString, XtDefaultForeground},
    {XtNrubberColor, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(rubber_pixel), XtRString, XtDefaultForeground},
    {XtNinnerDashed, XtCDashed, XtRBoolean, sizeof(Boolean),
     offset(inner_dashed), XtRImmediate, (XtPointer)False},
    {XtNouterDashed, XtCDashed, XtRBoolean, sizeof(Boolean),
     offset(outer_dashed), XtRImmediate, (XtPointer)False},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, XtDefaultFont},
    {XtNrowSpacing, XtCRowSpacing, XtRDimension, sizeof(Dimension),
     offset(row_spacing), XtRImmediate, (XtPointer)6},
    {XtNcolumnSpacing, XtCColumnSpacing, XtRDimension, sizeof(Dimension),
     offset(column_spacing), XtRImmediate, (XtPointer)32},
    {XtNinternalWidth, XtCInternalWidth, XtRDimension, sizeof(Dimension),
     offset(internal_width), XtRImmediate, (XtPointer)16},
    {XtNinternalHeight, XtCInternalHeight, XtRDimension, sizeof(Dimension),
     offset(internal_height), XtRImmediate, (XtPointer)8},
    {XtNinternalNodeWidth, XtCInternalNodeWidth,
     XtRDimension, sizeof(Dimension),
     offset(internal_node_width), XtRImmediate, (XtPointer)4},
    {XtNinternalNodeHeight, XtCInternalNodeHeight,
     XtRDimension, sizeof(Dimension),
     offset(internal_node_height), XtRImmediate, (XtPointer)0},
    {XtNnodeColumns, XtCNodeColumns, XtRDimension, sizeof(Dimension),
     offset(node_columns), XtRImmediate, (XtPointer)16},
    {XtNnodeRows, XtCNodeRows, XtRDimension, sizeof(Dimension),
     offset(node_rows), XtRImmediate, (XtPointer)1},
    {XtNpixmapWidth, XtCPixmapWidth, XtRDimension, sizeof(Dimension),
     offset(pixmap_width), XtRImmediate, (XtPointer)0},
    {XtNpixmapHeight, XtCPixmapHeight, XtRDimension, sizeof(Dimension),
     offset(pixmap_height), XtRImmediate, (XtPointer)0},
    {XtNpixmapSpacing, XtCPixmapSpacing, XtRDimension, sizeof(Dimension),
     offset(pixmap_spacing), XtRImmediate, (XtPointer)8},
    {XtNdepthOne, XtCDepthOne, XtRBoolean, sizeof(Boolean),
     offset(depth_one), XtRImmediate, (XtPointer)True},
    {XtNtree, XtCTree, XtRPointer, sizeof(ART_TREE_NODE*),
     offset(root), XtRImmediate, (XtPointer)NULL},
    {XtNwarpPointer, XtCWarpPointer, XtRBoolean, sizeof(Boolean),
     offset(warp_pointer), XtRImmediate, (XtPointer)True},
    {XtNvertical, XtCVertical, XtRBoolean, sizeof(Boolean),
     offset(vertical), XtRImmediate, (XtPointer)True},
    {XtNinnerCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(inner_callback), XtRCallback, (XtPointer)NULL},
    {XtNouterCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(outer_callback), XtRCallback, (XtPointer)NULL},
    {XtNselectCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(select_callback), XtRCallback, (XtPointer)NULL},
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRCallback, (XtPointer)NULL},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static void	Redisplay(Widget, XEvent*, Region);
static void	SuspendHook(ScrollableWidget);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);

static void start_rubberband(Widget, XEvent*, String*, Cardinal*);
static void move_rubberband(Widget, XEvent*, String*, Cardinal*);
static void stop_rubberband(Widget, XEvent*, String*, Cardinal*);
static void set_inner(Widget, XEvent*, String*, Cardinal*);
static void reset_inner(Widget, XEvent*, String*, Cardinal*);
static void toggle_inner(Widget, XEvent*, String*, Cardinal*);
static void set_outer(Widget, XEvent*, String*, Cardinal*);
static void reset_outer(Widget, XEvent*, String*, Cardinal*);
static void toggle_outer(Widget, XEvent*, String*, Cardinal*);
static void set_selected(Widget, XEvent*, String*, Cardinal*);
static void reset_selected(Widget, XEvent*, String*, Cardinal*);
static void toggle_selected(Widget, XEvent*, String*, Cardinal*);
static void notify(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"start-rubberband",	start_rubberband},
    {"move-rubberband",		move_rubberband},
    {"stop-rubberband",		stop_rubberband},
    {"set-inner",		set_inner},
    {"reset-inner",		reset_inner},
    {"toggle-inner",		toggle_inner},
    {"set-outer",		set_outer},
    {"reset-outer",		reset_outer},
    {"toggle-outer",		toggle_outer},
    {"set-selected",		set_selected},
    {"reset-selected",		reset_selected},
    {"toggle-selected",		toggle_selected},
    {"notify",			notify},
};

static char translations[] =
"<Btn2Down>:	start-rubberband() \n"
"<Btn2Up>:	stop-rubberband() \n"
"<Btn2Motion>:	move-rubberband() \n";

ArtTreeClassRec artTreeClassRec = {
    {					/* core fields			*/
	(WidgetClass) &scrollableClassRec,	/* superclass		*/
	"ArtTree",			/* class_name			*/
	sizeof(ArtTreeRec),		/* widget_size			*/
	NULL,				/* class_initialize		*/
	NULL,				/* class_part_initialize	*/
	FALSE,				/* class_inited			*/
	Initialize,			/* initialize			*/
	NULL,				/* initialize_hook		*/
	XtInheritRealize,		/* realize			*/
	actions,			/* actions			*/
	XtNumber(actions),		/* num_actions			*/
	resources,			/* resources			*/
	XtNumber(resources),		/* num_resources		*/
	NULLQUARK,			/* xrm_class			*/
	TRUE,				/* compress_motion		*/
#if (XtSpecificationRelease < 4)
	TRUE,				/* compress_exposure		*/
#else
	XtExposeCompressMaximal,	/* compress_exposure		*/
#endif
	TRUE,				/* compress_enterleave		*/
	FALSE,				/* visible_interest		*/
	Destroy,			/* destroy			*/
	XtInheritResize,		/* resize			*/
	Redisplay,			/* expose			*/
	SetValues,			/* set_values			*/
	NULL,				/* set_values_hook		*/
	XtInheritSetValuesAlmost,	/* set_values_almost		*/
	NULL,				/* get_values_hook		*/
	NULL,				/* accept_focus			*/
	XtVersion,			/* version			*/
	NULL,				/* callback_private		*/
	translations,			/* tm_table			*/
	NULL,				/* query_geometry		*/
	XtInheritDisplayAccelerator,	/* display_accelerator		*/
	NULL				/* extension			*/
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
    {					/* scrollable fields		*/
	XtInheritScrollableSetPos,	/* set_hpos			*/
	XtInheritScrollableSetPos,	/* set_vpos			*/
	SuspendHook,			/* suspend_hook			*/
	NULL,				/* extension			*/
    },
    {					/* art_tree fields		*/
	0				/* empty			*/
    }
};

WidgetClass artTreeWidgetClass = (WidgetClass)&artTreeClassRec;

/*************************************************************************/

static ART_TREE_NODE *find_node_by_coordinates(ArtTreeWidget w,
					       ART_TREE_NODE *root,
					       int x, int y)
{
    while (root) {
	ART_TREE_NODE	*temp;
	int		dx = x - root->hook->x;
	int		dy = y - root->hook->y;

	if (dx >= 0 && dy >= 0 &&
	    dx <= w->arttree.node_width &&
	    dy <= w->arttree.node_height)
	    return root;
	if (root->child1 &&
	    (temp = find_node_by_coordinates(w, root->child1, x, y)) )
	    return temp;
	root = root->sibling;
    }

    return NULL;
}

static void draw_rubberband(ArtTreeWidget w)
{
    Widget		pw = XtParent(w);
    long		width   = w->core.width;  /* need to loose */
    long		height  = w->core.height; /* unsignedness  */
    long		pwidth  = pw->core.width;
    long		pheight = pw->core.height;
    int			x, y;
    unsigned int	dx, dy;

    x = - w->core.x - w->core.x * pwidth / width;
    dx = (pwidth - 1) * pwidth / width;
    y = - w->core.y - w->core.y * pheight / height;
    dy = (pheight - 1) * pheight / height;
    if (dx == 0)
	dx = 1;
    if (dy == 0)
	dy = 1;

    XDrawRectangle(XtDisplay(w), XtWindow(w),
		   w->arttree.rubber_gc,
		   x, y, dx, dy);
}

static void start_rubberband(Widget gw, XEvent *event,
			     String *str, Cardinal *n)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;

    if (w->arttree.warp_pointer) {
	Widget	pw = XtParent(w);
	long	width   = w->core.width;
	long	height  = w->core.height;
	long	pwidth  = pw->core.width;
	long	pheight = pw->core.height;

	x = (pwidth / 2 - w->core.x) * pwidth / width;
	w->arttree.ptr_init_x = x;
	y = (pheight / 2 - w->core.y) * pheight / height;
	w->arttree.ptr_init_y = y;
	
	XWarpPointer(XtDisplay(pw), None, XtWindow(pw), 0, 0, 0, 0, x, y);
    } else {
	if (!get_event_xy(event, &x, &y)) {
	    XBell(XtDisplay(w), 0);
	    return;
	}
	
	w->arttree.ptr_init_x = x + w->core.x;
	w->arttree.ptr_init_y = y + w->core.y;
    }
    
    w->arttree.rubberbanding = True;
    w->arttree.init_x = w->core.x;
    w->arttree.init_y = w->core.y;

    draw_rubberband(w);
    ScrollableFitHBar((ScrollableWidget)w);
    ScrollableFitVBar((ScrollableWidget)w);
}

static void stop_rubberband(Widget gw, XEvent *event,
			    String *str, Cardinal *n)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (!w->arttree.rubberbanding)
	return;

    draw_rubberband(w);
    w->arttree.rubberbanding = False;
}

static void move_rubberband(Widget gw, XEvent *event,
			    String *str, Cardinal *n)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    Widget		pw = XtParent(w);
    long		width   = w->core.width;  /* need to loose */
    long		height  = w->core.height; /* unsignedness  */
    long		pwidth  = pw->core.width;
    long		pheight = pw->core.height;
    XtWidgetGeometry	req;
    int			x, y;
    Window		w1, w2;
    int			i1, i2;
    unsigned int	ui1;

    if (!w->arttree.rubberbanding)
	return;

    draw_rubberband(w);

    /*
     *  Get rid of the race by explicitly querying the pointer.
     */
    XQueryPointer(XtDisplay(w), XtWindow(pw),
		  &w1, &w2, &i1, &i2, &x, &y, &ui1);

    x = w->arttree.init_x + (w->arttree.ptr_init_x - x) * width / pwidth;
    y = w->arttree.init_y + (w->arttree.ptr_init_y - y) * height / pheight;

    req.request_mode = CWX | CWY;
    req.x = x;
    req.y = y;
    XtMakeGeometryRequest((Widget)w, &req, NULL);
    draw_rubberband(w);

    ScrollableHFromGeometry((ScrollableWidget)w);
    ScrollableVFromGeometry((ScrollableWidget)w);
    ScrollableFitHBar((ScrollableWidget)w);
    ScrollableFitVBar((ScrollableWidget)w);
}

static void notify(Widget gw, XEvent *event,
		   String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.callback;

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void set_inner(Widget gw, XEvent *event,
		      String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.inner_callback;

	ArtTreeNodeSetInner((Widget)w, node, True);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void reset_inner(Widget gw, XEvent *event,
			String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.inner_callback;

	ArtTreeNodeSetInner((Widget)w, node, False);

	if (c_list) XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void toggle_inner(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.inner_callback;

	ArtTreeNodeSetInner((Widget)w, node, !node->hook->inner);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void set_outer(Widget gw, XEvent *event,
		      String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.outer_callback;

	ArtTreeNodeSetOuter((Widget)w, node, True);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void reset_outer(Widget gw, XEvent *event,
			String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.outer_callback;

	ArtTreeNodeSetOuter((Widget)w, node, False);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void toggle_outer(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.outer_callback;

	ArtTreeNodeSetOuter((Widget)w, node, !node->hook->outer);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void set_selected(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.select_callback;

	ArtTreeNodeSetSelected((Widget)w, node, True);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void reset_selected(Widget gw, XEvent *event,
			   String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.select_callback;

	ArtTreeNodeSetSelected((Widget)w, node, False);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

static void toggle_selected(Widget gw, XEvent *event,
			    String *params, Cardinal *no_params)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			x, y;
    ART_TREE_NODE	*node;

    if (!w->arttree.active || !get_event_xy(event, &x, &y))
	return;

    node = find_node_by_coordinates(w, w->arttree.root, x, y);
    if (node) {
	XtCallbackList	c_list = w->arttree.select_callback;

	ArtTreeNodeSetSelected((Widget)w, node, !node->hook->selected);

	if (c_list)
	    XtCallCallbackList((Widget)w, c_list, (XtPointer)node);
    }
}

/***********************************************************************/

static void free_gcs(ArtTreeWidget w)
{
    XtReleaseGC((Widget)w, w->arttree.default_gc);
    XtReleaseGC((Widget)w, w->arttree.bg_gc);
    XtReleaseGC((Widget)w, w->arttree.inner_gc);
    XtReleaseGC((Widget)w, w->arttree.outer_gc);
    XtReleaseGC((Widget)w, w->arttree.rubber_gc);
}

static void free_dashed_gcs(ArtTreeWidget w)
{
    if (w->arttree.light_dashed_gc != 0)
	XtReleaseGC((Widget)w, w->arttree.light_dashed_gc);
    XtReleaseGC((Widget)w, w->arttree.dark_dashed_gc);
}

#define MIN(a,b) (unsigned short) (a < b) ? a : b
static void init_gcs(ArtTreeWidget w)
{
    XGCValues	values;
    long	mask;

    /* default gc */
    values.foreground = w->arttree.foreground_pixel;
    values.font = w->arttree.font->fid;
    w->arttree.default_gc =
	XtGetGC((Widget)w, GCForeground | GCFont, &values);
    /* bg_gc */
    values.foreground = w->core.background_pixel;
    w->arttree.bg_gc = XtGetGC((Widget)w, GCForeground, &values);
    /* inner_gc */
    mask = GCForeground | GCLineWidth;
    values.foreground = w->arttree.inner_pixel;
    values.line_width = 1;
    if (w->arttree.inner_dashed) {
	mask |= GCLineStyle;
	values.line_style = LineOnOffDash;
    }
    w->arttree.inner_gc = XtGetGC((Widget)w, mask, &values);
    /* outer_gc */
    mask = GCForeground | GCLineWidth;
    values.foreground = w->arttree.outer_pixel;
    values.line_width = 1;
    if (w->arttree.outer_dashed) {
	mask |= GCLineStyle;
	values.line_style = LineOnOffDash;
    }
    w->arttree.outer_gc = XtGetGC((Widget)w, mask, &values);

    /* rubber gc */
    values.function = GXxor;
    values.foreground = w->core.background_pixel ^ w->arttree.rubber_pixel;
    w->arttree.rubber_gc =
	XtGetGC((Widget)w, GCFunction | GCForeground, &values);
}

static void init_dashed_gcs(ArtTreeWidget w)
{
    XGCValues	values;
    long	mask;

    values.line_width = 1;
    values.line_style = LineOnOffDash;
    mask = GCLineWidth | GCLineStyle;
    if (w->shadow.alloced_shadow_pixels) {
	mask |= GCForeground;
	values.foreground = w->shadow.light_pixel;
	w->arttree.light_dashed_gc = XtGetGC((Widget)w, mask, &values);
	values.foreground = w->shadow.dark_pixel;
	w->arttree.dark_dashed_gc = XtGetGC((Widget)w, mask, &values);
    } else if (w->shadow.line_mode) {
	mask |= GCForeground;
	/* depth == 1 */
	values.foreground = (w->core.background_pixel == 1) ? 0 : 1;
	w->arttree.light_dashed_gc = XtGetGC((Widget)w, mask, &values);
	w->arttree.dark_dashed_gc = 0;
    } else {
	Screen	*scr = XtScreen(w);
	Visual	*vis = get_visual((Widget)w);
	long	temp_mask;
	Pixel	black, white;

	black_and_white(scr, vis, &black, &white);

	if (w->shadow.light_pixmap == None) { /* never true */
	    temp_mask = mask | GCForeground;
	    values.foreground =
		(w->core.background_pixel == black) ? white : black;
	} else {
	    temp_mask = mask | GCTile | GCFillStyle;
	    values.fill_style = FillTiled;
	    values.tile = w->shadow.light_pixmap;
	}
	w->arttree.light_dashed_gc =
	    XtGetGC((Widget)w, temp_mask, &values);

	if (w->shadow.dark_pixmap == None) {
	    temp_mask = mask | GCForeground;
	    values.foreground =
		(w->core.background_pixel == black) ? white : black;
	} else {
	    temp_mask = mask | GCTile | GCFillStyle;
	    values.fill_style = FillTiled;
	    values.tile = w->shadow.dark_pixmap;
	}
	w->arttree.dark_dashed_gc =
	    XtGetGC((Widget)w, temp_mask, &values);
    }
}

static void sub_layout_horiz(ArtTreeWidget w, ART_TREE_NODE *node,
			     int x, int y, int *max_x, int *max_y)
{
    for (;;) {
	node->hook->x = x;
	node->hook->y = y;

	if (node->child1) {
	    int	sub_x = *max_x;
	    int	sub_y = *max_y;

	    sub_layout_horiz(w, node->child1, x + w->arttree.node_width +
			     w->arttree.column_spacing, y, &sub_x, &sub_y);
	    node->hook->bb_width = sub_x - node->child1->hook->x;
	    node->hook->bb_height = sub_y - node->child1->hook->y;
	    if (sub_x > *max_x)
		*max_x = sub_x;
	    y = sub_y;
	} else {
	    if (x + w->arttree.node_width > *max_x)
		*max_x = x + w->arttree.node_width;
	    y += w->arttree.node_height;
	}
	
	node = node->sibling;
	if (node)
	    y += w->arttree.row_spacing;
	else
	    break;
    }

    *max_y = y;
}

static void sub_layout_vert(ArtTreeWidget w, ART_TREE_NODE *node,
			    int x, int y, int *max_x, int *max_y)
{
    node->hook->x = x;
    node->hook->y = y;
    y += w->arttree.node_height;

    for (;;) {
	if (node->child1) {
	    Position	x1 = x + 2 * w->arttree.column_spacing;

	    sub_layout_vert(w, node->child1,
			    x1, y + w->arttree.row_spacing, max_x, max_y);
	    node->hook->bb_width = *max_x - x;
	    node->hook->bb_height = *max_y - y + w->arttree.node_height;
	    y = *max_y;
	} else {
	    node->hook->bb_width = 0;
	    node->hook->bb_height = 0;
	}

	node = node->sibling;
	if (!node)
	    break;

	y += w->arttree.row_spacing;
	node->hook->x = x;
	node->hook->y = y;
	y += w->arttree.node_height;
    }

    *max_y = y;
    x += w->arttree.node_width;
    if (*max_x < x)
	*max_x = x;
}

static void layout(ArtTreeWidget w)
{
    if (w->arttree.root) {
	int	max_x = w->arttree.internal_width;
	int	max_y = w->arttree.internal_height;

	if (w->arttree.vertical)
	    sub_layout_vert(w, w->arttree.root, max_x, max_y, &max_x, &max_y);
	else
	    sub_layout_horiz(w, w->arttree.root, max_x, max_y, &max_x, &max_y);
	max_x += w->arttree.internal_width;
	max_y += w->arttree.internal_height;
	XtMakeResizeRequest((Widget)w, max_x, max_y, NULL, NULL);
	ScrollableHFromGeometry((ScrollableWidget)w);
	ScrollableVFromGeometry((ScrollableWidget)w);
	ScrollableFitHBar((ScrollableWidget)w);
	ScrollableFitVBar((ScrollableWidget)w);
    }
}

static void init_node_sizes(ArtTreeWidget w)
{
    w->arttree.node_width =
	2 * (w->arttree.internal_node_width + w->shadow.shadow_width) +
	w->arttree.node_columns * (w->arttree.font->max_bounds.lbearing +
				   w->arttree.font->max_bounds.rbearing);
    w->arttree.node_height =
	1 + 2 * (w->arttree.internal_node_height + w->shadow.shadow_width) +
	w->arttree.node_rows * (w->arttree.font->ascent +
				w->arttree.font->descent);
}

/*************************************************************************/

static void draw_outer_border(ArtTreeWidget w, ART_TREE_NODE *node)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    XPoint	points[6];
    GC		gc;

    gc = node->hook->outer ? w->arttree.outer_gc : w->arttree.bg_gc;

    if (node->child1)
	if (node->parent)
	    if (w->arttree.vertical) {
		points[0].x = node->hook->x + w->arttree.column_spacing - 1;
		points[1].x = points[2].x = node->hook->x - 1;
		points[0].y = points[1].y =
		    node->hook->y + w->arttree.node_height;
		points[2].y = node->hook->y + w->arttree.node_height / 2 + 1;
		XDrawLines(disp, win, gc, points, 3, CoordModeOrigin);

		points[0].x = points[1].x = node->hook->x - 1;
		points[2].x = points[3].x =
		    node->hook->x + w->arttree.node_width;
		points[4].x = node->hook->x + w->arttree.column_spacing + 1;
		points[0].y = node->hook->y + w->arttree.node_height / 2 - 1;
		points[1].y = points[2].y = node->hook->y - 1;
		points[3].y = points[4].y =
		    node->hook->y + w->arttree.node_height;
		XDrawLines(disp, win, gc, points, 5, CoordModeOrigin);
	    } else {
		points[0].x = points[1].x = node->hook->x - 1;
		points[2].x = points[3].x =
		    node->hook->x + w->arttree.node_width;
		points[0].y = points[3].y =
		    node->hook->y + w->arttree.node_height / 2 - 1;
		points[1].y = points[2].y = node->hook->y - 1;
		XDrawLines(disp, win, gc, points, 4, CoordModeOrigin);

		points[0].y += 3;
		points[3].y += 3;
		points[1].y = points[2].y =
		    node->hook->y + w->arttree.node_height;
		XDrawLines(disp, win, gc, points, 4, CoordModeOrigin);
	    }
	else
	    if (w->arttree.vertical) {
		points[0].x = node->hook->x + w->arttree.column_spacing - 1;
		points[1].x = points[2].x = node->hook->x - 1;
		points[3].x = points[4].x =
		    node->hook->x + w->arttree.node_width;
		points[5].x = node->hook->x + w->arttree.column_spacing + 1;
		points[0].y = points[1].y = points[4].y = points[5].y =
		    node->hook->y + w->arttree.node_height;
		points[2].y = points[3].y = node->hook->y - 1;
		XDrawLines(disp, win, gc, points, 6, CoordModeOrigin);
	    } else {
		points[0].x = points[1].x = points[4].x = points[5].x =
		    node->hook->x + w->arttree.node_width;
		points[2].x = points[3].x = node->hook->x - 1;
		points[0].y = node->hook->y + w->arttree.node_height / 2 - 1;
		points[5].y = points[0].y + 3;
		points[1].y = points[2].y = node->hook->y - 1;
		points[3].y = points[4].y =
		    node->hook->y + w->arttree.node_height;
		XDrawLines(disp, win, gc, points, 6, CoordModeOrigin);
	    }
    else
	if (!node->parent)
	    XDrawRectangle(disp, win, gc,
			   node->hook->x - 1, node->hook->y - 1,
			   w->arttree.node_width + 1,
			   w->arttree.node_height + 1);
	else {
	    points[0].x = points[1].x =
		points[4].x = points[5].x = node->hook->x - 1;
	    points[2].x = points[3].x =
		node->hook->x + w->arttree.node_width;
	    points[0].y =
		node->hook->y + w->arttree.node_height / 2 - 2;
	    points[5].y = points[0].y + 3;
	    points[1].y = points[2].y = node->hook->y - 1;
	    points[3].y = points[4].y =
		node->hook->y + w->arttree.node_height;
	    XDrawLines(disp, win, gc, points, 6, CoordModeOrigin);
	}
}

static void draw_inner_border(ArtTreeWidget w, ART_TREE_NODE *node)
{
    Display	*disp = XtDisplay(w);
    Window	win   = XtWindow(w);
    int		sw = w->shadow.shadow_width ;
    GC		gc;

    if (node->hook->inner)
	gc = w->arttree.inner_gc;
    else if (node->hook->selected && w->shadow.arm_gc != 0)
	gc = w->shadow.arm_gc;
    else
	gc = w->arttree.bg_gc;

    XDrawRectangle(disp, win, gc,
		   node->hook->x + sw, node->hook->y + sw,
		   w->arttree.node_width - 2 * sw - 1,
		   w->arttree.node_height - 2 * sw - 1);
}

static void draw_node(ArtTreeWidget w, ART_TREE_NODE *node)
{
    Display	*disp = XtDisplay(w);
    Window	win   = XtWindow(w);
    int		x, y;

    if (node->hook->selected && w->shadow.arm_gc != 0)
	XFillRectangle(disp, win, w->shadow.arm_gc,
		       node->hook->x, node->hook->y,
		       w->arttree.node_width, w->arttree.node_height);
    ShadowDrawShadows((ShadowWidget)w,
		      node->hook->x, node->hook->y,
		      w->arttree.node_width, w->arttree.node_height,
		      node->hook->selected);

    if (node->hook->inner)
	draw_inner_border(w, node);
 
    if (node->hook->outer)
	draw_outer_border(w, node);
    
    x = node->hook->x + w->arttree.internal_node_width +
	w->shadow.shadow_width;
    y = node->hook->y;
    if (node->hook->pixmap != None) {
	if (w->arttree.depth_one)
	    XCopyPlane(disp, node->hook->pixmap, win,
		       w->arttree.default_gc, 0, 0,
		       w->arttree.pixmap_width, w->arttree.pixmap_height,
		       x, y + (int)(w->arttree.node_height -
				    w->arttree.pixmap_height) / 2 +
		       (node->hook->selected ? 1 : 0), 1);
	else
	    XCopyArea(disp, node->hook->pixmap, win,
		      w->arttree.default_gc, 0, 0,
		      w->arttree.pixmap_width, w->arttree.pixmap_height,
		      x, y + (int)(w->arttree.node_height -
				   w->arttree.pixmap_height) / 2 +
		      (node->hook->selected ? 1 : 0));
	x += w->arttree.pixmap_width + w->arttree.pixmap_spacing;
    }
    y += w->arttree.font->ascent + w->arttree.internal_node_height +
	w->shadow.shadow_width;
    if (node->hook->selected)
	y++;
    if (node->label)
	XDrawString(disp, win, w->arttree.default_gc, x, y,
		    node->label, node->hook->label_len);
}

static void draw_subnodes(ArtTreeWidget w, ART_TREE_NODE *node, Region region)
{
    while (node) {
	if (!region ||
	    XRectInRegion(region, node->hook->x - 1, node->hook->y - 1,
			  w->arttree.node_width + 2,
			  w->arttree.node_height + 2) != RectangleOut)
	    draw_node(w, node);
	if (node->child1) {
	    if (!region ||
		XRectInRegion(region, node->child1->hook->x - 1,
			      node->child1->hook->y - 1,
			      node->hook->bb_width,
			      node->hook->bb_height) != RectangleOut)
		draw_subnodes(w, node->child1, region);
	}
	node = node->sibling;
    }
}

static void draw_branches(ArtTreeWidget w, ART_TREE_NODE *node)
{
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);
    ART_TREE_NODE	*child = node->child1;
    GC			gc1, gc2;
    int			x, y, y0;

    if (!child)
	return;

    if (w->arttree.vertical) {
	x = node->hook->x + w->arttree.column_spacing;
	y0 = node->hook->y + w->arttree.node_height - 1;
	y = child->hook->y + w->arttree.node_height / 2;
    } else {
	x = (node->hook->x + w->arttree.node_width + child->hook->x)/2;
	y = y0 = node->hook->y + w->arttree.node_height/2;
	XDrawLine(disp, win,
		  w->shadow.line_mode ? w->shadow.light_gc : w->shadow.dark_gc,
		  node->hook->x + w->arttree.node_width, y - 1, x + 1, y - 1);
	XDrawLine(disp, win, w->shadow.light_gc,
		  node->hook->x + w->arttree.node_width, y, x + 1, y);
    }

    for (;;) {
	if (child->hook->dashed) {
	    if (w->shadow.line_mode)
		gc1 = w->arttree.light_dashed_gc;
	    else
		gc1 = w->arttree.dark_dashed_gc;
	    gc2 = w->arttree.light_dashed_gc;
	} else {
	    if (w->shadow.line_mode)
		gc1 = w->shadow.light_gc;
	    else
		gc1 = w->shadow.dark_gc;
	    gc2 = w->shadow.light_gc;
	}
	XDrawLine(disp, win, gc1, x + 1, y - 1, child->hook->x, y - 1);
	XDrawLine(disp, win, gc2, x + 1, y, child->hook->x, y);

	child = child->sibling;
	if (!child)
	    break;
	y = child->hook->y + w->arttree.node_height/2;
    }

    XDrawLine(disp, win,
	      w->shadow.line_mode ? w->shadow.light_gc : w->shadow.dark_gc,
	      x - 1, y0 + 1, x - 1, y + 1);
    XDrawLine(disp, win, w->shadow.light_gc, x, y0 + 1, x, y + 1);

}

static void draw_branches_of_subtree(ArtTreeWidget w,
				     ART_TREE_NODE *node,
				     Region region)
{
    while (node) {
	if (node->child1) {
	    if (!region ||
		XRectInRegion(region, node->hook->x, node->hook->y,
			      node->child1->hook->x - node->hook->x,
			      node->hook->bb_height) != RectangleOut)
		draw_branches(w, node);

	    if (!region ||
		XRectInRegion(region, node->child1->hook->x,
			      node->child1->hook->y, node->hook->bb_width,
			      node->hook->bb_height) !=	RectangleOut)
		draw_branches_of_subtree(w, node->child1, region);
	}
	node = node->sibling;
    }
}

/***********************************************************************/

static void free_private_data(ART_TREE_NODE *node)
{
    while (node) {
	if (node->hook) {
	    XtFree((char *)node->hook);
	    node->hook = NULL;
	}
	if (node->child1)
	    free_private_data(node->child1);
	node = node->sibling;
    }
}

static void init_private_data(ArtTreeWidget w, ART_TREE_NODE *node)
{
    while (node) {
	node->hook = (ART_TREE_PRIVATE *)XtMalloc(sizeof(ART_TREE_PRIVATE));
	node->hook->pixmap   = None;
	node->hook->selected = False;
	node->hook->dashed   = False;
	node->hook->inner    = False;
	node->hook->outer    = False;
	if (node->label)
	    node->hook->label_len =
		MyXWidthToChars(w->arttree.font, node->label,
				strlen(node->label),
				w->arttree.node_width -
				2 * (w->arttree.internal_node_width +
				     w->shadow.shadow_width));
	else
	    node->hook->label_len = 0;

	if (node->child1)
	    init_private_data(w, node->child1);
	node = node->sibling;
    }
}

static void update_private_data(ArtTreeWidget w, ART_TREE_NODE *node)
{
    while (node) {
	if (node->label)
	    node->hook->label_len =
		MyXWidthToChars(w->arttree.font, node->label,
				strlen(node->label),
				w->arttree.node_width -
				2 * (w->arttree.internal_node_width +
				     w->shadow.shadow_width));
	else
	    node->hook->label_len = 0;
			    
	if (node->child1)
	    update_private_data(w, node->child1);
	node = node->sibling;
    }
}

/***********************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *num_args)
{
    ArtTreeWidget	new = (ArtTreeWidget)gnew;

    init_gcs(new);
    init_dashed_gcs(new);
    new->arttree.rubberbanding = False;
    new->arttree.active = True;

    init_node_sizes(new);

    if (new->arttree.root) {
	init_private_data(new, new->arttree.root);
	layout(new);
    }

    if (new->core.height == 0)
	new->core.height = 1;
    if (new->core.width == 0)
	new->core.width  = 1;
}

static void Destroy(Widget w)
{
    free_gcs((ArtTreeWidget)w);
    free_dashed_gcs((ArtTreeWidget)w);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (!XtIsRealized((Widget)w))
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    draw_subnodes(w, w->arttree.root, region);
    draw_branches_of_subtree(w, w->arttree.root, region);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);
}

static void SuspendHook(ScrollableWidget w)
{
    if (!w->scrollable.suspended)
	Redisplay((Widget)w, NULL, NULL);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    ArtTreeWidget	current = (ArtTreeWidget)gcurrent;
    ArtTreeWidget	new     = (ArtTreeWidget)gnew;
    Boolean		redisplay = False;
    Boolean		do_layout = False;

    if (new->arttree.root)
	new->arttree.vertical = current->arttree.vertical;
    else if (new->arttree.vertical != current->arttree.vertical)
	do_layout = True;

    if (new->arttree.rubber_pixel     != current->arttree.rubber_pixel     ||
	new->arttree.foreground_pixel != current->arttree.foreground_pixel ||
	new->arttree.font             != current->arttree.font             ||
	new->arttree.inner_pixel      != current->arttree.inner_pixel      ||
	new->arttree.outer_pixel      != current->arttree.outer_pixel      ||
	new->arttree.inner_dashed     != current->arttree.inner_dashed     ||
	new->arttree.outer_dashed     != current->arttree.outer_dashed) {
	free_gcs(new);
	init_gcs(new);
	redisplay = True;
    }

    if (new->arttree.font != current->arttree.font) {
	init_node_sizes(new);
	do_layout = True;
    }

    if (new->arttree.internal_node_width !=
	current->arttree.internal_node_width ||
	new->arttree.internal_node_height !=
	current->arttree.internal_node_height ||
	new->arttree.pixmap_spacing != current->arttree.pixmap_spacing) {
	init_node_sizes(new);
	if (new->arttree.root)
	    update_private_data(new, new->arttree.root);
	do_layout = True;

    }

    if (new->arttree.row_spacing != current->arttree.row_spacing ||
	new->arttree.internal_width != current->arttree.internal_width ||
	new->arttree.internal_height != current->arttree.internal_height ||
	new->shadow.shadow_width != current->shadow.shadow_width) {
	do_layout = True;
	redisplay = True;
    }

    if (new->arttree.root != current->arttree.root) {
	if (current->arttree.root)
	    free_private_data(current->arttree.root);
	if (new->arttree.root)
	    init_private_data(new, new->arttree.root);
	do_layout = True;
	if (!new->scrollable.suspended)
	    redisplay = True;
    }

    if (new->arttree.node_columns != current->arttree.node_columns ||
	new->arttree.node_rows != current->arttree.node_rows) {
	init_node_sizes(new);
	do_layout = True;
	redisplay = True;
    }

    if (do_layout)
	layout(new);

    return redisplay;
}

/******************************************************************/

void ArtTreeRedraw(Widget gw)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (w->arttree.rubberbanding) {
	w->arttree.rubberbanding = False;
	draw_rubberband(w);
    }
    XClearArea(XtDisplay(w), XtWindow(w), 0, 0,
			 w->core.width, w->core.height, True);
}

void ArtTreeSetTree(Widget gw, ART_TREE_NODE *root)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (w->arttree.rubberbanding) {
	draw_rubberband(w);
	w->arttree.rubberbanding = False;
    }

    if (w->arttree.root)
	free_private_data(w->arttree.root);
    w->arttree.root = root;
    if (w->arttree.root)
	init_private_data(w, w->arttree.root);
    layout(w);

    if (!w->scrollable.suspended && XtIsRealized((Widget)w))
	ArtTreeRedraw((Widget)w);
}

void ArtTreeNodeCenter(Widget gw, ART_TREE_NODE *node)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    Widget		pw = XtParent(w);
    XtWidgetGeometry	req;
    
    if (!node->hook)
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    req.request_mode = CWX | CWY;
    req.x =
	(int)(pw->core.width - w->arttree.node_width) / 2 - node->hook->x;
    req.y =
	(int)(pw->core.height - w->arttree.node_height) / 2 - node->hook->y;
    XtMakeGeometryRequest((Widget)w, &req, NULL);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);

    ScrollableHFromGeometry((ScrollableWidget)w);
    ScrollableVFromGeometry((ScrollableWidget)w);
    ScrollableFitHBar((ScrollableWidget)w);
    ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTreeNodeMakeVisible(Widget gw, ART_TREE_NODE *node)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    Widget		pw = XtParent(w);

    if (!node->hook)
	return;

    if (node->hook->x < -w->core.x || node->hook->y < -w->core.y ||
	node->hook->x >=
	(int)pw->core.width - w->core.x - w->arttree.node_width ||
	node->hook->y >=
	(int)pw->core.height - w->core.y - w->arttree.node_height)
	ArtTreeNodeCenter((Widget)w, node);
}

void ArtTreeNodeSetSelected(Widget gw, ART_TREE_NODE *node, int selected)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (!node->hook)
	return;

    node->hook->selected = selected;
    if (w->scrollable.suspended || !XtIsRealized((Widget)w))
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XClearArea(XtDisplay(w), XtWindow(w), node->hook->x, node->hook->y,
	       w->arttree.node_width, w->arttree.node_height, False);
    draw_node(w, node);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XFlush(XtDisplay(w));
}

void ArtTreeNodeSetDashed(Widget gw, ART_TREE_NODE *node, int dashed)
{
    ART_TREE_NODE	*parent = node->parent;
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (!parent || !node->hook)
	return;

    node->hook->dashed = dashed;
    if (w->scrollable.suspended || !XtIsRealized((Widget)w))
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    if (dashed)
	if (w->arttree.vertical)
	    /* FIXME... (knews doesn't need it) */;
	else
	    XClearArea(XtDisplay(w), XtWindow(w),
		       parent->hook->x + w->arttree.node_width,
		       parent->hook->y, w->arttree.column_spacing,
		       parent->hook->bb_height, False);
    draw_branches(w, parent);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XFlush(XtDisplay(w));
}

void ArtTreeNodeSetInner(Widget gw, ART_TREE_NODE *node, int inner)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (!node->hook)
	return;

    node->hook->inner = inner;
    if (w->scrollable.suspended || !XtIsRealized((Widget)w))
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    draw_inner_border(w, node);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XFlush(XtDisplay(w));
}

void ArtTreeNodeSetOuter(Widget gw, ART_TREE_NODE *node, int outer)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    if (!node->hook)
	return;

    node->hook->outer = outer;
    if (w->scrollable.suspended || !XtIsRealized((Widget)w))
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    draw_outer_border(w, node);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XFlush(XtDisplay(w));
}

void ArtTreeNodeSetPixmap(Widget gw, ART_TREE_NODE *node, Pixmap pixmap)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			width;

    if (!node->hook)
	return;

    node->hook->pixmap = pixmap;

    width = w->arttree.node_width -
	2 * (w->arttree.internal_node_width + w->shadow.shadow_width);
    if (pixmap != None)
	width -= w->arttree.pixmap_width + w->arttree.pixmap_spacing;
    if (node->label)
	node->hook->label_len =
	    MyXWidthToChars(w->arttree.font, node->label,
			    strlen(node->label), width);
    else
	node->hook->label_len = 0;

    if (w->scrollable.suspended || !XtIsRealized((Widget)w))
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XClearArea(XtDisplay(w), XtWindow(w), node->hook->x, node->hook->y,
	       w->arttree.node_width, w->arttree.node_height, False);
    draw_node(w, node);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XFlush(XtDisplay(w));
}

int ArtTreeNodeGetSelected(Widget gw, ART_TREE_NODE *node)
{
    return node->hook ? node->hook->selected : False;
}

int ArtTreeNodeGetDashed(Widget gw, ART_TREE_NODE *node)
{
    return node->hook ? node->hook->dashed : False;
}

int ArtTreeNodeGetInner(Widget gw, ART_TREE_NODE *node)
{
    return node->hook ? node->hook->inner : False;
}

int ArtTreeNodeGetOuter(Widget gw, ART_TREE_NODE *node)
{
    return node->hook ? node->hook->outer : False;
}

Pixmap ArtTreeNodeGetPixmap(Widget gw, ART_TREE_NODE *node)
{
    return node->hook ? node->hook->pixmap : None;
}

void ArtTreeNodeNotifyLabel(Widget gw, ART_TREE_NODE *node)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;
    int			width;

    if (!node->hook)
	return;

    width = w->arttree.node_width -
	2 * (w->arttree.internal_node_width + w->shadow.shadow_width);
    if (node->hook->pixmap != None)
	width -= w->arttree.pixmap_width + w->arttree.pixmap_spacing;
    if (node->label)
	node->hook->label_len =
	    MyXWidthToChars(w->arttree.font, node->label,
			    strlen(node->label), width);
    else
	node->hook->label_len = 0;

    if (w->scrollable.suspended || !XtIsRealized((Widget)w))
	return;

    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XClearArea(XtDisplay(w), XtWindow(w), node->hook->x, node->hook->y,
	       w->arttree.node_width, w->arttree.node_height, False);
    draw_node(w, node);
    if (w->arttree.rubberbanding)
	draw_rubberband(w);
    XFlush(XtDisplay(w));
}

void ArtTreeSetActive(Widget gw, int active)
{
    ArtTreeWidget	w = (ArtTreeWidget)gw;

    w->arttree.active = active;
}
