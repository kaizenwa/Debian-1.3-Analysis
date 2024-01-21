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
#include "ShadowP.h"
#include "Util.h"

static XtResource resources[] = {
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(ShadowRec, core.border_width), XtRImmediate, (XtPointer)0},
#define offset(field) XtOffsetOf(ShadowRec, shadow.field)
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     offset(shadow_width), XtRImmediate, (XtPointer)2},
    {XtNallocArmColor, XtCAllocArmColor, XtRBoolean, sizeof(Boolean),
     offset(alloc_arm_color), XtRImmediate, (XtPointer)True},
    {XtNallocArmPixmap, XtCAllocArmPixmap, XtRBoolean, sizeof(Boolean),
     offset(alloc_arm_pixmap), XtRImmediate, (XtPointer)True},
    {XtNallocShadowColors, XtCAllocShadowColors, XtRBoolean, sizeof(Boolean),
     offset(alloc_shadow_colors), XtRImmediate, (XtPointer)True},
    {XtNuseLineShadows, XtCUseLineShadows, XtRBoolean, sizeof(Boolean),
     offset(use_lines), XtRImmediate, (XtPointer)False},
#undef offset
};

static void	ClassPartInitialize(WidgetClass);
static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static void	Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static Boolean	AllocShadowColors(ShadowWidget, XColor*);
static void	AllocShadowPixmaps(ShadowWidget, Pixel);
static Boolean	AllocArmColor(ShadowWidget, XColor*);
static void	AllocArmPixmap(ShadowWidget, Pixel);
static void	AllocGCs(ShadowWidget);

ShadowClassRec shadowClassRec = {
    { /* core fields */
        (WidgetClass) &widgetClassRec,  /* superclass                   */
        "Shadow",                       /* class_name                   */
        sizeof(ShadowRec),	        /* widget_size                  */
        NULL,				/* class_initialize             */
        ClassPartInitialize,            /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        Realize,			/* realize                      */
        NULL,                           /* actions                      */
        0,                              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
        TRUE,                           /* compress_exposure            */
        TRUE,                           /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        Destroy,                        /* destroy                      */
        NULL,                           /* resize                       */
        NULL,                           /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,                           /* accept_focus                 */
        XtVersion,                      /* version                      */
        NULL,                           /* callback_private             */
        NULL,                           /* tm_table                     */
        NULL,                           /* query_geometry               */
        XtInheritDisplayAccelerator,    /* display_accelerator          */
        NULL                            /* extension                    */
    },
    { /* shadow fields */
	XtOffsetOf(CoreRec, core.background_pixel), /* pixel_offset	*/
	False,				/* use_arm_for_background	*/
	AllocShadowColors,		/* alloc_shadow_colors		*/
	AllocShadowPixmaps,		/* alloc_shadow_pixmaps		*/
	AllocArmColor,			/* alloc_arm_color		*/
	AllocArmPixmap,			/* alloc_arm_pixmap		*/
	AllocGCs,			/* alloc_gcs			*/
	NULL,				/* extension			*/
    }
};

WidgetClass shadowWidgetClass = (WidgetClass)&shadowClassRec;

/*************************************************************************/

static Pixel get_pixel(ShadowWidget w)
{
    ShadowWidgetClass	class = (ShadowWidgetClass)XtClass(w);
    Cardinal		offset = class->shadow_class.pixel_offset;

    return *(Pixel *)((char *)w + offset);
}

typedef struct pixmap_cache_ref {
    struct pixmap_cache_ref	*next;
    struct pixmap_cache_ref	*prev;
    Screen	*screen;
    Pixmap	pixmap;
    Pixel	fg;
    Pixel	bg;
    int		depth;
    int		size;
    int		ref_count;
} PixmapCacheRef;

static PixmapCacheRef *pixmap_cache;

static Pixmap CacheCreatePixmap(ShadowWidget w, int size, Pixel fg, Pixel bg)
{
    static char		bitmap2by2_data[] = {0x01, 0x02};
    static char		bitmap4by4_data[] = {0x08, 0x02, 0x04, 0x01};
    Display		*disp = XtDisplay(w);
    Screen		*screen = XtScreen(w);
    PixmapCacheRef	*loop;
    char		*bitmap_data;

    for (loop = pixmap_cache ; loop ; loop = loop->next)
	if (loop->screen == screen && loop->fg == fg && loop->bg == bg &&
	    loop->depth == w->core.depth && loop->size == size) {
	    loop->ref_count++;
	    return loop->pixmap;
	}

    switch (size) {
    case 2:
	bitmap_data = bitmap2by2_data;
	break;
    case 4:
	bitmap_data = bitmap4by4_data;
	break;
    default:
	return None;
    }

    loop = (PixmapCacheRef *)XtMalloc(sizeof(PixmapCacheRef));
    loop->next = pixmap_cache;
    loop->prev = NULL;
    loop->screen = screen;
    loop->depth = w->core.depth;
    loop->size = size;
    loop->fg = fg;
    loop->bg = bg;
    loop->ref_count = 1;
	
    loop->pixmap =
	XCreatePixmapFromBitmapData(disp, RootWindowOfScreen(screen),
				    bitmap_data, size, size, fg, bg,
				    w->core.depth);
    if (pixmap_cache)
	pixmap_cache->prev = loop;
    pixmap_cache = loop;
    
    return loop->pixmap;
}

static void CacheFreePixmap(ShadowWidget w, Pixmap pixmap)
{
    Display		*disp = XtDisplay(w);
    Screen		*screen = XtScreen(w);
    PixmapCacheRef	*loop;

    for (loop = pixmap_cache ; loop ; loop = loop->next)
	if (loop->screen == screen && loop->pixmap == pixmap) {
	    PixmapCacheRef	*prev = loop->prev;

	    loop->ref_count--;
	    if (loop->ref_count != 0)
		break;

	    XFreePixmap(disp, loop->pixmap);

	    if (prev) {
		prev->next = loop->next;
		if (loop->next)
		    loop->next->prev = prev;
	    } else {
		pixmap_cache = loop->next;
		if (pixmap_cache)
		    pixmap_cache->prev = NULL;
	    }
	    XtFree((char *)loop);
	    break;
	}
}

/*************************************************************************/

#define MIN(a, b) ((a) < (b) ? (a) : (b))
static Boolean AllocShadowColors(ShadowWidget w, XColor *col)
{
    Display	*disp = XtDisplay(w);
    Colormap	cmap = w->core.colormap;
    XColor	sh_col;

    if ((col->red > 0xefff && col->green > 0xefff && col->blue > 0xefff) ||
	(col->red < 0x1000 && col->green < 0x1000 && col->blue < 0x1000)) {
	sh_col.red   = 65535L * 4 / 5;
        sh_col.green = 65535L * 4 / 5;
        sh_col.blue  = 65535L * 4 / 5;
        if (!XAllocColor(disp, cmap, &sh_col))
	    return False;
	w->shadow.light_pixel = sh_col.pixel;

        sh_col.red   = 65535L * 3 /5;
        sh_col.green = 65535L * 3 /5;
        sh_col.blue  = 65535L * 3 /5;
        if (!XAllocColor(disp, cmap, &sh_col)) {
	    unsigned long	tmp = w->shadow.light_pixel;

	    XFreeColors(disp, cmap, &tmp, 1, 0);
	    return False;
	}
        w->shadow.dark_pixel = sh_col.pixel;
    } else {
        sh_col.red   = MIN (65535, 6 * (long)col->red   / 5);
        sh_col.green = MIN (65535, 6 * (long)col->green / 5);
        sh_col.blue  = MIN (65535, 6 * (long)col->blue  / 5);
        if (!XAllocColor(disp, cmap, &sh_col))
	    return False;
        w->shadow.light_pixel = sh_col.pixel;

        sh_col.red   = MIN (65535, 3 * (long)col->red   / 5);
        sh_col.green = MIN (65535, 3 * (long)col->green / 5);
        sh_col.blue  = MIN (65535, 3 * (long)col->blue  / 5);
        if (!XAllocColor(disp, cmap, &sh_col)) {
	    unsigned long	tmp = w->shadow.light_pixel;

	    XFreeColors(disp, cmap, &tmp, 1, 0);
	    return False;
	}
        w->shadow.dark_pixel = sh_col.pixel;
    }

    return True;
}
#undef MIN

static void AllocShadowPixmaps(ShadowWidget w, Pixel pix)
{
    if (w->core.depth == 1)
	w->shadow.light_pixmap = CacheCreatePixmap(w, 2, 0, 1);
    else {
	Screen	*screen = XtScreen(w);
	Visual	*visual = get_visual((Widget)w);
	Pixel	black, white;

	black_and_white(screen, visual, &black, &white);
	w->shadow.light_pixmap = CacheCreatePixmap(w, 2, pix, white);
	w->shadow.dark_pixmap  = CacheCreatePixmap(w, 2, pix, black);
    }
}

static Boolean AllocArmColor(ShadowWidget w, XColor *col)
{
    Display	*disp = XtDisplay(w);
    Colormap	cmap = w->core.colormap;
    XColor	arm_col;
    long	t, r, g, b;

    t = col->red + col->green + col->blue;
    t /= 3;
    r = 3 * t / 4 + 3 * ((long)col->red   - t) / 2;
    g = 3 * t / 4 + 3 * ((long)col->green - t) / 2;
    b = 3 * t / 4 + 3 * ((long)col->blue  - t) / 2;
    if (r < 0)
	r = 0;
    else if (r > 65535)
	r = 65535;
    if (g < 0)
	g = 0;
    else if (g > 65535)
	g = 65535;
    if (b < 0)
	b = 0;
    else if (b > 65535)
	b = 65535;

    arm_col.red   = r;
    arm_col.green = g;
    arm_col.blue  = b;
    if (!XAllocColor(disp, cmap, &arm_col))
	return False;
    w->shadow.arm_pixel = arm_col.pixel;

    return True;
}

static void AllocArmPixmap(ShadowWidget w, Pixel pix)
{
    if (w->core.depth == 1)
	w->shadow.arm_pixmap = CacheCreatePixmap(w, 4, 0, 1);
    else
	w->shadow.arm_pixmap =
	    CacheCreatePixmap(w, 4, get_black((Widget)w), pix);
}

static void AllocGCs(ShadowWidget w)
{
    ShadowWidgetClass	class = (ShadowWidgetClass)XtClass(w);
    XGCValues		values;

    if (w->shadow.alloced_shadow_pixels) {
	values.line_width = 1;
	values.foreground = w->shadow.light_pixel;
	w->shadow.light_gc =
	    XtGetGC((Widget)w, GCForeground | GCLineWidth, &values);
	values.foreground = w->shadow.dark_pixel;
	w->shadow.dark_gc =
	    XtGetGC((Widget)w, GCForeground | GCLineWidth, &values);
    } else if (w->shadow.line_mode) {
	Pixel	pix = get_pixel(w);

	/* depth == 1... */
	values.foreground = (pix == 1) ? 0 : 1;
	values.line_width = 1;
	w->shadow.light_gc =
	    XtGetGC((Widget)w, GCForeground | GCLineWidth, &values);
	values.line_width = w->shadow.shadow_width;
	w->shadow.dark_gc =
	    XtGetGC((Widget)w, GCForeground | GCLineWidth, &values);
    } else {
	Screen	*scr = XtScreen(w);
	Visual	*vis = get_visual((Widget)w);
	Pixel	black, white;

	black_and_white(scr, vis, &black, &white);
	values.line_width = 1;
	if (w->shadow.light_pixmap == None) {
	    values.foreground =
		(w->core.background_pixel == black) ? white : black;
	    w->shadow.light_gc =
		XtGetGC((Widget)w, GCLineWidth | GCForeground, &values);
	} else {
	    values.fill_style = FillTiled;
	    values.tile = w->shadow.light_pixmap;
	    w->shadow.light_gc =
		XtGetGC((Widget)w,
			GCLineWidth | GCFillStyle | GCTile,
			&values);
	}

	if (w->shadow.dark_pixmap == None) {
	    values.foreground =
		(w->core.background_pixel == black) ? white : black;
	    w->shadow.dark_gc =
		XtGetGC((Widget)w, GCLineWidth | GCForeground, &values);
	} else {
	    values.fill_style = FillTiled;
	    values.tile = w->shadow.dark_pixmap;
	    w->shadow.dark_gc =
		XtGetGC((Widget)w,
			GCLineWidth | GCFillStyle | GCTile,
			&values);
	}
    }

    if (class->shadow_class.use_arm_for_background)
	return;

    if (w->shadow.alloced_arm_pixel) {
	values.line_width = 1;
	values.foreground = w->shadow.arm_pixel;
	w->shadow.arm_gc =
	    XtGetGC((Widget)w, GCForeground | GCLineWidth, &values);
    } else if (w->shadow.arm_pixmap != None) {
	values.line_width = 1;
	values.tile = w->shadow.arm_pixmap;
	values.fill_style = FillTiled;
	w->shadow.arm_gc =
	    XtGetGC((Widget)w, GCFillStyle | GCTile| GCLineWidth, &values);
    }
}

/*************************************************************************/

static void ClassPartInitialize(WidgetClass gclass)
{
    ShadowClassRec	*class, *super;

    class = (ShadowClassRec *)gclass;
    super = (ShadowClassRec *)class->core_class.superclass;

    if (class->shadow_class.pixel_offset == XtInheritPixelOffset)
	class->shadow_class.pixel_offset = super->shadow_class.pixel_offset;
    if (class->shadow_class.alloc_shadow_colors == XtInheritAllocShadowColors)
	class->shadow_class.alloc_shadow_colors =
	    super->shadow_class.alloc_shadow_colors;
    if (class->shadow_class.alloc_shadow_pixmaps ==
	XtInheritAllocShadowPixmaps)
	class->shadow_class.alloc_shadow_pixmaps =
	    super->shadow_class.alloc_shadow_pixmaps;
    if (class->shadow_class.alloc_arm_color == XtInheritAllocArmColor)
	class->shadow_class.alloc_arm_color =
	    super->shadow_class.alloc_arm_color;
    if (class->shadow_class.alloc_arm_pixmap == XtInheritAllocArmPixmap)
	class->shadow_class.alloc_arm_pixmap =
	    super->shadow_class.alloc_arm_pixmap;
    if (class->shadow_class.alloc_gcs == XtInheritAllocGCs)
	class->shadow_class.alloc_gcs = super->shadow_class.alloc_gcs;
}

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    ShadowWidget	new = (ShadowWidget)gnew;
    ShadowWidgetClass	class = (ShadowWidgetClass)XtClass(new);
    Display		*disp = XtDisplay(new);
    Colormap		cmap = new->core.colormap;
    XColor		col;
    Boolean		query_done = False;

    new->shadow.light_pixmap = None;
    new->shadow.dark_pixmap  = None;
    new->shadow.arm_pixmap   = None;
    new->shadow.light_gc = (GC)0;
    new->shadow.dark_gc  = (GC)0;
    new->shadow.arm_gc   = (GC)0;
    new->shadow.line_mode = False;
    new->shadow.alloced_shadow_pixels = False;
    new->shadow.alloced_arm_pixel = False;
    col.pixel = get_pixel(new);

    if (new->shadow.alloc_shadow_colors && new->core.depth != 1 &&
	class->shadow_class.alloc_shadow_colors) {
	XQueryColor(disp, cmap, &col);
	query_done = True;
	if (class->shadow_class.alloc_shadow_colors(new, &col))
	    new->shadow.alloced_shadow_pixels = True;
    }

    if (!new->shadow.alloced_shadow_pixels)
	if (new->shadow.use_lines && new->core.depth == 1)
	    new->shadow.line_mode = True;
	else if (class->shadow_class.alloc_shadow_pixmaps)
	    class->shadow_class.alloc_shadow_pixmaps(new, col.pixel);

    if (new->shadow.alloc_arm_color && class->shadow_class.alloc_arm_color &&
	new->core.depth != 1) {
	if (!query_done)
	    XQueryColor(disp, cmap, &col);
	if (class->shadow_class.alloc_arm_color(new, &col))
	    new->shadow.alloced_arm_pixel = True;
    }

    if (!new->shadow.alloced_arm_pixel && new->shadow.alloc_arm_pixmap &&
	class->shadow_class.alloc_arm_pixmap)
	class->shadow_class.alloc_arm_pixmap(new, col.pixel);

    if (class->shadow_class.alloc_gcs)
	class->shadow_class.alloc_gcs(new);
}

static void Realize(Widget gw, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    ShadowWidget	w = (ShadowWidget)gw;
    ShadowWidgetClass	class = (ShadowWidgetClass)XtClass(w);

    if (class->shadow_class.use_arm_for_background)
	if (w->shadow.arm_pixmap != None) {
	    *mask |= CWBackPixmap;
	    *mask &= ~CWBackPixel;
	    attributes->background_pixmap = w->shadow.arm_pixmap;
	} else if (w->shadow.alloced_arm_pixel) {
	    *mask |= CWBackPixel;
	    attributes->background_pixel = w->shadow.arm_pixel;
	}

    coreWidgetClass->core_class.realize((Widget)w, mask, attributes);
}

static void Destroy(Widget gw)
{
    ShadowWidget	w = (ShadowWidget)gw;
    Display		*disp = XtDisplay(w);
    unsigned long	pixels[3];
    int			n = 0;

    if (w->shadow.light_gc != 0)
	XtReleaseGC((Widget)w, w->shadow.light_gc);
    if (w->shadow.dark_gc != 0)
	XtReleaseGC((Widget)w, w->shadow.dark_gc);
    if (w->shadow.arm_gc != 0)
	XtReleaseGC((Widget)w, w->shadow.arm_gc);
    if (w->shadow.light_pixmap != None)
	CacheFreePixmap(w, w->shadow.light_pixmap);
    if (w->shadow.dark_pixmap != None)
	CacheFreePixmap(w, w->shadow.dark_pixmap);
    if (w->shadow.arm_pixmap != None)
	CacheFreePixmap(w, w->shadow.arm_pixmap);
    if (w->shadow.alloced_shadow_pixels) {
	pixels[n] = w->shadow.light_pixel;
	n++;
	pixels[n] = w->shadow.dark_pixel;
	n++;
    }
    if (w->shadow.alloced_arm_pixel) {
	pixels[n] = w->shadow.arm_pixel;
	n++;
    }
    if (n != 0) {
	Colormap	cmap = w->core.colormap;

	XFreeColors(disp, cmap, pixels, n, 0);
    }
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    Boolean		redisplay = False;
    ShadowWidget	new = (ShadowWidget)gnew;
    ShadowWidget	current = (ShadowWidget)gcurrent;
    ShadowWidgetClass	class = (ShadowWidgetClass)XtClass(new);
    Pixel		new_pix = get_pixel(new);
    Pixel		current_pix = get_pixel(current);

    if (new_pix != current_pix ||
	(new->shadow.line_mode &&
	 new->shadow.shadow_width != current->shadow.shadow_width) ||
	new->shadow.alloc_shadow_colors !=
	current->shadow.alloc_shadow_colors ||
	new->shadow.alloc_arm_color != current->shadow.alloc_arm_color ||
	new->shadow.use_lines != current->shadow.use_lines ||
	new->shadow.alloc_arm_pixmap != current->shadow.alloc_arm_pixmap) {
	Pixel	pixels[3];
	int	n = 0;

	/* initialize first, then free, except for pixels... */
	if (current->shadow.alloced_shadow_pixels) {
	    pixels[n++] = current->shadow.light_pixel;
	    pixels[n++] = current->shadow.dark_pixel;
	}
	if (current->shadow.alloced_arm_pixel)
	    pixels[n++] = current->shadow.arm_pixel;

	if (n > 0)
	    XFreeColors(XtDisplay(current),
			current->core.colormap, pixels, n, 0);
	
	Initialize(NULL, (Widget)new, NULL, NULL);

	if (current->shadow.light_gc != 0)
	    XtReleaseGC((Widget)new, current->shadow.light_gc);
	if (current->shadow.dark_gc != 0)
	    XtReleaseGC((Widget)new, current->shadow.dark_gc);
	if (current->shadow.arm_gc != 0)
	    XtReleaseGC((Widget)new, current->shadow.arm_gc);
	if (current->shadow.light_pixmap != None)
	    CacheFreePixmap(new, current->shadow.light_pixmap);
	if (current->shadow.dark_pixmap != None)
	    CacheFreePixmap(new, current->shadow.dark_pixmap);
	if (current->shadow.arm_pixmap != None)
	    CacheFreePixmap(new, current->shadow.arm_pixmap);

	redisplay = True;
    }

    if (class->shadow_class.use_arm_for_background &&
	(redisplay ||
	 new->core.background_pixel != current->core.background_pixel)) {
	Display			*disp = XtDisplay(new);
	Window			win = XtWindow(new);

	if (new->shadow.alloced_arm_pixel)
	    XSetWindowBackground(disp, win, new->shadow.arm_pixel);
	else if (new->shadow.arm_pixmap != None)
	    XSetWindowBackgroundPixmap(disp, win, new->shadow.arm_pixmap);
	else
	    XSetWindowBackground(disp, win, new->core.background_pixel);
    }

    return redisplay;
}

/*************************************************************************/

void ShadowDrawShadows(ShadowWidget w, Position x, Position y,
		       Dimension width, Dimension height, Boolean swap)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    short	sw = w->shadow.shadow_width;

    if (sw == 0 || !XtIsRealized((Widget)w)) return;

    if (w->shadow.line_mode) {
	if (swap)
	    XDrawRectangle(disp, win, w->shadow.dark_gc, x + sw/2, y + sw/2,
			   width - sw, height - sw);	    
	else
	    XDrawRectangle(disp, win, w->shadow.light_gc, x, y,
			   width - 1, height - 1);
    } else {
	GC	light_gc, dark_gc;
	XPoint	points[6];

	if (swap) {
	    light_gc = w->shadow.dark_gc;
	    dark_gc  = w->shadow.light_gc;
	} else {
	    light_gc = w->shadow.light_gc;
	    dark_gc  = w->shadow.dark_gc;
	}
	
	points[0].x = x;
	points[0].y = y;
	points[1].x = x + width;
	points[1].y = y;
	points[2].x = x + width - sw;
	points[2].y = y + sw;
	points[3].x = x + sw;
	points[3].y = y + sw;
	points[4].x = x + sw;
	points[4].y = y + height - sw;
	points[5].x = x;
	points[5].y = y + height;
	XFillPolygon(disp, win, light_gc, points, 6,
		     (sw > 1) ? Nonconvex : Complex, CoordModeOrigin);
	
	points[0].x = x + width;
	points[0].y = y + height;
	points[3].x = x + width - sw;
	points[3].y = y + height - sw;
	XFillPolygon(disp, win, dark_gc, points, 6,
		     (sw > 1) ? Nonconvex : Complex, CoordModeOrigin);
    }
}

/* the widget must be able to handle NULL events and regions */

void ShadowRedrawWidget(Widget w)
{
    if (XtIsRealized(w)) {
	XtExposeProc	expose_proc = XtClass(w)->core_class.expose;
	
	if (expose_proc)
	    expose_proc(w, NULL, NULL);
    }
}
