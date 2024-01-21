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
#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include "Util.h"

#include "Compat.h"
#include "ArtTextP.h"

#define MAX_Y(w)	((w)->arttext.table[(w)->arttext.lines].y)

static XtResource resources[] = {
    {XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(ArtTextRec, shadow.shadow_width), XtRImmediate, (XtPointer)1},
#define offset(field) XtOffsetOf(ArtTextRec, arttext.field)
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct*),
     offset(font), XtRString, XtDefaultFont},
    {XtNhighlightColor, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(highlight_pixel), XtRString, XtDefaultForeground},
    {XtNurlCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(url_callback), XtRImmediate, (XtPointer)NULL},
    {XtNmargin, XtCMargin, XtRDimension, sizeof(Dimension),
     offset(margin), XtRImmediate, (XtPointer)8},
    {XtNimageMargin, XtCMargin, XtRDimension, sizeof(Dimension),
     offset(image_margin), XtRImmediate, (XtPointer)16},
    {XtNseparatorMargin, XtCMargin, XtRDimension, sizeof(Dimension),
     offset(separator_margin), XtRImmediate, (XtPointer)12},
    {XtNpreferredLines, XtCPreferredLines, XtRDimension, sizeof(Dimension),
     offset(preferred_lines), XtRImmediate, (XtPointer)32},
    {XtNpreferredColumns, XtCPreferredColumns, XtRDimension, sizeof(Dimension),
     offset(preferred_columns), XtRImmediate, (XtPointer)80},
    {XtNwrapLines, XtCWrapLines, XtRBoolean, sizeof(Boolean),
     offset(wrap_lines), XtRImmediate, (XtPointer)True},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static void	Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static void	Resize(Widget);
static void	Redisplay(Widget, XEvent*, Region);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static void	SetVPos(ScrollableWidget, long);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);

static void	select_start(Widget, XEvent*, String*, Cardinal*);
static void	select_extend(Widget, XEvent*, String*, Cardinal*);
static void	select_extend_start(Widget, XEvent*, String*, Cardinal*);
static void	select_end(Widget, XEvent*, String*, Cardinal*);
static void	click(Widget, XEvent*, String*, Cardinal*);
static void	call_url(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"select-start",		select_start},
    {"select-extend",		select_extend},
    {"select-extend-start",	select_extend_start},
    {"select-end",		select_end},
    {"click",			click},
    {"call-url",		call_url},
};

static char translations[] =
"<Btn1Down>:	select-start() click() \n"
"<Btn1Motion>:	select-extend() \n"
"<Btn1Up>:	select-end(PRIMARY) \n"
"<Btn2Down>:	call-url() click() \n"
"<Btn3Down>:	select-extend-start() click() \n"
"<Btn3Motion>:	select-extend() \n"
"<Btn3Up>:	select-end(PRIMARY) \n";

ArtTextClassRec artTextClassRec = {
    { /* core fields */
        (WidgetClass) &scrollableClassRec,	/* superclass		*/
        "ArtText",                      /* class_name                   */
        sizeof(ArtTextRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        Realize,        	        /* realize                      */
        actions,                        /* actions                      */
        XtNumber(actions),              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
#if (XtSpecificationRelease < 4)
        TRUE,                           /* compress_exposure            */
#else
	XtExposeCompressMaximal | XtExposeGraphicsExposeMerged,
					/* compress_exposure		*/
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
    { /* shadow fields */
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
	SetVPos,			/* set_vpos			*/
	NULL,				/* suspend_hook			*/
	NULL,				/* extension			*/
    },
    { /* arttext fields */
        0                               /* extension                    */
    }
};

WidgetClass artTextWidgetClass = (WidgetClass)&artTextClassRec;

/*************************************************************************/

static Dimension preferred_width(ArtTextWidget w)
{
    return w->arttext.max_width > 0 ? w->arttext.max_width :
	(w->arttext.font->max_bounds.width * w->arttext.preferred_columns +
	 2 * w->arttext.margin);
}

static Dimension preferred_height(ArtTextWidget w)
{
    return w->arttext.preferred_lines *
	(w->arttext.font->ascent + w->arttext.font->descent);
}

static long coord_to_line(ArtTextWidget w, long y)
{
    long	n = w->arttext.first;

    if (n >= w->arttext.lines)
	n = w->arttext.lines - 1;
    if (n < 0)
	n = 0;

    while (w->arttext.table[n].y <= y)
	if (++n > w->arttext.lines)
	    return w->arttext.lines;

    if (n > 0)
	do {
	    n--;
	} while (n > 0 && w->arttext.table[n].y > y);

    return n;
}

/*************************************************************************/

static int draw_item(ArtTextWidget w, long line)
{
    TSTable	*table = w->arttext.table + line;
    TSNode	*node = table->node;
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    GC		gc = w->arttext.gc;
    XFontStruct	*font;
    char	*str;
    XChar2b	*wstr;
    long	len;
    int		x, y = table->y - w->scrollable.pos_y;
    int		width;

#define SET_GC_FONT                        \
    if (font->fid != w->arttext.gc_fid) {  \
	XSetFont(disp, gc, font->fid);     \
	w->arttext.gc_fid = font->fid;     \
    }
#define SET_GC_FG(pixel)                   \
    if (pixel != w->arttext.gc_fg) {       \
	XSetForeground(disp, gc, pixel);   \
	w->arttext.gc_fg = pixel;          \
    }

    switch (node->gen.type) {
    case LineTypeString:
	if (y >= (int)w->core.height)
	    return False;

	font = node->str.font;
	x = w->arttext.margin;
	y += font->ascent;
	SET_GC_FONT;
	str = node->str.str + table->start;
	len = table->len;

	if (!w->arttext.sel_ok ||
	    line < w->arttext.sel_start_line ||
	    line > w->arttext.sel_stop_line) {
	    SET_GC_FG(node->str.pixel);
	    XDrawString(disp, win, gc, x, y, str, len);
	} else if (line > w->arttext.sel_start_line &&
		   line < w->arttext.sel_stop_line) {
	    SET_GC_FG(w->core.background_pixel);
	    XDrawImageString(disp, win, gc, x, y, str, len);
	    x += XTextWidth(font, str, len);
	    if (x < (int)w->core.width) {
		SET_GC_FG(w->arttext.highlight_pixel);
		XFillRectangle(disp, win, gc, x,
			       y - font->ascent, w->core.width - x,
			       font->ascent + font->descent);
	    }
	} else {
	    long	tmp = 0;

	    if (line == w->arttext.sel_start_line &&
		w->arttext.sel_start_offset > 0) {
		tmp = w->arttext.sel_start_offset;
		if (tmp > len)
		    tmp = len;
		SET_GC_FG(node->str.pixel);
		XDrawString(disp, win, gc, x, y, str, tmp);
		x += XTextWidth(font, str, tmp);
		str += tmp;
		len -= tmp;
	    }

	    if (line != w->arttext.sel_stop_line) {
		SET_GC_FG(w->core.background_pixel);
		XDrawImageString(disp, win, gc, x, y, str, len);
		x += XTextWidth(font, str, len);
		if (x < (int)w->core.width) {
		    SET_GC_FG(w->arttext.highlight_pixel);
		    XFillRectangle(disp, win, gc, x, y - font->ascent,
				   w->core.width - x,
				   font->ascent + font->descent);
		}
	    } else {
		long	tmp1;

		tmp = w->arttext.sel_stop_offset - tmp + 1;
		tmp1 = tmp < len ? tmp : len;
		if (tmp1 > 0) {
		    SET_GC_FG(w->core.background_pixel);
		    XDrawImageString(disp, win, gc, x, y, str, tmp1);
		    x += XTextWidth(font, str, tmp1);
		}
		str += tmp;
		len -= tmp;

		if (len >= 0) {
		    if (len > 0) {
			SET_GC_FG(node->str.pixel);
			XDrawString(disp, win, gc, x, y, str, len);
		    }
		} else if (x < (int)w->core.width) {
		    SET_GC_FG(w->arttext.highlight_pixel);
		    XFillRectangle(disp, win, gc, x, y - font->ascent,
				   w->core.width - x,
				   font->ascent + font->descent);
		}
	    }
	}
	break;
    case LineTypeWString:
	if (y >= (int)w->core.height)
	    return False;

	font = node->wstr.font;
	x = w->arttext.margin;
	y += font->ascent;
	SET_GC_FONT;
       	wstr = node->wstr.str + table->start;
	len = table->len;

	if (!w->arttext.sel_ok ||
	    line < w->arttext.sel_start_line ||
	    line > w->arttext.sel_stop_line) {
	    SET_GC_FG(node->wstr.pixel);
	    XDrawString16(disp, win, gc, x, y, wstr, len);
	} else if (line > w->arttext.sel_start_line &&
		   line < w->arttext.sel_stop_line) {
	    SET_GC_FG(w->core.background_pixel);
	    XDrawImageString16(disp, win, gc, x, y, wstr, len);
	    x += XTextWidth16(font, wstr, len);
	    if (x < (int)w->core.width) {
		SET_GC_FG(w->arttext.highlight_pixel);
		XFillRectangle(disp, win, gc, x,
			       y - font->ascent, w->core.width - x,
			       font->ascent + font->descent);
	    }
	} else {
	    long	tmp = 0;

	    if (line == w->arttext.sel_start_line &&
		w->arttext.sel_start_offset > 0) {
		tmp = w->arttext.sel_start_offset;
		if (tmp > len)
		    tmp = len;
		SET_GC_FG(node->wstr.pixel);
		XDrawString16(disp, win, gc, x, y, wstr, tmp);
		x += XTextWidth16(font, wstr, tmp);
		wstr += tmp;
		len -= tmp;
	    }

	    if (line != w->arttext.sel_stop_line) {
		SET_GC_FG(w->core.background_pixel);
		XDrawImageString16(disp, win, gc, x, y, wstr, len);
		x += XTextWidth16(font, wstr, len);
		if (x < (int)w->core.width) {
		    SET_GC_FG(w->arttext.highlight_pixel);
		    XFillRectangle(disp, win, gc, x, y - font->ascent,
				   w->core.width - x,
				   font->ascent + font->descent);
		}
	    } else {
		long	tmp1;

		tmp = w->arttext.sel_stop_offset - tmp + 1;
		tmp1 = tmp < len ? tmp : len;
		if (tmp1 > 0) {
		    SET_GC_FG(w->core.background_pixel);
		    XDrawImageString16(disp, win, gc, x, y, wstr, tmp1);
		    x += XTextWidth16(font, wstr, tmp1);
		}
		wstr += tmp;
		len -= tmp;

		if (len >= 0) {
		    if (len > 0) {
			SET_GC_FG(node->wstr.pixel);
			XDrawString16(disp, win, gc, x, y, wstr, len);
		    }
		} else if (x < (int)w->core.width) {
		    SET_GC_FG(w->arttext.highlight_pixel);
		    XFillRectangle(disp, win, gc, x, y - font->ascent,
				   w->core.width - x,
				   font->ascent + font->descent);
		}
	    }
	}
	break;
    case LineTypeSeparator:
	if (y >= (int)w->core.height)
	    return False;

	x = w->arttext.separator_margin;
	width = w->core.width - 2 * x;
	if (width > 0)
	    ShadowDrawShadows((ShadowWidget)w, x, y + node->sep.margin,
			      width, node->sep.height, True);
	break;
    case LineTypeClickable:
	if (y >= (int)w->core.height)
	    return False;

	font = node->cli.font;
	x = w->arttext.margin;
	y += font->ascent;
	SET_GC_FONT;
	SET_GC_FG(node->cli.pixel);

	len = strlen(node->cli.str);
	width = XTextWidth(font, node->cli.str, len);
	XDrawString(disp, win, gc, x, y, node->cli.str, len);
	XDrawLine(disp, win, gc, x, y + 1, width, y + 1);
	break;
    case LineTypeImage:
	if (y >= (int)w->core.height)
	    return False;

	XCopyArea(disp, node->img.pixmap, win, gc, 0, 0,
		  node->img.width, node->img.height,
		  w->arttext.image_margin, y);
	break;
    }

    return True;
}
#undef SET_GC_FONT
#undef SET_GC_BG
#undef SET_GC_FG

static void draw_items(ArtTextWidget w, long first, long last)
{
    if (first < w->arttext.first)
	first = w->arttext.first;
    if (last >= w->arttext.lines)
	last = w->arttext.lines - 1;

    while (first <= last)
	if (!draw_item(w, first++))
	    break;
}

static void clear_items(ArtTextWidget w, long first, long last)
{
    int	y, height;

    if (first >= w->arttext.lines)
	first = w->arttext.lines - 1;
    if (first < 0)
	first = 0;
    if (last >= w->arttext.lines)
	last = w->arttext.lines - 1;
    if (last < 0)
	last = 0;

    y = w->arttext.table[first].y - w->scrollable.pos_y;
    height = w->arttext.table[last + 1].y - w->scrollable.pos_y - y;

    XClearArea(XtDisplay(w), XtWindow(w), 0, y, 0, height, False);
}

static void draw_pixels(ArtTextWidget w, int y, int height, Region region)
{
    Display	*disp = XtDisplay((Widget)w);

    if (region)
	XSetRegion(disp, w->arttext.gc, region);
    else {
	XRectangle	rect;

	rect.x      = 0;
	rect.y      = y;
	rect.width  = w->core.width;
	rect.height = height;
	XSetClipRectangles(disp, w->arttext.gc, 0, 0, &rect, 1, YXBanded);
    }
    draw_items(w, coord_to_line(w, y + w->scrollable.pos_y),
	       coord_to_line(w, y + height + w->scrollable.pos_y));
    XSetClipMask(disp, w->arttext.gc, None);
}

/*************************************************************************/

static char *tab_strdup(const char *str, long len, long *new_len)
{
    char	*res;

    if (!strchr(str, '\t')) {
	*new_len = len;
	res = XtMalloc(len + 1);
	memcpy(res, str, len);
	res[len] = '\0';
    } else { /* this won't happen very often */
	int	i, j, n_alloced;
	
	n_alloced = len + 8;
	res = XtMalloc(n_alloced);

	for (i = 0, j = 0 ; i < len ; i++) {
	    if (j + 10 > n_alloced) {
		n_alloced += 16;
		res = XtRealloc(res, n_alloced);
	    }

	    if (str[i] != '\t')
		res[j++] = str[i];
	    else {
		int	m = 8 - (j & 7);

		while (m-- >= 0)
		    res[j++] = ' ';
	    }
	}

	res[j] = '\0';
	*new_len = j;
    }

    return res;
}

static char *tab_strdupcat(char *str, long len, const char *cat, long *new_len)
{
    long	tmp = strlen(cat);

    str = XtRealloc(str, len + tmp + 1);
    memcpy(str + len, cat, tmp);
    str[len + tmp] = '\0';
    *new_len = len + tmp;

    return str;
}

static void alloc_text_table(ArtTextWidget w, long n)
{
    if (n > w->arttext.n_alloc - 2) {
	long		i = w->arttext.n_alloc;

	w->arttext.n_alloc = 2 * (i + 1);
	w->arttext.table =
	    (TSTable *)XtRealloc((char *)w->arttext.table,
				 w->arttext.n_alloc *
				 sizeof w->arttext.table[0]);
	while (i < w->arttext.n_alloc) {
	    w->arttext.table[i].y = 0;
	    w->arttext.table[i].node = NULL;
	    w->arttext.table[i].start = 0;
	    w->arttext.table[i].len = 0;
	    i++;
	}
    }
}

static void call_clickable(ArtTextWidget w, TSNode *node, int click)
{
    CallbackData	*data;

    switch (node->gen.type) {
    case LineTypeClickable:
	data = node->cli.data;
	break;
    case LineTypeImage:
	data = node->img.data;
	break;
    default:
	return;
    }

    if (!data || !data->callback)
	return;

    data->callback((Widget)w, data->client_data, (XtPointer)&click);
}

static void free_text_data(ArtTextWidget w, long n)
{
    TSNode	*stream = w->arttext.stream;

    w->arttext.table =
	(TSTable *)XtRealloc((char *)w->arttext.table,
			     n * sizeof w->arttext.table[0]);
    w->arttext.n_alloc = n;

    while (n-- > 0) {
	w->arttext.table[n].y = 0;
	w->arttext.table[n].node = NULL;
	w->arttext.table[n].start = 0;
	w->arttext.table[n].len = 0;
    }

    w->arttext.first = 0;
    w->scrollable.pos_y = 0;
    w->scrollable.height = 0;
    w->arttext.lines = 0;
    w->arttext.stream = NULL;
    w->arttext.last = NULL;

    while (stream) {
	TSNode	*next = stream->gen.next;

	switch (stream->gen.type) {
	case LineTypeString:
	    XtFree(stream->str.str);
	    break;
	case LineTypeWString:
	    XtFree((char *)stream->wstr.str);
	    break;
	case LineTypeSeparator:
	    break;
	case LineTypeClickable:
	    call_clickable(w, stream, False);
	    XtFree(stream->cli.str);
	    XtFree((char *)stream->cli.data);
	    break;
	case LineTypeImage:
	    call_clickable(w, stream, False);
	    XtFree((char *)stream->img.data);
	    break;
	}
	XtFree((char *)stream);
	stream = next;
    }
}

static long build_and_append(ArtTextWidget w, TSNode *stream, long y)
{
    long	n = w->arttext.lines;
    XFontStruct	*font;
    long	pos, len;
    char	*str;
    XChar2b	*wstr;
    int		width     = w->core.width - w->arttext.margin;
    int		max_width = 0;

    switch (stream->gen.type) {
    case LineTypeString:
	pos = 0;
	font = stream->str.font;
	len = stream->str.len;
	str = stream->str.str;
	if (w->arttext.wrap_lines)
	    do {
		int	tmp;

		tmp = MyXWidthToChars(font, str, len, width);
		if (tmp <= 0 && len > 0)
		    tmp = 1;

		if (n + 8 > w->arttext.n_alloc)
		    alloc_text_table(w, 2 * (n + 1));
		w->arttext.table[n].y = y;
		w->arttext.table[n].node = stream;
		w->arttext.table[n].start = pos;
		w->arttext.table[n].len = tmp;
		y += font->ascent + font->descent;
		n++;
		str += tmp;
		pos += tmp;
		len -= tmp;
	    } while (len > 0);
	else {
	    max_width = XTextWidth(font, str, len) + 2 * w->arttext.margin;
	    if (n + 8 > w->arttext.n_alloc)
		alloc_text_table(w, 2 * (n + 1));
	    w->arttext.table[n].y = y;
	    w->arttext.table[n].node = stream;
	    w->arttext.table[n].start = 0;
	    w->arttext.table[n].len = len;
	    y += font->ascent + font->descent;
	    n++;
	}
	break;
    case LineTypeWString:
	pos = 0;
	font = stream->wstr.font;
	len = stream->wstr.len;
	wstr = stream->wstr.str;
	if (w->arttext.wrap_lines)
	    do {
		int	tmp;

		tmp = MyXWidthToWChars(font, wstr, len, width);
		if (tmp <= 0 && len > 0)
		    tmp = 1;

		if (n + 8 > w->arttext.n_alloc)
		    alloc_text_table(w, 2 * (n + 1));
		w->arttext.table[n].y = y;
		w->arttext.table[n].node = stream;
		w->arttext.table[n].start = pos;
		w->arttext.table[n].len = tmp;
		y += font->ascent + font->descent;
		n++;
		wstr += tmp;
		pos += tmp;
		len -= tmp;
	    } while (len > 0);
	else {
	    max_width = XTextWidth16(font, wstr, len) + 2 * w->arttext.margin;
	    if (n + 8 > w->arttext.n_alloc)
		alloc_text_table(w, 2 * (n + 1));
	    w->arttext.table[n].y = y;
	    w->arttext.table[n].node = stream;
	    w->arttext.table[n].start = 0;
	    w->arttext.table[n].len = len;
	    y += font->ascent + font->descent;
	    n++;
	}
	break;
    case LineTypeClickable:
	if (n + 8 > w->arttext.n_alloc)
	    alloc_text_table(w, 2 * (n + 8));

	font = stream->cli.font;
	max_width =
	    XTextWidth(font, stream->cli.str, strlen(stream->cli.str)) +
	    2 * w->arttext.margin;
	w->arttext.table[n].y = y;
	w->arttext.table[n].node = stream;
	w->arttext.table[n].start = 0;
	w->arttext.table[n].len = 0;
	y += font->ascent + font->descent;
	n++;
	break;
    case LineTypeSeparator:
	if (n + 8 > w->arttext.n_alloc)
	    alloc_text_table(w, 2 * (n + 8));

	w->arttext.table[n].y = y;
	w->arttext.table[n].node = stream;
	w->arttext.table[n].start = 0;
	w->arttext.table[n].len = 0;
	y += stream->sep.height + 2 * stream->sep.margin;
	n++;
	break;
    case LineTypeImage:
	if (n + 8 > w->arttext.n_alloc)
	    alloc_text_table(w, 2 * (n + 8));

	max_width = stream->img.width + 2 * w->arttext.image_margin;
	w->arttext.table[n].y = y;
	w->arttext.table[n].node = stream;
	w->arttext.table[n].start = 0;
	w->arttext.table[n].len = 0;
	y += stream->img.height;
	n++;
	break;
    }

    if (w->arttext.max_width < max_width)
	w->arttext.max_width = max_width;
    w->arttext.lines = n;

    return y;
}

static void build_text_table(ArtTextWidget w)
{
    TSNode	*stream = w->arttext.stream;
    long	n, y = 0;

    alloc_text_table(w, 8);
    w->arttext.lines = 0;
    w->arttext.table[0].y = 0;
    w->arttext.table[0].node = NULL;

    while (stream) {
	y = build_and_append(w, stream, y);
	stream = stream->gen.next;
    }

    n = w->arttext.lines;
    w->arttext.table[n].y = y;
    w->arttext.table[n].node = NULL;
    w->scrollable.height = y;
    if (w->arttext.first >= n)
	w->arttext.first = n > 0 ? n - 1 : 0;
}

static void append_text_node(ArtTextWidget w, TSNode *node)
{
    long	n = w->arttext.lines;
    long	y;
    int		width = w->arttext.max_width;

    node->gen.next = NULL;

    if (w->arttext.last)
	w->arttext.last->gen.next = node;
    else
	w->arttext.stream = node;
    w->arttext.last = node;

    y = build_and_append(w, node, w->arttext.table[n].y);
    w->arttext.table[w->arttext.lines].y = y;
    w->arttext.table[w->arttext.lines].node = NULL;
    w->scrollable.height = y;

    if (w->arttext.table[n].y < w->scrollable.pos_y + (int)w->core.height)
	draw_items(w, n, w->arttext.lines - 1);

    if (w->arttext.max_width > width) {
	XtMakeResizeRequest((Widget)w, w->arttext.max_width, w->core.height,
			    NULL, NULL);
	ScrollableFitHBar((ScrollableWidget)w);
    }
}

static void update_last_node(ArtTextWidget w)
{
    long	n = w->arttext.lines;
    long	y;

    do {
	n--;
    } while (n >= 0 && w->arttext.table[n].node == w->arttext.last);

    w->arttext.lines = ++n;
    y = w->arttext.table[n].y;
    w->arttext.table[n].node = NULL;
    y = build_and_append(w, w->arttext.last, y);
    w->arttext.table[w->arttext.lines].y = y;
    w->arttext.table[w->arttext.lines].node = NULL;
    w->scrollable.height = y;

    draw_items(w, n, w->arttext.lines);
    /*
     *  FIXME: resize if necessary?
     */
}

/*************************************************************************/

static void coords_to_line_offset(ArtTextWidget w, int ex, int ey,
				  long *line, long *offset)
{
    long	y, n;

    ex -= w->arttext.margin;
    y = ey + w->scrollable.pos_y;
    *offset = 0;
    if (y < 0 || w->arttext.lines <= 0) {
	*line = 0;
	return;
    }

    n = coord_to_line(w, y);
    *line = n;

    if (n >= 0 && n < w->arttext.lines) {
	TSTable		*table = w->arttext.table + n;
	TSNode		*node = table->node;
	XFontStruct	*font;

	switch (node->gen.type) {
	case LineTypeString:
	    font = node->str.font;
	    *offset = MyXWidthToChars(font, node->str.str + table->start,
				      table->len, ex);
	    break;
	case LineTypeWString:
	    font = node->wstr.font;
	    *offset = MyXWidthToWChars(font, node->wstr.str + table->start,
				       table->len, ex);
	    break;
	}
    }
}

static void make_visible(ArtTextWidget w, long line)
{
    long	y;

    if (w->arttext.table[line].y < w->scrollable.pos_y)
	y = w->arttext.table[line].y;
    else if (line >= w->arttext.lines)
	return;
    else if (w->arttext.table[line + 1].y >=
	     w->scrollable.pos_y + (int)w->core.height)
	y = w->arttext.table[line + 1].y - w->core.height;
    else
	return;

    if (y < 0)
	y = 0;
    ScrollableSetVPos((Widget)w, y);
}

/*************************************************************************/

static long get_sel_len(ArtTextWidget w)
{
    TSTable	*tstart, *tstop;
    TSNode	*start, *stop;
    long	len;
    long	start_off;
    long	stop_off;

    if (!w->arttext.sel_ok ||
	w->arttext.sel_start_line > w->arttext.sel_stop_line)
	return 0;

    tstart = w->arttext.table + w->arttext.sel_start_line;
    tstop = w->arttext.table + w->arttext.sel_stop_line;
    start = tstart->node;
    stop = tstop->node;

    if (!start || !stop)
	return 0;

    start_off = tstart->start + w->arttext.sel_start_offset;
    stop_off = tstop->start + w->arttext.sel_stop_offset;

    if (start == stop)
	if (start->gen.type != LineTypeString)
	    return 0;
	else
	    return stop_off - start_off + 1;

    len = 0;
    if (start->gen.type == LineTypeString) {
	if (start_off < start->str.len)
	    len += start->str.len - start_off;
	len++;
    }

    start = start->gen.next;
    while (start && start != stop) {
	if (start->gen.type == LineTypeString)
	    len += start->str.len + 1;
	start = start->gen.next;
    }

    if (stop->gen.type == LineTypeString) {
	if (stop->str.len == 0)
	    len++;
	else if (stop_off < stop->str.len)
	    len += stop_off + 1;
	else
	    len += stop->str.len + 1;
    }

    return len;
}

static long get_sel(ArtTextWidget w, char *buf)
{
    TSTable	*tstart, *tstop;
    TSNode	*start, *stop;
    long	len, tmp;
    long	start_off;
    long	stop_off;

    if (!w->arttext.sel_ok ||
	w->arttext.sel_start_line > w->arttext.sel_stop_line)
	return 0;

    tstart = w->arttext.table + w->arttext.sel_start_line;
    tstop = w->arttext.table + w->arttext.sel_stop_line;
    start = tstart->node;
    stop = tstop->node;

    if (!start || !stop)
	return 0;

    start_off = tstart->start + w->arttext.sel_start_offset;
    stop_off = tstop->start + w->arttext.sel_stop_offset;

    if (start == stop)
	if (start->gen.type != LineTypeString)
	    return 0;
	else {
	    len = stop_off - start_off + 1;
	    memcpy(buf, start->str.str + start_off, len);
	    if (stop_off >= start->str.len)
		buf[len - 1] = '\n';

	    return len;
	}

    len = 0;
    if (start->gen.type == LineTypeString) {
	if (start_off < start->str.len) {
	    len = start->str.len - start_off;
	    memcpy(buf, start->str.str + start_off, len);
	}
	buf[len++] = '\n';
    }

    while ((start = start->gen.next) && start != stop)
	if (start->gen.type == LineTypeString) {
	    tmp = start->str.len;
	    memcpy(buf + len, start->str.str, tmp);
	    len += tmp;
	    buf[len++] = '\n';
	}

    if (stop->gen.type == LineTypeString)
	if (stop->str.len == 0)
	    buf[len++] = '\n';
	else if (stop_off < stop->str.len) {
	    tmp = stop_off + 1;
	    memcpy(buf + len, stop->str.str, tmp);
	    len += tmp;
	} else {
	    tmp = stop->str.len;
	    memcpy(buf + len, stop->str.str, tmp);
	    len += tmp;
	    buf[len++] = '\n';
	}

    return len;
}

static Boolean convert_selection(Widget gw, Atom *selection, Atom *target,
				 Atom *type_return, XtPointer *value_return,
				 unsigned long *length_return,
				 int *format_return)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    Display		*disp = XtDisplay(w);

    if (!w->arttext.sel_ok || *selection != w->arttext.curr_sel)
	return False;

    if (*target == XA_STRING || *target == intern_atom(disp, "TEXT")) {
	char	*buffer;
	long	len;

	len = get_sel_len(w);
	buffer = XtMalloc(len + 16);
	len = get_sel(w, buffer);
	buffer[len] = '\0';

	*value_return = (XtPointer)buffer;
	*length_return = len;
	*type_return = XA_STRING;
	*format_return = 8;
	
	return True;
    }

    if (*target == intern_atom(disp, "TARGETS")) {
	Atom		*std_targets, *atom;
	unsigned long	std_length, n;

	if (!cvt_std_sel((Widget)w, w->arttext.sel_time,
			 selection, target, type_return,
			 (XPointer *)&std_targets, &std_length,
			 format_return))
	    return False;

	*value_return = (XtPointer)XtMalloc((std_length + 8) * sizeof(Atom));
	atom = (Atom *)*value_return;
	n = std_length;

	n++; *atom++ = XA_STRING;
	n++; *atom++ = intern_atom(disp, "TEXT");
	n++; *atom++ = intern_atom(disp, "LENGTH");
	n++; *atom++ = intern_atom(disp, "LIST_LENGTH");

	memcpy(atom, std_targets, std_length * sizeof(Atom));
	XtFree((char *)std_targets);

	*length_return = n;
	*type_return = intern_atom(disp, "ATOM");
	*format_return = 32;

	return True;
    }

    if (*target == intern_atom(disp, "LENGTH")) {
	long	*length;

	length = (long *)XtMalloc(sizeof(long));
	*length = get_sel_len(w);

	*value_return = (XtPointer)length;
	*type_return = XA_INTEGER;
	*length_return = 1;
	*format_return = 32;

	return True;
    }

    if (*target == intern_atom(disp, "LIST_LENGTH")) {
	long	*length = (long *)XtMalloc(sizeof(long));

	*length = 1;
	*value_return = (XtPointer)length;
	*type_return = XA_INTEGER;
	*length_return = 1;
	*format_return = 32;

	return True;
    }

    return cvt_std_sel((Widget)w, w->arttext.sel_time,
		       selection, target, type_return,
		       (XPointer *)value_return,
		       length_return, format_return);
}

static void lose_selection(Widget gw, Atom *selection)
{
    ArtTextWidget	w = (ArtTextWidget)gw;

    if (w->arttext.sel_ok) {
	w->arttext.sel_ok = False;
	clear_items(w, w->arttext.sel_start_line, w->arttext.sel_stop_line);
	draw_items(w, w->arttext.sel_start_line, w->arttext.sel_stop_line);
    }
}

static int change_selection(ArtTextWidget w,
			    long start_line, int start_offset,
			    long stop_line, int stop_offset)
{
    int		did_swap = False;
    long	o_start_line   = w->arttext.sel_start_line;
    long	o_stop_line    = w->arttext.sel_stop_line;
    int		o_start_offset = w->arttext.sel_start_offset;
    int		o_stop_offset  = w->arttext.sel_stop_offset;

    if (o_start_line == start_line && o_stop_line == stop_line &&
	o_start_offset == start_offset && o_stop_offset == stop_offset)
	return False;

    if (start_line > stop_line || (start_line == stop_line &&
				   start_offset > stop_offset)) {
    long	tmp;

#undef  SWAP
#define SWAP(a, b) (tmp = a, a = b, b = tmp)
	SWAP(start_line,   stop_line);
	SWAP(start_offset, stop_offset);
#undef SWAP
	did_swap = True;
    }

    w->arttext.sel_start_line   = start_line;
    w->arttext.sel_stop_line    = stop_line;
    w->arttext.sel_start_offset = start_offset;
    w->arttext.sel_stop_offset  = stop_offset;

    if (o_start_line > start_line)
	draw_items(w, start_line, o_start_line);
    else if (o_start_line < start_line) {
	clear_items(w, o_start_line, start_line);
	draw_items(w, o_start_line, start_line);
    } else {
	if (o_start_offset < start_offset)
	    clear_items(w, start_line, start_line);
	draw_item(w, start_line);
    }

    if (o_stop_line < stop_line)
	draw_items(w, o_stop_line, stop_line);
    else if (o_stop_line > stop_line) {
	clear_items(w, stop_line, o_stop_line);
	draw_items(w, stop_line, o_stop_line);
    } else {
	if (o_stop_offset > stop_offset)
	    clear_items(w, stop_line, stop_line);
	draw_item(w, stop_line);
    }

    return did_swap;
}

/*************************************************************************/

static void click(Widget    gw,
		  XEvent   *event,
		  String   *params,
		  Cardinal *no_params)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    long		line, offset;
    int			x, y, button;
    TSNode		*node;

    if (!get_event_xy(event, &x, &y)) {
	XBell(XtDisplay(w), 0);
	return;
    }

    coords_to_line_offset(w, x, y, &line, &offset);
    if (line < 0 || line >= w->arttext.lines)
	return;

    if (event->type != ButtonPress && event->type != ButtonRelease)
	button = 1;
    else
	button = event->xbutton.button;

    node = w->arttext.table[line].node;
    if (node)
	call_clickable(w, node, True);
}

static void select_start(Widget    gw,
			 XEvent   *event,
			 String   *params,
			 Cardinal *no_params)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    long		line, offset;
    int			x, y;

    if (!get_event_xy(event, &x, &y)) {
	XBell(XtDisplay(w), 0);
	return;
    }

    if (w->arttext.sel_ok) {
	w->arttext.sel_ok = False;
	clear_items(w, w->arttext.sel_start_line, w->arttext.sel_stop_line);
	draw_items(w, w->arttext.sel_start_line, w->arttext.sel_stop_line);
	if (w->arttext.curr_sel) {
	    XtDisownSelection((Widget)w, w->arttext.curr_sel,
			      w->arttext.sel_time);
	    w->arttext.curr_sel = (Atom)0;
	}
    }

    coords_to_line_offset(w, x, y, &line, &offset);
    if (line < 0 || line >= w->arttext.lines ||
	(w->arttext.table[line].node->gen.type != LineTypeString &&
	 w->arttext.table[line].node->gen.type != LineTypeWString))
	return;

    w->arttext.sel_start_line = line;
    w->arttext.sel_stop_line  = line;
    w->arttext.sel_start_offset = offset;
    w->arttext.sel_stop_offset  = offset;
    w->arttext.sel_ok = (w->arttext.lines > 0);
    w->arttext.extending = True;
    w->arttext.extend_end = True;

    draw_item(w, line);
}

static void select_extend_start(Widget    gw,
				XEvent   *event,
				String   *params,
				Cardinal *no_params)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    long		line, offset;
    int			x, y;
    Boolean		extend_start;

    if (!w->arttext.sel_ok) {
	select_start(gw, event, params, no_params);
	return;
    }

    if (!get_event_xy(event, &x, &y)) {
	XBell(XtDisplay(w), 0);
	return;
    }

    coords_to_line_offset(w, x, y, &line, &offset);
    if (line < 0 || line >= w->arttext.lines ||
	(w->arttext.table[line].node->gen.type != LineTypeString &&
	 w->arttext.table[line].node->gen.type != LineTypeWString))
	return;

    if (line < w->arttext.sel_start_line)
	extend_start = True;
    else if (line > w->arttext.sel_stop_line)
	extend_start = False;
    else if (w->arttext.sel_start_line == w->arttext.sel_stop_line) {
	if (offset <= w->arttext.sel_start_offset)
	    extend_start = True;
	else if (offset >= w->arttext.sel_stop_line)
	    extend_start = False;
	else if (offset - w->arttext.sel_start_offset <=
		 w->arttext.sel_stop_offset - offset)
	    extend_start = True;
	else
	    extend_start = False;
    } else {
	if (line - w->arttext.sel_start_line <=
	    w->arttext.sel_stop_line - line)
	    extend_start = True;
	else
	    extend_start = False;
    }

    w->arttext.extend_end = !extend_start;
    w->arttext.extending = True;
    if (extend_start)
	change_selection(w, line, offset, w->arttext.sel_stop_line,
			 w->arttext.sel_stop_offset);
    else
	change_selection(w, w->arttext.sel_start_line,
			 w->arttext.sel_start_offset, line, offset);
}

static void select_extend(Widget    gw,
			  XEvent   *event,
			  String   *params,
			  Cardinal *no_params)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    long		line, offset;
    int			x, y, did_swap;

    if (!w->arttext.sel_ok || !w->arttext.extending)
	return;

    if (!get_event_xy(event, &x, &y)) {
	XBell(XtDisplay(w), 0);
	return;
    }

    coords_to_line_offset(w, x, y, &line, &offset);
    if (line < 0 || line >= w->arttext.lines ||
	(w->arttext.table[line].node->gen.type != LineTypeString &&
	 w->arttext.table[line].node->gen.type != LineTypeWString))
	return;

    make_visible(w, line);

    if (w->arttext.extend_end)
	did_swap = change_selection(w, w->arttext.sel_start_line,
				    w->arttext.sel_start_offset, line, offset);
    else
	did_swap = change_selection(w, line, offset, w->arttext.sel_stop_line,
				    w->arttext.sel_stop_offset);

    if (did_swap)
	w->arttext.extend_end = !w->arttext.extend_end;
}

static void select_end(Widget    gw,
		       XEvent   *event,
		       String   *params,
		       Cardinal *no_params)
{
    ArtTextWidget	w = (ArtTextWidget)gw;

    w->arttext.extending = False;
    if (w->arttext.sel_ok) {
	Atom	atom = XA_PRIMARY;
	Time	time_stamp = get_event_time(event);

	if (*no_params > 0)
	    atom = intern_atom(XtDisplay(w), params[0]);

	if (XtOwnSelection((Widget)w, atom, time_stamp, convert_selection,
			   lose_selection, NULL)) {
	    w->arttext.curr_sel = atom;
	    w->arttext.sel_time = time_stamp;
	}
    }
}

static void call_url(Widget    gw,
		     XEvent   *event,
		     String   *params,
		     Cardinal *no_params)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    XtCallbackList	c_list = w->arttext.url_callback;
    ArtTextUrlReport	report;
    long		line, offset;
    int			x, y;

    if (!get_event_xy(event, &x, &y)) {
	XBell(XtDisplay(w), 0);
	return;
    }
    coords_to_line_offset(w, x, y, &line, &offset);
    if (line < 0 || line >= w->arttext.lines ||
	(w->arttext.table[line].node->gen.type != LineTypeString &&
	 w->arttext.table[line].node->gen.type != LineTypeWString))
	return;

    if (w->arttext.sel_ok) {
	if (line == w->arttext.sel_start_line &&
	    line == w->arttext.sel_stop_line &&
	    offset >= w->arttext.sel_start_offset &&
	    offset <= w->arttext.sel_stop_offset) {
	    report.sel_ok = True;
	    report.start = w->arttext.sel_start_offset;
	    report.stop = w->arttext.sel_stop_offset;
	} else {
	    w->arttext.sel_ok = False;
	    clear_items(w, w->arttext.sel_start_line,
			w->arttext.sel_stop_line);
	    draw_items(w, w->arttext.sel_start_line,
		       w->arttext.sel_stop_line);
	    if (w->arttext.curr_sel) {
		XtDisownSelection((Widget)w, w->arttext.curr_sel,
				  w->arttext.sel_time);
		w->arttext.curr_sel = 0;
	    }
	}
    }

    if (!w->arttext.sel_ok) {
	report.sel_ok = False;
	report.start = offset;
	report.stop = offset;
    }

    if (line < 0 || w->arttext.table[line].node->gen.type != LineTypeString) {
	XBell(XtDisplay(w), 0);
	return;
    }

    report.line =
	w->arttext.table[line].node->str.str +
	w->arttext.table[line].start;

    if (c_list)
	XtCallCallbackList((Widget)w, c_list, (XtPointer)&report);

    if (!w->arttext.sel_ok && report.sel_ok) {
	Atom	atom = XA_PRIMARY;
	Time	time_stamp = get_event_time(event);
	long    len = strlen(report.line);

	if (report.start >= 0 && report.stop <= len &&
	    report.stop >= report.start) {
	    w->arttext.sel_start_line = w->arttext.sel_stop_line = line;
	    w->arttext.sel_start_offset = report.start;
	    w->arttext.sel_stop_offset = report.stop;
	    w->arttext.sel_ok = True;
	    w->arttext.extending = False;
	    draw_item(w, line);

	    if (*no_params > 0)
		atom = intern_atom(XtDisplay(w), params[0]);

	    if (XtOwnSelection((Widget)w, atom, time_stamp, convert_selection,
			       lose_selection, NULL)) {
		w->arttext.curr_sel = atom;
		w->arttext.sel_time = time_stamp;
	    }
	}
    }
}

/*************************************************************************/

static void Initialize(Widget    grequest,
		       Widget    gnew,
		       ArgList   args,
		       Cardinal *no_args)
{
    ArtTextWidget	new = (ArtTextWidget)gnew;

    new->arttext.sel_ok = False;
    new->arttext.extending = False;
    new->arttext.curr_sel = (Atom)0;
    new->arttext.table = NULL;
    new->arttext.stream = NULL;
    new->arttext.n_alloc = 0;
    new->arttext.first = 0;
    new->arttext.lines = 0;
    new->arttext.gc_fid = None;
    new->arttext.max_width = 0;
    if (new->core.width  == 0)
	new->core.width  = preferred_width(new);
    if (new->core.height == 0)
	new->core.height = preferred_height(new);
    free_text_data(new, 256);
}

static void Destroy(Widget gw)
{
    ArtTextWidget	w = (ArtTextWidget)gw;

    if (w->arttext.sel_ok) {
	if (w->arttext.curr_sel) {
	    XtDisownSelection((Widget)w, w->arttext.curr_sel, CurrentTime);
	    w->arttext.curr_sel = (Atom)0;
	}
	w->arttext.sel_ok = False;
    }
    free_text_data(w, 0);
    w->arttext.first = 0;
    w->scrollable.pos_y = 0;
    w->arttext.lines = 0;
    w->arttext.table = NULL;
    w->arttext.n_alloc = 0;

    if (w->arttext.gc != 0)
	XFreeGC(XtDisplay(w), w->arttext.gc);
}

static void Realize(Widget gw, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    XGCValues		values;

    ScrollableHFromGeometry((ScrollableWidget)w);
    build_text_table(w);
    /*
     *  FIXME: resize if necessary?
     */
    w->scrollable.shown_y = w->core.height;
    w->scrollable.height = MAX_Y(w);
    scrollableWidgetClass->core_class.realize((Widget)w, mask, attributes);

    values.font       = w->arttext.gc_fid = w->arttext.font->fid;
    values.foreground = w->arttext.gc_fg  = 0;
    values.background = w->arttext.highlight_pixel;
    w->arttext.gc = XCreateGC(XtDisplay(w), XtWindow(w),
			      GCFont|GCForeground|GCBackground, &values);
}

static void Resize(Widget gw)
{
    ArtTextWidget	w = (ArtTextWidget)gw;

    if (!XtIsRealized((Widget)w))
	return;

    if (w->arttext.sel_ok) {
	if (w->arttext.curr_sel) {
	    XtDisownSelection((Widget)w, w->arttext.curr_sel, CurrentTime);
	    w->arttext.curr_sel = (Atom)0;
	}
	w->arttext.sel_ok = False;
    }

    ScrollableHFromGeometry((ScrollableWidget)w);
    build_text_table(w);
    w->scrollable.shown_y = w->core.height;
    w->scrollable.height = MAX_Y(w);
    ScrollableFitHBar((ScrollableWidget)w);
    ScrollableFitVBar((ScrollableWidget)w);
}

static void SetVPos(ScrollableWidget gw, long pos_y)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);
    long		d = w->scrollable.pos_y - pos_y;

    if (d == 0)
	return;

    w->arttext.first = coord_to_line(w, pos_y);
    w->scrollable.pos_y = pos_y;
    if (d < 0) {
	d = - d;
	if (d < (int)w->core.height) {
	    XCopyArea(disp, win, win, w->arttext.gc,
		      0, d, w->core.width, w->core.height - d, 0, 0);
	    XClearArea(disp, win, 0, w->core.height - d, 0, 0, False);
	    draw_pixels(w, w->core.height - d, w->core.height, NULL);
	} else {
	    XClearWindow(disp, win);
	    draw_pixels(w, 0, w->core.height, NULL);
	}
    } else {
	if (d < (int)w->core.height) {
	    XCopyArea(disp, win, win, w->arttext.gc,
		      0, 0, w->core.width, w->core.height - d, 0, d);
	    XClearArea(disp, win, 0, 0, 0, d, False);
	    draw_pixels(w, 0, d, NULL);
	} else {
	    XClearWindow(disp, win);
	    draw_pixels(w, 0, w->core.height, NULL);
	}
    }
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    int			y, height;

    if (!XtIsRealized((Widget)w))
	return;

    if (event)
	switch (event->type) {
	case Expose:
	    y = event->xexpose.y;
	    height = event->xexpose.height;
	    break;
	case GraphicsExpose:
	    y = event->xgraphicsexpose.y;
	    height = event->xgraphicsexpose.height;
	    break;
	default:
	    return;
	}
    else {
	y = 0;
	height = w->core.height;
    }

    draw_pixels(w, y, height, region);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    ArtTextWidget	current = (ArtTextWidget)gcurrent;
    ArtTextWidget	new = (ArtTextWidget)gnew;
    int			redisplay = False;
    int			relayout  = False;

    if (new->arttext.highlight_pixel != current->arttext.highlight_pixel) {
	if (new->arttext.gc != 0)
	    XSetBackground(XtDisplay(new), new->arttext.gc,
			   new->arttext.highlight_pixel);
	redisplay = True;
    }

    if (new->arttext.separator_margin != current->arttext.separator_margin)
	redisplay = True;

    if (new->arttext.image_margin != current->arttext.image_margin ||
	new->arttext.margin       != current->arttext.margin       ||
	new->arttext.wrap_lines   != current->arttext.wrap_lines)
	redisplay = relayout = True;

    if (relayout) {
	build_text_table(new);
	new->core.width = new->arttext.max_width;
    }

    return redisplay;
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
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
    else if (preferred->width == w->core.width &&
             preferred->height == w->core.height)
	return XtGeometryNo;
    else
	return XtGeometryAlmost;
}

/*************************************************************************/

static void rot_13(char *c, long len)
{
    while (len-- > 0) {
	if ((*c >= 'a' && *c < 'a' + 13) ||
	    (*c >= 'A' && *c < 'A' + 13))
	    *c += 13;
	else if ((*c >= 'a' + 13 && *c <= 'z') ||
		 (*c >= 'A' + 13 && *c <= 'Z'))
	    *c -= 13;
	c++;
    }
}

void ArtTextRot13(Widget gw)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*stream;

    for (stream = w->arttext.stream ; stream ; stream = stream->gen.next)
	if (stream->gen.type == LineTypeString)
	    rot_13(stream->str.str, stream->str.len);

    build_text_table(w);
    /*
     *  FIXME: resize if necessary?
     */
    if (XtIsRealized((Widget)w)) {
	XClearWindow(XtDisplay(w), XtWindow(w));
	Redisplay((Widget)w, NULL, 0);
    }
    ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextClearLines(Widget gw)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    Widget		parent = XtParent(w);

    if (w->arttext.sel_ok) {
	if (w->arttext.curr_sel) {
	    XtDisownSelection((Widget)w, w->arttext.curr_sel, CurrentTime);
	    w->arttext.curr_sel = (Atom)0;
	}
	w->arttext.sel_ok = False;
    }
    free_text_data(w, w->arttext.n_alloc);
    XClearWindow(XtDisplay(w), XtWindow(w));
    w->arttext.max_width = 0;
    XtMakeResizeRequest((Widget)w, parent->core.width,
			parent->core.height, NULL, NULL);
    ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextAddLine(Widget gw, const char *str, XFontStruct *font, Pixel pixel)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*node;

    if (!font)
	font = w->arttext.font;

    node = (TSNode *)XtMalloc(sizeof *node);
    node->str.type = LineTypeString;
    node->str.font = font;
    node->str.pixel = pixel;
    node->str.str = tab_strdup(str, strlen(str), &node->str.len);

    append_text_node(w, node);
    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextAppendToLast(Widget gw, const char *str)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*last = w->arttext.last;
    long		n;

    if (!last)
	ArtTextAddLine((Widget)w, str, NULL, w->core.background_pixel);
    else
	switch (last->gen.type) {
	case LineTypeString:
	    last->str.str =
		tab_strdupcat(last->str.str, last->str.len,
			      str, &last->str.len);
	    update_last_node(w);
	    break;
	case LineTypeClickable:
	    n = strlen(last->cli.str) + strlen(str) + 4;
	    last->cli.str = XtRealloc(last->cli.str, n);
	    strcat(last->cli.str, str);
	    update_last_node(w);
	    break;
	default:
	    ArtTextAddLine((Widget)w, str, NULL, 0);
	    break;
	}

    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextAddSelected(Widget gw, const char *str,
			XFontStruct *font, Pixel pixel,
			long sel_start, long sel_stop)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*node;
    long		n;

    for (n = 0 ; str[n] != '\0' ; n++) {
	if (str[n] == '\t') {
	    long	i = 8 - (n % 8);

	    if (sel_start > n)
		sel_start += i;
	    if (sel_stop > n)
		sel_stop += i;
	}
    }

    if (!font)
	font = w->arttext.font;

    node = (TSNode *)XtMalloc(sizeof *node);
    node->str.type = LineTypeString;
    node->str.font = font;
    node->str.pixel = pixel;
    node->str.str = tab_strdup(str, strlen(str), &node->str.len);

    n = w->arttext.lines;
    append_text_node(w, node);

    if (w->arttext.sel_ok) {
	if (w->arttext.curr_sel) {
	    XtDisownSelection((Widget)w, w->arttext.curr_sel, CurrentTime);
	    w->arttext.curr_sel = (Atom)0;
	}
	w->arttext.sel_ok = False;
	draw_items(w, w->arttext.sel_start_line, w->arttext.sel_stop_line);
    }

    if (sel_start > sel_stop)
	return;

    while (n < w->arttext.lines)
	if (w->arttext.table[n].start + w->arttext.table[n].len > sel_start)
	    break;
	else
	    n++;

    w->arttext.sel_start_line = n;
    sel_start -= w->arttext.table[n].start;
    while (n < w->arttext.lines)
	if (w->arttext.table[n].start + w->arttext.table[n].len > sel_stop)
	    break;
	else
	    n++;

    if (n == w->arttext.lines)
	return;

    w->arttext.sel_stop_line = n;
    sel_stop -= w->arttext.table[n].start;

    w->arttext.sel_start_offset = sel_start;
    w->arttext.sel_stop_offset = sel_stop;
    w->arttext.sel_ok = True;
    /* FIXME: own selection? */

    if (w->arttext.table[w->arttext.sel_start_line+1].y > (int)w->core.height)
	ScrollableSetVPos((Widget)w,
			  w->arttext.table[w->arttext.sel_start_line].y);
    else {
	draw_items(w, w->arttext.sel_start_line, w->arttext.sel_stop_line);
	ScrollableFitVBar((ScrollableWidget)w);
    }
}

void ArtTextAddSeparator(Widget gw, int height, int margin)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*node;

    node = (TSNode *)XtMalloc(sizeof *node);
    node->sep.type = LineTypeSeparator;
    node->sep.pixel = 0;
    node->sep.height = height;
    node->sep.margin = margin;

    append_text_node(w, node);
    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextAddClickable(Widget gw, const char *str,
			 XFontStruct *font, Pixel pixel,
			 XtCallbackProc callback, void *client_data)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*node;

    if (!font)
	font = w->arttext.font;

    node = (TSNode *)XtMalloc(sizeof *node);
    node->cli.type = LineTypeClickable;
    node->cli.font = font;
    node->cli.pixel = pixel;
    node->cli.str = XtNewString(str);
    node->cli.data = (CallbackData *)XtMalloc(sizeof *node->cli.data);
    node->cli.data->callback = callback;
    node->cli.data->client_data = client_data;

    append_text_node(w, node);
    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextAddImage(Widget gw, Pixmap pixmap, int width, int height,
		     XtCallbackProc callback, XtPointer client_data)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*node;

    node = (TSNode *)XtMalloc(sizeof *node);
    node->img.type = LineTypeImage;
    node->img.pixmap = pixmap;
    node->img.width  = width;
    node->img.height = height;
    node->img.data = (CallbackData *)XtMalloc(sizeof *node->img.data);
    node->img.data->callback = callback;
    node->img.data->client_data = client_data;

    append_text_node(w, node);
    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

int ArtTextDumpToFile(Widget gw, FILE *file)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*node;
    char		*c = NULL;

    for (node = w->arttext.stream ; node ; node = node->gen.next) {
	switch (node->gen.type) {
	case LineTypeString:
	    c = node->str.str;
	    break;
	case LineTypeWString:
	    c = NULL;
	    break;
	case LineTypeSeparator:
	    c = "------------------------------------------------";
	    break;
	case LineTypeClickable:
	    c = node->cli.str;
	    break;
	case LineTypeImage:
	    c = "[IMAGE]";
	    break;
	}

	if (c && fprintf(file, "%s\n", c) == EOF)
	    return -1;
    }

    return 0;
}

void ArtTextAddWLine(Widget gw, XChar2b *wstr, long len,
		     XFontStruct *font, Pixel pixel)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*node;

    if (!font)
	font = w->arttext.font;

    node = (TSNode *)XtMalloc(sizeof *node);
    node->wstr.type = LineTypeWString;
    node->wstr.font = font;
    node->wstr.pixel = pixel;
    node->wstr.str = (XChar2b *)XtMalloc(len * sizeof wstr[0]);
    node->wstr.len = len;
    memcpy(node->wstr.str, wstr, len * sizeof wstr[0]);

    append_text_node(w, node);
    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextWAppendToLast(Widget gw, XChar2b *wstr, long len)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    TSNode		*last = w->arttext.last;

    if (!last || last->gen.type != LineTypeWString) {
	ArtTextAddWLine((Widget)w, wstr, len, NULL, 0);
	return;
    }

    last->wstr.str =
	(XChar2b *)XtRealloc((char *)last->wstr.str,
			     (last->wstr.len + len) * sizeof wstr[0]);
    memcpy(last->wstr.str + last->wstr.len, wstr, len);
    last->wstr.len += len;

    update_last_node(w);
    if (!w->scrollable.suspended)
	ScrollableFitVBar((ScrollableWidget)w);
}

void ArtTextAllocLines(Widget gw, long n_alloc)
{
    ArtTextWidget	w = (ArtTextWidget)gw;
    long		n = w->arttext.n_alloc;

    if (n_alloc <= n)
	return;

    w->arttext.n_alloc = n_alloc;
    w->arttext.table =
	(TSTable *)XtRealloc((char *)w->arttext.table,
			     n_alloc * sizeof w->arttext.table[0]);

    while (n < n_alloc) {
	w->arttext.table[n].y = 0;
	w->arttext.table[n].node = NULL;
	w->arttext.table[n].start = 0;
	w->arttext.table[n].len = 0;
	n++;
    }
}
