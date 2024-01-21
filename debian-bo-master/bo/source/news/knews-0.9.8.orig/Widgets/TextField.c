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
#include <X11/Xatom.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "Compat.h"
#include "Util.h"
#include "TextFieldP.h"

static XtResource resources[] = {
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(TextFieldRec, core.border_width), XtRImmediate, (XtPointer)2},
#define offset(field) XtOffsetOf(TextFieldRec, textfield.field)
    {XtNbuffer, XtCBuffer, XtRString, sizeof(String),
     offset(buffer), XtRImmediate, (XtPointer)""},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(fg_pixel), XtRString, XtDefaultForeground},
    {XtNfocusColor, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(focus_pixel), XtRString, XtDefaultForeground},
    {XtNhighlightForeground, XtCBackground, XtRPixel, sizeof(Pixel),
     offset(highlight_fg), XtRString, XtDefaultBackground},
    {XtNhighlightBackground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(highlight_bg), XtRString, XtDefaultForeground},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, XtDefaultFont},
    {XtNinternalWidth, XtCInternalWidth, XtRDimension, sizeof(Dimension),
     offset(internal_width), XtRImmediate, (XtPointer)8},
    {XtNinternalHeight, XtCInternalHeight, XtRDimension, sizeof(Dimension),
     offset(internal_height), XtRImmediate, (XtPointer)3},
    {XtNpreferredChars, XtCPreferredChars, XtRInt, sizeof(int),
     offset(pref_chars), XtRImmediate, (XtPointer)20},
    {XtNpreferredLines, XtCPreferredLines, XtRInt, sizeof(int),
     offset(pref_lines), XtRImmediate, (XtPointer)1},
    {XtNsingleLine, XtCSingleLine, XtRBoolean, sizeof(Boolean),
     offset(single_line), XtRImmediate, (XtPointer)True},
    {XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(callback), XtRCallback, (XtPointer)NULL},
    {XtNtabCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(tab_callback), XtRCallback, (XtPointer)NULL},
    {XtNfocusCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
     offset(focus_callback), XtRCallback, (XtPointer)NULL},
    {XtNborderIn, XtCBorderIn, XtRBoolean, sizeof(Boolean),
     offset(border_in), XtRImmediate, (XtPointer)True},
    {XtNdisplayCaret, XtCDisplayCaret, XtRBoolean, sizeof(Boolean),
     offset(display_caret), XtRImmediate, (XtPointer)False},
    {XtNfocusRoot, XtCFocusRoot, XtRWidget, sizeof(Widget),
     offset(focus_root), XtRImmediate, (XtPointer)NULL},
    {XtNfocusHack, XtCHack, XtRBoolean, sizeof(Boolean),
     offset(focus_hack), XtRImmediate, (XtPointer)True},
    {XtNprintFocus, XtCDebug, XtRBoolean, sizeof(Boolean),
     offset(print_focus), XtRImmediate, (XtPointer)False},
    {XtNechoOff, XtCEchoOff, XtRBoolean, sizeof(Boolean),
     offset(echo_off), XtRImmediate, (XtPointer)False},
#undef offset
};

static void	Initialize(Widget, Widget, ArgList, Cardinal*);
static void	Destroy(Widget);
static void	Redisplay(Widget, XEvent*, Region);
static void	Resize(Widget);
static void	Realize(Widget, XtValueMask*, XSetWindowAttributes*);
static void	SetHPos(ScrollableWidget, long);
static void	SetVPos(ScrollableWidget, long);
static Boolean	SetValues(Widget, Widget, Widget, ArgList, Cardinal*);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry*,
				      XtWidgetGeometry*);

static void aquire_focus(Widget, XEvent*, String*, Cardinal*);
static void nop(Widget, XEvent*, String*, Cardinal*);
static void multiply(Widget, XEvent*, String*, Cardinal*);
static void beginning_of_line(Widget, XEvent*, String*, Cardinal*);
static void end_of_line(Widget, XEvent*, String*, Cardinal*);
static void home(Widget, XEvent*, String*, Cardinal*);
static void end(Widget, XEvent*, String*, Cardinal*);
static void left(Widget, XEvent*, String*, Cardinal*);
static void right(Widget, XEvent*, String*, Cardinal*);
static void up(Widget, XEvent*, String*, Cardinal*);
static void down(Widget, XEvent*, String*, Cardinal*);
static void page(Widget, XEvent*, String*, Cardinal*);
static void delete_next(Widget, XEvent*, String*, Cardinal*);
static void delete_previous(Widget, XEvent*, String*, Cardinal*);
static void kill_action(Widget, XEvent*, String*, Cardinal*);
static void redraw(Widget, XEvent*, String*, Cardinal*);
static void enter(Widget, XEvent*, String*, Cardinal*);
static void tab(Widget, XEvent*, String*, Cardinal*);
static void transpose(Widget, XEvent*, String*, Cardinal*);
static void set_border_color(Widget, XEvent*, String*, Cardinal*);
static void insert(Widget, XEvent*, String*, Cardinal*);
static void insert_string(Widget, XEvent*, String*, Cardinal*);
static void swap_select(Widget, XEvent*, String*, Cardinal*);
static void select_start(Widget, XEvent*, String*, Cardinal*);
static void extend_start(Widget, XEvent*, String*, Cardinal*);
static void select_extend(Widget, XEvent*, String*, Cardinal*);
static void select_end(Widget, XEvent*, String*, Cardinal*);
static void kill_selection(Widget, XEvent*, String*, Cardinal*);
static void insert_selection(Widget, XEvent*, String*, Cardinal*);
static void disown_selection(Widget, XEvent*, String*, Cardinal*);
static void display_caret(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] = {
    {"aquire-focus",		aquire_focus},
    {"nop",			nop},
    {"multiply",		multiply},
    {"beginning-of-line",	beginning_of_line},
    {"end-of-line",		end_of_line},
    {"home",			home},
    {"end",			end},
    {"left",			left},
    {"right",			right},
    {"up",			up},
    {"down",			down},
    {"page",			page},
    {"delete-next",		delete_next},
    {"delete-previous",		delete_previous},
    {"kill",			kill_action},
    {"redraw",			redraw},
    {"enter",			enter},
    {"tab",			tab},
    {"transpose",		transpose},
    {"set-border-color",	set_border_color},
    {"insert",			insert},
    {"insert-string",		insert_string},
    {"swap-select",		swap_select},
    {"select-start",		select_start},
    {"extend-start",		extend_start},
    {"select-extend",		select_extend},
    {"select-end",		select_end},
    {"kill-selection",		kill_selection},
    {"insert-selection",	insert_selection},
    {"disown-selection",	disown_selection},
    {"display-caret",		display_caret},
};

static char translations[] =
"Ctrl<Key>A:		beginning-of-line() \n"
"Ctrl<Key>B:		left() \n"
"Ctrl<Key>C:		nop() \n"
"Ctrl<Key>D:		delete-next() \n"
"Ctrl<Key>E:		end-of-line() \n"
"Ctrl<Key>F:		right() \n"
"Ctrl<Key>G:		multiply(0) disown-selection() \n"
"Ctrl<Key>H:		delete-previous() \n"
"Ctrl<Key>I:		tab() \n"
"Ctrl<Key>J:		enter() \n"
"Ctrl<Key>K:		kill() \n"
"Ctrl<Key>L:		redraw() \n"
"Ctrl<Key>M:		enter() \n"
"Ctrl<Key>N:		down() \n"
"Ctrl<Key>O:		enter() left() \n"
"Ctrl<Key>P:		up() \n"
"Ctrl<Key>T:		transpose() \n"
"Ctrl<Key>U:		multiply(4) \n"
"Ctrl<Key>V:		page(+1.0) \n"
"Ctrl<Key>W:		kill-selection(PRIMARY) \n"
"Ctrl<Key>X,Ctrl<Key>X:	swap-select(PRIMARY) \n"
"Ctrl<Key>space:	select-start() \n"
"<Key>Left:		left() \n"
"<Key>Right:		right() \n"
"<Key>Up:		up() \n"
"<Key>Down:		down() \n"
"<Key>Return:		enter() \n"
"<Key>BackSpace:	delete-previous() \n"
"<Key>Delete:		delete-previous() \n"
"<Key>Tab:		tab() \n"
"<Key>Home:		home() \n"
"<Key>End:		end() \n"
"<Key>Prior:		page(-1.0) \n"
"<Key>Next:		page(+1.0) \n"
"<FocusIn>:		set-border-color(focusColor) display-caret(on) \n"
"<FocusOut>:		set-border-color(background) display-caret(off) \n"
"<Btn1Down>:		aquire-focus() select-start() \n"
"<Btn1Motion>:		select-extend() \n"
"<Btn1Up>:		select-end(PRIMARY) \n"
"<Btn2Down>:		insert-selection(PRIMARY) \n"
"<Btn3Down>:		extend-start(PRIMARY) \n"
"<Btn3Motion>:		select-extend() \n"
"<Btn3Up>:		select-end(PRIMARY) \n"
"Ctrl<Key>0:		multiply(0) \n"
"Ctrl<Key>1:		multiply(1) \n"
"Ctrl<Key>2:		multiply(2) \n"
"Ctrl<Key>3:		multiply(3) \n"
"Ctrl<Key>4:		multiply(4) \n"
"Ctrl<Key>5:		multiply(5) \n"
"Ctrl<Key>6:		multiply(6) \n"
"Ctrl<Key>7:		multiply(7) \n"
"Ctrl<Key>8:		multiply(8) \n"
"Ctrl<Key>9:		multiply(9) \n"
"<Key>:			insert() \n";

TextFieldClassRec textFieldClassRec = {
    {                                   /* core fields			*/
        (WidgetClass) &scrollableClassRec,  /* superclass		*/
        "TextField",                    /* class_name                   */
        sizeof(TextFieldRec),	        /* widget_size                  */
        NULL,                           /* class_initialize             */
        NULL,                           /* class_part_initialize        */
        FALSE,                          /* class_inited                 */
        Initialize,                     /* initialize                   */
        NULL,                           /* initialize_hook              */
        Realize,			/* realize                      */
        actions,                        /* actions                      */
        XtNumber(actions),              /* num_actions                  */
        resources,                      /* resources                    */
        XtNumber(resources),            /* num_resources                */
        NULLQUARK,                      /* xrm_class                    */
        TRUE,                           /* compress_motion              */
#if (XtSpecificationRelease < 4)
	True,				/* compress_exposure		*/
#elif (XtSpecificationRelease < 6)
        XtExposeCompressMaximal | XtExposeGraphicsExposeMerged,
					/* compress_exposure		*/
#else
        XtExposeCompressMaximal |  XtExposeGraphicsExposeMerged |
	XtExposeNoRegion,		/* compress_exposure*/
#endif
        FALSE,                          /* compress_enterleave          */
        FALSE,                          /* visible_interest             */
        Destroy,                        /* destroy                      */
        Resize,                         /* resize                       */
        Redisplay,                      /* expose                       */
        SetValues,                      /* set_values                   */
        NULL,                           /* set_values_hook              */
        XtInheritSetValuesAlmost,       /* set_values_almost            */
        NULL,                           /* get_values_hook              */
        NULL,				/* accept_focus                 */
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
	NULL,				/* alloc_arm_pixmap		*/
	XtInheritAllocGCs,		/* alloc_gcs			*/
	NULL,				/* extension			*/
    },
    {					/* scrollable fields		*/
	SetHPos,			/* set_hpos			*/
	SetVPos,			/* set_vpos			*/
	NULL,				/* suspend_hook			*/
	NULL				/* extension			*/
    },
    {                                   /* textfield fields             */
        0                               /* extension                    */
    }
};

WidgetClass textFieldWidgetClass = (WidgetClass)&textFieldClassRec;

/*************************************************************************/

static void	delete_sel_callback(Widget, XtPointer, Atom*, Atom*,
				    XtPointer, unsigned long*, int*);
static void	insert_sel_callback(Widget, XtPointer, Atom*, Atom*,
				    XtPointer, unsigned long*, int*);
static Boolean	convert_sel_proc(Widget, Atom*, Atom*, Atom*,
				 XtPointer*, unsigned long*, int*);
static void	lose_sel_proc(Widget, Atom*);

static void init_gcs(TextFieldWidget w)
{
    XGCValues	values;

    values.foreground = w->textfield.fg_pixel;
    values.background = w->core.background_pixel;
    values.font = w->textfield.font->fid;
    w->textfield.gc =
	XtGetGC((Widget)w, GCForeground | GCBackground | GCFont, &values);

    values.foreground = w->textfield.highlight_fg;
    values.background = w->textfield.highlight_bg;
    values.font = w->textfield.font->fid;
    w->textfield.h_gc =
	XtGetGC((Widget)w, GCForeground | GCBackground | GCFont, &values);
}

static void free_gcs(TextFieldWidget w)
{
    XtReleaseGC((Widget)w, w->textfield.gc);
    XtReleaseGC((Widget)w, w->textfield.h_gc);
}

static void call_callbacks(TextFieldWidget w, XtCallbackList c_list)
{
    char	*buffer;

    if (!c_list)
	return;

    buffer = TextFieldGetBuffer((Widget)w);
    XtCallCallbackList((Widget)w, c_list, (XtPointer)buffer);
    XtFree(buffer);
}

static void free_lines(TextFieldWidget w)
{
    long	i;

    for (i = 0 ; i < w->scrollable.height ; i++)
	XtFree(w->textfield.lines[i].buf);
    XtFree((char *)w->textfield.lines);
    w->textfield.lines = NULL;
    w->textfield.n_lines = 0;
    w->scrollable.height = 0;
    w->scrollable.pos_y = 0;
    w->scrollable.width = 0;
    w->scrollable.pos_x = 0;
}

static void lines_from_buffer(TextFieldWidget w)
{
    char	*c = w->textfield.buffer;

    w->textfield.buffer = NULL;
    free_lines(w);

    if (w->textfield.single_line)
	w->textfield.n_lines = 1;
    else
	w->textfield.n_lines = 16;
    w->textfield.lines = (LineBuf *)XtMalloc(w->textfield.n_lines *
					     sizeof w->textfield.lines[0]);

    if (!c)
	c = "";

    if (w->textfield.single_line) {
	char	*p = strchr(c, '\n');
	long	len = p ? p - c : strlen(c);

	w->textfield.lines[0].len = len + 1;
	w->textfield.lines[0].buf = strcpy(XtMalloc(len + 1), c);
	w->textfield.lines[0].buf[len] = '\0';
	w->scrollable.width = len + 1;
	w->scrollable.height = 1;
	if (len < w->scrollable.shown_x)
	    w->scrollable.pos_x = 0;
	else
	    w->scrollable.pos_x = len - w->scrollable.shown_x + 1;
	w->scrollable.pos_y = 0;
	w->textfield.caret_x = len;
	w->textfield.caret_y = 0;
    } else {
	long	n = 0;
	long	max = 0;

	for (;;) {
	    char	*p = strchr(c, '\n');
	    long	len = p ? p - c : strlen(c);

	    if (n + 8 > w->textfield.n_lines)
		w->textfield.lines =
		    (LineBuf *)XtRealloc((char *)w->textfield.lines,
					 (w->textfield.n_lines = 2 * n) *
					 sizeof w->textfield.lines[0]);

	    w->textfield.lines[n].buf = memcpy(XtMalloc(len + 1), c, len);
	    w->textfield.lines[n].buf[len] = '\0';
	    w->textfield.lines[n++].len = len + 1;

	    if (max < len)
		max = len;
	    if (!p)
		break;
	    c = p + 1;
	}

	w->scrollable.width = max + 1;
	w->scrollable.height = n;
	w->scrollable.pos_x = 0;
	w->scrollable.pos_y = 0;
	w->textfield.caret_x = 0;
	w->textfield.caret_y = 0;

	while (n < w->textfield.n_lines) {
	    w->textfield.lines[n].buf = NULL;
	    w->textfield.lines[n].len = 0;
	    n++;
	}
    }
}

static void open_lines(TextFieldWidget w, long at, long size)
{
    long	n = w->textfield.n_lines;

    if (w->scrollable.height + size + 8 < n) {
	w->textfield.lines =
	    (LineBuf *)XtRealloc((char *)w->textfield.lines,
				 (w->textfield.n_lines =
				  2 * (w->scrollable.height + size + 8)) *
				 sizeof w->textfield.lines[0]);
	while (n < w->textfield.n_lines) {
	    w->textfield.lines[n].buf = NULL;
	    w->textfield.lines[n].len = 0;
	    n++;
	}
    }

    n = w->scrollable.height;
    w->scrollable.height += size;
    ScrollableFitVBar((ScrollableWidget)w);
    
    if (at < n)
	memmove(w->textfield.lines + at + size,
		w->textfield.lines + at,
		(n - at) * sizeof w->textfield.lines[0]);
    n = at + size;
    while (at < n) {
	w->textfield.lines[at].buf = XtMalloc(1);
	w->textfield.lines[at].buf[0] = '\0';
	w->textfield.lines[at].len = 1;
	at++;
    }
}

static void get_char_sizes(TextFieldWidget w)
{
    XFontStruct	*font = w->textfield.font;

    w->textfield.char_w = font->max_bounds.width;
    if (w->textfield.char_w <= 0)
	w->textfield.char_w = 1;
    w->textfield.char_h = font->ascent + font->descent;

    if (w->textfield.char_w != font->min_bounds.width)
	fputs("Warning: The TextFieldWidget "
	      "only works with fixed width fonts.\n", stderr);
}

static void get_preferred_sizes(TextFieldWidget  w,
				Dimension       *width,
				Dimension       *height)
{
    if (w->textfield.pref_chars <= 0)
	w->textfield.pref_chars = 1;
    if (w->textfield.pref_lines <= 0)
	w->textfield.pref_lines = 1;

    *width =
	2 * (w->textfield.internal_width + w->shadow.shadow_width) +
	w->textfield.pref_chars * w->textfield.char_w;

    *height =
	2 * (w->textfield.internal_height + w->shadow.shadow_width) +
	w->textfield.pref_lines * w->textfield.char_h;
}

static int event_to_pos(TextFieldWidget w, XEvent *event, long *x, long *y)
{
    int	e_x, e_y;

    if (!get_event_xy(event, &e_x, &e_y))
	return False;

    e_x -= w->shadow.shadow_width + w->textfield.internal_width;
    e_y -= w->shadow.shadow_width + w->textfield.internal_height;
    *x = w->scrollable.pos_x + e_x / w->textfield.char_w;
    *y = w->scrollable.pos_y + e_y / w->textfield.char_h;
    if (*x >= w->scrollable.width)
	*x = w->scrollable.width - 1;
    if (*x < 0)
	*x = 0;
    if (*y >= w->scrollable.height)
	*y = w->scrollable.height - 1;
    if (*y < 0)
	*y = 0;

    return True;
}

static void calc_shown(TextFieldWidget w)
{
    long	tmp;

    tmp = w->core.width;
    tmp -= 2 * (w->shadow.shadow_width + w->textfield.internal_width);
    if (tmp < 0)
	tmp = 0;
    w->scrollable.shown_x = tmp / w->textfield.char_w;

    tmp = w->core.height;
    tmp -= 2 * (w->shadow.shadow_width + w->textfield.internal_height);
    if (tmp < 0)
	tmp = 0;
    w->scrollable.shown_y = tmp / w->textfield.char_h;
}

static long max_width(TextFieldWidget w)
{
    long	n, max = 0;

    for (n = 0 ; n < w->scrollable.height ; n++) {
	long	tmp = strlen(w->textfield.lines[n].buf);

	if (max < tmp)
	    max = tmp;
    }

    return max;
}

static void sync_width(TextFieldWidget w, long old_w, long new_w)
{
    long	max, width = w->scrollable.width;

    old_w++;
    new_w++;
    if (new_w < width && old_w < width)
	return;

    max = 1;
    if (old_w < width || new_w <= old_w)
	max = max_width(w) + 1;
    if (max < new_w)
	max = new_w;

    if (max == width)
	return;

    w->scrollable.width = max;
    ScrollableFitHBar((ScrollableWidget)w);
}

static void clear_segment(TextFieldWidget w, long line, long pos, long width)
{
    long	x, y;

    line -= w->scrollable.pos_y;
    if (line < 0 || line >= w->scrollable.shown_y)
	return;
    pos -= w->scrollable.pos_x;
    if (pos >= w->scrollable.shown_x)
	return;
    if (pos < 0)
	width += pos, pos = 0;
    if (width > w->scrollable.shown_x - pos)
	width = w->scrollable.shown_x - pos;
    if (pos + width < 0)
	return;

    x = w->shadow.shadow_width + w->textfield.internal_width;
    y = w->shadow.shadow_width + w->textfield.internal_height;

    x += pos * w->textfield.char_w;
    y += line * w->textfield.char_h;
    width *= w->textfield.char_w;
    XClearArea(XtDisplay(w), XtWindow(w), x, y,
	       width, w->textfield.char_h, False);
}

static void draw_stars(Display *disp, Window win, GC gc,
		       long x, long y, long len)
{
    char	buffer[256];
    char	*c = buffer;

    if (len > sizeof buffer)
	c = XtMalloc(len);
    memset(c, '*', len);
    XDrawString(disp, win, gc, x, y, c, len);
    if (c != buffer)
	XtFree(c);
}

static void draw_rectangle(TextFieldWidget w, long x0, long y0,
			   long width, long height, int clear)
{
    Display	*disp = XtDisplay(w);
    Window	win = XtWindow(w);
    GC		gc = w->textfield.gc;
    GC		h_gc = w->textfield.h_gc;
    long	x, y, i, tmp;

    tmp = x0 - w->scrollable.pos_x;
    if (tmp < 0) {
	x0 = w->scrollable.pos_x;
	width += tmp;
    }
    tmp = w->scrollable.pos_x + w->scrollable.shown_x - x0;
    if (width > tmp)
	width = tmp;
    if (width <= 0)
	return;

    tmp = y0 - w->scrollable.pos_y;
    if (tmp < 0) {
	y0 = w->scrollable.pos_y;
	height += tmp;
    }
    tmp = w->scrollable.pos_y + w->scrollable.shown_y - y0;
    if (height > tmp)
	height = tmp;
    tmp = w->scrollable.height - y0;
    if (height > tmp)
	height = tmp;

    x = w->shadow.shadow_width + w->textfield.internal_width;
    y = w->shadow.shadow_width + w->textfield.internal_height;
    x += (x0 - w->scrollable.pos_x) * w->textfield.char_w;
    y += (y0 - w->scrollable.pos_y) * w->textfield.char_h;

    if (clear) {
	long	tmp;

	if (y0 + height < w->scrollable.height)
	    tmp = height;
	else
	    tmp = w->scrollable.shown_y + w->scrollable.pos_y - y0;
	if (tmp > 0)
	    XClearArea(disp, win, x, y, width * w->textfield.char_w,
		       tmp * w->textfield.char_h, False);
    }

    y += w->textfield.font->ascent;
    for (i = y0 ; i < y0 + height ; i++, y += w->textfield.char_h) {
	char	*str = w->textfield.lines[i].buf;
	long	 len = strlen(str);

	len -= x0;
	if (len <= 0)
	    continue;
	if (len > width)
	    len = width;
	str += x0;

	if (w->textfield.echo_off)
	    draw_stars(disp, win, gc, x, y, len);
	else if (!w->textfield.sel_set ||
		 i < w->textfield.sel_start_y ||
		 i > w->textfield.sel_stop_y)
	    XDrawString(disp, win, gc, x, y, str, len);
	else if (i > w->textfield.sel_start_y &&
		 i < w->textfield.sel_stop_y)
	    XDrawImageString(disp, win, h_gc, x, y, str, len);
	else {
	    long	n, pos = x0;
	    long	x_tmp = x;

	    if (i == w->textfield.sel_start_y &&
		(n = w->textfield.sel_start_x - pos) > 0) {
		if (n > len)
		    n = len;
		XDrawString(disp, win, gc, x_tmp, y, str, n);
		x_tmp += n * w->textfield.char_w;
		str += n;
		pos += n;
		len -= n;
	    }

	    if (i == w->textfield.sel_stop_y)
		n = w->textfield.sel_stop_x - pos;
	    else
		n = len;

	    if (n > 0) {
		if (n > len)
		    n = len;
		XDrawImageString(disp, win, h_gc, x_tmp, y, str, n);
		x_tmp += n * w->textfield.char_w;
		str += n;
		pos += n;
		len -= n;
	    }

	    if (len > 0)
		XDrawString(disp, win, gc, x_tmp, y, str, len);
	}
    }
}

static void draw_pixels(TextFieldWidget w, long x0, long y0,
			long width, long height, int clear)
{
    long	tmp;

    tmp = w->shadow.shadow_width + w->textfield.internal_width;
    x0 -= tmp;
    width += x0 + w->textfield.char_w - 1;
    if (x0 < 0)
	x0 = 0;
    else
	x0 /= w->textfield.char_w;
    width /= w->textfield.char_w;
    width -= x0;
    if (width <= 0)
	return;

    tmp = w->shadow.shadow_width + w->textfield.internal_height;
    y0 -= tmp;
    height += y0 + w->textfield.char_h - 1;
    if (y0 < 0)
	y0 = 0;
    else
	y0 /= w->textfield.char_h;
    height /= w->textfield.char_h;
    height -= y0;
    if (height <= 0)
	return;

    x0 += w->scrollable.pos_x;
    y0 += w->scrollable.pos_y;
    draw_rectangle(w, x0, y0, width, height, clear);
}

static void draw_caret(TextFieldWidget w)
{
    long	x, y, tmp;

    if (!w->textfield.display_caret ||
	(w->textfield.sel_set &&
	 (w->textfield.sel_start_x != w->textfield.sel_stop_x ||
	  w->textfield.sel_start_y != w->textfield.sel_stop_y)))
	return;

    x = w->shadow.shadow_width + w->textfield.internal_width;
    y = w->shadow.shadow_width + w->textfield.internal_height;

    tmp = w->textfield.caret_x - w->scrollable.pos_x;
    if (tmp < 0 || tmp >= w->scrollable.shown_x)
	return;
    x += tmp * w->textfield.char_w;

    tmp = w->textfield.caret_y - w->scrollable.pos_y;
    if (tmp < 0 || tmp >= w->scrollable.shown_y)
	return;
    y += tmp * w->textfield.char_h;

    XDrawLine(XtDisplay(w), XtWindow(w), w->textfield.gc,
	      x, y, x, y + w->textfield.char_h - 1);
}

static void undraw_caret(TextFieldWidget w)
{
    if (!w->textfield.display_caret ||
	(w->textfield.sel_set &&
	 (w->textfield.sel_start_x != w->textfield.sel_stop_x ||
	  w->textfield.sel_start_y != w->textfield.sel_stop_y)))
	return;

    draw_rectangle(w, w->textfield.caret_x,  w->textfield.caret_y, 1, 1, True);
}

static void make_visible(TextFieldWidget w, long x, long y)
{
    if (x < w->scrollable.pos_x)
	ScrollableSetHPos((Widget)w, x);
    else if (x >= w->scrollable.pos_x + w->scrollable.shown_x)
	ScrollableSetHPos((Widget)w, x - w->scrollable.shown_x + 1);
    if (y < w->scrollable.pos_y)
	ScrollableSetVPos((Widget)w, y);
    else if (y >= w->scrollable.pos_y + w->scrollable.shown_y)
	ScrollableSetVPos((Widget)w, y - w->scrollable.shown_y + 1);
}

static void order_sel(TextFieldWidget w)
{
    long	tmp;

#define SWAP(a, b) (tmp = (a), (a) = (b), (b) = tmp)
    if (w->textfield.sel_start_y == w->textfield.sel_stop_y &&
	w->textfield.sel_start_x > w->textfield.sel_stop_x)
	SWAP(w->textfield.sel_start_x, w->textfield.sel_stop_x);
    else if (w->textfield.sel_start_y > w->textfield.sel_stop_y) {
	SWAP(w->textfield.sel_start_x, w->textfield.sel_stop_x);
	SWAP(w->textfield.sel_start_y, w->textfield.sel_stop_y);
    }
#undef SWAP
}

static void invalidate_selection(TextFieldWidget w)
{
    long	start = w->textfield.sel_start_y;
    long	stop  = w->textfield.sel_stop_y;
    long	width = w->scrollable.width;

    if (w->textfield.sel_set) {
	w->textfield.sel_set = False;
	draw_rectangle(w, 0, start, width, stop - start + 1, True);
    }
    if (w->textfield.curr_sel != None) {
        XtDisownSelection((Widget)w,
			  w->textfield.curr_sel,
			  w->textfield.sel_time);
	w->textfield.curr_sel = None;
    }
}

static void change_pos(TextFieldWidget w, long caret_x, long caret_y)
{
    long	n;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;

    if (w->scrollable.height <= 0)
	return;

    if (caret_y < 0)
	caret_y = 0;
    if (caret_y >= w->scrollable.height)
	caret_y = w->scrollable.height - 1;

    if (caret_x < 0)
	caret_x = 0;
    n = strlen(w->textfield.lines[caret_y].buf);
    if (caret_x > n)
	caret_x = n;

    if (w->textfield.sel_set) {
	long	start_x = w->textfield.sel_start_x;
	long	start_y = w->textfield.sel_start_y;
	long	stop_x  = w->textfield.sel_stop_x;
	long	stop_y  = w->textfield.sel_stop_y;
	long	o_x     = w->textfield.caret_x;
	long	o_y     = w->textfield.caret_y;
	int	extend_end;

	extend_end = (o_x == stop_x && o_y == stop_y);
	if (!extend_end && (o_x != start_x || o_y != start_y)) /* wierd... */
	    invalidate_selection(w);
	else if (caret_y == o_y) {
	    if (extend_end)
		w->textfield.sel_stop_x  = caret_x;
	    else
		w->textfield.sel_start_x = caret_x;
	    order_sel(w);

	    if (w->textfield.sel_start_x == start_x) {
		long	diff = w->textfield.sel_stop_x - stop_x;

		if (diff < 0)
		    draw_rectangle(w, w->textfield.sel_stop_x, o_y,
				   - diff, 1, True);
		else if (diff > 0)
		    draw_rectangle(w, stop_x, o_y, diff, 1, False);
	    } else if (w->textfield.sel_stop_x == stop_x) {
		long	diff = w->textfield.sel_start_x - start_x;

		if (diff > 0)
		    draw_rectangle(w, start_x, o_y, diff, 1, True);
		else if (diff < 0)
		    draw_rectangle(w, w->textfield.sel_start_x, o_y,
				   - diff, 1, False);
	    } else {
		if (start_x > w->textfield.sel_start_x)
		    start_x = w->textfield.sel_start_x;
		if (stop_x < w->textfield.sel_stop_x)
		    stop_x = w->textfield.sel_stop_x;
		draw_rectangle(w, start_x, o_y, stop_x - start_x + 1, 1, True);
	    }
	} else {
	    long	width = w->scrollable.width;

	    if (extend_end) {
		w->textfield.sel_stop_x = caret_x;
		w->textfield.sel_stop_y = caret_y;
	    } else {
		w->textfield.sel_start_x = caret_x;
		w->textfield.sel_start_y = caret_y;
	    }
	    order_sel(w);

	    if (w->textfield.sel_start_x == start_x &&
		w->textfield.sel_start_y == start_y) {
		long	diff = w->textfield.sel_stop_y - stop_y;

		if (diff < 0)
		    draw_rectangle(w, 0, w->textfield.sel_stop_y,
				   width, - diff + 1, True);
		else if (diff > 0)
		    draw_rectangle(w, 0, stop_y, width, diff + 1, False);
	    } else if (w->textfield.sel_stop_x == stop_x &&
		       w->textfield.sel_stop_y == stop_y) {
		long	diff = w->textfield.sel_start_y - start_y;

		if (diff > 0)
		    draw_rectangle(w, 0, start_y, width, diff + 1, True);
		else if (diff < 0)
		    draw_rectangle(w, 0, w->textfield.sel_start_y,
				   width, - diff + 1, False);
	    } else {
		if (start_y > w->textfield.sel_start_y)
		    start_y = w->textfield.sel_start_y;
		if (stop_y < w->textfield.sel_stop_y)
		    stop_y = w->textfield.sel_stop_y;
		draw_rectangle(w, 0, start_y, w->scrollable.width,
			       stop_y - start_y + 1, True);
	    }
	}
    }

    undraw_caret(w);
    w->textfield.caret_x = caret_x;
    w->textfield.caret_y = caret_y;
    draw_caret(w);
    make_visible(w, caret_x, caret_y);
}

static void merge_line_with_next(TextFieldWidget w, long line)
{
    char	*c;
    long	pos, len, n;

    if (line < 0 || line >= w->scrollable.height - 1)
	return;

    n = --w->scrollable.height;
    c = w->textfield.lines[line + 1].buf;
    if (line < n)
	memmove(w->textfield.lines + line + 1,
		w->textfield.lines + line + 2,
		(n - line) * sizeof w->textfield.lines[0]);
    ScrollableFitVBar((ScrollableWidget)w);
    pos = strlen(w->textfield.lines[line].buf);
    len = strlen(c);
    sync_width(w, pos, pos + len);

    if (pos + len + 8 < w->textfield.lines[line].len)
	w->textfield.lines[line].buf =
	    XtRealloc(w->textfield.lines[line].buf,
		      (w->textfield.lines[line].len = pos + len + 8));

    strcpy(w->textfield.lines[line].buf + pos, c);
    XtFree(c);
    draw_rectangle(w, pos, line, len, 1, False);
    draw_rectangle(w, 0, line + 1, w->scrollable.width,
		   w->scrollable.height, True);
    change_pos(w, pos, line);
}

/*************************************************************************/

static void aquire_focus(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    XtCallbackList	c_list = w->textfield.focus_callback;

    if (!w->textfield.focus_root || !w->textfield.active)
	return;

    XtSetKeyboardFocus(w->textfield.focus_root, (Widget)w);
    if (c_list)
	XtCallCallbackList((Widget)w, c_list, NULL);
}

static void nop(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    w->textfield.multiply = 1;
    w->textfield.waiting_for_sel = False;
}

static void multiply(Widget gw, XEvent *event,
		     String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		factor = *no_params == 1 ? atol(params[0]) : 4;
    long		new_mult = w->textfield.multiply * factor;

    if (new_mult <= 0 || new_mult > 16384)
	new_mult = 1;
    w->textfield.multiply = new_mult;
    w->textfield.waiting_for_sel = False;
}

static void beginning_of_line(Widget gw, XEvent *event,
			      String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, 0, w->textfield.caret_y);
}

static void end_of_line(Widget gw, XEvent *event,
			String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, w->scrollable.width, w->textfield.caret_y);
}

static void home(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, 0, 0);
}

static void end(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, w->scrollable.width, w->scrollable.height);
}

static void left(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, w->textfield.caret_x - w->textfield.multiply,
	       w->textfield.caret_y);
}

static void right(Widget gw, XEvent *event,
		  String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, w->textfield.caret_x + w->textfield.multiply,
	       w->textfield.caret_y);
}

static void up(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, w->textfield.caret_x,
	       w->textfield.caret_y - w->textfield.multiply);
}

static void down(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    change_pos(w, w->textfield.caret_x,
	       w->textfield.caret_y + w->textfield.multiply);
}

static void page(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    float		amount;

    if (*no_params == 1 && sscanf(params[0], "%f", &amount) == 1)
	change_pos(w, w->textfield.caret_x,
		   w->textfield.caret_y +
		   w->scrollable.shown_y * amount * w->textfield.multiply);
}

static void delete_next(Widget gw, XEvent *event,
			String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		m = w->textfield.multiply;
    long		line = w->textfield.caret_y;
    long		pos  = w->textfield.caret_x;
    char		*c;
    long		n;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);
    if (pos < 0 || line < 0 || line >= w->scrollable.height)
	return;

    c = w->textfield.lines[line].buf;
    n = strlen(c);
    if (pos >= n)
	merge_line_with_next(w, line);
    else {
	clear_segment(w, line, pos, n);
	if (m > n - pos)
	    m = n - pos;
	n -= m;
	memmove(c + pos, c + pos + m, n - pos + 1);
	draw_rectangle(w, pos, line, n - pos, 1, False);
	draw_caret(w);
	sync_width(w, n + m, n);
    }
}

static void delete_previous(Widget gw, XEvent *event,
			    String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		m = w->textfield.multiply;
    long		line = w->textfield.caret_y;
    long		pos  = w->textfield.caret_x;
    char		*c;
    long		n;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);
    if (pos < 0 || line < 0 || line >= w->scrollable.height)
	return;

    c = w->textfield.lines[line].buf;
    n = strlen(c);
    if (pos > n)
	return;

    if (pos == 0)
	merge_line_with_next(w, line - 1);
    else {
	if (m > pos)
	    m = pos;
	clear_segment(w, line, pos - m, n);
	memmove(c + pos - m, c + pos, n - pos);
	c[n - m] = '\0';
	draw_rectangle(w, pos - m, line, n - pos + 1, 1, False);
	change_pos(w, pos - m, w->textfield.caret_y);
	sync_width(w, n, n - m);
    }
}

static void kill_action(Widget gw, XEvent *event,
			String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		line = w->textfield.caret_y;
    long		pos = w->textfield.caret_x;
    char		*c;
    long		n;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);
    if (line < 0 || line >= w->scrollable.height)
	return;

    c = w->textfield.lines[line].buf;
    n = strlen(c);
    if (pos >= n)
	merge_line_with_next(w, line);
    else {
	c[pos] = '\0';
	clear_segment(w, line, pos, n - pos);
	draw_caret(w);
	sync_width(w, n, pos);
    }
}

static void redraw(Widget gw, XEvent *event,
		   String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, True);
    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
}

static void enter(Widget gw, XEvent *event,
		  String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		m = w->textfield.multiply;
    long		line = w->textfield.caret_y;
    long		pos  = w->textfield.caret_x;
    char		*c;
    long		len, n;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);

    call_callbacks(w, w->textfield.callback);
    if (w->textfield.single_line || line < 0 || line >= w->scrollable.height)
	return;

    open_lines(w, line, m);

    c = w->textfield.lines[line + m].buf;
    len = strlen(c);
    if (pos > len)
	pos = len;
    w->textfield.lines[line].len = pos + 1;
    XtFree(w->textfield.lines[line].buf);
    w->textfield.lines[line].buf = memcpy(XtMalloc(pos + 1), c, pos);
    w->textfield.lines[line].buf[pos] = '\0';

    n = len - pos;
    if (n > 0)
	memmove(c, c + pos, n);
    c[n] = '\0';
    if (n < pos)
	n = pos;

    draw_rectangle(w, 0, line, w->scrollable.width,
		   w->scrollable.height, True);
    sync_width(w, len, n);  /* Don't sync before clear... */
    change_pos(w, 0, line + m);
}

static void tab(Widget gw, XEvent *event, String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    call_callbacks(w, w->textfield.tab_callback);
}

static void transpose(Widget gw, XEvent *event,
		      String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		line = w->textfield.caret_y;
    long		pos = w->textfield.caret_x;
    long		m = w->textfield.multiply;
    char		*c;
    long		len;
    int			ch;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);
    if (line < 0 || line >= w->scrollable.height)
	return;

    c = w->textfield.lines[line].buf;
    len = strlen(c);
    if (pos <= 0 || pos >= len)
	return;

    if (pos + m >= len)
	m = len - pos;

    ch = c[pos - 1];
    memmove(c + pos - 1, c + pos, m);
    c[pos - 1 + m] = ch;
    draw_rectangle(w, pos - 1, line, m + 1, 1, True);
    change_pos(w, pos + m, line);
}

static void set_border_color(Widget gw, XEvent *event,
			     String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    Arg			arg;

    if (*no_params != 1) {
        XBell(XtDisplay(w), 0);
        return;
    }

    if (w->textfield.print_focus && (event->type == FocusIn ||
				     event->type == FocusOut)) {
	fprintf(stderr,
		"Focus%s : %s\n"
		"          serial:     %ld\n"
		"          send_event: %s\n"
		"          window:     0x%lx\n"
		"          mode:       ",
		event->type == FocusIn ? "In " : "Out",
		XtName((Widget)w),
		event->xfocus.serial,
                event->xfocus.send_event ? "True" : "False",
		event->xfocus.window);
        switch (event->xfocus.mode) {
        case NotifyNormal:
            fputs("NotifyNormal\n", stderr);
            break;
        case NotifyGrab:
            fputs("NotifyGrab\n", stderr);
            break;
        case NotifyUngrab:
            fputs("NotifyUngrab\n", stderr);
            break;
        default:
            fputc('\n', stderr);
            break;
        }
        fputs("          detail:     ", stderr);
        switch (event->xfocus.detail) {
        case NotifyAncestor:
            fputs("NotifyAncestor\n", stderr);
            break;
        case NotifyVirtual:
            fputs("NotifyVirtual\n", stderr);
            break;
        case NotifyInferior:
            fputs("NotifyInferior\n", stderr);
            break;
        case NotifyNonlinear:
            fputs("NotifyNonlinear\n", stderr);
            break;
        case NotifyNonlinearVirtual:
            fputs("NotifyNonlinearVirtual\n", stderr);
            break;
        case NotifyPointer:
            fputs("NotifyPointer\n", stderr);
            break;
        case NotifyPointerRoot:
            fputs("NotifyPointerRoot\n", stderr);
            break;
        case NotifyDetailNone:
            fputs("NotifyDetailNone\n", stderr);
            break;
        default:
            fputc('\n', stderr);
            break;
        }
    }

    if (w->textfield.focus_hack &&
	event->type == FocusIn &&
	!event->xfocus.send_event)
        return;

    if (strlen(params[0]) < 15) {
        char	buffer[16];
        char	*c;
        long	i;

        c = params[0];
        i = 0;
        do {
            buffer[i++] =
                isupper((unsigned char)*c) ? tolower((unsigned char)*c) : *c;
        } while (*c++ != '\0');

        if (strcmp(buffer, XtNbackground) == 0) {
            XtSetArg(arg, XtNborderColor, w->core.background_pixel);
            XtSetValues((Widget)w, &arg, 1);
            return;
        } else if (strcmp(buffer, XtNforeground) == 0) {
            XtSetArg(arg, XtNborderColor, w->textfield.fg_pixel);
            XtSetValues((Widget)w, &arg, 1);
            return;
        } else if (strcmp(buffer, "focuscolor") == 0) {
            XtSetArg(arg, XtNborderColor, w->textfield.focus_pixel);
            XtSetValues((Widget)w, &arg, 1);
            return;
        }
    }

    XBell(XtDisplay(w), 0);
}

static void insert(Widget gw, XEvent *event,
		   String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    char		buffer[16];
    String		param;
    Cardinal		n_params;
    KeySym		keysym;
    long		len;

    if (!event || (event->type != KeyPress && event->type != KeyRelease))
	return;

    len = XLookupString(&event->xkey, buffer, sizeof buffer, &keysym, NULL);
    if (len <= 0)
	return;
    buffer[len] = '\0';
    param = buffer;
    n_params = 1;

    insert_string((Widget)w, event, &param, &n_params);
}

static void insert_string(Widget gw, XEvent *event,
			  String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    char		*c;
    long		m = w->textfield.multiply;
    long		line = w->textfield.caret_y;
    long		pos = w->textfield.caret_x;
    long		n_pos, n, len;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);

    if (*no_params != 1 || line < 0 || line >= w->scrollable.height)
	return;

    n = strlen(params[0]);
    if (n <= 0 || XTextWidth(w->textfield.font, params[0], n) <= 0)
	return;

    len = strlen(w->textfield.lines[line].buf);
    if (pos > len)
	pos = len;
    n_pos = pos + m * n;
    if (len + m * n + 8 > w->textfield.lines[line].len)
	w->textfield.lines[line].buf =
	    XtRealloc(w->textfield.lines[line].buf,
		      (w->textfield.lines[line].len = len + m * n + 8));
    sync_width(w, len, len + m * n);
    c = w->textfield.lines[line].buf + pos;
    memmove(c + m * n, c, len - pos + 1);
    while (m-- > 0) {
	memcpy(c, params[0], n);
	c += n;
    }
    draw_rectangle(w, pos, line, w->scrollable.width, 1, True);
    change_pos(w, n_pos, w->textfield.caret_y);
}

static void swap_select(Widget gw, XEvent *event,
			String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    Atom		atom = XA_PRIMARY;
    Time		time = get_event_time(event);
    long		o_caret_x = w->textfield.caret_x;
    long		o_caret_y = w->textfield.caret_y;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);

    change_pos(w, w->textfield.mark_x, w->textfield.mark_y);
    w->textfield.sel_start_x = w->textfield.caret_x;
    w->textfield.sel_start_y = w->textfield.caret_y;
    w->textfield.mark_x = w->textfield.sel_stop_x = o_caret_x;
    w->textfield.mark_y = w->textfield.sel_stop_y = o_caret_y;
    order_sel(w);

    if (*no_params > 0 && strcmp(params[0], "PRIMARY") != 0)
	atom = intern_atom(XtDisplay(w), params[0]);

    if (XtOwnSelection((Widget)w, atom, time,
		       convert_sel_proc, lose_sel_proc, NULL)) {
	long	y, height;

	w->textfield.curr_sel = atom;
	w->textfield.sel_time = time;
	w->textfield.sel_set = True;
	height = w->textfield.sel_stop_y - w->textfield.sel_start_y + 1;
	y = w->textfield.sel_start_y;
	undraw_caret(w);
	draw_rectangle(w, 0, y, w->scrollable.width, height, False);
    }
}

static void select_start(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		x, y;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);

    if (event->type == ButtonPress || event->type == ButtonRelease) {
	if (!event_to_pos(w, event, &x, &y))
	    return;
	change_pos(w, x, y);
    }

    w->textfield.sel_start_x = w->textfield.sel_stop_x =
	w->textfield.mark_x = w->textfield.caret_x;
    w->textfield.sel_start_y = w->textfield.sel_stop_y =
	w->textfield.mark_y = w->textfield.caret_y;
    w->textfield.sel_set = True;
}

static void extend_start(Widget gw, XEvent *event,
			 String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		x, y, n;
    int			extend_end;

    if (w->scrollable.height <= 0)
	return;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;

    if (!w->textfield.sel_set) {
	select_start((Widget)w, event, params, no_params);
	return;
    }

    if (!event_to_pos(w, event, &x, &y))
	return;

    if (w->textfield.sel_start_y == w->textfield.sel_stop_y)
	if (y < w->textfield.sel_start_y)
	    extend_end = False;
	else if (y > w->textfield.sel_start_y)
	    extend_end = True;
	else if (2 * x < w->textfield.sel_start_x + w->textfield.sel_stop_x)
	    extend_end = False;
	else
	    extend_end = True;
    else if (y <= w->textfield.sel_start_y)
	extend_end = False;
    else if (y >= w->textfield.sel_stop_y)
	extend_end = True;
    else if (2 * y < w->textfield.sel_start_y + w->textfield.sel_stop_y)
	extend_end = False;
    else
	extend_end = True;

    if (y >= w->scrollable.height)
	y = w->scrollable.height - 1;
    if (y < 0)
	y = 0;
    n = strlen(w->textfield.lines[y].buf);
    if (x > n)
	x = n;
    if (x < 0)
	x = 0;

    invalidate_selection(w);
    undraw_caret(w);
    if (extend_end) {
	w->textfield.sel_stop_x = x;
	w->textfield.sel_stop_y = y;
    } else {
	w->textfield.sel_start_x = x;
	w->textfield.sel_start_y = y;
    }
    w->textfield.sel_set = True;
    w->textfield.caret_x = x;
    w->textfield.caret_y = y;
    y = w->textfield.sel_start_y;
    n = w->textfield.sel_stop_y - y + 1;
    draw_rectangle(w, 0, y, w->scrollable.width, n, True);
    draw_caret(w);
}

static void select_extend(Widget gw, XEvent *event,
			  String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    long		x, y;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (!w->textfield.sel_set)
	return;

    if (!event_to_pos(w, event, &x, &y))
	return;

    change_pos(w, x, y);
}

static void select_end(Widget gw, XEvent *event,
		       String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    Atom		atom = XA_PRIMARY;
    Time		time = get_event_time(event);

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (!w->textfield.sel_set)
	return;

    if (w->textfield.curr_sel != None)
	XtDisownSelection((Widget)w, w->textfield.curr_sel,
			  w->textfield.sel_time);
    w->textfield.curr_sel = None;

    if (w->textfield.sel_start_y == w->textfield.sel_stop_y &&
	w->textfield.sel_start_x == w->textfield.sel_stop_x)
	return;

    if (*no_params > 0 && strcmp(params[0], "PRIMARY") != 0)
	atom = intern_atom(XtDisplay(w), params[0]);

    if (!XtOwnSelection((Widget)w, atom, time,
			convert_sel_proc, lose_sel_proc, NULL)) {
	invalidate_selection(w);
	return;
    }

    w->textfield.curr_sel = atom;
    w->textfield.sel_time = time;
}

static void kill_selection(Widget gw, XEvent *event,
			   String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    Display		*disp = XtDisplay(w);
    Atom		atom = XA_PRIMARY;
    Time		time = get_event_time(event);

    w->textfield.multiply = 1;

    if (*no_params > 0 && strcmp(params[0], "PRIMARY") != 0)
	atom = intern_atom(disp, params[0]);

    w->textfield.waiting_for_sel = True;
    XtGetSelectionValue((Widget)w, atom, intern_atom(disp, "DELETE"),
			delete_sel_callback, NULL, time);
}

static void insert_selection(Widget gw, XEvent *event,
			     String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    Display		*disp = XtDisplay(w);
    Atom		atom = XA_PRIMARY;
    Time		time = get_event_time(event);
    long		x, y;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;

    if (!event_to_pos(w, event, &x, &y))
	return;

    change_pos(w, x, y);
    w->textfield.waiting_for_sel = True;
    if (*no_params > 0 && strcmp(params[0], "PRIMARY") != 0)
	atom = intern_atom(disp, params[0]);
    XtGetSelectionValue((Widget)w, atom, XA_STRING,
			insert_sel_callback, NULL, time);
}

static void disown_selection(Widget gw, XEvent *event,
			     String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;
    if (w->textfield.sel_set)
	invalidate_selection(w);
    draw_rectangle(w, w->scrollable.pos_x, w->scrollable.pos_y,
		   w->scrollable.shown_x, w->scrollable.shown_y, True);
    draw_caret(w);
}

static void display_caret(Widget gw, XEvent *event,
			  String *params, Cardinal *no_params)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    w->textfield.waiting_for_sel = False;
    w->textfield.multiply = 1;

    if (w->textfield.focus_hack &&
        event->type == FocusIn &&
        !event->xfocus.send_event)
        return;

    if (*no_params == 1 && strlen(params[0]) < 15) {
        char	buffer[16];
        char	*c;
        long	i;

        c = params[0];
        i = 0;
        do {
            buffer[i++] =
                isupper((unsigned char)*c) ? tolower((unsigned char)*c) : *c;
        } while (*c++ != '\0');

        if (strcmp(buffer, "on") == 0 ||
	    strcmp(buffer, "true") == 0) {
            w->textfield.display_caret = True;
            draw_caret(w);
            return;
        } else if (strcmp(buffer, "off") == 0 ||
                   strcmp(buffer, "false") == 0) {
            undraw_caret(w);
            w->textfield.display_caret = False;
            return;
        }
    }

    XBell(XtDisplay(w), 0);
}

/*************************************************************************/

static void Initialize(Widget grequest, Widget gnew,
		       ArgList args, Cardinal *no_args)
{
    TextFieldWidget	new = (TextFieldWidget)gnew;
    Dimension		width, height;

    init_gcs(new);

    get_char_sizes(new);
    get_preferred_sizes(new, &width, &height);
    if (new->core.width == 0)
	new->core.width  = width;
    if (new->core.height == 0)
	new->core.height = height;
    calc_shown(new);

    new->textfield.waiting_for_sel = False;
    new->textfield.active          = True;
    new->textfield.sel_set         = False;
    new->textfield.curr_sel        = None;
    new->textfield.sel_time        = 0;
    new->textfield.sel_start_x     = 0;
    new->textfield.sel_start_y     = 0;
    new->textfield.sel_stop_x      = 0;
    new->textfield.sel_stop_y      = 0;
    new->textfield.multiply        = 1;
    new->textfield.caret_x         = 0;
    new->textfield.caret_y         = 0;
    new->textfield.mark_x          = 0;
    new->textfield.mark_y          = 0;

    new->textfield.lines = NULL;
    new->textfield.n_lines = 0;
    lines_from_buffer(new);

    if (new->textfield.single_line)
	new->textfield.pref_lines = 1;

    new->core.border_pixel = new->core.background_pixel;
}

static void Destroy(Widget gw)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    if (w->textfield.curr_sel != None)
	XtDisownSelection((Widget)w, w->textfield.curr_sel,
			  w->textfield.sel_time);
    w->textfield.curr_sel = None;
    free_lines(w);
    free_gcs(w);
}

static void Resize(Widget gw)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    calc_shown(w);
    if (!XtIsRealized((Widget)w))
	return;
    make_visible(w, w->textfield.caret_x, w->textfield.caret_y);
    ScrollableFitHBar((ScrollableWidget)w);
    ScrollableFitVBar((ScrollableWidget)w);
}

static void SetHPos(ScrollableWidget gw, long pos_x)
{
    TextFieldWidget	w       = (TextFieldWidget)gw;
    Display		*disp   = XtDisplay(w);
    Window		win     = XtWindow(w);
    long		old     = w->scrollable.pos_x;
    long		pos_y   = w->scrollable.pos_y;
    long		shown_x = w->scrollable.shown_x;
    long		shown_y = w->scrollable.shown_y;
    long		diff;

    if (pos_x > w->scrollable.width - shown_x)
	pos_x = w->scrollable.width - shown_x;
    if (pos_x < 0)
	pos_x = 0;

    diff = pos_x - old;
    if (diff == 0)
	return;

    undraw_caret(w);
    w->scrollable.pos_x = pos_x;
    if (diff <= - shown_x || diff >= shown_x)
	draw_rectangle(w, pos_x, pos_y, shown_x, shown_y, True);
    else {
	long	y0 = w->shadow.shadow_width + w->textfield.internal_height;
	long	x0 = w->shadow.shadow_width + w->textfield.internal_width;
	long	height = (long)w->core.height - 2 * y0;
	long	width, xdiff;
	GC	gc = w->textfield.gc;

	if (height <= 0)
	    return;

	if (diff < 0) {
	    diff = -diff;
	    width = (shown_x - diff) * w->textfield.char_w;
	    xdiff = diff * w->textfield.char_w;
	    XCopyArea(disp, win, win, gc,
		      x0, y0, width, height, x0 + xdiff, y0);
	    XClearArea(disp, win, x0, y0, xdiff, height, False);
	    draw_rectangle(w, pos_x, pos_y, diff, shown_y, False);
	} else {
	    width = (shown_x - diff) * w->textfield.char_w;
	    xdiff = diff * w->textfield.char_w;
	    XCopyArea(disp, win, win, gc,
		      x0 + xdiff, y0, width, height, x0, y0);
	    XClearArea(disp, win, x0 + width, y0, xdiff, height, False);
	    draw_rectangle(w, pos_x + shown_x - diff, pos_y,
			   diff, shown_y, False);
	}
    }
    draw_caret(w);
}

static void SetVPos(ScrollableWidget gw, long pos_y)
{
    TextFieldWidget	w       = (TextFieldWidget)gw;
    Display		*disp   = XtDisplay(w);
    Window		win     = XtWindow(w);
    long		old     = w->scrollable.pos_y;
    long		pos_x   = w->scrollable.pos_x;
    long		shown_x = w->scrollable.shown_x;
    long		shown_y = w->scrollable.shown_y;
    long		diff;

    if (pos_y > w->scrollable.height - shown_y)
	pos_y = w->scrollable.height - shown_y;
    if (pos_y < 0)
	pos_y = 0;

    diff = pos_y - old;
    if (diff == 0)
	return;

    undraw_caret(w);
    w->scrollable.pos_y = pos_y;
    if (diff <= - shown_y || diff >= shown_y)
	draw_rectangle(w, pos_x, pos_y, shown_x, shown_y, True);
    else {
	long	y0 = w->shadow.shadow_width + w->textfield.internal_height;
	long	x0 = w->shadow.shadow_width + w->textfield.internal_width;
	long	width = (long)w->core.width - 2 * x0;
	long	height, ydiff;
	GC	gc = w->textfield.gc;

	if (width <= 0)
	    return;

	if (diff < 0) {
	    diff = -diff;
	    height = (shown_y - diff) * w->textfield.char_h;
	    ydiff = diff * w->textfield.char_h;
	    XCopyArea(disp, win, win, gc,
		      x0, y0, width, height, x0, y0 + ydiff);
	    XClearArea(disp, win, x0, y0, width, ydiff, False);
	    draw_rectangle(w, pos_x, pos_y, shown_x, diff, False);
	} else {
	    height = (shown_y - diff) * w->textfield.char_h;
	    ydiff = diff * w->textfield.char_h;
	    XCopyArea(disp, win, win, gc,
		      x0, y0 + ydiff, width, height, x0, y0);
	    XClearArea(disp, win, x0, y0 + height, width, ydiff, False);
	    draw_rectangle(w, pos_x, pos_y + shown_y - diff,
			   shown_x, diff, False);
	}
    }
    draw_caret(w);
}

static void Realize(Widget gw, XtValueMask *mask,
		    XSetWindowAttributes *attributes)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    calc_shown(w);
    if (w->textfield.single_line) {
	w->scrollable.pos_x = w->scrollable.width - w->scrollable.shown_x;
	if (w->scrollable.pos_x < 0)
	    w->scrollable.pos_x = 0;
    }
    scrollableWidgetClass->core_class.realize((Widget)w, mask, attributes);
}

static void Redisplay(Widget gw, XEvent *event, Region region)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    int			x, y, width, height;

    if (event)
	switch (event->type) {
	case Expose:
	    x = event->xexpose.x;
	    y = event->xexpose.y;
	    width = event->xexpose.width;
	    height = event->xexpose.height;
	    break;
	case GraphicsExpose:
	    x = event->xgraphicsexpose.x;
	    y = event->xgraphicsexpose.y;
	    width = event->xgraphicsexpose.width;
	    height = event->xgraphicsexpose.height;
	    break;
	default:
	    return;
	}
    else {
	x = 0;
	y = 0;
	width = w->core.width;
	height = w->core.height;
    }

    ShadowDrawShadows((ShadowWidget)w, 0, 0, w->core.width,
		      w->core.height, w->textfield.border_in);
    draw_pixels(w, x, y, width, height, False);
    draw_caret(w);
}

static Boolean SetValues(Widget gcurrent,
			 Widget grequest,
			 Widget gnew,
			 ArgList args,
			 Cardinal *num_args)
{
    TextFieldWidget	new = (TextFieldWidget)gnew;
    TextFieldWidget	current = (TextFieldWidget)gcurrent;
    Boolean		redisplay = False;

    if (new->textfield.single_line)
	new->textfield.pref_lines = 1;

    if (new->textfield.font           != current->textfield.font         ||
	new->core.background_pixel    != current->core.background_pixel  ||
	new->textfield.fg_pixel       != current->textfield.fg_pixel     ||
	new->textfield.highlight_fg   != current->textfield.highlight_fg ||
	new->textfield.highlight_bg   != current->textfield.highlight_bg) {
	free_gcs(new);
	init_gcs(new);
	redisplay = True;
    }

    if (new->textfield.buffer) {
	if (new->textfield.sel_set)
	    invalidate_selection(new);
	lines_from_buffer(new);
	new->textfield.curr_sel = None;
	new->textfield.waiting_for_sel = False;
	ScrollableFitHBar((ScrollableWidget)new);
	ScrollableFitVBar((ScrollableWidget)new);
	redisplay = True;
    }

    if (new->textfield.font != current->textfield.font)
	get_char_sizes(new);

    if (new->textfield.font            != current->textfield.font           ||
	new->textfield.internal_width  != current->textfield.internal_width ||
	new->textfield.internal_height != current->textfield.internal_height) {
	Dimension	width, height;

	get_preferred_sizes(new, &width, &height);
	new->core.width = width;
	new->core.height = height;
	redisplay = True;
    }

    return redisplay;
}

static XtGeometryResult QueryGeometry(Widget gw,
				      XtWidgetGeometry *intended,
				      XtWidgetGeometry *preferred)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    Dimension		width, height;
    Dimension		intended_width, intended_height;

    get_preferred_sizes(w, &width, &height);
    preferred->request_mode = CWWidth | CWHeight;
    preferred->width = width;
    preferred->height = height;

    if (intended->request_mode & CWWidth)
	intended_width = intended->width;
    else
	intended_width = w->core.width;
    if (intended->request_mode & CWHeight)
	intended_height = intended->height;
    else
	intended_height = w->core.height;

    if (width == w->core.width && height == w->core.height)
	return XtGeometryNo;
    if (width == intended_width && height == intended_height)
	return XtGeometryYes;
    return XtGeometryAlmost;
}

/*************************************************************************/

void TextFieldSetActive(Widget gw, int active)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    w->textfield.active = active;
}

void TextFieldSetBuffer(Widget gw, char *buffer)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    if (w->textfield.sel_set)
	invalidate_selection(w);
    w->textfield.buffer = buffer;
    lines_from_buffer(w);
    ScrollableFitHBar((ScrollableWidget)w);
    ScrollableFitVBar((ScrollableWidget)w);
    XClearWindow(XtDisplay(w), XtWindow(w));
    Redisplay((Widget)w, NULL, NULL);
}

char *TextFieldGetBuffer(Widget gw)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    char		*buffer;
    long		i, n;

    if (w->scrollable.height <= 0)
	return XtNewString("");

    n = w->scrollable.height + 3;
    for (i = 0 ; i < w->scrollable.height ; i++)
	n += strlen(w->textfield.lines[i].buf);
    buffer = XtMalloc(n);

    strcpy(buffer, w->textfield.lines[0].buf);
    n = strlen(buffer);
    for (i = 1 ; i < w->scrollable.height ; i++) {
	buffer[n++] = '\n';
	strcpy(buffer + n, w->textfield.lines[i].buf);
	n += strlen(buffer + n);
    }
    buffer[n] = '\0';

    return buffer;
}

/*************************************************************************/

static long get_sel_len(TextFieldWidget w)
{
    long	start = w->textfield.sel_start_y;
    long	stop  = w->textfield.sel_stop_y;
    long	len, n;
    
    if (w->textfield.curr_sel == None)
	return 0;

    if (start < 0 || start >= w->scrollable.height ||
	stop < 0 || stop >= w->scrollable.height)
	return -1;

    if (start == stop)
	return w->textfield.sel_stop_x - w->textfield.sel_start_x;

    len = strlen(w->textfield.lines[start].buf) - w->textfield.sel_start_x + 1;
    for (n = start + 1 ; n < stop ; n++)
	len += strlen(w->textfield.lines[n].buf) + 1;
    len += w->textfield.sel_stop_x;

    return len;
}

static long get_sel(TextFieldWidget w, char *buffer)
{
    long	start = w->textfield.sel_start_y;
    long	stop  = w->textfield.sel_stop_y;
    long	n, len;
    char	*c;

    if (w->textfield.curr_sel == None || start < 0 || stop < 0 ||
	start >= w->scrollable.height || stop >= w->scrollable.height)
	return -1;

    c = w->textfield.lines[start].buf;
    n = strlen(c);

    if (start == stop) {
	start = w->textfield.sel_start_x;
	stop = w->textfield.sel_stop_x;
	if (start > n)
	    return -1;
	if (stop > n)
	    stop = n;
	len = stop - start;
	if (len > 0)
	    memcpy(buffer, c + start, len);
	buffer[len] = '\0';
	return len;
    }

    if (w->textfield.sel_start_x > n)
	len = 0;
    else {
	strcpy(buffer, c + w->textfield.sel_start_x);
	len = strlen(buffer);
    }
    buffer[len++] = '\n';

    for (n = start + 1 ; n < stop ; n++) {
	strcpy(buffer + len, w->textfield.lines[n].buf);
	len += strlen(buffer + len);
	buffer[len++] = '\n';
    }

    c = w->textfield.lines[stop].buf;
    n = strlen(c);
    stop = w->textfield.sel_stop_x;
    if (stop > n)
	stop = n;
    if (stop > 0)
	memcpy(buffer + len, c, stop);
    len += stop;
    buffer[len] = '\0';

    return len;
}

/*************************************************************************/

static void delete_sel_callback(Widget		 gw,
				XtPointer	 client_data,
				Atom		*selection,
				Atom		*type,
				XtPointer	 value,
				unsigned long	*len,
				int		*format)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    w->textfield.waiting_for_sel = False;
    XtFree((char *)value);
}

static void insert_sel_callback(Widget		 gw,
				XtPointer	 client_data,
				Atom		*selection,
				Atom		*type,
				XtPointer	 val,
				unsigned long	*lenp,
				int		*format)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    char		*value = (char *)val;
    char		*c, *p;
    long		pos = w->textfield.caret_x;
    long		line = w->textfield.caret_y;
    long		len = *lenp;
    long		m, n, i;
    int			was_waiting = w->textfield.waiting_for_sel;

    if (w->textfield.sel_set)
	invalidate_selection(w);

    c = memchr(value, '\n', len);
    w->textfield.waiting_for_sel = False;
    if (!was_waiting || *type == XT_CONVERT_FAIL || !value ||
	(c && w->textfield.single_line) ||
	pos < 0 || line < 0 || line >= w->scrollable.height) {
	XtFree(value);
	return;
    }

    m = 0;
    p = c;
    while (p) {
	m++;
	p++;
	p = memchr(p, '\n', value + len - p);
    }

    if (m == 0) {
	n = strlen(w->textfield.lines[line].buf);
	if (n + len + 8 > w->textfield.lines[line].len)
	    w->textfield.lines[line].buf =
		XtRealloc(w->textfield.lines[line].buf,
			  (w->textfield.lines[line].len = n + len + 8));
	if (pos > n)
	    pos = n;
	p = w->textfield.lines[line].buf;
	memmove(p + pos + len, p + pos, n - pos + 1);
	memcpy(p + pos, value, len);
	sync_width(w, n, n + len);
	draw_rectangle(w, pos, line, w->scrollable.width, 1, True);
	change_pos(w, pos + len, line);
    } else {
	n = strlen(w->textfield.lines[line].buf);
	if (pos > n)
	    pos = n;

	open_lines(w, line, m);

	n = c - value;
	XtFree(w->textfield.lines[line].buf);
	p = w->textfield.lines[line].buf =
	    XtMalloc(w->textfield.lines[line].len = pos + n + 1);
	memcpy(p, w->textfield.lines[line + m].buf, pos);
	memcpy(p + pos, value, n);
	p[pos + n] = '\0';

	c++;
	for (i = line + 1 ; i < line + m ; i++) {
	    p = memchr(c, '\n', value + len - c);
	    /* p != NULL */
	    n = p - c;
	    XtFree(w->textfield.lines[i].buf);
	    w->textfield.lines[i].buf =
		memcpy(XtMalloc(w->textfield.lines[i].len = n + 1), c, n);
	    w->textfield.lines[i].buf[n] = '\0';
	    c = p + 1;
	}

	len = value + len - c;
	n = strlen(w->textfield.lines[line + m].buf) - pos;
	p = XtMalloc(w->textfield.lines[line + m].len = len + n + 1);
	memcpy(p, c, len);
	memcpy(p + len, w->textfield.lines[line + m].buf + pos, n);
	p[n + len] = '\0';
	XtFree(w->textfield.lines[line + m].buf);
	w->textfield.lines[line + m].buf = p;

	draw_rectangle(w, 0, line, w->scrollable.width,
		       w->scrollable.height, True);

	w->scrollable.width = max_width(w);
	ScrollableFitHBar((ScrollableWidget)w);
	change_pos(w, pos, line + m);
    }

    XtFree(value);
}

static int delete_selection(TextFieldWidget w)
{
    long	start = w->textfield.sel_start_y;
    long	stop  = w->textfield.sel_stop_y;
    long	pos, len, n;
    char	*c;

    if (!w->textfield.sel_set || w->textfield.curr_sel == None ||
	(unsigned long)start >= w->scrollable.height ||
	(unsigned long)stop >= w->scrollable.height)
	return False;

    invalidate_selection(w);

    if (start == stop) {
	pos = w->textfield.sel_start_x;
	c = w->textfield.lines[start].buf;
	n = strlen(c);
	if (pos > n)
	    pos = n;
	len = w->textfield.sel_stop_x - pos;
	if (len > n - pos)
	    len = n - pos;
	memmove(c + pos, c + pos + len, n - pos - len + 1);
	draw_rectangle(w, pos, start, w->scrollable.width, 1, True);
	sync_width(w, n, n - len);
	change_pos(w, pos, start);
    } else {
	/*
	 * FIXME... (knews doesn't need it)
	 */
	fputs("TextField: Multiline DELETE not yet implemented.\n", stderr);
	return False;
    }

    return True;
}

static Boolean convert_sel_proc(Widget		 gw,
				Atom		*sel,
				Atom		*target,
				Atom		*type,
				XtPointer	*value,
				unsigned long	*len,
				int		*format)
{
    TextFieldWidget	w = (TextFieldWidget)gw;
    Display		*disp = XtDisplay(w);

    if (w->textfield.curr_sel == None || *sel != w->textfield.curr_sel)
	return False;

    if (*target == XA_STRING || *target == intern_atom(disp, "TEXT")) {
	long	length;
	char	*buffer;

	length = get_sel_len(w);
	if (length < 0)
	    return False;
	buffer = XtMalloc(length + 16);
	length = get_sel(w, buffer);
	if (length < 0) {
	    XtFree(buffer);
	    return False;
	}
	buffer[length] = '\0';

	*len = length;
	*value = (XtPointer)buffer;
	*type = XA_STRING;
	*format = 8;

	return True;
    }

    if (*target == intern_atom(disp, "DELETE")) {
	if (!delete_selection(w))
	    return False;

	*value = NULL;
	*type = intern_atom(disp, "NULL");
	*len = 0;
	*format = 32;

	return True;
    }

    if (*target == intern_atom(disp, "LENGTH")) {
	long	*length = (long *)XtMalloc(sizeof(long));

	*length = get_sel_len(w);
	*value = (XPointer)length;
	*type = XA_INTEGER;
	*len = 1;
	*format = 32;

	return True;
    }

    if (*target == intern_atom(disp, "TARGETS")) {
	Atom		*std_targets, *atom;
	unsigned long	std_length, n;

	if (!cvt_std_sel((Widget)w, w->textfield.sel_time,
			 sel, target, type,
			 (XPointer *)&std_targets, &std_length, format))
	    return False;

	*value = (XtPointer)XtMalloc((std_length + 8) * sizeof(Atom));
	atom = (Atom *)*value;
	n = std_length;

	n++; *atom++ = XA_STRING;
	n++; *atom++ = intern_atom(disp, "TEXT");
	n++; *atom++ = intern_atom(disp, "LENGTH");
	n++; *atom++ = intern_atom(disp, "LIST_LENGTH");
	n++; *atom++ = intern_atom(disp, "DELETE");

	memcpy(atom, std_targets, std_length * sizeof(Atom));
	XtFree((char *)std_targets);

	*len = n;
	*type = XA_ATOM;
	*format = 32;

	return True;
    }

    if (*target == intern_atom(disp, "LIST_LENGTH")) {
	long	*length = (long *)XtMalloc(sizeof(long));

	*length = 1;
	*value = (XPointer)length;
	*type = XA_INTEGER;
	*format = 32;

	return True;
    }

    return cvt_std_sel((Widget)w, w->textfield.sel_time,
		       sel, target, type, (XPointer *)value, len, format);
}

static void lose_sel_proc(Widget gw, Atom *sel)
{
    TextFieldWidget	w = (TextFieldWidget)gw;

    w->textfield.curr_sel = None;
    if (w->textfield.sel_set)
	invalidate_selection(w);
}
