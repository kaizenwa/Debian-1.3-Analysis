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

/*

Copyright (c) 1989  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/ShellP.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#include "Compat.h"
#include "Util.h"
#include "UtilI.h"

/*
 * This function is modified from the source to XTextWidth,
 * which is why I included the X Consortium copyright notice.
 */
int MyXWidthToChars(XFontStruct *font, const char *str, int len, int width)
{
    XCharStruct	*def;
    int		singlerow = (font->max_byte1 == 0);
    int		n;

    if (!str || width <= 0)
	return 0;

    if (singlerow)
	CI_GET_DEFAULT_INFO_1D(font, def);
    else
	CI_GET_DEFAULT_INFO_2D(font, def);

    for (n = 0 ; n < len ; n++) {
	XCharStruct	*cs;
	int		ch = (unsigned char)str[n];

	if (singlerow)
	    CI_GET_CHAR_INFO_1D(font, ch, def, cs);
	else
	    /*
	     * essentially the macro CI_GET_ROWZERO_CHAR_INFO_2D
	     */
	    if (font->min_byte1 == 0 &&
		ch >= font->min_char_or_byte2 && ch <= font->max_char_or_byte2)
		if (!font->per_char)
		    cs = &font->min_bounds;
		else {
		    cs = font->per_char + ch - font->min_char_or_byte2;
		    if (CI_NONEXISTCHAR(cs))
			cs = def;
		}
	    else
		cs = def;

	if (cs)
	    width -= cs->width;
	if (width < 0)
	    break;
    }	

    return n;
}

int MyXWidthToWChars(XFontStruct *font, const XChar2b *str, int len, int width)
{
    XCharStruct	*def;
    int		singlerow = (font->max_byte1 == 0);
    int		n;

    if (!font || width < 0)
	return 0;

    if (singlerow)
	CI_GET_DEFAULT_INFO_1D(font, def);
    else
	CI_GET_DEFAULT_INFO_2D(font, def);

    for (n = 0 ; n < len ; n++) {
	XCharStruct	*cs;

	if (singlerow) {
	    int	ch = (str[n].byte1 << 8) | str[n].byte2;

	    CI_GET_CHAR_INFO_1D(font, ch, def, cs);
	} else {
	    int	row = str[n].byte1;
	    int	col = str[n].byte2;

	    CI_GET_CHAR_INFO_2D(font, row, col, def, cs);
	}

	if (cs)
	    width -= cs->width;
	if (width < 0)
	    break;
    }

    return n;
}

/*************************************************************************/

typedef struct  {
    XtCallbackProc	proc;
    XtPointer		client_data;
} CALLBACK_ENTRY;

static void WM_DELETE_WINDOW_handler(Widget     w,
				     XtPointer  client_data,
				     XEvent    *event,
				     Boolean   *cont)
{
    Display		*disp = XtDisplay(w);
    CALLBACK_ENTRY	*entry = (CALLBACK_ENTRY *)client_data;

    if (event->type != ClientMessage || !entry->proc ||
	event->xclient.message_type != intern_atom(disp, "WM_PROTOCOLS") ||
	event->xclient.data.l[0] != intern_atom(disp, "WM_DELETE_WINDOW"))
	return;

    entry->proc(w, entry->client_data, (XtPointer)event);
    *cont = False;
}

static void destroy_callback(Widget w,
			     XtPointer client_data,
			     XtPointer call_data)
{
    CALLBACK_ENTRY	*entry = (CALLBACK_ENTRY *)client_data;

    entry->proc        = NULL;
    entry->client_data = NULL;
    XtFree((char *)entry);
}

void add_WM_DELETE_WINDOW_callback(Widget w,
				   XtCallbackProc proc,
				   XtPointer client_data)
{
    Display		*disp = XtDisplay(w);
    Window		win = XtWindow(w);
    Atom		wm_delete_window;
    CALLBACK_ENTRY	*entry;

    wm_delete_window = intern_atom(disp, "WM_DELETE_WINDOW");
    XSetWMProtocols(disp, win, &wm_delete_window, 1);

    entry = (CALLBACK_ENTRY *)XtMalloc(sizeof *entry);
    entry->proc        = proc;
    entry->client_data = client_data;

    XtAddEventHandler(w, 0, True, WM_DELETE_WINDOW_handler, (XtPointer)entry);
    XtAddCallback(w, XtNdestroyCallback, destroy_callback, (XtPointer)entry);
}

/*************************************************************************/

int get_event_xy(XEvent *event, int *x, int *y)
{
    if (!event)
	return False;

    switch (event->type) {
    case ButtonPress:
    case ButtonRelease:
        *x = event->xbutton.x;
        *y = event->xbutton.y;
        return True;
    case KeyPress:
    case KeyRelease:
        *x = event->xkey.x;
        *y = event->xkey.y;
        return True;
    case EnterNotify:
    case LeaveNotify:
        *x = event->xcrossing.x;
        *y = event->xcrossing.y;
        return True;
    case MotionNotify:
        *x = event->xmotion.x;
        *y = event->xmotion.y;
        return True;
    }

    return False;
}

Time get_event_time(XEvent *event)
{
    switch (event->type) {
    case ButtonPress:
    case ButtonRelease:
	return event->xbutton.time;
    case KeyPress:
    case KeyRelease:
	return event->xkey.time;
    case EnterNotify:
    case LeaveNotify:
	return event->xcrossing.time;
    case MotionNotify:
	return event->xmotion.time;
    case PropertyNotify:
	return event->xproperty.time;
    case SelectionClear:
	return event->xselectionclear.time;
    case SelectionNotify:
	return event->xselection.time;
    case SelectionRequest:
	return event->xselectionrequest.time;
    }

    return CurrentTime;
}

/*************************************************************************/

static int my_random(int n)
{
    static int	i = 1999;

    i *= 13;
    i = i % 2371;

    return (n * i) / 2371;
}

/*************************************************************************/

int is_popped_up(Widget w)
{
    return (XtIsSubclass(w, shellWidgetClass) &&
	    ((ShellWidget)w)->shell.popped_up);
}

void popup_under_pointer(Widget w, XtGrabKind grab)
{
    Widget		parent = XtParent(w);
    Arg			pos_args[2];
    int			x, y, x1, y1;
    Window		w1, w2;
    unsigned int	m;

    if (!XQueryPointer(XtDisplay(parent), XtWindow(parent),
		       &w1, &w2, &x, &y, &x1, &y1, &m))
	x = y = 0;
    x -= my_random(w->core.width);
    y -= my_random(w->core.height);
    if (x < 0)
	x = 0;
    else {
	int	temp = WidthOfScreen(XtScreen(parent)) - w->core.width;

	if (temp < 0)
	    x = 0;
	else if (x > temp)
	    x = temp;
    }
    if (y < 0)
	y = 0;
    else {
	int	temp = HeightOfScreen(XtScreen(parent)) - w->core.height;

	if (temp < 0)
	    y = 0;
	else if (y > temp)
	    y = temp;
    }
    
    XtSetArg(pos_args[0], XtNx, x);
    XtSetArg(pos_args[1], XtNy, y);
    XtSetValues(w, pos_args, XtNumber(pos_args));

    XtPopup(w, grab);
}

/*********************************************************************/

static void ascii_lower(char *c)
{
    while (*c != '\0') {
	if ((unsigned int)(*c - 'A') <= 'Z' - 'A')
	    *c += 'a' - 'A';
	c++;
    }
}

Boolean cvt_string_to_long(Display *disp,
			   XrmValue *args, Cardinal *no_args,
			   XrmValue *from, XrmValue *to,
			   XtPointer *client_data)
{
    static long	l;

    if (sscanf((char *)from->addr, "%ld", &l) != 1) {
	XtStringConversionWarning((char *)from->addr, XtRLong);
	return False;
    }

    if (!to->addr)
	to->addr = (XPointer)&l;
    else {
	if (to->size < sizeof l) {
	    to->size = sizeof l;
	    return False;
	}
	*(long *)to->addr = l;
    }
    to->size = sizeof l;

    return True;
}

Boolean cvt_string_to_justify(Display *disp,
			      XrmValue *args, Cardinal *no_args,
			      XrmValue *from, XrmValue *to,
			      XtPointer *client_data)
{
    static JustifyType	just;
    static XrmQuark     left = 0, center, right;
    XrmQuark		q;
    char		*s = (char *)from->addr;
    char		buffer[32];

    if (!s || strlen(s) > 30)
	return False;

    if (!left) {
        left   = XrmPermStringToQuark("left");
        center = XrmPermStringToQuark("center");
        right  = XrmPermStringToQuark("right");
    }

    if (to->addr && to->size < sizeof just) {
	to->size = sizeof just;
	return False;
    }

    strcpy(buffer, s);
    ascii_lower(buffer);
    q = XrmStringToQuark(buffer);

    if (q == left)
	just = JustifyTypeLeft;
    else if (q == center)
	just = JustifyTypeCenter;
    else if (q == right)
	just = JustifyTypeRight;
    else {
	XtStringConversionWarning((char *)from->addr, XtRJustify);
	return False;
    }

    to->size = sizeof just;
    if (to->addr)
	*(JustifyType *)to->addr = just;
    else
	to->addr = (XPointer)&just;

    return True;
}

/*********************************************************************/

Boolean cvt_std_sel(Widget           w,
		    Time             t,
		    Atom            *sel,
		    Atom            *target,
		    Atom            *type_ret,
		    XPointer        *val_ret,
		    unsigned long   *len_ret,
		    int             *format_ret)
{
    Display	*disp = XtDisplay(w);

#if 1
    if (*target == intern_atom(disp, "TIMESTAMP")) {
	long	l = t;
	int	i = t;

	if (sizeof l != 4 && sizeof i != 4)
	    return False;
        *val_ret = (XPointer)XtMalloc(4);
	if (sizeof l == 4)
	    memcpy(*val_ret, &l, 4);
	else
	    memcpy(*val_ret, &i, 4);
        *type_ret   = XA_INTEGER;
        *len_ret    = 1;
        *format_ret = 32;
        return True;
    }
#endif

    if (*target == intern_atom(disp, "USER")) {
        char	*name = getenv("USER");

        if (!name)
	    return False;
        *val_ret    = (XtPointer)XtNewString(name);
        *type_ret   = XA_STRING;
        *len_ret    = strlen(name);
        *format_ret = 8;
        return True;
    }

    if (*target == intern_atom(disp, "CLASS")) {
        char	*class;
        int	len;

        while (w && !XtIsApplicationShell(w))
            w = XtParent(w);

	if (!w)
	    return False;

	class = ((ApplicationShellWidget)w)->application.class;
	len = strlen(w->core.name);
        *len_ret = len + strlen(class) + 2;
        *val_ret = (XPointer)XtMalloc(*len_ret);
        strcpy((char *)*val_ret, w->core.name);
        strcpy((char *)*val_ret + len + 1, class);
        *type_ret = XA_STRING;
        *format_ret = 8;
        return True;
    }

    if (*target == intern_atom(disp, "NAME")) {
	while (w && !XtIsWMShell(w))
	    w = XtParent(w);

        while (w && !XtIsWMShell(w))
	    w = XtParent(w);

        if (!w)
	    return False;

        *val_ret = (XPointer)XtNewString(((WMShellWidget)w)->wm.title);
        *len_ret = strlen((char *)*val_ret);
        *type_ret = XA_STRING;
        *format_ret = 8;
        return True;
    }

    if (*target == intern_atom(disp, "CLIENT_WINDOW")) {
	Widget	p;

        while ((p = XtParent(w)))
	    w = p;

        *val_ret  = (XPointer)XtMalloc(sizeof(Window));
        *(Window *)*val_ret = w->core.window;
        *type_ret   = XA_WINDOW;
        *len_ret    = 1;
        *format_ret = 32;
        return True;
    }

    if (*target == intern_atom(disp, "TARGETS")) {
        Atom	*std_targets;
        int	n = 0;

	std_targets = (Atom *)XtMalloc(16 * sizeof *std_targets);

#if 1
        std_targets[n++] = intern_atom(disp, "TIMESTAMP");
#endif
        std_targets[n++] = intern_atom(disp, "USER");
        std_targets[n++] = intern_atom(disp, "CLASS");
        std_targets[n++] = intern_atom(disp, "NAME");
        std_targets[n++] = intern_atom(disp, "CLIENT_WINDOW");

        *val_ret    = (XPointer)std_targets;
        *type_ret   = XA_ATOM;
        *len_ret    = n;
        *format_ret = 32;
        return True;
    }

    return False;
}

/*********************************************************************/

typedef struct StippleCache StippleCache;

struct StippleCache {
    StippleCache	*next;
    Screen		*screen;
    Pixmap		pixmap;
    unsigned int	ref_cnt;
};

static StippleCache *stipple_cache = NULL;

Pixmap create_stipple(Screen *screen)
{
    static char		bits[] = {0x01, 0x02};
    StippleCache	*loop;

    for (loop = stipple_cache ; loop ; loop = loop->next)
	if (loop->screen == screen) {
	    loop->ref_cnt++;
	    return loop->pixmap;
	}

    loop = (StippleCache *)XtMalloc(sizeof *loop);
    loop->next = stipple_cache;
    stipple_cache = loop;
    loop->screen  = screen;
    loop->ref_cnt = 1;
    loop->pixmap =
	XCreatePixmapFromBitmapData(DisplayOfScreen(screen),
				    RootWindowOfScreen(screen),
				    bits, 2, 2, 0, 1, 1);

    return loop->pixmap;
}

void release_stipple(Screen *screen, Pixmap pixmap)
{
    StippleCache	**loop, *tmp;

    if (!stipple_cache)
	return;

    for (loop = &stipple_cache ; *loop ; loop = &(*loop)->next)
	if ((*loop)->screen == screen)
	    break;

    tmp = *loop;

    if (!tmp || --tmp->ref_cnt != 0)
	return;

    *loop = tmp->next;
    XtFree((char *)tmp);
}

/*********************************************************************/

Visual *get_visual(Widget w)
{
    while (w && !XtIsShell(w))
	w = XtParent(w);

    if (!w)
	return NULL;

    return ((ShellWidget)w)->shell.visual;
}

void black_and_white(Screen *screen, Visual *visual,
		     Pixel *black, Pixel *white)
{
    Display	*disp = DisplayOfScreen(screen);
    XVisualInfo	tmp, *info;
    int		n;

    *black = BlackPixelOfScreen(screen);
    *white = WhitePixelOfScreen(screen);
    if (!visual)
	return;

    tmp.visualid = XVisualIDFromVisual(visual);
    info = XGetVisualInfo(disp, VisualIDMask, &tmp, &n);
    if (!info)
	return;

    switch (info->class) {
    case StaticGray:
	*black = 0;
	*white = info->colormap_size - 1; /* Is this right? */
	break;
    case GrayScale:
	/* ?? */
	break;
    case StaticColor:
	*black = 0;
	*white = info->red_mask | info->green_mask | info->blue_mask;
	break;
    case PseudoColor:
	/* ?? */
	break;
    case TrueColor:
	*black = 0;
	*white = info->red_mask | info->green_mask | info->blue_mask;
    case DirectColor:
	/* ?? */
	break;
    }

    XFree((char *)info);
}

Pixel get_black(Widget w)
{
    Screen	*screen = XtScreen(w);
    Visual	*visual = get_visual(w);
    Pixel	black, white;

    black_and_white(screen, visual, &black, &white);
    return black;
}

/*********************************************************************/

typedef struct AtomCache {
    struct AtomCache	*next;
    Display		*disp;
    XrmQuark		name;
    Atom		atom;
} AtomCache;

/*
 *  This shouldn't grow to deep...
 */
static AtomCache	*atom_cache = NULL;

Atom intern_atom(Display *disp, char *name_str)
{
    XrmQuark	name = XrmStringToQuark(name_str);
    AtomCache	*loop = atom_cache;

    for (loop = atom_cache ; loop ; loop = loop->next)
	if (loop->name == name && loop->disp == disp)
	    return loop->atom;

    loop = (AtomCache *)XtMalloc(sizeof *loop);
    loop->next = atom_cache;
    loop->disp = disp;
    loop->name = name;
    loop->atom = XInternAtom(disp, name_str, False);

    atom_cache = loop;

    return loop->atom;
}
