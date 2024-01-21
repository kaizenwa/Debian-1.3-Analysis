/*
 * File:	x.c
 * Purpose:	Implement various X related things.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: x.c,v 1.20 1997/01/07 00:45:28 liw Exp $"
 */


#include <assert.h>
#include <stdlib.h>
#include <publib.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xatom.h>

#include "config.h"
#include "actions.h"
#include "buflist.h"
#include "endprompt.h"
#include "font.h"
#include "win.h"
#include "cmd.h"
#include "x.h"


/*
 * Variable:	fallback_resources
 * Purpose:	Builtin resources for use when the separate application
 *		defaults file is missing.
 * Note:	SeX.h is built from SeX (the app-def file) automatically.
 */
static String fallback_resources[] = {
#include "SeX.h"
	NULL
};


/*
 * Variable:	first_top_level
 * Purpose:	The widget identifier of the first top level window that
 *		is created when the application is created.
 * Note:	This is initialized by x_do_it.
 */
static Widget first_top_level;


/*
 * Variable:	first_top_level_has_been_used
 * Purpose:	Has the initial value of first_top_level been used?
 * See also:	function x_create_top_level.
 * Note:	This is initialized to -1 so that x_create_top_level
 *		can check that first_top_level has been set at all by
 *		x_do_it.
 */
static int first_top_level_has_been_used = -1;


/*
 * Variable:	application_context
 * Purpose:	The Xt application context.
 * Note:	Initialized by x_do_it.
 */
static XtAppContext application_context;



/*
 * Variables:	WM_PROTOCLS, WM_DELETE_WINDOW
 * Purpose:	X atoms window manager communication.
 * Note:	Initialized by x_do_it.
 */
static Atom WM_PROTOCOLS;
static Atom WM_DELETE_WINDOW;



/*
 * Prototypes for local functions.
 */
static void delete_window(Widget, XtPointer, XEvent *, Boolean *);



/*
 * Function:	create_buffer
 * Purpose:	Create a new Sbuf, with all the marks that SeX needs.
 */
Sbuf *create_buffer(void) {
	Sbuf *buf;
	Sbufmark *m;
	int i;
	
	buf = sbuf_create();
	if (buf == NULL)
		return NULL;
	for (i = DUMMY_BEFORE_MARK+1; i < DUMMY_AFTER_MARK; ++i) {
		m = sbuf_mark(buf, 0, 0);
		if (m == NULL)
			break;
		sbuf_set_mark_code(m, i);
	}
	if (i < DUMMY_AFTER_MARK || buflist_add(buf) == -1) {
		while (--i > DUMMY_BEFORE_MARK) {
			m = sbuf_find_mark_by_code(buf, i);
			if (m != NULL)
				sbuf_unmark(m);
		}
		sbuf_destroy(buf);
		return NULL;
	}
	return buf;
}



/*
 * Function:	destroy_buffer
 * Purpose:	Destroy a new Sbuf, with all usual marks.
 */
void destroy_buffer(Sbuf *buf) {
	Sbufmark *m;
	int i;

	buflist_remove(buf);	
	for (i = DUMMY_BEFORE_MARK+1; i < DUMMY_DELETE_MARK; ++i) {
		m = sbuf_find_mark_by_code(buf, i);
		if (m != NULL)
			sbuf_unmark(m);
	}
	sbuf_destroy(buf);
}



/*
 * Function:	x_do_it
 * Purpose:	Initialize various X and Xt related things.
 * Arguments:	none
 * Return:	nothing
 * Note:	Initializes first_top_level and 
 *		first_top_level_has_been_used.
 */
void x_do_it(int argc, char **argv) {
	struct win *win;
	Sbuf *buf;
	struct font *font;
	int i;
	char fullname[1024]; /* xxx boo hiss */
	size_t n;

	if (config_read() == -1) {
		XtWarning("couldn't read configuration file -- quitting");
		exit(1);
	}

	assert(first_top_level_has_been_used == -1);
	first_top_level = XtVaAppInitialize(&application_context, 
		"SeX", NULL, 0,
		&argc, argv, fallback_resources, NULL);
	first_top_level_has_been_used = 0;
	
	action_add(&application_context);
	if (font_load(&font, config_get_string(CONFIG_FONT)) == -1) {
		XtWarning("can't load necessary font -- aborting");
		exit(EXIT_FAILURE);
	}
	font_set_default(font);
	
	WM_DELETE_WINDOW = XInternAtom(XtDisplay(first_top_level),
				"WM_DELETE_WINDOW", False);
	WM_PROTOCOLS = XInternAtom(XtDisplay(first_top_level),
				"WM_PROTOCOLS", False);

	if (argc == 1) {
		buf = create_buffer();
		if (buf == NULL || win_create(&win, buf) == -1)
			XtWarning("couldn't create empty buffer & window");
	} else {
		for (i = 1; i < argc; ++i) {
			n = fnqualify(fullname, argv[i], sizeof(fullname));
			if (n > sizeof(fullname))
				errormsg(1, 0, "filename too long: %s", argv[i]);
			buf = create_buffer();
			if (buf == NULL || sbuf_set_name(buf, fullname) == -1)
				XtWarning("can't create buffers for all files");
			if (i == 1 && win_create(&win, buf) == -1)
				XtWarning("couldn't create all windows");
			else
				(void) cmd_load_file(win);
		}
	}

	XtAppMainLoop(application_context);
}



/*
 * Function:	x_pending
 * Arguments:	None.
 * Purpose:	Are there any pending X events?
 * Return:	True or false.
 */
int x_pending(void) {
#if 0
	return XtAppPending(application_context);
#else
	return 0;
#endif
}



/*
 * Function:	x_timeout
 * Purpose:	Call function after a given time.
 * Arguments:	usec	time to timeout (in milliseconds)
 *		func	function to call when timeout occurs
 * Return:	Nothing.
 */
static XtIntervalId timeout_id = 0;
static void call_timeout(XtPointer func, XtIntervalId *dummy) {
	((void (*)(void)) func)();
	timeout_id = 0;
}
void x_timeout(int usec, void (*func)(void)) {
	if (timeout_id != 0)
		XtRemoveTimeOut(timeout_id);
	timeout_id = XtAppAddTimeOut(application_context, usec, call_timeout, 
		(XtPointer) func);
}



/*
 * Function:	x_display
 * Purpose:	Return the display we're connected to.
 * Arguments:	None.
 * Return:	X identifier for display.
 */
Display *x_display(void) {
	assert(first_top_level_has_been_used != -1);
	return XtDisplay(first_top_level);
}



/*
 * Function:	x_create_top_level
 * Purpose:	Create a new top level window.
 * Arguments:	none
 * Return:	The widget identifier of the new window.
 * Note:	The first top level window is actually created when the
 *		application is initialized.  If that hasn't been used yet,
 *		i.e., first_top_level_has_been_used is false, then we return
 *		it.  Otherwise we create a new window.
 */
Widget x_create_top_level(void) {
	Widget top;

	assert(first_top_level_has_been_used != -1);

	if (first_top_level_has_been_used) {
		top = XtAppCreateShell("SeX", "SeX",
			topLevelShellWidgetClass,
			XtDisplay(first_top_level), 0, 0);
	} else {
		top = first_top_level;
		first_top_level_has_been_used = 1;
	}

	return top;
}



/*
 * Function:	x_set_wm_protocols
 * Purpose:	Make a window obey window manager protocols.
 * Arguments:	w	the widget
 *		fun	function to call when protocol action happens
 * Return:	Nothing.
 */
void x_set_wm_protocols(Widget w, void (*fun)(Widget)) {
	if (XSetWMProtocols(XtDisplay(w), XtWindow(w), &WM_DELETE_WINDOW, 1) == 0)
		XtWarning("XSetWMProtocols failed!");
	else
		XtAddEventHandler(w, NoEventMask, True, delete_window, 
			(XtPointer) fun);
}
	



/*
 * Function:	x_window_dimension
 * Purpose:	Return the width and height in pixels of an X window
 * Arguments:	window	the window to be inspected
 *		width	pointer to location where width is stored
 *		height	pointer to location where height is stored
 * Return:	nothing
 */
void x_window_dimensions(Widget window, unsigned *width, unsigned *height) {
	Window win;
	Window root;
	Display *dpy;
	Screen *scr;
	int x, y;
	unsigned border, depth;

	dpy = XtDisplay(window);
	win = XtWindow(window);
	scr = XtScreen(window);

	XGetGeometry(dpy, win, &root, &x, &y, width, height, &border, &depth);
}




/*
 * Function:	x_create_gc
 * Purpose:	Create an X graphics context for text.
 * Arguments:	w	widget to be drawed in
 *		font	font to be used
 *		fg	foreground color
 *		bg	background color
 * Return:	The graphics context.
 */
GC x_create_gc(Widget w, struct font *font, unsigned long fg, unsigned long bg) {
	GC gc;
	Display *dpy;
	Window win;

	dpy = x_display();
	win = XtWindow(w);
	
        gc = XCreateGC(dpy, win, 0, NULL);
        XSetForeground(dpy, gc, fg);
        XSetBackground(dpy, gc, bg);
        XSetFont(dpy, gc, font_get_x_font_id(font));

	return gc;
}




/**********************************************************************
 * Local functions follow                                             *
 *********************************************************************/
 


static void delete_window(Widget wid, XtPointer p, XEvent *e, Boolean *b) {
	if (e->xclient.message_type != WM_PROTOCOLS ||
	    (Atom) e->xclient.data.l[0] != WM_DELETE_WINDOW) {
		*b = True;
		return;
    	}

	((void (*)(Widget)) p)(wid);
	*b = False;
}
