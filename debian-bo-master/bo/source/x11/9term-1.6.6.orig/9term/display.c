/*
 *  Copyright © 1993 James Farrow, University of Sydney - all rights reserved
 *
 *  9term graphics interface
 */

#include <u.h>
#include <libc.h>
#include <libg.h>
#include <frame.h>
#include <text.h>
#include <signal.h>

#define Cursor	xCursor
#define Font	xFont
#define Event	xEvent

#define	_SYS_FCNTL_	1

#include <X11/Xlib.h>
#ifdef IRIX
#	include <X11/Xlibint.h>
#endif
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xatom.h>

#undef	Cursor
#undef	Font
#undef	Event

#include "9term.h"

extern	Display	*_dpy;
extern	Widget	_toplevel;

Atom	_XA_WM_DELETE_WINDOW;
Atom	_9wm_running;
Atom	_9wm_hold_mode;

static int	scrolling;

static	Cursor	whitearrow = {
	{0, 0},
	{0xFF, 0xE0, 0xFF, 0xE0, 0xFF, 0xC0, 0xFF, 0x00,
	 0xFF, 0x00, 0xFF, 0x80, 0xFF, 0xC0, 0xFF, 0xE0,
	 0xE7, 0xF0, 0xE3, 0xF8, 0xC1, 0xFC, 0x00, 0xFE,
	 0x00, 0x7F, 0x00, 0x3E, 0x00, 0x1C, 0x00, 0x08,},
	{0xFF, 0xE0, 0xFF, 0xE0, 0xC1, 0xC0, 0xC3, 0x00,
	 0xC3, 0x00, 0xC1, 0x80, 0xD8, 0xC0, 0xFC, 0x60,
	 0xE6, 0x30, 0xE3, 0x18, 0xC1, 0x8C, 0x00, 0xC6,
	 0x00, 0x63, 0x00, 0x36, 0x00, 0x1C, 0x00, 0x08,}
};

static void	delwin(Widget, XEvent*, String*, Cardinal*);

Text		*text;		/* the main and only text buffer */

	/* too many X options */
static XrmOptionDescRec optable[] = {
	{"-s",		"*scroll",	XrmoptionNoArg,		"true"},
	{"+s",		"*scroll",	XrmoptionNoArg,		"false"},
	{"-ls",		"*login",	XrmoptionNoArg,		"true"},
	{"+ls",		"*login",	XrmoptionNoArg,		"false"},
	{"-ut",		"*utmp",	XrmoptionNoArg,		"true"},
	{"+ut",		"*utmp",	XrmoptionNoArg,		"false"},
	{"-9",		"*kbdMode",	XrmoptionNoArg,		"plan9"},
	{"-unix",	"*kbdMode",	XrmoptionNoArg,		"unix"},
	{"-label",	"*label",	XrmoptionSepArg,        NULL},
	{"-high",	"*highwater",	XrmoptionSepArg,        NULL},
	{"-low",	"*lowwater",	XrmoptionSepArg,        NULL},
	{"-9wm",	"*9wm",		XrmoptionNoArg,		"true"},
	{"-beep",	"*beep",	XrmoptionNoArg,		"plan9 unix"},
	{"-debug",	"*debug",	XrmoptionNoArg,		"true"},
};

char *fallbacks[] = {
	"*scroll: false",
	"*login: false",
	"*utmp: false",
	"*label: 9term",
	"*kbdMode: plan9",
	"*geometry: 745x364",
	"*highwater: 50000",
	"*lowwater: 40000",
	"*9wm: false",
	"*beep: none",
	NULL
};

static XtActionsRec actions[] = {
	"Quit",		delwin,
};

/*
 *  X error handler.  just bomb out.
 */
static int
error_handler(Display *dpy, XErrorEvent *evp)
{
	quit(1);
}

/*
 *  X I/O error handler.  just bomb out.
 */
static int
io_error_handler(Display *dpy)
{
	quit(1);
}

/*
 *	delwin: Action proc to implement ICCCM delete_window.
 */
static void
delwin(Widget w, XEvent *event, String *params, Cardinal *n)
{
	if (w == _toplevel && event->type == ClientMessage && event->xclient.data.l[0] == _XA_WM_DELETE_WINDOW)
		_killpg(SIGHUP);
}

/*
 *	try to extract an X resource under a variety of names
 */
static char *
get_resource(char *resource, char *class, char *rname, char *cname)
{
	char str1[256], str2[256];
	static char result[512];
	XrmValue value;
	char *str_type;

	sprintf(str1, "%s.%s", resource, rname);
	sprintf(str2, "%s.%s", class, cname);
	if (XrmGetResource(
			XrmGetDatabase(_dpy),
			str1, str2, &str_type, &value) == True) {
		strncpy(result, value.addr, (int)value.size);
		return result;
	}
	return 0;
}

/*  
 *	look for X resources
 */
static void
extract_resources(char *resource, char *class, char *shargv[])
{
	char *s;
	char *new;
	Font *newfont;

	s = get_resource(resource, class, "debug", "Debug");
	if (s && strcasecmp(s, "true")) {
		XSetErrorHandler(error_handler);
		XSetIOErrorHandler(io_error_handler);
	}
	s = get_resource(resource, class, "login", "Login");
	if (s && !strcasecmp(s, "true")) {
		/* Change argv[0] if this is a login shell */
		new = (char *)malloc(strlen(shargv[0])+2);
		if (!new)
			error("malloc failure");
		new[0] = '-';
		strcpy(new+1, shargv[0]);
		shargv[0] = new;
	}
	s = get_resource(resource, class, "scroll", "Scroll");
	if (s && !strcasecmp(s, "true"))
		scrolling = 1;
	s = get_resource(resource, class, "utmp", "Utmp");
	if (s && !strcasecmp(s, "true"))
		utmpentry = 1;
	if (s = get_resource(resource, class, "label", "Label")) {
		XStoreName(_dpy, XtWindow(_toplevel), s);
		XSetIconName(_dpy, XtWindow(_toplevel), s);
		XFlush(_dpy);
	}
	if (s = get_resource(resource, class, "ttyModes", "TtyModes"))
		parsettymodes(UNIX, s);
	if (s = get_resource(resource, class, "p9TtyModes", "P9TTyModes"))
		parsettymodes(PLAN9, s);
	if (s = get_resource(resource, class, "kbdMode", "KbdMode"))
		if (!strcasecmp(s, "unix"))
			kbdmode = UNIX;
		else if (!strcasecmp(s, "plan9"))
			kbdmode = PLAN9;
	if (s = get_resource(resource, class, "p9font", "P9font"))
		setenv("font", s, 1);
	if (s = get_resource(resource, class, "highwater", "Highwater"))
		highwater = atoi(s);
	if (s = get_resource(resource, class, "lowwater", "Lowwater"))
		lowwater = atoi(s);
	if (s = get_resource(resource, class, "9wm", "9Wm"))
		ninewm = !strcasecmp(s, "true");
	if (s = get_resource(resource, class, "beep", "Beep")) {
		if (strstr(s, "unix"))
			beepmask |= UNIX;
		if (strstr(s, "plan9"))
			beepmask |= PLAN9;
	}
}
/*
 *	initialize the 9term display
 */
void
init_display(int *argc, char **argv, char **shargv, char *resource)
{
	XrmDatabase	rdb;
	XrmDatabase	cmd;
	char		**cp;
	char		id[512];
	Rectangle	r;

		/* look for X resources on command line */
	XrmInitialize();
	cmd = 0;
	XrmParseCommand(&cmd, optable, sizeof(optable)/sizeof(optable[0]),
					resource, argc, argv);

		/* use libg to initialize the display, window, & toolkit */
	xtbinit(0, resource, argc, argv, fallbacks);

		/* we're still not done with the command line */
	rdb = XrmGetDatabase(_dpy);
	XrmMergeDatabases(cmd, &rdb);
#ifdef DEBUG_X
	XSynchronize(_dpy, True);
	XSetErrorHandler(abort);
#endif
		/* export window id to environment */
	sprintf(id, "%d", XtWindow(_toplevel));
	setenv("WINDOWID", id, 1);

		/* register mouse and keyboard events */
	einit(Ekeyboard | Emouse);

		/* More X resource processing - why is this so complicated? */
	extract_resources(resource, "9Term", shargv);

		/* Initialization for 9wm compatibility */
	_9wm_running = XInternAtom(_dpy, "_9WM_RUNNING", False);
	_9wm_hold_mode = XInternAtom(_dpy, "_9WM_HOLD_MODE", False);
	if (XGetSelectionOwner(_dpy, _9wm_running) != None)
		ninewm = 1;

		/* work around textalloc wierdness */
	r = screen.r;
	if (ninewm) {
		r = inset(r, -3);
		r.min.x += 1;
	}
		/* bind the Text ADT to the screen*/
	text = textalloc(&screen, r, font);
	if (scrolling)
		text->scrolling = 1;

		/* Get WM_DELETE_WINDOW atom */
	_XA_WM_DELETE_WINDOW = XInternAtom (_dpy, "WM_DELETE_WINDOW", False);
	XSetWMProtocols(_dpy, XtWindow(_toplevel), &_XA_WM_DELETE_WINDOW, 1);

		/* ICCCM delete_window. */
	XtAppAddActions(XtDisplayToApplicationContext(_dpy), actions, XtNumber(actions));
	XtOverrideTranslations(_toplevel, XtParseTranslationTable ("<Message>WM_PROTOCOLS: Quit()"));
}

/*
 *	handle a reshape event
 */
void
ereshaped(Rectangle r)
{
	int width, height;

	if (ninewm) {
		/* work around textsetrects wierdness */
		r = inset(r, -3);
		r.min.x += 1;
	}
	if (text) {
		textsetrects(text, r, &screen);
		scr_get_size(&width, &height);
		tty_set_size(comm_fd, width, height, Dx(text->r), Dy(text->r));
		setborder();
	}
}

/*
 *	dummy mouse driver for the frame library
 */
void
frgetmouse(void)
{
	e.mouse = emouse();
}

/*
 *  Return the width and height of the screen.
 */
void
scr_get_size(int *width, int *height)
{
	*width = Dx(text->f.r) * 8 / text->f.maxtab;
	*height = text->f.maxlines;
}

/*
 *	for whom the bell tolls
 */
void
beep(void)
{
	XBell(_dpy, 50);
}

/*
 *	display the correct border according to the current hold mode
 */
void
setborder(void)
{
	cursorswitch(suspended ? &whitearrow : 0);
	if (ninewm) {
		XChangeProperty(_dpy, XtWindow(_toplevel), _9wm_hold_mode, XA_INTEGER, 32, PropModeReplace, (unsigned char *)&suspended, 1);
		return;
	}
	border(&screen, screen.r, 3, F);
	if (suspended)
		border(&screen, inset(screen.r, 1), 1, Zero);
}
