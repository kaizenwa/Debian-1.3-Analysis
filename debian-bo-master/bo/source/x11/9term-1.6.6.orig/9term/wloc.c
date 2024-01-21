/*
 * This bogus code has been taken from xwininfo from the X distribution
 * except for GetNotePosition which was taken from xpostit
 */

#include <u.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#include <X11/extensions/shape.h>
#include <X11/Xmu/WinUtil.h>
#include <stdio.h>
#ifdef	NEEDVARARG
#include <varargs.h>
#else
#include <stdarg.h>
#endif

char		*program_name;			/* This program */
Display		*dpy;				/* The current display */
int		screen;				/* The current screen */

/* This handler is enabled when we are checking
   to see if the -id the user specified is valid. */

/*
 * Fatal error routine.
 * Does not require dpy or screen defined.
 */

/*VARARGS0*/
/*
 * terminal error handling
 */
#ifdef NEEDVARARG
void
Fatal_Error(va_alist)
va_dcl
#else
void
Fatal_Error(char *fmt, ...)
#endif
{
	va_list args;
#ifdef NEEDVARARG
	char *fmt;
#endif

	fflush(stdout);
	fflush(stderr);
	fprintf(stderr, "%s: error: ", program_name);
#ifdef NEEDVARARG
  	va_start(args);
  	fmt = va_arg(args, char *);
#else
	va_start(args, fmt); 
#endif
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	exit(1);
}

bad_window_handler(Display *disp, XErrorEvent *err)
{
	char	badid[20];

	sprintf(badid, "0x%lx", err->resourceid);
	Fatal_Error("No such window with id %s.", badid);
	exit (1);
}

/*
 * Report the syntax for calling xwininfo:
 */
void
usage(void)
{
	fprintf (stderr,
		"usage:  %s [-options ...]\n\n", program_name);
	fprintf (stderr,
		"where options include:\n");
	fprintf (stderr,
		"    -help                print this message\n");
	fprintf (stderr,
		"    -display host:dpy    X server to contact\n");
	fprintf (stderr,
		"    -root                use the root window\n");
	fprintf (stderr,
		"    -id windowid         use the window with the specified id\n");
	fprintf (stderr,
		"    -name windowname     use the window with the specified name\n");
	fprintf (stderr,
		"\n");
	exit (1);
}

/*
 * Get_Display_Name (argc, argv) Look for -display, -d, or host:dpy (obselete)
 * If found, remove it from command line.  Don't go past a lone -.
 */
char *
Get_Display_Name(int *pargc, char **argv)
{
	int	argc = *pargc;
	char	**pargv = argv+1;
	char	*displayname = NULL;
	int	i;

	for (i = 1; i < argc; i++)
	{
		char	*arg = argv[i];

		if (!strcmp (arg, "-display") || !strcmp (arg, "-d")) {
			if (++i >= argc) usage ();

			displayname = argv[i];
			*pargc -= 2;
			continue;
	}
	if (!strcmp(arg,"-"))
	{
		while (i<argc)
			*pargv++ = argv[i++];
		break;
	}
	*pargv++ = arg;
    }

    *pargv = NULL;
    return (displayname);
}

/*
 * Open_Display: Routine to open a display with correct error handling.
 *               Does not require dpy or screen defined on entry.
 */
Display *
Open_Display(char *display_name)
{
	Display *d;

	d = XOpenDisplay(display_name);
	if (d == NULL)
	{
		fprintf (stderr, "%s:  unable to open display '%s'\n",
				program_name, XDisplayName (display_name));
		usage ();
		/* doesn't return */
	}

	return(d);
}

/*
 * Setup_Display_And_Screen: This routine opens up the correct display (i.e.,
 *                           it calls Get_Display_Name) and then stores a
 *                           pointer to it in dpy.  The default screen
 *                           for this display is then stored in screen.
 *                           Does not require dpy or screen defined.
 */
void
Setup_Display_And_Screen(int *argc, char **argv)
{
	dpy = Open_Display (Get_Display_Name(argc, argv));
	screen = DefaultScreen(dpy);
}

/*
 * Window_With_Name: routine to locate a window with a given name on a display.
 *                   If no window with the given name is found, 0 is returned.
 *                   If more than one window has the given name, the first
 *                   one found will be returned.  Only top and its subwindows
 *                   are looked at.  Normally, top should be the RootWindow.
 */
Window Window_With_Name(Display *dpy, Window top, char *name)
{
	Window		*children, dummy;
	unsigned int	nchildren;
	int		i;
	Window		w=0;
	char		*window_name;

	if (XFetchName(dpy, top, &window_name) && !strcmp(window_name, name))
		return(top);

	if (!XQueryTree(dpy, top, &dummy, &dummy, &children, &nchildren))
		return(0);

	for (i=0; i<nchildren; i++)
	{
		w = Window_With_Name(dpy, children[i], name);
		if (w)
			break;
	}
	if (children) XFree ((char *)children);
	return(w);
}

/*
 * Select_Window_Args: a rountine to provide a common interface for
 *                     applications that need to allow the user to select one
 *                     window on the screen for special consideration.
 *                     This routine implements the following command line
 *                     arguments:
 *
 *                       -root            Selects the root window.
 *                       -id <id>         Selects window with id <id>. <id> may
 *                                        be either in decimal or hex.
 *                       -name <name>     Selects the window with name <name>.
 *
 *                     Call as Select_Window_Args(&argc, argv) in main before
 *                     parsing any of your program's command line arguments.
 *                     Select_Window_Args will remove its arguments so that
 *                     your program does not have to worry about them.
 *                     The window returned is the window selected or 0 if
 *                     none of the above arguments was present.  If 0 is
 *                     returned, Select_Window should probably be called after
 *                     all command line arguments, and other setup is done.
 *                     For examples of usage, see xwininfo, xwd, or xprop.
 */
Window
Select_Window_Args(int *rargc, char **argv)
#define ARGC (*rargc)
{
	int	nargc=1;
	int	argc;
	char	**nargv;
	Window	w=0;

	nargv = argv+1; argc = ARGC;
#define OPTION argv[0]
#define NXTOPTP ++argv, --argc>0
#define NXTOPT if (++argv, --argc==0) usage()
#define COPYOPT nargv++[0]=OPTION; nargc++

	while (NXTOPTP) {
		if (!strcmp(OPTION, "-")) {
			COPYOPT;
			while (NXTOPTP)
				COPYOPT;
			break;
		}
		if (!strcmp(OPTION, "-root")) {
			w=RootWindow(dpy, screen);
			continue;
		}
		if (!strcmp(OPTION, "-name")) {
			NXTOPT;
			w = Window_With_Name(dpy, RootWindow(dpy, screen), OPTION);
			if (!w)
				Fatal_Error("No window with name %s exists!", OPTION);
			continue;
		}
		if (!strcmp(OPTION, "-id")) {
			NXTOPT;
			w=0;
			sscanf(OPTION, "0x%lx", &w);
			if (!w)
				sscanf(OPTION, "%ld", &w);
			if (!w)
				Fatal_Error("Invalid window id format: %s.", OPTION);
			continue;
		}
		COPYOPT;
	}
	ARGC = nargc;
	
	return(w);
}

/*
 * GetNotePosition - find the position of the widget window, taking into
 *		     account any borders stuck on by reparenting window
 *		     managers.
 *
 *		     This is a KLUDGE.  The ICCCM does not specify a way
 *		     for a client to find out what kind of border stuff
 *		     the window manager has added.
 *
 *		     Looks for a virtual root window.
 *
 *		     Thanks to Stewart Levin for the original code.
 */
GetWindowPosition(win, x, y, w, h)
int *x, *y, *w, *h;
Window win;
{
	Window			*children;
	int			status, nchildren;
	Window			here, parent, root, parentReturn, rootReturn;
	XWindowAttributes	win_attributes;
	int			screen;
	Atom			__SWM_VROOT = None;
	int			i;

	parent = win;

	/*
	 * Find the root window. We may have a virtual root window
	 */
	screen = DefaultScreen(dpy);
	root = RootWindow(dpy, screen);

	/* go look for a virtual root */
	__SWM_VROOT = XInternAtom(dpy, "__SWM_VROOT", False);
	XQueryTree(dpy, root, &rootReturn, &parentReturn,
			&children, &nchildren);
	for (i = 0; i < nchildren; i++)
	{
		Atom actual_type;
		int actual_format;
		long nitems, bytesafter;
		Window *newRoot = NULL;

		if (XGetWindowProperty(dpy, children[i], __SWM_VROOT, 0, 1, False,
				XA_WINDOW, &actual_type, &actual_format, &nitems, &bytesafter,
				(unsigned char **) &newRoot) == Success && newRoot)
		{
			root = *newRoot;
			break;
		}
	}

	/*
	 * Walk up the tree looking for a parent of this window whose
	 * parent is the root.  That'll either be this window (if there
	 * is no window manager window) or the window added by the
	 * window manager.
	 */
	do {
		here = parent;

		status = XQueryTree(dpy, here, &rootReturn, &parent, &children,
				    &nchildren);

		if (!status)
			break;

		if (children)
			XFree(children);
	} while (parent != root);

	/*
	 * Get the attributes of this window we just found.
	 */
	XGetWindowAttributes(dpy, here, &win_attributes);

	/*
	 * Now deduct the border from the position of the window.
	 * We know the coordinates don't need translating, since
	 * this window's parent is the root.
	 */
	*x = win_attributes.x;
	*y = win_attributes.y;
	*w = win_attributes.width;
	*h = win_attributes.height;
}

main(int argc, char **argv)
{
	int	i;
	Window	window;
	char	*w;
	int	wloc;
	int	label;

	program_name = argv[0];
	w = strrchr(program_name, '/');
	if (w == (char *)0)
		w = program_name;
	else
		w++;
	wloc = (strcmp(w, "wloc") == 0);
	label = !wloc && (strcmp(w, "label") == 0);

	/* Open display, handle command line arguments */
	Setup_Display_And_Screen(&argc, argv);

	/* Get window selected on command line, if any */
	window = Select_Window_Args(&argc, argv);

	/* If no window selected on command line, let user pick one the hard way */
	if (!window && (w = (char *)getenv("WINDOWID")))
		window = atoi(w);
	else if (!window)
		Fatal_Error("Unable to find a window.");

	/*
	 * make sure that the window is valid
	 */
	{
		Window		root;
		int		x, y, w, h;
		unsigned int	width, height, bw, depth;
		int		(*old_handler)();
		char		*name;

		old_handler = XSetErrorHandler(bad_window_handler);
		XGetGeometry (dpy, window, &root, &x, &y, &width, &height, &bw, &depth);
		XSync (dpy, False);
		(void) XSetErrorHandler(old_handler);

		if (wloc) {
			GetWindowPosition(window, &x, &y, &w, &h);
			printf("-geometry %dx%d+%d+%d\n", w, h, x, y);
		} else if (label) {
			if (argc > 1) {
				XStoreName(dpy, window, argv[1]);
				XSetIconName(dpy, window, argv[1]);
				XFlush(dpy);
			} else {
				XFetchName(dpy, window, &name);
				if (name)
					printf("%s\n", name);
				XFree(name);
			}
		}
	}
	XCloseDisplay(dpy);
	exit(0);
}
