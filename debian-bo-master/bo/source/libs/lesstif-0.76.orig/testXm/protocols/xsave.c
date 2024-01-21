/*
 * xsave - simple program to send WM_SAVE_YOURSELF to an X client
 *	   Derived from the X Consortium's "xkill" program.
 *
 *	Author of the "xsave" parts : Danny Backx (u27113@kb.be).
 *
 *
 * $XConsortium: xkill.c,v 1.18 91/03/20 19:44:15 converse Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Jim Fulton, MIT X Consortium; Dana Chee, Bellcore
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xproto.h>

#include <X11/Xmu/WinUtil.h>

Display *dpy = NULL;
char *ProgramName;

#define SelectButtonAny (-1)
#define SelectButtonFirst (-2)

XID parse_id(), get_window_id();
int parse_button();

Bool wm_state_set();
Bool wm_running();
int parse_button();

void
Exit (code)
    int code;
{
    if (dpy) {
	XCloseDisplay (dpy);
    }
    exit (code);
}

void
usage ()
{
    static char *options[] = {
"where options include:",
"    -display displayname    X server to contact",
"    -id resource            resource whose client is to be sent a message",
"    -frame                  don't ignore window manager frames",
"    -button number          specific button to be pressed to select window",
"",
NULL};
    char **cpp;

    fprintf (stderr, "usage:  %s [-option ...]\n",
	     ProgramName);
    for (cpp = options; *cpp; cpp++) {
	fprintf (stderr, "%s\n", *cpp);
    }
    Exit (1);
}

void
SendIt(Display *dpy, Window win)
{
	XClientMessageEvent	ev;
	Atom		a_wm_protocols, a_wm_save_yourself;

	fprintf(stderr, "Window Id is 0x%x\n", (unsigned int)win);

	a_wm_protocols = XInternAtom(dpy, "WM_PROTOCOLS", False);
	a_wm_save_yourself = XInternAtom(dpy, "WM_SAVE_YOURSELF", False);

	ev.type = ClientMessage;
	ev.window = win;
	ev.message_type = a_wm_protocols;
	ev.format = 32;
	ev.data.l[0] = a_wm_save_yourself;
	ev.data.l[1] = CurrentTime;

	(void) XSendEvent(dpy, win, True, NoEventMask, (XEvent *)&ev);
	XFlush(dpy);
}

int
main (argc, argv)
    int argc;
    char *argv[];
{
    int i;				/* iterator, temp variable */
    char *displayname = NULL;		/* name of server to contact */
    int screenno;			/* screen number of dpy */
    XID id = None;			/* resource to send a message to */
    char *button_name = NULL;		/* name of button for window select */
    int button;				/* button number or negative for all */
    Bool top = False;

    ProgramName = argv[0];

    for (i = 1; i < argc; i++) {
	char *arg = argv[i];

	if (arg[0] == '-') {
	    switch (arg[1]) {
	      case 'd':			/* -display displayname */
		if (++i >= argc) usage ();
		displayname = argv[i];
		continue;
	      case 'i':			/* -id resourceid */
		if (++i >= argc) usage ();
		id = parse_id (argv[i]);
		continue;
	      case 'b':			/* -button number */
		if (++i >= argc) usage ();
		button_name = argv[i];
		continue;
	      case 'f':			/* -frame */
		top = True;
		continue;
	      default:
		usage ();
	    }
	} else {
	    usage ();
	}
    }					/* end for */

    dpy = XOpenDisplay (displayname);
    if (!dpy) {
	fprintf (stderr, "%s:  unable to open display \"%s\"\n",
		 ProgramName, XDisplayName (displayname));
	Exit (1);
    }
    screenno = DefaultScreen (dpy);

    /*
     * if no id was given, we need to choose a window
     */

    if (id == None) {
	if (!button_name)
	    button_name = XGetDefault (dpy, ProgramName, "Button");

	if (!button_name)
	    button = SelectButtonFirst;
	else if (!parse_button (button_name, &button)) {
	    fprintf (stderr, "%s:  invalid button specification \"%s\"\n",
		     ProgramName, button_name);
	    Exit (1);
	}

	if (button >= 0 || button == SelectButtonFirst) {
	    unsigned char pointer_map[256];	 /* 8 bits of pointer num */
	    int count, j;
	    unsigned int ub = (unsigned int) button;


	    count = XGetPointerMapping (dpy, pointer_map, 256);
	    if (count <= 0) {
		fprintf (stderr, 
			 "%s:  no pointer mapping, can't select window\n",
			 ProgramName);
		Exit (1);
	    }

	    if (button >= 0) {			/* check button */
		for (j = 0; j < count; j++) {
		    if (ub == (unsigned int) pointer_map[j]) break;
		}
		if (j == count) {
		    fprintf (stderr,
	 "%s:  no button number %u in pointer map, can't select window\n",
			     ProgramName, ub);
		    Exit (1);
	        }
	    } else {				/* get first entry */
		button = (int) ((unsigned int) pointer_map[0]);
	    }
	}
	if ((id = get_window_id (dpy, screenno, button,
				"the window whose client you wish to send a message to")) != 0) {
	    if (id == RootWindow(dpy,screenno)) id = None;
	    else if (!top) {
		XID indicated = id;
		if ((id = XmuClientWindow(dpy, indicated)) == indicated) {
		    
		    /* Try not to target the window manager when the user
		     * indicates an icon to xsave.
		     */

		    if (! wm_state_set(dpy, id) && wm_running(dpy, screenno))
			id = None;

		} 
	    }
	}
    }

    if (id != None) {
	SendIt(dpy, id);
    }

    Exit (0);
    exit(0);
}

int parse_button (s, buttonp)
    register char *s;
    int *buttonp;
{
    register char *cp;

    /* lower case name */
    for (cp = s; *cp; cp++) {
	if (isascii (*cp) && isupper (*cp)) {
#ifdef _tolower
	    *cp = _tolower (*cp);
#else
	    *cp = tolower (*cp);
#endif /* _tolower */
	}
    }

    if (strcmp (s, "any") == 0) {
	*buttonp = SelectButtonAny;
	return (1);
    }

    /* check for non-numeric input */
    for (cp = s; *cp; cp++) {
	if (!(isascii (*cp) && isdigit (*cp))) return (0);  /* bogus name */
    }

    *buttonp = atoi (s);
    return (1);
}


XID parse_id (s)
    char *s;
{
    XID retval = None;
    char *fmt = "%ld";			/* since XID is long */

    if (s) {
	if (*s == '0') s++, fmt = "%lo";
	if (*s == 'x' || *s == 'X') s++, fmt = "%lx";
	sscanf (s, fmt, &retval);
    }
    return (retval);
}

XID get_window_id (dpy, screen, button, msg)
    Display *dpy;
    int screen;
    int button;
    char *msg;
{
    Cursor cursor;		/* cursor to use when selecting */
    Window root;		/* the current root */
    Window retwin = None;	/* the window that got selected */
    int retbutton = -1;		/* button used to select window */
    int pressed = 0;		/* count of number of buttons pressed */

#define MASK (ButtonPressMask | ButtonReleaseMask)

    root = RootWindow (dpy, screen);
    cursor = XCreateFontCursor (dpy, XC_draped_box);
    if (cursor == None) {
	fprintf (stderr, "%s:  unable to create selection cursor\n",
		 ProgramName);
	Exit (1);
    }

    printf ("Select %s with ", msg);
    if (button == -1)
      printf ("any button");
    else
      printf ("button %d", button);
    printf ("....\n");
    XSync (dpy, 0);			/* give xterm a chance */

    if (XGrabPointer (dpy, root, False, MASK, GrabModeSync, GrabModeAsync, 
    		      None, cursor, CurrentTime) != GrabSuccess) {
	fprintf (stderr, "%s:  unable to grab cursor\n", ProgramName);
	Exit (1);
    }

    /* from dsimple.c in xwininfo */
    while (retwin == None || pressed != 0) {
	XEvent event;

	XAllowEvents (dpy, SyncPointer, CurrentTime);
	XWindowEvent (dpy, root, MASK, &event);
	switch (event.type) {
	  case ButtonPress:
	    if (retwin == None) {
		retbutton = event.xbutton.button;
		retwin = ((event.xbutton.subwindow != None) ?
			  event.xbutton.subwindow : root);
	    }
	    pressed++;
	    continue;
	  case ButtonRelease:
	    if (pressed > 0) pressed--;
	    continue;
	}					/* end switch */
    }						/* end for */

    XUngrabPointer (dpy, CurrentTime);
    XFreeCursor (dpy, cursor);
    XSync (dpy, 0);

    return ((button == -1 || retbutton == button) ? retwin : None);
}


int catch_window_errors (dpy, ev)
    Display *dpy;
    XErrorEvent *ev;
{
    return 0;
}

/* Return True if the property WM_STATE is set on the window, otherwise
 * return False.
 */
Bool wm_state_set(dpy, win) 
Display	*dpy;
Window	win;
{
    Atom wm_state;
    Atom actual_type;
    int success;
    int actual_format;
    unsigned long nitems, remaining;
    unsigned char* prop = NULL;

    wm_state = XInternAtom(dpy, "WM_STATE", True);
    if (wm_state == None) return False;
    success = XGetWindowProperty(dpy, win, wm_state, 0L, 0L, False, 
				 AnyPropertyType, &actual_type, &actual_format,
				 &nitems, &remaining, &prop);
    if (prop) XFree((char *) prop);
    return (success == Success && actual_type != None && actual_format);
}

/* Using a heuristic method, return True if a window manager is running,
 * otherwise, return False.
 */

Bool wm_running(dpy, screenno)
Display *dpy;
int	screenno;
{
    XWindowAttributes	xwa;
    Status		status;

    status = XGetWindowAttributes(dpy, RootWindow(dpy, screenno), &xwa);
    return (status &&
	    ((xwa.all_event_masks & SubstructureRedirectMask) ||
	     (xwa.all_event_masks & SubstructureNotifyMask)));
}
