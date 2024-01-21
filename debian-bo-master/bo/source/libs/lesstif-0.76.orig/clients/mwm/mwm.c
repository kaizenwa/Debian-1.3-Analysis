/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 * mwm - "LessTif Window Manager"
 ***********************************************************************/

#include "mwm.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#ifdef TIME_WITH_SYS_TIME
#include <time.h>
#endif
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <unistd.h>
#include <stdlib.h>

#include <X11/Shell.h>
#include <Xm/DisplayP.h>
#include <Xm/ScreenP.h>

extern int putenv();

/*
 * application globals
 */
#define MAXHOSTNAME 255

MwmInternalInfo Mwm;
Display        *dpy;		/* which display are we talking to */
Widget          toplevel;
Widget          xmDisplay;
Boolean         multiscreen = False;
char           *mwm_name = "mwm";
int		verbose = 0;

XContext        MwmContext;	/* context for mwm windows */
XContext        MenuContext;	/* context for mwm menus */

int             JunkX = 0, JunkY = 0;
Window          JunkRoot, JunkChild;	/* junk window */
unsigned int    JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;

Bool            debugging = False;
char          **g_argv;


int             fd_width, x_fd;
XtAppContext    app;
volatile int    alarmed;

static int             last_event_type = 0;

/****************************************************************************/

/*
 * print usage
 */
void
usage(void)
{
    fprintf(stderr, USAGE, VERSION);
}

/*
 * restart on a signal
 */
static RETSIGTYPE
sig_restart(int sig)
{
    MWM_Done(1, *g_argv); 
#if RETSIGTYPE == void
    return;
#else
    return (RETSIGTYPE)0;
#endif
}

/*
 * exit on a signal
 */
static RETSIGTYPE
sig_done(int nonsense)
{
    MWM_Done(0, NULL);
#if RETSIGTYPE == void
    return;
#else
    return (RETSIGTYPE) 0;
#endif
}

/*
 * For auto-raising windows, this routine is called
 */
static RETSIGTYPE
sig_alarm(int nonsense)
{
    alarmed = True;
    signal(SIGALRM, sig_alarm);

#if RETSIGTYPE == void
    return;
#else
    return (RETSIGTYPE)0;
#endif
}


/*
 * figures out if there's another WM running
 */
XErrorHandler
catch_redirect(Display * dpy, XErrorEvent * event)
{
    fprintf(stderr, "mwm: Error: Another WM is running\n");
    exit(1);
}

/*
 * displays info on internal errors
 */
XErrorHandler
general_error(Display * dpy, XErrorEvent * event)
{
    /* some errors are acceptable, mostly they're caused by 
     * trying to update a lost  window */
    if ((event->error_code == BadWindow) ||
	(event->request_code == X_GetGeometry) ||
	(event->error_code == BadDrawable) ||
	(event->request_code == X_SetInputFocus) ||
	(event->request_code == X_GrabButton) ||
	(event->request_code == X_ChangeWindowAttributes) ||
	(event->request_code == X_InstallColormap))
	return 0;


    fprintf(stderr, "internal error");
    fprintf(stderr, "      Request %d, Error %d\n", event->request_code,
	    event->error_code);
    fprintf(stderr, "      EventType: %d", last_event_type);
    fprintf(stderr, "\n");
    return 0;
}

/*
 * do global mwm initialization
 */
static void
initialize_mwm()
{
    int i;
    ScreenInfo *scr;

    xmDisplay = XmGetXmDisplay(dpy);

    RES_Initialize();

    if (Mwm.multi_screen)
	multiscreen = True;

    EVENT_Initialize();

    PROP_Initialize();

    MwmContext = XUniqueContext();
    MenuContext = XUniqueContext();

    if (multiscreen) {

	Mwm.number_of_screens = ScreenCount(dpy);

	Mwm.screen_info = (ScreenInfo **)XtMalloc(Mwm.number_of_screens *
						  sizeof(ScreenInfo *));
	for (i = 0; i < Mwm.number_of_screens; i++) {

	    scr = (ScreenInfo *)XtCalloc(1, sizeof(ScreenInfo));

	    scr->screen = i;
	    scr->root_win = RootWindow(dpy, scr->screen);
	    if (scr->root_win == None) {
		fprintf(stderr, "Screen %ld is not a valid screen", scr->screen);
		exit(1);
	    }

	    SCREEN_Initialize(scr);

	    Mwm.screen_info[i] = scr;
	}
    }
    else {
	Mwm.number_of_screens = 1;

	Mwm.screen_info = (ScreenInfo **)XtMalloc(Mwm.number_of_screens *
						  sizeof(ScreenInfo *));

	scr = (ScreenInfo *)XtCalloc(1, sizeof(ScreenInfo));

	scr->screen = DefaultScreen(dpy);
	scr->root_win = RootWindow(dpy, scr->screen);
	if (scr->root_win == None) {
	    fprintf(stderr, "Screen %ld is not a valid screen", scr->screen);
	    exit(1);
	}

	SCREEN_Initialize(scr);

	Mwm.screen_info[0] = scr;
    }
}

/*
 * set the appropriate error handler
 */
void
MWM_SetErrorHandler(int which)
{
    if (which == REDIRECT)
	XSetErrorHandler((XErrorHandler)catch_redirect);
    else
	XSetErrorHandler((XErrorHandler)general_error);
}

/*
 * cleanup and exit mwm
 */
void
MWM_Done(int restart, char *command)
{
    ScreenInfo *scr;
    int i, done, j;


    for (i = 0; i < Mwm.number_of_screens; i++) {
	PAGER_MoveViewPort(Mwm.screen_info[i], 0, 0, False);

	WIN_ReleaseWindows(Mwm.screen_info[i]);
    }

    /*
     * serious cleanup
     */
    if (restart) {
	for (i = 0; i < Mwm.number_of_screens; i++)  {

	    scr = Mwm.screen_info[i];
	    DT_SaveState(scr);

	    /* Really make sure that the connection is closed and cleared! */
	    XSelectInput(dpy, scr->root_win, 0);
	}

	XSync(dpy, 0);
	XCloseDisplay(dpy);

	i = 0;
	j = 0;
	done = 0;

	/* really need to destroy all windows, explicitly,
	 * not sleep, but this is adequate for now */
	sleep(1);
	ReapChildren();

	execvp(command, g_argv);
	fprintf(stderr, "MWM: Call of '%s' failed!!!!\n", command);

	execvp(g_argv[0], g_argv);	/* that _should_ work */
	fprintf(stderr, "MWM: Call of '%s' failed!!!!\n", g_argv[0]);
    }
    else {

	for (i = 0; i < Mwm.number_of_screens; i++)
	    PROP_ClearBehavior(Mwm.screen_info[i]);

	XCloseDisplay(dpy);
	exit(0);
    }
}

/*
 * main - start of mwm
 */
void
main(int argc, char **argv)
{
    int i;
    int len;
    char *display_string;
    Bool option_error = False;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Mwm", NULL, 0, &argc, argv, NULL, NULL);
    dpy = XtDisplay(toplevel);

    for (i = 1; i < argc; i++) {
	if (strncasecmp(argv[i], "-debug", 6) == 0)
	    debugging = True;
	else if (strncasecmp(argv[i], "-multiscreen", 12) == 0) {
	    multiscreen = True;
	}
	else if (strncasecmp(argv[i], "-name", 5) == 0) {
	    if (++i >= argc)
		usage();
	    mwm_name = argv[i];
	}
	else if (strncasecmp(argv[i], "-version", 8) == 0) {
	    fprintf(stderr, "Mwm Version %s\n", VERSION);
	}
	else if (strncasecmp(argv[i], "-V", 2) == 0) {
	    verbose = 1;
	}
	else {
	    fprintf(stderr, "mwm:  Unknown option:  `%s'\n", argv[i]);
	    option_error = TRUE;
	}
    }

    if (option_error)
	usage();

    g_argv = argv;

    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
	signal(SIGINT, sig_done);
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
	signal(SIGHUP, sig_done);
    if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
	signal(SIGQUIT, sig_done);
    if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
	signal(SIGTERM, sig_done);

    signal(SIGUSR1, sig_restart);
    signal(SIGALRM, sig_alarm);

#ifdef HAVE_SYSCONF
    fd_width = sysconf(_SC_OPEN_MAX);
#else
#ifdef HAVE_GETDTABLESIZE
    fd_width = getdtablesize();
#else
#error You lose
#endif
#endif
    x_fd = XConnectionNumber(dpy);

    /*
     * this is enormously dangerous, but _is_ the original code. MLM
     */
    if (fcntl(x_fd, F_SETFD, 1) == -1) {
	fprintf(stderr, "close-on-exec failed");
	exit(1);
    }

    /*
     * Add a DISPLAY entry to the environment, in case we were started
     * with mwm -display term:0.0
     */
    len = strlen(XDisplayString(dpy));
    display_string = XtMalloc(len + 10);
    sprintf(display_string, "DISPLAY=%s", XDisplayString(dpy));
    putenv(display_string);

    /*
     * Add a HOSTDISPLAY environment variable, which is the same as
     * DISPLAY, unless display = :0.0 or unix:0.0, in which case the full
     * host name will be used for ease in networking .
     */
    if (strncmp(display_string, "DISPLAY=:", 9) == 0) {
	char            client[MAXHOSTNAME], *rdisplay_string;

	gethostname(client, MAXHOSTNAME);
	rdisplay_string = XtMalloc(len + 14 + strlen(client));
	sprintf(rdisplay_string, "HOSTDISPLAY=%s:%s", client, &display_string[9]);
	putenv(rdisplay_string);
    }
    else if (strncmp(display_string, "DISPLAY=unix:", 13) == 0) {
	char            client[MAXHOSTNAME], *rdisplay_string;

	gethostname(client, MAXHOSTNAME);
	rdisplay_string = XtMalloc(len + 14 + strlen(client));
	sprintf(rdisplay_string, "HOSTDISPLAY=%s:%s", client,
		&display_string[13]);
	putenv(rdisplay_string);
    }
    else {
	char           *rdisplay_string;

	rdisplay_string = XtMalloc(len + 14);
	sprintf(rdisplay_string, "HOSTDISPLAY=%s", XDisplayString(dpy));
	putenv(rdisplay_string);
    }

    initialize_mwm();

    while (TRUE) {
	XEvent   event;

	last_event_type = 0;
	if (EVENT_Next(&event)) {
	    EVENT_Dispatch(&event);
	}
    }

    exit(0);
}
