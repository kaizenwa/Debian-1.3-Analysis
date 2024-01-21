#ifndef lint
static char	*RCSid = "$Id: xpostit.c,v 2.0 1995/03/27 18:56:22 mjhammel Exp $";
#endif

/*
 * xpostit.c - Post-It Notes for the X Window System.
 *
 * Based on an X10R4 application.  This one is for X11R4 and uses the
 * Xt toolkit and the Athena widgets.
 *
 * David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * Modified by
 * Michael J. Hammel (03/01/95)
 * Contractor
 * 1150 Inca St. TH 70
 * Denver, CO 80204
 * mjhammel@csn.org
 *
 * $Log: xpostit.c,v $
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 *
 * Revision 1.2  90/06/14  11:21:24  davy
 * Ported to X11 Release 4.
 * 
 * Revision 1.1  90/06/13  09:48:51  davy
 * Initial revision
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/SimpleMenu.h>
#include <signal.h>
#include <stdio.h>

#include "xpostit.h"
#include "version.h"

/*
 * Command line options and the resources they set.
 */
static XrmOptionDescRec options[] = {
    { "-bs",	".bufSize",		XrmoptionSepArg,	NULL },
    { "-dir",	".noteDir",		XrmoptionSepArg,	NULL },
    { "-interval",	".interval",	XrmoptionSepArg,	NULL },
    { "-sb",	".scrollBar",		XrmoptionNoArg,		"true" },
    { "-sv",	".saveNotes",		XrmoptionNoArg,		"true" },
    { "-c",	".compatibility",	XrmoptionNoArg,		"true" },
    { "-nw",	".nameWidth",		XrmoptionSepArg,	NULL },
    { "-ns",	".noSave",		XrmoptionNoArg,		"true" },
    { "-na",	".noAlarm",		XrmoptionNoArg,		"true" },
    { "-help",	".help",		XrmoptionNoArg,		"true" },
    { "-version",	".version",		XrmoptionNoArg,		"true" },
    { "-?",	".help",		XrmoptionNoArg,		"true" },
    { "-ao",	".anchorOffset",	XrmoptionSepArg,	NULL },
    { "-tmpdir",	".tmpDir",	XrmoptionSepArg,	NULL },
    { "-printcmd",	".printCmd",	XrmoptionSepArg,	NULL },
    { "-calendarcmd",	".calendarCmd",	XrmoptionSepArg,	NULL },
    { "-emailcmd",	".emailCmd",	XrmoptionSepArg,	NULL },
    { "-homedir",	".homeDir",	XrmoptionSepArg,	NULL },
};

/*
 * Fallback resources.
 */
static String fallback_resources[] = {
#include "app_defaults.h"
    NULL
};
	
/*
 * Resources we maintain besides those maintained by the toolkit.
 */
static XtResource resources[] = {
#define offset(field)	XtOffset(AppResPtr,field)
    { "bufSize", "BufSize", XtRInt, sizeof(int),
      offset(buf_size), XtRImmediate, (caddr_t) DefaultBufSize },
    { "noteDir", "NoteDir", XtRString, sizeof(String),
      offset(note_dir), XtRString, DefaultNoteDir },
    { "interval", "Interval", XtRInt, sizeof(int),
      offset(interval), XtRImmediate, (caddr_t)DefaultInterval },
    { "saveNotes", "SaveNotes", XtRBoolean, sizeof(Boolean),
      offset(save_notes), XtRImmediate, (caddr_t) False },
    { "scrollBar", "Scroll", XtRBoolean, sizeof(Boolean),
      offset(scroll_bar), XtRImmediate, (caddr_t) False },
    { "compatibility", "Compatibility", XtRBoolean, sizeof(Boolean),
      offset(compatibility), XtRImmediate, (caddr_t) False },
    { "nameWidth", "NameWidth", XtRInt, sizeof(int),
      offset(name_width), XtRImmediate, (caddr_t) DefaultNameWidth },
    { "noSave", "NoSave", XtRBoolean, sizeof(Boolean),
      offset(nosave), XtRImmediate, (caddr_t) False },
    { "noAlarm", "NoAlarm", XtRBoolean, sizeof(Boolean),
      offset(noalarm), XtRImmediate, (caddr_t) False },
    { "help", "Help", XtRBoolean, sizeof(Boolean),
      offset(help), XtRImmediate, (caddr_t) False },
    { "version", "Version", XtRBoolean, sizeof(Boolean),
      offset(version), XtRImmediate, (caddr_t) False },
    { "anchorOffset", "AnchorOffset", XtRInt, sizeof(int),
      offset(anchor_offset), XtRImmediate, (caddr_t) DefaultAnchorOffset },
    { "tmpDir", "TmpDir", XtRString, sizeof(String),
      offset(tmp_dir), XtRString, (caddr_t) DefaultTmpDir },
    { "printCmd", "PrintCmd", XtRString, sizeof(String),
      offset(print_cmd), XtRString, (caddr_t) DefaultPrintCmd },
    { "calendarCmd", "CalendarCmd", XtRString, sizeof(String),
      offset(calendar_cmd), XtRString, (caddr_t) DefaultCalendarCmd },
    { "emailCmd", "EmailCmd", XtRString, sizeof(String),
      offset(email_cmd), XtRString, (caddr_t) DefaultEmailCmd },
    { "homeDir", "HomeDir", XtRString, sizeof(String),
      offset(home_dir), XtRString, NULL },
#undef offset
};


AppRes	app_res;		/* xpostit application resources	*/
Widget	toplevel;		/* top level application shell widget	*/
Screen	*screen;		/* pointer to the screen of the display	*/
Display	*display;		/* pointer to the display we're on	*/
XtAppContext appcontext;	/* application context			*/
int curr_screenx, curr_screeny; /* size of current screen */
XtIntervalId timer;		/* used for auto-save feature */
XtIntervalId alarm_timer;	/* used for alarms feature */

unsigned long timer_interval; 	/* auto-save interval */
unsigned long alarm_interval; 	/* alarm check interval */

static Atom wm_delete_window;
static Atom wm_protocols;

void WMProtocols(w, ev, params, n)
	Widget	w;
	XEvent	*ev;
	String	*params;
	Cardinal	*n;
{
	if (ev->type == ClientMessage &&
			ev->xclient.message_type == wm_protocols &&
			ev->xclient.data.l[0] == wm_delete_window) {

		ByeBye();
	}
}

static XtActionsRec actions[] = {
	{ "WMProtocols", WMProtocols }
};


void
main(argc, argv)
char **argv;
int argc;
{
	char *appname;
	/* char *rindex();
	 */
	Boolean setsigs = False;

	/*
	 * Ignore signals for now, but record whether they were
	 * already ignored or not so we can catch them later if
	 * need be.
	 */
	if ((signal(SIGQUIT, SIG_IGN) != SIG_IGN) &&
	    (signal(SIGINT, SIG_IGN) != SIG_IGN))
		setsigs = True;

	/*
	 * Get application name.
	 */
	if ((appname = rindex(*argv, '/')) == NULL)
		appname = *argv;
	else
		appname++;

	/*
	 * Initialize the toolkit and create an application shell.
	 */
	toplevel = XtAppInitialize(&appcontext, PostItNoteClass, options,
			XtNumber(options), &argc, argv, fallback_resources,
			NULL, 0);

	display = XtDisplay(toplevel);
	screen = DefaultScreenOfDisplay(display);
	curr_screenx = HeightOfScreen(screen);
	curr_screeny = WidthOfScreen(screen);

	/*
	 * If we need to handle keyboard signals, do it now.
	 */
	if (setsigs) {
		signal(SIGQUIT, ByeBye);
		signal(SIGINT, ByeBye);
	}

	/*
	 * Always handle these.
	 */
	signal(SIGTERM, ByeBye);
	signal(SIGHUP, ByeBye);

	/*
	 * Send X errors to the exit routine.
	 */
	XSetErrorHandler((XErrorHandler) ByeBye);

	/*
	 * Now get any resources we're interested in.
	 */
	XtGetApplicationResources(toplevel, &app_res, resources,
				  XtNumber(resources), (ArgList)argv, (Cardinal)argc);

	XtAppAddActions (appcontext, actions, XtNumber (actions));

	/*
	 * if the user requested help, provide it
	 */
	if ( app_res.help )
	{
		printf( USAGE );
		exit ( 0 );
	}
	/*
	 * if the user requested the version, provide it
	 */
	if ( app_res.version )
	{
		printf( "xpostit+ v%s\n", VERSION );
		printf( "Copyright 1994-1996 Michael J. Hammel\n\n");
		exit ( 0 );
	}
 
	/*
	 * Construct the path to the directory notes are
	 * stored in.
	 */
	SetNoteDir();

	/*
	 * Create the plaid, menu, and list widgets.
	 */
	CreatePlaidWidget();
	CreateMenuWidget();

	/*
	 * Let the top level shell know about the menus.
	 */
	XawSimpleMenuAddGlobalActions(appcontext);
	XtRealizeWidget(menuwidget);

	/*
	 * Realize the top level and flush the server, which will
	 * let the user position the plaid window and map it.
	 */
	XtRealizeWidget(toplevel);
	XFlush(display);

	/*
	 * Load the notes the user has saved, and create widgets
	 * for them.
	 */
	LoadSavedNotes();

	/*
	 * Start the autosave timer, unless requested not to
	 */
	if ( !app_res.nosave )
	{
		timer_interval = app_res.interval * 60 * 1000;
		timer = XtAppAddTimeOut (
				XtWidgetToApplicationContext(toplevel),
				timer_interval,
				AutoSave,
				NULL );
	}

	/*
	 * Start the alarm timer (once a minute)
	 */
	if ( !app_res.noalarm )
	{
		alarm_interval = 60 * 1000;
		alarm_timer = XtAppAddTimeOut (
			XtWidgetToApplicationContext(toplevel),
			alarm_interval,
			AlarmCheck,
			NULL );
	}

	wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW",
		False);
	wm_protocols = XInternAtom(XtDisplay(toplevel), "WM_PROTOCOLS", False);
	(void) XSetWMProtocols(XtDisplay(toplevel), XtWindow(toplevel),
		&wm_delete_window, 1);

	/*
	 * Never returns.
	 */
	XtAppMainLoop(appcontext);
}
