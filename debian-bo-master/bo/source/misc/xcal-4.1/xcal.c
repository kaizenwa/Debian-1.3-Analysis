#ifndef lint
static char    *sccsid = "@(#)xcal.c	3.53 (Hillside Systems) 9/13/95";
static char    *copyright = "@(#)Copyright 1989,1990,1993 Peter Collinson, Hillside Systems";
#endif				/* lint */
/***

* program name:
	xcal.c
* function:
	display the current calendar date
	if pressed as a button go into strip calendar mode
* switches:
	-format str	use str as a main display format
	-debug		run quickly incrementing time - 1 day per sec
	-alarmscan	print alarm debug info
	-format		Set date format of top level box
	-stripfmt	Set date format of strip
	-editfmt	Set date format of edit boxes
	-clocktick	Set clock tick rate.
	-u		Look at another user's calendar
* libraries used:
	libXaw.a, libXmu.a libXt.a libX11.a
* compile time parameters:
	standard
* history:
	Written November 1989
	Hacked again October 1993
	Peter Collinson
	Hillside Systems
* (C) Copyright: 1989 Hillside Systems/Peter Collinson
	
	Permission to use, copy, modify, and distribute this software
	and its documentation for any purpose is hereby granted to
	anyone, provided that the above copyright notice appear
	in all copies and that both that copyright notice and this
	permission notice appear in supporting documentation, and that
	the name of Peter Collinson not be used in advertising or
	publicity pertaining to distribution of the software without
	specific, written prior permission.  Hillside Systems makes no
	representations about the suitability of this software for any
	purpose.  It is provided "as is" without express or implied
	warranty.
	
	Peter Collinson DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
	SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
	AND FITNESS, IN NO EVENT SHALL Peter Collinson BE LIABLE FOR
	ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
	WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
	WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
	ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
	PERFORMANCE OF THIS SOFTWARE.

***/
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <X11/Intrinsic.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include "xcal.h"

char            date_area[BUFSIZ];

/* command line options specific to the application */
static XrmOptionDescRec Options[] = {
	{"-debug", "debug", XrmoptionNoArg, (caddr_t) "TRUE"},
	{"-alarmscan", "alarmScan", XrmoptionNoArg, (caddr_t) "TRUE"},
	{"-format", "format", XrmoptionSepArg, NULL},
	{"-stripfmt", "stripFmt", XrmoptionSepArg, NULL},
	{"-editfmt", "editFmt", XrmoptionSepArg, NULL},
	{"-clocktick", "clockTick", XrmoptionSepArg, NULL},
	{"-u", "otherUser", XrmoptionSepArg, NULL},
};

struct resources appResources;

Pixmap          HelpPix;
Pixmap          HelpPressPix;
Pixmap          IconPix;
Pixmap          MouseOnPix;
Pixmap          MouseOffPix;

XtAppContext	appContext;

#define offset(field) XtOffset(struct resources *, field)

static XtResource Resources[] = {
	{"debug", "Debug", XtRBoolean, sizeof(Boolean),
	offset(debug), XtRString, "False"},
        {"otherUser", "OtherUser", XtRString, sizeof(String),
        offset(otheruser), XtRString, NULL},
	{"alarmScan", "AlarmScan", XtRBoolean, sizeof(Boolean),
	offset(alarmScan), XtRString, "False"},
	{"reverseVideo", "ReverseVideo", XtRBoolean, sizeof(Boolean),
	offset(reverseVideo), XtRString, "False"},
	{"xcalendarCompat", "XcalendarCompat", XtRBoolean, sizeof(Boolean),
	offset(calCompat), XtRString, "False"},
	{"giveHelp", "GiveHelp", XtRBoolean, sizeof(Boolean),
	offset(giveHelp), XtRString, "True"},
	{"helpFromFile", "HelpFromFile", XtRBoolean, sizeof(Boolean),
	offset(helpFromFile), XtRString, "True"},
        {"helpFile", "HelpFile", XtRString, sizeof(String),
        offset(helpfile), XtRString, "/usr/lib/X11/app-defaults/XCal.help"},
	{"useMemo", "UseMemo", XtRBoolean, sizeof(Boolean),
	offset(useMemo), XtRString, "True"},
	{"memoLeft", "MemoLeft", XtRBoolean, sizeof(Boolean),
	offset(memoLeft), XtRString, "True"},
	{"initialCalendar", "InitialCalendar", XtRBoolean, sizeof(Boolean),
	offset(initialCalendar), XtRString, "False"},
	{"initialEdit", "InitialEdit", XtRBoolean, sizeof(Boolean),
	offset(initialEdit), XtRString, "False"},
	{"initialMemo", "InitialMemo", XtRBoolean, sizeof(Boolean),
	offset(initialMemo), XtRString, "False"},
        {"format", "Format", XtRString, sizeof(String),
        offset(format), XtRString, "%A %d %B %Y"},
        {"stripFmt", "StripFmt", XtRString, sizeof(String),
        offset(stripfmt), XtRString, "%B %Y"},
        {"editFmt", "EditFmt", XtRString, sizeof(String),
        offset(editfmt), XtRString, "%A %d %B %Y"},
	{"clockTick", "ClockTick", XtRInt, sizeof(int),
	offset(clocktick), XtRString, "0"},
	{"markToday", "MarkToday", XtRBoolean, sizeof(Boolean),
	offset(markToday), XtRString, "True"},
	{"fontToday", "FontToday", XtRFontStruct, sizeof(XFontStruct *),
	offset(fontToday), XtRString, "XtDefaultFont"},
	{"todayForeground", "TodayForeground", XtRPixel, sizeof(Pixel),
	offset(today.fg), XtRString, "White"},
	{"todayBackground", "TodayBackground", XtRPixel, sizeof(Pixel),
	offset(today.bg), XtRString, "Black"},
	{"directory", "Directory", XtRString, sizeof(String),
	offset(directory), XtRString, "Calendar"},
	{"textBufferSize", "TextBufferSize", XtRInt, sizeof(int),
	offset(textbufsz), XtRString, "2048"},
	{"useWmTitle", "UseWmTitle", XtRBoolean, sizeof(Boolean),
	offset(useWmTitle), XtRString, "True"},
	{"minStripWidth", "MinStripWidth", XtRDimension, sizeof(Dimension),
	offset(minstripwidth), XtRString, "0"},
	/* set to screen size in the code */
	{"maxStripHeight", "MaxStripHeight", XtRDimension, sizeof(Dimension),
	offset(maxstripheight), XtRString, "0"},
	{"january", "January", XtRString, sizeof(String),
	offset(mon[0]), XtRString, "January"},
	{"february", "February", XtRString, sizeof(String),
	offset(mon[1]), XtRString, "February"},
	{"march", "March", XtRString, sizeof(String),
	offset(mon[2]), XtRString, "March"},
	{"april", "April", XtRString, sizeof(String),
	offset(mon[3]), XtRString, "April"},
	{"may", "May", XtRString, sizeof(String),
	offset(mon[4]), XtRString, "May"},
	{"june", "June", XtRString, sizeof(String),
	offset(mon[5]), XtRString, "June"},
	{"july", "July", XtRString, sizeof(String),
	offset(mon[6]), XtRString, "July"},
	{"august", "August", XtRString, sizeof(String),
	offset(mon[7]), XtRString, "August"},
	{"september", "September", XtRString, sizeof(String),
	offset(mon[8]), XtRString, "September"},
	{"october", "October", XtRString, sizeof(String),
	offset(mon[9]), XtRString, "October"},
	{"november", "November", XtRString, sizeof(String),
	offset(mon[10]), XtRString, "November"},
	{"december", "December", XtRString, sizeof(String),
	offset(mon[11]), XtRString, "December"},
	{"jan", "Jan", XtRString, sizeof(String),
	offset(smon[0]), XtRString, "Jan"},
	{"feb", "Feb", XtRString, sizeof(String),
	offset(smon[1]), XtRString, "Feb"},
	{"mar", "Mar", XtRString, sizeof(String),
	offset(smon[2]), XtRString, "Mar"},
	{"apr", "Apr", XtRString, sizeof(String),
	offset(smon[3]), XtRString, "Apr"},
	{"may", "May", XtRString, sizeof(String),
	offset(smon[4]), XtRString, "May"},
	{"jun", "Jun", XtRString, sizeof(String),
	offset(smon[5]), XtRString, "Jun"},
	{"jul", "Jul", XtRString, sizeof(String),
	offset(smon[6]), XtRString, "Jul"},
	{"aug", "Aug", XtRString, sizeof(String),
	offset(smon[7]), XtRString, "Aug"},
	{"sep", "Sep", XtRString, sizeof(String),
	offset(smon[8]), XtRString, "Sep"},
	{"oct", "Oct", XtRString, sizeof(String),
	offset(smon[9]), XtRString, "Oct"},
	{"nov", "Nov", XtRString, sizeof(String),
	offset(smon[10]), XtRString, "Nov"},
	{"dec", "Dec", XtRString, sizeof(String),
	offset(smon[11]), XtRString, "Dec"},
	{"sunday", "Sunday", XtRString, sizeof(String),
	offset(day[0]), XtRString, "Sunday"},
	{"monday", "Monday", XtRString, sizeof(String),
	offset(day[1]), XtRString, "Monday"},
	{"tuesday", "Tuesday", XtRString, sizeof(String),
	offset(day[2]), XtRString, "Tuesday"},
	{"wednesday", "Wednesday", XtRString, sizeof(String),
	offset(day[3]), XtRString, "Wednesday"},
	{"thursday", "Thursday", XtRString, sizeof(String),
	offset(day[4]), XtRString, "Thursday"},
	{"friday", "Friday", XtRString, sizeof(String),
	offset(day[5]), XtRString, "Friday"},
	{"saturday", "Saturday", XtRString, sizeof(String),
	offset(day[6]), XtRString, "Saturday"},
	{"sun", "Sun", XtRString, sizeof(String),
	offset(sday[0]), XtRString, "Sun"},
	{"mon", "Mon", XtRString, sizeof(String),
	offset(sday[1]), XtRString, "Mon"},
	{"tue", "Tue", XtRString, sizeof(String),
	offset(sday[2]), XtRString, "Tue"},
	{"wed", "Wed", XtRString, sizeof(String),
	offset(sday[3]), XtRString, "Wed"},
	{"thu", "Thu", XtRString, sizeof(String),
	offset(sday[4]), XtRString, "Thu"},
	{"fri", "Fri", XtRString, sizeof(String),
	offset(sday[5]), XtRString, "Fri"},
	{"sat", "Sat", XtRString, sizeof(String),
	offset(sday[6]), XtRString, "Sat"},
	{"weekly", "Weekly", XtRString, sizeof(String),
	offset(weekly), XtRString, "Weekly"},
	{"alarms", "Alarms", XtRBoolean, sizeof(Boolean),
	offset(alarms), XtRString, "True"},
	{"execAlarms", "ExecAlarms", XtRBoolean, sizeof(Boolean),
 	offset(execalarms), XtRString, "True"},
	{"alarmWarp", "AlarmWarp", XtRBoolean, sizeof(Boolean),
	offset(alarmWarp), XtRString, "False"},
	{"minAlarmWarp", "MinAlarmWarp", XtRInt, sizeof(int),
	offset(minAlarmWarp), XtRString, "7"},
	{"update", "Update", XtRInt, sizeof(int),
	offset(update), XtRString, "0"},
	{"volume", "Volume", XtRInt, sizeof(int),
	offset(volume), XtRString, "50"},
	{"nbeeps", "Nbeeps", XtRInt, sizeof(int),
	offset(nbeeps), XtRString, "3"},
	{"cmd", "Cmd", XtRString, sizeof(String),
	offset(cmd), XtRString, NULL},
	{"countdown", "Countdown", XtRString, sizeof(String),
	offset(countdown), XtRString, "10,0"},
	{"autoquit", "Autoquit", XtRInt, sizeof(int),
	offset(autoquit), XtRString, "120"},
	{"alarmleft", "Alarmleft", XtRString, sizeof(String),
	offset(alarmleft), XtRString, "%d minutes before..."},
	{"alarmnow", "Alarmnow", XtRString, sizeof(String),
	offset(alarmnow), XtRString, "Time is now..."},
	{"private", "Private", XtRString, sizeof(String),
	offset(private), XtRString, "Private calendar entry"},
	{"already", "Already", XtRString, sizeof(String),
	offset(already), XtRString, "Already editing %d %B %Y"},
	{"alreadyWeekly", "AlreadyWeekly", XtRString, sizeof(String),
	offset(alreadyWeekly), XtRString, "Already editing %A"},
	{"memoFile", "MemoFile", XtRString, sizeof(String),
	offset(memoFile), XtRString, "memo"},
	{"maxDisplayLines", "MaxDisplayLines", XtRInt, sizeof(int),
	offset(maxDisplayLines), XtRString, "5"},
};

static XtCallbackRec callbacks[] = {
	{NULL, NULL},
	{NULL, NULL},
};
#define ClearCallbacks() memset((caddr_t)callbacks, '\0', sizeof (callbacks))

static XtActionsRec appActions[] = {
	{"setdate", SetDate},
	{"leave", AskLeave},
	{"SetDateAction", TextCal},
	{"LoadDateAction", LoadDateStrip },
};

static String   defTranslations =
"<Btn2Down>: set()\n\
	<Btn2Up>:setdate() unset()\n\
	<Btn3Down>: set()\n\
	<Btn3Up>: leave() unset()";

Widget          toplevel;

Widget          mHelp;		/* popup help widget */

Date            today;

int		updatefreq;	/* clock tick on the top level widget */

/*
 * external routines
 */
extern void     MainHelp();

/*
 * Forward routines local to this file
 */
void		AlterTitles();
static void	ConvDate();
static void     MkDate();
static void     DebugMkDate();
static void     PixInit();
static int	CycleDays();
static int	CycleMonths();
static void	SetUpdateFreq();
static int	UpdateFreq();
static void	AdjustHeights();

/*
 *	Create the three components of the date strip
 */
static Widget	MemoWidget();
static Widget	DateWidget();
static Widget	HelpWidget();

/*
 * Your compiler may complain about the fact that some of the
 * characters in these bitmaps are eight bits.
 * If this worries you 
 * #define UNSIGNED unsigned
 */
#define UNSIGNED

#include "calendar.bm"
#include "help.bm"
#include "help_press.bm"
#include "mouse.bm"
#include "mouseaway.bm"
#undef UNSIGNED

/* defines fallbackResources */
#include "xcal_ad.h"

void
main(argc, argv)
	int    		argc;
	char           *argv[];
{

	Widget          parent;
	Widget          memo;
	Widget          lab;
	static Arg      iconArg[] = {
		{XtNiconPixmap, (XtArgVal) NULL},
	};


	toplevel = XtAppInitialize(&appContext, "XCal",
				Options, XtNumber(Options), &argc, argv,
				fallbackResources, NULL, 0);

	PixInit(toplevel);
	iconArg[0].value = IconPix;
	XtSetValues(toplevel, iconArg, XtNumber(iconArg));

	if (argc != 1)
		fprintf(stderr, "%s: Error in arguments\n", argv[0]);

	XtGetApplicationResources(toplevel, (caddr_t) & appResources, Resources,
				  XtNumber(Resources), (ArgList) NULL, 0);

	/*
	 * If reverse video invert default colour settings
	 */
	if (appResources.reverseVideo) {
		Colour          old;
		old = appResources.today;
		appResources.today.fg = old.bg;
		appResources.today.bg = old.fg;
	}
	
	if (appResources.otheruser)
		AlterTitles();
	
	InitMonthEntries();

	/* get a maximum initial size of the box */
	DoTemplate(date_area, sizeof(date_area), appResources.format);

	parent = XtVaCreateManagedWidget("form", formWidgetClass, toplevel,
			 		  XtNborderWidth, 0,
				 	  XtNdefaultDistance, 0,
					  NULL);
	if (appResources.useMemo) {
		if (appResources.memoLeft) {
			memo = MemoWidget(parent, NULL);
			lab = DateWidget(parent, memo);
			if (appResources.giveHelp)
				mHelp = HelpWidget(parent, lab);
		} else {
			lab = DateWidget(parent, NULL);
			memo = MemoWidget(parent, lab);
			if (appResources.giveHelp)
				mHelp = HelpWidget(parent, memo);
		}
	} else {
		lab = DateWidget(parent, NULL);
		if (appResources.giveHelp)
			mHelp = HelpWidget(parent, lab);
	}

	AdjustHeights(appResources.useMemo ? memo : NULL,
		      lab,
		      appResources.giveHelp ? mHelp : NULL);	

	XtSetMappedWhenManaged(toplevel, False);

	XtRealizeWidget(toplevel);	/* set the default geometry */

	SetUpdateFreq(appResources.format);

	if (appResources.debug) {
		fprintf(stderr, "Debug ON\n");
		DebugMkDate(lab);
	} else
		MkDate(lab);

	XtAppAddActions(appContext, appActions, 4);	/* register actions */
	XtAugmentTranslations(lab, XtParseTranslationTable(defTranslations));

	XtMapWidget(toplevel);

	if (appResources.initialCalendar)
		DoCalendar(lab, NULL, NULL);

	if (appResources.initialEdit) {
		MonthEntry     *me;

		me = GetMonthEntry(today.year, today.month);
		if (me->me_have[today.day])
			StartEditing(lab, &today, NULL);
	}
	if (appResources.useMemo && appResources.initialMemo)
		DoMemo(memo, NULL, NULL);

	InitAlarms();

	XtAppMainLoop(appContext);
	exit(0);
}

/*
 *	Make three widgets for the main strip
 */
static Widget
MemoWidget(parent, horiz)
	Widget parent;
	Widget horiz;
{
	Widget ret;
	
	callbacks[0].callback = DoMemo;
	ret = XtVaCreateManagedWidget("today", commandWidgetClass, parent,
				XtNcallback, (XtArgVal) callbacks,
				XtNfromHoriz, horiz,
				XtNleft, XtChainLeft,
				XtNright, XtRubber,
				XtNborderWidth, 0,
				XtNbitmap, MouseOnPix,
				NULL);
	ClearCallbacks();
	return ret;
}

static Widget
DateWidget(parent, horiz)
	Widget	parent, horiz;
{
	Widget	ret;

	callbacks[0].callback = DoCalendar;
	ret = XtVaCreateManagedWidget("date", commandWidgetClass, parent,
				XtNlabel, (XtArgVal) date_area,
				XtNcallback, (XtArgVal) callbacks,
				XtNfromHoriz, horiz,
				XtNleft, XtChainLeft,
				XtNright, XtRubber,
				XtNborderWidth, 0,
				NULL);
	ClearCallbacks();
	return ret;
}

static Widget
HelpWidget(parent, horiz)
	Widget	parent, horiz;
{
	Widget	ret;

	callbacks[0].callback = MainHelp;
	ret = XtVaCreateManagedWidget("mainHelp", commandWidgetClass, parent,
				XtNcallback, (XtArgVal) callbacks,
				XtNfromHoriz, horiz,
				XtNleft, XtRubber,
				XtNright, XtChainRight,
				XtNborderWidth, 0,
				XtNbitmap, HelpPix,
				NULL);
	ClearCallbacks();
	return ret;
}
		
/*
 * Initialise Pixmaps
 */
static void
PixInit(toplevel)
	Widget          toplevel;
{
	Display        *theDisplay = XtDisplay(toplevel);

	HelpPix = XCreateBitmapFromData(theDisplay,
					DefaultRootWindow(theDisplay),
					help_bits, help_width, help_height);
	HelpPressPix = XCreateBitmapFromData(theDisplay,
					     DefaultRootWindow(theDisplay),
		      help_press_bits, help_press_width, help_press_height);
	IconPix = XCreateBitmapFromData(theDisplay,
					DefaultRootWindow(theDisplay),
					cal_bits, cal_width, cal_height);
	MouseOnPix = XCreateBitmapFromData(theDisplay,
					   DefaultRootWindow(theDisplay),
			    (char *) mouse_bits, mouse_width, mouse_height);
	MouseOffPix = XCreateBitmapFromData(theDisplay,
					    DefaultRootWindow(theDisplay),
		(char *) mouseaway_bits, mouseaway_width, mouseaway_height);
}

/*
 *	Adjust height of the command box
 */
static void
AdjustHeights(memo, date, help)
	Widget	memo;
	Widget	date;
	Widget	help;
{
	Dimension hm, hd, hh, max;

	hm = memo ? wHeight(memo): 0;
	hd = date ? wHeight(date): 0;
	hh = help ? wHeight(help): 0;
	
	max = hm;
	max = (hd > max) ? hd : max;
	max = (hh > max) ? hh : max;

	if (hm && hm < max)
		SetWidgetHeightMax(memo, hm, max);
	if (hd && hd < max)
		SetWidgetHeightMax(date, hd, max);
	if (hh && hh < max)
		SetWidgetHeightMax(help, hh, max);
}

void
SetWidgetHeightMax(w, h, max)
	Widget		w;
	Dimension	 h;
	Dimension 	max;
{
	Arg	args[3];
	int	adj;

	adj = ((int)max - (int)h)/2;
	if (adj > 0) {
		XtSetArg(args[0], XtNvertDistance, adj);
		XtSetArg(args[1], XtNfromVert, NULL);
		XtSetValues(w, args, 2);
	}
}

/*
 * If we are not dealing with our calendar entries add
 * (user-name)
 * to the end of the date format strings
 */
void
AlterTitles()
{
	char	us[16];
	char	fmt[256];

	(void) sprintf(us, " (%s)", appResources.otheruser);

	/* I am unsure whether it is safe to free these strings */
#define cstr(v) { strcpy(fmt, v); strcat(fmt, us); v = XtNewString(fmt); }
	cstr(appResources.format);
	cstr(appResources.stripfmt);
	cstr(appResources.editfmt);
#undef cstr
}
	
/*
 * Flip mouse state
 */
void
MouseShow(w, OnOff)
	Widget          w;
	Boolean         OnOff;
{
	Arg             arg[1];

	XtSetArg(arg[0], XtNbitmap, OnOff ? MouseOnPix : MouseOffPix);
	XtSetValues(w, arg, 1);
}

/*
 * Flip help state
 */
void
HelpShow(w, Pressed)
	Widget          w;
	Boolean         Pressed;
{
	Arg             arg[1];

	XtSetArg(arg[0], XtNbitmap, Pressed ? HelpPressPix : HelpPix);
	XtSetValues(w, arg, 1);
}


/*
 * Exit routine
 */
void
Leave(retval)
	int             retval;
{
	exit(retval);
}

/************************************************************************/
/*									*/
/*									*/
/*      This deals with the top level date `icon'			*/
/*									*/
/*									*/
/************************************************************************/
/*
 * Time management code
 * Set up a Date structure from today's information
 */
static void
ConvDate(tm, dp)
        struct tm      *tm;
        Date           *dp;
{
        dp->day = tm->tm_mday;
        dp->month = tm->tm_mon;
        dp->year = tm->tm_year + 1900;
        dp->wday = tm->tm_wday;
}

void
FmtTime(tm, buf, len, fmt)
	struct tm 	*tm;
	char		*buf;
	int		 len;
	char		*fmt;
	
{
	if (strftime(buf, len, fmt, tm) == 0) {
		strftime(buf, len, "%A %e %B %Y", tm);
	}
}

void
FmtDate(da, buf, len, fmt)
	Date	*da;
	char	*buf;
	int	len;
	char	*fmt;
{
	if (strfdate(buf, len, fmt, da) == 0) {
		strfdate(buf, len, "%A %e %B %Y", da);
	}
}
	
static void
DebugMkDate(w)
	Widget          w;
{
	static long     ti;
	struct tm      *tm;
	Date            yesterday;
	static		firsttime;

	yesterday = today;

	if (ti == 0)
		(void) time(&ti);
	else 
		ti += updatefreq;
	tm = localtime(&ti);

        ConvDate(tm, &today);

	FmtTime(tm, date_area, sizeof(date_area), appResources.format);

	XtVaSetValues(w, XtNlabel, (XtArgVal) date_area, NULL);

	if (firsttime != 0) {
		if (yesterday.day != today.day) {
			ChangeHighlight(&yesterday, &today);
			AlarmFilePoll(tm);
		}
		UpdateMemo();
	}
	firsttime = 1;
	XtAppAddTimeOut(appContext, (updatefreq < 60) ? 25 : 500, DebugMkDate, (caddr_t) w);
}

static void
MkDate(w)
	Widget          w;
{
	long            ti;
	struct tm      *tm;
	Date            yesterday;
	static		firsttime;

	yesterday = today;

	(void) time(&ti);
	tm = localtime(&ti);

        ConvDate(tm, &today);

	FmtTime(tm, date_area, sizeof(date_area), appResources.format);

	XtVaSetValues(w, XtNlabel, (XtArgVal) date_area, NULL);

	if (firsttime != 0) {
		if (yesterday.day != today.day) {
			ChangeHighlight(&yesterday, &today);
			AlarmFilePoll(tm);
		}
		UpdateMemo();
	}
	firsttime = 1;
	
	ti = ClockSync(tm, updatefreq);
	XtAppAddTimeOut(appContext, ti * 1000, MkDate, (caddr_t) w);
}

/*
 * Given a time structure and a frequency in secs
 * return no of secs to hit that interval
 */
long
ClockSync(tm, freq)
	struct	tm     *tm;
	long		freq;
{
	long	ti;

	ti = freq;
	if (ti > 1 && ti < 60)
		ti -= tm->tm_sec%freq;
	else
	if (ti >= 60 && ti < 3600)
		ti -= (tm->tm_min*60 + tm->tm_sec)%freq;
	else
	if (ti >= 3600)
		ti -= (tm->tm_hour*60*60 + tm->tm_min*60 + tm->tm_sec)%freq;
	return ti;
}

/*
 * DoTemplate
 * place an initial string into the date area so that the label
 * box will always be big enough
 */
void
DoTemplate(buf, len, fmt)
	char	*buf;
	size_t	len;
	char	*fmt;
{
	Tm	 *tm;
	Tm	  tml;	
	time_t    ti;
	
	time(&ti);
	tm = localtime(&ti);

	tml = *tm;	/* it seems that Solaris is unhappy when you */
	tm = &tml;	/* poke values into its own structure */
			/* but is OK if you have your own */
	/* generate a maximal structure */
	tm->tm_sec = 59;
	tm->tm_min = 59;
	tm->tm_hour = 23;
	tm->tm_mday = 27;
	tm->tm_mon = CycleMonths(tm);
	tm->tm_year = 93;
	tm->tm_wday = CycleDays(tm);
	FmtTime(tm, buf, len, fmt);
}

static int
CycleDays(settm)
	struct tm *settm;
{
	int	max = 0;
	int	maxday = 0;
	char	buf[BUFSIZ];
	struct tm tm;
	int	d;
	int	len;

	tm = *settm;
	for (d = 0; d < 7; d++) {
		tm.tm_wday = d;
		len = strftime(buf, sizeof buf, "%A", &tm);
		if (len > max) {
			maxday = d;
			max = len;
		}
	}
	return maxday;
}

static int
CycleMonths(settm)
	struct tm *settm;	
{
	int	max = 0;
	int	maxmon = 0;
	char	buf[BUFSIZ];
	struct tm tm;
	int	d;
	int	len;
	
	tm = *settm;
	for (d = 0; d < 11; d++) {
		tm.tm_mon = d;
		len = strftime(buf, sizeof buf, "%B", &tm);
		if (len > max) {
			maxmon = d;
			max = len;
		}
	}
	return maxmon;
}

/*
 * Look to see if we need to do something more than poll daily to change
 * the main toplevel strip
 */
static void
SetUpdateFreq(fmt)
	char	*fmt;
{	
	updatefreq = UpdateFreq(fmt);
	if (appResources.clocktick && updatefreq && 
	    updatefreq < appResources.clocktick)
		updatefreq = appResources.clocktick;
	if (updatefreq == 0)
		updatefreq = 24*60*60;
}


/*
 * Scan the time format string looking for an update frequency
 */
static int
UpdateFreq(fmt)
	char *fmt;
{
	int	update = 0;
#define	minu(v)	update = (update == 0 ? v : ((update < v) ? update : v))

	for(;*fmt; ++fmt) {
		if (*fmt == '%')
			switch(*++fmt) {
			case '\0':
				--fmt;
				break;
			case 'C':
			case 'c':
			case 'r':
			case 'S':
			case 's':
			case 'T':
			case 'X':
				minu(1);	/* every second */
				break;
			case 'M':
			case 'R':
				minu(60);	/* every minute */
				break;
			case 'H':
			case 'h':
			case 'I':
			case 'k':
			case 'l':
				minu(3600);	/* every hour */
				break;
			case 'p':
			case 'P':
				minu(43200);	/* AM/PM */
				break;
			}
	}
	return update;
}
