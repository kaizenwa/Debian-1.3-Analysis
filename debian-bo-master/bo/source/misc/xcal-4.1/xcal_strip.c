#ifndef lint
static char    *sccsid = "@(#)xcal_strip.c	3.27 (Hillside Systems) 11/1/93";
static char    *copyright = "@(#)Copyright 1989,1990,1993 Peter Collinson, Hillside Systems";
#endif				/* lint */
/***

* module name:
	xcal_strip.c
* function:
	Deal with the popup strip calendars obtained either by
	selection and the middle button, or by the < and > buttons
	on each strip.
* history:
	Written November 1989
	Peter Collinson
	Hillside Systems
* (C) Copyright: 1989 Hillside Systems/Peter Collinson
	
	For full permissions and copyright notice - see xcal.c
***/
#include <stdio.h>
#include <ctype.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/AsciiText.h>
#include "xcal.h"

static XtCallbackRec callbacks[] = {
	{NULL, NULL},
	{NULL, NULL},
	{NULL, NULL},
	{NULL, NULL}
};
#define ClearCallbacks() memset((caddr_t)callbacks, '\0', sizeof (callbacks))

Date            callb;		/* contains date when calendar day button */
				/* pressed */

static String   defTranslations =
"<Btn2Down>: set()\n\
	<Btn2Up>: LoadDateAction() unset()";

/*
 *	These translations used to make the middle mouse button
 *	load a date file from a selection when clicked on a strip
 */

/*
 * Forward routines local to this file
 */
static void     MakeMonth();
static void     DayBack();
#ifndef LONG_IS_32_BITS
static void     YmBack();
#endif
static void     StripQuit();
void            StripHelp();
void            WeeklyHelp();

/*
 * Local routines
 */
static void     MakeNewMonth();
static void	LoadDateCallback();
static Cardinal DateSum();
static void	setStripMax();
static Dimension CreateActionBar();
static Dimension CreateWeeklyActionBar();

#define argLD(N,V) { XtSetArg(args[nargs], N, V); nargs++; }

/*
 * Start a strip calendar happening a callback of left button
 */
/* ARGSUSED */
void
DoCalendar(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	NewMonthStrip(&today, NULL);	/* today is global */
}

/* ARGSUSED */
void
DoWeekly(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	Date            thisday;

	thisday.day = 0;
	thisday.month = 0;
	thisday.year = 0;
	thisday.wday = 0;
	NewMonthStrip(&thisday, w);/* today is global */
}


/*
 * Start a strip calendar happening a callback of the > or < buttons in
 * another strip
 */
/* ARGSUSED */
static void
MakeNewMonth(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	Date            thisday;

	thisday.year = YrUnpack((Cardinal) closure);
	thisday.month = MoUnpack((Cardinal) closure);
	thisday.day = today.day;
	NewMonthStrip(&thisday, NULL);
}

/* 
 *	Do all the X stuff to popup a Strip calendar
 *	A calendar strip is:
 *
 *	Popup ("<month year>")		// Name is the month and the year
 *	 Paned ("<month>")		// Name is the month
 *       Label ("header")		// optional contains Month Year
 *	 Form ("action")		// < Quit >
 *		Command ("back")	// contains < 
 *		Label ("quit")		// contains Quit
 *		Command ("next")	// contains >
 *	 ViewPort ("viewport")		// Viewport containing strip data
 *	        Paned ("data")		// Panel containing the calendar data
 *		    (Then many of..)
 *		    Form ("<dd DDD>")	// where DDD is the day of the week
 *		    Label ("label")	// contains the string above
 *		    Command ("info")	// contains the text from the file
 *		
 */
void
NewMonthStrip(td, but)
	Date           *td;
	Widget		but;
{
	Widget          shell, mon, dw, lw, lwi, form, monvp, mondt;
	Arg             args[15];
	char            titlestr[256];
	char		tbuf[256];
	char            iconName[256];
	int             type;
	MonthEntry     *me;
	Instance       *ins;
	register int    i;
	register Cardinal nargs;
	Cardinal        thisDay;
	Cardinal        startLoop;
	String          dayStr;
	Cardinal        numberOfDays;
	Boolean         defaultsAreSet = False;
	Boolean         markThisMonth = False;
	Dimension       labelH, infoH;
	Dimension       width;
	Dimension	totalHeight;
	Dimension	hdrHeight;
	Dimension       totalWidth;
	static		XtTranslations but2;

	type = (td->day == 0) ? ME_WEEKLY : ME_MONTHLY;
	hdrHeight = 0;

	/*
	 * There are lots of differences between Months and weekly strips
	 * here. Later tests are done using a switch structure
	 */
	switch (type) {
	case ME_MONTHLY:
		FmtDate(td, iconName, sizeof iconName, appResources.stripfmt);
		XtSetArg(args[0], XtNiconName, iconName);
		shell = XtCreatePopupShell(XtNewString(iconName), topLevelShellWidgetClass, toplevel, args, 1);
		ins = RegisterMonth(td->year, td->month, shell);
		mon = XtCreateManagedWidget(appResources.mon[td->month], panedWidgetClass, shell, NULL, 0);
		thisDay = FirstDay(td->month, td->year);
		numberOfDays = NumberOfDays(td->month, td->year);
		startLoop = 1;
		/*
		 * Get the map for this year
		 */
		me = GetMonthEntry(td->year, td->month);
		/*
		 * Title bar is month and date
		 */
		FmtDate(td, titlestr, sizeof titlestr, appResources.stripfmt);
		/*
		 * see if we will need to worry about marking today's entry
		 */
		if (appResources.markToday && td->year == today.year && td->month == today.month)
			markThisMonth = True;
		break;
	case ME_WEEKLY:
		(void) strcpy(iconName, appResources.weekly);
		nargs = 0;
		argLD(XtNiconName, iconName);
		shell = XtCreatePopupShell(XtNewString(iconName), topLevelShellWidgetClass, toplevel, args, nargs);
		if (but && XtIsSubclass(but, commandWidgetClass))
			ButtonOff(but, shell);
		ins = RegisterMonth(0, 0, shell);
		mon = XtCreateManagedWidget(iconName, panedWidgetClass, shell, NULL, 0);
		thisDay = 0;
		numberOfDays = 6;	/* test is <= */
		startLoop = 0;
		/*
		 * Get the map for this year
		 */
		me = GetWeeklyEntry();
		/*
		 * Title bar is from the resources
		 */
		strcpy(titlestr, iconName);
		/*
		 * see if we will need to worry about marking today's entry
		 */
		if (appResources.markToday)
			markThisMonth = True;
		break;
	}
	/*
	 * Find size of title bar by creating the widget and then throwing it
	 * away
	 */
	DoTemplate(tbuf, sizeof tbuf, appResources.stripfmt);
	XtSetArg(args[0], XtNlabel, tbuf);
	lw = XtCreateManagedWidget("sizer", labelWidgetClass, shell, args, 1);
	XtSetArg(args[0], XtNwidth, &totalWidth);
	XtGetValues(lw, args, 1);
	XtDestroyWidget(lw);
	/*
	 * Width is affected by a resource value
	 */
	if (appResources.minstripwidth && appResources.minstripwidth > totalWidth)
		totalWidth = appResources.minstripwidth;
	/*
	 * Now set the title bar should we need it
	 */
	if (appResources.useWmTitle) {
		XtSetArg(args[0], XtNlabel, XtNewString(titlestr));
		lw =  XtCreateManagedWidget("header", labelWidgetClass, mon, args, 1);
		hdrHeight = wHeight(lw);
	}
	/*
	 * Action bar
	 */
	nargs = 0;
	argLD(XtNshowGrip, False);
	argLD(XtNdefaultDistance, 2);
	dw = XtCreateManagedWidget("action", formWidgetClass, mon, args, nargs);

	switch (type) {
	case ME_MONTHLY:
		hdrHeight += CreateActionBar(shell, dw, mon, td);
		break;
	case ME_WEEKLY:
		hdrHeight += CreateWeeklyActionBar(shell, dw);
		break;
	}
	
	/*
	 * Create a Viewport, with a panel inside it
	 */
	nargs = 0;
	argLD(XtNshowGrip, False);
	argLD(XtNallowVert, True);
	monvp = XtCreateManagedWidget("viewport", viewportWidgetClass, mon, args, nargs);
	mondt = XtCreateManagedWidget("panel", panedWidgetClass, monvp, NULL, 0);


#ifdef	LONG_IS_32_BITS
	callbacks[0].callback = DayBack;
#else
	callbacks[0].callback = YmBack;
	callbacks[1].callback = DayBack;
#endif
	totalHeight = 0;
	for (i = startLoop; i <= numberOfDays; i++) {
		dayStr = appResources.sday[thisDay];
		switch (type) {
		case ME_MONTHLY:
			(void) sprintf(titlestr, "%2d %s", i, dayStr);
			break;
		case ME_WEEKLY:
			(void) strcpy(titlestr, dayStr);
			break;
		}
#ifdef LONG_IS_32_BITS
		callbacks[0].closure = (caddr_t) DatePack(thisDay, i, td->month, td->year);
#else
		callbacks[0].closure = (caddr_t) DatePack(td->month, td->year);
		callbacks[1].closure = (caddr_t) DayPack(thisDay, i);
#endif

		thisDay = (thisDay + 1) % 7;

		/*
		 * Each line in the strip is form containing label - command
		 */
		nargs = 0;
		argLD(XtNshowGrip, False);
		argLD(XtNdefaultDistance, 0);
		form = XtCreateManagedWidget(dayStr, formWidgetClass, mondt, args, nargs);

		nargs = 0;
		argLD(XtNlabel, XtNewString(titlestr));
		/* a little naughty here */
		/* this string memory is lost */
		/* on quit */
		argLD(XtNborderWidth, 0);
		argLD(XtNjustify, XtJustifyLeft);
		argLD(XtNfromHoriz, NULL);
		argLD(XtNleft, XtChainLeft);
		argLD(XtNright, XtChainLeft);

		ins->i_day_label[i] = lw = XtCreateManagedWidget("label", labelWidgetClass, form, args, nargs);

		/*
		 * To get a handle on the old values which are lost by
		 * highlighting we get them after we have created the widget.
		 * Then we highlight today.
		 */
		if (markThisMonth &&
		    ((type == ME_MONTHLY && today.day == i) ||
		     (type == ME_WEEKLY && today.wday == i))) {
			nargs = 0;
			argLD(XtNforeground, &ins->i_col.fg);
			argLD(XtNbackground, &ins->i_col.bg);
			argLD(XtNfont, &ins->i_font);
			XtGetValues(lw, args, nargs);

			nargs = 0;
			argLD(XtNforeground, appResources.today.fg);
			argLD(XtNbackground, appResources.today.bg);
			argLD(XtNfont, appResources.fontToday);
			XtSetValues(lw, args, nargs);
		}
		/*
		 * Done the first time through
		 * Gets the width of the line we have just made
		 */
		if (defaultsAreSet == False) {	/* compute text width */
			nargs = 0;
			argLD(XtNwidth, &width);
			argLD(XtNheight, &labelH);
			XtGetValues(lw, args, nargs);
			defaultsAreSet = True;
		}
		/*
		 * Start processing the RHS of the line
		 * This contains text from the file should any exist
		 */
		nargs = 0;
		argLD(XtNborderWidth, 0);
		argLD(XtNcallback, callbacks);
		argLD(XtNfromHoriz, lw);
		argLD(XtNleft, XtChainLeft);
		argLD(XtNright, XtChainRight);
		argLD(XtNjustify, XtJustifyLeft);
		argLD(XtNwidth, totalWidth - width);

		if (me->me_have[i]) {
			argLD(XtNlabel, me->me_have[i]);
		} else {
			argLD(XtNlabel, "    ");
		}
		ins->i_day_info[i] = lwi = XtCreateManagedWidget("info", commandWidgetClass, form, args, nargs);

		/* add translations */
		if (but2 == NULL) {
			but2 = XtParseTranslationTable(defTranslations);
		}
		XtAugmentTranslations(lwi, but2);

		/* deal with height */
		infoH = wHeight(lwi);
		if (labelH < infoH) {
			SetWidgetHeightMax(lw, labelH, infoH);
			totalHeight += infoH + 1;
		} else if (labelH > infoH) {
			SetWidgetHeightMax(lwi, infoH, labelH);
			totalHeight += labelH + 1;
		}
		/*
		 * cope with 1752
		 */
		if (td->year == 1752 && td->month == 8 && i == 2) {
			i = 13;
			numberOfDays += 11;	/* giving back the 11 days */
		}
	}
	ClearCallbacks();

	/* set up size for viewport scrolling */
	setStripMax(monvp, totalHeight, hdrHeight);

	/* here we go */
	XtPopup(shell, XtGrabNone);
}

/*
 * Get the height of the specified widget
 */
Dimension
wHeight(w)
	Widget	w;
{
	Arg		args[1];
	Dimension	H;

	XtSetArg(args[0], XtNheight, &H);
	XtGetValues(w, args, 1);
	return H;
}

/*
 * Set the max size of the viewport for the strip
 */
static void
setStripMax(w, stripHeight, hdrHeight)
	Widget	w;
	Dimension stripHeight;
	Dimension hdrHeight;
{
	Dimension	maxH;
	Arg             args[1];

	if (appResources.maxstripheight == 0) {
		/* remove the hdrHeight here as a guess */
		maxH = HeightOfScreen(XtScreen(toplevel));
		maxH -= hdrHeight;
	}	
	else	maxH = appResources.maxstripheight;
	maxH -= hdrHeight + 20;
	if (stripHeight > maxH) {
		XtSetArg(args[0], XtNheight, maxH);
		XtSetValues(w, args, 1);
	}
}
/*
 * Create action bar for normal monthly strip
 */
static Dimension
CreateActionBar(shell, dw, mon, td)
	Widget          shell;
	Widget          dw;
	Widget          mon;
	Date           *td;
{
	Widget          lw;
	register Cardinal nargs;
	Arg             args[8];
	Dimension	ht, maxht = 0;

	/*
	 * back one month label "<" from resources
	 */
	callbacks[0].callback = MakeNewMonth;
	callbacks[0].closure = (caddr_t) DateSum(td, -1);
	nargs = 0;
	argLD(XtNcallback, callbacks);
	argLD(XtNfromHoriz, NULL);
	argLD(XtNleft, XtChainLeft);
	argLD(XtNright, XtChainLeft);
	lw = XtCreateManagedWidget("back", commandWidgetClass, dw, args, nargs);
	maxht = wHeight(lw);
	maxht++;
	ClearCallbacks();

	/*
	 * Quit button label "quit" from resources
	 */
	callbacks[0].callback = StripQuit;
	callbacks[0].closure = (caddr_t) shell;
	nargs = 0;
	argLD(XtNcallback, callbacks);
	argLD(XtNfromHoriz, lw);
	argLD(XtNleft, XtChainLeft);
	argLD(XtNright, XtChainRight);
	lw = XtCreateManagedWidget("quit", commandWidgetClass, dw, args, nargs);
	ht = wHeight(lw);
	maxht = ht > maxht ? ht : maxht;
	ClearCallbacks();

	/*
	 * On one month label ">" from resources
	 */
	callbacks[0].callback = MakeNewMonth;
	callbacks[0].closure = (caddr_t) DateSum(td, 1);
	nargs = 0;
	argLD(XtNcallback, callbacks);
	argLD(XtNfromHoriz, lw);
	argLD(XtNleft, XtChainRight);
	argLD(XtNright, XtChainRight);
	lw = XtCreateManagedWidget("next", commandWidgetClass, dw, args, nargs);
	ht = wHeight(lw);
	maxht = ht > maxht ? ht : maxht;
	ClearCallbacks();

	/*
	 * Help button label help from resources
	 */
	if (appResources.giveHelp) {
		callbacks[0].callback = StripHelp;
		callbacks[0].closure = (caddr_t) 0;
		nargs = 0;
		argLD(XtNcallback, callbacks);
		argLD(XtNshowGrip, False);
		lw = XtCreateManagedWidget("help", commandWidgetClass, mon, args, nargs);
		maxht += wHeight(lw);
		ClearCallbacks();
	}
	return maxht;
}

/*
 * Create action bar for normal monthly strip
 */
static Dimension
CreateWeeklyActionBar(shell, dw)
	Widget          shell;
	Widget          dw;
{
	Widget          lw;
	register Cardinal nargs;
	Arg             args[8];
	Dimension	ht, maxht = 0;

	/*
	 * Quit button label "quit" from resources
	 */
	callbacks[0].callback = StripQuit;
	callbacks[0].closure = (caddr_t) shell;
	nargs = 0;
	argLD(XtNcallback, callbacks);
	argLD(XtNfromHoriz, NULL);
	argLD(XtNleft, XtChainLeft);
	argLD(XtNright, appResources.giveHelp ? XtChainLeft : XtChainRight);
	lw = XtCreateManagedWidget("quit", commandWidgetClass, dw, args, nargs);
	maxht = wHeight(lw);
	ClearCallbacks();

	/*
	 * Help button label help from resources
	 */
	if (appResources.giveHelp) {
		callbacks[0].callback = WeeklyHelp;
		callbacks[0].closure = (caddr_t) 0;
		nargs = 0;
		argLD(XtNcallback, callbacks);
		argLD(XtNfromHoriz, lw);
		argLD(XtNleft, XtChainLeft);
		argLD(XtNright, XtChainRight);
		lw = XtCreateManagedWidget("help", commandWidgetClass, dw, args, nargs);
		ht = wHeight(lw);
		maxht = ht > maxht ? ht : maxht;
		ClearCallbacks();
	}
	return maxht;
}

/*
 * Called when the date changes to ensure that the correct day has the
 * appropriate highlights
 */
void
ChangeHighlight(old, new)
	Date           *old;
	Date           *new;
{
	register Instance *ins;
	Arg             args[5];
	Cardinal        nargs;

	for (ins = FindInstanceList(old); ins; ins = ins->i_next) {
		nargs = 0;
		argLD(XtNforeground, ins->i_col.fg);
		argLD(XtNbackground, ins->i_col.bg);
		argLD(XtNfont, ins->i_font);
		XtSetValues(ins->i_day_label[old->day], args, nargs);
	}

	for (ins = FindInstanceList(new); ins; ins = ins->i_next) {
		nargs = 0;
		argLD(XtNforeground, &ins->i_col.fg);
		argLD(XtNbackground, &ins->i_col.bg);
		argLD(XtNfont, &ins->i_font);
		XtGetValues(ins->i_day_label[new->day], args, nargs);

		nargs = 0;
		argLD(XtNforeground, appResources.today.fg);
		argLD(XtNbackground, appResources.today.bg);
		argLD(XtNfont, appResources.fontToday);
		XtSetValues(ins->i_day_label[new->day], args, nargs);
	}
}

/*
 * Called when middle mouse button is clicked on a date box
 * This gets the current selection and adds it to the file
 * corresponding to the day.
 * This allows quick data loading
 */
void
LoadDateStrip(w, event, params, numb)
        Widget        	w;
        XSelectionEvent *event;
        String         *params;
        Cardinal       *numb;
{

	/* set up to get the selection */
	/* I am unconvinced that it should be this easy */
	XtGetSelectionValue(w, XA_PRIMARY, XA_STRING,
			LoadDateCallback, 0, XtLastTimestampProcessed(XtDisplay(w)));
	/* The work is done in the callback routine */
}

static void
LoadDateCallback(w, xcd, sel, seltype, val, len, fmt)
	Widget		w;
	XtPointer	xcd;
	Atom		*sel;
	Atom		*seltype;
	XtPointer	val;
	unsigned long	*len;
	int		*fmt;	
{	
	String		s;
	int		n;
	Arg             args[1];
	Cardinal	v;
	XtCallbackRec	*cb;
	Date		da;

	/* deal with arguments to get the text */
	if (*seltype != XA_STRING)
		n = 0;
	else
		n = (*len) * (*fmt/8);
	if (n == 0)
		return;

	s = (String) XtMalloc(n+1);
	if (n > 0)
		memcpy(s, (char *)val, n);
	s[n] = 0;
	XtFree(val);

	/* get closure data to find the date */
	XtSetArg(args[0], XtNcallback, &cb);
	XtGetValues(w, args, 1);
	v = (Cardinal) cb->closure;
	da.month = MoUnpack(v),
	da.year = YrUnpack(v);
#ifndef LONG_IS_32_BITS
	cb++;
	v = (Cardinal) cb->closure;
#endif
	da.day = DyUnpack(v);
	da.wday = WdUnpack(v);
	/* Add text to day file (code in xcal_edit.c) */
	AppendText(w, &da, s);
}


/*
 * Call back from a quit button to lose a month strip
 */
/* ARGSUSED */
static void
StripQuit(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	XtPopdown((Widget) closure);
	XtDestroyWidget((Widget) closure);
}


/*
 * Month arithmetic and packing
 */
static          Cardinal
DateSum(td, inx)
	Date           *td;
	int             inx;
{
	int             m, y;

	m = td->month;
	y = td->year;
	m += inx;
	if (m < 0) {
		m = 11;
		y--;
	} else if (m > 11) {
		m = 0;
		y++;
	}
#ifdef LONG_IS_32_BITS
	return (DatePack(0, 0, m, y));
#else
	return (DatePack(m, y));
#endif
}

/*
 * Call back from day selection button press
 * This is done in two stages if cannot fold dates into a closure
 */
/* ARGSUSED */
static void
DayBack(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
#ifdef LONG_IS_32_BITS
	callb.month = MoUnpack((Cardinal) closure);
	callb.year = YrUnpack((Cardinal) closure);
#endif
	callb.day = DyUnpack((Cardinal) closure);
	callb.wday = WdUnpack((Cardinal) closure);
	StartEditing(w, &callb, NULL);
}

#ifndef LONG_IS_32_BITS
/* ARGSUSED */
static void
YmBack(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	callb.month = MoUnpack((Cardinal) closure);
	callb.year = YrUnpack((Cardinal) closure);
}
#endif
