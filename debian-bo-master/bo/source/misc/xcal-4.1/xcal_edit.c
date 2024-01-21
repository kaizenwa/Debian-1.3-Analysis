#ifndef lint
static char    *sccsid = "@(#)xcal_edit.c	3.40 (Hillside Systems) 9/15/95";
static char    *copyright = "@(#)Copyright 1989,1990,1993 Peter Collinson, Hillside Systems";
#endif				/* lint */
/***

* module name:
	xcal_edit.c
* function:
	Deal with editable days
	This is derived from xcalendar's view of how to store things
* history:
	Written November 1989
	Peter Collinson
	Hillside Systems
* (C) Copyright: 1989 Hillside Systems/Peter Collinson

	For full permissions and copyright notice - see xcal.c
***/
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <X11/Intrinsic.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Dialog.h>
#include "xcal.h"
#include <sys/stat.h>
#if defined(NeXT) || defined(mips)
#include <sys/fcntl.h>
#include <sys/dir.h>
#include <sys/dirent.h>
#else
#include <dirent.h>
#endif

#define argLD(N,V) { XtSetArg(args[nargs], N, V); nargs++; }

typedef struct editline {
	struct editline *ed_next;	/* Pointer to next */
	struct meWrap  *ed_meWrap;	/* Pointer to head of the chain */
	Cardinal        ed_day;		/* What day we are */
	Widget          ed_popup;	/* widget of editor popup */
	Widget          ed_quit;	/* widget of quit button */
	Widget          ed_save;	/* widget of save button */
	Widget          ed_text;	/* the text area */
	Cardinal        ed_size;	/* size of the buffer */
	char           *ed_data;	/* pointer to malloc'ed data buffer */
} EditLine;

typedef struct meWrap {
	struct meWrap  *mw_next;
	String          mw_dir;		/* name of the directory */
	Boolean         mw_useTopDir;	/* have found some data in the top dir */
	MonthEntry      mw_me;		/* what the external world sees */
	Instance       *mw_list;	/* list of toplevel widget for the */
					/* current set of displayed strips */
	EditLine       *mw_ed;		/* data being edited */
} MeWrap;

#define	mw_year	mw_me.me_year
#define mw_month mw_me.me_month
#define mw_have	mw_me.me_have

#define	StripType(mw)	(mw->mw_me.me_type)

static MeWrap  *WrapBase;		/* base of the list */
static MeWrap  *WrapEnd;		/* the last one in the list */

char           *MapStem;		/* pointer to the string which is */
					/* where the map data is stored */

Boolean         FoundCalendarDir;	/* whether the Calendar directory */
					/* exists */

static XtCallbackRec callbacks[] = {
	{NULL, NULL},
	{NULL, NULL}
};
#define ClearCallbacks() memset((caddr_t)callbacks, '\0', sizeof (callbacks))

/*
 * Routine specs
 */	
static void	MakeMapStem();
static MeWrap  *NewMeWrap();
static MeWrap  *MeWrapSearch();
static void     DeRegisterMonth();
static Boolean  WriteCalendarFile();
static Boolean  WriteWeeklyFile();
static void     DeleteCalendarFile();
static int	EditCheck();
static void     SetAllButtons();
static void     TextChanged();
static void	UpdateDayDetails();
static void     FinishEditing();
static void     CleanEditPanel();
static void     CheckExit();
static void     CheckDia();
static void     YesCheck();
static void     NoCheck();
static int      DayMatch();
static void	SaveEdits();
void            StartDayEditor();
void            Fatal();



/*
 * Fire up the month entry environment called once
 */
void
InitMonthEntries()
{
	if (MapStem == NULL)
		MakeMapStem();

	if (access(MapStem, F_OK) < 0)
		FoundCalendarDir = False;
	else {
		FoundCalendarDir = True;
		/*
		 * If we can see the directory, then lurch into it
		 */
		if (chdir(MapStem) < 0)
			Fatal("Cannot change into %s", MapStem);
	}
}

/*
 *	Find the base of the Calendar structure
 */
static void
MakeMapStem()
{	
	char            buf[BUFSIZ];
	struct passwd	*pw;

	if (appResources.otheruser) {
		/* someone else */
		pw = getpwnam(appResources.otheruser);
		if (pw == NULL)
			Fatal("Cannot get password details for %s\n", appResources.otheruser);
	} else {
		pw = getpwuid(getuid());
		if (pw == NULL)
			Fatal("Cannot get password details for YOU!\n");
	}
	(void) sprintf(buf, "%s/%s", pw->pw_dir, appResources.directory);
	MapStem = XtNewString(buf);
}

/*
 *	Get the entry for a specific month
 *
 *	xcalendar files are all
 *	xc<d><Mon><Year>
 *	or
 *	xc<dd><Mon><Year>
 *
 *	where d or dd is the day (%02d would have been better)
 *	<Mon> is a capitalised first three letters of the
 *		English month name. If you need compatibility
 *		don't redefine the short names in the resources
 *		for this program.
 *	<Year> is the full numeric year.
 *
 *	We will follow this BUT we will also make this program
 *	create subdirectories for new years
 *		xy<Year>
 *	to speed up file access
 */
MonthEntry *
GetMonthEntry(yr, mo)
	Cardinal        yr;
	Cardinal        mo;
{
	MeWrap         *mw;
	char           *dir;
	DIR            *dirp;
	struct dirent  *dp;
	int             da;
	Boolean         inSubDir;
	char            yearbuf[5];
	char            monthbuf[4];

	if ((mw = MeWrapSearch(yr, mo)) == NULL)
		mw = NewMeWrap(yr, mo);
	mw->mw_me.me_type = ME_MONTHLY;
	if (!FoundCalendarDir)
		return (&mw->mw_me);

	/*
	 * need this for string match
	 */
	(void) sprintf(yearbuf, "%d", yr);
	(void) sprintf(monthbuf, "%s", appResources.smon[mo]);

	/* we are in the directory */
	/* so let's lookee here for the any files of interest */

	dir = ".";
	inSubDir = False;
	if (mw->mw_dir) {
		dir = mw->mw_dir;
		inSubDir = True;
	}
	if ((dirp = opendir(dir)) == NULL)
		Fatal("Cannot open directory: %s", MapStem);

	for (da = 1; da < 32; da++)
		if (mw->mw_have[da]) {
			XtFree(mw->mw_have[da]);
			mw->mw_have[da] = NULL;
		}
	while ((dp = readdir(dirp)) != NULL) {

		switch (strlen(dp->d_name)) {

		case 6:	/* xy<Year> ?? */
			if (dp->d_name[0] == 'x' &&
			    dp->d_name[1] == 'y' &&
			    dp->d_name[2] == yearbuf[0] &&
			    dp->d_name[3] == yearbuf[1] &&
			    dp->d_name[4] == yearbuf[2] &&
			    dp->d_name[5] == yearbuf[3] &&
			    appResources.calCompat == False) {
				/*
				 * well - we're wasting
				 * our time at the top
				 * level - rejig things
			         * to work in the
				 * subdirectory
				 */
				inSubDir = True;
				mw->mw_useTopDir = False;
				mw->mw_dir = XtNewString(dp->d_name);
				closedir(dirp);
				if ((dirp = opendir(mw->mw_dir)) == NULL)
					Fatal("Cannot open directory %s/%s", MapStem, mw->mw_dir);

			}
			break;
		case 10:	/* xc<d><Mon><Year> ?? */
			if (dp->d_name[0] == 'x' &&
			    dp->d_name[1] == 'c' &&
			    isdigit(dp->d_name[2]) &&
			    dp->d_name[3] == monthbuf[0] &&
			    dp->d_name[4] == monthbuf[1] &&
			    dp->d_name[5] == monthbuf[2] &&
			    dp->d_name[6] == yearbuf[0] &&
			    dp->d_name[7] == yearbuf[1] &&
			    dp->d_name[8] == yearbuf[2] &&
			    dp->d_name[9] == yearbuf[3]) {
				da = dp->d_name[2] - '0';
				mw->mw_have[da] = ReadCalendarFile(mw->mw_dir, dp->d_name);
				if (inSubDir == False)
					mw->mw_useTopDir = True;
			}
			break;
		case 11:	/* xc<dd><Mon><Year> ?? */
			if (dp->d_name[0] == 'x' &&
			    dp->d_name[1] == 'c' &&
			    isdigit(dp->d_name[2]) &&
			    isdigit(dp->d_name[3]) &&
			    dp->d_name[4] == monthbuf[0] &&
			    dp->d_name[5] == monthbuf[1] &&
			    dp->d_name[6] == monthbuf[2] &&
			    dp->d_name[7] == yearbuf[0] &&
			    dp->d_name[8] == yearbuf[1] &&
			    dp->d_name[9] == yearbuf[2] &&
			    dp->d_name[10] == yearbuf[3]) {
				da = (dp->d_name[2] - '0') * 10 + (dp->d_name[3] - '0');
				mw->mw_have[da] = ReadCalendarFile(mw->mw_dir, dp->d_name);
				if (inSubDir == False)
					mw->mw_useTopDir = True;
			}
			break;
		}
	}
	closedir(dirp);
	return (&mw->mw_me);
}

/*
 *	Get the entry for the weekly strip
 *	Files are xw<Day> in the Calendar directory
 */
MonthEntry *
GetWeeklyEntry()
{
	MeWrap         *mw;
	int             da;
	DIR            *dirp;
	struct dirent  *dp;

	if ((mw = MeWrapSearch(0, 0)) == NULL)
		mw = NewMeWrap(0, 0);
	mw->mw_me.me_type = ME_WEEKLY;

	if (!FoundCalendarDir)
		return (&mw->mw_me);

	if ((dirp = opendir(".")) == NULL)
		Fatal("Cannot open directory: %s", MapStem);

	for (da = 0; da < 7; da++)
		if (mw->mw_have[da]) {
			XtFree(mw->mw_have[da]);
			mw->mw_have[da] = NULL;
		}
	while ((dp = readdir(dirp)) != NULL) {
		if (dp->d_name[0] == 'x' &&
		    dp->d_name[1] == 'w' &&
		    ((da = DayMatch(&dp->d_name[2])) != -1)) {
			mw->mw_have[da] = ReadCalendarFile(NULL, dp->d_name);
		}
	}
	closedir(dirp);
	return (&mw->mw_me);
}

/*
 * Look for a short name match with a day
 */
static int
DayMatch(day)
	String          day;
{
	register        i;

	for (i = 0; i < 7; i++)
		if (strcmp(day, appResources.sday[i]) == 0)
			return (i);
	return (-1);
}

/*
 * create a new MapWrap area
 */
static MeWrap  *
NewMeWrap(yr, mo)
	Cardinal        yr;
	Cardinal        mo;
{
	register MeWrap *mw;
	static MeWrap	zerow;
	
	mw = (MeWrap *) XtMalloc(sizeof(MeWrap));
	*mw = zerow;
	if (WrapEnd)
		WrapEnd->mw_next = mw;
	WrapEnd = mw;
	if (WrapBase == NULL)
		WrapBase = mw;
	mw->mw_year = yr;
	mw->mw_month = mo;
	mw->mw_useTopDir = False;
	return (mw);
}

/*
 * Search the MapWrap list for a year
 */
static MeWrap  *
MeWrapSearch(yr, mo)
	Cardinal        yr;
	Cardinal        mo;
{
	register MeWrap *mw;

	if (WrapBase)
		for (mw = WrapBase; mw; mw = mw->mw_next)
			if (yr == mw->mw_year && mo == mw->mw_month)
				return (mw);
	return (NULL);
}

/*
 * Register an instance of a month Return a pointer to an instance structure
 * so it can be filled in by the caller
 */
Instance       *
RegisterMonth(yr, mo, w)
	Cardinal        yr;
	Cardinal        mo;
	Widget          w;
{
	register MeWrap *mw;
	register Instance *ins;

	if ((mw = MeWrapSearch(yr, mo)) == NULL)
		mw = NewMeWrap(yr, mo);

	ins = (Instance *) XtMalloc(sizeof(Instance));
	ins->i_next = mw->mw_list;
	mw->mw_list = ins;
	ins->i_w = w;

	callbacks[0].callback = DeRegisterMonth;
#ifdef LONG_IS_32_BITS
	callbacks[0].closure = (caddr_t) DatePack(0, 0, mo, yr);
#else
	callbacks[0].closure = (caddr_t) DatePack(mo, yr);
#endif

	XtAddCallbacks(w, XtNdestroyCallback, callbacks);
	return (ins);
}

/*
 * Return the head of an instance list - given a date
 */
Instance       *
FindInstanceList(da)
	Date           *da;
{
	register MeWrap *mw;

	if ((mw = MeWrapSearch(da->year, da->month)) == NULL)
		return (NULL);
	return (mw->mw_list);
}

/*
 * Delete an instance
 */
/* ARGSUSED */
static void
DeRegisterMonth(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	Cardinal        yr, mo;
	register Instance *ins, *inlast;
	register MeWrap *mw;

	yr = YrUnpack((Cardinal) closure);
	mo = MoUnpack((Cardinal) closure);

	if ((mw = MeWrapSearch(yr, mo)) == NULL)
		return;
	for (ins = mw->mw_list, inlast = NULL;
	     ins;
	     inlast = ins, ins = ins->i_next) {
		if (ins->i_w == w) {
			if (inlast)
				inlast->i_next = ins->i_next;
			else
				mw->mw_list = ins->i_next;
			XtFree((char *) ins);
			return;
		}
		inlast = ins;
	}
}


/*
 * Read a calendar file into memory into a string
 * if the file is zero length then unlink and return NULL
 */
String
ReadCalendarFile(dir, file)
	String          dir;
	String          file;
{
	char            fname[256];
	int             fd;
	String          destb;
	struct stat     fsb;

	if (dir) {
		(void) sprintf(fname, "%s/%s", dir, file);
		file = fname;
	}
	if ((fd = open(file, 0)) < 0) {
		if (MyCalendar)
			Fatal("Cannot open: %s for reading", file);
		else
			return(XtNewString(appResources.private));
	}	
	if (fstat(fd, &fsb) < 0)
		Fatal("Cannot fstat %s", file);

	if (fsb.st_size == 0) {
		if (MyCalendar)
			(void) unlink(file);
		close(fd);
		return (NULL);
	}
	destb = (String) XtMalloc(fsb.st_size + 1);

	if (read(fd, (String) destb, fsb.st_size) != fsb.st_size)
		Fatal("Read error on %s", file);

	close(fd);

	destb[fsb.st_size] = '\0';

	return (destb);
}

/*
 * Check to see if we should create the top directory
 */
Boolean
NeedTop()
{
	if (!FoundCalendarDir) {
		if (mkdir(MapStem, 0777) == -1) {
			XBell(XtDisplay(toplevel), 0);
			fprintf(stderr, "xcal: Could not create: %s directory.\n", MapStem);
			perror("xcal: mkdir");
			fflush(stderr);
			return (False);
		}
		if (chdir(MapStem) < 0) {
			XBell(XtDisplay(toplevel), 0);
			fprintf(stderr, "xcal: Could not chdir into %s.\n", MapStem);
			perror("xcal: chdir");
			fflush(stderr);
			return (False);
		}
		FoundCalendarDir = True;
	}
	return (True);
}

/*
 * Write a calendar file creating any directories which are needed
 * Return True if OK
 */
static Boolean
WriteCalendarFile(mw, day, contents)
	register MeWrap *mw;
	Cardinal        day;
	char           *contents;
{
	int             fd;
	Cardinal        len;
	char            fname[256];
	char            cname[16];

	len = strlen(contents);
	if (len == 0) {
		DeleteCalendarFile(mw, day);
		return (True);
	}
	if (!NeedTop())
		return (False);

	/*
	 * So that looks OK 
         * We can now create the output file. However, we
	 * would like to put any new data into subdirectories named for the
	 * year unless we are compatible with xcalendar
	 */
	fname[0] = '\0';
	if (appResources.calCompat == False && mw->mw_useTopDir == False) {
		/*
		 * we have no data in the top directory
		 * so let's create the directory name
		 */
		(void) sprintf(fname, "xy%d", mw->mw_year);

		if (access(fname, F_OK) < 0) {
			if (mkdir(fname, 0777) < 0) {
				XBell(XtDisplay(toplevel), 0);
				fprintf(stderr, "xcal: Could not create: %s/%s directory.\n", MapStem, fname);
				perror("xcal: mkdir ");
				fflush(stderr);
				return (False);
			}
		}
		strcat(fname, "/");
	}
	/*
	 * Whew - it looks as if we can now write the file
	 */
	(void) sprintf(cname, "xc%d%s%d", day,
		       appResources.smon[mw->mw_month],
		       mw->mw_year);

	strcat(fname, cname);

	if ((fd = open(fname, O_WRONLY | O_TRUNC | O_CREAT, 0666)) < 0) {
		XBell(XtDisplay(toplevel), 0);
		fprintf(stderr, "xcal: Could not open %s/%s for writing.\n", MapStem, fname);
		perror("xcal: open");
		fflush(stderr);
		return (False);
	}
	if (write(fd, contents, len) != len) {
		XBell(XtDisplay(toplevel), 0);
		fprintf(stderr, "xcal: Write error %s/%s file.\n", MapStem, fname);
		perror("xcal: write");
		fflush(stderr);
		close(fd);
		return (False);
	}
	close(fd);
	/*
	 * tickle the alarm system if we have altered `today'
	 */
	if (today.day == day && today.month == mw->mw_month &&
	    today.year == mw->mw_year)
		AlarmFilePoll(NULL);
	return (True);
}

static void
DeleteCalendarFile(mw, day)
	register MeWrap *mw;
	Cardinal        day;
{
	char            fname[256];
	char            cname[16];

	fname[0] = '\0';

	if (mw->mw_useTopDir == False) {
		/*
		 * we have no data in the top directory 
		 * so let's create the directory name
		 */
		(void) sprintf(fname, "xy%d", mw->mw_year);

		if (access(fname, F_OK) < 0)
			return;
		strcat(fname, "/");
	}
	(void) sprintf(cname, "xc%d%s%d", day,
		       appResources.smon[mw->mw_month],
		       mw->mw_year);

	strcat(fname, cname);

	unlink(fname);

	/*
	 * tickle the alarm system if we have altered `today'
	 */
	if (today.day == day && today.month == mw->mw_month &&
	    today.year == mw->mw_year)
		AlarmFilePoll(NULL);
}

/*
 * Write daily file out
 */
static          Boolean
WriteWeeklyFile(mw, day, contents)
	register MeWrap *mw;
	Cardinal        day;
	char           *contents;
{
	int             fd;
	Cardinal        len;
	char           *fname;

	fname = MakeWeeklyName(day);

	len = strlen(contents);
	if (len == 0)
		(void) unlink(fname);
	else {
		if ((fd = open(fname, O_WRONLY | O_TRUNC | O_CREAT, 0666)) < 0) {
			XBell(XtDisplay(toplevel), 0);
			fprintf(stderr, "xcal: Could not open %s/%s for writing.\n", MapStem, fname);
			perror("xcal: open");
			fflush(stderr);
			return (False);
		}
		if (write(fd, contents, len) != len) {
			XBell(XtDisplay(toplevel), 0);
			fprintf(stderr, "xcal: Write error %s/%s file.\n", MapStem, fname);
			perror("xcal: write");
			fflush(stderr);
			close(fd);
			return (False);
		}
		close(fd);
	}
	/*
	 * tickle the alarm system if we have altered `today'
	 */
	if (today.wday == day)
		AlarmFilePoll(NULL);

	return (True);
}

/*
 * Create a standard weekly file name
 */
String
MakeWeeklyName(day)
	Cardinal        day;
{
	static char     fname[16];

	(void) sprintf(fname, "xw%s", appResources.sday[day]);
	return (fname);
}

/*
 * Get the contents of the current Weekly file if any
 */
String
GetWeeklyFile(day)
	Cardinal        day;
{
	char           *fname;

	if (FoundCalendarDir == False)
		return (NULL);

	fname = MakeWeeklyName(day);

	if (access(fname, F_OK) < 0)
		return (NULL);

	return (ReadCalendarFile(NULL, fname));
}

/*
 * Start up an editor window from the callback
 * Pass the calling widget so we can change its contents after the edit
 */
/* ARGSUSED */
void
StartEditing(w, da, but)
	Widget          w;
	Date           *da;
	Widget		but;
{
	register MeWrap *mw;
	register EditLine *ed;
	static EditLine	zeroe;
	
	if ((mw = MeWrapSearch(da->year, da->month)) == NULL)
		mw = NewMeWrap(da->year, da->month);	/* shouldn`t happen */
	/*
	 * see if we are editing this day
	 */
	if (EditCheck(w, mw, da))
		return;
	/*
	 * Things are looking OK
	 * Create a new editing record
	 */
	ed = (EditLine *) XtMalloc(sizeof(EditLine));
	*ed = zeroe;
	ed->ed_day = da->day;
	ed->ed_meWrap = mw;	/* help for unlinking */
	/*
	 * Do we have a string now
	 */
	if (mw->mw_have[da->day]) {
		ed->ed_size = appResources.textbufsz + strlen(mw->mw_have[da->day]) + 1;
		ed->ed_data = XtMalloc(ed->ed_size);
		strcpy(ed->ed_data, mw->mw_have[da->day]);
	} else {
		ed->ed_data = XtMalloc(ed->ed_size = appResources.textbufsz);
		*ed->ed_data = '\0';
	}
	/*
	 * put the record into the list
	 */
	ed->ed_next = mw->mw_ed;
	mw->mw_ed = ed;
	/*
	 * We fiddle with the source widget too 
	 * Desensitise visible source widgets
	 * If the user starts up another strip for this month then
	 * the NoEditIsPossible() code above copes
	 */
	SetAllButtons(mw->mw_list, da->day, False);
	/*
	 * Now we should start up the edit window for this month
	 */
	StartDayEditor(mw, da, but);
}

/*
 *	See if we are editing a day already
 *	complain and return 1 if we are
 *	return 0 if not
 */
static int
EditCheck(w, mw, da)
	Widget		w;
	MeWrap		*mw;
	Date		*da;
{
	register EditLine *ed;

	/*
	 * see if we are already editing this day
	 */
	for (ed = mw->mw_ed; ed; ed = ed->ed_next) {
		if (ed->ed_day == da->day) {	/* we are! */
			/* Complain via a popup */
			switch (StripType(mw)) {
			case ME_MONTHLY:
				NoEditIsPossible(w, da);
				break;
			case ME_WEEKLY:
				NoDayEditIsPossible(w, da);
				break;
			}
			return 1;
		}
	}
	return 0;
}

/*
 * Set all the relevant buttons in a widget list to off or on
 */
static void
SetAllButtons(ins, day, val)
	Instance       *ins;
	Cardinal        day;
	Boolean         val;
{
	for (; ins; ins = ins->i_next)
		XtSetSensitive(ins->i_day_info[day], val);
}

/*
 * Start up a day editor Modelled on xcalendar.c
 */
void
StartDayEditor(mw, da, but)
	register MeWrap *mw;
	register Date  *da;
	Widget but;
{
	register EditLine *ed = mw->mw_ed;	/* top of the list is ours */
	Widget          lw, et;
	Widget          frame;
	Arg             args[10];
	Cardinal        nargs;
	char            buf[BUFSIZ];
	void            EditHelp();
	
	ed->ed_popup = XtCreatePopupShell("edit", topLevelShellWidgetClass, toplevel, NULL, 0);

	if (but && XtIsSubclass(but, commandWidgetClass))
		ButtonOff(but, ed->ed_popup);

	/*
	 * Create the title line - which is a form containing buttons and a
	 * date label
	 */
	et = XtCreateManagedWidget("panel", panedWidgetClass, ed->ed_popup, NULL, 0);

	nargs = 0;
	argLD(XtNshowGrip, False);
	argLD(XtNskipAdjust, True);
	argLD(XtNdefaultDistance, 1);
	frame = XtCreateManagedWidget("title", formWidgetClass, et, args, nargs);
	/*
	 * Take label "quit" from resources
	 */
	callbacks[0].callback = FinishEditing;
	callbacks[0].closure = (caddr_t) ed;
	nargs = 0;
	argLD(XtNcallback, callbacks);
	argLD(XtNfromHoriz, NULL);
	argLD(XtNleft, XtChainLeft);
	argLD(XtNright, XtChainLeft);
	lw = ed->ed_quit = XtCreateManagedWidget("quit", commandWidgetClass, frame, args, nargs);

	/*
	 * Take label "save" from resources
	 */
	if (MyCalendar) {
		callbacks[0].callback = SaveEdits;
		callbacks[0].closure = (caddr_t) ed;
		nargs = 0;
		argLD(XtNcallback, callbacks);
		argLD(XtNfromHoriz, ed->ed_quit);
		argLD(XtNleft, XtChainLeft);
		argLD(XtNright, XtChainLeft);
		argLD(XtNsensitive, False);
		lw = ed->ed_save = XtCreateManagedWidget("save", commandWidgetClass, frame, args, nargs);
		
	}	
	if (appResources.giveHelp) {
		/*
		 * Take label "help" from resources
		 */
		callbacks[0].callback = EditHelp;
		callbacks[0].closure = (caddr_t) 0;
		nargs = 0;
		argLD(XtNcallback, callbacks);
		argLD(XtNfromHoriz, lw);
		argLD(XtNleft, XtChainLeft);
		argLD(XtNright, XtChainLeft);
		lw = XtCreateManagedWidget("help", commandWidgetClass, frame, args, nargs);
	}
	switch (StripType(mw)) {
	case ME_MONTHLY:
		FmtDate(da, buf, sizeof buf, appResources.editfmt);
		break;
	case ME_WEEKLY:
		(void) strcpy(buf, appResources.day[da->day]);
		break;
	}
	nargs = 0;
	argLD(XtNlabel, buf);
	argLD(XtNborderWidth, 0);
	argLD(XtNfromHoriz, lw);
	argLD(XtNfromVert, NULL);
	argLD(XtNvertDistance, 2);
	argLD(XtNleft, XtChainLeft);
	argLD(XtNright, XtChainRight);
	lw = XtCreateManagedWidget("date", labelWidgetClass, frame, args, nargs);

	/*
	 * The text widget is in the pane below
	 * The Scroll Attributes are controlled from the application
	 * defaults file
	 */
	callbacks[0].callback = TextChanged;
	callbacks[0].closure = (caddr_t) ed;
	nargs = 0;
	argLD(XtNshowGrip, False);
	argLD(XtNstring, ed->ed_data);
	argLD(XtNeditType, XawtextEdit);
	argLD(XtNlength, ed->ed_size);
	argLD(XtNuseStringInPlace, True);
	argLD(XtNcallback, callbacks);
	ed->ed_text = XtCreateManagedWidget("text", asciiTextWidgetClass, et, args, nargs);

	XtPopup(ed->ed_popup, XtGrabNone);

}

/*
 * Callback for text widget
 * This gets called before the string is updated
 */
/* ARGSUSED */
static void
TextChanged(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	register EditLine *ed = (EditLine *) closure;

	if (MyCalendar)
		XtSetSensitive(ed->ed_save, True);
}


/* ARGSUSED */
static void
SaveEdits(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	register EditLine *ed = (EditLine *) closure;
	register MeWrap *mw;
	register Cardinal day;

	mw = ed->ed_meWrap;
	day = ed->ed_day;

	switch (StripType(mw)) {
	case ME_MONTHLY:
		if (WriteCalendarFile(mw, day, ed->ed_data) == False)
			return;
		break;
	case ME_WEEKLY:
		if (WriteWeeklyFile(mw, day, ed->ed_data) == False)
			return;
		break;
	}
	/*
	 * Otherwise change the displayed string
	 */
	if (mw->mw_have[day])
		XtFree(mw->mw_have[day]);
	mw->mw_have[day] = XtMalloc(strlen(ed->ed_data) + 1);
	strcpy(mw->mw_have[day], ed->ed_data);
	/*
	 * Update the visual image
	 */
	UpdateDayDetails(mw, day);
	XtSetSensitive(ed->ed_save, False);
}

static void
UpdateDayDetails(mw, day)
	MeWrap		*mw;
	Cardinal	day;
{
	Instance	*ins;
	int		nargs;
	Arg             args[3];

	XtSetArg(args[0], XtNlabel, mw->mw_have[day]);
	for (ins = mw->mw_list; ins; ins = ins->i_next) {
		nargs = 1;
		if (*mw->mw_have[day] == '\0') {
			argLD(XtNforeground, ins->i_col.fg);
			argLD(XtNbackground, ins->i_col.bg);
		}
		XtSetValues(ins->i_day_info[day], args, nargs);
	}

	/*
	 * worry about updating the memo system
	 */

	switch (StripType(mw)) {
	case ME_MONTHLY:
		if (today.day == day && today.month == mw->mw_month &&
		    today.year == mw->mw_year)
			UpdateMemo();
		break;
	case ME_WEEKLY:
		if (today.wday == day)
			UpdateMemo();
		break;
	}

}

static void
FinishEditing(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	register EditLine *ed = (EditLine *) closure;
	register MeWrap *mw;
	Cardinal        day;

	XtSetSensitive(ed->ed_quit, False);
	if (MyCalendar)
		XtSetSensitive(ed->ed_save, False);
	mw = ed->ed_meWrap;
	day = ed->ed_day;

	if (MyCalendar) {
		if (mw->mw_have[day] == NULL) {
			if (*ed->ed_data) {
				CheckExit(ed);
				return;
			}
		} else if (strcmp(mw->mw_have[day], ed->ed_data)) {
			CheckExit(ed);
			return;
		}
	}
	CleanEditPanel(w, ed, call_data);
}

static void
CleanEditPanel(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	register EditLine *ed = (EditLine *) closure;
	register EditLine *edl, *eds;
	register MeWrap *mw;
	Cardinal        day;
	Widget          popup;

	mw = ed->ed_meWrap;
	day = ed->ed_day;
	popup = ed->ed_popup;

	XtFree(ed->ed_data);

	for (edl = NULL, eds = mw->mw_ed;
	     eds;
	     edl = eds, eds = eds->ed_next) {
		if (eds == ed) {
			if (edl)
				edl->ed_next = ed->ed_next;
			else
				mw->mw_ed = ed->ed_next;
			break;
		}
	}
	XtFree((char *) ed);
	XtPopdown(popup);
	XtDestroyWidget(popup);

	SetAllButtons(mw->mw_list, day, True);
}

/*
 * We are trying to leave with saving the data
 * let us see if the user really wants to
 */
static void
CheckExit(ed)
	register EditLine *ed;
{

	DialogPopup(ed->ed_quit, CheckDia, ed, NULL);
}

/*
 * Here we do the work
 */
static void
CheckDia(pop, ed)
	Widget          pop;
	EditLine       *ed;
{
	Widget          dia;

	XtSetSensitive(ed->ed_quit, False);
	/* Take "Save file?" from resources */
	dia = XtCreateManagedWidget("check", dialogWidgetClass, pop, NULL, 0);
	XawDialogAddButton(dia, "yes", YesCheck, ed);
	XawDialogAddButton(dia, "no", NoCheck, ed);
}

/* ARGSUSED */
static void
YesCheck(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	SaveEdits(w, closure, call_data);
	CleanEditPanel(w, closure, call_data);
	XtDestroyWidget(XtParent(XtParent(w)));

}

/* ARGSUSED */
static void
NoCheck(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	CleanEditPanel(w, closure, call_data);
	XtDestroyWidget(XtParent(XtParent(w)));
}


/*
 * Slighty formatted XtError
 */
/* VARARGS1 */
void
Fatal(fmt, a, b)
	char           *fmt;
	char           *a;
	char           *b;
{
	char            buf[BUFSIZ];

	(void) sprintf(buf, fmt, a, b);
	XtAppError(appContext, buf);
	/* NOTREACHED */
}

/*
 * These routines used to insert text into a file
 * from the middle button press.
 * Code called from xcal_strip.c
 * The routine is responsible for freeing the text string
 */
void
AppendText(w, da, txt)
	Widget		w;
	Date		*da;
	String		txt;
{
	MeWrap		*mw;
	Cardinal	len;
	Cardinal	srclen;
	String		newstr;

	if ((mw = MeWrapSearch(da->year, da->month)) == NULL)
		mw = NewMeWrap();	/* shouldn`t happen */

	if (EditCheck(w, mw, da))
		return;

	/*
	 * if we have some text already, we must append the new text to it
	 */
	if (mw->mw_have[da->day]) {
		/*
		 * size of new string is sum of previous two
		 * plus two for the nulls at the end of the string
		 * plus one because we made need to insert a newline
		 * after the old text
		 */
		srclen = strlen(mw->mw_have[da->day]);
		if (srclen) {
			len = srclen + strlen(txt) +3;
			newstr = XtMalloc(len);
			strcpy(newstr, mw->mw_have[da->day]);
			if (newstr[srclen] == '\0') {
				newstr[srclen++] = '\n';
				newstr[srclen] = '\0';
			}
			strcat(newstr, txt);
			XtFree(txt);
			txt = newstr;
		}
	}

	switch (StripType(mw)) {
	case ME_MONTHLY:
		if (WriteCalendarFile(mw, da->day, txt) == False)
			return;
		break;
	case ME_WEEKLY:
		if (WriteWeeklyFile(mw, da->day, txt) == False)
			return;
		break;
	}
	/*
	 * Otherwise change the displayed string
	 */
	if (mw->mw_have[da->day])
		XtFree(mw->mw_have[da->day]);
	mw->mw_have[da->day] = txt;

	UpdateDayDetails(mw, da->day);
}
