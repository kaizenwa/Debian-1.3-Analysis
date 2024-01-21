#ifndef lint
static char	*RCSid = "$Id: note.c,v 2.1 1995/03/28 18:48:04 mjhammel Exp $";
#endif

/*
 * note.c - routines for handling notes.
 *
 * Originally by David A. Curry
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
 * mjhammel@csn.net
 *
 * $Log: note.c,v $
 * Revision 2.1  1995/03/28  18:48:04  mjhammel
 * changed options button to look like menu bar
 * changed horiz and vert distances of note form
 *
 * Revision 2.0  1995/03/27  18:56:22  mjhammel
 * Initial update to 2.0
 *
 *
 * Revision 1.4  90/06/14  11:20:09  davy
 * Ported to X11 Release 4.  Changed to get the note save position in an
 * ICCCM compliant (although kludgy) way.
 * 
 * Revision 1.3  89/01/10  09:57:24  davy
 * Changed XGetNormalHints to XGetWindowAttributes for getting the size of
 * the text window when saving.  XGetNormalHints comes up with the wrong
 * size, for some reason.
 * 
 * Revision 1.2  89/01/10  09:13:13  davy
 * Added XtChain... arguments to buttons and text window to prevent the
 * buttons from getting screwed up on a resize.
 * 
 * Revision 1.1  89/01/10  09:00:21  davy
 * Initial revision
 * 
 */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/TextSrc.h>
#include <X11/Xaw/TextSink.h>
#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xaw/AsciiSink.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Shell.h>
#include <sys/param.h>
#ifdef USE_DIR
#include <sys/dir.h>
#else
#include <dirent.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <time.h>


#include "xpostit.h"
#include "alarmon.xbm"
#include "alarmoff.xbm"

/* external variables */
extern Widget	dialogwidget;
extern Widget 	error, errdialog;		/* error dialog box */

/* external routines */
extern void ClearName();

/* notes that are hidden */
Widget		notelist[DefaultMaxNotes];

/* the pixmap for when alarms are set */
Pixmap		alarmon_pixmap;
Pixmap		alarmoff_pixmap;

/* static PostItNote	*notes = NULL;
 */
PostItNote	*notes = NULL;

static int raisestatus = 1;
static int showstatus = 1;

extern void ErrPopDown();
extern void ErrPopUp();
extern void TimerPopDown();
extern void CreateTimerPrompt();
extern void CreateInsertPrompt();
extern void CreateIOPrompt();
extern void MakeNoteList();
extern void EmailNote();
extern void InsertText();

PostItNote		*FindNote();
static PostItNote	*AllocNote();

static void		SaveNote();
static void		OpenNote();
static void		ExportNote();
static void		PrintNote();
static void		EraseNote();
static void		DestroyNote();
static void		CancelErase();
static void		ConfirmErase();
static void		CancelDestroy();
static void		ConfirmDestroy();
static void		MakeNoteWidget();
static int		NoteIndex();
static void		HideNote();
static void		NameNote();
static void		AnchorNote();
static void		UnAnchorNote();
static void		InsertCalendar();
static void		InsertDates();
static void		ConfirmName();
static void		CancelName();
static void 		GetNotePosition();
static void 		SetTimerNote();
static void 		UnSetTimerNote();
static void 		MakeAlarmPopup();
static void 		AlarmPopDown();
static void 		EmailNoteCB();
static int		StringToNoteSize();

/*
 * CreateNewNote - create a new note of the specified size.
 */
void
CreateNewNote(size)
int size;
{
	static int hpi = 0;
	static int wpi = 0;
	register PostItNote *pn;
	register int hmm, wmm, hpixel, wpixel;

	/*
	 * Find out the number of pixels per inch on the screen.  We
	 * can get the number of pixels, and the size in millimeters.
	 * Then we convert to pixels/inch using the formula
	 *
	 *       2.54 cm     10 mm     pixels     pixels
	 *	--------- x ------- x -------- = --------
	 *	  inch        cm         mm        inch
	 *
	 * The only problem with this is that some servers (like Xsun)
	 * lie about what these numbers really are.
	 */
	if ((hpi == 0) || (wpi == 0)) {
		hpixel = DisplayHeight(display, DefaultScreen(display));
		wpixel = DisplayWidth(display, DefaultScreen(display));

		hmm = DisplayHeightMM(display, DefaultScreen(display));
		wmm = DisplayWidthMM(display, DefaultScreen(display));

		hpi = (int) ((25.4 * hpixel) / (float) hmm + 0.5);
		wpi = (int) ((25.4 * wpixel) / (float) wmm + 0.5);
	}

	/*
	 * Calculate sizes for the note.
	 */
	switch (size) {
	case PostItNote_1p5x2:
		hpixel = 1.5 * hpi;
		wpixel = 2 * wpi;
		break;
	case PostItNote_2x3:
		hpixel = 2 * hpi;
		wpixel = 3 * wpi;
		break;
	case PostItNote_3x3:
		hpixel = 3 * hpi;
		wpixel = 3 * wpi;
		break;
	case PostItNote_3x4:
		hpixel = 3 * hpi;
		wpixel = 4 * wpi;
		break;
	case PostItNote_3x5:
		hpixel = 3 * hpi;
		wpixel = 5 * wpi;
		break;
	case PostItNote_4x6:
		hpixel = 4 * hpi;
		wpixel = 6 * wpi;
		break;
	default:
		hpixel = 1.5 * hpi;
		wpixel = 2 * wpi;
		break;
	}

	/*
	 * Allocate a new note structure.
	 */
	pn = AllocNote(NewIndex);

	/*
	 * Set the text window size.
	 */
	pn->pn_textwidth = wpixel;
	pn->pn_textheight = hpixel;

	/*
	 * Make the widget for the note.
	 */
	MakeNoteWidget(pn);
}

/*
 * LoadSavedNotes - load in the notes the user has saved.
 */
void
LoadSavedNotes()
{
	DIR *dp;
	FILE *fp;
	register PostItNote *pn;
#ifdef USE_DIR
	register struct direct *d;
#else
	register struct dirent *d;
#endif
	char buf[BUFSIZ], fname[MAXPATHLEN];
	int n, len, nlen, shellx, shelly, texth, textw, hidden, anchor, namelen;
	int pnalarm, month, day, hour, minute;
	int save_screenx, save_screeny;
	int version;
	time_t		time_tp;
	struct tm	*tm_tp;

	/*
	 * Try to open the directory.
	 */
	if ((dp = opendir(app_res.note_dir)) == NULL)
	{
		fprintf(stderr, "xpostit: ");
		perror(app_res.note_dir);
		return;
	}

	nlen = strlen(PostItNoteFname);

	/*
	 * For each entry...
	 */
	while ((d = readdir(dp)) != NULL) {
		/*
		 * Skip over anything which doesn't match our
		 * file naming scheme.
		 */
		if (strncmp(d->d_name, PostItNoteFname, nlen) != 0)
			continue;

		/*
		 * Make the full path name.
		 */
		sprintf(fname, "%s/%s", app_res.note_dir, d->d_name);

		/*
		 * Open the file.
		 */
		if ((fp = fopen(fname, "r")) == NULL)
			continue;

		/*
		 * Look for the magic cookie identifying this as
		 * a Post-It note.
		 */
		if (fscanf(fp, "%s", buf) == EOF) {
			fclose(fp);
			continue;
		}
		/*
		 * I changed the magic cookie so it didn't conflict with
		 * the Postscript magic cookie, but we need to maintain
		 * compatibility, if requested.
		 */
		version = 2;
		if (strcmp(buf, PostItNoteMagic_v2) != 0) {
			if (app_res.compatibility) {
				if (strcmp(buf, PostItNoteMagic) != 0) {
					fclose(fp);
					continue;
				}
				else 
					version = 1;
			}
		}
		else 
			version = 2;

		/*
		 * Get the note position and size information.
		 */
		fgets(buf, sizeof(buf), fp);

		if ( version == 1 )
		{
			n = sscanf(buf, "%d %d %d %d %d %d %d %d", 
				&shellx, &shelly, &texth, &textw, &len, 
				&hidden, &save_screenx, &save_screeny);
			/*
			 * Bad format; skip it.
			 */
			if (n < 5) {
				fclose(fp);
				continue;
			}
			if (n == 8) {
				/*
				 * New format, got screen size when note was saved
				 */
				if (curr_screenx!=save_screenx || curr_screeny!=save_screeny) {
					/*
					 * Saved on different sized screen, adjust coordinates 
					 */
					shellx = (shellx * curr_screenx) / save_screenx;
					shelly = (shelly * curr_screeny) / save_screeny;
				}
			}
		}
		else
		{
			n = sscanf(buf, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d", 
				&shellx, &shelly, &texth, &textw, &len, 
				&hidden, &anchor, &save_screenx, &save_screeny,
				&pnalarm, &month, &day, &hour, &minute);
			/*
			 * Bad format; skip it.
			 */
			if (n < 14) {
				fclose(fp);
				continue;
			}
			if (curr_screenx!=save_screenx || curr_screeny!=save_screeny) {
				/*
				 * Saved on different sized screen, adjust coordinates 
				 */
				shellx = (shellx * curr_screenx) / save_screenx;
				shelly = (shelly * curr_screeny) / save_screeny;
			}
		}


		/*
		 * Get the index number of this note.
		 */
		n = atoi(&(d->d_name[nlen]));

		/*
		 * Get a note structure.
		 */
		pn = AllocNote(n);

		/*
		 * Get the name of the note
		 */
		fgets(buf, sizeof(buf), fp);
		namelen = strlen ( buf ) - 1;
		/* remove newline character */
		bcopy ( (void *)"\0", (void *)(buf+namelen), 1 );
		pn->pn_name = XtNewString ( buf );

		/*
		 * Set the information.
		 */
		pn->pn_shellx = shellx;
		pn->pn_shelly = shelly;
		pn->pn_textwidth = textw;
		pn->pn_textheight = texth;
		pn->pn_positionit = True;
		pn->pn_hidden = (Boolean)hidden;
		if ( version == 2 )
		{
			pn->pn_anchor = (Boolean)anchor;
			pn->pn_alarm = (Boolean)pnalarm;
			pn->pn_alarm_mon = month;
			pn->pn_alarm_day = day;
			pn->pn_alarm_hour = hour;
			pn->pn_alarm_min = minute;
		}
		else
		{
			pn->pn_anchor = False;
			pn->pn_alarm = False;
			time(&time_tp);
			tm_tp = localtime(&time_tp);
			pn->pn_alarm_mon = tm_tp->tm_mon;
			pn->pn_alarm_day = tm_tp->tm_mday;
			pn->pn_alarm_hour = tm_tp->tm_hour;
			pn->pn_alarm_min = tm_tp->tm_min;
		}

		/*
		 * Save the file name.
		 */
		pn->pn_file = SafeAlloc(strlen(fname) + 1);
		strcpy(pn->pn_file, fname);

		/*
		 * If we need a bigger buffer than the default,
		 * get one.
		 */
		if (len >= pn->pn_textsize) {
			n = (len + app_res.buf_size - 1) / app_res.buf_size;
			n = n * app_res.buf_size;

			if ((pn->pn_text = (char *)realloc(pn->pn_text, n)) == NULL) {
				fprintf(stderr, "xpostit: out of memory.\n");
				ByeBye();
			}

			pn->pn_textsize = n;
		}

		/*
		 * Read in the text.
		 */
		fread(pn->pn_text, sizeof(char), len, fp);
		fclose(fp);

		/*
		 * Make a widget for this note.
		 */
		MakeNoteWidget(pn);
	}

	closedir(dp);
}

/*
 * RaiseAllNotes - raise all the notes by raising their shell windows.
 */
void
RaiseAllNotes()
{
	register PostItNote *pn;
	Boolean found;

	found = False;
	raisestatus = 1;
	for (pn = notes; pn != NULL; pn = pn->pn_next)
	{
		XRaiseWindow(display, XtWindow(pn->pn_shellwidget));
		found = True;
	}

	if ( !found )
		ErrPopUp("There are no notes to raise.");
}

/*
 * HideAllNotes - hide all the notes
 */
void
HideAllNotes()
{
	register PostItNote *pn;
	Boolean found;

	found = False;
	showstatus = 0;
	for (pn = notes; pn != NULL; pn = pn->pn_next)
	{
		if (pn->pn_hidden == False)
		{
			XtPopdown ( pn->pn_shellwidget );
			pn->pn_hidden = True;
			found = True;
		}
	}

	if ( !found )
		ErrPopUp("There are no notes to hide.");
}

/*
 * UnHideAllNotes - unhide all hidden notes
 */
void
UnHideAllNotes()
{
	register PostItNote *pn;
	Boolean	found;

	found = False;
	showstatus = 1;
	for (pn = notes; pn != NULL; pn = pn->pn_next)
		if ( pn->pn_hidden )
		{
			XtPopup(pn->pn_shellwidget, XtGrabNone);
			pn->pn_hidden = False;
			found = True;
		}

	if ( !found )
		ErrPopUp("There are no hidden notes to unhide.");
}

/*
 * LowerAllNotes - lower all the notes by lowering their shell windows.
 */
void
LowerAllNotes()
{
	register PostItNote *pn;
	Boolean found;

	found = False;
	raisestatus = 0;
	for (pn = notes; pn != NULL; pn = pn->pn_next)
	{
		XLowerWindow(display, XtWindow(pn->pn_shellwidget));
		found = True;
	}

	if ( !found )
		ErrPopUp("There are no notes to lower.");
}

/*
 * ToggleRaise - toggle raise status
 */
void
ToggleRaise() {
	if(!showstatus) {
		UnHideAllNotes(); RaiseAllNotes();
	} else
		raisestatus ? LowerAllNotes() : RaiseAllNotes();
}

/*
 * ToggleShow - toggle show status
 */
void
ToggleShow() {
	showstatus ? HideAllNotes() : UnHideAllNotes();
}

/*
 * SaveAllNotes - save all the notes.
 */
void
SaveAllNotes(showmsg)
int	showmsg;
{
	register PostItNote *pn;
	Boolean found;

	found = False;
	for (pn = notes; pn != NULL; pn = pn->pn_next)
	{
		SaveNote(pn->pn_shellwidget, (caddr_t) pn->pn_index, 0);
		found = True;
	}

	if ( showmsg )
		if ( !found )
			ErrPopUp("There are no notes to save.");
}

/*
 * MakeNewNote - called from the "tearoff" action.
 */
void
MakeNewNote(w, client_data, call_data)
XtPointer client_data, call_data;
Widget w;
{
	int notesize;

	if (call_data)
		notesize = StringToNoteSize((char *) call_data);
	else
		notesize = PostItNote_1p5x2;    /* arbitrary default */

	CreateNewNote(notesize);
}

/*
 * StringToNoteSize - convert strings to note sizes.
 */
static int
StringToNoteSize(s)
char *s;
{
	if (strcmp(s, "1.5x2") == 0)
		return(PostItNote_1p5x2);
	else if (strcmp(s, "2x3") == 0)
		return(PostItNote_2x3);
	else if (strcmp(s, "3x3") == 0)
		return(PostItNote_3x3);
	else if (strcmp(s, "3x4") == 0)
		return(PostItNote_3x4);
	else if (strcmp(s, "3x5") == 0)
		return(PostItNote_3x5);
	else if (strcmp(s, "4x6") == 0)
		return(PostItNote_4x6);
	else
		fprintf(stderr, "xpostit: bad note size: \"%s\".\n", s);

	return(PostItNote_1p5x2);
}

/*
 * CascadeNotes - cascade the notes on any anchor notes.
 * This isn't the best, but it distributes the notes among the notes
 * marked as anchors fairly evenly.
 */
void
CascadeNotes()
{
	register PostItNote *pn;
	PNLinks *linklist, *pnlink, *prevlink;
	int	nargs;
	Arg 	args[5];
	Dimension shellx, shelly;

	/*
	 * get a list of anchor notes
	 */
	linklist = pnlink = prevlink = NULL;
	for (pn = notes; pn != NULL; pn = pn->pn_next)
	{
		if ( pn->pn_anchor )
		{
			pnlink = (PNLinks *)SafeAlloc(sizeof (PNLinks));
			pnlink->pn = pn;
			pnlink->next = NULL;
			nargs = 0;
			SetArg(XtNx, &shellx);
			SetArg(XtNy, &shelly);
			XtGetValues(pn->pn_shellwidget, args, nargs);
			pnlink->x = (int)shellx;
			pnlink->y = (int)shelly;
			if ( linklist == NULL )
				linklist = pnlink;
			if ( prevlink != NULL )
				prevlink->next = pnlink;
			prevlink = pnlink;
		}
	}

	/*
	 * Now run the list of notes and put any non-anchored notes over the
	 * anchored notes.
	 */
	if ( linklist != NULL )
	{
		pnlink = linklist;
		for (pn = notes; pn != NULL; pn = pn->pn_next)
		{
			if ( ( ! pn->pn_anchor ) && ( ! pn->pn_hidden ) )
			{
				/*
				 * reposition the non-anchor down and to
				 * the right of the last note.
				 */
				nargs = 0;
				SetArg(XtNx, pnlink->x+app_res.anchor_offset);
				SetArg(XtNy, pnlink->y+app_res.anchor_offset);
				XtSetValues(pn->pn_shellwidget, args, nargs);
				XRaiseWindow(display, XtWindow(pn->pn_shellwidget));

				/*
				 * reset the anchor position
				 */
				pnlink->x = pnlink->x + app_res.anchor_offset;
				pnlink->y = pnlink->y + app_res.anchor_offset;

				/*
				 * move to the next anchor
				 */
				if ( pnlink->next != NULL )
					pnlink = pnlink->next;
				else
					pnlink = linklist;
			}
		}
	}

	/*
	 * Clean up that storage
	 */
	if ( linklist != NULL )
	{
		for ( pnlink = linklist, prevlink = linklist; pnlink != NULL; 
			prevlink = pnlink->next, free(pnlink), pnlink = prevlink );
		linklist = NULL;
	}
	else 
		ErrPopUp("There are no anchored notes.");
}

void
SetSaveSensitive(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote	*pn;

	pn = (PostItNote *)client_data;

	XtSetSensitive ( pn->pn_savewidget, True );
}

/*
 * MakeNoteWidget - make a widget for a Post-It note.
 */
static void
MakeNoteWidget(pn)
PostItNote *pn;
{
	Arg 		args[19];
	char 		geometry[64];
	Widget 		form, widgets[25];
	XtCallbackRec	callbacks[2];
	register int 	nargs, nwidgets;
	Dimension	width, shellx, shelly;

	bzero(callbacks, sizeof(callbacks));
	nargs = 0;

	/*
	 * If the shell window coordinates are valid, use them.
	 */
	if (pn->pn_positionit) {
		sprintf(geometry, "%dx%d+%d+%d", 
				pn->pn_textwidth, pn->pn_textheight, pn->pn_shellx, pn->pn_shelly);
		SetArg(XtNgeometry, geometry);
		SetArg(XtNx, pn->pn_shellx);
		SetArg(XtNy, pn->pn_shelly);
	}

	/*
	 * Make the shell widget for this note.  We just use
	 * a pop-up shell for this.
	 */
	SetArg(XtNtitle, pn->pn_name);
	pn->pn_shellwidget = XtCreatePopupShell("PostItNote",
						wmShellWidgetClass, toplevel,
						args, nargs);

	/*
	 * Make a form widget.
	 */
	nargs=0;
	SetArg(XtNdefaultDistance, 0);
	form = XtCreateManagedWidget("Note", formWidgetClass, pn->pn_shellwidget,
			      args, nargs);

	nwidgets = -1;

	/*
	 * Put each button into the shell widget.  The second and third
	 * buttons will position themselves relative to the first one;
	 * the first one will position itself relative to the edge of
	 * the form.
	 */

	/* the label first */

#ifdef NOWM
	nargs = 0;
	SetArg(XtNlabel, pn->pn_name);
	SetArg(XtNwidth, pn->pn_textwidth);
	SetArg(XtNtop, XtChainTop);
	SetArg(XtNbottom, XtChainTop);
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainRight);
	pn->pn_labelwidget = widgets[++nwidgets] = 
		XtCreateManagedWidget("Label", labelWidgetClass, form, args, nargs);
#endif

	/* create the File button */
	nargs = 0;
	SetArg(XtNlabel, "File");
	SetArg(XtNtop, XtChainTop);
	SetArg(XtNbottom, XtChainTop);
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNborderWidth, 0);
	SetArg(XtNhighlightThickness, 0);
#ifdef NOWM
	SetArg(XtNfromVert, pn->pn_labelwidget);
#endif
	SetArg(XtNmenuName, PostItFileMenu );
	pn->pn_filebutton = widgets[++nwidgets] = 
		XtCreateManagedWidget(
			"File", 
			menuButtonWidgetClass, 
			form, 
			args, nargs);


	/* create the options button
	 * In 2.3 this changed to a "Notes" button
	 */
	nargs = 0;
	SetArg(XtNlabel, "Notes");
	SetArg(XtNtop, XtChainTop);
	SetArg(XtNbottom, XtChainTop);
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNborderWidth, 0);
	SetArg(XtNhighlightThickness, 0);
#ifdef NOWM
	SetArg(XtNfromVert, pn->pn_labelwidget);
#endif
	SetArg(XtNfromHoriz, pn->pn_filebutton);
	SetArg(XtNmenuName, PostItOptionsMenu );
	pn->pn_options = widgets[++nwidgets] = 
		XtCreateManagedWidget(
			"Options", 
			menuButtonWidgetClass, 
			form, 
			args, nargs);


	/* create the alarm label and its pixmaps */
	alarmon_pixmap = XCreateBitmapFromData (
			XtDisplay(toplevel),
			RootWindowOfScreen(XtScreen(toplevel)),
			alarmon_bits, alarmon_width, alarmon_height);
	alarmoff_pixmap = XCreateBitmapFromData (
			XtDisplay(toplevel),
			RootWindowOfScreen(XtScreen(toplevel)),
			alarmoff_bits, alarmoff_width, alarmoff_height);
	nargs = 0;
	if ( pn->pn_alarm )
	{
		SetArg(XtNbitmap, alarmon_pixmap);
	}
	else
	{
		SetArg(XtNbitmap, alarmoff_pixmap);
	}
	SetArg(XtNtop, XtChainTop);
	SetArg(XtNbottom, XtChainTop);
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainLeft);
	SetArg(XtNborderWidth, 0);
	SetArg(XtNhighlightThickness, 0);
#ifdef NOWM
	SetArg(XtNfromVert, pn->pn_labelwidget);
#endif
	SetArg(XtNfromHoriz, pn->pn_options);
	pn->pn_alarmwidget = widgets[++nwidgets] = 
		XtCreateManagedWidget(
				"AlarmIcon", 
				labelWidgetClass, 
				form, 
				args, nargs);

	/* hack - make the button a little wider since it won't span the
	 * width of the note
	 */
	nargs = 0;
	SetArg(XtNwidth, &width);
	XtGetValues(pn->pn_options, args, nargs);
	width+=15;
	nargs = 0;
	SetArg(XtNwidth, width);
	XtSetValues(pn->pn_options, args, nargs);

	/* now create the pop down menus */

	/* the Notes menu */
	nargs = 0;
	SetArg(XtNlabel, "Menu");
	pn->pn_optionsmenu = 
		XtCreatePopupShell(
			PostItOptionsMenu, 
			simpleMenuWidgetClass, 
			pn->pn_options, 
			args, nargs);
	
	/* add a line seperator */
	widgets[++nwidgets] = 
		XtCreateManagedWidget(
			"OptionsLine", 
			smeLineObjectClass, 
			pn->pn_optionsmenu, 
			NULL, 0);
	
	/* the File menu */
	nargs = 0;
	SetArg(XtNlabel, "Menu");
	pn->pn_filemenu = 
		XtCreatePopupShell(
			PostItFileMenu, 
			simpleMenuWidgetClass, 
			pn->pn_filebutton, 
			args, nargs);
	
	/* add a line seperator */
	widgets[++nwidgets] = 
		XtCreateManagedWidget(
			"FileLine", 
			smeLineObjectClass, 
			pn->pn_filemenu, 
			NULL, 0);
	

	/* all the menu buttons */

	SetCallback(SaveNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	pn->pn_savewidget = widgets[++nwidgets] = 
		XtCreateManagedWidget(
			"Save", 
			smeBSBObjectClass, 
			pn->pn_filemenu, 
			args, nargs);

	SetCallback(OpenNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	widgets[++nwidgets] = 
		XtCreateManagedWidget(
			"Open", 
			smeBSBObjectClass, 
			pn->pn_filemenu, 
			args, nargs);

	SetCallback(ExportNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	widgets[++nwidgets] = 
		XtCreateManagedWidget(
			"Export", 
			smeBSBObjectClass, 
			pn->pn_filemenu, 
			args, nargs);


	SetCallback(PrintNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	widgets[++nwidgets] = XtCreateManagedWidget(
			"Print", 
			smeBSBObjectClass, 
			pn->pn_filemenu, 
			args, nargs);

	SetCallback(EmailNoteCB, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Email...");
	widgets[++nwidgets] = XtCreateManagedWidget(
			"EmailNote", 
			smeBSBObjectClass, 
			pn->pn_filemenu, 
			args, nargs);

	SetCallback(HideNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	widgets[++nwidgets] = XtCreateManagedWidget(
			"Hide", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	SetCallback(EraseNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	widgets[++nwidgets] = XtCreateManagedWidget(
			"Erase", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	widgets[++nwidgets] = XtCreateManagedWidget(
			"Line", 
			smeLineObjectClass,
			pn->pn_optionsmenu, 
			NULL, 0);

	SetCallback(DestroyNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	widgets[++nwidgets] = XtCreateManagedWidget(
			"Destroy", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	widgets[++nwidgets] = XtCreateManagedWidget(
			"Line", 
			smeLineObjectClass,
			pn->pn_optionsmenu, 
			NULL, 0);

	SetCallback(NameNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Name...");
	widgets[++nwidgets] = XtCreateManagedWidget(
			"Name", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	SetCallback(AnchorNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	pn->pn_anchorwidget = widgets[++nwidgets] = XtCreateManagedWidget(
			"Anchor", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	SetCallback(UnAnchorNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	pn->pn_unanchorwidget = widgets[++nwidgets] = XtCreateManagedWidget(
			"UnAnchor", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	if ( pn->pn_anchor )
	{
		XtSetSensitive ( pn->pn_unanchorwidget, True );
		XtSetSensitive ( pn->pn_anchorwidget, False );
	}
	else
	{
		XtSetSensitive ( pn->pn_unanchorwidget, False );
		XtSetSensitive ( pn->pn_anchorwidget, True );
	}

	SetCallback(SetTimerNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Set Alarm...");
	pn->pn_settimewidget = widgets[++nwidgets] = XtCreateManagedWidget(
			"SetAlarm", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	SetCallback(UnSetTimerNote, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Unset Alarm");
	pn->pn_unsettimewidget = widgets[++nwidgets] = XtCreateManagedWidget(
			"UnsetAlarm", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	if ( !app_res.noalarm )
	{
		if ( pn->pn_alarm )
		{
			XtSetSensitive ( pn->pn_unsettimewidget, True );
			XtSetSensitive ( pn->pn_settimewidget, False );
		}
		else
		{
			XtSetSensitive ( pn->pn_unsettimewidget, False );
			XtSetSensitive ( pn->pn_settimewidget, True );
		}
	}
	else
	{
		XtSetSensitive ( pn->pn_unsettimewidget, False );
		XtSetSensitive ( pn->pn_settimewidget, False );
	}

	SetCallback(InsertCalendar, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Insert Calendar");
	widgets[++nwidgets] = XtCreateManagedWidget(
			"InsertCalendar", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);

	SetCallback(InsertDates, pn->pn_index);

	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "Insert Date...");
	widgets[++nwidgets] = XtCreateManagedWidget(
			"InsertDate", 
			smeBSBObjectClass,
			pn->pn_optionsmenu, 
			args, nargs);


	/*
	 * Create the text window.
	 */

	/* set its callback, for when a character has changed in the text */
	SetCallback(SetSaveSensitive, pn);

	nargs = 0;
	SetArg(XtNtop, XtChainTop);
	SetArg(XtNleft, XtChainLeft);
	SetArg(XtNright, XtChainRight);
	SetArg(XtNbottom, XtChainBottom);
	SetArg(XtNfromVert, pn->pn_options);
	SetArg(XtNstring, pn->pn_text);
	SetArg(XtNeditType, XawtextEdit);
	SetArg(XtNuseStringInPlace, True);
	SetArg(XtNlength, pn->pn_textsize);
	SetArg(XtNwidth, pn->pn_textwidth);
	SetArg(XtNheight, pn->pn_textheight);
	SetArg(XtNresize, XawtextResizeNever);
	SetArg(XtNhorizDistance, 0);
	SetArg(XtNvertDistance, 0);
	SetArg(XtNcallback, callbacks);

	if (app_res.scroll_bar) {
		SetArg(XtNscrollVertical, XawtextScrollAlways);
	}
	else {
		SetArg(XtNscrollVertical, XawtextScrollWhenNeeded);
	}

	widgets[++nwidgets] = XtCreateManagedWidget("NoteText", asciiTextWidgetClass,
					     form, args, nargs);

	/*
	 * Save the text widget.
	 */
	pn->pn_textwidget = widgets[nwidgets];

	/*
	 * Let the top level shell know all these guys are here.
	 */
	XtSetSensitive ( pn->pn_savewidget, False );

	/*
	 * Realize the note and pop it up.
	 */
	XtRealizeWidget(pn->pn_shellwidget);

	/*
	 * set the note shells coordinates in the data struct
	 */
	nargs = 0;
	SetArg ( XtNx, &shellx);
	SetArg ( XtNy, &shelly);
	XtGetValues ( pn->pn_shellwidget, args, nargs);
	pn->pn_shellx = (int) shellx;
	pn->pn_shelly = (int) shelly;

	/* don't display if its a hidden note */
	if ( pn->pn_hidden == False )
		XtPopup(pn->pn_shellwidget, XtGrabNone);
}

/*
 * Open a file (ie import it) to a note
 */
static void
OpenNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	char *path;
	Boolean	freeit;


	freeit = False;

	/*
	 * Find the note we're opening.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/* get configured or current directory, whichever applies */
	if ( app_res.home_dir == NULL )
	{
		if ( getenv("HOME") == NULL )
		{
			path = "/";
		}
		else
		{
			path = (char *)malloc (strlen(getenv("HOME")));
			strcpy(path, getenv("HOME"));
			freeit = True;
		}
	}
	else
	{
		path = (char *)malloc (strlen(app_res.home_dir));
		strcpy(path, app_res.home_dir);
			freeit = True;
	}

	/*
	 * Call CreateIOPrompt() to allow user to select or name a file
	 */
	CreateIOPrompt(pn, XPOSTIT_OPEN_FILE, path);

	if (freeit)
		free(path);
}


/*
 * Export a note to a file (ie some other file besides the notes file)
 */
static void
ExportNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	char *path;
	Boolean	freeit;

	freeit = False;

	/*
	 * Find the note we're exporting.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/* get configured or current directory, whichever applies */
	if ( app_res.home_dir == NULL )
	{
		if ( getenv("HOME") == NULL )
		{
			path = "/";
		}
		else
		{
			path = (char *)malloc (strlen(getenv("HOME")));
			strcpy(path, getenv("HOME"));
			freeit = True;
		}
	}
	else
	{
		path = (char *)malloc (strlen(app_res.home_dir));
		strcpy(path, app_res.home_dir);
			freeit = True;
	}

	/*
	 * Call CreateIOPrompt() to allow user to select or name a file
	 */
	CreateIOPrompt(pn, XPOSTIT_EXPORT_FILE, path);

	if (freeit)
		free(path);
}


/*
 * SaveNote - save a note to a file.
 * Use temporary files for saves in case the system crashes or xpostit
 * exits unexpectedly during a save.  This will preserve, at a minimum,
 * the last copy of the note.
 */
static void
SaveNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	FILE *fp;
	char *MakeFname();
	register PostItNote *pn;
	XWindowAttributes win_attributes;
	Arg args[5];
	int nargs;
	int len, shellx, shelly, texth, textw;
	Boolean saveit;
	Boolean sensitive;
	char	tmpfile1[512], tmpfile2[512];
	struct stat stat_buf;
	char	cmd[512];


	/*
	 * Find the note we're saving.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/* by default, we're not going to save the note */
	saveit = False;

	/*
	 * if the notes text has changed, the sensitivity of the note
	 * save option will be True.
	 */
	nargs = 0;
	SetArg(XtNsensitive, &sensitive);
	XtGetValues(pn->pn_savewidget, args, nargs);
	if ( sensitive )
		saveit = True;

	/*
	 * Get the position of the shell window.
	 */
	GetNotePosition(pn->pn_shellwidget, &shellx, &shelly);

	if ( (pn->pn_shellx != shellx ) || (pn->pn_shelly != shelly ) )
	{
		pn->pn_shellx = shellx;
		pn->pn_shelly = shelly;
		saveit = True;
	}

	/*
	 * if nothing about the note has changed, don't waste effor saving it
	 */
	if ( !saveit )
		return;

	/*
	 * If it doesn't have a file name, make one.
	 */
	if (pn->pn_file == NULL)
		pn->pn_file = MakeFname(pn->pn_index);


	/*
	 * Get the size of the text window.
	 */
	XGetWindowAttributes(display, XtWindow(pn->pn_shellwidget),
			     &win_attributes);
	textw = win_attributes.width;
	texth = win_attributes.height;

	/*
	 * Get the length of the text in the window.
	 */
	len = strlen(pn->pn_text);

	/*
	 * Create the temporary filenames.  These temporary files go in
	 * the same directory as the real notes to avoid problems with
	 * renaming files across file system boundaries, etc.
	 */
	sprintf(tmpfile1,"%s/xp1.%d", app_res.note_dir, (int)getpid());
	sprintf(tmpfile2,"%s/xp2.%d", app_res.note_dir, (int)getpid());

	/*
	 * Open the first temporary file for writing
	 */
	if ((fp = fopen(tmpfile1, "w")) == NULL) {
		fprintf(stderr, "xpostit: ");
		perror(tmpfile1);
		return;
	}

	/*
	 * Print out the information needed to recreate the note.
	 */
	fprintf(fp, "%s %d %d %d %d %d %d %d %d %d %d %d %d %d %d \n%s\n", 
		PostItNoteMagic_v2, shellx, shelly, texth, textw, 
		len, pn->pn_hidden, pn->pn_anchor,
            	HeightOfScreen(XtScreen(pn->pn_shellwidget)),
            	WidthOfScreen(XtScreen(pn->pn_shellwidget)),
		pn->pn_alarm,
		pn->pn_alarm_mon, pn->pn_alarm_day, 
		pn->pn_alarm_hour, pn->pn_alarm_min, 
		pn->pn_name);

	/*
	 * Write out the text of the note.
	 */
	if (len)
		fwrite(pn->pn_text, sizeof(char), len, fp);

	fclose(fp);

	/* rename the old note (pn->pn_file, if it exists) to tmp2 */
	if ( stat(pn->pn_file, &stat_buf) == 0 )
	{
		if ( rename(pn->pn_file, tmpfile2) != 0 )
		{
			/* error - couldn't do save */
			sprintf(cmd, "xpostit: %s-%s", pn->pn_file, tmpfile2);
			perror(cmd);
			sprintf(cmd,"Couldn't rename %s\nto %s.\nSave of note failed.\n"
				"%s should contain valid copy of note.", 
				pn->pn_file, tmpfile2, tmpfile2);
			ErrPopUp(cmd);
			return;
		}

		/* check that rename worked (due to NFS problems) */
		if ( stat(tmpfile2, &stat_buf) != 0 )
		{
			/* rename didn't work! Goddam NFS.... */
			sprintf(cmd,"Rename %s to\n %s failed.\n"
				"Might be NFS problems.\nSave of note failed.", 
				pn->pn_file, tmpfile2);
			ErrPopUp(cmd);
			return;
		}
	}

	/* rename tmp1 note to old note */
	if ( rename(tmpfile1, pn->pn_file) != 0 )
	{
		/* error - couldn't do save - clean up */
		sprintf(cmd, "xpostit: %s-%s", tmpfile1, pn->pn_file);
		perror(cmd);
		sprintf(cmd,"Couldn't rename %s\nto %s.\n"
			"Save of note failed."
			"%s should contain valid copy of note.", 
			tmpfile1, pn->pn_file, tmpfile1);
		ErrPopUp(cmd);
		return;
	}

	/* check that rename worked (due to NFS problems) */
	if ( stat(pn->pn_file, &stat_buf) != 0 )
	{
		/* rename didn't work! Goddam NFS.... */
		sprintf(cmd,"Rename %s to\n %s failed.\n"
			"Might be NFS problems.\nSave of note failed.", 
			tmpfile1, pn->pn_file );
		ErrPopUp(cmd);
		return;
	}

	/* unlink tmp2 */
	if ( stat(tmpfile2, &stat_buf) == 0 )
	{
		if ( unlink(tmpfile2) != 0 )
		{
			sprintf(cmd,"Problems doing cleanup after save.\n"
				"%s could not be unlinked.\n", tmpfile2);
			ErrPopUp(cmd);
		}
	}

	/* reset save button to be insensitive */
	XtSetSensitive ( pn->pn_savewidget, False );
}

/*
 * EraseNote - erase all the text in a note.
 */
static void
EraseNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	XtCallbackRec cancel[2], confirm[2];

	bzero(confirm, sizeof(confirm));
	bzero(cancel, sizeof(cancel));

	/*
	 * Find the note we're erasing.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/*
	 * If there's nothing in the window, then there's
	 * no need to erase it.
	 */
	if (strlen(pn->pn_text) == 0)
		return;

	confirm[0].callback = ConfirmErase;
	confirm[0].closure = (caddr_t) pn->pn_index;
	cancel[0].callback = CancelErase;
	cancel[0].closure = (caddr_t) pn->pn_index;

	/*
	 * Get confirmation of what they want to do.
	 */
	ConfirmIt(confirm, cancel);
}

/*
 * DestroyNote - destroy a note.
 */
static void
DestroyNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	XtCallbackRec cancel[2], confirm[2];

	bzero(confirm, sizeof(confirm));
	bzero(cancel, sizeof(cancel));

	/*
	 * Find the note we're destroying.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	confirm[0].callback = ConfirmDestroy;
	confirm[0].closure = (caddr_t) pn->pn_index;
	cancel[0].callback = CancelDestroy;
	cancel[0].closure = (caddr_t) pn->pn_index;

	/*
	 * Get confirmation of what they want to do.
	 */
	ConfirmIt(confirm, cancel);
}

/*
 * PrintNote - prints a note.
 */
static void
PrintNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	FILE	*fp;
	int	len;
	char	tmpfile[512];
	char	cmd[1024];

#ifdef DEBUG
	int	rc;
#endif

	/*
	 * Find the note we're printing.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/*
	 * Save the text to a temporary file
	 */
	sprintf(tmpfile,"%s/xp%d", app_res.tmp_dir, (int)getpid());
	if ((fp = fopen(tmpfile, "w")) == NULL) {
		fprintf(stderr, "xpostit: ");
		perror(tmpfile);
		sprintf(cmd,"Can't open tempoary file for\nwriting: %s", tmpfile);
		ErrPopUp(cmd);
		return;
	}
	len = strlen(pn->pn_text);
	if (len)
		fwrite(pn->pn_text, sizeof(char), len, fp);
	else
	{
		ErrPopUp("No text in note to print!");
		return;
	}
	fclose(fp);

	/*
	 * Use the printcmd resource to determine how to print the note
	 */
	sprintf (cmd, app_res.print_cmd, tmpfile );
#ifdef DEBUG
	printf("Command used to print the note: %s\n", cmd);
	rc = system ( cmd );
	switch ( rc )
	{
		case 127:
			sprintf(cmd,"%s failed: errno=%d", cmd, errno);
			ErrPopUp(cmd);
			break;
			
		case -1:
			sprintf(cmd,"%s failed: errno=%d", cmd, rc);
			ErrPopUp(cmd);
			break;
			
		default:
			sprintf(cmd,"%s succeeded: rc=%d", cmd, rc);
			ErrPopUp(cmd);
			break;
	}
#else
	system ( cmd );
#endif

	/*
	 * clean up the temporary file
	 */
	sprintf (cmd, "rm %s", tmpfile );
	system ( cmd );

}


/*
 * EmailNoteCB - email a note.
 */
static void
EmailNoteCB(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;

	/*
	 * Find the note we're emailing.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	CreateEmailNotePrompt(pn);
}

/*
 * NameNote - name a note.
 */
static void
NameNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	XtCallbackRec cancel[2], confirm[2];

	bzero(confirm, sizeof(confirm));
	bzero(cancel, sizeof(cancel));

	/*
	 * Find the note we're naming.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	confirm[0].callback = ConfirmName;
	confirm[0].closure = (caddr_t) pn->pn_index;
	cancel[0].callback = CancelName;
	cancel[0].closure = (caddr_t) pn->pn_index;

	/*
	 * Get the title the user wants for this postitnote
	 */
	NameIt(pn, confirm, cancel);
}

/*
 * AnchorNote - name a note.
 */
static void
AnchorNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;

	/*
	 * Find the note we're naming.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	pn->pn_anchor = True;
	XtSetSensitive ( pn->pn_unanchorwidget, True );
	XtSetSensitive ( pn->pn_anchorwidget, False );
	XtSetSensitive ( pn->pn_savewidget, True );
}

/*
 * UnAnchorNote - name a note.
 */
static void
UnAnchorNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;

	/*
	 * Find the note we're naming.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	pn->pn_anchor = False;
	XtSetSensitive ( pn->pn_unanchorwidget, False );
	XtSetSensitive ( pn->pn_anchorwidget, True );
	XtSetSensitive ( pn->pn_savewidget, True );
}

/*
 * HideNote - hide a note.
 */
static void
HideNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote 	*pn;

	/*
	 * Find the note we're hiding.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/* mark it hidden */
	pn->pn_hidden = True;

	/*
	 * pop the window down
	 */
	XtPopdown ( pn->pn_shellwidget );
}

/*
 * MakeNoteListCB - create a list of the notes for the
 * user to choose from so a particular note can be
 * popped up at the current mouse pointer location.
 * This is really just a front end to MakeNoteList
 * so we can pass the notes pointer.
 */
void
MakeNoteListCB(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	CreateFindNotePrompt(notes, False);
}

/*
 * SetTimerNote - callback for setting the alarm for a note
 */
static void
SetTimerNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote 	*pn;

	/*
	 * Find the note we're hiding.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	CreateTimerPrompt(pn);

}

/*
 * UnSetTimerNote - callback for removing the alarm for a note
 */
static void
UnSetTimerNote(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote 	*pn;
	Arg			args[5];
	int			nargs;

	/*
	 * Find the note we're hiding.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	XtSetSensitive ( pn->pn_unsettimewidget, False );
	XtSetSensitive ( pn->pn_settimewidget, True );
	XtSetSensitive ( pn->pn_savewidget, True );

	/*
	 * Turn off the alarms settings
	 */
	pn->pn_alarm = False;

	/* turn off the alarm pixmap */
	nargs = 0;
	SetArg(XtNbitmap, alarmoff_pixmap);
	XtSetValues(pn->pn_alarmwidget, args, nargs);
}

/*
 * InsertCalendar - insert this months calendar into the note
 */
static void
InsertCalendar(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote *pn;
	char	tmpfile[512];
	char	cmd[1024];
	int	rc;

	/*
	 * Find the note we're adding the calender to.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/*
	 * Run the calendar generating program to get the text.
	 * We use system() instead of popen() because the latter
	 * doesn't reliably tell us if the process actually ran.
	 * system() will provide the return value of the invoked
	 * program.
	 */
	sprintf(tmpfile,"%s/xp%d", app_res.tmp_dir, (int)getpid());
	sprintf (cmd, "%s > %s", app_res.calendar_cmd, tmpfile );
	if ( (rc=system ( cmd )) != 0 ) {

		sprintf(tmpfile,"Couldn't run %s: return code = %d", 
			app_res.calendar_cmd, rc);
		ErrPopUp(tmpfile);
		/* cleanup, if necessary */
		sprintf (cmd, "rm %s", tmpfile );
		system ( cmd );
		return;
	}

	/* stuff the output into the note */
	InsertText(pn, tmpfile, True, FILE_TYPE);

	XtSetSensitive ( pn->pn_savewidget, True );
}


/*
 * InsertDates - callback for inserting a formatted date in the note
 */
static void
InsertDates(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	PostItNote 	*pn;

	/*
	 * Find the note we're hiding.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	CreateInsertPrompt(pn);

}

/*
 * ConfirmName - callback for name confirmation.
 */
static void
ConfirmName(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	Arg 			args[4];
	register int		nargs;
	register PostItNote	*pn;
	String			string;

	/*
	 * Get rid of the confirmation box.
	 */
	ClearName();

	/*
	 * Find the note we're naming.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/* get the name of the shell widget */
	string = XawDialogGetValueString ( dialogwidget );

	/* don't change the name if no text was entered */
	if ( strcmp ( string, "" ) != 0 )
	{
#ifdef NOWM
		nargs = 0;
		SetArg(XtNlabel, string );
		XtSetValues(pn->pn_labelwidget, args, nargs);
#endif
		nargs = 0;
		SetArg(XtNtitle, string );
		XtSetValues(pn->pn_shellwidget, args, nargs);

		/* I should just copy the name, but I'm not clear on
		 * how you do this with X Strings - mjh
		 */
		if ( pn->pn_name != NULL )
			XtFree ( pn->pn_name );
		pn->pn_name = XtNewString ( string );
		XtSetSensitive ( pn->pn_savewidget, True );
	}
}


/*
 * CancelName - callback for name cancellation.
 */
static void
CancelName(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	/*
	 * Get rid of the confirmation box.
	 */
	ClearName();
}


/*
 * ConfirmErase - callback for erase confirmation.
 */
static void
ConfirmErase(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	Arg args[4];
	register int nargs;
	register PostItNote *pn;

	/*
	 * Get rid of the confirmation box.
	 */
	ClearConfirm();

	/*
	 * Find the note we're erasing.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/*
	 * This is a kludge.  They should have provided
	 * an XtTextErase function.
	 */
	bzero(pn->pn_text, pn->pn_textsize);

	nargs = 0;
	SetArg(XtNstring, pn->pn_text);
	XtSetValues(pn->pn_textwidget, args, nargs);
	XtSetSensitive ( pn->pn_savewidget, True );
}

/*
 * CancelErase - callback for erase cancellation.
 */
static void
CancelErase(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	/*
	 * Get rid of the confirmation box.
	 */
	ClearConfirm();
}

/*
 * ConfirmDestroy - callback for destroy confirmation.
 */
static void
ConfirmDestroy(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	register PostItNote *pn, *prev;

	/*
	 * Get rid of the confirmation box.
	 */
	ClearConfirm();

	/*
	 * Find the note we're destroying.
	 */
	if ((pn = FindNote((int) client_data)) == NULL)
		return;

	/*
	 * Get rid of the widgets for this note.
	 */
	XtPopdown(pn->pn_shellwidget);
	XtDestroyWidget(pn->pn_shellwidget);

	/*
	 * Get rid of the note structure.
	 */
	if (pn != notes) {

		/*
		 * first, free up the name buffer
		 */
		if ( pn->pn_name != NULL )
			XtFree ( pn->pn_name );

		for (prev = notes; prev->pn_next; prev = prev->pn_next) {
			if (prev->pn_next == pn)
				break;
		}

		prev->pn_next = pn->pn_next;
	}
	else {
		notes = pn->pn_next;
	}

	/*
	 * Get rid of the file.
	 */
	if (pn->pn_file) {
		unlink(pn->pn_file);
		free(pn->pn_file);
	}

	/*
	 * Free the memory we used.
	 */
	free(pn->pn_text);
	free(pn);
}

/*
 * CancelDestroy - callback for destroy cancellation.
 */
static void
CancelDestroy(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	/*
	 * Get rid of the confirmation box.
	 */
	ClearConfirm();
}

/*
 * AllocNote - allocate a new note structure and insert in into the
 *	       list of notes.
 */
static PostItNote *
AllocNote(index)
int index;
{
	register PostItNote	*pn;
	char			label[15];		
	time_t			time_tp;
	struct tm		*tm_tp;

	/*
	 * Allocate a structure.
	 */
	if (notes == NULL) {
		notes = (PostItNote *) SafeAlloc(sizeof(PostItNote));
		pn = notes;
	}
	else {
		for (pn = notes; pn->pn_next != NULL; pn = pn->pn_next)
			;

		pn->pn_next = (PostItNote *) SafeAlloc(sizeof(PostItNote));
		pn = pn->pn_next;
	}

	/*
	 * Initialize the note.
	 */
	pn->pn_positionit = False;
	pn->pn_hidden = False;
	pn->pn_anchor = False;
	pn->pn_alarm = False;

	/* set todays date in the alarm entries */
	time(&time_tp);
	tm_tp = localtime(&time_tp);
	pn->pn_alarm_mon = tm_tp->tm_mon;
	pn->pn_alarm_day = tm_tp->tm_mday;
	pn->pn_alarm_hour = tm_tp->tm_hour;
	pn->pn_alarm_min = tm_tp->tm_min;

	pn->pn_hide_index = -1;
	pn->pn_textsize = app_res.buf_size;
	pn->pn_text = SafeAlloc(pn->pn_textsize);

	/*
	 * If the index number was given, use it.  Otherwise,
	 * get a new index number.
	 */
	pn->pn_index = (index == NewIndex ? NoteIndex() : index);

	/* save the name of the note */
	sprintf (label, "Note %d", pn->pn_index);
	pn->pn_name = XtNewString ( label );

	return(pn);
}

/*
 * FindNote - find the note structure with the given index number.
 */
PostItNote *
FindNote(index)
register int index;
{
	register PostItNote *pn;

	for (pn = notes; pn != NULL; pn = pn->pn_next) {
		if (pn->pn_index == index)
			return(pn);
	}

	return(NULL);
}

/*
 * NoteIndex - find the lowest free index number.
 */
static int
NoteIndex()
{
	register int index;
	register PostItNote *pn;

	/*
	 * This is O(n**2), but the list should be small.
	 */
	for (index = 1; ; index++) {
		if ((pn = FindNote(index)) == NULL)
			return(index);
	}
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
 *		     Thanks to Stewart Levin for the original code.
 */
static void
GetNotePosition(w, x, y)
int *x, *y;
Widget w;
{
	Window *children;
	int status, nchildren;
	Window here, parent, root;
	XWindowAttributes win_attributes;

	parent = XtWindow(w);

	/*
	 * Walk up the tree looking for a parent of this window whose
	 * parent is the root.  That'll either be this window (if there
	 * is no window manager window) or the window added by the
	 * window manager.
	 */
	do {
		here = parent;

		status = XQueryTree(display, here, &root, &parent, &children,
				    &nchildren);

		if (!status)
			break;

		if (children)
			XFree(children);
	} while (parent != root);

	/*
	 * Get the attributes of this window we just found.
	 */
	XGetWindowAttributes(display, here, &win_attributes);

	/*
	 * Now deduct the border from the position of the window.
	 * We know the coordinates don't need translating, since
	 * this window's parent is the root.
	 */
	*x = win_attributes.x - win_attributes.border_width;
	*y = win_attributes.y - win_attributes.border_width;
}


/*
 * Check the notes for any alarms that need to go off
 */
void
AlarmCheck(client_data, id)
XtPointer client_data;
XtIntervalId	*id;
{
	register PostItNote *pn;
	XtIntervalId	alarm_timer;
	time_t		time_tp;
	struct tm	*tm_tp;

	for (pn = notes; pn != NULL; pn = pn->pn_next)
	{
		if (pn->pn_alarm)
		{
			time(&time_tp);
			tm_tp = localtime(&time_tp);
			if ( (pn->pn_alarm_mon == tm_tp->tm_mon) &&
			     (pn->pn_alarm_day == tm_tp->tm_mday) &&
			     (pn->pn_alarm_hour == tm_tp->tm_hour) &&
			     (pn->pn_alarm_min == tm_tp->tm_min) )
			{
				MakeAlarmPopup(pn);
			}
		}
	}
	alarm_timer = XtAppAddTimeOut (
			XtWidgetToApplicationContext(toplevel),
			alarm_interval,
			AlarmCheck,
			NULL );
}

/*
 * Create popups for alarms so user knows the alarm has expired.
 */
static void
MakeAlarmPopup(pn)
PostItNote *pn;
{
	int		i;
	Arg		args[4];
	int		nargs;
	Widget		alarmshell, alarmdialog, alarmconfirm;
	Window 		root, child;
	unsigned int 	buttons;
	int 		root_x, root_y, child_x, child_y;
	char		string[512];
	XtCallbackRec	callbacks[2];

	/*
	 * Create a popup shell widget
	 */
	nargs = 0;
	SetArg(XtNtitle, PostItAlarmDialog);
	alarmshell = XtCreatePopupShell(
			"AlarmShell", 
			transientShellWidgetClass,
			toplevel, 
			args, nargs);

	/*
	 * Create the alarm dialog box
	 */
	alarmdialog = XtCreateManagedWidget(
				"alarmDialog",
				dialogWidgetClass,
				alarmshell,
				NULL, 0);

	/*
	 * Create the alarm dialog confirmation button
	 */
	bzero(callbacks, sizeof(callbacks));
	SetCallback(AlarmPopDown, (XtPointer)alarmshell);
	nargs = 0;
	SetArg(XtNcallback, callbacks);
	SetArg(XtNlabel, "OK");
	alarmconfirm = XtCreateManagedWidget(
				"alarmconfirm",
				commandWidgetClass,
				alarmdialog,
				args, nargs);

	/* get mouse location */
	XQueryPointer(display, XtWindow(toplevel), &root, &child,
		      &root_x, &root_y, &child_x, &child_y, &buttons);

	sprintf(string,
		"The alarm has expired for the PostIt Note\n"
		"with the following name:\n"
		"\"%s\"", pn->pn_name);

	nargs = 0;
	SetArg(XtNlabel, string);
	XtSetValues(alarmdialog, args, nargs);

	nargs = 0;
	SetArg(XtNx, root_x);
	SetArg(XtNy, root_y);
	XtSetValues(alarmshell, args, nargs);
	XtPopup( alarmshell, XtGrabNonexclusive );

	/* double-ring bell 3 times */
	for (i=0; i<3; i++)
		XBell(XtDisplay(toplevel), 80);
}

/*
 * Clean up the alarm pop up windows
 */
static void
AlarmPopDown(w, client_data, call_data)
caddr_t client_data, call_data;
Widget w;
{
	/* pop down shell */
	XtPopdown( (Widget)client_data );

	/* destroy shell */
	XtDestroyWidget((Widget)client_data);
}
