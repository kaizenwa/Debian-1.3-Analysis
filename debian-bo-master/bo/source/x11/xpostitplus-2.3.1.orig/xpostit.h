/*
 * $Header: /home/src/X/xpostit/xpostit/xpostit.h,v 2.0 1995/03/27 18:57:18 mjhammel Exp $
 *
 * xpostit.h - declarations for xpostit.
 *
 * This file is easiest to read with tabstops set to 8
 *
 * David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * $Log: xpostit.h,v $
 * Revision 2.0  1995/03/27  18:57:18  mjhammel
 * Initial update to 2.0
 *
 *
 * Revision x.x	 93/13/04	mjhammel
 * added pn_saved to PostItNote structure so save button can be
 * set to sensitive or insensitive depending on if any characters
 * have been written to the note.
 *
 * Revision 1.2  90/06/14  11:21:33  davy
 * Ported to X11 Release 4.
 * 
 * Revision 1.1  90/06/13  09:48:52  davy
 * Initial revision
 * 
 */

/*
 * Default values.
 */
#define DefaultBufSize		1024
#define DefaultNameWidth	10
#define DefaultAnchorOffset	15
#define DefaultNoteDir		".postitnotes"
#define DefaultTmpDir		"/tmp"
#define DefaultPrintCmd		"lpr %s"
#define DefaultEmailCmd		"mail -s\"%s\" %s"
#define DefaultCalendarCmd	"cal"
#define DefaultInterval		10
#define DefaultPlaidWidth	64
#define DefaultPlaidHeight	64
#define DefaultListWidth	64
#define DefaultListHeight	64
#define DefaultMaxNotes		255

/*
 * Post-It Note sizes.
 */
#define PostItNote_1p5x2	0
#define PostItNote_2x3		1
#define PostItNote_3x3		2
#define PostItNote_3x4		3
#define PostItNote_3x5		4
#define PostItNote_4x6		5

/*
 * Post-It Note file information.
 */
#define PostItNoteMagic		"%!<postitnote>"
#define PostItNoteMagic_v2	"%%!!<postitnote>"
#define PostItNoteFname		"note"

/*
 * Application class.
 */
#define PostItNoteClass		"XPostitPlus"

/*
 * Name for label in list of hidden notes
 */
#define PostItNoteList		"Hidden Notes"

/*
 * Name for Error Dialog pop up
 */
#define PostItErrDialog		"Xpostit Error"

/*
 * Name for Alarm Dialog pop up
 */
#define PostItAlarmDialog	"Xpostit Alarm"

/*
 * Name for Timer Prompt pop up
 */
#define PostItTimerPrompt	"Alarms Setup"

/*
 * Name for IO Prompt pop up
 */
#define PostItIOPrompt	"File Selection Window"

/*
 * Name for Insert Prompt pop up
 */
#define PostItInsertPrompt	"Insert Date"

/*
 * Name for Find Note Prompt pop up
 */
#define PostItFindPrompt	"Find A Note"

/*
 * Name for Email Note Prompt pop up
 */
#define PostItEmailPrompt	"Email Note"

/*
 * Name of Options Menu widget
 */
#define PostItOptionsMenu	"OptionsMenu"

/*
 * Name of File Menu widget
 */
#define PostItFileMenu		"FileMenu"

/*
 * name shown in the dialog pop up
 */
#define PostItNoteDialog	"Title for Note"

/*
 * Name for Save On Exit pop up
 */
#define PostItSaveOnExitPrompt	"Save Notes Prompt"

/*
 * Help Message
 */
#define USAGE \
"xpostit accepts all standard X Toolkit command line options, plus\n\
the following:\n\
\n\
xpostit [ -c | -sb | -sv | -ns | -help/-? | -bs bufsize | -dir notedir \n\
        | -interval seconds | -nw pixels | -ao pixels ]\n\
where\n\
    -c                  turns on compatibility mode\n\
    -sb                 turns on scrollbars for all notes\n\
    -sv                 enables save-on-exit\n\
    -ns                 disables the auto save feature\n\
    -na                 disables the alarm feature\n\
    -help | -?          prints this help message\n\
    -bs bufsize         sets the buffer size for all notes\n\
    -dir notedir        specifies the directory to load and save notes\n\
    -interval minutes   specifies the auto-save timeout value\n\
    -nw pixels          width, in pixels, of a single character\n\
    -ao offset          offset, in pixels, for X,Y coords of cascaded notes\n\
    -tmpdir tmpdir      directory to use for temporary files\n\
    -printcmd cmd       print command, in \"printf\" format\n\
    -calendarcmd cmd    calendar command\n\
    -emailcmd cmd       command to use to send email\n\
    -homedir path       default home directory (used by file selection window\n\
    -version            print the current version number\n\
\n\
"

/*
 * Request for a new note index number.
 */
#define NewIndex		-1

/*
 * Types for the InsertText() and FillList() functions
 */
#define TEXT_TYPE		0
#define FILE_TYPE		1
#define DIR_TYPE		2

/*
 * Types for the CreateIOPrompt() function
 */
#define XPOSTIT_OPEN_FILE	0
#define XPOSTIT_EXPORT_FILE	1

/*
 * Just in case.
 */
#ifndef MAXPATHLEN
#define MAXPATHLEN	1024
#endif

/*
 * Maximum number of directory files we can handle
 */
#define XP_MAX_FILES		1024

/*
 * Useful macros.
 */
#define SetArg(which, val)	XtSetArg(args[nargs], (which),\
					 (XtArgVal) (val)); nargs++
#define SetCallback(which, val)	callbacks[0].callback = (which); \
				callbacks[0].closure = (caddr_t) (val)

/*
 * The Post-It Note record.  One of these is allocated
 * for each note created.
 */
typedef struct _PostItNote {
	Widget	pn_shellwidget;		/* shell widget holding it all	*/
	Widget	pn_labelwidget;		/* label widget */
	Widget	pn_savewidget;		/* save widget */
	Widget	pn_alarmwidget;		/* alarm icon in menu bar */
	Widget	pn_options;		/* options button widget */
	Widget	pn_optionsmenu;		/* options menu widget */

	Widget	pn_filebutton; 		/* file button widget */
	Widget	pn_filemenu; 		/* file menu widget */

	Widget	pn_textwidget;		/* text widget of the note	*/
	Widget	pn_msgwidget;		/* save message widget of the note	*/
	Widget	pn_anchorwidget;	/* menu widget for "Anchor" option */
	Widget	pn_unanchorwidget;	/* menu widget for "UnAnchor" option */
	Widget	pn_settimewidget;	/* menu widget for "Set Alarm" option */
	Widget	pn_unsettimewidget;	/* menu widget for "Unset Alarm" option */

	char	*pn_file;		/* file note will be saved in	*/
	char	*pn_text;		/* buffer holding text of note	*/
	char	*pn_name;		/* name of note	*/

	int	pn_index;		/* index number of note		*/
	int	pn_hide_index;		/* index used when note is hidden */
	int	pn_shellx;		/* x coord of shell widget	*/
	int	pn_shelly;		/* y coord of shell widget	*/
	int	pn_textsize;		/* size of pn_text in chars	*/
	int	pn_textwidth;		/* width of text widget window	*/
	int	pn_textheight;		/* height of text widget window	*/

	int	pn_alarm_mon;		/* alarm month (0-11) */
	int	pn_alarm_day;		/* alarm day (0-31) */
	int	pn_alarm_hour;		/* alarm hour (0-23) */
	int	pn_alarm_min;		/* alarm minute (0-59) */

	Boolean	pn_positionit;		/* true if shellx/shelly valid	*/
	Boolean	pn_hidden;		/* true if note is currently hidden */
	Boolean	pn_saved;		/* false=save button sensitive	*/
	Boolean	pn_anchor;		/* true if note is an anchor note */
	Boolean	pn_alarm;		/* true if notes alarm is set */

	struct	_PostItNote *pn_next;	/* pointer to next note record	*/
} PostItNote;

/*
 * The resource record, for holding resources specific to xpostit.
 */
typedef struct {
	int	buf_size;		/* size of pn_text to be used	*/
	String	note_dir;		/* path to note directory	*/
	int	interval;		/* auto-save interval (in minutes) */
	Boolean	scroll_ovf;		/* NOT USED ANYMORE */
	Boolean	scroll_bar;		/* turn on scroll bars		*/
	Boolean save_notes;		/* save notes on exit		*/
	Boolean compatibility;		/* maintain prev version compatibility */
	Boolean nosave;			/* don't do auto-save */
	Boolean noalarm;		/* don't do alarms */
	Boolean help;			/* provide usage message */
	Boolean version;		/* provide version information */
	int	name_width;		/* used	in calculating width of dialog */
	int	anchor_offset;		/* used in cascading notes */
	String	tmp_dir;		/* path to tmp directory	*/
	String	print_cmd;		/* print command (using printf format) */
	String	calendar_cmd;		/* calendar command */
	String	email_cmd;		/* email command */
	String	home_dir;		/* default home directory */
} AppRes, *AppResPtr;

/*
 * The list of hidden notes
 */
typedef struct {
	char	*name;	/* string used in menu button */
	Widget	widget;	/* button in menu for hidden note */
} ItemList;

/*
 * link lists of notes
 */
typedef struct pnlinks{
	PostItNote	*pn;
	int		x; /* x coord of last note */
	int		y; /* y coord of last note */
	struct pnlinks	*next;
}PNLinks;


/*
 * External variable declarations.
 */
extern	AppRes app_res;

extern	Screen *screen;
extern	int curr_screenx, curr_screeny;
extern	Display *display;

extern	Widget toplevel;
extern	Widget listwidget;
extern	Widget menuwidget;
extern	Widget plaidwidget;
extern	unsigned long timer_interval;
extern	unsigned long alarm_interval;

/*
 * If you're SYSV system doesn't have bcopy(), bzero(), or rindex()
 */
#ifdef SYSV_USE_MEMSET
#undef bzero
#define bzero(a,b) memset(a,0,b)
#undef bcopy
#define bcopy(a,b,c) memcpy(b,a,c)
#undef rindex
#define rindex(a,b) strrchr(a,b)
#else
 extern char *rindex();
#endif /* SYSV_USE_MEMSET */

/*
 * Function declarations.
 */
char	*SafeAlloc();

void	ByeBye();
void	AutoSave();
void	AlarmCheck();
void	ConfirmIt();
void	ClearConfirm();
void	NameIt();
void	ClearName();
void	SaveAllNotes();
void	RaiseAllNotes();
void	HideAllNotes();
void	UnHideAllNotes();
void	LowerAllNotes();
void	LoadSavedNotes();
void	SetNoteDir();
void	CreateNewNote();
void	CreateMenuWidget();
void	CreatePlaidWidget();
void	CreateListWidget();
void	CreateFindNotePrompt();
void	SetSaveSensitive();
void	PopUpList();
void	CascadeNotes();
void	AddListItem();
void	CreateTimerPrompt();
void	CreateErrorDialog();
void	CreateEmailNotePrompt();
void	MakeNoteListCB();
void	MakeNewNote();
void	ToggleShow();
void	ToggleRaise();
void	WMProtocols();
