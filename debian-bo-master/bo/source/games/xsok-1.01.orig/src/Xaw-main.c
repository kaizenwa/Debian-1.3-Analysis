/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module Xaw-main.c				     */
/*									     */
/*	main function for the Athena Widget interface.			     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#include "X-sok.h"
#include "Tableau.h"
#include "version.h"

Widget toplevel;

static XtAppContext app_con;
static void (*execfunc)(void) = NULL;
static Widget messagebox, container, desktop;
static Widget dialog, popup, paned;
static Window mainwindow;


void show_message(const char *str, ...) {
    if (gamegraphic) {
	static char last_message[256];
	Arg Args;
	va_list args;
	va_start(args, str);
	
	if (!str) {
	    memset(last_message, ' ', sizeof(last_message)-1);
	    last_message[sizeof(last_message)-1] = '\0';
	} else
	    vsprintf(last_message, str, args);
	
	XtSetArg(Args, XtNlabel, last_message);
	XtSetValues(messagebox, &Args, 1);
    }
}

void SetTitle(void) {
    if (XtWindow(toplevel)) {
	static char windowname[48];
	sprintf(windowname, "%s - Level %d", game.type, game.level);
	/* printf("SetTitle(): toplevel = %p, window = %x\n",
	   toplev, XtWindow(toplevel)); */
	XStoreName(dpy, XtWindow(toplevel), windowname);
    }
}

void cmd_LeaveSok(void) {
    play_sound("goodbye");
    XtDestroyApplicationContext(app_con);
    exit(0);
}

static void perform_command(Widget widget, XtPointer client_data, XtPointer call_data) {
    (*(void (*)(void))client_data)();	/* any questions? */
}

static void popup_confirm(const char *prompt) {
    Arg args[2];
    Position x, y;
    Dimension xx, yy, wx, wy;

    XtSetArg(args[0], XtNlabel, prompt);
    XtSetValues(dialog, args, 1);
    XtVaGetValues(dialog,   XtNwidth, &xx, XtNheight, &yy, NULL);
    XtVaGetValues(toplevel, XtNwidth, &wx, XtNheight, &wy, NULL);
    x = (wx - xx)/2;
    y = (wy - yy)/2;

    XtTranslateCoords(toplevel, x, y, &x, &y);
    XtSetArg(args[0], XtNx, x);
    XtSetArg(args[1], XtNy, y);
    XtSetValues(popup, args, 2);
    XtPopup(popup, XtGrabNone);
}

void cmd_Confirm(void) {
    if (execfunc) {
	void (*execfunc2)(void) = execfunc;	/* erase it first! */
	execfunc = NULL;
	XtPopdown(popup);
	(*execfunc2)();	/* finally execute the desired function */
    }
}
void cmd_Cancel(void) {
    if (execfunc) {
	execfunc = NULL;
	XtPopdown(popup);
    }
}

/* type converter functions: */
static void mXtAP_Cancel (Widget w, XEvent *xev, String *params, Cardinal *num) { cmd_Cancel();  }
static void mXtAP_Confirm(Widget w, XEvent *xev, String *params, Cardinal *num) { cmd_Confirm(); }
static void Cancel(Widget widget, XtPointer client_data, XtPointer call_data)	{ cmd_Cancel();  }
static void Ok(Widget widget, XtPointer client_data, XtPointer call_data)	{ cmd_Confirm(); }

void request_confirm(void (*dofunc)(void), const char *prompt) {
    if (execfunc)
	return;		/* request pending => deny another one */
    if (game.finished || !game.n_moves) {
	(*dofunc)();	/* perform action without confirmation */
	return;
    }
    execfunc = dofunc;
    popup_confirm(prompt);
}

static String fallback_resources[] = { 
    "*beNiceToColormap:			false",
    "*shapeStyle:			Rectangle",
    "*topShadowContrast:		20",
    "*bottomShadowContrast:		40",
    "*Scrollbar*background:		Grey70",
    "*Background:			grey85",
    "*Foreground:			black",
    "*resizeToPreferred:		True",
    "*input:				True",
    "*showGrip:				off",
    "*shadowWidth:                   	2",
    "*messages.justify:			Left",
    "*upperbox.orientation:		XtorientHorizontal",
    "*lowerbox.orientation:		XtorientHorizontal",
    "*Tableau.backingStore:		WhenMapped",
    "*Tableau.keyboardFile:		keys",
    "*Tableau.messageFile:		messages",
    "*Tableau.background:		black",
    "*Tableau.rules:		        Sokoban",
    "*Viewport.allowHoriz:		False",		/* scrollbar disable! */
    "*Viewport.allowVert:		False",		/* code is buggy! */
    "*Viewport.forceBars:		False",
    "*Viewport.useBottom:		True",
    "*Viewport.useRight:		True",
    "*Label.shadowWidth:		0",
    "*Label.BorderWidth:		2",
    "*Dialog*Translations: #override \n<Key>y: Ok()\n<Key>n: Cancel()\n",
    "XSok*title:			XSok",
    "XSok.prompt.allowShellResize:	True",
    "XSok.prompt.saveUnder:		True",
    "*Dialog*resizable:			True",
    "*Hint.Translations: #override\n<BtnDown>:set()\n<BtnUp>:HintNotify()unset()\n",
    "*Sound.state:			True",
    "XSok.help.width:			403",
    "XSok.help.height:			200",
    "XSok.help.title:			XSok Help Window",
    "XSok.help.saveUnder:		True",
    "XSok*Close Help.fromHoriz:		Topic",
    "*helptext*string:			Please choose a topic.",
    "*helptext*displayCaret:		False",
    "*helptext*scrollHorizontal:	whenNeeded",
    "*helptext*scrollVertical:		whenNeeded",
    "*helptext*editType:		read",
    NULL,
};

static XrmOptionDescRec options[] = {
    /* tableau resources */
    { "-rules",		"*Tableau.rules",	XrmoptionSepArg, NULL },
    { "-level",		"*Tableau.level",	XrmoptionSepArg, NULL },
    { "-username",	"*Tableau.username",	XrmoptionSepArg, NULL },
    { "-xsokdir",	"*Tableau.xsokdir",	XrmoptionSepArg, NULL },
    { "-xpmdir",	"*Tableau.xpmdir",	XrmoptionSepArg, NULL },
    { "-savedir",	"*Tableau.savedir",	XrmoptionSepArg, NULL },
    { "-messageFile",	"*Tableau.messageFile",	XrmoptionSepArg, NULL },
    { "-keyboardFile",	"*Tableau.keyboardFile",XrmoptionSepArg, NULL },

    /* non-tableau resources */
#ifdef SOUND
    { "-sound", 	"*Sound.state",      	XrmoptionNoArg, (XtPointer)"True" },
    { "-nosound", 	"*Sound.state",      	XrmoptionNoArg, (XtPointer)"False" },
#endif
};

static XtActionsRec moreActions[] = {
    { "Cancel",		mXtAP_Cancel },
    { "Ok",		mXtAP_Confirm }
};

static void process_extra_args(int argc, char *argv[]) {
    /* check extra args */
    if (argc >= 2) {
	fprintf(stderr, "xsok: invalid argument: %s\n", argv[1]);
	fprintf(stderr, "usage: xsok [options]\n"
		"options are all standard X11 toolkit options and\n"
                "-rules (ruleset)     to initially use specified rules\n"
                "-level (levelnr)     to set the startlevel\n"
                "-username (username) to set individual username for highscores\n"
		"-xsokdir (dir)       to set the game directory\n"
		"-xpmdir (dir)        to set the directory for xpm files\n"
		"-savedir (dir)       to set the directory for saved games\n"
		"-messageFile (file)  to set the message file name\n"
		"-keyboardFile (file) define the keyboard assignment\n"
#ifdef SOUND
		"-sound               sound toggle on\n"
		"-nosound             sound toggle off\n"
#endif
		);
	exit(EXIT_FAILURE);
    }
}

const char *rulepool[16] = { "Xsok", "Sokoban", "Cyberbox", NULL };

static void selectrules(Widget w, XtPointer number, XtPointer garbage) {
    const char *s = XtName(w);
    /* printf("widget %s has been selected\n", s); */
    change_rules(s);
    NewLevel(1);
    cmd_LevelInfo();
}

#ifdef SOUND
static Widget sound;

int checksound(void) {
    Boolean retval;
    Arg args[1];
    XtSetArg(args[0], XtNstate, &retval);
    XtGetValues(sound, args, 1);
    return retval & 0xff;
}
#endif


static void read_gametypes(void) {
    char filename[256], s[80];
    FILE *fp;
    sprintf(filename, "%s/gametypes", xsokdir);
    if ((fp = fopen(filename, "r"))) {
	int i;
	for (i = 3; i < 15; ++i) {
	    if (!fgets(s, sizeof(s), fp))
		break;
	    if (*s == ';') {
		--i;
		continue;
	    }
	    if (strchr(s, '\n'))
		*strchr(s, '\n') = '\0';
	    if (strlen(s) > 8)
		s[8] = '\0';
	    rulepool[i] = strsav(s);
	}
	rulepool[i] = NULL;
	fclose(fp);
    } /* else keep default pool */
}

int main(int argc, char *argv[]) {
    Widget buttonpanel;
    Widget gamebutton, gamemenu, rulesbutton, rulesmenu;
    int i;
    struct button {
	const char *name; void (*func)(void);
    } *bp;
    static struct button buttons[] = {
	{ "Undo",	  	cmd_UndoMove },
	{ "Redo",	  	cmd_RedoMove },
	{ "Next Level",	  	cmd_NextLevel },
	{ "Score",	  	cmd_ShowScore },
#ifdef ONLINE_HELP
	{ "Help",	  	popup_help },
#endif
	{ "Save",	  	cmd_SaveGame },
	{ "Load",		cmd_LoadGame }
    }, mbuttons[] = {
	{ "Drop Bookmark",	cmd_DropBookmark },
	{ "Goto Bookmark",	cmd_GotoBookmark },
	{ "Replay",		cmd_ReplayGame },
	{ "Restart",		cmd_RestartGame },
 	{ "Next Level",		cmd_NextLevel },
 	{ "Previous Level",	cmd_PrevLevel },
	{ "Quit",		rq_LeaveSok }
    };

    /* use the command line arguments concerning the widgets */
    switch_uid(1);
    toplevel = XtAppInitialize(&app_con, "XSok", options, XtNumber(options),
			       &argc, argv, fallback_resources, NULL, 0);
    switch_uid(0);
    process_extra_args(argc, argv);
    XtAppAddActions(app_con, moreActions, XtNumber(moreActions));

    /* basic elements */
    paned       = XtCreateManagedWidget("paned",	panedWidgetClass,    toplevel,	  NULL, 0);
    buttonpanel = XtCreateManagedWidget("buttonpanel",	boxWidgetClass,     paned,	  NULL, 0);
    messagebox = XtCreateManagedWidget("messages", labelWidgetClass, paned, NULL, 0);
    show_message(TXT_WELCOME, VERSION);
    graphics_control(Disable);

    container   = XtCreateManagedWidget("container",	viewportWidgetClass, paned,	  NULL, 0);
    desktop     = XtCreateManagedWidget("desktop",	tableauWidgetClass,  container,	  NULL, 0);
    /* XtAddCallback(container, XtNreportCallback, reportfunc, NULL); */

    /* create the button panel and its menus */
    gamebutton  = XtCreateManagedWidget("Game", menuButtonWidgetClass, buttonpanel, NULL, 0);
    gamemenu    = XtCreatePopupShell("gamemenu", simpleMenuWidgetClass, gamebutton, NULL, 0);
    for (bp = mbuttons, i = 0; i < XtNumber(mbuttons); ++i) {
	Widget w;
	w = XtCreateManagedWidget(bp->name, smeBSBObjectClass, gamemenu, NULL, 0);
	if (bp->func) XtAddCallback(w, XtNcallback, perform_command, bp->func);
	++bp;
    }
    XtVaSetValues(gamebutton, XtNmenuName, "gamemenu", NULL);

    /* create Rules button just right of the Game button */
    rulesbutton = XtCreateManagedWidget("Level Subset", menuButtonWidgetClass, buttonpanel, NULL, 0);
    rulesmenu   = XtCreatePopupShell("rulesmenu", simpleMenuWidgetClass, rulesbutton, NULL, 0);
    XtVaSetValues(rulesbutton, XtNmenuName, "rulesmenu", NULL);

    /* rest of the buttons */
    for (bp = buttons, i = 0; i < XtNumber(buttons); ++i) {
	Widget w;
	w = XtCreateManagedWidget(bp->name, commandWidgetClass, buttonpanel, NULL, 0);
	if (bp->func) XtAddCallback(w, XtNcallback, perform_command, bp->func);
	++bp;
    }
#ifdef SOUND
    sound = XtCreateManagedWidget("Sound", toggleWidgetClass, buttonpanel, NULL, 0);
#endif

    /* OK. Now do the pop-up shells */
    popup = XtCreatePopupShell("prompt", transientShellWidgetClass, toplevel, NULL, 0);
    dialog = XtCreateManagedWidget("dialog", dialogWidgetClass, popup, NULL, 0);
    XawDialogAddButton(dialog, "ok",     Ok,     (XtPointer)dialog);
    XawDialogAddButton(dialog, "cancel", Cancel, (XtPointer)dialog);

#ifdef ONLINE_HELP
    create_help();
#endif
    graphic.width = graphic.height = 0;
    graphic.autolayout = 1;
    XtRealizeWidget(toplevel);
    XSync(dpy, 0);
    mainwindow = XtWindow(toplevel);
    XSetIconName(dpy, mainwindow, "xsok");
    SetTitle();
    table  = XtWindow(desktop);

    read_gametypes();
    {   const char **rp;
	for (rp = rulepool; *rp; ++rp) {
	    Widget w;
	    w = XtCreateManagedWidget(*rp, smeBSBObjectClass, rulesmenu, NULL, 0);
	    XtAddCallback(w, XtNcallback, selectrules, NULL);
	}
    }

    graphics_control(Enable);
    XtRealizeWidget(popup);
    XtAppMainLoop(app_con);	/* does not return */
    return 0;			/* keep compiler happy */
}

void Force_Resize(XSize_t w, XSize_t h) {
    Arg args[1];
    int hh;
    Dimension hhh = 0;
    XtSetArg(args[0], XtNheight, &hhh);
    XtGetValues(paned, args, 1);
    hh = hhh;		/* unsigned short => int */
    XtGetValues(container, args, 1);
    /* printf("rq %d, paned = %d, container = %d\n", h, hh, hhh); */
    h += hh - hhh;	/* difference between overall size and Viewport size */

    XResizeWindow(dpy, mainwindow, w, h);
}
