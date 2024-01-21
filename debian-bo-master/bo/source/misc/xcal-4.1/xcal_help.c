#ifndef lint
static char    *sccsid = "@(#)xcal_help.c	3.19 (Hillside Systems) 11/6/93";
static char    *copyright = "@(#)Copyright 1989,1990,1993 Peter Collinson, Hillside Systems";
#endif				/* lint */
/***

* module name:
	xcal_help.c
* function:
	Generate help screens in separate popup windows
* history:
	Written December 1989
	Completelt redone October 1993
	Peter Collinson
	Hillside Systems
* (C) Copyright: 1989,1990,1993 Hillside Systems/Peter Collinson
	
	For full permissions and copyright notice - see xcal.c
***/
#include <sys/types.h>
#include "xcal_mmap.h"
#ifndef NO_MMAP
#include <sys/mman.h>
#endif
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Paned.h>
#include "xcal.h"

#define argLD(N,V) { XtSetArg(args[nargs], N, V); nargs++; }

static XtCallbackRec callbacks[] = {
	{NULL, NULL},
	{NULL, NULL}
};

static	Boolean	helpRead = False;

typedef struct helpdata {
	String	name;		/* Help name */
	String	text;		/* the text we are showing */
	Widget	popup;		/* Widget value */
} Help;

static Help help[] = {
	{ "nohelp", "Sadly, Xcal help has not been installed correctly\nSee your sysadmin\n",},
	{ "main", },		/* don't move from here - see PopdownHelp */
	{ "edit", },
	{ "memo", },
	{ "weekly", },	
	{ "strip", },
	{ NULL, }
};

static Help    *findHelp();
static void	initHelp();
static char    *connectHelp();
static Widget   DisplayHelpWindow();
static void	PopdownHelp();
static void	makeHelpPopup();
#ifdef NO_MMAP
static char	*readbfile();
#endif

/*
 *	Initialise the help system
 *	Two methods - from a help file
 *	compiled in
 */
char	*helpText;
/* defines helpdata[] */
#include "xcal_help.h"

/*
 * Search the help structure looking for a name
 */
static Help *
findHelp(name)
	String name;
{
	Help *h;

	for (h = help; h->name; h++)
		if (strcmp(name, h->name) == 0)
			return h;
	return NULL;
}

/*
 * Initialise help data
 */
static void
initHelp()
{
	Help	*he;
	char	*h;
	char	*name;
	char	*text;
	char	*vers;
	int	lastc;
	register int    vlen;
	extern char     version[];

	/* 
	 * Find the text for the help data
	 * If take from a file - then connect the file in memory
	 * If that fails, then use the text compiled into the program
	 */
	if (appResources.helpFromFile == True) {
		helpText = connectHelp();
		if (helpText == NULL)
			helpText = helpdata;
	} else
		helpText = helpdata;
	/*
	 * Allow you to compile NO text into the program
	 */
	if (*helpText == '\0')
		helpText = NULL;
	/*
	 * helpRead says - I have TRIED to find the data
	 */
	helpRead = True;
	if (helpText == NULL)
		return;

	vlen = strlen(version);

	for (h = helpText; *h; ) {
		if ((h = strchr(h, '{')) == NULL)
			break;
		h++;			/* h points to name */
		name = h;

		/* look for the end of the name */
		while (!isspace(*h))	/* name is terminated by white space */
			h++;		/* or a newline */
		lastc = *h;
		*h++ = '\0';
		if (lastc != '\n') {
			while(isspace(*h))
				h++;
		}
		text = h;		/* that's the start of the text */

		/* find the place to insert the version */
		do {
			if ((h = strchr(h, '+')) == NULL)
				Fatal("Missing +++ lines in help text - %s\n", name);
		} while (h[1] != '+' && h[2] != '+');
		vers = h;
		
		/* and the end of the text */
		h = strchr(h, '}');
		if (h == NULL)
			Fatal("Missing } in help text - %s\n", name);
		*h++ = '\0';

		/* Insert version */
		memcpy(vers, version, vlen);
		vers[vlen] = '\n';
		vers[vlen + 1] = '\0';

		/*
		 * That was a bit scrappy..
		 * insert the details in the structure
		 */
		if (he = findHelp(name))
			he->text = text;
	}
}	

/*
 * If we are getting help from a file then the name is in the
 * resources. The trick here is to map the file into memory privately
 * so that it is just like one compiled in vector
 */
static char *
connectHelp()
{
        int     fd;
        struct  stat statb;
 	char	*fibase;

        if ((fd = open(appResources.helpfile, 0)) < 0)
                return NULL;
 
        if (fstat(fd, &statb) < 0) {
                printf("Cannot fstat %s (shouldn't happen)\n", appResources.helpfile);
		close(fd);
		return NULL;
 	}

        if (statb.st_size == 0) {
                printf("Zero length file? (%s)\n", appResources.helpfile);
		close(fd);
 		return NULL;
	}
#ifdef NO_MMAP
	fibase = readbfile(fd, statb.st_size);
#else
        fibase = (char *) mmap(0, statb.st_size, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);
#endif
        if ((int)fibase == -1) {
                printf("Cannot map %s into memory\n", appResources.helpfile);
 		return NULL;
	}
        close(fd);      /* we have it now */
        fibase[statb.st_size] = '\0';	/* we don't mind losing the last char */
        return(fibase);
}
		
static Widget
DisplayHelpWindow(str)
	String          str;
{
	Widget          shell, form, title;
	Arg             args[10];
	Cardinal        nargs;

	shell = XtCreatePopupShell("help", topLevelShellWidgetClass, toplevel, NULL, 0);

	form = XtCreateManagedWidget("helpPanel", panedWidgetClass, shell, NULL, 0);

	nargs = 0;
	argLD(XtNshowGrip, False);
	argLD(XtNdefaultDistance, 2);
	title = XtCreateManagedWidget("helpForm", formWidgetClass, form, args, nargs);
	/*
	 * Exit button Take "Quit" from resources
	 */
	callbacks[0].callback = PopdownHelp;
	callbacks[0].closure = (caddr_t) shell;
	nargs = 0;
	argLD(XtNcallback, callbacks);
	argLD(XtNfromHoriz, NULL);
	argLD(XtNleft, XtChainLeft);
	argLD(XtNright, XtChainLeft);
	(void) XtCreateManagedWidget("helpQuit", commandWidgetClass, title, args, nargs);
	/*
	 * Now the text which is the remainder of the panel
	 */
	nargs = 0;
	argLD(XtNshowGrip, False);
	argLD(XtNstring, str);
	argLD(XtNdisplayCaret, False);
	(void) XtCreateManagedWidget("helpText", asciiTextWidgetClass, form, args, nargs);

	XtPopup(shell, XtGrabNone);
	return shell;
}

/* ARGSUSED */
static void
PopdownHelp(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	extern Widget   mHelp;

	if (help[1].popup == (Widget)closure)
	 	HelpShow(mHelp, False);
	XtPopdown((Widget) closure);
	HelpButtonOn((Widget) closure);
}

/*
 * Create a Help Popup
 */
static void
makeHelpPopup(w, name)
	Widget	 w;
	char	*name;
{
	Help	*he;

	if (helpRead == False)
		initHelp();

	he = findHelp(name);
	if (he == NULL || he->text == NULL)
		he->text = help[0].text;
	
	if (he->popup)
		XtPopup(he->popup, XtGrabNone);
	else	he->popup = DisplayHelpWindow(he->text);

	ButtonOff(w, he->popup);
}

/* ARGSUSED */
void
StripHelp(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	makeHelpPopup(w, "strip");	
}

/* ARGSUSED */
void
EditHelp(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	makeHelpPopup(w, "edit");	
}

/* ARGSUSED */
void
MemoHelp(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	makeHelpPopup(w, "memo");	
}

/* ARGSUSED */
void
MainHelp(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	extern	Widget mHelp;

	HelpShow(mHelp, True);
	makeHelpPopup(w, "main");	
}

/* ARGSUSED */
void
WeeklyHelp(w, closure, call_data)
	Widget          w;
	caddr_t         closure;
	caddr_t         call_data;
{
	makeHelpPopup(w, "weekly");	
}

#ifdef NO_MMAP
/*
 * If the system as no mmap malloc some memory and
 * read the file into it
 */
static char *
readbfile(fd, len)
	int	fd;
	int	len;
{
	char	*base;
	
	base = XtMalloc(len+1);
	if (read(fd, base, len) != len)
		return ((char *)-1);
	return base;
}
#endif
