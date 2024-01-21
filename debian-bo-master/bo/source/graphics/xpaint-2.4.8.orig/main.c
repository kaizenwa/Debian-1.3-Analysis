/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)            | */
/* | Copyright 1995, 1996 Torsten Martinsen (bullestock@dk-online.dk)  | */
/* |                                                                   | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.  There is no           | */
/* | representations about the suitability of this software for        | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.                                              | */
/* |                                                                   | */
/* +-------------------------------------------------------------------+ */

/* $Id: main.c,v 1.15 1997/01/26 20:45:07 torsten Exp $ */

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <fcntl.h>
#include <time.h>
#ifndef NOSTDHDRS
#include <stdlib.h>
#include <unistd.h>
#endif

#include <xpm.h>

#define DEFINE_GLOBAL
#include "xpaint.h"
#include "Paint.h"
#include "misc.h"
#include "operation.h"
#include "graphic.h"
#include "protocol.h"
#include "rw/rwTable.h"

#include "XPaintIcon.xpm"

extern void BrushInit(Widget toplevel);

static char *appDefaults[] =
{
#include "XPaint.ad.h"
    NULL
};

typedef struct {
    String visualType;
    String size;
    String help;
    String rcFile;
    Boolean popped;
    Boolean nowarn;
} AppInfo;

static AppInfo appInfo;

static XtResource resources[] =
{
    {"visualType", "VisualType", XtRString, sizeof(String),
   XtOffset(AppInfo *, visualType), XtRImmediate, (XtPointer) "default"},
    {"size", "Size", XtRString, sizeof(String),
     XtOffset(AppInfo *, size), XtRImmediate, (XtPointer) "640x480"},
    {"rcFile", "RcFile", XtRString, sizeof(String),
     XtOffset(AppInfo *, rcFile), XtRImmediate, (XtPointer) NULL},
    {"help", "Help", XtRString, sizeof(String),
     XtOffset(AppInfo *, help), XtRImmediate, (XtPointer) NULL},
    {"popped", "Popped", XtRBoolean, sizeof(Boolean),
     XtOffset(AppInfo *, popped), XtRImmediate, (XtPointer) False},
    {"nowarn", "NoWarn", XtRBoolean, sizeof(Boolean),
     XtOffset(AppInfo *, nowarn), XtRImmediate, (XtPointer) False},
};

static XrmOptionDescRec options[] =
{
    {"-24", ".visualType", XrmoptionNoArg, (XtPointer) "true24"},
    {"-12", ".visualType", XrmoptionNoArg, (XtPointer) "cmap12"},
    {"-size", ".size", XrmoptionSepArg, (XtPointer) NULL},
    {"-rcFile", ".rcFile", XrmoptionSepArg, (XtPointer) NULL},
    {"-popped", ".popped", XrmoptionNoArg, (XtPointer) "True"},
    {"-nowarn", ".nowarn", XrmoptionNoArg, (XtPointer) "True"},
    {"-help", ".help", XrmoptionNoArg, (XtPointer) "command"},
    {"+help", ".help", XrmoptionSepArg, (XtPointer) NULL},
};


/*
**  Public query functions for application defaults
 */

void 
GetDefaultWH(int *w, int *h)
{
    int x, y;
    unsigned int width, height;

    XParseGeometry(appInfo.size, &x, &y, &width, &height);
    *w = width;
    *h = height;
}

char *
GetDefaultRC(void)
{
    return appInfo.rcFile;
}

/*
**  Create a nice icon image for XPaint...
 */
void 
SetIconImage(Widget w)
{
    static Pixmap icon = None;
    static int iconW = 1, iconH = 1;
    Window iconWindow;
    Screen *screen = XtScreen(w);
    Display *dpy = XtDisplay(w);
    XpmAttributes myattributes;

    /*
    **  Build the XPaint icon
     */
    iconWindow = XCreateSimpleWindow(dpy, RootWindowOfScreen(screen),
				     0, 0,	/* x, y */
				     iconW, iconH, 0,
				     BlackPixelOfScreen(screen),
				     BlackPixelOfScreen(screen));
    if (icon == None) {
 	myattributes.valuemask = XpmCloseness;
 	myattributes.closeness = 65535;
 	if (XpmCreatePixmapFromData(dpy, iconWindow,
				    XPaintIcon_xpm, &icon, NULL, &myattributes)) {
	    fprintf(stderr, "Cannot install XPM icon\n");
	    exit(1);
 	}
	GetPixmapWHD(dpy, icon, &iconW, &iconH, NULL);
	XResizeWindow(dpy, iconWindow, iconW, iconH);
    }
    XSetWindowBackgroundPixmap(dpy, iconWindow, icon);

    XtVaSetValues(w, XtNiconWindow, iconWindow, NULL);
}


/*
**  The rest of main
 */
static void 
usage(char *prog, char *msg)
{
    if (msg)
	fprintf(stderr, "%s\n", msg);
    fprintf(stderr, "Usage for %s (XPaint %s):\n", prog, XPAINT_VERSION);
    HelpTextOutput(stderr, appInfo.help == NULL ? "command" : appInfo.help);
    exit(1);
}

typedef struct {
    XtWorkProcId id;
    Widget toplevel;
    int nfiles;
    int pos;
    char **files;
} LocalInfo;

static Boolean
workProc(LocalInfo * l)
{
    char *file;
    void *v;

    if ((l == NULL) || (l->id == None))
	return TRUE;

    file = l->files[l->pos];

    StateSetBusy(True);

    if ((v = ReadMagic(file)) != NULL)
	GraphicOpenFile(l->toplevel, file, v);
    else
	Notice(l->toplevel, "Unable to open input file \"%s\"\n	%s",
	       file, RWGetMsg());

    StateSetBusy(False);

    if (++l->pos == l->nfiles) {
	XtFree((XtPointer) l->files);
	XtFree((XtPointer) l);
	return TRUE;
    } else
	return FALSE;
}


void 
main(int argc, char *argv[])
{
    Display *dpy;
    Widget toplevel;
    XtAppContext app;
    char xpTitle[20];
    int i;
    XEvent event;
    Arg args[5];
    int nargs = 0;
    XrmDatabase rdb;
    Boolean isIcon;

    InitTypeConverters();

    /*
    **  Create the application context
     */
    toplevel = XtAppInitialize(&Global.appContext, "XPaint",
			       options, XtNumber(options), &argc, argv,
			       appDefaults, args, nargs);

    XtGetApplicationResources(toplevel, (XtPointer) & appInfo,
			      resources, XtNumber(resources), NULL, 0);

    rdb = XtDatabase(XtDisplay(toplevel));

    if (appInfo.help != NULL)
	usage(argv[0], NULL);

    if (strcmp(appInfo.visualType, "default") != 0) {
	char *cp = appInfo.visualType;
	if (strncmp(cp, "cmap", 4) == 0) {
	    XrmPutStringResource(&rdb, "Canvas*visual", "PseudoColor");
	    cp += 4;
	} else if (strncmp(cp, "true", 4) == 0) {
	    XrmPutStringResource(&rdb, "Canvas*visual", "TrueColor");
	    cp += 4;
	} else if (strncmp(cp, "gray", 4) == 0) {
	    XrmPutStringResource(&rdb, "Canvas*visual", "StaticGray");
	    cp += 4;
	} else {
	    usage(argv[0], "bad visual type specification");
	}
	if (*cp != '\0')
	    XrmPutStringResource(&rdb, "Canvas*depth", cp);
    }
    if (argc != 1 && argv[1][0] == '-')
	usage(argv[0], "Invalid option");

    /*
    **  A little initialization
     */
    Global.region.image = NULL;
    Global.region.cmap = None;
    Global.region.width = 0;
    Global.region.height = 0;
    Global.region.pix = None;
    Global.region.mask = None;

    /*
    **
     */
    XtVaGetValues(toplevel, XtNiconic, &isIcon, NULL);
    if (isIcon)
	XrmPutStringResource(&rdb, "Canvas.iconic", "on");

    /*
    **  GRR 960525:  check depth and warn user (use AlertBox() instead?)
     */
    if (!appInfo.nowarn) {
	int depth;

	XtVaGetValues(toplevel, XtNdepth, &depth, NULL);
	HelpTextOutput(stderr, "data_loss");
	fprintf(stderr, "\nYour display depth is %d bits.\n\n", depth);
	switch (depth) {
	    case 8:
	    case 12:
		fprintf(stderr, "     =============================================================\n");
		fprintf(stderr, "     WARNING!  Most true-color images will suffer major data loss!\n");
		fprintf(stderr, "     =============================================================\n");
		fprintf(stderr, "\n");
		break;
	    case 1:
	    case 2:
	    case 4:
	    case 6:
	    case 15:
	    case 16:
		fprintf(stderr, "     ================================================================\n");
		fprintf(stderr, "     WARNING!  Most color-mapped (palette) and true-color images will\n");
		fprintf(stderr, "     lose information irreversibly!\n");
		fprintf(stderr, "     ================================================================\n");
		fprintf(stderr, "\n");
		break;
	    case 24:
	    case 32:
		fprintf(stderr, "     Normal images (color-mapped, 8-bit grayscale, 24-bit true-color)\n");
		fprintf(stderr, "     will not lose any color info, but non-true-color images may be\n");
		fprintf(stderr, "     promoted to 24-bit mode.\n\n");
		break;
	    default:
		break;
	}
    }

    /*
    **  Now build and construct the widgets
     */
    Global.timeToDie = False;
    app = XtWidgetToApplicationContext(toplevel);
    dpy = Global.display = XtDisplay(toplevel);

    /*
    **  Call the initializers
     */

    OperationInit(toplevel);

    /*
    **  A few rogue initializers
     */
    BrushInit(toplevel);
    HelpInit(toplevel);

    SRANDOM(time(0));

    SetIconImage(toplevel);

    /*
    **  GRR 960526:  put version number in title string (and icon name?)
     */
    sprintf(xpTitle, "XPaint %s", XPAINT_VERSION);
    XtVaSetValues(toplevel, XtNtitle, xpTitle, NULL);
 /* XtVaSetValues(toplevel, XtNiconName, xpTitle, XtNtitle, xpTitle, NULL); */

    /*
    **  Realize it  (doesn't hurt)
     */
    XtRealizeWidget(toplevel);


    /*
    **  Now open any file on the command line
     */
    if (argc != 1) {
	LocalInfo *l = XtNew(LocalInfo);
	l->pos = 0;
	l->toplevel = toplevel;
	l->nfiles = argc - 1;
	l->files = (char **) XtCalloc(argc, sizeof(char *));
	for (i = 1; i < argc; i++)
	    l->files[i - 1] = argv[i];
	l->id = XtAppAddWorkProc(app, (XtWorkProc) workProc, (XtPointer) l);
    } else if (appInfo.popped) {
	/*
	**  If nothing is coming up, bring up a blank canvas
	 */
	GraphicCreate(toplevel, 0);
    }
    /*
    **  MainLoop
     */
    do {
	XtAppNextEvent(app, &event);
	if (event.type == ButtonPress || event.type == ButtonRelease)
	    Global.currentTime = event.xbutton.time;
	XtDispatchEvent(&event);
    }
    while (!Global.timeToDie);
}
