
/*-
# MOTIF-BASED DINOSAUR CUBE
#
#  xmdino.c
#
###
#
#  Copyright (c) 1995 - 97	David Albert Bagley, bagleyd@bigfoot.com
#
#                   All Rights Reserved
#
#  Permission to use, copy, modify, and distribute this software and
#  its documentation for any purpose and without fee is hereby granted,
#  provided that the above copyright notice appear in all copies and
#  that both that copyright notice and this permission notice appear in
#  supporting documentation, and that the name of the author not be
#  used in advertising or publicity pertaining to distribution of the
#  software without specific, written prior permission.
#
#  This program is distributed in the hope that it will be "playable",
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
*/

/*-
  Version 5: 95/10/06 Xt/Motif
  Version 4: 94/05/30 Xt
*/

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#ifdef VMS
#include <unixlib.h>
#define getlogin() cuserid(NULL)
#else
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif
#if HAVE_FCNTL_H
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include "Dino.h"
#include "Dino2d.h"
#include "Dino3d.h"
#include "dino.xbm"
#include "mouse-l.xbm"
#include "mouse-r.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/dino.scores"
#endif

/* The following are in DinoP.h also */
#define PERIOD2 2
#define PERIOD3 3
#define BOTH 4
#define MAXMODES 3
#define MAXFACES 6

#define MAXRECORD 32767
#define FILENAMELEN 1024
#define USERNAMELEN 128
#define NOACCESS "noaccess"
#define NOBODY "nobody"

typedef struct {
	int         score;
	char        name[USERNAMELEN];
} GameRecord;

static void Initialize(void);
static void CallbackDino(Widget w, caddr_t clientData, dinoCallbackStruct * callData);

static void PrintRecord(int mode, Boolean orient, Boolean practice);
static int  HandleSolved(int counter, int mode, Boolean orient);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static void ModeToggle(Widget w, int mode, XmToggleButtonCallbackStruct * cbs);
static void OrientToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void PracticeToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void motif_print(Widget w, char *text);

static Arg  arg[5];
static Widget moves, record, message, dino2d, dino3d, modes[MAXMODES], orientSwitch,
            practiceSwitch;
static GameRecord dinoRecord[MAXMODES][2];
static int  movesDsp = 0;
static char messageDsp[128] = "Welcome";
static char usernameDsp[USERNAMELEN] = "";
static char buff[256];

static char *modeString[] =
{
	"Period 2", "Period 3", "Both"
};

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmdino\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		  "\t[-{border|bd} {color}] [-face{0|1|2|3|4|5} {color}]\n");
	(void) fprintf(stderr,
		 "\t[-{mode {int} | both}] [-[no]orient] [-[no]practice]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*dino.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*dino.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*dino.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*dino.pieceBorder", XrmoptionSepArg, NULL},
	{"-border", "*dino.pieceBorder", XrmoptionSepArg, NULL},
	{"-face0", "*dino.faceColor0", XrmoptionSepArg, NULL},
	{"-face1", "*dino.faceColor1", XrmoptionSepArg, NULL},
	{"-face2", "*dino.faceColor2", XrmoptionSepArg, NULL},
	{"-face3", "*dino.faceColor3", XrmoptionSepArg, NULL},
	{"-face4", "*dino.faceColor4", XrmoptionSepArg, NULL},
	{"-face5", "*dino.faceColor5", XrmoptionSepArg, NULL},
	{"-mode", "*dino.mode", XrmoptionSepArg, NULL},
	{"-both", "*dino.mode", XrmoptionNoArg, "4"},
	{"-orient", "*dino.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*dino.orient", XrmoptionNoArg, "FALSE"},
	{"-practice", "*dino.practice", XrmoptionNoArg, "TRUE"},
	{"-nopractice", "*dino.practice", XrmoptionNoArg, "FALSE"},
	{"-username", "*dino.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel;
	Widget      panel, panel2, rowcol, rowcol2, rowcol3, rowcol4;
	Pixmap      mouseLeftCursor, mouseRightCursor;
	Pixel       fg, bg;
	int         i;

	toplevel = XtInitialize(argv[0], "Dino",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			       (char *) dino_bits, dino_width, dino_height));
	XtSetArg(arg[1], XmNkeyboardFocusPolicy, XmPOINTER);	/* not XmEXPLICIT */
	XtSetValues(toplevel, arg, 2);
	panel = XtCreateManagedWidget("panel", xmPanedWindowWidgetClass, toplevel,
				      NULL, 0);
	panel2 = XtVaCreateManagedWidget("panel2", xmPanedWindowWidgetClass, panel,
					 XmNseparatorOn, False,
					 XmNsashWidth, 1,
					 XmNsashHeight, 1,
					 NULL);

	rowcol = XtVaCreateManagedWidget("Rowcol", xmRowColumnWidgetClass, panel2,
					 XmNnumColumns, 2,
					 XmNorientation, XmHORIZONTAL,
					 XmNpacking, XmPACK_COLUMN,
					 NULL);
	XtVaGetValues(rowcol, XmNforeground, &fg, XmNbackground, &bg, NULL);
	mouseLeftCursor = XCreatePixmapFromBitmapData(XtDisplay(rowcol),
	      RootWindowOfScreen(XtScreen(rowcol)), (char *) mouse_left_bits,
				 mouse_left_width, mouse_left_height, fg, bg,
				     DefaultDepthOfScreen(XtScreen(rowcol)));
	mouseRightCursor = XCreatePixmapFromBitmapData(XtDisplay(rowcol),
	     RootWindowOfScreen(XtScreen(rowcol)), (char *) mouse_right_bits,
			       mouse_right_width, mouse_right_height, fg, bg,
				     DefaultDepthOfScreen(XtScreen(rowcol)));
	XtVaCreateManagedWidget("mouseLeftText", xmLabelGadgetClass, rowcol,
		   XtVaTypedArg, XmNlabelString, XmRString, "Move", 5, NULL);
	XtVaCreateManagedWidget("mouseLeft", xmLabelGadgetClass, rowcol,
	      XmNlabelType, XmPIXMAP, XmNlabelPixmap, mouseLeftCursor, NULL);
	XtVaCreateManagedWidget("mouseRightText", xmLabelGadgetClass, rowcol,
	     XtVaTypedArg, XmNlabelString, XmRString, "Randomize", 10, NULL);
	XtVaCreateManagedWidget("mouseRight", xmLabelGadgetClass, rowcol,
	     XmNlabelType, XmPIXMAP, XmNlabelPixmap, mouseRightCursor, NULL);
	XtVaCreateManagedWidget("movesText", xmLabelGadgetClass, rowcol,
		  XtVaTypedArg, XmNlabelString, XmRString, "Moves", 6, NULL);
	moves = XtVaCreateManagedWidget("0", xmLabelWidgetClass, rowcol, NULL);
	XtVaCreateManagedWidget("recordText", xmLabelGadgetClass, rowcol,
		 XtVaTypedArg, XmNlabelString, XmRString, "Record", 7, NULL);
	record = XtVaCreateManagedWidget("0", xmLabelWidgetClass, rowcol, NULL);

	rowcol2 = XtVaCreateManagedWidget("Rowcol2", xmRowColumnWidgetClass, panel2,
					  XmNnumColumns, 1,
					  XmNorientation, XmHORIZONTAL,
					  XmNpacking, XmPACK_COLUMN,
					  XmNradioBehavior, True,
					  NULL);
	for (i = 0; i < XtNumber(modeString); i++) {
		modes[i] = XtVaCreateManagedWidget(modeString[i],
					  xmToggleButtonGadgetClass, rowcol2,
						   XmNradioBehavior, True,
						   NULL);
		XtAddCallback(modes[i], XmNvalueChangedCallback,
			      (XtCallbackProc) ModeToggle, (XtPointer) i);
	}

	rowcol3 = XtVaCreateManagedWidget("Rowcol3", xmRowColumnWidgetClass, panel2,
					  XmNnumColumns, 1,
					  XmNorientation, XmHORIZONTAL,
					  XmNpacking, XmPACK_COLUMN,
					  NULL);
	orientSwitch = XtVaCreateManagedWidget("Orient",
					  xmToggleButtonWidgetClass, rowcol3,
					       NULL);
	XtAddCallback(orientSwitch, XmNvalueChangedCallback,
		      (XtCallbackProc) OrientToggle, (XtPointer) NULL);
	practiceSwitch = XtVaCreateManagedWidget("Practice",
					  xmToggleButtonWidgetClass, rowcol3,
						 NULL);
	XtAddCallback(practiceSwitch, XmNvalueChangedCallback,
		      (XtCallbackProc) PracticeToggle, (XtPointer) NULL);

	rowcol4 = XtVaCreateManagedWidget("Rowcol4", xmRowColumnWidgetClass, panel2,
					  XmNnumColumns, 1,
					  XmNorientation, XmHORIZONTAL,
					  XmNpacking, XmPACK_COLUMN,
					  NULL);
	message = XtVaCreateManagedWidget("Play Dino! (use mouse and keypad)",
					  xmLabelWidgetClass, rowcol4,
					  NULL);

	dino2d = XtCreateManagedWidget("dino", dino2dWidgetClass, panel,
				       NULL, 0);
	XtVaSetValues(dino2d,
		      XtNheight, 200,
		      NULL);
	XtAddCallback(dino2d, XtNselectCallback, (XtCallbackProc) CallbackDino,
		      (XtPointer) NULL);
	dino3d = XtCreateManagedWidget("dino", dino3dWidgetClass, panel,
				       NULL, 0);
	XtVaSetValues(dino3d,
		      XtNheight, 200,
		      NULL);
	XtAddCallback(dino3d, XtNselectCallback, (XtCallbackProc) CallbackDino,
		      (XtPointer) NULL);
	Initialize();
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(dino2d), AnyButton, AnyModifier, XtWindow(dino2d),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(dino2d),
		    XCreateFontCursor(XtDisplay(dino2d), XC_crosshair));
	XGrabButton(XtDisplay(dino3d), AnyButton, AnyModifier, XtWindow(dino3d),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(dino3d),
		    XCreateFontCursor(XtDisplay(dino3d), XC_crosshair));
	XtMainLoop();

#ifdef VMS
	return 1;
#else
	return 0;
#endif
}

/* There's probably a better way to assure that they are the same but I don't
   know it off hand. */
static void
MakeEquivalent(String * username, int *mode, Boolean * orient, Boolean * practice)
{
	Boolean     mono;
	Pixel       foreground, background, pieceBorder;
	String      faceColor[MAXFACES];

	XtVaGetValues(dino2d,
		      XtNuserName, username,
		      XtNmode, mode,
		      XtNorient, orient,
		      XtNpractice, practice,
		      XtNmono, &mono,
		      XtNforeground, &foreground,
		      XtNbackground, &background,
		      XtNpieceBorder, &pieceBorder,
		      XtNfaceColor0, &(faceColor[0]),
		      XtNfaceColor1, &(faceColor[1]),
		      XtNfaceColor2, &(faceColor[2]),
		      XtNfaceColor3, &(faceColor[3]),
		      XtNfaceColor4, &(faceColor[4]),
		      XtNfaceColor5, &(faceColor[5]),
		      NULL);
	XtVaSetValues(dino2d,
		      XtNdirection, DINO_IGNORE,
		      XtNstart, FALSE,
		      NULL);
	XtVaSetValues(dino3d,
		      XtNuserName, *username,
		      XtNmode, *mode,
		      XtNorient, *orient,
		      XtNpractice, *practice,
		      XtNmono, mono,
		      XtNdirection, DINO_IGNORE,
		      XtNstart, FALSE,
		      XtNforeground, foreground,
		      XtNbackground, background,
		      XtNpieceBorder, pieceBorder,
		      XtNfaceColor0, faceColor[0],
		      XtNfaceColor1, faceColor[1],
		      XtNfaceColor2, faceColor[2],
		      XtNfaceColor3, faceColor[3],
		      XtNfaceColor4, faceColor[4],
		      XtNfaceColor5, faceColor[5],
		      NULL);
}

static void
Initialize(void)
{
	int         mode;
	Boolean     orient, practice;
	String      username;

	MakeEquivalent(&username, &mode, &orient, &practice);
	XmToggleButtonSetState(modes[mode - PERIOD2], True, False);
	XmToggleButtonSetState(orientSwitch, orient, True);
	XmToggleButtonSetState(practiceSwitch, practice, True);
	InitRecords();
	ReadRecords();
	(void) strcpy(usernameDsp, username);
	if (!strcmp(usernameDsp, "") || !strcmp(usernameDsp, NOACCESS) ||
	    !strcmp(usernameDsp, NOBODY)) {
		/* The NOACCESS is not necasary, but it stops people from being cute. */
		(void) sprintf(usernameDsp, "%s", getlogin());
		if (!strcmp(usernameDsp, "") || !strcmp(usernameDsp, NOACCESS))
			(void) sprintf(usernameDsp, "%s", NOBODY);	/* It really IS nobody */
	}
	PrintRecord(mode, orient, practice);
}

static void
CallbackDino(Widget w, caddr_t clientData, dinoCallbackStruct * callData)
{
	int         mode;
	Boolean     orient, practice, start;
	Widget      otherw;

	if (w == dino2d)
		otherw = dino3d;
	else			/* (w == dino3d) */
		otherw = dino2d;
	XtVaGetValues(w,
		      XtNorient, &orient,
		      XtNmode, &mode,
		      XtNpractice, &practice,
		      XtNstart, &start,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case DINO_RESTORE:
			XtSetArg(arg[0], XtNdirection, DINO_RESTORE);
			XtSetValues(otherw, arg, 1);
			XtSetValues(w, arg, 1);
			movesDsp = 0;
			break;
		case DINO_RESET:
			movesDsp = 0;
			break;
		case DINO_ILLEGAL:
			if (practice || start)
				(void) strcpy(messageDsp, "Illegal move");
			else
				(void) strcpy(messageDsp, "Randomize to start");
			break;
		case DINO_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetArg(arg[1], XtNface, callData->face);
			XtSetArg(arg[2], XtNpos, callData->position);
			XtSetArg(arg[3], XtNdirection, callData->direction);
			XtSetArg(arg[4], XtNstyle, callData->style);
			XtSetValues(otherw, arg, 5);
			XtSetValues(w, arg, 1);
			break;
		case DINO_CONTROL:
			XtSetArg(arg[0], XtNface, callData->face);
			XtSetArg(arg[1], XtNpos, callData->position);
			XtSetArg(arg[2], XtNdirection, callData->direction);
			XtSetArg(arg[3], XtNstyle, callData->style);
			XtSetValues(otherw, arg, 4);
			return;
		case DINO_SOLVED:
			if (practice)
				movesDsp = 0;
			else {
				if (HandleSolved(movesDsp, mode, orient))
					(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
				else
					(void) strcpy(messageDsp, "Solved!");
			}
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case DINO_PRACTICE:
			movesDsp = 0;
			practice = !practice;
			if (!practice)
				(void) strcpy(messageDsp, "Randomize to start");
			PrintRecord(mode, orient, practice);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XtSetValues(otherw, arg, 2);
			XmToggleButtonSetState(practiceSwitch, practice, True);
			break;
		case DINO_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNpractice, FALSE);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XtSetValues(otherw, arg, 2);
			break;
		case DINO_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(mode, orient, practice);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			XmToggleButtonSetState(orientSwitch, orient, True);
			break;
		case DINO_PERIOD2:
		case DINO_PERIOD3:
		case DINO_BOTH:
			movesDsp = 0;
			mode = callData->reason - DINO_PERIOD2 + PERIOD2;
			PrintRecord(mode, orient, practice);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			XmToggleButtonSetState(modes[mode - PERIOD2], True, True);
			break;
		case DINO_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case DINO_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetArg(arg[1], XtNface, callData->face);
			XtSetArg(arg[2], XtNpos, callData->position);
			XtSetArg(arg[3], XtNdirection, callData->direction);
			XtSetArg(arg[4], XtNstyle, callData->style);
			XtSetValues(otherw, arg, 5);
			XtSetValues(w, arg, 1);
			break;
	}
	motif_print(message, messageDsp);
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
}

static void
ModeToggle(Widget w, int mode, XmToggleButtonCallbackStruct * cbs)
{
	Boolean     orient, practice;

	if (cbs->set) {
		XtVaGetValues(dino2d,
			      XtNorient, &orient,
			      XtNpractice, &practice,
			      NULL);
		XtVaSetValues(dino2d,
			      XtNmode, mode + PERIOD2,
			      NULL);
		XtVaSetValues(dino3d,
			      XtNmode, mode + PERIOD2,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(mode + PERIOD2, orient, practice);
	}
}

static void
OrientToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         mode;
	Boolean     orient = cbs->set, practice;

	XtVaGetValues(dino2d,
		      XtNmode, &mode,
		      XtNpractice, &practice,
		      NULL);
	XtVaSetValues(dino2d,
		      XtNorient, orient,
		      NULL);
	XtVaSetValues(dino3d,
		      XtNorient, orient,
		      NULL);
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	PrintRecord(mode, orient, practice);
}

static void
PracticeToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         mode;
	Boolean     orient, practice = cbs->set;

	XtVaSetValues(dino2d,
		      XtNpractice, practice,
		      XtNstart, FALSE,
		      NULL);
	XtVaSetValues(dino3d,
		      XtNpractice, practice,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(dino2d,
		      XtNmode, &mode,
		      XtNpractice, &orient,
		      NULL);
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	if (!practice)
		(void) strcpy(messageDsp, "Randomize to start");
	PrintRecord(mode, orient, practice);
}

static void
PrintRecord(int mode, Boolean orient, Boolean practice)
{
	int         i = mode - PERIOD2;
	int         j = (orient) ? 1 : 0;

	if (practice)
		motif_print(record, "practice");
	else if (dinoRecord[i][j].score >= MAXRECORD) {
		(void) sprintf(buff, "NEVER %s", NOACCESS);
		motif_print(record, buff);
	} else {
		(void) sprintf(buff, "%d %s",
			       dinoRecord[i][j].score, dinoRecord[i][j].name);
		motif_print(record, buff);
	}
}

static int
HandleSolved(int counter, int mode, Boolean orient)
{
	int         i = mode - PERIOD2;
	int         j = (orient) ? 1 : 0;

	if (counter < dinoRecord[i][j].score) {
		dinoRecord[i][j].score = counter;
		(void) strcpy(dinoRecord[i][j].name, usernameDsp);
		if (orient && (counter < dinoRecord[!i][j].score)) {
			dinoRecord[!i][j].score = counter;
			(void) strcpy(dinoRecord[!i][j].name, usernameDsp);
		}
		WriteRecords();
		PrintRecord(mode, orient, FALSE);
		return TRUE;
	}
	return FALSE;
}

static void
InitRecords(void)
{
	int         mode, orient;

	for (mode = 0; mode < MAXMODES; mode++)
		for (orient = 0; orient < 2; orient++) {
			dinoRecord[mode][orient].score = MAXRECORD;
			(void) strcpy(dinoRecord[mode][orient].name, NOACCESS);
		}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         n, mode, orient;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL) {
		(void) sprintf(buff, "Can not open %s, taking defaults.", SCOREFILE);
		motif_print(message, buff);
	} else {
		for (mode = 0; mode < MAXMODES; mode++)
			for (orient = 0; orient < 2; orient++) {
				(void) fscanf(fp, "%d %s\n", &n, username);
				if (n <= dinoRecord[mode][orient].score) {
					dinoRecord[mode][orient].score = n;
					(void) strcpy(dinoRecord[mode][orient].name, username);
				}
			}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         mode, orient;

	if ((fp = fopen(SCOREFILE, "w")) == NULL) {
		(void) sprintf(buff, "Can not write to %s.", SCOREFILE);
		motif_print(message, buff);
	} else {
#if HAVE_FCNTL_H
		int         lfd;
		char        lockfile[FILENAMELEN];

		(void) strcpy(lockfile, SCOREFILE);
		(void) strcat(lockfile, ".lock");
		while (((lfd = open(lockfile, O_CREAT | O_EXCL, 0644)) < 0) &&
		       errno == EEXIST)
			(void) sleep(1);
		if (lfd < 0) {
#if 1
			(void) fprintf(stderr, "Lock file exists... guessing its an old one.\n");
#else
			(void) fprintf(stderr, "Lock file exists... score not recorded - sorry.\n"
				);
			return;
#endif
		}
#endif
		for (mode = 0; mode < MAXMODES; mode++) {
			for (orient = 0; orient < 2; orient++)
				(void) fprintf(fp, "%d %s\n",
					       dinoRecord[mode][orient].score, dinoRecord[mode][orient].name);
			(void) fprintf(fp, "\n");
		}
#if HAVE_FCNTL_H
		(void) close(lfd);
		(void) unlink(lockfile);
#endif
		(void) fclose(fp);
	}
}

static void
motif_print(Widget w, char *text)
{
	Arg         wargs[1];
	XmString    xmstr;

	if (!XtIsSubclass(w, xmLabelWidgetClass))
		XtError("motif_print() requires a Label Widget");
	xmstr = XmStringCreateLtoR(text, XmSTRING_DEFAULT_CHARSET);
	XtSetArg(wargs[0], XmNlabelString, xmstr);
	XtSetValues(w, wargs, 1);
}
