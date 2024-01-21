
/*-
# MOTIF-BASED PYRAMINX(tm)
#
#  xmpyraminx.c
#
###
#
#  Copyright (c) 1993 - 97	David Albert Bagley, bagleyd@bigfoot.com
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
#  This program is distributed in the hope that it will be "useful",
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
*/

/*-
  Version 5: 95/10/04 Xt/Motif
  Version 4: 94/05/31 Xt
  Version 3: 93/04/01 Motif
  Version 2: 92/01/29 XView
  Version 1: 91/03/19 SunView
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
#include "Pyraminx.h"
#include "pyraminx.xbm"
#include "mouse-l.xbm"
#include "mouse-r.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/pyraminx.scores"
#endif

/* The following are in PyraminxP.h also */
#define MINTETRAS 1
#define PERIOD2 2
#define PERIOD3 3
#define BOTH 4
#define MAXMODES 3

#define MAXTETRAS 7
#define MAXRECORD 32767
#define FILENAMELEN 1024
#define USERNAMELEN 128
#define NOACCESS "noaccess"
#define NOBODY "nobody"

typedef struct {
	int         score;
	char        name[USERNAMELEN];
} GameRecord;

static void Initialize(Widget w);
static void CallbackPyraminx(Widget w, caddr_t clientData,
			     pyraminxCallbackStruct * callData);

static void PrintRecord(int size, int mode,
			Boolean orient, Boolean sticky, Boolean practice);
static int  HandleSolved(int counter, int size, int mode,
			 Boolean orient, Boolean sticky);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static void TetraSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs);
static void ModeToggle(Widget w, int mode, XmToggleButtonCallbackStruct * cbs);
static void OrientToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void StickyToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void PracticeToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void motif_print(Widget w, char *text);

static Arg  arg[5];
static Widget moves, record, message, pyraminx, modes[MAXMODES], orientSwitch,
            stickySwitch, practiceSwitch, tetras;
static GameRecord pyraminxRecord[MAXMODES][2][MAXTETRAS - MINTETRAS + 2];
static int  movesDsp = 0;
static int  oldSize;
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
	(void) fprintf(stderr, "usage: xmpyraminx\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		       "\t[-{border|bd} {color}] [-face{0|1|2|3} {color}]\n");
	(void) fprintf(stderr,
		       "\t[-{size {int} | sticky}] [-{mode {int} | both}]\n");
	(void) fprintf(stderr,
		   "\t[-[no]orient] [-[no]practice] [-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*pyraminx.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*pyraminx.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*pyraminx.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*pyraminx.pieceBorder", XrmoptionSepArg, NULL},
	{"-border", "*pyraminx.pieceBorder", XrmoptionSepArg, NULL},
	{"-face0", "*pyraminx.faceColor0", XrmoptionSepArg, NULL},
	{"-face1", "*pyraminx.faceColor1", XrmoptionSepArg, NULL},
	{"-face2", "*pyraminx.faceColor2", XrmoptionSepArg, NULL},
	{"-face3", "*pyraminx.faceColor3", XrmoptionSepArg, NULL},
	{"-size", "*pyraminx.size", XrmoptionSepArg, NULL},
	{"-sticky", "*pyraminx.sticky", XrmoptionNoArg, "FALSE"},
	{"-mode", "*pyraminx.mode", XrmoptionSepArg, NULL},
	{"-both", "*pyraminx.mode", XrmoptionNoArg, "4"},
	{"-orient", "*pyraminx.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*pyraminx.orient", XrmoptionNoArg, "FALSE"},
	{"-practice", "*pyraminx.practice", XrmoptionNoArg, "TRUE"},
	{"-nopractice", "*pyraminx.practice", XrmoptionNoArg, "FALSE"},
	{"-username", "*pyraminx.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel;
	Widget      panel, panel2, rowcol, rowcol2, rowcol3, rowcol4, rowcol5;
	Pixmap      mouseLeftCursor, mouseRightCursor;
	Pixel       fg, bg;
	int         i;

	toplevel = XtInitialize(argv[0], "Pyraminx",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
		   (char *) pyraminx_bits, pyraminx_width, pyraminx_height));
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
	XtVaCreateManagedWidget("CallbackPyraminx", xmLabelGadgetClass, rowcol,
		  XtVaTypedArg, XmNlabelString, XmRString, "Moves", 6, NULL);
	moves = XtVaCreateManagedWidget("0", xmLabelWidgetClass, rowcol, NULL);
	XtVaCreateManagedWidget("record_text", xmLabelGadgetClass, rowcol,
		 XtVaTypedArg, XmNlabelString, XmRString, "Record", 7, NULL);
	record = XtVaCreateManagedWidget("0", xmLabelWidgetClass, rowcol, NULL);

	rowcol2 = XtVaCreateManagedWidget("Rowcol2", xmRowColumnWidgetClass, panel2,
					  NULL);
	XtVaGetValues(rowcol2, XmNforeground, &fg, XmNbackground, &bg, NULL);
	tetras = XtVaCreateManagedWidget("tetras", xmScaleWidgetClass, rowcol2,
			XtVaTypedArg, XmNtitleString, XmRString, "Tetras", 7,
					 XmNminimum, MINTETRAS,
					 XmNmaximum, MAXTETRAS,
					 XmNvalue, MINTETRAS,
					 XmNshowValue, True,
					 XmNorientation, XmHORIZONTAL,
					 NULL);
	XtAddCallback(tetras, XmNvalueChangedCallback, (XtCallbackProc) TetraSlider,
		      (XtPointer) NULL);
	rowcol3 = XtVaCreateManagedWidget("Rowcol3", xmRowColumnWidgetClass, panel2,
					  XmNnumColumns, 1,
					  XmNorientation, XmHORIZONTAL,
					  XmNpacking, XmPACK_COLUMN,
					  XmNradioBehavior, True,
					  NULL);
	for (i = 0; i < XtNumber(modeString); i++) {
		modes[i] = XtVaCreateManagedWidget(modeString[i],
					  xmToggleButtonGadgetClass, rowcol3,
						   XmNradioBehavior, True,
						   NULL);
		XtAddCallback(modes[i], XmNvalueChangedCallback,
			      (XtCallbackProc) ModeToggle, (XtPointer) i);
	}
	rowcol4 = XtVaCreateManagedWidget("Rowcol4", xmRowColumnWidgetClass, panel2,
					  XmNnumColumns, 1,
					  XmNorientation, XmHORIZONTAL,
					  XmNpacking, XmPACK_COLUMN,
					  NULL);
	orientSwitch = XtVaCreateManagedWidget("Orient",
					  xmToggleButtonWidgetClass, rowcol4,
					       NULL);
	XtAddCallback(orientSwitch, XmNvalueChangedCallback,
		      (XtCallbackProc) OrientToggle, (XtPointer) NULL);
	stickySwitch = XtVaCreateManagedWidget("Sticky",
					  xmToggleButtonWidgetClass, rowcol4,
					       NULL);
	XtAddCallback(stickySwitch, XmNvalueChangedCallback,
		      (XtCallbackProc) StickyToggle, (XtPointer) NULL);
	practiceSwitch = XtVaCreateManagedWidget("Practice",
					  xmToggleButtonWidgetClass, rowcol4,
						 NULL);
	XtAddCallback(practiceSwitch, XmNvalueChangedCallback,
		      (XtCallbackProc) PracticeToggle, (XtPointer) NULL);
	rowcol5 = XtVaCreateManagedWidget("Rowcol5", xmRowColumnWidgetClass, panel2,
					  NULL);
	message = XtVaCreateManagedWidget("Play Pyraminx! (use mouse and keypad)",
					  xmLabelWidgetClass, rowcol5,
					  NULL);

	pyraminx = XtCreateManagedWidget("pyraminx", pyraminxWidgetClass, panel,
					 NULL, 0);
	XtAddCallback(pyraminx, XtNselectCallback, (XtCallbackProc) CallbackPyraminx,
		      (XtPointer) NULL);
	Initialize(pyraminx);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(pyraminx), AnyButton, AnyModifier, XtWindow(pyraminx),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(pyraminx),
		    XCreateFontCursor(XtDisplay(pyraminx), XC_crosshair));
	XtMainLoop();

#ifdef VMS
	return 1;
#else
	return 0;
#endif
}

static void
Initialize(Widget w)
{
	int         size, mode;
	Boolean     orient, sticky, practice;
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNsize, &size,
		      XtNmode, &mode,
		      XtNorient, &orient,
		      XtNsticky, &sticky,
		      XtNpractice, &practice,
		      NULL);
	if (size <= MAXTETRAS)
		XmScaleSetValue(tetras, size);
	XmToggleButtonSetState(modes[mode - PERIOD2], True, False);
	XmToggleButtonSetState(orientSwitch, orient, True);
	XmToggleButtonSetState(stickySwitch, sticky, True);
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
	PrintRecord(size, mode, orient, sticky, practice);
	oldSize = size;
}

static void
CallbackPyraminx(Widget w, caddr_t clientData, pyraminxCallbackStruct * callData)
{
	int         size, mode;
	Boolean     orient, sticky, practice, start;

	XtVaGetValues(w,
		      XtNsize, &size,
		      XtNmode, &mode,
		      XtNorient, &orient,
		      XtNsticky, &sticky,
		      XtNpractice, &practice,
		      XtNstart, &start,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case PYRAMINX_RESTORE:
			if (practice)
				motif_print(record, "practice");
			movesDsp = 0;
			break;
		case PYRAMINX_RESET:
			movesDsp = 0;
			break;
		case PYRAMINX_AMBIGUOUS:
			(void) strcpy(messageDsp, "Ambiguous move");
			break;
		case PYRAMINX_ILLEGAL:
			if (practice || start)
				(void) strcpy(messageDsp, "Illegal move");
			else
				(void) strcpy(messageDsp, "Randomize to start");
			break;
		case PYRAMINX_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case PYRAMINX_CONTROL:
			return;
		case PYRAMINX_SOLVED:
			if (practice)
				movesDsp = 0;
			else {
				if (HandleSolved(movesDsp, size, mode, orient, sticky))
					(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
				else
					(void) strcpy(messageDsp, "Solved!");
			}
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case PYRAMINX_PRACTICE:
			movesDsp = 0;
			practice = !practice;
			if (!practice)
				(void) strcpy(messageDsp, "Randomize to start");
			PrintRecord(size, mode, orient, sticky, practice);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XmToggleButtonSetState(practiceSwitch, practice, True);
			break;
		case PYRAMINX_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNpractice, FALSE);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			break;
		case PYRAMINX_DEC:
			if (!sticky) {
				movesDsp = 0;
				size--;
				oldSize = size;
				PrintRecord(size, mode, orient, sticky, practice);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
				if (size <= MAXTETRAS)
					XmScaleSetValue(tetras, size);
			}
			break;
		case PYRAMINX_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(size, mode, orient, sticky, practice);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(orientSwitch, orient, True);
			break;
		case PYRAMINX_INC:
			if (!sticky) {
				movesDsp = 0;
				size++;
				oldSize = size;
				PrintRecord(size, mode, orient, sticky, practice);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
				if (size <= MAXTETRAS)
					XmScaleSetValue(tetras, size);
			}
			break;
		case PYRAMINX_PERIOD2:
		case PYRAMINX_PERIOD3:
		case PYRAMINX_BOTH:
			movesDsp = 0;
			mode = callData->reason - PYRAMINX_PERIOD2 + PERIOD2;
			PrintRecord(size, mode, orient, sticky, practice);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(modes[mode - PERIOD2], True, True);
			break;
		case PYRAMINX_STICKY:
			movesDsp = 0;
			sticky = !sticky;
			if (sticky)
				size = 4;
			else
				size = oldSize;
			PrintRecord(size, mode, orient, sticky, practice);
			XtSetArg(arg[0], XtNsticky, sticky);
			XtSetArg(arg[1], XtNsize, size);
			XtSetValues(w, arg, 2);
			XmToggleButtonSetState(stickySwitch, sticky, True);
			break;
		case PYRAMINX_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case PYRAMINX_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
	}
	motif_print(message, messageDsp);
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
}

static void
TetraSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         size = cbs->value, mode;
	Boolean     orient, sticky, practice;

	XtVaGetValues(pyraminx,
		      XtNsize, &oldSize,
		      XtNmode, &mode,
		      XtNorient, &orient,
		      XtNsticky, &sticky,
		      XtNpractice, &practice,
		      NULL);
	if (sticky)
		XmScaleSetValue(w, oldSize);
	else if (oldSize != size) {
		XtVaSetValues(pyraminx,
			      XtNsize, size,
			      NULL);
		oldSize = size;
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(size, mode, orient, sticky, practice);
	}
}

static void
ModeToggle(Widget w, int mode, XmToggleButtonCallbackStruct * cbs)
{
	int         size;
	Boolean     orient, sticky, practice;

	if (cbs->set) {
		XtVaGetValues(pyraminx,
			      XtNsize, &size,
			      XtNorient, &orient,
			      XtNsticky, &sticky,
			      XtNpractice, &practice,
			      NULL);
		XtVaSetValues(pyraminx,
			      XtNmode, mode + PERIOD2,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(size, mode + PERIOD2, orient, sticky, practice);
	}
}

static void
OrientToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         size, mode;
	Boolean     orient = cbs->set, sticky, practice;

	XtVaGetValues(pyraminx,
		      XtNsize, &size,
		      XtNmode, &mode,
		      XtNsticky, &sticky,
		      XtNpractice, &practice,
		      NULL);
	XtVaSetValues(pyraminx,
		      XtNorient, orient,
		      NULL);
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	PrintRecord(size, mode, orient, sticky, practice);
}

static void
StickyToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         size, mode;
	Boolean     orient, sticky = cbs->set, practice;

	XtVaGetValues(pyraminx,
		      XtNsize, &size,
		      XtNmode, &mode,
		      XtNorient, &orient,
		      XtNpractice, &practice,
		      NULL);
	XtVaSetValues(pyraminx,
		      XtNsticky, sticky,
		      NULL);
	if ((sticky && size != 4) || (!sticky && oldSize != 4)) {
		if (sticky)
			size = 4;
		else
			size = oldSize;
		if (size <= MAXTETRAS)
			XmScaleSetValue(tetras, size);
		XtVaSetValues(pyraminx,
			      XtNsize, size,
			      NULL);
	}
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	PrintRecord(size, mode, orient, sticky, practice);
}

static void
PracticeToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         size, mode;
	Boolean     orient, sticky, practice = cbs->set;

	XtVaSetValues(pyraminx,
		      XtNpractice, practice,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(pyraminx,
		      XtNsize, &size,
		      XtNmode, &mode,
		      XtNorient, &orient,
		      XtNsticky, &sticky,
		      NULL);
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	if (!practice)
		(void) strcpy(messageDsp, "Randomize to start");
	PrintRecord(size, mode, orient, sticky, practice);
}

static void
PrintRecord(int size, int mode, Boolean orient, Boolean sticky, Boolean practice)
{
	int         i = mode - PERIOD2;
	int         j = (orient) ? 1 : 0;
	int         k = (sticky) ? MAXTETRAS - MINTETRAS + 1 : size - MINTETRAS;

	if (practice)
		motif_print(record, "practice");
	else if (!sticky && size > MAXTETRAS)
		motif_print(record, "NOT RECORDED");
	else if (pyraminxRecord[i][j][k].score >= MAXRECORD) {
		(void) sprintf(buff, "NEVER %s", NOACCESS);
		motif_print(record, buff);
	} else {
		(void) sprintf(buff, "%d %s",
		pyraminxRecord[i][j][k].score, pyraminxRecord[i][j][k].name);
		motif_print(record, buff);
	}
}

static int
HandleSolved(int counter, int size, int mode, Boolean orient, Boolean sticky)
{
	int         i = mode - PERIOD2;
	int         j = (orient) ? 1 : 0;
	int         k = (sticky) ? MAXTETRAS - MINTETRAS + 1 : size - MINTETRAS;

	if ((sticky || size <= MAXTETRAS) &&
	    counter < pyraminxRecord[i][j][k].score) {
		pyraminxRecord[i][j][k].score = counter;
		(void) strcpy(pyraminxRecord[i][j][k].name, usernameDsp);
		if (size < 4 || mode == PERIOD2 || (orient &&
			       (counter < pyraminxRecord[i][!j][k].score))) {
			pyraminxRecord[i][!j][k].score = counter;
			(void) strcpy(pyraminxRecord[i][!j][k].name, usernameDsp);
		}
		WriteRecords();
		PrintRecord(size, mode, orient, sticky, FALSE);
		return TRUE;
	}
	return FALSE;
}

static void
InitRecords(void)
{
	int         i, mode, orient;

	for (mode = 0; mode < MAXMODES; mode++)
		for (orient = 0; orient < 2; orient++)
			for (i = 0; i <= MAXTETRAS - MINTETRAS + 1; i++) {
				pyraminxRecord[mode][orient][i].score = MAXRECORD;
				(void) strcpy(pyraminxRecord[mode][orient][i].name, NOACCESS);
			}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, n, mode, orient;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL) {
		(void) sprintf(buff, "Can not open %s, taking defaults.", SCOREFILE);
		motif_print(message, buff);
	} else {
		for (mode = 0; mode < MAXMODES; mode++)
			for (orient = 0; orient < 2; orient++)
				for (i = 0; i <= MAXTETRAS - MINTETRAS + 1; i++) {
					(void) fscanf(fp, "%d %s\n", &n, username);
					if (n <= pyraminxRecord[mode][orient][i].score) {
						pyraminxRecord[mode][orient][i].score = n;
						(void) strcpy(pyraminxRecord[mode][orient][i].name, username);
					}
				}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, mode, orient;

	ReadRecords();		/* Maybe its been updated by another */
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
			for (orient = 0; orient < 2; orient++) {
				for (i = 0; i <= MAXTETRAS - MINTETRAS + 1; i++)
					(void) fprintf(fp, "%d %s\n",
					pyraminxRecord[mode][orient][i].score,
					pyraminxRecord[mode][orient][i].name);
				(void) fprintf(fp, "\n");
			}
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
