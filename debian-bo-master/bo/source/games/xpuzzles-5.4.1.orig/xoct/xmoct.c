
/*-
# MOTIF-BASED OCTAHEDRON
#
#  xmoct.c
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
  Version 2: 92/02/01 XView
  Version 1: 91/06/10 SunView
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
#include "Oct.h"
#include "oct.xbm"
#include "mouse-l.xbm"
#include "mouse-r.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/oct.scores"
#endif

/* The following are in OctP.h also */
#define MINOCTAS 1
#define PERIOD3 3
#define PERIOD4 4
#define BOTH 5
#define MAXMODES 3

#define MAXOCTAS 6
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
static void CallbackOct(Widget w, caddr_t clientData,
			octCallbackStruct * callData);

static void PrintRecord(int size, int mode,
			Boolean orient, Boolean sticky, Boolean practice);
static int  HandleSolved(int counter, int size, int mode,
			 Boolean orient, Boolean sticky);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static void OctaSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs);
static void ModeToggle(Widget w, int mode, XmToggleButtonCallbackStruct * cbs);
static void OrientToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void StickyToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void PracticeToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs);
static void motif_print(Widget w, char *text);

static Arg  arg[5];
static Widget moves, record, message, oct, modes[MAXMODES], orientSwitch,
            stickySwitch, practiceSwitch, octas;
static GameRecord octRecord[MAXMODES][2][MAXOCTAS - MINOCTAS + 2];
static int  movesDsp = 0;
static int  oldSize;
static char messageDsp[128] = "Welcome";
static char usernameDsp[USERNAMELEN] = "";
static char buff[256];

static char *modeString[] =
{
	"Period 3", "Period 4", "Both"
};

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmoct\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
	      "\t[-{border|bd} {color}] [-face{0|1|2|3|4|5|6|7} {color}]\n");
	(void) fprintf(stderr,
		       "\t[-{size {int} | sticky}] [-{mode {int} | both}]\n");
	(void) fprintf(stderr,
		   "\t[-[no]orient] [-[no]practice] [-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*oct.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*oct.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*oct.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*oct.pieceBorder", XrmoptionSepArg, NULL},
	{"-border", "*oct.pieceBorder", XrmoptionSepArg, NULL},
	{"-face0", "*oct.faceColor0", XrmoptionSepArg, NULL},
	{"-face1", "*oct.faceColor1", XrmoptionSepArg, NULL},
	{"-face2", "*oct.faceColor2", XrmoptionSepArg, NULL},
	{"-face3", "*oct.faceColor3", XrmoptionSepArg, NULL},
	{"-face4", "*oct.faceColor4", XrmoptionSepArg, NULL},
	{"-face5", "*oct.faceColor5", XrmoptionSepArg, NULL},
	{"-face6", "*oct.faceColor6", XrmoptionSepArg, NULL},
	{"-face7", "*oct.faceColor7", XrmoptionSepArg, NULL},
	{"-size", "*oct.size", XrmoptionSepArg, NULL},
	{"-sticky", "*oct.sticky", XrmoptionNoArg, "FALSE"},
	{"-mode", "*oct.mode", XrmoptionSepArg, NULL},
	{"-both", "*oct.mode", XrmoptionNoArg, "4"},
	{"-orient", "*oct.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*oct.orient", XrmoptionNoArg, "FALSE"},
	{"-practice", "*oct.practice", XrmoptionNoArg, "TRUE"},
	{"-nopractice", "*oct.practice", XrmoptionNoArg, "FALSE"},
	{"-username", "*oct.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel;
	Widget      panel, panel2, rowcol, rowcol2, rowcol3, rowcol4, rowcol5;
	Pixmap      mouseLeftCursor, mouseRightCursor;
	Pixel       fg, bg;
	int         i;

	toplevel = XtInitialize(argv[0], "Oct",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
				  (char *) oct_bits, oct_width, oct_height));
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
	XtVaCreateManagedWidget("CallbackOct", xmLabelGadgetClass, rowcol,
		  XtVaTypedArg, XmNlabelString, XmRString, "Moves", 6, NULL);
	moves = XtVaCreateManagedWidget("0", xmLabelWidgetClass, rowcol, NULL);
	XtVaCreateManagedWidget("record_text", xmLabelGadgetClass, rowcol,
		 XtVaTypedArg, XmNlabelString, XmRString, "Record", 7, NULL);
	record = XtVaCreateManagedWidget("0", xmLabelWidgetClass, rowcol, NULL);

	rowcol2 = XtVaCreateManagedWidget("Rowcol2", xmRowColumnWidgetClass, panel2,
					  NULL);
	XtVaGetValues(rowcol2, XmNforeground, &fg, XmNbackground, &bg, NULL);
	octas = XtVaCreateManagedWidget("octas", xmScaleWidgetClass, rowcol2,
			 XtVaTypedArg, XmNtitleString, XmRString, "Octas", 6,
					XmNminimum, MINOCTAS,
					XmNmaximum, MAXOCTAS,
					XmNvalue, MINOCTAS,
					XmNshowValue, True,
					XmNorientation, XmHORIZONTAL,
					NULL);
	XtAddCallback(octas, XmNvalueChangedCallback, (XtCallbackProc) OctaSlider,
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
	message = XtVaCreateManagedWidget("Play Oct! (use mouse and keypad)",
					  xmLabelWidgetClass, rowcol5,
					  NULL);

	oct = XtCreateManagedWidget("oct", octWidgetClass, panel,
				    NULL, 0);
	XtAddCallback(oct, XtNselectCallback, (XtCallbackProc) CallbackOct,
		      (XtPointer) NULL);
	Initialize(oct);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(oct), AnyButton, AnyModifier, XtWindow(oct),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(oct),
		    XCreateFontCursor(XtDisplay(oct), XC_crosshair));
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
	if (size <= MAXOCTAS)
		XmScaleSetValue(octas, size);
	XmToggleButtonSetState(modes[mode - PERIOD3], True, False);
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
CallbackOct(Widget w, caddr_t clientData, octCallbackStruct * callData)
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
		case OCT_RESTORE:
			if (practice)
				motif_print(record, "practice");
			movesDsp = 0;
			break;
		case OCT_RESET:
			movesDsp = 0;
			break;
		case OCT_AMBIGUOUS:
			(void) strcpy(messageDsp, "Ambiguous move");
			break;
		case OCT_ILLEGAL:
			if (practice || start)
				(void) strcpy(messageDsp, "Illegal move");
			else
				(void) strcpy(messageDsp, "Randomize to start");
			break;
		case OCT_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case OCT_CONTROL:
			return;
		case OCT_SOLVED:
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
		case OCT_PRACTICE:
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
		case OCT_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNpractice, FALSE);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			break;
		case OCT_DEC:
			if (!sticky) {
				movesDsp = 0;
				size--;
				oldSize = size;
				PrintRecord(size, mode, orient, sticky, practice);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
				if (size <= MAXOCTAS)
					XmScaleSetValue(octas, size);
			}
			break;
		case OCT_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(size, mode, orient, sticky, practice);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(orientSwitch, orient, True);
			break;
		case OCT_INC:
			if (!sticky) {
				movesDsp = 0;
				size++;
				oldSize = size;
				PrintRecord(size, mode, orient, sticky, practice);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
				if (size <= MAXOCTAS)
					XmScaleSetValue(octas, size);
			}
			break;
		case OCT_PERIOD3:
		case OCT_PERIOD4:
		case OCT_BOTH:
			movesDsp = 0;
			mode = callData->reason - OCT_PERIOD3 + PERIOD3;
			PrintRecord(size, mode, orient, sticky, practice);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(modes[mode - PERIOD3], True, True);
			break;
		case OCT_STICKY:
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
		case OCT_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case OCT_UNDO:
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
OctaSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         size = cbs->value, mode;
	Boolean     orient, sticky, practice;

	XtVaGetValues(oct,
		      XtNsize, &oldSize,
		      XtNmode, &mode,
		      XtNorient, &orient,
		      XtNsticky, &sticky,
		      XtNpractice, &practice,
		      NULL);
	if (sticky)
		XmScaleSetValue(w, oldSize);
	else if (oldSize != size) {
		XtVaSetValues(oct,
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
		XtVaGetValues(oct,
			      XtNsize, &size,
			      XtNorient, &orient,
			      XtNsticky, &sticky,
			      XtNpractice, &practice,
			      NULL);
		XtVaSetValues(oct,
			      XtNmode, mode + PERIOD3,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(size, mode + PERIOD3, orient, sticky, practice);
	}
}

static void
OrientToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         size, mode;
	Boolean     orient = cbs->set, sticky, practice;

	XtVaGetValues(oct,
		      XtNsize, &size,
		      XtNmode, &mode,
		      XtNsticky, &sticky,
		      XtNpractice, &practice,
		      NULL);
	XtVaSetValues(oct,
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

	XtVaGetValues(oct,
		      XtNsize, &size,
		      XtNmode, &mode,
		      XtNorient, &orient,
		      XtNpractice, &practice,
		      NULL);
	XtVaSetValues(oct,
		      XtNsticky, sticky,
		      NULL);
	if ((sticky && size != 4) || (!sticky && oldSize != 4)) {
		if (sticky)
			size = 4;
		else
			size = oldSize;
		if (size <= MAXOCTAS)
			XmScaleSetValue(octas, size);
		XtVaSetValues(oct,
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

	XtVaSetValues(oct,
		      XtNpractice, practice,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(oct,
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
	int         i = mode - PERIOD3;
	int         j = (orient) ? 1 : 0;
	int         k = (sticky) ? MAXOCTAS - MINOCTAS + 1 : size - MINOCTAS;

	if (practice)
		motif_print(record, "practice");
	else if (!sticky && size > MAXOCTAS)
		motif_print(record, "NOT RECORDED");
	else if (octRecord[i][j][k].score >= MAXRECORD) {
		(void) sprintf(buff, "NEVER %s", NOACCESS);
		motif_print(record, buff);
	} else {
		(void) sprintf(buff, "%d %s",
			  octRecord[i][j][k].score, octRecord[i][j][k].name);
		motif_print(record, buff);
	}
}

static int
HandleSolved(int counter, int size, int mode, Boolean orient, Boolean sticky)
{
	int         i = mode - PERIOD3;
	int         j = (orient) ? 1 : 0;
	int         k = (sticky) ? MAXOCTAS - MINOCTAS + 1 : size - MINOCTAS;

	if ((sticky || size <= MAXOCTAS) && counter < octRecord[i][j][k].score) {
		octRecord[i][j][k].score = counter;
		(void) strcpy(octRecord[i][j][k].name, usernameDsp);
		if ((size < 2 && mode != PERIOD4) || (size < 4 && mode == PERIOD4) ||
		    (orient && (counter < octRecord[i][!j][k].score))) {
			octRecord[i][!j][k].score = counter;
			(void) strcpy(octRecord[i][!j][k].name, usernameDsp);
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
			for (i = 0; i <= MAXOCTAS - MINOCTAS + 1; i++) {
				octRecord[mode][orient][i].score = MAXRECORD;
				(void) strcpy(octRecord[mode][orient][i].name, NOACCESS);
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
				for (i = 0; i <= MAXOCTAS - MINOCTAS + 1; i++) {
					(void) fscanf(fp, "%d %s\n", &n, username);
					if (n <= octRecord[mode][orient][i].score) {
						octRecord[mode][orient][i].score = n;
						(void) strcpy(octRecord[mode][orient][i].name, username);
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
				for (i = 0; i <= MAXOCTAS - MINOCTAS + 1; i++)
					(void) fprintf(fp, "%d %s\n",
						       octRecord[mode][orient][i].score, octRecord[mode][orient][i].name);
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
		XtError("motif_printf() requires a Label Widget");
	xmstr = XmStringCreateLtoR(text, XmSTRING_DEFAULT_CHARSET);
	XtSetArg(wargs[0], XmNlabelString, xmstr);
	XtSetValues(w, wargs, 1);
}
