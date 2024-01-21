
/*-
# MOTIF-BASED HEXAGONS
#
#  xmhexagons.c
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
  Version 5: 95/10/01 Xt/Motif
  Version 4: 94/06/07 Xt
  Version 3: 93/04/01 Motif
  Version 2: 92/01/07 XView
  Version 1: 91/09/05 SunView
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
#include "Hexagons.h"
#include "hexagons.xbm"
#include "mouse-l.xbm"
#include "mouse-r.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/hexagons.scores"
#endif

/* The following are in HexagonsP.h also */
#define MINHEXAGONS 1
#define MAXORIENT 2
#define NOCORN 0
#define CORNERS 1

#define MAXHEXAGONS 12
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
static void CallbackHexagons(Widget w, caddr_t clientData,
			     hexagonsCallbackStruct * callData);

static void PrintRecord(int size, Boolean corners);
static int  HandleSolved(int counter, int size, int corners);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static void motif_print(Widget w, char *text);
static void HexSlider(Widget w, XtPointer clientData,
		      XmScaleCallbackStruct * cbs);
static void CornersToggle(Widget w, XtPointer clientData,
			  XmToggleButtonCallbackStruct * cbs);

static Arg  arg[2];
static Widget moves, record, message, hexagons, cornersSwitch, hex;
static GameRecord hexagonsRecord[MAXORIENT][MAXHEXAGONS - MINHEXAGONS + 1];
static int  movesDsp = 0;
static char messageDsp[128] = "Welcome";
static char usernameDsp[USERNAMELEN] = "";
static char buff[256];

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmhexagons\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]][-fg {color}]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		 "\t[-{border|bd} {color}] [-tile {color}] [-size {int}]\n");
	(void) fprintf(stderr,
		       "\t[-[no]corners] [-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-fg", "*hexagons.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*hexagons.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*hexagons.tileBorder", XrmoptionSepArg, NULL},
	{"-border", "*hexagons.tileBorder", XrmoptionSepArg, NULL},
	{"-tile", "*hexagons.tileColor", XrmoptionSepArg, NULL},
	{"-border", "*hexagons.tileBorder", XrmoptionSepArg, NULL},
	{"-size", "*hexagons.size", XrmoptionSepArg, NULL},
	{"-corners", "*hexagons.corners", XrmoptionNoArg, "TRUE"},
	{"-nocorners", "*hexagons.corners", XrmoptionNoArg, "FALSE"},
	{"-username", "*hexagons.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel;
	Widget      panel, panel2, rowcol, rowcol2;
	Pixmap      mouseLeftCursor, mouseRightCursor;
	Pixel       fg, bg;

	toplevel = XtInitialize(argv[0], "Hexagons",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
		   (char *) hexagons_bits, hexagons_width, hexagons_height));
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
	     XtVaTypedArg, XmNlabelString, XmRString, "Move tile", 10, NULL);
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
					  NULL);
	XtVaGetValues(rowcol2, XmNforeground, &fg, XmNbackground, &bg, NULL);
	hex = XtVaCreateManagedWidget("hexs", xmScaleWidgetClass, rowcol2,
			  XtVaTypedArg, XmNtitleString, XmRString, "Hexs", 5,
				      XmNminimum, MINHEXAGONS,
				      XmNmaximum, MAXHEXAGONS,
				      XmNvalue, MINHEXAGONS,
				      XmNshowValue, True,
				      XmNorientation, XmHORIZONTAL,
				      NULL);
	XtAddCallback(hex, XmNvalueChangedCallback, (XtCallbackProc) HexSlider,
		      (XtPointer) NULL);
	cornersSwitch = XtVaCreateManagedWidget("Corners",
					  xmToggleButtonWidgetClass, rowcol2,
						NULL);
	XtAddCallback(cornersSwitch, XmNvalueChangedCallback,
		      (XtCallbackProc) CornersToggle, (XtPointer) NULL);
	message = XtVaCreateManagedWidget("Play Hexagons! (use mouse or keypad)",
					  xmLabelWidgetClass, rowcol2,
					  NULL);

	hexagons = XtCreateManagedWidget("hexagons", hexagonsWidgetClass, panel,
					 NULL, 0);
	XtAddCallback(hexagons, XtNselectCallback, (XtCallbackProc) CallbackHexagons,
		      (XtPointer) NULL);
	Initialize(hexagons);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(hexagons), AnyButton, AnyModifier, XtWindow(hexagons),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(hexagons),
		    XCreateFontCursor(XtDisplay(hexagons), XC_crosshair));
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
	int         size;
	Boolean     corners;
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNsize, &size,
		      XtNcorners, &corners,
		      NULL);
	if (size <= MAXHEXAGONS)
		XmScaleSetValue(hex, size);
	XmToggleButtonSetState(cornersSwitch, corners, True);
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
	PrintRecord(size, corners);
}

static void
CallbackHexagons(Widget w, caddr_t clientData,
		 hexagonsCallbackStruct * callData)
{
	int         size;
	Boolean     corners;

	XtVaGetValues(w,
		      XtNsize, &size,
		      XtNcorners, &corners,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case HEXAGONS_RESTORE:
		case HEXAGONS_RESET:
			movesDsp = 0;
			break;
		case HEXAGONS_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case HEXAGONS_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case HEXAGONS_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case HEXAGONS_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_SOLVED:
			if (HandleSolved(movesDsp, size, corners))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_DEC:
			movesDsp = 0;
			size--;
			PrintRecord(size, corners);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			if (size <= MAXHEXAGONS)
				XmScaleSetValue(hex, size);
			break;
		case HEXAGONS_INC:
			movesDsp = 0;
			size++;
			PrintRecord(size, corners);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			if (size <= MAXHEXAGONS)
				XmScaleSetValue(hex, size);
			break;
		case HEXAGONS_CORNERS:
			movesDsp = 0;
			corners = !corners;
			PrintRecord(size, corners);
			XtSetArg(arg[0], XtNcorners, corners);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(cornersSwitch, corners, True);
			break;
		case HEXAGONS_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_UNDO:
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
HexSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         size = cbs->value, old;

	XtVaGetValues(hexagons,
		      XtNsize, &old,
		      NULL);
	if (old != size) {
		XtVaSetValues(hexagons,
			      XtNsize, size,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(size, XmToggleButtonGetState(cornersSwitch));
	}
}

static void
CornersToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         size;
	Boolean     corners = cbs->set;

	XtVaSetValues(hexagons,
		      XtNcorners, corners,
		      NULL);
	XtVaGetValues(hexagons,
		      XtNsize, &size,
		      NULL);
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	PrintRecord(size, corners);
}

static void
PrintRecord(int size, Boolean corners)
{
	int         i = (corners) ? 1 : 0, j = size - MINHEXAGONS;

	if (size > MAXHEXAGONS)
		motif_print(record, "NOT RECORDED");
	if (hexagonsRecord[i][j].score >= MAXRECORD) {
		(void) sprintf(buff, "NEVER %s", NOACCESS);
		motif_print(record, buff);
	} else {
		(void) sprintf(buff, "%d %s",
		      hexagonsRecord[i][j].score, hexagonsRecord[i][j].name);
		motif_print(record, buff);
	}
}


static int
HandleSolved(int counter, int size, int corners)
{
	int         i = (corners) ? 1 : 0, j = size - MINHEXAGONS;

	if (size <= MAXHEXAGONS && counter < hexagonsRecord[i][j].score) {
		hexagonsRecord[i][j].score = counter;
		(void) strcpy(hexagonsRecord[i][j].name, usernameDsp);
		WriteRecords();
		PrintRecord(size, corners);
		return TRUE;
	}
	return FALSE;
}

static void
InitRecords(void)
{
	int         i, j;

	for (i = 0; i < MAXORIENT; i++)
		for (j = 0; j < MAXHEXAGONS - MINHEXAGONS + 1; j++) {
			hexagonsRecord[i][j].score = MAXRECORD;
			(void) strcpy(hexagonsRecord[i][j].name, NOACCESS);
		}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, j, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL) {
		(void) sprintf(buff, "Can not open %s, taking defaults.", SCOREFILE);
		motif_print(message, buff);
	} else {
		for (i = 0; i < MAXORIENT; i++)
			for (j = 0; j < MAXHEXAGONS - MINHEXAGONS + 1; j++) {
				(void) fscanf(fp, "%d %s\n", &n, username);
				if (n <= hexagonsRecord[i][j].score) {
					hexagonsRecord[i][j].score = n;
					(void) strcpy(hexagonsRecord[i][j].name, username);
				}
			}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, j;

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
		for (i = 0; i < MAXORIENT; i++) {
			for (j = 0; j < MAXHEXAGONS - MINHEXAGONS + 1; j++)
				(void) fprintf(fp, "%d %s\n",
					       hexagonsRecord[i][j].score, hexagonsRecord[i][j].name);
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
