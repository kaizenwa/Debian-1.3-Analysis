/*-
# MOTIF-BASED PANEX(tm)
#
#  xmpanex.c
#
###
#
#  Copyright (c) 1996 - 97		David Albert Bagley, bagleyd@bigfoot.com
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
  Version 5.4: 97/01/01 Xt/Motif
  Version 5.3: 96/04/30 Xt/Motif
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
#include "Panex.h"
#include "panex.xbm"
#include "mouse-l.xbm"
#include "mouse-r.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/panex.scores"
#endif

/* The following are in PanexP.h also */
#define MINTILES 1
#define HANOI 0
#define PANEX 1
#define MAXMODES 2

#define MAXTILES 10
#define MAXRECORD 65536
#define FILENAMELEN 1024
#define USERNAMELEN 128
#define NOACCESS "noaccess"
#define NOBODY "nobody"

typedef struct {
	int         score;
	char        name[USERNAMELEN];
} GameRecord;

static void Initialize(Widget w);
static void CallbackPanex(Widget w,
			  caddr_t clientData, panexCallbackStruct * callData);

static void PrintRecord(int tiles, int mode);
static int  HandleSolved(int counter, int tiles, int mode);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static void motif_print(Widget w, char *text);
static void TileSlider(Widget w,
		       XtPointer clientData, XmScaleCallbackStruct * cbs);
static void ModeToggle(Widget w,
		       int mode, XmToggleButtonCallbackStruct * cbs);

static Arg  arg[2];
static Widget moves, record, message, panex, modes[MAXMODES], tile;
static GameRecord panexRecord[MAXMODES][MAXTILES - MINTILES + 1];
static int  movesDsp = 0;
static char messageDsp[128] = "Welcome";
static char usernameDsp[USERNAMELEN] = "";
static char buff[256];

static char *modeString[] =
{
	"Hanoi", "Panex"
};

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmpanex\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
	"\t[-{border|bd} {color}] [-tile {color}] [-pyramid{0|1} {color}]\n");
	(void) fprintf(stderr,
	    "\t[-tiles {int}] [-{mode {int}|hanoi|panex}] [-delay msecs]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*panex.mono", XrmoptionNoArg, "1"},
	{"-fg", "*panex.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*panex.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*panex.tileBorder", XrmoptionSepArg, NULL},
	{"-border", "*panex.tileBorder", XrmoptionSepArg, NULL},
	{"-tile", "*panex.tileColor", XrmoptionSepArg, NULL},
	{"-pyramid0", "*panex.pyramidColor0", XrmoptionSepArg, NULL},
	{"-pyramid1", "*panex.pyramidColor1", XrmoptionSepArg, NULL},
	{"-tiles", "*panex.tiles", XrmoptionSepArg, NULL},
	{"-mode", "*panex.mode", XrmoptionSepArg, NULL},
	{"-hanoi", "*panex.mode", XrmoptionNoArg, "0"},
	{"-panex", "*panex.mode", XrmoptionNoArg, "1"},
	{"-username", "*userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel;
	Widget      panel, panel2, rowcol, rowcol2, rowcol3;
	Pixmap      mouseLeftCursor, mouseRightCursor;
	Pixel       fg, bg;
	int         i;

	toplevel = XtInitialize(argv[0], "Panex",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) panex_bits, panex_width, panex_height));
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
		  XtVaTypedArg, XmNlabelString, XmRString, "Reset", 6, NULL);
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
	tile = XtVaCreateManagedWidget("tile", xmScaleWidgetClass, rowcol2,
			 XtVaTypedArg, XmNtitleString, XmRString, "Tiles", 6,
				       XmNminimum, MINTILES,
				       XmNmaximum, MAXTILES,
				       XmNvalue, MAXTILES,
				       XmNshowValue, True,
				       XmNorientation, XmHORIZONTAL,
				       NULL);
	XtAddCallback(tile, XmNvalueChangedCallback, (XtCallbackProc) TileSlider,
		      (XtPointer) NULL);
	rowcol3 = XtVaCreateManagedWidget("Rowcol3", xmRowColumnWidgetClass, rowcol2,
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
		XtAddCallback(modes[i], XmNvalueChangedCallback, (XtCallbackProc) ModeToggle,
			      (XtPointer) i);
	}
	message = XtVaCreateManagedWidget("Play Panex!",
					  xmLabelWidgetClass, rowcol2,
					  NULL);

	panex = XtCreateManagedWidget("panex", panexWidgetClass, panel,
				      NULL, 0);
	XtAddCallback(panex, XtNselectCallback, (XtCallbackProc) CallbackPanex,
		      (XtPointer) NULL);
	Initialize(panex);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(panex), AnyButton, AnyModifier, XtWindow(panex),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(panex),
		    XCreateFontCursor(XtDisplay(panex), XC_crosshair));
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
	int         tiles, mode;
	String      username;

	/*XtVaSetValues(w,
	   XtNstart, FALSE,
	   NULL); */
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNtiles, &tiles,
		      XtNmode, &mode,
		      NULL);
	if (tiles <= MAXTILES)
		XmScaleSetValue(tile, tiles);
	XmToggleButtonSetState(modes[mode - HANOI], True, False);
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
	PrintRecord(tiles, mode);
}

static void
CallbackPanex(Widget w, caddr_t clientData, panexCallbackStruct * callData)
{
	int         tiles, mode;

	XtVaGetValues(w,
		      XtNtiles, &tiles,
		      XtNmode, &mode,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case PANEX_RESTORE:
		case PANEX_RESET:
			movesDsp = 0;
			break;
		case PANEX_ILLEGAL:
			(void) strcpy(messageDsp, "Illegal move");
			break;
		case PANEX_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case PANEX_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case PANEX_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case PANEX_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_SOLVED:
			if (HandleSolved(movesDsp, tiles, mode))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_DEC:
			movesDsp = 0;
			tiles--;
			PrintRecord(tiles, mode);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			if (tiles <= MAXTILES)
				XmScaleSetValue(tile, tiles);
			break;
		case PANEX_INC:
			movesDsp = 0;
			tiles++;
			PrintRecord(tiles, mode);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			if (tiles <= MAXTILES)
				XmScaleSetValue(tile, tiles);
			break;
		case PANEX_MODE:
			movesDsp = 0;
			mode = (mode == PANEX) ? HANOI : PANEX;
			PrintRecord(tiles, mode);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(modes[mode - HANOI], True, True);
			break;
		case PANEX_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_UNDO:
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
TileSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         tiles = cbs->value, mode, old;

	XtVaGetValues(panex,
		      XtNtiles, &old,
		      XtNmode, &mode,
		      NULL);
	if (old != tiles) {
		XtVaSetValues(panex,
			      XtNtiles, tiles,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(tiles, mode);
	}
}

static void
ModeToggle(Widget w, int mode, XmToggleButtonCallbackStruct * cbs)
{
	int         tiles;

	if (cbs->set) {
		XtVaGetValues(panex,
			      XtNtiles, &tiles,
			      NULL);
		XtVaSetValues(panex,
			      XtNmode, mode + HANOI,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(tiles, mode + HANOI);
	}
}

static void
PrintRecord(int tiles, int mode)
{
	int         i = tiles - MINTILES, j = mode - HANOI;

	if (tiles > MAXTILES)
		motif_print(record, "NOT RECORDED");
	else if (panexRecord[j][i].score >= MAXRECORD) {
		(void) sprintf(buff, "NEVER %s", NOACCESS);
		motif_print(record, buff);
	} else {
		(void) sprintf(buff, "%d %s",
			    panexRecord[j][i].score, panexRecord[j][i].name);
		motif_print(record, buff);
	}
}

static int
HandleSolved(int counter, int tiles, int mode)
{
	int         i = tiles - MINTILES, j = mode - HANOI;

	if (tiles <= MAXTILES && counter < panexRecord[j][i].score) {
		panexRecord[j][i].score = counter;
		(void) strcpy(panexRecord[j][i].name, usernameDsp);
		WriteRecords();
		PrintRecord(tiles, mode);
		return TRUE;
	}
	return FALSE;
}

static void
InitRecords(void)
{
	int         i, mode;

	for (mode = 0; mode < MAXMODES; mode++)
		for (i = 0; i < MAXTILES - MINTILES + 1; i++) {
			panexRecord[mode][i].score = MAXRECORD;
			(void) strcpy(panexRecord[mode][i].name, NOACCESS);
		}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, mode, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL) {
		(void) sprintf(buff, "Can not open %s, taking defaults.", SCOREFILE);
		motif_print(message, buff);
	} else {
		for (mode = 0; mode < MAXMODES; mode++)
			for (i = 0; i < MAXTILES - MINTILES + 1; i++) {
				(void) fscanf(fp, "%d %s\n", &n, username);
				if (n <= panexRecord[mode][i].score) {
					panexRecord[mode][i].score = n;
					(void) strcpy(panexRecord[mode][i].name, username);
				}
			}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, mode;

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
			(void) fprintf(stderr, "Lock file exists... score not recorded - sorry.\n");
			return;
#endif
		}
#endif
		for (mode = 0; mode < MAXMODES; mode++) {
			for (i = 0; i < MAXTILES - MINTILES + 1; i++)
				(void) fprintf(fp, "%d %s\n",
					       panexRecord[mode][i].score, panexRecord[mode][i].name);
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
