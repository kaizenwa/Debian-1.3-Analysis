
/*-
# MOTIF-BASED CUBES
#
#  xmcubes.c
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
  Version 2: 92/12/19 XView
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
#include "Cubes.h"
#include "cubes.xbm"
#include "mouse-l.xbm"
#include "mouse-r.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/cubes.scores"
#endif

/* The following is in CubesP.h also */
#define MINCUBES 1

#define MAXCUBES 8
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
static void CallbackCubes(Widget w, caddr_t clientData, cubesCallbackStruct * callData);

static void PrintRecord(int sizeX, int sizeY, int sizeZ);
static int  HandleSolved(int counter, int sizeX, int sizeY, int sizeZ);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static void motif_print(Widget w, char *text);
static void BlockXSlider(Widget w, XtPointer clientData,
			 XmScaleCallbackStruct * cbs);
static void BlockYSlider(Widget w, XtPointer clientData,
			 XmScaleCallbackStruct * cbs);
static void BlockZSlider(Widget w, XtPointer clientData,
			 XmScaleCallbackStruct * cbs);

static Arg  arg[2];
static Widget moves, record, message, cubes, blockX, blockY, blockZ;
static GameRecord cubesRecord[MAXCUBES - MINCUBES + 1][MAXCUBES - MINCUBES + 1]
[MAXCUBES - MINCUBES + 1];
static int  movesDsp = 0;
static char messageDsp[128] = "Welcome";
static char usernameDsp[USERNAMELEN] = "";
static char buff[256];

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmcubes\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
	 "\t[-{border|bd} {color}] [-brick {color}] [-size{x|y|z} {int}]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-fg", "*cubes.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*cubes.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*cubes.brickBorder", XrmoptionSepArg, NULL},
	{"-border", "*cubes.brickBorder", XrmoptionSepArg, NULL},
	{"-brick", "*cubes.brickColor", XrmoptionSepArg, NULL},
	{"-sizex", "*cubes.sizeX", XrmoptionSepArg, NULL},
	{"-sizey", "*cubes.sizeY", XrmoptionSepArg, NULL},
	{"-sizez", "*cubes.sizeZ", XrmoptionSepArg, NULL},
	{"-username", "*cubes.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel;
	Widget      panel, panel2, rowcol, rowcol2;
	Pixmap      mouseLeftCursor, mouseRightCursor;
	Pixel       fg, bg;

	toplevel = XtInitialize(argv[0], "Cubes",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) cubes_bits, cubes_width, cubes_height));
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
	    XtVaTypedArg, XmNlabelString, XmRString, "Move brick", 11, NULL);
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
	blockX = XtVaCreateManagedWidget("blockX", xmScaleWidgetClass, rowcol2,
		      XtVaTypedArg, XmNtitleString, XmRString, "Blocks X", 8,
					 XmNminimum, MINCUBES,
					 XmNmaximum, MAXCUBES,
					 XmNvalue, MINCUBES,
					 XmNshowValue, True,
					 XmNorientation, XmHORIZONTAL,
					 NULL);
	XtAddCallback(blockX, XmNvalueChangedCallback, (XtCallbackProc) BlockXSlider,
		      (XtPointer) NULL);
	blockY = XtVaCreateManagedWidget("blockY", xmScaleWidgetClass, rowcol2,
		      XtVaTypedArg, XmNtitleString, XmRString, "Blocks Y", 8,
					 XmNminimum, MINCUBES,
					 XmNmaximum, MAXCUBES,
					 XmNvalue, MINCUBES,
					 XmNshowValue, True,
					 XmNorientation, XmHORIZONTAL,
					 NULL);
	XtAddCallback(blockY, XmNvalueChangedCallback, (XtCallbackProc) BlockYSlider,
		      (XtPointer) NULL);
	blockZ = XtVaCreateManagedWidget("blockZ", xmScaleWidgetClass, rowcol2,
		      XtVaTypedArg, XmNtitleString, XmRString, "Blocks Z", 8,
					 XmNminimum, MINCUBES,
					 XmNmaximum, MAXCUBES,
					 XmNvalue, MINCUBES,
					 XmNshowValue, True,
					 XmNorientation, XmHORIZONTAL,
					 NULL);
	XtAddCallback(blockZ, XmNvalueChangedCallback, (XtCallbackProc) BlockZSlider,
		      (XtPointer) NULL);
	message = XtVaCreateManagedWidget("Play Cubes! (use mouse or keypad)",
					  xmLabelWidgetClass, rowcol2,
					  NULL);

	cubes = XtCreateManagedWidget("cubes", cubesWidgetClass, panel, NULL,
				      0);
	XtAddCallback(cubes, XtNselectCallback, (XtCallbackProc) CallbackCubes,
		      (XtPointer) NULL);
	Initialize(cubes);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(cubes), AnyButton, AnyModifier, XtWindow(cubes),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(cubes),
		    XCreateFontCursor(XtDisplay(cubes), XC_crosshair));
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
	int         sizeX, sizeY, sizeZ;
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNsizeX, &sizeX,
		      XtNsizeY, &sizeY,
		      XtNsizeZ, &sizeZ,
		      NULL);
	if (sizeX <= MAXCUBES)
		XmScaleSetValue(blockX, sizeX);
	if (sizeY <= MAXCUBES)
		XmScaleSetValue(blockY, sizeY);
	if (sizeZ <= MAXCUBES)
		XmScaleSetValue(blockZ, sizeZ);
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
	PrintRecord(sizeX, sizeY, sizeZ);
}

static void
CallbackCubes(Widget w, caddr_t clientData, cubesCallbackStruct * callData)
{
	int         sizeX, sizeY, sizeZ;

	XtVaGetValues(w,
		      XtNsizeX, &sizeX,
		      XtNsizeY, &sizeY,
		      XtNsizeZ, &sizeZ,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case CUBES_RESTORE:
		case CUBES_RESET:
			movesDsp = 0;
			break;
		case CUBES_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case CUBES_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case CUBES_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case CUBES_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_SOLVED:
			if (HandleSolved(movesDsp, sizeX, sizeY, sizeZ))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_DEC_X:
			movesDsp = 0;
			sizeX--;
			PrintRecord(sizeX, sizeY, sizeZ);
			XtSetArg(arg[0], XtNsizeX, sizeX);
			XtSetValues(w, arg, 1);
			if (sizeX <= MAXCUBES)
				XmScaleSetValue(blockX, sizeX);
			break;
		case CUBES_INC_X:
			movesDsp = 0;
			sizeX++;
			PrintRecord(sizeX, sizeY, sizeZ);
			XtSetArg(arg[0], XtNsizeX, sizeX);
			XtSetValues(w, arg, 1);
			if (sizeX <= MAXCUBES)
				XmScaleSetValue(blockX, sizeX);
			break;
		case CUBES_DEC_Y:
			movesDsp = 0;
			sizeY--;
			PrintRecord(sizeX, sizeY, sizeZ);
			XtSetArg(arg[0], XtNsizeY, sizeY);
			XtSetValues(w, arg, 1);
			if (sizeY <= MAXCUBES)
				XmScaleSetValue(blockY, sizeY);
			break;
		case CUBES_INC_Y:
			movesDsp = 0;
			sizeY++;
			PrintRecord(sizeX, sizeY, sizeZ);
			XtSetArg(arg[0], XtNsizeY, sizeY);
			XtSetValues(w, arg, 1);
			if (sizeY <= MAXCUBES)
				XmScaleSetValue(blockY, sizeY);
			break;
		case CUBES_DEC_Z:
			movesDsp = 0;
			sizeZ--;
			PrintRecord(sizeX, sizeY, sizeZ);
			XtSetArg(arg[0], XtNsizeZ, sizeZ);
			XtSetValues(w, arg, 1);
			if (sizeZ <= MAXCUBES)
				XmScaleSetValue(blockZ, sizeZ);
			break;
		case CUBES_INC_Z:
			movesDsp = 0;
			sizeZ++;
			PrintRecord(sizeX, sizeY, sizeZ);
			XtSetArg(arg[0], XtNsizeZ, sizeZ);
			XtSetValues(w, arg, 1);
			if (sizeZ <= MAXCUBES)
				XmScaleSetValue(blockZ, sizeZ);
			break;
		case CUBES_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_UNDO:
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
BlockXSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         sizeX = cbs->value, sizeY, sizeZ, oldX;

	XtVaGetValues(cubes,
		      XtNsizeX, &oldX,
		      XtNsizeY, &sizeY,
		      XtNsizeZ, &sizeZ,
		      NULL);
	if (oldX != sizeX) {
		XtVaSetValues(cubes,
			      XtNsizeX, sizeX,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(sizeX, sizeY, sizeZ);
	}
}

static void
BlockYSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         sizeX, sizeY = cbs->value, sizeZ, oldY;

	XtVaGetValues(cubes,
		      XtNsizeX, &sizeX,
		      XtNsizeY, &oldY,
		      XtNsizeZ, &sizeZ,
		      NULL);
	if (oldY != sizeY) {
		XtVaSetValues(cubes,
			      XtNsizeY, sizeY,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(sizeX, sizeY, sizeZ);
	}
}

static void
BlockZSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         sizeX, sizeY, sizeZ = cbs->value, oldZ;

	XtVaGetValues(cubes,
		      XtNsizeX, &sizeX,
		      XtNsizeY, &sizeY,
		      XtNsizeZ, &oldZ,
		      NULL);
	if (oldZ != sizeZ) {
		XtVaSetValues(cubes,
			      XtNsizeZ, sizeZ,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(sizeX, sizeY, sizeZ);
	}
}

static void
PrintRecord(int sizeX, int sizeY, int sizeZ)
{
	int         i = sizeX - MINCUBES, j = sizeY - MINCUBES, k = sizeZ - MINCUBES;

	if (sizeX > MAXCUBES || sizeY > MAXCUBES || sizeZ > MAXCUBES)
		motif_print(record, "NOT RECORDED");
	else if (cubesRecord[i][j][k].score >= MAXRECORD) {
		(void) sprintf(buff, "NEVER %s", NOACCESS);
		motif_print(record, buff);
	} else {
		(void) sprintf(buff, "%d %s",
		      cubesRecord[i][j][k].score, cubesRecord[i][j][k].name);
		motif_print(record, buff);
	}
}

static int
HandleSolved(int counter, int sizeX, int sizeY, int sizeZ)
{
	int         i = sizeX - MINCUBES, j = sizeY - MINCUBES, k = sizeZ - MINCUBES;

	if (sizeX <= MAXCUBES && sizeY <= MAXCUBES && sizeZ <= MAXCUBES &&
	    counter < cubesRecord[i][j][k].score) {
		cubesRecord[i][j][k].score = cubesRecord[i][k][j].score =
			cubesRecord[j][i][k].score = cubesRecord[j][k][i].score =
			cubesRecord[k][i][j].score = cubesRecord[k][j][i].score =
			counter;
		(void) strcpy(cubesRecord[i][j][k].name, usernameDsp);
		(void) strcpy(cubesRecord[i][k][j].name, usernameDsp);
		(void) strcpy(cubesRecord[j][i][k].name, usernameDsp);
		(void) strcpy(cubesRecord[j][k][i].name, usernameDsp);
		(void) strcpy(cubesRecord[k][i][j].name, usernameDsp);
		(void) strcpy(cubesRecord[k][j][i].name, usernameDsp);
		WriteRecords();
		PrintRecord(sizeX, sizeY, sizeZ);
		return TRUE;
	}
	return FALSE;
}

static void
InitRecords(void)
{
	int         i, j, k;

	for (i = 0; i < MAXCUBES - MINCUBES + 1; i++)
		for (j = i; j < MAXCUBES - MINCUBES + 1; j++)
			for (k = j; k < MAXCUBES - MINCUBES + 1; k++) {
				cubesRecord[k][j][i].score = cubesRecord[k][i][j].score =
					cubesRecord[j][k][i].score = cubesRecord[j][i][k].score =
					cubesRecord[i][k][j].score = cubesRecord[i][j][k].score = MAXRECORD;
				(void) strcpy(cubesRecord[k][j][i].name, NOACCESS);
				(void) strcpy(cubesRecord[k][i][j].name, NOACCESS);
				(void) strcpy(cubesRecord[j][k][i].name, NOACCESS);
				(void) strcpy(cubesRecord[j][i][k].name, NOACCESS);
				(void) strcpy(cubesRecord[i][k][j].name, NOACCESS);
				(void) strcpy(cubesRecord[i][j][k].name, NOACCESS);
			}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, j, k, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL) {
		(void) sprintf(buff, "Can not open %s, taking defaults.", SCOREFILE);
		motif_print(message, buff);
	} else {
		for (i = 0; i < MAXCUBES - MINCUBES + 1; i++)
			for (j = i; j < MAXCUBES - MINCUBES + 1; j++)
				for (k = j; k < MAXCUBES - MINCUBES + 1; k++) {
					(void) fscanf(fp, "%d %s\n", &n, username);
					if (n <= cubesRecord[i][j][k].score) {
						cubesRecord[k][j][i].score = cubesRecord[k][i][j].score =
							cubesRecord[j][k][i].score = cubesRecord[j][i][k].score =
							cubesRecord[i][k][j].score = cubesRecord[i][j][k].score = n;
						(void) strcpy(cubesRecord[k][j][i].name, username);
						(void) strcpy(cubesRecord[k][i][j].name, username);
						(void) strcpy(cubesRecord[j][k][i].name, username);
						(void) strcpy(cubesRecord[j][i][k].name, username);
						(void) strcpy(cubesRecord[i][k][j].name, username);
						(void) strcpy(cubesRecord[i][j][k].name, username);
					}
				}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, j, k;

	ReadRecords();		/* Maybe its been updated by another */
	if ((fp = fopen(SCOREFILE, "w")) == NULL) {
		(void) sprintf(buff, "Can not write to %s.\n", SCOREFILE);
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
		for (i = 0; i < MAXCUBES - MINCUBES + 1; i++) {
			for (j = i; j < MAXCUBES - MINCUBES + 1; j++) {
				for (k = j; k < MAXCUBES - MINCUBES + 1; k++)
					(void) fprintf(fp, "%d %s\n",
						       cubesRecord[i][j][k].score, cubesRecord[i][j][k].name);
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
