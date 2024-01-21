
/*-
# MOTIF-BASED MISSING LINK(tm)
#
#  xmmlink.c
#
###
#
#  Copyright (c) 1994 - 97	David Albert Bagley, bagleyd@bigfoot.com
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
  Version 5: 95/10/01 Xt/Motif
  Version 1: 94/08/30 Xt
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
#include "Mlink.h"
#include "mlink.xbm"
#include "mouse-l.xbm"
#include "mouse-r.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/mlink.scores"
#endif

/* The following are in MlinkP.h also */
#define MINFACES 1
#define MAXFACES 8
#define MINTILES 1

#define MAXTILES 8
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
static void CallbackMlink(Widget w, caddr_t clientData,
			  mlinkCallbackStruct * callData);

static void PrintRecord(int tiles, int faces, Boolean orient, Boolean middle);
static int  HandleSolved(int counter, int tiles, int faces, Boolean orient,
			 Boolean middle);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static void motif_print(Widget w, char *text);
static void TileSlider(Widget w, XtPointer clientData,
		       XmScaleCallbackStruct * cbs);
static void FaceSlider(Widget w, XtPointer clientData,
		       XmScaleCallbackStruct * cbs);
static void OrientToggle(Widget w, XtPointer clientData,
			 XmToggleButtonCallbackStruct * cbs);
static void MiddleToggle(Widget w, XtPointer clientData,
			 XmToggleButtonCallbackStruct * cbs);

static Arg  arg[2];
static Widget moves, record, message, mlink, orientSwitch, middleSwitch,
            tile, face;
static GameRecord mlinkRecord[2][2][MAXFACES - MINFACES + 1]
[MAXTILES - MINTILES + 1];
static int  movesDsp = 0;
static char messageDsp[128] = "Welcome";
static char usernameDsp[USERNAMELEN] = "";
static char buff[256];

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmmlink\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		       "\t[-{border|bd} {color}] [-tile {color}]\n");
	(void) fprintf(stderr,
		       "\t[-face{0|1|2|3|4|5|6|7} {color}]\n");
	(void) fprintf(stderr,
	    "\t[-tiles {int}] [-faces {int}] [-[no]orient] [-[no]middle]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*mlink.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*mlink.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*mlink.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*mlink.tileBorder", XrmoptionSepArg, NULL},
	{"-border", "*mlink.tileBorder", XrmoptionSepArg, NULL},
	{"-tile", "*mlink.tileColor", XrmoptionSepArg, NULL},
	{"-face0", "*mlink.faceColor0", XrmoptionSepArg, NULL},
	{"-face1", "*mlink.faceColor1", XrmoptionSepArg, NULL},
	{"-face2", "*mlink.faceColor2", XrmoptionSepArg, NULL},
	{"-face3", "*mlink.faceColor3", XrmoptionSepArg, NULL},
	{"-face4", "*mlink.faceColor4", XrmoptionSepArg, NULL},
	{"-face5", "*mlink.faceColor5", XrmoptionSepArg, NULL},
	{"-face6", "*mlink.faceColor6", XrmoptionSepArg, NULL},
	{"-face7", "*mlink.faceColor7", XrmoptionSepArg, NULL},
	{"-tiles", "*mlink.tiles", XrmoptionSepArg, NULL},
	{"-faces", "*mlink.faces", XrmoptionSepArg, NULL},
	{"-orient", "*mlink.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*mlink.orient", XrmoptionNoArg, "FALSE"},
	{"-middle", "*mlink.middle", XrmoptionNoArg, "TRUE"},
	{"-nomiddle", "*mlink.middle", XrmoptionNoArg, "FALSE"},
	{"-username", "*mlink.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel;
	Widget      panel, panel2, rowcol, rowcol2, rowcol3, rowcol4, rowcol5;
	Pixmap      mouseLeftCursor, mouseRightCursor;
	Pixel       fg, bg;

	toplevel = XtInitialize(argv[0], "Mlink",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) mlink_bits, mlink_width, mlink_height));
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
	tile = XtVaCreateManagedWidget("tile", xmScaleWidgetClass, rowcol2,
			 XtVaTypedArg, XmNtitleString, XmRString, "Tiles", 6,
				       XmNminimum, MINTILES,
				       XmNmaximum, MAXTILES,
				       XmNvalue, MINTILES,
				       XmNshowValue, True,
				       XmNorientation, XmHORIZONTAL,
				       NULL);
	XtAddCallback(tile, XmNvalueChangedCallback, (XtCallbackProc) TileSlider,
		      (XtPointer) NULL);
	rowcol3 = XtVaCreateManagedWidget("Rowcol3", xmRowColumnWidgetClass, panel2,
					  NULL);
	face = XtVaCreateManagedWidget("face", xmScaleWidgetClass, rowcol3,
			 XtVaTypedArg, XmNtitleString, XmRString, "Faces", 6,
				       XmNminimum, MINFACES,
				       XmNmaximum, MAXFACES,
				       XmNvalue, MINFACES,
				       XmNshowValue, True,
				       XmNorientation, XmHORIZONTAL,
				       NULL);
	XtAddCallback(face, XmNvalueChangedCallback, (XtCallbackProc) FaceSlider,
		      (XtPointer) NULL);
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
	middleSwitch = XtVaCreateManagedWidget("Middle",
					  xmToggleButtonWidgetClass, rowcol4,
					       NULL);
	XtAddCallback(middleSwitch, XmNvalueChangedCallback,
		      (XtCallbackProc) MiddleToggle, (XtPointer) NULL);
	rowcol5 = XtVaCreateManagedWidget("Rowcol5", xmRowColumnWidgetClass, panel2,
					  NULL);
	message = XtVaCreateManagedWidget("Play Missing Link! (use mouse and keypad)",
					  xmLabelWidgetClass, rowcol5,
					  NULL);

	mlink = XtCreateManagedWidget("mlink", mlinkWidgetClass, panel,
				      NULL, 0);
	XtAddCallback(mlink, XtNselectCallback, (XtCallbackProc) CallbackMlink,
		      (XtPointer) NULL);
	Initialize(mlink);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(mlink), AnyButton, AnyModifier, XtWindow(mlink),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(mlink),
		    XCreateFontCursor(XtDisplay(mlink), XC_crosshair));
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
	int         tiles, faces;
	Boolean     orient, middle;
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNtiles, &tiles,
		      XtNfaces, &faces,
		      XtNorient, &orient,
		      XtNmiddle, &middle,
		      NULL);
	if (tiles <= MAXTILES)
		XmScaleSetValue(tile, tiles);
	XmScaleSetValue(face, faces);
	XmToggleButtonSetState(orientSwitch, orient, True);
	XmToggleButtonSetState(middleSwitch, middle, True);
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
	PrintRecord(tiles, faces, orient, middle);
}

static void
CallbackMlink(Widget w, caddr_t clientData, mlinkCallbackStruct * callData)
{
	int         tiles, faces;
	Boolean     orient, middle;

	XtVaGetValues(w,
		      XtNtiles, &tiles,
		      XtNfaces, &faces,
		      XtNorient, &orient,
		      XtNmiddle, &middle,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case MLINK_RESTORE:
		case MLINK_RESET:
			movesDsp = 0;
			break;
		case MLINK_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case MLINK_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case MLINK_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case MLINK_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_CONTROL:
			return;
		case MLINK_SOLVED:
			if (HandleSolved(movesDsp, tiles, faces, orient, middle))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(tiles, faces, orient, middle);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(orientSwitch, orient, True);
			break;
		case MLINK_MIDDLE:
			movesDsp = 0;
			middle = !middle;
			PrintRecord(tiles, faces, orient, middle);
			XtSetArg(arg[0], XtNmiddle, middle);
			XtSetValues(w, arg, 1);
			XmToggleButtonSetState(middleSwitch, middle, True);
			break;
		case MLINK_DEC_X:
			movesDsp = 0;
			tiles--;
			PrintRecord(tiles, faces, orient, middle);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			if (tiles <= MAXTILES)
				XmScaleSetValue(tile, tiles);
			break;
		case MLINK_INC_X:
			movesDsp = 0;
			tiles++;
			PrintRecord(tiles, faces, orient, middle);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			if (tiles <= MAXTILES)
				XmScaleSetValue(tile, tiles);
			break;
		case MLINK_DEC_Y:
			movesDsp = 0;
			faces--;
			PrintRecord(tiles, faces, orient, middle);
			XtSetArg(arg[0], XtNfaces, faces);
			XtSetValues(w, arg, 1);
			XmScaleSetValue(face, faces);
			break;
		case MLINK_INC_Y:
			movesDsp = 0;
			faces++;
			PrintRecord(tiles, faces, orient, middle);
			XtSetArg(arg[0], XtNfaces, faces);
			XtSetValues(w, arg, 1);
			XmScaleSetValue(face, faces);
			break;
		case MLINK_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_UNDO:
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
	int         tiles = cbs->value, faces, old;

	XtVaGetValues(mlink,
		      XtNtiles, &old,
		      XtNfaces, &faces,
		      NULL);
	if (old != tiles) {
		XtVaSetValues(mlink,
			      XtNtiles, tiles,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(tiles, faces, XmToggleButtonGetState(orientSwitch),
			    XmToggleButtonGetState(middleSwitch));
	}
}

static void
FaceSlider(Widget w, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
	int         tiles, faces = cbs->value, old;

	XtVaGetValues(mlink,
		      XtNtiles, &tiles,
		      XtNfaces, &old,
		      NULL);
	if (old != faces) {
		XtVaSetValues(mlink,
			      XtNfaces, faces,
			      NULL);
		movesDsp = 0;
		(void) sprintf(buff, "%d", movesDsp);
		motif_print(moves, buff);
		PrintRecord(tiles, faces, XmToggleButtonGetState(orientSwitch),
			    XmToggleButtonGetState(middleSwitch));
	}
}

static void
OrientToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         tiles, faces;
	Boolean     orient = cbs->set;

	XtVaSetValues(mlink,
		      XtNorient, orient,
		      NULL);
	XtVaGetValues(mlink,
		      XtNtiles, &tiles,
		      XtNfaces, &faces,
		      NULL);
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	PrintRecord(tiles, faces, orient, XmToggleButtonGetState(middleSwitch));
}

static void
MiddleToggle(Widget w, XtPointer clientData, XmToggleButtonCallbackStruct * cbs)
{
	int         tiles, faces;
	Boolean     middle = cbs->set;

	XtVaSetValues(mlink,
		      XtNmiddle, middle,
		      NULL);
	XtVaGetValues(mlink,
		      XtNtiles, &tiles,
		      XtNfaces, &faces,
		      NULL);
	movesDsp = 0;
	(void) sprintf(buff, "%d", movesDsp);
	motif_print(moves, buff);
	PrintRecord(tiles, faces, XmToggleButtonGetState(orientSwitch), middle);
}

static void
PrintRecord(int tiles, int faces, Boolean orient, Boolean middle)
{
	int         i = tiles - MINTILES, j = faces - MINFACES;
	int         k = (orient) ? 1 : 0, l = (middle) ? 1 : 0;

	if (tiles > MAXTILES)
		motif_print(record, "NOT RECORDED");
	else if (mlinkRecord[l][k][j][i].score >= MAXRECORD) {
		(void) sprintf(buff, "NEVER %s", NOACCESS);
		motif_print(record, buff);
	} else {
		(void) sprintf(buff, "%d %s",
		mlinkRecord[l][k][j][i].score, mlinkRecord[l][k][j][i].name);
		motif_print(record, buff);
	}
}

static int
HandleSolved(int counter, int tiles, int faces, Boolean orient, Boolean middle)
{
	int         i = tiles - MINTILES, j = faces - MINFACES;
	int         k = (orient) ? 1 : 0, l = (middle) ? 1 : 0;

	if (tiles <= MAXTILES && counter < mlinkRecord[l][k][j][i].score) {
		mlinkRecord[l][k][j][i].score = counter;
		(void) strcpy(mlinkRecord[l][k][j][i].name, usernameDsp);
		if (tiles < 4 || faces < 2) {
			mlinkRecord[!l][k][j][i].score = counter;
			(void) strcpy(mlinkRecord[!l][k][j][i].name, usernameDsp);
		}
		WriteRecords();
		PrintRecord(tiles, faces, orient, middle);
		return TRUE;
	}
	return FALSE;
}

static void
InitRecords(void)
{
	int         i, j, k, l;

	for (l = 0; l < 2; l++)
		for (k = 0; k < 2; k++)
			for (j = 0; j < MAXFACES - MINFACES + 1; j++)
				for (i = 0; i < MAXTILES - MINTILES + 1; i++) {
					mlinkRecord[l][k][j][i].score = MAXRECORD;
					(void) strcpy(mlinkRecord[l][k][j][i].name, NOACCESS);
				}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, j, k, l, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL) {
		(void) sprintf(buff, "Can not open %s, taking defaults.", SCOREFILE);
		motif_print(message, buff);
	} else {
		for (l = 0; l < 2; l++)
			for (k = 0; k < 2; k++)
				for (j = 0; j < MAXFACES - MINFACES + 1; j++)
					for (i = 0; i < MAXTILES - MINTILES + 1; i++) {
						(void) fscanf(fp, "%d %s\n", &n, username);
						if (n <= mlinkRecord[l][k][j][i].score) {
							mlinkRecord[l][k][j][i].score = n;
							(void) strcpy(mlinkRecord[l][k][j][i].name, username);
						}
					}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, j, k, l;

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
		for (l = 0; l < 2; l++) {
			for (k = 0; k < 2; k++) {
				for (j = 0; j < MAXFACES - MINFACES + 1; j++) {
					for (i = 0; i < MAXTILES - MINTILES + 1; i++)
						(void) fprintf(fp, "%d %s\n",
							       mlinkRecord[l][k][j][i].score, mlinkRecord[l][k][j][i].name);
					(void) fprintf(fp, "\n");
				}
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
