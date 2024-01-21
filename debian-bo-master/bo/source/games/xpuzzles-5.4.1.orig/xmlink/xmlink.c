
/*-
# X-BASED MISSING LINK(tm)
#
#  xmlink.c
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
#include "Mlink.h"
#include "mlink.xbm"

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

static void PrintRecord(int tiles, int faces, Boolean orient, Boolean middle,
			char *record);
static int  HandleSolved(int counter,
		       int tiles, int faces, Boolean orient, Boolean middle);
static void PrintState(Widget w,
		 char *prog, int tiles, int faces, Boolean middle, int moves,
		       char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Arg  arg[2];
static GameRecord mlinkRecord[2][2][MAXFACES - MINFACES + 1]
[MAXTILES - MINTILES + 1];
static int  movesDsp = 0;
static char progDsp[64] = "xmlink";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmlink\n");
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
	Widget      toplevel, mlink;

	toplevel = XtInitialize(argv[0], "Mlink",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) mlink_bits, mlink_width, mlink_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	mlink = XtCreateManagedWidget("mlink", mlinkWidgetClass, toplevel,
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
	PrintRecord(tiles, faces, orient, middle, recordDsp);
	PrintState(XtParent(w), progDsp, tiles, faces, middle, movesDsp,
		   recordDsp, messageDsp);
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
			PrintRecord(tiles, faces, orient, middle, recordDsp);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_MIDDLE:
			movesDsp = 0;
			middle = !middle;
			PrintRecord(tiles, faces, orient, middle, recordDsp);
			XtSetArg(arg[0], XtNmiddle, middle);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_DEC_X:
			movesDsp = 0;
			tiles--;
			PrintRecord(tiles, faces, orient, middle, recordDsp);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_INC_X:
			movesDsp = 0;
			tiles++;
			PrintRecord(tiles, faces, orient, middle, recordDsp);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_DEC_Y:
			movesDsp = 0;
			faces--;
			PrintRecord(tiles, faces, orient, middle, recordDsp);
			XtSetArg(arg[0], XtNfaces, faces);
			XtSetValues(w, arg, 1);
			break;
		case MLINK_INC_Y:
			movesDsp = 0;
			faces++;
			PrintRecord(tiles, faces, orient, middle, recordDsp);
			XtSetArg(arg[0], XtNfaces, faces);
			XtSetValues(w, arg, 1);
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
	PrintState(XtParent(w), progDsp, tiles, faces, middle, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int tiles, int faces, Boolean orient, Boolean middle, char *record)
{
	int         i = tiles - MINTILES, j = faces - MINFACES;
	int         k = (orient) ? 1 : 0, l = (middle) ? 1 : 0;

	if (tiles > MAXTILES)
		(void) strcpy(record, "NOT RECORDED");
	else if (mlinkRecord[l][k][j][i].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
		mlinkRecord[l][k][j][i].score, mlinkRecord[l][k][j][i].name);
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
		PrintRecord(tiles, faces, orient, middle, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int tiles, int faces, Boolean middle, int moves, char *record, char *message)
{
	if (middle)
		(void) sprintf(titleDsp, "%s: %dx%d norm@ (%d/%s) - %s",
			       prog, tiles, faces, moves, record, message);
	else
		(void) sprintf(titleDsp, "%s: %dx%d ind@ (%d/%s) - %s",
			       prog, tiles, faces, moves, record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
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

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
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
	if ((fp = fopen(SCOREFILE, "w")) == NULL)
		(void) sprintf(messageDsp, "Can not write to %s.", SCOREFILE);
	else {
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
