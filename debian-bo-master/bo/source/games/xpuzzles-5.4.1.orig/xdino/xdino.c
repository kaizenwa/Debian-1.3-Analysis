
/*-
# X-BASED DINOSAUR CUBE
#
#  xdino.c
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
#include "Dino.h"
#include "Dino2d.h"
#include "Dino3d.h"
#include "dino.xbm"

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
static void CallbackDino(Widget w, caddr_t clientData,
			 dinoCallbackStruct * callData);

static void PrintRecord(int mode, Boolean orient, Boolean practice,
			char *record);
static int  HandleSolved(int counter, int mode, Boolean orient);
static void PrintState(Widget w,
		       char *prog, int dim, int mode, int moves,
		       char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Widget dino2d, dino3d;
static Arg  arg[5];
static GameRecord dinoRecord[MAXMODES][2];
static int  movesDsp = 0;
static char progDsp[64] = "xdino";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xdino\n");
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
	Widget      toplevel, shell;

	toplevel = XtInitialize(argv[0], "Dino",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	shell = XtCreateApplicationShell(argv[0], topLevelShellWidgetClass, NULL,
					 0);
	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			       (char *) dino_bits, dino_width, dino_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(shell),
				       RootWindowOfScreen(XtScreen(shell)),
			       (char *) dino_bits, dino_width, dino_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(shell, arg, 2);
	dino2d = XtCreateManagedWidget("dino", dino2dWidgetClass, toplevel,
				       NULL, 0);
	XtAddCallback(dino2d, XtNselectCallback, (XtCallbackProc) CallbackDino,
		      (XtPointer) NULL);
	dino3d = XtCreateManagedWidget("dino", dino3dWidgetClass, shell,
				       NULL, 0);
	XtAddCallback(dino3d, XtNselectCallback, (XtCallbackProc) CallbackDino,
		      (XtPointer) NULL);
	Initialize();
	XtRealizeWidget(toplevel);
	XtRealizeWidget(shell);
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
	PrintRecord(mode, orient, practice, recordDsp);
	PrintState(XtParent(dino2d), progDsp, 2, mode, movesDsp,
		   recordDsp, messageDsp);
	PrintState(XtParent(dino3d), progDsp, 3, mode, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackDino(Widget w, caddr_t clientData, dinoCallbackStruct * callData)
{
	int         mode, dim, otherdim;
	Boolean     orient, practice, start;
	Widget      otherw;

	if (w == dino2d) {
		dim = 2;
		otherw = dino3d;
		otherdim = 3;
	} else {		/* (w == dino3d) */
		dim = 3;
		otherw = dino2d;
		otherdim = 2;
	}
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
#ifdef DEBUG
			if (movesDsp > 256)
				exit(1);
#endif
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
			PrintRecord(mode, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XtSetValues(otherw, arg, 2);
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
			PrintRecord(mode, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case DINO_PERIOD2:
		case DINO_PERIOD3:
		case DINO_BOTH:
			movesDsp = 0;
			mode = callData->reason - DINO_PERIOD2 + PERIOD2;
			PrintRecord(mode, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
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
	PrintState(XtParent(w), progDsp, dim, mode, movesDsp,
		   recordDsp, messageDsp);
	PrintState(XtParent(otherw), progDsp, otherdim, mode, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int mode, Boolean orient, Boolean practice, char *record)
{
	int         i = mode - PERIOD2;
	int         j = (orient) ? 1 : 0;

	if (practice)
		(void) strcpy(record, "practice");
	else if (dinoRecord[i][j].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
			       dinoRecord[i][j].score, dinoRecord[i][j].name);
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
		PrintRecord(mode, orient, False, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int dim, int mode, int moves, char *record, char *message)
{
	char        mb[10];

	if (mode == BOTH)
		(void) strcpy(mb, "both");
	else
		(void) sprintf(mb, "%d", mode);
	(void) sprintf(titleDsp, "%s%dd.%s: (%d/%s) - %s", prog, dim, mb, moves,
		       record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
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

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
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
