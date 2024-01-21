
/*-
# X-BASED RUBIK'S CUBE(tm)
#
#  xrubik.c
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
  Version 4: 94/04/07 Xt
  Version 3: 93/05/20 Motif
  Version 2: 92/01/16 XView
  Version 1: 91/01/16 SunView
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
#include "Rubik.h"
#include "Rubik2d.h"
#include "Rubik3d.h"
#include "rubik.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/rubik.scores"
#endif

/* The following are in RubikP.h also */
#define MINCUBES 1
#define MAXFACES 6

#define MAXCUBES 6
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
static void CallbackRubik(Widget w, caddr_t clientData,
			  rubikCallbackStruct * callData);

static void PrintRecord(int size, Boolean orient, Boolean practice,
			char *record);
static int  HandleSolved(int counter, int size, Boolean orient);
static void PrintState(Widget w,
		       char *prog, int dim, int size, int moves,
		       char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Widget rubik2d, rubik3d;
static Arg  arg[4];
static GameRecord rubikRecord[2][MAXCUBES - MINCUBES + 1];
static int  movesDsp = 0;
static char progDsp[64] = "xrubik";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xrubik\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		  "\t[-{border|bd} {color}] [-face{0|1|2|3|4|5} {color}]\n");
	(void) fprintf(stderr,
		       "\t[-size {int}] [-[no]orient] [-[no]practice]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*rubik.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*rubik.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*rubik.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*rubik.pieceBorder", XrmoptionSepArg, NULL},
	{"-border", "*rubik.pieceBorder", XrmoptionSepArg, NULL},
	{"-face0", "*rubik.faceColor0", XrmoptionSepArg, NULL},
	{"-face1", "*rubik.faceColor1", XrmoptionSepArg, NULL},
	{"-face2", "*rubik.faceColor2", XrmoptionSepArg, NULL},
	{"-face3", "*rubik.faceColor3", XrmoptionSepArg, NULL},
	{"-face4", "*rubik.faceColor4", XrmoptionSepArg, NULL},
	{"-face5", "*rubik.faceColor5", XrmoptionSepArg, NULL},
	{"-size", "*rubik.size", XrmoptionSepArg, NULL},
	{"-orient", "*rubik.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*rubik.orient", XrmoptionNoArg, "FALSE"},
	{"-practice", "*rubik.practice", XrmoptionNoArg, "TRUE"},
	{"-nopractice", "*rubik.practice", XrmoptionNoArg, "FALSE"},
	{"-username", "*rubik.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, shell;

	toplevel = XtInitialize(argv[0], "Rubik",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	shell = XtCreateApplicationShell(argv[0], topLevelShellWidgetClass, NULL,
					 0);
	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) rubik_bits, rubik_width, rubik_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(shell),
				       RootWindowOfScreen(XtScreen(shell)),
			    (char *) rubik_bits, rubik_width, rubik_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(shell, arg, 2);
	rubik2d = XtCreateManagedWidget("rubik", rubik2dWidgetClass, toplevel,
					NULL, 0);
	XtAddCallback(rubik2d, XtNselectCallback, (XtCallbackProc) CallbackRubik,
		      (XtPointer) NULL);
	rubik3d = XtCreateManagedWidget("rubik", rubik3dWidgetClass, shell,
					NULL, 0);
	XtAddCallback(rubik3d, XtNselectCallback, (XtCallbackProc) CallbackRubik,
		      (XtPointer) NULL);
	Initialize();
	XtRealizeWidget(toplevel);
	XtRealizeWidget(shell);
	XGrabButton(XtDisplay(rubik2d), AnyButton, AnyModifier, XtWindow(rubik2d),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(rubik2d),
		    XCreateFontCursor(XtDisplay(rubik2d), XC_crosshair));
	XGrabButton(XtDisplay(rubik3d), AnyButton, AnyModifier, XtWindow(rubik3d),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(rubik3d),
		    XCreateFontCursor(XtDisplay(rubik3d), XC_crosshair));
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
MakeEquivalent(String * username, int *size, Boolean * orient, Boolean * practice)
{
	Boolean     mono;
	Pixel       foreground, background, pieceBorder;
	String      faceColor[MAXFACES];

	XtVaGetValues(rubik2d,
		      XtNuserName, username,
		      XtNsize, size,
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
	XtVaSetValues(rubik2d,
		      XtNdirection, RUBIK_IGNORE,
		      XtNstart, FALSE,
		      NULL);
	XtVaSetValues(rubik3d,
		      XtNuserName, *username,
		      XtNsize, *size,
		      XtNorient, *orient,
		      XtNmono, mono,
		      XtNdirection, RUBIK_IGNORE,
		      XtNpractice, *practice,
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
	int         size;
	Boolean     orient, practice;
	String      username;

	MakeEquivalent(&username, &size, &orient, &practice);
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
	PrintRecord(size, orient, practice, recordDsp);
	PrintState(XtParent(rubik2d), progDsp, 2, size, movesDsp,
		   recordDsp, messageDsp);
	PrintState(XtParent(rubik3d), progDsp, 3, size, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackRubik(Widget w, caddr_t clientData, rubikCallbackStruct * callData)
{
	int         size, dim, otherdim;
	Boolean     orient, practice, start;
	Widget      otherw;

	if (w == rubik2d) {
		dim = 2;
		otherw = rubik3d;
		otherdim = 3;
	} else {		/* (w == rubik3d) */
		dim = 3;
		otherw = rubik2d;
		otherdim = 2;
	}
	XtVaGetValues(w,
		      XtNsize, &size,
		      XtNorient, &orient,
		      XtNpractice, &practice,
		      XtNstart, &start,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case RUBIK_RESTORE:
			XtSetArg(arg[0], XtNdirection, RUBIK_RESTORE);
			XtSetValues(otherw, arg, 1);
			XtSetValues(w, arg, 1);
			movesDsp = 0;
			break;
		case RUBIK_RESET:
			movesDsp = 0;
			break;
		case RUBIK_ILLEGAL:
			if (practice || start)
				(void) strcpy(messageDsp, "Illegal move");
			else
				(void) strcpy(messageDsp, "Randomize to start");
			break;
		case RUBIK_MOVED:
			movesDsp++;
#ifdef DEBUG
			if (movesDsp > 256)
				exit(1);
#endif
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetArg(arg[1], XtNface, callData->face);
			XtSetArg(arg[2], XtNpos, callData->position);
			XtSetArg(arg[3], XtNdirection, callData->direction);
			XtSetValues(otherw, arg, 4);
			XtSetValues(w, arg, 1);
			break;
		case RUBIK_CONTROL:
			XtSetArg(arg[0], XtNface, callData->face);
			XtSetArg(arg[1], XtNpos, callData->position);
			XtSetArg(arg[2], XtNdirection, callData->direction);
			XtSetValues(otherw, arg, 3);
			return;
		case RUBIK_SOLVED:
			if (practice)
				movesDsp = 0;
			else {
				if (HandleSolved(movesDsp, size, orient))
					(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
				else
					(void) strcpy(messageDsp, "Solved!");
			}
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case RUBIK_PRACTICE:
			movesDsp = 0;
			practice = !practice;
			if (!practice)
				(void) strcpy(messageDsp, "Randomize to start");
			PrintRecord(size, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XtSetValues(otherw, arg, 2);
			break;
		case RUBIK_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNpractice, FALSE);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XtSetValues(otherw, arg, 2);
			break;
		case RUBIK_DEC:
			movesDsp = 0;
			size--;
			PrintRecord(size, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case RUBIK_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(size, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case RUBIK_INC:
			movesDsp = 0;
			size++;
			PrintRecord(size, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case RUBIK_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case RUBIK_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetArg(arg[1], XtNface, callData->face);
			XtSetArg(arg[2], XtNpos, callData->position);
			XtSetArg(arg[3], XtNdirection, callData->direction);
			XtSetValues(otherw, arg, 4);
			XtSetValues(w, arg, 1);
			break;
	}
	PrintState(XtParent(w), progDsp, dim, size, movesDsp,
		   recordDsp, messageDsp);
	PrintState(XtParent(otherw), progDsp, otherdim, size, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int size, Boolean orient, Boolean practice, char *record)
{
	int         i = (orient) ? 1 : 0;
	int         j = size - MINCUBES;

	if (practice)
		(void) strcpy(record, "practice");
	else if (size > MAXCUBES)
		(void) strcpy(record, "NOT RECORDED");
	else if (rubikRecord[i][j].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
			    rubikRecord[i][j].score, rubikRecord[i][j].name);
}

static int
HandleSolved(int counter, int size, Boolean orient)
{
	int         i = (orient) ? 1 : 0;
	int         j = size - MINCUBES;

	if (size <= MAXCUBES && counter < rubikRecord[i][j].score) {
		rubikRecord[i][j].score = counter;
		(void) strcpy(rubikRecord[i][j].name, usernameDsp);
		if (size < 2 || (orient && (counter < rubikRecord[!i][j].score))) {
			rubikRecord[!i][j].score = counter;
			(void) strcpy(rubikRecord[!i][j].name, usernameDsp);
		}
		WriteRecords();
		PrintRecord(size, orient, False, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int dim, int size, int moves, char *record, char *message)
{
	(void) sprintf(titleDsp, "%s%dd: %d@ (%d/%s) - %s", prog, dim, size, moves,
		       record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
}

static void
InitRecords(void)
{
	int         i, orient;

	for (orient = 0; orient < 2; orient++)
		for (i = 0; i < MAXCUBES - MINCUBES + 1; i++) {
			rubikRecord[orient][i].score = MAXRECORD;
			(void) strcpy(rubikRecord[orient][i].name, NOACCESS);
		}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, n, orient;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp,
			     "Can not open %s, taking defaults.", SCOREFILE);
	else {
		for (orient = 0; orient < 2; orient++)
			for (i = 0; i < MAXCUBES - MINCUBES + 1; i++) {
				(void) fscanf(fp, "%d %s\n", &n, username);
				if (n <= rubikRecord[orient][i].score) {
					rubikRecord[orient][i].score = n;
					(void) strcpy(rubikRecord[orient][i].name, username);
				}
			}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, orient;

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
		for (orient = 0; orient < 2; orient++) {
			for (i = 0; i < MAXCUBES - MINCUBES + 1; i++)
				(void) fprintf(fp, "%d %s\n",
					       rubikRecord[orient][i].score, rubikRecord[orient][i].name);
			(void) fprintf(fp, "\n");
		}
#if HAVE_FCNTL_H
		(void) close(lfd);
		(void) unlink(lockfile);
#endif
		(void) fclose(fp);
	}
}
