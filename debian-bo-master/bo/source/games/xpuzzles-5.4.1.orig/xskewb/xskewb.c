
/*-
# X-BASED SKEWB
#
#  xskewb.c
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
  Version 4: 94/05/30 Xt
  Version 3: 93/10/14 Motif
  Version 2: 92/01/22 XView
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
#include "Skewb.h"
#include "Skewb2d.h"
#include "Skewb3d.h"
#include "skewb.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/skewb.scores"
#endif

/* The following is in SkewbP.h also */
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
static void CallbackSkewb(Widget w, caddr_t clientData,
			  skewbCallbackStruct * callData);

static void PrintRecord(Boolean orient, Boolean practice, char *record);
static int  HandleSolved(int counter, Boolean orient);
static void PrintState(Widget w,
		       char *prog, int dim, int moves,
		       char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Widget skewb2d, skewb3d;
static Arg  arg[4];
static GameRecord skewbRecord[2];
static int  movesDsp = 0;
static char progDsp[64] = "xskewb";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xskewb\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		  "\t[-{border|bd} {color}] [-face{0|1|2|3|4|5} {color}]\n");
	(void) fprintf(stderr,
		   "\t[-[no]orient] [-[no]practice] [-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*skewb.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*skewb.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*skewb.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*skewb.pieceBorder", XrmoptionSepArg, NULL},
	{"-border", "*skewb.pieceBorder", XrmoptionSepArg, NULL},
	{"-face0", "*skewb.faceColor0", XrmoptionSepArg, NULL},
	{"-face1", "*skewb.faceColor1", XrmoptionSepArg, NULL},
	{"-face2", "*skewb.faceColor2", XrmoptionSepArg, NULL},
	{"-face3", "*skewb.faceColor3", XrmoptionSepArg, NULL},
	{"-face4", "*skewb.faceColor4", XrmoptionSepArg, NULL},
	{"-face5", "*skewb.faceColor5", XrmoptionSepArg, NULL},
	{"-orient", "*skewb.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*skewb.orient", XrmoptionNoArg, "FALSE"},
	{"-practice", "*skewb.practice", XrmoptionNoArg, "TRUE"},
	{"-nopractice", "*skewb.practice", XrmoptionNoArg, "FALSE"},
	{"-username", "*skewb.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, shell;

	toplevel = XtInitialize(argv[0], "Skewb",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	shell = XtCreateApplicationShell(argv[0], topLevelShellWidgetClass, NULL,
					 0);
	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) skewb_bits, skewb_width, skewb_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(shell),
				       RootWindowOfScreen(XtScreen(shell)),
			    (char *) skewb_bits, skewb_width, skewb_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(shell, arg, 2);
	skewb2d = XtCreateManagedWidget("skewb", skewb2dWidgetClass, toplevel,
					NULL, 0);
	XtAddCallback(skewb2d, XtNselectCallback, (XtCallbackProc) CallbackSkewb,
		      (XtPointer) NULL);
	skewb3d = XtCreateManagedWidget("skewb", skewb3dWidgetClass, shell,
					NULL, 0);
	XtAddCallback(skewb3d, XtNselectCallback, (XtCallbackProc) CallbackSkewb,
		      (XtPointer) NULL);
	Initialize();
	XtRealizeWidget(toplevel);
	XtRealizeWidget(shell);
	XGrabButton(XtDisplay(skewb2d), AnyButton, AnyModifier, XtWindow(skewb2d),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(skewb2d),
		    XCreateFontCursor(XtDisplay(skewb2d), XC_crosshair));
	XGrabButton(XtDisplay(skewb3d), AnyButton, AnyModifier, XtWindow(skewb3d),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(skewb3d),
		    XCreateFontCursor(XtDisplay(skewb3d), XC_crosshair));
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
MakeEquivalent(String * username, Boolean * orient, Boolean * practice)
{
	Boolean     mono;
	Pixel       foreground, background, pieceBorder;
	String      faceColor[MAXFACES];

	XtVaGetValues(skewb2d,
		      XtNuserName, username,
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
	XtVaSetValues(skewb2d,
		      XtNdirection, SKEWB_IGNORE,
		      XtNstart, FALSE,
		      NULL);
	XtVaSetValues(skewb3d,
		      XtNuserName, *username,
		      XtNorient, *orient,
		      XtNpractice, *practice,
		      XtNmono, mono,
		      XtNdirection, SKEWB_IGNORE,
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
	Boolean     orient, practice;
	String      username;

	MakeEquivalent(&username, &orient, &practice);
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
	PrintRecord(orient, practice, recordDsp);
	PrintState(XtParent(skewb2d), progDsp, 2, movesDsp,
		   recordDsp, messageDsp);
	PrintState(XtParent(skewb3d), progDsp, 3, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackSkewb(Widget w, caddr_t clientData, skewbCallbackStruct * callData)
{
	int         dim, otherdim;
	Boolean     orient, practice, start;
	Widget      otherw;

	if (w == skewb2d) {
		dim = 2;
		otherw = skewb3d;
		otherdim = 3;
	} else {		/* (w == skewb3d) */
		dim = 3;
		otherw = skewb2d;
		otherdim = 2;
	}
	XtVaGetValues(w,
		      XtNorient, &orient,
		      XtNpractice, &practice,
		      XtNstart, &start,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case SKEWB_RESTORE:
			XtSetArg(arg[0], XtNdirection, SKEWB_RESTORE);
			XtSetValues(otherw, arg, 1);
			XtSetValues(w, arg, 1);
			movesDsp = 0;
			break;
		case SKEWB_RESET:
			movesDsp = 0;
			break;
		case SKEWB_ILLEGAL:
			if (practice || start)
				(void) strcpy(messageDsp, "Illegal move");
			else
				(void) strcpy(messageDsp, "Randomize to start");
			break;
		case SKEWB_MOVED:
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
		case SKEWB_CONTROL:
			XtSetArg(arg[0], XtNface, callData->face);
			XtSetArg(arg[1], XtNpos, callData->position);
			XtSetArg(arg[2], XtNdirection, callData->direction);
			XtSetValues(otherw, arg, 3);
			return;
		case SKEWB_SOLVED:
			if (practice)
				movesDsp = 0;
			else {
				if (HandleSolved(movesDsp, orient))
					(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
				else
					(void) strcpy(messageDsp, "Solved!");
			}
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case SKEWB_PRACTICE:
			movesDsp = 0;
			practice = !practice;
			if (!practice)
				(void) strcpy(messageDsp, "Randomize to start");
			PrintRecord(orient, practice, recordDsp);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XtSetValues(otherw, arg, 2);
			break;
		case SKEWB_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNpractice, FALSE);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			XtSetValues(otherw, arg, 2);
			break;
		case SKEWB_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(orient, practice, recordDsp);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case SKEWB_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			XtSetValues(otherw, arg, 1);
			break;
		case SKEWB_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetArg(arg[1], XtNface, callData->face);
			XtSetArg(arg[2], XtNpos, callData->position);
			XtSetArg(arg[3], XtNdirection, callData->direction);
			XtSetValues(otherw, arg, 4);
			XtSetValues(w, arg, 1);
			break;
	}
	PrintState(XtParent(w), progDsp, dim, movesDsp,
		   recordDsp, messageDsp);
	PrintState(XtParent(otherw), progDsp, otherdim, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(Boolean orient, Boolean practice, char *record)
{
	int         i = (orient) ? 1 : 0;

	if (practice)
		(void) strcpy(record, "practice");
	else if (skewbRecord[i].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
			       skewbRecord[i].score, skewbRecord[i].name);
}

static int
HandleSolved(int counter, Boolean orient)
{
	int         i = (orient) ? 1 : 0;

	if (counter < skewbRecord[i].score) {
		skewbRecord[i].score = counter;
		(void) strcpy(skewbRecord[i].name, usernameDsp);
		if (orient && (counter < skewbRecord[!i].score)) {
			skewbRecord[!i].score = counter;
			(void) strcpy(skewbRecord[!i].name, usernameDsp);
		}
		WriteRecords();
		PrintRecord(orient, False, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int dim, int moves, char *record, char *message)
{
	(void) sprintf(titleDsp, "%s%dd: (%d/%s) - %s", prog, dim, moves,
		       record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
}

static void
InitRecords(void)
{
	int         orient;

	for (orient = 0; orient < 2; orient++) {
		skewbRecord[orient].score = MAXRECORD;
		(void) strcpy(skewbRecord[orient].name, NOACCESS);
	}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         n, orient;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
		for (orient = 0; orient < 2; orient++) {
			(void) fscanf(fp, "%d %s\n", &n, username);
			if (n <= skewbRecord[orient].score) {
				skewbRecord[orient].score = n;
				(void) strcpy(skewbRecord[orient].name, username);
			}
		}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         orient;

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
		for (orient = 0; orient < 2; orient++)
			(void) fprintf(fp, "%d %s\n",
			skewbRecord[orient].score, skewbRecord[orient].name);
#if HAVE_FCNTL_H
		(void) close(lfd);
		(void) unlink(lockfile);
#endif
		(void) fclose(fp);
	}
}
