
/*-
# X-BASED PYRAMINX(tm)
#
#  xpyraminx.c
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
#include "Pyraminx.h"
#include "pyraminx.xbm"

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
	     Boolean orient, Boolean sticky, Boolean practice, char *record);
static int  HandleSolved(int counter, int size, int mode,
			 Boolean orient, Boolean sticky);
static void PrintState(Widget w, char *prog, int mode, int size,
		     Boolean sticky, int moves, char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Arg  arg[5];
static GameRecord pyraminxRecord[MAXMODES][2][MAXTETRAS - MINTETRAS + 2];
static int  movesDsp = 0;
static char progDsp[64] = "xpyraminx";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";
static int  oldSize;

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xpyraminx\n");
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
	Widget      toplevel, pyraminx;

	toplevel = XtInitialize(argv[0], "Pyraminx",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
		   (char *) pyraminx_bits, pyraminx_width, pyraminx_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	pyraminx = XtCreateManagedWidget("pyraminx", pyraminxWidgetClass, toplevel,
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
	PrintRecord(size, mode, orient, sticky, practice, recordDsp);
	oldSize = size;
	PrintState(XtParent(w), progDsp, mode, size, sticky, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackPyraminx(Widget w, caddr_t clientData, pyraminxCallbackStruct * callData)
{
	int         size, mode;
	Boolean     orient, sticky, practice, start;

	XtVaGetValues(w,
		      XtNsize, &size,
		      XtNorient, &orient,
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
				(void) strcpy(recordDsp, "practice");
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
#ifdef DEBUG
			if (movesDsp > 256)
				exit(1);
#endif
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
			PrintRecord(size, mode, orient, sticky, practice, recordDsp);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
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
				PrintRecord(size, mode, orient, sticky, practice, recordDsp);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
			}
			break;
		case PYRAMINX_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(size, mode, orient, sticky, practice, recordDsp);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			break;
		case PYRAMINX_INC:
			if (!sticky) {
				movesDsp = 0;
				size++;
				oldSize = size;
				PrintRecord(size, mode, orient, sticky, practice, recordDsp);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
			}
			break;
		case PYRAMINX_PERIOD2:
		case PYRAMINX_PERIOD3:
		case PYRAMINX_BOTH:
			movesDsp = 0;
			mode = callData->reason - PYRAMINX_PERIOD2 + PERIOD2;
			PrintRecord(size, mode, orient, sticky, practice, recordDsp);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			break;
		case PYRAMINX_STICKY:
			movesDsp = 0;
			sticky = !sticky;
			if (sticky)
				size = 4;
			else
				size = oldSize;
			PrintRecord(size, mode, orient, sticky, practice, recordDsp);
			XtSetArg(arg[0], XtNsticky, sticky);
			XtSetArg(arg[1], XtNsize, size);
			XtSetValues(w, arg, 2);
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
	PrintState(XtParent(w), progDsp, mode, size, sticky, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int size, int mode, Boolean orient, Boolean sticky, Boolean practice, char *record)
{
	int         i = mode - PERIOD2;
	int         j = (orient) ? 1 : 0;
	int         k = (sticky) ? MAXTETRAS - MINTETRAS + 1 : size - MINTETRAS;

	if (practice)
		(void) strcpy(record, "practice");
	else if (!sticky && size > MAXTETRAS)
		(void) strcpy(record, "NOT RECORDED");
	else if (pyraminxRecord[i][j][k].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
		pyraminxRecord[i][j][k].score, pyraminxRecord[i][j][k].name);
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
		if (size < 4 || mode == PERIOD2 ||
		    (orient && (counter < pyraminxRecord[i][!j][k].score))) {
			pyraminxRecord[i][!j][k].score = counter;
			(void) strcpy(pyraminxRecord[i][!j][k].name, usernameDsp);
		}
		WriteRecords();
		PrintRecord(size, mode, orient, sticky, False, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int mode, int size, Boolean sticky, int moves, char *record, char *message)
{
	char        ss[10], mb[10];

	if (sticky)
		(void) strcpy(ss, "sticky");
	else
		(void) sprintf(ss, "%d", size);
	if (mode == BOTH)
		(void) strcpy(mb, "both");
	else
		(void) sprintf(mb, "%d", mode);
	(void) sprintf(titleDsp, "%s.%s: %s@ (%d/%s) - %s", prog, mb, ss, moves,
		       record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
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

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
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
