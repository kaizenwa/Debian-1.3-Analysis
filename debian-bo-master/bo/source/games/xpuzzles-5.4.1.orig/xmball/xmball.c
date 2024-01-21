
/*-
# X-BASED MASTERBALL(tm)
#
#  xmball.c
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
  Version 1: 94/09/15 Xt
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
#include "Mball.h"
#include "mball.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/mball.scores"
#endif

/* The following are in MballP.h also */
#define MINWEDGES 2
#define MAXWEDGES 8
#define MINRINGS 1

#define MAXRINGS 6
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
static void CallbackMball(Widget w, caddr_t clientData,
			  mballCallbackStruct * callData);

static void PrintRecord(int wedges, int rings,
			Boolean orient, Boolean practice, char *record);
static int  HandleSolved(int counter, int wedges, int rings, Boolean orient);
static void PrintState(Widget w,
		       char *prog, int wedges, int rings, int moves,
		       char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Arg  arg[5];
static GameRecord mballRecord[2][(MAXWEDGES - MINWEDGES) / 2 + 1]
[MAXRINGS - MINRINGS + 1];
static int  movesDsp = 0;
static char progDsp[64] = "xmball";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xmball\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
	     "\t[-{border|bd} {color}] [-wedge{0|1|2|3|4|5|6|7} {color}]\n");
	(void) fprintf(stderr,
		       "\t[-{wedges {int}}] [-{rings {int}}] [-[no]orient] [-[no]practice]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*mball.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*mball.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*mball.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*mball.pieceBorder", XrmoptionSepArg, NULL},
	{"-border", "*mball.pieceBorder", XrmoptionSepArg, NULL},
	{"-wedge0", "*mball.wedgeColor0", XrmoptionSepArg, NULL},
	{"-wedge1", "*mball.wedgeColor1", XrmoptionSepArg, NULL},
	{"-wedge2", "*mball.wedgeColor2", XrmoptionSepArg, NULL},
	{"-wedge3", "*mball.wedgeColor3", XrmoptionSepArg, NULL},
	{"-wedge4", "*mball.wedgeColor4", XrmoptionSepArg, NULL},
	{"-wedge5", "*mball.wedgeColor5", XrmoptionSepArg, NULL},
	{"-wedge6", "*mball.wedgeColor6", XrmoptionSepArg, NULL},
	{"-wedge7", "*mball.wedgeColor7", XrmoptionSepArg, NULL},
	{"-wedges", "*mball.wedges", XrmoptionSepArg, NULL},
	{"-rings", "*mball.rings", XrmoptionSepArg, NULL},
	{"-orient", "*mball.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*mball.orient", XrmoptionNoArg, "FALSE"},
	{"-practice", "*mball.practice", XrmoptionNoArg, "TRUE"},
	{"-nopractice", "*mball.practice", XrmoptionNoArg, "FALSE"},
	{"-username", "*mball.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, mball;

	toplevel = XtInitialize(argv[0], "Mball",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) mball_bits, mball_width, mball_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	mball = XtCreateManagedWidget("mball", mballWidgetClass, toplevel,
				      NULL, 0);
	XtAddCallback(mball, XtNselectCallback, (XtCallbackProc) CallbackMball,
		      (XtPointer) NULL);
	Initialize(mball);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(mball), AnyButton, AnyModifier, XtWindow(mball),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(mball),
		    XCreateFontCursor(XtDisplay(mball), XC_crosshair));
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
	int         wedges, rings;
	Boolean     orient, practice;
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNwedges, &wedges,
		      XtNrings, &rings,
		      XtNorient, &orient,
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
	PrintRecord(wedges, rings, orient, practice, recordDsp);
	PrintState(XtParent(w), progDsp, wedges, rings, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackMball(Widget w, caddr_t clientData, mballCallbackStruct * callData)
{
	int         wedges, rings;
	Boolean     orient, practice, start;

	XtVaGetValues(w,
		      XtNwedges, &wedges,
		      XtNrings, &rings,
		      XtNorient, &orient,
		      XtNpractice, &practice,
		      XtNstart, &start,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case MBALL_RESTORE:
			if (practice)
				(void) strcpy(recordDsp, "practice");
			movesDsp = 0;
			break;
		case MBALL_RESET:
			movesDsp = 0;
			break;
		case MBALL_AMBIGUOUS:
			(void) strcpy(messageDsp, "Ambiguous move");
			break;
		case MBALL_ILLEGAL:
			if (practice || start)
				(void) strcpy(messageDsp, "Illegal move");
			else
				(void) strcpy(messageDsp, "Randomize to start");
			break;
		case MBALL_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case MBALL_CONTROL:
			return;
		case MBALL_SOLVED:
			if (practice)
				movesDsp = 0;
			else {
				if (HandleSolved(movesDsp, wedges, rings, orient))
					(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
				else
					(void) strcpy(messageDsp, "Solved!");
			}
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case MBALL_PRACTICE:
			movesDsp = 0;
			practice = !practice;
			if (!practice)
				(void) strcpy(messageDsp, "Randomize to start");
			PrintRecord(wedges, rings, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			break;
		case MBALL_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNpractice, FALSE);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			break;
		case MBALL_DEC:
			movesDsp = 0;
			rings--;
			PrintRecord(wedges, rings, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNrings, rings);
			XtSetValues(w, arg, 1);
			break;
		case MBALL_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(wedges, rings, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			break;
		case MBALL_INC:
			movesDsp = 0;
			rings++;
			PrintRecord(wedges, rings, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNrings, rings);
			XtSetValues(w, arg, 1);
			break;
		case MBALL_WEDGE2:
		case MBALL_WEDGE4:
		case MBALL_WEDGE6:
		case MBALL_WEDGE8:
			movesDsp = 0;
			wedges = 2 * (callData->reason - MBALL_WEDGE2) + MINWEDGES;
			PrintRecord(wedges, rings, orient, practice, recordDsp);
			XtSetArg(arg[0], XtNwedges, wedges);
			XtSetValues(w, arg, 1);
			break;
		case MBALL_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case MBALL_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
	}
	PrintState(XtParent(w), progDsp, wedges, rings, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int wedges, int rings, Boolean orient, Boolean practice, char *record)
{
	int         i = (orient) ? 1 : 0;
	int         w = (wedges - MINWEDGES) / 2;
	int         r = rings - MINRINGS;

	if (practice)
		(void) strcpy(record, "practice");
	else if (rings > MAXRINGS)
		(void) strcpy(record, "NOT RECORDED");
	else if (mballRecord[i][w][r].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
		      mballRecord[i][w][r].score, mballRecord[i][w][r].name);
}

static int
HandleSolved(int counter, int wedges, int rings, Boolean orient)
{
	int         i = (orient) ? 1 : 0;
	int         w = (wedges - MINWEDGES) / 2;
	int         r = rings - MINRINGS;

	if (rings <= MAXRINGS && counter < mballRecord[i][w][r].score) {
		mballRecord[i][w][r].score = counter;
		(void) strcpy(mballRecord[i][w][r].name, usernameDsp);
		if (orient && counter < mballRecord[!i][w][r].score) {
			mballRecord[!i][w][r].score = counter;
			(void) strcpy(mballRecord[!i][w][r].name, usernameDsp);
		}
		WriteRecords();
		PrintRecord(wedges, rings, orient, False, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int wedges, int rings, int moves, char *record, char *message)
{
	(void) sprintf(titleDsp, "%s.%d: %d@ (%d/%s) - %s", prog, wedges, rings,
		       moves, record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
}

static void
InitRecords(void)
{
	int         i, j, orient;

	for (orient = 0; orient < 2; orient++)
		for (i = 0; i < (MAXWEDGES - MINWEDGES) / 2 + 1; i++)
			for (j = 0; j < MAXRINGS - MINRINGS + 1; j++) {
				mballRecord[orient][i][j].score = MAXRECORD;
				(void) strcpy(mballRecord[orient][i][j].name, NOACCESS);
			}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, j, n, orient;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
		for (orient = 0; orient < 2; orient++)
			for (i = 0; i < (MAXWEDGES - MINWEDGES) / 2 + 1; i++)
				for (j = 0; j < MAXRINGS - MINRINGS + 1; j++) {
					(void) fscanf(fp, "%d %s\n", &n, username);
					if (n <= mballRecord[orient][i][j].score) {
						mballRecord[orient][i][j].score = n;
						(void) strcpy(mballRecord[orient][i][j].name, username);
					}
				}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, j, orient;

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
			for (i = 0; i < (MAXWEDGES - MINWEDGES) / 2 + 1; i++) {
				for (j = 0; j < MAXRINGS - MINRINGS + 1; j++)
					(void) fprintf(fp, "%d %s\n",
						       mballRecord[orient][i][j].score, mballRecord[orient][i][j].name);
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
