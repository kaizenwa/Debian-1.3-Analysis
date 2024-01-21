
/*-
# X-BASED OCTAHEDRON
#
#  xoct.c
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
#include "Oct.h"
#include "oct.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/oct.scores"
#endif

/* The following are in OctP.h also */
#define MINOCTAS 1
#define PERIOD3 3
#define PERIOD4 4
#define BOTH 5
#define MAXMODES 3

#define MAXOCTAS 6
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
static void CallbackOct(Widget w, caddr_t clientData,
			octCallbackStruct * callData);

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
static GameRecord octRecord[MAXMODES][2][MAXOCTAS - MINOCTAS + 2];
static int  movesDsp = 0;
static char progDsp[64] = "xoct";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";
static int  oldSize;

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xoct\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
	      "\t[-{border|bd} {color}] [-face{0|1|2|3|4|5|6|7} {color}]\n");
	(void) fprintf(stderr,
		       "\t[-{size {int} | sticky}] [-{mode {int} | both}]\n");
	(void) fprintf(stderr,
		   "\t[-[no]orient] [-[no]practice] [-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*oct.mono", XrmoptionNoArg, "TRUE"},
	{"-fg", "*oct.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*oct.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*oct.pieceBorder", XrmoptionSepArg, NULL},
	{"-border", "*oct.pieceBorder", XrmoptionSepArg, NULL},
	{"-face0", "*oct.faceColor0", XrmoptionSepArg, NULL},
	{"-face1", "*oct.faceColor1", XrmoptionSepArg, NULL},
	{"-face2", "*oct.faceColor2", XrmoptionSepArg, NULL},
	{"-face3", "*oct.faceColor3", XrmoptionSepArg, NULL},
	{"-face4", "*oct.faceColor4", XrmoptionSepArg, NULL},
	{"-face5", "*oct.faceColor5", XrmoptionSepArg, NULL},
	{"-face6", "*oct.faceColor6", XrmoptionSepArg, NULL},
	{"-face7", "*oct.faceColor7", XrmoptionSepArg, NULL},
	{"-size", "*oct.size", XrmoptionSepArg, NULL},
	{"-sticky", "*oct.sticky", XrmoptionNoArg, "FALSE"},
	{"-mode", "*oct.mode", XrmoptionSepArg, NULL},
	{"-both", "*oct.mode", XrmoptionNoArg, "4"},
	{"-orient", "*oct.orient", XrmoptionNoArg, "TRUE"},
	{"-noorient", "*oct.orient", XrmoptionNoArg, "FALSE"},
	{"-practice", "*oct.practice", XrmoptionNoArg, "TRUE"},
	{"-nopractice", "*oct.practice", XrmoptionNoArg, "FALSE"},
	{"-username", "*oct.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, oct;

	toplevel = XtInitialize(argv[0], "Oct",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
				  (char *) oct_bits, oct_width, oct_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	oct = XtCreateManagedWidget("oct", octWidgetClass, toplevel,
				    NULL, 0);
	XtAddCallback(oct, XtNselectCallback, (XtCallbackProc) CallbackOct,
		      (XtPointer) NULL);
	Initialize(oct);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(oct), AnyButton, AnyModifier, XtWindow(oct),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(oct),
		    XCreateFontCursor(XtDisplay(oct), XC_crosshair));
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
CallbackOct(Widget w, caddr_t clientData, octCallbackStruct * callData)
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
		case OCT_RESTORE:
			if (practice)
				(void) strcpy(recordDsp, "practice");
			movesDsp = 0;
			break;
		case OCT_RESET:
			movesDsp = 0;
			break;
		case OCT_AMBIGUOUS:
			(void) strcpy(messageDsp, "Ambiguous move");
			break;
		case OCT_ILLEGAL:
			if (practice || start)
				(void) strcpy(messageDsp, "Illegal move");
			else
				(void) strcpy(messageDsp, "Randomize to start");
			break;
		case OCT_MOVED:
			movesDsp++;
#ifdef DEBUG
			if (movesDsp > 256)
				exit(1);
#endif
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case OCT_CONTROL:
			return;
		case OCT_SOLVED:
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
		case OCT_PRACTICE:
			movesDsp = 0;
			practice = !practice;
			if (!practice)
				(void) strcpy(messageDsp, "Randomize to start");
			PrintRecord(size, mode, orient, sticky, practice, recordDsp);
			XtSetArg(arg[0], XtNpractice, practice);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			break;
		case OCT_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNpractice, FALSE);
			XtSetArg(arg[1], XtNstart, FALSE);
			XtSetValues(w, arg, 2);
			break;
		case OCT_DEC:
			if (!sticky) {
				movesDsp = 0;
				size--;
				oldSize = size;
				PrintRecord(size, mode, orient, sticky, practice, recordDsp);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
			}
			break;
		case OCT_ORIENT:
			movesDsp = 0;
			orient = !orient;
			PrintRecord(size, mode, orient, sticky, practice, recordDsp);
			XtSetArg(arg[0], XtNorient, orient);
			XtSetValues(w, arg, 1);
			break;
		case OCT_INC:
			if (!sticky) {
				movesDsp = 0;
				size++;
				oldSize = size;
				PrintRecord(size, mode, orient, sticky, practice, recordDsp);
				XtSetArg(arg[0], XtNsize, size);
				XtSetValues(w, arg, 1);
			}
			break;
		case OCT_PERIOD3:
		case OCT_PERIOD4:
		case OCT_BOTH:
			movesDsp = 0;
			mode = callData->reason - OCT_PERIOD3 + PERIOD3;
			PrintRecord(size, mode, orient, sticky, practice, recordDsp);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			break;
		case OCT_STICKY:
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
		case OCT_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case OCT_UNDO:
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
	int         i = mode - PERIOD3;
	int         j = (orient) ? 1 : 0;
	int         k = (sticky) ? MAXOCTAS - MINOCTAS + 1 : size - MINOCTAS;

	if (practice)
		(void) strcpy(record, "practice");
	else if (!sticky && size > MAXOCTAS)
		(void) strcpy(record, "NOT RECORDED");
	else if (octRecord[i][j][k].score >= MAXRECORD)
		(void) printf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
			  octRecord[i][j][k].score, octRecord[i][j][k].name);
}

static int
HandleSolved(int counter, int size, int mode, Boolean orient, Boolean sticky)
{
	int         i = mode - PERIOD3;
	int         j = (orient) ? 1 : 0;
	int         k = (sticky) ? MAXOCTAS - MINOCTAS + 1 : size - MINOCTAS;

	if ((sticky || size <= MAXOCTAS) && counter < octRecord[i][j][k].score) {
		octRecord[i][j][k].score = counter;
		(void) strcpy(octRecord[i][j][k].name, usernameDsp);
		if ((size < 2 && mode != PERIOD4) || (size < 4 && mode == PERIOD4) ||
		    (orient && (counter < octRecord[i][!j][k].score))) {
			octRecord[i][!j][k].score = counter;
			(void) strcpy(octRecord[i][!j][k].name, usernameDsp);
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
			for (i = 0; i <= MAXOCTAS - MINOCTAS + 1; i++) {
				octRecord[mode][orient][i].score = MAXRECORD;
				(void) strcpy(octRecord[mode][orient][i].name, NOACCESS);
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
				for (i = 0; i <= MAXOCTAS - MINOCTAS + 1; i++) {
					(void) fscanf(fp, "%d %s\n", &n, username);
					if (n <= octRecord[mode][orient][i].score) {
						octRecord[mode][orient][i].score = n;
						(void) strcpy(octRecord[mode][orient][i].name, username);
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
				for (i = 0; i <= MAXOCTAS - MINOCTAS + 1; i++)
					(void) fprintf(fp, "%d %s\n",
						       octRecord[mode][orient][i].score, octRecord[mode][orient][i].name);
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
