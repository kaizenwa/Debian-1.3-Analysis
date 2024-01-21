
/*-
# X-BASED HEXAGONS
#
#  xhexagons.c
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
  Version 4: 94/06/07 Xt
  Version 3: 93/04/01 Motif
  Version 2: 92/01/06 XView
  Version 1: 91/09/05 SunView
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
#include "Hexagons.h"
#include "hexagons.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/hexagons.scores"
#endif

/* The following are in HexagonsP.h also */
#define MINHEXAGONS 1
#define NOCORN 0
#define CORNERS 1
#define MAXORIENT 2

#define MAXHEXAGONS 12
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
static void CallbackHexagons(Widget w,
		      caddr_t clientData, hexagonsCallbackStruct * callData);

static void PrintRecord(int size, Boolean corners, char *record);
static int  HandleSolved(int counter, int size, Boolean corners);
static void PrintState(Widget w,
	       char *prog, int size, int moves, char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Arg  arg[1];
static GameRecord hexagonsRecord[MAXORIENT][MAXHEXAGONS - MINHEXAGONS + 1];
static int  movesDsp = 0;
static char progDsp[64] = "xhexagons";
static char recordDsp[128] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xhexagons\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		 "\t[-{border|bd} {color}] [-tile {color}] [-size {int}]\n");
	(void) fprintf(stderr,
		       "\t[-[no]corners] [-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-fg", "*hexagons.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*hexagons.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*hexagons.tileBorder", XrmoptionSepArg, NULL},
	{"-border", "*hexagons.tileBorder", XrmoptionSepArg, NULL},
	{"-tile", "*hexagons.tileColor", XrmoptionSepArg, NULL},
	{"-size", "*hexagons.size", XrmoptionSepArg, NULL},
	{"-corners", "*hexagons.corners", XrmoptionNoArg, "TRUE"},
	{"-nocorners", "*hexagons.corners", XrmoptionNoArg, "FALSE"},
	{"-username", "*hexagons.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, hexagons;

	toplevel = XtInitialize(argv[0], "Hexagons",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
		   (char *) hexagons_bits, hexagons_width, hexagons_height));
	XtSetValues(toplevel, arg, 1);
	hexagons = XtCreateManagedWidget("hexagons", hexagonsWidgetClass, toplevel,
					 NULL, 0);
	XtAddCallback(hexagons, XtNselectCallback, (XtCallbackProc) CallbackHexagons,
		      (XtPointer) NULL);
	Initialize(hexagons);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(hexagons), AnyButton, AnyModifier, XtWindow(hexagons),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(hexagons),
		    XCreateFontCursor(XtDisplay(hexagons), XC_crosshair));
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
	int         size;
	Boolean     corners;
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNsize, &size,
		      XtNcorners, &corners,
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
	PrintRecord(size, corners, recordDsp);
	PrintState(XtParent(w), progDsp, size, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackHexagons(Widget w, caddr_t clientData,
		 hexagonsCallbackStruct * callData)
{
	int         size;
	Boolean     corners;

	XtVaGetValues(w,
		      XtNsize, &size,
		      XtNcorners, &corners,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case HEXAGONS_RESTORE:
		case HEXAGONS_RESET:
			movesDsp = 0;
			break;
		case HEXAGONS_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case HEXAGONS_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case HEXAGONS_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case HEXAGONS_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_SOLVED:
			if (HandleSolved(movesDsp, size, corners))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_DEC:
			movesDsp = 0;
			size--;
			PrintRecord(size, corners, recordDsp);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_INC:
			movesDsp = 0;
			size++;
			PrintRecord(size, corners, recordDsp);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_CORNERS:
			movesDsp = 0;
			corners = !corners;
			PrintRecord(size, corners, recordDsp);
			XtSetArg(arg[0], XtNcorners, corners);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case HEXAGONS_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
	}
	PrintState(XtParent(w), progDsp, size, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int size, Boolean corners, char *record)
{
	int         i = (corners) ? 1 : 0, j = size - MINHEXAGONS;

	if (size > MAXHEXAGONS)
		(void) strcpy(record, "NOT RECORDED");
	else if (hexagonsRecord[i][j].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
		      hexagonsRecord[i][j].score, hexagonsRecord[i][j].name);
}

static int
HandleSolved(int counter, int size, Boolean corners)
{
	int         i = (corners) ? 1 : 0, j = size - MINHEXAGONS;

	if (size <= MAXHEXAGONS && counter < hexagonsRecord[i][j].score) {
		hexagonsRecord[i][j].score = counter;
		(void) strcpy(hexagonsRecord[i][j].name, usernameDsp);
		WriteRecords();
		PrintRecord(size, corners, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w,
	   char *prog, int size, int moves, char *record, char *message)
{
	(void) sprintf(titleDsp, "%s: %d@ (%d/%s) - %s",
		       prog, size, moves, record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
}

static void
InitRecords(void)
{
	int         i, j;

	for (i = 0; i < MAXORIENT; i++)
		for (j = 0; j < MAXHEXAGONS - MINHEXAGONS + 1; j++) {
			hexagonsRecord[i][j].score = MAXRECORD;
			(void) strcpy(hexagonsRecord[i][j].name, NOACCESS);
		}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, j, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
		for (i = 0; i < MAXORIENT; i++)
			for (j = 0; j < MAXHEXAGONS - MINHEXAGONS + 1; j++) {
				(void) fscanf(fp, "%d %s\n", &n, username);
				if (n <= hexagonsRecord[i][j].score) {
					hexagonsRecord[i][j].score = n;
					(void) strcpy(hexagonsRecord[i][j].name, username);
				}
			}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, j;

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
		for (i = 0; i < MAXORIENT; i++) {
			for (j = 0; j < MAXHEXAGONS - MINHEXAGONS + 1; j++)
				(void) fprintf(fp, "%d %s\n",
					       hexagonsRecord[i][j].score, hexagonsRecord[i][j].name);
			(void) fprintf(fp, "\n");
		}
#if HAVE_FCNTL_H
		(void) close(lfd);
		(void) unlink(lockfile);
#endif
		(void) fclose(fp);
	}
}
