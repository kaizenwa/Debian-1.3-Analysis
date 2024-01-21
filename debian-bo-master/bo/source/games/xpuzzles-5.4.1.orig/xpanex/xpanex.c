/*-
# X-BASED PANEX(tm)
#
#  xpanex.c
#
###
#
#  Copyright (c) 1996 - 97		David Albert Bagley, bagleyd@bigfoot.com
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
  Version 5.4: 97/01/01 Xt
  Version 5.3: 96/04/30 Xt
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
#include "Panex.h"
#include "panex.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/panex.scores"
#endif

/* The following are in PanexP.h also */
#define MINTILES 1
#define HANOI 0
#define PANEX 1
#define MAXMODES 2

#define MAXTILES 10
#define MAXRECORD 65536
#define FILENAMELEN 1024
#define USERNAMELEN 128
#define NOACCESS "noaccess"
#define NOBODY "nobody"

typedef struct {
	int         score;
	char        name[USERNAMELEN];
} GameRecord;

static void Initialize(Widget w);
static void CallbackPanex(Widget w,
			  caddr_t clientData, panexCallbackStruct * callData);

static void PrintRecord(int tiles, int mode, char *record);
static int  HandleSolved(int counter, int tiles, int mode);
static void PrintState(Widget w,
	      char *prog, int tiles, int moves, char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Arg  arg[1];
static GameRecord panexRecord[MAXMODES][MAXTILES - MINTILES + 1];
static int  movesDsp = 0;
static char progDsp[MAXMODES][64] =
{"xhanoi", "xpanex"};
static char recordDsp[128] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xpanex\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]] [-mono]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
	"\t[-{border|bd} {color}] [-tile {color}] [-pyramid{0|1} {color}]\n");
	(void) fprintf(stderr,
	    "\t[-tiles {int}] [-{mode {int}|hanoi|panex}] [-delay msecs]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-mono", "*panex.mono", XrmoptionNoArg, "1"},
	{"-fg", "*panex.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*panex.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*panex.tileBorder", XrmoptionSepArg, NULL},
	{"-border", "*panex.tileBorder", XrmoptionSepArg, NULL},
	{"-tile", "*panex.tileColor", XrmoptionSepArg, NULL},
	{"-pyramid0", "*panex.pyramidColor0", XrmoptionSepArg, NULL},
	{"-pyramid1", "*panex.pyramidColor1", XrmoptionSepArg, NULL},
	{"-tiles", "*panex.tiles", XrmoptionSepArg, NULL},
	{"-mode", "*panex.mode", XrmoptionSepArg, NULL},
	{"-hanoi", "*panex.mode", XrmoptionNoArg, "0"},
	{"-panex", "*panex.mode", XrmoptionNoArg, "1"},
	{"-username", "*panex.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, panex;

	toplevel = XtInitialize(argv[0], "Panex",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) panex_bits, panex_width, panex_height));
	XtSetValues(toplevel, arg, 1);
	panex = XtCreateManagedWidget("panex", panexWidgetClass, toplevel,
				      NULL, 0);
	XtAddCallback(panex, XtNselectCallback, (XtCallbackProc) CallbackPanex,
		      (XtPointer) NULL);
	Initialize(panex);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(panex), AnyButton, AnyModifier, XtWindow(panex),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(panex),
		    XCreateFontCursor(XtDisplay(panex), XC_crosshair));
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
	int         tiles, mode;
	String      username;

	/*XtVaSetValues(w,
	   XtNstart, FALSE,
	   NULL); */
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNtiles, &tiles,
		      XtNmode, &mode,
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
	PrintRecord(tiles, mode, recordDsp);
	PrintState(XtParent(w), progDsp[mode], tiles, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackPanex(Widget w, caddr_t clientData, panexCallbackStruct * callData)
{
	int         tiles, mode;

	XtVaGetValues(w,
		      XtNtiles, &tiles,
		      XtNmode, &mode,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case PANEX_RESTORE:
		case PANEX_RESET:
			movesDsp = 0;
			break;
		case PANEX_ILLEGAL:
			(void) strcpy(messageDsp, "Illegal move");
			break;
		case PANEX_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case PANEX_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case PANEX_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case PANEX_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_SOLVED:
			if (HandleSolved(movesDsp, tiles, mode))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_DEC:
			movesDsp = 0;
			tiles--;
			PrintRecord(tiles, mode, recordDsp);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_INC:
			movesDsp = 0;
			tiles++;
			PrintRecord(tiles, mode, recordDsp);
			XtSetArg(arg[0], XtNtiles, tiles);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_MODE:
			movesDsp = 0;
			mode = (mode == PANEX) ? HANOI : PANEX;
			PrintRecord(tiles, mode, recordDsp);
			XtSetArg(arg[0], XtNmode, mode);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case PANEX_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
	}
	PrintState(XtParent(w), progDsp[mode], tiles, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int tiles, int mode, char *record)
{
	int         i = tiles - MINTILES, j = mode - HANOI;

	if (tiles > MAXTILES)
		(void) strcpy(record, "NOT RECORDED");
	else if (panexRecord[j][i].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
			    panexRecord[j][i].score, panexRecord[j][i].name);
}

static int
HandleSolved(int counter, int tiles, int mode)
{
	int         i = tiles - MINTILES, j = mode - HANOI;

	if (tiles <= MAXTILES && counter < panexRecord[j][i].score) {
		panexRecord[j][i].score = counter;
		(void) strcpy(panexRecord[j][i].name, usernameDsp);
		WriteRecords();
		PrintRecord(tiles, mode, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w,
	   char *prog, int tiles, int moves, char *record, char *message)
{
	(void) sprintf(titleDsp, "%s: %d @ (%d/%s) - %s",
		       prog, tiles, moves, record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
}

static void
InitRecords(void)
{
	int         i, mode;

	for (mode = 0; mode < MAXMODES; mode++)
		for (i = 0; i < MAXTILES - MINTILES + 1; i++) {
			panexRecord[mode][i].score = MAXRECORD;
			(void) strcpy(panexRecord[mode][i].name, NOACCESS);
		}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, mode, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
		for (mode = 0; mode < MAXMODES; mode++)
			for (i = 0; i < MAXTILES - MINTILES + 1; i++) {
				(void) fscanf(fp, "%d %s\n", &n, username);
				if (n <= panexRecord[mode][i].score) {
					panexRecord[mode][i].score = n;
					(void) strcpy(panexRecord[mode][i].name, username);
				}
			}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, mode;

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
			(void) fprintf(stderr, "Lock file exists... score not recorded - sorry.\n");
			return;
#endif
		}
#endif
		for (mode = 0; mode < MAXMODES; mode++) {
			for (i = 0; i < MAXTILES - MINTILES + 1; i++)
				(void) fprintf(fp, "%d %s\n",
					       panexRecord[mode][i].score, panexRecord[mode][i].name);
			(void) fprintf(fp, "\n");
		}
#if HAVE_FCNTL_H
		(void) close(lfd);
		(void) unlink(lockfile);
#endif
		(void) fclose(fp);
	}
}
