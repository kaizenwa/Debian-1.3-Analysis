
/*-
# X-BASED TRIANGLES
#
#  xtriangles.c
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
#include "Triangles.h"
#include "triangles.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/triangles.scores"
#endif

/* The following are in TrianglesP.h also */
#define MINTRIANGLES 1

#define MAXTRIANGLES 16
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
static void CallbackTriangles(Widget w, caddr_t clientData,
			      trianglesCallbackStruct * callData);

static void PrintRecord(int size, char *record);
static int  HandleSolved(int counter, int size);
static void PrintState(Widget w, char *prog, int size, int moves,
		       char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Arg  arg[2];
static GameRecord trianglesRecord[MAXTRIANGLES - MINTRIANGLES + 1];
static int  movesDsp = 0;
static char progDsp[64] = "xtriangles";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xtriangles\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
		 "\t[-{border|bd} {color}] [-tile {color}] [-size {int}]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-fg", "*triangles.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*triangles.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*triangles.tileBorder", XrmoptionSepArg, NULL},
	{"-border", "*triangles.tileBorder", XrmoptionSepArg, NULL},
	{"-tile", "*triangles.tileColor", XrmoptionSepArg, NULL},
	{"-size", "*triangles.size", XrmoptionSepArg, NULL},
	{"-username", "*triangles.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, triangles;

	toplevel = XtInitialize(argv[0], "Triangles",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
		(char *) triangles_bits, triangles_width, triangles_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	triangles = XtCreateManagedWidget("triangles", trianglesWidgetClass, toplevel,
					  NULL, 0);
	XtAddCallback(triangles, XtNselectCallback,
		      (XtCallbackProc) CallbackTriangles, (XtPointer) NULL);
	Initialize(triangles);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(triangles), AnyButton, AnyModifier, XtWindow(triangles),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(triangles),
		    XCreateFontCursor(XtDisplay(triangles), XC_crosshair));
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
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNsize, &size,
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
	PrintRecord(size, recordDsp);
	PrintState(XtParent(w), progDsp, size, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackTriangles(Widget w, caddr_t clientData,
		  trianglesCallbackStruct * callData)
{
	int         size;

	XtVaGetValues(w,
		      XtNsize, &size,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case TRIANGLES_RESTORE:
		case TRIANGLES_RESET:
			movesDsp = 0;
			break;
		case TRIANGLES_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case TRIANGLES_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case TRIANGLES_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case TRIANGLES_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case TRIANGLES_SOLVED:
			if (HandleSolved(movesDsp, size))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case TRIANGLES_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case TRIANGLES_DEC:
			movesDsp = 0;
			size--;
			PrintRecord(size, recordDsp);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			break;
		case TRIANGLES_INC:
			movesDsp = 0;
			size++;
			PrintRecord(size, recordDsp);
			XtSetArg(arg[0], XtNsize, size);
			XtSetValues(w, arg, 1);
			break;
		case TRIANGLES_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case TRIANGLES_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
	}
	PrintState(XtParent(w), progDsp, size, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int size, char *record)
{
	int         i = size - MINTRIANGLES;

	if (size > MAXTRIANGLES)
		(void) strcpy(record, "NOT RECORDED");
	else if (trianglesRecord[i].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
			  trianglesRecord[i].score, trianglesRecord[i].name);
}

static int
HandleSolved(int counter, int size)
{
	int         i = size - MINTRIANGLES;

	if (size <= MAXTRIANGLES && counter < trianglesRecord[i].score) {
		trianglesRecord[i].score = counter;
		(void) strcpy(trianglesRecord[i].name, usernameDsp);
		WriteRecords();
		PrintRecord(size, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int size, int moves, char *record, char *message)
{
	(void) sprintf(titleDsp, "%s: %d@ (%d/%s) - %s",
		       prog, size, moves, record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
}

static void
InitRecords(void)
{
	int         i;

	for (i = 0; i < MAXTRIANGLES - MINTRIANGLES + 1; i++) {
		trianglesRecord[i].score = MAXRECORD;
		(void) strcpy(trianglesRecord[i].name, NOACCESS);
	}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
		for (i = 0; i < MAXTRIANGLES - MINTRIANGLES + 1; i++) {
			(void) fscanf(fp, "%d %s\n", &n, username);
			if (n <= trianglesRecord[i].score) {
				trianglesRecord[i].score = n;
				(void) strcpy(trianglesRecord[i].name, username);
			}
		}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i;

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
		for (i = 0; i < MAXTRIANGLES - MINTRIANGLES + 1; i++)
			(void) fprintf(fp, "%d %s\n",
			  trianglesRecord[i].score, trianglesRecord[i].name);
#if HAVE_FCNTL_H
		(void) close(lfd);
		(void) unlink(lockfile);
#endif
		(void) fclose(fp);
	}
}
