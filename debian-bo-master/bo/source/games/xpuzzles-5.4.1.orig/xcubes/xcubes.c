
/*-
# X-BASED CUBES
#
#  xcubes.c
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
#include "Cubes.h"
#include "cubes.xbm"

#ifndef SCOREFILE
#define SCOREFILE "/usr/games/lib/cubes.scores"
#endif

/* The following is in CubesP.h also */
#define MINCUBES 1

#define MAXCUBES 8
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
static void CallbackCubes(Widget w, caddr_t clientData, cubesCallbackStruct * callData);

static void PrintRecord(int sizeX, int sizeY, int sizeZ, char *record);
static int  HandleSolved(int counter, int sizeX, int sizeY, int sizeZ);
static void PrintState(Widget w, char *prog, int sizeX, int sizeY, int sizeZ,
		       int moves, char *record, char *message);
static void InitRecords(void);
static void ReadRecords(void);
static void WriteRecords(void);

static Arg  arg[2];
static GameRecord cubesRecord[MAXCUBES - MINCUBES + 1][MAXCUBES - MINCUBES + 1]
[MAXCUBES - MINCUBES + 1];
static int  movesDsp = 0;
static char progDsp[64] = "xcubes";
static char recordDsp[16] = "INF";
static char messageDsp[128] = "Welcome";
static char titleDsp[256] = "";
static char usernameDsp[USERNAMELEN] = "";

static void
Usage(void)
{
	(void) fprintf(stderr, "usage: xcubes\n");
	(void) fprintf(stderr,
	     "\t[-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
	(void) fprintf(stderr,
		       "\t[-display [{host}]:[{vs}]]\n");
	(void) fprintf(stderr,
		"\t[-{foreground|fg} {color}] [-{background|bg} {color}]\n");
	(void) fprintf(stderr,
	 "\t[-{border|bd} {color}] [-brick {color}] [-size{x|y|z} {int}]\n");
	(void) fprintf(stderr,
		       "\t[-username {string}]\n");
	exit(1);
}

static XrmOptionDescRec options[] =
{
	{"-fg", "*cubes.Foreground", XrmoptionSepArg, NULL},
	{"-foreground", "*cubes.Foreground", XrmoptionSepArg, NULL},
	{"-bg", "*Background", XrmoptionSepArg, NULL},
	{"-background", "*Background", XrmoptionSepArg, NULL},
	{"-bd", "*cubes.brickBorder", XrmoptionSepArg, NULL},
	{"-border", "*cubes.brickBorder", XrmoptionSepArg, NULL},
	{"-brick", "*cubes.brickColor", XrmoptionSepArg, NULL},
	{"-sizex", "*cubes.sizeX", XrmoptionSepArg, NULL},
	{"-sizey", "*cubes.sizeY", XrmoptionSepArg, NULL},
	{"-sizez", "*cubes.sizeZ", XrmoptionSepArg, NULL},
	{"-username", "*cubes.userName", XrmoptionSepArg, NULL}
};

int
main(int argc, char **argv)
{
	Widget      toplevel, cubes;

	toplevel = XtInitialize(argv[0], "Cubes",
				options, XtNumber(options), &argc, argv);
	if (argc != 1)
		Usage();

	XtSetArg(arg[0], XtNiconPixmap,
		 XCreateBitmapFromData(XtDisplay(toplevel),
				       RootWindowOfScreen(XtScreen(toplevel)),
			    (char *) cubes_bits, cubes_width, cubes_height));
	XtSetArg(arg[1], XtNinput, True);
	XtSetValues(toplevel, arg, 2);
	cubes = XtCreateManagedWidget("cubes", cubesWidgetClass, toplevel,
				      NULL, 0);
	XtAddCallback(cubes, XtNselectCallback, (XtCallbackProc) CallbackCubes,
		      (XtPointer) NULL);
	Initialize(cubes);
	XtRealizeWidget(toplevel);
	XGrabButton(XtDisplay(cubes), AnyButton, AnyModifier, XtWindow(cubes),
		TRUE, ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, XtWindow(cubes),
		    XCreateFontCursor(XtDisplay(cubes), XC_crosshair));
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
	int         sizeX, sizeY, sizeZ;
	String      username;

	XtVaSetValues(w,
		      XtNstart, FALSE,
		      NULL);
	XtVaGetValues(w,
		      XtNuserName, &username,
		      XtNsizeX, &sizeX,
		      XtNsizeY, &sizeY,
		      XtNsizeZ, &sizeZ,
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
	PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
	PrintState(XtParent(w), progDsp, sizeX, sizeY, sizeZ, movesDsp,
		   recordDsp, messageDsp);
}

static void
CallbackCubes(Widget w, caddr_t clientData, cubesCallbackStruct * callData)
{
	int         sizeX, sizeY, sizeZ;

	XtVaGetValues(w,
		      XtNsizeX, &sizeX,
		      XtNsizeY, &sizeY,
		      XtNsizeZ, &sizeZ,
		      NULL);
	(void) strcpy(messageDsp, "");
	switch (callData->reason) {
		case CUBES_RESTORE:
		case CUBES_RESET:
			movesDsp = 0;
			break;
		case CUBES_BLOCKED:
			(void) strcpy(messageDsp, "Blocked");
			break;
		case CUBES_SPACE:
			/*(void) strcpy(messageDsp, "Spaces can't move"); *//* Too annoying */
			break;
		case CUBES_IGNORE:
			(void) strcpy(messageDsp, "Randomize to start");
			break;
		case CUBES_MOVED:
			movesDsp++;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_SOLVED:
			if (HandleSolved(movesDsp, sizeX, sizeY, sizeZ))
				(void) sprintf(messageDsp, "Congratulations %s!!", usernameDsp);
			else
				(void) strcpy(messageDsp, "Solved!");
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_RANDOMIZE:
			movesDsp = 0;
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_DEC_X:
			movesDsp = 0;
			sizeX--;
			PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
			XtSetArg(arg[0], XtNsizeX, sizeX);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_INC_X:
			movesDsp = 0;
			sizeX++;
			PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
			XtSetArg(arg[0], XtNsizeX, sizeX);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_DEC_Y:
			movesDsp = 0;
			sizeY--;
			PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
			XtSetArg(arg[0], XtNsizeY, sizeY);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_INC_Y:
			movesDsp = 0;
			sizeY++;
			PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
			XtSetArg(arg[0], XtNsizeY, sizeY);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_DEC_Z:
			movesDsp = 0;
			sizeZ--;
			PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
			XtSetArg(arg[0], XtNsizeZ, sizeZ);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_INC_Z:
			movesDsp = 0;
			sizeZ++;
			PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
			XtSetArg(arg[0], XtNsizeZ, sizeZ);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_COMPUTED:
			XtSetArg(arg[0], XtNstart, FALSE);
			XtSetValues(w, arg, 1);
			break;
		case CUBES_UNDO:
			movesDsp--;
			XtSetArg(arg[0], XtNstart, TRUE);
			XtSetValues(w, arg, 1);
			break;
	}
	PrintState(XtParent(w), progDsp, sizeX, sizeY, sizeZ, movesDsp,
		   recordDsp, messageDsp);
}

static void
PrintRecord(int sizeX, int sizeY, int sizeZ, char *record)
{
	int         i = sizeX - MINCUBES, j = sizeY - MINCUBES, k = sizeZ - MINCUBES;

	if (sizeX > MAXCUBES || sizeY > MAXCUBES || sizeZ > MAXCUBES)
		(void) strcpy(record, "NOT RECORDED");
	else if (cubesRecord[i][j][k].score >= MAXRECORD)
		(void) sprintf(record, "NEVER %s", NOACCESS);
	else
		(void) sprintf(record, "%d %s",
		      cubesRecord[i][j][k].score, cubesRecord[i][j][k].name);
}

static int
HandleSolved(int counter, int sizeX, int sizeY, int sizeZ)
{
	int         i = sizeX - MINCUBES, j = sizeY - MINCUBES, k = sizeZ - MINCUBES;

	if (sizeX <= MAXCUBES && sizeY <= MAXCUBES && sizeZ <= MAXCUBES &&
	    counter < cubesRecord[i][j][k].score) {
		cubesRecord[i][j][k].score = cubesRecord[i][k][j].score =
			cubesRecord[j][i][k].score = cubesRecord[j][k][i].score =
			cubesRecord[k][i][j].score = cubesRecord[k][j][i].score =
			counter;
		(void) strcpy(cubesRecord[i][j][k].name, usernameDsp);
		(void) strcpy(cubesRecord[i][k][j].name, usernameDsp);
		(void) strcpy(cubesRecord[j][i][k].name, usernameDsp);
		(void) strcpy(cubesRecord[j][k][i].name, usernameDsp);
		(void) strcpy(cubesRecord[k][i][j].name, usernameDsp);
		(void) strcpy(cubesRecord[k][j][i].name, usernameDsp);
		WriteRecords();
		PrintRecord(sizeX, sizeY, sizeZ, recordDsp);
		return TRUE;
	}
	return FALSE;
}

static void
PrintState(Widget w, char *prog, int sizeX, int sizeY, int sizeZ, int moves, char *record, char *message)
{
	(void) sprintf(titleDsp, "%s: %dx%dx%d@ (%d/%s) - %s",
		       prog, sizeX, sizeY, sizeZ, moves, record, message);
	XtSetArg(arg[0], XtNtitle, titleDsp);
	XtSetValues(w, arg, 1);
}

static void
InitRecords(void)
{
	int         i, j, k;

	for (i = 0; i < MAXCUBES - MINCUBES + 1; i++)
		for (j = i; j < MAXCUBES - MINCUBES + 1; j++)
			for (k = j; k < MAXCUBES - MINCUBES + 1; k++) {
				cubesRecord[k][j][i].score = cubesRecord[k][i][j].score =
					cubesRecord[j][k][i].score = cubesRecord[j][i][k].score =
					cubesRecord[i][k][j].score = cubesRecord[i][j][k].score = MAXRECORD;
				(void) strcpy(cubesRecord[k][j][i].name, NOACCESS);
				(void) strcpy(cubesRecord[k][i][j].name, NOACCESS);
				(void) strcpy(cubesRecord[j][k][i].name, NOACCESS);
				(void) strcpy(cubesRecord[j][i][k].name, NOACCESS);
				(void) strcpy(cubesRecord[i][k][j].name, NOACCESS);
				(void) strcpy(cubesRecord[i][j][k].name, NOACCESS);
			}
}

static void
ReadRecords(void)
{
	FILE       *fp;
	int         i, j, k, n;
	char        username[USERNAMELEN];

	if ((fp = fopen(SCOREFILE, "r")) == NULL)
		(void) sprintf(messageDsp, "Can not open %s, taking defaults.", SCOREFILE);
	else {
		for (i = 0; i < MAXCUBES - MINCUBES + 1; i++)
			for (j = i; j < MAXCUBES - MINCUBES + 1; j++)
				for (k = j; k < MAXCUBES - MINCUBES + 1; k++) {
					(void) fscanf(fp, "%d %s\n", &n, username);
					if (n <= cubesRecord[i][j][k].score) {
						cubesRecord[k][j][i].score = cubesRecord[k][i][j].score =
							cubesRecord[j][k][i].score = cubesRecord[j][i][k].score =
							cubesRecord[i][k][j].score = cubesRecord[i][j][k].score = n;
						(void) strcpy(cubesRecord[k][j][i].name, username);
						(void) strcpy(cubesRecord[k][i][j].name, username);
						(void) strcpy(cubesRecord[j][k][i].name, username);
						(void) strcpy(cubesRecord[j][i][k].name, username);
						(void) strcpy(cubesRecord[i][k][j].name, username);
						(void) strcpy(cubesRecord[i][j][k].name, username);
					}
				}
		(void) fclose(fp);
	}
}

static void
WriteRecords(void)
{
	FILE       *fp;
	int         i, j, k;

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
		for (i = 0; i < MAXCUBES - MINCUBES + 1; i++) {
			for (j = i; j < MAXCUBES - MINCUBES + 1; j++) {
				for (k = j; k < MAXCUBES - MINCUBES + 1; k++)
					(void) fprintf(fp, "%d %s\n",
						       cubesRecord[i][j][k].score, cubesRecord[i][j][k].name);
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
