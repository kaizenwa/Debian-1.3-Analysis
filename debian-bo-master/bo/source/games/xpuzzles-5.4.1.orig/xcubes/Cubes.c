/*-
# X-BASED CUBES
#
#  Cubes.c
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

/* Methods file for Cubes */

#include <stdio.h>
#include <stdlib.h>
#ifdef VMS
#include <unixlib.h>
#else
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "CubesP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/cubes.data"
#endif

static void InitializeCubes(Widget request, Widget new);
static void ExposeCubes(Widget new, XEvent * event, Region region);
static void ResizeCubes(CubesWidget w);
static void DestroyCubes(Widget old);
static Boolean SetValuesCubes(Widget current, Widget request, Widget new);

static void QuitCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static void MoveCubesOut(CubesWidget w, XEvent * event, char **args, int nArgs);
static void MoveCubesTop(CubesWidget w, XEvent * event, char **args, int nArgs);
static void MoveCubesLeft(CubesWidget w, XEvent * event, char **args, int nArgs);
static void MoveCubesIn(CubesWidget w, XEvent * event, char **args, int nArgs);
static void MoveCubesRight(CubesWidget w, XEvent * event, char **args, int nArgs);
static void MoveCubesBottom(CubesWidget w, XEvent * event, char **args, int nArgs);
static void SelectCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static void ReleaseCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeCubesMaybe(CubesWidget w, XEvent * event, char **args, int nArgs);
static void GetCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static void WriteCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static void UndoCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static void SolveCubes(CubesWidget w, XEvent * event, char **args, int nArgs);
static int  MoveCubes(CubesWidget w, int direction, int control);
static int  PositionToBrick(CubesWidget w, int x, int y, int *i, int *j, int *k);
static void SelectBricks(CubesWidget w, int i, int j, int k);
static void CheckBricks(CubesWidget w);
static void ResetBricks(CubesWidget w);
static void ResizeBricks(CubesWidget w);
static void MoveNoBricks(CubesWidget w);
static int  MoveBricksDir(CubesWidget w, int direction);
static void RandomizeBricks(CubesWidget w);
static void MoveBricks(CubesWidget w, int from);
static int  ExchangeBricks(CubesWidget w, int pos1, int pos2);
static void DrawFrame(CubesWidget w, GC gc);
static void DrawBrick(CubesWidget w, GC brickGC, GC borderGC, int pos, int offset);
static int  Row(CubesWidget w, int pos);
static int  Column(CubesWidget w, int pos);
static int  Stack(CubesWidget w, int pos);

static char defaultTranslationsCubes[] =
"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
   <KeyPress>o: MoveOut()\n\
   <KeyPress>KP_Divide: MoveOut()\n\
   <KeyPress>R5: MoveOut()\n\
   <KeyPress>Up: MoveTop()\n\
   <KeyPress>KP_8: MoveTop()\n\
   <KeyPress>R8: MoveTop()\n\
   <KeyPress>Left: MoveLeft()\n\
   <KeyPress>KP_4: MoveLeft()\n\
   <KeyPress>R10: MoveLeft()\n\
   <KeyPress>i: MoveIn()\n\
   <KeyPress>Begin: MoveIn()\n\
   <KeyPress>KP_5: MoveIn()\n\
   <KeyPress>R11: MoveIn()\n\
   <KeyPress>Right: MoveRight()\n\
   <KeyPress>KP_6: MoveRight()\n\
   <KeyPress>R12: MoveRight()\n\
   <KeyPress>Down: MoveBottom()\n\
   <KeyPress>KP_2: MoveBottom()\n\
   <KeyPress>R14: MoveBottom()\n\
   <Btn1Down>: Select()\n\
   <Btn1Up>: Release()\n\
   <KeyPress>r: Randomize()\n\
   <Btn3Down>(2+): Randomize()\n\
   <Btn3Down>: RandomizeMaybe()\n\
   <KeyPress>g: Get()\n\
   <KeyPress>w: Write()\n\
   <KeyPress>u: Undo()\n\
   <KeyPress>s: Solve()";

static XtActionsRec actionsListCubes[] =
{
	{"Quit", (XtActionProc) QuitCubes},
	{"MoveOut", (XtActionProc) MoveCubesOut},
	{"MoveTop", (XtActionProc) MoveCubesTop},
	{"MoveLeft", (XtActionProc) MoveCubesLeft},
	{"MoveIn", (XtActionProc) MoveCubesIn},
	{"MoveRight", (XtActionProc) MoveCubesRight},
	{"MoveBottom", (XtActionProc) MoveCubesBottom},
	{"Select", (XtActionProc) SelectCubes},
	{"Release", (XtActionProc) ReleaseCubes},
	{"Randomize", (XtActionProc) RandomizeCubes},
	{"RandomizeMaybe", (XtActionProc) RandomizeCubesMaybe},
	{"Get", (XtActionProc) GetCubes},
	{"Write", (XtActionProc) WriteCubes},
	{"Undo", (XtActionProc) UndoCubes},
	{"Solve", (XtActionProc) SolveCubes}
};

static XtResource resourcesCubes[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(CubesWidget, cubes.username), XtRString, "nobody"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
    XtOffset(CubesWidget, cubes.foreground), XtRString, XtDefaultForeground},
	{XtNbrickColor, XtCColor, XtRPixel, sizeof (Pixel),
    XtOffset(CubesWidget, cubes.brickColor), XtRString, XtDefaultForeground},
	{XtNbrickBorder, XtCColor, XtRPixel, sizeof (Pixel),
   XtOffset(CubesWidget, cubes.borderColor), XtRString, XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(CubesWidget, core.width), XtRString, "100"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(CubesWidget, core.height), XtRString, "300"},
	{XtNsizeX, XtCSizeX, XtRInt, sizeof (int),
	 XtOffset(CubesWidget, cubes.sizeX), XtRString, "3"},	/* DEFAULTCUBES */
	{XtNsizeY, XtCSizeY, XtRInt, sizeof (int),
	 XtOffset(CubesWidget, cubes.sizeY), XtRString, "3"},	/* DEFAULTCUBES */
	{XtNsizeZ, XtCSizeZ, XtRInt, sizeof (int),
	 XtOffset(CubesWidget, cubes.sizeZ), XtRString, "3"},	/* DEFAULTCUBES */
	{XtNbase, XtCBase, XtRInt, sizeof (int),
	 XtOffset(CubesWidget, cubes.base), XtRString, "10"},
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(CubesWidget, cubes.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(CubesWidget, cubes.select), XtRCallback, NULL}
};

CubesClassRec cubesClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Cubes",	/* class name */
		sizeof (CubesRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeCubes,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListCubes,	/* actions */
		XtNumber(actionsListCubes),	/* num actions */
		resourcesCubes,	/* resources */
		XtNumber(resourcesCubes),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		(XtWidgetProc) DestroyCubes,	/* destroy */
		(XtWidgetProc) ResizeCubes,	/* resize */
		(XtExposeProc) ExposeCubes,	/* expose */
		(XtSetValuesFunc) SetValuesCubes,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		NULL,		/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsCubes,	/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	}
};

WidgetClass cubesWidgetClass = (WidgetClass) & cubesClassRec;

static void
InitializeCubes(Widget request, Widget new)
{
	CubesWidget w = (CubesWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;

	w->cubes.brickOfPosition = NULL;
	CheckBricks(w);
	InitMoves();
	ResetBricks(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.foreground = w->cubes.foreground;
	values.background = w->core.background_pixel;
	w->cubes.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->cubes.brickColor;
	w->cubes.brickGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->cubes.borderColor;
	w->cubes.borderGC = XtGetGC(new, valueMask, &values);
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->cubes.foreground;
	w->cubes.inverseGC = XtGetGC(new, valueMask, &values);
	ResizeCubes(w);
}

static void
DestroyCubes(Widget old)
{
	CubesWidget w = (CubesWidget) old;

	XtReleaseGC(old, w->cubes.brickGC);
	XtReleaseGC(old, w->cubes.borderGC);
	XtReleaseGC(old, w->cubes.puzzleGC);
	XtReleaseGC(old, w->cubes.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->cubes.select);
}

static void
ResizeCubes(CubesWidget w)
{
	w->cubes.delta.x = 3;
	w->cubes.delta.y = 3;
	w->cubes.offset.x = MAX(((int) w->core.width - w->cubes.delta.x) /
				w->cubes.sizeX, 0);
	w->cubes.offset.y = MAX(((int) w->core.height - w->cubes.delta.y) /
				w->cubes.sizeY, 0);
	if (w->cubes.offset.y >= w->cubes.offset.x) {
		w->cubes.vertical = TRUE;
		w->cubes.offset.y = MAX(((int) w->core.height / w->cubes.sizeZ -
				      w->cubes.delta.y) / w->cubes.sizeY, 0);
	} else {
		w->cubes.vertical = FALSE;
		w->cubes.offset.x = MAX(((int) w->core.width / w->cubes.sizeZ -
				      w->cubes.delta.x) / w->cubes.sizeX, 0);
	}
	w->cubes.faceSize.x = w->cubes.offset.x * w->cubes.sizeX +
		w->cubes.delta.x + 2;
	w->cubes.faceSize.y = w->cubes.offset.y * w->cubes.sizeY +
		w->cubes.delta.y + 2;
	if (w->cubes.vertical) {
		w->cubes.puzzleSize.x = w->cubes.faceSize.x;
		w->cubes.puzzleSize.y = (w->cubes.faceSize.y - w->cubes.delta.y) *
			w->cubes.sizeZ + w->cubes.delta.y;
	} else {
		w->cubes.puzzleSize.x = (w->cubes.faceSize.x - w->cubes.delta.x) *
			w->cubes.sizeZ + w->cubes.delta.x;
		w->cubes.puzzleSize.y = w->cubes.faceSize.y;
	}
	w->cubes.puzzleOffset.x = ((int) w->core.width -
				   w->cubes.puzzleSize.x + 2) / 2;
	w->cubes.puzzleOffset.y = ((int) w->core.height -
				   w->cubes.puzzleSize.y + 2) / 2;
	w->cubes.brickSize.x = MAX(w->cubes.offset.x - w->cubes.delta.x, 0);
	w->cubes.brickSize.y = MAX(w->cubes.offset.y - w->cubes.delta.y, 0);
	ResizeBricks(w);
}

static void
ExposeCubes(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	CubesWidget w = (CubesWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->cubes.puzzleGC);
		DrawAllBricks(w, w->cubes.brickGC, w->cubes.borderGC);
	}
}

static      Boolean
SetValuesCubes(Widget current, Widget request, Widget new)
{
	CubesWidget c = (CubesWidget) current, w = (CubesWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	Boolean     redrawBricks = FALSE;

	CheckBricks(w);
	if (w->cubes.foreground != c->cubes.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->cubes.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->cubes.puzzleGC);
		w->cubes.puzzleGC = XtGetGC(new, valueMask, &values);
		redrawBricks = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->cubes.foreground;
		XtReleaseGC(new, w->cubes.inverseGC);
		w->cubes.inverseGC = XtGetGC(new, valueMask, &values);
		redrawBricks = TRUE;
	}
	if (w->cubes.brickColor != c->cubes.brickColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->cubes.brickColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->cubes.brickGC);
		w->cubes.brickGC = XtGetGC(new, valueMask, &values);
		redrawBricks = TRUE;
	}
	if (w->cubes.borderColor != c->cubes.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->cubes.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->cubes.borderGC);
		w->cubes.borderGC = XtGetGC(new, valueMask, &values);
		redrawBricks = TRUE;
	}
	if (w->cubes.sizeX != c->cubes.sizeX ||
	    w->cubes.sizeY != c->cubes.sizeY ||
	    w->cubes.sizeZ != c->cubes.sizeZ ||
	    w->cubes.base != c->cubes.base) {
		ResetBricks(w);
		ResizeCubes(w);
		redraw = TRUE;
	} else if (w->cubes.offset.x != c->cubes.offset.x ||
		   w->cubes.offset.y != c->cubes.offset.y) {
		ResizeCubes(w);
		redraw = TRUE;
	}
	if (redrawBricks && !redraw && XtIsRealized(new) && new->core.visible) {
		DrawFrame(c, c->cubes.inverseGC);
		DrawAllBricks(c, c->cubes.inverseGC, c->cubes.inverseGC);
		DrawFrame(w, w->cubes.puzzleGC);
		DrawAllBricks(w, w->cubes.brickGC, w->cubes.borderGC);
	}
	return (redraw);
}

static void
QuitCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	int         pos, i, j, k;

	pos = PositionToBrick(w, event->xbutton.x, event->xbutton.y, &i, &j, &k);
	if (pos >= 0) {
		if (CheckSolved(w)) {
			MoveNoBricks(w);
			w->cubes.currentPosition = -1;
			return;
		}
		w->cubes.currentPosition = pos;
		w->cubes.currentRow[0] = i;
		w->cubes.currentRow[1] = j;
		w->cubes.currentRow[2] = k;
		if (pos != w->cubes.spacePosition)
			DrawBrick(w, w->cubes.borderGC, w->cubes.brickGC,
				  w->cubes.currentPosition, TRUE);
		else
			w->cubes.currentPosition = -1;
	} else
		w->cubes.currentPosition = -1;
}

static void
ReleaseCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	if (w->cubes.currentPosition == -1)
		return;
	DrawBrick(w, w->cubes.inverseGC, w->cubes.inverseGC,
		  w->cubes.currentPosition, TRUE);
	DrawBrick(w, w->cubes.brickGC, w->cubes.borderGC,
		  w->cubes.currentPosition, FALSE);
	SelectBricks(w, w->cubes.currentRow[0], w->cubes.currentRow[1],
		     w->cubes.currentRow[2]);
	w->cubes.currentPosition = -1;
}

static void
RandomizeCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizeBricks(w);
}

static void
RandomizeCubesMaybe(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->cubes.started)
		RandomizeBricks(w);
}

static void
GetCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, sizeX, sizeY, sizeZ, moves;
	cubesCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &sizeX);
		if (sizeX >= MINCUBES) {
			for (i = w->cubes.sizeX; i < sizeX; i++) {
				cb.reason = CUBES_INC_X;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->cubes.sizeX; i > sizeX; i--) {
				cb.reason = CUBES_DEC_X;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: sizeX %d should be between %d and MAXINT\n",
				      DATAFILE, sizeX, MINCUBES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &sizeY);
		if (sizeY >= MINCUBES) {
			for (i = w->cubes.sizeY; i < sizeY; i++) {
				cb.reason = CUBES_INC_Y;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->cubes.sizeY; i > sizeY; i--) {
				cb.reason = CUBES_DEC_Y;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: sizeY %d should be between %d and MAXINT\n",
				      DATAFILE, sizeY, MINCUBES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &sizeZ);
		if (sizeZ >= MINCUBES) {
			for (i = w->cubes.sizeZ; i < sizeZ; i++) {
				cb.reason = CUBES_INC_Z;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->cubes.sizeZ; i > sizeZ; i--) {
				cb.reason = CUBES_DEC_Z;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: sizeZ %d should be between %d and MAXINT\n",
				      DATAFILE, sizeZ, MINCUBES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = CUBES_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		SetStartPosition(w);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: sizeX %d, sizeY %d, sizeZ %d, moves %d.\n",
			      DATAFILE, sizeX, sizeY, sizeZ, moves);
	}
}

static void
WriteCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "sizeX%c %d\n", SYMBOL, w->cubes.sizeX);
		(void) fprintf(fp, "sizeY%c %d\n", SYMBOL, w->cubes.sizeY);
		(void) fprintf(fp, "sizeZ%c %d\n", SYMBOL, w->cubes.sizeZ);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         direction;

		GetMove(&direction);
		direction = (direction < 4) ? (direction + 2) % 4 :
			((direction == IN) ? OUT : IN);

		if (MoveBricksDir(w, direction)) {
			cubesCallbackStruct cb;

			cb.reason = CUBES_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolveCubes(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolveBricks(w); *//* Sorry, unimplemented */
}

static void
MoveCubesOut(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveCubes(w, OUT, (int) (event->xkey.state & ControlMask));
}

static void
MoveCubesTop(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveCubes(w, TOP, (int) (event->xkey.state & ControlMask));
}

static void
MoveCubesLeft(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveCubes(w, LEFT, (int) (event->xkey.state & ControlMask));
}

static void
MoveCubesIn(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveCubes(w, IN, (int) (event->xkey.state & ControlMask));
}

static void
MoveCubesRight(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveCubes(w, RIGHT, (int) (event->xkey.state & ControlMask));
}

static void
MoveCubesBottom(CubesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveCubes(w, BOTTOM, (int) (event->xkey.state & ControlMask));
}

static int
MoveCubes(CubesWidget w, int direction, int control)
{
	cubesCallbackStruct cb;

	if (control) {
		cb.reason = CUBES_IGNORE;
		switch (direction) {
			case TOP:
				if (w->cubes.sizeY <= MINCUBES)
					return FALSE;
				cb.reason = CUBES_DEC_Y;
				break;
			case RIGHT:
				cb.reason = CUBES_INC_X;
				break;
			case BOTTOM:
				cb.reason = CUBES_INC_Y;
				break;
			case LEFT:
				if (w->cubes.sizeX <= MINCUBES)
					return FALSE;
				cb.reason = CUBES_DEC_X;
				break;
			case IN:
				if (w->cubes.sizeZ <= MINCUBES)
					return FALSE;
				cb.reason = CUBES_DEC_Z;
				break;
			case OUT:
				cb.reason = CUBES_INC_Z;
				break;
			default:
				(void) printf("MoveCubes: direction %d\n", direction);
		}
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return FALSE;
	}
	if (CheckSolved(w)) {
		MoveNoBricks(w);
		return FALSE;
	}
	if (!MoveCubesDir(w, direction)) {
		cb.reason = CUBES_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return FALSE;
	}
	if (CheckSolved(w)) {
		cb.reason = CUBES_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	return TRUE;
}

int
MoveCubesDir(CubesWidget w, int direction)
{
	cubesCallbackStruct cb;

	if (MoveBricksDir(w, direction)) {
		cb.reason = CUBES_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		PutMove(direction);
		return TRUE;
	}
	return FALSE;
}

static int
PositionToBrick(CubesWidget w, int x, int y, int *i, int *j, int *k)
{
	if (w->cubes.vertical) {
		*i = (x - w->cubes.delta.x / 2 -
		      w->cubes.puzzleOffset.x) / w->cubes.offset.x;
		*j = ((y - w->cubes.delta.y / 2 -
		       w->cubes.puzzleOffset.y) % (w->cubes.sizeY * w->cubes.offset.y +
				  w->cubes.delta.y - 1)) / w->cubes.offset.y;
		*k = (y - w->cubes.delta.y / 2 -
		      w->cubes.puzzleOffset.y) / (w->cubes.sizeY * w->cubes.offset.y +
						  w->cubes.delta.y - 1);
	} else {
		*i = ((x - w->cubes.delta.x / 2 -
		       w->cubes.puzzleOffset.x) % (w->cubes.sizeX * w->cubes.offset.x +
				  w->cubes.delta.x - 1)) / w->cubes.offset.x;
		*j = (y - w->cubes.delta.y / 2 -
		      w->cubes.puzzleOffset.y) / w->cubes.offset.y;
		*k = (x - w->cubes.delta.x / 2 -
		      w->cubes.puzzleOffset.x) / (w->cubes.sizeX * w->cubes.offset.x +
						  w->cubes.delta.x - 1);
	}
	if (*i >= 0 && *j >= 0 && *k >= 0 &&
	    *i < w->cubes.sizeX && *j < w->cubes.sizeY && *k < w->cubes.sizeZ)
		return (*i + w->cubes.sizeX * *j + w->cubes.sizeRect * *k);
	else
		return -1;
}

static void
SelectBricks(CubesWidget w, int i, int j, int k)
{
	cubesCallbackStruct cb;
	int         l, m, n;

	l = i + w->cubes.sizeX * j - w->cubes.spacePosition % w->cubes.sizeRect;
	m = i + w->cubes.sizeX * j + w->cubes.sizeRect * k - w->cubes.spacePosition;
	/* Order important if w->cubes.sizeX = 1 */
	if (l % w->cubes.sizeX == 0 && k == Stack(w, w->cubes.spacePosition)) {
		if (l < 0) {
			for (n = 1; n <= -l / w->cubes.sizeX; n++) {
				MoveBricks(w, w->cubes.spacePosition - w->cubes.sizeX);
				cb.reason = CUBES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				PutMove(BOTTOM);
			}
		} else if (l > 0) {
			for (n = 1; n <= l / w->cubes.sizeX; n++) {
				MoveBricks(w, w->cubes.spacePosition + w->cubes.sizeX);
				cb.reason = CUBES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				PutMove(TOP);
			}
		} else {	/* (l == 0) */
			cb.reason = CUBES_SPACE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			return;
		}
	} else if (l / w->cubes.sizeX == 0 &&
		   j == Column(w, w->cubes.spacePosition) &&
		   k == Stack(w, w->cubes.spacePosition)) {
		if (l < 0) {
			for (n = 1; n <= -l % w->cubes.sizeX; n++) {
				MoveBricks(w, w->cubes.spacePosition - 1);
				cb.reason = CUBES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				PutMove(RIGHT);
			}
		} else {	/* (l > 0) */
			for (n = 1; n <= l % w->cubes.sizeX; n++) {
				MoveBricks(w, w->cubes.spacePosition + 1);
				cb.reason = CUBES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				PutMove(LEFT);
			}
		}
	} else if (m % w->cubes.sizeRect == 0) {
		if (m < 0) {
			for (n = 1; n <= -Stack(w, m); n++) {
				MoveBricks(w, w->cubes.spacePosition - w->cubes.sizeRect);
				cb.reason = CUBES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				PutMove(IN);
			}
		} else {	/* (m > 0) */
			for (n = 1; n <= Stack(w, m); n++) {
				MoveBricks(w, w->cubes.spacePosition + w->cubes.sizeRect);
				cb.reason = CUBES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				PutMove(OUT);
			}
		}
	} else {
		cb.reason = CUBES_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return;
	}
	if (CheckSolved(w)) {
		cb.reason = CUBES_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
CheckBricks(CubesWidget w)
{
	char        buf[121];

	if (w->cubes.sizeX < MINCUBES) {
		(void) sprintf(buf,
			       "Number of Cubes in X direction out of bounds, use %d..MAXINT",
			       MINCUBES);
		XtWarning(buf);
		w->cubes.sizeX = DEFAULTCUBES;
	}
	if (w->cubes.sizeY < MINCUBES) {
		(void) sprintf(buf,
			       "Number of Cubes in Y direction out of bounds, use %d..MAXINT",
			       MINCUBES);
		XtWarning(buf);
		w->cubes.sizeY = DEFAULTCUBES;
	}
	if (w->cubes.sizeZ < MINCUBES) {
		(void) sprintf(buf,
			       "Number of Cubes in Z direction out of bounds, use %d..MAXINT",
			       MINCUBES);
		XtWarning(buf);
		w->cubes.sizeZ = DEFAULTCUBES;
	}
	w->cubes.base = 10;
}

static void
ResetBricks(CubesWidget w)
{
	int         i;

	w->cubes.sizeRect = w->cubes.sizeX * w->cubes.sizeY;
	w->cubes.sizeBlock = w->cubes.sizeRect * w->cubes.sizeZ;
	if (w->cubes.brickOfPosition)
		(void) free((void *) w->cubes.brickOfPosition);
	if (!(w->cubes.brickOfPosition = (int *)
	      malloc(sizeof (int) * w->cubes.sizeBlock)))
		            XtError("Not enough memory, exiting.");

	if (startPosition)
		(void) free((void *) startPosition);
	if (!(startPosition = (int *)
	      malloc(sizeof (int) * w->cubes.sizeBlock)))
		            XtError("Not enough memory, exiting.");

	w->cubes.spacePosition = w->cubes.sizeBlock - 1;
	w->cubes.brickOfPosition[w->cubes.sizeBlock - 1] = 0;
	for (i = 1; i < w->cubes.sizeBlock; i++)
		w->cubes.brickOfPosition[i - 1] = i;
	FlushMoves(w);
	w->cubes.currentPosition = -1;
	w->cubes.started = FALSE;
}

static void
ResizeBricks(CubesWidget w)
{
	w->cubes.digitOffset.x = 3;
	w->cubes.digitOffset.y = 4;
}

static void
MoveNoBricks(CubesWidget w)
{
	cubesCallbackStruct cb;

	cb.reason = CUBES_IGNORE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static int
MoveBricksDir(CubesWidget w, int direction)
{
	switch (direction) {
		case TOP:
			if (Column(w, w->cubes.spacePosition) < w->cubes.sizeY - 1) {
				MoveBricks(w, w->cubes.spacePosition + w->cubes.sizeX);
				return TRUE;
			}
			break;
		case RIGHT:
			if (Row(w, w->cubes.spacePosition) > 0) {
				MoveBricks(w, w->cubes.spacePosition - 1);
				return TRUE;
			}
			break;
		case BOTTOM:
			if (Column(w, w->cubes.spacePosition) > 0) {
				MoveBricks(w, w->cubes.spacePosition - w->cubes.sizeX);
				return TRUE;
			}
			break;
		case LEFT:
			if (Row(w, w->cubes.spacePosition) < w->cubes.sizeX - 1) {
				MoveBricks(w, w->cubes.spacePosition + 1);
				return TRUE;
			}
			break;
		case IN:
			if (Stack(w, w->cubes.spacePosition) > 0) {
				MoveBricks(w, w->cubes.spacePosition - w->cubes.sizeRect);
				return TRUE;
			}
			break;
		case OUT:
			if (Stack(w, w->cubes.spacePosition) < w->cubes.sizeZ - 1) {
				MoveBricks(w, w->cubes.spacePosition + w->cubes.sizeRect);
				return TRUE;
			}
			break;
		default:
			(void) printf("MoveBricksDir: direction %d\n", direction);
	}
	return FALSE;
}

static void
RandomizeBricks(CubesWidget w)
{
	cubesCallbackStruct cb;

	/* First interchange bricks an even number of times */
	if (w->cubes.sizeX > 1 && w->cubes.sizeY > 1 && w->cubes.sizeZ > 1 &&
	    w->cubes.sizeBlock > 4) {
		int         pos, count = 0;

		for (pos = 0; pos < w->cubes.sizeBlock; pos++) {
			int         randomPos = pos;

			while (randomPos == pos)
				randomPos = NRAND(w->cubes.sizeBlock);
			count += ExchangeBricks(w, pos, randomPos);
		}
		if (count % 2)
			if (!ExchangeBricks(w, 0, 1))
				if (!ExchangeBricks(w, w->cubes.sizeBlock - 2, w->cubes.sizeBlock - 1))
					(void) printf("RandomizeBricks: should not get here\n");
		DrawAllBricks(w, w->cubes.brickGC, w->cubes.borderGC);
	}
	/* Now move the space around randomly */
	if (w->cubes.sizeX > 1 || w->cubes.sizeY > 1 || w->cubes.sizeZ > 1) {
		int         big = w->cubes.sizeBlock + NRAND(2);
		int         lastDirection = 0;
		int         randomDirection;

		cb.reason = CUBES_RESET;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
		big = 3;
#endif

		if (big > 1000)
			big = 1000;
		while (big--) {
			randomDirection = NRAND(COORD);

#ifdef DEBUG
			sleep(1);
#endif

			if ((randomDirection + COORD / 2) % COORD != lastDirection ||
			    (w->cubes.sizeX == 1 && w->cubes.sizeY == 1) ||
			    (w->cubes.sizeY == 1 && w->cubes.sizeZ == 1) ||
			    (w->cubes.sizeZ == 1 && w->cubes.sizeX == 1)) {
				if (MoveCubesDir(w, randomDirection))
					lastDirection = randomDirection;
				else
					big++;
			}
		}
		FlushMoves(w);
		cb.reason = CUBES_RANDOMIZE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	if (CheckSolved(w)) {
		cb.reason = CUBES_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MoveBricks(CubesWidget w, int from)
{
	int         tempBrick;

	tempBrick = w->cubes.brickOfPosition[from];
	w->cubes.brickOfPosition[from] =
		w->cubes.brickOfPosition[w->cubes.spacePosition];
	w->cubes.brickOfPosition[w->cubes.spacePosition] = tempBrick;
	DrawBrick(w, w->cubes.brickGC, w->cubes.borderGC, w->cubes.spacePosition,
		  FALSE);
	w->cubes.spacePosition = from;
	DrawBrick(w, w->cubes.inverseGC, w->cubes.inverseGC, w->cubes.spacePosition,
		  FALSE);
}

static int
ExchangeBricks(CubesWidget w, int pos1, int pos2)
{
	int         tempBrick;

	if (w->cubes.brickOfPosition[pos1] <= 0)
		return FALSE;
	else if (w->cubes.brickOfPosition[pos2] <= 0)
		return FALSE;
	tempBrick = w->cubes.brickOfPosition[pos1];
	w->cubes.brickOfPosition[pos1] = w->cubes.brickOfPosition[pos2];
	w->cubes.brickOfPosition[pos2] = tempBrick;
	return TRUE;
}

static void
DrawFrame(CubesWidget w, GC gc)
{
	int         sumX, sumY, sumZ, offsetX, offsetY, dz, k;

	sumX = w->cubes.sizeX * w->cubes.offset.x + w->cubes.delta.x / 2 + 1;
	sumY = w->cubes.sizeY * w->cubes.offset.y + w->cubes.delta.y / 2 + 1;
	offsetX = w->cubes.puzzleOffset.x;
	offsetY = w->cubes.puzzleOffset.y;
	if (w->cubes.vertical) {
		sumZ = offsetY;
		dz = sumY;
	} else {
		sumZ = offsetX;
		dz = sumX;
	}
	for (k = 0; k < w->cubes.sizeZ; k++) {
		if (w->cubes.vertical) {
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       offsetX, sumZ, 1, sumY);
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       offsetX, sumZ, sumX, 1);
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       sumX + offsetX, sumZ, 1, sumY + 1);
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       offsetX, sumY + sumZ, sumX + 1, 1);
		} else {
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       sumZ, offsetY, 1, sumY);
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       sumZ, offsetY, sumX, 1);
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       sumX + sumZ, offsetY, 1, sumY + 1);
			XFillRectangle(XtDisplay(w), XtWindow(w), gc,
				       sumZ, sumY + offsetY, sumX + 1, 1);
		}
		sumZ += dz;
	}
}

void
DrawAllBricks(CubesWidget w, GC brickGC, GC borderGC)
{
	int         k;

	for (k = 0; k < w->cubes.sizeBlock; k++)
		if (w->cubes.brickOfPosition[k] <= 0)
			DrawBrick(w, w->cubes.inverseGC, w->cubes.inverseGC, k, FALSE);
		else
			DrawBrick(w, brickGC, borderGC, k, FALSE);
}

static void
DrawBrick(CubesWidget w, GC brickGC, GC borderGC, int pos, int offset)
{
	int         dx, dy, dz;

	if (w->cubes.vertical) {
		dx = Row(w, pos) * w->cubes.offset.x + w->cubes.delta.x +
			w->cubes.puzzleOffset.x + offset;
		dy = Column(w, pos) * w->cubes.offset.y + w->cubes.delta.y +
			w->cubes.puzzleOffset.y + offset;
		dz = Stack(w, pos) * (w->cubes.sizeY * w->cubes.offset.y +
				      w->cubes.delta.y - 1);
		dy += dz;
	} else {
		dx = Row(w, pos) * w->cubes.offset.x + w->cubes.delta.x +
			w->cubes.puzzleOffset.x + offset;
		dy = Column(w, pos) * w->cubes.offset.y + w->cubes.delta.y +
			w->cubes.puzzleOffset.y + offset;
		dz = Stack(w, pos) * (w->cubes.sizeX * w->cubes.offset.x +
				      w->cubes.delta.x - 1);
		dx += dz;
	}
	XFillRectangle(XtDisplay(w), XtWindow(w), brickGC, dx, dy,
		       w->cubes.brickSize.x, w->cubes.brickSize.y);
	XDrawRectangle(XtDisplay(w), XtWindow(w), borderGC, dx, dy,
		       w->cubes.brickSize.x, w->cubes.brickSize.y);
	if (brickGC != w->cubes.inverseGC || borderGC != w->cubes.inverseGC) {
		int         i = 0, offsetX = 0, brick = w->cubes.brickOfPosition[pos];
		char        buf[5];

		(void) sprintf(buf, "%d", brick);
		while (brick >= 1) {
			brick /= w->cubes.base;
			offsetX += w->cubes.digitOffset.x;
			i++;
		}
		XDrawString(XtDisplay(w), XtWindow(w), w->cubes.inverseGC,
			    dx + w->cubes.brickSize.x / 2 - offsetX,
			    dy + w->cubes.brickSize.y / 2 + w->cubes.digitOffset.y, buf, i);
	}
}

static int
Row(CubesWidget w, int pos)
{
	return ((pos % w->cubes.sizeRect) % w->cubes.sizeX);
}

static int
Column(CubesWidget w, int pos)
{
	return ((pos % w->cubes.sizeRect) / w->cubes.sizeX);
}

static int
Stack(CubesWidget w, int pos)
{
	return (pos / w->cubes.sizeRect);
}

Boolean
CheckSolved(CubesWidget w)
{
	int         i;

	for (i = 1; i < w->cubes.sizeBlock; i++)
		if (w->cubes.brickOfPosition[i - 1] != i)
			return FALSE;
	return TRUE;
}
