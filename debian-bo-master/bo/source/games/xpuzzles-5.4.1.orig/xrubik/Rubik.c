
/*-
# X-BASED RUBIK'S CUBE(tm)
#
#  Rubik.c
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

/* Methods file for Rubik */

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
#include "RubikP.h"
#include "Rubik2dP.h"
#include "Rubik3dP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/rubik.data"
#endif

static void InitializeRubik(Widget request, Widget new);
static void DestroyRubik(Widget old);
static Boolean SetValuesRubik(Widget current, Widget request, Widget new);
static void GetColor(RubikWidget w, int face, int init);
static void MoveControlCb(RubikWidget w, int face, int direction);
static void CheckPolyhedrons(RubikWidget w);
static int  SelectPolyhedrons(RubikWidget w,
			      int x, int y, int *face, int *position);
static int  NarrowSelection(RubikWidget w,
			    int *face, int *position, int *direction);
static int  PositionPolyhedrons(RubikWidget w,
		     int x, int y, int *face, int *position, int *direction);
static void MoveNoPolyhedrons(RubikWidget w);
static void PracticePolyhedrons(RubikWidget w);
static void RandomizePolyhedrons(RubikWidget w);
static void MovePolyhedrons(RubikWidget w,
			    int face, int position, int direction);

/* rc : row or column */
static void ReadRc(RubikWidget w, int face, int dir, int h, int orient);
static void RotateRc(RubikWidget w, int rotate, int orient);
static void ReverseRc(RubikWidget w, int orient);
static void WriteRc(RubikWidget w, int face, int dir, int h, int orient);
static void RotateFace(RubikWidget w, int face, int direction);
static void DrawSquare(RubikWidget w, int face, int position, int offset);
static int  CheckMoveDir(RubikWidget w,
			 int position1, int position2, int *direction);

RubikClassRec rubikClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Rubik",	/* class name */
		sizeof (RubikRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeRubik,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		NULL,		/* actions */
		0,		/* num actions */
		NULL,		/* resources */
		0,		/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		(XtWidgetProc) DestroyRubik,	/* destroy */
		NULL,		/* resize */
		NULL,		/* expose */
		(XtSetValuesFunc) SetValuesRubik,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		NULL,		/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		NULL,		/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	}
};

WidgetClass rubikWidgetClass = (WidgetClass) & rubikClassRec;

static RubikLoc slideNextRow[MAXFACES][MAXORIENT] =
{
	{
		{5, STRT},
		{3, CW},
		{2, STRT},
		{1, CCW}},
	{
		{0, CW},
		{2, STRT},
		{4, CCW},
		{5, HALF}},
	{
		{0, STRT},
		{3, STRT},
		{4, STRT},
		{1, STRT}},
	{
		{0, CCW},
		{5, HALF},
		{4, CW},
		{2, STRT}},
	{
		{2, STRT},
		{3, CCW},
		{5, STRT},
		{1, CW}},
	{
		{4, STRT},
		{3, HALF},
		{0, STRT},
		{1, HALF}}
};
static int  rowToRotate[MAXFACES][MAXORIENT] =
{
	{3, 2, 1, 5},
	{2, 4, 5, 0},
	{3, 4, 1, 0},
	{5, 4, 2, 0},
	{3, 5, 1, 2},
	{3, 0, 1, 4}
};

static void
InitializeRubik(Widget request, Widget new)
{
	RubikWidget w = (RubikWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         face, orient;

	for (face = 0; face < MAXFACES; face++)
		w->rubik.cubeLoc[face] = NULL;
	for (orient = 0; orient < MAXORIENT; orient++)
		w->rubik.rowLoc[orient] = NULL;
	w->rubik.faceLoc = NULL;
	CheckPolyhedrons(w);
	InitMoves();
	ResetPolyhedrons(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	values.foreground = w->rubik.foreground;
	w->rubik.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->rubik.borderColor;
	w->rubik.borderGC = XtGetGC(new, valueMask, &values);
	w->rubik.depth = DefaultDepthOfScreen(XtScreen(w));
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->rubik.foreground;
	w->rubik.inverseGC = XtGetGC(new, valueMask, &values);
	for (face = 0; face < MAXFACES; face++)
		GetColor(w, face, TRUE);
}

static void
DestroyRubik(Widget old)
{
	RubikWidget w = (RubikWidget) old;
	int         face;

	for (face = 0; face < MAXFACES; face++)
		XtReleaseGC(old, w->rubik.faceGC[face]);
	XtReleaseGC(old, w->rubik.borderGC);
	XtReleaseGC(old, w->rubik.puzzleGC);
	XtReleaseGC(old, w->rubik.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->rubik.select);
}

static      Boolean
SetValuesRubik(Widget current, Widget request, Widget new)
{
	RubikWidget c = (RubikWidget) current, w = (RubikWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	int         face;

	CheckPolyhedrons(w);
	if (w->rubik.foreground != c->rubik.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->rubik.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->rubik.puzzleGC);
		w->rubik.puzzleGC = XtGetGC(new, valueMask, &values);
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->rubik.foreground;
		XtReleaseGC(new, w->rubik.inverseGC);
		w->rubik.inverseGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->rubik.borderColor != c->rubik.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->rubik.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->rubik.borderGC);
		w->rubik.borderGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->rubik.mono || w->rubik.depth == 1) {
		valueMask = GCForeground | GCBackground;
		values.background = w->core.background_pixel;
		values.foreground = w->rubik.foreground;
		for (face = 0; face < MAXFACES; face++) {
			XtReleaseGC(new, w->rubik.faceGC[face]);
			w->rubik.faceGC[face] = XtGetGC(new, valueMask, &values);
		}
		redraw = TRUE;
	}
	for (face = 0; face < MAXFACES; face++) {
		if (strcmp(w->rubik.faceName[face], c->rubik.faceName[face]))
			GetColor(w, face, FALSE);
	}
	if (w->rubik.orient != c->rubik.orient) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	} else if (w->rubik.practice != c->rubik.practice) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	}
	if (w->rubik.currentDirection == RUBIK_RESTORE) {
		SetStartPosition(w);
		w->rubik.currentDirection = RUBIK_IGNORE;
	} else if (w->rubik.currentDirection != RUBIK_IGNORE) {
		MovePolyhedrons(w, w->rubik.currentFace, w->rubik.currentPosition,
				w->rubik.currentDirection);
		w->rubik.currentDirection = RUBIK_IGNORE;
	}
	return redraw;
}

void
QuitRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

void
SelectRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	int         control;

	if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
		     &(w->rubik.currentFace), &(w->rubik.currentPosition))) {
		control = (int) (event->xkey.state & ControlMask);
		if (control || w->rubik.practice || !CheckSolved(w))
			DrawSquare(w, w->rubik.currentFace, w->rubik.currentPosition,
				   TRUE);
	} else {
		w->rubik.currentFace = RUBIK_IGNORE;
		w->rubik.currentDirection = RUBIK_IGNORE;
	}
}

void
ReleaseRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	int         control, face, position, count = -1, direction = 0;
	rubikCallbackStruct cb;

	if (w->rubik.currentFace == RUBIK_IGNORE)
		return;
	DrawSquare(w, w->rubik.currentFace, w->rubik.currentPosition, FALSE);
	control = (int) (event->xkey.state & ControlMask);
	if (!control && !w->rubik.practice && CheckSolved(w))
		MoveNoPolyhedrons(w);
	else if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
				   &face, &position)) {
		control = (control) ? 1 : 0;
		if (face == w->rubik.currentFace)
			count = CheckMoveDir(w, w->rubik.currentPosition, position, &direction);
		if (count == 1) {
			MoveRubik(w, face, w->rubik.currentPosition, direction, control);
			if (!control && CheckSolved(w)) {
				cb.reason = RUBIK_SOLVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else if (count == 0)
			MoveNoPolyhedrons(w);
	}
}

void
PracticeRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	PracticePolyhedrons(w);
}

void
PracticeRubikMaybe(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->rubik.started)
		PracticePolyhedrons(w);
}

void
RandomizeRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizePolyhedrons(w);
}

void
RandomizeRubikMaybe(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->rubik.started)
		RandomizePolyhedrons(w);
}

void
GetRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, size, orient, practice, moves;
	rubikCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &size);
		if (size >= MINCUBES) {
			for (i = w->rubik.size; i < size; i++) {
				cb.reason = RUBIK_INC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->rubik.size; i > size; i--) {
				cb.reason = RUBIK_DEC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: size %d should be between %d and MAXINT\n",
				      DATAFILE, size, MINCUBES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &orient);
		if (w->rubik.orient != (Boolean) orient) {
			cb.reason = RUBIK_ORIENT;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &practice);
		if (w->rubik.practice != (Boolean) practice) {
			cb.reason = RUBIK_PRACTICE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = RUBIK_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: size %d, orient %d, practice %d, moves %d.\n",
			      DATAFILE, size, orient, practice, moves);
	}
}

void
WriteRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "size%c %d\n", SYMBOL, w->rubik.size);
		(void) fprintf(fp, "orient%c %d\n", SYMBOL, (w->rubik.orient) ? 1 : 0);
		(void) fprintf(fp, "practice%c %d\n", SYMBOL, (w->rubik.practice) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

void
UndoRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         face, position, direction, control;

		GetMove(&face, &position, &direction, &control);
		direction = (direction < MAXORIENT) ? (direction + MAXORIENT / 2) %
			MAXORIENT : 3 * MAXORIENT - direction;
		if (control)
			MoveControlCb(w, face, direction);
		else {
			rubikCallbackStruct cb;

			MovePolyhedrons(w, face, position, direction);
			cb.reason = RUBIK_UNDO;
			cb.face = face;
			cb.position = position;
			cb.direction = direction;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

void
SolveRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	if (w->rubik.size <= 2 || (w->rubik.size == 3 && !w->rubik.orient))
		SolvePolyhedrons(w);
}

void
IncrementRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	rubikCallbackStruct cb;

	cb.reason = RUBIK_INC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
DecrementRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	rubikCallbackStruct cb;

	if (w->rubik.size <= MINCUBES)
		return;
	cb.reason = RUBIK_DEC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
OrientizeRubik(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	rubikCallbackStruct cb;

	cb.reason = RUBIK_ORIENT;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
MoveRubikCcw(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput(w, event->xbutton.x, event->xbutton.y, CCW,
		       (int) (event->xbutton.state & ControlMask));
}

void
MoveRubikCw(RubikWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput(w, event->xbutton.x, event->xbutton.y, CW,
		       (int) (event->xkey.state & ControlMask));
}

void
MoveRubikInput(RubikWidget w, int x, int y, int direction, int control)
{
	int         face, position;

	if (!w->rubik.practice && !control && CheckSolved(w)) {
		MoveNoPolyhedrons(w);
		return;
	}
	if (!PositionPolyhedrons(w, x, y, &face, &position, &direction))
		return;
	control = (control) ? 1 : 0;
	MoveRubik(w, face, position, direction, control);
	if (!control && CheckSolved(w)) {
		rubikCallbackStruct cb;

		cb.reason = RUBIK_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

void
MoveRubik(RubikWidget w, int face, int position, int direction, int control)
{
	if (control)
		MoveControlCb(w, face, direction);
	else {
		rubikCallbackStruct cb;

		MovePolyhedrons(w, face, position, direction);
		cb.reason = RUBIK_MOVED;
		cb.face = face;
		cb.position = position;
		cb.direction = direction;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	PutMove(face, position, direction, control);
}

static void
GetColor(RubikWidget w, int face, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->rubik.depth > 1 && !w->rubik.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
				w->rubik.faceName[face], &colorCell, &rgb)) {
			values.foreground = w->rubik.faceColor[face] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->rubik.faceGC[face]);
			w->rubik.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined",
				       w->rubik.faceName[face]);
			XtWarning(buf);
		}
	}
	values.foreground = w->rubik.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->rubik.faceGC[face]);
	w->rubik.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
}

static void
MoveControlCb(RubikWidget w, int face, int direction)
{
	rubikCallbackStruct cb;
	int         k, position;

	for (k = 0; k < w->rubik.size; k++) {
		position = k * w->rubik.size + k;
		MovePolyhedrons(w, face, position, direction);
		cb.reason = RUBIK_CONTROL;
		cb.face = face;
		cb.position = position;
		cb.direction = direction;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
CheckPolyhedrons(RubikWidget w)
{
	if (w->rubik.size < MINCUBES) {
		char        buf[121];

		(void) sprintf(buf, "Number of Cubes on edge out of bounds, use %d..MAXINT",
			       MINCUBES);
		XtWarning(buf);
		w->rubik.size = DEFAULTCUBES;
	}
}

void
ResetPolyhedrons(RubikWidget w)
{
	int         face, position, orient;

	w->rubik.sizeSize = w->rubik.size * w->rubik.size;
	for (face = 0; face < MAXFACES; face++) {
		if (w->rubik.cubeLoc[face])
			(void) free((void *) w->rubik.cubeLoc[face]);
		if (!(w->rubik.cubeLoc[face] = (RubikLoc *)
		      malloc(sizeof (RubikLoc) * w->rubik.sizeSize)))
			XtError("Not enough memory, exiting.");
		if (startLoc[face])
			(void) free((void *) startLoc[face]);
		if (!(startLoc[face] = (RubikLoc *)
		      malloc(sizeof (RubikLoc) * w->rubik.sizeSize)))
			XtError("Not enough memory, exiting.");
	}
	for (orient = 0; orient < MAXORIENT; orient++) {
		if (w->rubik.rowLoc[orient])
			(void) free((void *) w->rubik.rowLoc[orient]);
		if (!(w->rubik.rowLoc[orient] = (RubikLoc *)
		      malloc(sizeof (RubikLoc) * w->rubik.size)))
			XtError("Not enough memory, exiting.");
	}
	if (w->rubik.faceLoc)
		(void) free((void *) w->rubik.faceLoc);
	if (!(w->rubik.faceLoc = (RubikLoc *)
	      malloc(sizeof (RubikLoc) * w->rubik.sizeSize)))
		XtError("Not enough memory, exiting.");
	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->rubik.sizeSize; position++) {
			w->rubik.cubeLoc[face][position].face = face;
			w->rubik.cubeLoc[face][position].rotation = STRT - MAXORIENT;
		}
	FlushMoves(w);
	w->rubik.started = FALSE;
}

static int
SelectPolyhedrons(RubikWidget w, int x, int y, int *face, int *position)
{
	if (w->rubik.dim == 2)
		return SelectPolyhedrons2D((Rubik2DWidget) w, x, y,
					   face, position);
	else if (w->rubik.dim == 3)
		return SelectPolyhedrons3D((Rubik3DWidget) w, x, y,
					   face, position);
	return FALSE;
}

static int
NarrowSelection(RubikWidget w, int *face, int *position, int *direction)
{
	if (w->rubik.dim == 2)
		return NarrowSelection2D((Rubik2DWidget) w, face, position, direction);
	else if (w->rubik.dim == 3)
		return NarrowSelection3D((Rubik3DWidget) w, face, position, direction);
	return FALSE;
}

static int
PositionPolyhedrons(RubikWidget w, int x, int y, int *face, int *position, int *direction)
{
	if (!SelectPolyhedrons(w, x, y, face, position))
		return FALSE;
	return NarrowSelection(w, face, position, direction);
}

static void
MoveNoPolyhedrons(RubikWidget w)
{
	rubikCallbackStruct cb;

	cb.reason = RUBIK_ILLEGAL;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
PracticePolyhedrons(RubikWidget w)
{
	rubikCallbackStruct cb;

	cb.reason = RUBIK_PRACTICE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
RandomizePolyhedrons(RubikWidget w)
{
	rubikCallbackStruct cb;
	int         face, position, direction;
	int         big = w->rubik.sizeSize * 3 + NRAND(2);

	if (big > 1000)
		big = 1000;
	if (w->rubik.practice)
		PracticePolyhedrons(w);
	cb.reason = RUBIK_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
	big = 3;
#endif

	while (big--) {
		face = NRAND(MAXFACES);
		position = NRAND(w->rubik.sizeSize);
		direction = NRAND(MAXORIENT);
		MoveRubik(w, face, position, direction, FALSE);
	}
	FlushMoves(w);
	cb.reason = RUBIK_RANDOMIZE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (CheckSolved(w)) {
		cb.reason = RUBIK_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MovePolyhedrons(RubikWidget w, int face, int position, int direction)
{
	int         newFace, newDirection, rotate, reverse = FALSE, h, k,
	            newH = 0;
	int         i, j;

	i = position % w->rubik.size;
	j = position / w->rubik.size;
	h = (direction == TOP || direction == BOTTOM) ? i : j;
	/* rotate sides CW or CCW */
	if (h == w->rubik.size - 1) {
		newDirection = (direction == TOP || direction == BOTTOM) ?
			TOP : RIGHT;
		if (direction == TOP || direction == RIGHT)
			RotateFace(w, rowToRotate[face][newDirection], CW);
		else		/* direction == BOTTOM || direction == LEFT */
			RotateFace(w, rowToRotate[face][newDirection], CCW);
	}
	if (h == 0) {
		newDirection = (direction == TOP || direction == BOTTOM) ?
			BOTTOM : LEFT;
		if (direction == TOP || direction == RIGHT)
			RotateFace(w, rowToRotate[face][newDirection], CCW);
		else		/* direction == BOTTOM  || direction == LEFT */
			RotateFace(w, rowToRotate[face][newDirection], CW);
	}
	/* Slide rows */
	ReadRc(w, face, direction, h, 0);
	for (k = 1; k <= MAXORIENT; k++) {
		newFace = slideNextRow[face][direction].face;
		rotate = slideNextRow[face][direction].rotation;
		newDirection = (rotate + direction) % MAXORIENT;
		switch (rotate) {
			case STRT:
				newH = h;
				reverse = FALSE;
				break;
			case CW:
				if (newDirection == TOP || newDirection == BOTTOM) {
					newH = w->rubik.size - 1 - h;
					reverse = FALSE;
				} else {	/* newDirection == RIGHT || newDirection == LEFT */
					newH = h;
					reverse = TRUE;
				}
				break;
			case HALF:
				newH = w->rubik.size - 1 - h;
				reverse = TRUE;
				break;
			case CCW:
				if (newDirection == TOP || newDirection == BOTTOM) {
					newH = h;
					reverse = TRUE;
				} else {	/* newDirection == RIGHT || newDirection == LEFT */
					newH = w->rubik.size - 1 - h;
					reverse = FALSE;
				}
				break;
			default:
				(void) printf("MovePolyhedrons: rotate %d\n", rotate);
		}
		if (k != MAXORIENT)
			ReadRc(w, newFace, newDirection, newH, k);
		RotateRc(w, rotate, k - 1);
		if (reverse == TRUE)
			ReverseRc(w, k - 1);
		WriteRc(w, newFace, newDirection, newH, k - 1);
		face = newFace;
		direction = newDirection;
		h = newH;
	}
}

static void
ReadRc(RubikWidget w, int face, int dir, int h, int orient)
{
	int         g;

	if (dir == TOP || dir == BOTTOM)
		for (g = 0; g < w->rubik.size; g++)
			w->rubik.rowLoc[orient][g] =
				w->rubik.cubeLoc[face][g * w->rubik.size + h];
	else			/* dir == RIGHT || dir == LEFT */
		for (g = 0; g < w->rubik.size; g++)
			w->rubik.rowLoc[orient][g] =
				w->rubik.cubeLoc[face][h * w->rubik.size + g];
}

static void
RotateRc(RubikWidget w, int rotate, int orient)
{
	int         g;

	for (g = 0; g < w->rubik.size; g++)
		w->rubik.rowLoc[orient][g].rotation =
			(w->rubik.rowLoc[orient][g].rotation + rotate) % MAXORIENT;
}

static void
ReverseRc(RubikWidget w, int orient)
{
	int         g;
	RubikLoc    temp;

	for (g = 0; g < w->rubik.size / 2; g++) {
		temp = w->rubik.rowLoc[orient][w->rubik.size - 1 - g];
		w->rubik.rowLoc[orient][w->rubik.size - 1 - g] = w->rubik.rowLoc[orient][g];
		w->rubik.rowLoc[orient][g] = temp;
	}
}

static void
WriteRc(RubikWidget w, int face, int dir, int h, int orient)
{
	int         g, position;

	if (dir == TOP || dir == BOTTOM) {
		for (g = 0; g < w->rubik.size; g++) {
			position = g * w->rubik.size + h;
			w->rubik.cubeLoc[face][position] = w->rubik.rowLoc[orient][g];
			DrawSquare(w, face, position, FALSE);
		}
	} else {		/* dir == RIGHT || dir == LEFT */
		for (g = 0; g < w->rubik.size; g++) {
			position = h * w->rubik.size + g;
			w->rubik.cubeLoc[face][position] = w->rubik.rowLoc[orient][g];
			DrawSquare(w, face, position, FALSE);
		}
	}
}

static void
RotateFace(RubikWidget w, int face, int direction)
{
	int         position, i, j;

	/* Read Face */
	for (position = 0; position < w->rubik.sizeSize; position++)
		w->rubik.faceLoc[position] = w->rubik.cubeLoc[face][position];
	/* Write Face */
	for (position = 0; position < w->rubik.sizeSize; position++) {
		i = position % w->rubik.size;
		j = position / w->rubik.size;
		w->rubik.cubeLoc[face][position] = (direction == CW) ?
			w->rubik.faceLoc[(w->rubik.size - i - 1) * w->rubik.size + j] :
			w->rubik.faceLoc[i * w->rubik.size + w->rubik.size - j - 1];
		w->rubik.cubeLoc[face][position].rotation =
			(w->rubik.cubeLoc[face][position].rotation + direction - MAXORIENT) %
			MAXORIENT;
		DrawSquare(w, face, position, FALSE);
	}
}

void
DrawAllPolyhedrons(RubikWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->rubik.sizeSize; position++)
			DrawSquare(w, face, position, FALSE);
}

static void
DrawSquare(RubikWidget w, int face, int position, int offset)
{
	if (w->rubik.dim == 2)
		DrawSquare2D((Rubik2DWidget) w, face, position, offset);
	else if (w->rubik.dim == 3)
		DrawSquare3D((Rubik3DWidget) w, face, position, offset);
}

Boolean
CheckSolved(RubikWidget w)
{
	int         face, position;
	RubikLoc    test;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->rubik.sizeSize; position++) {
			if (!position) {
				test.face = w->rubik.cubeLoc[face][position].face;
				test.rotation = w->rubik.cubeLoc[face][position].rotation;
			} else if (test.face !=		/*face */
				   w->rubik.cubeLoc[face][position].face ||
				   (w->rubik.orient && test.rotation !=		/*STRT - MAXORIENT */
				  w->rubik.cubeLoc[face][position].rotation))
				return FALSE;
		}
	return TRUE;
}

static int
CheckMoveDir(RubikWidget w, int position1, int position2, int *direction)
{
	int         count = 0;
	int         column1, column2, row1, row2;

	column1 = position1 % w->rubik.size;
	column2 = position2 % w->rubik.size;
	row1 = position1 / w->rubik.size;
	row2 = position2 / w->rubik.size;
	if (column1 == column2 && row1 != row2) {
		*direction = (row2 > row1) ? BOTTOM : TOP;
		count = 1;
	} else if (row1 == row2 && column1 != column2) {
		*direction = (column2 > column1) ? RIGHT : LEFT;
		count = 1;
	} else if (row1 == row2 && column1 == column2)
		count = 2;
	return count;
}


#ifdef DEBUG

void
PrintCube(RubikWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++) {
		for (position = 0; position < w->rubik.sizeSize; position++) {
			(void) printf("%d %d  ", w->rubik.cubeLoc[face][position].face,
				  w->rubik.cubeLoc[face][position].rotation);
			if (!((position + 1) % w->rubik.size))
				(void) printf("\n");
		}
		(void) printf("\n");
	}
	(void) printf("\n");
}

void
PrintFace(RubikWidget w)
{
	int         position;

	for (position = 0; position < w->rubik.sizeSize; position++) {
		(void) printf("%d %d  ", w->rubik.faceLoc[position].face,
			      w->rubik.faceLoc[position].rotation);
		if (!((position + 1) % w->rubik.size))
			(void) printf("\n");
	}
	(void) printf("\n");
}

void
PrintRow(RubikWidget w, int orient)
{
	int         i;

	(void) printf("Row %d:\n", orient);
	for (i = 0; i < w->rubik.size; i++)
		(void) printf("%d %d  ", w->rubik.rowLoc[orient][i].face,
			      w->rubik.rowLoc[orient][i].rotation);
	(void) printf("\n");
}

#endif
