/*-
# X-BASED DINOSAUR CUBE
#
#  Dino.c
#
###
#
#  Copyright (c) 1995 - 97	David Albert Bagley, bagleyd@bigfoot.com
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

/* Methods file for Dino */

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
#include "DinoP.h"
#include "Dino2dP.h"
#include "Dino3dP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/dino.data"
#endif

static void InitializeDino(Widget request, Widget new);
static void DestroyDino(Widget old);
static Boolean SetValuesDino(Widget current, Widget request, Widget new);
static void GetColor(DinoWidget w, int face, int init);
static void MoveControlCb(DinoWidget w, int face, int position, int direction, int style);
static void CheckPolyhedrons(DinoWidget w);
static void ResetPolyhedrons(DinoWidget w);
static int  SelectPolyhedrons(DinoWidget w, int x, int y, int *face, int *position);
static int  NarrowSelection(DinoWidget w, int *face, int *direction);
static int  PositionPolyhedrons(DinoWidget w, int x, int y, int *face, int *position, int *direction);
static int  GetStyle(DinoWidget w, int shift, int control, int alt);
static void MoveNoPolyhedrons(DinoWidget w);
static void PracticePolyhedrons(DinoWidget w);
static void RandomizePolyhedrons(DinoWidget w);
static void MovePolyhedrons(DinoWidget w, int face, int position, int direction, int style);
static void MoveInsideCorners(DinoWidget w, int face, int corner, int direction);
static void MoveOutsideCorners(DinoWidget w, int face, int corner, int direction);
static void MoveEdges(DinoWidget w, int face, int corner, int direction);
static void MoveFaces(DinoWidget w, int f, int d, int rotate);
static void ReadDiagonal(DinoWidget w, int face, int corner, int h);
static void WriteDiagonal(DinoWidget w, int face, int corner, int rotate, int h);
static void ReadFace(DinoWidget w, int face, int h);
static void WriteFace(DinoWidget w, int face, int rotate, int h);
static void RotateFace(DinoWidget w, int face, int direction);
static void DrawTriangle(DinoWidget w, int face, int position, int offset);
static int  CheckMoveDir(int position1, int position2, int *direction);

DinoClassRec dinoClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Dino",		/* class name */
		sizeof (DinoRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeDino,	/* initialize */
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
		(XtWidgetProc) DestroyDino,	/* destroy */
		NULL,		/* resize */
		NULL,		/* expose */
		(XtSetValuesFunc) SetValuesDino,	/* set values */
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

WidgetClass dinoWidgetClass = (WidgetClass) & dinoClassRec;

static DinoLoc slideCorner[MAXFACES][MAXORIENT][MAXORIENT / 2] =
{
	{
		{
			{3, TR, 0},
			{5, BR, 0}},
		{
			{3, TL, 1},
			{2, TR, 0}},
		{
			{2, TL, 1},
			{1, TR, 0}},
		{
			{5, BL, 1},
			{1, TL, 1}}
	},
	{
		{
			{2, TL, 0},
			{0, BL, 0}},
		{
			{2, BL, 0},
			{4, TL, 0}},
		{
			{4, BL, 0},
			{5, TL, 0}},
		{
			{0, TL, 0},
			{5, BL, 0}}
	},
	{
		{
			{3, TL, 0},
			{0, BR, 0}},
		{
			{3, BL, 0},
			{4, TR, 0}},
		{
			{4, TL, 1},
			{1, BR, 1}},
		{
			{0, BL, 1},
			{1, TR, 1}}
	},
	{
		{
			{5, BR, 1},
			{0, TR, 1}},
		{
			{5, TR, 1},
			{4, BR, 1}},
		{
			{4, TR, 1},
			{2, BR, 1}},
		{
			{0, BR, 1},
			{2, TR, 1}}
	},
	{
		{
			{3, BL, 1},
			{2, BR, 0}},
		{
			{3, BR, 0},
			{5, TR, 0}},
		{
			{5, TL, 1},
			{1, BL, 1}},
		{
			{2, BL, 1},
			{1, BR, 0}}
	},
	{
		{
			{3, BR, 1},
			{4, BR, 0}},
		{
			{3, TR, 1},
			{0, TR, 0}},
		{
			{0, TL, 1},
			{1, TL, 0}},
		{
			{4, BL, 1},
			{1, BL, 0}}
	}
};

static int  oppFace[MAXFACES] =
{4, 3, 5, 1, 0, 2};

static DinoCornerLoc oppCorner[MAXFACES][MAXORIENT] =
{
	{
		{4, 3},
		{4, 2},
		{4, 1},
		{4, 0}},
	{
		{3, 1},
		{3, 0},
		{3, 3},
		{3, 2}},
	{
		{5, 3},
		{5, 2},
		{5, 1},
		{5, 0}},
	{
		{1, 1},
		{1, 0},
		{1, 3},
		{1, 2}},
	{
		{0, 3},
		{0, 2},
		{0, 1},
		{0, 0}},
	{
		{2, 3},
		{2, 2},
		{2, 1},
		{2, 0}}
};

/* static int slideNextFace2[MAXFACES] = {4, 3, 5, 1, 0, 2}; */

static DinoCornerLoc slideNextFace[MAXFACES][MAXORIENT] =
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

static int  faceToRotate2[MAXFACES][MAXORIENT][2] =
{
	{
		{3, 5},
		{2, 3},
		{1, 2},
		{1, 5}},
	{
		{0, 2},
		{2, 4},
		{4, 5},
		{0, 5}},
	{
		{3, 0},
		{4, 3},
		{1, 4},
		{0, 1}},
	{
		{0, 5},
		{4, 5},
		{2, 4},
		{0, 2}},
	{
		{2, 3},
		{3, 5},
		{1, 5},
		{1, 2}},
	{
		{4, 3},
		{3, 0},
		{0, 1},
		{1, 4}}
};

static int  faceToRotate[MAXFACES][MAXORIENT] =
{
	{3, 2, 1, 5},
	{2, 4, 5, 0},
	{3, 4, 1, 0},
	{5, 4, 2, 0},
	{3, 5, 1, 2},
	{3, 0, 1, 4}
};

static void
InitializeDino(Widget request, Widget new)
{
	DinoWidget  w = (DinoWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         face;

	CheckPolyhedrons(w);
	InitMoves();
	ResetPolyhedrons(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	values.foreground = w->dino.foreground;
	w->dino.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->dino.borderColor;
	w->dino.borderGC = XtGetGC(new, valueMask, &values);
	w->dino.depth = DefaultDepthOfScreen(XtScreen(w));
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->dino.foreground;
	w->dino.inverseGC = XtGetGC(new, valueMask, &values);
	for (face = 0; face < MAXFACES; face++)
		GetColor(w, face, TRUE);
}

static void
DestroyDino(Widget old)
{
	DinoWidget  w = (DinoWidget) old;
	int         face;

	for (face = 0; face < MAXFACES; face++)
		XtReleaseGC(old, w->dino.faceGC[face]);
	XtReleaseGC(old, w->dino.borderGC);
	XtReleaseGC(old, w->dino.puzzleGC);
	XtReleaseGC(old, w->dino.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->dino.select);
}

static      Boolean
SetValuesDino(Widget current, Widget request, Widget new)
{
	DinoWidget  c = (DinoWidget) current, w = (DinoWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	int         face;

	if (w->dino.foreground != c->dino.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->dino.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->dino.puzzleGC);
		w->dino.puzzleGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->dino.foreground;
		XtReleaseGC(new, w->dino.inverseGC);
		w->dino.inverseGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->dino.borderColor != c->dino.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->dino.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->dino.borderGC);
		w->dino.borderGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->dino.mono || w->dino.depth == 1) {
		valueMask = GCForeground | GCBackground;
		values.background = w->core.background_pixel;
		values.foreground = w->dino.foreground;
		for (face = 0; face < MAXFACES; face++) {
			XtReleaseGC(new, w->dino.faceGC[face]);
			w->dino.faceGC[face] = XtGetGC(new, valueMask, &values);
		}
		redraw = TRUE;
	}
	for (face = 0; face < MAXFACES; face++) {
		if (strcmp(w->dino.faceName[face], c->dino.faceName[face]))
			GetColor(w, face, FALSE);
	}
	if (w->dino.orient != c->dino.orient) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	} else if (w->dino.practice != c->dino.practice) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	}
	if (w->dino.mode != c->dino.mode) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	}
	if (w->dino.currentDirection == DINO_RESTORE) {
		SetStartPosition(w);
		w->dino.currentDirection = DINO_IGNORE;
	} else if (w->dino.currentDirection != DINO_IGNORE) {
		MovePolyhedrons(w, w->dino.currentFace, w->dino.currentPosition,
				w->dino.currentDirection, w->dino.style);
		w->dino.currentDirection = DINO_IGNORE;
	}
	return redraw;
}

void
QuitDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

void
SelectDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	int         control;

	if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
		       &(w->dino.currentFace), &(w->dino.currentPosition))) {
		control = (int) (event->xkey.state & ControlMask);
		if (control || w->dino.practice || !CheckSolved(w))
			DrawTriangle(w, w->dino.currentFace, w->dino.currentPosition,
				     TRUE);
	} else {
		w->dino.currentFace = DINO_IGNORE;
		w->dino.currentDirection = DINO_IGNORE;
	}
}

void
ReleaseDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	int         shift, control, alt, style, face, position, count = -1,
	            direction = 0;
	dinoCallbackStruct cb;

	if (w->dino.currentFace == DINO_IGNORE)
		return;
	DrawTriangle(w, w->dino.currentFace, w->dino.currentPosition, FALSE);
	shift = (int) (event->xbutton.state & (ShiftMask | LockMask));
	control = (int) (event->xkey.state & ControlMask);
	alt = (int) (event->xkey.state &
		     (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask));
	style = GetStyle(w, shift, control, alt);
	if (!control && !w->dino.practice && CheckSolved(w))
		MoveNoPolyhedrons(w);
	else if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
		  &face, &position) && position != w->dino.currentPosition) {
		control = (control) ? 1 : 0;
		if (face == w->dino.currentFace)
			count = CheckMoveDir(w->dino.currentPosition, position, &direction);
		if (count == 1) {
			MoveDino(w, face, w->dino.currentPosition, direction, style, control);
			if (!control && CheckSolved(w)) {
				cb.reason = DINO_SOLVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else if (count == 0)
			MoveNoPolyhedrons(w);
	}
	w->dino.currentFace = DINO_IGNORE;
	w->dino.currentDirection = DINO_IGNORE;
}

void
PracticeDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	PracticePolyhedrons(w);
}

void
PracticeDinoMaybe(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->dino.started)
		PracticePolyhedrons(w);
}

void
RandomizeDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizePolyhedrons(w);
}

void
RandomizeDinoMaybe(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->dino.started)
		RandomizePolyhedrons(w);
}

void
GetDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         mode, orient, practice, moves;
	dinoCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &mode);
		if (mode >= PERIOD2 && mode <= BOTH)
			switch (mode) {
				case PERIOD2:
					cb.reason = DINO_PERIOD2;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
					break;
				case PERIOD3:
					cb.reason = DINO_PERIOD3;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
					break;
				case BOTH:
					cb.reason = DINO_BOTH;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else
			(void) printf("%s corrupted: mode %d should be between %d and %d\n",
				      DATAFILE, mode, PERIOD2, BOTH);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &orient);
		if (w->dino.orient != (Boolean) orient) {
			cb.reason = DINO_ORIENT;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &practice);
		if (w->dino.practice != (Boolean) practice) {
			cb.reason = DINO_PRACTICE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = DINO_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: mode %d, orient %d, practice %d, moves %d.\n",
			      DATAFILE, mode, orient, practice, moves);
	}
}

void
WriteDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "mode%c %d\n", SYMBOL, w->dino.mode);
		(void) fprintf(fp, "orient%c %d\n", SYMBOL, (w->dino.orient) ? 1 : 0);
		(void) fprintf(fp, "practice%c %d\n", SYMBOL, (w->dino.practice) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

void
UndoDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         face, position, direction, style, control;

		GetMove(&face, &position, &direction, &style, &control);
		direction = (direction < MAXORIENT) ? (direction + MAXORIENT / 2) %
			MAXORIENT : 5 * MAXORIENT - direction;
		if (control)
			MoveControlCb(w, face, position, direction, style);
		else {
			dinoCallbackStruct cb;

			MovePolyhedrons(w, face, position, direction, style);
			cb.reason = DINO_UNDO;
			cb.face = face;
			cb.position = position;
			cb.direction = direction;
			cb.style = style;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

void
SolveDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolvePolyhedrons(w); *//* Sorry, unimplemented */
}

void
OrientizeDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	dinoCallbackStruct cb;

	cb.reason = DINO_ORIENT;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
Period2ModeDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	dinoCallbackStruct cb;

	cb.reason = DINO_PERIOD2;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
Period3ModeDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	dinoCallbackStruct cb;

	cb.reason = DINO_PERIOD3;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
BothModeDino(DinoWidget w, XEvent * event, char **args, int nArgs)
{
	dinoCallbackStruct cb;

	cb.reason = DINO_BOTH;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
MoveDinoInput(DinoWidget w, int x, int y, int direction, int shift, int control, int alt)
{
	int         face, position, style;

	if (!w->dino.practice && !control && CheckSolved(w)) {
		MoveNoPolyhedrons(w);
		return;
	}
	if (!PositionPolyhedrons(w, x, y, &face, &position, &direction))
		return;
	if (direction >= 2 * MAXORIENT) {
		if (control)
			style = FACE;
		else
			return;
	} else
		style = GetStyle(w, shift, control, alt);
	control = (control) ? 1 : 0;
	MoveDino(w, face, position, direction, style, control);
	if (!control && CheckSolved(w)) {
		dinoCallbackStruct cb;

		cb.reason = DINO_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

void
MoveDino(DinoWidget w, int face, int position, int direction, int style, int control)
{
	if (control)
		MoveControlCb(w, face, position, direction, style);
	else {
		dinoCallbackStruct cb;

		MovePolyhedrons(w, face, position, direction, style);
		cb.reason = DINO_MOVED;
		cb.face = face;
		cb.position = position;
		cb.direction = direction;
		cb.style = style;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	PutMove(face, position, direction, style, control);
}

static void
GetColor(DinoWidget w, int face, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->dino.depth > 1 && !w->dino.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
				 w->dino.faceName[face], &colorCell, &rgb)) {
			values.foreground = w->dino.faceColor[face] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->dino.faceGC[face]);
			w->dino.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined",
				       w->dino.faceName[face]);
			XtWarning(buf);
		}
	}
	values.foreground = w->dino.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->dino.faceGC[face]);
	w->dino.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
}

static void
MoveControlCb(DinoWidget w, int face, int position, int direction, int style)
{
	dinoCallbackStruct cb;
	int         newFace, newSide, newDirection, corner, newCorner;

	MovePolyhedrons(w, face, position, direction, style);
	cb.reason = DINO_CONTROL;
	cb.face = face;
	cb.position = position;
	cb.direction = direction;
	cb.style = style;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (style == CORNER) {
		newSide = (position + 2) % MAXORIENT;
		MovePolyhedrons(w, face, newSide, direction, MIDDLE);
		cb.face = face;
		cb.position = newSide;
		cb.direction = direction;
		cb.style = MIDDLE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		corner = (position - !((position + direction) % 2) + MAXORIENT) % MAXORIENT;
		newFace = oppCorner[face][corner].face;
		newCorner = oppCorner[face][corner].rotation;
		newDirection = 2 * ((direction / 2 + (face != 1 && face != 3)) % 2) +
			!(newCorner % 2);
		newSide = newCorner;
		MovePolyhedrons(w, newFace, newSide, newDirection, CORNER);
		cb.face = newFace;
		cb.position = newSide;
		cb.direction = newDirection;
		cb.style = CORNER;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	} else if (style == MIDDLE) {
		newSide = (position + 2) % MAXORIENT;
		MovePolyhedrons(w, face, newSide, direction, CORNER);
		cb.face = face;
		cb.position = newSide;
		cb.direction = direction;
		cb.style = CORNER;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		corner = (position - !((position + direction) % 2) + MAXORIENT) % MAXORIENT;
		newCorner = oppCorner[face][corner].rotation;
		newFace = oppCorner[face][corner].face;
		if (newFace != 1 && newFace != 3)
			newDirection = (direction + 2) % MAXORIENT;
		else
			newDirection = direction;
		newDirection = 2 * (newDirection / 2) + !(newDirection % 2);
		newSide = (newCorner + 2) % MAXORIENT;
		MovePolyhedrons(w, newFace, newSide, newDirection, CORNER);
		cb.face = newFace;
		cb.position = newSide;
		cb.direction = newDirection;
		cb.style = CORNER;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	} else if (style == EDGE) {
		newSide = (position + 2) % MAXORIENT;
		MovePolyhedrons(w, face, newSide, direction, EDGE);
		cb.face = face;
		cb.position = newSide;
		cb.direction = direction;
		cb.style = EDGE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
CheckPolyhedrons(DinoWidget w)
{
	if (w->dino.mode < PERIOD2 || w->dino.mode > BOTH) {
		XtWarning("Mode is in error, use 2 for Period2, 3 for Period3, 4 for Both");
		w->dino.mode = DEFAULTMODE;
	}
}

static void
ResetPolyhedrons(DinoWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXORIENT; position++) {
			w->dino.cubeLoc[face][position].face = face;
			w->dino.cubeLoc[face][position].rotation = STRT - MAXORIENT;
		}
	FlushMoves(w);
	w->dino.started = FALSE;
}

static int
SelectPolyhedrons(DinoWidget w, int x, int y, int *face, int *position)
{
	if (w->dino.dim == 2)
		return SelectPolyhedrons2D((Dino2DWidget) w, x, y,
					   face, position);
	else if (w->dino.dim == 3)
		return SelectPolyhedrons3D((Dino3DWidget) w, x, y,
					   face, position);
	return FALSE;
}

static int
NarrowSelection(DinoWidget w, int *face, int *direction)
{
	if (w->dino.dim == 2)
		return NarrowSelection2D((Dino2DWidget) w, face, direction);
	else if (w->dino.dim == 3)
		return NarrowSelection3D((Dino3DWidget) w, face, direction);
	return FALSE;
}

static int
PositionPolyhedrons(DinoWidget w, int x, int y, int *face, int *position, int *direction)
{
	if (!SelectPolyhedrons(w, x, y, face, position))
		return FALSE;
	return NarrowSelection(w, face, direction);
}

static int
GetStyle(DinoWidget w, int shift, int control, int alt)
{
	if (w->dino.mode != BOTH) {
		if (control && shift) {
			if (w->dino.mode == PERIOD3)
				return EDGE;
			else if (alt)
				return MIDDLE;
			else
				return CORNER;
		} else if (w->dino.mode == PERIOD2)
			return EDGE;
		else if (alt)
			return MIDDLE;
		else
			return CORNER;
	} else {
		if (shift)
			return EDGE;
		else {
			if (alt)
				return MIDDLE;
			else
				return CORNER;
		}
	}
}
static void
MoveNoPolyhedrons(DinoWidget w)
{
	dinoCallbackStruct cb;

	cb.reason = DINO_ILLEGAL;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
PracticePolyhedrons(DinoWidget w)
{
	dinoCallbackStruct cb;

	cb.reason = DINO_PRACTICE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
RandomizePolyhedrons(DinoWidget w)
{
	dinoCallbackStruct cb;
	int         face, position, direction, style;
	int         big = MAXORIENT * 3 + NRAND(2);

	if (w->dino.practice)
		PracticePolyhedrons(w);
	cb.reason = DINO_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
	big = 3;
#endif

	while (big--) {
		face = NRAND(MAXFACES);
		position = NRAND(MAXORIENT);
		direction = ((NRAND(2)) ? position + 1 : position + 3) % MAXORIENT;
		if (w->dino.mode == PERIOD2)
			style = EDGE;
		else if (w->dino.mode == BOTH)
			style = NRAND(3);
		else
			style = NRAND(2);
		MoveDino(w, face, position, direction, style, FALSE);
	}
	FlushMoves(w);
	cb.reason = DINO_RANDOMIZE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (CheckSolved(w)) {
		cb.reason = DINO_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MovePolyhedrons(DinoWidget w, int face, int position, int direction, int style)
{
	int         corner, newCorner;

	corner = (position - !((position + direction) % 2) + MAXORIENT) % MAXORIENT;
	if (style == CORNER) {
		MoveInsideCorners(w, face, corner, direction);
	} else if (style == MIDDLE) {
		MoveOutsideCorners(w, face, corner, direction);
		newCorner = oppCorner[face][corner].rotation;
		face = oppCorner[face][corner].face;
		if (((face != 1 && face != 3) + corner) % 2)
			direction = (direction + 1) % MAXORIENT;
		else
			direction = (direction + 3) % MAXORIENT;
		corner = newCorner;
		MoveOutsideCorners(w, face, corner, direction);
	} else if (style == EDGE) {
		MoveEdges(w, face, corner, direction);
		MoveFaces(w, face, corner,
			((face == 2 || face == 5) ? CCW : HALF) % MAXORIENT);
	} else {
		int         k, newFace, rotate, newDirection;

		RotateFace(w, faceToRotate[face][direction % MAXORIENT], CW);
		RotateFace(w, faceToRotate[face][(direction + 2) % MAXORIENT], CCW);
		ReadFace((DinoWidget) w, face, 0);
		for (k = 1; k <= MAXORIENT; k++) {
			newFace = slideNextFace[face][direction % MAXORIENT].face;
			rotate = slideNextFace[face][direction % MAXORIENT].rotation;
			newDirection = (rotate + direction) % MAXORIENT;
			if (k != MAXORIENT)
				ReadFace((DinoWidget) w, newFace, k);
			WriteFace(w, newFace, rotate, k - 1);
			face = newFace;
			direction = newDirection;
		}
	}
}

static void
MoveInsideCorners(DinoWidget w, int face, int corner, int direction)
{
	int         newFace, newCorner, newDirection, dir, k;

	ReadDiagonal((DinoWidget) w, face, corner, 0);
	for (k = 1; k <= MAXROTATE; k++) {
		dir = direction / 2;
		newFace = slideCorner[face][corner][dir].face;
		newCorner = slideCorner[face][corner][dir].side;
		newDirection = 2 * slideCorner[face][corner][dir].dir + !(newCorner % 2);
		if (k != MAXROTATE)
			ReadDiagonal((DinoWidget) w, newFace, newCorner, k);
		WriteDiagonal(w, newFace, newCorner,
		  (newDirection - direction + MAXORIENT) % MAXORIENT, k - 1);
		face = newFace;
		corner = newCorner;
		direction = newDirection;
	}
}

static void
MoveOutsideCorners(DinoWidget w, int face, int corner, int direction)
{
	int         newFace, newCorner, newDirection, dir, k;

	ReadDiagonal((DinoWidget) w, face, corner, 0);
	for (k = 1; k <= MAXROTATE; k++) {
		corner = (corner + 2) % MAXORIENT;
		dir = direction / 2;
		newFace = slideCorner[face][corner][dir].face;
		newCorner = (slideCorner[face][corner][dir].side + 2) % MAXORIENT;
		newDirection = 2 * slideCorner[face][corner][dir].dir + !(newCorner % 2);
		if (k != MAXROTATE)
			ReadDiagonal((DinoWidget) w, newFace, newCorner, k);
		WriteDiagonal(w, newFace, newCorner,
		  (newDirection - direction + MAXORIENT) % MAXORIENT, k - 1);
		face = newFace;
		corner = newCorner;
		direction = newDirection;
	}
}

static void
MoveEdges(DinoWidget w, int face, int corner, int direction)
{
	int         k, newFace, rotate, newCorner, newDirection;

	ReadDiagonal((DinoWidget) w, face, corner, 0);
	for (k = 1; k <= 2; k++) {
		newFace = oppFace[face];
		/*rotate = (((face == 1 || face == 3) ? 1 : 3) + 3 * direction) %
		   MAXORIENT; */
		newCorner = ((((face == 1 || face == 3) + corner) % 2) ?
			     (corner + 3) : (corner + 1)) % MAXORIENT;
		rotate = (newCorner - corner + MAXORIENT) % MAXORIENT;
		newDirection = (rotate + direction) % MAXORIENT;
		if (k != 2)
			ReadDiagonal((DinoWidget) w, newFace, newCorner, k);
		WriteDiagonal(w, newFace, newCorner, rotate, k - 1);
		face = newFace;
		corner = newCorner;
		direction = newDirection;
	}
}

static void
MoveFaces(DinoWidget w, int f, int d, int rotate)
{
	int         k, face, newFace;

	face = faceToRotate2[f][d][0],
		ReadFace((DinoWidget) w, face, 0);
	for (k = 1; k <= 2; k++) {
		newFace = faceToRotate2[f][d][k % 2],
			rotate = MAXORIENT - rotate;
		if (k != 2)
			ReadFace((DinoWidget) w, newFace, k);
		WriteFace(w, newFace, rotate, k - 1);
		face = newFace;
	}
}

static void
ReadDiagonal(DinoWidget w, int face, int corner, int h)
{
	w->dino.spindleLoc[h][0] = w->dino.cubeLoc[face][corner];
	w->dino.spindleLoc[h][1] = w->dino.cubeLoc[face][(corner + 1) % MAXORIENT];
}

static void
WriteDiagonal(DinoWidget w, int face, int corner, int rotate, int h)
{
	w->dino.spindleLoc[h][0].rotation =
		(w->dino.spindleLoc[h][0].rotation + rotate) % MAXORIENT;
	w->dino.spindleLoc[h][1].rotation =
		(w->dino.spindleLoc[h][1].rotation + rotate) % MAXORIENT;
	w->dino.cubeLoc[face][corner] = w->dino.spindleLoc[h][0];
	DrawTriangle(w, face, corner, FALSE);
	w->dino.cubeLoc[face][(corner + 1) % MAXORIENT] = w->dino.spindleLoc[h][1];
	DrawTriangle(w, face, (corner + 1) % MAXORIENT, FALSE);
}

static void
ReadFace(DinoWidget w, int face, int h)
{
	int         side;

	for (side = 0; side < MAXORIENT; side++)
		w->dino.rowLoc[h][side] = w->dino.cubeLoc[face][side];
}

static void
WriteFace(DinoWidget w, int face, int rotate, int h)
{
	int         side, newSide;

	for (side = 0; side < MAXORIENT; side++) {
		newSide = (side + rotate) % MAXORIENT;
		w->dino.cubeLoc[face][newSide] = w->dino.rowLoc[h][side];
		w->dino.cubeLoc[face][newSide].rotation =
			(w->dino.cubeLoc[face][newSide].rotation + rotate) % MAXORIENT;
		DrawTriangle(w, face, (side + rotate) % MAXORIENT, FALSE);
	}
}

static void
RotateFace(DinoWidget w, int face, int direction)
{
	int         side;

	/* Read Face */
	for (side = 0; side < MAXORIENT; side++)
		w->dino.faceLoc[side] = w->dino.cubeLoc[face][side];
	/* Write Face */
	for (side = 0; side < MAXORIENT; side++) {
		w->dino.cubeLoc[face][side] = (direction == CW) ?
			w->dino.faceLoc[(side + MAXORIENT - 1) % MAXORIENT] :
			w->dino.faceLoc[(side + 1) % MAXORIENT];
		w->dino.cubeLoc[face][side].rotation =
			(w->dino.cubeLoc[face][side].rotation + direction) % MAXORIENT;
		DrawTriangle(w, face, side, FALSE);
	}
}

void
DrawAllPolyhedrons(DinoWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXORIENT; position++)
			DrawTriangle(w, face, position, FALSE);
}

static void
DrawTriangle(DinoWidget w, int face, int position, int offset)
{
	if (w->dino.dim == 2)
		DrawTriangle2D((Dino2DWidget) w, face, position, offset);
	else if (w->dino.dim == 3)
		DrawTriangle3D((Dino3DWidget) w, face, position, offset);
}

Boolean
CheckSolved(DinoWidget w)
{
	int         face, position;
	DinoCornerLoc test;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXORIENT; position++) {
			if (!position) {
				test.face = w->dino.cubeLoc[face][position].face;
				test.rotation = w->dino.cubeLoc[face][position].rotation;
			} else if (test.face !=		/*face */
				   w->dino.cubeLoc[face][position].face ||
				   (w->dino.orient && test.rotation !=	/*STRT - MAXORIENT */
				    w->dino.cubeLoc[face][position].rotation))
				return FALSE;
		}
	return TRUE;
}

static int
CheckMoveDir(int position1, int position2, int *direction)
{
	if (!((position1 - position2 + MAXORIENT) % 2))
		return FALSE;
	switch (position1) {
		case 0:
			*direction = (position2 == 1) ? 1 : 2;
			break;
		case 1:
			*direction = (position2 == 2) ? 2 : 3;
			break;
		case 2:
			*direction = (position2 == 3) ? 3 : 0;
			break;
		case 3:
			*direction = (position2 == 0) ? 0 : 1;
			break;
		default:
			return FALSE;
	}
	return TRUE;
}

#ifdef DEBUG

void
PrintCube(DinoWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++) {
		for (position = 0; position < MAXORIENT; position++)
			(void) printf("%d %d  ", w->dino.cubeLoc[face][position].face,
				   w->dino.cubeLoc[face][position].rotation);
		(void) printf("\n");
	}
	(void) printf("\n");
}

#endif
