/*-
# X-BASED SKEWB
#
#  Skewb.c
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

/* Methods file for Skewb */

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
#include "SkewbP.h"
#include "Skewb2dP.h"
#include "Skewb3dP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/skewb.data"
#endif

static void InitializeSkewb(Widget request, Widget new);
static void DestroySkewb(Widget old);
static Boolean SetValuesSkewb(Widget current, Widget request, Widget new);
static void GetColor(SkewbWidget w, int face, int init);
static void MoveControlCb(SkewbWidget w, int face, int position, int direction);
static void MoveAltCb(SkewbWidget w, int face, int position, int direction);
static void ResetPolyhedrons(SkewbWidget w);
static int  SelectPolyhedrons(SkewbWidget w, int x, int y, int *face, int *position);
static int  NarrowSelection(SkewbWidget w, int *face, int *position, int *direction);
static int  PositionPolyhedrons(SkewbWidget w, int x, int y, int *face, int *position, int *direction);
static void MoveNoPolyhedrons(SkewbWidget w);
static void PracticePolyhedrons(SkewbWidget w);
static void RandomizePolyhedrons(SkewbWidget w);
static void MovePolyhedrons(SkewbWidget w, int face, int position,
			    int direction);
static void ReadDiagonal(SkewbWidget w, int face, int corner, int orient,
			 int size);
static void RotateDiagonal(SkewbWidget w, int rotate, int orient, int size);
static void WriteDiagonal(SkewbWidget w, int face, int corner, int orient,
			  int size);
static void ReadFace(SkewbWidget w, int face, int h);
static void WriteFace(SkewbWidget w, int face, int rotate, int h);
static void RotateFace(SkewbWidget w, int face, int direction);
static void DrawDiamond(SkewbWidget w, int face);
static void DrawTriangle(SkewbWidget w, int face, int position, int offset);
static int  CheckMoveDir(int position1, int position2, int *direction);

SkewbClassRec skewbClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Skewb",	/* class name */
		sizeof (SkewbRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeSkewb,	/* initialize */
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
		(XtWidgetProc) DestroySkewb,	/* destroy */
		NULL,		/* resize */
		NULL,		/* expose */
		(XtSetValuesFunc) SetValuesSkewb,	/* set values */
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

WidgetClass skewbWidgetClass = (WidgetClass) & skewbClassRec;

static SkewbLoc slideNextRow[MAXFACES][MAXORIENT][MAXORIENT / 2] =
{
	{
		{
			{2, CW},
			{1, HALF}},
		{
			{5, CCW},
			{1, STRT}},
		{
			{3, STRT},
			{5, CW}},
		{
			{3, HALF},
			{2, CCW}}
	},
	{
		{
			{4, STRT},
			{5, CW}},
		{
			{0, STRT},
			{5, CCW}},
		{
			{2, CCW},
			{0, HALF}},
		{
			{2, CW},
			{4, HALF}}
	},
	{
		{
			{4, CW},
			{1, CCW}},
		{
			{0, CCW},
			{1, CW}},
		{
			{3, CCW},
			{0, CW}},
		{
			{3, CW},
			{4, CCW}}
	},
	{
		{
			{4, HALF},
			{2, CCW}},
		{
			{0, HALF},
			{2, CW}},
		{
			{5, CW},
			{0, STRT}},
		{
			{5, CCW},
			{4, STRT}}
	},
	{
		{
			{5, CW},
			{1, STRT}},
		{
			{2, CCW},
			{1, HALF}},
		{
			{3, HALF},
			{2, CW}},
		{
			{3, STRT},
			{5, CCW}}
	},
	{
		{
			{0, CW},
			{1, CW}},
		{
			{4, CCW},
			{1, CCW}},
		{
			{3, CW},
			{4, CW}},
		{
			{3, CCW},
			{0, CCW}}
	}
};
static SkewbLoc minToMaj[MAXFACES][MAXORIENT] =
{				/* other equivalent mappings possible */
	{
		{3, CW},
		{2, STRT},
		{1, CCW},
		{5, STRT}},
	{
		{2, STRT},
		{4, CCW},
		{5, HALF},
		{0, CW}},
	{
		{3, STRT},
		{4, STRT},
		{1, STRT},
		{0, STRT}},
	{
		{5, HALF},
		{4, CW},
		{2, STRT},
		{0, CCW}},
	{
		{3, CCW},
		{5, STRT},
		{1, CW},
		{2, STRT}},
	{
		{3, HALF},
		{0, STRT},
		{1, HALF},
		{4, STRT}}
};

static SkewbLoc slideNextFace[MAXFACES][MAXORIENT] =
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

static int  faceToRotate[MAXFACES][MAXORIENT] =
{
	{3, 2, 1, 5},
	{2, 4, 5, 0},
	{3, 4, 1, 0},
	{5, 4, 2, 0},
	{3, 5, 1, 2},
	{3, 0, 1, 4}
};

static SkewbLocPos orthToDiag[MAXFACES][MAXORIENT][MAXORIENT] =
{
	{
		{
			{3, 0, 1},
			{5, 1, 0},
			{3, 0, 3},
			{5, 1, 2}},
		{
			{3, 3, 0},
			{2, 0, 1},
			{3, 3, 2},
			{2, 0, 3}},
		{
			{1, 0, 3},
			{2, 3, 0},
			{1, 0, 1},
			{2, 3, 2}},
		{
			{1, 3, 2},
			{5, 2, 1},
			{1, 3, 0},
			{5, 2, 3}}
	},
	{
		{
			{2, 3, 0},
			{0, 2, 1},
			{2, 3, 2},
			{0, 2, 3}},
		{
			{2, 2, 3},
			{4, 3, 0},
			{2, 2, 1},
			{4, 3, 2}},
		{
			{5, 3, 2},
			{4, 2, 3},
			{5, 3, 0},
			{4, 2, 1}},
		{
			{5, 2, 1},
			{0, 3, 2},
			{5, 2, 3},
			{0, 3, 0}}
	},
	{
		{
			{3, 3, 0},
			{0, 1, 0},
			{3, 3, 2},
			{0, 1, 2}},
		{
			{3, 2, 3},
			{4, 0, 1},
			{3, 2, 1},
			{4, 0, 3}},
		{
			{1, 1, 0},
			{4, 3, 0},
			{1, 1, 2},
			{4, 3, 2}},
		{
			{1, 0, 3},
			{0, 2, 1},
			{1, 0, 1},
			{0, 2, 3}}
	},
	{
		{
			{5, 1, 2},
			{0, 0, 3},
			{5, 1, 0},
			{0, 0, 1}},
		{
			{5, 0, 1},
			{4, 1, 2},
			{5, 0, 3},
			{4, 1, 0}},
		{
			{2, 1, 0},
			{4, 0, 1},
			{2, 1, 2},
			{4, 0, 3}},
		{
			{2, 0, 3},
			{0, 1, 0},
			{2, 0, 1},
			{0, 1, 2}}
	},
	{
		{
			{3, 2, 3},
			{2, 1, 0},
			{3, 2, 1},
			{2, 1, 2}},
		{
			{3, 1, 2},
			{5, 0, 1},
			{3, 1, 0},
			{5, 0, 3}},
		{
			{1, 2, 1},
			{5, 3, 0},
			{1, 2, 3},
			{5, 3, 2}},
		{
			{1, 1, 0},
			{2, 2, 1},
			{1, 1, 2},
			{2, 2, 3}}
	},
	{
		{
			{3, 1, 2},
			{4, 1, 0},
			{3, 1, 0},
			{4, 1, 2}},
		{
			{3, 0, 1},
			{0, 0, 1},
			{3, 0, 3},
			{0, 0, 3}},
		{
			{1, 3, 2},
			{0, 3, 0},
			{1, 3, 0},
			{0, 3, 2}},
		{
			{1, 2, 1},
			{4, 2, 1},
			{1, 2, 3},
			{4, 2, 3}}
	}
};

static void
InitializeSkewb(Widget request, Widget new)
{
	SkewbWidget w = (SkewbWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         face;

	InitMoves();
	ResetPolyhedrons(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	values.foreground = w->skewb.foreground;
	w->skewb.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->skewb.borderColor;
	w->skewb.borderGC = XtGetGC(new, valueMask, &values);
	w->skewb.depth = DefaultDepthOfScreen(XtScreen(w));
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->skewb.foreground;
	w->skewb.inverseGC = XtGetGC(new, valueMask, &values);
	for (face = 0; face < MAXFACES; face++)
		GetColor(w, face, TRUE);
}

static void
DestroySkewb(Widget old)
{
	SkewbWidget w = (SkewbWidget) old;
	int         face;

	for (face = 0; face < MAXFACES; face++)
		XtReleaseGC(old, w->skewb.faceGC[face]);
	XtReleaseGC(old, w->skewb.borderGC);
	XtReleaseGC(old, w->skewb.puzzleGC);
	XtReleaseGC(old, w->skewb.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->skewb.select);
}

static      Boolean
SetValuesSkewb(Widget current, Widget request, Widget new)
{
	SkewbWidget c = (SkewbWidget) current, w = (SkewbWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	int         face;

	if (w->skewb.foreground != c->skewb.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->skewb.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->skewb.puzzleGC);
		w->skewb.puzzleGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->skewb.foreground;
		XtReleaseGC(new, w->skewb.inverseGC);
		w->skewb.inverseGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->skewb.borderColor != c->skewb.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->skewb.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->skewb.borderGC);
		w->skewb.borderGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->skewb.mono || w->skewb.depth == 1) {
		valueMask = GCForeground | GCBackground;
		values.background = w->core.background_pixel;
		values.foreground = w->skewb.foreground;
		for (face = 0; face < MAXFACES; face++) {
			XtReleaseGC(new, w->skewb.faceGC[face]);
			w->skewb.faceGC[face] = XtGetGC(new, valueMask, &values);
		}
		redraw = TRUE;
	}
	for (face = 0; face < MAXFACES; face++) {
		if (strcmp(w->skewb.faceName[face], c->skewb.faceName[face]))
			GetColor(w, face, FALSE);
	}
	if (w->skewb.orient != c->skewb.orient) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	} else if (w->skewb.practice != c->skewb.practice) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	}
	if (w->skewb.currentDirection == SKEWB_RESTORE) {
		SetStartPosition(w);
		w->skewb.currentDirection = SKEWB_IGNORE;
	} else if (w->skewb.currentDirection != SKEWB_IGNORE) {
		MovePolyhedrons(w, w->skewb.currentFace, w->skewb.currentPosition,
				w->skewb.currentDirection);
		w->skewb.currentDirection = SKEWB_IGNORE;
	}
	return redraw;
}

void
QuitSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

void
SelectSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	int         control, alt;

	if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
		     &(w->skewb.currentFace), &(w->skewb.currentPosition)) &&
	    w->skewb.currentPosition != MAXORIENT) {
		control = (int) (event->xkey.state & ControlMask);
		alt = (int) (event->xkey.state &
		     (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask));
		if (control || alt || w->skewb.practice || !CheckSolved(w))
			DrawTriangle(w, w->skewb.currentFace, w->skewb.currentPosition,
				     TRUE);
	} else {
		w->skewb.currentFace = SKEWB_IGNORE;
		w->skewb.currentDirection = SKEWB_IGNORE;
	}
}

void
ReleaseSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	int         control, alt, face, position, count = -1, direction = 0;
	skewbCallbackStruct cb;

	if (w->skewb.currentFace == SKEWB_IGNORE)
		return;
	DrawTriangle(w, w->skewb.currentFace, w->skewb.currentPosition, FALSE);
	control = (int) (event->xkey.state & ControlMask);
	alt = (int) (event->xkey.state &
		     (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask));
	if (!control && !alt && !w->skewb.practice && CheckSolved(w))
		MoveNoPolyhedrons(w);
	else if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
				&face, &position) && position != MAXORIENT &&
		 position != w->skewb.currentPosition) {
		if (alt)
			control = 2;
		else
			control = (control) ? 1 : 0;
		if (face == w->skewb.currentFace)
			count = CheckMoveDir(w->skewb.currentPosition, position, &direction);
		if (count == 1) {
			MoveSkewb(w, face, w->skewb.currentPosition, direction, control);
			if (!control && CheckSolved(w)) {
				cb.reason = SKEWB_SOLVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else if (count == 0)
			MoveNoPolyhedrons(w);
	}
	w->skewb.currentFace = SKEWB_IGNORE;
	w->skewb.currentDirection = SKEWB_IGNORE;
}

void
PracticeSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	PracticePolyhedrons(w);
}

void
PracticeSkewbMaybe(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->skewb.started)
		PracticePolyhedrons(w);
}

void
RandomizeSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizePolyhedrons(w);
}

void
RandomizeSkewbMaybe(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->skewb.started)
		RandomizePolyhedrons(w);
}

void
GetSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         orient, practice, moves;
	skewbCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &orient);
		if (w->skewb.orient != (Boolean) orient) {
			cb.reason = SKEWB_ORIENT;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &practice);
		if (w->skewb.practice != (Boolean) practice) {
			cb.reason = SKEWB_PRACTICE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = SKEWB_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: orient %d, practice %d, moves %d.\n",
			      DATAFILE, orient, practice, moves);
	}
}

void
WriteSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "orient%c %d\n", SYMBOL, (w->skewb.orient) ? 1 : 0);
		(void) fprintf(fp, "practice%c %d\n", SYMBOL, (w->skewb.practice) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

void
UndoSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         face, position, direction, control;

		GetMove(&face, &position, &direction, &control);
		if (direction < 2 * MAXORIENT)
			direction = (direction < MAXORIENT) ? (direction + MAXORIENT / 2) %
				MAXORIENT : 3 * MAXORIENT - direction;
		else
			direction = 5 * MAXORIENT - direction;
		if (control == 2)
			MoveAltCb(w, face, position, direction);
		else if (control)
			MoveControlCb(w, face, position, direction);
		else {
			skewbCallbackStruct cb;

			if (direction >= 2 * MAXORIENT) {
				SkewbLocPos newpos;

				newpos = orthToDiag[face][position][direction % MAXORIENT];
				face = newpos.face;
				position = newpos.position;
				direction = newpos.direction;
			}
			MovePolyhedrons(w, face, position, direction);
			cb.reason = SKEWB_UNDO;
			cb.face = face;
			cb.position = position;
			cb.direction = direction;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

void
SolveSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolvePolyhedrons(w); *//* Sorry, unimplemented */
}

void
OrientizeSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	skewbCallbackStruct cb;

	cb.reason = SKEWB_ORIENT;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

void
MoveSkewbCcw(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput(w, event->xbutton.x, event->xbutton.y, CCW,
		       (int) (event->xbutton.state & ControlMask), FALSE);
}

void
MoveSkewbCw(SkewbWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput(w, event->xbutton.x, event->xbutton.y, CW,
		       (int) (event->xkey.state & ControlMask), FALSE);
}

void
MoveSkewbInput(SkewbWidget w, int x, int y, int direction, int control, int alt)
{
	int         face, position;

	if (!w->skewb.practice && !control && !alt && CheckSolved(w)) {
		MoveNoPolyhedrons(w);
		return;
	}
	if (!PositionPolyhedrons(w, x, y, &face, &position, &direction))
		return;
	if (alt)
		control = 2;
	else
		control = (control) ? 1 : 0;
	if (position == MAXORIENT)
		return;
	MoveSkewb(w, face, position, direction, control);
	if (!control && CheckSolved(w)) {
		skewbCallbackStruct cb;

		cb.reason = SKEWB_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

void
MoveSkewb(SkewbWidget w, int face, int position, int direction, int control)
{
	if (control == 2)
		MoveAltCb(w, face, position, direction);
	else if (control)
		MoveControlCb(w, face, position, direction);
	else {
		SkewbLocPos newpos;
		skewbCallbackStruct cb;

		newpos.face = face;
		newpos.position = position;
		newpos.direction = direction;
		if (direction >= 2 * MAXORIENT)
			newpos = orthToDiag[face][position][direction % MAXORIENT];
		MovePolyhedrons(w, newpos.face, newpos.position, newpos.direction);
		cb.reason = SKEWB_MOVED;
		cb.face = newpos.face;
		cb.position = newpos.position;
		cb.direction = newpos.direction;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	PutMove(face, position, direction, control);
}

static void
GetColor(SkewbWidget w, int face, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->skewb.depth > 1 && !w->skewb.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
				w->skewb.faceName[face], &colorCell, &rgb)) {
			values.foreground = w->skewb.faceColor[face] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->skewb.faceGC[face]);
			w->skewb.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined",
				       w->skewb.faceName[face]);
			XtWarning(buf);
		}
	}
	values.foreground = w->skewb.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->skewb.faceGC[face]);
	w->skewb.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
}

static void
MoveControlCb(SkewbWidget w, int face, int position, int direction)
{
	skewbCallbackStruct cb;
	int         newFace, rotate;

	if (direction >= 2 * MAXORIENT) {
		SkewbLocPos newpos;

		newpos = orthToDiag[face][position][direction % MAXORIENT];
		face = newpos.face;
		position = newpos.position;
		direction = newpos.direction;
	}
	MovePolyhedrons(w, face, position, direction);
	cb.reason = SKEWB_CONTROL;
	cb.face = face;
	cb.position = position;
	cb.direction = direction;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	newFace = minToMaj[face][position].face;
	rotate = minToMaj[face][position].rotation % MAXORIENT;
	direction = (rotate + direction) % MAXORIENT;
	position = (position + rotate + 2) % MAXORIENT;
	MovePolyhedrons(w, newFace, position, direction);
	cb.face = newFace;
	cb.position = position;
	cb.direction = direction;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MoveAltCb(SkewbWidget w, int face, int position, int direction)
{
	skewbCallbackStruct cb;

	MovePolyhedrons(w, face, position, direction);
	cb.reason = SKEWB_CONTROL;
	cb.face = face;
	cb.position = position;
	cb.direction = direction;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
ResetPolyhedrons(SkewbWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXCUBES; position++) {
			w->skewb.cubeLoc[face][position].face = face;
			w->skewb.cubeLoc[face][position].rotation = STRT - MAXORIENT;
		}
	FlushMoves(w);
	w->skewb.started = FALSE;
}

static int
SelectPolyhedrons(SkewbWidget w, int x, int y, int *face, int *position)
{
	if (w->skewb.dim == 2)
		return SelectPolyhedrons2D((Skewb2DWidget) w, x, y,
					   face, position);
	else if (w->skewb.dim == 3)
		return SelectPolyhedrons3D((Skewb3DWidget) w, x, y,
					   face, position);
	return FALSE;
}

static int
NarrowSelection(SkewbWidget w, int *face, int *position, int *direction)
{
	if (w->skewb.dim == 2)
		return NarrowSelection2D((Skewb2DWidget) w, face, position, direction);
	else if (w->skewb.dim == 3)
		return NarrowSelection3D((Skewb3DWidget) w, face, position, direction);
	return FALSE;
}

static int
PositionPolyhedrons(SkewbWidget w, int x, int y, int *face, int *position, int *direction)
{
	if (!SelectPolyhedrons(w, x, y, face, position))
		return FALSE;
	return NarrowSelection(w, face, position, direction);
}

static void
MoveNoPolyhedrons(SkewbWidget w)
{
	skewbCallbackStruct cb;

	cb.reason = SKEWB_ILLEGAL;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
PracticePolyhedrons(SkewbWidget w)
{
	skewbCallbackStruct cb;

	cb.reason = SKEWB_PRACTICE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
RandomizePolyhedrons(SkewbWidget w)
{
	skewbCallbackStruct cb;
	int         face, position, direction;
	int         big = MAXCUBES * 3 + NRAND(2);

	if (w->skewb.practice)
		PracticePolyhedrons(w);
	cb.reason = SKEWB_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
	big = 3;
#endif

	while (big--) {
		face = NRAND(MAXFACES);
		position = NRAND(MAXORIENT);
		direction = ((NRAND(2)) ? position + 1 : position + 3) % MAXORIENT;
		MoveSkewb(w, face, position, direction, FALSE);
	}
	FlushMoves(w);
	cb.reason = SKEWB_RANDOMIZE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (CheckSolved(w)) {
		cb.reason = SKEWB_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MovePolyhedrons(SkewbWidget w, int face, int position, int direction)
{
	int         newFace, newDirection, newCorner, k, size, rotate;

	if (direction < 2 * MAXORIENT) {
		/* position as MAXORIENT is ambiguous */
		for (size = MINOR; size <= MAJOR; size++) {
			ReadDiagonal((SkewbWidget) w, face, position, 0, size);
			for (k = 1; k <= MAXROTATE; k++) {
				newFace = slideNextRow[face][position][direction / 2].face;
				rotate = slideNextRow[face][position][direction / 2].rotation %
					MAXORIENT;
				newDirection = (rotate + direction) % MAXORIENT;
				newCorner = (rotate + position) % MAXORIENT;
				if (k != MAXROTATE)
					ReadDiagonal((SkewbWidget) w, newFace, newCorner, k, size);
				RotateDiagonal((SkewbWidget) w, rotate, k - 1, size);
				WriteDiagonal(w, newFace, newCorner, k - 1, size);
				face = newFace;
				position = newCorner;
				direction = newDirection;
			}
			if (size == MINOR) {
				newFace = minToMaj[face][position].face;
				rotate = minToMaj[face][position].rotation % MAXORIENT;
				direction = (rotate + direction) % MAXORIENT;
				position = (position + rotate + 2) % MAXORIENT;
				face = newFace;
			}
		}
	} else {
		RotateFace(w, faceToRotate[face][direction % MAXORIENT], CW);
		RotateFace(w, faceToRotate[face][(direction + 2) % MAXORIENT], CCW);
		ReadFace((SkewbWidget) w, face, 0);
		for (k = 1; k <= MAXORIENT; k++) {
			newFace = slideNextFace[face][direction % MAXORIENT].face;
			rotate = slideNextFace[face][direction % MAXORIENT].rotation;
			newDirection = (rotate + direction) % MAXORIENT;
			if (k != MAXORIENT)
				ReadFace((SkewbWidget) w, newFace, k);
			WriteFace(w, newFace, rotate, k - 1);
			face = newFace;
			direction = newDirection;
		}
	}
}

static void
ReadDiagonal(SkewbWidget w, int face, int corner, int orient, int size)
{
	int         g;

	if (size == MINOR)
		w->skewb.minorLoc[orient] = w->skewb.cubeLoc[face][corner];
	else {			/* size == MAJOR */
		for (g = 1; g < MAXORIENT; g++)
			w->skewb.majorLoc[orient][g - 1] =
				w->skewb.cubeLoc[face][(corner + g) % MAXORIENT];
		w->skewb.majorLoc[orient][MAXORIENT - 1] =
			w->skewb.cubeLoc[face][MAXORIENT];
	}
}

static void
RotateDiagonal(SkewbWidget w, int rotate, int orient, int size)
{
	int         g;

	if (size == MINOR)
		w->skewb.minorLoc[orient].rotation =
			(w->skewb.minorLoc[orient].rotation + rotate) % MAXORIENT;
	else			/* size == MAJOR */
		for (g = 0; g < MAXORIENT; g++)
			w->skewb.majorLoc[orient][g].rotation =
				(w->skewb.majorLoc[orient][g].rotation + rotate) % MAXORIENT;
}

static void
WriteDiagonal(SkewbWidget w, int face, int corner, int orient, int size)
{
	int         g, h;

	if (size == MINOR) {
		w->skewb.cubeLoc[face][corner] = w->skewb.minorLoc[orient];
		DrawTriangle(w, face, corner, FALSE);
	} else {		/* size == MAJOR */
		w->skewb.cubeLoc[face][MAXORIENT] =
			w->skewb.majorLoc[orient][MAXORIENT - 1];
		DrawDiamond(w, face);
		for (g = 1; g < MAXORIENT; g++) {
			h = (corner + g) % MAXORIENT;
			w->skewb.cubeLoc[face][h] = w->skewb.majorLoc[orient][g - 1];
			DrawTriangle(w, face, h, FALSE);
		}
	}
}

static void
ReadFace(SkewbWidget w, int face, int h)
{
	int         position;

	for (position = 0; position < MAXCUBES; position++)
		w->skewb.rowLoc[h][position] = w->skewb.cubeLoc[face][position];
}

static void
WriteFace(SkewbWidget w, int face, int rotate, int h)
{
	int         corner, newCorner;

	for (corner = 0; corner < MAXORIENT; corner++) {
		newCorner = (corner + rotate) % MAXORIENT;
		w->skewb.cubeLoc[face][newCorner] = w->skewb.rowLoc[h][corner];
		w->skewb.cubeLoc[face][newCorner].rotation =
			(w->skewb.cubeLoc[face][newCorner].rotation + rotate) % MAXORIENT;
		DrawTriangle(w, face, (corner + rotate) % MAXORIENT, FALSE);
	}
	w->skewb.cubeLoc[face][MAXORIENT] = w->skewb.rowLoc[h][MAXORIENT];
	w->skewb.cubeLoc[face][MAXORIENT].rotation =
		(w->skewb.cubeLoc[face][MAXORIENT].rotation + rotate) % MAXORIENT;
	DrawDiamond(w, face);
}

static void
RotateFace(SkewbWidget w, int face, int direction)
{
	int         corner;

	/* Read Face */
	for (corner = 0; corner < MAXORIENT; corner++)
		w->skewb.faceLoc[corner] = w->skewb.cubeLoc[face][corner];
	/* Write Face */
	for (corner = 0; corner < MAXORIENT; corner++) {
		w->skewb.cubeLoc[face][corner] = (direction == CW) ?
			w->skewb.faceLoc[(corner + MAXORIENT - 1) % MAXORIENT] :
			w->skewb.faceLoc[(corner + 1) % MAXORIENT];
		w->skewb.cubeLoc[face][corner].rotation =
			(w->skewb.cubeLoc[face][corner].rotation + direction) % MAXORIENT;
		DrawTriangle(w, face, corner, FALSE);
	}
	w->skewb.cubeLoc[face][MAXORIENT].rotation =
		(w->skewb.cubeLoc[face][MAXORIENT].rotation + direction) % MAXORIENT;
	DrawDiamond(w, face);
}

void
DrawAllPolyhedrons(SkewbWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++) {
		DrawDiamond(w, face);
		for (position = 0; position < MAXORIENT; position++)
			DrawTriangle(w, face, position, FALSE);
	}
}

static void
DrawDiamond(SkewbWidget w, int face)
{
	if (w->skewb.dim == 2)
		DrawDiamond2D((Skewb2DWidget) w, face);
	else if (w->skewb.dim == 3)
		DrawDiamond3D((Skewb3DWidget) w, face);
}

static void
DrawTriangle(SkewbWidget w, int face, int position, int offset)
{
	if (w->skewb.dim == 2)
		DrawTriangle2D((Skewb2DWidget) w, face, position, offset);
	else if (w->skewb.dim == 3)
		DrawTriangle3D((Skewb3DWidget) w, face, position, offset);
}

Boolean
CheckSolved(SkewbWidget w)
{
	int         face, position;
	SkewbLoc    test;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXCUBES; position++) {
			if (!position) {
				test.face = w->skewb.cubeLoc[face][position].face;
				test.rotation = w->skewb.cubeLoc[face][position].rotation;
			} else if (test.face !=		/*face */
				   w->skewb.cubeLoc[face][position].face ||
				   (w->skewb.orient && test.rotation !=		/*STRT - MAXORIENT */
				  w->skewb.cubeLoc[face][position].rotation))
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
			*direction = (position2 == 1) ? 2 : 3;
			break;
		case 1:
			*direction = (position2 == 2) ? 3 : 0;
			break;
		case 2:
			*direction = (position2 == 3) ? 0 : 1;
			break;
		case 3:
			*direction = (position2 == 0) ? 1 : 2;
			break;
		default:
			return FALSE;
	}
	*direction += 2 * MAXORIENT;
	return TRUE;
}

#ifdef DEBUG

void
PrintCube(SkewbWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++) {
		for (position = 0; position < MAXCUBES; position++)
			(void) printf("%d %d  ", w->skewb.cubeLoc[face][position].face,
				  w->skewb.cubeLoc[face][position].rotation);
		(void) printf("\n");
	}
	(void) printf("\n");
}

#endif
