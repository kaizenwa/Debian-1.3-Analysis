/*-
# X-BASED TRIANGLES
#
#  Triangles.c
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

/* Methods file for Triangles */

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
#include "TrianglesP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/triangles.data"
#endif

static void InitializeTriangles(Widget request, Widget new);
static void ExposeTriangles(Widget new, XEvent * event, Region region);
static void ResizeTriangles(TrianglesWidget w);
static void DestroyTriangles(Widget old);
static Boolean SetValuesTriangles(Widget current, Widget request, Widget new);
static void QuitTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void MoveTrianglesTl(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void MoveTrianglesTr(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void MoveTrianglesLeft(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void MoveRightTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void MoveTrianglesBl(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void MoveTrianglesBr(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void SelectTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void ReleaseTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeTrianglesMaybe(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void GetTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void WriteTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void UndoTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void SolveTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void IncrementTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static void DecrementTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs);
static int  MoveTriangles(TrianglesWidget w, int direction);
static int  PositionToTile(TrianglesWidget w, int x, int y, int *row, int *trbl, int *tlbr, int *orient);
static void SelectTiles(TrianglesWidget w);
static void CheckTiles(TrianglesWidget w);
static void ResetTiles(TrianglesWidget w);
static void ResizeTiles(TrianglesWidget w);
static void MoveNoTiles(TrianglesWidget w);
static int  MoveTilesDir(TrianglesWidget w, int direction);
static void RandomizeTiles(TrianglesWidget w);
static void MoveTiles(TrianglesWidget w, int from, int orient);
static int  ExchangeTiles(TrianglesWidget w, int pos1, int pos2);
static int  TileNextToSpace(TrianglesWidget w, int rowType, int orient, int direction);
static void DrawFrame(TrianglesWidget w, GC gc);
static void DrawTile(TrianglesWidget w, GC tileGC, GC borderGC, int pos, int orient, int offset);
static int  ToOrient(int row, int trbl, int tlbr);
static int  ToPosition(int row, int trbl, int tlbr);
static int  Sqrt(int i);

static char defaultTranslationsTriangles[] =
"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
   <KeyPress>Home: MoveTl()\n\
   <KeyPress>KP_7: MoveTl()\n\
   <KeyPress>R7: MoveTl()\n\
   <KeyPress>Prior: MoveTr()\n\
   <KeyPress>KP_9: MoveTr()\n\
   <KeyPress>R9: MoveTr()\n\
   <KeyPress>Left: MoveLeft()\n\
   <KeyPress>KP_4: MoveLeft()\n\
   <KeyPress>R10: MoveLeft()\n\
   <KeyPress>Right: MoveRight()\n\
   <KeyPress>KP_6: MoveRight()\n\
   <KeyPress>R12: MoveRight()\n\
   <KeyPress>End: MoveBl()\n\
   <KeyPress>KP_1: MoveBl()\n\
   <KeyPress>R13: MoveBl()\n\
   <KeyPress>Next: MoveBr()\n\
   <KeyPress>KP_3: MoveBr()\n\
   <KeyPress>R15: MoveBr()\n\
   <Btn1Down>: Select()\n\
   <Btn1Up>: Release()\n\
   <KeyPress>r: Randomize()\n\
   <Btn3Down>(2+): Randomize()\n\
   <Btn3Down>: RandomizeMaybe()\n\
   <KeyPress>g: Get()\n\
   <KeyPress>w: Write()\n\
   <KeyPress>u: Undo()\n\
   <KeyPress>s: Solve()\n\
   <KeyPress>i: Increment()\n\
   <KeyPress>d: Decrement()";

static XtActionsRec actionsListTriangles[] =
{
	{"Quit", (XtActionProc) QuitTriangles},
	{"MoveTl", (XtActionProc) MoveTrianglesTl},
	{"MoveTr", (XtActionProc) MoveTrianglesTr},
	{"MoveLeft", (XtActionProc) MoveTrianglesLeft},
	{"MoveRight", (XtActionProc) MoveRightTriangles},
	{"MoveBl", (XtActionProc) MoveTrianglesBl},
	{"MoveBr", (XtActionProc) MoveTrianglesBr},
	{"Select", (XtActionProc) SelectTriangles},
	{"Release", (XtActionProc) ReleaseTriangles},
	{"Randomize", (XtActionProc) RandomizeTriangles},
	{"RandomizeMaybe", (XtActionProc) RandomizeTrianglesMaybe},
	{"Get", (XtActionProc) GetTriangles},
	{"Write", (XtActionProc) WriteTriangles},
	{"Undo", (XtActionProc) UndoTriangles},
	{"Solve", (XtActionProc) SolveTriangles},
	{"Increment", (XtActionProc) IncrementTriangles},
	{"Decrement", (XtActionProc) DecrementTriangles}
};

static XtResource resourcesTriangles[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(TrianglesWidget, triangles.username), XtRString, "nobody"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
	 XtOffset(TrianglesWidget, triangles.foreground), XtRString,
	 XtDefaultForeground},
	{XtNtileColor, XtCColor, XtRPixel, sizeof (Pixel),
	 XtOffset(TrianglesWidget, triangles.tileColor), XtRString,
	 XtDefaultForeground},
	{XtNtileBorder, XtCColor, XtRPixel, sizeof (Pixel),
	 XtOffset(TrianglesWidget, triangles.borderColor), XtRString,
	 XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(TrianglesWidget, core.width), XtRString, "200"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(TrianglesWidget, core.height), XtRString, "173"},
	{XtNsize, XtCSize, XtRInt, sizeof (int),
	 XtOffset(TrianglesWidget, triangles.size), XtRString, "4"},	/* DEFAULTTRIS */
	{XtNbase, XtCBase, XtRInt, sizeof (int),
	 XtOffset(TrianglesWidget, triangles.base), XtRString, "10"},
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(TrianglesWidget, triangles.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(TrianglesWidget, triangles.select), XtRCallback, NULL}
};

TrianglesClassRec trianglesClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Triangles",	/* class name */
		sizeof (TrianglesRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeTriangles,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListTriangles,	/* actions */
		XtNumber(actionsListTriangles),		/* num actions */
		resourcesTriangles,	/* resources */
		XtNumber(resourcesTriangles),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		(XtWidgetProc) DestroyTriangles,	/* destroy */
		(XtWidgetProc) ResizeTriangles,		/* resize */
		(XtExposeProc) ExposeTriangles,		/* expose */
		(XtSetValuesFunc) SetValuesTriangles,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		NULL,		/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsTriangles,	/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	}
};

WidgetClass trianglesWidgetClass = (WidgetClass) & trianglesClassRec;

static XPoint triangleUnit[MAXORIENT][ROWTYPES + 1] =
{
	{
		{0, 0},
		{-1, -1},
		{2, 0},
		{-1, 1}},
	{
		{0, 0},
		{1, 1},
		{-2, 0},
		{1, -1}}
};
static XPoint triangleList[MAXORIENT][ROWTYPES + 1];

static void
InitializeTriangles(Widget request, Widget new)
{
	TrianglesWidget w = (TrianglesWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;

	w->triangles.tileOfPosition = NULL;
	CheckTiles(w);
	InitMoves();
	ResetTiles(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.foreground = w->triangles.foreground;
	values.background = w->core.background_pixel;
	w->triangles.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->triangles.tileColor;
	w->triangles.tileGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->triangles.borderColor;
	w->triangles.borderGC = XtGetGC(new, valueMask, &values);
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->triangles.foreground;
	w->triangles.inverseGC = XtGetGC(new, valueMask, &values);
	ResizeTriangles(w);
}

static void
DestroyTriangles(Widget old)
{
	TrianglesWidget w = (TrianglesWidget) old;

	XtReleaseGC(old, w->triangles.tileGC);
	XtReleaseGC(old, w->triangles.borderGC);
	XtReleaseGC(old, w->triangles.puzzleGC);
	XtReleaseGC(old, w->triangles.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->triangles.select);
}

static void
ResizeTriangles(TrianglesWidget w)
{
	double      sqrt_3 = 1.73205080756887729352744634150587237;
	XPoint      tempSize;

	w->triangles.delta.x = 5;
	w->triangles.delta.y = 3;
	w->triangles.offset.x = MAX(((int) w->core.width -
			       w->triangles.delta.x) / w->triangles.size, 0);
	w->triangles.offset.y = MAX(((int) w->core.height -
			   2 * w->triangles.delta.y) / w->triangles.size, 0);
	tempSize.y = (int) ((double) w->triangles.offset.x * sqrt_3 / 2.0);
	tempSize.x = (int) ((double) w->triangles.offset.y * 2.0 / sqrt_3);
	if (tempSize.y < w->triangles.offset.y)
		w->triangles.offset.y = tempSize.y;
	else			/* tempSize.x <=  w->triangles.wid */
		w->triangles.offset.x = tempSize.x;
	w->triangles.puzzleSize.x = w->triangles.offset.x * w->triangles.size +
		w->triangles.delta.x + 2;
	w->triangles.puzzleSize.y = w->triangles.offset.y * w->triangles.size +
		w->triangles.delta.y + 2;
	w->triangles.puzzleOffset.x = ((int) w->core.width -
				       w->triangles.puzzleSize.x + 2) / 2;
	w->triangles.puzzleOffset.y = ((int) w->core.height -
				       w->triangles.puzzleSize.y + 2) / 2;
	w->triangles.tileSize.x = MAX(w->triangles.offset.x - w->triangles.delta.x,
				      0);
	w->triangles.tileSize.y = MAX(w->triangles.offset.y - w->triangles.delta.y,
				      0);
	ResizeTiles(w);
}

static void
ExposeTriangles(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	TrianglesWidget w = (TrianglesWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->triangles.puzzleGC);
		DrawAllTiles(w, w->triangles.tileGC, w->triangles.borderGC);
	}
}

static      Boolean
SetValuesTriangles(Widget current, Widget request, Widget new)
{
	TrianglesWidget c = (TrianglesWidget) current, w = (TrianglesWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	Boolean     redrawTiles = FALSE;

	CheckTiles(w);
	if (w->triangles.foreground != c->triangles.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->triangles.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->triangles.puzzleGC);
		w->triangles.puzzleGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->triangles.foreground;
		XtReleaseGC(new, w->triangles.inverseGC);
		w->triangles.inverseGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->triangles.tileColor != c->triangles.tileColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->triangles.tileColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->triangles.tileGC);
		w->triangles.tileGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->triangles.borderColor != c->triangles.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->triangles.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->triangles.borderGC);
		w->triangles.borderGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->triangles.size != c->triangles.size ||
	    w->triangles.base != c->triangles.base) {
		ResetTiles(w);
		ResizeTriangles(w);
		redraw = TRUE;
	} else if (w->triangles.offset.x != c->triangles.offset.x ||
		   w->triangles.offset.y != c->triangles.offset.y) {
		ResizeTriangles(w);
		redraw = TRUE;
	}
	if (redrawTiles && !redraw && XtIsRealized(new) && new->core.visible) {
		DrawFrame(c, c->triangles.inverseGC);
		DrawAllTiles(c, c->triangles.inverseGC, c->triangles.inverseGC);
		DrawFrame(w, w->triangles.puzzleGC);
		DrawAllTiles(w, w->triangles.tileGC, w->triangles.borderGC);
	}
	return (redraw);
}

static void
QuitTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	int         pos, orient, row, trbl, tlbr;

	pos = PositionToTile(w, event->xbutton.x, event->xbutton.y,
			     &row, &trbl, &tlbr, &orient);
	if (pos >= 0) {
		if (CheckSolved(w)) {
			MoveNoTiles(w);
			w->triangles.currentPosition = -1;
			return;
		}
		w->triangles.currentPosition = pos;
		w->triangles.currentPositionOrient = orient;
		w->triangles.currentRow[TRBL] = trbl;
		w->triangles.currentRow[TLBR] = tlbr;
		w->triangles.currentRow[ROW] = row;
		if (w->triangles.spacePosition[UP] != w->triangles.currentPosition &&
		    w->triangles.spacePosition[DOWN] != w->triangles.currentPosition)
			DrawTile(w, w->triangles.borderGC, w->triangles.tileGC,
				 w->triangles.currentPosition, w->triangles.currentPositionOrient, TRUE);
		else
			w->triangles.currentPosition = -1;
	} else
		w->triangles.currentPosition = -1;
}

static void
ReleaseTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	if (w->triangles.currentPosition == -1)
		return;
	DrawTile(w, w->triangles.inverseGC, w->triangles.inverseGC,
		 w->triangles.currentPosition, w->triangles.currentPositionOrient, TRUE);
	DrawTile(w, w->triangles.tileGC, w->triangles.borderGC,
		 w->triangles.currentPosition, w->triangles.currentPositionOrient, FALSE);
	SelectTiles(w);
	w->triangles.currentPosition = -1;
}

static void
RandomizeTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizeTiles(w);
}

static void
RandomizeTrianglesMaybe(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->triangles.started)
		RandomizeTiles(w);
}

static void
GetTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, size, moves;
	trianglesCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &size);
		if (size >= MINTRIANGLES) {
			for (i = w->triangles.size; i < size; i++) {
				cb.reason = TRIANGLES_INC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->triangles.size; i > size; i--) {
				cb.reason = TRIANGLES_DEC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: size %d should be between %d and MAXINT\n",
				      DATAFILE, size, MINTRIANGLES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = TRIANGLES_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		SetStartPosition(w);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: size %d, moves %d.\n", DATAFILE, size, moves);
	}
}

static void
WriteTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "size%c %d\n", SYMBOL, w->triangles.size);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         direction;

		GetMove(&direction);
		direction = (direction + (COORD / 2)) % COORD;
		if (MoveTilesDir(w, direction)) {
			trianglesCallbackStruct cb;

			cb.reason = TRIANGLES_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolveTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolveTiles(w); *//* Sorry, unimplemented */
}

static void
IncrementTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	trianglesCallbackStruct cb;

	cb.reason = TRIANGLES_INC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
DecrementTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	trianglesCallbackStruct cb;

	if (w->triangles.size <= MINTRIANGLES)
		return;
	cb.reason = TRIANGLES_DEC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MoveTrianglesTl(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveTriangles(w, TL);
}

static void
MoveTrianglesTr(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveTriangles(w, TR);
}

static void
MoveTrianglesLeft(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveTriangles(w, LEFT);
}

static void
MoveRightTriangles(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveTriangles(w, RIGHT);
}

static void
MoveTrianglesBl(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveTriangles(w, BL);
}

static void
MoveTrianglesBr(TrianglesWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveTriangles(w, BR);
}

static int
MoveTriangles(TrianglesWidget w, int direction)
{
	trianglesCallbackStruct cb;

	if (CheckSolved(w)) {
		MoveNoTiles(w);
		return FALSE;
	}
	if (!MoveTrianglesDir(w, direction)) {
		cb.reason = TRIANGLES_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return FALSE;
	}
	if (CheckSolved(w)) {
		cb.reason = TRIANGLES_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	return TRUE;
}

int
MoveTrianglesDir(TrianglesWidget w, int direction)
{
	trianglesCallbackStruct cb;

	if (MoveTilesDir(w, direction)) {
		cb.reason = TRIANGLES_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		PutMove(direction);
		return TRUE;
	}
	return FALSE;
}

static int
PositionToTile(TrianglesWidget w, int x, int y, int *row, int *trbl, int *tlbr, int *orient)
{
	int         sumX, sumY, sumX2;

	sumX = w->triangles.size * w->triangles.offset.x + w->triangles.delta.x;
	sumY = w->triangles.size * w->triangles.offset.y + w->triangles.delta.y + 1;
	sumX2 = sumX / 2;
	x -= w->triangles.puzzleOffset.x;
	y -= w->triangles.puzzleOffset.y;
	if (x * (sumY + w->triangles.delta.y) + y * (sumX2 + 1) <
	    (sumX2 + 1) * (sumY + w->triangles.delta.y) ||
	    x * (sumY + w->triangles.delta.y) - y * (sumX2 - 1) >
	    (sumX2 - 1) * (sumY + w->triangles.delta.y) ||
	    y > sumY - w->triangles.delta.y)
		return -1;
	*row = (y - w->triangles.delta.y) / w->triangles.offset.y;
	*trbl = (x - sumX2 - 1 + *row * w->triangles.offset.x / 2) /
		w->triangles.offset.x;
	*trbl += ((x - (*trbl + 1) * w->triangles.offset.x) *
		  (sumY + w->triangles.delta.y) + y * (sumX2 + 1)) /
		((sumX2 + 1) * (sumY + w->triangles.delta.y));
	*tlbr = (-x + sumX2 - 1 + *row * w->triangles.offset.x / 2) /
		w->triangles.offset.x;
	*tlbr += 1 + ((-x - (*tlbr + 1) * w->triangles.offset.x) *
		      (sumY + w->triangles.delta.y) + y * (sumX2 - 1)) /
		((sumX2 - 1) * (sumY + w->triangles.delta.y));
	if (*row >= 0 && *trbl >= 0 && *tlbr >= 0 && *row < w->triangles.size &&
	    *trbl < w->triangles.size && *tlbr < w->triangles.size) {
		*orient = ToOrient(*row, *trbl, *tlbr);
		return ToPosition(*row, *trbl, *tlbr);
	} else
		return -1;
}

static void
SelectTiles(TrianglesWidget w)
{
	trianglesCallbackStruct cb;
	int         rowType = -1, l, orient, next;

	/* Are the spaces in a "row" with the mouse click?
	   (If two, then one clicked on a space). */
	for (l = 0; l < ROWTYPES; l++)
		if (w->triangles.currentRow[l] == w->triangles.spaceRow[DOWN][l] &&
		w->triangles.currentRow[l] == w->triangles.spaceRow[UP][l]) {
			if (rowType == -1)
				rowType = l;
			else {
				cb.reason = TRIANGLES_SPACE;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				return;
			}
		}
	if (rowType >= 0) {
		if (w->triangles.currentPosition <
		    w->triangles.spacePosition[w->triangles.currentPositionOrient]) {
			orient = (w->triangles.spacePosition[UP] + (rowType == TRBL) <
			       w->triangles.spacePosition[DOWN]) ? UP : DOWN;
			while (w->triangles.currentPosition <
			       w->triangles.spacePosition[w->triangles.currentPositionOrient]) {
				next = TileNextToSpace(w, rowType, orient, UP);
				orient = !orient;
				MoveTiles(w, next, orient);
				cb.reason = TRIANGLES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				switch (rowType) {
					case TLBR:
						PutMove(BR);
						break;
					case TRBL:
						PutMove(BL);
						break;
					case ROW:
						PutMove(RIGHT);
						break;
					default:
						(void) printf("SelectTiles: rowType %d\n", rowType);
				}
			}
		} else {	/*w->triangles.currentPosition >
				   w->triangles.spacePosition[w->triangles.currentPositionOrient] */
			orient = (w->triangles.spacePosition[UP] + 2 * (rowType == TRBL) >
			       w->triangles.spacePosition[DOWN]) ? UP : DOWN;
			while (w->triangles.currentPosition >
			       w->triangles.spacePosition[w->triangles.currentPositionOrient]) {
				next = TileNextToSpace(w, rowType, orient, DOWN);
				orient = !orient;
				MoveTiles(w, next, orient);
				cb.reason = TRIANGLES_MOVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				switch (rowType) {
					case TLBR:
						PutMove(TL);
						break;
					case TRBL:
						PutMove(TR);
						break;
					case ROW:
						PutMove(LEFT);
						break;
					default:
						(void) printf("SelectTiles: rowType %d\n", rowType);
				}
			}
		}
	} else {
		cb.reason = TRIANGLES_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return;
	}
	if (CheckSolved(w)) {
		cb.reason = TRIANGLES_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
CheckTiles(TrianglesWidget w)
{
	char        buf[121];

	if (w->triangles.size < MINTRIANGLES) {
		(void) sprintf(buf,
		"Number of Triangles on a edge out of bounds, use %d..MAXINT",
			       MINTRIANGLES);
		XtWarning(buf);
		w->triangles.size = DEFAULTTRIANGLES;
	}
	w->triangles.base = 10;
}

static void
ResetTiles(TrianglesWidget w)
{
	int         i;

	w->triangles.sizeSize = w->triangles.size * w->triangles.size;
	if (w->triangles.tileOfPosition)
		(void) free((void *) w->triangles.tileOfPosition);
	if (!(w->triangles.tileOfPosition = (int *)
	      malloc(sizeof (int) * w->triangles.sizeSize)))
		            XtError("Not enough memory, exiting.");

	if (startPosition)
		(void) free((void *) startPosition);
	if (!(startPosition = (int *)
	      malloc(sizeof (int) * w->triangles.sizeSize)))
		            XtError("Not enough memory, exiting.");

	w->triangles.spacePosition[UP] = w->triangles.sizeSize - 1;
	w->triangles.spaceRow[UP][TRBL] = w->triangles.size - 1;	/*i */
	w->triangles.spaceRow[UP][TLBR] = 0;	/*j */
	w->triangles.spaceRow[UP][ROW] = w->triangles.size - 1;		/*k */
	if (w->triangles.size > 1) {
		w->triangles.spacePosition[DOWN] = w->triangles.sizeSize - 2;
		w->triangles.spaceRow[DOWN][TRBL] = w->triangles.size - 2;	/*i */
		w->triangles.spaceRow[DOWN][TLBR] = 0;	/*j */
		w->triangles.spaceRow[DOWN][ROW] = w->triangles.size - 1;	/*k */
		w->triangles.tileOfPosition[w->triangles.sizeSize - 2] = -1;
	}
	w->triangles.tileOfPosition[w->triangles.sizeSize - 1] = 0;
	for (i = 1; i < w->triangles.sizeSize - 1; i++)
		w->triangles.tileOfPosition[i - 1] = i;
	FlushMoves(w);
	w->triangles.currentPosition = -1;
	w->triangles.started = FALSE;
}

static void
ResizeTiles(TrianglesWidget w)
{
	int         i, j;

	for (j = 0; j < MAXORIENT; j++)
		for (i = 0; i <= ROWTYPES; i++) {
			triangleList[j][i].x = (w->triangles.tileSize.x / 2) *
				triangleUnit[j][i].x;
			triangleList[j][i].y = w->triangles.tileSize.y *
				triangleUnit[j][i].y;
		}
	w->triangles.digitOffset.x = 3;
	w->triangles.digitOffset.y = 2;
}

static void
MoveNoTiles(TrianglesWidget w)
{
	trianglesCallbackStruct cb;

	cb.reason = TRIANGLES_IGNORE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static int
MoveTilesDir(TrianglesWidget w, int direction)
{
	int         orient;

	switch (direction) {
		case TR:
			if (w->triangles.spaceRow[UP][TRBL] == w->triangles.spaceRow[DOWN][TRBL]
			    && w->triangles.spaceRow[UP][ROW] != w->triangles.size - 1) {
				orient = (w->triangles.spacePosition[UP] + 2 >
				w->triangles.spacePosition[DOWN]) ? UP : DOWN;
				MoveTiles(w, TileNextToSpace(w, TRBL, orient, DOWN), !orient);
				return TRUE;
			}
			break;
		case RIGHT:
			if (w->triangles.spaceRow[UP][ROW] == w->triangles.spaceRow[DOWN][ROW]
			    && w->triangles.spaceRow[UP][TRBL] != 0) {
				orient = (w->triangles.spacePosition[UP] <
				w->triangles.spacePosition[DOWN]) ? UP : DOWN;
				MoveTiles(w, TileNextToSpace(w, ROW, orient, UP), !orient);
				return TRUE;
			}
			break;
		case BR:
			if (w->triangles.spaceRow[UP][TLBR] == w->triangles.spaceRow[DOWN][TLBR]
			    && w->triangles.spaceRow[UP][TRBL] != 0) {
				orient = (w->triangles.spacePosition[UP] <
				w->triangles.spacePosition[DOWN]) ? UP : DOWN;
				MoveTiles(w, TileNextToSpace(w, TLBR, orient, UP), !orient);
				return TRUE;
			}
			break;
		case BL:
			if (w->triangles.spaceRow[UP][TRBL] == w->triangles.spaceRow[DOWN][TRBL]
			    && w->triangles.spaceRow[UP][TLBR] != 0) {
				orient = (w->triangles.spacePosition[UP] + 1 <
				w->triangles.spacePosition[DOWN]) ? UP : DOWN;
				MoveTiles(w, TileNextToSpace(w, TRBL, orient, UP), !orient);
				return TRUE;
			}
			break;
		case LEFT:
			if (w->triangles.spaceRow[UP][ROW] == w->triangles.spaceRow[DOWN][ROW]
			    && w->triangles.spaceRow[UP][TLBR] != 0) {
				orient = (w->triangles.spacePosition[UP] >
				w->triangles.spacePosition[DOWN]) ? UP : DOWN;
				MoveTiles(w, TileNextToSpace(w, ROW, orient, DOWN), !orient);
				return TRUE;
			}
			break;
		case TL:
			if (w->triangles.spaceRow[UP][TLBR] == w->triangles.spaceRow[DOWN][TLBR]
			    && w->triangles.spaceRow[UP][ROW] != w->triangles.size - 1) {
				orient = (w->triangles.spacePosition[UP] >
				w->triangles.spacePosition[DOWN]) ? UP : DOWN;
				MoveTiles(w, TileNextToSpace(w, TLBR, orient, DOWN), !orient);
				return TRUE;
			}
			break;
		default:
			(void) printf("MoveTilesDir: direction %d\n", direction);
	}
	return FALSE;
}

static void
RandomizeTiles(TrianglesWidget w)
{
	trianglesCallbackStruct cb;

	/* First interchange tiles but only with other tiles of the same
	   orientation */
	if (w->triangles.size > 2) {
		int         currentPos, randomPos, randomRow;
		int         currentOrient = UP, randomOrient = DOWN;
		int         step = 1, fin = 1;

		for (currentPos = 0; currentPos < w->triangles.sizeSize; currentPos++) {
			randomPos = currentPos;
			while (currentPos == randomPos || currentOrient != randomOrient) {
				randomPos = NRAND(w->triangles.sizeSize);
				randomRow = Row(randomPos);
				randomOrient = !((randomRow + TrBl(randomPos, randomRow) / 2 +
					TlBr(randomPos, randomRow) / 2) % 2);
			}
			(void) ExchangeTiles(w, currentPos, randomPos);
			if (fin == currentPos + 1) {
				currentOrient = UP;
				step += 2;
				fin += step;
			} else
				currentOrient = !currentOrient;
		}
		DrawAllTiles(w, w->triangles.tileGC, w->triangles.borderGC);
	}
	/* Now move the spaces around randomly */
	if (w->triangles.size > 1) {
		int         big = w->triangles.sizeSize + NRAND(2);
		int         lastDirection = 0;
		int         randomDirection;

		cb.reason = TRIANGLES_RESET;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

		if (w->triangles.size == 2)
			big *= big;

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

			if ((randomDirection + COORD / 2) % COORD != lastDirection) {
				if (MoveTrianglesDir(w, randomDirection))
					lastDirection = randomDirection;
				else
					big++;
			}
		}
		FlushMoves(w);
		cb.reason = TRIANGLES_RANDOMIZE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	if (CheckSolved(w)) {
		cb.reason = TRIANGLES_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MoveTiles(TrianglesWidget w, int from, int orient)
{
	int         tempTile;

	tempTile = w->triangles.tileOfPosition[from];
	w->triangles.tileOfPosition[from] =
		w->triangles.tileOfPosition[w->triangles.spacePosition[orient]];
	w->triangles.tileOfPosition[w->triangles.spacePosition[orient]] =
		tempTile;
	DrawTile(w, w->triangles.tileGC, w->triangles.borderGC,
		 w->triangles.spacePosition[orient], orient, FALSE);
	w->triangles.spacePosition[orient] = from;
	w->triangles.spaceRow[orient][ROW] = Row(from);
	w->triangles.spaceRow[orient][TRBL] =
		TrBl(from, w->triangles.spaceRow[orient][ROW]) / 2;
	w->triangles.spaceRow[orient][TLBR] =
		TlBr(from, w->triangles.spaceRow[orient][ROW]) / 2;
	DrawTile(w, w->triangles.inverseGC, w->triangles.inverseGC,
		 w->triangles.spacePosition[orient], orient, FALSE);
}

static int
ExchangeTiles(TrianglesWidget w, int pos1, int pos2)
{
	int         tempTile;

	if (w->triangles.tileOfPosition[pos1] <= 0)
		return FALSE;
	else if (w->triangles.tileOfPosition[pos2] <= 0)
		return FALSE;
	tempTile = w->triangles.tileOfPosition[pos1];
	w->triangles.tileOfPosition[pos1] = w->triangles.tileOfPosition[pos2];
	w->triangles.tileOfPosition[pos2] = tempTile;
	return TRUE;
}

static int
TileNextToSpace(TrianglesWidget w, int rowType, int orient, int direction)
{
	if (direction == UP) {
		if (rowType == TRBL)
			return ((orient == UP) ? w->triangles.spacePosition[orient] + 1 :
				w->triangles.spacePosition[orient] -
				2 * w->triangles.spaceRow[orient][ROW]);
		else if (rowType == TLBR)
			return ((orient == UP) ? w->triangles.spacePosition[orient] - 1 :
				w->triangles.spacePosition[orient] -
				2 * w->triangles.spaceRow[orient][ROW]);
		else		/* rowType == ROW */
			return (w->triangles.spacePosition[orient] - 1);
	} else {		/* direction == DOWN */
		if (rowType == TRBL)
			return ((orient == DOWN) ? w->triangles.spacePosition[orient] - 1 :
				w->triangles.spacePosition[orient] +
				2 * (w->triangles.spaceRow[orient][ROW] + 1));
		else if (rowType == TLBR)
			return ((orient == DOWN) ? w->triangles.spacePosition[orient] + 1 :
				w->triangles.spacePosition[orient] +
				2 * (w->triangles.spaceRow[orient][ROW] + 1));
		else		/* rowType == ROW */
			return (w->triangles.spacePosition[orient] + 1);
	}
}

static void
DrawFrame(TrianglesWidget w, GC gc)
{
	int         sumX, sumY, sumX2, offsetX, offsetY;

	sumX = w->triangles.size * w->triangles.offset.x + w->triangles.delta.x + 1;
	sumY = w->triangles.size * w->triangles.offset.y + w->triangles.delta.y + 1;
	offsetX = w->triangles.puzzleOffset.x;
	offsetY = w->triangles.puzzleOffset.y;
	sumX2 = sumX / 2 + offsetX;
	sumX += offsetX;
	sumY += offsetY;
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  sumX2 - 1, offsetY, offsetX, sumY);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  sumX2 - 2, offsetY, sumX - 2, sumY);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  offsetX, sumY, sumX - 2, sumY);
}

void
DrawAllTiles(TrianglesWidget w, GC tileGC, GC borderGC)
{
	int         k, side = UP, fin = 1, step = 1;

	for (k = 0; k < w->triangles.sizeSize; k++) {
		if (w->triangles.tileOfPosition[k] <= 0)
			DrawTile(w, w->triangles.inverseGC, w->triangles.inverseGC, k, side,
				 FALSE);
		else
			DrawTile(w, tileGC, borderGC, k, side, FALSE);
		if (fin == k + 1) {
			side = UP;
			step += 2;
			fin += step;
		} else
			side = !side;
	}
}

static void
DrawTile(TrianglesWidget w, GC tileGC, GC borderGC, int pos, int orient, int offset)
{
	int         dx, dy, k = Row(pos);

	dy = (orient == UP) ? 0 : w->triangles.tileSize.y;
	dy += k * w->triangles.offset.y + w->triangles.delta.y + 2 +
		w->triangles.puzzleOffset.y + offset;
	dx = (TrBl(pos, k) - k + w->triangles.size) * w->triangles.offset.x / 2 +
		w->triangles.delta.x / 2 + w->triangles.puzzleOffset.x + offset;
	triangleList[orient][0].x = dx;
	triangleList[orient][0].y = dy;
	XFillPolygon(XtDisplay(w), XtWindow(w), tileGC, triangleList[orient], 3,
		     Convex, CoordModePrevious);
	XDrawLines(XtDisplay(w), XtWindow(w), borderGC, triangleList[orient], 4,
		   CoordModePrevious);
	if (tileGC != w->triangles.inverseGC || borderGC != w->triangles.inverseGC) {
		int         i = 0, offsetX = 0, offsetY = 0;
		int         tile = w->triangles.tileOfPosition[pos];
		char        buf[5];

		(void) sprintf(buf, "%d", tile);
		while (tile >= 1) {
			tile /= w->triangles.base;
			offsetX += w->triangles.digitOffset.x;
			i++;
		}
		offsetY = (orient == UP) ? w->triangles.digitOffset.y +
			2 * w->triangles.delta.y + 2 :
			-w->triangles.digitOffset.y - w->triangles.tileSize.y +
			w->triangles.delta.y - 2;
		XDrawString(XtDisplay(w), XtWindow(w), w->triangles.inverseGC,
			    dx - offsetX, dy + w->triangles.tileSize.y / 2 + offsetY, buf, i);
	}
}

int
Row(int pos)
{
	return Sqrt(pos);
}

/* Passing row so there is no sqrt calculation again */
int
TrBl(int pos, int posRow)
{
	return (pos - posRow * posRow);
}

int
TlBr(int pos, int posRow)
{
	return (posRow * posRow + 2 * posRow - pos);
}

static int
ToOrient(int row, int trbl, int tlbr)
{
	return (trbl + tlbr == row);
}

static int
ToPosition(int row, int trbl, int tlbr)
{
	return (row * row + row + trbl - tlbr);
}

/* This is fast for small i, a -1 is returned for negative i. */
static int
Sqrt(int i)
{
	int         j = 0;

	while (j * j <= i)
		j++;
	return (j - 1);
}

Boolean
CheckSolved(TrianglesWidget w)
{
	int         i;

	for (i = 1; i < w->triangles.sizeSize - 1; i++)
		if (w->triangles.tileOfPosition[i - 1] != i)
			return FALSE;
	return TRUE;
}
