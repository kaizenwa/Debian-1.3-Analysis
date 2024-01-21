/*-
# X-BASED HEXAGONS
#
#  Hexagons.c
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

/* Methods file for Hexagons */

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
#include "HexagonsP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/hexagons.data"
#endif

static void InitializeHexagons(Widget request, Widget new);
static void ExposeHexagons(Widget new, XEvent * event, Region region);
static void ResizeHexagons(HexagonsWidget w);
static void DestroyHexagons(Widget old);
static Boolean SetValuesHexagons(Widget current, Widget request, Widget new);
static void QuitHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void MoveHexagonsTl(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void MoveHexagonsTr(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void MoveHexagonsLeft(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void MoveHexagonsRight(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void MoveHexagonsBl(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void MoveHexagonsBr(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void SelectHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void ReleaseHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeHexagonsMaybe(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void GetHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void WriteHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void UndoHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void SolveHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void IncrementHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void DecrementHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static void CornerHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs);
static int  MoveHexagons(HexagonsWidget w, int direction);
static int  PositionToTile(HexagonsWidget w, int x, int y, int *row);
static void SelectCornerTiles(HexagonsWidget w, int space);
static void SelectNoCornTiles(HexagonsWidget w);
static void CheckTiles(HexagonsWidget w);
static void ResetTiles(HexagonsWidget w);
static void ResizeTiles(HexagonsWidget w);
static int  MovableCornerTiles(HexagonsWidget w, int direction, int *position, int *posRow, int *space);
static void MoveNoTiles(HexagonsWidget w);
static int  MoveTilesDir(HexagonsWidget w, int direction);
static int  MoveCornerTilesDir(HexagonsWidget w, int direction);
static int  MoveNoCornTilesDir(HexagonsWidget w, int direction);
static void RandomizeTiles(HexagonsWidget w);
static void MoveCornerTiles(HexagonsWidget w, int from, int posRow, int space);
static void MoveNoCornTiles(HexagonsWidget w, int from, int posRow);
static int  ExchangeTiles(HexagonsWidget w, int pos1, int pos2);

#ifdef DEBUG
static int  WithinFrame(HexagonsWidget w, int x, int y, int dx, int dy);

#endif
static int  NextToWall(HexagonsWidget w, int pos, int posRow, int spaceType);
static int  TileNextToSpace(HexagonsWidget w, int rowType, int direction);
static int  FindTileTriangle(HexagonsWidget w, int pI, int pJ, int pK, int rI, int rJ, int rK);
static int  FindDir(HexagonsWidget w, int posTile, int posSpace, int rowTile, int rowSpace);
static int  FindSpaceType(HexagonsWidget w, int pos1, int pos2, int row1, int row2);
static void FindMovableTile(HexagonsWidget w, int pos, int posRow, int spaceType, int side, int *tilePos, int *tileRow);
static void DrawFrame(HexagonsWidget w, GC gc);
static void DrawTile(HexagonsWidget w, GC tileGC, GC borderGC, int pos, int offset);

#ifdef DEBUG
static int  PositionInRow();

#endif
static int  PositionFromRow(HexagonsWidget w, int rowPosition, int posRow);
static int  Sqrt(int i);
static void swap(int *a, int *b);

static char defaultTranslationsHexagons[] =
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
   <KeyPress>d: Decrement()\n\
   <KeyPress>c: Corner()";

static XtActionsRec actionsListHexagons[] =
{
	{"Quit", (XtActionProc) QuitHexagons},
	{"MoveTl", (XtActionProc) MoveHexagonsTl},
	{"MoveTr", (XtActionProc) MoveHexagonsTr},
	{"MoveLeft", (XtActionProc) MoveHexagonsLeft},
	{"MoveRight", (XtActionProc) MoveHexagonsRight},
	{"MoveBl", (XtActionProc) MoveHexagonsBl},
	{"MoveBr", (XtActionProc) MoveHexagonsBr},
	{"Select", (XtActionProc) SelectHexagons},
	{"Release", (XtActionProc) ReleaseHexagons},
	{"Randomize", (XtActionProc) RandomizeHexagons},
	{"RandomizeMaybe", (XtActionProc) RandomizeHexagonsMaybe},
	{"Get", (XtActionProc) GetHexagons},
	{"Write", (XtActionProc) WriteHexagons},
	{"Undo", (XtActionProc) UndoHexagons},
	{"Solve", (XtActionProc) SolveHexagons},
	{"Increment", (XtActionProc) IncrementHexagons},
	{"Decrement", (XtActionProc) DecrementHexagons},
	{"Corner", (XtActionProc) CornerHexagons}
};

static XtResource resourcesHexagons[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(HexagonsWidget, hexagons.username), XtRString, "nobody"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
	 XtOffset(HexagonsWidget, hexagons.foreground), XtRString,
	 XtDefaultForeground},
	{XtNtileColor, XtCColor, XtRPixel, sizeof (Pixel),
	 XtOffset(HexagonsWidget, hexagons.tileColor), XtRString,
	 XtDefaultForeground},
	{XtNtileBorder, XtCColor, XtRPixel, sizeof (Pixel),
	 XtOffset(HexagonsWidget, hexagons.borderColor), XtRString,
	 XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(HexagonsWidget, core.width), XtRString, "259"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(HexagonsWidget, core.height), XtRString, "200"},
	{XtNsize, XtCSize, XtRInt, sizeof (int),
	 XtOffset(HexagonsWidget, hexagons.size), XtRString, "3"},	/* DEFAULTHEXS */
	{XtNcorners, XtCCorners, XtRBoolean, sizeof (Boolean),
	 XtOffset(HexagonsWidget, hexagons.corners), XtRString, "TRUE"},	/*DEFAULTCORN */
	{XtNbase, XtCBase, XtRInt, sizeof (int),
	 XtOffset(HexagonsWidget, hexagons.base), XtRString, "10"},
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(HexagonsWidget, hexagons.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(HexagonsWidget, hexagons.select), XtRCallback, NULL}
};

HexagonsClassRec hexagonsClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Hexagons",	/* class name */
		sizeof (HexagonsRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeHexagons,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListHexagons,	/* actions */
		XtNumber(actionsListHexagons),	/* num actions */
		resourcesHexagons,	/* resources */
		XtNumber(resourcesHexagons),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		(XtWidgetProc) DestroyHexagons,		/* destroy */
		(XtWidgetProc) ResizeHexagons,	/* resize */
		(XtExposeProc) ExposeHexagons,	/* expose */
		(XtSetValuesFunc) SetValuesHexagons,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		NULL,		/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsHexagons,	/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	}
};

WidgetClass hexagonsWidgetClass = (WidgetClass) & hexagonsClassRec;

static XPoint hexagonUnit[MAXORIENT][7] =
{
	{
		{0, 0},
		{2, 0},
		{1, 1},
		{-1, 1},
		{-2, 0},
		{-1, -1},
		{1, -1}},
	{
		{0, 0},
		{1, 1},
		{0, 2},
		{-1, 1},
		{-1, -1},
		{0, -2},
		{1, -1}}
};
static XPoint hexagonList[MAXORIENT][7];

static void
InitializeHexagons(Widget request, Widget new)
{
	HexagonsWidget w = (HexagonsWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;

	w->hexagons.tileOfPosition = NULL;
	CheckTiles(w);
	InitMoves();
	ResetTiles(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.foreground = w->hexagons.foreground;
	values.background = w->core.background_pixel;
	w->hexagons.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->hexagons.tileColor;
	w->hexagons.tileGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->hexagons.borderColor;
	w->hexagons.borderGC = XtGetGC(new, valueMask, &values);
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->hexagons.foreground;
	w->hexagons.inverseGC = XtGetGC(new, valueMask, &values);
	ResizeHexagons(w);
}

static void
DestroyHexagons(Widget old)
{
	HexagonsWidget w = (HexagonsWidget) old;

	XtReleaseGC(old, w->hexagons.tileGC);
	XtReleaseGC(old, w->hexagons.borderGC);
	XtReleaseGC(old, w->hexagons.puzzleGC);
	XtReleaseGC(old, w->hexagons.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->hexagons.select);
}

static void
ResizeHexagons(HexagonsWidget w)
{
	double      sqrt_3 = 1.73205080756887729352744634150587237;
	XPoint      tempSize;

	w->hexagons.delta.x = 2;
	w->hexagons.delta.y = 2;
	w->hexagons.tileSize.x = MAX((2 * ((int) w->core.width +
			2 * w->hexagons.delta.x - 1) - 4 * w->hexagons.size *
		       w->hexagons.delta.x) / (4 * w->hexagons.size - 1), 0);
	w->hexagons.tileSize.y = MAX(((int) w->core.height - 1 -
				2 * w->hexagons.size * w->hexagons.delta.y) /
				     (3 * w->hexagons.size - 1), 0);
	w->hexagons.offset.x = w->hexagons.tileSize.x + w->hexagons.delta.x;
	w->hexagons.offset.y = w->hexagons.tileSize.y + 2 * w->hexagons.delta.y;
	tempSize.y = (int) ((double) w->hexagons.offset.x / sqrt_3);
	tempSize.x = (int) ((double) w->hexagons.offset.y * sqrt_3);
	if (tempSize.y < w->hexagons.offset.y) {
		w->hexagons.offset.x = w->hexagons.tileSize.x + w->hexagons.delta.x;
		w->hexagons.offset.y = tempSize.y;
	} else {		/* tempSize.x <=  w->hexagons.offset.x */
		w->hexagons.offset.x = tempSize.x;
		w->hexagons.offset.y = w->hexagons.tileSize.y + 2 * w->hexagons.delta.y;
	}
	w->hexagons.tileSize.x = MAX(w->hexagons.offset.x - w->hexagons.delta.x, 0);
	w->hexagons.tileSize.y = MAX(w->hexagons.offset.y - 2 * w->hexagons.delta.y,
				     0);
	w->hexagons.puzzleSize.x = w->hexagons.size * 2 * w->hexagons.offset.x -
		w->hexagons.tileSize.x / 2 - 2 * w->hexagons.delta.x + 1;
	w->hexagons.puzzleSize.y = w->hexagons.size * (3 * w->hexagons.tileSize.y +
		       2 * w->hexagons.delta.y) - w->hexagons.tileSize.y + 2;
	w->hexagons.puzzleOffset.x =
		((int) w->core.width - w->hexagons.puzzleSize.x + 2) / 2;
	w->hexagons.puzzleOffset.y =
		((int) w->core.height - w->hexagons.puzzleSize.y + 2) / 2;
	ResizeTiles(w);
}

static void
ExposeHexagons(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	HexagonsWidget w = (HexagonsWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->hexagons.puzzleGC);
		DrawAllTiles(w, w->hexagons.tileGC, w->hexagons.borderGC);
	}
}

static      Boolean
SetValuesHexagons(Widget current, Widget request, Widget new)
{
	HexagonsWidget c = (HexagonsWidget) current, w = (HexagonsWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	Boolean     redrawTiles = FALSE;

	CheckTiles(w);
	if (w->hexagons.foreground != c->hexagons.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->hexagons.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->hexagons.puzzleGC);
		w->hexagons.puzzleGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->hexagons.foreground;
		XtReleaseGC(new, w->hexagons.inverseGC);
		w->hexagons.inverseGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->hexagons.tileColor != c->hexagons.tileColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->hexagons.tileColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->hexagons.tileGC);
		w->hexagons.tileGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->hexagons.borderColor != c->hexagons.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->hexagons.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->hexagons.borderGC);
		w->hexagons.borderGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->hexagons.size != c->hexagons.size ||
	    w->hexagons.corners != c->hexagons.corners ||
	    w->hexagons.base != c->hexagons.base) {
		ResetTiles(w);
		ResizeHexagons(w);
		redraw = TRUE;
	} else if (w->hexagons.offset.x != c->hexagons.offset.x ||
		   w->hexagons.offset.y != c->hexagons.offset.y) {
		ResizeHexagons(w);
		redraw = TRUE;
	}
	if (redrawTiles && !redraw && XtIsRealized(new) && new->core.visible) {
		DrawFrame(c, c->hexagons.inverseGC);
		DrawAllTiles(c, c->hexagons.inverseGC, c->hexagons.inverseGC);
		DrawFrame(w, w->hexagons.puzzleGC);
		DrawAllTiles(w, w->hexagons.tileGC, w->hexagons.borderGC);
	}
	return (redraw);
}

static void
QuitHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	int         pos, row;

	pos = PositionToTile(w, event->xbutton.x, event->xbutton.y, &row);
	if (pos >= 0) {
		if (CheckSolved(w)) {
			MoveNoTiles(w);
			w->hexagons.currentPosition = -1;
			return;
		}
		w->hexagons.currentPosition = PositionFromRow(w, pos, row);
		w->hexagons.currentRow[ROW] = row;
		w->hexagons.currentRow[TRBL] = TrBl(w, w->hexagons.currentPosition, row);
		w->hexagons.currentRow[TLBR] = TlBr(w, w->hexagons.currentPosition, row);
		if (w->hexagons.spacePosition[HIGH] != w->hexagons.currentPosition &&
		    (!w->hexagons.corners ||
		     w->hexagons.spacePosition[LOW] != w->hexagons.currentPosition))
			DrawTile(w, w->hexagons.borderGC, w->hexagons.tileGC,
				 w->hexagons.currentPosition, TRUE);
		else
			w->hexagons.currentPosition = -1;
	} else
		w->hexagons.currentPosition = -1;
}

static void
ReleaseHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	int         pos, row, space;

	if (w->hexagons.currentPosition == -1)
		return;
	DrawTile(w, w->hexagons.inverseGC, w->hexagons.inverseGC,
		 w->hexagons.currentPosition, TRUE);
	DrawTile(w, w->hexagons.tileGC, w->hexagons.borderGC,
		 w->hexagons.currentPosition, FALSE);
	if (!w->hexagons.corners) {
		SelectNoCornTiles(w);
		w->hexagons.currentPosition = -1;
		return;
	}
	pos = PositionToTile(w, event->xbutton.x, event->xbutton.y, &row);
	if (pos >= 0) {
		pos = PositionFromRow(w, pos, row);
		if (w->hexagons.spacePosition[HIGH] == pos)
			space = (w->hexagons.spacePosition[HIGH] >
				 w->hexagons.spacePosition[LOW]);
		else if (w->hexagons.spacePosition[LOW] == pos)
			space = (w->hexagons.spacePosition[HIGH] <
				 w->hexagons.spacePosition[LOW]);
		else {
			w->hexagons.currentPosition = -1;
			return;
		}
		SelectCornerTiles(w, space);
	}
	w->hexagons.currentPosition = -1;
}

static void
RandomizeHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizeTiles(w);
}

static void
RandomizeHexagonsMaybe(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->hexagons.started)
		RandomizeTiles(w);
}

static void
GetHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, size, corners, moves;
	hexagonsCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &size);
		if (size >= MINHEXAGONS) {
			for (i = w->hexagons.size; i < size; i++) {
				cb.reason = HEXAGONS_INC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->hexagons.size; i > size; i--) {
				cb.reason = HEXAGONS_DEC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: size %d should be between %d and MAXINT\n",
				      DATAFILE, size, MINHEXAGONS);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &corners);
		if (w->hexagons.corners != (Boolean) corners) {
			cb.reason = HEXAGONS_CORNERS;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = HEXAGONS_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		SetStartPosition(w);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: size %d, corners %d, moves %d.\n",
			      DATAFILE, size, corners, moves);
	}
}

static void
WriteHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "size%c %d\n", SYMBOL, w->hexagons.size);
		(void) fprintf(fp, "corners%c %d\n", SYMBOL, (w->hexagons.corners) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         direction;

		GetMove(&direction);
		direction = (direction + (COORD / 2)) % COORD;
		if (MoveTilesDir(w, direction)) {
			hexagonsCallbackStruct cb;

			cb.reason = HEXAGONS_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolveHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolveTiles(w); *//* Sorry, unimplemented */
}

static void
IncrementHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	hexagonsCallbackStruct cb;

	cb.reason = HEXAGONS_INC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
DecrementHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	hexagonsCallbackStruct cb;

	if (w->hexagons.size <= MINHEXAGONS)
		return;
	cb.reason = HEXAGONS_DEC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
CornerHexagons(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	hexagonsCallbackStruct cb;

	cb.reason = HEXAGONS_CORNERS;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MoveHexagonsTl(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveHexagons(w, TL);
}

static void
MoveHexagonsTr(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveHexagons(w, TR);
}

static void
MoveHexagonsLeft(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveHexagons(w, LEFT);
}

static void
MoveHexagonsRight(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveHexagons(w, RIGHT);
}

static void
MoveHexagonsBl(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveHexagons(w, BL);
}

static void
MoveHexagonsBr(HexagonsWidget w, XEvent * event, char **args, int nArgs)
{
	(void) MoveHexagons(w, BR);
}

static int
MoveHexagons(HexagonsWidget w, int direction)
{
	hexagonsCallbackStruct cb;

	if (CheckSolved(w)) {
		MoveNoTiles(w);
		return FALSE;
	}
	if (!MoveHexagonsDir(w, direction)) {
		cb.reason = HEXAGONS_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return FALSE;
	}
	if (CheckSolved(w)) {
		cb.reason = HEXAGONS_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	return TRUE;
}

int
MoveHexagonsDir(HexagonsWidget w, int direction)
{
	hexagonsCallbackStruct cb;

	if (MoveTilesDir(w, direction)) {
		cb.reason = HEXAGONS_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		PutMove(direction);
		return TRUE;
	}
	return FALSE;
}

static int
PositionToTile(HexagonsWidget w, int x, int y, int *row)
{
	int         i, j, k, modI, modJ;

	x -= w->hexagons.puzzleOffset.x;
	y -= w->hexagons.puzzleOffset.y;
	/* First convert x and y coordinates to hexagon grid.  Keep in mind that
	   the starting hexagon x position changes with "w->hexagons.size % 2". */
	if (x < w->hexagons.tileSize.x / 4)
		return -1;
	i = 2 * (x - w->hexagons.tileSize.x / 4) / w->hexagons.offset.x;
	j = 3 * (y - w->hexagons.delta.y) /
		(3 * w->hexagons.tileSize.y / 2 + w->hexagons.delta.y);
	modI = 2 * (x - w->hexagons.tileSize.x / 4) % w->hexagons.offset.x;
	modJ = 3 * (y - w->hexagons.delta.y) %
		(3 * w->hexagons.tileSize.y / 2 + w->hexagons.delta.y);
	*row = j / 3;		/* Approximate to a rectangle just for now */
	if (j % 3 == 0) {	/* Then it is the triangle near bottom or top point */
		if ((w->hexagons.size - 1 + *row + i) % 2)	/* \ */
			*row -= (modJ * w->hexagons.offset.x < modI *
				 (3 * w->hexagons.tileSize.y / 2 + w->hexagons.delta.y));
		else		/* / */
			*row -= (modJ * w->hexagons.offset.x < (w->hexagons.offset.x - modI) *
				 (3 * w->hexagons.tileSize.y / 2 + w->hexagons.delta.y));
	}
	if (i < (w->hexagons.size - 1 + *row) % 2 || *row < 0 ||
	    *row > 2 * (w->hexagons.size - 1))
		return -1;
	k = (i - ((w->hexagons.size - 1 + *row) % 2)) / 2;
	/* Map the hexagon grid to hexagon position in puzzle. */
	i = (*row < w->hexagons.size) ?
		k - (w->hexagons.size - 1 - *row) / 2 :
		k + (w->hexagons.size - 1 - *row) / 2;
	j = (*row < w->hexagons.size) ?
		w->hexagons.size - 1 + *row : 3 * (w->hexagons.size - 1) - *row;
	if (i < 0 || i > j)
		return -1;
	return i;
}

static void
SelectCornerTiles(HexagonsWidget w, int space)
{
	int         orient;
	hexagonsCallbackStruct cb;

	/* Are the spaces in a "row" with the mouse click?
	   (If two, then one clicked on a space). */
	if (w->hexagons.currentPosition == w->hexagons.spacePosition[LOW] ||
	    w->hexagons.currentPosition == w->hexagons.spacePosition[HIGH]) {
		cb.reason = HEXAGONS_SPACE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return;
	}
	if (FindTileTriangle(w,
		w->hexagons.currentPosition, w->hexagons.spacePosition[HIGH],
		 w->hexagons.spacePosition[LOW], w->hexagons.currentRow[ROW],
		    w->hexagons.spaceRow[HIGH], w->hexagons.spaceRow[LOW])) {
		orient = (w->hexagons.spacePosition[HIGH] <
			  w->hexagons.spacePosition[LOW]) ? !space : space;
		PutMove(FindDir(w,
				w->hexagons.currentPosition, w->hexagons.spacePosition[orient],
		 w->hexagons.currentRow[ROW], w->hexagons.spaceRow[orient]));
		MoveCornerTiles(w, w->hexagons.currentPosition,
				w->hexagons.currentRow[ROW], orient);
		cb.reason = HEXAGONS_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	} else {
		cb.reason = HEXAGONS_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return;
	}
	if (CheckSolved(w)) {
		cb.reason = HEXAGONS_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
SelectNoCornTiles(HexagonsWidget w)
{
	hexagonsCallbackStruct cb;
	int         rowType = -1, l, orient;

	/* Are the spaces in a "row" with the mouse click?
	   (If two, then one clicked on a space). */
	for (l = 0; l < ROWTYPES; l++)
		if (w->hexagons.currentRow[l] == w->hexagons.spaceRow[l]) {
			if (rowType == -1)
				rowType = l;
			else {
				cb.reason = HEXAGONS_SPACE;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				return;
			}
		}
	if (rowType != -1) {
		if (w->hexagons.currentPosition < w->hexagons.spacePosition[HIGH]) {
			while (w->hexagons.currentPosition < w->hexagons.spacePosition[HIGH]) {
				orient = (rowType == ROW) ?
					w->hexagons.spaceRow[ROW] : w->hexagons.spaceRow[ROW] - 1;
				MoveNoCornTiles(w, TileNextToSpace(w, rowType, HIGH), orient);
				cb.reason = HEXAGONS_MOVED;
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
						(void) printf("SelectNoCornTiles: rowType %d\n", rowType);
				}
			}
		} else {	/*w->hexagons.currentPosition > w->hexagons.spacePosition[HIGH] */
			while (w->hexagons.currentPosition > w->hexagons.spacePosition[HIGH]) {
				orient = (rowType == ROW) ?
					w->hexagons.spaceRow[ROW] : w->hexagons.spaceRow[ROW] + 1;
				MoveNoCornTiles(w, TileNextToSpace(w, rowType, LOW), orient);
				cb.reason = HEXAGONS_MOVED;
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
						(void) printf("SelectNoCornTiles: rowType %d\n", rowType);
				}
			}
		}
	} else {
		cb.reason = HEXAGONS_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return;
	}
	if (CheckSolved(w)) {
		cb.reason = HEXAGONS_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
CheckTiles(HexagonsWidget w)
{
	char        buf[121];

	if (w->hexagons.size < MINHEXAGONS) {
		(void) sprintf(buf,
		"Number of Hexagons on a edge out of bounds, use %d..MAXINT",
			       MINHEXAGONS);
		XtWarning(buf);
		w->hexagons.size = DEFAULTHEXAGONS;
	}
	w->hexagons.base = 10;
}

static void
ResetTiles(HexagonsWidget w)
{
	int         i;

	w->hexagons.sizeSize = 3 * w->hexagons.size * (w->hexagons.size - 1) + 1;
	w->hexagons.sizeCenter = (w->hexagons.sizeSize - 1) / 2;
	if (w->hexagons.tileOfPosition)
		(void) free((void *) w->hexagons.tileOfPosition);
	if (!(w->hexagons.tileOfPosition = (int *)
	      malloc(sizeof (int) * w->hexagons.sizeSize)))
		            XtError("Not enough memory, exiting.");

	if (startPosition)
		(void) free((void *) startPosition);
	if (!(startPosition = (int *)
	      malloc(sizeof (int) * w->hexagons.sizeSize)))
		            XtError("Not enough memory, exiting.");

	w->hexagons.spacePosition[HIGH] = w->hexagons.sizeSize - 1;
	if (w->hexagons.corners) {
		w->hexagons.spaceRow[HIGH] = 2 * w->hexagons.size - 2;
		if (w->hexagons.size > 1) {
			w->hexagons.spacePosition[LOW] = w->hexagons.sizeSize - 2;
			w->hexagons.spaceRow[LOW] = 2 * w->hexagons.size - 2;
			w->hexagons.tileOfPosition[w->hexagons.sizeSize - 2] = -1;
		}
	} else {
		w->hexagons.spaceRow[ROW] = w->hexagons.spaceRow[TRBL] =
			2 * w->hexagons.size - 2;
		w->hexagons.spaceRow[TLBR] = w->hexagons.size - 1;
	}
	w->hexagons.tileOfPosition[w->hexagons.sizeSize - 1] = 0;
	for (i = 1; i < w->hexagons.sizeSize - ((w->hexagons.corners) ? 1 : 0);
	     i++)
		w->hexagons.tileOfPosition[i - 1] = i;
	FlushMoves(w);
	w->hexagons.currentPosition = -1;
	w->hexagons.started = FALSE;
}

static void
ResizeTiles(HexagonsWidget w)
{
	int         i;

	for (i = 0; i <= 6; i++) {
		hexagonList[NOCORN][i].x = w->hexagons.tileSize.x *
			hexagonUnit[NOCORN][i].x / 4;
		hexagonList[NOCORN][i].y = 3 * w->hexagons.tileSize.y *
			hexagonUnit[NOCORN][i].y / 4;
		hexagonList[CORNERS][i].x = w->hexagons.tileSize.x *
			hexagonUnit[CORNERS][i].x / 2;
		hexagonList[CORNERS][i].y = w->hexagons.tileSize.y *
			hexagonUnit[CORNERS][i].y / 2;
	}
	w->hexagons.digitOffset.x = 3;
	w->hexagons.digitOffset.y = 4;
}

static int
MovableCornerTiles(HexagonsWidget w, int direction, int *position, int *posRow, int *space)
{
	int         spaceType, highest, side = -1;

	highest = (w->hexagons.spacePosition[HIGH] >
		   w->hexagons.spacePosition[LOW]) ? HIGH : LOW;
	spaceType = FindSpaceType(w,
	     w->hexagons.spacePosition[HIGH], w->hexagons.spacePosition[LOW],
		      w->hexagons.spaceRow[HIGH], w->hexagons.spaceRow[LOW]);
	switch (spaceType) {
		case TRBL:
			if (direction == TR || direction == BL)
				return FALSE;
			side = NextToWall(w, w->hexagons.spacePosition[highest],
				   w->hexagons.spaceRow[highest], spaceType);
			if (side != -1) {
				if ((side == HIGH && direction == RIGHT) ||
				    (side == HIGH && direction == BR) ||
				    (side == LOW && direction == LEFT) ||
				    (side == LOW && direction == TL))
					return FALSE;
			} else
				side = (direction == TL || direction == LEFT);
			*space = (direction == BR || direction == LEFT);
			break;
		case TLBR:
			if (direction == TL || direction == BR)
				return FALSE;
			side = NextToWall(w, w->hexagons.spacePosition[highest],
				   w->hexagons.spaceRow[highest], spaceType);
			if (side != -1) {
				if ((side == LOW && direction == TR) ||
				    (side == LOW && direction == RIGHT) ||
				    (side == HIGH && direction == BL) ||
				    (side == HIGH && direction == LEFT))
					return FALSE;
			} else
				side = (direction == TR || direction == RIGHT);
			*space = (direction == RIGHT || direction == BL);
			break;
		case ROW:
			if (direction == LEFT || direction == RIGHT)
				return FALSE;
			side = NextToWall(w, w->hexagons.spacePosition[highest],
				   w->hexagons.spaceRow[highest], spaceType);
			if (side != -1) {
				if ((side == LOW && direction == TR) ||
				    (side == HIGH && direction == BR) ||
				    (side == HIGH && direction == BL) ||
				    (side == LOW && direction == TL))
					return FALSE;
			} else
				side = (direction == TR || direction == TL);
			*space = (direction == TR || direction == BR);
			break;
	}
	FindMovableTile(w, w->hexagons.spacePosition[highest],
	   w->hexagons.spaceRow[highest], spaceType, side, position, posRow);
	return TRUE;
}

static void
MoveNoTiles(HexagonsWidget w)
{
	hexagonsCallbackStruct cb;

	cb.reason = HEXAGONS_IGNORE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static int
MoveTilesDir(HexagonsWidget w, int direction)
{
	if (w->hexagons.corners)
		return MoveCornerTilesDir(w, direction);
	else
		return MoveNoCornTilesDir(w, direction);
}


static int
MoveCornerTilesDir(HexagonsWidget w, int direction)
{
	int         position, posRow, space;

	if (MovableCornerTiles(w, direction, &position, &posRow, &space)) {
		MoveCornerTiles(w, position, posRow, (w->hexagons.spacePosition[HIGH] <
			   w->hexagons.spacePosition[LOW]) ? !space : space);
		return TRUE;
	}
	return FALSE;
}

static int
MoveNoCornTilesDir(HexagonsWidget w, int direction)
{
	switch (direction) {
		case TR:
			if (w->hexagons.spaceRow[ROW] != 2 * w->hexagons.size - 2 &&
			    w->hexagons.spaceRow[TLBR] != 2 * w->hexagons.size - 2) {
				MoveNoCornTiles(w, TileNextToSpace(w, TRBL, LOW),
					      w->hexagons.spaceRow[ROW] + 1);
				return TRUE;
			}
			break;
		case RIGHT:
			if (w->hexagons.spaceRow[TRBL] != 0 &&
			    w->hexagons.spaceRow[TLBR] != 2 * w->hexagons.size - 2) {
				MoveNoCornTiles(w, TileNextToSpace(w, ROW, HIGH),
						w->hexagons.spaceRow[ROW]);
				return TRUE;
			}
			break;
		case BR:
			if (w->hexagons.spaceRow[ROW] != 0 &&
			    w->hexagons.spaceRow[TRBL] != 0) {
				MoveNoCornTiles(w, TileNextToSpace(w, TLBR, HIGH),
					      w->hexagons.spaceRow[ROW] - 1);
				return TRUE;
			}
			break;
		case BL:
			if (w->hexagons.spaceRow[ROW] != 0 &&
			    w->hexagons.spaceRow[TLBR] != 0) {
				MoveNoCornTiles(w, TileNextToSpace(w, TRBL, HIGH),
					      w->hexagons.spaceRow[ROW] - 1);
				return TRUE;
			}
			break;
		case LEFT:
			if (w->hexagons.spaceRow[TLBR] != 0 &&
			    w->hexagons.spaceRow[TRBL] != 2 * w->hexagons.size - 2) {
				MoveNoCornTiles(w, TileNextToSpace(w, ROW, LOW),
						w->hexagons.spaceRow[ROW]);
				return TRUE;
			}
			break;
		case TL:
			if (w->hexagons.spaceRow[ROW] != 2 * w->hexagons.size - 2 &&
			    w->hexagons.spaceRow[TRBL] != 2 * w->hexagons.size - 2) {
				MoveNoCornTiles(w, TileNextToSpace(w, TLBR, LOW),
					      w->hexagons.spaceRow[ROW] + 1);
				return TRUE;
			}
			break;
		default:
			(void) printf("MoveNoCornTilesDir: direction %d\n", direction);
	}
	return FALSE;
}

static void
RandomizeTiles(HexagonsWidget w)
{
	hexagonsCallbackStruct cb;

	/* First interchange tiles an even number of times */
	if (w->hexagons.size > 1 + ((w->hexagons.corners) ? 1 : 0)) {
		int         currentPos, randomPos;
		int         count = 0;

		for (currentPos = 0; currentPos < w->hexagons.sizeSize; currentPos++) {
			randomPos = currentPos;
			while (currentPos == randomPos)
				randomPos = NRAND(w->hexagons.sizeSize);
			count += ExchangeTiles(w, currentPos, randomPos);
		}
		if (count % 2 && w->hexagons.corners)
			if (!ExchangeTiles(w, 0, 1))
				if (!ExchangeTiles(w,
						   w->hexagons.sizeSize - 2, w->hexagons.sizeSize - 1))
					(void) printf("RandomizeTiles: should not get here\n");
		DrawAllTiles(w, w->hexagons.tileGC, w->hexagons.borderGC);
	}
	/* Now move the spaces around randomly */
	if (w->hexagons.size > 1) {
		int         big = w->hexagons.sizeSize + NRAND(2);
		int         lastDirection = 0;
		int         randomDirection;

		cb.reason = HEXAGONS_RESET;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

		if (w->hexagons.corners && w->hexagons.size == 2)
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
				if (MoveHexagonsDir(w, randomDirection))
					lastDirection = randomDirection;
				else
					big++;
			}
		}
		FlushMoves(w);
		cb.reason = HEXAGONS_RANDOMIZE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	if (CheckSolved(w)) {
		cb.reason = HEXAGONS_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MoveCornerTiles(HexagonsWidget w, int from, int posRow, int space)
{
	int         tempTile;

	tempTile = w->hexagons.tileOfPosition[from];
	w->hexagons.tileOfPosition[from] =
		w->hexagons.tileOfPosition[w->hexagons.spacePosition[space]];
	w->hexagons.tileOfPosition[w->hexagons.spacePosition[space]] =
		tempTile;
	DrawTile(w, w->hexagons.tileGC, w->hexagons.borderGC,
		 w->hexagons.spacePosition[space], FALSE);
	w->hexagons.spacePosition[space] = from;
	w->hexagons.spaceRow[space] = posRow;
	DrawTile(w, w->hexagons.inverseGC, w->hexagons.inverseGC,
		 w->hexagons.spacePosition[space], FALSE);
}

static void
MoveNoCornTiles(HexagonsWidget w, int from, int posRow)
{
	int         tempTile;

	tempTile = w->hexagons.tileOfPosition[from];
	w->hexagons.tileOfPosition[from] =
		w->hexagons.tileOfPosition[w->hexagons.spacePosition[HIGH]];
	w->hexagons.tileOfPosition[w->hexagons.spacePosition[HIGH]] = tempTile;
	DrawTile(w, w->hexagons.tileGC, w->hexagons.borderGC,
		 w->hexagons.spacePosition[HIGH], FALSE);
	w->hexagons.spacePosition[HIGH] = from;
	w->hexagons.spaceRow[ROW] = posRow;
	w->hexagons.spaceRow[TRBL] = TrBl(w, from, posRow);
	w->hexagons.spaceRow[TLBR] = TlBr(w, from, posRow);
	DrawTile(w, w->hexagons.inverseGC, w->hexagons.inverseGC,
		 w->hexagons.spacePosition[HIGH], FALSE);
}

static int
ExchangeTiles(HexagonsWidget w, int pos1, int pos2)
{
	int         tempTile;

	if (w->hexagons.tileOfPosition[pos1] <= 0 ||
	    w->hexagons.tileOfPosition[pos2] <= 0)
		return FALSE;
	tempTile = w->hexagons.tileOfPosition[pos1];
	w->hexagons.tileOfPosition[pos1] = w->hexagons.tileOfPosition[pos2];
	w->hexagons.tileOfPosition[pos2] = tempTile;
	return TRUE;
}

#ifdef DEBUG
static int
WithinFrame(HexagonsWidget w, int x, int y, int dx, int dy)
{
	return
		(x < dx + w->hexagons.tileSize.x / 2 &&
		 x > dx - w->hexagons.tileSize.x / 2 &&
		 w->hexagons.tileSize.y * (x - dx) < w->hexagons.tileSize.x * (y - dy) &&
		 w->hexagons.tileSize.y * (dx - x) < w->hexagons.tileSize.x * (y - dy) &&
	     w->hexagons.tileSize.y * (x - dx + 2 * w->hexagons.tileSize.x) >
		 w->hexagons.tileSize.x * (y - dy) &&
	     w->hexagons.tileSize.y * (dx - x + 2 * w->hexagons.tileSize.x) >
		 w->hexagons.tileSize.x * (y - dy));
}
#endif

static int
NextToWall(HexagonsWidget w, int pos, int posRow, int spaceType)
{
	switch (spaceType) {
		case TRBL:
			if (posRow < w->hexagons.size && pos ==
			w->hexagons.size * posRow + posRow * (posRow - 1) / 2)
				return (HIGH);
			else if (posRow >= w->hexagons.size && pos == w->hexagons.size *
				 (posRow - w->hexagons.size) + 3 * w->hexagons.size - posRow -
				 2 + 4 * w->hexagons.size * (w->hexagons.size - 1) / 2 -
				 (2 * w->hexagons.size - posRow - 2) *
				 (2 * w->hexagons.size - posRow - 1) / 2)
				return (LOW);
			else
				return (-1);
		case TLBR:
			if (posRow < w->hexagons.size && pos ==
			    w->hexagons.size * (posRow + 1) + posRow * (posRow + 1) / 2 - 1)
				return (HIGH);
			else if (posRow >= w->hexagons.size && pos == w->hexagons.size *
				 (posRow - w->hexagons.size) + 1 + 4 * w->hexagons.size *
				 (w->hexagons.size - 1) / 2 - (2 * w->hexagons.size - posRow - 2) *
				 (2 * w->hexagons.size - posRow - 1) / 2)
				return (LOW);
			else
				return (-1);
		case ROW:
			if (posRow == 0)
				return (HIGH);
			else if (posRow == 2 * (w->hexagons.size - 1))
				return (LOW);
			else
				return (-1);
	}
	return (-2);		/*Unknown space formation. */
}

static int
TileNextToSpace(HexagonsWidget w, int rowType, int direction)
{
	if (direction == HIGH) {
		if (rowType == TRBL)
			return ((w->hexagons.spaceRow[ROW] < w->hexagons.size) ?
			 w->hexagons.spacePosition[HIGH] - w->hexagons.size -
				w->hexagons.spaceRow[ROW] + 1 :
				w->hexagons.spacePosition[HIGH] - 3 * w->hexagons.size +
				w->hexagons.spaceRow[ROW] + 2);
		else if (rowType == TLBR)
			return ((w->hexagons.spaceRow[ROW] < w->hexagons.size) ?
			 w->hexagons.spacePosition[HIGH] - w->hexagons.size -
				w->hexagons.spaceRow[ROW] :
				w->hexagons.spacePosition[HIGH] - 3 * w->hexagons.size +
				w->hexagons.spaceRow[ROW] + 1);
		else		/* rowType == ROW */
			return (w->hexagons.spacePosition[HIGH] - 1);
	} else {		/* direction == LOW */
		if (rowType == TRBL)
			return ((w->hexagons.spaceRow[ROW] < w->hexagons.size - 1) ?
			 w->hexagons.spacePosition[HIGH] + w->hexagons.size +
				w->hexagons.spaceRow[ROW] :
				w->hexagons.spacePosition[HIGH] + 3 * w->hexagons.size -
				w->hexagons.spaceRow[ROW] - 3);
		else if (rowType == TLBR)
			return ((w->hexagons.spaceRow[ROW] < w->hexagons.size - 1) ?
			 w->hexagons.spacePosition[HIGH] + w->hexagons.size +
				w->hexagons.spaceRow[ROW] + 1 :
				w->hexagons.spacePosition[HIGH] + 3 * w->hexagons.size -
				w->hexagons.spaceRow[ROW] - 2);
		else		/* rowType == ROW */
			return (w->hexagons.spacePosition[HIGH] + 1);
	}
}

static int
FindTileTriangle(HexagonsWidget w, int pI, int pJ, int pK, int rI, int rJ, int rK)
{
	int         found = TRUE, temp = 0, k = 0, row1 = 0, row2 = 0, pos;

	if (rI == rJ) {
		if (pI == pJ - 1)
			temp = pJ;
		else if (pI == pJ + 1)
			temp = pI;
		else
			found = FALSE;
		k = pK;
		row1 = rI;
		row2 = rK;
	} else if (rJ == rK) {
		if (pJ == pK - 1)
			temp = pK;
		else if (pJ == pK + 1)
			temp = pJ;
		else
			found = FALSE;
		k = pI;
		row1 = rJ;
		row2 = rI;
	} else if (rK == rI) {
		if (pK == pI - 1)
			temp = pI;
		else if (pK == pI + 1)
			temp = pK;
		else
			found = FALSE;
		k = pJ;
		row1 = rK;
		row2 = rJ;
	}
	if (found == FALSE)
		return (0);
	pos = -1;
	if (row1 == row2 + 1) {
		if (row1 <= w->hexagons.size - 1)
			pos = temp - w->hexagons.size - row1;
		else		/* row1 > w->hexagons.size - 1 */
			pos = temp - 3 * w->hexagons.size + row1 + 1;
	} else if (row1 == row2 - 1) {
		if (row1 < w->hexagons.size - 1)
			pos = temp + w->hexagons.size + row1;
		else		/* row1 >= w->hexagons.size - 1 */
			pos = temp + 3 * (w->hexagons.size - 1) - row1;
	}
	if (k == pos)
		return (1);
	return (0);
}

static int
FindDir(HexagonsWidget w, int posTile, int posSpace, int rowTile, int rowSpace)
{
	if (rowTile == rowSpace) {
		if (posTile > posSpace)
			return LEFT;
		else
			return RIGHT;
	} else if (TrBl(w, posTile, rowTile) == TrBl(w, posSpace, rowSpace)) {
		if (posTile > posSpace)
			return TR;
		else
			return BL;
	} else {
		/* if (TlBr(w, posTile, rowTile) == TlBr(w, posSpace, rowSpace)) */
		if (posTile > posSpace)
			return TL;
		else
			return BR;
	}
}

static int
FindSpaceType(HexagonsWidget w, int pos1, int pos2, int row1, int row2)
{
	if (row1 == row2 && (pos1 == pos2 + 1 || pos1 == pos2 - 1))
		return (ROW);
	else if (row1 == row2 - 1) {
		swap(&row1, &row2);
		swap(&pos1, &pos2);
	}
	if (row1 == row2 + 1) {
		if (row1 <= w->hexagons.size - 1) {
			if (pos2 == pos1 - w->hexagons.size - row1)
				return (TLBR);
			else if (pos2 == pos1 - w->hexagons.size - row1 + 1)
				return (TRBL);
		} else {	/* row1 > w->hexagons.size - 1 */
			if (pos2 == pos1 - 3 * w->hexagons.size + row1 + 1)
				return (TLBR);
			else if (pos2 == pos1 - 3 * w->hexagons.size + row1 + 2)
				return (TRBL);
		}
	}
	return (-1);
}

static void
FindMovableTile(HexagonsWidget w, int pos, int posRow, int spaceType, int side, int *tilePos, int *tileRow)
{
	switch (spaceType) {
		case TRBL:
			if (side == HIGH) {
				*tileRow = posRow;
				*tilePos = pos + 1;
			} else {	/* side == LOW */
				*tileRow = posRow - 1;
				*tilePos = (posRow <= w->hexagons.size - 1) ?
					pos - w->hexagons.size - posRow :
					pos - 3 * w->hexagons.size + posRow + 1;
			}
			break;
		case TLBR:
			if (side == HIGH) {
				*tileRow = posRow;
				*tilePos = pos - 1;
			} else {	/* side == LOW */
				*tileRow = posRow - 1;
				*tilePos = (posRow <= w->hexagons.size - 1) ?
					pos - w->hexagons.size - posRow + 1 :
					pos - 3 * w->hexagons.size + posRow + 2;
			}
			break;
		case ROW:
			if (side == HIGH) {
				*tileRow = posRow + 1;
				*tilePos = (posRow < w->hexagons.size - 1) ?
					pos + w->hexagons.size + posRow :
					pos + 3 * w->hexagons.size - posRow - 3;
			} else {	/* side == LOW */
				*tileRow = posRow - 1;
				*tilePos = (posRow <= w->hexagons.size - 1) ?
					pos - w->hexagons.size - posRow :
					pos - 3 * w->hexagons.size + posRow + 1;
			}
			break;
		default:
			(void) printf("FindMovableTile: spaceType %d.\n", spaceType);
	}
}

static void
DrawFrame(HexagonsWidget w, GC gc)
{
	int         sumX, sumY, sumX4, sum3X4, sumY2, offsetX, offsetY;

	sumX = w->hexagons.size * (2 * w->hexagons.offset.x) -
		w->hexagons.tileSize.x / 2 - 2 * w->hexagons.delta.x - 1;
	sumY = w->hexagons.size * (3 * w->hexagons.tileSize.y + 2 *
			   w->hexagons.delta.y) - w->hexagons.tileSize.y - 1;
	offsetX = w->hexagons.puzzleOffset.x - 1;
	offsetY = w->hexagons.puzzleOffset.y;
	sumX4 = sumX / 4 + offsetX;
	sum3X4 = 3 * sumX / 4 + offsetX + 2;
	sumY2 = sumY / 2 + offsetY;
	sumX += offsetX + 1 + w->hexagons.size / 2;
	sumY += offsetY;
	offsetX += 1 - w->hexagons.size / 2;
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  sumX4, offsetY, sum3X4, offsetY);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  sum3X4, offsetY, sumX, sumY2);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  sumX, sumY2, sum3X4, sumY);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  sum3X4, sumY, sumX4, sumY);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  sumX4, sumY, offsetX, sumY2);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  offsetX, sumY2, sumX4, offsetY);
}

void
DrawAllTiles(HexagonsWidget w, GC tileGC, GC borderGC)
{
	int         k;

	for (k = 0; k < w->hexagons.sizeSize; k++)
		if (w->hexagons.tileOfPosition[k] <= 0)
			DrawTile(w, w->hexagons.inverseGC, w->hexagons.inverseGC, k, FALSE);
		else
			DrawTile(w, tileGC, borderGC, k, FALSE);
}

static void
DrawTile(HexagonsWidget w, GC tileGC, GC borderGC, int pos, int offset)
{
	int         dx, dy, k = Row(w, pos), orient = (w->hexagons.corners) ? 1 : 0;

	dx = w->hexagons.tileSize.x / 4 - 1 + (2 * TrBl(w, pos, k) +
			   w->hexagons.size - k) * w->hexagons.offset.x / 2 +
		w->hexagons.puzzleOffset.x + offset;
	dy = k * (3 * w->hexagons.tileSize.y / 2 + w->hexagons.delta.y) +
		w->hexagons.delta.y - 1 + w->hexagons.puzzleOffset.y + offset;
	if (orient) {
		hexagonList[orient][0].x = dx;
		hexagonList[orient][0].y = dy;
	} else {
		hexagonList[orient][0].x = dx - w->hexagons.offset.x / 4;
		hexagonList[orient][0].y = dy +
			(w->hexagons.tileSize.y + w->hexagons.delta.y) / 4;
	}
	XFillPolygon(XtDisplay(w), XtWindow(w), tileGC, hexagonList[orient], 6,
		     Convex, CoordModePrevious);
	XDrawLines(XtDisplay(w), XtWindow(w), borderGC, hexagonList[orient], 7,
		   CoordModePrevious);
	if (tileGC != w->hexagons.inverseGC || borderGC != w->hexagons.inverseGC) {
		int         i = 0, offsetX = 0;
		int         tile = w->hexagons.tileOfPosition[pos];
		char        buf[5];

		(void) sprintf(buf, "%d", tile);
		while (tile >= 1) {
			tile /= w->hexagons.base;
			offsetX += w->hexagons.digitOffset.x;
			i++;
		}
		XDrawString(XtDisplay(w), XtWindow(w), w->hexagons.inverseGC,
			    dx - offsetX,
			    dy + w->hexagons.tileSize.y + w->hexagons.digitOffset.y, buf, i);
	}
}

#ifdef DEBUG
static int
PositionInRow(w, position, posRow)
	HexagonsWidget w;
	int         position, posRow;
{
	return (posRow <= w->hexagons.size - 1) ?
		(position - w->hexagons.size * posRow - posRow * (posRow - 1) / 2) :
		(position - w->hexagons.size * (posRow - w->hexagons.size) -
		 4 * w->hexagons.size * (w->hexagons.size - 1) / 2 + (2 *
	w->hexagons.size - posRow - 2) * (2 * w->hexagons.size - posRow - 1) /
		 2 - 1);
}
#endif

static int
PositionFromRow(HexagonsWidget w, int rowPosition, int posRow)
{
	return (posRow <= w->hexagons.size - 1) ?
		(w->hexagons.size * posRow + posRow * (posRow - 1) / 2 + rowPosition) :
		(w->hexagons.size * (posRow - w->hexagons.size) + 4 *
		 w->hexagons.size * (w->hexagons.size - 1) / 2 - (2 * w->hexagons.size -
								  posRow - 2) * (2 * w->hexagons.size - posRow - 1) / 2 + 1 + rowPosition);
}

int
Row(HexagonsWidget w, int pos)
{
	return (pos <= w->hexagons.sizeCenter) ?
		(1 + Sqrt(1 + 8 * (pos + w->hexagons.size *
		       (w->hexagons.size - 1) / 2))) / 2 - w->hexagons.size :
		3 * w->hexagons.size - 2 - (1 + Sqrt(1 + 8 * (w->hexagons.sizeSize - 1 +
		  w->hexagons.size * (w->hexagons.size - 1) / 2 - pos))) / 2;
}

/* Passing row so there is no sqrt calculation again */
int
TrBl(HexagonsWidget w, int pos, int posRow)
{
	return (pos <= w->hexagons.sizeCenter) ?
		(pos + w->hexagons.size * (w->hexagons.size - 1) / 2) -
		(posRow + w->hexagons.size) * (posRow + w->hexagons.size - 1) / 2 :
		2 * w->hexagons.size - 2 - (w->hexagons.sizeSize - 1 +
			w->hexagons.size * (w->hexagons.size - 1) / 2 - pos -
					    (3 * w->hexagons.size - posRow - 2) * (3 * w->hexagons.size - posRow - 3) /
					    2);
}

int
TlBr(HexagonsWidget w, int pos, int posRow)
{
	return (pos <= w->hexagons.sizeCenter) ?
		-1 - ((pos + w->hexagons.size * (w->hexagons.size - 1) / 2) -
	 (posRow + w->hexagons.size + 1) * (posRow + w->hexagons.size) / 2) :
		2 * w->hexagons.size - 1 + (w->hexagons.sizeSize - 1 +
			w->hexagons.size * (w->hexagons.size - 1) / 2 - pos -
					    (3 * w->hexagons.size - posRow - 1) * (3 * w->hexagons.size - posRow - 2) /
					    2);
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
CheckSolved(HexagonsWidget w)
{
	int         i;

	for (i = 1; i < w->hexagons.sizeSize - ((w->hexagons.corners) ? 1 : 0); i++)
		if (w->hexagons.tileOfPosition[i - 1] != i)
			return FALSE;
	return TRUE;
}

static void
swap(int *a, int *b)
{
	int         c;

	c = *b;
	*b = *a;
	*a = c;
}
