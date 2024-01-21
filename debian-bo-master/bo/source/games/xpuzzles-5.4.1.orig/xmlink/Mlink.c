/*-
# X-BASED MISSING LINK(tm)
#
#  Mlink.c
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

/* Methods file for Mlink */

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
#include "MlinkP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/mlink.data"
#endif

static void InitializeMlink(Widget request, Widget new);
static void ExposeMlink(Widget new, XEvent * event, Region region);
static void ResizeMlink(MlinkWidget w);
static void DestroyMlink(Widget old);
static Boolean SetValuesMlink(Widget current, Widget request, Widget new);
static void QuitMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void MoveMlinkTop(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void MoveMlinkLeft(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void MoveMlinkRight(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void MoveMlinkBottom(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void MoveMlinkInput(MlinkWidget w, int x, int direction, int shift, int control);
static void SelectMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void ReleaseMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeMlinkMaybe(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void GetMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void WriteMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void UndoMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void SolveMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void OrientizeMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void MiddleMlink(MlinkWidget w, XEvent * event, char **args, int nArgs);
static void ControlMlink(MlinkWidget w, int direction);
static void RotateMlink(MlinkWidget w, int direction, int tile);
static void SlideMlink(MlinkWidget w, int direction);
static int  SelectTiles(MlinkWidget w, int x, int y, int *i, int *j);
static void SelectSlideTiles(MlinkWidget w, int pos);
static void GetColor(MlinkWidget w, int face, int init);
static void MoveShiftCb(MlinkWidget w, int direction);
static void CheckTiles(MlinkWidget w);
static void ResetTiles(MlinkWidget w);
static void ResizeTiles(MlinkWidget w);
static void MoveNoTiles(MlinkWidget w);
static int  MoveTilesDir(MlinkWidget w, int direction, int tile);
static void RandomizeTiles(MlinkWidget w);
static void MoveTiles(MlinkWidget w, int from);
static void RotateTiles(MlinkWidget w, int c, int direction);
static int  ExchangeTiles(MlinkWidget w, int pos1, int pos2);
static void DrawFrame(MlinkWidget w, GC gc);
static void DrawTile(MlinkWidget w, GC tileGC, GC borderGC, int pos, int offset);
static void DrawLink(MlinkWidget w, GC gc, int pos, int offset);
static int  PositionTile(MlinkWidget w, int x, int *r);
static int  Row(MlinkWidget w, int pos);
static int  Column(MlinkWidget w, int pos);

static char defaultTranslationsMlink[] =
"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
   <KeyPress>Up: MoveTop()\n\
   <KeyPress>KP_8: MoveTop()\n\
   <KeyPress>R8: MoveTop()\n\
   <KeyPress>Left: MoveLeft()\n\
   <KeyPress>KP_4: MoveLeft()\n\
   <KeyPress>R10: MoveLeft()\n\
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
   <KeyPress>s: Solve()\n\
   <KeyPress>o: Orientize()\n\
   <KeyPress>m: Middle()";

static XtActionsRec actionsListMlink[] =
{
	{"Quit", (XtActionProc) QuitMlink},
	{"MoveTop", (XtActionProc) MoveMlinkTop},
	{"MoveLeft", (XtActionProc) MoveMlinkLeft},
	{"MoveRight", (XtActionProc) MoveMlinkRight},
	{"MoveBottom", (XtActionProc) MoveMlinkBottom},
	{"Select", (XtActionProc) SelectMlink},
	{"Release", (XtActionProc) ReleaseMlink},
	{"Randomize", (XtActionProc) RandomizeMlink},
	{"RandomizeMaybe", (XtActionProc) RandomizeMlinkMaybe},
	{"Get", (XtActionProc) GetMlink},
	{"Write", (XtActionProc) WriteMlink},
	{"Undo", (XtActionProc) UndoMlink},
	{"Solve", (XtActionProc) SolveMlink},
	{"Orientize", (XtActionProc) OrientizeMlink},
	{"Middle", (XtActionProc) MiddleMlink}
};

static XtResource resourcesMlink[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.username), XtRString, "nobody"},
	{XtNfaceColor0, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[0]), XtRString, "White"},
	{XtNfaceColor1, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[1]), XtRString, "Yellow"},
	{XtNfaceColor2, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[2]), XtRString, "Green"},
	{XtNfaceColor3, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[3]), XtRString, "Red"},
	{XtNfaceColor4, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[4]), XtRString, "Blue"},
	{XtNfaceColor5, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[5]), XtRString, "Magenta"},
	{XtNfaceColor6, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[6]), XtRString, "Cyan"},
	{XtNfaceColor7, XtCLabel, XtRString, sizeof (String),
	 XtOffset(MlinkWidget, mlink.faceName[7]), XtRString, "Orange"},
	{XtNforeground, XtCBackground, XtRPixel, sizeof (Pixel),
    XtOffset(MlinkWidget, mlink.foreground), XtRString, XtDefaultBackground},
	{XtNtileColor, XtCForeground, XtRPixel, sizeof (Pixel),
     XtOffset(MlinkWidget, mlink.tileColor), XtRString, XtDefaultForeground},
	{XtNtileBorder, XtCBackground, XtRPixel, sizeof (Pixel),
   XtOffset(MlinkWidget, mlink.borderColor), XtRString, XtDefaultBackground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(MlinkWidget, core.width), XtRString, "200"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(MlinkWidget, core.height), XtRString, "200"},
	{XtNtiles, XtCTiles, XtRInt, sizeof (int),
	 XtOffset(MlinkWidget, mlink.tiles), XtRString, "4"},	/* DEFAULTTILES */
	{XtNfaces, XtCFaces, XtRInt, sizeof (int),
	 XtOffset(MlinkWidget, mlink.faces), XtRString, "4"},	/* DEFAULTFACES */
	{XtNorient, XtCOrient, XtRBoolean, sizeof (Boolean),
	 XtOffset(MlinkWidget, mlink.orient), XtRString, "FALSE"},	/*DEFAULTORIENT */
	{XtNmiddle, XtCMiddle, XtRBoolean, sizeof (Boolean),
	 XtOffset(MlinkWidget, mlink.middle), XtRString,
	 "TRUE"},		/*DEFAULTMIDDLE */
	{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
	 XtOffset(MlinkWidget, mlink.mono), XtRString, "FALSE"},
	{XtNbase, XtCBase, XtRInt, sizeof (int),
	 XtOffset(MlinkWidget, mlink.base), XtRString, "10"},
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(MlinkWidget, mlink.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(MlinkWidget, mlink.select), XtRCallback, NULL}
};

MlinkClassRec mlinkClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Mlink",	/* class name */
		sizeof (MlinkRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeMlink,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListMlink,	/* actions */
		XtNumber(actionsListMlink),	/* num actions */
		resourcesMlink,	/* resources */
		XtNumber(resourcesMlink),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		(XtWidgetProc) DestroyMlink,	/* destroy */
		(XtWidgetProc) ResizeMlink,	/* resize */
		(XtExposeProc) ExposeMlink,	/* expose */
		(XtSetValuesFunc) SetValuesMlink,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		NULL,		/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsMlink,	/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	}
};

WidgetClass mlinkWidgetClass = (WidgetClass) & mlinkClassRec;

static void
InitializeMlink(Widget request, Widget new)
{
	MlinkWidget w = (MlinkWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         face;

	w->mlink.tileOfPosition = NULL;
	CheckTiles(w);
	InitMoves();
	ResetTiles(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.foreground = w->mlink.foreground;
	values.background = w->core.background_pixel;
	w->mlink.puzzleGC = XtGetGC(new, valueMask, &values);
	w->mlink.depth = DefaultDepthOfScreen(XtScreen(w));
	values.foreground = w->mlink.tileColor;
	w->mlink.tileGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->mlink.borderColor;
	w->mlink.borderGC = XtGetGC(new, valueMask, &values);
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->mlink.foreground;
	w->mlink.inverseGC = XtGetGC(new, valueMask, &values);
	for (face = 0; face < MAXFACES; face++)
		GetColor(w, face, TRUE);
	ResizeMlink(w);
}

static void
DestroyMlink(Widget old)
{
	MlinkWidget w = (MlinkWidget) old;
	int         face;

	for (face = 0; face < MAXFACES; face++)
		XtReleaseGC(old, w->mlink.faceGC[face]);
	XtReleaseGC(old, w->mlink.tileGC);
	XtReleaseGC(old, w->mlink.puzzleGC);
	XtReleaseGC(old, w->mlink.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->mlink.select);
}

static void
ResizeMlink(MlinkWidget w)
{
	w->mlink.delta.x = 3;
	w->mlink.delta.y = 3;
	w->mlink.offset.x = MAX(((int) w->core.width - w->mlink.delta.x) /
				w->mlink.tiles, 0);
	w->mlink.offset.y = MAX(((int) w->core.height - w->mlink.delta.y) /
				w->mlink.faces, 0);
	w->mlink.faceSize.x = w->mlink.offset.x * w->mlink.tiles +
		w->mlink.delta.x;
	w->mlink.faceSize.y = w->mlink.offset.y + w->mlink.delta.y;
	w->mlink.puzzleSize.x = w->mlink.faceSize.x + w->mlink.delta.x;
	w->mlink.puzzleSize.y = (w->mlink.faceSize.y - w->mlink.delta.y) *
		w->mlink.faces + w->mlink.delta.y;
	w->mlink.puzzleOffset.x = ((int) w->core.width -
				   w->mlink.puzzleSize.x + 2) / 2;
	w->mlink.puzzleOffset.y = ((int) w->core.height -
				   w->mlink.puzzleSize.y + 2) / 2;
	w->mlink.tileSize.x = MAX(w->mlink.offset.x - w->mlink.delta.x, 0);
	w->mlink.tileSize.y = MAX(w->mlink.offset.y - w->mlink.delta.y, 0);
	ResizeTiles(w);
}

static void
ExposeMlink(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	MlinkWidget w = (MlinkWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->mlink.puzzleGC);
		DrawAllTiles(w, w->mlink.tileGC, w->mlink.borderGC);
	}
}

static      Boolean
SetValuesMlink(Widget current, Widget request, Widget new)
{
	MlinkWidget c = (MlinkWidget) current, w = (MlinkWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	Boolean     redrawTiles = FALSE;
	int         face;

	CheckTiles(w);
	if (w->mlink.foreground != c->mlink.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->mlink.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->mlink.puzzleGC);
		w->mlink.puzzleGC = XtGetGC(new, valueMask, &values);
		if (w->mlink.mono || w->mlink.depth == 1) {
			values.foreground = w->core.background_pixel;
			values.background = w->mlink.tileColor;
			for (face = 0; face < MAXFACES; face++) {
				XtReleaseGC((Widget) w, w->mlink.faceGC[face]);
				w->mlink.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
			}
			c->mlink.mono = w->mlink.mono;
		}
		redrawTiles = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->mlink.foreground;
		XtReleaseGC(new, w->mlink.inverseGC);
		w->mlink.inverseGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->mlink.tileColor != c->mlink.tileColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->mlink.tileColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->mlink.tileGC);
		w->mlink.tileGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	if (w->mlink.borderColor != c->mlink.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->mlink.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->mlink.borderGC);
		w->mlink.borderGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	for (face = 0; face < MAXFACES; face++) {
		if (strcmp(w->mlink.faceName[face], c->mlink.faceName[face]))
			GetColor(w, face, FALSE);
	}
	if (w->mlink.tiles != c->mlink.tiles ||
	    w->mlink.faces != c->mlink.faces ||
	    w->mlink.orient != c->mlink.orient ||
	    w->mlink.middle != c->mlink.middle ||
	    w->mlink.base != c->mlink.base) {
		ResetTiles(w);
		ResizeMlink(w);
		redraw = TRUE;
	} else if (w->mlink.offset.x != c->mlink.offset.x ||
		   w->mlink.offset.y != c->mlink.offset.y) {
		ResizeMlink(w);
		redraw = TRUE;
	}
	if (redrawTiles && !redraw && XtIsRealized(new) && new->core.visible) {
		DrawFrame(w, c->mlink.inverseGC);
		DrawAllTiles(c, c->mlink.inverseGC, c->mlink.inverseGC);
		DrawFrame(w, w->mlink.puzzleGC);
		DrawAllTiles(w, c->mlink.tileGC, c->mlink.borderGC);
	}
	return (redraw);
}

static void
QuitMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	int         i, j, pos, shift;

	pos = SelectTiles(w, event->xbutton.x, event->xbutton.y, &i, &j);
	if (-w->mlink.tileFaces != pos) {
		w->mlink.currentTile = i;
		w->mlink.currentFace = j;
		w->mlink.currentRef = pos;
		shift = (int) (event->xkey.state & (ShiftMask | LockMask));
		if (shift || !CheckSolved(w)) {
			pos = w->mlink.currentTile + w->mlink.currentFace * w->mlink.tiles;
			if (w->mlink.tileOfPosition[i + j * w->mlink.tiles] <= 0)
				DrawTile(w, w->mlink.borderGC, w->mlink.borderGC, pos, TRUE);
			else
				DrawTile(w, w->mlink.borderGC, w->mlink.tileGC, pos, TRUE);
		}
	} else
		w->mlink.currentRef = -w->mlink.tileFaces;
}

static void
ReleaseMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	int         i, j, pos, diff, shift;

	if (w->mlink.currentRef == -w->mlink.tileFaces)
		return;
	pos = w->mlink.currentTile + w->mlink.currentFace * w->mlink.tiles;
	DrawTile(w, w->mlink.inverseGC, w->mlink.inverseGC, pos, TRUE);
	if (w->mlink.tileOfPosition[pos] > 0)
		DrawTile(w, w->mlink.tileGC, w->mlink.borderGC, pos, FALSE);
	shift = (int) (event->xkey.state & (ShiftMask | LockMask));
	if (!shift && CheckSolved(w))
		MoveNoTiles(w);
	else {
		pos = SelectTiles(w, event->xbutton.x, event->xbutton.y, &i, &j);
		if (-w->mlink.tileFaces != pos) {
			if (j == w->mlink.currentFace) {
				pos = w->mlink.currentRef;
				if (pos / w->mlink.tiles == 0 &&
				    j == Column(w, w->mlink.spacePosition) && pos != 0) {
					if (shift && CheckSolved(w))
						MoveNoTiles(w);
					else {
						SelectSlideTiles(w, pos);
						w->mlink.currentTile = w->mlink.tiles;
						if (CheckSolved(w)) {
							mlinkCallbackStruct cb;

							cb.reason = MLINK_SOLVED;
							XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
						}
					}
				}
			} else {
				diff = (w->mlink.currentFace - j + w->mlink.faces) % w->mlink.faces;
				if (diff > w->mlink.faces / 2)
					for (i = 0; i < w->mlink.faces - diff; i++)
						(void) MoveMlink(w, BOTTOM, w->mlink.currentTile, shift);
				else
					for (i = 0; i < diff; i++)
						(void) MoveMlink(w, TOP, w->mlink.currentTile, shift);
				if (!shift && CheckSolved(w)) {
					mlinkCallbackStruct cb;

					cb.reason = MLINK_SOLVED;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				}
			}
		}
	}
}

static void
RandomizeMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizeTiles(w);
}

static void
RandomizeMlinkMaybe(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->mlink.started)
		RandomizeTiles(w);
}

static void
GetMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, tiles, faces, middle, orient, moves;
	mlinkCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &tiles);
		if (tiles >= MINTILES) {
			for (i = w->mlink.tiles; i < tiles; i++) {
				cb.reason = MLINK_INC_X;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->mlink.tiles; i > tiles; i--) {
				cb.reason = MLINK_DEC_X;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: tiles %d should be between %d and MAXINT\n",
				      DATAFILE, tiles, MINTILES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &faces);
		if (faces >= MINFACES && faces <= MAXFACES) {
			for (i = w->mlink.faces; i < faces; i++) {
				cb.reason = MLINK_INC_Y;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->mlink.faces; i > faces; i--) {
				cb.reason = MLINK_DEC_Y;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: faces %d should be between %d and %d\n",
				      DATAFILE, faces, MINFACES, MAXFACES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &middle);
		if (w->mlink.middle != (Boolean) middle) {
			cb.reason = MLINK_MIDDLE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &orient);
		if (w->mlink.orient != (Boolean) orient) {
			cb.reason = MLINK_ORIENT;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = MLINK_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		SetStartPosition(w);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: tiles %d, faces %d, middle %d, orient %d, moves %d.\n",
			      DATAFILE, tiles, faces, middle, orient, moves);
	}
}

static void
WriteMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "tiles%c %d\n", SYMBOL, w->mlink.tiles);
		(void) fprintf(fp, "faces%c %d\n", SYMBOL, w->mlink.faces);
		(void) fprintf(fp, "middle%c %d\n", SYMBOL, (w->mlink.middle) ? 1 : 0);
		(void) fprintf(fp, "orient%c %d\n", SYMBOL, (w->mlink.orient) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         direction, tile, shift;
		mlinkCallbackStruct cb;

		GetMove(&direction, &tile, &shift);
		direction = (direction + COORD / 2) % COORD;
		if (shift && (direction == TOP || direction == BOTTOM))
			MoveShiftCb(w, direction);
		else if (MoveTilesDir(w, direction, tile)) {
			cb.reason = MLINK_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolveMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolveTiles(w); *//* Sorry, unimplemented */
}

static void
OrientizeMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	mlinkCallbackStruct cb;

	cb.reason = MLINK_ORIENT;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MiddleMlink(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	mlinkCallbackStruct cb;

	cb.reason = MLINK_MIDDLE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MoveMlinkTop(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMlinkInput(w, event->xbutton.x, TOP,
		       (int) (event->xkey.state & (ShiftMask | LockMask)),
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMlinkLeft(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMlinkInput(w, event->xbutton.x, LEFT,
		       (int) (event->xkey.state & (ShiftMask | LockMask)),
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMlinkRight(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMlinkInput(w, event->xbutton.x, RIGHT,
		       (int) (event->xkey.state & (ShiftMask | LockMask)),
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMlinkBottom(MlinkWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMlinkInput(w, event->xbutton.x, BOTTOM,
		       (int) (event->xkey.state & (ShiftMask | LockMask)),
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMlinkInput(MlinkWidget w, int x, int direction, int shift, int control)
{
	mlinkCallbackStruct cb;
	int         r;

	if (control)
		ControlMlink(w, direction);
	else if (shift && (direction == TOP || direction == BOTTOM)) {
		MoveShiftCb(w, direction);
		PutMove(direction, 0, 1);
	} else if (CheckSolved(w)) {
		MoveNoTiles(w);
		return;
	} else if (direction == LEFT || direction == RIGHT) {
		SlideMlink(w, direction);
		if (CheckSolved(w)) {
			cb.reason = MLINK_SOLVED;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	} else {
		if (!PositionTile(w, x, &r))
			return;
		RotateMlink(w, direction, r);
		if (CheckSolved(w)) {
			cb.reason = MLINK_SOLVED;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

int
MoveMlink(MlinkWidget w, int direction, int tile, int shift)
{
	mlinkCallbackStruct cb;

	if (shift && (direction == TOP || direction == BOTTOM)) {
		MoveShiftCb(w, direction);
		return TRUE;
	} else if (CheckSolved(w)) {
		MoveNoTiles(w);
		return FALSE;
	} else if (MoveTilesDir(w, direction, tile)) {
		cb.reason = MLINK_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		PutMove(direction, tile, 0);
		return TRUE;
	}
	return FALSE;
}

static void
ControlMlink(MlinkWidget w, int direction)
{
	mlinkCallbackStruct cb;

	cb.reason = MLINK_IGNORE;
	switch (direction) {
		case TOP:
			if (w->mlink.faces <= MINFACES)
				return;
			cb.reason = MLINK_DEC_Y;
			break;
		case RIGHT:
			cb.reason = MLINK_INC_X;
			break;
		case BOTTOM:
			if (w->mlink.faces >= MAXFACES)
				return;
			cb.reason = MLINK_INC_Y;
			break;
		case LEFT:
			if (w->mlink.tiles <= MINTILES)
				return;
			cb.reason = MLINK_DEC_X;
			break;
		default:
			(void) printf("ControlMlink: direction %d\n", direction);
	}
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
RotateMlink(MlinkWidget w, int direction, int tile)
{
	mlinkCallbackStruct cb;

	if (CheckSolved(w)) {
		MoveNoTiles(w);
		return;
	}
	(void) MoveMlink(w, direction, tile, FALSE);
	if (CheckSolved(w)) {
		cb.reason = MLINK_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	return;
}

static void
SlideMlink(MlinkWidget w, int direction)
{
	mlinkCallbackStruct cb;

	if (CheckSolved(w)) {
		MoveNoTiles(w);
		return;
	}
	if (!MoveMlink(w, direction, 0, FALSE)) {
		cb.reason = MLINK_BLOCKED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		return;
	}
	if (CheckSolved(w)) {
		cb.reason = MLINK_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	return;
}

static int
SelectTiles(MlinkWidget w, int x, int y, int *i, int *j)
{
	*i = (x - w->mlink.delta.x / 2 - w->mlink.puzzleOffset.x) /
		w->mlink.offset.x;
	*j = ((y - w->mlink.delta.y / 2 - w->mlink.puzzleOffset.y) %
	      (w->mlink.faces * w->mlink.offset.y + w->mlink.delta.y - 1)) /
		w->mlink.offset.y;
	if (*i >= 0 && *j >= 0 &&
	    *i < w->mlink.tiles && *j < w->mlink.faces)
		return (*i + w->mlink.tiles * *j -
			w->mlink.spacePosition % w->mlink.tileFaces);
	return -w->mlink.tileFaces;
}

static void
SelectSlideTiles(MlinkWidget w, int pos)
{
	mlinkCallbackStruct cb;
	int         n;

	if (pos < 0) {
		for (n = 1; n <= -pos % w->mlink.tiles; n++) {
			MoveTiles(w, w->mlink.spacePosition - 1);
			cb.reason = MLINK_MOVED;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			PutMove(RIGHT, 0, 0);
		}
	} else if (pos > 0) {
		for (n = 1; n <= pos % w->mlink.tiles; n++) {
			MoveTiles(w, w->mlink.spacePosition + 1);
			cb.reason = MLINK_MOVED;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			PutMove(LEFT, 0, 0);
		}
	}
}

static void
GetColor(MlinkWidget w, int face, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->mlink.depth > 1 && !w->mlink.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
				w->mlink.faceName[face], &colorCell, &rgb)) {
			values.foreground = w->mlink.faceColor[face] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->mlink.faceGC[face]);
			w->mlink.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined",
				       w->mlink.faceName[face]);
			XtWarning(buf);
		}
	}
	values.foreground = w->core.background_pixel;
	values.background = w->mlink.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->mlink.faceGC[face]);
	w->mlink.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
}

static void
MoveShiftCb(MlinkWidget w, int direction)
{
	mlinkCallbackStruct cb;

	if (w->mlink.middle) {
		(void) MoveTilesDir(w, direction, 0);
		(void) MoveTilesDir(w, direction, 1);
		(void) MoveTilesDir(w, direction, w->mlink.tiles - 1);
	} else {
		int         r;

		for (r = 0; r < w->mlink.tiles; r++)
			(void) MoveTilesDir(w, direction, r);
	}
	cb.reason = MLINK_CONTROL;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
CheckTiles(MlinkWidget w)
{
	char        buf[121];

	if (w->mlink.tiles < MINTILES) {
		(void) sprintf(buf,
			       "Number of Mlink in X direction out of bounds, use %d..MAXINT",
			       MINTILES);
		XtWarning(buf);
		w->mlink.tiles = DEFAULTTILES;
	}
	if (w->mlink.faces < MINFACES || w->mlink.faces > MAXFACES) {
		(void) sprintf(buf,
		  "Number of Mlink in Y direction out of bounds, use %d..%d",
			       MINFACES, MAXFACES);
		XtWarning(buf);
		w->mlink.faces = DEFAULTFACES;
	}
	w->mlink.base = 10;
}

static void
ResetTiles(MlinkWidget w)
{
	int         i;

	w->mlink.tileFaces = w->mlink.tiles * w->mlink.faces;
	if (w->mlink.tileOfPosition)
		(void) free((void *) w->mlink.tileOfPosition);
	if (!(w->mlink.tileOfPosition = (int *)
	      malloc(sizeof (int) * w->mlink.tileFaces)))
		            XtError("Not enough memory, exiting.");

	if (startPosition)
		(void) free((void *) startPosition);
	if (!(startPosition = (int *)
	      malloc(sizeof (int) * w->mlink.tileFaces)))
		            XtError("Not enough memory, exiting.");

	w->mlink.spacePosition = w->mlink.tileFaces - 1;
	w->mlink.tileOfPosition[w->mlink.tileFaces - 1] = 0;
	for (i = 1; i < w->mlink.tileFaces; i++)
		w->mlink.tileOfPosition[i - 1] = i;
	FlushMoves(w);
	w->mlink.started = FALSE;
}

static void
ResizeTiles(MlinkWidget w)
{
	w->mlink.digitOffset.x = 3;
	w->mlink.digitOffset.y = 4;
}

static void
MoveNoTiles(MlinkWidget w)
{
	mlinkCallbackStruct cb;

	cb.reason = MLINK_IGNORE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static int
MoveTilesDir(MlinkWidget w, int direction, int tile)
{
	switch (direction) {
		case TOP:
			RotateTiles(w, tile, TOP);
			return TRUE;
		case RIGHT:
			if (Row(w, w->mlink.spacePosition) > 0) {
				MoveTiles(w, w->mlink.spacePosition - 1);
				return TRUE;
			}
			break;
		case BOTTOM:
			RotateTiles(w, tile, BOTTOM);
			return TRUE;
		case LEFT:
			if (Row(w, w->mlink.spacePosition) < w->mlink.tiles - 1) {
				MoveTiles(w, w->mlink.spacePosition + 1);
				return TRUE;
			}
			break;
		default:
			(void) printf("MoveTilesDir: direction %d\n", direction);
	}
	return FALSE;
}

static void
RandomizeTiles(MlinkWidget w)
{
	mlinkCallbackStruct cb;

	/* First interchange tiles an even number of times */
	if (w->mlink.tiles > 1 && w->mlink.faces > 1 && w->mlink.tileFaces > 4) {
		int         pos, count = 0;

		for (pos = 0; pos < w->mlink.tileFaces; pos++) {
			int         randomPos = pos;

			while (randomPos == pos)
				randomPos = NRAND(w->mlink.tileFaces);
			count += ExchangeTiles(w, pos, randomPos);
		}
		if (count % 2)
			if (!ExchangeTiles(w, 0, 1))
				if (!ExchangeTiles(w,
						   w->mlink.tileFaces - 2, w->mlink.tileFaces - 1))
					(void) printf("RandomizeTiles: should not get here\n");
		DrawAllTiles(w, w->mlink.tileGC, w->mlink.borderGC);
	}
	/* Now move the space around randomly */
	if (w->mlink.tiles > 1 || w->mlink.faces > 1) {
		int         big = w->mlink.tileFaces + NRAND(2);
		int         lastDirection = 0;
		int         randomDirection;

		cb.reason = MLINK_RESET;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
		big = 3;
#endif
		if (big > 100)
			big = 100;
		while (big--) {
			randomDirection = NRAND(COORD);

#ifdef DEBUG
			sleep(1);
#endif

			if ((randomDirection + COORD / 2) % COORD != lastDirection ||
			    (w->mlink.tiles == 1 && w->mlink.faces == 1)) {
				if (MoveMlink(w, randomDirection, NRAND(w->mlink.tiles), FALSE))
					lastDirection = randomDirection;
				else
					big++;
			}
		}
		FlushMoves(w);
		cb.reason = MLINK_RANDOMIZE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	if (CheckSolved(w)) {
		cb.reason = MLINK_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MoveTiles(MlinkWidget w, int from)
{
	int         tempTile;

	tempTile = w->mlink.tileOfPosition[from];
	w->mlink.tileOfPosition[from] =
		w->mlink.tileOfPosition[w->mlink.spacePosition];
	w->mlink.tileOfPosition[w->mlink.spacePosition] = tempTile;
	DrawTile(w, w->mlink.tileGC, w->mlink.borderGC, w->mlink.spacePosition,
		 FALSE);
	w->mlink.spacePosition = from;
	DrawTile(w, w->mlink.inverseGC, w->mlink.inverseGC, w->mlink.spacePosition,
		 FALSE);
}

static void
RotateTiles(MlinkWidget w, int c, int direction)
{
	int         newR, k, tempTile, currTile, pos, newPos;
	int         r = 0, space = -1, loop = FALSE;

	if (w->mlink.middle && c > 0 && c < w->mlink.tiles - 1) {
		loop = TRUE;
		c = 1;
	}
	do {
		currTile = w->mlink.tileOfPosition[r * w->mlink.tiles + c];
		for (k = 1; k <= w->mlink.faces; k++) {
			newR = (direction == TOP) ? (r + w->mlink.faces - 1) % w->mlink.faces :
				(r + 1) % w->mlink.faces;
			pos = r * w->mlink.tiles + c;
			newPos = newR * w->mlink.tiles + c;
			tempTile = w->mlink.tileOfPosition[newPos];
			w->mlink.tileOfPosition[newPos] = currTile;
			currTile = tempTile;
			if (w->mlink.spacePosition == pos) {
				DrawTile(w, w->mlink.inverseGC, w->mlink.inverseGC, newPos, FALSE);
				space = newPos;
			} else
				DrawTile(w, w->mlink.tileGC, w->mlink.borderGC, newPos, FALSE);
			r = newR;
		}
		if (space >= 0)
			w->mlink.spacePosition = space;
	} while (loop && ++c < w->mlink.tiles - 1);
}

static int
ExchangeTiles(MlinkWidget w, int pos1, int pos2)
{
	int         tempTile;

	if (w->mlink.tileOfPosition[pos1] <= 0)
		return FALSE;
	else if (w->mlink.tileOfPosition[pos2] <= 0)
		return FALSE;
	tempTile = w->mlink.tileOfPosition[pos1];
	w->mlink.tileOfPosition[pos1] = w->mlink.tileOfPosition[pos2];
	w->mlink.tileOfPosition[pos2] = tempTile;
	return TRUE;
}

static void
DrawFrame(MlinkWidget w, GC gc)
{
	int         sumX, sumY, offsetX, offsetY, r;

	sumX = w->mlink.tiles * w->mlink.offset.x + w->mlink.delta.x / 2 + 1;
	sumY = w->mlink.offset.y;
	offsetX = w->mlink.puzzleOffset.x;
	offsetY = w->mlink.puzzleOffset.y;
	for (r = 0; r < w->mlink.faces; r++) {
		XFillRectangle(XtDisplay(w), XtWindow(w), gc,
			       offsetX, offsetY, 1, sumY);
		XFillRectangle(XtDisplay(w), XtWindow(w), gc,
			       offsetX, offsetY, sumX, 1);
		XFillRectangle(XtDisplay(w), XtWindow(w), gc,
			       sumX + offsetX, offsetY, 1, sumY + 1);
		XFillRectangle(XtDisplay(w), XtWindow(w), gc,
			       offsetX, sumY + offsetY, sumX + 1, 1);
		offsetY += sumY;
	}
}

void
DrawAllTiles(MlinkWidget w, GC tileGC, GC borderGC)
{
	int         k;

	for (k = 0; k < w->mlink.tileFaces; k++)
		if (w->mlink.tileOfPosition[k] <= 0)
			DrawTile(w, w->mlink.inverseGC, w->mlink.inverseGC, k, FALSE);
		else
			DrawTile(w, tileGC, borderGC, k, FALSE);
}

static void
DrawTile(MlinkWidget w, GC tileGC, GC borderGC, int pos, int offset)
{
	int         dx, dy, tile = w->mlink.tileOfPosition[pos];
	GC          gc = w->mlink.faceGC[((tile - 1) / w->mlink.tiles + 1) % w->mlink.faces];

	dx = Row(w, pos) * w->mlink.offset.x + w->mlink.delta.x +
		w->mlink.puzzleOffset.x + offset;
	dy = Column(w, pos) * w->mlink.offset.y + w->mlink.delta.y +
		w->mlink.puzzleOffset.y + offset;
	XFillRectangle(XtDisplay(w), XtWindow(w), tileGC, dx, dy,
		       w->mlink.tileSize.x, w->mlink.tileSize.y - 2);
	if (tileGC != w->mlink.inverseGC || borderGC != w->mlink.inverseGC) {
		int         i = 0, offsetX = 0, t = tile, mono;
		char        buf[6];

		mono = (w->mlink.depth == 1 || w->mlink.mono);
		if (w->mlink.orient || mono) {
			if (mono) {
				if (w->mlink.orient) {
					(void) sprintf(buf, "%d%c", tile,
						       w->mlink.faceName[((tile - 1) / w->mlink.tiles + 1) %
							 w->mlink.faces][0]);
					t *= w->mlink.base;
				} else {
					(void) sprintf(buf, "%c", w->mlink.faceName[((tile - 1) /
										     w->mlink.tiles + 1) % w->mlink.faces][0]);
					t = 1;
					if (offset)
						gc = w->mlink.tileGC;
				}
			} else
				(void) sprintf(buf, "%d", tile);
			while (t >= 1) {
				t /= w->mlink.base;
				offsetX += w->mlink.digitOffset.x;
				i++;
			}
			XDrawString(XtDisplay(w), XtWindow(w), gc,
				    dx + w->mlink.tileSize.x / 2 - offsetX,
				    dy + w->mlink.tileSize.y / 2 + w->mlink.digitOffset.y, buf, i);
		}
	}
	if (tile != 0 &&
	    ((tile != w->mlink.tileFaces - 1 && w->mlink.tiles > 1) ||
	     w->mlink.tiles > 2))
		DrawLink(w, gc, pos, offset);
	XDrawRectangle(XtDisplay(w), XtWindow(w), borderGC, dx, dy,
		       w->mlink.tileSize.x, w->mlink.tileSize.y - 2);
}

#define MULT 64
#define THICKNESS 7
static void
DrawLink(MlinkWidget w, GC gc, int pos, int offset)
{
	int         dx, dy, sizex, sizey, tile = w->mlink.tileOfPosition[pos],
	            thick;

	sizex = w->mlink.offset.x * 3 / 2 - 3;
	sizey = w->mlink.offset.y * 3 / 4 - 3;
	dx = Row(w, pos) * w->mlink.offset.x + w->mlink.delta.x +
		w->mlink.puzzleOffset.x - sizex / 2 + offset;
	dy = Column(w, pos) * w->mlink.offset.y + w->mlink.delta.y +
		w->mlink.puzzleOffset.y + w->mlink.tileSize.y / 2 - sizey / 2 - 1 + offset;
	thick = MIN(sizey, sizex) / 3;
	thick = MIN(thick, THICKNESS);
	if (thick > 1) {
		XSetLineAttributes(XtDisplay(w), gc, thick,
				   LineSolid, CapNotLast, JoinRound);
		if (tile % w->mlink.tiles == 0 || tile == w->mlink.tileFaces - 1)
			XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
				 sizex, sizey, 89 * MULT, -179 * MULT);
		else if (tile % w->mlink.tiles == 1)
			XDrawArc(XtDisplay(w), XtWindow(w), gc, dx + w->mlink.tileSize.x - 1, dy,
				 sizex, sizey, 90 * MULT, 180 * MULT);
		else {
			XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
				 sizex, sizey, 89 * MULT, -124 * MULT);
			XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
				 sizex, sizey, -90 * MULT, 30 * MULT);
			dx += w->mlink.tileSize.x - 1;
			XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
				 sizex, sizey, 90 * MULT, 30 * MULT);
			XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
				 sizex, sizey, -90 * MULT, -125 * MULT);
		}
		XSetLineAttributes(XtDisplay(w), gc, 1, LineSolid, CapNotLast, JoinRound);
	}
}
/*-
#define MULT 64
#define THICKNESS 8 
static void DrawLink(w, gc, pos, offset)
  MlinkWidget w;
  GC gc;
  int pos;
{
  int dx, dy, sizex, sizey, i, tile = w->mlink.tileOfPosition[pos];

  for (i = 0; i < THICKNESS; i++) {
    sizex = w->mlink.offset.x * 3 / 2 - i;
    sizey = w->mlink.offset.y * 3 / 4 - i;
    dx = Row(w, pos) * w->mlink.offset.x + w->mlink.delta.x +
      w->mlink.puzzleOffset.x - sizex / 2 + offset;
    dy = Column(w, pos) * w->mlink.offset.y + w->mlink.delta.y +
      w->mlink.puzzleOffset.y + w->mlink.tileSize.y / 2 - sizey / 2 + offset;
    if (tile % w->mlink.tiles == 0 || tile == w->mlink.tileFaces - 1)
      XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
        sizex, sizey, 89 * MULT, -179 * MULT);
    else if (tile % w->mlink.tiles == 1)
      XDrawArc(XtDisplay(w), XtWindow(w), gc, dx + w->mlink.tileSize.x - 1, dy,
        sizex, sizey, 90 * MULT, 180 * MULT);
    else {
      XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
        sizex, sizey, 89 * MULT, -32 * MULT);
      XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
        sizex, sizey, -90 * MULT, 128 * MULT);
      dx += w->mlink.tileSize.x - 1;
      XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
        sizex, sizey, 90 * MULT, 128 * MULT);
      XDrawArc(XtDisplay(w), XtWindow(w), gc, dx, dy,
        sizex, sizey, -90 * MULT, -33 * MULT);
    }
  }
}
*/
static int
PositionTile(MlinkWidget w, int x, int *r)
{
	*r = (x - w->mlink.delta.x / 2 - w->mlink.puzzleOffset.x) / w->mlink.offset.x;
	if (*r < 0 || *r >= w->mlink.tiles)
		return FALSE;
	return TRUE;
}

static int
Row(MlinkWidget w, int pos)
{
	return (pos % w->mlink.tiles);
}

static int
Column(MlinkWidget w, int pos)
{
	return (pos / w->mlink.tiles);
}

Boolean
CheckSolved(MlinkWidget w)
{
	int         i, j, firstTile, lastTile, midTile;

	if (w->mlink.tiles < 2)
		return TRUE;
	if (w->mlink.orient) {
		firstTile = w->mlink.tileOfPosition[0];
		if (firstTile != 0 && (firstTile - 1) % w->mlink.tiles == 0) {
			for (i = 1; i < w->mlink.tileFaces; i++) {
				midTile = w->mlink.tileOfPosition[i];
				if (midTile && (midTile - firstTile + w->mlink.tileFaces) %
				    w->mlink.tileFaces != i)
					return FALSE;
			}
		} else
			return FALSE;
	} else {
		for (i = 0; i < w->mlink.faces; i++) {
			firstTile = w->mlink.tileOfPosition[i * w->mlink.tiles];
			if (firstTile == 0)
				firstTile = w->mlink.tileOfPosition[i * w->mlink.tiles + 1];
			lastTile = w->mlink.tileOfPosition[(i + 1) * w->mlink.tiles - 1];
			if (lastTile == 0)
				lastTile = w->mlink.tileOfPosition[(i + 1) * w->mlink.tiles - 2];
			if (firstTile % w->mlink.tiles != 1 ||
			    (lastTile % w->mlink.tiles != 0 &&
			     lastTile != w->mlink.tileFaces - 1) ||
			    (lastTile - 1) / w->mlink.tiles !=
			    (firstTile - 1) / w->mlink.tiles)
				return FALSE;
			for (j = 1; j < w->mlink.tiles - 1; j++) {
				midTile = w->mlink.tileOfPosition[i * w->mlink.tiles + j];
				if ((midTile - 1) / w->mlink.tiles !=
				    (firstTile - 1) / w->mlink.tiles || midTile == 0)
					return FALSE;
			}
		}
	}
	return TRUE;
}
