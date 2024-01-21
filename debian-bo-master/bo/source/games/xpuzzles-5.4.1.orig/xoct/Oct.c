/*-
# X-BASED OCTAHEDRON
#
#  Oct.c
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

/* Methods file for Oct */

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
#include "OctP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/oct.data"
#endif

#define NOTDIR(x) ((x==CW)?CCW:CW)

typedef struct _RTT {
	int         row, trbl, tlbr;
} RTT;

static void InitializeOct(Widget request, Widget new);
static void ExposeOct(Widget new, XEvent * event, Region region);
static void ResizeOct(OctWidget w);
static void DestroyOct(Widget old);
static Boolean SetValuesOct(Widget current, Widget request, Widget new);
static void QuitOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void PracticeOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void PracticeOctMaybe(OctWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeOctMaybe(OctWidget w, XEvent * event, char **args, int nArgs);
static void GetOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void WriteOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void UndoOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void SolveOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void IncrementOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void DecrementOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void OrientizeOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void Period3ModeOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void Period4ModeOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void BothModeOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void StickyModeOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctTl(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctTop(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctTr(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctLeft(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctCw(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctRight(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctBl(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctBottom(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctBr(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctCcw(OctWidget w, XEvent * event, char **args, int nArgs);
static void MoveOctInput(OctWidget w, int x, int y, int direction, int shift, int control);
static void SelectOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void ReleaseOct(OctWidget w, XEvent * event, char **args, int nArgs);
static void GetColor(OctWidget w, int face, int init);
static void MoveControlCb(OctWidget w, int face, int direction, int style);
static void CheckPolyhedrons(OctWidget w);
static void ResetPolyhedrons(OctWidget w);
static void ResizePolyhedrons(OctWidget w);
static int  SelectPolyhedrons(OctWidget w, int x, int y, int *face, RTT * rtt);
static int  NarrowSelection(OctWidget w, int style, int *face, int *direction);
static int  PositionPolyhedrons(OctWidget w, int x, int y, int style, int *face, RTT * rtt, int *direction);
static void MoveNoPolyhedrons(OctWidget w);
static void PracticePolyhedrons(OctWidget w);
static void RandomizePolyhedrons(OctWidget w);
static void MovePolyhedrons(OctWidget w, int face, RTT rtt, int direction, int style);
static void RotateFace(OctWidget w, int view, int side, int direction);

/* RTT : Row, Top right bottom left, Top left bottom right */
static void ReadRTT(OctWidget w, int view, int side, int dir, int h, int len, int orient);
static void RotateRTT(OctWidget w, int rotate, int len, int orient);
static void ReverseRTT(OctWidget w, int len, int orient);
static void WriteRTT(OctWidget w, int view, int side, int dir, int h, int len, int orient);
static void DrawFrame(OctWidget w, GC gc);
static void DrawTriangle(OctWidget w, int face, int position, int offset);
static void DrawOrientLine(OctWidget w, int orient, int face, int side, int dx, int dy);
static int  Length(OctWidget w, int dir, int h);
static int  CheckMoveDir(OctWidget w, RTT rtt1, RTT rtt2, int face, int *direction);
static RTT  ToRTT(int position);
static int  ToPosition(RTT rtt);
static int  Sqrt(int i);

#ifdef DEBUG
static void PrintOcta(OctWidget w);
static void PrintFace(OctWidget w);
static void PrintRow(OctWidget w, int len, int orient);

#endif

static char defaultTranslationsOct[] =
"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
   <KeyPress>KP_Divide: MoveCcw()\n\
   <KeyPress>Home: MoveTl()\n\
   <KeyPress>KP_7: MoveTl()\n\
   <KeyPress>R7: MoveTl()\n\
   <KeyPress>Up: MoveTop()\n\
   <KeyPress>KP_8: MoveTop()\n\
   <KeyPress>R8: MoveTop()\n\
   <KeyPress>Prior: MoveTr()\n\
   <KeyPress>KP_9: MoveTr()\n\
   <KeyPress>R9: MoveTr()\n\
   <KeyPress>Left: MoveLeft()\n\
   <KeyPress>KP_4: MoveLeft()\n\
   <KeyPress>R10: MoveLeft()\n\
   <KeyPress>Begin: MoveCw()\n\
   <KeyPress>KP_5: MoveCw()\n\
   <KeyPress>R11: MoveCw()\n\
   <KeyPress>Right: MoveRight()\n\
   <KeyPress>KP_6: MoveRight()\n\
   <KeyPress>R12: MoveRight()\n\
   <KeyPress>End: MoveBl()\n\
   <KeyPress>KP_1: MoveBl()\n\
   <KeyPress>R13: MoveBl()\n\
   <KeyPress>Down: MoveBottom()\n\
   <KeyPress>KP_2: MoveBottom()\n\
   <KeyPress>R14: MoveBottom()\n\
   <KeyPress>Next: MoveBr()\n\
   <KeyPress>KP_3: MoveBr()\n\
   <KeyPress>R15: MoveBr()\n\
   <Btn1Down>: Select()\n\
   <Btn1Up>: Release()\n\
   <KeyPress>p: Practice()\n\
   <Btn2Down>(2+): Practice()\n\
   <Btn2Down>: PracticeMaybe()\n\
   <KeyPress>r: Randomize()\n\
   <Btn3Down>(2+): Randomize()\n\
   <Btn3Down>: RandomizeMaybe()\n\
   <KeyPress>g: Get()\n\
   <KeyPress>w: Write()\n\
   <KeyPress>u: Undo()\n\
   <KeyPress>s: Solve()\n\
   <KeyPress>i: Increment()\n\
   <KeyPress>d: Decrement()\n\
   <KeyPress>o: Orientize()\n\
   <KeyPress>3: Period3()\n\
   <KeyPress>4: Period4()\n\
   <KeyPress>b: Both()\n\
   <KeyPress>y: Sticky()";

static XtActionsRec actionsListOct[] =
{
	{"Quit", (XtActionProc) QuitOct},
	{"MoveCcw", (XtActionProc) MoveOctCcw},
	{"MoveTl", (XtActionProc) MoveOctTl},
	{"MoveTop", (XtActionProc) MoveOctTop},
	{"MoveTr", (XtActionProc) MoveOctTr},
	{"MoveLeft", (XtActionProc) MoveOctLeft},
	{"MoveCw", (XtActionProc) MoveOctCw},
	{"MoveRight", (XtActionProc) MoveOctRight},
	{"MoveBl", (XtActionProc) MoveOctBl},
	{"MoveBottom", (XtActionProc) MoveOctBottom},
	{"MoveBr", (XtActionProc) MoveOctBr},
	{"Select", (XtActionProc) SelectOct},
	{"Release", (XtActionProc) ReleaseOct},
	{"Practice", (XtActionProc) PracticeOct},
	{"PracticeMaybe", (XtActionProc) PracticeOctMaybe},
	{"Randomize", (XtActionProc) RandomizeOct},
	{"RandomizeMaybe", (XtActionProc) RandomizeOctMaybe},
	{"Get", (XtActionProc) GetOct},
	{"Write", (XtActionProc) WriteOct},
	{"Undo", (XtActionProc) UndoOct},
	{"Solve", (XtActionProc) SolveOct},
	{"Increment", (XtActionProc) IncrementOct},
	{"Decrement", (XtActionProc) DecrementOct},
	{"Orientize", (XtActionProc) OrientizeOct},
	{"Period3", (XtActionProc) Period3ModeOct},
	{"Period4", (XtActionProc) Period4ModeOct},
	{"Both", (XtActionProc) BothModeOct},
	{"Sticky", (XtActionProc) StickyModeOct}
};

static XtResource resourcesOct[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.username), XtRString, "nobody"},
  /* Beware color values are swapped */
	{XtNfaceColor0, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[0]), XtRString, "Red"},
	{XtNfaceColor1, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[1]), XtRString, "Blue"},
	{XtNfaceColor2, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[2]), XtRString, "White"},
	{XtNfaceColor3, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[3]), XtRString, "Magenta"},
	{XtNfaceColor4, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[4]), XtRString, "Orange"},
	{XtNfaceColor5, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[5]), XtRString, "Pink"},
	{XtNfaceColor6, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[6]), XtRString, "Green"},
	{XtNfaceColor7, XtCLabel, XtRString, sizeof (String),
	 XtOffset(OctWidget, oct.faceName[7]), XtRString, "Yellow"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
	 XtOffset(OctWidget, oct.foreground), XtRString, XtDefaultForeground},
	{XtNpieceBorder, XtCColor, XtRPixel, sizeof (Pixel),
       XtOffset(OctWidget, oct.borderColor), XtRString, XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(OctWidget, core.width), XtRString, "200"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(OctWidget, core.height), XtRString, "400"},
	{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
	 XtOffset(OctWidget, oct.mono), XtRString, "FALSE"},
	{XtNsize, XtCSize, XtRInt, sizeof (int),
	 XtOffset(OctWidget, oct.size), XtRString, "3"},	/*DEFAULTOCTAS */
	{XtNsticky, XtCSticky, XtRBoolean, sizeof (Boolean),
	 XtOffset(OctWidget, oct.sticky), XtRString, "FALSE"},
	{XtNmode, XtCMode, XtRInt, sizeof (int),
	 XtOffset(OctWidget, oct.mode), XtRString, "4"},	/*DEFAULTMODE */
	{XtNorient, XtCOrient, XtRBoolean, sizeof (Boolean),
	 XtOffset(OctWidget, oct.orient), XtRString, "FALSE"},	/*DEFAULTORIENT */
	{XtNpractice, XtCPractice, XtRBoolean, sizeof (Boolean),
	 XtOffset(OctWidget, oct.practice), XtRString, "FALSE"},	/*DEFAULTPRACTICE */
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(OctWidget, oct.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(OctWidget, oct.select), XtRCallback, NULL}
};

OctClassRec octClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Oct",		/* class name */
		sizeof (OctRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeOct,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListOct,	/* actions */
		XtNumber(actionsListOct),	/* num actions */
		resourcesOct,	/* resources */
		XtNumber(resourcesOct),		/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		(XtWidgetProc) DestroyOct,	/* destroy */
		(XtWidgetProc) ResizeOct,	/* resize */
		(XtExposeProc) ExposeOct,	/* expose */
		(XtSetValuesFunc) SetValuesOct,		/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		NULL,		/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsOct,		/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	}
};

WidgetClass octWidgetClass = (WidgetClass) & octClassRec;

typedef struct _RowNextP3 {
	int         viewChanged, face, direction;
} RowNextP3;
static OctLoc rowToRotate[MAXFACES][COORD] =
{
	{
		{IGNORE, IGNORE},
		{1, CW},
		{2, CW},
		{3, CW},
		{IGNORE, IGNORE},
		{1, CCW},
		{2, CCW},
		{3, CCW}},

	{
		{3, CCW},
		{0, CCW},
		{IGNORE, IGNORE},
		{2, CW},
		{3, CW},
		{0, CW},
		{IGNORE, IGNORE},
		{2, CCW}},

	{
		{IGNORE, IGNORE},
		{3, CCW},
		{0, CCW},
		{1, CCW},
		{IGNORE, IGNORE},
		{3, CW},
		{0, CW},
		{1, CW}},

	{
		{1, CW},
		{2, CW},
		{IGNORE, IGNORE},
		{0, CCW},
		{1, CCW},
		{2, CCW},
		{IGNORE, IGNORE},
		{0, CW}},

	{
		{IGNORE, IGNORE},
		{3, CCW},
		{2, CCW},
		{1, CCW},
		{IGNORE, IGNORE},
		{3, CW},
		{2, CW},
		{1, CW}},

	{
		{1, CW},
		{0, CW},
		{IGNORE, IGNORE},
		{2, CCW},
		{1, CCW},
		{0, CCW},
		{IGNORE, IGNORE},
		{2, CW}},

	{
		{IGNORE, IGNORE},
		{1, CW},
		{0, CW},
		{3, CW},
		{IGNORE, IGNORE},
		{1, CCW},
		{0, CCW},
		{3, CCW}},

	{
		{3, CCW},
		{2, CCW},
		{IGNORE, IGNORE},
		{0, CW},
		{3, CW},
		{2, CW},
		{IGNORE, IGNORE},
		{0, CCW}}
};
static RowNextP3 slideNextRowP3[MAXSIDES][COORD] =
{
	{
		{IGNORE, IGNORE, IGNORE},
		{TRUE, 2, TR},
		{FALSE, 1, BR},
		{FALSE, 1, BOTTOM},
		{IGNORE, IGNORE, IGNORE},
		{FALSE, 3, BOTTOM},
		{FALSE, 3, BL},
		{TRUE, 2, TL}
	},
	{
		{FALSE, 0, TL},
		{TRUE, 1, BL},
		{IGNORE, IGNORE, IGNORE},
		{TRUE, 1, TL},
		{FALSE, 2, BL},
		{FALSE, 2, LEFT},
		{IGNORE, IGNORE, IGNORE},
		{FALSE, 0, LEFT}
	},
	{
		{IGNORE, IGNORE, IGNORE},
		{FALSE, 1, TOP},
		{FALSE, 1, TR},
		{TRUE, 0, BR},
		{IGNORE, IGNORE, IGNORE},
		{TRUE, 0, BL},
		{FALSE, 3, TL},
		{FALSE, 3, TOP},
	},
	{
		{FALSE, 0, TR},
		{FALSE, 0, RIGHT},
		{IGNORE, IGNORE, IGNORE},
		{FALSE, 2, RIGHT},
		{FALSE, 2, BR},
		{TRUE, 3, TR},
		{IGNORE, IGNORE, IGNORE},
		{TRUE, 3, BR}
	}
};
static int  reverseP3[MAXSIDES][COORD] =
{
	{IGNORE, FALSE, FALSE, TRUE, IGNORE, TRUE, TRUE, TRUE},
	{TRUE, TRUE, IGNORE, FALSE, FALSE, TRUE, IGNORE, TRUE},
	{IGNORE, TRUE, TRUE, TRUE, IGNORE, FALSE, FALSE, TRUE},
	{FALSE, TRUE, IGNORE, TRUE, TRUE, TRUE, IGNORE, FALSE}
};
static int  rotateOrientP3[MAXSIDES][COORD] =
{
	{IGNORE, 11, 7, 2, IGNORE, 10, 5, 1},
	{11, 7, IGNORE, 5, 1, 2, IGNORE, 10},
	{IGNORE, 10, 5, 1, IGNORE, 11, 7, 2},
	{1, 2, IGNORE, 10, 11, 7, IGNORE, 5}
};

static XPoint triangleUnit[MAXSIDES][4] =
{
	{
		{0, 0},
		{1, 1},
		{-2, 0},
		{1, -1}},
	{
		{0, 0},
		{-1, 1},
		{0, -2},
		{1, 1}},
	{
		{0, 0},
		{-1, -1},
		{2, 0},
		{-1, 1}},
	{
		{0, 0},
		{1, -1},
		{0, 2},
		{-1, -1}}
};
static XPoint triangleList[MAXSIDES][4], letterList[MAXSIDES];

static void
InitializeOct(Widget request, Widget new)
{
	OctWidget   w = (OctWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         face, orient;

	for (face = 0; face < MAXFACES; face++)
		w->oct.octaLoc[face] = NULL;
	for (orient = 0; orient < MAXORIENT / 2; orient++)
		w->oct.rowLoc[orient] = NULL;
	w->oct.faceLoc = NULL;
	CheckPolyhedrons(w);
	InitMoves();
	ResetPolyhedrons(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	values.foreground = w->oct.foreground;
	w->oct.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->oct.borderColor;
	w->oct.borderGC = XtGetGC(new, valueMask, &values);
	w->oct.depth = DefaultDepthOfScreen(XtScreen(w));
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->oct.foreground;
	w->oct.inverseGC = XtGetGC(new, valueMask, &values);
	for (face = 0; face < MAXFACES; face++)
		GetColor(w, face, TRUE);
	ResizeOct(w);
}

static void
DestroyOct(Widget old)
{
	OctWidget   w = (OctWidget) old;
	int         face;

	for (face = 0; face < MAXFACES; face++)
		XtReleaseGC(old, w->oct.faceGC[face]);
	XtReleaseGC(old, w->oct.borderGC);
	XtReleaseGC(old, w->oct.puzzleGC);
	XtReleaseGC(old, w->oct.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->oct.select);
}

static void
ResizeOct(OctWidget w)
{
	int         tempLength;

	w->oct.delta = 4;
	w->oct.vertical = (w->core.height >= w->core.width);
	if (w->oct.vertical)
		tempLength = MIN(w->core.height / 2, w->core.width);
	else
		tempLength = MIN(w->core.height, w->core.width / 2);
	w->oct.octaLength = MAX((tempLength - w->oct.delta + 1) / w->oct.size, 0);
	w->oct.faceLength = w->oct.size * w->oct.octaLength;
	w->oct.viewLength = w->oct.faceLength + w->oct.delta + 3;
	w->oct.viewMiddle = w->oct.viewLength / 2;
	if (w->oct.vertical) {
		w->oct.puzzleSize.x = w->oct.viewLength - 1;
		w->oct.puzzleSize.y = 2 * w->oct.viewLength - w->oct.delta - 2;
	} else {
		w->oct.puzzleSize.x = 2 * w->oct.viewLength - w->oct.delta - 2;
		w->oct.puzzleSize.y = w->oct.viewLength - 1;
	}
	w->oct.puzzleOffset.x = ((int) w->core.width - w->oct.puzzleSize.x) / 2;
	w->oct.puzzleOffset.y = ((int) w->core.height - w->oct.puzzleSize.y) / 2;
	ResizePolyhedrons(w);
}

static void
ExposeOct(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	OctWidget   w = (OctWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->oct.puzzleGC);
		DrawAllPolyhedrons(w);
	}
}

static      Boolean
SetValuesOct(Widget current, Widget request, Widget new)
{
	OctWidget   c = (OctWidget) current, w = (OctWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	int         face;

	CheckPolyhedrons(w);
	if (w->oct.foreground != c->oct.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->oct.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->oct.puzzleGC);
		w->oct.puzzleGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->oct.foreground;
		XtReleaseGC(new, w->oct.inverseGC);
		w->oct.inverseGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->oct.borderColor != c->oct.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->oct.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->oct.borderGC);
		w->oct.borderGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->oct.mono || w->oct.depth == 1) {
		valueMask = GCForeground | GCBackground;
		values.background = w->core.background_pixel;
		values.foreground = w->oct.foreground;
		for (face = 0; face < MAXFACES; face++) {
			XtReleaseGC(new, w->oct.faceGC[face]);
			w->oct.faceGC[face] = XtGetGC(new, valueMask, &values);
		}
		redraw = TRUE;
	}
	for (face = 0; face < MAXFACES; face++) {
		if (strcmp(w->oct.faceName[face], c->oct.faceName[face]))
			GetColor(w, face, FALSE);
	}
	if (w->oct.orient != c->oct.orient) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	} else if (w->oct.practice != c->oct.practice) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	}
	if (w->oct.size != c->oct.size ||
	    w->oct.mode != c->oct.mode ||
	    w->oct.sticky != c->oct.sticky) {
		ResetPolyhedrons(w);
		ResizeOct(w);
		redraw = TRUE;
	}
	if (w->oct.octaLength != c->oct.octaLength) {
		ResizeOct(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
QuitOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	int         control;
	RTT         rtt;

	if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
			      &(w->oct.currentFace), &rtt)) {
		control = (int) (event->xkey.state & ControlMask);
		if (control || w->oct.practice || !CheckSolved(w)) {
			w->oct.currentPosition = ToPosition(rtt);
			DrawTriangle(w, w->oct.currentFace, w->oct.currentPosition, TRUE);
		}
	} else
		w->oct.currentFace = -1;
}

static void
ReleaseOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	int         shift, control, style, face, count = -1, direction = 0;
	RTT         rtt;
	octCallbackStruct cb;

	if (w->oct.currentFace == -1)
		return;
	DrawTriangle(w, w->oct.currentFace, w->oct.currentPosition, FALSE);
	shift = (int) (event->xbutton.state & (ShiftMask | LockMask));
	control = (int) (event->xkey.state & ControlMask);
	if (!control && !w->oct.practice && CheckSolved(w))
		MoveNoPolyhedrons(w);
	else if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
				   &face, &rtt)) {
		control = (control) ? 1 : 0;
		if (w->oct.mode != BOTH) {
			if (control && shift)
				style = (w->oct.mode == PERIOD4) ? PERIOD3 : PERIOD4;
			else
				style = (w->oct.mode == PERIOD3) ? PERIOD3 : PERIOD4;
		} else
			style = (shift) ? PERIOD4 : PERIOD3;
		if (face == w->oct.currentFace)
			count = CheckMoveDir(w, ToRTT(w->oct.currentPosition), rtt, face,
					     &direction);
		if (count == 1 && NarrowSelection(w, style, &face, &direction)) {
			MoveOct(w, face, w->oct.currentPosition, direction, style, control);
			if (!control && CheckSolved(w)) {
				cb.reason = OCT_SOLVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else if (count == 2) {
			cb.reason = OCT_AMBIGUOUS;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else if (!count)
			MoveNoPolyhedrons(w);
	}
}

static void
PracticeOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	PracticePolyhedrons(w);
}

static void
PracticeOctMaybe(OctWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->oct.started)
		PracticePolyhedrons(w);
}

static void
RandomizeOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizePolyhedrons(w);
}

static void
RandomizeOctMaybe(OctWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->oct.started)
		RandomizePolyhedrons(w);
}

static void
GetOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, size, mode, sticky, orient, practice, moves;
	octCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s to enter.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &size);
		if (size >= MINOCTAS) {
			for (i = w->oct.size; i < size; i++) {
				cb.reason = OCT_INC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->oct.size; i > size; i--) {
				cb.reason = OCT_DEC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: size %d should be between %d and MAXINT\n",
				      DATAFILE, size, MINOCTAS);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &mode);
		if (mode >= PERIOD3 && mode <= BOTH)
			switch (mode) {
				case PERIOD3:
					cb.reason = OCT_PERIOD3;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
					break;
				case PERIOD4:
					cb.reason = OCT_PERIOD4;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
					break;
				case BOTH:
					cb.reason = OCT_BOTH;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else
			(void) printf("%s corrupted: mode %d should be between %d and %d\n",
				      DATAFILE, mode, PERIOD3, BOTH);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &sticky);
		if (w->oct.sticky != (Boolean) sticky) {
			cb.reason = OCT_STICKY;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &orient);
		if (w->oct.orient != (Boolean) orient) {
			cb.reason = OCT_ORIENT;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &practice);
		if (w->oct.practice != (Boolean) practice) {
			cb.reason = OCT_PRACTICE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = OCT_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		SetStartPosition(w);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: size %d, mode %d, sticky %d, orient %d",
			      DATAFILE, size, mode, sticky, orient);
		(void) printf(", practice %d, moves %d.\n", practice, moves);
	}
}

static void
WriteOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "size%c %d\n", SYMBOL, w->oct.size);
		(void) fprintf(fp, "mode%c %d\n", SYMBOL, w->oct.mode);
		(void) fprintf(fp, "sticky%c %d\n", SYMBOL, (w->oct.sticky) ? 1 : 0);
		(void) fprintf(fp, "orient%c %d\n", SYMBOL, (w->oct.orient) ? 1 : 0);
		(void) fprintf(fp, "practice%c %d\n", SYMBOL, (w->oct.practice) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         face, position, direction, style, control;

		GetMove(&face, &position, &direction, &style, &control);
		direction = (direction < COORD) ?
			(direction + MAXSIDES) % MAXFACES : 3 * COORD - direction;
		if (control)
			MoveControlCb(w, face, direction, style);
		else {
			octCallbackStruct cb;

			MovePolyhedrons(w, face, ToRTT(position), direction, style);
			cb.reason = OCT_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolveOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolvePolyhedrons(w); *//* Sorry, unimplemented */
}

static void
IncrementOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	octCallbackStruct cb;

	cb.reason = OCT_INC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
DecrementOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	octCallbackStruct cb;

	if (w->oct.size <= MINOCTAS)
		return;
	cb.reason = OCT_DEC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
OrientizeOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	octCallbackStruct cb;

	cb.reason = OCT_ORIENT;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Period3ModeOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	octCallbackStruct cb;

	cb.reason = OCT_PERIOD3;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Period4ModeOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	octCallbackStruct cb;

	cb.reason = OCT_PERIOD4;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
BothModeOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	octCallbackStruct cb;

	cb.reason = OCT_BOTH;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
StickyModeOct(OctWidget w, XEvent * event, char **args, int nArgs)
{
	octCallbackStruct cb;

	cb.reason = OCT_STICKY;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MoveOctCcw(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, CCW,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xbutton.state & ControlMask));
}

static void
MoveOctTl(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, TL,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctTop(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, TOP,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctTr(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, TR,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctLeft(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, LEFT,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctCw(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, CW,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctRight(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, RIGHT,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctBl(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, BL,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctBottom(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, BOTTOM,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctBr(OctWidget w, XEvent * event, char **args, int nArgs)
{
	MoveOctInput(w, event->xbutton.x, event->xbutton.y, BR,
		     (int) (event->xbutton.state & (ShiftMask | LockMask)),
		     (int) (event->xkey.state & ControlMask));
}

static void
MoveOctInput(OctWidget w, int x, int y, int direction, int shift, int control)
{
	int         style, face;
	RTT         rtt;

	if (w->oct.mode != BOTH) {
		if (control && shift)
			style = (w->oct.mode == PERIOD4) ? PERIOD3 : PERIOD4;
		else
			style = (w->oct.mode == PERIOD3) ? PERIOD3 : PERIOD4;
	} else
		style = (shift) ? PERIOD4 : PERIOD3;
	if (!w->oct.practice && !control && CheckSolved(w)) {
		MoveNoPolyhedrons(w);
		return;
	}
	if (!PositionPolyhedrons(w, x, y, style, &face, &rtt, &direction))
		return;
	control = (control) ? 1 : 0;
	MoveOct(w, face, ToPosition(rtt), direction, style, control);
	if (!control && CheckSolved(w)) {
		octCallbackStruct cb;

		cb.reason = OCT_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

void
MoveOct(OctWidget w, int face, int position, int direction, int style, int control)
{
	if (control)
		MoveControlCb(w, face, direction, style);
	else {
		octCallbackStruct cb;

		MovePolyhedrons(w, face, ToRTT(position), direction, style);
		cb.reason = OCT_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	PutMove(face, position, direction, style, control);
}

static void
GetColor(OctWidget w, int face, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->oct.depth > 1 && !w->oct.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
				  w->oct.faceName[face], &colorCell, &rgb)) {
			values.foreground = w->oct.faceColor[face] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->oct.faceGC[face]);
			w->oct.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined",
				       w->oct.faceName[face]);
			XtWarning(buf);
		}
	}
	values.foreground = w->oct.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->oct.faceGC[face]);
	w->oct.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
}

static void
MoveControlCb(OctWidget w, int face, int direction, int style)
{
	octCallbackStruct cb;
	int         i, j;
	RTT         rtt;

	if (w->oct.sticky) {
		if (style == PERIOD3)
			for (i = 0; i < 3; i++) {
				if (i == 2)
					i++;
				rtt.row = i;
				rtt.trbl = i;
				rtt.tlbr = i;
				MovePolyhedrons(w, face, rtt, direction, style);
				cb.reason = OCT_CONTROL;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else		/* (style == PERIOD4) */
			for (i = 0; i < 3; i++) {
				if (i < 2) {
					if (i == 1)
						j = i + 2;
					else
						j = i;
					rtt.row = j;
					rtt.trbl = j;
					rtt.tlbr = j;
					MovePolyhedrons(w, face, rtt, direction, style);
				} else {
					if (direction == CW || direction == CCW) {
						j = i - 2;
						rtt.row = j;
						rtt.trbl = j;
						rtt.tlbr = j;
						MovePolyhedrons(w,
								(!(face / MAXSIDES)) * MAXSIDES + ((face % 2) ?
												   (face + MAXSIDES / 2) % MAXSIDES : face % MAXSIDES), rtt,
								(direction == CW) ? CCW : CW, style);
					} else {
						j = i + 1;
						rtt.row = j;
						rtt.trbl = j;
						rtt.tlbr = j;
						MovePolyhedrons(w,
								(!(face / MAXSIDES)) * MAXSIDES + ((face % 2) ?
												   (face + MAXSIDES / 2) % MAXSIDES : face % MAXSIDES), rtt,
								((direction / 2) % 2) ? (direction + 2) % MAXFACES :
								(direction + 6) % MAXFACES, style);
					}
				}
				cb.reason = OCT_CONTROL;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
	} else {
		if (style == PERIOD3)
			for (i = 0; i < w->oct.size; i++) {
				rtt.row = i;
				rtt.trbl = i;
				rtt.tlbr = i;
				MovePolyhedrons(w, face, rtt, direction, style);
				cb.reason = OCT_CONTROL;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else		/* (style == PERIOD4) */
			for (i = 0; i < 2 * w->oct.size - 1; i++) {
				if (i < w->oct.size) {
					rtt.row = i;
					rtt.trbl = i;
					rtt.tlbr = i;
					MovePolyhedrons(w, face, rtt, direction, style);
				} else {
					if (direction == CW || direction == CCW) {
						j = i - w->oct.size;
						rtt.row = j;
						rtt.trbl = j;
						rtt.tlbr = j;
						MovePolyhedrons(w,
								(!(face / MAXSIDES)) * MAXSIDES + ((face % 2) ?
												   (face + MAXSIDES / 2) % MAXSIDES : face % MAXSIDES), rtt,
								(direction == CW) ? CCW : CW, style);
					} else {
						j = i - w->oct.size + 1;
						rtt.row = j;
						rtt.trbl = j;
						rtt.tlbr = j;
						MovePolyhedrons(w,
								(!(face / MAXSIDES)) * MAXSIDES + ((face % 2) ?
												   (face + MAXSIDES / 2) % MAXSIDES : face % MAXSIDES), rtt,
								((direction / 2) % 2) ? (direction + 2) % MAXFACES :
								(direction + 6) % MAXFACES, style);
					}
				}
				cb.reason = OCT_CONTROL;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
	}
}

static void
CheckPolyhedrons(OctWidget w)
{
	if (w->oct.size < MINOCTAS) {
		char        buf[121];

		(void) sprintf(buf, "Number of Octas on edge out of bounds, use %d..MAXINT",
			       MINOCTAS);
		XtWarning(buf);
		w->oct.size = DEFAULTOCTAS;
	}
	if (w->oct.mode < PERIOD3 || w->oct.mode > BOTH) {
		XtWarning("Mode is in error, use 3 for Period3, 4 for Period4, 5 for Both");
		w->oct.mode = DEFAULTMODE;
	}
}

static void
ResetPolyhedrons(OctWidget w)
{
	int         face, position, orient;

	w->oct.sizeSize = w->oct.size * w->oct.size;
	for (face = 0; face < MAXFACES; face++) {
		if (w->oct.octaLoc[face])
			(void) free((void *) w->oct.octaLoc[face]);
		if (!(w->oct.octaLoc[face] = (OctLoc *)
		      malloc(sizeof (OctLoc) * w->oct.sizeSize)))
			XtError("Not enough memory, exiting.");
		if (startLoc[face])
			(void) free((void *) startLoc[face]);
		if (!(startLoc[face] = (OctLoc *)
		      malloc(sizeof (OctLoc) * w->oct.sizeSize)))
			XtError("Not enough memory, exiting.");
	}
	for (orient = 0; orient < MAXORIENT / 2; orient++) {
		if (w->oct.rowLoc[orient])
			(void) free((void *) w->oct.rowLoc[orient]);
		if (!(w->oct.rowLoc[orient] = (OctLoc *)
		      malloc(sizeof (OctLoc) * (2 * w->oct.size - 1))))
			XtError("Not enough memory, exiting.");
	}
	if (w->oct.faceLoc)
		(void) free((void *) w->oct.faceLoc);
	if (!(w->oct.faceLoc = (OctLoc *)
	      malloc(sizeof (OctLoc) * w->oct.sizeSize)))
		XtError("Not enough memory, exiting.");
	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->oct.sizeSize; position++) {
			w->oct.octaLoc[face][position].face = face;
			w->oct.octaLoc[face][position].rotation = 3 * face % MAXORIENT;
		}
	FlushMoves(w);
	w->oct.started = FALSE;
}

static void
ResizePolyhedrons(OctWidget w)
{
	int         i, j;

	w->oct.octaLength = w->oct.faceLength / (2 * w->oct.size) -
		w->oct.delta - 1;
	for (i = 0; i <= 3; i++)
		for (j = 0; j < MAXSIDES; j++) {
			triangleList[j][i].x = triangleUnit[j][i].x * w->oct.octaLength;
			triangleList[j][i].y = triangleUnit[j][i].y * w->oct.octaLength;
		}
	letterList[TOP / 2].x = -2;
	letterList[TOP / 2].y = 3 * w->oct.octaLength / 5 + 3;
	letterList[RIGHT / 2].x = -3 * w->oct.octaLength / 5 - 4;
	letterList[RIGHT / 2].y = 3;
	letterList[BOTTOM / 2].x = -2;
	letterList[BOTTOM / 2].y = -3 * w->oct.octaLength / 5 + 2;
	letterList[LEFT / 2].x = 3 * w->oct.octaLength / 5 - 1;
	letterList[LEFT / 2].y = 3;
	w->oct.orientLineLength = w->oct.octaLength / 4;
}

static int
SelectPolyhedrons(OctWidget w, int x, int y, int *face, RTT * rtt)
{
	int         view;

	x -= w->oct.puzzleOffset.x;
	y -= w->oct.puzzleOffset.y;
	if (w->oct.vertical && y > w->oct.viewLength - 1) {
		y -= (w->oct.viewLength - 1);
		view = DOWN;
	} else if (!w->oct.vertical && x > w->oct.viewLength - 1) {
		x -= (w->oct.viewLength - 1);
		view = DOWN;
	} else
		view = UP;
	if (x <= 0 || y <= 0 ||
	    x >= w->oct.faceLength + w->oct.delta ||
	    y >= w->oct.faceLength + w->oct.delta)
		return FALSE;
	else if (x + y > w->oct.faceLength) {
		if (x > y)
			*face = 1;
		else if (x < y)
			*face = 2;
		else
			return FALSE;
	} else {
		if (x > y)
			*face = 0;
		else if (x < y)
			*face = 3;
		else
			return FALSE;
	}
	rtt->row = 0;
	while ((x <= (w->oct.size - (rtt->row + 1)) *
		(w->oct.octaLength + w->oct.delta) ||
		x >= w->oct.viewMiddle + (rtt->row + 1) *
		(w->oct.octaLength + w->oct.delta) + 1 ||
		y <= (w->oct.size - (rtt->row + 1)) *
		(w->oct.octaLength + w->oct.delta) ||
		y >= w->oct.viewMiddle + (rtt->row + 1) *
		(w->oct.octaLength + w->oct.delta) + 1) &&
	       rtt->row < w->oct.size)
		rtt->row++;
	rtt->trbl = 0;
	while ((x + y) / 2 <= (w->oct.size - (rtt->trbl + 1)) *
	       (w->oct.octaLength + w->oct.delta) + 2 * w->oct.delta ||
	       (x + y) / 2 >= w->oct.viewMiddle + (rtt->trbl + 1) *
	       (w->oct.octaLength + w->oct.delta))
		rtt->trbl++;
	rtt->tlbr = 0;
	while (x <= y - 2 * (rtt->tlbr + 1) *
	       (w->oct.octaLength + w->oct.delta) - 2 ||
	       y <= x - 2 * (rtt->tlbr + 1) *
	       (w->oct.octaLength + w->oct.delta) - 2)
		rtt->tlbr++;
	if (!w->oct.vertical && view == DOWN)
		*face = (*face + MAXSIDES / 2) % MAXSIDES;
	*face += MAXSIDES * view;
	if (*face % 2) {
		view = rtt->tlbr;
		rtt->tlbr = rtt->trbl;
		rtt->trbl = view;
	}
	return TRUE;
}

static int
NarrowSelection(OctWidget w, int style, int *face, int *direction)
{
	int         side, view;

	side = *face % MAXSIDES;
	view = *face / MAXSIDES;
	if (!w->oct.vertical && view == DOWN)
		*direction = (*direction + MAXSIDES) % MAXFACES;
	if (!(*direction % 2) && !((*direction / 2 + side) % 2))
		return FALSE;
	if (style == PERIOD4) {
		if (!(*direction % 2)) {
			if (*direction == (2 * side + 2) % MAXFACES)
				*direction = CW;
			else	/* *direction == (2 * side + 6) % MAXFACES */
				*direction = CCW;
		}
/* if (*direction > TL) h = rtt->row; else if (!((*direction / 2) % 2)) h =
		   rtt->trbl; else *//* (*direction / 2) % 2 == 1 *//*
		   h = rtt->tlbr; */
	}
	return TRUE;
}

static int
PositionPolyhedrons(OctWidget w, int x, int y, int style, int *face, RTT * rtt, int *direction)
{
	if (!SelectPolyhedrons(w, x, y, face, rtt))
		return FALSE;
	return NarrowSelection(w, style, face, direction);
}

static void
MoveNoPolyhedrons(OctWidget w)
{
	octCallbackStruct cb;

	cb.reason = OCT_ILLEGAL;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
PracticePolyhedrons(OctWidget w)
{
	octCallbackStruct cb;

	cb.reason = OCT_PRACTICE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
RandomizePolyhedrons(OctWidget w)
{
	octCallbackStruct cb;
	int         randomDirection, face, position, style;
	int         big = w->oct.sizeSize * 3 + NRAND(2);

	if (big > 1000)
		big = 1000;
	if (w->oct.practice)
		PracticePolyhedrons(w);
	if (w->oct.sticky)
		big /= 3;
	cb.reason = OCT_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
	big = 3;
#endif

	while (big--) {
		face = NRAND(MAXFACES);
		if (w->oct.mode == BOTH)
			style = NRAND(MAXMODES - 1) + PERIOD3;
		else
			style = w->oct.mode;
		if (w->oct.sticky) {
			if (style == PERIOD3) {
				position = 6;
				randomDirection = NRAND(6);
				if (randomDirection >= MAXSIDES) {
					if (randomDirection == 4)
						randomDirection = CW;
					else if (randomDirection == 5)
						randomDirection = CCW;
				} else
					randomDirection = randomDirection * 2 + 1;
			} else {	/* style == PERIOD4 */
				if (NRAND(2))	/* a point */
					position = 9;
				else	/* the center */
					position = 6;
				randomDirection = NRAND(6);
				if (randomDirection == 4)
					randomDirection = CW;
				else if (randomDirection == 5)
					randomDirection = CCW;
				else {
					randomDirection = randomDirection * 2 + 1;
					position = 0;
				}
			}
		} else {	/* (!w->oct.sticky) */
			if (style == PERIOD3) {
				randomDirection = NRAND(MAXORIENT / 2);
				if (randomDirection >= MAXSIDES) {
					if (randomDirection == 4)
						if (face % 2)
							randomDirection = BOTTOM;
						else
							randomDirection = RIGHT;
					else if (randomDirection == 5)
						if (face % 2)
							randomDirection = TOP;
						else
							randomDirection = LEFT;
				} else
					randomDirection = randomDirection * 2 + 1;;
			} else {	/* style == PERIOD4 */
				randomDirection = NRAND(MAXORIENT / 2);
				if (randomDirection == 4)
					randomDirection = CW;
				else if (randomDirection == 5)
					randomDirection = CCW;
				else
					randomDirection = randomDirection * 2 + 1;
			}
			position = NRAND(w->oct.sizeSize);
		}
		MoveOct(w, face, position, randomDirection, style, FALSE);
		cb.reason = OCT_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	FlushMoves(w);
	cb.reason = OCT_RANDOMIZE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (CheckSolved(w)) {
		cb.reason = OCT_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MovePolyhedrons(OctWidget w, int face, RTT rtt, int direction, int style)
{
	int         view, side, orient, newView, rotate, g, h, len;
	int         newSide, oldSide, newDirection, bound, l = 0;

	view = face / MAXSIDES;
	side = face % MAXSIDES;
	if (style == PERIOD3) {
		if (direction == CW || direction == CCW) {	/* Remap to row movement */
			side = (side + 2) % MAXSIDES;
			direction = ((side + direction) % MAXSIDES) * 2;
			face = side + view * MAXSIDES;
			rtt.row = rtt.trbl = rtt.tlbr = 0;
		}
		if ((!rtt.row && !(direction % 2)) ||
		    (!rtt.trbl && (direction % 2) && !((side + direction / 2) % 2)) ||
		    (!rtt.tlbr && (direction % 2) && ((side + direction / 2) % 2)))
			RotateFace(w, view, rowToRotate[side][direction].face,
				   rowToRotate[side][direction].rotation);
		if ((rtt.row == w->oct.size - 1 && !(direction % 2)) ||
		    (rtt.trbl == w->oct.size - 1 && (direction % 2) &&
		     !((side + direction / 2) % 2)) ||
		    (rtt.tlbr == w->oct.size - 1 && (direction % 2) &&
		     (side + direction / 2) % 2))
			RotateFace(w, !view, rowToRotate[MAXSIDES + side][direction].face,
			   rowToRotate[MAXSIDES + side][direction].rotation);
		if (!(direction % 2))
			h = rtt.row;
		else if ((direction / 2 + side) % 2)
			h = rtt.tlbr;
		else		/* (!((direction / 2 + side) % 2)) */
			h = rtt.trbl;
		if (w->oct.sticky && (h == 1 || h == 2)) {
			l = 0;
			bound = TRUE;
			h = 1;
		} else
			bound = FALSE;
		newView = view;
		newSide = side;
		newDirection = direction;
		do {
			len = Length(w, direction, h);
			rotate = rotateOrientP3[side][direction];
			ReadRTT(w, view, side, direction, h, len, 0);
			if (reverseP3[side][direction])
				ReverseRTT(w, len, 0);
			RotateRTT(w, rotate, len, 0);
			for (orient = 1; orient < 8; orient++) {
				if (slideNextRowP3[side][direction].viewChanged) {
					view = !view;
					h = w->oct.size - 1 - h;
				}
				oldSide = slideNextRowP3[side][direction].face;
				direction = slideNextRowP3[side][direction].direction;
				side = oldSide;
				len = Length(w, direction, h);
				rotate = rotateOrientP3[side][direction];
				if (orient < 6) {
					ReadRTT(w, view, side, direction, h, len, orient);
					if (reverseP3[side][direction])
						ReverseRTT(w, len, orient);
					RotateRTT(w, rotate, len, orient);
				}
				if (orient >= 2)
					WriteRTT(w, view, side, direction, h, len, orient - 2);
			}
			l++;
			h = 2;
			view = newView;
			side = newSide;
			direction = newDirection;
		} while (bound && l < 2);
	} else {		/* style == PERIOD4 */
		if (direction > TL)
			h = rtt.row;
		else if ((direction / 2 + side) % 2)
			h = rtt.tlbr;
		else		/* (!((direction / 2 + side) % 2)) */
			h = rtt.trbl;

		if (w->oct.sticky &&
		    !((direction > TL && h == w->oct.size - 1) ||
		      (direction <= TL && !h))) {
			l = 0;
			h = (direction <= TL);
			bound = TRUE;
		} else
			bound = FALSE;
		g = 0;
		do {		/* In case this is on an edge */
			len = Length(w, direction, h);
			if (g == 1) {
				if (direction > TL) {
					direction = (direction == CW) ? CCW : CW;
					view = !view;
				} else
					side = (side + 2) % MAXSIDES;
			}
			ReadRTT(w, view, side, direction, h, len, 0);
			for (orient = 1; orient <= 4; orient++) {
				if (direction <= TL) {
					if ((side - direction / 2 + COORD) % MAXSIDES < 2) {
						newView = !view;
						newSide = (!(side % 2)) ? (side + 2) % MAXSIDES : side;
						newDirection = (!((direction / 2) % 2)) ?
							((direction + 6) % MAXFACES) : ((direction + 2) % MAXFACES);
						if (!(side % 2))
							rotate = (((newDirection - direction) / 2 + MAXSIDES) %
								  MAXSIDES == 1) ? 2 : MAXORIENT - 2;
						else
							rotate = (((newDirection - direction) / 2 + MAXSIDES) %
								  MAXSIDES == 1) ? 4 : MAXORIENT - 4;
					} else {	/* Next is on same view */
						newView = view;
						newSide = MAXSIDES - side - 1;
						if ((direction / 2) % 2 == 1)
							newSide = (newSide + 2) % MAXSIDES;
						newDirection = direction;
						rotate = ((side - newSide + MAXSIDES) % MAXSIDES == 1) ?
							1 : MAXORIENT - 1;
					}
				} else {	/* direction == CW || direction == CCW */
					newView = view;
					newSide = (side + direction) % MAXSIDES;
					newDirection = direction;
					rotate = 3 * newDirection;
				}
				if (orient != 4)
					ReadRTT(w, newView, newSide, newDirection, h, len, orient);
				RotateRTT(w, rotate, len, orient - 1);
				if (direction <= TL)
					ReverseRTT(w, len, orient - 1);
				WriteRTT(w, newView, newSide, newDirection, h, len, orient - 1);
				view = newView;
				side = newSide;
				direction = newDirection;
			}
			l++;
			if (w->oct.sticky &&
			    !((direction > TL && h == w->oct.size - 1) ||
			      (direction <= TL && !h)))
				h++;
			else
				g++;
		} while ((bound && l < w->oct.size - 1) ||
			 (((direction > TL && h == w->oct.size - 1) ||
			   (direction <= TL && !h)) && g < 2 && !bound));
	}
}

static void
RotateFace(OctWidget w, int view, int side, int direction)
{
	int         g, square, s;
	int         i = 0, j = 0, k, i1, j1, k1, position;

	/* Read Face */
	k = -1;
	square = 0;
	for (g = 0; g < w->oct.sizeSize; g++) {
		/* This is the old algorithm, its now more efficient
		   k = Sqrt(g);
		   j = (g - k * k) / 2;
		   i = ((k + 1) * (k + 1) - g - 1) / 2; */
		if (square <= g) {
			k++;
			square = (k + 1) * (k + 1);
			j = -1;
			i = k;
		}
		if (!((square - g) % 2))
			i--;
		else
			j++;
		if (direction == CW) {
			k1 = w->oct.size - 1 - i;
			i1 = j;
			j1 = w->oct.size - 1 - k;
		} else {	/* (direction == CCW) */
			k1 = w->oct.size - 1 - j;
			j1 = i;
			i1 = w->oct.size - 1 - k;
		}
		position = k1 * k1 + 2 * j1 + (j1 != k1 - i1);
		w->oct.faceLoc[position] = w->oct.octaLoc[view * MAXSIDES + side][g];
	}
	/* Write Face */
	square = 1;
	s = 0;
	for (g = 0; g < w->oct.sizeSize; g++) {
		w->oct.octaLoc[view * MAXSIDES + side][g] = w->oct.faceLoc[g];
		w->oct.octaLoc[view * MAXSIDES + side][g].rotation = (direction == CW) ?
			(w->oct.octaLoc[view * MAXSIDES + side][g].rotation + 4) % MAXORIENT :
			(w->oct.octaLoc[view * MAXSIDES + side][g].rotation + 8) % MAXORIENT;
		DrawTriangle(w, view * MAXSIDES + side, g, FALSE);
		s = !s;
		if (g == square * square - 1) {
			s = 0;
			++square;
		}
	}
}

static void
ReadRTT(OctWidget w, int view, int side, int dir, int h, int len, int orient)
{
	int         f, g, s;
	int         base = h * h;

	if (!(dir % 2) || dir > COORD)
		for (g = 0; g < len; g++)
			w->oct.rowLoc[orient][g] =
				w->oct.octaLoc[view * MAXSIDES + side][base + g];
	else if ((dir / 2 + side) % 2) {
		f = -1;
		for (g = 0; g < len; g++) {
			s = g % 2;
			w->oct.rowLoc[orient][g] =
				w->oct.octaLoc[view * MAXSIDES + side][base + f + !s];
			if (s == SAME)
				f += g + 2 * (h + 1) + 1;
		}
	} else {
		base += 2 * h;
		f = 1;
		for (g = 0; g < len; g++) {
			s = g % 2;
			w->oct.rowLoc[orient][g] =
				w->oct.octaLoc[view * MAXSIDES + side][base + f - !s];
			if (s == SAME)
				f += g + 2 * h + 1;
		}
	}
}

static void
RotateRTT(OctWidget w, int rotate, int len, int orient)
{
	int         g;

	for (g = 0; g < len; g++)
		w->oct.rowLoc[orient][g].rotation =
			(w->oct.rowLoc[orient][g].rotation + rotate) % MAXORIENT;
}

static void
ReverseRTT(OctWidget w, int len, int orient)
{
	int         g;
	OctLoc      temp;

	for (g = 0; g < (len - 1) / 2; g++) {
		temp = w->oct.rowLoc[orient][len - 1 - g];
		w->oct.rowLoc[orient][len - 1 - g] = w->oct.rowLoc[orient][g];
		w->oct.rowLoc[orient][g] = temp;
	}
}

static void
WriteRTT(OctWidget w, int view, int side, int dir, int h, int len, int orient)
{
	int         f, g, s;
	int         base = h * h;

	if (!(dir % 2) || dir > COORD) {	/* CW || CCW */
		for (g = 0; g < len; g++) {
			s = g % 2;
			w->oct.octaLoc[view * MAXSIDES + side][base + g] =
				w->oct.rowLoc[orient][g];
			DrawTriangle(w, view * MAXSIDES + side, base + g, FALSE);
		}
	} else if ((dir / 2 + side) % 2) {
		f = -1;
		for (g = 0; g < len; g++) {
			s = g % 2;
			w->oct.octaLoc[view * MAXSIDES + side][base + f + !s] =
				w->oct.rowLoc[orient][g];
			DrawTriangle(w, view * MAXSIDES + side, base + f + !s, FALSE);
			if (s == SAME)
				f += g + 2 * (h + 1) + 1;
		}
	} else {
		base += 2 * h;
		f = 1;
		for (g = 0; g < len; g++) {
			s = g % 2;
			w->oct.octaLoc[view * MAXSIDES + side][base + f - !s] =
				w->oct.rowLoc[orient][g];
			DrawTriangle(w, view * MAXSIDES + side, base + f - !s, FALSE);
			if (s == SAME)
				f += g + 2 * h + 1;
		}
	}
}

static void
DrawFrame(OctWidget w, GC gc)
{
	int         startx, starty, lengthx, lengthy, longlength;

	startx = 1 + w->oct.puzzleOffset.x;
	starty = 1 + w->oct.puzzleOffset.y;
	lengthx = w->oct.viewLength - w->oct.delta + w->oct.puzzleOffset.x;
	lengthy = w->oct.viewLength - w->oct.delta + w->oct.puzzleOffset.y;
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, starty, lengthx, starty);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, starty, startx, lengthy);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, lengthx, starty, lengthx, lengthy);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, lengthy, lengthx, lengthy);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, lengthy, lengthx, starty);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, starty, lengthx, lengthy);
	if (w->oct.vertical) {
		longlength = 2 * w->oct.viewLength - 2 * w->oct.delta - 1 +
			w->oct.puzzleOffset.y;
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx, lengthy, startx, longlength);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  lengthx, lengthy, lengthx, longlength);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx, longlength, lengthx, longlength);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx, longlength, lengthx, lengthy);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx, lengthy, lengthx, longlength);
	} else {
		longlength = 2 * w->oct.viewLength - 2 * w->oct.delta - 1 +
			w->oct.puzzleOffset.x;
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  lengthx, starty, longlength, starty);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  lengthx, lengthy, longlength, lengthy);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  longlength, starty, longlength, lengthy);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  longlength, starty, lengthx, lengthy);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  lengthx, starty, longlength, lengthy);
	}
}

void
DrawAllPolyhedrons(OctWidget w)
{
	int         position, face;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->oct.sizeSize; position++)
			DrawTriangle(w, face, position, FALSE);
}

static void
DrawTriangle(OctWidget w, int face, int position, int offset)
{
	int         dx = 0, dy = 0;
	int         side, row = Sqrt(position);
	int         base = row * row;
	int         g = position - base;
	int         s = g % 2;
	int         pos = row * (w->oct.octaLength + w->oct.delta);
	int         orient, view, faceOnView;

	view = face / MAXSIDES;
	faceOnView = face - view * MAXSIDES;
	orient = (!w->oct.vertical && view == DOWN) ?
		(faceOnView + MAXSIDES / 2) % MAXSIDES : faceOnView;
	switch (orient) {
		case 0:
			dy = w->oct.viewMiddle - w->oct.delta - 2 - pos;
			dx = w->oct.viewMiddle + pos - 1 - g * (w->oct.octaLength + w->oct.delta);
			break;
		case 1:
			dx = w->oct.viewMiddle + w->oct.delta + pos;
			dy = w->oct.viewMiddle + pos - 1 - g * (w->oct.octaLength + w->oct.delta);
			break;
		case 2:
			dy = w->oct.viewMiddle + w->oct.delta + pos;
			dx = w->oct.viewMiddle - pos - 1 + g * (w->oct.octaLength + w->oct.delta);
			break;
		case 3:
			dx = w->oct.viewMiddle - w->oct.delta - 2 - pos;
			dy = w->oct.viewMiddle - pos - 1 + g * (w->oct.octaLength + w->oct.delta);
			break;
		default:
			(void) printf("DrawTriangle: orient %d\n", orient);
	}
	if (faceOnView % 2)
		side = ((faceOnView == 1) ? !s : s) * 2 + 1;
	else			/* faceOnView == (RIGHT / 2) || faceOnView == (LEFT / 2) */
		side = ((!faceOnView) ? !s : s) * 2;
	side = (!w->oct.vertical && view == DOWN) ?
		(side + MAXSIDES / 2) % MAXSIDES : side;
	if (s == OPPOSITE)
		switch (side) {
			case 0:
				dy -= w->oct.octaLength;
				break;
			case 1:
				dx += w->oct.octaLength;
				break;
			case 2:
				dy += w->oct.octaLength;
				break;
			case 3:
				dx -= w->oct.octaLength;
				break;
			default:
				(void) printf("DrawTriangle: side %d\n", side);
		}
	dx += w->oct.puzzleOffset.x;
	dy += w->oct.puzzleOffset.y;
	if (view == DOWN) {
		if (w->oct.vertical)
			dy += w->oct.viewLength - w->oct.delta - 1;
		else
			dx += w->oct.viewLength - w->oct.delta - 1;
	}
	triangleList[side][0].x = dx;
	triangleList[side][0].y = dy;
	if (offset) {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			     w->oct.borderGC,
			   triangleList[side], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			   w->oct.faceGC[w->oct.octaLoc[face][position].face],
			   triangleList[side], 4, CoordModePrevious);
	} else {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			  w->oct.faceGC[w->oct.octaLoc[face][position].face],
			   triangleList[side], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
		  w->oct.borderGC, triangleList[side], 4, CoordModePrevious);
	}
	if (w->oct.depth == 1 || w->oct.mono) {
		int         letterX, letterY;
		char        buf[2];

		(void) sprintf(buf, "%c", w->oct.faceName[w->oct.octaLoc
						  [face][position].face][0]);
		letterX = dx + letterList[side].x;
		letterY = dy + letterList[side].y;
		XDrawString(XtDisplay(w), XtWindow(w), w->oct.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->oct.orient)
		DrawOrientLine(w, w->oct.octaLoc[face][position].rotation, faceOnView,
			       side, dx, dy);
}

static void
DrawOrientLine(OctWidget w, int orient, int face, int side, int dx, int dy)
{
	int         x1 = 0, x2 = 0, y1 = 0, y2 = 0;
	int         temp1 = w->oct.octaLength + 1;
	int         temp2 = w->oct.octaLength / 2 + 1;
	int         temp3 = w->oct.orientLineLength / 3;

	/* clock positions */
	switch ((side == face) ? orient : (orient + MAXORIENT / 2) % MAXORIENT) {
		case 0:
			x2 = x1 = dx;
			y1 = dy + temp1;
			y2 = y1 - w->oct.orientLineLength;
			break;
		case 1:
			x1 = dx + temp2;
			y1 = dy + temp2;
			x2 = x1 + temp3;
			y2 = y1 - w->oct.orientLineLength;
			break;
		case 2:
			x1 = dx - temp2;
			y1 = dy - temp2;
			x2 = x1 + w->oct.orientLineLength;
			y2 = y1 - temp3;
			break;
		case 3:
			x1 = dx - temp1;
			x2 = x1 + w->oct.orientLineLength;
			y2 = y1 = dy;
			break;
		case 4:
			x1 = dx - temp2;
			y1 = dy + temp2;
			x2 = x1 + w->oct.orientLineLength;
			y2 = y1 + temp3;
			break;
		case 5:
			x1 = dx + temp2;
			y1 = dy - temp2;
			x2 = x1 + temp3;
			y2 = y1 + w->oct.orientLineLength;
			break;
		case 6:
			x2 = x1 = dx;
			y1 = dy - temp1;
			y2 = y1 + w->oct.orientLineLength;
			break;
		case 7:
			x1 = dx - temp2;
			y1 = dy - temp2;
			x2 = x1 - temp3;
			y2 = y1 + w->oct.orientLineLength;
			break;
		case 8:
			x1 = dx + temp2;
			y1 = dy + temp2;
			x2 = x1 - w->oct.orientLineLength;
			y2 = y1 + temp3;
			break;
		case 9:
			x1 = dx + temp1;
			x2 = x1 - w->oct.orientLineLength;
			y2 = y1 = dy;
			break;
		case 10:
			x1 = dx + temp2;
			y1 = dy - temp2;
			x2 = x1 - w->oct.orientLineLength;
			y2 = y1 - temp3;
			break;
		case 11:
			x1 = dx - temp2;
			y1 = dy + temp2;
			x2 = x1 - temp3;
			y2 = y1 - w->oct.orientLineLength;
			break;
		default:
			(void) printf("DrawOrientLine: orient %d\n", orient);
	}
	XDrawLine(XtDisplay(w), XtWindow(w), w->oct.inverseGC, x1, y1, x2, y2);
}

Boolean
CheckSolved(OctWidget w)
{
	int         face, position;
	OctLoc      test;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->oct.sizeSize; position++) {
			if (!position) {
				test.face = w->oct.octaLoc[face][position].face;
				test.rotation = w->oct.octaLoc[face][position].rotation;
			} else if (test.face !=		/*MAXSIDES * view + face */
				   w->oct.octaLoc[face][position].face ||
				   (w->oct.orient && test.rotation !=
				    w->oct.octaLoc[face][position].rotation))
				return FALSE;
		}
	return TRUE;
}

static int
Length(OctWidget w, int dir, int h)
{
	if (!(dir % 2) || dir > COORD)
		return (2 * h + 1);
	else
		return (2 * (w->oct.size - h) - 1);
}

static int
CheckMoveDir(OctWidget w, RTT rtt1, RTT rtt2, int face, int *direction)
{
	int         which = -1, count = 0;
	int         i, *p1, *p2;

	p1 = &(rtt1.row);
	p2 = &(rtt2.row);
	for (i = 0; i < 3; i++, p1++, p2++)
		if (*p1 == *p2) {
			which = i;
			count++;
		}
	if (count == 1)
		switch (which) {
			case 0:	/* ROW */
				if (rtt2.trbl > rtt1.trbl)
					*direction = (2 * face + LEFT) % COORD;
				else
					*direction = (2 * face + RIGHT) % COORD;
				break;
			case 1:	/* TRBL */
				if (rtt2.row > rtt1.row)
					*direction = (2 * face + TR) % COORD;
				else
					*direction = (2 * face + BL) % COORD;
				break;
			case 2:	/* TLBR */
				if (rtt2.row > rtt1.row)
					*direction = (2 * face + TL) % COORD;
				else
					*direction = (2 * face + BR) % COORD;
				break;
		}
	if (!w->oct.vertical && face >= MAXSIDES && *direction > TL)
		*direction = (*direction + MAXSIDES) % MAXFACES;
	return count;
}

static      RTT
ToRTT(int position)
{
	RTT         rtt;

	rtt.row = Sqrt(position);
	/* Passing row so there is no sqrt calculation again */
	rtt.trbl = (position - rtt.row * rtt.row) / 2;
	rtt.tlbr = (rtt.row * rtt.row + 2 * rtt.row - position) / 2;
	return rtt;
}

static int
ToPosition(RTT rtt)
{
	return (rtt.row * rtt.row + rtt.row + rtt.trbl - rtt.tlbr);
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

#ifdef DEBUG

static void
PrintOcta(OctWidget w)
{
	int         face, position, square;

	for (face = 0; face < MAXSIDES; face++) {
		square = 1;
		for (position = 0; position < w->oct.sizeSize; position++) {
			(void) printf("%d %d  ",
				      w->oct.octaLoc[face][position].face,
				    w->oct.octaLoc[face][position].rotation);
			if (position == square * square - 1) {
				(void) printf("\n");
				++square;
			}
		}
		(void) printf("\n");
	}
	(void) printf("\n");
}

static void
PrintFace(OctWidget w)
{
	int         position;
	int         square = 1;

	for (position = 0; position < w->oct.sizeSize; position++) {
		(void) printf("%d %d  ",
			      w->oct.faceLoc[position].face,
			      w->oct.faceLoc[position].rotation);
		if (position == square * square - 1) {
			(void) printf("\n");
			++square;
		}
	}
	(void) printf("\n");
}

static void
PrintRow(OctWidget w, int len, int orient)
{
	int         i;

	(void) printf("Row %d:\n", orient);
	for (i = 0; i < len; i++)
		(void) printf("%d %d  ", w->oct.rowLoc[orient][i].face,
			      w->oct.rowLoc[orient][i].rotation);
	(void) printf("\n");
}

#endif
