/*-
# X-BASED PYRAMINX(tm)
#
#  Pyraminx.c
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

/* Methods file for Pyraminx */

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
#include "PyraminxP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/pyraminx.data"
#endif

#define NOTDIR(x) ((x==CW)?CCW:CW)

typedef struct _CRD {
	int         column, row, diagonal;
} CRD;

static void InitializePyraminx(Widget request, Widget new);
static void ExposePyraminx(Widget new, XEvent * event, Region region);
static void ResizePyraminx(PyraminxWidget w);
static void DestroyPyraminx(Widget old);
static Boolean SetValuesPyraminx(Widget current, Widget request, Widget new);
static void QuitPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void PracticePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void PracticePyraminxMaybe(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void RandomizePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void RandomizePyraminxMaybe(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void GetPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void WritePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void UndoPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void SolvePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void IncrementPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void DecrementPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void OrientizePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void Period2ModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void Period3ModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void BothModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void StickyModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxTop(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxTr(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxLeft(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxCw(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxRight(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxBl(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxBottom(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxCcw(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void MovePyraminxInput(PyraminxWidget w, int x, int y, int direction, int shift, int control);
static void SelectPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void ReleasePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs);
static void GetColor(PyraminxWidget w, int face, int init);
static void MoveControlCb(PyraminxWidget w, int face, int direction, int style);
static void CheckPolyhedrons(PyraminxWidget w);
static void ResetPolyhedrons(PyraminxWidget w);
static void ResizePolyhedrons(PyraminxWidget w);
static int  SelectPolyhedrons(PyraminxWidget w, int x, int y, int *face, CRD * crd);
static void NarrowSelection(PyraminxWidget w, int style, int *face, CRD * crd, int *direction);
static int  PositionPolyhedrons(PyraminxWidget w, int x, int y, int style, int *face, CRD * crd, int *direction);
static void MoveNoPolyhedrons(PyraminxWidget w);
static void PracticePolyhedrons(PyraminxWidget w);
static void RandomizePolyhedrons(PyraminxWidget w);
static void MovePolyhedrons(PyraminxWidget w, int face, CRD crd, int direction, int style);
static void RotateFace(PyraminxWidget w, int view, int faceOnView, int direction);

/* CRD : Column, Row, Diagonal */
static void ReadCRD2(PyraminxWidget w, int view, int dir, int h, int orient);
static void ReadCRD3(PyraminxWidget w, int view, int faceOnView, int dir, int h, int len, int orient);
static void RotateCRD2(PyraminxWidget w, int rotate, int orient);
static void RotateCRD3(PyraminxWidget w, int faceOnView, int dir, int len, int orient);
static void ReverseCRD2(PyraminxWidget w, int orient);
static void ReverseCRD3(PyraminxWidget w, int len, int orient);
static void WriteCRD2(PyraminxWidget w, int view, int dir, int h, int orient);
static void WriteCRD3(PyraminxWidget w, int view, int faceOnView, int dir, int h, int len, int orient);
static void DrawFrame(PyraminxWidget w, GC gc);
static void DrawTriangle(PyraminxWidget w, int face, int position, int offset);
static void DrawOrientLine(PyraminxWidget w, int orient, int dx, int dy, int side);
static int  Crd(int dir, int I, int J, int side);
static int  Length(PyraminxWidget w, int face, int dir, int h);
static int  CheckMoveDir(PyraminxWidget w, CRD crd1, CRD crd2, int face, int *direction);
static CRD  ToCRD(PyraminxWidget w, int face, int position);
static int  ToPosition(PyraminxWidget w, CRD crd);
static int  Sqrt(int i);

#ifdef DEBUG
static void PrintTetra(PyraminxWidget w);
static void PrintFace(PyraminxWidget w);
static void PrintRow2(PyraminxWidget w, int orient);
static void PrintRow3(PyraminxWidget w, int len, int orient);

#endif

static char defaultTranslationsPyraminx[] =
"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
   <KeyPress>KP_Divide: MoveCcw()\n\
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
   <KeyPress>2: Period2()\n\
   <KeyPress>3: Period3()\n\
   <KeyPress>b: Both()\n\
   <KeyPress>y: Sticky()";

static XtActionsRec actionsListPyraminx[] =
{
	{"Quit", (XtActionProc) QuitPyraminx},
	{"MoveCcw", (XtActionProc) MovePyraminxCcw},
	{"MoveTop", (XtActionProc) MovePyraminxTop},
	{"MoveTr", (XtActionProc) MovePyraminxTr},
	{"MoveLeft", (XtActionProc) MovePyraminxLeft},
	{"MoveCw", (XtActionProc) MovePyraminxCw},
	{"MoveRight", (XtActionProc) MovePyraminxRight},
	{"MoveBl", (XtActionProc) MovePyraminxBl},
	{"MoveBottom", (XtActionProc) MovePyraminxBottom},
	{"Select", (XtActionProc) SelectPyraminx},
	{"Release", (XtActionProc) ReleasePyraminx},
	{"Practice", (XtActionProc) PracticePyraminx},
	{"PracticeMaybe", (XtActionProc) PracticePyraminxMaybe},
	{"Randomize", (XtActionProc) RandomizePyraminx},
	{"RandomizeMaybe", (XtActionProc) RandomizePyraminxMaybe},
	{"Get", (XtActionProc) GetPyraminx},
	{"Write", (XtActionProc) WritePyraminx},
	{"Undo", (XtActionProc) UndoPyraminx},
	{"Solve", (XtActionProc) SolvePyraminx},
	{"Increment", (XtActionProc) IncrementPyraminx},
	{"Decrement", (XtActionProc) DecrementPyraminx},
	{"Orientize", (XtActionProc) OrientizePyraminx},
	{"Period2", (XtActionProc) Period2ModePyraminx},
	{"Period3", (XtActionProc) Period3ModePyraminx},
	{"Both", (XtActionProc) BothModePyraminx},
	{"Sticky", (XtActionProc) StickyModePyraminx}
};

static XtResource resourcesPyraminx[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(PyraminxWidget, pyraminx.username), XtRString, "nobody"},
  /* Beware color values are swapped */
	{XtNfaceColor0, XtCLabel, XtRString, sizeof (String),
	 XtOffset(PyraminxWidget, pyraminx.faceName[0]), XtRString, "Blue"},
	{XtNfaceColor1, XtCLabel, XtRString, sizeof (String),
	 XtOffset(PyraminxWidget, pyraminx.faceName[1]), XtRString, "Red"},
	{XtNfaceColor2, XtCLabel, XtRString, sizeof (String),
	 XtOffset(PyraminxWidget, pyraminx.faceName[2]), XtRString, "Yellow"},
	{XtNfaceColor3, XtCLabel, XtRString, sizeof (String),
	 XtOffset(PyraminxWidget, pyraminx.faceName[3]), XtRString, "Green"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
	 XtOffset(PyraminxWidget, pyraminx.foreground), XtRString,
	 XtDefaultForeground},
	{XtNpieceBorder, XtCColor, XtRPixel, sizeof (Pixel),
	 XtOffset(PyraminxWidget, pyraminx.borderColor), XtRString,
	 XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(PyraminxWidget, core.width), XtRString, "200"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(PyraminxWidget, core.height), XtRString, "400"},
	{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
	 XtOffset(PyraminxWidget, pyraminx.mono), XtRString, "FALSE"},
	{XtNsize, XtCSize, XtRInt, sizeof (int),
	 XtOffset(PyraminxWidget, pyraminx.size), XtRString, "3"},	/*DEFAULTTETRAS */
	{XtNsticky, XtCSticky, XtRBoolean, sizeof (Boolean),
	 XtOffset(PyraminxWidget, pyraminx.sticky), XtRString, "FALSE"},
	{XtNmode, XtCMode, XtRInt, sizeof (int),
	 XtOffset(PyraminxWidget, pyraminx.mode), XtRString, "3"},	/*DEFAULTMODE */
	{XtNorient, XtCOrient, XtRBoolean, sizeof (Boolean),
	 XtOffset(PyraminxWidget, pyraminx.orient), XtRString,
	 "FALSE"},		/*DEFAULTORIENT */
	{XtNpractice, XtCPractice, XtRBoolean, sizeof (Boolean),
	 XtOffset(PyraminxWidget, pyraminx.practice), XtRString,
	 "FALSE"},		/*DEFAULTPRACTICE */
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(PyraminxWidget, pyraminx.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(PyraminxWidget, pyraminx.select), XtRCallback, NULL}
};

PyraminxClassRec pyraminxClassRec =
{
	{
		(WidgetClass) & widgetClassRec,		/* superclass */
		"Pyraminx",	/* class name */
		sizeof (PyraminxRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializePyraminx,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListPyraminx,	/* actions */
		XtNumber(actionsListPyraminx),	/* num actions */
		resourcesPyraminx,	/* resources */
		XtNumber(resourcesPyraminx),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		(XtWidgetProc) DestroyPyraminx,		/* destroy */
		(XtWidgetProc) ResizePyraminx,	/* resize */
		(XtExposeProc) ExposePyraminx,	/* expose */
		(XtSetValuesFunc) SetValuesPyraminx,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		NULL,		/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsPyraminx,	/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	}
};

WidgetClass pyraminxWidgetClass = (WidgetClass) & pyraminxClassRec;

typedef struct _RowNextP3 {
	int         viewChanged, face, direction, reverse;
} RowNextP3;
static PyraminxLoc slideNextRowP2[MAXSIDES][3] =
{
	{
		{2, 0},
		{1, 3},
		{2, 3}},
	{
		{2, 0},
		{0, 3},
		{2, 3}}
};
static RowNextP3 slideNextRowP3[MAXSIDES][MAXORIENT] =
{
	{
		{TRUE, UP, TR, FALSE},
		{TRUE, UP, TOP, FALSE},
		{FALSE, UP, BOTTOM, TRUE},
		{FALSE, UP, RIGHT, TRUE},
		{TRUE, DOWN, RIGHT, TRUE},
		{TRUE, DOWN, TR, TRUE}
	},
	{
		{FALSE, DOWN, LEFT, TRUE},
		{TRUE, UP, LEFT, TRUE},
		{TRUE, UP, BL, TRUE},
		{TRUE, DOWN, BL, FALSE},
		{TRUE, DOWN, BOTTOM, FALSE},
		{FALSE, DOWN, TOP, TRUE}
	}
};
static int  rotOrientRowP3[3][MAXORIENT] =
/* current orient, rotation */
{
	{1, 5, 1, 5, 4, 2},
	{2, 0, 2, 0, 5, 3},
	{3, 1, 3, 1, 0, 4}
};

static XPoint triangleUnit[MAXSIDES][4] =
{
	{
		{0, 2},
		{1, 0},
		{-1, 1},
		{0, -1}},
	{
		{1, 3},
		{-1, 0},
		{1, -1},
		{0, 1}}
};
static XPoint triangleList[MAXSIDES][4], letterList[MAXSIDES], offsetList[MAXSIDES];

static void
InitializePyraminx(Widget request, Widget new)
{
	PyraminxWidget w = (PyraminxWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         face, orient, side;

	for (face = 0; face < MAXFACES; face++)
		w->pyraminx.tetraLoc[face] = NULL;
	for (orient = 0; orient < 3; orient++)
		for (side = 0; side < MAXSIDES; side++)
			w->pyraminx.rowLoc[orient][side] = NULL;
	for (side = 0; side < MAXSIDES; side++)
		w->pyraminx.faceLoc[side] = NULL;
	CheckPolyhedrons(w);
	InitMoves();
	ResetPolyhedrons(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	values.foreground = w->pyraminx.foreground;
	w->pyraminx.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->pyraminx.borderColor;
	w->pyraminx.borderGC = XtGetGC(new, valueMask, &values);
	w->pyraminx.depth = DefaultDepthOfScreen(XtScreen(w));
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->pyraminx.foreground;
	w->pyraminx.inverseGC = XtGetGC(new, valueMask, &values);
	for (face = 0; face < MAXFACES; face++)
		GetColor(w, face, TRUE);
	ResizePyraminx(w);
}

static void
DestroyPyraminx(Widget old)
{
	PyraminxWidget w = (PyraminxWidget) old;
	int         face;

	for (face = 0; face < MAXFACES; face++)
		XtReleaseGC(old, w->pyraminx.faceGC[face]);
	XtReleaseGC(old, w->pyraminx.borderGC);
	XtReleaseGC(old, w->pyraminx.puzzleGC);
	XtReleaseGC(old, w->pyraminx.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->pyraminx.select);
}

static void
ResizePyraminx(PyraminxWidget w)
{
	int         tempLength;

	w->pyraminx.delta = 4;
	w->pyraminx.vertical = (w->core.height >= w->core.width);
	if (w->pyraminx.vertical)
		tempLength = MIN(w->core.height / 2, w->core.width);
	else
		tempLength = MIN(w->core.height, w->core.width / 2);
	w->pyraminx.tetraLength = MAX((tempLength - w->pyraminx.delta + 1) /
				      w->pyraminx.size, 0);
	w->pyraminx.faceLength = w->pyraminx.size * w->pyraminx.tetraLength;
	w->pyraminx.viewLength = w->pyraminx.faceLength + w->pyraminx.delta + 3;
	if (w->pyraminx.vertical) {
		w->pyraminx.puzzleSize.x = w->pyraminx.viewLength - 1;
		w->pyraminx.puzzleSize.y = 2 * w->pyraminx.viewLength -
			w->pyraminx.delta - 2;
	} else {
		w->pyraminx.puzzleSize.x = 2 * w->pyraminx.viewLength -
			w->pyraminx.delta - 2;
		w->pyraminx.puzzleSize.y = w->pyraminx.viewLength - 1;
	}
	w->pyraminx.puzzleOffset.x = ((int) w->core.width -
				      w->pyraminx.puzzleSize.x) / 2;
	w->pyraminx.puzzleOffset.y = ((int) w->core.height -
				      w->pyraminx.puzzleSize.y) / 2;
	ResizePolyhedrons(w);
}

static void
ExposePyraminx(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	PyraminxWidget w = (PyraminxWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->pyraminx.puzzleGC);
		DrawAllPolyhedrons(w);
	}
}

static      Boolean
SetValuesPyraminx(Widget current, Widget request, Widget new)
{
	PyraminxWidget c = (PyraminxWidget) current, w = (PyraminxWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	int         face;

	CheckPolyhedrons(w);
	if (w->pyraminx.foreground != c->pyraminx.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->pyraminx.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->pyraminx.puzzleGC);
		w->pyraminx.puzzleGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->pyraminx.foreground;
		XtReleaseGC(new, w->pyraminx.inverseGC);
		w->pyraminx.inverseGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->pyraminx.borderColor != c->pyraminx.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->pyraminx.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->pyraminx.borderGC);
		w->pyraminx.borderGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->pyraminx.mono || w->pyraminx.depth == 1) {
		valueMask = GCForeground | GCBackground;
		values.background = w->core.background_pixel;
		values.foreground = w->pyraminx.foreground;
		for (face = 0; face < MAXFACES; face++) {
			XtReleaseGC(new, w->pyraminx.faceGC[face]);
			w->pyraminx.faceGC[face] = XtGetGC(new, valueMask, &values);
		}
		redraw = TRUE;
	}
	for (face = 0; face < MAXFACES; face++) {
		if (strcmp(w->pyraminx.faceName[face], c->pyraminx.faceName[face]))
			GetColor(w, face, FALSE);
	}
	if (w->pyraminx.orient != c->pyraminx.orient) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	} else if (w->pyraminx.practice != c->pyraminx.practice) {
		ResetPolyhedrons(w);
		redraw = TRUE;
	}
	if (w->pyraminx.size != c->pyraminx.size ||
	    w->pyraminx.mode != c->pyraminx.mode ||
	    w->pyraminx.sticky != c->pyraminx.sticky) {
		ResetPolyhedrons(w);
		ResizePyraminx(w);
		redraw = TRUE;
	}
	if (w->pyraminx.tetraLength != c->pyraminx.tetraLength) {
		ResizePyraminx(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
QuitPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	int         control;
	CRD         crd;

	if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
			      &(w->pyraminx.currentFace), &crd)) {
		control = (int) (event->xkey.state & ControlMask);
		if (control || w->pyraminx.practice || !CheckSolved(w)) {
			w->pyraminx.currentPosition = ToPosition(w, crd);
			DrawTriangle(w, w->pyraminx.currentFace, w->pyraminx.currentPosition,
				     TRUE);
		}
	} else
		w->pyraminx.currentFace = -1;
}

static void
ReleasePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	int         shift, control, style, face, count = -1, direction = 0;
	CRD         crd;
	pyraminxCallbackStruct cb;

	if (w->pyraminx.currentFace == -1)
		return;
	DrawTriangle(w, w->pyraminx.currentFace, w->pyraminx.currentPosition,
		     FALSE);
	shift = (int) (event->xbutton.state & (ShiftMask | LockMask));
	control = (int) (event->xkey.state & ControlMask);
	if (!control && !w->pyraminx.practice && CheckSolved(w))
		MoveNoPolyhedrons(w);
	else if (SelectPolyhedrons(w, event->xbutton.x, event->xbutton.y,
				   &face, &crd)) {
		control = (control) ? 1 : 0;
		if (w->pyraminx.mode != BOTH) {
			if (control && shift)
				style = (w->pyraminx.mode == PERIOD3) ? PERIOD2 : PERIOD3;
			else
				style = (w->pyraminx.mode == PERIOD2) ? PERIOD2 : PERIOD3;
		} else
			style = (shift) ? PERIOD3 : PERIOD2;
		if (face == w->pyraminx.currentFace)
			count = CheckMoveDir(w, ToCRD(w, face, w->pyraminx.currentPosition),
					     crd, face, &direction);
		if (count == 1) {
			NarrowSelection(w, style, &face, &crd, &direction);
			MovePyraminx(w, face, w->pyraminx.currentPosition, direction, style,
				     control);
			if (!control && CheckSolved(w)) {
				cb.reason = PYRAMINX_SOLVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else if (count == 2) {
			cb.reason = PYRAMINX_AMBIGUOUS;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else if (count == 0)
			MoveNoPolyhedrons(w);
	}
}

static void
PracticePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	PracticePolyhedrons(w);
}

static void
PracticePyraminxMaybe(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->pyraminx.started)
		PracticePolyhedrons(w);
}

static void
RandomizePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizePolyhedrons(w);
}

static void
RandomizePyraminxMaybe(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->pyraminx.started)
		RandomizePolyhedrons(w);
}

static void
GetPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, size, mode, sticky, orient, practice, moves;
	pyraminxCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for a get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &size);
		if (size >= MINTETRAS) {
			for (i = w->pyraminx.size; i < size; i++) {
				cb.reason = PYRAMINX_INC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->pyraminx.size; i > size; i--) {
				cb.reason = PYRAMINX_DEC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: size %d should be between %d and MAXINT\n",
				      DATAFILE, size, MINTETRAS);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &mode);
		if (mode >= PERIOD2 && mode <= BOTH)
			switch (mode) {
				case PERIOD2:
					cb.reason = PYRAMINX_PERIOD2;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
					break;
				case PERIOD3:
					cb.reason = PYRAMINX_PERIOD3;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
					break;
				case BOTH:
					cb.reason = PYRAMINX_BOTH;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else
			(void) printf("%s corrupted: mode %d should be between %d and %d\n",
				      DATAFILE, mode, PERIOD2, BOTH);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &sticky);
		if (w->pyraminx.sticky != (Boolean) sticky) {
			cb.reason = PYRAMINX_STICKY;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &orient);
		if (w->pyraminx.orient != (Boolean) orient) {
			cb.reason = PYRAMINX_ORIENT;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &practice);
		if (w->pyraminx.practice != (Boolean) practice) {
			cb.reason = PYRAMINX_PRACTICE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = PYRAMINX_RESTORE;
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
WritePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "size%c %d\n", SYMBOL, w->pyraminx.size);
		(void) fprintf(fp, "mode%c %d\n", SYMBOL, w->pyraminx.mode);
		(void) fprintf(fp, "sticky%c %d\n", SYMBOL, (w->pyraminx.sticky) ? 1 : 0);
		(void) fprintf(fp, "orient%c %d\n", SYMBOL, (w->pyraminx.orient) ? 1 : 0);
		(void) fprintf(fp, "practice%c %d\n", SYMBOL,
			       (w->pyraminx.practice) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         face, position, direction, style, control;

		GetMove(&face, &position, &direction, &style, &control);
		direction = (direction < MAXORIENT) ? (direction + MAXORIENT / 2) %
			MAXORIENT : 3 * MAXORIENT - direction;
		if (control)
			MoveControlCb(w, face, direction, style);
		else {
			pyraminxCallbackStruct cb;

			MovePolyhedrons(w, face, ToCRD(w, face, position), direction, style);
			cb.reason = PYRAMINX_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolvePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolvePolyhedrons(w); *//* Sorry, unimplemented */
}

static void
IncrementPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_INC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
DecrementPyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	pyraminxCallbackStruct cb;

	if (w->pyraminx.size <= MINTETRAS)
		return;
	cb.reason = PYRAMINX_DEC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
OrientizePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_ORIENT;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Period2ModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_PERIOD2;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Period3ModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_PERIOD3;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
BothModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_BOTH;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
StickyModePyraminx(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_STICKY;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MovePyraminxCcw(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, CCW,
		       (int) (event->xbutton.state & (ShiftMask | LockMask)),
			  (int) (event->xbutton.state & ControlMask));
}

static void
MovePyraminxTop(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, TOP,
			  (int) (event->xkey.state & (ShiftMask | LockMask)),
			  (int) (event->xkey.state & ControlMask));
}

static void
MovePyraminxTr(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, TR,
			  (int) (event->xkey.state & (ShiftMask | LockMask)),
			  (int) (event->xkey.state & ControlMask));
}

static void
MovePyraminxLeft(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, LEFT,
			  (int) (event->xkey.state & (ShiftMask | LockMask)),
			  (int) (event->xkey.state & ControlMask));
}

static void
MovePyraminxCw(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, CW,
			  (int) (event->xkey.state & (ShiftMask | LockMask)),
			  (int) (event->xkey.state & ControlMask));
}

static void
MovePyraminxRight(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, RIGHT,
			  (int) (event->xkey.state & (ShiftMask | LockMask)),
			  (int) (event->xkey.state & ControlMask));
}

static void
MovePyraminxBl(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, BL,
			  (int) (event->xkey.state & (ShiftMask | LockMask)),
			  (int) (event->xkey.state & ControlMask));
}

static void
MovePyraminxBottom(PyraminxWidget w, XEvent * event, char **args, int nArgs)
{
	MovePyraminxInput(w, event->xbutton.x, event->xbutton.y, BOTTOM,
			  (int) (event->xkey.state & (ShiftMask | LockMask)),
			  (int) (event->xkey.state & ControlMask));
}

static void
MovePyraminxInput(PyraminxWidget w, int x, int y, int direction, int shift, int control)
{
	int         style, face;
	CRD         crd;

	if (w->pyraminx.mode != BOTH) {
		if (control && shift)
			style = (w->pyraminx.mode == PERIOD3) ? PERIOD2 : PERIOD3;
		else
			style = (w->pyraminx.mode == PERIOD2) ? PERIOD2 : PERIOD3;
	} else
		style = (shift) ? PERIOD3 : PERIOD2;
	if (!w->pyraminx.practice && !control && CheckSolved(w)) {
		MoveNoPolyhedrons(w);
		return;
	}
	if (!PositionPolyhedrons(w, x, y, style, &face, &crd, &direction))
		return;
	control = (control) ? 1 : 0;
	MovePyraminx(w, face, ToPosition(w, crd), direction, style, control);
	if (!control && CheckSolved(w)) {
		pyraminxCallbackStruct cb;

		cb.reason = PYRAMINX_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

void
MovePyraminx(PyraminxWidget w, int face, int position, int direction, int style, int control)
{
	if (control)
		MoveControlCb(w, face, direction, style);
	else {
		pyraminxCallbackStruct cb;

		MovePolyhedrons(w, face, ToCRD(w, face, position), direction, style);
		cb.reason = PYRAMINX_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	PutMove(face, position, direction, style, control);
}

static void
GetColor(PyraminxWidget w, int face, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->pyraminx.depth > 1 && !w->pyraminx.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
			     w->pyraminx.faceName[face], &colorCell, &rgb)) {
			values.foreground = w->pyraminx.faceColor[face] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->pyraminx.faceGC[face]);
			w->pyraminx.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined",
				       w->pyraminx.faceName[face]);
			XtWarning(buf);
		}
	}
	values.foreground = w->pyraminx.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->pyraminx.faceGC[face]);
	w->pyraminx.faceGC[face] = XtGetGC((Widget) w, valueMask, &values);
}

static void
MoveControlCb(PyraminxWidget w, int face, int direction, int style)
{
	pyraminxCallbackStruct cb;
	int         i, faceOnView;
	CRD         crd;

	faceOnView = face % MAXSIDES;
	if (w->pyraminx.sticky) {
		if (style == PERIOD2)
			for (i = 0; i < 3; i++) {
				if (direction == TR || direction == BL) {
					crd.column = 0;
					crd.row = 3 * i / 2;
					crd.diagonal = crd.row + i % 2;
					MovePolyhedrons(w, face, crd, direction, style);
				} else {
					crd.column = 3 * i / 2;
					crd.row = 3 * i / 2;
					crd.diagonal = crd.row + crd.column;
					MovePolyhedrons(w, face, crd, direction, style);
				}
				cb.reason = PYRAMINX_CONTROL;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else		/* (style == PERIOD3) */
			for (i = 0; i < 2; i++) {
				if (direction == TR || direction == BL) {
					crd.column = 1 + faceOnView;
					crd.row = 1 + faceOnView;
					crd.diagonal = crd.row + crd.column + i;
					MovePolyhedrons(w, face, crd, direction, style);
				} else {
					crd.column = i + 2 * faceOnView;
					crd.row = i + 2 * faceOnView;
					crd.diagonal = crd.row + crd.column + faceOnView;
					MovePolyhedrons(w, face, crd, direction, style);
				}
				cb.reason = PYRAMINX_CONTROL;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
	} else {
		for (i = 0; i < w->pyraminx.size; i++) {
			if (direction == TR || direction == BL) {
				if (style == PERIOD2) {
					crd.column = 0;
					crd.row = i;
					crd.diagonal = crd.row;
					MovePolyhedrons(w, face, crd, direction, style);
				} else {
					crd.column = faceOnView * (w->pyraminx.size - 1);
					crd.row = i;
					crd.diagonal = crd.column + crd.row + faceOnView;
					MovePolyhedrons(w, face, crd, direction, style);
				}
			} else {
				crd.column = i;
				crd.row = w->pyraminx.size - 1 - i;
				crd.diagonal = crd.column + crd.row + faceOnView;
				MovePolyhedrons(w, face, crd, direction, style);
			}
			cb.reason = PYRAMINX_CONTROL;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
CheckPolyhedrons(PyraminxWidget w)
{
	if (w->pyraminx.size < MINTETRAS) {
		char        buf[121];

		(void) sprintf(buf,
		    "Number of Tetras on edge out of bounds, use %d..MAXINT",
			       MINTETRAS);
		XtWarning(buf);
		w->pyraminx.size = DEFAULTTETRAS;
	}
	if (w->pyraminx.mode < PERIOD2 || w->pyraminx.mode > BOTH) {
		XtWarning("Mode is in error, use 2 for Period2, 3 for Period3, 4 for Both");
		w->pyraminx.mode = DEFAULTMODE;
	}
}

static void
ResetPolyhedrons(PyraminxWidget w)
{
	int         face, position, orient, side;

	w->pyraminx.sizeSize = w->pyraminx.size * w->pyraminx.size;
	for (face = 0; face < MAXFACES; face++) {
		if (w->pyraminx.tetraLoc[face])
			(void) free((void *) w->pyraminx.tetraLoc[face]);
		if (!(w->pyraminx.tetraLoc[face] = (PyraminxLoc *)
		      malloc(sizeof (PyraminxLoc) * w->pyraminx.sizeSize)))
			XtError("Not enough memory, exiting.");
		if (startLoc[face])
			(void) free((void *) startLoc[face]);
		if (!(startLoc[face] = (PyraminxLoc *)
		      malloc(sizeof (PyraminxLoc) * w->pyraminx.sizeSize)))
			XtError("Not enough memory, exiting.");
	}
	for (orient = 0; orient < 3; orient++)
		for (side = 0; side < MAXSIDES; side++) {
			if (w->pyraminx.rowLoc[orient][side])
				(void) free((void *) w->pyraminx.rowLoc[orient][side]);
			if (!(w->pyraminx.rowLoc[orient][side] = (PyraminxLoc *)
			    malloc(sizeof (PyraminxLoc) * w->pyraminx.size)))
				XtError("Not enough memory, exiting.");
		}
	for (side = 0; side < MAXSIDES; side++) {
		if (w->pyraminx.faceLoc)
			(void) free((void *) w->pyraminx.faceLoc[side]);
		if (!(w->pyraminx.faceLoc[side] = (PyraminxLoc *)
		      malloc(sizeof (PyraminxLoc) * w->pyraminx.sizeSize)))
			XtError("Not enough memory, exiting.");
	}
	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->pyraminx.sizeSize; position++) {
			w->pyraminx.tetraLoc[face][position].face = face;
			w->pyraminx.tetraLoc[face][position].rotation = TOP;
		}
	FlushMoves(w);
	w->pyraminx.started = FALSE;
}

static void
ResizePolyhedrons(PyraminxWidget w)
{
	int         i, j;

	w->pyraminx.tetraLength = w->pyraminx.faceLength / w->pyraminx.size -
		w->pyraminx.delta - 1;
	for (i = 0; i <= 3; i++)
		for (j = 0; j < MAXSIDES; j++) {
			triangleList[j][i].x = triangleUnit[j][i].x *
				w->pyraminx.tetraLength;
			triangleList[j][i].y = triangleUnit[j][i].y *
				w->pyraminx.tetraLength;
		}
	offsetList[DOWN].x = 0;
	offsetList[UP].x = w->pyraminx.tetraLength + 2;
	offsetList[DOWN].y = 0;
	offsetList[UP].y = w->pyraminx.tetraLength + 2;
	letterList[DOWN].x = w->pyraminx.tetraLength / 4 - 3;
	letterList[UP].x = 3 * w->pyraminx.tetraLength / 4;
	letterList[DOWN].y = w->pyraminx.tetraLength / 4 + 5;
	letterList[UP].y = 3 * w->pyraminx.tetraLength / 4 + 5;
	w->pyraminx.sideOffset = 3 * w->pyraminx.size / 4;
	w->pyraminx.orientLineLength = w->pyraminx.tetraLength / 4;
	w->pyraminx.orientDiagLength = MAX(w->pyraminx.orientLineLength - 3, 0);
}

static int
SelectPolyhedrons(PyraminxWidget w, int x, int y, int *face, CRD * crd)
{
	int         offset, modI, modJ, side, view;

	x -= w->pyraminx.puzzleOffset.x;
	y -= w->pyraminx.puzzleOffset.y;
	if (w->pyraminx.vertical && y > w->pyraminx.viewLength - 1) {
		y -= (w->pyraminx.viewLength - 1);
		view = DOWN;
	} else if (!w->pyraminx.vertical && x > w->pyraminx.viewLength - 1) {
		x -= (w->pyraminx.viewLength - 1);
		view = DOWN;
	} else
		view = UP;
	if (x <= 0 || y <= 0 ||
	    x >= w->pyraminx.faceLength + w->pyraminx.delta ||
	    y >= w->pyraminx.faceLength + w->pyraminx.delta)
		return FALSE;
	else if (x + y > w->pyraminx.faceLength)
		offset = 2 * w->pyraminx.delta + 1;
	else
		offset = w->pyraminx.delta;
	crd->column = (x - offset) / (w->pyraminx.tetraLength + w->pyraminx.delta);
	crd->row = (y - offset) / (w->pyraminx.tetraLength + w->pyraminx.delta);
	modI = (x - offset) % (w->pyraminx.tetraLength + w->pyraminx.delta);
	modJ = (y - offset) % (w->pyraminx.tetraLength + w->pyraminx.delta);
	side = (modI + modJ > w->pyraminx.tetraLength + 1);
	if (!w->pyraminx.vertical && view == DOWN) {
		crd->row = w->pyraminx.size - crd->row - 1;
		crd->column = w->pyraminx.size - crd->column - 1;
		side = !side;
	}
	crd->diagonal = crd->row + crd->column + side;
	*face = view * MAXSIDES + (crd->diagonal >= w->pyraminx.size);
	return TRUE;
}

static void
NarrowSelection(PyraminxWidget w, int style, int *face, CRD * crd, int *direction)
{
	if (!w->pyraminx.vertical && *face >= MAXSIDES && *direction < MAXORIENT)
		*direction = (*direction + MAXORIENT / 2) % MAXORIENT;
	if (style == PERIOD2) {
		if (*direction == CW)
			*direction = TR;
		else if (*direction == CCW)
			*direction = BL;
	} else {		/* style == PERIOD3 */
		if (*direction == CW || *direction == CCW) {
			crd->diagonal = w->pyraminx.size - (crd->diagonal < w->pyraminx.size);
			*direction = ((*direction == CW && crd->diagonal == w->pyraminx.size) ||
				      (*direction == CCW && crd->diagonal != w->pyraminx.size))
				? TR : BL;
			*face = !(*face % 2) + 2 * (*face / 2);
			crd->row = w->pyraminx.size - 1;
			crd->column = 0;
		}
	}
}

static int
PositionPolyhedrons(PyraminxWidget w, int x, int y, int style, int *face, CRD * crd, int *direction)
{
	if (!SelectPolyhedrons(w, x, y, face, crd))
		return FALSE;
	NarrowSelection(w, style, face, crd, direction);
	return TRUE;
}

static void
MoveNoPolyhedrons(PyraminxWidget w)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_ILLEGAL;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
PracticePolyhedrons(PyraminxWidget w)
{
	pyraminxCallbackStruct cb;

	cb.reason = PYRAMINX_PRACTICE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
RandomizePolyhedrons(PyraminxWidget w)
{
	pyraminxCallbackStruct cb;
	int         randomDirection, face, position, style;
	int         big = w->pyraminx.sizeSize * 3 + NRAND(2);

	if (big > 1000)
		big = 1000;
	if (w->pyraminx.practice)
		PracticePolyhedrons(w);
	if (w->pyraminx.sticky)
		big /= 3;
	cb.reason = PYRAMINX_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
	big = 3;
#endif

	while (big--) {
		face = NRAND(MAXFACES);
		if (w->pyraminx.mode == BOTH)
			style = NRAND(MAXMODES - 1) + PERIOD2;
		else
			style = w->pyraminx.mode;
		if (w->pyraminx.sticky) {
			if (style == PERIOD2) {
				if (NRAND(3) == 2) {
					position = (NRAND(2)) ? 9 : 6;
					randomDirection = (NRAND(2)) ? TR : BL;
				} else {
					position = (NRAND(2)) ? 6 : 0;
					if (NRAND(2))
						randomDirection = (NRAND(2)) ? LEFT : RIGHT;
					else
						randomDirection = (NRAND(2)) ? TOP : BOTTOM;
				}
			} else {	/* style == PERIOD3 */
				position = 6;
				randomDirection = NRAND(6);
			}
		} else {	/* (!w->pyraminx.sticky) */
			randomDirection = NRAND(MAXORIENT);
			position = NRAND(w->pyraminx.sizeSize);
			if (w->pyraminx.mode == BOTH)
				style = NRAND(BOTH);
			else
				style = w->pyraminx.mode;
		}
		MovePyraminx(w, face, position, randomDirection, style, FALSE);
		cb.reason = PYRAMINX_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	FlushMoves(w);
	cb.reason = PYRAMINX_RANDOMIZE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (CheckSolved(w)) {
		cb.reason = PYRAMINX_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
MovePolyhedrons(PyraminxWidget w, int face, CRD crd, int direction, int style)
{
	int         view, side;
	int         newSide, newView, rotate, reverse, h, k, newH, faceOnView,
	            len;
	int         newFace, newDirection, bound, l = 0;

	view = face / MAXSIDES;
	side = crd.diagonal - crd.column - crd.row;
	if (style == PERIOD2) {
		/* Period 2 Slide rows */
		h = Crd(direction, crd.column, crd.row, side);
		if (w->pyraminx.sticky && (h % 4 == 1 || h % 4 == 2)) {
			bound = TRUE;
			l = 0;
			if (h % 4 == 2)
				h = h - 1;
		} else
			bound = FALSE;
		do {
			ReadCRD2(w, view, direction, h, 0);
			for (k = 1; k <= 2; k++) {
				rotate = slideNextRowP2[side][direction % 3].rotation;
				if (direction == TR || direction == BL) {
					newView = view;
					newSide = !side;
					newH = 2 * w->pyraminx.size - 1 - h;
					reverse = FALSE;
				} else if (!rotate) {
					newView = !view;
					newSide = side;
					newH = h;
					reverse = FALSE;
				} else {	/* rotate == 3 */
					newView = !view;
					newSide = !side;
					newH = w->pyraminx.size - 1 - h;
					reverse = TRUE;
				}
				if (k != 2)
					ReadCRD2(w, newView, direction, newH, k);
				RotateCRD2(w, rotate, k - 1);
				if (reverse == TRUE)
					ReverseCRD2(w, k - 1);
				WriteCRD2(w, newView, direction, newH, k - 1);
				view = newView;
				h = newH;
				side = newSide;
			}
			l++;
			h++;
		} while (bound && l < 2);
	} else {		/* style == PERIOD3 */
		faceOnView = (crd.diagonal >= w->pyraminx.size);
		h = Crd(direction, crd.column, crd.row, side);
		bound = FALSE;
		if (direction == TR || direction == BL) {
			if (h == w->pyraminx.size)
				RotateFace(w, view, DOWN, (direction == TR) ? CCW : CW);
			else if (h == w->pyraminx.size - 1)
				RotateFace(w, view, UP, (direction == TR) ? CW : CCW);
			else if (w->pyraminx.sticky)
				bound = TRUE;
		} else if (!crd.column && !faceOnView &&
			   (direction == TOP || direction == BOTTOM))
			RotateFace(w, !view, DOWN,
			 (direction == TOP || direction == LEFT) ? CCW : CW);
		else if (crd.column == w->pyraminx.size - 1 && faceOnView &&
			 (direction == TOP || direction == BOTTOM))
			RotateFace(w, !view, UP,
				   (direction == BOTTOM || direction == RIGHT) ? CCW : CW);
		else if (!crd.row && !faceOnView &&
			 (direction == RIGHT || direction == LEFT))
			RotateFace(w, !view, UP,
				   (direction == BOTTOM || direction == RIGHT) ? CCW : CW);
		else if (crd.row == w->pyraminx.size - 1 && faceOnView &&
			 (direction == RIGHT || direction == LEFT))
			RotateFace(w, !view, DOWN,
			 (direction == TOP || direction == LEFT) ? CCW : CW);
		else if (w->pyraminx.sticky)
			bound = TRUE;
		/* Slide rows */
		if (bound == TRUE) {
			l = 0;
			if (direction == TR || direction == BL)
				h = (faceOnView == UP) ? 0 : w->pyraminx.size + 1;
			else
				h = (faceOnView == UP) ? 1 : 0;
		}
		do {
			len = Length(w, faceOnView, direction, h);
			ReadCRD3(w, view, faceOnView, direction, h, len, 0);
			for (k = 1; k <= 3; k++) {
				newView = (slideNextRowP3[faceOnView][direction].viewChanged)
					? !view : view;
				newFace = !slideNextRowP3[faceOnView][direction].face;
				newDirection = slideNextRowP3[faceOnView][direction].direction;
				reverse = slideNextRowP3[faceOnView][direction].reverse;
				newH = w->pyraminx.size - 1 - h;
				if (!faceOnView) {
					if (direction == TOP)
						newH = w->pyraminx.size + h;
					else if (direction == TR)
						newH = h;
					else
						newH = w->pyraminx.size - 1 - h;
				} else {
					if (direction == TR || direction == RIGHT)
						newH = 2 * w->pyraminx.size - 1 - h;
					else if (direction == BOTTOM)
						newH = h;
					else if (direction == BL)
						newH = h - w->pyraminx.size;
					else
						newH = w->pyraminx.size - 1 - h;
				}
				if (k != 3)
					ReadCRD3(w, newView, newFace, newDirection, newH, len, k);
				RotateCRD3(w, faceOnView, direction, len, k - 1);
				if (reverse == TRUE)
					ReverseCRD3(w, len, k - 1);
				WriteCRD3(w, newView, newFace, newDirection, newH, len, k - 1);
				view = newView;
				faceOnView = newFace;
				direction = newDirection;
				h = newH;
			}
			h++;
			l++;
		} while (bound && l < w->pyraminx.size - 1);
	}
}

static void
RotateFace(PyraminxWidget w, int view, int faceOnView, int direction)
{
	int         g, h, side, face, position;
	CRD         crd;

	/* Read Face */
	for (g = 0; g < w->pyraminx.size; g++)
		for (h = 0; h < w->pyraminx.size - g; h++)
			for (side = 0; side < MAXSIDES; side++)
				if (g + h + side < w->pyraminx.size) {
					if (faceOnView == DOWN) {
						crd.column = h;
						crd.row = g;
						crd.diagonal = h + g + side;
						face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
						position = ToPosition(w, crd);
						w->pyraminx.faceLoc[side][h + g * w->pyraminx.size] =
							w->pyraminx.tetraLoc[face][position];
					} else {	/* faceOnView == UP */
						crd.column = w->pyraminx.size - 1 - h;
						crd.row = w->pyraminx.size - 1 - g;
						crd.diagonal = crd.column + crd.row + !side;
						face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
						position = ToPosition(w, crd);
						w->pyraminx.faceLoc[side][h + g * w->pyraminx.size] =
							w->pyraminx.tetraLoc[face][position];
					}
				}
	/* Write Face */
	if (faceOnView == DOWN) {
		for (g = 0; g < w->pyraminx.size; g++)
			for (h = 0; h < w->pyraminx.size - g; h++)
				for (side = 0; side < MAXSIDES; side++)
					if (g + h + side < w->pyraminx.size) {
						crd.column = h;
						crd.row = g;
						crd.diagonal = h + g + side;
						face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
						position = ToPosition(w, crd);
						if (direction == CCW)
							w->pyraminx.tetraLoc[face][position] =
								w->pyraminx.faceLoc[side][w->pyraminx.size - 1 - g - h - side +
							h * w->pyraminx.size];
						else	/* direction == CW */
							w->pyraminx.tetraLoc[face][position] =
								w->pyraminx.faceLoc[side][g +
											  (w->pyraminx.size - 1 - g - h - side) * w->pyraminx.size];
						w->pyraminx.tetraLoc[face][position].rotation =
							(direction == CW) ?
							(w->pyraminx.tetraLoc[face][position].rotation + 2) %
							MAXORIENT :
							(w->pyraminx.tetraLoc[face][position].rotation + 4) %
							MAXORIENT;
						DrawTriangle(w, face, position, FALSE);
					}
	} else {		/* faceOnView == UP */
		for (g = w->pyraminx.size - 1; g >= 0; g--)
			for (h = w->pyraminx.size - 1; h >= w->pyraminx.size - 1 - g; h--)
				for (side = 1; side >= 0; side--)
					if (g + h + side >= w->pyraminx.size) {
						crd.column = h;
						crd.row = g;
						crd.diagonal = h + g + side;
						face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
						position = ToPosition(w, crd);
						if (direction == CCW)
							w->pyraminx.tetraLoc[face][position] =
								w->pyraminx.faceLoc[!side][g + h - w->pyraminx.size + 1 - !side
											   + (w->pyraminx.size - 1 - h) * w->pyraminx.size];
						else	/* (direction == CW) */
							w->pyraminx.tetraLoc[face][position] =
								w->pyraminx.faceLoc[!side][w->pyraminx.size - 1 - g +
											   (g + h - w->pyraminx.size + 1 - !side) * w->pyraminx.size];
						w->pyraminx.tetraLoc[face][position].rotation =
							(direction == CW) ?
							(w->pyraminx.tetraLoc[face][position].rotation + 2) %
							MAXORIENT :
							(w->pyraminx.tetraLoc[face][position].rotation + 4) %
							MAXORIENT;
						DrawTriangle(w, face, position, FALSE);
					}
	}
}

static void
ReadCRD2(PyraminxWidget w, int view, int dir, int h, int orient)
{
	int         g, i, j, side, faceOnView, s, face, position;
	CRD         crd;

	if (dir == TOP || dir == BOTTOM)
		for (g = 0; g < w->pyraminx.size; g++)
			for (side = 0; side < MAXSIDES; side++) {
				crd.column = h;
				crd.row = g;
				crd.diagonal = h + g + side;
				face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
				position = ToPosition(w, crd);
				w->pyraminx.rowLoc[orient][side][g] =
					w->pyraminx.tetraLoc[face][position];
	} else if (dir == RIGHT || dir == LEFT)
		for (g = 0; g < w->pyraminx.size; g++)
			for (side = 0; side < MAXSIDES; side++) {
				crd.column = g;
				crd.row = h;
				crd.diagonal = h + g + side;
				face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
				position = ToPosition(w, crd);
				w->pyraminx.rowLoc[orient][side][g] =
					w->pyraminx.tetraLoc[face][position];
	} else {		/* dir == TR || dir == BL */
		faceOnView = (h < w->pyraminx.size);
		i = (faceOnView == UP) ? w->pyraminx.size - 1 : 0;
		j = h % w->pyraminx.size;
		for (g = 0; g < w->pyraminx.size; g++) {
			for (side = 0; side < MAXSIDES; side++) {
				s = (side == UP) ? !faceOnView : faceOnView;
				crd.column = i;
				crd.row = j;
				crd.diagonal = i + j + s;
				face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
				position = ToPosition(w, crd);
				w->pyraminx.rowLoc[orient][side][g] =
					w->pyraminx.tetraLoc[face][position];
				if (!side) {
					if (faceOnView == UP) {
						if (j == w->pyraminx.size - 1)
							view = !view;
						j = (j + 1) % w->pyraminx.size;
					} else {	/* faceOnView == DOWN */
						if (!j)
							view = !view;
						j = (j - 1 + w->pyraminx.size) % w->pyraminx.size;
					}
				}
			}
			i = (faceOnView == UP) ? i - 1 : i + 1;
		}
	}
}

static void
ReadCRD3(PyraminxWidget w, int view, int faceOnView, int dir, int h, int len, int orient)
{
	int         g, i, j, side, s, face, position;
	CRD         crd;

	if (dir == TOP || dir == BOTTOM) {
		for (g = 0; g <= len; g++)
			for (side = 0; side < MAXSIDES; side++)
				if (!side || g < len) {
					crd.column = h;
					crd.row = (faceOnView == UP) ? g : w->pyraminx.size - 1 - g;
					crd.diagonal = h + crd.row + ((faceOnView == UP) ? side : !side);
					face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
					position = ToPosition(w, crd);
					w->pyraminx.rowLoc[orient][side][g] =
						w->pyraminx.tetraLoc[face][position];
				}
	} else if (dir == RIGHT || dir == LEFT) {
		for (g = 0; g <= len; g++)
			for (side = 0; side < MAXSIDES; side++)
				if (!side || g < len) {
					crd.column = (faceOnView == UP) ? g : w->pyraminx.size - 1 - g;
					crd.row = h;
					crd.diagonal = h + crd.column + ((faceOnView == UP) ? side : !side);
					face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
					position = ToPosition(w, crd);
					w->pyraminx.rowLoc[orient][side][g] =
						w->pyraminx.tetraLoc[face][position];
				}
	} else {		/* dir == TR || dir == BL */
		i = (faceOnView == DOWN) ? w->pyraminx.size - 1 : 0;
		j = h % w->pyraminx.size;
		for (g = 0; g <= len; g++) {
			for (side = 0; side < MAXSIDES; side++) {
				if (!side || g < len) {
					s = (side == UP) ? faceOnView : !faceOnView;
					crd.column = i;
					crd.row = j;
					crd.diagonal = i + j + s;
					face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
					position = ToPosition(w, crd);
					w->pyraminx.rowLoc[orient][side][g] =
						w->pyraminx.tetraLoc[face][position];
					if (!side)
						j = (faceOnView == DOWN) ? j + 1 : j - 1;
				}
			}
			i = (faceOnView == DOWN) ? i - 1 : i + 1;
		}
	}
}

static void
RotateCRD2(PyraminxWidget w, int rotate, int orient)
{
	int         g, side;

	for (g = 0; g < w->pyraminx.size; g++)
		for (side = 0; side < MAXSIDES; side++)
			w->pyraminx.rowLoc[orient][side][g].rotation =
				(w->pyraminx.rowLoc[orient][side][g].rotation + rotate) % MAXORIENT;
}

static void
RotateCRD3(PyraminxWidget w, int faceOnView, int dir, int len, int orient)
{
	int         g, side, direction, tetraOrient;

	for (g = 0; g <= len; g++)
		for (side = 0; side < MAXSIDES; side++)
			if (!side || g < len) {
				direction = (faceOnView == DOWN) ? (dir + 3) % MAXORIENT : dir;
				tetraOrient = w->pyraminx.rowLoc[orient][side][g].rotation;
				w->pyraminx.rowLoc[orient][side][g].rotation =
					(tetraOrient >= 3) ?
					(rotOrientRowP3[tetraOrient - 3][direction] + 3) % MAXORIENT :
					rotOrientRowP3[tetraOrient][direction];
			}
}

static void
ReverseCRD2(PyraminxWidget w, int orient)
{
	PyraminxLoc temp;
	int         g;

	for (g = 0; g < w->pyraminx.size; g++) {
		temp = w->pyraminx.rowLoc[orient][g % 2][g / 2];
		w->pyraminx.rowLoc[orient][g % 2][g / 2] =
			w->pyraminx.rowLoc[orient][!(g % 2)][w->pyraminx.size - 1 - g / 2];
		w->pyraminx.rowLoc[orient][!(g % 2)][w->pyraminx.size - 1 - g / 2] =
			temp;
	}
}

static void
ReverseCRD3(PyraminxWidget w, int len, int orient)
{
	PyraminxLoc temp;
	int         g;

	for (g = 0; g < len; g++) {
		temp = w->pyraminx.rowLoc[orient][g % 2][len - ((g + 1) / 2)];
		w->pyraminx.rowLoc[orient][g % 2][len - ((g + 1) / 2)] =
			w->pyraminx.rowLoc[orient][g % 2][g / 2];
		w->pyraminx.rowLoc[orient][g % 2][g / 2] = temp;
	}
}

static void
WriteCRD2(PyraminxWidget w, int view, int dir, int h, int orient)
{
	int         g, side, i, j, s, faceOnView, face, position;
	CRD         crd;

	if (dir == TOP || dir == BOTTOM) {
		for (g = 0; g < w->pyraminx.size; g++)
			for (side = 0; side < MAXSIDES; side++) {
				crd.column = h;
				crd.row = g;
				crd.diagonal = h + g + side;
				face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
				position = ToPosition(w, crd);
				w->pyraminx.tetraLoc[face][position] =
					w->pyraminx.rowLoc[orient][side][g];
				DrawTriangle(w,
					     view * MAXSIDES + (crd.diagonal >= w->pyraminx.size),
					     ToPosition(w, crd), FALSE);
			}
	} else if (dir == RIGHT || dir == LEFT) {
		for (g = 0; g < w->pyraminx.size; g++)
			for (side = 0; side < MAXSIDES; side++) {
				crd.column = g;
				crd.row = h;
				crd.diagonal = h + g + side;
				face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
				position = ToPosition(w, crd);
				w->pyraminx.tetraLoc[face][position] =
					w->pyraminx.rowLoc[orient][side][g];
				DrawTriangle(w, face, position, FALSE);
			}
	} else {		/* dir == TR || dir == BL */
		faceOnView = (h < w->pyraminx.size);
		i = (faceOnView == UP) ? w->pyraminx.size - 1 : 0;
		j = h % w->pyraminx.size;
		for (g = 0; g < w->pyraminx.size; g++) {
			for (side = 0; side < MAXSIDES; side++) {
				s = (side == UP) ? !faceOnView : faceOnView;
				crd.column = i;
				crd.row = j;
				crd.diagonal = i + j + s;
				face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
				position = ToPosition(w, crd);
				w->pyraminx.tetraLoc[face][position] =
					w->pyraminx.rowLoc[orient][side][g];
				DrawTriangle(w, face, position, FALSE);
				if (!side) {
					if (faceOnView == UP) {
						if (j == w->pyraminx.size - 1) {
							view = !view;
							j = 0;
						} else
							++j;
					} else {	/* FACE == DOWN */
						if (!j) {
							view = !view;
							j = w->pyraminx.size - 1;
						} else
							--j;
					}
				}
			}
			if (faceOnView == UP)
				--i;
			else	/* faceOnView == DOWN */
				++i;
		}
	}
}

static void
WriteCRD3(PyraminxWidget w, int view, int faceOnView, int dir, int h, int len, int orient)
{
	int         g, side, i, j, k, s, face, position;
	CRD         crd;

	if (dir == TOP || dir == BOTTOM) {
		for (k = 0; k <= len; k++)
			for (side = 0; side < MAXSIDES; side++)
				if (!side || k < len) {
					g = (faceOnView == DOWN) ? w->pyraminx.size - 1 - k : k;
					s = (faceOnView == DOWN) ? !side : side;
					crd.column = h;
					crd.row = g;
					crd.diagonal = h + g + s;
					face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
					position = ToPosition(w, crd);
					w->pyraminx.tetraLoc[face][position] =
						w->pyraminx.rowLoc[orient][side][k];
					DrawTriangle(w, face, position, FALSE);
				}
	} else if (dir == RIGHT || dir == LEFT) {
		for (k = 0; k <= len; k++)
			for (side = 0; side < MAXSIDES; side++)
				if (!side || k < len) {
					g = (faceOnView == DOWN) ? w->pyraminx.size - 1 - k : k;
					s = (faceOnView == DOWN) ? !side : side;
					crd.column = g;
					crd.row = h;
					crd.diagonal = h + g + s;
					face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
					position = ToPosition(w, crd);
					w->pyraminx.tetraLoc[face][position] =
						w->pyraminx.rowLoc[orient][side][k];
					DrawTriangle(w, face, position, FALSE);
				}
	} else {		/* dir == TR || dir == BL */
		faceOnView = (h < w->pyraminx.size);
		i = (faceOnView == UP) ? w->pyraminx.size - 1 : 0;
		j = h % w->pyraminx.size;
		for (k = 0; k <= len; k++) {
			for (side = 0; side < MAXSIDES; side++)
				if (!side || k < len) {
					s = (side == UP) ? !faceOnView : faceOnView;
					crd.column = i;
					crd.row = j;
					crd.diagonal = i + j + s;
					face = view * MAXSIDES + (crd.diagonal >= w->pyraminx.size);
					position = ToPosition(w, crd);
					w->pyraminx.tetraLoc[face][position] =
						w->pyraminx.rowLoc[orient][side][k];
					DrawTriangle(w, face, position, FALSE);
					if (!side) {
						if (faceOnView == UP) {
							if (j == w->pyraminx.size - 1) {
								view = !view;
								j = 0;
							} else
								++j;
						} else {	/* FACE == DOWN */
							if (!j) {
								view = !view;
								j = w->pyraminx.size - 1;
							} else
								--j;
						}
					}
				}
			if (faceOnView == UP)
				--i;
			else	/* faceOnView == DOWN */
				++i;
		}
	}
}

static void
DrawFrame(PyraminxWidget w, GC gc)
{
	int         startx, starty, lengthx, lengthy, longlength;

	startx = 1 + w->pyraminx.puzzleOffset.x;
	starty = 1 + w->pyraminx.puzzleOffset.y;
	lengthx = w->pyraminx.viewLength - w->pyraminx.delta +
		w->pyraminx.puzzleOffset.x;
	lengthy = w->pyraminx.viewLength - w->pyraminx.delta +
		w->pyraminx.puzzleOffset.y;
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, starty, lengthx, starty);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, starty, startx, lengthy);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, lengthx, starty, lengthx, lengthy);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, lengthy, lengthx, lengthy);
	XDrawLine(XtDisplay(w), XtWindow(w), gc, startx, lengthy, lengthx, starty);
	if (w->pyraminx.vertical) {
		longlength = 2 * w->pyraminx.viewLength - 2 * w->pyraminx.delta - 1 +
			w->pyraminx.puzzleOffset.y;
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx, lengthy, startx, longlength);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  lengthx, lengthy, lengthx, longlength);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx, longlength, lengthx, longlength);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx, longlength, lengthx, lengthy);
	} else {
		longlength = 2 * w->pyraminx.viewLength - 2 * w->pyraminx.delta - 1 +
			w->pyraminx.puzzleOffset.x;
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  lengthx, starty, longlength, starty);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  lengthx, lengthy, longlength, lengthy);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  longlength, starty, longlength, lengthy);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  longlength, starty, lengthx, lengthy);
	}
}

void
DrawAllPolyhedrons(PyraminxWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->pyraminx.sizeSize; position++)
			DrawTriangle(w, face, position, FALSE);
}


static void
DrawTriangle(PyraminxWidget w, int face, int position, int offset)
{
	int         dx, dy, faceOnView, orient, side, view;
	CRD         crd;

	crd = ToCRD(w, face, position);
	view = face / MAXSIDES;
	side = crd.diagonal - crd.column - crd.row;
	if (!w->pyraminx.vertical && view == DOWN) {
		dx = (w->pyraminx.size - crd.column - 1) *
			(w->pyraminx.tetraLength + w->pyraminx.delta) + w->pyraminx.delta;
		dy = (w->pyraminx.size - crd.row - 1) *
			(w->pyraminx.tetraLength + w->pyraminx.delta) + w->pyraminx.delta;
		faceOnView = !side;
		orient = (w->pyraminx.tetraLoc[face][position].rotation +
			  MAXORIENT / 2) % MAXORIENT;
		if (2 * w->pyraminx.size - crd.column - crd.row - 2 + faceOnView >=
		    w->pyraminx.size) {
			dx += w->pyraminx.sideOffset;
			dy += w->pyraminx.sideOffset;
		}
	} else {
		dx = crd.column * (w->pyraminx.tetraLength + w->pyraminx.delta) +
			w->pyraminx.delta;
		dy = crd.row * (w->pyraminx.tetraLength + w->pyraminx.delta) +
			w->pyraminx.delta;
		faceOnView = side;
		orient = w->pyraminx.tetraLoc[face][position].rotation;
		if (crd.column + crd.row + faceOnView >= w->pyraminx.size) {
			dx += w->pyraminx.sideOffset;
			dy += w->pyraminx.sideOffset;
		}
	}
	dx += w->pyraminx.puzzleOffset.x;
	dy += w->pyraminx.puzzleOffset.y;
	if (view == DOWN) {
		if (w->pyraminx.vertical)
			dy += w->pyraminx.viewLength - w->pyraminx.delta - 1;
		else
			dx += w->pyraminx.viewLength - w->pyraminx.delta - 1;
	}
	triangleList[faceOnView][0].x = offsetList[!faceOnView].x + dx;
	triangleList[faceOnView][0].y = offsetList[!faceOnView].y + dy;
	if (offset) {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			     w->pyraminx.borderGC,
		     triangleList[faceOnView], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
		w->pyraminx.faceGC[w->pyraminx.tetraLoc[face][position].face],
			   triangleList[faceOnView], 4, CoordModePrevious);
	} else {
		XFillPolygon(XtDisplay(w), XtWindow(w),
		w->pyraminx.faceGC[w->pyraminx.tetraLoc[face][position].face],
		     triangleList[faceOnView], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			   w->pyraminx.borderGC, triangleList[faceOnView], 4, CoordModePrevious);
	}
	if (w->pyraminx.depth == 1 || w->pyraminx.mono) {
		int         letterX, letterY;
		char        buf[2];

		(void) sprintf(buf, "%c",
			       w->pyraminx.faceName[w->pyraminx.tetraLoc[face][position].face][0]);
		letterX = dx + letterList[!faceOnView].x;
		letterY = dy + letterList[!faceOnView].y;
		XDrawString(XtDisplay(w), XtWindow(w), w->pyraminx.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->pyraminx.orient)
		DrawOrientLine(w, orient, dx, dy, !faceOnView);
}

static void
DrawOrientLine(PyraminxWidget w, int orient, int dx, int dy, int side)
{
	int         x1 = 0, x2 = 0, y1 = 0, y2 = 0;
	int         temp1 = w->pyraminx.tetraLength / 2 + 1;
	int         temp2 = w->pyraminx.tetraLength + 1;
	int         fix = (w->pyraminx.size == 1) ? 1 : 0;

	switch (orient) {
		case TOP:
			x2 = x1 = dx + temp1;
			y1 = (side == UP) ? dy + temp1 + 1 + fix : dy;
			y2 = y1 + w->pyraminx.orientLineLength;
			break;
		case TR:
			x1 = (side == UP) ? dx + temp2 + 2 : dx + temp1;
			x2 = x1 - w->pyraminx.orientDiagLength;
			y1 = (side == UP) ? dy + temp1 : dy - 1;
			y2 = y1 + w->pyraminx.orientDiagLength;
			break;
		case RIGHT:
			x1 = (side == UP) ? dx + temp2 + 1 : dx + temp1 - fix;
			x2 = x1 - w->pyraminx.orientLineLength;
			y2 = y1 = dy + temp1;
			break;
		case BOTTOM:
			x2 = x1 = dx + temp1;
			y1 = (side == UP) ? dy + temp2 + 1 : dy + temp1 - fix;
			y2 = y1 - w->pyraminx.orientLineLength;
			break;
		case BL:
			x1 = (side == UP) ? dx + temp1 : dx - 1;
			x2 = x1 + w->pyraminx.orientDiagLength;
			y1 = (side == UP) ? dy + temp2 + 1 : dy + temp1;
			y2 = y1 - w->pyraminx.orientDiagLength;
			break;
		case LEFT:
			x1 = (side == UP) ? dx + temp1 + 1 + fix : dx;
			x2 = x1 + w->pyraminx.orientLineLength;
			y2 = y1 = dy + temp1;
			break;
		default:
			(void) printf("DrawOrientLine: orient %d\n", orient);
	}
	XDrawLine(XtDisplay(w), XtWindow(w), w->pyraminx.inverseGC, x1, y1, x2, y2);
}

Boolean
CheckSolved(PyraminxWidget w)
{
	int         face, position;
	PyraminxLoc test;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < w->pyraminx.sizeSize; position++) {
			if (!position) {
				test.face = w->pyraminx.tetraLoc[face][position].face;
				test.rotation = w->pyraminx.tetraLoc[face][position].rotation;
			} else if (test.face !=		/*MAXSIDES * view + face */
				 w->pyraminx.tetraLoc[face][position].face ||
				   (w->pyraminx.orient && test.rotation !=
			      w->pyraminx.tetraLoc[face][position].rotation))
				return FALSE;
		}
	return TRUE;
}

static int
Crd(int dir, int I, int J, int side)
{
	if (dir == TOP || dir == BOTTOM)
		return (I);
	else if (dir == RIGHT || dir == LEFT)
		return (J);
	else			/* dir == TR || dir == BL */
		return (I + J + side);
}

static int
Length(PyraminxWidget w, int face, int dir, int h)
{
	if (dir == TR || dir == BL)
		return ((face) ? 2 * w->pyraminx.size - 1 - h : h);
	else
		return ((face) ? h : w->pyraminx.size - 1 - h);
}

static int
CheckMoveDir(PyraminxWidget w, CRD crd1, CRD crd2, int face, int *direction)
{
	int         which = -1, count = 0;
	int         i, *p1, *p2;

	p1 = &(crd1.column);
	p2 = &(crd2.column);
	for (i = 0; i < 3; i++, p1++, p2++)
		if (*p1 == *p2) {
			which = i;
			count++;
		}
	if (count == 1)
		switch (which) {
			case 0:	/* COLUMN */
				*direction = (crd2.row > crd1.row) ? BOTTOM : TOP;
				break;
			case 1:	/* ROW */
				*direction = (crd2.column > crd1.column) ? RIGHT : LEFT;
				break;
			case 2:	/* DIAGONAL */
				*direction = (crd2.column > crd1.column) ? TR : BL;
				break;
		}
	if (!w->pyraminx.vertical && face >= MAXSIDES && *direction > LEFT)
		*direction = (*direction + MAXSIDES) % MAXFACES;
	return count;
}

static      CRD
ToCRD(PyraminxWidget w, int face, int position)
{
	CRD         crd;
	int         diag, diag2;

	diag = Sqrt(position);
	diag2 = diag * diag;
	/* Passing diagonal so there is no sqrt calculation again */
	if (face % 2) {
		crd.diagonal = 2 * w->pyraminx.size - 1 - diag;
		crd.column = w->pyraminx.size - 1 - (position - diag2) / 2;
		crd.row = w->pyraminx.size - 1 - (diag2 + 2 * diag - position) / 2;
	} else {
		crd.diagonal = diag;
		crd.column = (position - diag2) / 2;
		crd.row = (diag2 + 2 * diag - position) / 2;
	}
	return crd;
}

static int
ToPosition(PyraminxWidget w, CRD crd)
{
	int         diag;

	if (crd.diagonal < w->pyraminx.size)
		return (crd.diagonal * crd.diagonal + crd.diagonal + crd.column - crd.row);
	diag = 2 * w->pyraminx.size - 1 - crd.diagonal;
	return (diag * diag + diag + crd.row - crd.column);
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
PrintTetra(PyraminxWidget w)
{
	int         face, position, square;

	for (face = 0; face < MAXSIDES; face++) {
		square = 1;
		for (position = 0; position < w->pyraminx.sizeSize; position++) {
			(void) printf("%d %d  ",
				   w->pyraminx.tetraLoc[face][position].face,
			      w->pyraminx.tetraLoc[face][position].rotation);
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
PrintFace(PyraminxWidget w)
{
	int         g, h, side;

	for (g = 0; g < w->pyraminx.size; g++) {
		for (h = 0; h < w->pyraminx.size - g; h++)
			for (side = 0; side < MAXSIDES; side++)
				if (!side || h < w->pyraminx.size - g - 1)
					(void) printf("%d %d  ", w->pyraminx.faceLoc[side][h +
						  g * w->pyraminx.size].face,
						      w->pyraminx.faceLoc[side][h + g * w->pyraminx.size].rotation);
		(void) printf("\n");
	}
	(void) printf("\n");

#if 0
	int         position;
	int         square = 1;

	for (position = 0; position < w->pyraminx.sizeSize; position++) {
		(void) printf("%d %d  ", w->pyraminx.faceLoc[position].face,
			      w->pyraminx.faceLoc[position].rotation);
		if (position == square * square - 1) {
			(void) printf("\n");
			++square;
		}
	}
	(void) printf("\n");
#endif
}

static void
PrintRow2(PyraminxWidget w, int orient)
{
	int         g, side;

	(void) printf("Row %d:\n", orient);
	for (g = 0; g < w->pyraminx.size; g++)
		for (side = 0; side < MAXSIDES; side++)
			(void) printf("%d %d  ", w->pyraminx.rowLoc[orient][side][g].face,
			       w->pyraminx.rowLoc[orient][side][g].rotation);
	(void) printf("\n");
}

static void
PrintRow3(PyraminxWidget w, int len, int orient)
{
	int         g, side;

	(void) printf("Row %d:\n", orient);
	for (g = 0; g <= len; g++)
		for (side = 0; side < MAXSIDES; side++)
			if (!side || g < len)
				(void) printf("%d %d  ", w->pyraminx.rowLoc[orient][side][g].face,
				w->pyraminx.rowLoc[orient][side][g].rotation);
	(void) printf("\n");
}

#endif
