/*-
# X-BASED SKEWB
#
#  Skewb2d.c
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

/* Methods file for Skewb2d */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "SkewbP.h"
#include "Skewb2dP.h"

static void InitializeSkewb2D(Widget request, Widget new);
static void ResizeSkewb2D(Skewb2DWidget w);
static void ExposeSkewb2D(Widget new, XEvent * event, Region region);
static Boolean SetValuesSkewb2D(Widget current, Widget request, Widget new);
static void MoveSkewb2DTl(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb2DTop(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb2DTr(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb2DLeft(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb2dRight(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb2DBl(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb2DBottom(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb2DBr(Skewb2DWidget w, XEvent * event, char **args, int nArgs);
static void ResizePolyhedrons(Skewb2DWidget w);
static void DrawFrame(Skewb2DWidget w, GC gc);
static void DrawOrientLine(Skewb2DWidget w, int cube, int orient, int dx, int dy);

static char defaultTranslationsSkewb2D[] =
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
   <KeyPress>R1: MoveBl()\n\
   <KeyPress>Down: MoveBottom()\n\
   <KeyPress>KP_2: MoveBottom()\n\
   <KeyPress>R14: MoveBottom()\n\
   <KeyPress>Next: MoveBr()\n\
   <KeyPress>KP_3: MoveBr()\n\
   <KeyPress>R3: MoveBr()\n\
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
   <KeyPress>o: Orientize()";

static XtActionsRec actionsListSkewb2D[] =
{
	{"Quit", (XtActionProc) QuitSkewb},
	{"MoveCcw", (XtActionProc) MoveSkewbCcw},
	{"MoveTl", (XtActionProc) MoveSkewb2DTl},
	{"MoveTop", (XtActionProc) MoveSkewb2DTop},
	{"MoveTr", (XtActionProc) MoveSkewb2DTr},
	{"MoveLeft", (XtActionProc) MoveSkewb2DLeft},
	{"MoveCw", (XtActionProc) MoveSkewbCw},
	{"MoveRight", (XtActionProc) MoveSkewb2dRight},
	{"MoveBl", (XtActionProc) MoveSkewb2DBl},
	{"MoveBottom", (XtActionProc) MoveSkewb2DBottom},
	{"MoveBr", (XtActionProc) MoveSkewb2DBr},
	{"Select", (XtActionProc) SelectSkewb},
	{"Release", (XtActionProc) ReleaseSkewb},
	{"Practice", (XtActionProc) PracticeSkewb},
	{"PracticeMaybe", (XtActionProc) PracticeSkewbMaybe},
	{"Randomize", (XtActionProc) RandomizeSkewb},
	{"RandomizeMaybe", (XtActionProc) RandomizeSkewbMaybe},
	{"Get", (XtActionProc) GetSkewb},
	{"Write", (XtActionProc) WriteSkewb},
	{"Undo", (XtActionProc) UndoSkewb},
	{"Solve", (XtActionProc) SolveSkewb},
	{"Orientize", (XtActionProc) OrientizeSkewb}
};

static XtResource resourcesSkewb2D[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(SkewbWidget, skewb.username), XtRString, "nobody"},
	{XtNfaceColor0, XtCLabel, XtRString, sizeof (String),
	 XtOffset(SkewbWidget, skewb.faceName[0]), XtRString, "Red"},
	{XtNfaceColor1, XtCLabel, XtRString, sizeof (String),
	 XtOffset(SkewbWidget, skewb.faceName[1]), XtRString, "Blue"},
	{XtNfaceColor2, XtCLabel, XtRString, sizeof (String),
	 XtOffset(SkewbWidget, skewb.faceName[2]), XtRString, "White"},
	{XtNfaceColor3, XtCLabel, XtRString, sizeof (String),
	 XtOffset(SkewbWidget, skewb.faceName[3]), XtRString, "Green"},
	{XtNfaceColor4, XtCLabel, XtRString, sizeof (String),
	 XtOffset(SkewbWidget, skewb.faceName[4]), XtRString, "Pink"},
	{XtNfaceColor5, XtCLabel, XtRString, sizeof (String),
	 XtOffset(SkewbWidget, skewb.faceName[5]), XtRString, "Yellow"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
    XtOffset(SkewbWidget, skewb.foreground), XtRString, XtDefaultForeground},
	{XtNpieceBorder, XtCColor, XtRPixel, sizeof (Pixel),
   XtOffset(SkewbWidget, skewb.borderColor), XtRString, XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(SkewbWidget, core.width), XtRString, "300"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(SkewbWidget, core.height), XtRString, "400"},
	{XtNorient, XtCOrient, XtRBoolean, sizeof (Boolean),
	 XtOffset(SkewbWidget, skewb.orient), XtRString, "FALSE"},	/* DEFAULTORIENT */
	{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
	 XtOffset(SkewbWidget, skewb.mono), XtRString, "FALSE"},
	{XtNface, XtCFace, XtRInt, sizeof (int),
	 XtOffset(SkewbWidget, skewb.currentFace), XtRString, "-1"},
	{XtNpos, XtCPos, XtRInt, sizeof (int),
	 XtOffset(SkewbWidget, skewb.currentPosition), XtRString, "-1"},
	{XtNdirection, XtCDirection, XtRInt, sizeof (int),
	 XtOffset(SkewbWidget, skewb.currentDirection), XtRString, "-1"},
	{XtNpractice, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(SkewbWidget, skewb.practice), XtRString, "FALSE"},
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(SkewbWidget, skewb.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(SkewbWidget, skewb.select), XtRCallback, NULL}
};

Skewb2DClassRec skewb2dClassRec =
{
	{
		(WidgetClass) & skewbClassRec,	/* superclass */
		"Skewb2D",	/* class name */
		sizeof (Skewb2DRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeSkewb2D,		/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListSkewb2D,	/* actions */
		XtNumber(actionsListSkewb2D),	/* num actions */
		resourcesSkewb2D,	/* resources */
		XtNumber(resourcesSkewb2D),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		NULL,		/* destroy */
		(XtWidgetProc) ResizeSkewb2D,	/* resize */
		(XtExposeProc) ExposeSkewb2D,	/* expose */
		(XtSetValuesFunc) SetValuesSkewb2D,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		XtInheritAcceptFocus,	/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsSkewb2D,	/* tm table */
		NULL,		/* query geometry */
		NULL,		/* display accelerator */
		NULL		/* extension */
	},
	{
		0		/* ignore */
	},
	{
		0		/* ignore */
	}
};

WidgetClass skewb2dWidgetClass = (WidgetClass) & skewb2dClassRec;

static int  planeToCube[MAXRECT] =
{6, 0, 6, 1, 2, 3, 6, 4, 6, 6, 5, 6};
static int  cubeToPlane[MAXFACES] =
{1, 3, 4, 5, 7, 10};
static XPoint diamondUnit[MAXORIENT + 1] =
{
	{0, 1},
	{1, -1},
	{1, 1},
	{-1, 1},
	{-1, -1}},  triangleUnit[MAXORIENT][4] =
{
	{
		{2, 0},
		{1, 0},
		{0, 1},
		{-1, -1}},
	{
		{3, 2},
		{0, 1},
		{-1, 0},
		{1, -1}},
	{
		{1, 3},
		{-1, 0},
		{0, -1},
		{1, 1}},
	{
		{0, 1},
		{0, -1},
		{1, 0},
		{-1, 1}}
},          letterUnit[MAXCUBES] =
{
	{2, 0},
	{2, 2},
	{0, 2},
	{0, 0},
	{1, 1}};
static XPoint diamondList[MAXORIENT + 1], triangleList[MAXORIENT][4], letterList[MAXCUBES],
            offsetList[MAXCUBES];

static void
InitializeSkewb2D(Widget request, Widget new)
{
	Skewb2DWidget w = (Skewb2DWidget) new;

	w->skewb.dim = 2;
	ResizeSkewb2D(w);
}

static void
ResizeSkewb2D(Skewb2DWidget w)
{
	w->skewb.delta = 3;
	w->skewb.vertical = (w->core.height >= w->core.width);
	if (w->skewb.vertical)
		w->skewb2d.faceLength = MIN(w->core.height / MAXY, w->core.width / MAXX);
	else
		w->skewb2d.faceLength = MIN(w->core.height / MAXX, w->core.width / MAXY);
	w->skewb2d.faceLength = MAX(w->skewb2d.faceLength - w->skewb.delta - 1, 0);
	w->skewb2d.diamondLength = w->skewb2d.faceLength - w->skewb.delta;
	w->skewb2d.viewLength = w->skewb2d.faceLength + w->skewb.delta;
	if (w->skewb.vertical) {
		w->skewb.puzzleSize.x = MAXX * (w->skewb2d.viewLength - 1) +
			w->skewb.delta;
		w->skewb.puzzleSize.y = MAXY * (w->skewb2d.viewLength - 1) +
			w->skewb.delta;
	} else {
		w->skewb.puzzleSize.x = MAXY * (w->skewb2d.viewLength - 1) +
			w->skewb.delta;
		w->skewb.puzzleSize.y = MAXX * (w->skewb2d.viewLength - 1) +
			w->skewb.delta;
	}
	w->skewb.puzzleOffset.x = ((int) w->core.width - w->skewb.puzzleSize.x)
		/ 2;
	w->skewb.puzzleOffset.y = ((int) w->core.height - w->skewb.puzzleSize.y)
		/ 2;
	ResizePolyhedrons(w);
}

static void
ExposeSkewb2D(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	Skewb2DWidget w = (Skewb2DWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->skewb.puzzleGC);
		DrawAllPolyhedrons((SkewbWidget) w);
	}
}

static      Boolean
SetValuesSkewb2D(Widget current, Widget request, Widget new)
{
	Skewb2DWidget c = (Skewb2DWidget) current, w = (Skewb2DWidget) new;
	Boolean     redraw = FALSE;

	if (w->skewb2d.diamondLength != c->skewb2d.diamondLength) {
		ResizeSkewb2D(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
MoveSkewb2DTl(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, TL,
		       (int) (event->xkey.state & ControlMask), FALSE);
}

static void
MoveSkewb2DTop(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, TOP,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb2DTr(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, TR,
		       (int) (event->xkey.state & ControlMask), FALSE);
}

static void
MoveSkewb2DLeft(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, LEFT,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb2dRight(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, RIGHT,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb2DBl(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, BL,
		       (int) (event->xkey.state & ControlMask), FALSE);
}

static void
MoveSkewb2DBottom(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, BOTTOM,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb2DBr(Skewb2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, BR,
		       (int) (event->xkey.state & ControlMask), FALSE);
}

static void
ResizePolyhedrons(Skewb2DWidget w)
{
	int         i, j;

	w->skewb.orientLineLength = w->skewb2d.diamondLength / 8;
	w->skewb.letterOffset.x = -2;
	w->skewb.letterOffset.y = 4;
	for (i = 0; i <= MAXORIENT; i++) {
		diamondList[i].x = diamondUnit[i].x * (w->skewb2d.diamondLength / 2 -
						       w->skewb.delta);
		diamondList[i].y = diamondUnit[i].y * (w->skewb2d.diamondLength / 2 -
						       w->skewb.delta);
	}
	for (i = 0; i < MAXORIENT; i++) {
		for (j = 0; j <= 3; j++) {
			triangleList[i][j].x = triangleUnit[i][j].x *
				(w->skewb2d.diamondLength / 2 - 3 * w->skewb.delta);
			triangleList[i][j].y = triangleUnit[i][j].y *
				(w->skewb2d.diamondLength / 2 - 3 * w->skewb.delta);
		}
	}

	for (i = 0; i < MAXORIENT; i++) {
		if (letterUnit[i].x == 0)
			letterList[i].x = w->skewb2d.diamondLength / 8 +
				w->skewb.letterOffset.x;
		else if (letterUnit[i].x == 2)
			letterList[i].x = 7 * w->skewb2d.diamondLength / 8 - 2 +
				w->skewb.letterOffset.x;
		if (letterUnit[i].y == 0)
			letterList[i].y = w->skewb2d.diamondLength / 8 + 2 +
				w->skewb.letterOffset.y;
		else if (letterUnit[i].y == 2)
			letterList[i].y = 7 * w->skewb2d.diamondLength / 8 - 3 +
				w->skewb.letterOffset.y;

		if (triangleUnit[i][0].x == 0)
			offsetList[i].x = w->skewb.delta - 1;
		else if (triangleUnit[i][0].x == 1)
			offsetList[i].x = w->skewb2d.diamondLength / 2 -
				2 * w->skewb.delta - 1;
		else if (triangleUnit[i][0].x == 2)
			offsetList[i].x = w->skewb2d.diamondLength / 2 +
				2 * w->skewb.delta;
		else if (triangleUnit[i][0].x == 3)
			offsetList[i].x = w->skewb2d.diamondLength - w->skewb.delta - 1;
		if (triangleUnit[i][0].y == 0)
			offsetList[i].y = w->skewb.delta - 1;
		else if (triangleUnit[i][0].y == 1)
			offsetList[i].y = w->skewb2d.diamondLength / 2 -
				2 * w->skewb.delta - 1;
		else if (triangleUnit[i][0].y == 2)
			offsetList[i].y = w->skewb2d.diamondLength / 2 +
				2 * w->skewb.delta - 1;
		else if (triangleUnit[i][0].y == 3)
			offsetList[i].y = w->skewb2d.diamondLength - w->skewb.delta - 2;
	}
	if (diamondUnit[0].x == 0)
		offsetList[MAXORIENT].x = w->skewb.delta - 2;
	else if (diamondUnit[0].x == 1)
		offsetList[MAXORIENT].x = w->skewb2d.diamondLength / 2 - 1;
	if (diamondUnit[0].y == 0)
		offsetList[MAXORIENT].y = w->skewb.delta - 2;
	else if (diamondUnit[0].y == 1)
		offsetList[MAXORIENT].y = w->skewb2d.diamondLength / 2 - 2;
	if (letterUnit[MAXORIENT].x == 1)
		letterList[MAXORIENT].x = w->skewb2d.diamondLength / 2 - 2 +
			w->skewb.letterOffset.x;
	if (letterUnit[MAXORIENT].y == 1)
		letterList[MAXORIENT].y = w->skewb2d.diamondLength / 2 - 2 +
			w->skewb.letterOffset.y;
}

int
SelectPolyhedrons2D(Skewb2DWidget w, int x, int y, int *face, int *position)
{
	int         faceX, faceY, i, j;

	x -= w->skewb.puzzleOffset.x;
	y -= w->skewb.puzzleOffset.y;
	faceX = x / w->skewb2d.viewLength;
	faceY = y / w->skewb2d.viewLength;
	i = x - faceX * w->skewb2d.viewLength;
	j = y - faceY * w->skewb2d.viewLength;
	if (i - j > w->skewb2d.viewLength / 2 - 3)
		*position = TR;
	else if (i + j > 3 * w->skewb2d.viewLength / 2)
		*position = BR;
	else if (j - i > w->skewb2d.viewLength / 2 - 2)
		*position = BL;
	else if (i + j < w->skewb2d.viewLength / 2 + 7)
		*position = TL;
	else
		*position = MAXORIENT;
	if ((faceX != 1 && faceY != 1) ||
	    (faceX >= 3 && w->skewb.vertical) ||
	    (faceY >= 3 && !w->skewb.vertical))
		return FALSE;
	*face = planeToCube[faceX + faceY * MAXX];
	if (faceX == 3) {
		*face = MAXFACES - 1;
		if (*position != MAXORIENT)
			*position = (*position + HALF) % MAXORIENT;
	}
	return TRUE;
}

int
NarrowSelection2D(Skewb2DWidget w, int *face, int *position, int *direction)
{
	if (*face == MAXFACES - 1 && *direction < MAXORIENT && !w->skewb.vertical) {
		if (*direction < MAXORIENT)
			*direction = (*direction + HALF) % MAXORIENT;
		else if (*direction >= 2 * MAXORIENT)
			*direction = 2 * MAXORIENT + (*direction + HALF) % MAXORIENT;
	}
	if (*position != MAXORIENT) {
		if (*direction == CW)
			*direction = (*position + 1) % MAXORIENT;
		else if (*direction == CCW)
			*direction = (*position + 3) % MAXORIENT;
		else if (*direction < MAXORIENT && !((*direction + *position) % 2))
			return FALSE;
	}
	return TRUE;
}

static void
DrawFrame(Skewb2DWidget w, GC gc)
{
	int         i;
	XPoint      pos[MAXXY + 1], letters;

	for (i = 0; i <= MAXXY; i++) {
		pos[i].x = i * w->skewb2d.viewLength + w->skewb.puzzleOffset.x;
		pos[i].y = i * w->skewb2d.viewLength + w->skewb.puzzleOffset.y;
	}
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[1].x, pos[0].y, pos[2].x, pos[0].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[3].x, pos[1].y, pos[3].x, pos[2].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[1].x, pos[3].y, pos[2].x, pos[3].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[0].x, pos[1].y, pos[0].x, pos[2].y);
	letters.x = pos[0].x + w->skewb2d.viewLength / 2 - w->skewb.delta;
	letters.y = pos[0].y + w->skewb2d.viewLength / 2;
	XDrawString(XtDisplay(w), XtWindow(w), gc,
		    (int) (letters.x + 5 * w->skewb.letterOffset.x),
		    (int) (letters.y + w->skewb.letterOffset.y), "Front", 5);
	letters.x = pos[2].x + w->skewb2d.viewLength / 2 - w->skewb.delta;
	letters.y = pos[2].y + w->skewb2d.viewLength / 2;
	XDrawString(XtDisplay(w), XtWindow(w), gc,
		    (int) (letters.x + 4 * w->skewb.letterOffset.x),
		    (int) (letters.y + w->skewb.letterOffset.y), "Back", 4);
	if (w->skewb.vertical) {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[1].x, pos[0].y, pos[1].x, pos[4].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[2].x, pos[0].y, pos[2].x, pos[4].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[0].x, pos[1].y, pos[3].x, pos[1].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[0].x, pos[2].y, pos[3].x, pos[2].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[1].x, pos[4].y, pos[2].x, pos[4].y);
	} else {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[0].x, pos[1].y, pos[4].x, pos[1].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[0].x, pos[2].y, pos[4].x, pos[2].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[1].x, pos[0].y, pos[1].x, pos[3].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[2].x, pos[0].y, pos[2].x, pos[3].y);
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  pos[4].x, pos[1].y, pos[4].x, pos[2].y);
	}
}

void
DrawDiamond2D(Skewb2DWidget w, int face)
{
	int         dx, dy, orient;

	orient = w->skewb.cubeLoc[face][MAXORIENT].rotation;
	if (w->skewb.vertical || face != MAXFACES - 1) {
		dx = (cubeToPlane[face] % MAXX) * w->skewb2d.viewLength +
			w->skewb.delta;
		dy = (cubeToPlane[face] / MAXX) * w->skewb2d.viewLength +
			w->skewb.delta;
	} else {
		dx = (cubeToPlane[face] / MAXX) * w->skewb2d.viewLength +
			w->skewb.delta;
		dy = (cubeToPlane[face] % MAXX) * w->skewb2d.viewLength +
			w->skewb.delta;
		orient = (orient + HALF) % STRT;
	}
	dx += w->skewb.puzzleOffset.x + w->skewb.delta;
	dy += w->skewb.puzzleOffset.y + w->skewb.delta;
	diamondList[0].x = offsetList[MAXORIENT].x + dx;
	diamondList[0].y = offsetList[MAXORIENT].y + dy;
	XFillPolygon(XtDisplay(w), XtWindow(w),
		     w->skewb.faceGC[w->skewb.cubeLoc[face][MAXORIENT].face],
		     diamondList, 4, Convex, CoordModePrevious);
	XDrawLines(XtDisplay(w), XtWindow(w),
		   w->skewb.borderGC, diamondList, 5, CoordModePrevious);
	if (w->skewb.depth == 1 || w->skewb.mono) {
		int         letterX, letterY;
		char        buf[2];

		(void) sprintf(buf, "%c",
		w->skewb.faceName[w->skewb.cubeLoc[face][MAXORIENT].face][0]);
		letterX = dx + letterList[MAXORIENT].x;
		letterY = dy + letterList[MAXORIENT].y;
		XDrawString(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->skewb.orient)
		DrawOrientLine(w, MAXORIENT, orient, dx, dy);
}

void
DrawTriangle2D(Skewb2DWidget w, int face, int position, int offset)
{
	int         dx, dy, letterX, letterY, orient, newCorner;

	orient = w->skewb.cubeLoc[face][position].rotation;
	if (w->skewb.vertical || face != MAXFACES - 1) {
		dx = (cubeToPlane[face] % MAXX) * w->skewb2d.viewLength +
			w->skewb.delta - 1;
		dy = (cubeToPlane[face] / MAXX) * w->skewb2d.viewLength +
			w->skewb.delta - 1;
		newCorner = position;
	} else {
		dx = (cubeToPlane[face] / MAXX) * w->skewb2d.viewLength +
			w->skewb.delta - 1;
		dy = (cubeToPlane[face] % MAXX) * w->skewb2d.viewLength +
			w->skewb.delta - 1;
		newCorner = (position + HALF) % STRT;
		orient = (orient + HALF) % STRT;
	}
	dx += w->skewb.puzzleOffset.x + w->skewb.delta;
	dy += w->skewb.puzzleOffset.y + w->skewb.delta;
	letterX = dx + letterList[newCorner].x;
	letterY = dy + letterList[newCorner].y;
	triangleList[newCorner][0].x = offsetList[newCorner].x + dx;
	triangleList[newCorner][0].y = offsetList[newCorner].y + dy;
	if (offset) {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			     w->skewb.borderGC,
		      triangleList[newCorner], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
		      w->skewb.faceGC[w->skewb.cubeLoc[face][position].face],
			   triangleList[newCorner], 4, CoordModePrevious);
	} else {
		XFillPolygon(XtDisplay(w), XtWindow(w),
		      w->skewb.faceGC[w->skewb.cubeLoc[face][position].face],
		      triangleList[newCorner], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			   w->skewb.borderGC, triangleList[newCorner], 4, CoordModePrevious);
	}
	if (w->skewb.depth == 1 || w->skewb.mono) {
		char        buf[2];

		(void) sprintf(buf, "%c",
		w->skewb.faceName[w->skewb.cubeLoc[face][position].face][0]);
		XDrawString(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->skewb.orient)
		DrawOrientLine(w, newCorner, orient,
			       letterX - w->skewb.letterOffset.x, letterY - w->skewb.letterOffset.y);
}

static void
DrawOrientLine(Skewb2DWidget w, int cube, int orient, int dx, int dy)
{
	if (cube == MAXORIENT)
		switch (orient) {
			case TR:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
				       dx + w->skewb2d.diamondLength / 2 - 2,
					  dy + w->skewb.delta - 2,
				       dx + w->skewb2d.diamondLength / 2 - 2,
					  dy + w->skewb.delta + w->skewb.orientLineLength);
				return;
			case BR:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
					  dx + w->skewb2d.diamondLength - 2,
				       dy + w->skewb2d.diamondLength / 2 - 2,
					  dx + w->skewb2d.diamondLength -
					  w->skewb.orientLineLength - 7,
				      dy + w->skewb2d.diamondLength / 2 - 2);
				return;
			case BL:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
				       dx + w->skewb2d.diamondLength / 2 - 2,
					  dy + w->skewb2d.diamondLength - 2,
				       dx + w->skewb2d.diamondLength / 2 - 2,
					  dy + w->skewb2d.diamondLength -
					  w->skewb.orientLineLength - 7);
				return;
			case TL:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
					  dx + w->skewb.delta - 2,
				       dy + w->skewb2d.diamondLength / 2 - 2,
					  dx + w->skewb.delta + w->skewb.orientLineLength,
				      dy + w->skewb2d.diamondLength / 2 - 2);
				return;
			default:
				(void) printf("DrawOrientLine: orient %d\n", orient);
	} else			/* cube != MAXORIENT */
		switch (orient) {
			case TR:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
					  dx,
					  dy - w->skewb.delta,
					  dx,
					  dy - w->skewb.delta - w->skewb.orientLineLength / 2);
				return;
			case BR:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
					  dx + w->skewb.delta,
					  dy,
					  dx + w->skewb.delta + w->skewb.orientLineLength / 2,
					  dy);
				return;
			case BL:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
					  dx,
					  dy + w->skewb.delta,
					  dx,
					  dy + w->skewb.delta + w->skewb.orientLineLength / 2);
				return;
			case TL:
				XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
					  dx - w->skewb.delta,
					  dy,
					  dx - w->skewb.delta - w->skewb.orientLineLength / 2,
					  dy);
				return;
			default:
				(void) printf("DrawOrientLine: orient %d\n", orient);
		}
}
