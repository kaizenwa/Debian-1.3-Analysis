/*-
# X-BASED DINOSAUR CUBE
#
#  Dino2d.c
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

/* Methods file for Dino2d */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "DinoP.h"
#include "Dino2dP.h"

static void InitializeDino2D(Widget request, Widget new);
static void ResizeDino2D(Dino2DWidget w);
static void ExposeDino2D(Widget new, XEvent * event, Region region);
static Boolean SetValuesDino2D(Widget current, Widget request, Widget new);
static void MoveDino2DTl(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino2DTop(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino2DTr(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino2DLeft(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino2DRight(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino2DBl(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino2DBottom(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino2DBr(Dino2DWidget w, XEvent * event, char **args, int nArgs);
static void ResizePolyhedrons(Dino2DWidget w);
static void DrawFrame(Dino2DWidget w, GC gc);
static void DrawOrientLine(Dino2DWidget w, int orient, int dx, int dy);

static char defaultTranslationsDino2D[] =
"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
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
   <KeyPress>o: Orientize()\n\
   <KeyPress>2: Period2()\n\
   <KeyPress>3: Period3()\n\
   <KeyPress>b: Both()";

static XtActionsRec actionsListDino2D[] =
{
	{"Quit", (XtActionProc) QuitDino},
	{"MoveTl", (XtActionProc) MoveDino2DTl},
	{"MoveTop", (XtActionProc) MoveDino2DTop},
	{"MoveTr", (XtActionProc) MoveDino2DTr},
	{"MoveLeft", (XtActionProc) MoveDino2DLeft},
	{"MoveRight", (XtActionProc) MoveDino2DRight},
	{"MoveBl", (XtActionProc) MoveDino2DBl},
	{"MoveBottom", (XtActionProc) MoveDino2DBottom},
	{"MoveBr", (XtActionProc) MoveDino2DBr},
	{"Select", (XtActionProc) SelectDino},
	{"Release", (XtActionProc) ReleaseDino},
	{"Practice", (XtActionProc) PracticeDino},
	{"PracticeMaybe", (XtActionProc) PracticeDinoMaybe},
	{"Randomize", (XtActionProc) RandomizeDino},
	{"RandomizeMaybe", (XtActionProc) RandomizeDinoMaybe},
	{"Get", (XtActionProc) GetDino},
	{"Write", (XtActionProc) WriteDino},
	{"Undo", (XtActionProc) UndoDino},
	{"Solve", (XtActionProc) SolveDino},
	{"Orientize", (XtActionProc) OrientizeDino},
	{"Period2", (XtActionProc) Period2ModeDino},
	{"Period3", (XtActionProc) Period3ModeDino},
	{"Both", (XtActionProc) BothModeDino}
};

static XtResource resourcesDino2D[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(DinoWidget, dino.username), XtRString, "nobody"},
	{XtNfaceColor0, XtCLabel, XtRString, sizeof (String),
	 XtOffset(DinoWidget, dino.faceName[0]), XtRString, "Orange"},
	{XtNfaceColor1, XtCLabel, XtRString, sizeof (String),
	 XtOffset(DinoWidget, dino.faceName[1]), XtRString, "Pink"},
	{XtNfaceColor2, XtCLabel, XtRString, sizeof (String),
	 XtOffset(DinoWidget, dino.faceName[2]), XtRString, "White"},
	{XtNfaceColor3, XtCLabel, XtRString, sizeof (String),
	 XtOffset(DinoWidget, dino.faceName[3]), XtRString, "Violet"},
	{XtNfaceColor4, XtCLabel, XtRString, sizeof (String),
	 XtOffset(DinoWidget, dino.faceName[4]), XtRString, "Yellow"},
	{XtNfaceColor5, XtCLabel, XtRString, sizeof (String),
	 XtOffset(DinoWidget, dino.faceName[5]), XtRString, "Blue"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
      XtOffset(DinoWidget, dino.foreground), XtRString, XtDefaultForeground},
	{XtNpieceBorder, XtCColor, XtRPixel, sizeof (Pixel),
     XtOffset(DinoWidget, dino.borderColor), XtRString, XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(DinoWidget, core.width), XtRString, "300"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(DinoWidget, core.height), XtRString, "400"},
	{XtNmode, XtCMode, XtRInt, sizeof (int),
	 XtOffset(DinoWidget, dino.mode), XtRString, "3"},	/*DEFAULTMODE */
	{XtNorient, XtCOrient, XtRBoolean, sizeof (Boolean),
	 XtOffset(DinoWidget, dino.orient), XtRString, "FALSE"},	/* DEFAULTORIENT */
	{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
	 XtOffset(DinoWidget, dino.mono), XtRString, "FALSE"},
	{XtNface, XtCFace, XtRInt, sizeof (int),
	 XtOffset(DinoWidget, dino.currentFace), XtRString, "-1"},
	{XtNpos, XtCPos, XtRInt, sizeof (int),
	 XtOffset(DinoWidget, dino.currentPosition), XtRString, "-1"},
	{XtNdirection, XtCDirection, XtRInt, sizeof (int),
	 XtOffset(DinoWidget, dino.currentDirection), XtRString, "-1"},
	{XtNstyle, XtCStyle, XtRInt, sizeof (int),
	 XtOffset(DinoWidget, dino.style), XtRString, "-1"},
	{XtNpractice, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(DinoWidget, dino.practice), XtRString, "FALSE"},
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(DinoWidget, dino.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(DinoWidget, dino.select), XtRCallback, NULL}
};

Dino2DClassRec dino2dClassRec =
{
	{
		(WidgetClass) & dinoClassRec,	/* superclass */
		"Dino2D",	/* class name */
		sizeof (Dino2DRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeDino2D,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListDino2D,	/* actions */
		XtNumber(actionsListDino2D),	/* num actions */
		resourcesDino2D,	/* resources */
		XtNumber(resourcesDino2D),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		NULL,		/* destroy */
		(XtWidgetProc) ResizeDino2D,	/* resize */
		(XtExposeProc) ExposeDino2D,	/* expose */
		(XtSetValuesFunc) SetValuesDino2D,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		XtInheritAcceptFocus,	/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsDino2D,	/* tm table */
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

WidgetClass dino2dWidgetClass = (WidgetClass) & dino2dClassRec;

static int  planeToCube[MAXRECT] =
{6, 0, 6, 1, 2, 3, 6, 4, 6, 6, 5, 6};
static int  cubeToPlane[MAXFACES] =
{1, 3, 4, 5, 7, 10};
static XPoint triangleUnit[MAXORIENT][4] =
{
	{
		{2, 0},
		{-1, 1},
		{-1, -1},
		{2, 0}},
	{
		{3, 2},
		{-1, -1},
		{1, -1},
		{0, 2}},
	{
		{1, 3},
		{1, -1},
		{1, 1},
		{-2, 0}},
	{
		{0, 1},
		{1, 1},
		{-1, 1},
		{0, -2}}
},          letterUnit[MAXORIENT] =
{
	{1, 0},
	{2, 1},
	{1, 2},
	{0, 1}};
static XPoint triangleList[MAXORIENT][4], letterList[MAXORIENT], offsetList[MAXORIENT];

static void
InitializeDino2D(Widget request, Widget new)
{
	Dino2DWidget w = (Dino2DWidget) new;

	w->dino.dim = 2;
	ResizeDino2D(w);
}

static void
ResizeDino2D(Dino2DWidget w)
{
	w->dino.delta = 2;
	w->dino.vertical = (w->core.height >= w->core.width);
	if (w->dino.vertical)
		w->dino2d.faceLength = MIN(w->core.height / MAXY, w->core.width / MAXX);
	else
		w->dino2d.faceLength = MIN(w->core.height / MAXX, w->core.width / MAXY);
	w->dino2d.faceLength = MAX(w->dino2d.faceLength - w->dino.delta - 1, 0);
	w->dino2d.diamondLength = w->dino2d.faceLength - w->dino.delta;
	w->dino2d.viewLength = w->dino2d.faceLength + w->dino.delta;
	if (w->dino.vertical) {
		w->dino.puzzleSize.x = MAXX * (w->dino2d.viewLength - 1) +
			w->dino.delta;
		w->dino.puzzleSize.y = MAXY * (w->dino2d.viewLength - 1) +
			w->dino.delta;
	} else {
		w->dino.puzzleSize.x = MAXY * (w->dino2d.viewLength - 1) +
			w->dino.delta;
		w->dino.puzzleSize.y = MAXX * (w->dino2d.viewLength - 1) +
			w->dino.delta;
	}
	w->dino.puzzleOffset.x = ((int) w->core.width - w->dino.puzzleSize.x)
		/ 2;
	w->dino.puzzleOffset.y = ((int) w->core.height - w->dino.puzzleSize.y)
		/ 2;
	ResizePolyhedrons(w);
}

static void
ExposeDino2D(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	Dino2DWidget w = (Dino2DWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->dino.puzzleGC);
		DrawAllPolyhedrons((DinoWidget) w);
	}
}

static      Boolean
SetValuesDino2D(Widget current, Widget request, Widget new)
{
	Dino2DWidget c = (Dino2DWidget) current, w = (Dino2DWidget) new;
	Boolean     redraw = FALSE;

	if (w->dino2d.diamondLength != c->dino2d.diamondLength) {
		ResizeDino2D(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
MoveDino2DTl(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, TL,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino2DTop(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, TOP,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino2DTr(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, TR,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino2DLeft(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, LEFT,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino2DRight(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, RIGHT,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino2DBl(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, BL,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino2DBottom(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, BOTTOM,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino2DBr(Dino2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, BR,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
ResizePolyhedrons(Dino2DWidget w)
{
	int         i, j;

	w->dino.orientLineLength = w->dino2d.diamondLength / 3;
	w->dino.letterOffset.x = -2;
	w->dino.letterOffset.y = 4;
	for (i = 0; i < MAXORIENT; i++) {
		for (j = 0; j <= 3; j++) {
			triangleList[i][j].x = triangleUnit[i][j].x *
				(w->dino2d.diamondLength / 2 - 2 * w->dino.delta);
			triangleList[i][j].y = triangleUnit[i][j].y *
				(w->dino2d.diamondLength / 2 - 2 * w->dino.delta);
		}
		if (letterUnit[i].x == 0)
			letterList[i].x = w->dino2d.diamondLength / 5 +
				w->dino.letterOffset.x;
		else if (letterUnit[i].x == 1)
			letterList[i].x = w->dino2d.diamondLength / 2 - 1 +
				w->dino.letterOffset.x;
		else if (letterUnit[i].x == 2)
			letterList[i].x = 4 * w->dino2d.diamondLength / 5 - 2 +
				w->dino.letterOffset.x;
		if (letterUnit[i].y == 0)
			letterList[i].y = w->dino2d.diamondLength / 5 + 2 +
				w->dino.letterOffset.y;
		else if (letterUnit[i].y == 1)
			letterList[i].y = w->dino2d.diamondLength / 2 - 1 +
				w->dino.letterOffset.y;
		else if (letterUnit[i].y == 2)
			letterList[i].y = 4 * w->dino2d.diamondLength / 5 - 3 +
				w->dino.letterOffset.y;

		if (triangleUnit[i][0].x == 0)
			offsetList[i].x = w->dino.delta - 1;
		else if (triangleUnit[i][0].x == 1)
			offsetList[i].x = 2 * w->dino.delta - 1;
		else if (triangleUnit[i][0].x == 2)
			offsetList[i].x = w->dino2d.diamondLength -
				2 * w->dino.delta - 2;
		else if (triangleUnit[i][0].x == 3)
			offsetList[i].x = w->dino2d.diamondLength - w->dino.delta - 2;
		if (triangleUnit[i][0].y == 0)
			offsetList[i].y = w->dino.delta - 2;
		else if (triangleUnit[i][0].y == 1)
			offsetList[i].y = 2 * w->dino.delta - 1;
		else if (triangleUnit[i][0].y == 2)
			offsetList[i].y = w->dino2d.diamondLength -
				2 * w->dino.delta - 2;
		else if (triangleUnit[i][0].y == 3)
			offsetList[i].y = w->dino2d.diamondLength - w->dino.delta - 1;
	}
}

int
SelectPolyhedrons2D(Dino2DWidget w, int x, int y, int *face, int *position)
{
	int         faceX, faceY, i, j;

	x -= w->dino.puzzleOffset.x;
	y -= w->dino.puzzleOffset.y;
	faceX = x / w->dino2d.viewLength;
	faceY = y / w->dino2d.viewLength;
	i = x - faceX * w->dino2d.viewLength;
	j = y - faceY * w->dino2d.viewLength;
	if (i + j < w->dino2d.viewLength && i - j > 0)
		*position = TOP;
	else if (i - j > 0 && i + j > w->dino2d.viewLength)
		*position = RIGHT;
	else if (i + j > w->dino2d.viewLength && j - i > 0)
		*position = BOTTOM;
	else if (j - i > 0 && i + j < w->dino2d.viewLength)
		*position = LEFT;
	else
		*position = MAXORIENT;
	if ((faceX != 1 && faceY != 1) ||
	    (faceX >= 3 && w->dino.vertical) ||
	    (faceY >= 3 && !w->dino.vertical))
		return FALSE;
	*face = planeToCube[faceX + faceY * MAXX];
	if (*position != MAXORIENT && faceX == 3) {
		*face = MAXFACES - 1;
		*position = (*position + HALF) % MAXORIENT;
	} else
		*position %= MAXORIENT;
	return TRUE;
}

int
NarrowSelection2D(Dino2DWidget w, int *face, int *direction)
{
	if (*face == MAXFACES - 1 && *direction < MAXORIENT && !w->dino.vertical) {
		if (*direction < MAXORIENT)
			*direction = (*direction + HALF) % MAXORIENT;
		else if (*direction >= 2 * MAXORIENT)
			*direction = 2 * MAXORIENT + (*direction + HALF) % MAXORIENT;
	}
	return TRUE;
}

static void
DrawFrame(Dino2DWidget w, GC gc)
{
	int         i;
	XPoint      pos[MAXXY + 1], letters;

	for (i = 0; i <= MAXXY; i++) {
		pos[i].x = i * w->dino2d.viewLength + w->dino.puzzleOffset.x;
		pos[i].y = i * w->dino2d.viewLength + w->dino.puzzleOffset.y;
	}
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[1].x, pos[0].y, pos[2].x, pos[0].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[3].x, pos[1].y, pos[3].x, pos[2].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[1].x, pos[3].y, pos[2].x, pos[3].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[0].x, pos[1].y, pos[0].x, pos[2].y);
	letters.x = pos[0].x + w->dino2d.viewLength / 2 - w->dino.delta;
	letters.y = pos[0].y + w->dino2d.viewLength / 2;
	XDrawString(XtDisplay(w), XtWindow(w), gc,
		    (int) (letters.x + 5 * w->dino.letterOffset.x),
		    (int) (letters.y + w->dino.letterOffset.y), "Front", 5);
	letters.x = pos[2].x + w->dino2d.viewLength / 2 - w->dino.delta;
	letters.y = pos[2].y + w->dino2d.viewLength / 2;
	XDrawString(XtDisplay(w), XtWindow(w), gc,
		    (int) (letters.x + 4 * w->dino.letterOffset.x),
		    (int) (letters.y + w->dino.letterOffset.y), "Back", 4);
	if (w->dino.vertical) {
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
DrawTriangle2D(Dino2DWidget w, int face, int position, int offset)
{
	int         dx, dy, letterX, letterY, newSide, orient;

	orient = w->dino.cubeLoc[face][position].rotation;
	if (w->dino.vertical || face != MAXFACES - 1) {
		dx = (cubeToPlane[face] % MAXX) * w->dino2d.viewLength;
		dy = (cubeToPlane[face] / MAXX) * w->dino2d.viewLength;
		newSide = position;
	} else {
		dx = (cubeToPlane[face] / MAXX) * w->dino2d.viewLength;
		dy = (cubeToPlane[face] % MAXX) * w->dino2d.viewLength;
		newSide = (position + HALF) % STRT;
		orient = (orient + HALF) % STRT;
	}
	dx += w->dino.puzzleOffset.x + 2 * w->dino.delta;
	dy += w->dino.puzzleOffset.y + 2 * w->dino.delta;
	letterX = dx + letterList[newSide].x - 1;
	letterY = dy + letterList[newSide].y;
	triangleList[newSide][0].x = offsetList[newSide].x + dx;
	triangleList[newSide][0].y = offsetList[newSide].y + dy;
	if (offset) {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			     w->dino.borderGC,
			triangleList[newSide], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			w->dino.faceGC[w->dino.cubeLoc[face][position].face],
			   triangleList[newSide], 4, CoordModePrevious);
	} else {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			w->dino.faceGC[w->dino.cubeLoc[face][position].face],
			triangleList[newSide], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			   w->dino.borderGC, triangleList[newSide], 4, CoordModePrevious);
	}
	if (w->dino.depth == 1 || w->dino.mono) {
		char        buf[2];

		(void) sprintf(buf, "%c",
		  w->dino.faceName[w->dino.cubeLoc[face][position].face][0]);
		XDrawString(XtDisplay(w), XtWindow(w), w->dino.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->dino.orient)
		DrawOrientLine(w, orient + 2 * MAXORIENT,
			       letterX - w->dino.letterOffset.x, letterY - w->dino.letterOffset.y);
}

static void
DrawOrientLine(Dino2DWidget w, int orient, int dx, int dy)
{
	switch (orient) {
		case TOP:
			XDrawLine(XtDisplay(w), XtWindow(w), w->dino.inverseGC,
				  dx,
				  dy - w->dino.delta,
				  dx,
			  dy - w->dino.delta - w->dino.orientLineLength / 2);
			return;
		case RIGHT:
			XDrawLine(XtDisplay(w), XtWindow(w), w->dino.inverseGC,
				  dx + w->dino.delta,
				  dy,
			   dx + w->dino.delta + w->dino.orientLineLength / 2,
				  dy);
			return;
		case BOTTOM:
			XDrawLine(XtDisplay(w), XtWindow(w), w->dino.inverseGC,
				  dx,
				  dy + w->dino.delta,
				  dx,
			  dy + w->dino.delta + w->dino.orientLineLength / 2);
			return;
		case LEFT:
			XDrawLine(XtDisplay(w), XtWindow(w), w->dino.inverseGC,
				  dx - w->dino.delta,
				  dy,
			   dx - w->dino.delta - w->dino.orientLineLength / 2,
				  dy);
			return;
		default:
			(void) printf("DrawOrientLine: orient %d\n", orient);
	}
}
