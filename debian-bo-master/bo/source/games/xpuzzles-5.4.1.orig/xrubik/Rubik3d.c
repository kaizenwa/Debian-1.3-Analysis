
/*-
# X-BASED RUBIK'S CUBE(tm)
#
#  Rubik3d.c
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

/* Methods file for Rubik3d */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "RubikP.h"
#include "Rubik3dP.h"

static void InitializeRubik3D(Widget request, Widget new);
static void ExposeRubik3D(Widget new, XEvent * event, Region region);
static void ResizeRubik3D(Rubik3DWidget w);
static Boolean SetValuesRubik3D(Widget current, Widget request, Widget new);
static void MoveRubik3DTl(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveRubik3DTop(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveRubik3DTr(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveRubik3DLeft(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveRubik3DRight(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveRubik3DBl(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveRubik3DBottom(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveRubik3DBr(Rubik3DWidget w, XEvent * event, char **args, int nArgs);
static void ResizePolyhedrons(Rubik3DWidget w);
static void DrawFrame(Rubik3DWidget w, GC gc);
static void MapTo3D(Rubik3DWidget w, int face, int i, int j, int *x, int *y);

#ifdef DEBUG
static void MapFrom3D();

#endif
static void MapOrientFrom3D(int face, int corner, int *side);
static void CubeOffset3D(Rubik3DWidget w, int face, int x, int y, int *dx, int *dy);

static char defaultTranslationsRubik3D[] =
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
   <KeyPress>o: Orientize()";

static XtActionsRec actionsListRubik3D[] =
{
	{"Quit", (XtActionProc) QuitRubik},
	{"MoveCcw", (XtActionProc) MoveRubikCcw},
	{"MoveTl", (XtActionProc) MoveRubik3DTl},
	{"MoveTop", (XtActionProc) MoveRubik3DTop},
	{"MoveTr", (XtActionProc) MoveRubik3DTr},
	{"MoveLeft", (XtActionProc) MoveRubik3DLeft},
	{"MoveCw", (XtActionProc) MoveRubikCw},
	{"MoveRight", (XtActionProc) MoveRubik3DRight},
	{"MoveBl", (XtActionProc) MoveRubik3DBl},
	{"MoveBottom", (XtActionProc) MoveRubik3DBottom},
	{"MoveBr", (XtActionProc) MoveRubik3DBr},
	{"Select", (XtActionProc) SelectRubik},
	{"Release", (XtActionProc) ReleaseRubik},
	{"Practice", (XtActionProc) PracticeRubik},
	{"PracticeMaybe", (XtActionProc) PracticeRubikMaybe},
	{"Randomize", (XtActionProc) RandomizeRubik},
	{"RandomizeMaybe", (XtActionProc) RandomizeRubikMaybe},
	{"Get", (XtActionProc) GetRubik},
	{"Write", (XtActionProc) WriteRubik},
	{"Undo", (XtActionProc) UndoRubik},
	{"Solve", (XtActionProc) SolveRubik},
	{"Increment", (XtActionProc) IncrementRubik},
	{"Decrement", (XtActionProc) DecrementRubik},
	{"Orientize", (XtActionProc) OrientizeRubik}
};

static XtResource resourcesRubik3D[] =
{
	{XtNuserName, XtCUserName, XtRString, sizeof (String),
	 XtOffset(RubikWidget, rubik.username), XtRString, "nobody"},
	{XtNfaceColor0, XtCLabel, XtRString, sizeof (String),
	 XtOffset(RubikWidget, rubik.faceName[0]), XtRString, "Red"},
	{XtNfaceColor1, XtCLabel, XtRString, sizeof (String),
	 XtOffset(RubikWidget, rubik.faceName[1]), XtRString, "Yellow"},
	{XtNfaceColor2, XtCLabel, XtRString, sizeof (String),
	 XtOffset(RubikWidget, rubik.faceName[2]), XtRString, "White"},
	{XtNfaceColor3, XtCLabel, XtRString, sizeof (String),
	 XtOffset(RubikWidget, rubik.faceName[3]), XtRString, "Green"},
	{XtNfaceColor4, XtCLabel, XtRString, sizeof (String),
	 XtOffset(RubikWidget, rubik.faceName[4]), XtRString, "Orange"},
	{XtNfaceColor5, XtCLabel, XtRString, sizeof (String),
	 XtOffset(RubikWidget, rubik.faceName[5]), XtRString, "Blue"},
	{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
    XtOffset(RubikWidget, rubik.foreground), XtRString, XtDefaultForeground},
	{XtNpieceBorder, XtCColor, XtRPixel, sizeof (Pixel),
   XtOffset(RubikWidget, rubik.borderColor), XtRString, XtDefaultForeground},
	{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
	 XtOffset(RubikWidget, core.width), XtRString, "250"},
	{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
	 XtOffset(RubikWidget, core.height), XtRString, "400"},
	{XtNsize, XtCSize, XtRInt, sizeof (int),
	 XtOffset(RubikWidget, rubik.size), XtRString, "3"},	/* DEFAULTCUBES */
	{XtNorient, XtCOrient, XtRBoolean, sizeof (Boolean),
	 XtOffset(RubikWidget, rubik.orient), XtRString, "FALSE"},	/* DEFAULTORIENT */
	{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
	 XtOffset(RubikWidget, rubik.mono), XtRString, "FALSE"},
	{XtNface, XtCFace, XtRInt, sizeof (int),
	 XtOffset(RubikWidget, rubik.currentFace), XtRString, "-1"},
	{XtNpos, XtCPos, XtRInt, sizeof (int),
	 XtOffset(RubikWidget, rubik.currentPosition), XtRString, "-1"},
	{XtNdirection, XtCDirection, XtRInt, sizeof (int),
	 XtOffset(RubikWidget, rubik.currentDirection), XtRString, "-1"},
	{XtNpractice, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(RubikWidget, rubik.practice), XtRString, "FALSE"},
	{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
	 XtOffset(RubikWidget, rubik.started), XtRString, "FALSE"},
	{XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	 XtOffset(RubikWidget, rubik.select), XtRCallback, NULL}
};

Rubik3DClassRec rubik3dClassRec =
{
	{
		(WidgetClass) & rubikClassRec,	/* superclass */
		"Rubik3D",	/* class name */
		sizeof (Rubik3DRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeRubik3D,		/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListRubik3D,	/* actions */
		XtNumber(actionsListRubik3D),	/* num actions */
		resourcesRubik3D,	/* resources */
		XtNumber(resourcesRubik3D),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		NULL,		/* destroy */
		(XtWidgetProc) ResizeRubik3D,	/* resize */
		(XtExposeProc) ExposeRubik3D,	/* expose */
		(XtSetValuesFunc) SetValuesRubik3D,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		XtInheritAcceptFocus,	/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsRubik3D,	/* tm table */
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

WidgetClass rubik3dWidgetClass = (WidgetClass) & rubik3dClassRec;

static XPoint faceLoc3D[MAXFACES][MAXORIENT];
static XPoint cubeLoc3D[MAXFACES][MAXORIENT + 1];
static XPoint letter3DList[MAXFACES];
static XPoint orient3DList[MAXFACES][MAXORIENT][2];
static RowNext rotateToRow[MAXFACES] =	/*CW to min face */
{
	{1, LEFT, TOP},
	{0, BOTTOM, RIGHT},
	{0, RIGHT, BOTTOM},
	{0, TOP, LEFT},
	{1, RIGHT, BOTTOM},
	{0, LEFT, TOP}
};

static void
InitializeRubik3D(Widget request, Widget new)
{
	Rubik3DWidget w = (Rubik3DWidget) new;

	w->rubik.dim = 3;
	ResizeRubik3D(w);
}

static void
ResizeRubik3D(Rubik3DWidget w)
{
	XPoint      tempSize;

	w->rubik.delta = 4;
	w->rubik.vertical = (w->core.height >= w->core.width);
	if (w->rubik.vertical) {
		tempSize.y = w->core.height / MAXVIEWS;
		tempSize.x = w->core.width;
		if (tempSize.x >= DIVIDE(tempSize.y)) {
			w->rubik3d.cubeSize.y = MAX((tempSize.y - 3 * w->rubik.delta) /
				(2 * w->rubik.size) - w->rubik.delta - 2, 0);
			w->rubik3d.cubeSize.x = DIVIDE(w->rubik3d.cubeSize.y);
		} else {
			w->rubik3d.cubeSize.x = MAX((tempSize.x - 2 * w->rubik.delta - 7) /
				    (2 * w->rubik.size) - w->rubik.delta, 0);
			w->rubik3d.cubeSize.y = MULTIPLY(w->rubik3d.cubeSize.x);
		}
		w->rubik3d.cubeDiagonal = w->rubik3d.cubeSize.x / 2;
		w->rubik3d.faceSize.x = w->rubik.size *
			(w->rubik3d.cubeSize.x + w->rubik.delta) + w->rubik.delta + 1;
		w->rubik3d.faceSize.y = w->rubik.size *
			(w->rubik3d.cubeSize.y + w->rubik.delta) + w->rubik.delta + 1;
		w->rubik3d.faceDiagonal = w->rubik3d.faceSize.x / 2;
		w->rubik3d.viewSize.x = 2 * w->rubik3d.faceSize.x + 3;
		w->rubik3d.viewSize.y = 2 * w->rubik3d.faceSize.y + 3;
		w->rubik.puzzleSize.x = w->rubik3d.viewSize.x + 1;
		w->rubik.puzzleSize.y = MAXVIEWS * w->rubik3d.viewSize.y + 1;
	} else {
		tempSize.x = w->core.width / MAXVIEWS;
		tempSize.y = w->core.height;
		if (tempSize.y >= DIVIDE(tempSize.x)) {
			w->rubik3d.cubeSize.x = MAX((tempSize.x - 3 * w->rubik.delta) /
				(2 * w->rubik.size) - w->rubik.delta - 2, 0);
			w->rubik3d.cubeSize.y = DIVIDE(w->rubik3d.cubeSize.x);
		} else {
			w->rubik3d.cubeSize.y = MAX((tempSize.y - 2 * w->rubik.delta - 7) /
				    (2 * w->rubik.size) - w->rubik.delta, 0);
			w->rubik3d.cubeSize.x = MULTIPLY(w->rubik3d.cubeSize.y);
		}
		w->rubik3d.cubeDiagonal = w->rubik3d.cubeSize.y / 2;
		w->rubik3d.faceSize.y = w->rubik.size *
			(w->rubik3d.cubeSize.y + w->rubik.delta) + w->rubik.delta + 1;
		w->rubik3d.faceSize.x = w->rubik.size *
			(w->rubik3d.cubeSize.x + w->rubik.delta) + w->rubik.delta + 1;
		w->rubik3d.faceDiagonal = w->rubik3d.faceSize.y / 2;
		w->rubik3d.viewSize.y = 2 * w->rubik3d.faceSize.y + 3;
		w->rubik3d.viewSize.x = 2 * w->rubik3d.faceSize.x + 3;
		w->rubik.puzzleSize.y = w->rubik3d.viewSize.y + 1;
		w->rubik.puzzleSize.x = MAXVIEWS * w->rubik3d.viewSize.x + 1;
	}
	w->rubik.puzzleOffset.x = ((int) w->core.width - w->rubik.puzzleSize.x) / 2;
	w->rubik.puzzleOffset.y = ((int) w->core.height - w->rubik.puzzleSize.y) /
		2;
	ResizePolyhedrons(w);
}

static void
ExposeRubik3D(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	Rubik3DWidget w = (Rubik3DWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->rubik.puzzleGC);
		DrawAllPolyhedrons((RubikWidget) w);
	}
}

static      Boolean
SetValuesRubik3D(Widget current, Widget request, Widget new)
{
	Rubik3DWidget c = (Rubik3DWidget) current, w = (Rubik3DWidget) new;
	Boolean     redraw = FALSE;

	if (w->rubik.size != c->rubik.size) {
		ResetPolyhedrons((RubikWidget) w);
		ResizeRubik3D(w);
		redraw = TRUE;
	}
	if (w->rubik3d.cubeSize.x != c->rubik3d.cubeSize.x) {
		ResizeRubik3D(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
MoveRubik3DTl(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, TL,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik3DTop(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, TOP,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik3DTr(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, TR,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik3DLeft(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, LEFT,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik3DRight(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, RIGHT,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik3DBl(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, BL,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik3DBottom(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, BOTTOM,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik3DBr(Rubik3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, BR,
		       (int) (event->xkey.state & ControlMask));
}

static void
ResizePolyhedrons(Rubik3DWidget w)
{
	int         face, orient, side, corner;
	XPoint      subcubeLoc3D[MAXFACES][MAXORIENT];
	XPoint      diamondLoc3D[MAXFACES][MAXORIENT];
	XPoint      subdiamondLoc3D[MAXFACES][MAXORIENT];

	w->rubik.letterOffset.x = -2;
	w->rubik.letterOffset.y = 4;
	w->rubik3d.viewMiddle.x = w->rubik3d.faceSize.x +
		w->rubik.puzzleOffset.x;
	w->rubik3d.viewMiddle.y = w->rubik3d.faceSize.y +
		w->rubik.puzzleOffset.y;
	for (face = 0; face < MAXFACES; face++) {
		faceLoc3D[face][0].x = w->rubik3d.viewMiddle.x;
		faceLoc3D[face][0].y = w->rubik3d.viewMiddle.y;
		for (orient = 1; orient < MAXORIENT; orient++) {
			faceLoc3D[face][orient].x = w->rubik3d.faceSize.x;
			faceLoc3D[face][orient].y = w->rubik3d.faceSize.y;
		}
	}
	if (w->rubik.vertical) {
		faceLoc3D[0][1].x /= -2;
		faceLoc3D[0][1].y /= -1;
		faceLoc3D[0][2].y = 0;
		faceLoc3D[0][3].x /= 2;

		faceLoc3D[1][1].x /= -2;
		faceLoc3D[1][2].x /= -2;
		faceLoc3D[1][2].y /= -1;
		faceLoc3D[1][3].x /= 2;
		faceLoc3D[1][3].y /= -1;

		faceLoc3D[2][1].y = 0;
		faceLoc3D[2][2].x /= -2;
		faceLoc3D[2][3].x /= -1;
		faceLoc3D[2][3].y = 0;

		for (face = MAXFACES / 2; face < MAXFACES; face++)
			faceLoc3D[face][0].y += w->rubik3d.viewSize.y + 3;

		faceLoc3D[3][1].x /= 2;
		faceLoc3D[3][1].y /= -1;
		faceLoc3D[3][2].x /= 2;
		faceLoc3D[3][3].x /= -2;

		faceLoc3D[4][1].x /= -1;
		faceLoc3D[4][1].y = 0;
		faceLoc3D[4][2].x /= 2;
		faceLoc3D[4][2].y /= -1;
		faceLoc3D[4][3].y = 0;

		faceLoc3D[5][1].x /= 2;
		faceLoc3D[5][2].x /= -1;
		faceLoc3D[5][2].y = 0;
		faceLoc3D[5][3].x /= -2;
		faceLoc3D[5][3].y /= -1;
	} else {
		faceLoc3D[0][1].x /= -1;
		faceLoc3D[0][1].y /= -2;
		faceLoc3D[0][2].y /= -2;
		faceLoc3D[0][3].y /= 2;

		faceLoc3D[1][1].x = 0;
		faceLoc3D[1][2].x /= -1;
		faceLoc3D[1][2].y /= -2;
		faceLoc3D[1][3].x = 0;
		faceLoc3D[1][3].y /= -1;

		faceLoc3D[2][1].y /= -2;
		faceLoc3D[2][2].x = 0;
		faceLoc3D[2][3].y /= 2;
		faceLoc3D[2][3].x /= -1;

		for (face = MAXFACES / 2; face < MAXFACES; face++)
			faceLoc3D[face][0].x += w->rubik3d.viewSize.x + 3;

		faceLoc3D[3][1].x /= -1;
		faceLoc3D[3][1].y /= 2;
		faceLoc3D[3][2].x = 0;
		faceLoc3D[3][2].y /= -1;
		faceLoc3D[3][3].y /= -2;

		faceLoc3D[4][1].y /= 2;
		faceLoc3D[4][2].x /= -1;
		faceLoc3D[4][2].y /= 2;
		faceLoc3D[4][3].x /= -1;
		faceLoc3D[4][3].y /= -2;

		faceLoc3D[5][1].x = 0;
		faceLoc3D[5][1].y /= -1;
		faceLoc3D[5][2].y /= 2;
		faceLoc3D[5][3].x = 0;
	}
	for (face = 0; face < MAXFACES; face++) {
		cubeLoc3D[face][0].x = faceLoc3D[face][0].x;
		cubeLoc3D[face][0].y = faceLoc3D[face][0].y;
		subcubeLoc3D[face][0].x = faceLoc3D[face][0].x;
		subcubeLoc3D[face][0].y = faceLoc3D[face][0].y;
		for (orient = 1; orient < MAXORIENT; orient++) {
			cubeLoc3D[face][orient].x =
				(faceLoc3D[face][orient].x - (3 + w->rubik.size) *
				 w->rubik.delta * faceLoc3D[face][orient].x /
				 w->rubik3d.faceSize.x) / w->rubik.size;
			cubeLoc3D[face][orient].y =
				(faceLoc3D[face][orient].y - (3 + w->rubik.size) *
				 w->rubik.delta * faceLoc3D[face][orient].y /
				 w->rubik3d.faceSize.y) / w->rubik.size;
			subcubeLoc3D[face][orient].x =
				(faceLoc3D[face][orient].x - (5 + w->rubik.size) *
				 w->rubik.delta * faceLoc3D[face][orient].x /
				 w->rubik3d.faceSize.x) / (2 * w->rubik.size);
			subcubeLoc3D[face][orient].y =
				(faceLoc3D[face][orient].y - (5 + w->rubik.size) *
				 w->rubik.delta * faceLoc3D[face][orient].y /
				 w->rubik3d.faceSize.y) / (2 * w->rubik.size);
		}
		cubeLoc3D[face][MAXORIENT].x = -cubeLoc3D[face][1].x -
			cubeLoc3D[face][2].x - cubeLoc3D[face][3].x;
		cubeLoc3D[face][MAXORIENT].y = -cubeLoc3D[face][1].y -
			cubeLoc3D[face][2].y - cubeLoc3D[face][3].y;
	}
	w->rubik3d.cubeSize.x = (w->rubik3d.faceSize.x - w->rubik.delta) /
		w->rubik.size - w->rubik.delta;
	w->rubik3d.cubeDiagonal = (w->rubik3d.faceDiagonal - w->rubik.delta) /
		w->rubik.size - w->rubik.delta;
	w->rubik3d.cubeSize.y = (w->rubik3d.faceSize.y - w->rubik.delta) /
		w->rubik.size - w->rubik.delta;
	w->rubik3d.cubeDiag = (w->rubik3d.faceDiagonal + w->rubik.delta) /
		w->rubik.size + w->rubik.delta;
	if (w->rubik.vertical) {
		letter3DList[0].x = w->rubik3d.cubeSize.x / 4;
		letter3DList[0].y = -w->rubik3d.cubeSize.y / 2;
		letter3DList[1].x = -w->rubik3d.cubeDiagonal;
		letter3DList[1].y = 0;
		letter3DList[2].x = w->rubik3d.cubeSize.x / 4;
		letter3DList[2].y = w->rubik3d.cubeSize.y / 2;
		letter3DList[3].x = w->rubik3d.cubeDiagonal;
		letter3DList[3].y = 0;
		letter3DList[4].x = -w->rubik3d.cubeSize.x / 4;
		letter3DList[4].y = -w->rubik3d.cubeSize.y / 2;
		letter3DList[5].x = -w->rubik3d.cubeSize.x / 4;
		letter3DList[5].y = w->rubik3d.cubeSize.y / 2;
	} else {
		letter3DList[0].x = 0;
		letter3DList[0].y = -w->rubik3d.cubeDiagonal;
		letter3DList[1].x = -w->rubik3d.cubeSize.x / 2;
		letter3DList[1].y = w->rubik3d.cubeSize.y / 4;
		letter3DList[2].x = w->rubik3d.cubeSize.x / 2;
		letter3DList[2].y = w->rubik3d.cubeSize.y / 4;
		letter3DList[3].x = -w->rubik3d.cubeSize.x / 2;
		letter3DList[3].y = -w->rubik3d.cubeSize.y / 4;
		letter3DList[4].x = 0;
		letter3DList[4].y = w->rubik3d.cubeDiagonal;
		letter3DList[5].x = w->rubik3d.cubeSize.x / 2;
		letter3DList[5].y = -w->rubik3d.cubeSize.y / 4;
	}
	/* The following figures out where to put the orient lines */
	for (face = 0; face < MAXFACES; face++) {
		for (orient = 0; orient < MAXORIENT - 1; orient++) {
			diamondLoc3D[face][orient].x = (cubeLoc3D[face][orient].x +
					  cubeLoc3D[face][orient + 1].x) / 2;
			diamondLoc3D[face][orient].y = (cubeLoc3D[face][orient].y +
					  cubeLoc3D[face][orient + 1].y) / 2;
			subdiamondLoc3D[face][orient].x = (subcubeLoc3D[face][orient].x +
				       subcubeLoc3D[face][orient + 1].x) / 2;
			subdiamondLoc3D[face][orient].y = (subcubeLoc3D[face][orient].y +
				       subcubeLoc3D[face][orient + 1].y) / 2;
		}
		/* Its a parallelagram so take advantage of that */
		diamondLoc3D[face][orient].x = (cubeLoc3D[face][MAXORIENT - 1].x -
				       cubeLoc3D[face][MAXORIENT / 2].x) / 2;
		diamondLoc3D[face][orient].y = (cubeLoc3D[face][MAXORIENT - 1].y -
				       cubeLoc3D[face][MAXORIENT / 2].y) / 2;
		subdiamondLoc3D[face][orient].x = (subcubeLoc3D[face][MAXORIENT - 1].x -
				    subcubeLoc3D[face][MAXORIENT / 2].x) / 2;
		subdiamondLoc3D[face][orient].y = (subcubeLoc3D[face][MAXORIENT - 1].y -
				    subcubeLoc3D[face][MAXORIENT / 2].y) / 2;

		MapOrientFrom3D(face, 1, &corner);
		orient3DList[face][corner][0].x = cubeLoc3D[face][1].x / 2;
		orient3DList[face][corner][0].y = cubeLoc3D[face][1].y / 2;
		orient3DList[face][corner][1].x = orient3DList[face][corner][0].x +
			(cubeLoc3D[face][2].x - subcubeLoc3D[face][2].x) / 2;
		orient3DList[face][corner][1].y = orient3DList[face][corner][0].y +
			(cubeLoc3D[face][2].y - subcubeLoc3D[face][2].y) / 2;
		for (orient = 1; orient < MAXORIENT; orient++) {
			side = corner;
			MapOrientFrom3D(face, (orient + 1) % MAXORIENT, &corner);
			orient3DList[face][corner][0].x =
				orient3DList[face][side][0].x + diamondLoc3D[face][orient].x;
			orient3DList[face][corner][0].y =
				orient3DList[face][side][0].y + diamondLoc3D[face][orient].y;
			orient3DList[face][corner][1].x =
				orient3DList[face][side][1].x + subdiamondLoc3D[face][orient].x;
			orient3DList[face][corner][1].y =
				orient3DList[face][side][1].y + subdiamondLoc3D[face][orient].y;
		}
	}
}

int
SelectPolyhedrons3D(Rubik3DWidget w, int x, int y, int *face, int *position)
{
	int         u, v, front, tl, ur, ul, i, j;

	if (w->rubik.vertical) {
		x -= w->rubik3d.viewMiddle.x;
		front = (y < w->rubik3d.viewSize.y + w->rubik.puzzleOffset.y);
		if (!front)
			y -= (w->rubik3d.viewSize.y);
		tl = (y < w->rubik3d.viewMiddle.y);
		y -= w->rubik3d.viewMiddle.y;
		u = -w->rubik3d.faceSize.y * x + w->rubik3d.faceDiagonal * y;
		v = w->rubik3d.faceSize.y * x + w->rubik3d.faceDiagonal * y;
		ur = (u < 0);
		ul = (v < 0);
		if (front) {
			if (tl)
				*face = (ur) ? 0 : 1;
			else
				*face = (ul) ? 1 : 2;
		} else {
			if (tl)
				*face = (ul) ? 4 : 3;
			else
				*face = (ur) ? 3 : 5;
		}
	} else {
		y -= w->rubik3d.viewMiddle.y;
		front = (x < w->rubik3d.viewSize.x + w->rubik.puzzleOffset.x);
		if (!front)
			x -= (w->rubik3d.viewSize.x);
		tl = (x < w->rubik3d.viewMiddle.x);
		x -= w->rubik3d.viewMiddle.x;
		u = -w->rubik3d.faceSize.x * y + w->rubik3d.faceDiagonal * x;
		v = w->rubik3d.faceSize.x * y + w->rubik3d.faceDiagonal * x;
		ur = (u < 0);
		ul = (v < 0);
		if (front) {
			if (tl)
				*face = (ur) ? 1 : 0;
			else
				*face = (ul) ? 0 : 2;
		} else {
			if (tl)
				*face = (ul) ? 3 : 4;
			else
				*face = (ur) ? 4 : 5;
		}
	}
	if (w->rubik.vertical)
		switch (*face) {
			case 0:
				i = (x - 2 - (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(w->rubik3d.cubeSize.x + w->rubik.delta);
				j = (y + 2) / (w->rubik3d.cubeSize.y + w->rubik.delta) +
					w->rubik.size - 1;
				break;
			case 1:
				i = (x + 4 + (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta)) + w->rubik.size - 1;
				j = (-x - 6 + (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta));
				break;
			case 2:
				i = (x - 4 + (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(w->rubik3d.cubeSize.x + w->rubik.delta);
				j = (y - 4) / (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 3:
				i = (-x + 5 + (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta)) + w->rubik.size - 1;
				j = (-x + 7 - (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta)) + w->rubik.size - 1;
				break;
			case 4:
				i = (x + (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(w->rubik3d.cubeSize.x + w->rubik.delta) + w->rubik.size - 1;
				j = (y - 2) / (w->rubik3d.cubeSize.y + w->rubik.delta) +
					w->rubik.size - 1;
				break;
			case 5:
				i = (x + 2 - (y * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.y)) /
					(w->rubik3d.cubeSize.x + w->rubik.delta) + w->rubik.size - 1;
				j = (y - 6) / (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			default:
				return FALSE;
	} else
		switch (*face) {
			case 0:
				i = (-y - 3 + (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta));
				j = (y + 1 + (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta)) + w->rubik.size - 1;
				break;
			case 1:
				i = (x + 2) / (w->rubik3d.cubeSize.x + w->rubik.delta) +
					w->rubik.size - 1;
				j = (y - 3 - (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 2:
				i = (x - 4) / (w->rubik3d.cubeSize.x + w->rubik.delta);
				j = (y - 6 + (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 3:
				i = x / (w->rubik3d.cubeSize.x + w->rubik.delta) +
					w->rubik.size - 1;
				j = (y + (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(w->rubik3d.cubeSize.y + w->rubik.delta) + w->rubik.size - 1;
				break;
			case 4:
				i = (-y + 9 - (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta)) + w->rubik.size - 1;
				j = (-y + 7 + (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(2 * (w->rubik3d.cubeDiagonal + w->rubik.delta)) + w->rubik.size - 1;
				break;
			case 5:
				i = (-x + 7) / (w->rubik3d.cubeSize.x + w->rubik.delta) +
					w->rubik.size - 1;
				j = (-y - 4 + (x * w->rubik3d.faceDiagonal / w->rubik3d.faceSize.x)) /
					(w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			default:
				return FALSE;
		}
	if (i < 0 || j < 0 || i >= w->rubik.size || j >= w->rubik.size)
		return FALSE;
	*position = j * w->rubik.size + i;
	return TRUE;
}

int
NarrowSelection3D(Rubik3DWidget w, int *face, int *position, int *direction)
{
	int         i, j;

	switch (*direction) {
		case TOP:
		case RIGHT:
		case BOTTOM:
		case LEFT:
			if (w->rubik.vertical) {
				if (*face == 1 || *face == 3)
					return FALSE;
			} else {
				if (*face == 0 || *face == 4)
					return FALSE;
				if (*face == 5)
					*direction = (*direction + 2) % MAXORIENT;
			}
			break;
		case CCW:
		case CW:
			break;
		case TR:
			if (w->rubik.vertical) {
				if (*face == 0 || *face == 5)
					return FALSE;
				else if (*face == 1 || *face == 2 || *face == 4)
					*direction = TOP;
				else	/* (*face == 3) */
					*direction = LEFT;
			} else {
				if (*face == 1 || *face == 5)
					return FALSE;
				else if (*face == 0 || *face == 2 || *face == 3)
					*direction = RIGHT;
				else	/* (*face == 4) */
					*direction = BOTTOM;
			}
			break;
		case BR:
			if (w->rubik.vertical) {
				if (*face == 2 || *face == 4)
					return FALSE;
				else if (*face == 0 || *face == 5)
					*direction = BOTTOM;
				else if (*face == 1)
					*direction = RIGHT;
				else	/* (*face == 3) */
					*direction = TOP;
			} else {
				if (*face == 2 || *face == 3)
					return FALSE;
				else if (*face == 4 || *face == 5)
					*direction = LEFT;
				else if (*face == 0)
					*direction = BOTTOM;
				else	/* (*face == 1) */
					*direction = RIGHT;
			}
			break;
		case BL:
			if (w->rubik.vertical) {
				if (*face == 0 || *face == 5)
					return FALSE;
				else if (*face == 1 || *face == 2 || *face == 4)
					*direction = BOTTOM;
				else	/* (*face == 3) */
					*direction = RIGHT;
			} else {
				if (*face == 1 || *face == 5)
					return FALSE;
				else if (*face == 0 || *face == 2 || *face == 3)
					*direction = LEFT;
				else	/* (*face == 4) */
					*direction = TOP;
			}
			break;
		case TL:
			if (w->rubik.vertical) {
				if (*face == 2 || *face == 4)
					return FALSE;
				else if (*face == 0 || *face == 5)
					*direction = TOP;
				else if (*face == 1)
					*direction = LEFT;
				else	/* (*face == 3) */
					*direction = BOTTOM;
			} else {
				if (*face == 2 || *face == 3)
					return FALSE;
				else if (*face == 4 || *face == 5)
					*direction = RIGHT;
				else if (*face == 0)
					*direction = TOP;
				else	/* (*face == 1) */
					*direction = LEFT;
			}
			break;
		default:
			return FALSE;
	}
	/*cubeDiag = (w->rubik3d.faceDiagonal + w->rubik.delta) / w->rubik.size +
	   w->rubik.delta; */
	/* Remap to row movement */
	if (*direction == CW || *direction == CCW) {
		*direction = (*direction == CCW) ?
			(rotateToRow[*face].direction + 2) % MAXORIENT :
			rotateToRow[*face].direction;
		i = j = (rotateToRow[*face].sideFace == LEFT ||
			 rotateToRow[*face].sideFace == BOTTOM) ?
			w->rubik.size - 1 : 0;
		*face = rotateToRow[*face].face;
		*position = j * w->rubik.size + i;
	}
	return TRUE;
}

static void
DrawFrame(Rubik3DWidget w, GC gc)
{
	int         face, dx, dy;

	dx = w->rubik3d.viewSize.x + w->rubik.puzzleOffset.x;
	dy = w->rubik3d.viewSize.y + w->rubik.puzzleOffset.y;
	if (w->rubik.vertical) {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  0, dy, dx + w->rubik.puzzleOffset.x + 1, dy);
		XDrawString(XtDisplay(w), XtWindow(w), gc,
			    (int) (2 * w->rubik.delta),
			(int) (3 * w->rubik.delta + w->rubik.letterOffset.y),
			    "Front", 5);
		XDrawString(XtDisplay(w), XtWindow(w), gc, (int)
			    (-4 * w->rubik.delta + 2 * 4 * w->rubik.letterOffset.x + w->core.width),
			    (int) (-w->rubik.delta - 2 * w->rubik.letterOffset.y + w->core.height),
			    "Back", 4);
	} else {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  dx, 0, dx, dy + w->rubik.puzzleOffset.y + 1);
		XDrawString(XtDisplay(w), XtWindow(w), gc,
			    (int) (2 * w->rubik.delta),
			(int) (3 * w->rubik.delta + w->rubik.letterOffset.y),
			    "Front", 5);
		XDrawString(XtDisplay(w), XtWindow(w), gc, (int)
			    (-4 * w->rubik.delta + 2 * 4 * w->rubik.letterOffset.x + w->core.width),
			    (int) (-w->rubik.delta - 2 * w->rubik.letterOffset.y + w->core.height),
			    "Back", 4);
	}
	for (face = 0; face < MAXFACES; face++)
		XDrawLines(XtDisplay(w), XtWindow(w), gc,
			   faceLoc3D[face], MAXORIENT, CoordModePrevious);
}

void
DrawSquare3D(Rubik3DWidget w, int face, int position, int offset)
{
	int         x, y, dx, dy, i, j;

	i = position % w->rubik.size;
	j = position / w->rubik.size;
	MapTo3D(w, face, i, j, &x, &y);
	CubeOffset3D(w, face, x, y, &dx, &dy);
	cubeLoc3D[face][0].x = dx;
	cubeLoc3D[face][0].y = dy;
	if (offset) {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			     w->rubik.borderGC,
		      cubeLoc3D[face], MAXORIENT, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
		      w->rubik.faceGC[w->rubik.cubeLoc[face][position].face],
			   cubeLoc3D[face], 5, CoordModePrevious);
	} else {
		XFillPolygon(XtDisplay(w), XtWindow(w),
		      w->rubik.faceGC[w->rubik.cubeLoc[face][position].face],
		      cubeLoc3D[face], MAXORIENT, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
		   w->rubik.borderGC, cubeLoc3D[face], 5, CoordModePrevious);
	}
	if (w->rubik.depth == 1 || w->rubik.mono) {
		int         letterX, letterY;
		char        buf[2];

		(void) sprintf(buf, "%c",
		w->rubik.faceName[w->rubik.cubeLoc[face][position].face][0]);
		letterX = dx + letter3DList[face].x + w->rubik.letterOffset.x;
		letterY = dy + letter3DList[face].y + w->rubik.letterOffset.y;
		XDrawString(XtDisplay(w), XtWindow(w), w->rubik.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->rubik.orient)
		XDrawLine(XtDisplay(w), XtWindow(w), w->rubik.inverseGC,
			  dx + orient3DList[face][w->rubik.cubeLoc[face][position].rotation][0].x,
			  dy + orient3DList[face][w->rubik.cubeLoc[face][position].rotation][0].y,
			  dx + orient3DList[face][w->rubik.cubeLoc[face][position].rotation][1].x,
			  dy + orient3DList[face][w->rubik.cubeLoc[face][position].rotation][1].y);
}

static void
MapTo3D(Rubik3DWidget w, int face, int i, int j, int *x, int *y)
{
	switch (face) {
		case 0:
			*x = w->rubik.size - 1 - j;
			*y = i;
			break;
		case 1:
			*x = j;
			*y = w->rubik.size - 1 - i;
			break;
		case 2:
			*x = i;
			*y = j;
			break;
		case 3:
			*x = w->rubik.size - 1 - i;
			*y = w->rubik.size - 1 - j;
			break;
		case 4:
			*x = w->rubik.size - 1 - i;
			*y = w->rubik.size - 1 - j;
			break;
		case 5:
			*x = j;
			*y = w->rubik.size - 1 - i;
			break;
		default:
			(void) printf("MapTo3D: face %d\n", face);
	}
}

#ifdef DEBUG
static void
MapFrom3D(w, face, x, y, i, j)
	Rubik3DWidget w;
	int         face, x, y, *i, *j;
{
	switch (face) {
		case 0:
			*i = y;
			*j = w->rubik.size - 1 - x;
			break;
		case 1:
			*i = w->rubik.size - 1 - y;
			*j = x;
			break;
		case 2:
			*i = x;
			*j = y;
			break;
		case 3:
			*i = w->rubik.size - 1 - x;
			*j = w->rubik.size - 1 - y;
			break;
		case 4:
			*i = w->rubik.size - 1 - x;
			*j = w->rubik.size - 1 - y;
			break;
		case 5:
			*i = w->rubik.size - 1 - y;
			*j = x;
			break;
		default:
			(void) printf("MapFrom3D: face %d\n", face);
	}
}

#endif

static void
MapOrientFrom3D(int face, int corner, int *side)
{
	switch (face) {
		case 0:
			*side = (corner + 2) % MAXORIENT;
			break;
		case 1:
			*side = corner;
			break;
		case 2:
			*side = (corner + 3) % MAXORIENT;
			break;
		case 3:
			*side = (corner + 1) % MAXORIENT;
			break;
		case 4:
			*side = (corner + 1) % MAXORIENT;
			break;
		case 5:
			*side = corner;
			break;
		default:
			(void) printf("MapFrom3D: face %d\n", face);
	}
}

static void
CubeOffset3D(Rubik3DWidget w, int face, int x, int y, int *dx, int *dy)
{
	if (w->rubik.vertical)
		switch (face) {
			case 0:
				*dx = w->rubik3d.viewMiddle.x + w->rubik.delta - 1 +
					y * (w->rubik3d.cubeSize.x + w->rubik.delta) -
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y - w->rubik.delta - 2 -
					x * (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 1:
				*dx = w->rubik3d.viewMiddle.x - 2 * w->rubik.delta -
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta) -
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y +
					x * (w->rubik3d.cubeSize.y + w->rubik.delta) -
					y * (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 2:
				*dx = w->rubik3d.viewMiddle.x + w->rubik.delta - 1 +
					x * (w->rubik3d.cubeSize.x + w->rubik.delta) -
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y + 2 * w->rubik.delta - 1 +
					y * (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 3:
				*dx = w->rubik3d.viewMiddle.x + 2 * w->rubik.delta +
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta) +
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				*dy = w->rubik3d.viewSize.y + w->rubik3d.viewMiddle.y +
					w->rubik.delta - 1 -
					x * (w->rubik3d.cubeSize.y + w->rubik.delta) +
					y * (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 4:
				*dx = w->rubik3d.viewMiddle.x - w->rubik.delta + 1 -
					x * (w->rubik3d.cubeSize.x + w->rubik.delta) +
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				*dy = w->rubik3d.viewSize.y + w->rubik3d.viewMiddle.y -
					w->rubik.delta + 1 -
					y * (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			case 5:
				*dx = w->rubik3d.viewMiddle.x - 2 -
					y * (w->rubik3d.cubeSize.x + w->rubik.delta) +
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				*dy = w->rubik3d.viewSize.y + w->rubik3d.viewMiddle.y +
					2 * w->rubik.delta + 2 +
					x * (w->rubik3d.cubeSize.y + w->rubik.delta);
				break;
			default:
				(void) printf("CubeOffset3D: face %d\n", face);
	} else
		switch (face) {
			case 0:
				*dx = w->rubik3d.viewMiddle.x +
					y * (w->rubik3d.cubeSize.x + w->rubik.delta) -
					x * (w->rubik3d.cubeSize.x + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y - 2 * w->rubik.delta + 1 -
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta) -
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				break;
			case 1:
				*dx = w->rubik3d.viewMiddle.x - w->rubik.delta - 2 -
					y * (w->rubik3d.cubeSize.x + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y + w->rubik.delta +
					x * (w->rubik3d.cubeSize.y + w->rubik.delta) -
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				break;
			case 2:
				*dx = w->rubik3d.viewMiddle.x + 2 * w->rubik.delta - 1 +
					x * (w->rubik3d.cubeSize.x + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y + w->rubik.delta +
					y * (w->rubik3d.cubeSize.y + w->rubik.delta) -
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				break;
			case 3:
				*dx = w->rubik3d.viewSize.x + w->rubik3d.viewMiddle.x -
					w->rubik.delta + 1 -
					x * (w->rubik3d.cubeSize.x + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y - w->rubik.delta -
					y * (w->rubik3d.cubeSize.y + w->rubik.delta) +
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				break;
			case 4:
				*dx = w->rubik3d.viewSize.x + w->rubik3d.viewMiddle.x +
					w->rubik.delta - 1 -
					y * (w->rubik3d.cubeSize.x + w->rubik.delta) +
					x * (w->rubik3d.cubeSize.x + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y + 2 * w->rubik.delta +
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta) +
					x * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				break;
			case 5:
				*dx = w->rubik3d.viewSize.x + w->rubik3d.viewMiddle.x +
					2 * w->rubik.delta + 2 +
					y * (w->rubik3d.cubeSize.x + w->rubik.delta);
				*dy = w->rubik3d.viewMiddle.y - w->rubik.delta -
					x * (w->rubik3d.cubeSize.y + w->rubik.delta) +
					y * (w->rubik3d.cubeDiagonal + w->rubik.delta);
				break;
			default:
				(void) printf("CubeOffset3D: face %d\n", face);
		}
}
