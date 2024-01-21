
/*-
# X-BASED SKEWB
#
#  Skewb3d.c
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

/* Methods file for Skewb3d */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "SkewbP.h"
#include "Skewb3dP.h"

static void InitializeSkewb3D(Widget request, Widget new);
static void ExposeSkewb3D(Widget new, XEvent * event, Region region);
static void ResizeSkewb3D(Skewb3DWidget w);
static Boolean SetValuesSkewb3D(Widget current, Widget request, Widget new);
static void MoveSkewb3DTl(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb3DTop(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb3DTr(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb3DLeft(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb3DRight(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb3DBl(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb3DBottom(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveSkewb3DBr(Skewb3DWidget w, XEvent * event, char **args, int nArgs);
static void ResizePolyhedrons(Skewb3DWidget w);
static void DrawFrame(Skewb3DWidget w, GC gc);
static void DiamondOffset3D(Skewb3DWidget w, int face, int *dx, int *dy);
static void MapFrom3D(int face, int corner, int *side);
static void MapTo3D(int face, int side, int *corner);
static void CubeOffset3D(Skewb3DWidget w, int face, int corner, int *dx, int *dy);

static char defaultTranslationsSkewb3D[] =
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
   <KeyPress>o: Orientize()";

static XtActionsRec actionsListSkewb3D[] =
{
	{"Quit", (XtActionProc) QuitSkewb},
	{"MoveCcw", (XtActionProc) MoveSkewbCcw},
	{"MoveTl", (XtActionProc) MoveSkewb3DTl},
	{"MoveTop", (XtActionProc) MoveSkewb3DTop},
	{"MoveTr", (XtActionProc) MoveSkewb3DTr},
	{"MoveLeft", (XtActionProc) MoveSkewb3DLeft},
	{"MoveCw", (XtActionProc) MoveSkewbCw},
	{"MoveRight", (XtActionProc) MoveSkewb3DRight},
	{"MoveBl", (XtActionProc) MoveSkewb3DBl},
	{"MoveBottom", (XtActionProc) MoveSkewb3DBottom},
	{"MoveBr", (XtActionProc) MoveSkewb3DBr},
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

static XtResource resourcesSkewb3D[] =
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
	 XtOffset(SkewbWidget, core.width), XtRString, "250"},
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

Skewb3DClassRec skewb3dClassRec =
{
	{
		(WidgetClass) & skewbClassRec,	/* superclass */
		"Skewb3D",	/* class name */
		sizeof (Skewb3DRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeSkewb3D,		/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListSkewb3D,	/* actions */
		XtNumber(actionsListSkewb3D),	/* num actions */
		resourcesSkewb3D,	/* resources */
		XtNumber(resourcesSkewb3D),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		NULL,		/* destroy */
		(XtWidgetProc) ResizeSkewb3D,	/* resize */
		(XtExposeProc) ExposeSkewb3D,	/* expose */
		(XtSetValuesFunc) SetValuesSkewb3D,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		XtInheritAcceptFocus,	/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsSkewb3D,	/* tm table */
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

WidgetClass skewb3dWidgetClass = (WidgetClass) & skewb3dClassRec;

static XPoint faceLoc3D[MAXFACES][MAXORIENT];
static XPoint cubeLoc3D[MAXFACES][MAXORIENT];
static XPoint diamondLoc3D[MAXFACES][MAXORIENT + 1];
static XPoint triangleLoc3D[MAXFACES][MAXORIENT][4];
static XPoint letter3DList[MAXFACES][MAXCUBES];
static XPoint orientDiamond[MAXFACES][MAXORIENT][2];
static XPoint orientTriangle[MAXFACES][MAXORIENT][2];

static void
InitializeSkewb3D(Widget request, Widget new)
{
	Skewb3DWidget w = (Skewb3DWidget) new;

	w->skewb.dim = 3;
	ResizeSkewb3D(w);
}

static void
ResizeSkewb3D(Skewb3DWidget w)
{
	XPoint      tempSize;

	w->skewb.delta = 4;
	w->skewb.vertical = (w->core.height >= w->core.width);
	if (w->skewb.vertical) {
		tempSize.y = w->core.height / MAXVIEWS;
		tempSize.x = w->core.width;
		if (tempSize.x >= DIVIDE(tempSize.y)) {
			w->skewb3d.cubeSize.y = MAX((tempSize.y - 3 * w->skewb.delta) / 2 -
						    w->skewb.delta - 2, 0);
			w->skewb3d.cubeSize.x = DIVIDE(w->skewb3d.cubeSize.y);
		} else {
			w->skewb3d.cubeSize.x = MAX((tempSize.x - 2 * w->skewb.delta - 7) / 2 -
						    w->skewb.delta, 0);
			w->skewb3d.cubeSize.y = MULTIPLY(w->skewb3d.cubeSize.x);
		}
		w->skewb3d.cubeDiagonal = w->skewb3d.cubeSize.x / 2;
		w->skewb3d.faceSize.x = w->skewb3d.cubeSize.x + 2 * w->skewb.delta + 1;
		w->skewb3d.faceSize.y = w->skewb3d.cubeSize.y + 2 * w->skewb.delta + 1;
		w->skewb3d.faceDiagonal = w->skewb3d.faceSize.x / 2;
		w->skewb3d.viewSize.x = 2 * w->skewb3d.faceSize.x + 3;
		w->skewb3d.viewSize.y = 2 * w->skewb3d.faceSize.y + 3;
		w->skewb.puzzleSize.x = w->skewb3d.viewSize.x + 1;
		w->skewb.puzzleSize.y = MAXVIEWS * w->skewb3d.viewSize.y + 1;
	} else {
		tempSize.x = w->core.width / MAXVIEWS;
		tempSize.y = w->core.height;
		if (tempSize.y >= DIVIDE(tempSize.x)) {
			w->skewb3d.cubeSize.x = MAX((tempSize.x - 3 * w->skewb.delta) / 2 -
						    w->skewb.delta - 2, 0);
			w->skewb3d.cubeSize.y = DIVIDE(w->skewb3d.cubeSize.x);
		} else {
			w->skewb3d.cubeSize.y = MAX((tempSize.y - 2 * w->skewb.delta - 7) / 2 -
						    w->skewb.delta, 0);
			w->skewb3d.cubeSize.x = MULTIPLY(w->skewb3d.cubeSize.y);
		}
		w->skewb3d.cubeDiagonal = w->skewb3d.cubeSize.y / 2;
		w->skewb3d.faceSize.y = w->skewb3d.cubeSize.y + 2 * w->skewb.delta + 1;
		w->skewb3d.faceSize.x = w->skewb3d.cubeSize.x + 2 * w->skewb.delta + 1;
		w->skewb3d.faceDiagonal = w->skewb3d.faceSize.y / 2;
		w->skewb3d.viewSize.y = 2 * w->skewb3d.faceSize.y + 3;
		w->skewb3d.viewSize.x = 2 * w->skewb3d.faceSize.x + 3;
		w->skewb.puzzleSize.y = w->skewb3d.viewSize.y + 1;
		w->skewb.puzzleSize.x = MAXVIEWS * w->skewb3d.viewSize.x + 1;
	}
	w->skewb.puzzleOffset.x = ((int) w->core.width - w->skewb.puzzleSize.x) / 2;
	w->skewb.puzzleOffset.y = ((int) w->core.height - w->skewb.puzzleSize.y) /
		2;
	ResizePolyhedrons(w);
}

static void
ExposeSkewb3D(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	Skewb3DWidget w = (Skewb3DWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->skewb.puzzleGC);
		DrawAllPolyhedrons((SkewbWidget) w);
	}
}

static      Boolean
SetValuesSkewb3D(Widget current, Widget request, Widget new)
{
	Skewb3DWidget c = (Skewb3DWidget) current, w = (Skewb3DWidget) new;
	Boolean     redraw = FALSE;

	if (w->skewb3d.cubeSize.x != c->skewb3d.cubeSize.x) {
		ResizeSkewb3D(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
MoveSkewb3DTl(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, TL,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb3DTop(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, TOP,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb3DTr(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, TR,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb3DLeft(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, LEFT,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb3DRight(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, RIGHT,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb3DBl(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, BL,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb3DBottom(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, BOTTOM,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveSkewb3DBr(Skewb3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveSkewbInput((SkewbWidget) w, event->xbutton.x, event->xbutton.y, BR,
		       (int) (event->xkey.state & ControlMask),
		       (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
ResizePolyhedrons(Skewb3DWidget w)
{
	int         face, orient, side, corner;
	XPoint      subcubeLoc3D[MAXFACES][MAXORIENT];
	XPoint      orientCubeLoc3D[2][MAXFACES][MAXORIENT];
	XPoint      subdiamondLoc3D[MAXFACES][MAXORIENT];
	XPoint      orientDiamondLoc3D[2][MAXFACES][MAXORIENT];

	w->skewb.letterOffset.x = -2;
	w->skewb.letterOffset.y = 3;
	w->skewb3d.viewMiddle.x = w->skewb3d.faceSize.x +
		w->skewb.puzzleOffset.x;
	w->skewb3d.viewMiddle.y = w->skewb3d.faceSize.y +
		w->skewb.puzzleOffset.y;
	for (face = 0; face < MAXFACES; face++) {
		faceLoc3D[face][0].x = w->skewb3d.viewMiddle.x;
		faceLoc3D[face][0].y = w->skewb3d.viewMiddle.y;
		for (orient = 1; orient < MAXORIENT; orient++) {
			faceLoc3D[face][orient].x = w->skewb3d.faceSize.x;
			faceLoc3D[face][orient].y = w->skewb3d.faceSize.y;
		}
	}
	if (w->skewb.vertical) {
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
			faceLoc3D[face][0].y += w->skewb3d.viewSize.y + 3;

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
			faceLoc3D[face][0].x += w->skewb3d.viewSize.x + 3;

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
		orientCubeLoc3D[0][face][0].x = orientCubeLoc3D[1][face][0].x = 0;
		orientCubeLoc3D[0][face][0].y = orientCubeLoc3D[1][face][0].y = 0;
		for (orient = 1; orient < MAXORIENT; orient++) {
			cubeLoc3D[face][orient].x = faceLoc3D[face][orient].x - 3 *
				w->skewb.delta * faceLoc3D[face][orient].x /
				w->skewb3d.faceSize.x;
			cubeLoc3D[face][orient].y = faceLoc3D[face][orient].y - 3 *
				w->skewb.delta * faceLoc3D[face][orient].y /
				w->skewb3d.faceSize.y;
			subcubeLoc3D[face][orient].x = (faceLoc3D[face][orient].x -
			     5 * faceLoc3D[face][orient].x * w->skewb.delta /
						  w->skewb3d.faceSize.x) / 2;
			subcubeLoc3D[face][orient].y = (faceLoc3D[face][orient].y -
			     5 * faceLoc3D[face][orient].y * w->skewb.delta /
						  w->skewb3d.faceSize.y) / 2;
			orientCubeLoc3D[0][face][orient].x = (faceLoc3D[face][orient].x -
			     5 * faceLoc3D[face][orient].x * w->skewb.delta /
						  w->skewb3d.faceSize.x) / 4;
			orientCubeLoc3D[0][face][orient].y = (faceLoc3D[face][orient].y -
			     5 * faceLoc3D[face][orient].y * w->skewb.delta /
						  w->skewb3d.faceSize.y) / 4;
			orientCubeLoc3D[1][face][orient].x = (faceLoc3D[face][orient].x -
			     7 * faceLoc3D[face][orient].x * w->skewb.delta /
						  w->skewb3d.faceSize.x) / 6;
			orientCubeLoc3D[1][face][orient].y = (faceLoc3D[face][orient].y -
			     7 * faceLoc3D[face][orient].y * w->skewb.delta /
						  w->skewb3d.faceSize.y) / 6;
		}
		triangleLoc3D[face][0][1].x = subcubeLoc3D[face][1].x;
		triangleLoc3D[face][0][2].x = subcubeLoc3D[face][2].x -
			subcubeLoc3D[face][1].x;
		triangleLoc3D[face][0][1].y = subcubeLoc3D[face][1].y;
		triangleLoc3D[face][0][2].y = subcubeLoc3D[face][2].y -
			subcubeLoc3D[face][1].y;
		triangleLoc3D[face][1][1].x = subcubeLoc3D[face][2].x;
		triangleLoc3D[face][1][2].x = -subcubeLoc3D[face][1].x -
			subcubeLoc3D[face][2].x;
		triangleLoc3D[face][1][1].y = subcubeLoc3D[face][2].y;
		triangleLoc3D[face][1][2].y = -subcubeLoc3D[face][1].y -
			subcubeLoc3D[face][2].y;
		triangleLoc3D[face][2][1].x = -subcubeLoc3D[face][1].x;
		triangleLoc3D[face][2][2].x = subcubeLoc3D[face][1].x -
			subcubeLoc3D[face][2].x;
		triangleLoc3D[face][2][1].y = -subcubeLoc3D[face][1].y;
		triangleLoc3D[face][2][2].y = subcubeLoc3D[face][1].y -
			subcubeLoc3D[face][2].y;
		triangleLoc3D[face][3][1].x = -subcubeLoc3D[face][2].x;
		triangleLoc3D[face][3][2].x = subcubeLoc3D[face][1].x +
			subcubeLoc3D[face][2].x;
		triangleLoc3D[face][3][1].y = -subcubeLoc3D[face][2].y;
		triangleLoc3D[face][3][2].y = subcubeLoc3D[face][1].y +
			subcubeLoc3D[face][2].y;
		for (orient = 0; orient < MAXORIENT; orient++) {
			letter3DList[face][orient].x =
				(2 * triangleLoc3D[face][orient][1].x +
				 triangleLoc3D[face][orient][2].x) / 3;
			letter3DList[face][orient].y =
				(2 * triangleLoc3D[face][orient][1].y +
				 triangleLoc3D[face][orient][2].y) / 3;
			triangleLoc3D[face][orient][3].x =
				-triangleLoc3D[face][orient][1].x - triangleLoc3D[face][orient][2].x;
			triangleLoc3D[face][orient][3].y =
				-triangleLoc3D[face][orient][1].y - triangleLoc3D[face][orient][2].y;
		}
	}
	w->skewb3d.cubeSize.x = w->skewb3d.faceSize.x - 2 * w->skewb.delta;
	w->skewb3d.cubeSize.y = w->skewb3d.faceSize.y - 2 * w->skewb.delta;
	w->skewb3d.cubeDiagonal = w->skewb3d.faceDiagonal - 2 * w->skewb.delta;
	w->skewb3d.cubeDiag = w->skewb3d.faceDiagonal + 2 * w->skewb.delta;

	if (w->skewb.vertical) {
		letter3DList[0][MAXORIENT].x = w->skewb3d.cubeSize.x / 4;
		letter3DList[0][MAXORIENT].y = -w->skewb3d.cubeSize.y / 2 + 2;
		letter3DList[1][MAXORIENT].x = -w->skewb3d.cubeDiagonal;
		letter3DList[1][MAXORIENT].y = 0;
		letter3DList[2][MAXORIENT].x = w->skewb3d.cubeSize.x / 4;
		letter3DList[2][MAXORIENT].y = w->skewb3d.cubeSize.y / 2 - 2;
		letter3DList[3][MAXORIENT].x = w->skewb3d.cubeDiagonal;
		letter3DList[3][MAXORIENT].y = 0;
		letter3DList[4][MAXORIENT].x = -w->skewb3d.cubeSize.x / 4;
		letter3DList[4][MAXORIENT].y = -w->skewb3d.cubeSize.y / 2 + 2;
		letter3DList[5][MAXORIENT].x = -w->skewb3d.cubeSize.x / 4;
		letter3DList[5][MAXORIENT].y = w->skewb3d.cubeSize.y / 2 - 2;
	} else {
		letter3DList[0][MAXORIENT].x = 0;
		letter3DList[0][MAXORIENT].y = -w->skewb3d.cubeDiagonal;
		letter3DList[1][MAXORIENT].x = -w->skewb3d.cubeSize.x / 2 + 2;
		letter3DList[1][MAXORIENT].y = w->skewb3d.cubeSize.y / 4;
		letter3DList[2][MAXORIENT].x = w->skewb3d.cubeSize.x / 2 - 2;
		letter3DList[2][MAXORIENT].y = w->skewb3d.cubeSize.y / 4;
		letter3DList[3][MAXORIENT].x = -w->skewb3d.cubeSize.x / 2 + 2;
		letter3DList[3][MAXORIENT].y = -w->skewb3d.cubeSize.y / 4;
		letter3DList[4][MAXORIENT].x = 0;
		letter3DList[4][MAXORIENT].y = w->skewb3d.cubeDiagonal;
		letter3DList[5][MAXORIENT].x = w->skewb3d.cubeSize.x / 2 - 2;
		letter3DList[5][MAXORIENT].y = -w->skewb3d.cubeSize.y / 4;
	}

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
			orientDiamondLoc3D[0][face][orient].x =
				(orientCubeLoc3D[0][face][orient].x +
				 orientCubeLoc3D[0][face][orient + 1].x) / 2;
			orientDiamondLoc3D[0][face][orient].y =
				(orientCubeLoc3D[0][face][orient].y +
				 orientCubeLoc3D[0][face][orient + 1].y) / 2;
			orientDiamondLoc3D[1][face][orient].x =
				(orientCubeLoc3D[1][face][orient].x +
				 orientCubeLoc3D[1][face][orient + 1].x) / 2;
			orientDiamondLoc3D[1][face][orient].y =
				(orientCubeLoc3D[1][face][orient].y +
				 orientCubeLoc3D[1][face][orient + 1].y) / 2;
		}
		/* Its a parallelagram so take advantage of that */
		diamondLoc3D[face][orient].x = (cubeLoc3D[face][MAXORIENT - 1].x -
				       cubeLoc3D[face][MAXORIENT / 2].x) / 2;
		diamondLoc3D[face][orient].y = (cubeLoc3D[face][MAXORIENT - 1].y -
				       cubeLoc3D[face][MAXORIENT / 2].y) / 2;
		diamondLoc3D[face][MAXORIENT].x = -diamondLoc3D[face][1].x -
			diamondLoc3D[face][2].x - diamondLoc3D[face][3].x;
		diamondLoc3D[face][MAXORIENT].y = -diamondLoc3D[face][1].y -
			diamondLoc3D[face][2].y - diamondLoc3D[face][3].y;

		subdiamondLoc3D[face][orient].x = (subcubeLoc3D[face][MAXORIENT - 1].x -
				    subcubeLoc3D[face][MAXORIENT / 2].x) / 2;
		subdiamondLoc3D[face][orient].y = (subcubeLoc3D[face][MAXORIENT - 1].y -
				    subcubeLoc3D[face][MAXORIENT / 2].y) / 2;
		orientDiamondLoc3D[0][face][orient].x =
			(orientCubeLoc3D[0][face][MAXORIENT - 1].x -
			 orientCubeLoc3D[0][face][MAXORIENT / 2].x) / 2;
		orientDiamondLoc3D[0][face][orient].y =
			(orientCubeLoc3D[0][face][MAXORIENT - 1].y -
			 orientCubeLoc3D[0][face][MAXORIENT / 2].y) / 2;
		orientDiamondLoc3D[1][face][orient].x =
			(orientCubeLoc3D[1][face][MAXORIENT - 1].x -
			 orientCubeLoc3D[1][face][MAXORIENT / 2].x) / 2;
		orientDiamondLoc3D[1][face][orient].y =
			(orientCubeLoc3D[1][face][MAXORIENT - 1].y -
			 orientCubeLoc3D[1][face][MAXORIENT / 2].y) / 2;

		MapFrom3D(face, 1, &corner);
		orientDiamond[face][corner][0].x = cubeLoc3D[face][1].x / 2;
		orientDiamond[face][corner][0].y = cubeLoc3D[face][1].y / 2;
		orientDiamond[face][corner][1].x = orientDiamond[face][corner][0].x +
			(cubeLoc3D[face][2].x - subcubeLoc3D[face][2].x) / 2;
		orientDiamond[face][corner][1].y = orientDiamond[face][corner][0].y +
			(cubeLoc3D[face][2].y - subcubeLoc3D[face][2].y) / 2;
		orientTriangle[face][corner][0].x = -orientCubeLoc3D[0][face][2].x / 2;
		orientTriangle[face][corner][0].y = -orientCubeLoc3D[0][face][2].y / 2;
		orientTriangle[face][corner][1].x = -orientCubeLoc3D[1][face][2].x / 2;
		orientTriangle[face][corner][1].y = -orientCubeLoc3D[1][face][2].y / 2;
		for (orient = 1; orient < MAXORIENT; orient++) {
			side = corner;
			MapFrom3D(face, (orient + 1) % MAXORIENT, &corner);
			orientDiamond[face][corner][0].x =
				orientDiamond[face][side][0].x +
				diamondLoc3D[face][orient].x;
			orientDiamond[face][corner][0].y =
				orientDiamond[face][side][0].y +
				diamondLoc3D[face][orient].y;
			orientDiamond[face][corner][1].x =
				orientDiamond[face][side][1].x +
				subdiamondLoc3D[face][orient].x;
			orientDiamond[face][corner][1].y =
				orientDiamond[face][side][1].y +
				subdiamondLoc3D[face][orient].y;
			orientTriangle[face][corner][0].x =
				orientTriangle[face][side][0].x +
				orientDiamondLoc3D[0][face][orient].x;
			orientTriangle[face][corner][0].y =
				orientTriangle[face][side][0].y +
				orientDiamondLoc3D[0][face][orient].y;
			orientTriangle[face][corner][1].x =
				orientTriangle[face][side][1].x +
				orientDiamondLoc3D[1][face][orient].x;
			orientTriangle[face][corner][1].y =
				orientTriangle[face][side][1].y +
				orientDiamondLoc3D[1][face][orient].y;
		}
	}
}

int
SelectPolyhedrons3D(Skewb3DWidget w, int x, int y, int *face, int *position)
{
	int         u, v, front, tl, ur, ul, found, side, x1, y1, x2, y2,
	            dx, dy;

	x1 = x;
	y1 = y;
	if (w->skewb.vertical) {
		x -= w->skewb3d.viewMiddle.x;
		front = (y < w->skewb3d.viewSize.y + w->skewb.puzzleOffset.y);
		if (!front)
			y -= (w->skewb3d.viewSize.y);
		tl = (y < w->skewb3d.viewMiddle.y);
		y -= w->skewb3d.viewMiddle.y;
		u = -w->skewb3d.faceSize.y * x + w->skewb3d.faceDiagonal * y;
		v = w->skewb3d.faceSize.y * x + w->skewb3d.faceDiagonal * y;
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
		y -= w->skewb3d.viewMiddle.y;
		front = (x < w->skewb3d.viewSize.x + w->skewb.puzzleOffset.x);
		if (!front)
			x -= (w->skewb3d.viewSize.x);
		tl = (x < w->skewb3d.viewMiddle.x);
		x -= w->skewb3d.viewMiddle.x;
		u = -w->skewb3d.faceSize.x * y + w->skewb3d.faceDiagonal * x;
		v = w->skewb3d.faceSize.x * y + w->skewb3d.faceDiagonal * x;
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
	x = x1;
	y = y1;
	found = 0;
	DiamondOffset3D(w, *face, &dx, &dy);
	for (side = 0; side < MAXORIENT; side++) {
		x1 = dx + orientDiamond[*face][side][0].x;
		y1 = dy + orientDiamond[*face][side][0].y;
		x2 = dx + orientDiamond[*face][(side + 1) % MAXORIENT][0].x;
		y2 = dy + orientDiamond[*face][(side + 1) % MAXORIENT][0].y;
		if ((x2 - x1) * (y - y1) <= (y2 - y1) * (x - x1)) {
			*position = side;
			found++;
		}
	}
	if (found == 0)
		*position = MAXORIENT;
	else if (found > 1)
		return FALSE;
	return TRUE;
}

int
NarrowSelection3D(Skewb3DWidget w, int *face, int *position, int *direction)
{
	switch (*direction) {
		case TR:
		case BR:
		case BL:
		case TL:
			if (w->skewb.vertical) {
				if (*face == 1)
					*direction = *direction + 2 * MAXORIENT;
				else if (*face == 3)
					*direction = (*direction + 3) % MAXORIENT + 2 * MAXORIENT;
			} else {
				if (*face == 0)
					*direction = (*direction + 3) % MAXORIENT + 2 * MAXORIENT;
				else if (*face == 4)
					*direction = (*direction + 2) % MAXORIENT + 2 * MAXORIENT;
				else if (*face == 5)
					*direction = (*direction + 2) % MAXORIENT;
			}
			break;
		case CCW:
		case CW:
			break;
		case TOP:
		case RIGHT:
		case BOTTOM:
		case LEFT:
			if (w->skewb.vertical) {
				if (*face == 1)
					*direction = (TL + *direction) % MAXORIENT;
				else if (*face == 3)
					*direction = (BL + *direction) % MAXORIENT;
			} else {
				if (*face == 0)
					*direction = (TR + *direction) % MAXORIENT;
				else if (*face == 4)
					*direction = (BR + *direction) % MAXORIENT;
				else if (*face == 5)
					*direction = (BL + *direction) % MAXORIENT + 2 * MAXORIENT;
			}
			break;
		default:
			return FALSE;
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
DrawFrame(Skewb3DWidget w, GC gc)
{
	int         face, dx, dy;

	dx = w->skewb3d.viewSize.x + w->skewb.puzzleOffset.x;
	dy = w->skewb3d.viewSize.y + w->skewb.puzzleOffset.y;
	if (w->skewb.vertical) {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  0, dy, dx + w->skewb.puzzleOffset.x + 1, dy);
		XDrawString(XtDisplay(w), XtWindow(w), gc,
			    (int) (2 * w->skewb.delta),
			(int) (3 * w->skewb.delta + w->skewb.letterOffset.y),
			    "Front", 5);
		XDrawString(XtDisplay(w), XtWindow(w), gc, (int)
			    (-4 * w->skewb.delta + 2 * 4 * w->skewb.letterOffset.x + w->core.width),
			    (int) (-w->skewb.delta - 2 * w->skewb.letterOffset.y + w->core.height),
			    "Back", 4);
	} else {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  dx, 0, dx, dy + w->skewb.puzzleOffset.y + 1);
		XDrawString(XtDisplay(w), XtWindow(w), gc,
			    (int) (2 * w->skewb.delta),
			(int) (3 * w->skewb.delta + w->skewb.letterOffset.y),
			    "Front", 5);
		XDrawString(XtDisplay(w), XtWindow(w), gc, (int)
			    (-4 * w->skewb.delta + 2 * 4 * w->skewb.letterOffset.x + w->core.width),
			    (int) (-w->skewb.delta - 2 * w->skewb.letterOffset.y + w->core.height),
			    "Back", 4);
	}
	for (face = 0; face < MAXFACES; face++)
		XDrawLines(XtDisplay(w), XtWindow(w), gc,
			   faceLoc3D[face], MAXORIENT, CoordModePrevious);
}

void
DrawDiamond3D(Skewb3DWidget w, int face)
{
	int         dx, dy;

	DiamondOffset3D(w, face, &dx, &dy);
	diamondLoc3D[face][0].x = dx + cubeLoc3D[face][1].x / 2;
	diamondLoc3D[face][0].y = dy + cubeLoc3D[face][1].y / 2;
	XFillPolygon(XtDisplay(w), XtWindow(w),
		     w->skewb.faceGC[w->skewb.cubeLoc[face][MAXORIENT].face],
		     diamondLoc3D[face], 4, Convex, CoordModePrevious);
	XDrawLines(XtDisplay(w), XtWindow(w),
		w->skewb.borderGC, diamondLoc3D[face], 5, CoordModePrevious);
	if (w->skewb.depth == 1 || w->skewb.mono) {
		int         letterX, letterY;
		char        buf[2];

		(void) sprintf(buf, "%c",
		w->skewb.faceName[w->skewb.cubeLoc[face][MAXORIENT].face][0]);
		letterX = dx + letter3DList[face][MAXORIENT].x + w->skewb.letterOffset.x;
		letterY = dy + letter3DList[face][MAXORIENT].y + w->skewb.letterOffset.y;
		XDrawString(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->skewb.orient)
		XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
			  dx +
			  orientDiamond[face][w->skewb.cubeLoc[face][MAXORIENT].rotation][0].x,
			  dy +
			  orientDiamond[face][w->skewb.cubeLoc[face][MAXORIENT].rotation][0].y,
			  dx +
			  orientDiamond[face][w->skewb.cubeLoc[face][MAXORIENT].rotation][1].x,
			  dy +
			  orientDiamond[face][w->skewb.cubeLoc[face][MAXORIENT].rotation][1].y);
}

void
DrawTriangle3D(Skewb3DWidget w, int face, int position, int offset)
{
	int         side, dx, dy, letterX, letterY;

	MapTo3D(face, position, &side);
	CubeOffset3D(w, face, side, &dx, &dy);
	letterX = dx + letter3DList[face][side].x;
	letterY = dy + letter3DList[face][side].y;
	triangleLoc3D[face][side][0].x = dx;
	triangleLoc3D[face][side][0].y = dy;
	if (offset) {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			     w->skewb.borderGC,
		    triangleLoc3D[face][side], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
		      w->skewb.faceGC[w->skewb.cubeLoc[face][position].face],
			   triangleLoc3D[face][side], 4, CoordModePrevious);
	} else {
		XFillPolygon(XtDisplay(w), XtWindow(w),
		      w->skewb.faceGC[w->skewb.cubeLoc[face][position].face],
		    triangleLoc3D[face][side], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			   w->skewb.borderGC, triangleLoc3D[face][side], 4, CoordModePrevious);
	}
	if (w->skewb.depth == 1 || w->skewb.mono) {
		char        buf[2];

		(void) sprintf(buf, "%c",
		w->skewb.faceName[w->skewb.cubeLoc[face][position].face][0]);
		XDrawString(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
			    letterX + w->skewb.letterOffset.x, letterY + w->skewb.letterOffset.y,
			    buf, 1);
	}
	if (w->skewb.orient)
		XDrawLine(XtDisplay(w), XtWindow(w), w->skewb.inverseGC,
			  letterX +
			  orientTriangle[face][w->skewb.cubeLoc[face][position].rotation][0].x,
			  letterY +
			  orientTriangle[face][w->skewb.cubeLoc[face][position].rotation][0].y,
			  letterX +
			  orientTriangle[face][w->skewb.cubeLoc[face][position].rotation][1].x,
			  letterY +
			  orientTriangle[face][w->skewb.cubeLoc[face][position].rotation][1].y);
}

static void
MapTo3D(int face, int side, int *corner)
{
	switch (face) {
		case 0:
			*corner = (side + 2) % MAXORIENT;
			break;
		case 1:
		case 5:
			*corner = side;
			break;
		case 2:
			*corner = (side + 1) % MAXORIENT;
			break;
		case 3:
		case 4:
			*corner = (side + 3) % MAXORIENT;
			break;
		default:
			(void) printf("MapTo3D: face %d\n", face);
	}
}

static void
MapFrom3D(int face, int corner, int *side)
{
	switch (face) {
		case 0:
			*side = (corner + 2) % MAXORIENT;
			break;
		case 1:
		case 5:
			*side = corner;
			break;
		case 2:
			*side = (corner + 3) % MAXORIENT;
			break;
		case 3:
		case 4:
			*side = (corner + 1) % MAXORIENT;
			break;
		default:
			(void) printf("MapFrom3D: face %d\n", face);
	}
}

static void
DiamondOffset3D(Skewb3DWidget w, int face, int *dx, int *dy)
{
	if (w->skewb.vertical) {
		switch (face) {
			case 0:
				*dx = w->skewb3d.viewMiddle.x + w->skewb.delta - 1;
				*dy = w->skewb3d.viewMiddle.y - w->skewb.delta - 2;
				break;
			case 1:
				*dx = w->skewb3d.viewMiddle.x - 2 * w->skewb.delta;
				*dy = w->skewb3d.viewMiddle.y;
				break;
			case 2:
				*dx = w->skewb3d.viewMiddle.x + w->skewb.delta - 1;
				*dy = w->skewb3d.viewMiddle.y + 2 * w->skewb.delta - 1;
				break;
			case 3:
				*dx = w->skewb3d.viewMiddle.x + 2 * w->skewb.delta;
				*dy = w->skewb3d.viewSize.y + w->skewb3d.viewMiddle.y +
					w->skewb.delta - 1;
				break;
			case 4:
				*dx = w->skewb3d.viewMiddle.x - w->skewb.delta + 1;
				*dy = w->skewb3d.viewSize.y + w->skewb3d.viewMiddle.y -
					w->skewb.delta + 1;
				break;
			case 5:
				*dx = w->skewb3d.viewMiddle.x - 2;
				*dy = w->skewb3d.viewSize.y + w->skewb3d.viewMiddle.y +
					2 * w->skewb.delta + 2;
				break;
			default:
				(void) printf("DiamondOffset3D: face %d\n", face);
		}
	} else {
		switch (face) {
			case 0:
				*dx = w->skewb3d.viewMiddle.x;
				*dy = w->skewb3d.viewMiddle.y - 2 * w->skewb.delta + 1;
				break;
			case 1:
				*dx = w->skewb3d.viewMiddle.x - w->skewb.delta - 2;
				*dy = w->skewb3d.viewMiddle.y + w->skewb.delta;
				break;
			case 2:
				*dx = w->skewb3d.viewMiddle.x + 2 * w->skewb.delta - 1;
				*dy = w->skewb3d.viewMiddle.y + w->skewb.delta;
				break;
			case 3:
				*dx = w->skewb3d.viewSize.x + w->skewb3d.viewMiddle.x -
					w->skewb.delta + 1;
				*dy = w->skewb3d.viewMiddle.y - w->skewb.delta;
				break;
			case 4:
				*dx = w->skewb3d.viewSize.x + w->skewb3d.viewMiddle.x +
					w->skewb.delta - 1;
				*dy = w->skewb3d.viewMiddle.y + 2 * w->skewb.delta;
				break;
			case 5:
				*dx = w->skewb3d.viewSize.x + w->skewb3d.viewMiddle.x +
					2 * w->skewb.delta + 2;
				*dy = w->skewb3d.viewMiddle.y - w->skewb.delta;
				break;
			default:
				(void) printf("DiamondOffset3D: face %d\n", face);
		}
	}
}

static void
CubeOffset3D(Skewb3DWidget w, int face, int corner, int *dx, int *dy)
{
	int         side;

	DiamondOffset3D(w, face, dx, dy);
	for (side = 1; side <= corner; side++) {
		*dx += cubeLoc3D[face][side].x;
		*dy += cubeLoc3D[face][side].y;
	}
}
