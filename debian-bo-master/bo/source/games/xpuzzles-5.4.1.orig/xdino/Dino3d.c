
/*-
# X-BASED DINOSAUR CUBE
#
#  Dino3d.c
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

/* Methods file for Dino3d */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "DinoP.h"
#include "Dino3dP.h"

static void InitializeDino3D(Widget request, Widget new);
static void ExposeDino3D(Widget new, XEvent * event, Region region);
static void ResizeDino3D(Dino3DWidget w);
static Boolean SetValuesDino3D(Widget current, Widget request, Widget new);
static void MoveDino3DCcw(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DTl(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DTop(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DTr(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DLeft(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DCw(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DRight(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DBl(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DBottom(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void MoveDino3DBr(Dino3DWidget w, XEvent * event, char **args, int nArgs);
static void ResizePolyhedrons(Dino3DWidget w);
static void DrawFrame(Dino3DWidget w, GC gc);
static void DiamondOffset3D(Dino3DWidget w, int face, int *dx, int *dy);
static void MapFrom3D(int face, int side, int *corner);
static void MapTo3D(int face, int corner, int *side);
static void CubeOffset3D(Dino3DWidget w, int face, int side, int *dx, int *dy);

static char defaultTranslationsDino3D[] =
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
   <KeyPress>o: Orientize()\n\
   <KeyPress>2: Period2()\n\
   <KeyPress>3: Period3()\n\
   <KeyPress>b: Both()";

static XtActionsRec actionsListDino3D[] =
{
	{"Quit", (XtActionProc) QuitDino},
	{"MoveCcw", (XtActionProc) MoveDino3DCcw},
	{"MoveTl", (XtActionProc) MoveDino3DTl},
	{"MoveTop", (XtActionProc) MoveDino3DTop},
	{"MoveTr", (XtActionProc) MoveDino3DTr},
	{"MoveLeft", (XtActionProc) MoveDino3DLeft},
	{"MoveCw", (XtActionProc) MoveDino3DCw},
	{"MoveRight", (XtActionProc) MoveDino3DRight},
	{"MoveBl", (XtActionProc) MoveDino3DBl},
	{"MoveBottom", (XtActionProc) MoveDino3DBottom},
	{"MoveBr", (XtActionProc) MoveDino3DBr},
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

static XtResource resourcesDino3D[] =
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
	 XtOffset(DinoWidget, core.width), XtRString, "250"},
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

Dino3DClassRec dino3dClassRec =
{
	{
		(WidgetClass) & dinoClassRec,	/* superclass */
		"Dino3D",	/* class name */
		sizeof (Dino3DRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeDino3D,	/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListDino3D,	/* actions */
		XtNumber(actionsListDino3D),	/* num actions */
		resourcesDino3D,	/* resources */
		XtNumber(resourcesDino3D),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		NULL,		/* destroy */
		(XtWidgetProc) ResizeDino3D,	/* resize */
		(XtExposeProc) ExposeDino3D,	/* expose */
		(XtSetValuesFunc) SetValuesDino3D,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		XtInheritAcceptFocus,	/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsDino3D,	/* tm table */
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

WidgetClass dino3dWidgetClass = (WidgetClass) & dino3dClassRec;

static XPoint faceLoc3D[MAXFACES][MAXORIENT];
static XPoint cubeLoc3D[MAXFACES][MAXORIENT];
static XPoint diamondLoc3D[MAXFACES][MAXORIENT];
static XPoint tinyDiamondLoc3D[MAXFACES][MAXORIENT];
static XPoint triangleLoc3D[MAXFACES][MAXORIENT][4];
static XPoint letter3DList[MAXFACES][MAXORIENT];
static XPoint orientTriangle[MAXFACES][MAXORIENT][2];

static void
InitializeDino3D(Widget request, Widget new)
{
	Dino3DWidget w = (Dino3DWidget) new;

	w->dino.dim = 3;
	ResizeDino3D(w);
}

static void
ResizeDino3D(Dino3DWidget w)
{
	XPoint      tempSize;

	w->dino.delta = 4;
	w->dino.vertical = (w->core.height >= w->core.width);
	if (w->dino.vertical) {
		tempSize.y = w->core.height / MAXVIEWS;
		tempSize.x = w->core.width;
		if (tempSize.x >= DIVIDE(tempSize.y)) {
			w->dino3d.cubeSize.y = MAX((tempSize.y - 3 * w->dino.delta) / 2 -
						   w->dino.delta - 2, 0);
			w->dino3d.cubeSize.x = DIVIDE(w->dino3d.cubeSize.y);
		} else {
			w->dino3d.cubeSize.x = MAX((tempSize.x - 2 * w->dino.delta - 7) / 2 -
						   w->dino.delta, 0);
			w->dino3d.cubeSize.y = MULTIPLY(w->dino3d.cubeSize.x);
		}
		w->dino3d.cubeDiagonal = w->dino3d.cubeSize.x / 2;
		w->dino3d.faceSize.x = w->dino3d.cubeSize.x + 2 * w->dino.delta + 1;
		w->dino3d.faceSize.y = w->dino3d.cubeSize.y + 2 * w->dino.delta + 1;
		w->dino3d.faceDiagonal = w->dino3d.faceSize.x / 2;
		w->dino3d.viewSize.x = 2 * w->dino3d.faceSize.x + 3;
		w->dino3d.viewSize.y = 2 * w->dino3d.faceSize.y + 3;
		w->dino.puzzleSize.x = w->dino3d.viewSize.x + 1;
		w->dino.puzzleSize.y = MAXVIEWS * w->dino3d.viewSize.y + 1;
	} else {
		tempSize.x = w->core.width / MAXVIEWS;
		tempSize.y = w->core.height;
		if (tempSize.y >= DIVIDE(tempSize.x)) {
			w->dino3d.cubeSize.x = MAX((tempSize.x - 3 * w->dino.delta) / 2 -
						   w->dino.delta - 2, 0);
			w->dino3d.cubeSize.y = DIVIDE(w->dino3d.cubeSize.x);
		} else {
			w->dino3d.cubeSize.y = MAX((tempSize.y - 2 * w->dino.delta - 7) / 2 -
						   w->dino.delta, 0);
			w->dino3d.cubeSize.x = MULTIPLY(w->dino3d.cubeSize.y);
		}
		w->dino3d.cubeDiagonal = w->dino3d.cubeSize.y / 2;
		w->dino3d.faceSize.y = w->dino3d.cubeSize.y + 2 * w->dino.delta + 1;
		w->dino3d.faceSize.x = w->dino3d.cubeSize.x + 2 * w->dino.delta + 1;
		w->dino3d.faceDiagonal = w->dino3d.faceSize.y / 2;
		w->dino3d.viewSize.y = 2 * w->dino3d.faceSize.y + 3;
		w->dino3d.viewSize.x = 2 * w->dino3d.faceSize.x + 3;
		w->dino.puzzleSize.y = w->dino3d.viewSize.y + 1;
		w->dino.puzzleSize.x = MAXVIEWS * w->dino3d.viewSize.x + 1;
	}
	w->dino.puzzleOffset.x = ((int) w->core.width - w->dino.puzzleSize.x) / 2;
	w->dino.puzzleOffset.y = ((int) w->core.height - w->dino.puzzleSize.y) /
		2;
	ResizePolyhedrons(w);
}

static void
ExposeDino3D(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	Dino3DWidget w = (Dino3DWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->dino.puzzleGC);
		DrawAllPolyhedrons((DinoWidget) w);
	}
}

static      Boolean
SetValuesDino3D(Widget current, Widget request, Widget new)
{
	Dino3DWidget c = (Dino3DWidget) current, w = (Dino3DWidget) new;
	Boolean     redraw = FALSE;

	if (w->dino3d.cubeSize.x != c->dino3d.cubeSize.x) {
		ResizeDino3D(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
MoveDino3DTl(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, TL,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DTop(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, TOP,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DTr(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, TR,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DLeft(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, LEFT,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DCw(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, CW,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DRight(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, RIGHT,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DBl(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, BL,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DBottom(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, BOTTOM,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DBr(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, BR,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
MoveDino3DCcw(Dino3DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveDinoInput((DinoWidget) w, event->xbutton.x, event->xbutton.y, CCW,
		      (int) (event->xkey.state & (ShiftMask | LockMask)),
		      (int) (event->xkey.state & ControlMask),
		      (int) (event->xkey.state &
		    (Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask)));
}

static void
ResizePolyhedrons(Dino3DWidget w)
{
	int         face, orient, side, corner;
	XPoint      subcubeLoc3D[MAXFACES][MAXORIENT];
	XPoint      orientCubeLoc3D[2][MAXFACES][MAXORIENT];
	XPoint      subdiamondLoc3D[MAXFACES][MAXORIENT];
	XPoint      orientDiamondLoc3D[2][MAXFACES][MAXORIENT];

	w->dino.letterOffset.x = -2;
	w->dino.letterOffset.y = 3;
	w->dino3d.viewMiddle.x = w->dino3d.faceSize.x +
		w->dino.puzzleOffset.x;
	w->dino3d.viewMiddle.y = w->dino3d.faceSize.y +
		w->dino.puzzleOffset.y;
	for (face = 0; face < MAXFACES; face++) {
		faceLoc3D[face][0].x = w->dino3d.viewMiddle.x;
		faceLoc3D[face][0].y = w->dino3d.viewMiddle.y;
		for (orient = 1; orient < MAXORIENT; orient++) {
			faceLoc3D[face][orient].x = w->dino3d.faceSize.x;
			faceLoc3D[face][orient].y = w->dino3d.faceSize.y;
		}
	}
	if (w->dino.vertical) {
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
			faceLoc3D[face][0].y += w->dino3d.viewSize.y + 3;

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
			faceLoc3D[face][0].x += w->dino3d.viewSize.x + 3;

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
				w->dino.delta * faceLoc3D[face][orient].x /
				w->dino3d.faceSize.x;
			cubeLoc3D[face][orient].y = faceLoc3D[face][orient].y - 3 *
				w->dino.delta * faceLoc3D[face][orient].y /
				w->dino3d.faceSize.y;
			subcubeLoc3D[face][orient].x = faceLoc3D[face][orient].x -
				4 * faceLoc3D[face][orient].x * w->dino.delta /
				w->dino3d.faceSize.x;
			subcubeLoc3D[face][orient].y = faceLoc3D[face][orient].y -
				4 * faceLoc3D[face][orient].y * w->dino.delta /
				w->dino3d.faceSize.y;
			orientCubeLoc3D[0][face][orient].x = (faceLoc3D[face][orient].x -
			      5 * faceLoc3D[face][orient].x * w->dino.delta /
						   w->dino3d.faceSize.x) / 3;
			orientCubeLoc3D[0][face][orient].y = (faceLoc3D[face][orient].y -
			      5 * faceLoc3D[face][orient].y * w->dino.delta /
						   w->dino3d.faceSize.y) / 3;
			orientCubeLoc3D[1][face][orient].x = (faceLoc3D[face][orient].x -
			      7 * faceLoc3D[face][orient].x * w->dino.delta /
						   w->dino3d.faceSize.x) / 6;
			orientCubeLoc3D[1][face][orient].y = (faceLoc3D[face][orient].y -
			      7 * faceLoc3D[face][orient].y * w->dino.delta /
						   w->dino3d.faceSize.y) / 6;
		}

		MapFrom3D(face, 1, &corner);
		orientTriangle[face][corner][0].x = -orientCubeLoc3D[0][face][2].x / 2;
		orientTriangle[face][corner][0].y = -orientCubeLoc3D[0][face][2].y / 2;
		orientTriangle[face][corner][1].x = -orientCubeLoc3D[1][face][2].x / 2;
		orientTriangle[face][corner][1].y = -orientCubeLoc3D[1][face][2].y / 2;
		for (orient = 0; orient < MAXORIENT - 1; orient++) {
			diamondLoc3D[face][orient].x = (cubeLoc3D[face][orient].x +
					  cubeLoc3D[face][orient + 1].x) / 2;
			diamondLoc3D[face][orient].y = (cubeLoc3D[face][orient].y +
					  cubeLoc3D[face][orient + 1].y) / 2;
			subdiamondLoc3D[face][orient].x = (subcubeLoc3D[face][orient].x +
				       subcubeLoc3D[face][orient + 1].x) / 2;
			subdiamondLoc3D[face][orient].y = (subcubeLoc3D[face][orient].y +
				       subcubeLoc3D[face][orient + 1].y) / 2;
			tinyDiamondLoc3D[face][orient + 1].x = diamondLoc3D[face][orient].x -
				subdiamondLoc3D[face][orient].x;
			tinyDiamondLoc3D[face][orient + 1].y = diamondLoc3D[face][orient].y -
				subdiamondLoc3D[face][orient].y;
			side = corner;
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
		subdiamondLoc3D[face][orient].x = (subcubeLoc3D[face][MAXORIENT - 1].x -
				    subcubeLoc3D[face][MAXORIENT / 2].x) / 2;
		subdiamondLoc3D[face][orient].y = (subcubeLoc3D[face][MAXORIENT - 1].y -
				    subcubeLoc3D[face][MAXORIENT / 2].y) / 2;
		tinyDiamondLoc3D[face][0].x = diamondLoc3D[face][orient].x -
			subdiamondLoc3D[face][orient].x;
		tinyDiamondLoc3D[face][0].y = diamondLoc3D[face][orient].y -
			subdiamondLoc3D[face][orient].y;
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
		orientTriangle[face][corner][0].x = -orientCubeLoc3D[0][face][2].x / 2;
		orientTriangle[face][corner][0].y = -orientCubeLoc3D[0][face][2].y / 2;
		orientTriangle[face][corner][1].x = -orientCubeLoc3D[1][face][2].x / 2;
		orientTriangle[face][corner][1].y = -orientCubeLoc3D[1][face][2].y / 2;

		triangleLoc3D[face][0][1].x = subdiamondLoc3D[face][2].x;
		triangleLoc3D[face][0][2].x = -subdiamondLoc3D[face][1].x -
			subdiamondLoc3D[face][2].x;
		triangleLoc3D[face][0][1].y = subdiamondLoc3D[face][2].y;
		triangleLoc3D[face][0][2].y = -subdiamondLoc3D[face][1].y -
			subdiamondLoc3D[face][2].y;
		triangleLoc3D[face][1][1].x = -subdiamondLoc3D[face][1].x;
		triangleLoc3D[face][1][2].x = subdiamondLoc3D[face][1].x -
			subdiamondLoc3D[face][2].x;
		triangleLoc3D[face][1][1].y = -subdiamondLoc3D[face][1].y;
		triangleLoc3D[face][1][2].y = subdiamondLoc3D[face][1].y -
			subdiamondLoc3D[face][2].y;
		triangleLoc3D[face][2][1].x = -subdiamondLoc3D[face][2].x;
		triangleLoc3D[face][2][2].x = subdiamondLoc3D[face][1].x +
			subdiamondLoc3D[face][2].x;
		triangleLoc3D[face][2][1].y = -subdiamondLoc3D[face][2].y;
		triangleLoc3D[face][2][2].y = subdiamondLoc3D[face][1].y +
			subdiamondLoc3D[face][2].y;
		triangleLoc3D[face][3][1].x = subdiamondLoc3D[face][1].x;
		triangleLoc3D[face][3][2].x = subdiamondLoc3D[face][2].x -
			subdiamondLoc3D[face][1].x;
		triangleLoc3D[face][3][1].y = subdiamondLoc3D[face][1].y;
		triangleLoc3D[face][3][2].y = subdiamondLoc3D[face][2].y -
			subdiamondLoc3D[face][1].y;
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
		for (orient = 1; orient < MAXORIENT; orient++) {
			side = corner;
			MapFrom3D(face, (orient + 1) % MAXORIENT, &corner);
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
	w->dino3d.cubeSize.x = w->dino3d.faceSize.x - 2 * w->dino.delta;
	w->dino3d.cubeSize.y = w->dino3d.faceSize.y - 2 * w->dino.delta;
	w->dino3d.cubeDiagonal = w->dino3d.faceDiagonal - 2 * w->dino.delta;
	w->dino3d.cubeDiag = w->dino3d.faceDiagonal + 2 * w->dino.delta;
}

int
SelectPolyhedrons3D(Dino3DWidget w, int x, int y, int *face, int *position)
{
	int         u, v, front, tl, ur, ul, found, corner, x1, y1, x2,
	            y2, dx, dy;

	x1 = x;
	y1 = y;
	if (w->dino.vertical) {
		x -= w->dino3d.viewMiddle.x;
		front = (y < w->dino3d.viewSize.y + w->dino.puzzleOffset.y);
		if (!front)
			y -= (w->dino3d.viewSize.y);
		tl = (y < w->dino3d.viewMiddle.y);
		y -= w->dino3d.viewMiddle.y;
		u = -w->dino3d.faceSize.y * x + w->dino3d.faceDiagonal * y;
		v = w->dino3d.faceSize.y * x + w->dino3d.faceDiagonal * y;
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
		y -= w->dino3d.viewMiddle.y;
		front = (x < w->dino3d.viewSize.x + w->dino.puzzleOffset.x);
		if (!front)
			x -= (w->dino3d.viewSize.x);
		tl = (x < w->dino3d.viewMiddle.x);
		x -= w->dino3d.viewMiddle.x;
		u = -w->dino3d.faceSize.x * y + w->dino3d.faceDiagonal * x;
		v = w->dino3d.faceSize.x * y + w->dino3d.faceDiagonal * x;
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
	for (corner = 0; corner < MAXORIENT; corner++) {
		int         ok = 0;

		x1 = dx + triangleLoc3D[*face][corner][1].x;
		y1 = dy + triangleLoc3D[*face][corner][1].y;
		x2 = x1 + triangleLoc3D[*face][corner][2].x;
		y2 = y1 + triangleLoc3D[*face][corner][2].y;
		if ((x1 - dx) * (y2 - dy) <= (y1 - dy) * (x2 - dx)) {
			if ((x1 - dx) * (y - dy) <= (y1 - dy) * (x - dx))
				ok++;
		} else if ((x1 - dx) * (y - dy) >= (y1 - dy) * (x - dx))
			ok++;
		if ((x2 - dx) * (y1 - dy) <= (y2 - dy) * (x1 - dx)) {
			if ((x2 - dx) * (y - dy) <= (y2 - dy) * (x - dx))
				ok++;
		} else if ((x2 - dx) * (y - dy) >= (y2 - dy) * (x - dx))
			ok++;
		if (ok == 2) {
			MapFrom3D(*face, corner, position);
			found++;
		}
	}
	if (found != 1)
		return FALSE;
	return TRUE;
}


int
NarrowSelection3D(Dino3DWidget w, int *face, int *direction)
{
	switch (*direction) {
		case TR:
		case BR:
		case BL:
		case TL:
			if (w->dino.vertical) {
				if (*face == 1)
					*direction = *direction + 2 * MAXORIENT;
				else if (*face == 3)
					*direction = (*direction + 3) % MAXORIENT + 2 * MAXORIENT;
			} else {
				if (*face == 0)
					*direction = (*direction + 3) % MAXORIENT + 2 * MAXORIENT;
				else if (*face == 4)
					*direction = (*direction + 2) % MAXORIENT + 2 * MAXORIENT;
				if (*face == 5)
					*direction = (*direction + 2) % MAXORIENT;
			}
			break;
		case CCW:
		case CW:
			if (*face == 3 || *face == 4)
				*direction = (TR + *direction - CW) % MAXORIENT;
			else if (*face == 0)
				*direction = (BR + *direction - CW) % MAXORIENT;
			else if (*face == 2)
				*direction = (BL + *direction - CW) % MAXORIENT;
			else if (*face == 1 || *face == 5)
				*direction = (TL + *direction - CW) % MAXORIENT;
			break;
		case TOP:
		case RIGHT:
		case BOTTOM:
		case LEFT:
			if (w->dino.vertical) {
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
	return TRUE;
}

static void
DrawFrame(Dino3DWidget w, GC gc)
{
	int         face, dx, dy;

	dx = w->dino3d.viewSize.x + w->dino.puzzleOffset.x;
	dy = w->dino3d.viewSize.y + w->dino.puzzleOffset.y;
	if (w->dino.vertical) {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  0, dy, dx + w->dino.puzzleOffset.x + 1, dy);
		XDrawString(XtDisplay(w), XtWindow(w), gc,
			    (int) (2 * w->dino.delta),
			  (int) (3 * w->dino.delta + w->dino.letterOffset.y),
			    "Front", 5);
		XDrawString(XtDisplay(w), XtWindow(w), gc, (int)
			    (-4 * w->dino.delta + 2 * 4 * w->dino.letterOffset.x + w->core.width),
			    (int) (-w->dino.delta - 2 * w->dino.letterOffset.y + w->core.height),
			    "Back", 4);
	} else {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  dx, 0, dx, dy + w->dino.puzzleOffset.y + 1);
		XDrawString(XtDisplay(w), XtWindow(w), gc,
			    (int) (2 * w->dino.delta),
			  (int) (3 * w->dino.delta + w->dino.letterOffset.y),
			    "Front", 5);
		XDrawString(XtDisplay(w), XtWindow(w), gc, (int)
			    (-4 * w->dino.delta + 2 * 4 * w->dino.letterOffset.x + w->core.width),
			    (int) (-w->dino.delta - 2 * w->dino.letterOffset.y + w->core.height),
			    "Back", 4);
	}
	for (face = 0; face < MAXFACES; face++)
		XDrawLines(XtDisplay(w), XtWindow(w), gc,
			   faceLoc3D[face], MAXORIENT, CoordModePrevious);
}

void
DrawTriangle3D(Dino3DWidget w, int face, int position, int offset)
{
	int         corner, dx, dy, letterX, letterY;

	MapTo3D(face, position, &corner);
	CubeOffset3D(w, face, corner, &dx, &dy);
	letterX = dx + letter3DList[face][corner].x;
	letterY = dy + letter3DList[face][corner].y;
	triangleLoc3D[face][corner][0].x = dx;
	triangleLoc3D[face][corner][0].y = dy;
	if (offset) {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			     w->dino.borderGC,
		  triangleLoc3D[face][corner], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			w->dino.faceGC[w->dino.cubeLoc[face][position].face],
			   triangleLoc3D[face][corner], 4, CoordModePrevious);
	} else {
		XFillPolygon(XtDisplay(w), XtWindow(w),
			w->dino.faceGC[w->dino.cubeLoc[face][position].face],
		  triangleLoc3D[face][corner], 3, Convex, CoordModePrevious);
		XDrawLines(XtDisplay(w), XtWindow(w),
			   w->dino.borderGC, triangleLoc3D[face][corner], 4, CoordModePrevious);
	}
	if (w->dino.depth == 1 || w->dino.mono) {
		char        buf[2];

		(void) sprintf(buf, "%c",
		  w->dino.faceName[w->dino.cubeLoc[face][position].face][0]);
		XDrawString(XtDisplay(w), XtWindow(w), w->dino.inverseGC,
			    letterX + w->dino.letterOffset.x, letterY + w->dino.letterOffset.y,
			    buf, 1);
	}
	if (w->dino.orient)
		XDrawLine(XtDisplay(w), XtWindow(w), w->dino.inverseGC,
			  letterX +
			  orientTriangle[face][w->dino.cubeLoc[face][position].rotation][0].x,
			  letterY +
			  orientTriangle[face][w->dino.cubeLoc[face][position].rotation][0].y,
			  letterX +
			  orientTriangle[face][w->dino.cubeLoc[face][position].rotation][1].x,
			  letterY +
			  orientTriangle[face][w->dino.cubeLoc[face][position].rotation][1].y);
}

static void
MapTo3D(int face, int corner, int *side)
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
			*side = (corner + 1) % MAXORIENT;
			break;
		case 3:
		case 4:
			*side = (corner + 3) % MAXORIENT;
			break;
		default:
			(void) printf("MapTo3D: face %d\n", face);
	}
}

static void
MapFrom3D(int face, int side, int *corner)
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
			*corner = (side + 3) % MAXORIENT;
			break;
		case 3:
		case 4:
			*corner = (side + 1) % MAXORIENT;
			break;
		default:
			(void) printf("MapFrom3D: face %d\n", face);
	}
}

static void
DiamondOffset3D(Dino3DWidget w, int face, int *dx, int *dy)
{
	if (w->dino.vertical) {
		switch (face) {
			case 0:
				*dx = (2 * faceLoc3D[0][0].x + faceLoc3D[0][1].x +
				       faceLoc3D[0][2].x) / 2;
				*dy = (2 * faceLoc3D[0][0].y + faceLoc3D[0][1].y) / 2;
				break;
			case 1:
				*dx = faceLoc3D[1][0].x + faceLoc3D[1][1].x;
				*dy = faceLoc3D[1][0].y;
				break;
			case 2:
				*dx = (2 * faceLoc3D[2][0].x + faceLoc3D[2][1].x +
				       faceLoc3D[2][2].x) / 2;
				*dy = (2 * faceLoc3D[2][0].y + faceLoc3D[2][1].y +
				       faceLoc3D[2][2].y) / 2;
				break;
			case 3:
				*dx = faceLoc3D[3][0].x + faceLoc3D[3][1].x;
				*dy = faceLoc3D[3][0].y;
				break;
			case 4:
				*dx = (2 * faceLoc3D[4][0].x + faceLoc3D[4][1].x +
				       faceLoc3D[4][2].x) / 2;
				*dy = (2 * faceLoc3D[4][0].y + faceLoc3D[4][1].y +
				       faceLoc3D[4][2].y) / 2;
				break;
			case 5:
				*dx = (2 * faceLoc3D[5][0].x + faceLoc3D[5][1].x +
				       faceLoc3D[5][2].x) / 2;
				*dy = (2 * faceLoc3D[5][0].y + faceLoc3D[5][1].y) / 2;
				break;
			default:
				(void) printf("DiamondOffset3D: face %d\n", face);
		}
	} else {
		switch (face) {
			case 0:
				*dx = faceLoc3D[0][0].x;
				*dy = faceLoc3D[0][0].y + faceLoc3D[0][1].y;
				break;
			case 1:
				*dx = (2 * faceLoc3D[1][0].x + faceLoc3D[1][1].x +
				       faceLoc3D[1][2].x) / 2;
				*dy = (2 * faceLoc3D[1][0].y + faceLoc3D[1][1].y +
				       faceLoc3D[1][2].y) / 2;
				break;
			case 2:
				*dx = (2 * faceLoc3D[2][0].x + faceLoc3D[2][1].x) / 2;
				*dy = (2 * faceLoc3D[2][0].y + faceLoc3D[2][1].y +
				       faceLoc3D[2][2].y) / 2;
				break;
			case 3:
				*dx = (2 * faceLoc3D[3][0].x + faceLoc3D[3][1].x) / 2;
				*dy = (2 * faceLoc3D[3][0].y + faceLoc3D[3][1].y +
				       faceLoc3D[3][2].y) / 2;
				break;
			case 4:
				*dx = faceLoc3D[4][0].x;
				*dy = faceLoc3D[4][0].y + faceLoc3D[4][1].y;
				break;
			case 5:
				*dx = (2 * faceLoc3D[5][0].x + faceLoc3D[5][1].x +
				       faceLoc3D[5][2].x) / 2;
				*dy = (2 * faceLoc3D[5][0].y + faceLoc3D[5][1].y +
				       faceLoc3D[5][2].y) / 2;
				break;
			default:
				(void) printf("DiamondOffset3D: face %d\n", face);
		}
	}
}

static void
CubeOffset3D(Dino3DWidget w, int face, int side, int *dx, int *dy)
{
	int         corner;

	DiamondOffset3D(w, face, dx, dy);
	for (corner = 1; corner <= side; corner++) {
		*dx += tinyDiamondLoc3D[face][corner].x;
		*dy += tinyDiamondLoc3D[face][corner].y;
	}
}
