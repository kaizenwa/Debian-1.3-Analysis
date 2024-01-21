/*-
# X-BASED RUBIK'S CUBE(tm)
#
#  Rubik2d.c
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

/* Methods file for Rubik2d */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "RubikP.h"
#include "Rubik2dP.h"

static void InitializeRubik2D(Widget request, Widget new);
static void ResizeRubik2D(Rubik2DWidget w);
static void ExposeRubik2D(Widget new, XEvent * event, Region region);
static Boolean SetValuesRubik2D(Widget current, Widget request, Widget new);
static void MoveRubik2DTop(Rubik2DWidget w,
			   XEvent * event, char **args, int nArgs);
static void MoveRubik2DLeft(Rubik2DWidget w,
			    XEvent * event, char **args, int nArgs);
static void MoveRubik2DRight(Rubik2DWidget w,
			     XEvent * event, char **args, int nArgs);
static void MoveRubik2DBottom(Rubik2DWidget w,
			      XEvent * event, char **args, int nArgs);
static void ResizePolyhedrons(Rubik2DWidget w);
static void DrawFrame(Rubik2DWidget w, GC gc);
static void DrawOrientLine(Rubik2DWidget w, int orient, int dx, int dy);

static char defaultTranslationsRubik2D[] =
"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
   <KeyPress>KP_Divide: MoveCcw()\n\
   <KeyPress>Up: MoveTop()\n\
   <KeyPress>KP_8: MoveTop()\n\
   <KeyPress>R8: MoveTop()\n\
   <KeyPress>Left: MoveLeft()\n\
   <KeyPress>KP_4: MoveLeft()\n\
   <KeyPress>R10: MoveLeft()\n\
   <KeyPress>Begin: MoveCw()\n\
   <KeyPress>KP_5: MoveCw()\n\
   <KeyPress>R11: MoveCw()\n\
   <KeyPress>Right: MoveRight()\n\
   <KeyPress>KP_6: MoveRight()\n\
   <KeyPress>R12: MoveRight()\n\
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
   <KeyPress>o: Orientize()";

static XtActionsRec actionsListRubik2D[] =
{
	{"Quit", (XtActionProc) QuitRubik},
	{"MoveCcw", (XtActionProc) MoveRubikCcw},
	{"MoveTop", (XtActionProc) MoveRubik2DTop},
	{"MoveLeft", (XtActionProc) MoveRubik2DLeft},
	{"MoveCw", (XtActionProc) MoveRubikCw},
	{"MoveRight", (XtActionProc) MoveRubik2DRight},
	{"MoveBottom", (XtActionProc) MoveRubik2DBottom},
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

static XtResource resourcesRubik2D[] =
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
	 XtOffset(RubikWidget, core.width), XtRString, "300"},
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

Rubik2DClassRec rubik2dClassRec =
{
	{
		(WidgetClass) & rubikClassRec,	/* superclass */
		"Rubik2D",	/* class name */
		sizeof (Rubik2DRec),	/* widget size */
		NULL,		/* class initialize */
		NULL,		/* class part initialize */
		FALSE,		/* class inited */
		(XtInitProc) InitializeRubik2D,		/* initialize */
		NULL,		/* initialize hook */
		XtInheritRealize,	/* realize */
		actionsListRubik2D,	/* actions */
		XtNumber(actionsListRubik2D),	/* num actions */
		resourcesRubik2D,	/* resources */
		XtNumber(resourcesRubik2D),	/* num resources */
		NULLQUARK,	/* xrm class */
		TRUE,		/* compress motion */
		TRUE,		/* compress exposure */
		TRUE,		/* compress enterleave */
		TRUE,		/* visible interest */
		NULL,		/* destroy */
		(XtWidgetProc) ResizeRubik2D,	/* resize */
		(XtExposeProc) ExposeRubik2D,	/* expose */
		(XtSetValuesFunc) SetValuesRubik2D,	/* set values */
		NULL,		/* set values hook */
		XtInheritSetValuesAlmost,	/* set values almost */
		NULL,		/* get values hook */
		XtInheritAcceptFocus,	/* accept focus */
		XtVersion,	/* version */
		NULL,		/* callback private */
		defaultTranslationsRubik2D,	/* tm table */
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

WidgetClass rubik2dWidgetClass = (WidgetClass) & rubik2dClassRec;

static RowNext rotateToRow[MAXFACES] =	/*CW to min face */
{
	{1, LEFT, TOP},
	{0, BOTTOM, RIGHT},
	{0, RIGHT, BOTTOM},
	{0, TOP, LEFT},
	{1, RIGHT, BOTTOM},
	{0, LEFT, TOP}
};
static int  planeToCube[MAXRECT] =
{6, 0, 6, 1, 2, 3, 6, 4, 6, 6, 5, 6};
static int  cubeToPlane[MAXFACES] =
{1, 3, 4, 5, 7, 10};

static void
InitializeRubik2D(Widget request, Widget new)
{
	Rubik2DWidget w = (Rubik2DWidget) new;

	w->rubik.dim = 2;
	ResizeRubik2D(w);
}

static void
ResizeRubik2D(Rubik2DWidget w)
{
	int         tempLength;

	w->rubik.delta = 3;
	w->rubik.vertical = (w->core.height >= w->core.width);
	if (w->rubik.vertical)
		tempLength = MIN(w->core.height / MAXY, w->core.width / MAXX);
	else
		tempLength = MIN(w->core.height / MAXX, w->core.width / MAXY);
	w->rubik2d.cubeLength = MAX((tempLength - w->rubik.delta - 1) /
				    w->rubik.size, 0);
	w->rubik2d.faceLength = w->rubik.size * w->rubik2d.cubeLength;
	w->rubik2d.viewLength = w->rubik2d.faceLength + w->rubik.delta;
	if (w->rubik.vertical) {
		w->rubik.puzzleSize.x = MAXX * (w->rubik2d.viewLength - 1) +
			w->rubik.delta;
		w->rubik.puzzleSize.y = MAXY * (w->rubik2d.viewLength - 1) +
			w->rubik.delta;
	} else {
		w->rubik.puzzleSize.x = MAXY * (w->rubik2d.viewLength - 1) +
			w->rubik.delta;
		w->rubik.puzzleSize.y = MAXX * (w->rubik2d.viewLength - 1) +
			w->rubik.delta;
	}
	w->rubik.puzzleOffset.x = ((int) w->core.width - w->rubik.puzzleSize.x)
		/ 2;
	w->rubik.puzzleOffset.y = ((int) w->core.height - w->rubik.puzzleSize.y)
		/ 2;
	ResizePolyhedrons(w);
}

static void
ExposeRubik2D(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	Rubik2DWidget w = (Rubik2DWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->rubik.puzzleGC);
		DrawAllPolyhedrons((RubikWidget) w);
	}
}

static      Boolean
SetValuesRubik2D(Widget current, Widget request, Widget new)
{
	Rubik2DWidget c = (Rubik2DWidget) current, w = (Rubik2DWidget) new;
	Boolean     redraw = FALSE;

	if (w->rubik.size != c->rubik.size) {
		ResetPolyhedrons((RubikWidget) w);
		ResizeRubik2D(w);
		redraw = TRUE;
	}
	if (w->rubik2d.cubeLength != c->rubik2d.cubeLength) {
		ResizeRubik2D(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
MoveRubik2DTop(Rubik2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, TOP,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik2DLeft(Rubik2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, LEFT,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik2DRight(Rubik2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, RIGHT,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveRubik2DBottom(Rubik2DWidget w, XEvent * event, char **args, int nArgs)
{
	MoveRubikInput((RubikWidget) w, event->xbutton.x, event->xbutton.y, BOTTOM,
		       (int) (event->xkey.state & ControlMask));
}

static void
ResizePolyhedrons(Rubik2DWidget w)
{
	w->rubik2d.cubeLength = MAX(w->rubik2d.faceLength /
				    w->rubik.size - w->rubik.delta + 1, 0);
	w->rubik.orientLineLength = w->rubik2d.cubeLength / 4;
	w->rubik.letterOffset.x = -2;
	w->rubik.letterOffset.y = 4;
}

int
SelectPolyhedrons2D(Rubik2DWidget w, int x, int y, int *face, int *position)
{
	int         faceX, faceY, i, j;

	x -= w->rubik.puzzleOffset.x;
	y -= w->rubik.puzzleOffset.y;
	faceX = x / w->rubik2d.viewLength;
	faceY = y / w->rubik2d.viewLength;
	i = MAX((x - faceX * w->rubik2d.viewLength - w->rubik.delta) /
		(w->rubik2d.cubeLength + w->rubik.delta - 1), 0);
	j = MAX((y - faceY * w->rubik2d.viewLength - w->rubik.delta) /
		(w->rubik2d.cubeLength + w->rubik.delta - 1), 0);
	if ((faceX != 1 && faceY != 1) ||
	    (faceX >= 3 && w->rubik.vertical) ||
	    (faceY >= 3 && !w->rubik.vertical))
		return FALSE;
	if (i >= w->rubik.size)
		i = w->rubik.size - 1;
	if (j >= w->rubik.size)
		j = w->rubik.size - 1;
	*face = planeToCube[faceX + faceY * MAXX];
	if (faceX == 3) {
		*face = MAXFACES - 1;
		i = w->rubik.size - 1 - i;
		j = w->rubik.size - 1 - j;
	}
	*position = j * w->rubik.size + i;
	return TRUE;
}

int
NarrowSelection2D(Rubik2DWidget w, int *face, int *position, int *direction)
{
	int         i, j;

	if (*face == MAXFACES - 1 && *direction < MAXORIENT && !w->rubik.vertical)
		*direction = (*direction + HALF) % MAXORIENT;
	/* Remap to row movement */
	if (*direction == CW || *direction == CCW) {
		*direction = (*direction == CCW) ?
			(rotateToRow[*face].direction + 2) % MAXORIENT :
			rotateToRow[*face].direction;
		i = j = (rotateToRow[*face].sideFace == LEFT ||
			 rotateToRow[*face].sideFace == BOTTOM) ? w->rubik.size - 1 : 0;
		*face = rotateToRow[*face].face;
		*position = j * w->rubik.size + i;
	}
	return TRUE;
}

static void
DrawFrame(Rubik2DWidget w, GC gc)
{
	int         i;
	XPoint      pos[MAXXY + 1], letters;

	for (i = 0; i <= MAXXY; i++) {
		pos[i].x = i * w->rubik2d.viewLength + w->rubik.puzzleOffset.x;
		pos[i].y = i * w->rubik2d.viewLength + w->rubik.puzzleOffset.y;
	}
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[1].x, pos[0].y, pos[2].x, pos[0].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[3].x, pos[1].y, pos[3].x, pos[2].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[1].x, pos[3].y, pos[2].x, pos[3].y);
	XDrawLine(XtDisplay(w), XtWindow(w), gc,
		  pos[0].x, pos[1].y, pos[0].x, pos[2].y);
	letters.x = pos[0].x + w->rubik2d.viewLength / 2 - w->rubik.delta;
	letters.y = pos[0].y + w->rubik2d.viewLength / 2;
	XDrawString(XtDisplay(w), XtWindow(w), gc,
		    (int) (letters.x + 5 * w->rubik.letterOffset.x),
		    (int) (letters.y + w->rubik.letterOffset.y), "Front", 5);
	letters.x = pos[2].x + w->rubik2d.viewLength / 2 - w->rubik.delta;
	letters.y = pos[2].y + w->rubik2d.viewLength / 2;
	XDrawString(XtDisplay(w), XtWindow(w), gc,
		    (int) (letters.x + 4 * w->rubik.letterOffset.x),
		    (int) (letters.y + w->rubik.letterOffset.y), "Back", 4);
	if (w->rubik.vertical) {
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
DrawSquare2D(Rubik2DWidget w, int face, int position, int offset)
{
	int         dx, dy, orient, i, j;

	i = position % w->rubik.size;
	j = position / w->rubik.size;
	orient = w->rubik.cubeLoc[face][position].rotation;
	if (w->rubik.vertical || face != MAXFACES - 1) {
		dx = (cubeToPlane[face] % MAXX) * w->rubik2d.viewLength +
			i * (w->rubik2d.cubeLength + w->rubik.delta - 1);
		dy = (cubeToPlane[face] / MAXX) * w->rubik2d.viewLength +
			j * (w->rubik2d.cubeLength + w->rubik.delta - 1);
	} else {
		dx = (cubeToPlane[face] / MAXX) * w->rubik2d.viewLength +
			(w->rubik.size - 1 - i) * (w->rubik2d.cubeLength + w->rubik.delta -
						   1);
		dy = (cubeToPlane[face] % MAXX) * w->rubik2d.viewLength +
			(w->rubik.size - 1 - j) * (w->rubik2d.cubeLength + w->rubik.delta -
						   1);
		orient = (orient + HALF) % STRT;
	}
	dx += w->rubik.puzzleOffset.x + w->rubik.delta;
	dy += w->rubik.puzzleOffset.y + w->rubik.delta;
	if (offset) {
		XFillRectangle(XtDisplay(w), XtWindow(w),
			       w->rubik.borderGC, dx, dy, w->rubik2d.cubeLength, w->rubik2d.cubeLength);
		XDrawRectangle(XtDisplay(w), XtWindow(w),
		      w->rubik.faceGC[w->rubik.cubeLoc[face][position].face],
		       dx, dy, w->rubik2d.cubeLength, w->rubik2d.cubeLength);
	} else {
		XFillRectangle(XtDisplay(w), XtWindow(w),
		      w->rubik.faceGC[w->rubik.cubeLoc[face][position].face],
		       dx, dy, w->rubik2d.cubeLength, w->rubik2d.cubeLength);
		XDrawRectangle(XtDisplay(w), XtWindow(w),
			       w->rubik.borderGC, dx, dy, w->rubik2d.cubeLength, w->rubik2d.cubeLength);
	}
	if (w->rubik.depth == 1 || w->rubik.mono) {
		int         letterX, letterY;
		char        buf[2];

		(void) sprintf(buf, "%c",
		w->rubik.faceName[w->rubik.cubeLoc[face][position].face][0]);
		letterX = dx + w->rubik2d.cubeLength / 2 + w->rubik.letterOffset.x;
		letterY = dy + w->rubik2d.cubeLength / 2 + w->rubik.letterOffset.y;
		XDrawString(XtDisplay(w), XtWindow(w), w->rubik.inverseGC,
			    letterX, letterY, buf, 1);
	}
	if (w->rubik.orient)
		DrawOrientLine(w, orient, dx, dy);
}

static void
DrawOrientLine(Rubik2DWidget w, int orient, int dx, int dy)
{
	switch (orient) {
		case TOP:
			XDrawLine(XtDisplay(w), XtWindow(w), w->rubik.inverseGC,
				  dx + w->rubik2d.cubeLength / 2,
				  dy,
				  dx + w->rubik2d.cubeLength / 2,
				  dy + w->rubik.orientLineLength);
			return;
		case RIGHT:
			XDrawLine(XtDisplay(w), XtWindow(w), w->rubik.inverseGC,
				  dx + w->rubik2d.cubeLength,
				  dy + w->rubik2d.cubeLength / 2,
				  dx + w->rubik2d.cubeLength - w->rubik.orientLineLength -
				  1,
				  dy + w->rubik2d.cubeLength / 2);
			return;
		case BOTTOM:
			XDrawLine(XtDisplay(w), XtWindow(w), w->rubik.inverseGC,
				  dx + w->rubik2d.cubeLength / 2,
				  dy + w->rubik2d.cubeLength,
				  dx + w->rubik2d.cubeLength / 2,
				  dy + w->rubik2d.cubeLength - w->rubik.orientLineLength -
				  1);
			return;
		case LEFT:
			XDrawLine(XtDisplay(w), XtWindow(w), w->rubik.inverseGC,
				  dx,
				  dy + w->rubik2d.cubeLength / 2,
				  dx + w->rubik.orientLineLength,
				  dy + w->rubik2d.cubeLength / 2);
			return;
		default:
			(void) printf("DrawOrientLine: orient %d\n", orient);
	}
}
