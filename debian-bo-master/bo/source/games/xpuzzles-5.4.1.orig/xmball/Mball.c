/*-
# X-BASED MASTERBALL(tm)
#
#  Mball.c
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

/* Methods file for Mball */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef VMS
#define M_PI 3.14159265358979323846
#endif
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
#include "MballP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/mball.data"
#endif

static void InitializeMball(Widget request, Widget new);
static void ExposeMball(Widget new, XEvent * event, Region region);
static void ResizeMball(MballWidget w);
static void DestroyMball(Widget old);
static Boolean SetValuesMball(Widget current, Widget request, Widget new);
static void QuitMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void PracticeMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void PracticeMballMaybe(MballWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void RandomizeMballMaybe(MballWidget w, XEvent * event, char **args, int nArgs);
static void GetMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void WriteMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void UndoMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void SolveMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void IncrementMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void DecrementMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void OrientizeMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void Wedge2ModeMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void Wedge4ModeMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void Wedge6ModeMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void Wedge8ModeMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballTl(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballTop(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballTr(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballLeft(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballCw(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballRight(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballBl(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballBottom(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballBr(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballCcw(MballWidget w, XEvent * event, char **args, int nArgs);
static void MoveMballInput(MballWidget w, int x, int y, int direction, int control);
static void SelectMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void ReleaseMball(MballWidget w, XEvent * event, char **args, int nArgs);
static void GetColor(MballWidget w, int wedge, int init);
static void MoveControlCb(MballWidget w, int wedge, int direction);
static void CheckWedges(MballWidget w);
static void ResetWedges(MballWidget w);
static void ResizeWedges(MballWidget w);
static int  SelectWedges(MballWidget w, int x, int y, int *wedge, int *ring, int *view);
static int  PositionWedges(MballWidget w, int x, int y, int *wedge, int *ring, int *direction);
static void MoveNoWedges(MballWidget w);
static void PracticeWedges(MballWidget w);
static void RandomizeWedges(MballWidget w);
static void MoveWedges(MballWidget w, int wedge, int ring, int direction);

/* rcd : row, column, or diagonal */
static void SwapWedges(MballWidget w, int wedge1, int wedge2);
static void DrawFrame(MballWidget w, GC gc);
static void DrawWedge(MballWidget w, int wedge);
static void DrawSector(MballWidget w, int wedge, int ring, int offset);
static void DrawRadar(MballWidget w, GC gc, int startx, int starty, int lengthx, int lengthy);
static void DrawSect(MballWidget w, GC wedgeGC, GC borderGC, int r, int wedge, int startx, int starty, int lengthx, int lengthy);
static void LetterPosition(MballWidget w, int wedge, int ring, int lengthx, int lengthy, int *dx, int *dy);
static void OffsetSect(MballWidget w, int wedge, int *dx, int *dy);
static void XFillSector(Display * display, Drawable drawable, GC gc, int xo, int yo, int width1, int height1, int width2, int height2, int angle1, int angle2);
static void XDrawSector(Display * display, Drawable drawable, GC gc, int xo, int yo, int width1, int height1, int width2, int height2, int angle1, int angle2);

#ifdef DEBUG
static void 
PrintMball(MballWidget w)
#endif

	static char defaultTranslationsMball[] =
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
   <KeyPress>p: Practice()\n\
   <Btn1Down>: Select()\n\
   <Btn1Up>: Release()\n\
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
   <KeyPress>2: Wedge2()\n\
   <KeyPress>4: Wedge4()\n\
   <KeyPress>6: Wedge6()\n\
   <KeyPress>8: Wedge8()";

	static XtActionsRec actionsListMball[] =
	{
		{"Quit", (XtActionProc) QuitMball},
		{"MoveCcw", (XtActionProc) MoveMballCcw},
		{"MoveTl", (XtActionProc) MoveMballTl},
		{"MoveTop", (XtActionProc) MoveMballTop},
		{"MoveTr", (XtActionProc) MoveMballTr},
		{"MoveLeft", (XtActionProc) MoveMballLeft},
		{"MoveCw", (XtActionProc) MoveMballCw},
		{"MoveRight", (XtActionProc) MoveMballRight},
		{"MoveBl", (XtActionProc) MoveMballBl},
		{"MoveBottom", (XtActionProc) MoveMballBottom},
		{"MoveBr", (XtActionProc) MoveMballBr},
		{"Select", (XtActionProc) SelectMball},
		{"Release", (XtActionProc) ReleaseMball},
		{"Practice", (XtActionProc) PracticeMball},
		{"PracticeMaybe", (XtActionProc) PracticeMballMaybe},
		{"Randomize", (XtActionProc) RandomizeMball},
		{"RandomizeMaybe", (XtActionProc) RandomizeMballMaybe},
		{"Get", (XtActionProc) GetMball},
		{"Write", (XtActionProc) WriteMball},
		{"Undo", (XtActionProc) UndoMball},
		{"Solve", (XtActionProc) SolveMball},
		{"Increment", (XtActionProc) IncrementMball},
		{"Decrement", (XtActionProc) DecrementMball},
		{"Orientize", (XtActionProc) OrientizeMball},
		{"Wedge2", (XtActionProc) Wedge2ModeMball},
		{"Wedge4", (XtActionProc) Wedge4ModeMball},
		{"Wedge6", (XtActionProc) Wedge6ModeMball},
		{"Wedge8", (XtActionProc) Wedge8ModeMball}
};

	static XtResource resourcesMball[] =
	{
		{XtNuserName, XtCUserName, XtRString, sizeof (String),
		 XtOffset(MballWidget, mball.username), XtRString, "nobody"},
		{XtNwedgeColor0, XtCLabel, XtRString, sizeof (String),
	     XtOffset(MballWidget, mball.wedgeName[0]), XtRString, "Yellow"},
		{XtNwedgeColor1, XtCLabel, XtRString, sizeof (String),
	       XtOffset(MballWidget, mball.wedgeName[1]), XtRString, "Blue"},
		{XtNwedgeColor2, XtCLabel, XtRString, sizeof (String),
		 XtOffset(MballWidget, mball.wedgeName[2]), XtRString, "Red"},
		{XtNwedgeColor3, XtCLabel, XtRString, sizeof (String),
	    XtOffset(MballWidget, mball.wedgeName[3]), XtRString, "Magenta"},
		{XtNwedgeColor4, XtCLabel, XtRString, sizeof (String),
	      XtOffset(MballWidget, mball.wedgeName[4]), XtRString, "Green"},
		{XtNwedgeColor5, XtCLabel, XtRString, sizeof (String),
	     XtOffset(MballWidget, mball.wedgeName[5]), XtRString, "Orange"},
		{XtNwedgeColor6, XtCLabel, XtRString, sizeof (String),
	       XtOffset(MballWidget, mball.wedgeName[6]), XtRString, "Cyan"},
		{XtNwedgeColor7, XtCLabel, XtRString, sizeof (String),
	  XtOffset(MballWidget, mball.wedgeName[7]), XtRString, "DarkGreen"},
		{XtNforeground, XtCForeground, XtRPixel, sizeof (Pixel),
    XtOffset(MballWidget, mball.foreground), XtRString, XtDefaultForeground},
		{XtNpieceBorder, XtCColor, XtRPixel, sizeof (Pixel),
   XtOffset(MballWidget, mball.borderColor), XtRString, XtDefaultForeground},
		{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
		 XtOffset(MballWidget, core.width), XtRString, "200"},
		{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
		 XtOffset(MballWidget, core.height), XtRString, "400"},
		{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
		 XtOffset(MballWidget, mball.mono), XtRString, "FALSE"},
		{XtNwedges, XtCWedges, XtRInt, sizeof (int),
		 XtOffset(MballWidget, mball.wedges), XtRString, "8"},	/*DEFAULTWEDGES */
		{XtNrings, XtCRings, XtRInt, sizeof (int),
		 XtOffset(MballWidget, mball.rings), XtRString, "4"},	/*DEFAULTRINGS */
		{XtNorient, XtCOrient, XtRBoolean, sizeof (Boolean),
		 XtOffset(MballWidget, mball.orient), XtRString, "FALSE"},	/*DEFAULTORIENT */
		{XtNpractice, XtCPractice, XtRBoolean, sizeof (Boolean),
		 XtOffset(MballWidget, mball.practice), XtRString, "FALSE"},	/*DEFAULTPRACTICE */
		{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
		 XtOffset(MballWidget, mball.started), XtRString, "FALSE"},
	      {XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	       XtOffset(MballWidget, mball.select), XtRCallback, NULL}
};

	MballClassRec mballClassRec =
	{
		{
			(WidgetClass) & widgetClassRec,		/* superclass */
			"Mball",	/* class name */
			sizeof (MballRec),	/* widget size */
			NULL,	/* class initialize */
			NULL,	/* class part initialize */
			FALSE,	/* class inited */
			(XtInitProc) InitializeMball,	/* initialize */
			NULL,	/* initialize hook */
			XtInheritRealize,	/* realize */
			actionsListMball,	/* actions */
			XtNumber(actionsListMball),	/* num actions */
			resourcesMball,		/* resources */
			XtNumber(resourcesMball),	/* num resources */
			NULLQUARK,	/* xrm class */
			TRUE,	/* compress motion */
			TRUE,	/* compress exposure */
			TRUE,	/* compress enterleave */
			TRUE,	/* visible interest */
			(XtWidgetProc) DestroyMball,	/* destroy */
			(XtWidgetProc) ResizeMball,	/* resize */
			(XtExposeProc) ExposeMball,	/* expose */
			(XtSetValuesFunc) SetValuesMball,	/* set values */
			NULL,	/* set values hook */
			XtInheritSetValuesAlmost,	/* set values almost */
			NULL,	/* get values hook */
			NULL,	/* accept focus */
			XtVersion,	/* version */
			NULL,	/* callback private */
			defaultTranslationsMball,	/* tm table */
			NULL,	/* query geometry */
			NULL,	/* display accelerator */
			NULL	/* extension */
		},
		{
			0	/* ignore */
		}
};

	static int  mapDirToWedge[(MAXWEDGES - MINWEDGES) / 2 + 1][COORD] =
{
	{
		0, 4, 4, 4, 0, 4, 4, 4
	},
	{
		0, 4, 1, 4, 0, 4, 1, 4
	},
	{
		0, 1, 4, 2, 0, 1, 4, 2
	},
	{
		0, 1, 2, 3, 0, 1, 2, 3
	}
};
static int  mapWedgeToDir[(MAXWEDGES - MINWEDGES) / 2 + 1][COORD] =
{
	{
		0, 4, 8, 8, 8, 8, 8, 8
	},
	{
		0, 2, 4, 6, 8, 8, 8, 8
	},
	{
		0, 1, 3, 4, 5, 7, 8, 8
	},
	{
		0, 1, 2, 3, 4, 5, 6, 7
	}
};

WidgetClass mballWidgetClass = (WidgetClass) & mballClassRec;

static void
InitializeMball(Widget request, Widget new)
{
	MballWidget w = (MballWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         wedge;

	for (wedge = 0; wedge < MAXWEDGES; wedge++)
		w->mball.mballLoc[wedge] = NULL;
	CheckWedges(w);
	InitMoves();
	ResetWedges(w);
	(void) SRAND(getpid());
	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	values.foreground = w->mball.foreground;
	w->mball.puzzleGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->mball.borderColor;
	w->mball.borderGC = XtGetGC(new, valueMask, &values);
	w->mball.depth = DefaultDepthOfScreen(XtScreen(w));
	valueMask = GCForeground | GCBackground;
	values.foreground = w->core.background_pixel;
	values.background = w->mball.foreground;
	w->mball.inverseGC = XtGetGC(new, valueMask, &values);
	for (wedge = 0; wedge < MAXWEDGES; wedge++)
		GetColor(w, wedge, TRUE);
	ResizeMball(w);
}

static void
DestroyMball(Widget old)
{
	MballWidget w = (MballWidget) old;
	int         wedge;

	for (wedge = 0; wedge < MAXWEDGES; wedge++) {
		XtReleaseGC(old, w->mball.wedgeGC[wedge]);
	}
	XtReleaseGC(old, w->mball.borderGC);
	XtReleaseGC(old, w->mball.puzzleGC);
	XtReleaseGC(old, w->mball.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->mball.select);
}

static void
ResizeMball(MballWidget w)
{
	int         tempLength;

	w->mball.delta = 4;
	w->mball.vertical = (w->core.height >= w->core.width);
	if (w->mball.vertical)
		tempLength = MIN(w->core.height / 2, w->core.width);
	else
		tempLength = MIN(w->core.height, w->core.width / 2);
	w->mball.mballLength = MAX((tempLength - w->mball.delta + 1) /
				   w->mball.wedges, 0);
	w->mball.wedgeLength = w->mball.wedges * w->mball.mballLength;
	w->mball.viewLength = w->mball.wedgeLength + w->mball.delta;
	w->mball.viewMiddle = w->mball.viewLength / 2;
	if (w->mball.vertical) {
		w->mball.puzzleSize.x = w->mball.viewLength - 1;
		w->mball.puzzleSize.y = 2 * w->mball.viewLength - w->mball.delta - 2;
	} else {
		w->mball.puzzleSize.x = 2 * w->mball.viewLength - w->mball.delta - 2;
		w->mball.puzzleSize.y = w->mball.viewLength - 1;
	}
	w->mball.puzzleOffset.x = ((int) w->core.width - w->mball.puzzleSize.x) / 2;
	w->mball.puzzleOffset.y = ((int) w->core.height - w->mball.puzzleSize.y) / 2;
	ResizeWedges(w);
}

static void
ExposeMball(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	MballWidget w = (MballWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->mball.puzzleGC);
		DrawAllWedges(w);
	}
}

static      Boolean
SetValuesMball(Widget current, Widget request, Widget new)
{
	MballWidget c = (MballWidget) current, w = (MballWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	int         wedge;

	CheckWedges(w);
	if (w->mball.foreground != c->mball.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->mball.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->mball.puzzleGC);
		w->mball.puzzleGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->core.background_pixel;
		values.background = w->mball.foreground;
		XtReleaseGC(new, w->mball.inverseGC);
		w->mball.inverseGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->mball.borderColor != c->mball.borderColor) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->mball.borderColor;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->mball.borderGC);
		w->mball.borderGC = XtGetGC(new, valueMask, &values);
		redraw = TRUE;
	}
	if (w->mball.mono || w->mball.depth == 1) {
		valueMask = GCForeground | GCBackground;
		values.background = w->core.background_pixel;
		values.foreground = w->mball.foreground;
		for (wedge = 0; wedge < MAXWEDGES; wedge++) {
			XtReleaseGC(new, w->mball.wedgeGC[wedge]);
			w->mball.wedgeGC[wedge] = XtGetGC(new, valueMask, &values);
		}
		redraw = TRUE;
	}
	for (wedge = 0; wedge < MAXWEDGES; wedge++) {
		if (strcmp(w->mball.wedgeName[wedge], c->mball.wedgeName[wedge]))
			GetColor(w, wedge, FALSE);
	}
	if (w->mball.orient != c->mball.orient) {
		ResetWedges(w);
		redraw = TRUE;
	} else if (w->mball.practice != c->mball.practice) {
		ResetWedges(w);
		redraw = TRUE;
	}
	if (w->mball.wedges != c->mball.wedges ||
	    w->mball.rings != c->mball.rings) {
		ResetWedges(w);
		ResizeMball(w);
		redraw = TRUE;
	}
	if (w->mball.mballLength != c->mball.mballLength) {
		ResizeMball(w);
		redraw = TRUE;
	}
	return (redraw);
}

static void
QuitMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	int         view, control;

	if (SelectWedges(w, event->xbutton.x, event->xbutton.y,
		 &(w->mball.currentWedge), &(w->mball.currentRing), &view)) {
		control = (int) (event->xkey.state & ControlMask);
		if (control || w->mball.practice || !CheckSolved(w))
			DrawSector(w, w->mball.currentWedge, w->mball.currentRing, TRUE);
	} else
		w->mball.currentWedge = -1;
}

static void
ReleaseMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	int         control, wedge, ring, view, i, diff, opp;
	mballCallbackStruct cb;

	if (w->mball.currentWedge == -1)
		return;
	DrawSector(w, w->mball.currentWedge, w->mball.currentRing, FALSE);
	control = (int) (event->xkey.state & ControlMask);
	if (!control && !w->mball.practice && CheckSolved(w))
		MoveNoWedges(w);
	else if (SelectWedges(w, event->xbutton.x, event->xbutton.y,
			      &wedge, &ring, &view)) {
		opp = (w->mball.currentWedge + w->mball.wedges / 2) % w->mball.wedges;
		if (ring == w->mball.currentRing) {
			if (wedge == w->mball.currentWedge)
				return;
			if (opp == (wedge + 1) % w->mball.wedges ||
			    wedge == (opp + 1) % w->mball.wedges) {
				cb.reason = MBALL_AMBIGUOUS;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			} else {
				diff = (w->mball.currentWedge - wedge + w->mball.wedges) %
					w->mball.wedges;
				if (diff > w->mball.wedges / 2)
					for (i = 0; i < w->mball.wedges - diff; i++)
						MoveMball(w, wedge, ring, CW, control);
				else
					for (i = 0; i < diff; i++)
						MoveMball(w, wedge, ring, CCW, control);
				if (!control && CheckSolved(w)) {
					cb.reason = MBALL_SOLVED;
					XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
				}
			}
		} else if (wedge == w->mball.currentWedge && w->mball.wedges > 2) {
			cb.reason = MBALL_AMBIGUOUS;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else if (opp == (wedge + 1) % w->mball.wedges)
			MoveMball(w, wedge, ring,
			     mapWedgeToDir[(w->mball.wedges - MINWEDGES) / 2]
				  [w->mball.currentWedge], control);
		else if (wedge == (opp + 1) % w->mball.wedges)
			MoveMball(w, wedge, ring,
				  mapWedgeToDir[(w->mball.wedges - MINWEDGES) / 2][wedge], control);
		else
			MoveNoWedges(w);
	}
}

static void
PracticeMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	PracticeWedges(w);
}

static void
PracticeMballMaybe(MballWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->mball.started)
		PracticeWedges(w);
}

static void
RandomizeMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	RandomizeWedges(w);
}

static void
RandomizeMballMaybe(MballWidget w, XEvent * event, char **args, int nArgs)
{
	if (!w->mball.started)
		RandomizeWedges(w);
}

static void
GetMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, wedge, ring, orient, practice, moves;
	mballCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &wedge);
		if (wedge >= MINWEDGES && wedge <= MAXWEDGES && !(wedge % 2)) {
			if (w->mball.wedges != wedge) {
				cb.reason = (wedge - MINWEDGES) / 2 + MBALL_WEDGE2;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else {
			(void) printf("%s corrupted: ", DATAFILE);
			(void) printf("wedge %d should be even and between %d and %d\n",
				      wedge, MINWEDGES, MAXWEDGES);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &ring);
		if (ring >= MINRINGS) {
			for (i = w->mball.rings; i < ring; i++) {
				cb.reason = MBALL_INC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->mball.rings; i > ring; i--) {
				cb.reason = MBALL_DEC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: ring %d should be between %d and MAXINT\n",
				      DATAFILE, ring, MINRINGS);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &orient);
		if (w->mball.orient != (Boolean) orient) {
			cb.reason = MBALL_ORIENT;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &practice);
		if (w->mball.practice != (Boolean) practice) {
			cb.reason = MBALL_PRACTICE;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = MBALL_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		SetStartPosition(w);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		(void) printf("%s: wedge %d, ring %d, orient %d, ",
			      DATAFILE, wedge, ring, orient);
		(void) printf("practice %d, moves %d\n", practice, moves);
	}
}

static void
WriteMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "wedge%c %d\n", SYMBOL, w->mball.wedges);
		(void) fprintf(fp, "ring%c %d\n", SYMBOL, w->mball.rings);
		(void) fprintf(fp, "orient%c %d\n", SYMBOL, (w->mball.orient) ? 1 : 0);
		(void) fprintf(fp, "practice%c %d\n", SYMBOL, (w->mball.practice) ? 1 : 0);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		int         wedge, ring, direction, control;

		GetMove(&wedge, &ring, &direction, &control);
		direction = (direction < COORD) ? direction : 3 * COORD - direction;
		if (control)
			MoveControlCb(w, wedge, direction);
		else {
			mballCallbackStruct cb;

			MoveWedges(w, wedge, ring, direction);
			cb.reason = MBALL_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolveMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	/* SolveWedges(w); *//* Sorry, unimplemented */
}

static void
IncrementMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_INC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
DecrementMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	mballCallbackStruct cb;

	if (w->mball.rings <= MINRINGS)
		return;
	cb.reason = MBALL_DEC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
OrientizeMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_ORIENT;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Wedge2ModeMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_WEDGE2;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Wedge4ModeMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_WEDGE4;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Wedge6ModeMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_WEDGE6;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
Wedge8ModeMball(MballWidget w, XEvent * event, char **args, int nArgs)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_WEDGE8;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
MoveMballCcw(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, CCW,
		       (int) (event->xbutton.state & ControlMask));
}

static void
MoveMballTl(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, TL,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballTop(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, TOP,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballTr(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, TR,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballLeft(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, LEFT,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballCw(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, CW,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballRight(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, RIGHT,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballBl(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, BL,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballBottom(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, BOTTOM,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballBr(MballWidget w, XEvent * event, char **args, int nArgs)
{
	MoveMballInput(w, event->xbutton.x, event->xbutton.y, BR,
		       (int) (event->xkey.state & ControlMask));
}

static void
MoveMballInput(MballWidget w, int x, int y, int direction, int control)
{
	int         wedge, ring;

	if (CheckSolved(w) && !w->mball.practice && !control) {
		MoveNoWedges(w);
		return;
	}
	if (!PositionWedges(w, x, y, &wedge, &ring, &direction))
		return;
	control = (control) ? 1 : 0;
	MoveMball(w, wedge, ring, direction, control);
	if (!control && CheckSolved(w)) {
		mballCallbackStruct cb;

		cb.reason = MBALL_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

void
MoveMball(MballWidget w, int wedge, int ring, int direction, int control)
{
	mballCallbackStruct cb;

	if (control)
		MoveControlCb(w, wedge, direction);
	else {
		MoveWedges(w, wedge, ring, direction);
		cb.reason = MBALL_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
	PutMove(wedge, ring, direction, control);
}

static void
GetColor(MballWidget w, int wedge, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->mball.depth > 1 && !w->mball.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
			      w->mball.wedgeName[wedge], &colorCell, &rgb)) {
			values.foreground = w->mball.wedgeColor[wedge] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->mball.wedgeGC[wedge]);
			w->mball.wedgeGC[wedge] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined %d",
				       w->mball.wedgeName[wedge], wedge);
			XtWarning(buf);
		}
	}
	values.foreground = w->mball.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->mball.wedgeGC[wedge]);
	w->mball.wedgeGC[wedge] = XtGetGC((Widget) w, valueMask, &values);
}

static void
MoveControlCb(MballWidget w, int wedge, int direction)
{
	mballCallbackStruct cb;
	int         ring;

	if (direction > COORD)
		for (ring = 0; ring < w->mball.rings; ring++) {
			MoveWedges(w, wedge, ring, direction);
			cb.reason = MBALL_CONTROL;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	} else {
		MoveWedges(w, 0, 0, direction);
		cb.reason = MBALL_CONTROL;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		MoveWedges(w, w->mball.wedges / 2, 0, direction);
		cb.reason = MBALL_CONTROL;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
CheckWedges(MballWidget w)
{
	if (w->mball.wedges < MINWEDGES || w->mball.wedges > MAXWEDGES ||
	    w->mball.wedges % 2) {
		char        buf[121];

		(void) sprintf(buf,
			       "Number of wedges out of bounds, use even %d..%d", MINWEDGES, MAXWEDGES);
		XtWarning(buf);
		w->mball.wedges = DEFAULTWEDGES;
	}
	if (w->mball.rings < MINRINGS) {
		char        buf[121];

		(void) sprintf(buf,
		  "Number of rings out of bounds, use %d..MAXINT", MINRINGS);
		XtWarning(buf);
		w->mball.rings = DEFAULTRINGS;
	}
}

static void
ResetWedges(MballWidget w)
{
	int         wedge, ring;

	for (wedge = 0; wedge < MAXWEDGES; wedge++) {
		if (w->mball.mballLoc[wedge])
			(void) free((void *) w->mball.mballLoc[wedge]);
		if (!(w->mball.mballLoc[wedge] = (MballLoc *)
		      malloc(sizeof (MballLoc) * w->mball.rings)))
			XtError("Not enough memory, exiting.");
		if (startLoc[wedge])
			(void) free((void *) startLoc[wedge]);
		if (!(startLoc[wedge] = (MballLoc *)
		      malloc(sizeof (MballLoc) * w->mball.rings)))
			XtError("Not enough memory, exiting.");
	}
	for (wedge = 0; wedge < w->mball.wedges; wedge++)
		for (ring = 0; ring < w->mball.rings; ring++) {
			w->mball.mballLoc[wedge][ring].wedge = wedge;
			w->mball.mballLoc[wedge][ring].direction = DOWN;
		}
	FlushMoves(w);
	w->mball.started = FALSE;
}

static void
ResizeWedges(MballWidget w)
{
	w->mball.mballLength = w->mball.wedgeLength / (2 * w->mball.wedges) -
		w->mball.delta - 1;
	w->mball.letterOffset.x = -2;
	w->mball.letterOffset.y = 4;
	w->mball.dr = w->mball.wedges;
}

static int
SelectWedges(MballWidget w, int x, int y, int *wedge, int *ring, int *view)
{
	double      angle, radius;

	x -= w->mball.puzzleOffset.x;
	y -= w->mball.puzzleOffset.y;
	if (w->mball.vertical && y > w->mball.viewLength - 1) {
		y -= (w->mball.viewLength - 1);
		*view = DOWN;
	} else if (!w->mball.vertical && x > w->mball.viewLength - 1) {
		x -= (w->mball.viewLength - 1);
		*view = DOWN;
	} else
		*view = UP;
	x -= (w->mball.wedgeLength + 1) / 2;
	y -= (w->mball.wedgeLength + 1) / 2;
	radius = sqrt((double) x * x + y * y);
	if (y >= 0)
		angle = atan2((double) -x, (double) y) + M_PI;
	else if (x < 0)
		angle = 2 * M_PI - atan2((double) -x, (double) -y);
	else
		angle = -atan2((double) -x, (double) -y);
	*ring = (int) (radius * (double) w->mball.rings /
		       ((double) w->mball.wedgeLength));
	*wedge = (int) (angle * (double) w->mball.wedges / (2.0 * M_PI));
	if (*view == DOWN) {
		if (w->mball.vertical)
			*wedge = (3 * w->mball.wedges / 2 - 1 - *wedge) % w->mball.wedges;
		else
			*wedge = (w->mball.wedges - 1 - *wedge) % w->mball.wedges;
		*ring = w->mball.rings - 1 - *ring;
	}
	if (radius > w->mball.wedgeLength / 2 + w->mball.delta)
		return FALSE;
	return TRUE;
}

static int
PositionWedges(MballWidget w, int x, int y, int *wedge, int *ring, int *direction)
{
	int         view, inside;

	inside = SelectWedges(w, x, y, wedge, ring, &view);
	if ((*direction == CW || *direction == CCW) && !inside)
		return FALSE;
	if (view == DOWN) {
		if (*direction == CCW)
			*direction = CW;
		else if (*direction == CW)
			*direction = CCW;
		else if (*direction < COORD)
			*direction = (COORD - *direction) % COORD;
	}
	if (w->mball.wedges % 4 && (*direction == LEFT || *direction == RIGHT))
		return FALSE;
	if (w->mball.wedges <= 4 && *direction % 2 && *direction < COORD)
		return FALSE;
	return TRUE;
}

static void
MoveNoWedges(MballWidget w)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_ILLEGAL;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
PracticeWedges(MballWidget w)
{
	mballCallbackStruct cb;

	cb.reason = MBALL_PRACTICE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
RandomizeWedges(MballWidget w)
{
	mballCallbackStruct cb;
	int         randomDirection, wedge, ring;
	int         big = w->mball.wedges * (w->mball.rings + 1) + NRAND(2);

	if (big > 100)
		big = 100;
	if (w->mball.practice)
		PracticeWedges(w);
	cb.reason = MBALL_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);

#ifdef DEBUG
	big = 3;
#endif

	while (big--) {
		wedge = NRAND(w->mball.wedges);
		ring = NRAND(w->mball.rings);
		do
			randomDirection = NRAND(2 * COORD);
		while (randomDirection < COORD &&
		       mapDirToWedge[(w->mball.wedges - MINWEDGES) / 2][randomDirection] ==
		       CUTS);
		if (randomDirection >= COORD) {
			if (randomDirection - COORD < CUTS)
				randomDirection = CW;
			else
				randomDirection = CCW;
		}
		MoveMball(w, wedge, ring, randomDirection, FALSE);
	}
	FlushMoves(w);
	cb.reason = MBALL_RANDOMIZE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (CheckSolved(w)) {
		cb.reason = MBALL_SOLVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	}
}

static void
SwapWedges(MballWidget w, int wedge1, int wedge2)
{
	MballLoc    temp;
	int         ring;

	if (wedge1 == wedge2) {
		for (ring = 0; ring < w->mball.rings / 2; ring++) {
			temp = w->mball.mballLoc[wedge1][ring];
			w->mball.mballLoc[wedge1][ring] =
				w->mball.mballLoc[wedge1][w->mball.rings - 1 - ring];
			w->mball.mballLoc[wedge1][w->mball.rings - 1 - ring] = temp;
		}
		for (ring = 0; ring < w->mball.rings; ring++)
			w->mball.mballLoc[wedge1][ring].direction =
				!w->mball.mballLoc[wedge1][ring].direction;
		DrawWedge(w, wedge1);
	} else {
		for (ring = 0; ring < w->mball.rings; ring++) {
			temp = w->mball.mballLoc[wedge1][ring];
			w->mball.mballLoc[wedge1][ring] =
				w->mball.mballLoc[wedge2][w->mball.rings - 1 - ring];
			w->mball.mballLoc[wedge2][w->mball.rings - 1 - ring] = temp;
			w->mball.mballLoc[wedge1][ring].direction =
				!w->mball.mballLoc[wedge1][ring].direction;
			w->mball.mballLoc[wedge2][w->mball.rings - 1 - ring].direction =
				!w->mball.mballLoc[wedge2][w->mball.rings - 1 - ring].direction;
		}
		DrawWedge(w, wedge1);
		DrawWedge(w, wedge2);
	}
}

static void
MoveWedges(MballWidget w, int wedge, int ring, int direction)
{
	int         i;

	if (direction == CW || direction == CCW) {	/* rotate ring */
		int         newI;
		MballLoc    temp1, temp2;

		for (i = 0; i < w->mball.wedges; i++) {
			newI = (direction == CW) ? i : w->mball.wedges - 1 - i;
			if (newI == ((direction == CW) ? 0 : w->mball.wedges - 1)) {
				temp1 = w->mball.mballLoc[newI][ring];
				w->mball.mballLoc[newI][ring] = w->mball.mballLoc
					[((direction == CW) ? w->mball.wedges - 1 : 0)][ring];
			} else {
				temp2 = temp1;
				temp1 = w->mball.mballLoc[newI][ring];
				w->mball.mballLoc[newI][ring] = temp2;
			}
			DrawSector(w, newI, ring, FALSE);
		}
	} else {		/* flip */
		int         sphereDir = mapDirToWedge[(w->mball.wedges - MINWEDGES) / 2][direction];
		int         offset = w->mball.wedges / 2;
		int         wedge1, wedge2;

		for (i = 0; i < w->mball.wedges / 2; i++)
			if (wedge == i + sphereDir)
				offset = 0;
		for (i = 0; i < (w->mball.wedges + 2) / 4; i++) {
			wedge1 = (i + sphereDir + offset) % w->mball.wedges;
			wedge2 = (w->mball.wedges / 2 - 1 - i + sphereDir + offset) %
				w->mball.wedges;
			SwapWedges(w, wedge1, wedge2);
		}
	}
}

static void
DrawFrame(MballWidget w, GC gc)
{
	int         startx, starty, lengthx, lengthy;

	startx = 1 + w->mball.puzzleOffset.x;
	starty = 1 + w->mball.puzzleOffset.y;
	lengthx = w->mball.viewLength - w->mball.delta + w->mball.puzzleOffset.x;
	lengthy = w->mball.viewLength - w->mball.delta + w->mball.puzzleOffset.y;
	DrawRadar(w, gc, startx, starty, lengthx - startx, lengthy - starty);
	if (w->mball.vertical) {
		XDrawLine(XtDisplay(w), XtWindow(w), gc, 0, lengthy + 1,
			  (int) w->core.width - 1, lengthy + 1);
		DrawRadar(w, gc, startx, lengthy + 3, lengthx - startx, lengthy - starty);
	} else {
		XDrawLine(XtDisplay(w), XtWindow(w), gc, lengthx + 1, 0,
			  lengthx + 1, (int) w->core.height - 1);
		DrawRadar(w, gc, lengthx + 3, starty, lengthx - startx, lengthy - starty);
	}
}

void
DrawAllWedges(MballWidget w)
{
	int         wedge;

	for (wedge = 0; wedge < w->mball.wedges; wedge++)
		DrawWedge(w, wedge);
}

static void
DrawWedge(MballWidget w, int wedge)
{
	int         ring;

	for (ring = 0; ring < w->mball.rings; ring++)
		DrawSector(w, wedge, ring, FALSE);
}

static void
LetterPosition(MballWidget w, int wedge, int ring, int lengthx, int lengthy, int *dx, int *dy)
{
	double      angle, radius;

	angle = (double) (2 * wedge + 1) * M_PI / w->mball.wedges;
	if (w->mball.rings % 2 && ring == w->mball.rings / 2)
		radius = ((double) 4.0 * ring + 1.0) / ((double) 4.0 * w->mball.rings);
	else
		radius = ((double) 2.0 * ring + 1.0) / ((double) 2.0 * w->mball.rings);
	*dx = lengthx / 2 + (int) ((double) lengthx * radius * cos(angle - M_PI / 2));
	*dy = lengthy / 2 + (int) ((double) lengthy * radius * sin(angle - M_PI / 2));
}

static void
OffsetSect(MballWidget w, int wedge, int *dx, int *dy)
{
	double      angle = (double) (2 * wedge + 1) * M_PI / w->mball.wedges;

	*dx = (int) ((double) w->mball.dr * cos(angle - M_PI / 2));
	*dy = (int) ((double) w->mball.dr * sin(angle - M_PI / 2));
}

static void
DrawSector(MballWidget w, int wedge, int ring, int offset)
{
	int         startx, starty, lengthx, lengthy;

	startx = 1 + w->mball.puzzleOffset.x;
	starty = 1 + w->mball.puzzleOffset.y;
	lengthx = w->mball.viewLength - w->mball.delta + w->mball.puzzleOffset.x;
	lengthy = w->mball.viewLength - w->mball.delta + w->mball.puzzleOffset.y;
	if (ring < (w->mball.rings + 1) / 2)
		if (offset)
			DrawSect(w, w->mball.borderGC,
				 w->mball.wedgeGC[w->mball.mballLoc[wedge][ring].wedge], ring, wedge,
			 startx, starty, lengthx - startx, lengthy - starty);
		else
			DrawSect(w, w->mball.wedgeGC[w->mball.mballLoc[wedge][ring].wedge],
				 w->mball.borderGC, ring, wedge,
			 startx, starty, lengthx - startx, lengthy - starty);
	if (ring + 1 > w->mball.rings / 2) {
		if (w->mball.vertical)
			if (offset)
				DrawSect(w, w->mball.borderGC,
					 w->mball.wedgeGC[w->mball.mballLoc[wedge][ring].wedge],
					 w->mball.rings - 1 - ring,
					 (3 * w->mball.wedges / 2 - 1 - wedge) % w->mball.wedges,
					 startx, lengthy + 3, lengthx - startx, lengthy - starty);
			else
				DrawSect(w, w->mball.wedgeGC[w->mball.mballLoc[wedge][ring].wedge],
				w->mball.borderGC, w->mball.rings - 1 - ring,
					 (3 * w->mball.wedges / 2 - 1 - wedge) % w->mball.wedges,
					 startx, lengthy + 3, lengthx - startx, lengthy - starty);
		else if (offset)
			DrawSect(w, w->mball.borderGC,
				 w->mball.wedgeGC[w->mball.mballLoc[wedge][ring].wedge],
				 w->mball.rings - 1 - ring, w->mball.wedges - 1 - wedge,
				 lengthx + 3, starty, lengthx - startx, lengthy - starty);
		else
			DrawSect(w, w->mball.wedgeGC[w->mball.mballLoc[wedge][ring].wedge],
				 w->mball.borderGC,
				 w->mball.rings - 1 - ring, w->mball.wedges - 1 - wedge,
				 lengthx + 3, starty, lengthx - startx, lengthy - starty);
	}
	if (w->mball.depth == 1 || w->mball.mono) {
		int         letterX, letterY;
		char        buf[3];

		if (ring < (w->mball.rings + 1) / 2) {
			LetterPosition(w, wedge, ring, lengthx - startx, lengthy - starty,
				       &letterX, &letterY);
			letterX += startx + w->mball.letterOffset.x;
			letterY += starty + w->mball.letterOffset.y;
			if (w->mball.orient && !w->mball.mballLoc[wedge][ring].direction) {
				(void) sprintf(buf, "%d%c", w->mball.mballLoc[wedge][ring].wedge,
					       w->mball.wedgeName[w->mball.mballLoc[wedge][ring].wedge][0]);
				letterX += w->mball.letterOffset.x;
				XDrawString(XtDisplay(w), XtWindow(w), w->mball.inverseGC,
					    letterX, letterY, buf, 2);
			} else {
				(void) sprintf(buf, "%c",
					       w->mball.wedgeName[w->mball.mballLoc[wedge][ring].wedge][0]);
				XDrawString(XtDisplay(w), XtWindow(w), w->mball.inverseGC,
					    letterX, letterY, buf, 1);
			}
		}
		if (ring + 1 > w->mball.rings / 2) {
			if (w->mball.vertical) {
				LetterPosition(w,
					       (3 * w->mball.wedges / 2 - 1 - wedge) % w->mball.wedges,
					       w->mball.rings - 1 - ring,
					  lengthx - startx, lengthy - starty,
					       &letterX, &letterY);
				letterX += startx + w->mball.letterOffset.x;
				letterY += lengthy + 3 + w->mball.letterOffset.y;
			} else {
				LetterPosition(w,
					       w->mball.wedges - 1 - wedge,
					       w->mball.rings - 1 - ring,
					  lengthx - startx, lengthy - starty,
					       &letterX, &letterY);
				letterX += lengthx + 3 + w->mball.letterOffset.x;
				letterY += starty + w->mball.letterOffset.y;
			}
			if (w->mball.orient && w->mball.mballLoc[wedge][ring].direction) {
				(void) sprintf(buf, "%d%c", w->mball.mballLoc[wedge][ring].wedge,
					       w->mball.wedgeName[w->mball.mballLoc[wedge][ring].wedge][0]);
				letterX += w->mball.letterOffset.x;
				XDrawString(XtDisplay(w), XtWindow(w), w->mball.inverseGC,
					    letterX, letterY, buf, 2);
			} else {
				(void) sprintf(buf, "%c",
					       w->mball.wedgeName[w->mball.mballLoc[wedge][ring].wedge][0]);
				XDrawString(XtDisplay(w), XtWindow(w), w->mball.inverseGC,
					    letterX, letterY, buf, 1);
			}
		}
	} else if (w->mball.orient) {
		int         letterX, letterY;
		char        buf[2];

		if (ring < (w->mball.rings + 1) / 2 &&
		    !w->mball.mballLoc[wedge][ring].direction) {
			LetterPosition(w, wedge, ring, lengthx - startx, lengthy - starty,
				       &letterX, &letterY);
			letterX += startx + w->mball.letterOffset.x;
			letterY += starty + w->mball.letterOffset.y;
			(void) sprintf(buf, "%d", w->mball.mballLoc[wedge][ring].wedge);
			XDrawString(XtDisplay(w), XtWindow(w), w->mball.inverseGC,
				    letterX, letterY, buf, 1);
		}
		if (ring + 1 > w->mball.rings / 2 &&
		    w->mball.mballLoc[wedge][ring].direction) {
			if (w->mball.vertical) {
				LetterPosition(w,
					       (3 * w->mball.wedges / 2 - 1 - wedge) % w->mball.wedges,
					       w->mball.rings - 1 - ring,
					  lengthx - startx, lengthy - starty,
					       &letterX, &letterY);
				letterX += startx + w->mball.letterOffset.x;
				letterY += lengthy + 3 + w->mball.letterOffset.y;
			} else {
				LetterPosition(w,
					       w->mball.wedges - 1 - wedge,
					       w->mball.rings - 1 - ring,
					  lengthx - startx, lengthy - starty,
					       &letterX, &letterY);
				letterX += lengthx + 3 + w->mball.letterOffset.x;
				letterY += starty + w->mball.letterOffset.y;
			}
			(void) sprintf(buf, "%d", w->mball.mballLoc[wedge][ring].wedge);
			XDrawString(XtDisplay(w), XtWindow(w), w->mball.inverseGC,
				    letterX, letterY, buf, 1);
		}
	}
}

static void
DrawRadar(MballWidget w, GC gc, int startx, int starty, int lengthx, int lengthy)
{
	int         r, i;
	double      angle, increment;

	XDrawArc(XtDisplay(w), XtWindow(w), gc, startx, starty,
		 lengthx, lengthy, 0, CIRCLE);
	if (w->mball.rings % 2)
		for (r = 1; r < w->mball.rings / 2 + 1; r++)
			XDrawArc(XtDisplay(w), XtWindow(w), gc,
				 startx - lengthx / (2 * w->mball.rings) +
				 (w->mball.rings / 2 + 1 - r) * lengthx / w->mball.rings,
				 starty - lengthy / (2 * w->mball.rings) +
				 (w->mball.rings / 2 + 1 - r) * lengthy / w->mball.rings,
				 r * 2 * lengthx / w->mball.rings,
				 r * 2 * lengthy / w->mball.rings,
				 0, CIRCLE);
	else
		for (r = 1; r < w->mball.rings / 2; r++)
			XDrawArc(XtDisplay(w), XtWindow(w), gc,
				 startx + (w->mball.rings / 2 - r) * lengthx / w->mball.rings,
				 starty + (w->mball.rings / 2 - r) * lengthy / w->mball.rings,
				 r * 2 * lengthx / w->mball.rings,
				 r * 2 * lengthy / w->mball.rings,
				 0, CIRCLE);
	increment = RADIANS(NUM_DEGREES) / (double) w->mball.wedges;
	angle = RADIANS(RT_ANG);
	for (i = 0; i < w->mball.wedges; i++) {
		XDrawLine(XtDisplay(w), XtWindow(w), gc,
			  startx + lengthx / 2, starty + lengthy / 2,
			  startx + lengthx / 2 + (int) ((double) lengthx * cos(angle) / 2.0),
			  starty + lengthy / 2 + (int) ((double) lengthy * sin(angle) / 2.0));
		angle += increment;
	}
}

static void
DrawSect(MballWidget w, GC wedgeGC, GC borderGC, int r, int wedge, int startx, int starty, int lengthx, int lengthy)
{
	int         dx, dy;

	OffsetSect(w, wedge, &dx, &dy);
	if (w->mball.rings % 2) {
		if (r == w->mball.rings / 2) {
			XFillSector(XtDisplay(w), XtWindow(w), wedgeGC,
			startx + lengthx / 2 + dx, starty + lengthy / 2 + dy,
			  r * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
			  r * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthx / (w->mball.rings + 1) - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthy / (w->mball.rings + 1) - 2 * w->mball.dr,
				 CIRCLE_4 - CIRCLE * wedge / w->mball.wedges,
				    -CIRCLE / w->mball.wedges);
			XDrawSector(XtDisplay(w), XtWindow(w), borderGC,
			startx + lengthx / 2 + dx, starty + lengthy / 2 + dy,
			  r * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
			  r * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthx / (w->mball.rings + 1) - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthy / (w->mball.rings + 1) - 2 * w->mball.dr,
				 CIRCLE_4 - CIRCLE * wedge / w->mball.wedges,
				    -CIRCLE / w->mball.wedges);
		} else {
			XFillSector(XtDisplay(w), XtWindow(w), wedgeGC,
			startx + lengthx / 2 + dx, starty + lengthy / 2 + dy,
			  r * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
			  r * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
				 CIRCLE_4 - CIRCLE * wedge / w->mball.wedges,
				    -CIRCLE / w->mball.wedges);
			XDrawSector(XtDisplay(w), XtWindow(w), borderGC,
			startx + lengthx / 2 + dx, starty + lengthy / 2 + dy,
			  r * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
			  r * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
				    (r + 1) * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
				 CIRCLE_4 - CIRCLE * wedge / w->mball.wedges,
				    -CIRCLE / w->mball.wedges);
		}
	} else {
		XFillSector(XtDisplay(w), XtWindow(w), wedgeGC,
			startx + lengthx / 2 + dx, starty + lengthy / 2 + dy,
			  r * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
			  r * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
		    (r + 1) * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
		    (r + 1) * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
			    CIRCLE_4 - CIRCLE * wedge / w->mball.wedges,
			    -CIRCLE / w->mball.wedges);
		XDrawSector(XtDisplay(w), XtWindow(w), borderGC,
			startx + lengthx / 2 + dx, starty + lengthy / 2 + dy,
			  r * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
			  r * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
		    (r + 1) * 2 * lengthx / w->mball.rings - 2 * w->mball.dr,
		    (r + 1) * 2 * lengthy / w->mball.rings - 2 * w->mball.dr,
			    CIRCLE_4 - CIRCLE * wedge / w->mball.wedges,
			    -CIRCLE / w->mball.wedges);
	}
}

static void
XFillSector(Display * display, Drawable drawable, GC gc, int xo, int yo, int width1, int height1, int width2, int height2, int angle1, int angle2)
{
	int         d, r1 = MIN(width1, height1) / 2, r2 = MIN(width2, height2) / 2;
	int         w = MAX(r2 - r1 - 8, 1);

	if (r1 > r2) {
		d = r1;
		r1 = r2;
		r2 = d;
	}
	if (r1 < 0)
		r1 = -3;
	d = MAX(r1 + r2 + 2, 2);
	XSetLineAttributes(display, gc, w, LineSolid, CapNotLast, JoinRound);
	XDrawArc(display, drawable, gc, xo - d / 2, yo - d / 2, d, d,
		 angle1, angle2);
	XSetLineAttributes(display, gc, 1, LineSolid, CapNotLast, JoinRound);
}

static void
XDrawSector(Display * display, Drawable drawable, GC gc, int xo, int yo, int width1, int height1, int width2, int height2, int angle1, int angle2)
{
	int         d, r1 = MIN(width1, height1) / 2, r2 = MIN(width2, height2) / 2;

	/*double ang, x, y; */

	if (r1 > r2) {
		d = r1;
		r1 = r2;
		r2 = d;
	}
	if (r1 < 0)
		r1 = -3;
	d = MAX(2 * (r1 + 3), 1);
	XDrawArc(display, drawable, gc, xo - d / 2, yo - d / 2, d, d,
		 angle1, angle2);
	d = MAX(2 * (r2 - 1), 3);
	XDrawArc(display, drawable, gc, xo - d / 2, yo - d / 2, d, d,
		 angle1, angle2);

	/*ang = RADIANS((double) angle1 / MULT);
	   x = cos(ang);
	   y = sin(ang);
	   XDrawLine(display, drawable, gc,
	   (int) ((double) r1 * x) + xo, (int) ((double) r1 * y) + yo,
	   (int) ((double) r2 * x) + xo, (int) ((double) r2 * y) + yo);
	   ang = RADIANS((double) angle2 / MULT);
	   x = cos(ang);
	   y = sin(ang);
	   XDrawLine(display, drawable, gc,
	   (int) ((double) r1 * x) + xo, (int) ((double) r1 * y) + yo,
	   (int) ((double) r2 * x) + xo, (int) ((double) r2 * y) + yo); */
}

/*-
static void XFillSector(display, drawable, gc,
    xo, yo, width1, height1, width2, height2, angle1, angle2)
  Display *display;
  Drawable drawable;
  GC gc;
  int xo, yo;
  int width1, height1, width2, height2;
  int angle1, angle2;
{
  int d, r1 = MIN(width1, height1) / 2, r2 = MIN(width2, height2) / 2;

  if (r1 > r2) {
    d = r1; r1 = r2; r2 = d;
  }
  if (r1 < 0)
    r1 = -3;
  for (d = 2 * (r1 + 3); d < 2 * (r2 - 1); d++)
    XDrawArc(display, drawable, gc, xo - d / 2, yo - d / 2, d, d,
      angle1, angle2);
}
*/
Boolean
CheckSolved(MballWidget w)
{
	int         wedge, ring;
	MballLoc    test;

	if (w->mball.orient)
		for (wedge = 0; wedge < w->mball.wedges; wedge++) {
			if (wedge == 0) {
				test.wedge = w->mball.mballLoc[wedge][0].wedge;
				test.direction = w->mball.mballLoc[wedge][0].direction;
			}
			for (ring = 0; ring < w->mball.rings; ring++) {
				if (test.direction != w->mball.mballLoc[wedge][ring].direction)
					return FALSE;
				if (test.direction) {
					if ((w->mball.wedges - w->mball.mballLoc[wedge][ring].wedge +
					     test.wedge) % w->mball.wedges != wedge)
						return FALSE;
				} else {
					if ((w->mball.wedges + w->mball.mballLoc[wedge][ring].wedge -
					     test.wedge) % w->mball.wedges != wedge)
						return FALSE;
				}
			}
	} else
		for (wedge = 0; wedge < w->mball.wedges; wedge++)
			for (ring = 0; ring < w->mball.rings; ring++)
				if (ring == 0) {
					test.wedge = w->mball.mballLoc[wedge][ring].wedge;
					test.direction = w->mball.mballLoc[wedge][ring].direction;
				} else if (test.wedge != w->mball.mballLoc[wedge][ring].wedge)
					return FALSE;
	return TRUE;
}

#ifdef DEBUG

static void
PrintMball(MballWidget w)
{
	int         wedge, ring;

	for (wedge = 0; wedge < w->mball.wedges; wedge++) {
		for (ring = 0; ring < w->mball.rings; ring++) {
			(void) printf("%d %d  ", w->mball.mballLoc[wedge][ring].wedge,
				   w->mball.mballLoc[wedge][ring].direction);
		}
		(void) printf("\n");
	}
	(void) printf("\n");
}

#endif
