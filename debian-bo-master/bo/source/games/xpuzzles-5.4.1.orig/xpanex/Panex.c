/*-
# X-BASED PANEX(tm)
#
#  Panex.c
#
###
#
#  Copyright (c) 1996 - 97	David Albert Bagley, bagleyd@bigfoot.com
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

/* Methods file for Panex */

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
#include "PanexP.h"

#ifndef DATAFILE
#define DATAFILE "/usr/games/lib/panex.data"
#endif

static void InitializePanex(Widget request, Widget new);
static void ExposePanex(Widget new, XEvent * event, Region region);
static void ResizePanex(PanexWidget w);
static void DestroyPanex(Widget old);
static Boolean SetValuesPanex(Widget current, Widget request, Widget new);
static void QuitPanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void SelectPanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void ReleasePanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void ResetPanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void GetPanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void WritePanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void UndoPanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void SolvePanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void IncrementPanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void DecrementPanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static void ModePanex(PanexWidget w, XEvent * event, char **args, int nArgs);
static int  SelectTile(PanexWidget w, int x);
static void GetColor(PanexWidget w, int pyramid, int init);
static void CheckTiles(PanexWidget w);
static void ResetTiles(PanexWidget w);
static int  MoveTile(PanexWidget w, int fromStack, int fromPosition, int toStack);
static void SlideTile(PanexWidget w, int fromStack, int fromPosition, int toStack, int toPosition);
static int  RequestMove(PanexWidget w, int fromStack, int fromPosition, int toStack);
static void DrawFrame(PanexWidget w, GC gc, GC stackgc);
static void DrawTile(PanexWidget w, GC tileGC, GC borderGC, int i, int j, int offset);
static void DrawPyramid(PanexWidget w, int color, int i, int j, int size, int offset);

#ifdef DEBUG
static void PrintStacks(PanexWidget w);
static void
PrintTiles(PanexWidget w)
#endif

	static char defaultTranslationsPanex[] =
	"<KeyPress>q: Quit()\n\
   Ctrl<KeyPress>C: Quit()\n\
   <Btn1Down>: Select()\n\
   <Btn1Up>: Release()\n\
   <KeyPress>r: Reset()\n\
   <Btn3Down>(2+): Reset()\n\
   <KeyPress>g: Get()\n\
   <KeyPress>w: Write()\n\
   <KeyPress>u: Undo()\n\
   <KeyPress>s: Solve()\n\
   <KeyPress>i: Increment()\n\
   <KeyPress>d: Decrement()\n\
   <KeyPress>m: Mode()";

	static XtActionsRec actionsListPanex[] =
	{
		{"Quit", (XtActionProc) QuitPanex},
		{"Select", (XtActionProc) SelectPanex},
		{"Release", (XtActionProc) ReleasePanex},
		{"Reset", (XtActionProc) ResetPanex},
		{"Get", (XtActionProc) GetPanex},
		{"Write", (XtActionProc) WritePanex},
		{"Undo", (XtActionProc) UndoPanex},
		{"Solve", (XtActionProc) SolvePanex},
		{"Increment", (XtActionProc) IncrementPanex},
		{"Decrement", (XtActionProc) DecrementPanex},
		{"Mode", (XtActionProc) ModePanex}
};

	static XtResource resourcesPanex[] =
	{
		{XtNuserName, XtCUserName, XtRString, sizeof (String),
		 XtOffset(PanexWidget, panex.username), XtRString, "nobody"},
		{XtNpyramidColor0, XtCLabel, XtRString, sizeof (String),
	     XtOffset(PanexWidget, panex.pyramidName[0]), XtRString, "Blue"},
		{XtNpyramidColor1, XtCLabel, XtRString, sizeof (String),
	      XtOffset(PanexWidget, panex.pyramidName[1]), XtRString, "Red"},
		{XtNforeground, XtCBackground, XtRPixel, sizeof (Pixel),
    XtOffset(PanexWidget, panex.foreground), XtRString, XtDefaultBackground},
		{XtNstackColor, XtCForeground, XtRPixel, sizeof (Pixel),
    XtOffset(PanexWidget, panex.stackColor), XtRString, XtDefaultForeground},
		{XtNtileColor, XtCForeground, XtRPixel, sizeof (Pixel),
     XtOffset(PanexWidget, panex.tileColor), XtRString, XtDefaultForeground},
		{XtNtileBorder, XtCBackground, XtRPixel, sizeof (Pixel),
   XtOffset(PanexWidget, panex.borderColor), XtRString, XtDefaultBackground},
		{XtNwidth, XtCWidth, XtRDimension, sizeof (Dimension),
		 XtOffset(PanexWidget, core.width), XtRString, "400"},
		{XtNheight, XtCHeight, XtRDimension, sizeof (Dimension),
		 XtOffset(PanexWidget, core.height), XtRString, "200"},
		{XtNtiles, XtCTiles, XtRInt, sizeof (int),
		 XtOffset(PanexWidget, panex.tiles), XtRString, "10"},	/*DEFAULTTILES */
		{XtNmode, XtCMode, XtRInt, sizeof (int),
		 XtOffset(PanexWidget, panex.mode), XtRString, "1"},	/*DEFAULTMODE */
		{XtNmono, XtCMono, XtRBoolean, sizeof (Boolean),
		 XtOffset(PanexWidget, panex.mono), XtRString, "FALSE"},
		{XtNstart, XtCBoolean, XtRBoolean, sizeof (Boolean),
		 XtOffset(PanexWidget, panex.started), XtRString, "TRUE"},
		{XtNdelay, XtCDelay, XtRInt, sizeof (int),
		 XtOffset(PanexWidget, panex.delay), XtRString, "300"},
	      {XtNselectCallback, XtCCallback, XtRCallback, sizeof (caddr_t),
	       XtOffset(PanexWidget, panex.select), XtRCallback, NULL}
};

	PanexClassRec panexClassRec =
	{
		{
			(WidgetClass) & widgetClassRec,		/* superclass */
			"Panex",	/* class name */
			sizeof (PanexRec),	/* widget size */
			NULL,	/* class initialize */
			NULL,	/* class part initialize */
			FALSE,	/* class inited */
			(XtInitProc) InitializePanex,	/* initialize */
			NULL,	/* initialize hook */
			XtInheritRealize,	/* realize */
			actionsListPanex,	/* actions */
			XtNumber(actionsListPanex),	/* num actions */
			resourcesPanex,		/* resources */
			XtNumber(resourcesPanex),	/* num resources */
			NULLQUARK,	/* xrm class */
			TRUE,	/* compress motion */
			TRUE,	/* compress exposure */
			TRUE,	/* compress enterleave */
			TRUE,	/* visible interest */
			(XtWidgetProc) DestroyPanex,	/* destroy */
			(XtWidgetProc) ResizePanex,	/* resize */
			(XtExposeProc) ExposePanex,	/* expose */
			(XtSetValuesFunc) SetValuesPanex,	/* set values */
			NULL,	/* set values hook */
			XtInheritSetValuesAlmost,	/* set values almost */
			NULL,	/* get values hook */
			NULL,	/* accept focus */
			XtVersion,	/* version */
			NULL,	/* callback private */
			defaultTranslationsPanex,	/* tm table */
			NULL,	/* query geometry */
			NULL,	/* display accelerator */
			NULL	/* extension */
		},
		{
			0	/* ignore */
		}
};

	WidgetClass panexWidgetClass = (WidgetClass) & panexClassRec;

	static int  startPositions[MAXMODES][MAXSTACKS] =
	{
		{0, -1, -1},
		{0, -1, 1}
	};
	static int  finishPositions[MAXMODES][MAXSTACKS] =
	{
		{-1, -1, 0},
		{1, -1, 0}
	};

/* Only applies to Panex */
	static int  middlePositions[MAXSTACKS] =
	{-1, 0, 1};

	static XPoint trapazoidUnit[5] =
	{
		{0, 0},
		{1, 1},
		{-3, 0},
		{1, -1},
		{2, 0}
	};

#ifndef HAVE_USLEEP
#if !defined( VMS ) || defined( XVMSUTILS ) ||  ( __VMS_VER >= 70000000 )
#ifdef USE_XVMSUTILS
#include <X11/unix_time.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#endif
#endif
#if defined(SYSV) || defined(SVR4)
#ifdef LESS_THAN_AIX3_2
#include <sys/poll.h>
#else /* !LESS_THAN_AIX3_2 */
#include <poll.h>
#endif /* !LESS_THAN_AIX3_2 */
#endif /* defined(SYSV) || defined(SVR4) */

static int
usleep(long unsigned int usec)
{
#if (defined (SYSV) || defined(SVR4)) && !defined(__hpux)
#if defined(HAVE_NANOSLEEP)
	{
		struct timespec rqt;

		rqt.tv_nsec = 1000 * (usec % (unsigned long) 1000000);
		rqt.tv_sec = usec / (unsigned long) 1000000;
		return nanosleep(&rqt, NULL);
	}
#else
	(void) poll((void *) 0, (int) 0, usec / 1000);	/* ms resolution */
#endif
#else
#ifdef VMS
	long        timadr[2];

	if (usec != 0) {
		timadr[0] = -usec * 10;
		timadr[1] = -1;

		sys$setimr(4, &timadr, 0, 0, 0);
		sys$waitfr(4);
	}
#else
	struct timeval time_out;

#if  0				/* (!defined(AIXV3) && !defined(__hpux)) */
	extern int  select(int, fd_set *, fd_set *, fd_set *, struct timeval *);

#endif

	time_out.tv_usec = usec % (unsigned long) 1000000;
	time_out.tv_sec = usec / (unsigned long) 1000000;
	(void) select(0, (void *) 0, (void *) 0, (void *) 0, &time_out);
#endif
#endif
	return 0;
}
#endif

static void
Sleep(long unsigned int cMilliseconds)
{
	(void) usleep(cMilliseconds * 1000);
}

static void
InitializePanex(Widget request, Widget new)
{
	PanexWidget w = (PanexWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	int         pyramid, stack;

	for (stack = 0; stack < MAXSTACKS; stack++) {
		w->panex.tileOfPosition[stack] = NULL;
		w->panex.positionOfTile[stack] = NULL;
	}
	CheckTiles(w);
	InitMoves();
	ResetTiles(w);
	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	values.foreground = w->panex.foreground;
	w->panex.puzzleGC = XtGetGC(new, valueMask, &values);
	w->panex.depth = DefaultDepthOfScreen(XtScreen(w));
	values.foreground = w->panex.stackColor;
	w->panex.stackGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->panex.tileColor;
	w->panex.tileGC = XtGetGC(new, valueMask, &values);
	values.foreground = w->panex.borderColor;
	w->panex.borderGC = XtGetGC(new, valueMask, &values);
	values.background = w->panex.foreground;
	values.foreground = w->core.background_pixel;
	w->panex.inverseGC = XtGetGC(new, valueMask, &values);
	for (pyramid = 0; pyramid <= w->panex.mode; pyramid++)
		GetColor(w, pyramid, TRUE);
	ResizePanex(w);
}

static void
DestroyPanex(Widget old)
{
	PanexWidget w = (PanexWidget) old;
	int         pyramid;

	for (pyramid = 0; pyramid <= w->panex.mode; pyramid++)
		XtReleaseGC(old, w->panex.pyramidGC[pyramid]);
	XtReleaseGC(old, w->panex.puzzleGC);
	XtReleaseGC(old, w->panex.stackGC);
	XtReleaseGC(old, w->panex.tileGC);
	XtReleaseGC(old, w->panex.borderGC);
	XtReleaseGC(old, w->panex.inverseGC);
	XtRemoveCallbacks(old, XtNselectCallback, w->panex.select);
}

static void
ResizePanex(PanexWidget w)
{
	w->panex.delta.x = 8;
	w->panex.delta.y = 2;
	w->panex.pos.x = MAX(((int) w->core.width - w->panex.delta.x) /
			     MAXSTACKS, w->panex.delta.x);
	w->panex.pos.y = MAX(((int) w->core.height - 2 * w->panex.delta.y - 2) /
			     (w->panex.tiles + 1), w->panex.delta.y);
	w->panex.width = w->panex.pos.x * MAXSTACKS + w->panex.delta.x + 2;
	w->panex.height = w->panex.pos.y * (w->panex.tiles + 1) +
		w->panex.delta.y + 2;
	w->panex.puzzleOffset.x = ((int) w->core.width - w->panex.width + 2) / 2;
	w->panex.puzzleOffset.y = ((int) w->core.height - w->panex.height + 2) / 2;
	/* Make the following even */
	w->panex.tileSize.x = ((w->panex.pos.x - w->panex.delta.x) >> 1) << 1;
	w->panex.tileSize.y = w->panex.pos.y - w->panex.delta.y;
	w->panex.letterOffset.x = 3;
	w->panex.letterOffset.y = 4;
}

static void
ExposePanex(Widget new, XEvent * event, Region region)
		 /* Not used */
{
	PanexWidget w = (PanexWidget) new;

	if (w->core.visible) {
		DrawFrame(w, w->panex.puzzleGC, w->panex.stackGC);
		DrawAllTiles(w, w->panex.tileGC, w->panex.borderGC);
	}
}

static      Boolean
SetValuesPanex(Widget current, Widget request, Widget new)
{
	PanexWidget c = (PanexWidget) current, w = (PanexWidget) new;
	XGCValues   values;
	XtGCMask    valueMask;
	Boolean     redraw = FALSE;
	Boolean     redrawTiles = FALSE;
	int         pyramid;

	CheckTiles(w);
	if (w->panex.foreground != c->panex.foreground) {
		valueMask = GCForeground | GCBackground;
		values.foreground = w->panex.foreground;
		values.background = w->core.background_pixel;
		XtReleaseGC(new, w->panex.puzzleGC);
		w->panex.puzzleGC = XtGetGC(new, valueMask, &values);
		if (w->panex.mono || w->panex.depth == 1) {
			values.foreground = w->core.background_pixel;
			values.background = w->panex.tileColor;
			for (pyramid = 0; pyramid < MAXSTACKS; pyramid++) {
				XtReleaseGC((Widget) w, w->panex.pyramidGC[pyramid]);
				w->panex.pyramidGC[pyramid] = XtGetGC((Widget) w, valueMask, &values);
			}
			c->panex.mono = w->panex.mono;
		}
		redrawTiles = TRUE;
	}
	if (w->panex.stackColor != c->panex.stackColor ||
	    w->panex.tileColor != c->panex.tileColor ||
	    w->panex.borderColor != c->panex.borderColor ||
	    w->core.background_pixel != c->core.background_pixel) {
		valueMask = GCForeground | GCBackground;
		values.background = w->core.background_pixel;

		values.foreground = w->panex.stackColor;
		XtReleaseGC(new, w->panex.stackGC);
		w->panex.stackGC = XtGetGC(new, valueMask, &values);
		values.foreground = w->panex.borderColor;
		XtReleaseGC(new, w->panex.borderGC);
		w->panex.borderGC = XtGetGC(new, valueMask, &values);
		values.foreground = w->panex.tileColor;
		XtReleaseGC(new, w->panex.tileGC);
		w->panex.tileGC = XtGetGC(new, valueMask, &values);
		values.background = w->panex.tileColor;
		values.foreground = w->core.background_pixel;
		XtReleaseGC(new, w->panex.inverseGC);
		w->panex.inverseGC = XtGetGC(new, valueMask, &values);
		redrawTiles = TRUE;
	}
	for (pyramid = 0; pyramid <= w->panex.mode; pyramid++) {
		if (strcmp(w->panex.pyramidName[pyramid], c->panex.pyramidName[pyramid]))
			GetColor(w, pyramid, FALSE);
	}
	if (w->panex.tiles != c->panex.tiles) {
		DrawFrame(c, c->panex.inverseGC, c->panex.inverseGC);
		ResetTiles(w);
		ResizePanex(w);
		redraw = TRUE;
	} else if (w->panex.mode != c->panex.mode) {
		ResetTiles(w);
		redraw = TRUE;
	} else if (w->panex.puzzleOffset.x != c->panex.puzzleOffset.x ||
		   w->panex.puzzleOffset.y != c->panex.puzzleOffset.y) {
		DrawFrame(c, c->panex.inverseGC, c->panex.inverseGC);
		ResizePanex(w);
		redraw = TRUE;
	}
	if (redrawTiles && !redraw && XtIsRealized(new) && new->core.visible) {
		DrawFrame(c, c->panex.inverseGC, c->panex.inverseGC);
		DrawFrame(w, w->panex.puzzleGC, c->panex.stackGC);
		DrawAllTiles(w, w->panex.tileGC, w->panex.borderGC);
	}
	return (redraw);
}

static void
QuitPanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

static void
SelectPanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	int         stack;

	if (!CheckSolved(w) &&
	    ((stack = SelectTile(w, event->xbutton.x)) >= 0) &&
	    ((w->panex.currentPosition = TopOfStack(w, stack)) >= 0)) {
		w->panex.currentStack = stack;
		DrawTile(w, w->panex.borderGC, w->panex.tileGC,
			 stack, w->panex.currentPosition, 1);
	} else
		w->panex.currentStack = -1;
}

static void
ReleasePanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	panexCallbackStruct cb;
	int         toStack, toPosition;

	if (w->panex.currentStack < 0)
		return;
	DrawTile(w, w->panex.inverseGC, w->panex.inverseGC,
		 w->panex.currentStack, w->panex.currentPosition, 1);
	DrawTile(w, w->panex.tileGC, w->panex.borderGC,
		 w->panex.currentStack, w->panex.currentPosition, 0);
	if ((toStack = SelectTile(w, event->xbutton.x)) >= 0 &&
	    toStack != w->panex.currentStack) {
		if ((toPosition = MovePanex(w,
					    w->panex.currentStack, w->panex.currentPosition, toStack)) >= 0) {
			if (CheckSolved(w)) {
				cb.reason = PANEX_SOLVED;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else if (toPosition == -1) {
			cb.reason = PANEX_BLOCKED;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		} else {
			cb.reason = PANEX_ILLEGAL;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
	w->panex.currentStack = -1;
}

static void
ResetPanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	panexCallbackStruct cb;

	DrawFrame(w, w->panex.inverseGC, w->panex.inverseGC);
	ResetTiles(w);
	DrawFrame(w, w->panex.puzzleGC, w->panex.stackGC);
	DrawAllTiles(w, w->panex.tileGC, w->panex.borderGC);
	cb.reason = PANEX_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
GetPanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;
	char        c;
	int         i, tiles, mode, moves;
	panexCallbackStruct cb;

	if ((fp = fopen(DATAFILE, "r")) == NULL)
		(void) printf("Can not read %s for get.\n", DATAFILE);
	else {
		FlushMoves(w);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &mode);
		if (mode >= HANOI && mode <= PANEX) {
			if (w->panex.mode != mode) {
				cb.reason = PANEX_MODE;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: mode %d should be between %d and %d\n",
				      DATAFILE, mode, HANOI, PANEX);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &tiles);
		if (tiles >= MINTILES) {
			for (i = w->panex.tiles; i < tiles; i++) {
				cb.reason = PANEX_INC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
			for (i = w->panex.tiles; i > tiles; i--) {
				cb.reason = PANEX_DEC;
				XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
			}
		} else
			(void) printf("%s corrupted: tiles %d should be between %d and MAXINT\n",
				      DATAFILE, tiles, MINTILES);
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &moves);
		ScanStartPosition(fp, w);
		cb.reason = PANEX_RESTORE;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		SetStartPosition(w);
		ScanMoves(fp, w, moves);
		(void) fclose(fp);
		if (mode == 0)
			(void) printf("%s: mode hanoi, tiles %d, moves %d.\n",
				      DATAFILE, tiles, moves);
		else
			(void) printf("%s: mode panex, tiles %d, moves %d.\n",
				      DATAFILE, tiles, moves);
	}
}

static void
WritePanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	FILE       *fp;

	if ((fp = fopen(DATAFILE, "w")) == NULL)
		(void) printf("Can not write to %s.\n", DATAFILE);
	else {
		(void) fprintf(fp, "mode%c %d\n", SYMBOL, w->panex.mode);
		(void) fprintf(fp, "tiles%c %d\n", SYMBOL, w->panex.tiles);
		(void) fprintf(fp, "moves%c %d\n", SYMBOL, NumMoves());
		PrintStartPosition(fp, w);
		PrintMoves(fp);
		(void) fclose(fp);
		(void) printf("Saved to %s.\n", DATAFILE);
	}
}

static void
UndoPanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	if (MadeMoves()) {
		panexCallbackStruct cb;
		int         fromStack, fromPosition, toStack;

		GetMove(&toStack, &fromStack);
		if ((fromPosition = TopOfStack(w, fromStack)) < 0 ||
		    MoveTile(w, fromStack, fromPosition, toStack) < 0)
			(void) printf(
					     "move from %d to %d can not be made.", fromStack, toStack);
		else {
			cb.reason = PANEX_UNDO;
			XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		}
	}
}

static void
SolvePanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	panexCallbackStruct cb;

	/* Cheat and Reset To Start */
	DrawFrame(w, w->panex.inverseGC, w->panex.inverseGC);
	ResetTiles(w);
	DrawFrame(w, w->panex.puzzleGC, w->panex.stackGC);
	DrawAllTiles(w, w->panex.tileGC, w->panex.borderGC);
	cb.reason = PANEX_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	SolveTiles(w);
}

static void
IncrementPanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	panexCallbackStruct cb;

	cb.reason = PANEX_INC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
DecrementPanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	panexCallbackStruct cb;

	if (w->panex.tiles <= MINTILES)
		return;
	cb.reason = PANEX_DEC;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

static void
ModePanex(PanexWidget w, XEvent * event, char **args, int nArgs)
{
	panexCallbackStruct cb;

	cb.reason = PANEX_MODE;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

int
MovePanex(PanexWidget w, int fromStack, int fromPosition, int toStack)
{
	panexCallbackStruct cb;
	int         toPosition;

	if ((toPosition = MoveTile(w, fromStack, fromPosition, toStack)) >= 0) {
		cb.reason = PANEX_MOVED;
		XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
		PutMove(fromStack, toStack);
	}
	return toPosition;
}

static int
SelectTile(PanexWidget w, int x)
{
	int         i;

	x -= w->panex.puzzleOffset.x;
	i = (x - w->panex.delta.x / 2) / w->panex.pos.x;
	if (i < 0)
		i = 0;
	else if (i >= MAXSTACKS)
		i = MAXSTACKS - 1;
	/*
	   y -= w->panex.puzzleOffset.y;
	   j = (y - w->panex.delta.y / 2) / w->panex.pos.y;
	   if (j < 0)
	   j = 0;
	   else if (j > w->panex.tiles)
	   j = w->panex.tiles;
	 */
	return i;
}

static void
GetColor(PanexWidget w, int pyramid, int init)
{
	XGCValues   values;
	XtGCMask    valueMask;
	XColor      colorCell, rgb;

	valueMask = GCForeground | GCBackground;
	values.background = w->core.background_pixel;
	if (w->panex.depth > 1 && !w->panex.mono) {
		if (XAllocNamedColor(XtDisplay(w),
				  DefaultColormap(XtDisplay(w), XtWindow(w)),
			  w->panex.pyramidName[pyramid], &colorCell, &rgb)) {
			values.foreground = w->panex.pyramidColor[pyramid] = colorCell.pixel;
			if (!init)
				XtReleaseGC((Widget) w, w->panex.pyramidGC[pyramid]);
			w->panex.pyramidGC[pyramid] = XtGetGC((Widget) w, valueMask, &values);
			return;
		} else {
			char        buf[121];

			(void) sprintf(buf, "Color name \"%s\" is not defined",
				       w->panex.pyramidName[pyramid]);
			XtWarning(buf);
		}
	}
	values.foreground = w->panex.foreground;
	if (!init)
		XtReleaseGC((Widget) w, w->panex.pyramidGC[pyramid]);
	w->panex.pyramidGC[pyramid] = XtGetGC((Widget) w, valueMask, &values);
}

static void
CheckTiles(PanexWidget w)
{
	char        buf[121];

	if (w->panex.tiles < 1) {
		(void) sprintf(buf, "Number of Tiles out of bounds, use 1..MAXINT");
		XtWarning(buf);
		w->panex.tiles = DEFAULTTILES;
	}
	if (w->panex.mode < HANOI || w->panex.mode > PANEX) {
		XtWarning("Mode is in error, use 0 for Hanoi, 1 for Panex");
		w->panex.mode = DEFAULTMODE;
	}
	if (w->panex.delay < 0) {
		(void) sprintf(buf, "Delay out of bounds, use 0..MAXINT");
		XtWarning(buf);
		w->panex.delay = -w->panex.delay;
	}
}

static void
ResetTiles(PanexWidget w)
{
	int         stack, loc;
	PanexLoc    i;

	w->panex.currentStack = -1;
	for (stack = 0; stack < MAXMODES; stack++) {
		if (w->panex.positionOfTile[stack]) {
			(void) free((void *) w->panex.positionOfTile[stack]);
			w->panex.positionOfTile[stack] = NULL;
		}
		if (startLoc[stack]) {
			(void) free((void *) startLoc[stack]);
			startLoc[stack] = NULL;
		}
	}
	for (stack = 0; stack <= w->panex.mode; stack++) {
		if (!(w->panex.positionOfTile[stack] = (PanexLoc *)
		      malloc(sizeof (PanexLoc) * w->panex.tiles)))
			XtError("Not enough memory, exiting.");
		if (!(startLoc[stack] = (PanexLoc *)
		      malloc(sizeof (PanexLoc) * w->panex.tiles)))
			XtError("Not enough memory, exiting.");
	}
	for (stack = 0; stack < MAXSTACKS; stack++) {
		if (w->panex.tileOfPosition[stack])
			(void) free((void *) w->panex.tileOfPosition[stack]);
		if (!(w->panex.tileOfPosition[stack] = (PanexLoc *)
		      malloc(sizeof (PanexLoc) * (w->panex.tiles + 1))))
			XtError("Not enough memory, exiting.");
	}
	for (stack = 0; stack <= w->panex.mode; stack++) {
		i.stack = 2 * stack;
		for (loc = 0; loc < w->panex.tiles; loc++) {
			i.loc = loc + 1;
			w->panex.positionOfTile[stack][loc] = i;
		}
	}
	for (stack = 0; stack < MAXSTACKS; stack++) {
		i.stack = -1;
		i.loc = -1;
		w->panex.tileOfPosition[stack][0] = i;
		if ((i.stack = startPositions[w->panex.mode][stack]) >= 0)
			for (loc = 1; loc <= w->panex.tiles; loc++) {
				i.loc = loc - 1;
				w->panex.tileOfPosition[stack][loc] = i;
		} else
			for (loc = 1; loc <= w->panex.tiles; loc++)
				w->panex.tileOfPosition[stack][loc] = i;
	}
	FlushMoves(w);
	w->panex.started = FALSE;
}

int
TopOfStack(PanexWidget w, int stack)
{
	int         i;

	for (i = 0; i <= w->panex.tiles; i++)
		if (w->panex.tileOfPosition[stack][i].stack >= 0)
			return i;
	return -1;
}

static int
RequestMove(PanexWidget w, int fromStack, int fromPosition, int toStack)
{
	int         i;

	/* Do not have to check above stack since it is the top one */
	if (toStack > fromStack)
		for (i = fromStack + 1; i <= toStack; i++) {
			if (w->panex.tileOfPosition[i][0].stack >= 0)
				return (-1);
	} else			/* Already ruled out toStack == fromStack */
		for (i = fromStack - 1; i >= toStack; i--) {
			if (w->panex.tileOfPosition[i][0].stack >= 0)
				return (-1);
		}
	i = TopOfStack(w, toStack);
	i = (i == -1) ? w->panex.tiles : i - 1;
	if (w->panex.mode == HANOI) {
		if (i == w->panex.tiles || w->panex.tileOfPosition[toStack][i + 1].loc >
		    w->panex.tileOfPosition[fromStack][fromPosition].loc)
			return i;
		else
			return -2;
	} else {
		if (i > w->panex.tileOfPosition[fromStack][fromPosition].loc + 1)
			return (w->panex.tileOfPosition[fromStack][fromPosition].loc + 1);
		else
			return i;
	}
}

int
MoveTile(PanexWidget w, int fromStack, int fromPosition, int toStack)
{
	int         toPosition;

	if ((toPosition = RequestMove(w, fromStack, fromPosition, toStack)) >= 0)
		SlideTile(w, fromStack, fromPosition, toStack, toPosition);
	return toPosition;
}

static void
SlideTile(PanexWidget w, int fromStack, int fromPosition, int toStack, int toPosition)
{
	PanexLoc    top;
	int         currentStack = fromStack, currentPosition = fromPosition;
	int         nextStack = fromStack, nextPosition = fromPosition;

	while (currentPosition > 0) {
		DrawTile(w, w->panex.inverseGC, w->panex.inverseGC,
			 currentStack, currentPosition, 0);
		top = w->panex.tileOfPosition[currentStack][currentPosition];
		nextPosition = currentPosition - 1;
		w->panex.tileOfPosition[nextStack][nextPosition] = top;
		w->panex.positionOfTile[top.stack][top.loc].stack = nextStack;
		w->panex.positionOfTile[top.stack][top.loc].loc = nextPosition;
		w->panex.tileOfPosition[currentStack][currentPosition].stack = -1;
		currentPosition = nextPosition;
		DrawTile(w, w->panex.tileGC, w->panex.borderGC,
			 currentStack, currentPosition, 0);
		XFlush(XtDisplay(w));
		Sleep((unsigned long) w->panex.delay / (w->panex.tiles + MAXSTACKS - 1));
	}
	while (currentStack != toStack) {
		DrawTile(w, w->panex.inverseGC, w->panex.inverseGC,
			 currentStack, currentPosition, 0);
		top = w->panex.tileOfPosition[currentStack][currentPosition];
		nextStack = (currentStack < toStack) ? currentStack + 1 :
			currentStack - 1;
		w->panex.tileOfPosition[nextStack][nextPosition] = top;
		w->panex.positionOfTile[top.stack][top.loc].stack = nextStack;
		w->panex.positionOfTile[top.stack][top.loc].loc = nextPosition;
		w->panex.tileOfPosition[currentStack][currentPosition].stack = -1;
		currentStack = nextStack;
		DrawTile(w, w->panex.tileGC, w->panex.borderGC,
			 currentStack, currentPosition, 0);
		XFlush(XtDisplay(w));
		Sleep((unsigned long) w->panex.delay / (w->panex.tiles + MAXSTACKS - 1));
	}
	while (currentPosition < toPosition) {
		DrawTile(w, w->panex.inverseGC, w->panex.inverseGC,
			 currentStack, currentPosition, 0);
		top = w->panex.tileOfPosition[currentStack][currentPosition];
		nextPosition = currentPosition + 1;
		w->panex.tileOfPosition[nextStack][nextPosition] = top;
		w->panex.positionOfTile[top.stack][top.loc].stack = nextStack;
		w->panex.positionOfTile[top.stack][top.loc].loc = nextPosition;
		w->panex.tileOfPosition[currentStack][currentPosition].stack = -1;
		currentPosition = nextPosition;
		DrawTile(w, w->panex.tileGC, w->panex.borderGC,
			 currentStack, currentPosition, 0);
		XFlush(XtDisplay(w));
		Sleep((unsigned long) w->panex.delay / (w->panex.tiles + MAXSTACKS - 1));
	}
}

static void
DrawFrame(PanexWidget w, GC gc, GC stackgc)
{
	int         i, dx, dy, x, y;

	x = MAXSTACKS * w->panex.pos.x + w->panex.delta.x - 1;
	dx = w->panex.tileSize.x / 3 + w->panex.delta.x + w->panex.puzzleOffset.x;
	y = (w->panex.tiles + 1) * w->panex.pos.y + w->panex.delta.y - 1;
	dy = w->panex.delta.y / 2 + w->panex.puzzleOffset.y + w->panex.tileSize.y / 3;
	if (gc == w->panex.inverseGC && stackgc == w->panex.inverseGC) {
		XFillRectangle(XtDisplay(w), XtWindow(w), gc,
		 w->panex.puzzleOffset.x, w->panex.puzzleOffset.y, x, y + 1);
		return;
	}
	XFillRectangle(XtDisplay(w), XtWindow(w), gc,
		     w->panex.puzzleOffset.x, w->panex.puzzleOffset.y, x, 1);
	XFillRectangle(XtDisplay(w), XtWindow(w), gc,
		 w->panex.puzzleOffset.x, y + w->panex.puzzleOffset.y, x, 1);
	XFillRectangle(XtDisplay(w), XtWindow(w), gc,
		     w->panex.puzzleOffset.x, w->panex.puzzleOffset.y, 1, y);
	XFillRectangle(XtDisplay(w), XtWindow(w), gc,
	     x + w->panex.puzzleOffset.x, w->panex.puzzleOffset.y, 1, y + 1);
	XFillRectangle(XtDisplay(w), XtWindow(w), stackgc, dx, dy,
		  w->panex.tileSize.x / 3 + (MAXSTACKS - 1) * w->panex.pos.x,
		       w->panex.tileSize.y / 3);
	for (i = 0; i < MAXSTACKS; i++) {
		XFillRectangle(XtDisplay(w), XtWindow(w), stackgc, dx, dy,
		w->panex.tileSize.x / 3, y - 1 - 2 * w->panex.tileSize.y / 3);
		dx += w->panex.pos.x;
	}
}

void
DrawAllTiles(PanexWidget w, GC tileGC, GC borderGC)
{
	int         i, j;

	for (i = 0; i < MAXSTACKS; i++)
		for (j = 0; j <= w->panex.tiles; j++)
			if (w->panex.tileOfPosition[i][j].stack >= 0)
				DrawTile(w, tileGC, borderGC, i, j, 0);
}

static void
DrawTile(PanexWidget w, GC tileGC, GC borderGC, int i, int j, int offset)
{
	int         dx, dy;

	dx = i * w->panex.pos.x + w->panex.delta.x + w->panex.puzzleOffset.x;
	dy = j * w->panex.pos.y + w->panex.delta.y + w->panex.puzzleOffset.y;
	if (tileGC != w->panex.inverseGC || borderGC != w->panex.inverseGC) {
		XFillRectangle(XtDisplay(w), XtWindow(w), tileGC,
			   dx, dy, w->panex.tileSize.x, w->panex.tileSize.y);
		XDrawRectangle(XtDisplay(w), XtWindow(w), borderGC,
			   dx, dy, w->panex.tileSize.x, w->panex.tileSize.y);
		DrawPyramid(w,
			    w->panex.tileOfPosition[i][j].stack, i, j,
			    w->panex.tileOfPosition[i][j].loc, offset);
	} else {		/* Draw Slots */
		XFillRectangle(XtDisplay(w), XtWindow(w), tileGC,
		   dx, dy, w->panex.tileSize.x + 1, w->panex.tileSize.y + 1);
		if (j == 0) {
			if (i == 0) {
				XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
					       dx + w->panex.tileSize.x / 3, dy + w->panex.tileSize.y / 3 - 1,
					       w->panex.tileSize.x / 3, 2 * w->panex.tileSize.y / 3 + 3);
				XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
					       dx + w->panex.tileSize.x / 3, dy + w->panex.tileSize.y / 3 - 1,
					       2 * w->panex.tileSize.x / 3 + 2, w->panex.tileSize.y / 3);
			} else if (i == MAXSTACKS - 1) {
				XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
					       dx + w->panex.tileSize.x / 3, dy + w->panex.tileSize.y / 3 - 1,
					       w->panex.tileSize.x / 3, 2 * w->panex.tileSize.y / 3 + 3);
				XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
					dx, dy + w->panex.tileSize.y / 3 - 1,
					       2 * w->panex.tileSize.x / 3 - 1, w->panex.tileSize.y / 3);
			} else {
				XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
					       dx + w->panex.tileSize.x / 3, dy + w->panex.tileSize.y / 3 - 1,
					       w->panex.tileSize.x / 3, 2 * w->panex.tileSize.y / 3 + 3);
				XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
					dx, dy + w->panex.tileSize.y / 3 - 1,
					       w->panex.tileSize.x + 1, w->panex.tileSize.y / 3);
			}
		} else if (j == w->panex.tiles)
			XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
				       dx + w->panex.tileSize.x / 3, dy,
				       w->panex.tileSize.x / 3, 2 * w->panex.tileSize.y / 3 + 2);
		else
			XFillRectangle(XtDisplay(w), XtWindow(w), w->panex.stackGC,
				       dx + w->panex.tileSize.x / 3, dy,
			   w->panex.tileSize.x / 3, w->panex.tileSize.y + 1);
	}
}

static void
DrawPyramid(PanexWidget w, int color, int i, int j, int size, int offset)
{
	XPoint      trapazoidList[5];
	int         k;

	for (k = 0; k <= 4; k++) {
		if (ABS(trapazoidUnit[k].x) == 3)
			trapazoidList[k].x = (size + 1) * SIGN(trapazoidUnit[k].x) *
				w->panex.tileSize.x / (w->panex.tiles + 1);
		else if (ABS(trapazoidUnit[k].x) == 2)
			trapazoidList[k].x = size * SIGN(trapazoidUnit[k].x) *
				w->panex.tileSize.x / (w->panex.tiles + 1);
		else
			trapazoidList[k].x = w->panex.tileSize.x /
				(2 * (w->panex.tiles + 1));
		trapazoidList[k].y = (w->panex.tileSize.y - 3) * trapazoidUnit[k].y;
	}
	k = w->panex.delta.x + i * w->panex.pos.x + w->panex.puzzleOffset.x +
		w->panex.tileSize.x / 2 + offset + 1;
	trapazoidList[0].x = trapazoidList[4].x / 2 + k;
	trapazoidList[0].y = j * w->panex.pos.y + w->panex.delta.y +
		w->panex.puzzleOffset.y + offset + 2;
	if (w->panex.depth == 1 || w->panex.mono) {
		char        buf[2];

		XFillPolygon(XtDisplay(w), XtWindow(w), (offset) ? w->panex.tileGC :
			     w->panex.inverseGC,
			     trapazoidList, 4, Convex, CoordModePrevious);
		(void) sprintf(buf, "%c", w->panex.pyramidName[color][0]);
		XDrawString(XtDisplay(w), XtWindow(w),
			    (offset) ? w->panex.inverseGC : w->panex.tileGC,
			    k - w->panex.letterOffset.x, trapazoidList[0].y +
		  w->panex.tileSize.y / 2 + w->panex.letterOffset.y, buf, 1);
	} else
		XFillPolygon(XtDisplay(w), XtWindow(w), w->panex.pyramidGC[color],
			     trapazoidList, 4, Convex, CoordModePrevious);
}

Boolean
CheckMiddle(PanexWidget w)
{
	int         stack, loc;
	PanexLoc    i;

	for (stack = 0; stack < MAXSTACKS; stack++)
		if ((i.stack = middlePositions[stack]) >= 0)
			for (loc = 1; loc <= w->panex.tiles; loc++) {
				i.loc = loc - 1;
				if (w->panex.tileOfPosition[stack][loc].stack != i.stack ||
				    w->panex.tileOfPosition[stack][loc].loc != i.loc)
					return FALSE;
			}
	return TRUE;
}

Boolean
CheckSolved(PanexWidget w)
{
	int         stack, loc;
	PanexLoc    i;

	for (stack = 0; stack < MAXSTACKS; stack++)
		if ((i.stack = finishPositions[w->panex.mode][stack]) >= 0)
			for (loc = 1; loc <= w->panex.tiles; loc++) {
				i.loc = loc - 1;
				if (w->panex.tileOfPosition[stack][loc].stack != i.stack ||
				    w->panex.tileOfPosition[stack][loc].loc != i.loc)
					return FALSE;
			}
	return TRUE;
}

#ifdef DEBUG
void
PrintStacks(PanexWidget w)
{
	int         stack, position;

	(void) printf("top: where are the tiles in the stack\n");
	for (position = 0; position <= w->panex.tiles; position++)
		for (stack = 0; stack < MAXSTACKS; stack++) {
			printf("%d,%d", w->panex.tileOfPosition[stack][position].stack,
			       w->panex.tileOfPosition[stack][position].loc);
			if (stack + 1 == MAXSTACKS)
				(void) printf("\n");
			else
				(void) printf(" | ");
		}
}

void
PrintTiles(PanexWidget w)
{
	int         stack, position;

	(void) printf("pot: which stack are the tiles in\n");
	for (position = 0; position < w->panex.tiles; position++)
		for (stack = 0; stack <= w->panex.mode; stack++) {
			printf("%d,%d", w->panex.positionOfTile[stack][position].stack,
			       w->panex.positionOfTile[stack][position].loc);
			if (stack == w->panex.mode)
				(void) printf("\n");
			else
				(void) printf(" | ");
		}
}

#endif
