/*-
# X-BASED PYRAMINX(tm)
#
#  PyraminxP.h
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

/* Private header file for Pyraminx */

#ifndef _PyraminxP_h
#define _PyraminxP_h

#include "Pyraminx.h"

/*** random number generator ***/
#if HAVE_RAND48
#define SRAND srand48
#define LRAND lrand48
#define MAXRAND (2147483648.0)
#else /* HAVE_RAND48 */
#if HAVE_RANDOM
#define SRAND srand48
#define LRAND lrand48
#define MAXRAND (2147483648.0)
#else /* HAVE_RANDOM */
#if HAVE_RAND
#define SRAND srand48
#define LRAND lrand48
#ifdef AIXV3
#define MAXRAND (2147483648.0)
#else
#define MAXRAND (32768.0)
#endif
#endif /* HAVE_RAND */
#endif /* HAVE_RANDOM */
#endif /* HAVE_RAND48 */

#ifndef SRAND
extern void SetRNG(long int s);

#define SRAND(X) SetRNG((long) X)
#endif
#ifndef LRAND
extern long LongRNG(void);

#define LRAND() LongRNG()
#endif
#ifndef MAXRAND
#define MAXRAND (2147483648.0)
#endif

#define NRAND(X) ((int)(LRAND()%(X)))

#define SYMBOL ':'

#define IGNORE (-1)
#define TOP 0
#define TR 1
#define RIGHT 2
#define BOTTOM 3
#define BL 4
#define LEFT 5
#define COORD 6
#define CW 7
#define CCW 11

/* The following are in xpyraminx.c also */
#define MINTETRAS 1
#define PERIOD2 2
#define PERIOD3 3
#define BOTH 4
#define MAXMODES 3

#define DEFAULTMODE PERIOD3
#define DEFAULTTETRAS 3
#define MAXFACES 4
#define DOWN 1
#define UP 0
#define MAXVIEWS 2
#define MAXSIDES (MAXFACES/MAXVIEWS)
#define MAXORIENT (3*MAXSIDES)

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?(-1):1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _PyraminxLoc {
	int         face, rotation;
} PyraminxLoc;

typedef struct _PyraminxPart {
	Pixel       foreground;
	Pixel       borderColor;
	Pixel       faceColor[MAXFACES];
	PyraminxLoc *tetraLoc[MAXFACES];
	PyraminxLoc *faceLoc[MAXSIDES];
	PyraminxLoc *rowLoc[3][MAXSIDES];
	int         currentFace, currentPosition;
	Boolean     started, practice, orient, vertical, mono, sticky;
	int         mode, depth;
	int         size, sizeSize;
	Position    delta;
	Position    tetraLength;
	Position    faceLength;
	Position    viewLength;
	Position    sideOffset, orientLineLength, orientDiagLength;
	XPoint      puzzleSize;
	XPoint      puzzleOffset;
	GC          puzzleGC;
	GC          borderGC;
	GC          faceGC[MAXFACES];
	GC          inverseGC;
	String      faceName[MAXFACES];
	char       *username;
	XtCallbackList select;
} PyraminxPart;

typedef struct _PyraminxRec {
	CorePart    core;
	PyraminxPart pyraminx;
} PyraminxRec;

/* This gets around C's inability to do inheritance */
typedef struct _PyraminxClassPart {
	int         ignore;
} PyraminxClassPart;

typedef struct _PyraminxClassRec {
	CoreClassPart core_class;
	PyraminxClassPart pyraminx_class;
} PyraminxClassRec;

extern PyraminxClassRec pyraminxClassRec;
extern PyraminxLoc *startLoc[MAXFACES];

extern void MovePyraminx(PyraminxWidget w, int face, int position, int direction, int style, int control);

																		   /* extern void SolvePolyhedrons(); *//* For future auto-solver */
extern void DrawAllPolyhedrons(PyraminxWidget w);
extern Boolean CheckSolved(PyraminxWidget w);
extern void InitMoves(void);
extern void PutMove(int face, int position, int direction, int style, int control);
extern void GetMove(int *face, int *position, int *direction, int *style, int *control);
extern int  MadeMoves(void);
extern void FlushMoves(PyraminxWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, PyraminxWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, PyraminxWidget w);
extern void PrintStartPosition(FILE * fp, PyraminxWidget w);
extern void SetStartPosition(PyraminxWidget w);

#endif /* _PyraminxP_h */
