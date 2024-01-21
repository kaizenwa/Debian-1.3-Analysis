/*-
# X-BASED OCTAHEDRON
#
#  OctP.h
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

/* Private header file for Oct */

#ifndef _OctP_h
#define _OctP_h

#include "Oct.h"

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
#define BR 3
#define BOTTOM 4
#define BL 5
#define LEFT 6
#define TL 7
#define COORD 8
#define CW 9
#define CCW 15

/* The following are in xoct.c also */
#define MINOCTAS 1
#define PERIOD3 3
#define PERIOD4 4
#define BOTH 5
#define MAXMODES 3

#define DEFAULTMODE PERIOD4
#define DEFAULTOCTAS 3
#define MAXFACES 8
#define SAME 0
#define OPPOSITE 1
#define UP 0
#define DOWN 1
#define MAXVIEWS 2
#define MAXSIDES (MAXFACES/MAXVIEWS)
#define MAXORIENT (3*MAXSIDES)

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?(-1):1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _OctLoc {
	int         face, rotation;
} OctLoc;

typedef struct _OctPart {
	Pixel       foreground;
	Pixel       borderColor;
	Pixel       faceColor[MAXFACES];
	OctLoc     *octaLoc[MAXFACES];
	OctLoc     *faceLoc;
	OctLoc     *rowLoc[MAXORIENT / 2];
	int         currentFace, currentPosition;
	Boolean     started, practice, orient, vertical, mono, sticky;
	int         mode, depth;
	int         size, sizeSize;
	Position    delta;
	Position    octaLength;
	Position    faceLength;
	Position    viewLength, viewMiddle;
	Position    orientLineLength;
	XPoint      puzzleSize;
	XPoint      puzzleOffset;
	GC          puzzleGC;
	GC          borderGC;
	GC          faceGC[MAXFACES];
	GC          inverseGC;
	String      faceName[MAXFACES];
	char       *username;
	XtCallbackList select;
} OctPart;

typedef struct _OctRec {
	CorePart    core;
	OctPart     oct;
} OctRec;

/* This gets around C's inability to do inheritance */
typedef struct _OctClassPart {
	int         ignore;
} OctClassPart;

typedef struct _OctClassRec {
	CoreClassPart core_class;
	OctClassPart oct_class;
} OctClassRec;

extern OctClassRec octClassRec;
extern OctLoc *startLoc[MAXFACES];

extern void MoveOct(OctWidget w, int face, int position, int direction, int style, int control);

																		   /* extern void SolvePolyhedrons(); *//* For future auto-solver */
extern void DrawAllPolyhedrons(OctWidget w);
extern Boolean CheckSolved(OctWidget w);
extern void InitMoves(void);
extern void PutMove(int face, int position, int direction, int style, int control);
extern void GetMove(int *face, int *position, int *direction, int *style, int *control);
extern int  MadeMoves(void);
extern void FlushMoves(OctWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, OctWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, OctWidget w);
extern void PrintStartPosition(FILE * fp, OctWidget w);
extern void SetStartPosition(OctWidget w);

#endif /* _OctP_h */
