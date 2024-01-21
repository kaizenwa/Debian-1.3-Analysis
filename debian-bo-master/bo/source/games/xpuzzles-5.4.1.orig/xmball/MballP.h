/*-
# X-BASED MASTERBALL(tm)
#
#  MballP.h
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

/* Private header file for Mball */

#ifndef _MballP_h
#define _MballP_h

#include "Mball.h"

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
#define CUTS (COORD / 2)

/* The following are in xmball.c also */
#define MINWEDGES 2
#define MAXWEDGES 8
#define MINRINGS 1

#define DEFAULTWEDGES 8
#define DEFAULTRINGS 4
#define SAME 0
#define OPPOSITE 1
#define DOWN 0
#define UP 1
#define MAXVIEWS 2

#define NUM_DEGREES 360
#define ST_ANG  (NUM_DEGREES/2)
#define RT_ANG  (NUM_DEGREES/4)
#define DEGREES(x) ((x)/M_PI*(double)ST_ANG)
#define RADIANS(x) (M_PI*(x)/(double)ST_ANG)
#define MULT 64
#define CIRCLE (NUM_DEGREES*MULT)
#define CIRCLE_2  (CIRCLE/2)
#define CIRCLE_4  (CIRCLE/4)

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?(-1):1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _MballLoc {
	int         wedge, direction;
} MballLoc;

typedef struct _MballPart {
	Pixel       foreground;
	Pixel       borderColor;
	Pixel       wedgeColor[MAXWEDGES];
	MballLoc   *mballLoc[MAXWEDGES];
	int         currentWedge, currentRing;
	Boolean     started, practice, orient, vertical, mono;
	int         depth;
	int         wedges, rings;
	Position    delta, dr;
	Position    mballLength;
	Position    wedgeLength;
	Position    viewLength, viewMiddle;
	XPoint      puzzleSize;
	XPoint      puzzleOffset, letterOffset;
	GC          puzzleGC;
	GC          borderGC;
	GC          wedgeGC[MAXWEDGES];
	GC          inverseGC;
	String      wedgeName[MAXWEDGES];
	char       *username;
	XtCallbackList select;
} MballPart;

typedef struct _MballRec {
	CorePart    core;
	MballPart   mball;
} MballRec;

/* This gets around C's inability to do inheritance */
typedef struct _MballClassPart {
	int         ignore;
} MballClassPart;

typedef struct _MballClassRec {
	CoreClassPart core_class;
	MballClassPart mball_class;
} MballClassRec;

extern MballClassRec mballClassRec;
extern MballLoc *startLoc[MAXWEDGES];

extern void MoveMball(MballWidget w, int wedge, int ring, int direction, int control);

											       /* extern void SolveWedges(); *//* For future auto-solver */
extern void DrawAllWedges(MballWidget w);
extern Boolean CheckSolved(MballWidget w);
extern void InitMoves(void);
extern void PutMove(int wedge, int ring, int direction, int control);
extern void GetMove(int *wedge, int *ring, int *direction, int *control);
extern int  MadeMoves(void);
extern void FlushMoves(MballWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, MballWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, MballWidget w);
extern void PrintStartPosition(FILE * fp, MballWidget w);
extern void SetStartPosition(MballWidget w);

#endif /* _MballP_h */
