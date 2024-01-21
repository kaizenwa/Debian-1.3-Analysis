/*-
# X-BASED SKEWB
#
#  SkewbP.h
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

/* Private header file for Skewb */

#ifndef _SkewbP_h
#define _SkewbP_h

#include "Skewb.h"

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
#define TR 0
#define BR 1
#define BL 2
#define TL 3
#define STRT 4
#define CW 5
#define HALF 6
#define CCW 7
#define TOP 8
#define RIGHT 9
#define BOTTOM 10
#define LEFT 11
#define MAXORIENT 4
#define MAXROTATE 3
#define MAXCUBES (MAXORIENT+1)
#define MINOR 0
#define MAJOR 1

/* The following is in xskewb.c also */
#define MAXFACES 6

#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _SkewbLoc {
	int         face, rotation;
} SkewbLoc;

typedef struct _SkewbLocPos {
	int         face, position, direction;
} SkewbLocPos;

typedef struct _SkewbPart {
	Pixel       foreground;
	Pixel       borderColor;
	Pixel       faceColor[MAXFACES];
	SkewbLoc    cubeLoc[MAXFACES][MAXCUBES];
	SkewbLoc    faceLoc[MAXCUBES];
	SkewbLoc    rowLoc[MAXORIENT][MAXCUBES];
	SkewbLoc    minorLoc[MAXORIENT], majorLoc[MAXORIENT][MAXORIENT];
	int         currentFace, currentPosition, currentDirection;
	Boolean     started, practice, orient, vertical, mono;
	int         dim;	/* This allows us to reuse code between 2d and 3d */
	int         depth;
	Position    delta;
	Position    orientLineLength;
	XPoint      puzzleSize;
	XPoint      puzzleOffset, letterOffset;
	GC          puzzleGC;
	GC          borderGC;
	GC          faceGC[MAXFACES];
	GC          inverseGC;
	String      faceName[MAXFACES];
	char       *username;
	XtCallbackList select;
	SkewbLoc    skewbLoc[MAXFACES][MAXCUBES];
} SkewbPart;

typedef struct _SkewbRec {
	CorePart    core;
	SkewbPart   skewb;
} SkewbRec;

/* This gets around C's inability to do inheritance */
typedef struct _SkewbClassPart {
	int         ignore;
} SkewbClassPart;

typedef struct _SkewbClassRec {
	CoreClassPart core_class;
	SkewbClassPart skewb_class;
} SkewbClassRec;

typedef struct _RowNext {
	int         face, direction, sideFace;
} RowNext;

extern SkewbClassRec skewbClassRec;

extern void QuitSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void PracticeSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void PracticeSkewbMaybe(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void RandomizeSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void RandomizeSkewbMaybe(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void GetSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void WriteSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void UndoSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void SolveSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void OrientizeSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void MoveSkewbCw(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void MoveSkewbCcw(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void MoveSkewbInput(SkewbWidget w, int x, int y, int direction, int control, int alt);
extern void MoveSkewb(SkewbWidget w, int face, int position, int direction, int control);
extern void SelectSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);
extern void ReleaseSkewb(SkewbWidget w, XEvent * event, char **args, int nArgs);

/* extern void SolvePolyhedrons(); */
extern void DrawAllPolyhedrons(SkewbWidget w);
extern Boolean CheckSolved(SkewbWidget w);
extern void InitMoves(void);
extern void PutMove(int face, int position, int direction, int control);
extern void GetMove(int *face, int *position, int *direction, int *control);
extern int  MadeMoves(void);
extern void FlushMoves(SkewbWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, SkewbWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, SkewbWidget w);
extern void PrintStartPosition(FILE * fp, SkewbWidget w);
extern void SetStartPosition(SkewbWidget w);

#ifdef DEBUG
extern void PrintCube(SkewbWidget w);

#endif

#endif /* _SkewbP_h */
