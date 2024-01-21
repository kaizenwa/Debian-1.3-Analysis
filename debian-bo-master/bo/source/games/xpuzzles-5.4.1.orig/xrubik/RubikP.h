/*-
# X-BASED RUBIK'S CUBE(tm)
#
#  RubikP.h
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

/* Private header file for Rubik */

#ifndef _RubikP_h
#define _RubikP_h

#include "Rubik.h"

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
#define RIGHT 1
#define BOTTOM 2
#define LEFT 3
#define STRT 4
#define CW 5
#define HALF 6
#define CCW 7
#define MAXORIENT 4

/* The following are in xrubik.c also */
#define MAXFACES 6
#define MINCUBES 1

#define DEFAULTCUBES 3

#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _RubikLoc {
	int         face, rotation;
} RubikLoc;

typedef struct _RubikPart {
	Pixel       foreground;
	Pixel       borderColor;
	Pixel       faceColor[MAXFACES];
	RubikLoc   *cubeLoc[MAXFACES];
	RubikLoc   *faceLoc;
	RubikLoc   *rowLoc[MAXORIENT];
	int         currentFace, currentPosition, currentDirection;
	Boolean     started, practice, orient, vertical, mono;
	int         dim;	/* This allows us to reuse code between 2d and 3d */
	int         depth;
	int         size, sizeSize;
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
} RubikPart;

typedef struct _RubikRec {
	CorePart    core;
	RubikPart   rubik;
} RubikRec;

/* This gets around C's inability to do inheritance */
typedef struct _RubikClassPart {
	int         ignore;
} RubikClassPart;

typedef struct _RubikClassRec {
	CoreClassPart core_class;
	RubikClassPart rubik_class;
} RubikClassRec;

typedef struct _RowNext {
	int         face, direction, sideFace;
} RowNext;

extern RubikClassRec rubikClassRec;
extern RubikLoc *startLoc[MAXFACES];

extern void QuitRubik(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void PracticeRubik(RubikWidget w, XEvent * event, char **args,
			  int nArgs);
extern void PracticeRubikMaybe(RubikWidget w, XEvent * event, char **args,
			       int nArgs);
extern void RandomizeRubik(RubikWidget w, XEvent * event, char **args,
			   int nArgs);
extern void RandomizeRubikMaybe(RubikWidget w, XEvent * event, char **args,
				int nArgs);
extern void GetRubik(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void WriteRubik(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void UndoRubik(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void SolveRubik(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void IncrementRubik(RubikWidget w, XEvent * event, char **args,
			   int nArgs);
extern void DecrementRubik(RubikWidget w, XEvent * event, char **args,
			   int nArgs);
extern void OrientizeRubik(RubikWidget w, XEvent * event, char **args,
			   int nArgs);
extern void MoveRubikCw(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void MoveRubikCcw(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void MoveRubikInput(RubikWidget w,
			   int x, int y, int direction, int control);
extern void MoveRubik(RubikWidget w,
		      int face, int position, int direction, int control);
extern void SelectRubik(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void ReleaseRubik(RubikWidget w, XEvent * event, char **args, int nArgs);
extern void ResetPolyhedrons(RubikWidget w);
extern void SolvePolyhedrons(RubikWidget w);
extern void DrawAllPolyhedrons(RubikWidget w);
extern Boolean CheckSolved(RubikWidget w);
extern void InitMoves(void);
extern void PutMove(int face, int position, int direction, int control);
extern void GetMove(int *face, int *position, int *direction, int *control);
extern int  MadeMoves(void);
extern void FlushMoves(RubikWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, RubikWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, RubikWidget w);
extern void PrintStartPosition(FILE * fp, RubikWidget w);
extern void SetStartPosition(RubikWidget w);

#ifdef DEBUG
extern void PrintCube(RubikWidget w);
extern void PrintFace(RubikWidget w);
extern void PrintRow(RubikWidget w, int orient);

#endif

#endif /* _RubikP_h */
