/*-
# X-BASED CUBES
#
#  CubesP.h
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

/* Private header file for Cubes */

#ifndef _CubesP_h
#define _CubesP_h

#include "Cubes.h"

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
#define RIGHT 1
#define BOTTOM 2
#define LEFT 3
#define IN 4
#define OUT 5
#define COORD 6

/* The following is in xcubes.c also */
#define MINCUBES 1

#define DEFAULTCUBES 3

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?(-1):1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _CubesPart {
	Pixel       foreground;
	Pixel       brickColor, borderColor;
	int        *brickOfPosition, spacePosition;
	int         currentPosition, currentRow[3];
	Boolean     started, vertical;
	int         base;
	int         sizeX, sizeY, sizeZ, sizeRect, sizeBlock;
	XPoint      offset;
	XPoint      brickSize, faceSize, puzzleSize;
	XPoint      delta, puzzleOffset, digitOffset;
	GC          puzzleGC;
	GC          brickGC;
	GC          borderGC;
	GC          inverseGC;
	char       *username;
	XtCallbackList select;
} CubesPart;

typedef struct _CubesRec {
	CorePart    core;
	CubesPart   cubes;
} CubesRec;

/* This gets around C's inability to do inheritance */
typedef struct _CubesClassPart {
	int         ignore;
} CubesClassPart;

typedef struct _CubesClassRec {
	CoreClassPart core_class;
	CubesClassPart cubes_class;
} CubesClassRec;

extern CubesClassRec cubesClassRec;
int        *startPosition;

extern int  MoveCubesDir(CubesWidget w, int direction);

															       /* extern void SolveBricks(); *//* For future auto-solver */
extern void DrawAllBricks(CubesWidget w, GC brickGC, GC borderGC);
extern Boolean CheckSolved(CubesWidget w);
extern void InitMoves(void);
extern void PutMove(int direction);
extern void GetMove(int *direction);
extern int  MadeMoves(void);
extern void FlushMoves(CubesWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, CubesWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, CubesWidget w);
extern void PrintStartPosition(FILE * fp, CubesWidget w);
extern void SetStartPosition(CubesWidget w);

#endif /* _CubesP_h */
