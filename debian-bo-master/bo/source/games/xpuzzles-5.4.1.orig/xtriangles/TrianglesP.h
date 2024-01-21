/*-
# X-BASED TRIANGLES
#
#  TriangleP.h
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

/* Private header file for Triangles */

#ifndef _TrianglesP_h
#define _TrianglesP_h

#include "Triangles.h"

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

#define TR 0
#define RIGHT 1
#define BR 2
#define BL 3
#define LEFT 4
#define TL 5
#define COORD 6
#define TRBL 0
#define TLBR 1
#define ROW 2
#define ROWTYPES 3
#define DOWN 0
#define UP 1
#define MAXORIENT 2

/* The following is in xtriangles.c also */
#define MINTRIANGLES 1

#define DEFAULTTRIANGLES 4

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?(-1):1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _TrianglesPart {
	Pixel       foreground;
	Pixel       tileColor, borderColor;
	int        *tileOfPosition, spacePosition[MAXORIENT];
	int         spaceRow[MAXORIENT][ROWTYPES];
	int         currentPosition, currentRow[ROWTYPES], currentPositionOrient;
	Boolean     started, vertical;
	int         base;
	int         size, sizeSize;
	XPoint      offset, tileSize, puzzleSize;
	XPoint      delta, puzzleOffset, digitOffset;
	GC          puzzleGC;
	GC          tileGC;
	GC          borderGC;
	GC          inverseGC;
	char       *username;
	XtCallbackList select;
} TrianglesPart;

typedef struct _TrianglesRec {
	CorePart    core;
	TrianglesPart triangles;
} TrianglesRec;

/* This gets around C's inability to do inheritance */
typedef struct _TrianglesClassPart {
	int         ignore;
} TrianglesClassPart;

typedef struct _TrianglesClassRec {
	CoreClassPart core_class;
	TrianglesClassPart triangles_class;
} TrianglesClassRec;

extern TrianglesClassRec trianglesClassRec;
extern int *startPosition;

extern int  MoveTrianglesDir(TrianglesWidget w, int direction);

															   /* extern void SolveTiles(); *//* For future auto-solver */
extern void DrawAllTiles(TrianglesWidget w, GC tileGC, GC borderGC);
extern Boolean CheckSolved(TrianglesWidget w);
extern void InitMoves(void);
extern void PutMove(int direction);
extern void GetMove(int *direction);
extern int  MadeMoves(void);
extern void FlushMoves(TrianglesWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, TrianglesWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, TrianglesWidget w);
extern void PrintStartPosition(FILE * fp, TrianglesWidget w);
extern void SetStartPosition(TrianglesWidget w);
extern int  Row(int pos);
extern int  TrBl(int pos, int posRow);
extern int  TlBr(int pos, int posRow);

#endif /* _TrianglesP_h */
