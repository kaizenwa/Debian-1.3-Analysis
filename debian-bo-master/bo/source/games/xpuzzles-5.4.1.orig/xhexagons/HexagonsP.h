/*-
# X-BASED HEXAGONS
#
#  HexagonP.h
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

/* Private header file for Hexagons */

#ifndef _HexagonsP_h
#define _HexagonsP_h

#include "Hexagons.h"

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
#define LOW 0
#define HIGH 1
#define SPACES 2

/* The following are in xhexagons.c also */
#define MINHEXAGONS 1
#define NOCORN 0
#define CORNERS 1
#define MAXORIENT 2

#define DEFAULTCORNERS CORNERS
#define DEFAULTHEXAGONS 3

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?(-1):1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _HexagonsPart {
	Pixel       foreground;
	Pixel       tileColor, borderColor;
	int        *tileOfPosition, spacePosition[SPACES];
	int         spaceRow[ROWTYPES];
	int         currentPosition, currentRow[ROWTYPES];
	Boolean     started, vertical, corners;
	int         base;
	int         size, sizeSize, sizeCenter;
	XPoint      offset, tileSize, puzzleSize;
	XPoint      delta, puzzleOffset, digitOffset;
	GC          puzzleGC;
	GC          tileGC;
	GC          borderGC;
	GC          inverseGC;
	char       *username;
	XtCallbackList select;
} HexagonsPart;

typedef struct _HexagonsRec {
	CorePart    core;
	HexagonsPart hexagons;
} HexagonsRec;

/* This gets around C's inability to do inheritance */
typedef struct _HexagonsClassPart {
	int         ignore;
} HexagonsClassPart;

typedef struct _HexagonsClassRec {
	CoreClassPart core_class;
	HexagonsClassPart hexagons_class;
} HexagonsClassRec;

extern HexagonsClassRec hexagonsClassRec;
extern int *startPosition;

extern int  MoveHexagonsDir(HexagonsWidget w, int direction);

											    /* extern void SolveTiles(); *//* For future auto-solver */
extern void DrawAllTiles(HexagonsWidget w, GC tileGC, GC borderGC);
extern Boolean CheckSolved(HexagonsWidget w);
extern void InitMoves(void);
extern void PutMove(int direction);
extern void GetMove(int *direction);
extern int  MadeMoves(void);
extern void FlushMoves(HexagonsWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, HexagonsWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, HexagonsWidget w);
extern void PrintStartPosition(FILE * fp, HexagonsWidget w);
extern void SetStartPosition(HexagonsWidget w);
extern int  Row(HexagonsWidget w, int pos);
extern int  TrBl(HexagonsWidget w, int pos, int posRow);
extern int  TlBr(HexagonsWidget w, int pos, int posRow);

#endif /* _HexagonsP_h */
