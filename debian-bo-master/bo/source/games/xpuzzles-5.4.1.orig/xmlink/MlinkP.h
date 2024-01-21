/*-
# X-BASED MISSING LINK(tm)
#
#  MlinkP.h
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

/* Private header file for Mlink */

#ifndef _MlinkP_h
#define _MlinkP_h

#include "Mlink.h"

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
#define COORD 4

/* The following are in xmlink.c also */
#define MINFACES 1
#define MAXFACES 8
#define MINTILES 1

#define DEFAULTTILES 4
#define DEFAULTFACES 4

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?(-1):1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _MlinkPart {
	Pixel       foreground;
	Pixel       tileColor, borderColor;
	Pixel       faceColor[MAXFACES];
	int        *tileOfPosition, spacePosition;
	int         currentTile, currentFace, currentRef;
	Boolean     started, orient, mono, middle;
	int         depth;
	int         base;
	int         tiles, faces, tileFaces;
	XPoint      offset, tileSize, faceSize, puzzleSize;
	XPoint      delta, puzzleOffset, digitOffset;
	GC          puzzleGC;
	GC          tileGC;
	GC          borderGC;
	GC          faceGC[MAXFACES];
	GC          inverseGC;
	String      faceName[MAXFACES];
	char       *username;
	XtCallbackList select;
} MlinkPart;

typedef struct _MlinkRec {
	CorePart    core;
	MlinkPart   mlink;
} MlinkRec;

/* This gets around C's inability to do inheritance */
typedef struct _MlinkClassPart {
	int         ignore;
} MlinkClassPart;

typedef struct _MlinkClassRec {
	CoreClassPart core_class;
	MlinkClassPart mlink_class;
} MlinkClassRec;

extern MlinkClassRec mlinkClassRec;
extern int *startPosition;

extern int  MoveMlink(MlinkWidget w, int direction, int tile, int shift);

															   /* extern void SolveTiles(); *//* For future auto-solver */
extern void DrawAllTiles(MlinkWidget w, GC tileGC, GC borderGC);
extern Boolean CheckSolved(MlinkWidget w);
extern void InitMoves(void);
extern void PutMove(int direction, int tile, int control);
extern void GetMove(int *direction, int *tile, int *control);
extern int  MadeMoves(void);
extern void FlushMoves(MlinkWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, MlinkWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, MlinkWidget w);
extern void PrintStartPosition(FILE * fp, MlinkWidget w);
extern void SetStartPosition(MlinkWidget w);

#endif /* _MlinkP_h */
