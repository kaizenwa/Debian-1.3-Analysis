/*-
# X-BASED PANEX(tm)
#
#  PanexP.h
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

/* Private header file for Panex */

#ifndef _PanexP_h
#define _PanexP_h

#include "Panex.h"

#define SYMBOL ':'

/* The following are in xpanex.c also */
#define MINTILES 1
#define HANOI 0
#define PANEX 1
#define MAXMODES 2

#define DEFAULTMODE PANEX
#define DEFAULTTILES 10
#define MAXSTACKS 3
#define UP 1
#define DOWN 0

#define ABS(a) (((a)<0)?(-a):(a))
#define SIGN(a) (((a)<0)?-1:1)
#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _PanexLoc {
	int         stack, loc;
} PanexLoc;

typedef struct _PanexPart {
	Pixel       foreground;
	Pixel       stackColor, tileColor, borderColor;
	Pixel       pyramidColor[MAXSTACKS - 1];
	PanexLoc   *tileOfPosition[MAXSTACKS];
	PanexLoc   *positionOfTile[MAXSTACKS];
	Position    height;
	int         currentStack, currentPosition;
	Boolean     started, mono;
	int         depth;
	int         base;
	int         mode, tiles;
	int         delay;
	Position    width;
	XPoint      tileSize;
	XPoint      pos;
	XPoint      delta, puzzleOffset, letterOffset;
	GC          pyramidGC[MAXSTACKS - 1];
	GC          puzzleGC;
	GC          stackGC;
	GC          tileGC;
	GC          borderGC;
	GC          inverseGC;
	String      pyramidName[MAXSTACKS - 1];
	char       *username;
	XtCallbackList select;
} PanexPart;

typedef struct _PanexRec {
	CorePart    core;
	PanexPart   panex;
} PanexRec;

/* This gets around C's inability to do inheritance */
typedef struct _PanexClassPart {
	int         ignore;
} PanexClassPart;

typedef struct _PanexClassRec {
	CoreClassPart core_class;
	PanexClassPart panex_class;
} PanexClassRec;

extern PanexClassRec panexClassRec;
extern PanexLoc *startLoc[MAXSTACKS - 1];

extern int  MovePanex(PanexWidget w, int fromStack, int fromPosition, int toStack);
extern void SolveTiles(PanexWidget w);
extern void DrawAllTiles(PanexWidget w, GC tileGC, GC borderGC);
extern int  TopOfStack(PanexWidget w, int stack);
extern Boolean CheckMiddle(PanexWidget w);
extern Boolean CheckSolved(PanexWidget w);
extern void InitMoves(void);
extern void PutMove(int from, int to);
extern void GetMove(int *from, int *to);
extern int  MadeMoves(void);
extern void FlushMoves(PanexWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, PanexWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, PanexWidget w);
extern void PrintStartPosition(FILE * fp, PanexWidget w);
extern void SetStartPosition(PanexWidget w);

#endif /* _PanexP_h */
