/*-
# X-BASED DINOSAUR CUBE
#
#  DinoP.h
#
###
#
#  Copyright (c) 1995 - 97	David Albert Bagley, bagleyd@bigfoot.com
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

/* Private header file for Dino */

#ifndef _DinoP_h
#define _DinoP_h

#include "Dino.h"

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
#define CORNER 0
#define MIDDLE 1
#define EDGE 2
#define FACE 3

/* The following are in xdino.c also */
#define PERIOD2 2
#define PERIOD3 3
#define BOTH 4
#define MAXMODES 3
#define MAXFACES 6

#define DEFAULTMODE PERIOD3

#define MIN(a,b) (((int)(a)<(int)(b))?(int)(a):(int)(b))
#define MAX(a,b) (((int)(a)>(int)(b))?(int)(a):(int)(b))

typedef struct _DinoLoc {
	int         face, side, dir;
} DinoLoc;

typedef struct _DinoCornerLoc {
	int         face, rotation;
} DinoCornerLoc;

typedef struct _DinoPart {
	Pixel       foreground;
	Pixel       borderColor;
	Pixel       faceColor[MAXFACES];
	DinoCornerLoc cubeLoc[MAXFACES][MAXORIENT];
	DinoCornerLoc faceLoc[MAXORIENT];
	DinoCornerLoc rowLoc[MAXORIENT][MAXORIENT];
	DinoCornerLoc spindleLoc[MAXROTATE][2];
	int         currentFace, currentPosition, currentDirection;
	Boolean     started, practice, orient, vertical, mono;
	int         dim;	/* This allows us to reuse code between 2d and 3d */
	int         mode, depth, style;
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
} DinoPart;

typedef struct _DinoRec {
	CorePart    core;
	DinoPart    dino;
} DinoRec;

/* This gets around C's inability to do inheritance */
typedef struct _DinoClassPart {
	int         ignore;
} DinoClassPart;

typedef struct _DinoClassRec {
	CoreClassPart core_class;
	DinoClassPart dino_class;
} DinoClassRec;

typedef struct _RowNext {
	int         face, direction, side_face;
} RowNext;

extern DinoClassRec dinoClassRec;

extern void QuitDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void PracticeDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void PracticeDinoMaybe(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void RandomizeDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void RandomizeDinoMaybe(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void GetDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void WriteDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void UndoDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void SolveDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void OrientizeDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void Period2ModeDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void Period3ModeDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void BothModeDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void MoveDinoInput(DinoWidget w, int x, int y, int direction, int shift, int control, int alt);
extern void MoveDino(DinoWidget w, int face, int position, int direction, int style, int control);
extern void SelectDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void ReleaseDino(DinoWidget w, XEvent * event, char **args, int nArgs);
extern void DrawAllPolyhedrons(DinoWidget w);
extern Boolean CheckSolved(DinoWidget w);
extern void InitMoves(void);
extern void PutMove(int face, int position, int direction, int style, int control);
extern void GetMove(int *face, int *position, int *direction, int *style, int *control);
extern int  MadeMoves(void);
extern void FlushMoves(DinoWidget w);
extern int  NumMoves(void);
extern void ScanMoves(FILE * fp, DinoWidget w, int moves);
extern void PrintMoves(FILE * fp);
extern void ScanStartPosition(FILE * fp, DinoWidget w);
extern void PrintStartPosition(FILE * fp, DinoWidget w);
extern void SetStartPosition(DinoWidget w);

#ifdef DEBUG
extern void PrintCube(DinoWidget w);

#endif

#endif /* _DinoP_h */
