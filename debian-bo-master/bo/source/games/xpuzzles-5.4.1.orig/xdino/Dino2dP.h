/*-
# X-BASED DINOSAUR CUBE
#
#  Dino2dP.h
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

/* Private header file for Dino2d */

#ifndef _Dino2dP_h
#define _Dino2dP_h

#include "Dino2d.h"

#define FRONTLOC 0
#define BACKLOC 11
#define COORD2D 8
#define MAXX 3
#define MAXY 4
#define MAXXY 4			/* (MAX(MAXX,MAXY)) */
#define MAXRECT (MAXX*MAXY)

typedef struct _Dino2DPart {
	Position    diamondLength, triangleLength, triangleWidth;
	Position    faceLength;
	Position    viewLength;
} Dino2DPart;

typedef struct _Dino2DRec {
	CorePart    core;
	DinoPart    dino;
	Dino2DPart  dino2d;
} Dino2DRec;

/* This gets around C's inability to do inheritance */
typedef struct _Dino2DClassPart {
	int         ignore;
} Dino2DClassPart;

typedef struct _Dino2DClassRec {
	CoreClassPart coreClass;
	DinoClassPart dinoClass;
	Dino2DClassPart dino2dClass;
} Dino2DClassRec;

extern Dino2DClassRec dino2dClassRec;

extern int  SelectPolyhedrons2D(Dino2DWidget w, int x, int y, int *face, int *position);
extern int  NarrowSelection2D(Dino2DWidget w, int *face, int *direction);
extern void DrawTriangle2D(Dino2DWidget w, int face, int position, int offset);

#endif /* _Dino2dP_h */
