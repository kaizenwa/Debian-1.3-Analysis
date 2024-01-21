/*-
# X-BASED DINOSAUR CUBE
#
#  Dino3dP.h
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

/* Private header file for Dino3d */

#ifndef _Dino3dP_h
#define _Dino3dP_h

#include "Dino3d.h"

#define SAME 0
#define OPPOSITE 1
#define DOWN 0
#define UP 1
#define COORD3D 12
#define MAXVIEWS 2
#define MAXORIENT 4

#define SQRT_3 1.732050808
#define MULTIPLY(a) ((int)((double)a*SQRT_3/2.0))
#define DIVIDE(a) ((int)((double)a*2.0/SQRT_3))

typedef struct _Dino3DPart {
	Position    cubeDiagonal, cubeDiag, diamondDiagonal, faceDiagonal;
	XPoint      cubeSize, diamondSize, faceSize, viewSize;
	XPoint      viewMiddle;
} Dino3DPart;

typedef struct _Dino3DRec {
	CorePart    core;
	DinoPart    dino;
	Dino3DPart  dino3d;
} Dino3DRec;

/* This gets around C's inability to do inheritance */
typedef struct _Dino3DClassPart {
	int         ignore;
} Dino3DClassPart;

typedef struct _Dino3DClassRec {
	CoreClassPart coreClass;
	DinoClassPart dinoClass;
	Dino3DClassPart dino3dClass;
} Dino3DClassRec;

extern Dino3DClassRec dino3dClassRec;

extern int  SelectPolyhedrons3D(Dino3DWidget w, int x, int y, int *face, int *position);
extern int  NarrowSelection3D(Dino3DWidget w, int *face, int *direction);
extern void DrawTriangle3D(Dino3DWidget w, int face, int position, int offset);

#endif /* _Dino3dP_h */
