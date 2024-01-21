/*-
# X-BASED RUBIK'S CUBE(tm)
#
#  Rubik3dP.h
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

/* Private header file for Rubik3d */

#ifndef _Rubik3dP_h
#define _Rubik3dP_h

#include "Rubik3d.h"

#define TR 8
#define BR 9
#define BL 10
#define TL 11
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

typedef struct _Rubik3DPart {
	Position    cubeDiagonal, cubeDiag, faceDiagonal;
	XPoint      cubeSize, faceSize, viewSize;
	XPoint      viewMiddle;
} Rubik3DPart;

typedef struct _Rubik3DRec {
	CorePart    core;
	RubikPart   rubik;
	Rubik3DPart rubik3d;
} Rubik3DRec;

/* This gets around C's inability to do inheritance */
typedef struct _Rubik3DClassPart {
	int         ignore;
} Rubik3DClassPart;

typedef struct _Rubik3DClassRec {
	CoreClassPart core_class;
	RubikClassPart rubik_class;
	Rubik3DClassPart rubik3d_class;
} Rubik3DClassRec;

extern Rubik3DClassRec rubik3dClassRec;

extern int  SelectPolyhedrons3D(Rubik3DWidget w, int x, int y, int *face, int *position);
extern int  NarrowSelection3D(Rubik3DWidget w, int *face, int *position, int *direction);
extern void DrawSquare3D(Rubik3DWidget w, int face, int position, int offset);

#endif /* _Rubik3dP_h */
