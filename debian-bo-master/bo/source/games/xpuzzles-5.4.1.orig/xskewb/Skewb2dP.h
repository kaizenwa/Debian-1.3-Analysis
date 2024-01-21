/*-
# X-BASED SKEWB
#
#  Skewb2dP.h
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

/* Private header file for Skewb2d */

#ifndef _Skewb2dP_h
#define _Skewb2dP_h

#include "Skewb2d.h"

#define FRONTLOC 0
#define BACKLOC 11
#define COORD2D 8
#define MAXX 3
#define MAXY 4
#define MAXXY 4			/* (MAX(MAXX,MAXY)) */
#define MAXRECT (MAXX*MAXY)

typedef struct _Skewb2DPart {
	Position    diamondLength;
	Position    faceLength;
	Position    viewLength;
} Skewb2DPart;

typedef struct _Skewb2DRec {
	CorePart    core;
	SkewbPart   skewb;
	Skewb2DPart skewb2d;
} Skewb2DRec;

/* This gets around C's inability to do inheritance */
typedef struct _Skewb2DClassPart {
	int         ignore;
} Skewb2DClassPart;

typedef struct _Skewb2DClassRec {
	CoreClassPart core_class;
	SkewbClassPart skewb_class;
	Skewb2DClassPart skewb2d_class;
} Skewb2DClassRec;

extern Skewb2DClassRec skewb2dClassRec;

extern int  SelectPolyhedrons2D(Skewb2DWidget w, int x, int y, int *face, int *position);
extern int  NarrowSelection2D(Skewb2DWidget w, int *face, int *position, int *direction);
extern void DrawDiamond2D(Skewb2DWidget w, int face);
extern void DrawTriangle2D(Skewb2DWidget w, int face, int position, int offset);

#endif /* _Skewb2dP_h */
