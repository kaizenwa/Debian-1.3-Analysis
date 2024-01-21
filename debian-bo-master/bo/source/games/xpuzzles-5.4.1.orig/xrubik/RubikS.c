
/*- 
# X-BASED RUBIK'S CUBE(tm)
#
#  RubikS.c
###
#
#  Taken from code originally written by
#  Michael B. Martin <martinm@sps1.phys.vt.edu>
#  From cubist10.c-- for IBM PC.
#  Used by permission.
#  Taken from the algorithm in the Ideal Solution book.
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

/* Solver file for Rubik */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "RubikP.h"
#include "Rubik2dP.h"
#include "Rubik3dP.h"

/* Mappings of Ideal notation to my General nxnxn cube notation */
#define RotateLeft(w)	MoveRubik(w,2,0,LEFT,TRUE)
#define RotateRight(w)	MoveRubik(w,2,0,RIGHT,TRUE)
#define RotateUp(w)	MoveRubik(w,2,0,TOP,TRUE)
#define RotateDown(w)	MoveRubik(w,2,0,BOTTOM,TRUE)
#define RotateCw(w)	MoveRubik(w,0,0,RIGHT,TRUE)
#define RotateCcw(w)	MoveRubik(w,0,0,LEFT,TRUE)

#define RotateTopLeft(w)	MoveRubik(w,2,0,LEFT,FALSE)
#define RotateCenterLeft(w)	MoveRubik(w,2,w->rubik.size,LEFT,FALSE)
#define RotateBottomLeft(w)	MoveRubik(w,2,w->rubik.size*(w->rubik.size-1),LEFT,FALSE)
#define RotateTopRight(w)	MoveRubik(w,2,0,RIGHT,FALSE)
#define RotateCenterRight(w)	MoveRubik(w,2,w->rubik.size,RIGHT,FALSE)
#define RotateBottomRight(w)	MoveRubik(w,2,w->rubik.size*(w->rubik.size-1),RIGHT,FALSE)
#define RotateLeftUp(w)		MoveRubik(w,2,0,TOP,FALSE)
#define RotateCenterUp(w)	MoveRubik(w,2,1,TOP,FALSE)
#define RotateRightUp(w)	MoveRubik(w,2,w->rubik.size-1,TOP,FALSE)
#define RotateLeftDown(w)	MoveRubik(w,2,0,BOTTOM,FALSE)
#define RotateCenterDown(w)	MoveRubik(w,2,1,BOTTOM,FALSE)
#define RotateRightDown(w)	MoveRubik(w,2,w->rubik.size-1,BOTTOM,FALSE)
#define RotateFrontCw(w)	MoveRubik(w,0,w->rubik.size*(w->rubik.size-1),RIGHT,FALSE)
#define RotateFrontCcw(w)	MoveRubik(w,0,w->rubik.size*(w->rubik.size-1),LEFT,FALSE)

#define BLUE 0
#define WHITE 1
#define RED 2
#define YELLOW 3
#define GREEN 4
#define ORANGE 5

#ifdef DEBUG
static int  mapGeneralToIdeal[MAXFACES] =
{5, 4, 1, 2, 6, 3};

#endif
static int  mapIdealToGeneral[MAXFACES] =
{2, 3, 5, 1, 0, 4};		/* Remember to subtract 1 */

static int  Side(RubikWidget w, int m, int c);
static int  FindCorner(RubikWidget w, int color1, int color2, int color3);
static int  FindEdge(RubikWidget w, int color1, int color2);
static void PlaceEdge(RubikWidget w, int edge, int top_color);
static void SolveTop(RubikWidget w);
static void AlignCorners(RubikWidget w);
static void SwapBottomCorners(RubikWidget w);
static void SolveBottom(RubikWidget w);
static void Solve_2_2(RubikWidget w);
static void SolveBottomEdges(RubikWidget w);
static void AlignLastEdge(RubikWidget w);
static void ColorAlignFront(RubikWidget w);
static void AlignEdges(RubikWidget w);
static void ShuffleEdges(RubikWidget w);
static void RecolorTopEdges(RubikWidget w);

static int
Side(RubikWidget w, int m, int c)
{
	int         d, i, j;

	d = mapIdealToGeneral[m - 1];
	i = c % 3;
	j = c / 3;
	if (i == 2)
		i = w->rubik.size - 1;
	if (j == 2)
		j = w->rubik.size - 1;
	return w->rubik.cubeLoc[d][j * w->rubik.size + i].face;
}

/* This procedure finds the location of a specified corner.  An */
/* improperly-specified color combination will result in a return */
/* value of 9. */
static int
FindCorner(RubikWidget w, int color1, int color2, int color3)
{
	int         corner = 0, temp = 1;

	do {
		if (Side(w, 5, 6) == color1) {
			if (Side(w, 1, 0) == color2)
				if (Side(w, 4, 2) == color3)
					corner = temp;
		} else if (Side(w, 5, 6) == color2) {
			if (Side(w, 1, 0) == color3)
				if (Side(w, 4, 2) == color1)
					corner = temp;
		} else if (Side(w, 5, 6) == color3) {
			if (Side(w, 1, 0) == color1)
				if (Side(w, 4, 2) == color2)
					corner = temp;
		}
		if (corner == 0) {
			if (temp < 4)
				RotateLeft(w);
			else if (temp == 4) {
				RotateCw(w);
				RotateCw(w);
			} else if (temp < 8)
				RotateRight(w);
			else if (temp == 8)
				corner = 9;
			temp++;
		}
	} while (corner == 0);

	/* put the cube back to the way it was */
	if (corner == 2)
		RotateRight(w);
	else if (corner == 3) {
		RotateRight(w);
		RotateRight(w);
	} else if (corner == 4)
		RotateLeft(w);
	else if (corner == 5) {
		RotateCw(w);
		RotateCw(w);
		RotateLeft(w);
	} else if (corner == 6) {
		RotateCw(w);
		RotateCw(w);
	} else if (corner == 7) {
		RotateCw(w);
		RotateCw(w);
		RotateRight(w);
	} else if (corner == 8) {
		RotateCw(w);
		RotateCw(w);
		RotateRight(w);
		RotateRight(w);
	}
	return (corner);
}

/* This procedure finds the location of a specified edge.  An */
/* improperly-specified color combination will result in a return */
/* value of 13. */
static int
FindEdge(RubikWidget w, int color1, int color2)
{
	int         edge = 0, temp = 1;

	do {
		if (Side(w, 5, 7) == color1) {
			if (Side(w, 1, 1) == color2)
				edge = temp;
		} else if (Side(w, 5, 7) == color2) {
			if (Side(w, 1, 1) == color1)
				edge = temp;
		}
		if (edge == 0) {
			if (temp < 4)
				RotateLeft(w);
			else if (temp == 4) {
				RotateLeft(w);
				RotateCw(w);
			} else if (temp < 8)
				RotateUp(w);
			else if (temp == 8) {
				RotateUp(w);
				RotateCw(w);
			} else if (temp < 12)
				RotateRight(w);
			else if (temp == 12)
				edge = 13;	/* end condition, just in case */
			temp++;
		}
	} while (edge == 0);

	/* put the cube back to the way it was */
	if (edge == 2) {
		RotateRight(w);
	} else if (edge == 3) {
		RotateRight(w);
		RotateRight(w);
	} else if (edge == 4) {
		RotateLeft(w);
	} else if (edge == 5) {
		RotateCcw(w);
	} else if (edge == 6) {
		RotateCcw(w);
		RotateRight(w);
	} else if (edge == 7) {
		RotateCcw(w);
		RotateRight(w);
		RotateRight(w);
	} else if (edge == 8) {
		RotateCcw(w);
		RotateLeft(w);
	} else if (edge == 9) {
		RotateCcw(w);
		RotateCcw(w);
	} else if (edge == 10) {
		RotateCcw(w);
		RotateCcw(w);
		RotateRight(w);
	} else if (edge == 11) {
		RotateUp(w);
		RotateUp(w);
	} else if (edge == 12) {
		RotateUp(w);
		RotateUp(w);
		RotateRight(w);
	}
	return (edge);
}

/* This procedure places the specified edge piece in edge position */
/* #1 (front top). */
static void
PlaceEdge(RubikWidget w, int edge, int top_color)
{
	/* first put the edge piece in position #8 (center row, left rear) */
	if (edge == 1) {
		if (Side(w, 5, 7) == top_color)
			return;	/* already in place */
		else {
			RotateFrontCcw(w);
			RotateCenterLeft(w);
			RotateFrontCw(w);
		}
	} else if (edge == 2) {
		RotateTopLeft(w);
		RotateFrontCcw(w);
		RotateCenterLeft(w);
		RotateFrontCw(w);
		RotateTopRight(w);
	} else if (edge == 3) {
		RotateTopLeft(w);
		RotateTopLeft(w);
		RotateFrontCcw(w);
		RotateCenterLeft(w);
		RotateFrontCw(w);
		RotateTopRight(w);
		RotateTopRight(w);
	} else if (edge == 4) {
		RotateTopRight(w);
		RotateFrontCcw(w);
		RotateCenterLeft(w);
		RotateFrontCw(w);
		RotateTopLeft(w);
	} else if (edge == 5) {
		RotateCenterLeft(w);
	} else if (edge == 6) {
		RotateCenterLeft(w);
		RotateCenterLeft(w);
	} else if (edge == 7) {
		RotateCenterRight(w);
	} else if (edge == 9) {
		RotateFrontCw(w);
		RotateCenterLeft(w);
		RotateFrontCcw(w);
	} else if (edge == 10) {
		RotateBottomLeft(w);
		RotateFrontCw(w);
		RotateCenterLeft(w);
		RotateFrontCcw(w);
		RotateBottomRight(w);
	} else if (edge == 11) {
		RotateBottomLeft(w);
		RotateBottomLeft(w);
		RotateFrontCw(w);
		RotateCenterLeft(w);
		RotateFrontCcw(w);
		RotateBottomRight(w);
		RotateBottomRight(w);
	} else if (edge == 12) {
		RotateBottomRight(w);
		RotateFrontCw(w);
		RotateCenterLeft(w);
		RotateFrontCcw(w);
		RotateBottomLeft(w);
	}
	/* put the piece in place */
	if (Side(w, 4, 3) == top_color) {
		RotateFrontCw(w);
		RotateCenterRight(w);
		RotateCenterRight(w);
		RotateFrontCcw(w);
	} else {
		RotateFrontCcw(w);
		RotateCenterRight(w);
		RotateFrontCw(w);
	}
}

/* This procedure solves the top (BLUE) side, except for one edge. */
static void
SolveTop(RubikWidget w)
{
	int         corner, edge;

	/* put the blue face on the top */
	if (Side(w, 1, 4) == BLUE)
		RotateUp(w);
	else if (Side(w, 2, 4) == BLUE)
		RotateCcw(w);
	else if (Side(w, 3, 4) == BLUE)
		RotateDown(w);
	else if (Side(w, 4, 4) == BLUE)
		RotateCw(w);
	else if (Side(w, 6, 4) == BLUE) {
		RotateUp(w);
		RotateUp(w);
	}
	/* first find the blue-red-white corner and place it */
	corner = FindCorner(w, BLUE, RED, WHITE);
	if (corner == 1) {
		if (Side(w, 5, 6) == RED) {
			RotateFrontCw(w);
			RotateTopLeft(w);
		} else if (Side(w, 5, 6) == WHITE) {
			RotateLeftUp(w);
			RotateTopRight(w);
		}
	} else if (corner == 2) {
		if (Side(w, 5, 8) == BLUE)
			RotateTopLeft(w);
		else if (Side(w, 5, 8) == RED) {
			RotateRightUp(w);
			RotateTopLeft(w);
			RotateTopLeft(w);
		} else if (Side(w, 5, 8) == WHITE)
			RotateFrontCcw(w);
	} else if (corner == 3) {
		if (Side(w, 5, 2) == BLUE) {
			RotateTopLeft(w);
			RotateTopLeft(w);
		} else if (Side(w, 5, 2) == RED) {
			RotateTopRight(w);
			RotateLeftDown(w);
		} else if (Side(w, 5, 2) == WHITE) {
			RotateRightDown(w);
			RotateTopLeft(w);
		}
	} else if (corner == 4) {
		if (Side(w, 5, 0) == BLUE)
			RotateTopRight(w);
		else if (Side(w, 5, 0) == RED)
			RotateLeftDown(w);
		else if (Side(w, 5, 0) == WHITE) {
			RotateTopRight(w);
			RotateLeftUp(w);
			RotateTopRight(w);
		}
	} else if (corner == 5) {
		if (Side(w, 6, 0) == BLUE) {
			RotateBottomLeft(w);
			RotateLeftUp(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 0) == RED)
			RotateLeftUp(w);
		else if (Side(w, 6, 0) == WHITE)
			RotateFrontCw(w);
	} else if (corner == 6) {
		if (Side(w, 6, 2) == BLUE) {
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 2) == RED) {
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 2) == WHITE) {
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 7) {
		if (Side(w, 6, 8) == BLUE) {
			RotateBottomLeft(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 8) == RED) {
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 8) == WHITE) {
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 8) {
		if (Side(w, 6, 6) == BLUE) {
			RotateLeftUp(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 6) == RED) {
			RotateBottomRight(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 6) == WHITE) {
			RotateBottomRight(w);
			RotateFrontCw(w);
		}
	}
	/* now find the blue-yellow-red corner and place it */
	RotateLeft(w);
	corner = FindCorner(w, BLUE, YELLOW, RED);
	if (corner == 1) {
		if (Side(w, 5, 6) == YELLOW) {
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCcw(w);
			RotateFrontCcw(w);
		} else if (Side(w, 5, 6) == RED) {
			RotateFrontCw(w);
			RotateFrontCw(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 2) {
		if (Side(w, 5, 8) == BLUE) {
			RotateRightDown(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		} else if (Side(w, 5, 8) == YELLOW) {
			RotateRightDown(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 5, 8) == RED)
			RotateFrontCcw(w);
	} else if (corner == 3) {
		if (Side(w, 5, 2) == BLUE) {
			RotateRightDown(w);
			RotateRightDown(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 5, 2) == YELLOW) {
			RotateRightDown(w);
			RotateFrontCcw(w);
		} else if (Side(w, 5, 2) == RED) {
			RotateRightDown(w);
			RotateRightDown(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 5) {
		if (Side(w, 6, 0) == BLUE) {
			RotateBottomRight(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 0) == YELLOW) {
			RotateBottomRight(w);
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 0) == RED)
			RotateFrontCw(w);
	} else if (corner == 6) {
		if (Side(w, 6, 2) == BLUE) {
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 2) == YELLOW) {
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 2) == RED) {
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 7) {
		if (Side(w, 6, 8) == BLUE) {
			RotateBottomLeft(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 8) == YELLOW) {
			RotateBottomLeft(w);
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 8) == RED) {
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 8) {
		if (Side(w, 6, 6) == BLUE) {
			RotateBottomRight(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 6) == YELLOW) {
			RotateBottomRight(w);
			RotateBottomRight(w);
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 6) == RED) {
			RotateBottomRight(w);
			RotateFrontCw(w);
		}
	}
	/* now find the blue-orange-yellow corner and place it */
	RotateLeft(w);
	corner = FindCorner(w, BLUE, ORANGE, YELLOW);
	if (corner == 1) {
		if (Side(w, 5, 6) == ORANGE) {
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCcw(w);
			RotateFrontCcw(w);
		} else if (Side(w, 5, 6) == YELLOW) {
			RotateFrontCw(w);
			RotateFrontCw(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 2) {
		if (Side(w, 5, 8) == BLUE) {
			RotateRightDown(w);
			RotateBottomLeft(w);
			RotateRightUp(w);
			RotateFrontCw(w);
		} else if (Side(w, 5, 8) == ORANGE) {
			RotateFrontCw(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		} else if (Side(w, 5, 8) == YELLOW)
			RotateFrontCcw(w);
	} else if (corner == 5) {
		if (Side(w, 6, 0) == BLUE) {
			RotateBottomRight(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 0) == ORANGE) {
			RotateRightDown(w);
			RotateBottomRight(w);
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 0) == YELLOW)
			RotateFrontCw(w);
	} else if (corner == 6) {
		if (Side(w, 6, 2) == BLUE) {
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 2) == ORANGE) {
			RotateBottomLeft(w);
			RotateRightDown(w);
			RotateBottomRight(w);
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 2) == YELLOW) {
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 7) {
		if (Side(w, 6, 8) == BLUE) {
			RotateBottomLeft(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 8) == ORANGE) {
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateRightDown(w);
			RotateBottomRight(w);
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 8) == YELLOW) {
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 8) {
		if (Side(w, 6, 6) == BLUE) {
			RotateBottomRight(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 6) == ORANGE) {
			RotateRightDown(w);
			RotateBottomRight(w);
			RotateBottomRight(w);
			RotateRightUp(w);
			RotateFrontCcw(w);
		} else if (Side(w, 6, 6) == YELLOW) {
			RotateBottomRight(w);
			RotateFrontCw(w);
		}
	}
	/* and now find the blue-white-orange corner and place it */
	RotateLeft(w);
	corner = FindCorner(w, BLUE, WHITE, ORANGE);
	if (corner == 1) {
		if (Side(w, 5, 6) == WHITE) {
			RotateLeftDown(w);
			RotateBottomRight(w);
			RotateLeftUp(w);
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
		} else if (Side(w, 5, 6) == ORANGE) {
			RotateFrontCcw(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
			RotateBottomRight(w);
			RotateBottomRight(w);
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		}
	} else if (corner == 5) {
		if (Side(w, 6, 0) == BLUE) {
			RotateBottomRight(w);
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
			RotateBottomRight(w);
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 0) == WHITE) {
			RotateBottomRight(w);
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 0) == ORANGE) {
			RotateBottomLeft(w);
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
		}
	} else if (corner == 6) {
		if (Side(w, 6, 2) == BLUE) {
			RotateBottomRight(w);
			RotateFrontCcw(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
			RotateBottomLeft(w);
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 2) == WHITE) {
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 2) == ORANGE) {
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
		}
	} else if (corner == 7) {
		if (Side(w, 6, 8) == BLUE) {
			RotateLeftDown(w);
			RotateBottomRight(w);
			RotateLeftUp(w);
			RotateBottomRight(w);
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 8) == WHITE) {
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 8) == ORANGE) {
			RotateFrontCcw(w);
			RotateBottomLeft(w);
			RotateBottomLeft(w);
			RotateFrontCw(w);
		}
	} else if (corner == 8) {
		if (Side(w, 6, 6) == BLUE) {
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
			RotateBottomLeft(w);
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
		} else if (Side(w, 6, 6) == WHITE) {
			RotateBottomRight(w);
			RotateBottomRight(w);
			RotateLeftDown(w);
			RotateBottomLeft(w);
			RotateLeftUp(w);
		} else if (Side(w, 6, 6) == ORANGE) {
			RotateFrontCcw(w);
			RotateBottomRight(w);
			RotateFrontCw(w);
		}
	}
	RotateLeft(w);

	/* find the blue-red edge and place it */
	edge = FindEdge(w, BLUE, RED);
	PlaceEdge(w, edge, BLUE);
	RotateLeft(w);

	/* find the blue-yellow edge and place it */
	edge = FindEdge(w, BLUE, YELLOW);
	PlaceEdge(w, edge, BLUE);
	RotateLeft(w);

	/* find the blue-orange edge and place it */
	edge = FindEdge(w, BLUE, ORANGE);
	PlaceEdge(w, edge, BLUE);
	RotateLeft(w);

	RotateUp(w);		/* put the blue side to the back */
}

/* This procedure places the front (GREEN) four corners into */
/* their correct positions. */
static void
AlignCorners(RubikWidget w)
{
	int         corner;

	/* find and place the green-orange-white corner (upper left) */
	corner = FindCorner(w, GREEN, ORANGE, WHITE);
	if (corner == 2)
		RotateFrontCcw(w);
	else if (corner == 5)
		RotateFrontCw(w);
	else if (corner == 6) {
		RotateFrontCw(w);
		RotateFrontCw(w);
	}
	/* find and place the green-yellow-orange corner (lower left) */
	corner = FindCorner(w, GREEN, YELLOW, ORANGE);
	if (corner == 2) {
		RotateCw(w);
		SwapBottomCorners(w);
		RotateCcw(w);
		SwapBottomCorners(w);
	} else if (corner == 6)
		SwapBottomCorners(w);

	/* find and place the green-red-yellow corner (lower right) */
	corner = FindCorner(w, GREEN, RED, YELLOW);
	if (corner == 2) {
		RotateCw(w);
		SwapBottomCorners(w);
		RotateCcw(w);
	}
}

/* This procedure swaps the the bottom front corners. */
static void
SwapBottomCorners(RubikWidget w)
{
	RotateTopRight(w);
	RotateFrontCw(w);
	RotateTopLeft(w);
	RotateLeftUp(w);
	RotateTopLeft(w);
	RotateLeftDown(w);
	RotateTopRight(w);
	RotateFrontCw(w);
	RotateFrontCw(w);
}

/* This procedure completes the GREEN side by color-aligning the GREEN */
/* corners and putting the GREEN edges in place. */
static void
SolveBottom(RubikWidget w)
{
	int         aligned;

	/* the GREEN corners (currently in the front) should now be in their */
	/* proper locations; next, we color align them, and then move the */
	/* bottom edges into place */
	do {
		aligned = 0;
		if (Side(w, 1, 0) == GREEN)
			aligned++;
		if (Side(w, 1, 2) == GREEN)
			aligned++;
		if (Side(w, 1, 6) == GREEN)
			aligned++;
		if (Side(w, 1, 8) == GREEN)
			aligned++;

		if (aligned == 0) {
			ColorAlignFront(w);
		} else if (aligned == 1) {
			/* place aligned corner in upper right */
			if (Side(w, 1, 0) == GREEN)
				RotateFrontCw(w);
			if (Side(w, 1, 6) == GREEN) {
				RotateFrontCw(w);
				RotateFrontCw(w);
			}
			if (Side(w, 1, 8) == GREEN)
				RotateFrontCcw(w);
			ColorAlignFront(w);
		} else if (aligned == 2) {
			if (Side(w, 1, 0) != GREEN)
				RotateFrontCw(w);
			else if (Side(w, 1, 2) == GREEN)
				RotateFrontCw(w);
			if (Side(w, 1, 0) != GREEN)
				RotateFrontCw(w);
			ColorAlignFront(w);
		} else if (aligned == 3)	/* not sure if this is possible */
			ColorAlignFront(w);
	} while (aligned < 4);
}

static void
Solve_2_2(RubikWidget w)
{
	int         i;

	for (i = 0; i < 3; i++)	/* This is not always efficient */
		if (!CheckSolved(w))
			RotateFrontCw(w);
}

static void
SolveBottomEdges(RubikWidget w)
{
	int         edge, color;

	/* next we move the bottom edges into place */
	RotateDown(w);		/* put the green face on top */
	RotateCw(w);
	RotateCw(w);

	color = Side(w, 1, 0);	/* get upper front corner color */
	edge = FindEdge(w, GREEN, color);
	PlaceEdge(w, edge, GREEN);
	RotateTopRight(w);

	color = Side(w, 1, 0);	/* get upper front corner color */
	edge = FindEdge(w, GREEN, color);
	PlaceEdge(w, edge, GREEN);
	RotateTopRight(w);

	color = Side(w, 1, 0);	/* get upper front corner color */
	edge = FindEdge(w, GREEN, color);
	PlaceEdge(w, edge, GREEN);
	RotateTopRight(w);

	color = Side(w, 1, 0);	/* get upper front corner color */
	edge = FindEdge(w, GREEN, color);
	PlaceEdge(w, edge, GREEN);
	RotateTopRight(w);

	/* now, align the fourth blue edge piece (if necessary) */
	RotateCw(w);
	RotateCw(w);
	edge = FindEdge(w, BLUE, WHITE);
	if (edge == 1) {
		if (Side(w, 1, 1) == BLUE) {
			RotateFrontCcw(w);
			RotateCenterLeft(w);
			RotateFrontCw(w);
			RotateCenterLeft(w);
			RotateFrontCw(w);
			RotateCenterLeft(w);
			RotateFrontCcw(w);
		}
	} else {
		if (edge == 5)
			RotateCenterRight(w);
		else if (edge == 7)
			RotateCenterLeft(w);
		else if (edge == 8) {
			RotateCenterRight(w);
			RotateCenterRight(w);
		}
		AlignLastEdge(w);
	}
}

/* This procedure moves the remaining BLUE edge piece into position */
/* from edge position #6. */
static void
AlignLastEdge(RubikWidget w)
{
	/* edge piece should be in edge position #6 */
	/* check its orientation and decide which sequence to perform */
	if (Side(w, 1, 5) == BLUE) {
		RotateCenterLeft(w);
		RotateFrontCw(w);
		RotateCenterRight(w);
		RotateFrontCcw(w);
		RotateCenterLeft(w);
		RotateFrontCcw(w);
		RotateCenterRight(w);
		RotateFrontCw(w);
	} else {
		RotateFrontCcw(w);
		RotateCenterLeft(w);
		RotateFrontCw(w);
		RotateCenterRight(w);
		RotateFrontCw(w);
		RotateCenterLeft(w);
		RotateFrontCcw(w);
	}
}

/* This procedure aligns the bottom corner colors (may need to be repeated). */
static void
ColorAlignFront(RubikWidget w)
{
	RotateTopRight(w);
	RotateFrontCw(w);
	RotateFrontCw(w);
	RotateTopLeft(w);
	RotateFrontCw(w);
	RotateTopRight(w);
	RotateFrontCw(w);
	RotateTopLeft(w);
	RotateFrontCw(w);
	RotateFrontCw(w);
}

/* This procedure completes the solution process by placing the */
/* remaining edges in place and alignment. */
static void
AlignEdges(RubikWidget w)
{
	int         aligned, color, edge;

	/* move the red side to the front */
	if (Side(w, 1, 4) == YELLOW)
		RotateRight(w);
	else if (Side(w, 1, 4) == ORANGE) {
		RotateRight(w);
		RotateRight(w);
	} else if (Side(w, 1, 4) == WHITE)
		RotateLeft(w);

	/* rotate the top until its aligned with the center colors */
	edge = FindEdge(w, BLUE, RED);
	if (edge == 2)
		RotateTopLeft(w);
	else if (edge == 3) {
		RotateTopLeft(w);
		RotateTopLeft(w);
	} else if (edge == 4)
		RotateTopRight(w);

	/* rotate the bottom until its aligned with the center colors */
	edge = FindEdge(w, GREEN, RED);
	if (edge == 10)
		RotateBottomLeft(w);
	else if (edge == 11) {
		RotateBottomLeft(w);
		RotateBottomLeft(w);
	} else if (edge == 12)
		RotateBottomRight(w);

	if (CheckSolved(w))
		return;

	RotateCcw(w);		/* place unaligned edges vertically */

	/* see if any edges are in correct position */
	aligned = 0;
	edge = FindEdge(w, RED, YELLOW);
	if (edge == 1)
		aligned++;
	edge = FindEdge(w, YELLOW, ORANGE);
	if (edge == 3)
		aligned++;
	edge = FindEdge(w, ORANGE, WHITE);
	if (edge == 11)
		aligned++;
	edge = FindEdge(w, WHITE, RED);
	if (edge == 9)
		aligned++;

	if (aligned == 0) {
		ShuffleEdges(w);	/* put one edge into position */
		aligned++;
	}
	if (aligned == 1) {
		/* find the correct piece and move it to the back bottom edge */
		edge = FindEdge(w, RED, YELLOW);
		if (edge == 1) {
			RotateDown(w);
			RotateDown(w);
		} else {
			edge = FindEdge(w, YELLOW, ORANGE);
			if (edge == 3)
				RotateUp(w);
			else {
				edge = FindEdge(w, WHITE, RED);
				if (edge == 9)
					RotateDown(w);
			}
		}

		/* shuffle */
		color = Side(w, 1, 4);
		if (Side(w, 1, 7) == color) {
			RotateRight(w);
			RotateRight(w);
			RotateDown(w);
			ShuffleEdges(w);
		} else if (Side(w, 6, 1) == color) {
			RotateRight(w);
			RotateRight(w);
			RotateDown(w);
			ShuffleEdges(w);
		} else
			ShuffleEdges(w);
	}
	/* pieces should be in place; complete color alignment */
	/* find number of unaligned edge pieces and fix them */
	aligned = 0;
	if (Side(w, 1, 1) == Side(w, 1, 4))
		aligned++;
	if (Side(w, 1, 7) == Side(w, 1, 4))
		aligned++;
	if (Side(w, 3, 1) == Side(w, 3, 4))
		aligned++;
	if (Side(w, 3, 7) == Side(w, 3, 4))
		aligned++;
	if (aligned == 0) {
		RecolorTopEdges(w);
		RotateDown(w);
		RotateDown(w);
		RecolorTopEdges(w);
	} else if (aligned == 2) {
		if (Side(w, 1, 1) == Side(w, 1, 4))
			do {
				RotateDown(w);
			} while (Side(w, 1, 1) == Side(w, 1, 4));
		if (Side(w, 1, 7) != Side(w, 1, 4))
			RotateUp(w);
		RecolorTopEdges(w);
		if (!CheckSolved(w)) {
			RotateDown(w);
			RecolorTopEdges(w);
		}
	}
}

/* This procedure "shuffles" the three center edges on the front and */
/* top faces. */
static void
ShuffleEdges(RubikWidget w)
{
	RotateCenterUp(w);
	RotateTopRight(w);
	RotateTopRight(w);
	RotateCenterDown(w);
	RotateTopRight(w);
	RotateTopRight(w);
}

/* This procedure should be used when the two opposing top front and back */
/* edge pieces are in position but not color aligned (this sequence is */
/* known as Rubik's Maneuver). */
static void
RecolorTopEdges(RubikWidget w)
{
	RotateCenterUp(w);
	RotateTopRight(w);
	RotateCenterUp(w);
	RotateTopRight(w);
	RotateCenterUp(w);
	RotateTopRight(w);
	RotateTopRight(w);
	RotateCenterDown(w);
	RotateTopRight(w);
	RotateCenterDown(w);
	RotateTopRight(w);
	RotateCenterDown(w);
	RotateTopRight(w);
	RotateTopRight(w);
}

/* This procedure coordinates the solution process. */
void
SolvePolyhedrons(RubikWidget w)
{
	rubikCallbackStruct cb;

	cb.reason = RUBIK_RESET;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
	if (!CheckSolved(w)) {
		SolveTop(w);
		AlignCorners(w);
		SolveBottom(w);
		if (w->rubik.size > 2) {
			SolveBottomEdges(w);
			AlignEdges(w);
		} else if (w->rubik.size == 2)
			Solve_2_2(w);
	}
	cb.reason = RUBIK_COMPUTED;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}
