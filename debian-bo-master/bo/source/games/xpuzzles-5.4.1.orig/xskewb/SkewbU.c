/*-
# X-BASED SKEWB
#
#  SkewbU.c
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

/* Undo algorithm */

#include <stdio.h>
#include <stdlib.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "SkewbP.h"
#include "Skewb2dP.h"
#include "Skewb3dP.h"

typedef struct _MoveRecord {
	/* int face, position, direction, control; */
	unsigned short int packed;	/* This makes assumptions on the data. */
} MoveRecord;

typedef struct _MoveStack {
	MoveRecord  move;
	struct _MoveStack *previous, *next;
} MoveStack;

static MoveStack *currMove, *lastMove, *firstMove;
static int  count;
static SkewbLoc startLoc[MAXFACES][MAXCUBES];

static void InitStack(void);
static void PushStack(MoveRecord move);
static void PopStack(MoveRecord * move);
static int  EmptyStack(void);
static void FlushStack(void);

static void
InitStack(void)
{
	if (!(lastMove = (MoveStack *) malloc(sizeof (MoveStack))))
		XtError("Not enough memory, exiting.");
	if (!(firstMove = (MoveStack *) malloc(sizeof (MoveStack))))
		XtError("Not enough memory, exiting.");
	firstMove->previous = lastMove->next = NULL;
	firstMove->next = lastMove;
	lastMove->previous = firstMove;
	count = 0;
}

static void
PushStack(MoveRecord move)
{
	if (!(currMove = (MoveStack *) malloc(sizeof (MoveStack))))
		XtError("Not enough memory, exiting.");
	lastMove->previous->next = currMove;
	currMove->previous = lastMove->previous;
	currMove->next = lastMove;
	lastMove->previous = currMove;
	currMove->move = move;
	count++;
}

static void
PopStack(MoveRecord * move)
{
	*move = lastMove->previous->move;
	currMove = lastMove->previous;
	lastMove->previous->previous->next = lastMove;
	lastMove->previous = lastMove->previous->previous;
	(void) free((void *) currMove);
	count--;
}

static int
EmptyStack(void)
{
	return (lastMove->previous == firstMove);
}

static void
FlushStack(void)
{
	while (lastMove->previous != firstMove) {
		currMove = lastMove->previous;
		lastMove->previous->previous->next = lastMove;
		lastMove->previous = lastMove->previous->previous;
		(void) free((void *) currMove);
	}
	count = 0;
}

/**********************************/

void
InitMoves(void)
{
	InitStack();
}

static void
WriteMove(MoveRecord * move, int face, int position, int direction, int control)
{
	/* move.face = face; move.position = position;
	   move.direction = direction; move.control = control; */
	move->packed = ((control & 0xF) << 12) + ((direction & 0xF) << 8) +
		((position & 0xF) << 4) + (face & 0xF);
}

static void
ReadMove(int *face, int *position, int *direction, int *control, MoveRecord move)
{
	/* *face = move->face; *position = move->position;
	   *direction = move.direction; *control = move.control; */
	*face = move.packed & 0xF;
	*position = (move.packed >> 4) & 0xF;
	*direction = (move.packed >> 8) & 0xF;
	*control = (move.packed >> 12) & 0xF;
}

void
PutMove(int face, int position, int direction, int control)
{
	MoveRecord  move;

	WriteMove(&move, face, position, direction, control);
	PushStack(move);
}

void
GetMove(int *face, int *position, int *direction, int *control)
{
	MoveRecord  move;

	PopStack(&move);
	ReadMove(face, position, direction, control, move);
}

int
MadeMoves(void)
{
	return !EmptyStack();
}

void
FlushMoves(SkewbWidget w)
{
	int         face, position;

	FlushStack();
	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXCUBES; position++) {
			startLoc[face][position].face = w->skewb.cubeLoc[face][position].face;
			startLoc[face][position].rotation =
				w->skewb.cubeLoc[face][position].rotation;
		}
}

int
NumMoves(void)
{
	return count;
}

void
ScanMoves(FILE * fp, SkewbWidget w, int moves)
{
	int         face, position, direction, control, k;
	char        c;

	for (k = 0; k < moves; k++) {
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d %d %d %d", &face, &position, &direction, &control);
		MoveSkewb(w, face, position, direction, control);
	}
}

void
PrintMoves(FILE * fp)
{
	int         face, position, direction, control, counter = 0;

	currMove = firstMove->next;
	(void) fprintf(fp, "moves\tface\tpos\tdir\tcon\n");
	while (currMove != lastMove) {
		ReadMove(&face, &position, &direction, &control, currMove->move);
		(void) fprintf(fp, "%d%c\t%d\t%d\t%d\t%d\n",
		      ++counter, SYMBOL, face, position, direction, control);
		currMove = currMove->next;
	}
}

void
ScanStartPosition(FILE * fp, SkewbWidget w)
{
	int         face, position, num;
	char        c;

	while ((c = getc(fp)) != EOF && c != SYMBOL);
	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXCUBES; position++) {
			(void) fscanf(fp, "%d ", &num);
			startLoc[face][position].face = num;
			if (w->skewb.orient) {
				(void) fscanf(fp, "%d ", &num);
				startLoc[face][position].rotation = num;
			}
		}
}

void
PrintStartPosition(FILE * fp, SkewbWidget w)
{
	int         face, position;

	(void) fprintf(fp, "\nstartingPosition%c\n", SYMBOL);
	for (face = 0; face < MAXFACES; face++) {
		for (position = 0; position < MAXCUBES; position++) {
			(void) fprintf(fp, "%d ", startLoc[face][position].face);
			if (w->skewb.orient)
				(void) fprintf(fp, "%d  ", startLoc[face][position].rotation);
		}
		(void) fprintf(fp, "\n");
	}
}

void
SetStartPosition(SkewbWidget w)
{
	int         face, position;

	for (face = 0; face < MAXFACES; face++)
		for (position = 0; position < MAXCUBES; position++) {
			w->skewb.cubeLoc[face][position].face = startLoc[face][position].face;
			if (w->skewb.orient)
				w->skewb.cubeLoc[face][position].rotation =
					startLoc[face][position].rotation;
		}
	DrawAllPolyhedrons(w);
}
