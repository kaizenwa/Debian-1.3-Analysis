/*-
# X-BASED CUBES
#
#  CubesU.c
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
#include "CubesP.h"

typedef struct _MoveRecord {
	/* int direction; */
	unsigned char packed;	/* This makes assumptions on the data. */
} MoveRecord;

typedef struct _MoveStack {
	MoveRecord  move;
	struct _MoveStack *previous, *next;
} MoveStack;

static MoveStack *currMove, *lastMove, *firstMove;
static int  count;
static int  startSpace;
int        *startPosition = NULL;

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
WriteMove(MoveRecord * move, int direction)
{
	/* move->direction = direction; */
	move->packed = direction & 0xF;
}

static void
ReadMove(int *direction, MoveRecord move)
{
	/* *direction = move.direction; */
	*direction = move.packed & 0xF;
}

void
PutMove(int direction)
{
	MoveRecord  move;

	WriteMove(&move, direction);
	PushStack(move);
}

void
GetMove(int *direction)
{
	MoveRecord  move;

	PopStack(&move);
	ReadMove(direction, move);
}

int
MadeMoves(void)
{
	return !EmptyStack();
}

void
FlushMoves(CubesWidget w)
{
	int         i;

	FlushStack();
	startSpace = w->cubes.spacePosition;
	startPosition[w->cubes.sizeBlock - 1] = 0;
	for (i = 0; i < w->cubes.sizeBlock; i++)
		startPosition[i] = w->cubes.brickOfPosition[i];
}

int
NumMoves(void)
{
	return count;
}

void
ScanMoves(FILE * fp, CubesWidget w, int moves)
{
	int         direction, l;
	char        c;

	for (l = 0; l < moves; l++) {
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &direction);
		if (!MoveCubesDir(w, direction))
			(void) fprintf(fp, "%d move in direction %d, can not be made.",
				       l, direction);
	}
}

void
PrintMoves(FILE * fp)
{
	int         direction, counter = 0;

	currMove = firstMove->next;
	(void) fprintf(fp, "moves\tdir\n");
	while (currMove != lastMove) {
		ReadMove(&direction, currMove->move);;
		(void) fprintf(fp, "%d%c\t%d\n", ++counter, SYMBOL, direction);
		currMove = currMove->next;
	}
}

void
ScanStartPosition(FILE * fp, CubesWidget w)
{
	int         i, num;
	char        c;

	while ((c = getc(fp)) != EOF && c != SYMBOL);
	for (i = 0; i < w->cubes.sizeBlock; i++) {
		(void) fscanf(fp, "%d ", &num);
		startPosition[i] = num;
		if (!num)
			startSpace = i;
	}
}

void
PrintStartPosition(FILE * fp, CubesWidget w)
{
	int         i;

	(void) fprintf(fp, "\nstartingPosition%c\n", SYMBOL);
	for (i = 0; i < w->cubes.sizeBlock; i++) {
		(void) fprintf(fp, "%3d ", startPosition[i]);
		if (!((i + 1) % w->cubes.sizeX))
			(void) fprintf(fp, "\n");
		if (!((i + 1) % w->cubes.sizeRect))
			(void) fprintf(fp, "\n");
	}
	(void) fprintf(fp, "\n");
}

void
SetStartPosition(CubesWidget w)
{
	int         i;

	w->cubes.spacePosition = startSpace;
	for (i = 0; i < w->cubes.sizeBlock; i++)
		w->cubes.brickOfPosition[i] = startPosition[i];
	DrawAllBricks(w, w->cubes.brickGC, w->cubes.borderGC);
}
