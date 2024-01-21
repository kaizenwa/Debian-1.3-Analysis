/*-
# X-BASED TRIANGLES
#
#  TrianglesU.c
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
#include "TrianglesP.h"

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
static int  startSpace[MAXORIENT];
static int  startRow[MAXORIENT][ROWTYPES];
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
FlushMoves(TrianglesWidget w)
{
	int         i;

	FlushStack();
	startSpace[UP] = w->triangles.spacePosition[UP];
	startSpace[DOWN] = w->triangles.spacePosition[DOWN];
	startRow[UP][TRBL] = w->triangles.spaceRow[UP][TRBL];
	startRow[UP][TLBR] = w->triangles.spaceRow[UP][TLBR];
	startRow[UP][ROW] = w->triangles.spaceRow[UP][ROW];
	startRow[DOWN][TRBL] = w->triangles.spaceRow[DOWN][TRBL];
	startRow[DOWN][TLBR] = w->triangles.spaceRow[DOWN][TLBR];
	startRow[DOWN][ROW] = w->triangles.spaceRow[DOWN][ROW];
	for (i = 0; i < w->triangles.sizeSize; i++)
		startPosition[i] = w->triangles.tileOfPosition[i];
}

int
NumMoves(void)
{
	return count;
}

void
ScanMoves(FILE * fp, TrianglesWidget w, int moves)
{
	int         direction, l;
	char        c;

	for (l = 0; l < moves; l++) {
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &direction);
		if (!MoveTrianglesDir(w, direction))
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
ScanStartPosition(FILE * fp, TrianglesWidget w)
{
	int         i, num;
	char        c;

	while ((c = getc(fp)) != EOF && c != SYMBOL);
	for (i = 0; i < w->triangles.sizeSize; i++) {
		(void) fscanf(fp, "%d ", &num);
		startPosition[i] = num;
		if (!num)
			startSpace[UP] = i;
		else if (num == -1)
			startSpace[DOWN] = i;
	}
	for (i = DOWN; i <= UP; i++) {
		startRow[i][ROW] = Row(startSpace[i]);
		startRow[i][TRBL] = TrBl(startSpace[i], startRow[i][ROW]) / 2;
		startRow[i][TLBR] = TlBr(startSpace[i], startRow[i][ROW]) / 2;
	}
}

void
PrintStartPosition(FILE * fp, TrianglesWidget w)
{
	int         r = 0, p, space, temp;

	(void) fprintf(fp, "\nstartingPosition%c\n", SYMBOL);
	for (p = 0; p < w->triangles.sizeSize; p++) {
		if (p == r * r)
			for (space = 0; space < w->triangles.size - r - 1; space++)
				(void) fprintf(fp, "    ");
		(void) fprintf(fp, "%3d ", startPosition[p]);
		temp = (r + 1) * (r + 1) - 1;
		if (p == temp) {
			(void) fprintf(fp, "\n");
			r++;
		}
	}
	(void) fprintf(fp, "\n");
}

void
SetStartPosition(TrianglesWidget w)
{
	int         i;

	w->triangles.spacePosition[UP] = startSpace[UP];
	w->triangles.spacePosition[DOWN] = startSpace[DOWN];
	w->triangles.spaceRow[UP][TRBL] = startRow[UP][TRBL];
	w->triangles.spaceRow[UP][TLBR] = startRow[UP][TLBR];
	w->triangles.spaceRow[UP][ROW] = startRow[UP][ROW];
	w->triangles.spaceRow[DOWN][TRBL] = startRow[DOWN][TRBL];
	w->triangles.spaceRow[DOWN][TLBR] = startRow[DOWN][TLBR];
	w->triangles.spaceRow[DOWN][ROW] = startRow[DOWN][ROW];
	for (i = 0; i < w->triangles.sizeSize; i++)
		w->triangles.tileOfPosition[i] = startPosition[i];
	DrawAllTiles(w, w->triangles.tileGC, w->triangles.borderGC);
}
