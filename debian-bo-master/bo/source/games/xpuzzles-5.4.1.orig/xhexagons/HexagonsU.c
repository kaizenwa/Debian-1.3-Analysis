
/*-
# X-BASED HEXAGONS
#
#  HexagonsU.c
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
#include "HexagonsP.h"

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
static int  startSpace[SPACES];
static int  startRow[ROWTYPES];
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
FlushMoves(HexagonsWidget w)
{
	int         i;

	FlushStack();
	startSpace[LOW] = w->hexagons.spacePosition[LOW];
	startSpace[HIGH] = w->hexagons.spacePosition[HIGH];
	startRow[TRBL] = w->hexagons.spaceRow[TRBL];
	startRow[TLBR] = w->hexagons.spaceRow[TLBR];
	startRow[ROW] = w->hexagons.spaceRow[ROW];
	for (i = 0; i < w->hexagons.sizeSize; i++)
		startPosition[i] = w->hexagons.tileOfPosition[i];
}

int
NumMoves(void)
{
	return count;
}

void
ScanMoves(FILE * fp, HexagonsWidget w, int moves)
{
	int         direction, l;
	char        c;

	for (l = 0; l < moves; l++) {
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d", &direction);
		if (!MoveHexagonsDir(w, direction))
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
		ReadMove(&direction, currMove->move);
		(void) fprintf(fp, "%d%c\t%d\n", ++counter, SYMBOL, direction);
		currMove = currMove->next;
	}
}

void
ScanStartPosition(FILE * fp, HexagonsWidget w)
{
	int         i, num;
	char        c;

	while ((c = getc(fp)) != EOF && c != SYMBOL);
	for (i = 0; i < w->hexagons.sizeSize; i++) {
		(void) fscanf(fp, "%d ", &num);
		startPosition[i] = num;
		if (!num)
			startSpace[HIGH] = i;
		else if (num == -1)
			startSpace[LOW] = i;
	}
	if (w->hexagons.corners) {
		startRow[HIGH] = Row(w, startSpace[HIGH]);
		if (w->hexagons.size > 1)
			startRow[LOW] = Row(w, startSpace[LOW]);
	} else {
		startRow[ROW] = Row(w, startSpace[HIGH]);
		startRow[TRBL] = TrBl(w, startSpace[HIGH], startRow[ROW]);
		startRow[TLBR] = TlBr(w, startSpace[HIGH], startRow[ROW]);
	}
}

void
PrintStartPosition(FILE * fp, HexagonsWidget w)
{
	int         rowPos = 0, r = 0, rp, p, length = 0;

	(void) fprintf(fp, "\nstartingPosition%c\n", SYMBOL);
	for (p = 0; p < w->hexagons.sizeSize; p++) {
		if (rowPos == 0) {
			if (p < w->hexagons.sizeSize / 2) {	/* CENTER */
				length = w->hexagons.size + r;
				for (rp = 0; rp < w->hexagons.size - 1 - r; rp++)
					(void) fprintf(fp, "  ");
			} else {
				length = 3 * w->hexagons.size - r - 2;
				for (rp = 0; rp < r - w->hexagons.size + 1; rp++)
					(void) fprintf(fp, "  ");
			}
		}
		(void) fprintf(fp, "%3d ", startPosition[p]);
		if (rowPos == length - 1) {
			r++;
			rowPos = 0;
			(void) fprintf(fp, "\n");
		} else
			rowPos++;
	}
	(void) fprintf(fp, "\n");
}

void
SetStartPosition(HexagonsWidget w)
{
	int         i;

	w->hexagons.spacePosition[HIGH] = startSpace[HIGH];
	w->hexagons.spacePosition[LOW] = startSpace[LOW];
	w->hexagons.spaceRow[TRBL] = startRow[TRBL];
	w->hexagons.spaceRow[TLBR] = startRow[TLBR];
	w->hexagons.spaceRow[ROW] = startRow[ROW];
	for (i = 0; i < w->hexagons.sizeSize; i++)
		w->hexagons.tileOfPosition[i] = startPosition[i];
	DrawAllTiles(w, w->hexagons.tileGC, w->hexagons.borderGC);
}
