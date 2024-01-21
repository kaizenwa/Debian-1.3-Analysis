/*-
# X-BASED PANEX(tm)
#
#  PanexU.c
#
###
#
#  Copyright (c) 1996 - 97	David Albert Bagley, bagleyd@bigfoot.com
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
#include "PanexP.h"

typedef struct _MoveRecord {
	/* int from, to; */
	unsigned char packed;	/* This makes assumptions on the data. */
} MoveRecord;

typedef struct _MoveStack {
	MoveRecord  move;
	struct _MoveStack *previous, *next;
} MoveStack;

static MoveStack *currMove, *lastMove, *firstMove;
static int  count;
PanexLoc   *startLoc[MAXSTACKS - 1] =
{NULL, NULL};

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
WriteMove(MoveRecord * move, int from, int to)
{
	/* move->from = from; move->to = to; */
	move->packed = ((from & 0xF) << 4) + (to & 0xF);
}

static void
ReadMove(int *from, int *to, MoveRecord move)
{
	/* *from = move.from; *to = move.to; */
	*to = move.packed & 0xF;
	*from = (move.packed >> 4) & 0xF;
}

void
PutMove(int from, int to)
{
	MoveRecord  move;

	WriteMove(&move, from, to);
	PushStack(move);
}

void
GetMove(int *from, int *to)
{
	MoveRecord  move;

	PopStack(&move);
	ReadMove(from, to, move);
}

int
MadeMoves(void)
{
	return !EmptyStack();
}

void
FlushMoves(PanexWidget w)
{
	int         i, j;

	FlushStack();
	for (i = 0; i <= w->panex.mode; i++)
		for (j = 0; j < w->panex.tiles; j++)
			startLoc[i][j] = w->panex.positionOfTile[i][j];
}

int
NumMoves(void)
{
	return count;
}

void
ScanMoves(FILE * fp, PanexWidget w, int moves)
{
	int         fromStack, fromPosition, toStack, l;
	char        c;

	for (l = 0; l < moves; l++) {
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d %d", &fromStack, &toStack);
		if ((fromPosition = TopOfStack(w, fromStack)) < 0 ||
		    MovePanex(w, fromStack, fromPosition, toStack) < 0)
			(void) printf("%d move from %d to %d can not be made.",
				      l, fromStack, toStack);
	}
}

void
PrintMoves(FILE * fp)
{
	int         from, to, counter = 0;

	currMove = firstMove->next;
	(void) fprintf(fp, "moves\tfrom\tto\n");
	while (currMove != lastMove) {
		ReadMove(&from, &to, currMove->move);
		(void) fprintf(fp, "%d%c\t%d\t%d\n", ++counter, SYMBOL, from, to);
		currMove = currMove->next;
	}
}

void
ScanStartPosition(FILE * fp, PanexWidget w)
{
	int         i, j;
	char        c;

	while ((c = getc(fp)) != EOF && c != SYMBOL);
	for (i = 0; i <= w->panex.mode; i++)
		for (j = 0; j < w->panex.tiles; j++)
			(void) fscanf(fp, "%d %d",
			     &(startLoc[i][j].stack), &(startLoc[i][j].loc));
}

void
PrintStartPosition(FILE * fp, PanexWidget w)
{
	int         i, j;

	(void) fprintf(fp, "\nstartingPosition%c\n", SYMBOL);
	for (i = 0; i <= w->panex.mode; i++) {
		for (j = 0; j < w->panex.tiles; j++) {
			(void) fprintf(fp, "%3d %3d  ", startLoc[i][j].stack,
				       startLoc[i][j].loc);
		}
		(void) fprintf(fp, "\n");
	}
	(void) fprintf(fp, "\n");
}

void
SetStartPosition(PanexWidget w)
{
	int         i, j;

	for (i = 0; i < MAXSTACKS; i++)
		for (j = 0; j <= w->panex.tiles; j++)
			w->panex.tileOfPosition[i][j].stack = -1;
	for (i = 0; i <= w->panex.mode; i++)
		for (j = 0; j < w->panex.tiles; j++) {
			w->panex.positionOfTile[i][j] = startLoc[i][j];
			w->panex.tileOfPosition[startLoc[i][j].stack][startLoc[i][j].loc].stack =
				i;
			w->panex.tileOfPosition[startLoc[i][j].stack][startLoc[i][j].loc].loc =
				j;
		}
	DrawAllTiles(w, w->panex.tileGC, w->panex.borderGC);
}
