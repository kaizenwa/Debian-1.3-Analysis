/*-
# X-BASED MASTERBALL(tm)
#
#  MballU.c
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
#include "MballP.h"

typedef struct _MoveRecord {
	/* int wedge, direction, control; */
	unsigned short int packed;	/* This makes assumptions on the data. */
	int         ring;	/* Do not make assumptions on this one. */
} MoveRecord;

typedef struct _MoveStack {
	MoveRecord  move;
	struct _MoveStack *previous, *next;
} MoveStack;

static MoveStack *currMove, *lastMove, *firstMove;
static int  count;
MballLoc   *startLoc[MAXWEDGES] =
{
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

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
WriteMove(MoveRecord * move, int wedge, int ring, int direction, int control)
{
	/* move->wedge = wedge; move->direction = direction;
	   move->control = control; */
	move->packed = ((control & 0xF) << 8) + ((direction & 0xF) << 4) +
		(wedge & 0xF);
	move->ring = ring;
}

static void
ReadMove(int *wedge, int *ring, int *direction, int *control, MoveRecord move)
{
	/* *wedge = move.wedge; *direction = move.direction;
	   *control = move.control; */
	*wedge = move.packed & 0xF;
	*direction = (move.packed >> 4) & 0xF;
	*control = (move.packed >> 8) & 0xF;
	*ring = move.ring;
}

void
PutMove(int wedge, int ring, int direction, int control)
{
	MoveRecord  move;

	WriteMove(&move, wedge, ring, direction, control);
	PushStack(move);
}

void
GetMove(int *wedge, int *ring, int *direction, int *control)
{
	MoveRecord  move;

	PopStack(&move);
	ReadMove(wedge, ring, direction, control, move);
}

int
MadeMoves(void)
{
	return !EmptyStack();
}

void
FlushMoves(MballWidget w)
{
	int         wedge, ring;

	FlushStack();
	for (wedge = 0; wedge < w->mball.wedges; wedge++)
		for (ring = 0; ring < w->mball.rings; ring++) {
			startLoc[wedge][ring].wedge =
				w->mball.mballLoc[wedge][ring].wedge;
			startLoc[wedge][ring].direction =
				w->mball.mballLoc[wedge][ring].direction;
		}
}

int
NumMoves(void)
{
	return count;
}

void
ScanMoves(FILE * fp, MballWidget w, int moves)
{
	int         wedge, ring, direction, control, l;
	char        c;

	for (l = 0; l < moves; l++) {
		while ((c = getc(fp)) != EOF && c != SYMBOL);
		(void) fscanf(fp, "%d %d %d %d", &wedge, &ring, &direction, &control);
		MoveMball(w, wedge, ring, direction, control);
	}
}

void
PrintMoves(FILE * fp)
{
	int         wedge, ring, direction, control, counter = 0;

	currMove = firstMove->next;
	(void) fprintf(fp, "moves\twedge\tring\tdir\tcon\n");
	while (currMove != lastMove) {
		ReadMove(&wedge, &ring, &direction, &control, currMove->move);
		(void) fprintf(fp, "%d%c\t%d\t%d\t%d\t%d\n",
			 ++counter, SYMBOL, wedge, ring, direction, control);
		currMove = currMove->next;
	}
}

void
ScanStartPosition(FILE * fp, MballWidget w)
{
	int         wedge, ring, num;
	char        c;

	while ((c = getc(fp)) != EOF && c != SYMBOL);
	for (wedge = 0; wedge < w->mball.wedges; wedge++)
		for (ring = 0; ring < w->mball.rings; ring++) {
			(void) fscanf(fp, "%d ", &num);
			startLoc[wedge][ring].wedge = num;
			if (w->mball.orient) {
				(void) fscanf(fp, "%d ", &num);
				startLoc[wedge][ring].direction = num;
			}
		}
}

void
PrintStartPosition(FILE * fp, MballWidget w)
{
	int         wedge, ring;

	(void) fprintf(fp, "\nstartingPosition%c\n", SYMBOL);
	for (wedge = 0; wedge < w->mball.wedges; wedge++) {
		for (ring = 0; ring < w->mball.rings; ring++) {
			(void) fprintf(fp, "%d ", startLoc[wedge][ring].wedge);
			if (w->mball.orient)
				(void) fprintf(fp, "%d  ", startLoc[wedge][ring].direction);
		}
		(void) fprintf(fp, "\n");
	}
}

void
SetStartPosition(MballWidget w)
{
	int         wedge, ring;

	for (wedge = 0; wedge < w->mball.wedges; wedge++)
		for (ring = 0; ring < w->mball.rings; ring++) {
			w->mball.mballLoc[wedge][ring].wedge =
				startLoc[wedge][ring].wedge;
			if (w->mball.orient)
				w->mball.mballLoc[wedge][ring].direction =
					startLoc[wedge][ring].direction;
		}
	DrawAllWedges(w);
}
