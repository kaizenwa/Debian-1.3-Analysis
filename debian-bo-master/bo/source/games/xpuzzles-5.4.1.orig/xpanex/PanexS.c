/*-
# X-BASED PANEX(tm)
#
#  PanexS.c
#
###
#
#  Taken from code originally written by
#  Rene Jansen <RENE.R.J.JANSEN@RCC.nl>
#  From his Pascal code.
#  Used by permission.
#  Algorithm used is from an issue in Quantum January/February 1996.
#  Some minor improvements discovered by Rene Jansen have been included.
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

/* Solver file for Panex */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "PanexP.h"

static      Boolean
MoveATile(PanexWidget w, int fromStack, int toStack)
{
	int         fromPosition;

	if ((fromPosition = TopOfStack(w, fromStack)) < 0 ||
	    MovePanex(w, fromStack, fromPosition, toStack) < 0) {
		(void) printf("move from %d to %d can not be made.",
			      fromStack, toStack);
		return False;
	}
	return True;
}

static void
SolveHanoiMode(PanexWidget w)
{
	int         triangleTurn = TRUE;
	int         triangleDirection = (w->panex.tiles & 1) ? 2 : 1;
	int         triangleStack = 0;
	int         fromStack, toStack, temp1Stack, temp2Stack, temp1Loc,
	            temp2Loc;
	panexCallbackStruct cb;

	while (!CheckSolved(w)) {
		if (triangleTurn) {
			fromStack = triangleStack;
			toStack = (triangleStack + triangleDirection) % MAXSTACKS;
			triangleStack = toStack;
			triangleTurn = FALSE;
		} else {
			temp1Stack = (triangleStack + 1) % MAXSTACKS;
			temp2Stack = (triangleStack + 2) % MAXSTACKS;
			temp1Loc = TopOfStack(w, temp1Stack);
			temp2Loc = TopOfStack(w, temp2Stack);
			if (temp1Loc < 0 && temp2Loc < 0)
				return;
			else if (temp1Loc < 0) {
				fromStack = temp2Stack;
				toStack = temp1Stack;
			} else if (temp2Loc < 0) {
				fromStack = temp1Stack;
				toStack = temp2Stack;
			} else if (w->panex.tileOfPosition[temp1Stack][temp1Loc].loc <
			 w->panex.tileOfPosition[temp2Stack][temp2Loc].loc) {
				fromStack = temp1Stack;
				toStack = temp2Stack;
			} else {
				fromStack = temp2Stack;
				toStack = temp1Stack;
			}
			triangleTurn = TRUE;
		}
		if (!MoveATile(w, fromStack, toStack))
			return;
	}
	cb.reason = PANEX_COMPUTED;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

#define FIRST_LAST 9

static void
move_it(PanexWidget w, int from, int dest)
{
	/*-
	 * A move is not immediately written to the output, but stored in two
	 * buffer variables (from1 and dest1). This is to deal with the situation
	 * that the last move of a part of the algorithm is played with the same
	 * piece as the first move of the next part. The two moves will then be
	 * combined to one.
	 */
	static int  from1 = FIRST_LAST, dest1 = FIRST_LAST;

	if (from == dest1) {
		dest1 = dest;
		return;
	}
	if (from1 < FIRST_LAST) {
		(void) MoveATile(w, from1, dest1);
	}
	from1 = from;
	dest1 = dest;
}

#define QUANTUM_IMPROVED

#if 0
/* MOVES[1..10] = {3, 13, 45, 136, 365, 932, 2309, 5648, 13717, 33212} */
#define QUANTUM_IMPROVED	/* Quantum Improved Algorithm */

/* MOVES[1..10] = {3, 13, 46, 137, 366, 933, 2310, 5649, 13718, 33213} */
#define QUANTUM			/* Quantum Algorithm */

/* MOVES[1..10] = {3, 13, 47, 141, 379, 965, 2391, 5845, 14195, 34365} */
#endif

/*-
 * level 10 is known to be 27564<=N<=31537
 */

#if defined( QUANTUM ) || defined( QUANTUM_IMPROVED )
/* Quantum Algorithm, coded by Rene Jansen */

static void
px(PanexWidget w, char func, int inv, int n)
{
	/*-
	 * This procedure can perform two recursive procedures D and S. The
	 * definition of D refers to S and vice versa. This is not allowed in
	 * Pascal.  So therefore D and S had to be combined to a single procedure px.
	 * Px will perform D or S, depending on 'func'.
	 * If inv=1 the inverse of D or S will be performed.
	 */
	if (func == 'D' && inv == 0) {
		if (n == 2) {
			move_it(w, 0, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			return;
		}
		if (n == 3) {
			move_it(w, 0, 2);
			move_it(w, 0, 1);
			move_it(w, 0, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			return;
		}
		if (n == 4) {
			move_it(w, 0, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 0);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			move_it(w, 0, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 0);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 0);
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			return;
		}
		if (n <= 4)
			return;
		px(w, 'D', 0, n - 1);
		px(w, 'S', 0, n - 2);
		move_it(w, 1, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		move_it(w, 2, 1);
		px(w, 'S', 1, n - 2);
		move_it(w, 1, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		move_it(w, 2, 1);
		return;
	}
	if (func == 'D' && inv == 1) {
		if (n == 2) {
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 0);
			return;
		}
		if (n == 3) {
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 0);
			move_it(w, 1, 0);
			move_it(w, 2, 0);
			return;
		}
		if (n == 4) {
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			move_it(w, 0, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 0, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 0);
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 0, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 0);
			return;
		}
		if (n <= 4)
			return;
		move_it(w, 1, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 0, 1);
		px(w, 'S', 0, n - 2);
		move_it(w, 1, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 0, 1);
		px(w, 'S', 1, n - 2);
		px(w, 'D', 1, n - 1);
		return;
	}
	if (func == 'S' && inv == 0) {
		if (n == 1) {
			move_it(w, 0, 1);
			return;
		}
		if (n == 2) {
			move_it(w, 0, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 0);
			return;
		}
		if (n != 3) {
			px(w, 'D', 0, n);
			px(w, 'S', 1, n - 1);
			return;
		}
		move_it(w, 0, 2);
		move_it(w, 0, 1);
		move_it(w, 0, 1);
		move_it(w, 2, 0);
		move_it(w, 1, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		move_it(w, 0, 1);
		move_it(w, 2, 0);
		move_it(w, 1, 0);
		return;
	}
	if (func != 'S' || inv != 1)
		return;

	if (n == 1) {
		move_it(w, 1, 0);
		return;
	}
	if (n == 2) {
		move_it(w, 0, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 0);
		return;
	}
	if (n != 3) {
		px(w, 'S', 0, n - 1);
		px(w, 'D', 1, n);
		return;
	}
	move_it(w, 0, 1);
	move_it(w, 0, 2);
	move_it(w, 1, 0);
	move_it(w, 1, 0);
	move_it(w, 2, 1);
	move_it(w, 0, 1);
	move_it(w, 0, 2);
	move_it(w, 1, 0);
	move_it(w, 1, 0);
	move_it(w, 2, 0);
}

static void
mpx(PanexWidget w, char func, int inv, int n)
{
	/*mpx is the mirror image of px
	 */
	if (func == 'D' && inv == 0) {
		if (n == 2) {
			move_it(w, 2, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			return;
		}
		if (n == 3) {
			move_it(w, 2, 0);
			move_it(w, 2, 1);
			move_it(w, 2, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			return;
		}
		if (n == 4) {
			move_it(w, 2, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 2);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			move_it(w, 2, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 2);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 2);
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			return;
		}
		if (n <= 4)
			return;
		mpx(w, 'D', 0, n - 1);
		mpx(w, 'S', 0, n - 2);
		move_it(w, 1, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 0, 1);
		mpx(w, 'S', 1, n - 2);
		move_it(w, 1, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 0, 1);
		return;
	}
	if (func == 'D' && inv == 1) {
		if (n == 2) {
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 2);
			return;
		}
		if (n == 3) {
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 2);
			move_it(w, 1, 2);
			move_it(w, 0, 2);
			return;
		}
		if (n == 4) {
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			move_it(w, 2, 1);
			move_it(w, 2, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 2, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 2);
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 1);
			move_it(w, 2, 1);
			move_it(w, 0, 2);
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 2);
			return;
		}
		if (n <= 4)
			return;
		move_it(w, 1, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		move_it(w, 2, 1);
		mpx(w, 'S', 0, n - 2);
		move_it(w, 1, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		move_it(w, 2, 1);
		mpx(w, 'S', 1, n - 2);
		mpx(w, 'D', 1, n - 1);
		return;
	}
	if (func == 'S' && inv == 0) {
		if (n == 1) {
			move_it(w, 2, 1);
			return;
		}
		if (n == 2) {
			move_it(w, 2, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 2);
			return;
		}
		if (n != 3) {
			mpx(w, 'D', 0, n);
			mpx(w, 'S', 1, n - 1);
			return;
		}
		move_it(w, 2, 0);
		move_it(w, 2, 1);
		move_it(w, 2, 1);
		move_it(w, 0, 2);
		move_it(w, 1, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 2, 1);
		move_it(w, 0, 2);
		move_it(w, 1, 2);
		return;
	}
	if (func != 'S' || inv != 1)
		return;

	if (n == 1) {
		move_it(w, 1, 2);
		return;
	}
	if (n == 2) {
		move_it(w, 2, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 2);
		return;
	}
	if (n != 3) {
		mpx(w, 'S', 0, n - 1);
		mpx(w, 'D', 1, n);
		return;
	}
	move_it(w, 2, 1);
	move_it(w, 2, 0);
	move_it(w, 1, 2);
	move_it(w, 1, 2);
	move_it(w, 0, 1);
	move_it(w, 2, 1);
	move_it(w, 2, 0);
	move_it(w, 1, 2);
	move_it(w, 1, 2);
	move_it(w, 0, 2);
}

static void
mL(PanexWidget w, int n)
{
	if (n == 1) {
		move_it(w, 2, 0);
		return;
	}
	if (n == 2) {
		move_it(w, 2, 0);
		move_it(w, 2, 1);
	} else {
		mpx(w, 'D', 0, n);
		mL(w, n - 2);
	}
}

/*-
 * Basic procedures D, S and L have now been defined.
 * It's time to construct the algorithm.
 */

static void
B(PanexWidget w, int n)
{
	if (n <= 2)
		return;
	mpx(w, 'S', 1, n - 1);
	px(w, 'S', 0, n - 1);
	move_it(w, 1, 0);
	move_it(w, 1, 2);
	move_it(w, 0, 1);
	move_it(w, 2, 1);
	px(w, 'S', 1, n - 1);
}

static void
qa(PanexWidget w, int level)
{
	int         i;

	if (level == 1) {
		move_it(w, 2, 1);
		move_it(w, 0, 2);
		move_it(w, 1, 0);
		return;
	}
	if (level == 2) {
		move_it(w, 2, 0);
		move_it(w, 2, 1);
		move_it(w, 0, 1);
		move_it(w, 0, 2);
		move_it(w, 0, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 2, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 2);
		move_it(w, 0, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 0);
		return;
	}
	mL(w, level);
	move_it(w, 0, 1);
#ifdef QUANTUM_IMPROVED
	/* now the improved sequence for B and B2-->B3 */
	move_it(w, 0, 2);
	move_it(w, 0, 2);
	move_it(w, 1, 0);
	move_it(w, 2, 1);

	move_it(w, 2, 0);
	move_it(w, 1, 2);
	move_it(w, 0, 2);
	move_it(w, 0, 1);

	move_it(w, 0, 2);
	move_it(w, 1, 0);
	move_it(w, 1, 0);
	move_it(w, 2, 1);

	move_it(w, 0, 2);
	move_it(w, 0, 1);
	move_it(w, 2, 0);
	move_it(w, 1, 0);
#else /* QUANTUM */
	move_it(w, 0, 2);
	move_it(w, 1, 2);
	move_it(w, 0, 1);
	move_it(w, 2, 0);
#endif

#ifdef QUANTUM_IMPROVED
	for (i = 4; i <= level; i++)
#else /* QUANTUM */
	for (i = 3; i <= level; i++)
#endif
		B(w, i);
	mpx(w, 'S', 1, level);
	px(w, 'S', 1, level);
}

static void
SolvePanexMode(PanexWidget w)
{
	panexCallbackStruct cb;

	if (w->panex.tiles > 0) {
		qa(w, w->panex.tiles);
		move_it(w, FIRST_LAST, FIRST_LAST);
	}
	cb.reason = PANEX_COMPUTED;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}

#else
/* Rene's Original Algorithm, coded by Rene Jansen */

static void
px(PanexWidget w, char func, int inv, int n)
{
	/*-
	 * The algorithm is based on two recursive procedures f and g. However,
	 * the definition of f refers to g and vice versa. This is not allowed in
	 * Pascal.  So therefore f and g had to be combined to a single procedure px.
	 * Px will perform f or g, depending on 'func'. If inv=1 (inversion),
	 * f or g will be performed backwards.
	 */
	if (func == 'f' && inv == 0) {
		if (n == 1) {
			move_it(w, 2, 1);
			return;
		}
		if (n == 2) {
			move_it(w, 2, 0);
			move_it(w, 2, 1);
			move_it(w, 0, 1);
			return;
		}
		px(w, 'f', 0, n - 1);
		move_it(w, 2, 0);
		px(w, 'g', 0, n - 2);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		move_it(w, 2, 1);
		px(w, 'f', 0, n - 2);
		return;
	}
	if (func == 'f' && inv == 1) {
		if (n == 1) {
			move_it(w, 1, 2);
			return;
		}
		if (n == 2) {
			move_it(w, 1, 0);
			move_it(w, 1, 2);
			move_it(w, 0, 2);
			return;
		}
		px(w, 'f', 1, n - 2);
		move_it(w, 1, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		px(w, 'g', 1, n - 2);
		move_it(w, 0, 2);
		px(w, 'f', 1, n - 1);
		return;
	}
	if (func == 'g' && inv == 0) {
		if (n == 1) {
			move_it(w, 1, 2);
			return;
		}
		px(w, 'g', 0, n - 1);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		move_it(w, 2, 0);
		px(w, 'g', 1, n - 1);
		move_it(w, 0, 2);
		px(w, 'f', 1, n - 1);
		move_it(w, 1, 0);
		return;
	}
	if (func != 'g' || inv != 1)
		return;
	if (n == 1) {
		move_it(w, 2, 1);
		return;
	}
	move_it(w, 0, 1);
	px(w, 'f', 0, n - 1);
	move_it(w, 2, 0);
	px(w, 'g', 0, n - 1);
	move_it(w, 0, 2);
	move_it(w, 1, 0);
	move_it(w, 2, 1);
	px(w, 'g', 1, n - 1);
}

static void
spx(PanexWidget w, char func, int inv, int n)
{
	/*spx is the mirror image of px
	 */
	if (func == 'f' && inv == 0) {
		if (n == 1) {
			move_it(w, 0, 1);
			return;
		}
		if (n == 2) {
			move_it(w, 0, 2);
			move_it(w, 0, 1);
			move_it(w, 2, 1);
			return;
		}
		spx(w, 'f', 0, n - 1);
		move_it(w, 0, 2);
		spx(w, 'g', 0, n - 2);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 0, 1);
		spx(w, 'f', 0, n - 2);
		return;
	}
	if (func == 'f' && inv == 1) {
		if (n == 1) {
			move_it(w, 1, 0);
			return;
		}
		if (n == 2) {
			move_it(w, 1, 2);
			move_it(w, 1, 0);
			move_it(w, 2, 0);
			return;
		}
		spx(w, 'f', 1, n - 2);
		move_it(w, 1, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 1);
		spx(w, 'g', 1, n - 2);
		move_it(w, 2, 0);
		spx(w, 'f', 1, n - 1);
		return;
	}
	if (func == 'g' && inv == 0) {
		if (n == 1) {
			move_it(w, 1, 0);
			return;
		}
		spx(w, 'g', 0, n - 1);
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		move_it(w, 0, 2);
		spx(w, 'g', 1, n - 1);
		move_it(w, 2, 0);
		spx(w, 'f', 1, n - 1);
		move_it(w, 1, 2);
		return;
	}
	if (func != 'g' || inv != 1)
		return;
	if (n == 1) {
		move_it(w, 0, 1);
		return;
	}
	move_it(w, 2, 1);
	spx(w, 'f', 0, n - 1);
	move_it(w, 0, 2);
	spx(w, 'g', 0, n - 1);
	move_it(w, 2, 0);
	move_it(w, 1, 2);
	move_it(w, 0, 1);
	spx(w, 'g', 1, n - 1);
}

/* The algorithm has 5 parts, p1, p2, ... p5. They use the basic procedures f
   and g. Except for p2, which also refers to p2 (itself), p3 and p4. */
static void
p1(PanexWidget w, int n)
{
	px(w, 'f', 0, n);
}

static void
p3(PanexWidget w, int n)
{
	if (n == 2) {
		move_it(w, 1, 0);
		move_it(w, 2, 1);
		return;
	}
	if (n <= 2)
		return;
	spx(w, 'g', 0, n - 2);
	move_it(w, 1, 0);
	move_it(w, 2, 1);
	move_it(w, 0, 2);
	spx(w, 'g', 1, n - 2);
	move_it(w, 2, 0);
	spx(w, 'f', 1, n - 2);
}

static void
p4(PanexWidget w, int n)
{
	if (n == 2) {
		move_it(w, 2, 0);
		move_it(w, 1, 2);
		move_it(w, 0, 2);
		return;
	}
	if (n <= 2)
		return;
	px(w, 'f', 0, n - 2);
	move_it(w, 2, 0);
	px(w, 'g', 0, n - 2);
	move_it(w, 0, 2);
	move_it(w, 1, 0);
	move_it(w, 2, 1);
	px(w, 'g', 1, n - 2);
	move_it(w, 0, 2);
	px(w, 'f', 1, n - 1);
}

static void
p5(PanexWidget w, int n)
{
	if (n == 1) {
		move_it(w, 1, 0);
		return;
	}
	if (n == 2) {
		move_it(w, 0, 2);
		move_it(w, 1, 0);
		move_it(w, 2, 0);
		return;
	}
	if (n <= 2)
		return;
	spx(w, 'f', 0, n - 2);
	move_it(w, 0, 2);
	spx(w, 'g', 0, n - 2);
	move_it(w, 2, 0);
	move_it(w, 1, 2);
	move_it(w, 0, 1);
	spx(w, 'g', 1, n - 2);
	move_it(w, 2, 0);
	spx(w, 'f', 1, n - 1);
}

static void
p2(PanexWidget w, int n)
{
	if (n == 1) {
		move_it(w, 0, 2);
		return;
	}
	if (n == 2) {
		move_it(w, 0, 2);
		move_it(w, 0, 2);
		return;
	}
	p2(w, n - 1);
	p3(w, n - 1);
	p4(w, n - 1);
	spx(w, 'f', 0, n - 2);
	move_it(w, 0, 2);
}

static void
SolvePanexMode(PanexWidget w)
{
	panexCallbackStruct cb;

	if (w->panex.tiles > 0) {
		p1(w, w->panex.tiles);
		p2(w, w->panex.tiles);
		p3(w, w->panex.tiles);
		p4(w, w->panex.tiles);
		p5(w, w->panex.tiles);
		move_it(w, FIRST_LAST, FIRST_LAST);
	}
	cb.reason = PANEX_COMPUTED;
	XtCallCallbacks((Widget) w, XtNselectCallback, &cb);
}
#endif

void
SolveTiles(PanexWidget w)
{
	if (w->panex.mode == PANEX)
		SolvePanexMode(w);
	else
		SolveHanoiMode(w);
}
