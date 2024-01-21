/* Copyright (C) 1991 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Definitions for SET type.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/*
 *
 * Author: Jukka Virtanen <jtv@hut.fi>
 *
 */

#ifndef _GPC_SET_H_
#define _GPC_SET_H_
/*
 * This is the first test version, to test the algorithms.
 */

/* The sets are WORD ALIGNED. A pointer to the set bit vector, along
 * with the low and high element numbers are passed in strcut set_t.
 *
 * Code assumes that compiler has done type checking properly.
 * So, even if the set bounds are different, we must adjust the operation
 * to handle the difference. Also, the result set must be large
 * enough to hold the result.
 */

/* @@@ BUG: Does not work with negative bounds! */

/* Sets are returned as int pointer, since the size does not change here */
typedef unsigned long SET_VECTOR;

/* Get the address of the bit vector */
#define BITV(s) ((SET_VECTOR *)s)

/* Return the first bit number of the first word.
   This bit does not necessarily belong to the set. */
#define RLO(lo) (lo-((lo)%BITS_PER_WORD))

/* Returns the size of the set as number of elements in it. */
#define SET_SIZE(lo,hi) (hi-lo+1)

/* Returns the word number from start of set in which the A'th member is in */
#define THE_WORD(a,lo) (((a)-RLO(lo)) / BITS_PER_WORD)

/* Returns the bit number in THE_WORD the A'th member is */
#define THE_BIT(a)  ((a) % BITS_PER_WORD)

/* returns the number of words in the set */
#define NUM_WORDS(lo,hi) (((hi)+BITS_PER_WORD-RLO(lo)) / BITS_PER_WORD)

/* Return the number of words needeed to adjust set vector A to B */
#define VECTOR_ADJUST(lo_a,lo_b) ((RLO(lo_b)-RLO(lo_a))/BITS_PER_WORD)

/* Returns the number of bytes in the set */
/* Note that the "count" is word aligned */
#define NUM_BYTES(lo,hi) (NUM_WORDS(lo,hi) * UNITS_PER_WORD)

/* Return 1 if the set is empty or the special node empty_set_node */
#ifdef USE_SET_LIBCALLS
#define EMPTY_SET(s,lo,hi) (((hi) == -1 && (lo) == 0) || !rts_card (s,lo,hi))
#else
#define EMPTY_SET(s,lo,hi) ((hi) == -1 && (lo) == 0)
#endif

/* Return 1 if the set is a sparse set.
   This is special format:
   	HI(s) == -2,
	LO(s) specifies the number of range pairs that
	      specify the elements in the set. This is
	      only used when the number of elements in the
	      set would be HUGE, e.g. [ -maxint..maxint ]

   This is not implemented yet.
 */   
#define SPARSE_SET(lo,hi) ((hi) == -2 && (lo) > 0)

#ifdef USE_SET_LIBCALLS
int
_p_set_in PROTO((SET_VECTOR *setA, int low_a, int high_a, long val));

static void
rts_clear_outside PROTO((SET_VECTOR *setA, int low_a, int high_a, int lo, int hi));


void
_p_set_copy PROTO((SET_VECTOR *setA, int low_a, int high_a,
	     SET_VECTOR *setR, int low_r, int high_r));

int
_p_set_trap PROTO((SET_VECTOR *setA, int low_a, int high_a,
	     SET_VECTOR *setB, int low_b, int high_b));

void
_p_set_range PROTO((SET_VECTOR *setR, int low_r, int high_r, int elem_size,
	      int count, char *ablock));

void
_p_set_single PROTO((SET_VECTOR *setR, int low_r, int high_r, int elem_size,
	       int count, char *ablock));

void
_p_set_union PROTO((SET_VECTOR *setA, int low_a, int high_a,
	      SET_VECTOR *setB, int low_b, int high_b,
	      SET_VECTOR *setR, int low_r, int high_r));

void
_p_set_diff PROTO((SET_VECTOR *setA, int low_a, int high_a,
	     SET_VECTOR *setB, int low_b, int high_b,
	     SET_VECTOR *setR, int low_r, int high_r));

void
_p_set_intersection PROTO((SET_VECTOR *setA, int low_a, int high_a,
		     SET_VECTOR *setB, int low_b, int high_b,
		     SET_VECTOR *setR, int low_r, int high_r));

int
_p_set_equal PROTO((SET_VECTOR *setA, int low_a, int high_a,
	      SET_VECTOR *setB, int low_b, int high_b));

int
_p_set_le PROTO((SET_VECTOR *setA, int low_a, int high_a,
	   SET_VECTOR *setB, int low_b, int high_b));

void
_p_set_symdiff PROTO((SET_VECTOR *setA, int low_a, int high_a,
		SET_VECTOR *setB, int low_b, int high_b,
		SET_VECTOR *setR, int low_r, int high_r));
#endif /* USE_SET_LIBCALLS */

long
_p_set_card PROTO((SET_VECTOR *setA, int low_a,   int high));

void
_p_dumpset PROTO((SET_VECTOR *, int, int, int, char *));

#endif /* _GPC_SET_H_ */
