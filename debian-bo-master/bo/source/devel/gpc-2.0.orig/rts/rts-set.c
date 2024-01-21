/* Copyright (C) 1991 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Operations for SET type.

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
 * Author: Jukka Virtanen <jtv@hut.fi>
 *
 * Most of the routines in this file is inlined by the compiler.
 */

#include "rts.h"

#include "rts-set.h"

#ifdef DEBUG
/* 
 * Internal routine to count elements of the set */
static long
rts_card (setA, low_a, high_a)
SET_VECTOR *setA;
int low_a, high_a;
{
  long wordsA   = NUM_WORDS (low_a, high_a);
  long members  = 0;
  SET_VECTOR *a = BITV (setA);

  /* EMPTY_SET calls this */
  if (high_a == -1 && low_a == 0)
    return 0;

#ifdef USE_SET_LIBCALLS
  /* Don't care about this if not using libcalls */
  rts_clear_outside (setA, low_a, high_a, low_a, high_a);
#endif

  while (wordsA--) {
    if (*a)
      if (*a == -1)
	members += BITS_PER_WORD;
      else {
	SET_VECTOR word = *a;
	while (word) {
	  members += !! (word & (1L << (BITS_PER_WORD-1)));
	  word <<= 1;
	}
      }
    a++;
  }
  return members;
}

void
_p_dumpset (setA, low_a, high_a, level, name)
SET_VECTOR *setA;
int low_a, high_a;
int level;
char *name;
{
  int words 	= NUM_WORDS (low_a, high_a);
  int elements  = SET_SIZE  (low_a, high_a);
  int align_low = RLO(low_a);
  int is_sparse = SPARSE_SET (low_a, high_a);
  int members;
  int is_empty;

  int old_debug = Gpc_debug;
  
  if (Gpc_debug || !name)
    {
      if (! name)
	Gpc_debug = 3;

      if (Gpc_debug > level)
	level = Gpc_debug;
  
      Gpc_debug  = 0;
      
      members = rts_card (setA, low_a, high_a);
      is_empty = EMPTY_SET (setA, low_a, high_a);
      
      printf ("%s: Set limits: `%d'..`%d' low element align %d\n",
	      name ? name : "NONAME", low_a, high_a, align_low);
      printf ("%s: %s set; size %d elements; %d words; card %d\n",
	      name ? name : "NONAME",
	      is_empty ? "Empty" : is_sparse ? "Sparse" : "Normal",
	      elements, words, members);
      if (level > 1 && !is_empty && !is_sparse)
	{
	  int i;
	  SET_VECTOR *vector = BITV (setA);
	  printf ("Setd element bits (bit 0 on the right side)\n");
	  printf ("setd: ");
	  for (i = 0; i < words; i++)
	    {
	      if (i && (i % 8 == 0))
		{
		  printf ("\nsetd: ");
		}
#if BITS_PER_WORD == 64
	      printf ("%016lx ", *(vector+words-i-1));
#else
	      printf ("%08lx ", *(vector+words-i-1));
#endif
	    }
	  printf ("\n\n");
	}
      Gpc_debug = old_debug;
    }
}
#endif

#ifdef USE_SET_LIBCALLS
/* INLINED
 * Code clears the setA bits outside bounds.
 */
static void
rts_clear_outside (setA, low_a, high_a, lo, hi)
SET_VECTOR *setA;
int low_a, high_a;
int lo;
int hi;
{
  if (lo >= low_a && hi <= high_a && lo <= hi)
    {
      long nwl = THE_WORD (lo, low_a);
      long nwh = THE_WORD (hi, high_a);
      int  nbl = THE_BIT (lo);
      int  nbh = THE_BIT (hi);

      long owl = THE_WORD (low_a,  low_a);
      long owh = THE_WORD (high_a, high_a);
	
      if (owl == owh)			/* Is it the only word we have? */
	*(BITV(setA)+nwl) &= (~(-2L << (nbh - nbl))) << nbl;
      else
	{
	  int count = nwl - owl;
	  
	  while (count > 0)	/* Clear leading words */
	    BITV(setA)[ count-- ] = 0;
	  
	  if (nwl == nwh)
	    *(BITV(setA)+nwl) &= (~(-2L << (nbh - nbl))) << nbl;
	  else
	    {
	      /* Bits to leave on in the first word */
	      *(BITV(setA)+nwl) &= -1L << nbl;
	      
	      /* The words between are OK */
	      
	      /* Bits to leave on in the last word */
	      *(BITV(setA)+nwh) &= ~(-2L << nbh);
	    }

	  /* Clear trailing words */
	  count = owh - nwh;
	  while (count > 0)
	    BITV(setA)[ count-- ] = 0;
	}
    }
}


/* UNUSED (some similarity with setop_runtime in compiler setop.c)
 * TRAP ON BITMASK (runtime check)
 *
 * Return 1 if setA contains 1's in the bits that setB contains 0's
 * else return 0.
 *
 */
int
_p_set_trp (setA, low_a, high_a, setB, low_b, high_b)
SET_VECTOR *setA;
int low_a, high_a;
SET_VECTOR *setB;
int low_b, high_b;
{
  int wordsA = NUM_WORDS (low_a, high_a);
  int wordsB = NUM_WORDS (low_b, high_b);

  SET_VECTOR *a, *b;

  D(1, _p_dumpset(setA, low_a, high_a, 2, "trap (a)"));
  D(1, _p_dumpset(setB, low_b, high_b, 2, "trap (b)"));

  if (EMPTY_SET (setA, low_a, high_a))
    return 0;

  if (SPARSE_SET (low_a, high_a) || SPARSE_SET (low_b, high_b))
    abort ();

  if (wordsA != wordsB || low_a != low_b)
    _p_error (ABORT, "compiler has generated an illegal set trap mask");

  a = BITV (setA);
  b = BITV (setB);

  while (wordsA--) {
    if (*a++ & ~*b++)
      return 1;
  }
  return 0;
}


/* INLINED
 * A = A + [ B_LOW..B_HIGH ]
 *
 * Mark each member from B_LOW to B_HIGH as being in the set
 *
 * If B_LOW  > B_HIGH do nothing (give a warning)
 * If B_LOW == B_HIGH it's not a range, it's a single member, so act
 * 		      accordingly.
 * 
 * SETR     is the resulting set
 * LOW_R
 * HIGH_R
 * SIZE	    size of the elements in ABLOCK
 * COUNT    is the number of range elements
 * ABLOCK   address of the element block
 */

void
_p_set_range (setR, low_r, high_r, elem_size, count, ablock)
     SET_VECTOR *setR;
     int low_r, high_r, elem_size, count;
     char *ablock;
{
  long b_low;
  long b_high;

  if (count & 1)
    _p_error (ABORT, "Odd count in _p_set_range");

  /* Ranges are consumed in pairs */
  count >>= 1;

  while (count-- > 0)
    {
      switch (elem_size) {
      case 1: b_low  = *(char *)ablock;
	      b_high = *(char *)(ablock+elem_size);
	      break;
      case 2: b_low  = *(short *)ablock;
	      b_high = *(short *)(ablock+elem_size);
	      break;
      case 4: b_low  = *(int   *)ablock;
	      b_high = *(int   *)(ablock+elem_size);
	      break;
#if BITS_PER_WORD == 64
      case 8: b_low  = *(long *)ablock;
	      b_high = *(long *)(ablock+elem_size);
	      break;
#endif
      default:
	_p_error (705);
      }
      ablock += 2*elem_size;
      
      if (b_low > b_high) {
	_p_warning ("Empty set range");
	return;
      }
      
      if (b_low < low_r) {
	_p_generic (50);
	b_low = low_r;
      }

      /* @@@ ditto */
      if (b_high > high_r) {
	_p_generic (50); /* Gee, I should get rid of these stupid numbers! */
	b_high = high_r;
      }

      if (b_low == b_high) { /* Set a single member */
	BITV (setR)[ THE_WORD(b_low, low_r) ] |= 1L << THE_BIT(b_low);
	continue;
      }

      /* Now we need to set a valid range of bits between b_low & b_high */
      {
	long wl = THE_WORD (b_low,  low_r);
	long wh = THE_WORD (b_high, low_r);
	int  bl = THE_BIT (b_low);
	int  bh = THE_BIT (b_high);
	
	SET_VECTOR *setw = BITV (setR)+wl; /* Start modifying at this word */

	if (wl == wh)			/* Is it the only word we modify? */
	  *setw |= (~(-2L << (bh - bl))) << bl; /* Yes, so we are done */
	else {
	  *setw++ |= -1L << bl;		/* Bits to set in the first word */
	  
	  while (wl++ < wh)			/* The words between are all ones */
	    *setw++ = -1;
	  
	  *setw |= ~(-2L << bh);		/* Bits to set in the last word */
	}
      }
    }

  D(1, _p_dumpset(setR, low_r, high_r, 2, "range (result)"));
}

/* INLINED
 * A = A + [ element ]
 *
 * Mark one member of the set as being in it.
 *
 */
void
_p_set_single (setR, low_r, high_r, elem_size, count, ablock)
     SET_VECTOR *setR;
     int low_r, high_r, elem_size, count;
     char *ablock;
{
  long elem;

  while (count-- > 0)
    {
      switch (elem_size) {
      case 1: elem  = (low_r >= 0) ? *(unsigned char  *)ablock :
				     *(char  *)ablock;
	      break;
      case 2: elem  = (low_r >= 0) ? *(unsigned short  *)ablock :
				     *(short  *)ablock;
	      break;
      case 4: elem  = (low_r >= 0) ? *(unsigned int  *)ablock :
				     *(int  *)ablock;
	      break;
#if BITS_PER_WORD == 64
      case 8: elem  = (low_r >= 0) ? *(unsigned long  *)ablock :
				  *(long  *)ablock;
	      break;
#endif
      default:
	_p_error (705);
      }
      ablock += elem_size;
      
      if (elem < low_r || high_r < elem)
	{
	  D(1, fprintf (stderr, "While adding single %d into (%d .. %d)\n",
			elem, low_r, high_r));
	  _p_generic (50);
	}
      else
	BITV (setR)[ THE_WORD(elem, low_r) ] |= (1L << THE_BIT(elem));
    }
  
  D(1, _p_dumpset(setR, low_r, high_r, 2, "single (result)"));
}

/*
 * Sets are aligned on word boundaries, first element is in the
 * first word of the set, bit THE_BIT(low_a).
 * The bit number of the first element on word 0 is RLO(low_a), which  does
 * not belong to the set if felem%BITS_PER_WORD != 0
 *
 * The last bit high_a is in the last word of the set,
 * in bit THE_BIT(high_a).
 *
 * There may be unused bits in the end of the set, due to alignment.
 *
 * The number of words in the setX is NUM_WORDS(low_x, high_x)
 *
 */

/* INLINED */
/* IN:
 *	setA, low_a, high_a, elem
 *	
 * OUT:
 * 	function returns 1 if ELEM is a member of set A else returns 0
 */
int
_p_set_in (setA, low_a, high_a, elem)
SET_VECTOR *setA;
int low_a, high_a;
long elem;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "in"));

  if (EMPTY_SET(setA, low_a, high_a) || low_a > elem || elem > high_a)
    return 0;

  return !!(BITV (setA)[ THE_WORD (elem, low_a) ] & (1L << THE_BIT (elem)));
}

/* INLINED */
/*
 * R = A
 *
 * Assign (copy) A to R
 */
void
_p_set_copy (setA, low_a, high_a, setR, low_r, high_r)
     SET_VECTOR *setA;
     int low_a, high_a;
     SET_VECTOR *setR;
     int low_r, high_r;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "copy (source)"));
  if (EMPTY_SET (setA, low_a, high_a))
    bzero (BITV (setR), NUM_BYTES (low_r, high_r));
  else
    {
      int  wordsR = NUM_WORDS (low_r, high_r);
      int  wordsA = NUM_WORDS (low_a, high_a);

      if (wordsA <= wordsR && low_a == low_r)
	{
	  bcopy ((char *)BITV (setA),
		 (char *)BITV (setR),
		 NUM_BYTES(low_a, high_a));
	}
      else
	{
	  int first_word;

	  if (high_a < low_r || low_a > high_r)
	    _p_error (ABORT,"Sets have no common element in _p_set_copy");

	  /* note: This copies only PARTIAL SET */
	  if (low_a <= low_r && high_a >= low_r)
	    low_a = low_r;

	  if (high_a >= high_r && low_a <= high_r)
	    high_a = high_r;

	  first_word = THE_WORD (low_a, low_r);

	  /* @@@ Why +1 ??? */
	  bcopy ((char *)BITV(setA),
		 (char *)&(BITV (setR)[ first_word+1 ]),
		 NUM_BYTES (low_a, high_a));
	}

      /* Clear out the bits not in range */
      rts_clear_outside (setR, low_r, high_r, low_a, high_a);
    }
  D(1, _p_dumpset(setR, low_r, high_r, 2, "copy (result)"));
}

/* INLINED */
/*
 * R = A + B (UNION)
 * 
 */

void
_p_set_union (setA, low_a, high_a, setB, low_b, high_b, setR, low_r, high_r)
     SET_VECTOR *setA; int low_a, high_a;
     SET_VECTOR *setB; int low_b, high_b;
     SET_VECTOR *setR; int low_r, high_r;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "union + (a)"));
  D(1, _p_dumpset(setB, low_b, high_b, 2, "union + (b)"));

  /* First, try the easy cases */

  if (EMPTY_SET (setA, low_a, high_a) && EMPTY_SET (setB, low_b, high_b))
    bzero (BITV (setR), NUM_BYTES (low_r, high_r));
  else if (EMPTY_SET (setA, low_a, high_a))
    _p_set_copy (setB, low_b, high_b, setR, low_r, high_r);
  else
    {
      int wordsB;
      int wordsR;
      
      SET_VECTOR *b = BITV (setB),
      		 *r = BITV (setR);
      
      _p_set_copy (setA, low_a, high_a, setR, low_r, high_r);

      if (EMPTY_SET (setB, low_b, high_b))
	return;

      if (low_b < low_r || high_b > high_r || low_b > high_r || high_b < low_r)
	_p_error (ABORT, "_p_set_union internal error");
      
      if (low_b > low_r)
	{
	  b += VECTOR_ADJUST (low_b, low_r);
	  low_b = low_r;
	}

      wordsB = NUM_WORDS (low_b, high_b);
      
      if (low_b != low_r)
	r += THE_WORD (low_b, low_r);
	  
      while (wordsB--)
	{
	  *r++ |= *b++;
	}
    }
  D(1, _p_dumpset(setR, low_r, high_r, 2, "union (results)"));
}

/* INLINED */
/*
 * R = A - B (DIFFERENCE)
 * 
 */

void
_p_set_diff (setA, low_a, high_a, setB, low_b, high_b, setR, low_r, high_r)
     SET_VECTOR *setA; int low_a, high_a;
     SET_VECTOR *setB; int low_b, high_b;
     SET_VECTOR *setR; int low_r, high_r;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "diff - (a)"));
  D(1, _p_dumpset(setB, low_b, high_b, 2, "diff - (b)"));
  /* First, try the easy cases */
  if (EMPTY_SET (setB, low_b, high_b))
    _p_set_copy (setA, low_a, high_a, setR, low_r, high_r);
  else if (EMPTY_SET (setA, low_a, high_a))
    bzero (BITV (setR), NUM_BYTES (low_r, high_r));
  else
    {
      _p_set_copy (setA, low_a, high_a, setR, low_r, high_r);

      if (low_b <= high_r && high_b >= low_r)
	{
	  int wordsB;

	  SET_VECTOR *b = BITV (setB),
	  	     *r = BITV (setR);
      
	  if (low_b < low_r)
	    {
	      b += VECTOR_ADJUST (low_b, low_r);
	      low_b = low_r;
	    }

	  if (high_b > high_r)
	    high_b = high_r;

	  wordsB = NUM_WORDS (low_b, high_b);

	  if (low_b != low_r)
	    r += THE_WORD (low_b, low_r);
	  
	  while (wordsB--)
	    {
	      *r++ &= ~*b++;
	    }
	  rts_clear_outside (setR, low_r, high_r, low_a, high_a);
	}
    }
  D(1, _p_dumpset(setR, low_r, high_r, 2, "diff (result)"));;
}

/* INLINED */
/*
 * R = A * B (INTERSECTION)
 * 
 */

void
_p_set_intersection (setA, low_a, high_a,
		     setB, low_b, high_b,
		     setR, low_r, high_r)
     SET_VECTOR *setA; int low_a, high_a;
     SET_VECTOR *setB; int low_b, high_b;
     SET_VECTOR *setR; int low_r, high_r;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "intersect * (a)"));
  D(1, _p_dumpset(setB, low_b, high_b, 2, "intersect * (b)"));
  /* First, try the easy cases */
  if (EMPTY_SET (setB, low_b, high_b) || EMPTY_SET (setA, low_a, high_a) ||
      low_a > high_b || high_a < low_b)
    bzero (BITV (setR), NUM_BYTES (low_r, high_r));
  else
    {
      /* Nonempty sets have at least some possible common elements */

      int wordsB;
      
      SET_VECTOR *b = BITV (setB),
      		 *r = BITV (setR);
      
      _p_set_copy (setA, low_a, high_a, setR, low_r, high_r);
      
      if (low_b < low_r)
	{
	  b += VECTOR_ADJUST (low_b, low_r);
	  low_b = low_r;
	}
      else if (low_b > high_r)
	_p_error (ABORT, "set intersection internal error");
      else
	{
	  /* Do NOT adjust bounds here */
	  r += THE_WORD (low_b, low_r);
	}

      if (high_b > high_r)
	high_b = high_r;
      
      /* Number of common words */
      wordsB = NUM_WORDS (low_b, high_b);

      while (wordsB--)
	{
	  *r++ &= *b++;
	}

      /* Clear parts that don't overlap */
      rts_clear_outside (setR, low_r, high_r, low_b, high_b);
    }
  D(1, _p_dumpset(setR, low_r, high_r, 2, "intersection (result)"));
}

/* INLINED */
/*
 * A = B (EQUALITY)
 * 
 * Returns 1 if sets are equal, 0 otherwise.
 */

int
_p_set_equal (setA, low_a, high_a, setB, low_b, high_b)
     SET_VECTOR *setA; int low_a, high_a;
     SET_VECTOR *setB; int low_b, high_b;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "equal (a)"));
  D(1, _p_dumpset(setB, low_b, high_b, 2, "equal (b)"));
  /* First, try the easy cases */
  if (EMPTY_SET (setB, low_b, high_b) && EMPTY_SET (setA, low_a, high_a))
    return 1;
  else if (EMPTY_SET (setB, low_b, high_b))
    return ! rts_card (setA, low_a, high_a);
  else if (EMPTY_SET (setA, low_a, high_a))
    return ! rts_card (setB, low_b, high_b);
  else if (low_a > high_b || high_a < low_b)
    return 0;	/* No overlap, non-empty sets */
  
  {
    int wordsB = NUM_WORDS (low_b, high_b);
    int wordsA = NUM_WORDS (low_a, high_a);
      
    if (wordsB == wordsA && low_b == low_a)
      /* @@@@ memcmp!! */
      return !bcmp (BITV (setA), BITV (setB), NUM_BYTES (low_a, high_a));
    else
      {
	SET_VECTOR *seta;
	SET_VECTOR *setb;
	int count, equal;
	
	if (low_a < low_b)
	  {
	    seta = BITV(setA)+THE_WORD(low_b, low_a);
	    setb = BITV(setB);
	    if (high_a >= high_b)	/* B fits in A range */
	      count = NUM_WORDS (low_b, high_b);
	    else
	      count = (high_a+BITS_PER_WORD-RLO(low_b)) / BITS_PER_WORD;
	  }
	else
	  {
	    seta = BITV(setA);
	    setb = BITV(setB)+THE_WORD(low_a, low_b);
	    if (high_b >= high_a)	/* A fits in B range */
	      count = NUM_WORDS (low_a, high_a);
	    else
	      count = (high_b+BITS_PER_WORD-RLO(low_a)) / BITS_PER_WORD;
	  }

	equal = 1;
	while (count-- && equal)
	  equal = *seta++ == *setb++;

	if (equal)
	  equal = rts_card (setB, low_b, high_b)
	    	  == rts_card (setA,low_a, high_a);

	return equal;
      }
  }
}

/* INLINED */
/*
 * A <= B (INCLUSION of A in B)
 * 
 * Returns 1 if true, 0 otherwise
 */
int
_p_set_le (setA, low_a, high_a, setB, low_b, high_b)
     SET_VECTOR *setA; int low_a, high_a;
     SET_VECTOR *setB; int low_b, high_b;
{
  D(1, _p_dumpset(setA, low_a, high_a ,2, "le (a)"));
  D(1, _p_dumpset(setB, low_b, high_b, 2, "le (b)"));
  /* First, try the easy cases */
  if (EMPTY_SET (setA, low_a, high_a))
    return 1;
  else if (EMPTY_SET (setB, low_b, high_b))
    return ! rts_card (setA, low_a, high_a);
  else
    {
      int wordsB = NUM_WORDS (low_b, high_b);
      int wordsA = NUM_WORDS (low_a, high_a);
      
      SET_VECTOR *b = BITV (setB),
      		 *a = BITV (setA);
      
      if (wordsA <= wordsB && low_a == low_b)
	{
	  while (wordsA--)
	    if (*a++ & ~*b++)
	      return 0;
	}
      else {
	/* @@@@@@@@@@@@ finish this */
	_p_error (ABORT, "_p_set_le is under construction, sorry");
      }
      
    }
  return 1;
}

/* Following routines are defined only in Extended Pascal */

/* INLINED */
/*
 * R = A >< B (SYMMETRIC DIFFERENCE)
 * 
 */

void
_p_set_symdiff (setA, low_a, high_a, setB, low_b, high_b, setR, low_r, high_r)
     SET_VECTOR *setA; int low_a, high_a;
     SET_VECTOR *setB; int low_b, high_b;
     SET_VECTOR *setR; int low_r, high_r;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "symdif >< (a)"));
  D(1, _p_dumpset(setB, low_b, high_b, 2, "symdif >< (b)"));
  /* First, try the easy cases */
  if (EMPTY_SET (setB, low_b, high_b))
    _p_set_copy (setA, low_a, high_a, setR, low_r, high_r);
  else if (EMPTY_SET (setA, low_a, high_a))
    _p_set_copy (setB, low_b, high_b, setR, low_r, high_r);
  else
    {
      int wordsB = NUM_WORDS (low_b, high_b);
      int wordsR;
      
      SET_VECTOR *b = BITV (setB),
      		 *r = BITV (setR);
      
      _p_set_copy (setA, low_a, high_a, setR, low_r, high_r);
      wordsR = NUM_WORDS (low_r, high_r);

      if (wordsB > wordsR || high_b > high_r || low_b < low_r)
	_p_generic (50); /* @@@@ */
      else
	{
	  if (low_b != low_r)
	    r += THE_WORD (low_b, low_r);
	  
	  while (wordsB--)
	    {
	      *r++ ^= *b++;
	    }
	}
    }
  D(1, _p_dumpset(setR, low_r, high_r, 2, "symdiff (result)"));
}

/* INLINED */
/*
 * CARD(A) (CARD)
 * 
 * Returns a number of members in the set
 *
 */
long
_p_set_card (setA, low_a, high_a)
SET_VECTOR *setA;
int low_a, high_a;
{
  D(1, _p_dumpset(setA, low_a, high_a, 2, "card (a)"));

  return rts_card (setA,low_a, high_a);
}

#endif /* USE_SET_LIBCALLS */
