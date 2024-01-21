/* Copyright (C) 1993 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>
   Modified September 30, 1995 by Jim Van Zandt <jrv@vanzandt.mv.com>

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <malloc.h>
#include <string.h>

#include <search.h>

/*
 * [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
 * [Knuth]            The Art of Computer Programming, part 3 (6.4)
 */


/*
 * We need a local static variable which contains the pointer to the
 * allocated memory for the hash table. An entry in this table contains
 * an ENTRY and a flag for usage.
 */

typedef struct { 
    int   used;
    ENTRY entry;
} _ENTRY;

static _ENTRY   * htable = NULL;
static unsigned   hsize;
static unsigned   filled;


/* 
 * For the double hash method used here, the table size has to be a
 * prime. To correct the user given table size we need a prime test.
 * This trivial algorithm is adequate because 
 * a) the code is (most probably) only called once per program run and
 * b) the number is small because the table must fit in the core 
 */

static int
DEFUN(isprime, (number), unsigned number)
{
    /* no even number will be passed */
    unsigned div = 3;

    while (div*div < number && number%div != 0)
        div += 2;

    return number%div != 0;
}


/*
 * Before using the hash table we must allocate memory for it.  Test
 * for an existing table. We allocate one element more than the found
 * prime number says. This is done for more effective indexing as
 * explained in the comment for the hsearch function.  The contents of
 * the table is zeroed.  In particular, the field used becomes zero.  
 */

int
DEFUN(hcreate, (nel), unsigned nel)
{
    /* There is still a table active. Return with error. */
    if (htable != NULL)
	return 0;

    /* Ensure that the original table will hold the requested number
       of entries, with no need for enlargement. */
    nel = (nel*10)/9+1;

    /* Change nel to the first prime number not smaller than nel. */
    nel |= 1;      /* make odd */
    while (!isprime(nel)) nel += 2;

    hsize  = nel;
    filled = 0;

    /* allocate memory and zero out */
    if ((htable = calloc(hsize+1, sizeof(_ENTRY))) == NULL)
	return 0;

    /* everything went all right */
    return 1;
}


/*
 * After use, the hash table can be destroyed. The memory used can
 * be freed and the local static variable can be marked as not used.
 */

void
DEFUN_VOID(hdestroy)
{
    /* free used memory */
    free(htable);

    /* the sign for an existing table is a value != NULL in htable */ 
    htable = NULL;
}


/*
 * This is the search function. It uses double hashing with open
 * adressing.  The argument item.key has to be a pointer to a zero
 * terminated array of char, most probably ASCII characters. The
 * function for generating a hash number for a string is simple but
 * fast. It can be replaced by a more complex function like ajw (see
 * [Aho,Sethi,Ullman]) if necessary.
 *
 * We use a trick to speed up the lookup. The table is created by
 * hcreate with one extra element available. This enables us to use the
 * index zero specially. This index will never be used because we
 * store the hash index in the field `used' where zero means not
 * used. Every other value means used. The `used' field can be used as
 * a first fast comparison for equality of the stored and the
 * parameter value. This helps to prevent unnecessary expensive calls
 * of strcmp.  
 */

ENTRY*
DEFUN(hsearch, (item, action), ENTRY item AND ACTION action)
{
    register unsigned hval;
    register unsigned hval2;
    register unsigned count;
    register unsigned idx;

    /*
     * If table is near full and another entry should be entered then
     * enlarge the table.  
     */
    if (action == ENTER && filled*10 > hsize*9) {
  	_ENTRY * oldtable = htable;
  	unsigned i, oldsize = hsize;
  	htable = NULL;
  	if (!hcreate(oldsize*2)) 
            return NULL;
  	for (i = 1; i <= oldsize; i++)
            if (oldtable[i].used)
                (void)hsearch(oldtable[i].entry, ENTER);
  	free(oldtable);
    }
      

    /* Compute a value for the given string. Could perhaps use a
       better method. */
    count = hval = strlen(item.key);
    while (count-- > 0) {
        hval <<= 4;
	hval += item.key[count];
    }

    /* First hash function: simply take the modulus but prevent zero. */
    hval %= hsize;
    if (hval == 0) hval++;

    /* The first index tried. */
    idx = hval;

    if (htable[idx].used) {

        if (htable[idx].used == hval &&
            strcmp(item.key, htable[idx].entry.key) == 0) {

	    return &htable[idx].entry;
        }

	/* Second hash function, as suggested in [Knuth] */
        hval2 = 1 + hval % (hsize-2);
	
        do {
	    /* 
	     * Because hsize is prime this is guaranteed to step
	     * through all available indices.  
	     */
            if (idx <= hval2)
	        idx = hsize+idx-hval2;
	    else
	        idx -= hval2;

            /* If entry is found use it. */
            if (htable[idx].used == hval &&
                strcmp(item.key, htable[idx].entry.key) == 0) {

	        return &htable[idx].entry;
            }


	} while (htable[idx].used);
    }

    /* An empty bucket has been found. */
    if (action == ENTER) {
        htable[idx].used  = hval;
        htable[idx].entry = item;

	filled++;

        return &htable[idx].entry;
    } else
        return NULL;
}
