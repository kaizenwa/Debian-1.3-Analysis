/* @(#) _str2set.c,v 1.2 1990/10/24 05:19:14 tron Exp */
/*
 * File   : _str2set.c
 * Author : Richard A. O'Keefe.
 * Updated: 20 April 1984
 * Defines: _set_ctr, _set_vec[], _str2set().
 * Purpose: Convert a character string to a set.
 */

/*
 * The obvious way of representing a set of characters  is  as  a
 * vector  of 0s and 1s.  The snag with that is that to convert a
 * string to such a vector, we have to clear all the elements  to
 * 0,  and  then  set the elements corresponding to characters in
 * the string to 1, so the cost is  O(|alphabet|+|string|).  This
 * package  uses another method, where there is a vector of small
 * numbers and a counter.  A character is in the current  set  if
 * and  only  if the corresponding element of the vector is equal
 * to the current value of  the  counter.   Every  so  often  the
 * vector  elements  would  overflow  and  we  have  to clear the
 * vector, but the cost is reduced to O(|string|+1).
 * 
 * Note that NUL ('\0') will never be in any set built by str2set.
 * 
 * While this method reduces the cost of building a set, it would
 * be useful to avoid it entirely.  So when the "set" argument is
 * NullS the set is not changed.  Use NullS to mean "the same set
 * as before."  MaxPosChar is the largest integer value which can
 * be stored in a "char".  Although we might get a slightly wider
 * range by using "unsigned char", "char" may be cheaper (as on a
 * PDP-11).  By all means change the number from 127 if your C is
 * one of those that treats char as unsigned, but don't change it
 * just because _AlphabetSize is 256, the two are unrelated.  And
 * don't dare change it on a VAX: it is built into the asm code!
 */

#include "strings.h"
#include "_str2set.h"

#if	CharsAreSigned
#define	MaxPosChar	127
#else
#define MaxPosChar	255
#endif

int  _set_ctr = MaxPosChar;
char _set_vec[_AlphabetSize];


void
_str2set(set)
    register char *set;
{
    if (set == NullS) return;
    if (++_set_ctr == MaxPosChar+1) {
#if	VaxAsm
	asm("movc5 $0,4(ap),$0,$128,__set_vec");
#else
	register char *w = &_set_vec[_AlphabetSize];
	do *--w = NUL; while (w != &_set_vec[0]);
#endif
	_set_ctr = 1;
    }
    while (*set) _set_vec[*set++] = _set_ctr;
}
