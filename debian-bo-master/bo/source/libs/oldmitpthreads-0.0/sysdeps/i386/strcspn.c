/* strcspn (str, ss) -- Return the length of the initial segement of STR
			which contains no characters from SS.
   For Intel 80x86, x>=3.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>
   Rewritten by Alan Modra <Alan@SPRI.Levels.UniSA.Edu.Au>
     - there was a horrible bug with the stop-char table being stored
       at -256(esp).  (interrupt => trashed table)

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

#include <string.h>

#include "asm-ops.h"

size_t
strcspn(const char *str, const char *stopset)
{
register size_t __res;
__asm__(
	/*
	 * First we create a table with flags for all possible characters.
	 * For the ASCII (7bit/8bit) or ISO-8859-X character sets which are
	 * supported by the C string functions we have 256 characters.
	 * Before inserting marks for the stop characters we clear the whole
	 * table.  The unrolled form is much faster than a loop.
	 */
	"xorl %%ecx,%%ecx\n\t"		/* Put 0 in a register because the  */
	"pushl %%ecx\n\t"		/* resulting code for all the moves */
	"pushl %%ecx\n\t"		/* is a lot shorter.  We need ecx   */
	"pushl %%ecx\n\t"		/* zero later anyway.		    */
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"
	"pushl %%ecx\n\t"

	ALIGN
	
	/* For understanding the following code remember that %ecx == 0 now.
	 * Although all the following instruction only modify %cl we always
	 * have a correct zero-extended 32-bit value in %ecx.  */


/*
 * Don't change the "testb $0xff,%%cl" to "testb %%cl,%%cl".  We want a
 * longer instruction so that the next loop aligns without adding nops
 */
LL(2)	"\tmovb (%1),%%cl\n\t"		/* get char from stopset */
	"movb %%cl,(%%esp,%%ecx)\n\t"	/* mark as stop character in table */
	"testb %%cl,%%cl\n\t"		/* was NULL char */
	"jz " LF(1) "\n\t"		/* yes, then stop */

	"movb 1(%1),%%cl\n\t"
	"movb %%cl,(%%esp,%%ecx)\n\t"
	"testb %%cl,%%cl\n\t"
	"jz " LF(1) "\n\t"

	"movb 2(%1),%%cl\n\t"
	"movb %%cl,(%%esp,%%ecx)\n\t"
	"testb %%cl,%%cl\n\t"
	"jz " LF(1) "\n\t"

	"movb 3(%1),%%cl\n\t"
	"addl $4,%1\n\t"		/* add loop increment */
	"movb %%cl,(%%esp,%%ecx)\n\t"
	"testb $0xff,%%cl\n\t"
	"jnz " LB(2) "\n"

LL(1)	"\tmovl $-4,%0\n"

	/* We use the base+index adressing mode because this means we only
	 * have to increment one counter in the loop.
	 */

LL(3)	"\taddl $4,%0\n\t"		/* add loop increment */

	"movb (%2,%0),%%cl\n\t"		/* get current string character */
	"cmpb %%cl,(%%esp,%%ecx)\n\t"	/* is char in table?  NULL always is */
	"je " LF(4) "\n\t"		/* yes, then exit */

	"movb 1(%2,%0),%%cl\n\t"
	"cmpb %%cl,(%%esp,%%ecx)\n\t"
	"je " LF(5) "\n\t"

	"movb 2(%2,%0),%%cl\n\t"
	"cmpb %%cl,(%%esp,%%ecx)\n\t"
	"je " LF(6) "\n\t"

	"movb 3(%2,%0),%%cl\n\t"
	"cmpb %%cl,(%%esp,%%ecx)\n\t"
	"jne " LB(3) "\n\t"

	"incl %0\n"			/* correct result length counter */
LL(6)	"\tincl %0\n"
LL(5)	"\tincl %0\n"
LL(4)	"\taddl $256,%%esp\n"		/* This stack fixup is only needed */
					/* when using -fomit-frame-pointer */

	: "=a" (__res) : "0" (stopset), "d" (str) : "cx");
return __res;
}
