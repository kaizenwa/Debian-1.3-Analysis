/*
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>.
   Optimised by Alan Modra <Alan@SPRI.Levels.UniSA.Edu.Au>
     - also removed code for case where first two chars of needle are
       different.  The small gain isn't worth the extra code.
     
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

char *
strstr(const char *haystack, const char *needle)
{
register char *__res;
__asm__(
	"movl %1,%0\n\t"	/* we keep the possible result in %0 */

	"xorl %%ecx,%%ecx\n\t"
	"xorl %%edx,%%edx\n\t"
	"orb (%2),%%cl\n\t"	/* is needle == "" ? */
	"jz " LF(1) "\n\t"	/* yes, return haystack */

	/* Because the first two characters are the ones that are
	 * compared more often than all others we keep them in %cx
	 * all the time.
	 */
	"orb 1(%2),%%ch\n\t"

	/* But we have to handle the special case of an one-char string */
	"jz " LF(6) "\n\t"

	/* Handle normal case: length needle >= 2 */
	"cmpb (%1),%%dh\n\t"
	"jz " LF(2) "\n\t"
	"addl $2,%2\n"

#if 0
	"\tcmpb %%cl,%%ch\n\t"
	"je " LF(4) "\n"

LL(8)	"\tmovw (%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"je " LF(90) "\n\t"

	"movw 1(%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"je " LF(91) "\n\t"

	"movw 2(%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"je " LF(92) "\n\t"

	"movw 3(%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"addl $4,%1\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"jne " LB(8) "\n\t"

	"subl $3,%1\n"
LL(92)	"\tincl %1\n"
LL(91)	"\tincl %1\n"
LL(90)	"\tmovl %1,%0\n"

LL(10)	"\tmovb (%2),%%dl\n\t"
	"incl %2\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(1) "\n\t"
	"movb 2(%1),%%dh\n\t"
	"incl %1\n\t"
	"xorb %%dh,%%dl\n\t"
	"je " LB(10) "\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"

	"subl %1,%2\n\t"
	"leal 2(%0),%1\n\t"
	"addl %0,%2\n\t"
	"jmp " LB(8) "\n\t"
#endif

LL(4)	"\tmovw (%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"je " LF(30) "\n\t"

	"movw 1(%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"je " LF(31) "\n\t"

	"movw 2(%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"je " LF(32) "\n\t"

	"movw 3(%1),%%dx\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"
	"addl $4,%1\n\t"
	"cmpl %%edx,%%ecx\n\t"
	"jne " LB(4) "\n\t"

	"subl $3,%1\n"
LL(32)	"\tincl %1\n"
LL(31)	"\tincl %1\n"
LL(30)	"\tmovl %1,%0\n"

LL(7)	"\tmovb (%2),%%dl\n\t"
	"incl %2\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(1) "\n\t"
	"movb 2(%1),%%dh\n\t"
	"incl %1\n\t"
	"xorb %%dh,%%dl\n\t"
	"je " LB(7) "\n\t"
	"testb %%dh,%%dh\n\t"
	"je " LF(2) "\n\t"

	"subl %1,%2\n\t"
	"leal 1(%0),%1\n\t"
	"addl %0,%2\n\t"
	"jmp " LB(4) "\n\t"

	ALIGN
/* handle special case of a one-character-needle */
LL(6)	"\tmovb (%0),%%ch\n\t"	/* load char */
	"cmpb %%cl,%%ch\n\t"	/* equal to string character ? */
	"je " LF(1) "\n\t"	/* yes, then finished */
	"incl %0\n\t"		/* increment string pointer */
	"testb %%ch,%%ch\n\t"	/* is NULL? */
	"jne " LB(6) "\n"	/* no, then loop */

LL(2)	"\txorl %0,%0\n"
LL(1)
	: "=a" (__res) : "S" (haystack), "D" (needle) : "si","di","cx","dx");
  return __res;
}
