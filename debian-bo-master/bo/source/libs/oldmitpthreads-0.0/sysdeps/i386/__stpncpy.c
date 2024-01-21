/* stpncpy -- copy no more then N bytes from SRC to DEST, returning the
	      address of the terminating '\0' in DEST.
   For Intel 80x86, x>=3.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>.
   Rewritten by Alan Modra <Alan@SPRI.Levels.UniSA.Edu.Au>
     - original wrote n+1 chars in some cases.
     - stpncpy() ought to behave like strncpy() ie. not null-terminate
       if limited by n.  glibc-1.09 stpncpy() does this.

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

/* This function is defined neither in ANSI nor POSIX standards but is
   also not invented here.  */

#include <string.h>
#include "asm-ops.h"

char *
__stpncpy(char * dest, const char * src, size_t n)
{
register char * __res;
__asm__("subl %0,%1\n\t"
	"jmp " LF(1) "\n\t"

	ALIGN
	/* Four times unfolded loop with two loop counters.  We get the
	 * the third value (the source address) by using the index+base
	 * adressing mode.
	 */
LL(2)	"\tmovb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"testb %%dl,%%dl\n\t"	/* if last character */
	"jz " LF(7) "\n\t"	/* then exit */

	"movb 1(%0,%1),%%dl\n\t"
	"movb %%dl,1(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jz " LF(6) "\n\t"

	"movb 2(%0,%1),%%dl\n\t"
	"movb %%dl,2(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jz " LF(5) "\n\t"

	"movb 3(%0,%1),%%dl\n\t"
	"movb %%dl,3(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jz " LF(4) "\n\t"
	"addl $4,%0\n"		/* increment loop counter */
LL(1)	"\tsubl $4,%3\n\t"	/* decrement length counter */
	"jae " LB(2) "\n\t"	/* >= 4 chars left, then repeat */

/* Process rest of string byte by byte. (0 to 3 chars) */

	"addl $4,%3\n\t"
	"jz " LF(9) "\n\t"
	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jz " LF(3) "\n\t"
	"incl %0\n\t"
	"decl %3\n\t"
	"jz " LF(9) "\n\t"

	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jz " LF(3) "\n\t"
	"incl %0\n\t"
	"decl %3\n\t"
	"jz " LF(9) "\n\t"

	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jz " LF(9) "\n\t"
	"incl %0\n\t"
	"jmp " LF(9) "\n"


LL(4)	"\tdecl %3\n\t"
	"incl %0\n"
LL(5)	"\tdecl %3\n\t"
	"incl %0\n"
LL(6)	"\tdecl %3\n\t"
	"incl %0\n"		/* %0 points to the NULL we've just written */
LL(7)	"\taddl $3,%3\n\t"	/* %3 has byte count not yet written */
	"jz " LF(9) "\n"

/* fill with zero.  Here is a difference between how I understand the
   function and how it is implemented in the GNU libc (1.09).  In glibc
   the pointer returned is to the LAST '\0' written.  But I think it is
   more useful to point to the '\0' terminating the string.

   Roland McGrath agreed with me on this.  Hopefully in one of the next GNU
   libc releases it will be changed.  */

LL(8)	"\tmovb $0,(%3,%0)\n"
LL(3)	"\tdecl %3\n\t"
	"jnz " LB(8) "\n"

LL(9)
	:"=a" (__res):"S" (src),"0" (dest),"c" (n):"si","cx","dx");
return __res;
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__stpncpy, stpncpy);
#endif
