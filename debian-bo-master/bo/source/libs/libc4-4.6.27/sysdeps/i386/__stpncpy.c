/* stpncpy -- copy no more then N bytes from SRC to DEST, returning the
	      address of the terminating '\0' in DEST.
   For Intel 80x86, x>=3.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>.

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
	"addl $4,%3\n\t"
	"subl $4,%0\n"
/* four time unfold loop */
LL(1)	"\taddl $4,%0\n\t"
	"subl $4,%3\n\t"
	"testl $0xfffffffc,%3\n\t"
	"je " LF(4) "\n\t"
	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(3) "\n\t"
	"movb 1(%0,%1),%%dl\n\t"
	"movb %%dl,1(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(6) "\n\t"
	"movb 2(%0,%1),%%dl\n\t"
	"movb %%dl,2(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(5) "\n\t"
	"movb 3(%0,%1),%%dl\n\t"
	"movb %%dl,3(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jne " LB(1) "\n\t"
	"decl %3\n\t"
	"incl %0\n"
LL(5)	"\tdecl %3\n\t"
	"incl %0\n"
LL(6)	"\tdecl %3\n\t"
	"incl %0\n\t"
	"jmp " LF(3) "\n"

/* rest of loop */
LL(4)	"testb %3,%3\n\t"
	"je " LF(2) "\n\t"
	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"incl %0\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(32) "\n\t"
	"decl %3\n\t"
	"je " LF(2) "\n\t"

	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"incl %0\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(32) "\n\t"
	"decl %3\n\t"
	"je " LF(2) "\n\t"

	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"incl %0\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(32) "\n\t"
	"decl %3\n\t"
	"je " LF(2) "\n\t"

/* fill with zero.  Here is a difference between how I understand the
   function and how it is implemented in the GNU libc.  In glibc the pointer
   returned is to the LAST '\0' written.  But I think is more useful to point
   to the '\0' terminating the string.  */
#ifdef GLIBC_COMPAT
LL(3)	"\tmovb $0,(%0)\n\t"
	"incl %0\n"
LL(32)	"\tdecl %3\n\t"
#else
LL(3)	"\tmovb $0,(%3,%0)\n"
LL(32)	"decl %3\n\t"
#endif
	"jne " LB(3) "\n"
LL(2)	"movb $0,(%0)\n\t"
	:"=a" (__res):"S" (src),"0" (dest),"c" (n):"si","cx","dx");
return __res;
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__stpncpy, stpncpy);
#endif
