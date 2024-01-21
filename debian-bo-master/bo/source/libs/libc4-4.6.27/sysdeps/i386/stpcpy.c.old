/* stpcpy -- copy SRC to DEST returning the address of the terminating '\0'
	     in DEST.
   For Intel 80x86, x>=3.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper (drepper@ira.uka.de).

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

char * stpcpy(char * dest, const char * src)
{
register char * __res;
__asm__("subl %0,%1\n\t"
	"subl $4,%0\n"
	/* four times unfolded loop without loop index increment */
	LL(1) "\taddl $4,%0\n\t"
	"movb (%0,%1),%%dl\n\t"
	"movb %%dl,(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(2) "\n\t"
	"movb 1(%0,%1),%%dl\n\t"
	"movb %%dl,1(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(3) "\n\t"
	"movb 2(%0,%1),%%dl\n\t"
	"movb %%dl,2(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"je " LF(4) "\n\t"
	"movb 3(%0,%1),%%dl\n\t"
	"movb %%dl,3(%0)\n\t"
	"testb %%dl,%%dl\n\t"
	"jne " LB(1) "\n\t"
	"incl %0\n"
	LL(4) "\tincl %0\n"
	LL(3) "\tincl %0\n"
	LL(2)
	:"=a" (__res):"c" (src),"0" (dest):"cx","dx");
return __res;
}
