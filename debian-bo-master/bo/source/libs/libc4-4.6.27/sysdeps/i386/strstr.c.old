/* 
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

#include <string.h>
#include "asm-ops.h"

char *
strstr(const char *haystack, const char *needle)
{
register char *__res;
__asm__(
#if defined(__PIC__) || defined(__pic__)
        "pushl %%ebx\n\t"
#endif
	"movl %1,%0\n\t"
	"testb $0xff,(%2)\n\t"
	"je " LF(1) "\n\t"
	"movw (%2),%%cx\n\t"
	"addl $2,%2\n\t"
	"cld\n\t"

	"orb %%ch,%%ch\n\t"
	"jne " LF(5) "\n"
/* handle special case of a one-character-needle */
LL(6)	"\tmovb (%1),%%al\n\t"
	"orb %%al,%%al\n\t"
	"je " LF(2) "\n\t"
	"incl %1\n\t"
	"xorb %%cl,%%al\n\t"
	"jne " LB(6) "\n\t"
	"leal -1(%1),%0\n\t"
	"jmp " LF(1) "\n"

LL(5)	"\ttestb $0xff,(%1)\n\t"
	"je " LF(2) "\n\t"

	"cmpb %%cl,%%ch\n\t"
	"jne " LF(8) "\n"

LL(4)	"\tmovw (%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(30) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"movw 1(%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(31) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"movw 2(%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(32) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"movw 3(%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(33) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"addl $4,%1\n\t"
	"jmp " LB(4) "\n"

LL(33)	"\tincl %1\n"
LL(32)	"\tincl %1\n"
LL(31)	"\tincl %1\n"
LL(30)	"\tmovl %1,%0\n\t"
	"addl $2,%1\n\t"
	"movl %2,%%ebx\n"
LL(7)	"\tmovb (%%ebx),%%ah\n\t"
	"incl %%ebx\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(1) "\n\t"
	"movb (%1),%%al\n\t"
	"incl %1\n\t"
	"xorb %%al,%%ah\n\t"
	"je " LB(7) "\n\t"
	"orb %%al,%%al\n\t"
	"je " LF(2) "\n\t"
	"leal 1(%0),%1\n\t"
	"jmp " LB(4) "\n"

LL(8)	"\tmovw (%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(90) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"movw 1(%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(91) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"movw 2(%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(92) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"movw 3(%1),%%ax\n\t"
	"cmpw %%ax,%%cx\n\t"
	"je " LF(93) "\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(2) "\n\t"
	"addl $4,%1\n\t"
	"jmp " LB(8) "\n"

LL(93)	"\tincl %1\n"
LL(92)	"\tincl %1\n"
LL(91)	"\tincl %1\n"
LL(90)	"\tmovl %1,%0\n\t"
	"addl $2,%1\n\t"
	"movl %2,%%ebx\n"
LL(10)	"\tmovb (%%ebx),%%ah\n\t"
	"incl %%ebx\n\t"
	"orb %%ah,%%ah\n\t"
	"je " LF(1) "\n\t"
	"movb (%1),%%al\n\t"
	"incl %1\n\t"
	"xorb %%al,%%ah\n\t"
	"je " LB(10) "\n\t"
	"orb %%al,%%al\n\t"
	"je " LF(2) "\n\t"
	"leal 2(%0),%1\n\t"
	"jmp " LB(8) "\n"

LL(2)	"\txorl %0,%0\n"
#if defined(__PIC__) || defined(__pic__)
LL(1)	"\tpopl %%ebx\n"
        :"=D" (__res):"S" (haystack),"d" (needle):"si","di","cx","dx");
#else
LL(1)
        :"=D" (__res):"S" (haystack),"d" (needle):"si","di","bx","cx","dx");
#endif
  return __res;
}
