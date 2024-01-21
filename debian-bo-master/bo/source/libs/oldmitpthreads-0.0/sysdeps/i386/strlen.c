/* strlen(str) -- determine the length of the string STR.
   For Intel 80x86, x>=3.
   Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
   i386 version: Contributed by Torbjorn Granlund (tege@sics.se).
   i[45]86 version: Contributed by Ulrich Drepper <drepper@ira.uka.de>.

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
#include <string.h>

#include "asm-ops.h"

size_t
strlen(const char *str)
{
#ifdef CHECKER
/* The assembly version of this routine gives checker heartburn because it
   accesses things by word and sometimes goes beyond the end of the string.
   So, we supply a simple slow version for checker.  Hey, its slow anyway. */

   size_t count;

   count = 0;
   while (*str != '\0')
   {
      count++;
      str++;
   }

   return(count);
#else
register size_t __res;

__asm__(
#if !defined(__i486__) && !defined(__i586__)
/*
 * We have to use two different versions, one for i386 and for the rest
 * because the difference is so big.  Be aware when the name of the i586
 * (aka Pentium) in the gcc changes (I assume __i586__) or the next
 * generation (i686) is available.
 */
	"cld\n\t"
	"repne\n\t"
	"scasb\n\t"
	"notl %3\n\t"
	"leal -1(%3),%0"
	: "=a" (__res) : "D" (str), "0" (0), "c" (0xffffffff) : "cx","di");
#else
	/* test the first bytes separately until aligned */
	"movl %1,%%ecx\n\t"
	"andl $3,%%ecx\n\t"
	"jz " LF(1) "\n\t"
	"cmpb %%ch,(%1)\n\t"
	"je " LF(2) "\n\t"
	"incl %0\n\t"
	"xorl $3,%%ecx\n\t"
	"jz " LF(1) "\n\t"
	"cmpb %%ch,1(%1)\n\t"
	"je " LF(2) "\n\t"
	"incl %0\n\t"
	"decl %%ecx\n\t"
	"jz " LF(1) "\n\t"
	"cmpb %%ch,2(%1)\n\t"
	"je " LF(2) "\n\t"
	"incl %0\n\t"
	"jmp " LF(1) "\n\t"

	ALIGN

LL(4)	"\taddl $16,%0\n"

LL(1)	"\tmovl (%1,%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(3) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(3) "\n\t"

	"movl 4(%1,%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(5) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(5) "\n\t"

	"movl 8(%1,%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(6) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(6) "\n\t"

	"movl 12(%1,%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(7) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jz " LB(4) "\n"

LL(7)	"\taddl $4,%0\n"
LL(6)	"\taddl $4,%0\n"
LL(5)	"\taddl $4,%0\n"

LL(3)	"\ttestb %%cl,%%cl\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"
	"testb %%ch,%%ch\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"
	"testl $0xff0000,%%ecx\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n"
LL(2)
	: "=a" (__res) : "d" (str), "0" (0) : "cx","di");
#endif

return __res;
#endif /* CHECKER */
}
