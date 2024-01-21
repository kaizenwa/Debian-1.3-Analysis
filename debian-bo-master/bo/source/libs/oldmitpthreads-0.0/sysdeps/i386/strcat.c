/* strcat(dest, src) -- Append SRC on the end of DEST.
   For Intel 80x86, x>=3.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@ipd.info.uni-karlsruhe.de>.
   Optimised a little by Alan Modra <Alan@SPRI.Levels.UniSA.Edu.Au>

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

char *
strcat(char * dest, const char *src)
{
#ifdef CHECKER
/* The assembly version of this routine gives checker heartburn because it
   accesses things by word and sometimes goes beyond the end of the string.
   So, we supply a simple slow version for checker.  Hey, its slow anyway. */

   char *retval;

   retval = dest;
   while (*dest != '\0')
   {
      dest++;
   }

   while (*src != '\0')
   {
      *dest = *src;
      src++;
      dest++;
   }

   *dest = '\0';

   return(retval);
#else
/*
 * We have to use two different versions, one for i386 and one for the rest
 * because the difference is so big.  Be aware when the name of the i586
 * (aka Pentium) in the gcc changes (I assume __i586__) or the next
 * generation (i686) is available.
 */
#if !defined(__i486__) && !defined(__i586__)

__asm__(
	"xorl %%eax,%%eax\n\t"
	"movl $-1,%%ecx\n\t"
	"cld\n\t"
	"repne\n\t"
	"scasb\n\t"
	"decl %0\n\t"

	"subl %0,%1\n\t"

	"testl $0x3,%0\n\t"
	"jz " LF(1) "\n\t"
	"movb (%1,%0),%%al\n\t"
	"movb %%al,(%0)\n\t"
	"orb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"incl %0\n\t"

	"testl $0x3,%0\n\t"
	"jz " LF(1) "\n\t"
	"movb (%1,%0),%%al\n\t"
	"movb %%al,(%0)\n\t"
	"orb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"incl %0\n\t"

	"testl $0x3,%0\n\t"
	"jz " LF(1) "\n\t"
	"movb (%1,%0),%%al\n\t"
	"movb %%al,(%0)\n\t"
	"orb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"incl %0\n\t"
	"jmp " LF(1) "\n\t"

	ALIGN
LL(2)
	"\tmovl %%eax,12(%0)\n\t"
	"addl $16,%0\n"

LL(1)	"\tmovl (%1,%0),%%eax\n\t"
	"movl $0xfefefeff,%%ecx\n\t"
	"addl %%eax,%%ecx\n\t"
	"jnc " LF(9) "\n\t"
	"xorl %%eax,%%ecx\n\t"
	"orl $0xfefefeff,%%ecx\n\t"
	"incl %%ecx\n\t"
	"jnz " LF(9) "\n\t"
	"movl %%eax,(%0)\n\t"

	"movl 4(%1,%0),%%eax\n\t"
	"movl $0xfefefeff,%%ecx\n\t"
	"addl %%eax,%%ecx\n\t"
	"jnc " LF(91) "\n\t"
	"xorl %%eax,%%ecx\n\t"
	"orl $0xfefefeff,%%ecx\n\t"
	"incl %%ecx\n\t"
	"jnz " LF(91) "\n\t"
	"movl %%eax,4(%0)\n\t"

	"movl 8(%1,%0),%%eax\n\t"
	"movl $0xfefefeff,%%ecx\n\t"
	"addl %%eax,%%ecx\n\t"
	"jnc " LF(92) "\n\t"
	"xorl %%eax,%%ecx\n\t"
	"orl $0xfefefeff,%%ecx\n\t"
	"incl %%ecx\n\t"
	"jnz " LF(92) "\n\t"
	"movl %%eax,8(%0)\n\t"

	"movl 12(%1,%0),%%eax\n\t"
	"movl $0xfefefeff,%%ecx\n\t"
	"addl %%eax,%%ecx\n\t"
	"jnc " LF(93) "\n\t"
	"xorl %%eax,%%ecx\n\t"
	"orl $0xfefefeff,%%ecx\n\t"
	"incl %%ecx\n\t"
	"jz " LB(2) "\n"

LL(93)	"\taddl $4,%0\n"
LL(92)	"\taddl $4,%0\n"
LL(91)	"\taddl $4,%0\n"

LL(9)	"\tmovb %%al,(%0)\n\t"
	"orb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"movb %%ah,1(%0)\n\t"
	"orb %%ah,%%ah\n\t"
	"jz " LF(8) "\n\t"
	"shrl $16,%%eax\n\t"
	"movb %%al,2(%0)\n\t"
	"orb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"movb %%ah,3(%0)\n"
LL(8)

	: : "D" (dest), "d" (src) : "ax","cx","dx","di");
#else
__asm__(
	"testb $0xff,(%1)\n\t"
	"jz " LF(8) "\n\t"

	/* test the first bytes separately until aligned */
	"testb $3,%0\n\t"
	"jz " LF(1) "\n\t"
	"testb $0xff,(%0)\n\t"	/* was last character ? */
	"jz " LF(2) "\n\t"	/* yes -> branch */
	"incl %0\n\t"

	"testb $3,%0\n\t"
	"jz " LF(1) "\n\t"
	"testb $0xff,(%0)\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"

	"testb $3,%0\n\t"
	"jz " LF(1) "\n\t"
	"testl $0xff,(%0)\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"
	"jmp " LF(1) "\n\t"


	ALIGN
LL(4)	"\taddl $16,%0\n"

LL(1)	"\tmovl (%0),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(3) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(3) "\n\t"

	"movl 4(%0),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(5) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(5) "\n\t"

	"movl 8(%0),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(6) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(6) "\n\t"

	"movl 12(%0),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(7) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jz " LB(4) "\n"

LL(7)	"\taddl $4,%0\n"
LL(6)	"\taddl $4,%0\n"
LL(5)	"\taddl $4,%0\n"

LL(3)
	"\ttestb %%al,%%al\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"
	"testb %%ah,%%ah\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"
	"testl $0xff0000,%%eax\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n"

LL(2)	"\tsubl %1,%0\n\t"

	"testb $3,%1\n\t"
	"jz " LF(29) "\n\t"
	"movb (%1),%%al\n\t"
	"movb %%al,(%0,%1)\n\t"
	"testb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"incl %1\n\t"

	"testb $3,%1\n\t"
	"jz " LF(29) "\n\t"
	"movb (%1),%%al\n\t"
	"movb %%al,(%0,%1)\n\t"
	"testb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"incl %1\n\t"

	"testb $3,%1\n\t"
	"jz " LF(29) "\n\t"
	"movb (%1),%%al\n\t"
	"movb %%al,(%0,%1)\n\t"
	"testb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"incl %1\n\t"
	"jmp " LF(29) "\n\t"

	ALIGN
LL(28)
	"\tmovl %%eax,12(%0,%1)\n\t"
	"addl $16,%1\n"
LL(29)
	"\tmovl (%1),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(9) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(9) "\n\t"
	"movl %%eax,(%0,%1)\n\t"

	"movl 4(%1),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(91) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(91) "\n\t"
	"movl %%eax,4(%0,%1)\n\t"

	"movl 8(%1),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(92) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(92) "\n\t"
	"movl %%eax,8(%0,%1)\n\t"

	"movl 12(%1),%%eax\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%eax,%%edi\n\t"
	"jnc " LF(93) "\n\t"
	"xorl %%eax,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jz " LB(28) "\n"

LL(93)	"\taddl $4,%1\n"
LL(92)	"\taddl $4,%1\n"
LL(91)	"\taddl $4,%1\n"

LL(9)	"\tmovb %%al,(%0,%1)\n\t"
	"orb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"movb %%ah,1(%0,%1)\n\t"
	"orb %%ah,%%ah\n\t"
	"jz " LF(8) "\n\t"
	"shrl $16,%%eax\n\t"
	"movb %%al,2(%0,%1)\n\t"
	"orb %%al,%%al\n\t"
	"jz " LF(8) "\n\t"
	"movb %%ah,3(%0,%1)\n"

LL(8)
	: : "d" (dest), "c" (src) : "ax","dx","cx","di");
#endif

/*
 * The following is to stop gcc putting "dest" into a register.
 * Any register gcc uses must be saved, as we've used all the scratch
 * registers, so it's silly of gcc to put this value into a register
 * when it can easily be retrieved from the stack.
 */
return *(char *volatile *)&dest;
#endif /* CHECKER */
}
