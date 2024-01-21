/* strchr (str, ch) -- Return pointer to first occurrence of CH in STR.
   For Intel 80x86, x>=3.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>
   Some optimisations by Alan Modra <Alan@SPRI.Levels.UniSA.Edu.Au>

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
strchr(const char * s,int c)
{
#ifdef CHECKER
/* The assembly version of this routine gives checker heartburn because it
   accesses things by word and sometimes goes beyond the end of the string.
   So, we supply a simple slow version for checker.  Hey, its slow anyway. */

   char *retval = NULL;

   while (*s != '\0')
   {
      if (*s == c)
      {
         retval = s;
         break;
      }
      s++;
   }

   return(retval);
#else
register char * __res;
__asm__(
	/* Some optimizations:
	 * 0. use as few jumps as possible (this must ALWAYS be done)
	 * 1. loop unfolded four times
	 * 2. loop pointer incremented only at the end of the loop
	 * 3. don't use register used for register variables (i.e. which
	 *    must be saved)
	 */
	"movb %%dl,%%dh\n\t"	/* now %dx is c|c */
	"movl %%edx,%%ecx\n\t"
	"shll $16,%%edx\n\t"	/*     %edx is c|c|0|0 */
	"movw %%cx,%%dx\n\t"	/*     %edx is c|c|c|c */

	"testb $3,%0\n\t"	/* correctly aligned ? */
	"je " LF(11) "\n\t"	/* yes => begin loop */
	"movb (%0),%%cl\n\t"
	"cmpb %%cl,%%dl\n\t"	/* compare byte */
	"je " LF(6) "\n\t"	/* target found => return */
	"orb %%cl,%%cl\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"		/* increment source ptr */

	"testb $3,%0\n\t"	/* ditto */
	"je " LF(11) "\n\t"
	"movb (%0),%%cl\n\t"
	"cmpb %%cl,%%dl\n\t"
	"je " LF(6) "\n\t"
	"orb %%cl,%%cl\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"

	"testb $3,%0\n\t"	/* ditto */
	"je " LF(11) "\n\t"
	"movb (%0),%%cl\n\t"
	"cmpb %%cl,%%dl\n\t"
	"je " LF(6) "\n\t"
	"orb %%cl,%%cl\n\t"
	"jz " LF(2) "\n\t"
	"incl %0\n\t"
	"jmp " LF(11) "\n\t"

	ALIGN
LL(1)	"\taddl $16,%0\n"	/* increment loop counter */

LL(11)	"\tmovl (%0),%%ecx\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(7) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(7) "\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(2) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(2) "\n\t"

	"movl 4(%0),%%ecx\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(71) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(71) "\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(2) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(2) "\n\t"

	"movl 8(%0),%%ecx\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(72) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(72) "\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(2) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(2) "\n\t"

	"movl 12(%0),%%ecx\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(73) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(73) "\n\t"
	"xorl %%edx,%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(2) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jz " LB(1) "\n"	/* next round */

LL(2)	"\txorl %0,%0\n\t"	/* return NULL pointer for NOT FOUND */
	"jmp " LF(6) "\n"

LL(73)	"\taddl $4,%0\n"	/* correct counter */
LL(72)	"\taddl $4,%0\n"
LL(71)	"\taddl $4,%0\n"
LL(7)
	/* We now scan for the byte in which the character was matched.
	 * But we have to take care of the case that a NULL char is
	 * found before this in the dword.  */

	"\ttestb %%cl,%%cl\n\t"	/* first byte equal to c ? */
	"jz " LF(6) "\n\t"	/* equal, branch */
	"cmpb %%dl,%%cl\n\t"	/* was NULL char ? */
	"je " LB(2) "\n\t"	/* yes, return NULL */
	"testb %%ch,%%ch\n\t"	/* second byte equal to c ? */
	"jz " LF(5) "\n\t"	/* equal, branch */
	"cmpb %%dl,%%ch\n\t"	/* was NULL char ? */
	"je " LB(2) "\n\t"	/* yes, return NULL */
	"shrl $16,%%ecx\n\t"	/* we cannot access the upper 16 bits easily */
	"testb %%cl,%%cl\n\t"	/* third byte equal to c ? */
	"jz " LF(4) "\n\t"	/* equal, branch */
	"cmpb %%dl,%%cl\n\t"	/* was NULL char ? */
	"je " LB(2) "\n\t"	/* yes, return NULL */

	/* it must be in the fourth byte and it cannot be NULL */

	"incl %0\n"
LL(4)	"\tincl %0\n"		/* correct missing loop increment */
LL(5)	"\tincl %0\n"
LL(6)
	: "=a" (__res) : "0" (s), "d" (c) : "dx","di");
return __res;
#endif /* CHECKER */
}

#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (strchr, index);
#endif
