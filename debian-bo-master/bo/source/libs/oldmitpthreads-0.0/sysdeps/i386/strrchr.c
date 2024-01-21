/* strrchr (str, ch) -- Return pointer to last occurrence of CH in STR.
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
strrchr(const char * str, int ch)
{
#ifdef CHECKER
/* The assembly version of this routine gives checker heartburn because it
   accesses things by word and sometimes goes beyond the end of the string.
   So, we supply a simple slow version for checker.  Hey, its slow anyway. */

   char *retval = NULL;

   while (*str != '\0')
   {
      if (*str == ch)
      {
         retval = str;
      }
      str++;
   }

   return(retval);
#else
register char * __res;

__asm__(
	"movb %%cl,%%ch\n\t"	/* now %cx is c|c */
	"movl %%ecx,%%edx\n\t"
	"shll $16,%%ecx\n\t"	/*     %ecx is c|c|0|0 */
	"movw %%dx,%%cx\n\t"	/*     %ecx is c|c|c|c */

	"testl $3,%2\n\t"
	"jz " LF(19) "\n\t"
	"movb (%2),%%dl\n\t"
	"cmpb %%cl,%%dl\n\t"
	"jne " LF(11) "\n\t"
	"movl %2,%0\n"
LL(11)	"\torb %%dl,%%dl\n\t"
	"jz " LF(2) "\n\t"
	"incl %2\n\t"

	"testl $3,%2\n\t"
	"jz " LF(19) "\n\t"
	"movb (%2),%%dl\n\t"
	"cmpb %%cl,%%dl\n\t"
	"jne " LF(12) "\n\t"
	"movl %2,%0\n"
LL(12)	"\torb %%dl,%%dl\n\t"
	"jz " LF(2) "\n\t"
	"incl %2\n\t"

	"testl $3,%2\n\t"
	"jz " LF(19) "\n\t"
	"movb (%2),%%dl\n\t"
	"cmpb %%cl,%%dl\n\t"
	"jne " LF(13) "\n\t"
	"movl %2,%0\n"
LL(13)	"\torb %%dl,%%dl\n\t"
	"jz " LF(2) "\n\t"
	"incl %2\n\t"

	"jmp " LF(19) "\n\t"

/* The following makes LL(1) aligned since we have 56 bytes between here
 * and LL(1).  Of course, if you change the code between here and LL(1),
 * you will need to count code bytes, and modify the following expression.
 */
#if 0
	".space -(. - "ASIDENT(strrchr)" + 56) & ("ALIGNTO"-1), 0x90\n"
#else
/* Arrgh, gas as at 950624, makes a mistake when evaluating the previous
 * "correct" expression, and gets the value of "." wrong by 4 due to
 * resolving "." too early.  Anyway, the following works quite well
 */
	".space -(. - "ASIDENT(strrchr)" + 56) & 3, 0x90\n"
#endif

LL(4)	"\tsubl $4,%2\n"	/* correct missing pointer increments	*/
LL(41)	"\tsubl $4,%2\n"	/*  (allowing for following +16)	*/
LL(42)	"\tsubl $4,%2\n"
LL(43)	"\ttestl $0xff000000,%%edx\n\t"
	"jnz " LF(33) "\n\t"
	"leal 15(%2),%0\n\t"
	"jmp " LF(1) "\n"

LL(3)	"\tsubl $4,%2\n"	/* correct missing pointer increments	*/
LL(31)	"\tsubl $4,%2\n"	/*  (allowing for following +16)	*/
LL(32)	"\tsubl $4,%2\n"
LL(33)	"\ttestl $0xff0000,%%edx\n\t"
	"jnz " LF(51) "\n\t"
	"leal 14(%2),%0\n\t"
	"jmp " LF(1) "\n"

LL(51)	"\ttestb %%dh,%%dh\n\t"
	"jnz " LF(52) "\n\t"
	"leal 13(%2),%0\n\t"
	"jmp " LF(1) "\n"

LL(52)	"\tleal 12(%2),%0\n"

	/*
	 * Some optimizations:
	 * 1. four times unfolded loop
	 * 2. loop pointer increment only at the end of the loop
	 * 3. don't use register used for register variables (i.e. which
	 *    must be saved)
	 * 4. optimize main flow path; the assumption is that the searched
	 *    character does not appear as often as other ones.	 So the
	 *    more expensive jump after the comparison if done for matched
	 *    characters.
	 */

LL(1)	"\taddl $16,%2\n"

LL(19)	"\tmovl (%2),%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LF(20) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(20) "\n\t"
	"xorl %%ecx,%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LB(4) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LB(3) "\n\t"

	"movl 4(%2),%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LF(21) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(21) "\n\t"
	"xorl %%ecx,%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LB(41) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LB(31) "\n\t"

	"movl 8(%2),%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LF(22) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(22) "\n\t"
	"xorl %%ecx,%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LB(42) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LB(32) "\n\t"

	"movl 12(%2),%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LF(23) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jnz " LF(23) "\n\t"
	"xorl %%ecx,%%edx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"addl %%edx,%%edi\n\t"
	"jnc " LB(43) "\n\t"
	"xorl %%edx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jz " LB(1) "\n\t"
	"jmp " LB(33) "\n"

LL(23)	"\taddl $4,%2\n"	/* correct pointer increments */
LL(22)	"\taddl $4,%2\n"
LL(21)	"\taddl $4,%2\n"

	/* What remains to do is to test which byte the NULL char is and
	 * whether the searched character appears in one of the bytes
	 * before.  A special case is that the searched byte maybe NULL.
	 * In this case a pointer to the terminating NULL char has to be
	 * returned.  */

LL(20)	"\tcmpb %%cl,%%dl\n\t"	/* is first byte the searched one ? */
	"jne " LF(24) "\n\t"	/* no, branch */
	"movl %2,%0\n"		/* remember position */
LL(24)	"\ttestb %%dl,%%dl\n\t" /* is NULL char ? */
	"jz " LF(2) "\n\t"	/* yes, then end */
	"cmpb %%cl,%%dh\n\t"
	"jne " LF(25) "\n\t"
	"leal 1(%2),%0\n"
LL(25)	"\ttestb %%dh,%%dh\n\t"
	"jz " LF(2) "\n\t"
	"shrl $16,%%edx\n\t"
	"cmpb %%cl,%%dl\n\t"
	"jne " LF(26) "\n\t"
	"leal 2(%2),%0\n"
LL(26)	"\ttestb %%dl,%%dl\n\t"
	"jz " LF(2) "\n\t"
	"cmpb %%cl,%%dh\n\t"
	"jne " LF(2) "\n\t"
	"leal 3(%2),%0\n"

LL(2)
	: "=a" (__res) : "0" (0), "S" (str), "c" (ch) : "cx","dx","di","si");
return __res;
#endif /* CHECKER */
}

#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (strrchr, rindex);
#endif
