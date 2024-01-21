/* memchr (str, ch, n) -- Return pointer to first occurrence of CH in STR less
   than N.
   For Intel 80x86, x>=3.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>
   Optimised a little by Alan Modra <Alan@SPRI.Levels.UniSA.Edu.Au>

   This version is developed using the same algorithm as the fast C
   version which carries the following introduction:

   Based on strlen implemention by Torbjorn Granlund (tege@sics.se),
   with help from Dan Sahlin (dan@sics.se) and
   commentary by Jim Blandy (jimb@ai.mit.edu);
   adaptation to memchr suggested by Dick Karpinski (dick@cca.ucsf.edu),
   and implemented by Roland McGrath (roland@ai.mit.edu).

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

#ifdef	__GNUC__

#include "asm-ops.h"

PTR
memchr(CONST PTR str, int c, size_t len)
{
PTR __res;
__asm__(
	"cmpl $4,%3\n\t"
	"jb " LF(3) "\n\t"	/* if len < 4, check byte by byte */

	"movb %%dl,%%dh\n\t"	/* now %dx is c|c */
	"movl %%edx,%%edi\n\t"
	"shll $16,%%edx\n\t"	/*     %edx is c|c|0|0 */
	"movw %%di,%%dx\n\t"	/*     %edx is c|c|c|c */

	/* Better performance can be achieved if the word (32 bit) memory
	 * access is aligned on a four-byte-boundary.  So process first
	 * bytes one by one until boundary is reached.	Don't use a loop
	 * for better performance.  */

	"testb $3,%0\n\t"	/* correctly aligned ? */
	"je " LF(2) "\n\t"	/* yes => begin loop */
	"cmpb %%dl,(%0)\n\t"	/* compare byte */
	"je " LF(9) "\n\t"	/* target found => return */
	"incl %0\n\t"		/* increment source ptr */
	"decl %3\n\t"		/* decrement len counter */
	"je " LF(4) "\n\t"	/* len==0 => return NULL */

	"testb $3,%0\n\t"	/* ditto */
	"je " LF(2) "\n\t"
	"cmpb %%dl,(%0)\n\t"
	"je " LF(9) "\n\t"
	"incl %0\n\t"
	"decl %3\n\t"
	"je " LF(4) "\n\t"

	"testb $3,%0\n\t"	/* ditto */
	"je " LF(2) "\n\t"
	"cmpb %%dl,(%0)\n\t"
	"je " LF(9) "\n\t"
	"incl %0\n\t"
	"decl %3\n\t"
	/* no test for %3==0 here, because this is done in the loop head */
	"jmp " LF(2) "\n\t"

      /* We exit the loop if adding MAGIC_BITS to LONGWORD fails to
	 change any of the hole bits of LONGWORD.

	 1) Is this safe?  Will it catch all the zero bytes?
	 Suppose there is a byte with all zeros.  Any carry bits
	 propagating from its left will fall into the hole at its
	 least significant bit and stop.  Since there will be no
	 carry from its most significant bit, the LSB of the
	 byte to the left will be unchanged, and the zero will be
	 detected.

	 2) Is this worthwhile?  Will it ignore everything except
	 zero bytes?  Suppose every byte of LONGWORD has a bit set
	 somewhere.  There will be a carry into bit 8.	If bit 8
	 is set, this will carry into bit 16.  If bit 8 is clear,
	 one of bits 9-15 must be set, so there will be a carry
	 into bit 16.  Similarly, there will be a carry into bit
	 24.  If one of bits 24-31 is set, there will be a carry
	 into bit 32 (=carry flag), so all of the hole bits will
	 be changed.

	 3) But wait!  Aren't we looking for C, not zero?
	 Good point.  So what we do is XOR LONGWORD with a longword,
	 each of whose bytes is C.  This turns each byte that is C
	 into a zero.  */


	/* each time round the main loop processes 16 bytes. */

	ALIGN
	/* Get word (= 4 bytes) in question) */
LL(1)	"\tmovl (%0),%%ecx\n\t"

	/* magic value */
	"movl $0xfefefeff,%%edi\n\t"

	/* XOR with word c|c|c|c => bytes of str == c, are now 0 */
	"xorl %%edx,%%ecx\n\t"

	/* Add the magic value to the word.  We get carry bits reported
	   for each byte which != 0  */
	"addl %%ecx,%%edi\n\t"

	/* According to the algorithm we had to reverse the effect of the
	 * XOR first and then test the overflow bits.  But because the
	 * following XOR would destroy the carry flag and it would (in a
	 * representation with more than 32 bits) not alter then last
	 * overflow, we can now test this condition.  If no carry is signaled
	 * no overflow must have occured in the last byte => it was 0.	*/
	"jnc " LF(8) "\n\t"

	/* We are only interested in carry bits that change due to the
	   previous add, so remove original bits */
	"xorl %%ecx,%%edi\n\t"	/* ((word^charmask)+magic)^(word^charmask) */

	/* Now test for the other three overflow bits.	*/
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"

	/* If at least one bit is set a matching byte was detected.  */
	"jne " LF(8) "\n\t"

	/* this process is unfolded four times for better performance.
	 * we don't increment the source pointer each time.  Instead we
	 * use offsets and increment by 16 in each run of the loop.  But
	 * before probing for the matching byte we need some extra code
	 * (following LL(13) below).  Even the len can be compared with
	 * constants instead of decrementing each time.  */

	"movl 4(%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"xorl %%edx,%%ecx\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(7) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jne " LF(7) "\n\t"

	"movl 8(%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"xorl %%edx,%%ecx\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(6) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jne " LF(6) "\n\t"

	"movl 12(%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"xorl %%edx,%%ecx\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(5) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jne " LF(5) "\n\t"

	/* Decrement both counters for a full round, i.e. 16 bytes.  */
	"addl $16,%0\n"
LL(2)	"\tsubl $16,%3\n\t"
	/* If less than 16 bytes remain to test do them outside the loop.  */
	"jae " LB(1) "\n\t"


	"cmpl $4-16,%3\n\t"		/* rest < 4 bytes ? */
	"jb " LF(3) "\n\t"		/* yes, than test byte by byte */

	"movl (%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"xorl %%edx,%%ecx\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(8) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jne " LF(8) "\n\t"
	"addl $4,%0\n\t"

	"cmpl $8-16,%3\n\t"		/* rest < 8 bytes ? */
	"jb " LF(3) "\n\t"		/* yes, than test byte by byte */

	"movl (%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"xorl %%edx,%%ecx\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(8) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jne " LF(8) "\n\t"
	"addl $4,%0\n\t"

	"cmpl $12-16,%3\n\t"		/* rest < 12 bytes ? */
	"jb " LF(3) "\n\t"		/* yes, than test byte by byte */

	"movl (%0),%%ecx\n\t"
	"movl $0xfefefeff,%%edi\n\t"
	"xorl %%edx,%%ecx\n\t"
	"addl %%ecx,%%edi\n\t"
	"jnc " LF(8) "\n\t"
	"xorl %%ecx,%%edi\n\t"
	"orl $0xfefefeff,%%edi\n\t"
	"incl %%edi\n\t"
	"jne " LF(8) "\n\t"
	"addl $4,%0\n"

	/* Check the remaining bytes one by one.  */
LL(3)	"\tandl $3,%3\n\t"
	"jz " LF(4) "\n\t"
	"cmpb %%dl,(%0)\n\t"
	"je " LF(9) "\n\t"
	"incl %0\n\t"
	"decl %3\n\t"
	"je " LF(4) "\n\t"
	"cmpb %%dl,(%0)\n\t"
	"je " LF(9) "\n\t"
	"incl %0\n\t"
	"decl %3\n\t"
	"je " LF(4) "\n\t"
	"cmpb %%dl,(%0)\n\t"
	"je " LF(9) "\n"
LL(4)	"\txorl %0,%0\n\t"
	"jmp " LF(9) "\n"


	/* add missing source pointer increments */
LL(5)	"\taddl $4,%0\n"
LL(6)	"\taddl $4,%0\n"
LL(7)	"\taddl $4,%0\n"

	/* Test for the matching byte.	%ecx contains a NULL char in that
	 * byte which contains the searched character.	*/

LL(8)	"\ttestb %%cl,%%cl\n\t"
	"jz " LF(9) "\n\t"
	"incl %0\n\t"
	"testb %%ch,%%ch\n\t"
	"jz " LF(9) "\n\t"
	"incl %0\n\t"
	"testl $0xff0000,%%ecx\n\t"
	"jz " LF(9) "\n\t"
	"incl %0\n"
	/* No further test needed because we know one of them matches.	*/

LL(9)
	: "=a" (__res) : "d" (c), "0" (str), "S" (len) : "cx","di","dx","si");
return __res;
}

#else
#include <sysdeps/generic/memchr.c>
#endif
