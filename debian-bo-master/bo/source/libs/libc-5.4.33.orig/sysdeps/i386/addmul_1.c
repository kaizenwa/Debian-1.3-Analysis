/* i80386 __mpn_addmul_1 -- Multiply a limb vector with a limb and add
   the result to a second limb vector.

Copyright (C) 1992, 1994 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
License for more details.

You should have received a copy of the GNU Library General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

/*
   INPUT PARAMETERS
   res_ptr	(sp + 4)
   s1_ptr	(sp + 8)
   size		(sp + 12)
   s2_limb	(sp + 16)
*/

#include <asm-ops.h>
#include <gmp.h>

mp_limb
__mpn_addmul_1(mp_ptr res_ptr, mp_srcptr s1_ptr, mp_size_t size,
	       mp_limb s2_limb)
{
register mp_limb __res;
__asm__(
#if defined(__PIC__) || defined (__pic__)
	"pushl	%%ebx\n\t"
	"movl	%%eax,%%ebx\n\t"
#endif
	"pushl	%%ebp\n\t"

	"leal	(%1,%3,4),%1\n\t"
	"leal	(%2,%3,4),%2\n\t"
	"negl	%3\n\t"
	"xorl	%%ebp,%%ebp\n\t"
	ALIGN "\n"
"Loop:\tmovl	(%2,%3,4),%0\n\t"
	"mull	%%ebx\n\t"
	"addl	%%ebp,%0\n\t"
	"adcl	$0,%%edx\n\t"
	"addl	%0,(%1,%3,4)\n\t"
	"adcl	$0,%%edx\n\t"
	"movl	%%edx,%%ebp\n\t"

	"incl	%3\n\t"
	"jnz	Loop\n\t"
	"movl	%%ebp,%0\n\t"

	"popl	%%ebp\n\t"
#if defined(__PIC__) || defined (__pic__)
	"popl	%%ebx"
	: "=a" (__res)
	: "D" (res_ptr),"S" (s1_ptr),"c" (size),"0" (s2_limb)
	: "di","si","bx");
#else
	: "=a" (__res)
	: "D" (res_ptr),"S" (s1_ptr),"c" (size),"b" (s2_limb)
	: "di","si","bx");
#endif
return __res;
}
