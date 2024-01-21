/* i80386 __mpn_mul_1 -- Multiply a limb vector with a limb and store
   the result in a second limb vector.

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

#include "asm-ops.h"
#include "gmp.h"

mp_limb
__mpn_mul_1(mp_ptr res_ptr, mp_srcptr s1_ptr, mp_size_t size, mp_limb s2_limb)
{
register mp_limb __res;
__asm__(
#if defined(__PIC__) || defined (__pic__)
	"pushl	%%ebx\n\t"
	"movl	%%eax,%%ebx\n\t"
#endif 
	"pushl	%%ebp\n\t"

	"leal	(%%edi,%%ecx,4),%%edi\n\t"
	"leal	(%%esi,%%ecx,4),%%esi\n\t"
	"negl	%%ecx\n\t"
	"xorl	%%edx,%%edx\n\t"

	ALIGN "\n"
"Loop:\t"
	"movl	%%edx,%%ebp\n\t"
	"movl	(%%esi,%%ecx,4),%%eax\n\t"

	"mull	%%ebx\n\t"

	"addl	%%ebp,%%eax\n\t"

	"adcl	$0,%%edx\n\t"
	"movl	%%eax,(%%edi,%%ecx,4)\n\t"

	"incl	%%ecx\n\t"
	"jnz	Loop\n\t"

	"movl	%%edx,%%eax\n\t"

	"popl	%%ebp\n\t"

#if defined(__PIC__) || defined (__pic__)
	"popl	%%ebx"
	:"=a" (__res)
	:"D" (res_ptr),"S" (s1_ptr),"c" (size), "0" (s2_limb)
	:"bx","di","si");
#else
	:"=a" (__res)
	:"D" (res_ptr),"S" (s1_ptr),"c" (size), "b" (s2_limb)
	:"bx","di","si");
#endif
return __res;
}
