/* i80386 __mpn_add_n -- Add two limb vectors of the same length > 0 and store
   sum in a third limb vector.

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
  the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


  INPUT PARAMETERS
  res_ptr	(sp + 4)
  s1_ptr	(sp + 8)
  s2_ptr	(sp + 12)
  size		(sp + 16)
*/

#include "asm-ops.h"
#include "gmp.h"


mp_limb
__mpn_add_n(mp_ptr res_ptr, mp_srcptr s1_ptr, mp_srcptr s2_ptr, mp_size_t size)
{
register mp_limb __res;
__asm__(
	"movl	%%ecx,%%eax\n\t"
	"shrl	$3,%%ecx\n\t"		/* compute loop count for unrolled loop */
	"negl	%%eax\n\t"
	"andl	$7,%%eax\n\t"		/* get index where to start loop */
	"jz	Loop\n\t"		/* necessary special case for 0 */
	"incl	%%ecx\n\t"		/* adjust loop count */
	"shll	$2,%%eax\n\t"		/* adjustment for pointers... */
	"subl	%%eax,%%edi\n\t"	/* ... since they are offset ... */
	"subl	%%eax,%%esi\n\t"	/* ... by a constant when we enter ... */
	"subl	%%eax,%%edx\n\t"	/* ... the loop */
	"shrl	$2,%%eax\n\t"		/* restore previous value */
#if defined(__PIC__) || defined(__pic__)
	"pushl	%%ebx\n\t"		/* save PIC register */
	"call	here\n"
"here:\tpopl	%%ebx\n\t"		/* load EIP into EBX */
	"leal	Loop-3-here(%%eax,%%eax,8),%%eax\n\t"
	"addl	%%ebx,%%eax\n\t"	/* calc start addr in loop */
	"popl	%%ebx\n\t"		/* restore PIC register */
#else
	"leal	Loop-3(%%eax,%%eax,8),%%eax\n\t"	/* calc start addr in loop */
#endif
	"jmp	*%%eax\n\t"		/* jump into loop */
	ALIGN	"\n"
"Loop:\tmovl	(%%esi),%%eax\n\t"
	"adcl	(%%edx),%%eax\n\t"
	"movl	%%eax,(%%edi)\n\t"
	"movl	4(%%esi),%%eax\n\t"
	"adcl	4(%%edx),%%eax\n\t"
	"movl	%%eax,4(%%edi)\n\t"
	"movl	8(%%esi),%%eax\n\t"
	"adcl	8(%%edx),%%eax\n\t"
	"movl	%%eax,8(%%edi)\n\t"
	"movl	12(%%esi),%%eax\n\t"
	"adcl	12(%%edx),%%eax\n\t"
	"movl	%%eax,12(%%edi)\n\t"
	"movl	16(%%esi),%%eax\n\t"
	"adcl	16(%%edx),%%eax\n\t"
	"movl	%%eax,16(%%edi)\n\t"
	"movl	20(%%esi),%%eax\n\t"
	"adcl	20(%%edx),%%eax\n\t"
	"movl	%%eax,20(%%edi)\n\t"
	"movl	24(%%esi),%%eax\n\t"
	"adcl	24(%%edx),%%eax\n\t"
	"movl	%%eax,24(%%edi)\n\t"
	"movl	28(%%esi),%%eax\n\t"
	"adcl	28(%%edx),%%eax\n\t"
	"movl	%%eax,28(%%edi)\n\t"
	"leal	32(%%edi),%%edi\n\t"
	"leal	32(%%esi),%%esi\n\t"
	"leal	32(%%edx),%%edx\n\t"
	"decl	%%ecx\n\t"
	"jnz	Loop\n\t"

	"sbbl	%%eax,%%eax\n\t"
	"negl	%%eax\n\t"

	:"=a" (__res)
	:"D" (res_ptr),"S" (s1_ptr), "d" (s2_ptr), "c" (size)
	:"cx","dx","di","si");
return __res;
}
