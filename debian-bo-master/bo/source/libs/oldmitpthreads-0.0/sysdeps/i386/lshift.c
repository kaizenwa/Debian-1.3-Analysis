/* i80386 __mpn_lshift -- 
 *
 * Copyright (C) 1992, 1994 Free Software Foundation, Inc.
 *
 * This file is part of the GNU MP Library.
 *
 * The GNU MP Library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * The GNU MP Library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 * INPUT PARAMETERS
 * res_ptr	(sp + 4)
 * s_ptr	(sp + 8)
 * size		(sp + 12)
 * cnt		(sp + 16)
 */

#include "asm-ops.h"
#include "gmp.h"

mp_limb
__mpn_lshift(mp_ptr res_ptr, mp_srcptr s_ptr, mp_size_t size, unsigned int cnt)
{
register mp_limb __res;
__asm__(
#if defined(__PIC__) || defined (__pic__)
	"pushl	%%ebx\n\t"
#endif
	"subl	$4,%%esi\n\t"		/* adjust s_ptr */

	"movl	(%%esi,%%edx,4),%%ebx\n\t" /* read most significant limb */
	"xorl	%%eax,%%eax\n\t"
	"shldl	%%cl,%%ebx,%%eax\n\t"	/* compute carry limb */
	"decl	%%edx\n\t"
	"jz	Lend\n\t"
	"pushl	%%eax\n\t"		/* push carry limb onto stack */
	"testb	$1,%%edx\n\t"
	"jnz	L1\n\t"			/* enter loop in the middle */
	"movl	%%ebx,%%eax\n\t"

	ALIGN	"\n"	
"Loop:"	"\tmovl	(%%esi,%%edx,4),%%ebx\n\t" /* load next lower limb */
	"shldl	%%cl,%%ebx,%%eax\n\t"	/* compute result limb */
	"movl	%%eax,(%%edi,%%edx,4)\n\t" /* store it */
	"decl	%%edx\n"
"L1:"	"\tmovl	(%%esi,%%edx,4),%%eax\n\t"
	"shldl	%%cl,%%eax,%%ebx\n\t"
	"movl	%%ebx,(%%edi,%%edx,4)\n\t"
	"decl	%%edx\n\t"
	"jnz	Loop\n\t"

	"shll	%%cl,%%eax\n\t"		/* compute least significant limb */
	"movl	%%eax,(%%edi)\n\t"	/* store it */

	"popl	%%eax\n\t"		/* pop carry limb */
	"jmp	Lout\n"

"Lend:"	"\tshll	%%cl,%%ebx\n\t"		/* compute least significant limb */
	"movl	%%ebx,(%%edi)\n"	/* store it */
"Lout:"
#if defined(__PIC__) || defined (__pic__)
	"popl	%%ebx\n\t"
#endif
	:"=a" (__res)
	:"D" (res_ptr),"S" (s_ptr),"d" (size),"c" (cnt)
	:"bx","di","si");
return __res;
}
