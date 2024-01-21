/*
** Copyright (C) 1991 DJ Delorie, 24 Kirsten Ave, Rochester NH 03867-2954
**
** Modified O.ROBERT 24, Avenue de Verdun 92170 VANVES, FRANCE
**
** E-mail: roberto@germinal.ibp.fr
**
** This file is distributed under the terms listed in the document
** "copying.dj", available from DJ Delorie at the address above.
** A copy of "copying.dj" should accompany this file; if not, a copy
** should be available from where this file was obtained.  This file
** may not be distributed without a verbatim copy of "copying.dj".
**
** This file is distributed WITHOUT ANY WARRANTY; without even the implied
** warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
** or	03-Apr-1991	corrected bug about argument zero to pow
**			fyl2x didn't like it
*/

/* Modified by H.J. Lu for Linux, 1992 */
/* Modified by J.-H. Rechtien, 2.2.1994 */

/* History:15,24 */

	.file	"exp.S"

#ifdef __ELF__
#define _pow10	pow10
#endif

#ifdef __ELF__
	.section	.rodata
#else
	.text
#endif

#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif

#ifdef __ELF__
.LC0:
#else
LC0:
#endif
	.double	0d1.0000000000000000000e+00

	.text
	.globl	_pow10
#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif

_pow10:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$8,%esp
#ifdef __PIC__
	pushl	%ebx
	call	.L1
.L1:
	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-.L1],%ebx
#endif
	fldl	8(%ebp)
	fldl2t
	fmulp	%st,%st(1)
	fstcw	-4(%ebp)
	fstcw	-8(%ebp)
	fwait
	andw	$0xf3ff,-4(%ebp)
	fldcw	-4(%ebp)
	fldl	%st(0)
	frndint
	fldcw	-8(%ebp)
	fxch	%st(1)
	fsub	%st(1),%st
	f2xm1
	fwait			
#ifdef __PIC__
	faddl	.LC0@GOTOFF(%ebx)
#else
#ifdef __ELF__
	faddl	.LC0
#else
	faddl	LC0
#endif
#endif
	fscale
	fstp	%st(1)

#ifdef __PIC__
	popl	%ebx
#endif
	movl	%ebp,%esp
	popl	%ebp
	ret

#ifdef __ELF__
	.type	pow10,@function
#endif
