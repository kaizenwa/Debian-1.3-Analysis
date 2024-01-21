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

/* History:15,24 */

	.file	"expm1.S"

#ifdef __ELF__
#define _expm1	expm1
#endif

	.text
	.globl	_expm1
#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif

_expm1:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$8,%esp
	fldl	8(%ebp)
	fldl2e
	fmulp	%st,%st(1)
	fstcw	-4(%ebp)
	fstcw	-8(%ebp)
	andw	$0xf3ff,-4(%ebp)
	fldcw	-4(%ebp)
	fldl	%st(0)
	frndint
	fldcw	-8(%ebp)
	fxch	%st(1)
	fsub	%st(1),%st
	f2xm1

	fscale
	fxch	%st(1)
	fld1
	fscale
	fld1
	fsubp	%st,%st(1)
	fstp	%st(1)
	fsubrp	%st,%st(1)

	movl	%ebp,%esp
	popl	%ebp
	ret

#ifdef __ELF__
	.type expm1,@function
#endif
