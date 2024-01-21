/* Copyright (C) 1993  Hongjiu Lu
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

	.file	"tanhl.S"

#ifdef __ELF__
#define _tanhl	tanhl
#endif

	.text
	.globl	_tanhl
#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif

_tanhl:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$8,%esp
	fldt	8(%ebp)
	ftst
	fstsw	%ax
	sahf
	jna	L3
	fchs
	xorl	%eax,%eax
	incl	%eax
	jmp	L1

L3:
	xorl	%eax,%eax

L1:
	fld	%st(0)
	faddp	%st,%st(1)
	fldl2e
	fmulp	%st,%st(1)
	fst	%st(1)
	fstcw	-4(%ebp)
	fstcw	-8(%ebp)
	fwait
	andw	$0xf3ff,-4(%ebp)
	fldcw	-4(%ebp)
	frndint
	fldcw	-8(%ebp)
	fst	%st(2)
	fsubrp	%st,%st(1)
	f2xm1
	fld1
	faddp	%st,%st(1)
	fscale
	fst	%st(1)

	fld1
	faddp	%st,%st(1)
	fxch	%st(1)
	fld1
	fsubrp	%st,%st(1)
	fdivp	%st,%st(1)

	testl	%eax,%eax
	jna	L2
	fchs

L2:
	movl	%ebp,%esp
	popl	%ebp
	ret

#ifdef __ELF__
	.type tanhl,@function
#endif
