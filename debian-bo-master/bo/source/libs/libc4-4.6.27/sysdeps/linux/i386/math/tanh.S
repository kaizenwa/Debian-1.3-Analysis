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

	.file	"tanh.S"

#ifdef __ELF__
#define _tanh	tanh
#endif

	.text
	.globl	_tanh
#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif

_tanh:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$8,%esp
	fldl	8(%ebp)
	ftst
	fstsw	%ax
	sahf
#ifdef __ELF__
	jna	.L3
#else
	jna	L3
#endif
	fchs
	xorl	%eax,%eax
	incl	%eax
#ifdef __ELF__
	jmp	.L1
#else
	jmp	L1
#endif

#ifdef __ELF__
.L3:
#else
L3:
#endif
	xorl	%eax,%eax

#ifdef __ELF__
.L1:
#else
L1:
#endif
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
#ifdef __ELF__
	jna	.L2
#else
	jna	L2
#endif
	fchs

#ifdef __ELF__
.L2:
#else
L2:
#endif
	movl	%ebp,%esp
	popl	%ebp
	ret

#ifdef __ELF__
.type tanh,@function
#endif
