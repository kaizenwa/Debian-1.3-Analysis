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

	.file	"cosh.S"

#ifdef __ELF__
#define _cosh	cosh
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
	.double 0d5.0000000000000000000000e-01

	.text
	.globl	_cosh
#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif

_cosh:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$16,%esp
#ifdef __PIC__
	pushl	%ebx
	call	.L2
.L2:
	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-.L2],%ebx
#endif
	fldl	8(%ebp)
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
	fdivp	%st,%st(1)
	faddp	%st,%st(1)

#ifdef __PIC__
	fldl	.LC0@GOTOFF(%ebx)
#else
#ifdef __ELF__
	fldl	.LC0
#else
	fldl	LC0
#endif
#endif
	fmulp	%st,%st(1)

/* Start of overflow checking: if result is INF or -INF, set errno to ERANGE */
	fstl	-16(%ebp)
	movl	-16(%ebp),%eax
	testl	%eax,%eax
	jnz	.L9
	movl	-12(%ebp),%eax
	andl	$0x7FFFFFFF,%eax
	cmpl	$0x7FF00000,%eax
	jne	.L9
	movl	$34,%eax
#ifdef __PIC__
	movl	%eax,errno@GOT(%ebx)
#else
	movl	%eax,errno
#endif
.L9:
/* End of overflow checking */

#ifdef __PIC__
	popl	%ebx
#endif
	movl	%ebp,%esp
	popl	%ebp
	ret


#ifdef __ELF__
	.type	cosh,@function
#endif
