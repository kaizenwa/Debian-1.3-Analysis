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

	.file	"fmodl.S"
	.text
#ifdef __ELF__
#define _fmodl fmodl
#endif

	.globl	_fmodl

#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
_fmodl:
	fldt	4(%esp)
	ftst
	fnstsw	%ax
	sahf
	jz	zero1
	fldt	16(%esp)
	ftst
	fnstsw	%ax
	sahf
	jz	zero2
	fxch	%st(1)
loop:
	fprem
	fnstsw	%ax
	sahf
	jpe	loop
	fstp	%st(1)
	ret
	
#ifdef __i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
zero2:
	fstp	%st(0)
zero1:
	ret	

#ifdef __ELF__
	.type fmodl,@function
#endif
