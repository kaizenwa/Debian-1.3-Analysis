/* Copyright (C) 1993  Hongjiu Lu
   Changed by Olaf Flebbe for long double
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

	.file	"asinhl.S"

	.text
#ifdef __ELF__
#define _asinhl asinhl
#endif
	.globl	_asinhl

#ifdef	__i486__
	.align	4,0x90
#else
	.align	2,0x90
#endif
_asinhl:
	fldln2
	fldt	4(%esp)
	fld	%st
	fmul	%st(1),%st
	fld1
	faddp	%st,%st(1)
	fsqrt   
	faddp	%st,%st(1)
	fyl2x
	ret

#ifdef __ELF__
	.type asinhl,@function
#endif
