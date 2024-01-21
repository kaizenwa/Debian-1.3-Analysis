/* Copyright (C) 1991, 1995 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <stdlib.h>
#include <random.h>

#undef	random_r
#undef	srandom_r
#undef	initstate_r
#undef	setstate_r

#include <gnu-stabs.h>

function_alias(random_r, __random_r, int, (retval, rand_data),
	       DEFUN(random_r, (retval, rand_data),
		     long int *retval AND struct random_data *rand_data))
function_alias(srandom_r, __srandom_r, int, (seed, rand_data),
	       DEFUN(srandom_r, (seed, rand_data),
		     unsigned int seed AND struct random_data *rand_data))
function_alias(initstate_r,
	       __initstate_r, int, (seed, buf, size, retval, rand_data),
	       DEFUN(initstate_r, (seed, buf, size, retval, rand_data),
		     unsigned int seed AND PTR buf AND size_t size AND
		     PTR *retval AND struct random_data *rand_data))
function_alias(setstate_r, __setstate_r, int, (buf, retval, rand_data),
	       DEFUN(setstate_r, (buf, retval, rand_data),
		     PTR buf AND PTR * retval
		     AND struct random_data *rand_data))

