/* Copyright (C) 1991, 1992, 1995 Free Software Foundation, Inc.
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
#include <math.h>
#include "ieee854.h"

#undef __isqnanl

/* Return nonzero if VALUE is not a number and is quiet.  */
int
DEFUN(__isqnanl, (value), long double value)
{
  union ieee854_double *u = (union ieee854_double *) &value;

  /* IEEE 854 NaN's have the maximum possible
     exponent and a nonzero mantissa.  If it
     is quiet the highest bit in the mantissa
     is set.  */
  return ((u->ieee_nan.exponent & 0x7fff) == 0x7fff &&
	  (u->ieee_nan.quiet_nan == 1) && (u->ieee_nan.one == 1));
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__isqnanl, isqnanl);
#endif
