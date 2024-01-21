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
#include "ieee754.h"

#undef __issnan

/* Return nonzero if VALUE is not a number and is signaling.  */
int
DEFUN(__issnan, (value), double value)
{
  union ieee754_double *u = (union ieee754_double *) &value;

  /* IEEE 754 NaN's have the maximum possible
     exponent and a nonzero mantissa.  If it
     is signaling the highest bit in the mantissa
     is cleared.  */
  return ((u->ieee_nan.exponent & 0x7ff) == 0x7ff &&
	  (u->ieee_nan.quiet_nan == 0) &&
	  (u->ieee_nan.mantissa0 != 0 || u->ieee_nan.mantissa1 != 0));
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__issnan, issnan);
#endif
