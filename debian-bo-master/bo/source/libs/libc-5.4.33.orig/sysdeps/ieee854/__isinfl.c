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

#undef __isinfl

/* Return 0 if VALUE is finite or NaN, +1 if it
   is +Infinity, -1 if it is -Infinity.  */
int
DEFUN(__isinfl, (value), long double value)
{
  union ieee854_double *u = (union ieee854_double *) &value;

  /* An IEEE 854 infinity has an exponent with the
     maximum possible value and a mantissa of 0x8000000000000000.
     For the 68881 the integer bit is dont-care.  */
  if ((u->ieee.exponent & 0x7fff) == 0x7fff &&
#ifdef __mc68000__
      (u->ieee.mantissa0 & 0x7fffffff) == 0 &&
#else
      u->ieee.mantissa0 == 0x80000000 &&
#endif
      u->ieee.mantissa1 == 0)
    return u->ieee.negative ? -1 : 1;

  return 0;
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__isinfl, isinfl);
#endif
