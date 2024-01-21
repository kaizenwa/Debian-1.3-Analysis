/* Copyright (C) 1993  Hongjiu Lu
   Changed by Andreas Schwab for long double
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

#include <ansidecl.h>
#define __NO_MATH_INLINES
#include <math.h>

/*  Stores binary exponent of d in e, and returns whole fraction of d
 *   (with binary exponent of 0) (special case for d=0)
 */
long double
DEFUN (frexpl, (d, e),
       long double d AND int *e)
{
  return __frexpl (d, e);
}

long double
DEFUN (ldexpl, (x, exp),
       long double x AND int exp)
{
  return __ldexpl (x, exp);
}
