/* Copyright (C) 1993  Hongjiu Lu
   Changed by Olaf Flebbe for long double
   Changed by Andreas Schwab for m68k
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.  */

#include <ansidecl.h>
#include <fp.h>
#include <errno.h>
#define __NO_MATH_INLINES
#include <math.h>

long double
DEFUN(asinhl, (x), long double x)
{
  return __m81_u (logl) (x + __m81_u (sqrtl) (x * x + 1.0L));
}
