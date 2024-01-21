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
Library General Public License for more details.  */

#include <ansidecl.h>
#include <math.h>
#include <errno.h>

/* Return the sqrt root of x.  */

long double
DEFUN(sqrtl, (x), long double x)
{
  const long double zero = 0.0L;

  if (x >= zero) {
    if (x != zero) {
    __asm__ __volatile__ ("fsqrt"
			  :"=t" (x) : "0" (x));
    }
    return x;
  }
  else return __infnanl( EDOM);
}
