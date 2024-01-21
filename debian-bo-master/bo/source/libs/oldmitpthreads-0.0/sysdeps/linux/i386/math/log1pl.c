/* Copyright (C) 1993  Hongjiu Lu
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
#include <errno.h>
#include <math.h>

static inline long double
domain ( long double x)
{
    /* perror("log1pl"); */
    if (x < -1.0)
      return __infnanl( EDOM);
    else /* i.e. x == -1.0 */
      return __infnanl(-ERANGE);
}

long double
DEFUN(log1pl, (x), long double x)
{
  if (x <= -1.0L) return domain (x);

#ifndef CYRIX_83D87
  /* lousy 387 can only handle abs(x) < 1-sqrt(2)/2 in fly2xp1. */
  if (fabsl (x) < 0.2929)
  {
#endif
  	__asm__ __volatile__ ("fldln2\n\t"
			"fxch %%st(1)\n\t"
			"fyl2xp1"
			:"=t" (x) : "0" (x));
#ifndef CYRIX_83D87
  }
  else
  {
  	x += 1.0L;
  	__asm__ __volatile__ ("fldln2\n\t"
			"fxch %%st(1)\n\t"
			"fyl2x"
			:"=t" (x) : "0" (x));
  }
#endif
  return x;
}
