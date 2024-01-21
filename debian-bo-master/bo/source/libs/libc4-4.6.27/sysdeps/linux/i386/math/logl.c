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
#include "mathl.h"
#include <errno.h>
#include <fp.h>

static inline long double
domain ( long double x)
{
    /* perror("logl"); */
    if (x < 0.)
      return __infnanl( EDOM);
    else /* i.e. x== 0. */
      return __infnanl(-ERANGE);
}

long double
DEFUN(logl, (x), long double x)
{
  if (x <= 0.0L) return domain (x);

  __asm__ __volatile__ ("fldln2\n\t"
			"fxch %%st(1)\n\t"
			"fyl2x"
			:"=t" (x) : "0" (x));
  return x;
}
