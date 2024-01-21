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
#include <math.h>
#include <errno.h>

double
DEFUN(acos, (x), double x)
{
  if (x <= -1.0 ) {
    if (x == -1.0) {
      return M_PI;
    }
    return __infnan( EDOM);
  } 

  if (x >= 1.0 ) {
    if (x == 1.0) {
      return 0.0;
    }
    return __infnan( EDOM);
  } 

  x = sqrt ((1.0 - x) / (1.0 + x));
  __asm__ __volatile__ ("fpatan"
			:"=t" (x) : "0" (1.0), "u" (x));
  return x + x;
}
