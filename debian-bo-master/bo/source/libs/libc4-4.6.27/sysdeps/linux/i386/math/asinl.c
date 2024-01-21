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
#include <fp.h>
#include <errno.h>
#include "mathl.h"

long double
DEFUN(asinl, (x), long double x)
{
  long double y;

  if (x <= -1.0L ) {
    if (x == -1.0L) {
      return -1.57079632679489661923L;
    }
    return __infnanl( EDOM);
  } 

  if (x >= 1.0L ) {
    if (x == 1.0L) {
      return 1.57079632679489661923L;
    }
    return __infnanl( EDOM);
  } 

  y = sqrtl (1.0L - x * x);
  __asm__ __volatile__ ("fpatan"
			:"=t" (y) : "0" (y), "u" (x));
  return y;
}
