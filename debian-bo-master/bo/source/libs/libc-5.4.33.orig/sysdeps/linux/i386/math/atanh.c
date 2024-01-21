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

/* 
 Original Code by John C. Bowman (bowman@hagar.ph.utexas.edu).
*/


#include <ansidecl.h>
#include <math.h>
#include <errno.h>

double
DEFUN(atanh, (x), double x)
{
  double y = fabs( x);
  if (y >= 1.0 ) {
    if (y==1.0)
      return __infnan( ERANGE);
    else
      return __infnan( (x > 0? EDOM:-EDOM));
  } 
  return copysign( 0.5*log1p(-2.0*y/(1.0+y)), x);
}

