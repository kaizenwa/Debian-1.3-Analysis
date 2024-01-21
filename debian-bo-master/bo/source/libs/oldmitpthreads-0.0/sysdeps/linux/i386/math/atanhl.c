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

/* 
 Original Code by John C. Bowman (bowman@hagar.ph.utexas.edu).
*/


#include <ansidecl.h>
#include <errno.h>
#include <math.h>

long double
DEFUN(atanhl, (x), long double x)
{
  long double y = fabsl( x);
  if (y >= 1.0 ) {
    if (y==1.0)
      return __infnanl( ERANGE);
    else
      return __infnanl( (x > 0? EDOM:-EDOM));
  } 
  return copysignl( 0.5*log1pl(-2.0*y/(1.0+y)), x);
}
