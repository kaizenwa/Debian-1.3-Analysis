/* Copyright (C) 1996  Olaf Flebbe 
                O.Flebbe@science-computing.uni-tuebingen.de
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
 Idea by John C. Bowman (bowman@hagar.ph.utexas.edu).
*/

#include <math.h>

#define M_LN2L 0.69314718055994530942L

long double asinhl(const long double x)
{
	long double arg = x, y=fabsl(arg);
	
	if ( y > 1e30L) {
	  /* 
	     Make sure y*y does not overflow; difference is beyond
             machine precision
	  */
	  return copysignl( M_LN2L + logl( y), x);
	} else {
	  long double z=y*y;

	  return copysignl( log1pl( z/(sqrtl(z+1.0)+1.0)+y), x);
	}
}
