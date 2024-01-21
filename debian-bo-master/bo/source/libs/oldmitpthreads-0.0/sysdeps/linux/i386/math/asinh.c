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

double asinh(const double x)
{
	double arg = x, y=fabs(arg);
	
	if ( y > 1e30) {
	  /* 
	     Make sure y*y does not overflow; difference is beyond
             machine precision
	  */
	  return copysign( M_LN2 + log( y), x);
	} else {
	  double z=y*y;

	  return copysign( log1p( z/(sqrt(z+1.0)+1.0)+y), x);
	}
}
