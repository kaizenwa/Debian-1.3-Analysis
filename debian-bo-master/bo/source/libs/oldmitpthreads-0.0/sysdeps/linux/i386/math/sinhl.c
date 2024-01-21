/* Copyright (C) 1995 O.Flebbe@science-computing.uni-tuebingen.de
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

/* 
 Original Code by John C. Bowman (bowman@hagar.ph.utexas.edu).
*/

#include <ansidecl.h>
#include <math.h>

long double
DEFUN(sinhl, (x), long double x)
{
	long double __sinh = expm1l(fabsl( x));

	return copysignl( 0.5*(__sinh/(__sinh+1.0)+__sinh), x);
}
