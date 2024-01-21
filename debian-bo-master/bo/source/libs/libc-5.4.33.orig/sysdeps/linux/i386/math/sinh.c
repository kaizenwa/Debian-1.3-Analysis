/* Copyright (C) 1995  O.Flebbe@science-computing.uni-tuebingen.de
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

#include <math.h>
#include <ansidecl.h>

double
DEFUN(sinh, (x), double x)
{
	register double __sinharg = x, __sinh = expm1(fabs(__sinharg));

	return __copysign( 0.5*(__sinh/(__sinh+1.0)+__sinh),__sinharg);
}
