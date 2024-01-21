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

#include <math.h>
#include <ansidecl.h>

double
DEFUN(tanh, (x), double x)
{
	double __tanh = expm1(-fabs(x+x));
	
	return  copysign(__tanh/(__tanh+2.0), x);
}
