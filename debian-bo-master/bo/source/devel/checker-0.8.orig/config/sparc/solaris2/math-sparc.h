/* Some mathematic functions.
   Copyright 1995 Tristan Gingold
		  Written June 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "math-ieee.h"

static inline double
sqrt_double (double n)
{
  double res;
  asm __volatile__ (
  	"fsqrtd %1,%0"
  	: "=f" (res)
  	: "f" (n));
  return res;
}

static inline float
sqrt_float (float n)
{
  float res;
  asm __volatile__ (
  	"fsqrts %1,%0"
  	: "=f" (res)
  	: "f" (n));
  return res;
}

static inline int
isnan_float (float f)
{
 union ieee754_float num;
 
 num.f = f;
 
 if ((unsigned)num.ieee.exponent == 255 && num.ieee.mantissa != 0)
   return 1;
 else
   return 0;
}

static inline int
isnan_double (double d)
{
 union ieee754_double num;
 
 num.d = d;
 
 if ((unsigned)num.ieee.exponent == 2047 && (num.ieee.mantissa0 != 0 || num.ieee.mantissa1 != 0))
   return 1;
 else
   return 0;
}

static inline int
isnan_extended (long double e)
{
 union ieee754_extended num;
 
 num.e = e;
 
 if ((unsigned)num.ieee.exponent == 32767 && (num.ieee.mantissa0 != 0 || num.ieee.mantissa1 != 0))
   return 1;
 else
   return 0;
}

      