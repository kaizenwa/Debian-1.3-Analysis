/* Copyright (C) 1993  Hongjiu Lu
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

#include <errno.h>
#include <math.h>
#include <fpu_control.h>

double pow (x,y)  	
double x,y;
{
  int negative;
  __volatile__ unsigned short cw, saved_cw;	/* 80387 control word */

  if (y == 0.0) return 1.0;

  if (y == 1.0) return x;
  
  if (!finite(x) || !finite(y)) {
    /*
     * Handle some IEEE special values
     */
    /*    printf("Handling special values %g and %g\n", x, y); */
    if (__isnan(x) || __isnan(y))
      return __infnan(EDOM);
    else if (fabs(x) == 1.0)
      return __infnan(EDOM);
    else if (y < 0.0)
      return (fabs(x) < 1.0) ? __infnan(ERANGE) : 0.0;
    else if (!finite(y))
      return (fabs(x) < 1.0) ? 0.0 : __infnan(ERANGE);
    else {
      /*
       * x is +/-Inf, y is positive & finite
       */
      long long tmp;
      
      tmp = (long long) y; 
      
      /* Even or odd */
      negative = tmp & 1;
      
      /* That is only valid if y is an integer < 2^64. */
      if ((y != (double) tmp) & (x < 0)) {
	return __infnan( EDOM);
      }
      
      return negative ? x : __infnan(ERANGE);
    }
    /*
     * All other special values will fall out of the
     * calculation below
     */
  }

  if (x == 0.0) return (y < 0.0) ? __infnan(ERANGE) : 0.0;

  if (x < 0.0) {
    long long tmp;

    tmp = (long long) y; 

    /* Even or odd */
    negative = tmp & 1;

    /* That is only valid if |y| < 2^64. */
    if (y != (double) tmp) {
       return __infnan( EDOM);
    }

    x = -x;
  } else {
    negative = 0;
  }

 /*
  * Inline assembler implements the following algorithm:
  *
  * 1 - x' = y *log2(x)
  * 2 - find x1, x2 where x' = x1 + x2 and |x2| <= 0.5
  * 3 - x = 2^x2 scaled by x1
  */

  __asm__ __volatile__ ("fnstcw %0" : "=m" (cw) : );
  saved_cw = cw;

  cw &= 0xf3ff;	/* force round to nearest */
  cw |= 0x003f; /* turn off exceptions (see ANSI/ISO 9989 section 7.5.1) */

  __asm__ __volatile__ ("fldcw %0" : : "m" (cw));

  __asm__ __volatile__ ("fyl2x;fstl %2;frndint;fstl %%st(2);fsubrp;f2xm1;"
			"fld1;faddp;fscale"
    			  : "=t" (x) : "0" (x), "u" (y));

  /* Restore the control word */
  __asm__ __volatile__ ("fldcw %0" : : "m" (saved_cw));

  /* Check for results that can not be represented (kludgy): */
  if (__isinf(x) || __isnan(x))
    errno = ERANGE;

  return (negative) ? -x : x;
}
