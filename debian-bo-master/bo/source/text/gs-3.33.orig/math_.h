/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* math_.h */
/* Generic substitute for math.h */

/* We must include std.h before any file that includes sys/types.h. */
#include "std.h"

#if defined(VMS) && defined(__GNUC__)
/*  DEC VAX/VMS C comes with a math.h file, but GNU VAX/VMS C does not. */
#  include "vmsmath.h"
#else
#  include <math.h>
#endif

/* math.h is different for Turbo and Unix.... */
#ifndef M_PI
#  ifdef PI
#    define M_PI PI
#  else
#    define M_PI 3.14159265358979324
#  endif
#endif

/* Factors for converting between degrees and radians */
#define degrees_to_radians (M_PI / 180.0)
#define radians_to_degrees (180.0 / M_PI)

/* Define the hypot procedure on those few systems that don't provide it. */
#ifdef _IBMR2
/* The RS/6000 has hypot, but math.h doesn't declare it! */
extern double hypot(double, double);
#else
#  if !defined(__TURBOC__) && !defined(BSD4_2) && !defined(VMS)
#    define hypot(x,y) sqrt((x)*(x)+(y)*(y))
#  endif
#endif

#ifdef OSK
/* OSK has atan2 and ldexp, but math.h doesn't declare them! */
extern double atan2(), ldexp();
#endif

#ifdef DEBUG

/* Intercept calls on sqrt for debugging. */
extern double gs_sqrt(P3(double, const char _ds *, int));
#undef sqrt
#define sqrt(x) gs_sqrt(x, __FILE__, __LINE__)

#endif				/* DEBUG */
