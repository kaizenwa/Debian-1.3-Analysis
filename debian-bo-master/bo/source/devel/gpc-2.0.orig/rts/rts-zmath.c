/* Copyright (C) 1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Library functions for complex type arguments. Uses libm.a.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/* Author: Juki <jtv@hut.fi> */

/*
 * Formulaes from :
 *   Erwin Kreyszig, Advanced Engineering Mathematics (6th edition)
 */ 

#include "rts.h"
#include <math.h>

#ifdef p_POLAR
COMPLEX
_p_polar (length, theta)
     double length;
     double theta;
{
  COMPLEX r;

  CPART_RE(r) = length * cos (theta);
  CPART_IM(r) = length * sin (theta);
  return r;
}
#endif /* p_POLAR */

#ifdef p_ARG
double
_p_arg (z)
COMPLEX z;
{
  if (! CPART_RE(z) && ! CPART_IM(z))
    _p_error (ABORT, "Can't take `arg' of zero");

  return z_ANGLE (z);
}
#endif /* p_ARG */

#ifdef z_ARCTAN
COMPLEX
_p_z_arctan (z)
     COMPLEX z;
{
  /* From Extended Pascal standard:
   *
   *  arctan (z)    == (-i/2) * ln ((1+i*z) / (1-i*z))
   *
   * From pen and paper :-)
   *
   *  argctan(x+yi) == (-i/2) * ln (((1-y)+i*x) / ((1+y)-i*x))
   *		    == (-i/2) * ln (((XY-x**2)/D)+i*(2x/D))
   *
   * Where X == (1 - y)
   *   and Y == (1 + y)
   *   and D == (Y*Y + x*x)
   *   and x == CPART_RE(z)
   *   and y == CPART_IM(z)
   */

  COMPLEX r, t;
  double  X = (1 - CPART_IM(z));
  double  Y = (1 + CPART_IM(z));
  double  D = (Y*Y+CPART_RE(z)*CPART_RE(z));

  CPART_RE(r) = (X*Y-(CPART_RE(z) * CPART_RE(z)))/D;
  CPART_IM(r) = 2*CPART_RE(z)/D;

  t   = _p_z_ln (r);

  CPART_RE(r) =  CPART_IM(t) / 2;
  CPART_IM(r) = -CPART_RE(t) / 2;

  return r;
}
#endif /* z_ARCTAN */

#ifdef z_EXPON
COMPLEX
_p_z_expon (z, y)
     COMPLEX z;
     double  y;
{
  /* z**y == e**(y*ln(z)) */

  COMPLEX r = _p_z_ln (z);
  COMPLEX tmp;

  CPART_RE(tmp) = CPART_RE(r) * y;
  CPART_IM(tmp) = CPART_IM(r) * y;

  return _p_z_exp (tmp);
}
#endif /* z_EXPON */

#ifdef z_LN
COMPLEX
_p_z_ln (z)
     COMPLEX z;
{
  COMPLEX r;

  CPART_RE(r) = log (z_LENGTH (z));
  CPART_IM(r) = z_ANGLE (z);

  return r;
}
#endif /* z_LN */

#ifdef z_EXP
COMPLEX
_p_z_exp (z)
     COMPLEX z;
{
  /* exp(x+iy) = e**x * (cos(y) + i*sin(y)) */

  COMPLEX r;
  double ex;

  errno = 0;
  ex = exp (CPART_RE(z));

  switch (errno)
    {
    case 0 :
      break;
    case ERANGE :
      _p_error (ABORT, "Overflow in complex EXP function");
      break;
    default :
      _p_error (REPORT, "Unknown errno value %d in complex EXP", errno);
    }

  CPART_RE(r) = ex * cos (CPART_IM(z));
  CPART_IM(r) = ex * sin (CPART_IM(z));

  return r;
}
#endif /* z_EXP */

#ifdef z_SIN
COMPLEX
_p_z_sin (z)
     COMPLEX z;
{
  /* sin(x+iy) = sin(x)*cosh(y)+i*cos(x)*sinh(y) */

  COMPLEX r;
  CPART_RE(r) = sin (CPART_RE(z)) * cosh (CPART_IM(z));
  CPART_IM(r) = cos (CPART_RE(z)) * sinh (CPART_IM(z));

  return r;
}
#endif /* z_SIN */

#ifdef z_COS
COMPLEX
_p_z_cos (z)
     COMPLEX z;
{
  /* cos(x+iy) = cos(x)*cosh(y)-i*sin(x)*sinh(y) */

  COMPLEX r;

  CPART_RE(r) =   cos (CPART_RE(z)) * cosh (CPART_IM(z));
  CPART_IM(r) = - sin (CPART_RE(z)) * sinh (CPART_IM(z));

  return r;
}
#endif /* z_COS */

#ifdef z_SQRT
COMPLEX
_p_z_sqrt (z)
     COMPLEX z;
{
  /* This formula is from: T. Pentikainen, Matematiikan Kaavoja, page 11.
   *
   * sqrt(x+iy) == (+/-) sqrt(0.5*(x+sqrt(xx+yy))) (+/-) i * sqrt(0.5*(-x+sqrt(xx+yy)))
   *		== (+/-) sqrt((LEN + x) / 2) SIGN i * sqrt ((LEN - x) / 2)
   *
   * Where
   *    LEN == z_LENGTH(z) == sqrt(xx+yy)
   *   SIGN == sign of y
   *
   * Principal value defined in the Extended Pascal standard: exp(0.5*ln(z))
   */
#ifdef __GNUC__
  COMPLEX r   = 0;
#else
  COMPLEX r   = { 0, 0 };
#endif
  double  LEN = z_LENGTH (z);
  int    SIGN = CPART_IM(z) < 0 ? -1 : 1;

  if (LEN)
    {
      CPART_RE(r) = sqrt ((LEN + CPART_RE(z)) / 2);
      CPART_IM(r) = SIGN * sqrt ((LEN - CPART_RE(z)) / 2);
    }
  return r;
}
#endif /* z_SQRT */

#ifdef z_POW
COMPLEX
_p_z_pow (z, y)
     COMPLEX z;
     int y;
{
#ifdef __GNUC__
  COMPLEX r = 1;
#else
  COMPLEX r = { 1, 0 };
#endif

  if (! y)
    return r;

  if (y < 0)
    {
      double  div = (CPART_RE(z)*CPART_RE(z) + CPART_IM(z)*CPART_IM(z));

      if (! div)
	_p_error (ABORT, "Executed 'x POW y' when complex X is zero and y < 0");

      /* divide 1.0/z */
      CPART_RE(z) =   CPART_RE(z) / div;
      CPART_IM(z) = - CPART_IM(z) / div;

      y = -y;
    }

  while (1)
    {
      double tmp;
      if (y & 1)
	{
	  /* R = (X+iY)*(x+iy) == Xx-Yy+i(Xy+Yx) */
	  tmp = CPART_RE(r) * CPART_RE(z) - CPART_IM(r) * CPART_IM(z);
	  CPART_IM(r) =   CPART_RE(r) * CPART_IM(z)
	                + CPART_IM(r) * CPART_RE(z);
	  CPART_RE(r) = tmp;
	}

      if (y >>= 1)
	{
	  /* Z = (x+iy)*(x+iy) == xx-yy+2xyi */
	  tmp = CPART_RE(z) * CPART_RE(z) - CPART_IM(z) * CPART_IM(z);
	  CPART_IM(z) = 2 * CPART_RE(z) * CPART_IM(z);
	  CPART_RE(z) = tmp;
	}
      else
	break;
    }

  return r;
}
#endif /* z_POW */
