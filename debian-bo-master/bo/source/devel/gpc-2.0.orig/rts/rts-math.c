/* Copyright (C) 1991 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Transcendential functions. Uses libm.a.

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

/* Author: Juki <jtv@hut.fi>
 */

#include "rts.h"
#include <math.h>

#ifdef p_ARCTAN
double
_p_arctan(x)
double	x;
{
    double val;

    val = atan(x);

    return(val);
}
#endif /* p_ARCTAN */

#ifdef p_SQRT
double
_p_sqrt(x)
double	x;
{
    if (x < 0.0) {
	_p_generic(34);
	x = -x;
    }
    return (sqrt(x));
}
#endif /* p_SQRT */

#ifdef p_LN
double
_p_ln(x)
double	x;
{
    if (x <= 0.0) {
	_p_generic(33);
	if (x == 0.0)
#ifdef DBL_MIN
	    return (DBL_MIN);
#else
	    return (0.0);
#endif
	x = -x;
    }
    return(log(x));
}
#endif /* p_LN */

#ifdef p_EXP
double
_p_exp(x)
double	x;
{
    double ret;

    errno = 0;
    ret = exp (x);
    switch (errno)
      {
      case 0 :
	break;
      case ERANGE :
	_p_generic (621);
	break;
      case EDOM :
	_p_generic (622);
	break;
	default :
	  _p_error (REPORT, "Undocumented errno value %d in libm.a(exp)", errno);
      }
    return(ret);
}
#endif /* p_EXP */

#ifdef p_SIN
double
_p_sin(x)
double	x;
{
    return(sin(x));
}
#endif /* p_SIN */

#ifdef p_COS
double
_p_cos(x)
double	x;
{
    return(cos(x));
}
#endif /* p_COS */

#ifdef p_POW
double
_p_pow (x,y)
double	x;
int	y;
{
  double rval = 1.0;

  if (y)
    {
      if (y < 0)
	{
	  y = -y;
	  if (! x)
	    _p_error (ABORT, "Executed 'x POW y' when X is zero");
	  x = 1/x;
	}

      while (1)
	{
	  if (y & 1)
	    rval *= x;
	  if (y >>= 1)
	    x *= x;
	  else
	    break;
	}
    }

  return (rval);
}
#endif /* p_POW */

#ifdef p_EXPON
/* Extended Pascal '**' operator calls the POW function in libmath.a */
double
_p_expon (x,y)
double	x;
double	y;
{
  double rval;

  errno = 0;
  rval = pow (x, y);
  switch (errno)
    {
    case 0 :
      break;
    case ERANGE :
      _p_generic (621);
      break;
    case EDOM :
      _p_generic (622);
      break;
    default :
      _p_error (REPORT, "Undocumented errno value %d in libm.a(pow)", errno);
    }

  return (rval);
}
#endif /* p_EXPON */
