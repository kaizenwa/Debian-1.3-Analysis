/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
   Changed by Andreas Schwab for long double
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <fp.h>
#include <math.h>

#ifdef	__GNUC__

long double
DEFUN(atan2l, (y, x), long double y AND long double x)
{
  long double one = 1.0, zero = 0.0;
  long double signx, signy;
  long double pi, PIo4, PIo2;

  if (__isnanl(x))
    return x;
  if (__isnanl(y))
    return y;

  signy = __copysignl(one, y);
  signx = __copysignl(one, x);

  asm("fmovecr%.x %1, %0" : "=f" (pi) : "i" (0));
  PIo2 = pi / 2;
  PIo4 = pi / 4;

  if (y == zero)
    return signx == one ? y : __copysignl(pi, signy);

  if (x == zero)
    return __copysignl(PIo2, signy);

  if (__isinfl(x))
    {
      if (__isinfl(y))
	return __copysignl(signx == one ? PIo4 : 3 * PIo4, signy);
      else
	return __copysignl(signx == one ? zero : pi, signy);
    }

  if (__isinfl(y))
    return __copysignl(PIo2, signy);

  y = fabsl(y);

  if (x < 0.0)
    /* X is negative.  */
    return __copysignl(pi - atanl(y / -x), signy);

  return __copysignl(atanl(y / x), signy);
}

#else
#include <sysdeps/generic/atan2l.c>
#endif
