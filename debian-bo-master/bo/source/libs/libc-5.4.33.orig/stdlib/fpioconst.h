/* Header file for constants used in floating point <-> decimal conversions.
Copyright (C) 1995 Free Software Foundation, Inc.
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

#ifndef _FPIOCONST_H
#define	_FPIOCONST_H

#include <float.h>
#include "gmp.h"

/* Define the maximum i for which 10^(2^i) is represented in the table
   in fpioconst.c.  */
#define	LAST_POW10	12

/* Table of powers of ten.  This is used by __printf_fp and by
   strtof/strtod/strtold.  */
struct mp_power
  {
    const mp_limb *array;	/* The array with the number representation. */
    mp_size_t arraysize;	/* Size of the array.  */
    int p_expo;			/* Exponent of the number 10^(2^i).  */
    int m_expo;			/* Exponent of the number 10^-(2^i-1).  */
  };
extern const struct mp_power _fpioconst_pow10[LAST_POW10 + 1];

#endif	/* fpioconst.h */
