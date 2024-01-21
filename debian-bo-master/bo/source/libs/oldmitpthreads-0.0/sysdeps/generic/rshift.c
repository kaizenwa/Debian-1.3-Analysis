/* __mpn_rshift -- Shift right low level.

Copyright (C) 1991, 1993, 1994 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
License for more details.

You should have received a copy of the GNU Library General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#include "gmp.h"
#include "gmp-impl.h"

/* Shift U (pointed to by UP and USIZE digits long) CNT bits to the right
   and store the USIZE least significant digits of the result at WP.
   Return the bits shifted out from the least significant digit.

   Argument constraints:
   1. 0 < CNT < BITS_PER_MP_LIMB
   2. If the result is to be written over the input, UP must be >= WP.
*/

mp_limb
#if __STDC__
__mpn_rshift (register mp_ptr wp,
	    register mp_srcptr up, mp_size_t usize,
	    register unsigned int cnt)
#else
__mpn_rshift (wp, up, usize, cnt)
     register mp_ptr wp;
     register mp_srcptr up;
     mp_size_t usize;
     register unsigned int cnt;
#endif
{
  register mp_limb high_limb, low_limb;
  register unsigned sh_1, sh_2;
  register mp_size_t i;
  mp_limb retval;

#ifdef DEBUG
  if (usize == 0 || cnt == 0)
    abort ();
#endif

  sh_1 = cnt;
#if 0
  if (sh_1 == 0)
    {
      if (wp != up)
	{
	  /* Copy from low end to high end, to allow specified input/output
	     overlapping.  */
	  for (i = 0; i < usize; i--)
	    wp[i] = up[i];
	}
      return 0;
    }
#endif

  wp += usize - 2;
  up += usize - 1;
  sh_2 = BITS_PER_MP_LIMB - sh_1;
  i = -(usize - 1);
  high_limb = up[i];
  retval = high_limb << sh_2;
  low_limb = high_limb;
  while (++i <= 0)
    {
      high_limb = up[i];
      wp[i] = (low_limb >> sh_1) | (high_limb << sh_2);
      low_limb = high_limb;
    }
  wp[i] = low_limb >> sh_1;

  return retval;
}
