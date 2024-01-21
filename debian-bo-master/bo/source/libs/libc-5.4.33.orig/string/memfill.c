/* memfill -- fill a block of memory with n bytes.
   For Intel 80x86, x>=3.
   Copyright (C) 1993 Free Software Foundation, Inc.
   Contributed by H.J. Lu (hlu@eecs.wsu.edu)

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
#include <string.h>

PTR
DEFUN(memfill, (dstpp, dstlen, srcpp, srclen),
      PTR dstpp AND size_t dstlen AND PTR srcpp AND size_t srclen)
{
  unsigned long int dstp = (unsigned long int) dstpp;

  if (!dstlen || !srclen) return dstpp;

  for (;dstlen >= srclen; dstlen -= srclen, dstp += srclen) {
    memcpy ((PTR) dstp, srcpp, srclen);
  }

  if (dstlen)
    memcpy ((PTR) dstp, srcpp, dstlen);

  return (dstpp);
}
