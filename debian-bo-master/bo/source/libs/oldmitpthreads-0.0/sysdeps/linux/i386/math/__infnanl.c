/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
   changed for long double by olaf flebbe

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
#include <errno.h>
#if defined(__i386__)
#include <fpu_control.h>
#include <signal.h>
extern int __getfpucw(void);
#endif /*  defined(__i386__) */

/* Deal with an infinite or NaN result.
   If ERROR is ERANGE, result is +Inf;
   if ERROR is - ERANGE, result is -Inf;
   otherwise result is NaN.
   This will set `errno' to either ERANGE or EDOM,
   and may return an infinity or NaN, or may do something else.  */

long double __infnanl( int );

long double
DEFUN(__infnanl, (error), int error)
{
  union {
    unsigned int i[3];
    long double d;
  }u;

   u.i[0] = 0;
   u.i[1] = 0;
  switch (error)
    {
    case ERANGE:
      errno = ERANGE;
#if defined(__i386__)
      if (!(__getfpucw() & _FPU_MASK_OM))
	(void) raise(SIGFPE);
#endif /* defined(__i386__) */
      u.i[2] = 0x7fff;
      return (u.d);

    case - ERANGE:
      errno = ERANGE;
#if defined(__i386__)
      if (!(__getfpucw() & _FPU_MASK_OM))
	(void) raise(SIGFPE);
#endif /* defined(__i386__) */
      u.i[2] = 0xffff;
      return (u.d);

    default:
      errno = EDOM;
#if defined(__i386__)
      if (!(__getfpucw() & _FPU_MASK_IM))
	(void) raise(SIGFPE);
#endif /* defined(__i386__) */
      u.i[2] = 0xffff;
      u.i[1] = 0xC0000000;
      return (u.d);
    }
}

