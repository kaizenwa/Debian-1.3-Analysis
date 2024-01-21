/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
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
#include <stddef.h>
#include <setjmp.h>
#include <signal.h>

/* Store the calling environment in ENV, also saving the
   signal mask if SAVEMASK is nonzero.  Return 0.  */
int
DEFUN(__old_sigsetjmp, (env, savemask), sigjmp_buf env AND int savemask)
{
  if (savemask)
    env[0].__mask_was_saved = sigprocmask(SIG_BLOCK, (sigset_t *) NULL,
				    &env[0].__saved_mask) == 0;
  else
    env[0].__mask_was_saved = 0;

  /* Save data registers D1 through D7.  */
  asm volatile("movem%.l d1-d7, %0" : : "m" (env[0].__jmpbuf[0].__dregs[0]));

  /* Save return address in place of register A0.  */
  env[0].__jmpbuf[0].__aregs[0] = (PTR) ((PTR *) &env)[-1];

  /* Save address registers A1 through A5.  */
  asm volatile("movem%.l a1-a5, %0" : : "m" (env[0].__jmpbuf[0].__aregs[1]));

  /* Save caller's FP, not our own.  */
  env[0].__jmpbuf[0].__fp = (PTR) ((PTR *) &env)[-2];

/* XXX FIX ME */
/*
 * The Amiga GCC uses A5 as the frame pointer, rather than A6, so this
 * code loses on that compiler, since this code expects A6 to be the
 * frame pointer.
 * This change fix will only be compiled occur if __amiga__ is defined.
 *
 * The long term solution is to create a "linux/68k" gcc which uses a6
 * as the frame pointer.
 */

#ifdef __amiga__
  env[0].__jmpbuf[0].__aregs[5] = env[0].__jmpbuf[0].__fp;
  asm volatile ("move%.l a6, %0" : : "m" (env[0].__jmpbuf[0].__fp));
#endif

/* XXX */

  /* Save caller's SP, not our own.  */
  env[0].__jmpbuf[0].__sp = (PTR) &env;

#if	defined(__HAVE_68881__) || defined(__HAVE_FPU__)
  /* Save floating-point (68881) registers FP0 through FP7.  */
  asm volatile("fmovem%.x fp0-fp7, %0" : : "m" (env[0].__jmpbuf[0].__fpregs[0]));
#endif

  return 0;
}
