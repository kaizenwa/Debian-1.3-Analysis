/* Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
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
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <ansidecl.h>
#include "checker.h"
#include <setjmp.h>

#define REGS \
  REG (bx);\
  REG (si);\
  REG (di)

#ifndef linux
#define REG(xx) register long int xx asm (#xx)
REGS;
#undef	REG
#endif

/* Save the current program position in ENV and return 0.  */
int
__setjmp(jmp_buf env)
{
  chkr_check_addr(&env, sizeof(jmp_buf*), CHKR_RO);
  chkr_check_addr(env, sizeof(jmp_buf), CHKR_WO);
  
  /* Save the general registers.  */
#ifdef linux
#define REG(xx) asm volatile ("movl %%e" #xx ",%0" : \
		"=m" ((long int) (env[0].__##xx)) : )
#else
#define	REG(xx)	env[0].__##xx = xx
#endif
  REGS;

  /* Save the return PC.  */
  env[0].__pc = (PTR) ((PTR *) &env)[-1];

  /* Save caller's FP, not our own.  */
  env[0].__bp = (PTR) ((PTR *) &env)[-2];

  /* Save caller's SP, not our own.  */
  env[0].__sp = (PTR) &env;

  return 0;
}
