/* Checker stubs for functions defined in setjmp.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_SETJMP_H
#include <setjmp.h>
#include "checker_api.h"

#undef HAVE___sigjmp_save
#undef HAVE_siglongjmp
#undef HAVE___longjmp
#undef HAVE__longjmp

#if 0
#define HAVE_longjmp
#define HAVE___setjmp
#endif

/* compiled from: . */
#ifdef HAVE___sigjmp_save
void
chkr$__sigjmp_save (struct { __jmp_buf __jmpbuf; int __mask_was_saved; sigset_t __saved_mask; } * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct { __jmp_buf __jmpbuf; int __mask_was_saved; sigset_t __saved_mask; }), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (__sigjmp_save);
#else
  __sigjmp_save (arg0, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___sigjmp_save */

#ifdef HAVE_siglongjmp
void
chkr$siglongjmp (const struct { __jmp_buf __jmpbuf; int __mask_was_saved; sigset_t __saved_mask; } * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct { __jmp_buf __jmpbuf; int __mask_was_saved; sigset_t __saved_mask; }), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (siglongjmp);
#else
  siglongjmp (arg0, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_siglongjmp */

#ifdef HAVE___longjmp
void
chkr$__longjmp (const struct { long int __bx; long int __si; long int __di; void *__bp; void *__sp; void *__pc; } * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct { long int __bx; long int __si; long int __di; void *__bp; void *__sp; void *__pc; }), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (__longjmp);
#else
  __longjmp (arg0, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE___longjmp */

#ifdef HAVE_longjmp
void
chkr$longjmp (const jmp_buf env, int val)
{
/*  stubs_chkr_check_addr (env, sizeof (jmp_buf), CHKR_RO); */
#if USE_BI_JUMP
  __builtin_jump (longjmp);
#else
  longjmp (env, val);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_longjmp */

#ifdef HAVE___setjmp
int
chkr$__setjmp (jmp_buf env)
{
  int res;
  stubs_chkr_check_addr (env, sizeof (env), CHKR_TW, "env");
  res = __setjmp (env);
  return res;
}
#endif /* HAVE___setjmp */

#ifdef HAVE__longjmp
void
chkr$_longjmp (const struct { long int __bx; long int __si; long int __di; void *__bp; void *__sp; void *__pc; } * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct { long int __bx; long int __si; long int __di; void *__bp; void *__sp; void *__pc; }), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (_longjmp);
#else
  _longjmp (arg0, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__longjmp */

#ifdef HAVE_setjmp
int
chkr$setjmp (jmp_buf env)
{
  int res;
  stubs_chkr_check_addr (env, sizeof (env), CHKR_TW, "env");
  res = setjmp (env);
  return res;
}
#endif /* HAVE_setjmp */

#endif /* HAVE_SETJMP_H */
