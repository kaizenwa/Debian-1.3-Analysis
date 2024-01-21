/* Machine-dependent pthreads configuration and inline functions.
   i386 version.
Copyright (C) 1996 Free Software Foundation, Inc.
This file is part of the GNU C Library.
Contributed by Richard Henderson <rth@tamu.edu>.

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


/* Spinlock implementation; required.  */
extern inline int testandset(int *spinlock)
{
  int ret;

  __asm__ __volatile__("xchgl %0, %1"
	: "=r"(ret), "=m"(*spinlock)
	: "0"(1), "m"(*spinlock));

  return ret;
}


/* Get some notion of the current stack.  Need not be exactly the top
   of the stack, just something somewhere in the current frame.  */
#define CURRENT_STACK_FRAME  stack_pointer
register char * stack_pointer __asm__("%esp");

/* Compare-and-swap for semaphores. */

#define HAS_COMPARE_AND_SWAP

extern inline int compare_and_swap(long * p, long oldval, long newval)
{
  char ret;
  long readval;

  __asm__ __volatile__ ("lock; cmpxchg %3, %1; sete %0"
                       : "=r" (ret), "=m" (*p), "=a" (readval)
                       : "r" (newval), "m" (*p), "a" (oldval));
  return ret;
}
