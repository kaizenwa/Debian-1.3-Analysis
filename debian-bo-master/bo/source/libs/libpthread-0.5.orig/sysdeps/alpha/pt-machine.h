/* Machine-dependent pthreads configuration and inline functions.
   Alpha version.
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

#include <asm/pal.h>


/* Spinlock implementation; required.  */
extern inline long testandset(int *spinlock)
{
  long ret, temp;

  __asm__ __volatile__(
	"/* Inline spinlock test & set */\n"
	"1:\t"
	"ldl_l %0,%3\n\t"
	"bne %0,2f\n\t"
	"or $31,1,%1\n\t"
	"stl_c %1,%2\n\t"
	"beq %1,1b\n"
	"2:\tmb\n"
	"/* End spinlock test & set */"
	: "=&r"(ret), "=&r"(temp), "=m"(*spinlock)
	: "m"(*spinlock)
        : "memory");

  return ret;
}

/* Spinlock release; default is just set to zero.  */
#define RELEASE(spinlock) \
  __asm__ __volatile__("mb" : : : "memory"); \
  *spinlock = 0

/* Begin allocating thread stacks at this address.  Default is to allocate
   them just below the initial program stack.  */
#define THREAD_STACK_START_ADDRESS  0x40000000000


/* Get some notion of the current stack.  Need not be exactly the top
   of the stack, just something somewhere in the current frame.  */
#define CURRENT_STACK_FRAME  stack_pointer
register char *stack_pointer __asm__("$30");


/* Return the thread descriptor for the current thread.  */
#define THREAD_SELF						\
{								\
  register pthread_t __self __asm__("$0");			\
  __asm__ ("call_pal %1" : "=r"(__self) : "i"(PAL_rduniq));	\
  return __self;						\
}

/* Initialize the thread-unique value.  */
#define INIT_THREAD_SELF(descr)						 \
{									 \
  register pthread_t __self __asm__("$16") = (descr);			 \
  __asm__ __volatile__ ("call_pal %1" : : "r"(__self), "i"(PAL_wruniq)); \
}

/* Compare-and-swap for semaphores. */

#define HAS_COMPARE_AND_SWAP

extern inline int compare_and_swap(long * p, long oldval, long newval)
{
  long ret, temp;

  __asm__ __volatile__ (
      "ldq_l %2, %1\n\t"
      "subq %2, %3, %2\n\t"
      "bne %2, 1f\n\t"
      "stl_c %0, %1\n\t"
      "mb\n\t"
      "1:\t"
      "cmovne %2, $31, %0"
   : "=&r" (ret), "=m" (*p), "=&r" (temp)
   : "r" (oldval), "0" (newval), "m" (*p));
  return ret;
}
