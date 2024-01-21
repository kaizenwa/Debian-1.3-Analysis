/* bitops - operation with bits, used by malloc
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written December 1993 by Tristan Gingold

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

#ifndef __BITOPS_H__
#define __BITOPS_H__
/* If you want speed, you can define log_size with machine instructions.
   Consequently, the fonction is inline and faster.
   log_size return the offset of the higher bit set to one. If this offset
   is greather than 19, the function must return 19.
   e.g: log_size(32) is 5
        log_size(48) is 5
   There is a C equivalent function below.
 */

#ifdef __GNUC__
#define XSTR(x) STR(x)
#define STR(x) #x

static __inline__ int
log_size (size_t size)
{
  register int __res __asm__ ("ax");

  __asm__ ("bsrl %1,%0\n\t"
	   "cmpl $" XSTR(HASH_SIZE) ",%0\n\t"
	   "jb 1f\n\t"
	   "movl $" XSTR((HASH_SIZE-1)) ",%0\n"
	   "1:"
: "=a" (__res):"r" (size));
  return __res;
}

/* stolen code */
static __inline__ uint
mutex_atomic_swap(volatile uint *p, uint newval)
{
  uint semval = newval;
  
  /* If one of the operands for the XCHG instructions is a memory ref,
   * it makes the swap an uninterruptible RMW cycle.
   *
   * One operand must be in memory, the other in a register, otherwise
   * the swap may not be atomic.
   */

  asm __volatile__ ("xchgl %2, %0\n"
		    : /* outputs: semval   */ "=r" (semval)
		    : /* inputs: newval, p */ "0" (semval), "m" (*p)
		   );	/* p is a var, containing an address */
  return semval;
}
#endif
