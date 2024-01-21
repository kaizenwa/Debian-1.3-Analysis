/* sbrk() function.
   Copyright 1995 Tristan Gingold
		  Written June 1995 by Tristan Gingold

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
#include <sys/syscall.h>

extern int chkr_errno;
extern char *end;

void *chkr_sbrk (int increment);
int chkr_brk (void *addr);
void chkr_init_sbrk (void);

void *_sbrk_addr = &end;

#define ALIGN_VAL 8
#define ALIGN (ALIGN_VAL - 1)

void *
chkr_sbrk (int increment)
{
  void *new_addr;
  void *old_addr;
  int status;
  
  increment = (increment + ALIGN) & ~ALIGN;
  old_addr = (void*)(((int)_sbrk_addr + ALIGN) & ~ALIGN);
  new_addr = old_addr + increment;
#if 0
  chkr_printf("chkr_sbrk called with incr=0x%08x, old_addr=0x%08x, new_addr=0x%08x\n", increment, old_addr, new_addr);
#endif
  
  __asm__ volatile (
    "mov %2, %%o0\n\t"	/* new_addr -> %o0 */
    "mov %3, %%g1\n\t"	/* SYS_brk -> %g1 */
    "ta 8\n\t"		/* brk() */
    "bcc 1f\n\t"	/* error ? */
    "mov %%o0, %1\n\t"	/* error -> chkr_errno */
    "b 2f\n\t"
    "mov -1, %0\n\t"	/* (-1) -> status */
    "1:mov 0, %0\n\t"   /* (0) -> status */
    "2:"
    : "=r" (status), "=r" (chkr_errno)
    : "r" (new_addr), "I" (SYS_brk)
    : "%g1", "%o0");
  if (!status)
    {
      _sbrk_addr = new_addr;
      return old_addr;
    }
  else
    return 0;
}

int
chkr_brk (void *addr)
{
  int status;
  
  addr = (void *)(((int)addr + ALIGN) & ~ALIGN);
  __asm__ volatile (
    "mov %3, %%o0\n\t"
    "mov %4, %%g1\n\t"
    "ta 8\n\t"
    "bcc 1f\n\t"
    "mov %%o0, %1\n\t"
    "b 2f\n\t"
    "mov -1, %0\n\t"
    "1: mov %3, %2\n\t"
    "mov 0, %0\n\t"
    "2:"
    : "=r" (status), "=r" (chkr_errno), "=r" (_sbrk_addr)
    : "r" (addr), "I" (SYS_brk), "2" (_sbrk_addr)
    : "%g1", "%o0");
  return status;
}

void *_sbrk_unlocked(int);

void
chkr_init_sbrk (void)
{
#if 0
 chkr_printf ("call sbrk_unlocked\n");
#endif
 _sbrk_addr = _sbrk_unlocked(0);
#if 0
 chkr_printf("_sbrk_addr = 0x%08x\n", _sbrk_addr);
#endif
 return;
}
