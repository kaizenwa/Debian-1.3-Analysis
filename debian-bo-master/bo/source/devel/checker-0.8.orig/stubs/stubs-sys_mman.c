/* Checker stubs for functions defined in sys/mman.h
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

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#include "checker_api.h"

#undef HAVE_mremap

/* compiled from: . */
#ifdef HAVE_mmap
/* From `/usr/include/sys/mman.h:18'.  */
void *
chkr$mmap (void *addr, size_t len, int prot, int flags, int fd, off_t off)
{
#if USE_BI_JUMP
  __builtin_jump (mmap);
#else
  return mmap (addr, len, prot, flags, fd, off);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mmap */

#ifdef HAVE_munmap
/* From `/usr/include/sys/mman.h:19'.  */
int
chkr$munmap (void *addr, size_t len)
{
#if USE_BI_JUMP
  __builtin_jump (munmap);
#else
  return munmap (addr, len);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_munmap */

#ifdef HAVE_mprotect
/* From `/usr/include/sys/mman.h:20'.  */
int
chkr$mprotect (const void *addr, size_t len, int prot)
{
#if USE_BI_JUMP
  __builtin_jump (mprotect);
#else
  return mprotect (addr, len, prot);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mprotect */

#ifdef HAVE_msync
/* From `/usr/include/sys/mman.h:22'.  */
int
chkr$msync (void *addr, size_t len, int flags)
{
#if USE_BI_JUMP
  __builtin_jump (msync);
#else
  return msync (addr, len, flags);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_msync */

#ifdef HAVE_mlock
/* From `/usr/include/sys/mman.h:24'.  */
int
chkr$mlock (const void *addr, size_t len)
{
#if USE_BI_JUMP
  __builtin_jump (mlock);
#else
  return mlock (addr, len);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mlock */

#ifdef HAVE_munlock
/* From `/usr/include/sys/mman.h:25'.  */
int
chkr$munlock (const void *addr, size_t len)
{
#if USE_BI_JUMP
  __builtin_jump (munlock);
#else
  return munlock (addr, len);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_munlock */

#ifdef HAVE_mlockall
/* From `/usr/include/sys/mman.h:27'.  */
int
chkr$mlockall (int flags)
{
#if USE_BI_JUMP
  __builtin_jump (mlockall);
#else
  return mlockall (flags);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mlockall */

#ifdef HAVE_munlockall
/* From `/usr/include/sys/mman.h:28'.  */
int
chkr$munlockall (void)
{
#if USE_BI_JUMP
  __builtin_jump (munlockall);
#else
  return munlockall ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_munlockall */

#ifdef HAVE_mremap
/* From `/usr/include/sys/mman.h:31'.  */
void *
chkr$mremap (void *addr, size_t old_len, size_t new_len, int may_move)
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (void), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (mremap);
#else
  return mremap (arg0, arg1, arg2, arg3);
  {
    void * res;
    res = mremap (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mremap */

#endif /* HAVE_SYS_MMAN_H */
