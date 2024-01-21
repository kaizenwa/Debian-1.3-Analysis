/* stubs-syscalls.c: stubs for syscalls.
   Copyright 1995 Tristan Gingold

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
#define NEED_MM
#include <sys/mman.h>
#include <unistd.h>
#include "checker.h"
#include "message.h"
#include <errno.h>

/* The purpose of these stubs is to catch some important system calls (such as
   those that handle the memory).
   If there were only called by the user, the stubs in the stubs/ directory
   could do what we want, but they could also be called by library functions.
   
   There is an hot point: we need to redefine the system calls.  This is done
   in config/, by .S files.
   Not really portable.  */
 
/* Override macros of chkrsyscalls.h.  */
#undef mmap
#undef munmap
#undef mprotect
#undef _exit

/* XXX FIXME */
#ifdef __linux__
#define MMAP_ADDR __ptr_t
#else
#define MMAP_ADDR caddr_t
#endif

MMAP_ADDR
mmap (MMAP_ADDR addr, size_t len, int prot, int flags, int fd, off_t off)
{
  MMAP_ADDR res;
#ifdef MAP_ANONYMOUS
  if (!(flags & MAP_ANONYMOUS))
#endif
    fd_used_by_prog (fd);
    
  /* Do not allow perturbation of Checker private memory.  */
  if (addr >= (MMAP_ADDR)MM_LOW && addr <= (MMAP_ADDR)MM_HIGH)
    if (flags & MAP_FIXED)
      {
        errno = EINVAL;
        return 0;
      }
    else
      addr = 0;

  /* Do the *real* system call.  */
  res = chkr_mmap (addr, len, prot, flags, fd, off);
  errno = chkr_errno;

  /* Tell Checker what happened.  */
  if (res)
    new_segmmap (res, len, prot, flags, fd, off);
  return res;
}

int
mprotect (const MMAP_ADDR addr, size_t len, int prot)
{
#if 0
  int res;
  res = chkr_mprotect (addr, len, prot);
  errno = chkr_errno;
  if (res != -1)
    seg_mprotect (addr, len, prot);
  return res;
#else
  chkr_abort ();
#endif
}

int
munmap (MMAP_ADDR addr, size_t len)
{
  int res;

  if (addr >= (MMAP_ADDR)MM_HIGH || addr + len <= (MMAP_ADDR)MM_LOW)
    {
      res = chkr_munmap (addr, len);
      errno = chkr_errno;
      if (res != -1)
        remove_mmap (addr, len);
      return res;
    }
  else
    {
      errno = EINVAL;
      return -1;
    }
}

/* We need to call chkr_do_end.  */
void
_exit (int status)
{
  chkr_do_end ();
  chkr__exit (status);
}

#ifndef NO_SIGNALS

#ifdef __linux__
/* No comments...  */
#define CONST
#else
#define CONST const
#endif

int
sigaction (int sig, CONST struct sigaction *act, struct sigaction *oldact)
{
  int ret;
  ret = user_sigaction (sig, act, oldact);
  errno = chkr_errno;
  return ret;
}
#endif
