#include <unistd.h>
#include <errno.h>
#undef _POSIX_PTHREADS
#include <syscall.h>
#undef lseek

#include "dirstream.h"

#ifdef __SVR4_I386_ABI_L1__
#define lseek __libc_lseek
#else
static inline
_syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)
#endif

#ifdef __ELF__
#pragma weak seekdir = __libc_seekdir
#endif

void
__libc_seekdir(DIR * dir, off_t offset)
{
  if (!dir) {
    errno = EBADF;
    return;
  }

#if 0
  switch (dir->dd_getdents)
  {
  case no_getdents:
  case have_getdents:
  case unknown:
    dir->dd_nextoff = lseek(dir->dd_fd, offset, SEEK_SET);
    dir->dd_size = dir->dd_nextloc = 0;
    break;

  default:
    errno = EBADF;
    break;
  }
#else
  dir->dd_nextoff = lseek(dir->dd_fd, offset, SEEK_SET);
  dir->dd_size = dir->dd_nextloc = 0;
#endif
}
