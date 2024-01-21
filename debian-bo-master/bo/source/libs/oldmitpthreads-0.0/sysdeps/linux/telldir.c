#include <unistd.h>
#include <syscall.h>
#include <errno.h>
#undef lseek

#include "dirstream.h"

#ifdef __SVR4_I386_ABI_L1__
#define lseek	__libc_lseek
#else
static inline
_syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)
#endif

#ifdef __ELF__
#pragma weak telldir = __libc_telldir
#endif

off_t
__libc_telldir(DIR * dir)
{
  off_t offset;

  if (!dir) {
    errno = EBADF;
    return -1;
  }

  switch (dir->dd_getdents)
  {
  case no_getdents:
    /* We are running the old kernel. This is the starting offset
       of the next readdir(). */
    offset = lseek(dir->dd_fd, 0, SEEK_CUR);
    break;

  case unknown:
    /* readdir () is not called yet. but seekdir () may be called. */
  case have_getdents:
    /* The next entry. */
    offset = dir->dd_nextoff;
    break;

  default:
    errno = EBADF;
    offset = -1;
  }

  return offset;
}
