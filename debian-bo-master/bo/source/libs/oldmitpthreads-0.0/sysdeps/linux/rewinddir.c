#include <unistd.h>
#include <errno.h>
#undef _POSIX_PTHREADS
#include <syscall.h>
#undef lseek

#include "dirstream.h"

#ifdef __SVR4_ABI_L1
#define lseek __libc_lseek
#else
static inline
_syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)
#endif

#ifdef __ELF__
#pragma weak rewinddir = __libc_rewinddir
#endif

/*
 * rewinddir() just does an lseek(fd,0,0) - see close for comments
 */
void
__libc_rewinddir(DIR * dir)
{
  if (!dir) {
    errno = EBADF;
    return;
  }
  lseek(dir->dd_fd,0,SEEK_SET);
  dir->dd_nextoff = dir->dd_nextloc = dir->dd_size = 0;
}
