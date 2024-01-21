#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#undef _POSIX_PTHREADS
#include <syscall.h>
#include "dirstream.h"

#ifdef __SVR4_I386_ABI_L1__
#define close	__libc_close
#else
static inline
_syscall1(int,close,int,fd)
#endif

#ifdef __ELF__
#pragma weak closedir = __libc_closedir
#endif

#ifdef _LIBPTHREAD
#define __libc_closedir closedir
#endif

int
__libc_closedir(DIR * dir)
{
  int fd;

  if (!dir) {
    errno = EBADF;
    return -1;
  }

#ifdef _LIBPTHREAD
  pthread_mutex_lock (dir->dd_lock);
#endif

  /* We need to check dd_fd. */
  if (dir->dd_fd == -1)
  {
    errno = EBADF;
    return -1;
  }
  fd = dir->dd_fd;
  dir->dd_fd = -1;
  free(dir->dd_buf);

  /* Other threads may wake up. */
#ifdef _LIBPTHREAD
  pthread_mutex_free (dir->dd_lock);
#endif

  free(dir);
  return close(fd);
}
