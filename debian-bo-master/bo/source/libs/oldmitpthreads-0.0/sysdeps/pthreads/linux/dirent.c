#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#undef _POSIX_PTHREADS
#include <syscall.h>
#include "../dirstream.h"

#ifdef __SVR4_I386_ABI_L1__
#define close	__libc_close
#else
static inline
_syscall1(int,close,int,fd)
#endif

int closedir(DIR * dir)
{
  int fd;

  if (!dir) {
    errno = EBADF;
    return -1;
  }
  pthread_mutex_lock (dir->dd_lock);
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
  __pthread_mutex_free (dir->dd_lock);
  free(dir);
  return close(fd);
}

DIR *
opendir(const char * name)
{
  DIR *ptr;
  ptr = __libc_opendir (name);
  if (ptr)
    ptr->dd_lock = __pthread_mutex_malloc (0);
  return ptr;
}

struct dirent *
readdir(DIR * dir)
{
  struct dirent *de;

  if (!dir) {
    errno = EBADF;
    return NULL; 
  }

  pthread_mutex_lock (dir->dd_lock);
  de = __libc_readdir (dir);
  pthread_mutex_unlock (dir->dd_lock);

  return de;
}

int
readdir_r (DIR * dir, struct dirent * entry, struct dirent ** ret)
{
  int result;

  if (!dir || !entry || !ret || !*ret) {
    return EBADF;
  }

  pthread_mutex_lock (dir->dd_lock);
  result = __libc_readdir_r (dir, entry, ret);
  pthread_mutex_unlock (dir->dd_lock);
  return result;
}

void
rewinddir(DIR * dir)
{
  if (!dir) {
    errno = EBADF;
    return;
  }
  pthread_mutex_lock (dir->dd_lock);
  __libc_rewinddir (dir);
  pthread_mutex_unlock (dir->dd_lock);
}
#include "../dirstream.h"

void
seekdir(DIR * dir, off_t offset)
{
  if (!dir) {
    errno = EBADF;
    return;
  }

  pthread_mutex_lock (dir->dd_lock);
  __libc_seekdir (dir, offset);
  pthread_mutex_unlock (dir->dd_lock);
}

off_t
telldir(DIR * dir)
{
  off_t offset;

  if (!dir) {
    errno = EBADF;
    return -1;
  }

  pthread_mutex_lock (dir->dd_lock);
  offset = __libc_telldir (dir);
  pthread_mutex_unlock (dir->dd_lock);
  return offset;
}
