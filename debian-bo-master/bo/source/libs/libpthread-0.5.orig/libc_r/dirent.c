#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#undef _POSIX_PTHREADS
#include <syscall.h>
#include "dirstream.h"

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
  pthread_mutex_unlock(dir->dd_lock);
  pthread_mutex_destroy(dir->dd_lock);
  free(dir->dd_lock);
  free(dir);
  return close(fd);
}

DIR *
opendir(const char * name)
{
  DIR *ptr;
  pthread_mutex_t *lock;

  lock = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
  if (lock == NULL) {
    errno = ENOMEM;
    return NULL;
  }
  ptr = __libc_opendir (name);
  if (ptr) {
    pthread_mutex_init(lock, NULL);
    ptr->dd_lock = lock;
  } else {
    free(lock);
  }
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
