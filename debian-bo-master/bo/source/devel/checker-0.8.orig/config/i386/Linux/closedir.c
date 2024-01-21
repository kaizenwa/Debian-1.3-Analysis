#include "checker.h"
#define DIRENT_ILLEGAL_ACCESS 1
#include <dirent.h>
#include <errno.h>

int
closedir (DIR * dir)
{
  int fd;

  if (!dir) {
    errno = EBADF;
    return -1;
  }
  fd = dir->dd_fd;
  sys_free (dir->dd_buf);
  sys_free (dir);
  return close (fd);
}
