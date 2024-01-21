#include <sys/stat.h>
#define DIRENT_ILLEGAL_ACCESS 1
#define NUMENT 1
#include <dirent.h>
#include <sys/dirent.h>
#include <fcntl.h>
#include <errno.h>

#include "checker.h"

/*
 * opendir just makes an open() call - it return NULL if it fails
 * (open sets errno), otherwise it returns a DIR * pointer.
 */
DIR *
opendir (const char *name)
{
  int fd;
  struct stat statbuf;
  struct dirent *buf;
  DIR *ptr;

  if (stat (name, &statbuf))
    return NULL;
  if (!S_ISDIR (statbuf.st_mode))
    {
      chkr_errno = ENOTDIR;
      return NULL;
    }
  if ((fd = open (name, O_RDONLY)) < 0)
    return NULL;
  /* According to POSIX, directory streams should be closed when
   * exec. From "Anna Pluzhnikov" <besp@midway.uchicago.edu>.
   */
  if (fcntl (fd, F_SETFD, FD_CLOEXEC) < 0)
    return NULL;
    
  if (!(ptr = sys_malloc (sizeof (DIR))))
    {
      close (fd);
      chkr_errno = ENOMEM;
      return NULL;
    }
#ifdef NUMENT
  if (!(buf = sys_malloc (NUMENT * sizeof (struct dirent))))
#else
  ptr->dd_max = statbuf.st_blksize;
  if (ptr->dd_max < 512)
    ptr->dd_max = 512;
  if (!(buf = sys_malloc (ptr->dd_max)))
#endif
    {
      close (fd);
      sys_free (ptr);
      chkr_errno = ENOMEM;
      return NULL;
    }
  ptr->dd_fd = fd;
  ptr->dd_loc = ptr->dd_size = 0;
  ptr->dd_buf = buf;
  return ptr;
}
