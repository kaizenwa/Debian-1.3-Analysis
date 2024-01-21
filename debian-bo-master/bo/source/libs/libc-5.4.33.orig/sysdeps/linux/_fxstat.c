#include <syscall.h>
#include <errno.h>
#include <sys/stat.h>
#include <gnu-stabs.h>
#include "glibcstat.h"

#ifdef __SVR4_I386_ABI_L1__
#define prev_fstat	__prev_fstat
#else
static inline
_syscall2(int,prev_fstat,int,fd, struct stat *,statbuf)
#endif

#ifdef PTHREAD_KERNEL

int
__machdep_sys__fxstat(int version, int fd, struct stat * statbuf)
{
  int result;
  struct stat kbuf;
  struct glibcstat *buf = (struct glibcstat *) statbuf; 

  switch(version)
  {
  case 1:
    result = prev_fstat (fd, statbuf);
    break;

  case _STAT_VER_LINUX:
    result = prev_fstat (fd, &kbuf);
    if (result == 0)
    {
      /* Convert to current kernel version of `struct stat'.  */
      buf->st_dev = kbuf.st_dev;
      buf->__pad1 = 0;
      buf->st_ino = kbuf.st_ino;
      buf->st_mode = kbuf.st_mode;
      buf->st_nlink = kbuf.st_nlink;
      buf->st_uid = kbuf.st_uid;
      buf->st_gid = kbuf.st_gid;
      buf->st_rdev = kbuf.st_rdev;
      buf->__pad2 = 0;
      buf->st_size = kbuf.st_size;
      buf->st_blksize = kbuf.st_blksize;
      buf->st_blocks = kbuf.st_blocks;
      buf->st_atime = kbuf.st_atime;
      buf->__unused1 = 0;
      buf->st_mtime = kbuf.st_mtime;
      buf->__unused2 = 0;
      buf->st_ctime = kbuf.st_ctime;
      buf->__unused3 = 0;
      buf->__unused4 = 0;
      buf->__unused5 = 0;
    }
    break;

  default:
    result = EINVAL;
    break;
  }

  return result;
}

#else

#ifdef _POSIX_THREADS
#pragma weak _fxstat
#endif

int
_fxstat(int version, int fd, struct stat * statbuf)
{
  int result;
  struct stat kbuf;
  struct glibcstat *buf = (struct glibcstat *) statbuf; 

  switch(version)
  {
  case 1:
    result = prev_fstat (fd, statbuf);
    break;

  case _STAT_VER_LINUX:
    result = prev_fstat (fd, &kbuf);
    if (result == 0)
    {
      /* Convert to current kernel version of `struct stat'.  */
      buf->st_dev = kbuf.st_dev;
      buf->__pad1 = 0;
      buf->st_ino = kbuf.st_ino;
      buf->st_mode = kbuf.st_mode;
      buf->st_nlink = kbuf.st_nlink;
      buf->st_uid = kbuf.st_uid;
      buf->st_gid = kbuf.st_gid;
      buf->st_rdev = kbuf.st_rdev;
      buf->__pad2 = 0;
      buf->st_size = kbuf.st_size;
      buf->st_blksize = kbuf.st_blksize;
      buf->st_blocks = kbuf.st_blocks;
      buf->st_atime = kbuf.st_atime;
      buf->__unused1 = 0;
      buf->st_mtime = kbuf.st_mtime;
      buf->__unused2 = 0;
      buf->st_ctime = kbuf.st_ctime;
      buf->__unused3 = 0;
      buf->__unused4 = 0;
      buf->__unused5 = 0;
    }
    break;

  default:
    errno = EINVAL;
    result = -1;
    break;
  }

  return result;
}

elf_alias (_fxstat, __fxstat);

#endif
