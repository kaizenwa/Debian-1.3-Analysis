#include <sys/dirent.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#define open __normal_open
#define fcntl __normal_fcntl
#include <fcntl.h>
#include <unistd.h>
#undef _POSIX_PTHREADS
#include <syscall.h>
#include <errno.h>

#include "dirstream.h"

#undef close
#undef open
#undef fcntl 
#undef stat

#ifdef __SVR4_I386_ABI_L1__
#define close	 __libc_close
#define open	__libc_open
#define fcntl	 __libc_fcntl
#else
static inline
_syscall1(int,close,int,fd)

static inline
_syscall2(int,open,const char *,name,int,flags)

static inline
_syscall3(int,fcntl,int,fd,int,cmd,long,args)
#endif

#ifdef __ELF__
#pragma weak opendir = __libc_opendir
#endif

/*
 * opendir just makes an open() call - it return NULL if it fails
 * (open sets errno), otherwise it returns a DIR * pointer.
 */
DIR *
__libc_opendir(const char * name)
{
  int fd;
  struct stat statbuf;
  struct dirent *buf;
  DIR *ptr;

  if (stat(name,&statbuf)) return NULL;
  if (!S_ISDIR(statbuf.st_mode)) {
    errno = ENOTDIR;
    return NULL;
  }
  if ((fd = open(name,O_RDONLY)) < 0)
    return NULL;
  /* According to POSIX, directory streams should be closed when
   * exec. From "Anna Pluzhnikov" <besp@midway.uchicago.edu>.
   */
  if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0)
    return NULL;
  if (!(ptr=malloc(sizeof(*ptr)))) {
    close(fd);
    errno = ENOMEM;
    return NULL;
  }

#if 0
  struct utsname uts;
  int version;
  if (uname (&uts))
  {
    return NULL;
  }

#define MULTI_READDIR_VERSION "1.2"

  /* We check out the kernel version here. */
  version = strncmp (uts.release, MULTI_READDIR_VERSION,
	sizeof (MULTI_READDIR_VERSION));
  if (version < 0)
  {
    ptr->dd_version = SINGLE_READDIR;
  }
  else
  {
    if (version == 0)
      ptr->dd_version = MULTI_READDIR;
    else 
      ptr->dd_version = NEW_READDIR;
  }
#endif

  ptr->dd_max = statbuf.st_blksize;
  if (ptr->dd_max < 512)
      ptr->dd_max = 512;

  if (!(buf=malloc(ptr->dd_max))) {
    close(fd);
    free(ptr);
    errno = ENOMEM;
    return NULL;
  }
  ptr->dd_fd = fd;
  ptr->dd_nextoff = ptr->dd_nextloc = ptr->dd_size = 0;
  ptr->dd_buf = buf;
  ptr->dd_getdents = unknown;
  return ptr;
}
