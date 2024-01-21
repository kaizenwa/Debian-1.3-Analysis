#include <syscall.h>
#include <errno.h>
#include <sys/stat.h>
#include <gnu-stabs.h>

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
  switch(version)
  {
  case 1:
    return prev_fstat (fd, statbuf);
  default:
    return EINVAL;
  }
}

#else

#ifdef _POSIX_THREADS
#pragma weak _fxstat
#endif

int
_fxstat(int version, int fd, struct stat * statbuf)
{
  switch(version)
  {
  case 1:
    return prev_fstat (fd, statbuf);
  default:
    errno = EINVAL;
    return -1;
  }
}

elf_alias (_fxstat, __fxstat);

#endif
