#include <syscall.h>
#include <errno.h>
#include <sys/stat.h>
#include <gnu-stabs.h>

#ifdef __SVR4_I386_ABI_L1__
#define prev_lstat	__prev_lstat
#else
static inline
_syscall2(int,prev_lstat,const char *,path, struct stat *,statbuf)
#endif

int
_lxstat(int version, const char * path, struct stat * statbuf)
{
  switch(version)
  {
  case 1:
    return prev_lstat (path, statbuf);
  default:
    errno = EINVAL;
    return -1;
  }
}

elf_alias (_lxstat, __lxstat);
