#include <syscall.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef __SVR4_I386_ABI_L1__
#define prev_mknod	__prev_mknod
#else
static inline
_syscall3(int,prev_mknod,const char *,path, mode_t, mode, dev_t, dev)
#endif

int
_xmknod (int version, const char * path, mode_t mode, dev_t *dev)
{
  switch(version)
  {
  case 1:
    return prev_mknod (path, mode, *dev);
  default:
    errno = EINVAL;
    return -1;
  }
}
