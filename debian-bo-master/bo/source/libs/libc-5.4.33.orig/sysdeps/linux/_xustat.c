#include <syscall.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef __SVR4_I386_ABI_L1__
#define prev_ustat	__prev_ustat
#else
static inline
_syscall2(int,prev_ustat,dev_t,dev, struct ustat *,ustatbuf)
#endif

int _xustat (int, dev_t, struct ustat *);

int
_xustat(int version, dev_t dev, struct ustat * ustatbuf)
{
  switch(version)
  {
  case 1:
    return prev_ustat (dev, ustatbuf);
  default:
    errno = EINVAL;
    return -1;
  }
}
