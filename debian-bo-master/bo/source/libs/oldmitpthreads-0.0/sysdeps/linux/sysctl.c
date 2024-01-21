#include <syscall.h>
#include <sys/sysctl.h>
#include <linux/sysctl.h>

static inline
_syscall1(int, _sysctl, struct __sysctl_args *, args);

int
sysctl(int *name, int nlen, void *oldval, size_t *oldlenp,
	void *newval, size_t newlen)
{
  struct __sysctl_args args;

  args.name = name;
  args.nlen = nlen;
  args.oldval = oldval;
  args.oldlenp = oldlenp;
  args.newval = newval;
  args.newlen = newlen;
  return _sysctl(&args);
}

#ifdef __ELF__
#ifdef __GNUC__
int sysctl(int *, int, void *, size_t *, void *, size_t)
   __attribute__ ((weak));
#else
#pragma weak sysctl
#endif
#endif
