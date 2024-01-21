#include <syscall.h>
#include <unistd.h>
#include <errno.h>

#ifndef __i386__
inline static
_syscall5(int,_llseek,int,fd,off_t,hoff,off_t,loff,loff_t*,res,int,whence);
#endif

loff_t
__llseek (int fd, loff_t offset, int whence)
{
  int ret;
  loff_t result;

#ifndef __i386__
  ret = _llseek (fd, (off_t) (offset >> 32),
#else
  ret = syscall (SYS__llseek, fd, (off_t) (offset >> 32),
#endif
	(off_t) (offset & 0xffffffff), &result, whence);

  return ret ? (loff_t) ret : result;
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__llseek, llseek);
#endif
