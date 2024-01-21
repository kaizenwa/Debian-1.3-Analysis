#include <syscall.h>
#include <sys/types.h>
#include <sys/mman.h>

extern int chkr_errno;
caddr_t chkr_mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t off);

#ifndef __MAX_ERRNO
#define __MAX_ERRNO 4096
#endif

#if defined(__PIC__) || defined (__pic__)
static inline long
_mmap(unsigned long *buffer)
{ 
  long __res;
  __asm__ __volatile__ ("pushl %%ebx\n\t"
		  "movl %%ecx,%%ebx\n\t"
		  "int $0x80\n\t" 
		  "popl %%ebx"
	: "=a" (__res)
	: "0" (SYS_mmap),"c" ((long)(buffer)):"bx");
  if ((__res) >= 0 || (__res) < -__MAX_ERRNO)
	return (long) __res;
  chkr_errno = -__res;
  return -1;
}

#else

static inline long
_mmap(unsigned long *buffer)
{
  long __res;
  __asm__ __volatile__ ("int $0x80"
	: "=a" (__res)
	: "0" (SYS_mmap),"b" ((long)(buffer)):"bx");
  if ((__res) >= 0 || (__res) < -__MAX_ERRNO)
    return (long) __res;
  chkr_errno = -__res;
  return -1;
}
#endif

caddr_t
chkr_mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t off)
{
	unsigned long buffer[6];

	buffer[0] = (unsigned long)addr;
	buffer[1] = (unsigned long)len;
	buffer[2] = (unsigned long)prot;
	buffer[3] = (unsigned long)flags;
	buffer[4] = (unsigned long)fd;
	buffer[5] = (unsigned long)off;
	return (caddr_t) _mmap(buffer);
}
