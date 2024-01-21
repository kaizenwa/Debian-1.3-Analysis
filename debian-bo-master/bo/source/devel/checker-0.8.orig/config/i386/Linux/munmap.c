#include <syscall.h>
#include <sys/types.h>
#include <sys/mman.h>

extern int chkr_errno;
int chkr_munmap(caddr_t addr, size_t len);

#if defined(__PIC__) || defined (__pic__)
int
chkr_munmap(caddr_t addr, size_t len)
{
  long __res;
  __asm__ __volatile__ ("pushl %%ebx\n\t"
		  "movl %%edx,%%ebx\n\t"
		  "int $0x80\n\t"
		  "popl %%ebx"
	: "=a" (__res)
	: "0" (SYS_munmap),"d" ((long)(addr)),"c" ((long)(len)):"bx");
  if (__res >= 0)
    return (int) __res;
  chkr_errno = -__res;
  return -1;
}
#else
int
chkr_munmap(caddr_t addr, size_t len)
{
  long __res;
  __asm__ __volatile__ ("int $0x80"
	: "=a" (__res)
	: "0" (SYS_munmap),"b" ((long)(addr)),"c" ((long)(len)):"bx");
  if (__res >= 0)
    return (int) __res;
  chkr_errno = -__res;
  return -1;
}
#endif

