#include <errno.h>
#include <linux/utsname.h>
#include <sys/syscall.h>

int
__old__uname (struct old_utsname *buf)
{
	register int res;

#if defined(__PIC__) || defined(__pic__)
	__asm__("pushl %%ebx\n\t"
		"movl %%ecx,%%ebx\n\t"
		"int $0x80\n\t"
		"popl %%ebx"
		:"=a" (res)
		:"0" (SYS_olduname),"c" (buf));
#else
	__asm__("int $0x80"
		:"=a" (res)
		:"0" (SYS_olduname),"b" (buf));
#endif
	if (res>=0)
		return res;
	errno = -res;
	return -1;
}
