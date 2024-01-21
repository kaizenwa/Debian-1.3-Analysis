#include <errno.h>
#include <linux/utsname.h>
#include <sys/syscall.h>

int
__old__uname (struct old_utsname *buf)
{
	register int res asm ("d0") = SYS_olduname;

	__asm__("movel %2,d1\n\t"
                "trap #0"
		:"=g" (res)
		:"0" (SYS_olduname),"g" (buf)
                : "d0", "d1");
	if (res>=0)
		return res;
	errno = -res;
	return -1;
}
