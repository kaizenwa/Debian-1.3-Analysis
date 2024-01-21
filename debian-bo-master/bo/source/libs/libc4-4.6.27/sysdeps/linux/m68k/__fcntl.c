#include <fcntl.h>
#include <errno.h>
#include <sys/syscall.h>
#include <stdarg.h>

int
__fcntl(int fildes, int cmd, ...)
{
	register int res asm ("d0") = SYS_fcntl;
	va_list arg;

	va_start(arg,cmd);
	__asm__("movel %2,d1\n\t"
                "movel %3,d2\n\t"
                "movel %4,d3\n\t"
                "trap #0\n\t"
		:"=g" (res)
		:"0" (SYS_fcntl),"g" (fildes),"g" (cmd), "d" (va_arg(arg,int))
                : "d0", "d1", "d2", "d3");
	if (res>=0)
		return res;
	errno = -res;
	va_end (arg);
	return -1;
} /*  */
