#include <errno.h>
#include <sys/resource.h>
#include <sys/syscall.h>

#define PZERO	20

int
getpriority(int which, int who)
{
	register long res asm ("d0") = SYS_getpriority;

#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%edx,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (res)
		:"0" (SYS_getpriority),"d" (which), "c" (who));
#else
	__asm__ volatile ("movel %2,d1\n\t"
			  "movel %3,d2\n\t"
			  "trap  #0\n\t"
		:"=g" (res)
		:"0" (SYS_getpriority), "g" (which), "g" (who)
		: "d0", "d1", "d2");
#endif
	if (res >= 0) {
		errno = 0;
		return (int) PZERO - res;
	}
	errno = -res;
	return -1;
}
