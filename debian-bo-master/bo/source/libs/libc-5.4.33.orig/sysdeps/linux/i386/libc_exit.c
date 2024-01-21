#include <unistd.h>
#include <sys/syscall.h>

#ifdef PTHREAD_KERNEL
#pragma weak machdep_sys__exit = __machdep_sys__exit

void
__machdep_sys__exit(int exit_code)
#else /* PTHREAD_KERNEL */

#ifdef _POSIX_THREADS
#pragma weak _exit
#endif

void
_exit(int exit_code)
#endif /* PTHREAD_KERNEL */
{
#if defined(__PIC__) || defined(__pic__)
	__asm__("pushl %%ebx\n\t"
		"movl %%ecx,%%ebx\n\t"
		"int $0x80\n\t"
		"popl %%ebx"
		::"a" (SYS_exit),"c" (exit_code));
#else
	__asm__("int $0x80"
		::"a" (SYS_exit),"b" (exit_code));
#endif
}
