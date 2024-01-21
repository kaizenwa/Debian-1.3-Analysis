#include <syscall.h>
#include <sys/signal.h>

#ifdef PTHREAD_KERNEL

#pragma weak machdep_sys_sigsuspend = __machdep_sys_sigsuspend

int __machdep_sys_sigsuspend (const sigset_t *);

int
__machdep_sys_sigsuspend (const sigset_t *sigmask)
{
	int res;

#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%esi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (res)
		:"0" (SYS_sigsuspend), "S" (0), "c" (0), "d" (*sigmask));
#else
	__asm__("int $0x80"
		:"=a" (res)
		:"0" (SYS_sigsuspend), "b" (0), "c" (0), "d" (*sigmask));
#endif
	return res;
}

#else /* PTHREAD_KERNEL */

#ifdef _POSIX_THREADS
#pragma weak sigsuspend
#endif

int
sigsuspend(const sigset_t *sigmask)
{
	int res;

#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%esi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (res)
		:"0" (SYS_sigsuspend), "S" (0), "c" (0), "d" (*sigmask));
#else
	__asm__("int $0x80"
		:"=a" (res)
		:"0" (SYS_sigsuspend), "b" (0), "c" (0), "d" (*sigmask));
#endif
	if (res >= 0)
		return res;
	errno = -res;
	return -1;
}

#endif /* PTHREAD_KERNEL */
