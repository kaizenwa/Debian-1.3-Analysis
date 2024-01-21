#include <errno.h>
#include <sys/ptrace.h>
#include <sys/syscall.h>

int
ptrace(int request, int pid, int addr, int data)
{
	long ret;
	long res;
	if (request > 0 && request < 4) (long *)data = &ret;

#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%edi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (res)
		:"0" (SYS_ptrace),"D" (request), "c" (pid),
		 "d" (addr), "S" (data));
#else
	__asm__ volatile ("movel %1,d0\n\t"
			  "movel %2,d1\n\t"
			  "movel %3,d2\n\t"
			  "movel %4,d3\n\t"
			  "movel %5,d4\n\t"
			  "trap  #0\n\t"
			  "movel d0,%0"
		:"=g" (res)
		:"i" (SYS_ptrace), "g" (request), "g" (pid),
		 "g" (addr), "g" (data) : "d0", "d1", "d2", "d3", "d4");
#endif

	if (res >= 0) {
		if (request > 0 && request < 4) {
			errno = 0;
			return (ret);
		}
		return (int) res;
	}
	errno = -res;
	return -1;
}
