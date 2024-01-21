#include <unistd.h>
#include <sys/syscall.h>

void
_exit(int exit_code)
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
