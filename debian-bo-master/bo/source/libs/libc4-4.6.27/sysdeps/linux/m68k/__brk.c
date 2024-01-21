#include <unistd.h>
#include <sys/syscall.h>
#include <errno.h>

void * ___brk_addr = 0;

int __brk(void * end_data_seg)
{
	__asm__ volatile ("movel %2,d1\n\t"
			  "moveq %1,d0\n\t"
			  "trap  #0\n\t"
			  "movel d0,%0"
		:"=g" (___brk_addr)
		:"i" (SYS_brk),"g" (end_data_seg) : "d0", "d1");
	if (___brk_addr == end_data_seg)
		return 0;
	errno = ENOMEM;
	return -1;
}
