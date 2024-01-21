#include "lcall7.h"


int
lcall7(int opcode, ...)
{
	register int res;

	__asm__ volatile ("addl $4,%%esp\n"
			".byte\t0x9a,0,0,0,0,7,0\n"
			"\tjnc Lexit\n"
			"\tmovl %%eax,_errno\n"
			"\tmovl $-1,%%eax\n"
			"Lexit:\tsubl $4,%%esp\n\t"
			: "=a" (res)
			: "0" (opcode));

	return res;
}
