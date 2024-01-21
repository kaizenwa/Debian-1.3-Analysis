#include <string.h>

#include "asm-ops.h"

int strcmp(const char * cs,const char * ct)
{
register int __res;
__asm__("cld\n"
	LL(1) "\tlodsb\n\t"
	"scasb\n\t"
	"jne " LF(2) "\n\t"
	"testb %%al,%%al\n\t"
	"jne " LB(1) "\n\t"
	"xorl %%eax,%%eax\n\t"
	"jmp " LF(3) "\n"
	LL(2) "\tsbbl %%eax,%%eax\n\t"
	"orb $1,%%al\n"
	LL(3) 
	:"=a" (__res):"S" (cs),"D" (ct):"si","di");
return __res;
}
