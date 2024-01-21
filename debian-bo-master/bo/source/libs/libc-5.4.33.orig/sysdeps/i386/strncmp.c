#include <string.h>

#include "asm-ops.h"

int strncmp(const char * cs,const char * ct,size_t count)
{
register int __res;
__asm__("cld\n\t"
	"incl %3\n"
	LL(1) "\tdecl %3\n\t"
	"je " LF(2) "\n\t"
	"lodsb\n\t"
	"scasb\n\t"
	"jne " LF(3) "\n\t"
	"testb %%al,%%al\n\t"
	"jne " LB(1) "\n"
	LL(2) "\txorl %%eax,%%eax\n\t"
	"jmp " LF(4) "\n"
	LL(3) "\tsbbl %%eax,%%eax\n\t"
	"orb $1,%%al\n"
	LL(4) 
	:"=a" (__res):"S" (cs),"D" (ct),"c" (count):"si","di","cx");
return __res;
}
