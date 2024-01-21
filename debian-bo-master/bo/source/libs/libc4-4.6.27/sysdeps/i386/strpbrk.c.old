#include <string.h>

#include "asm-ops.h"

char * strpbrk(const char * cs,const char * ct)
{
register char * __res;
__asm__("cld\n\t"
	"movl %4,%%edi\n\t"
	"repne\n\t"
	"scasb\n\t"
	"notl %%ecx\n\t"
	"decl %%ecx\n\t"
	"movl %%ecx,%%edx\n"
	LL(1) "\tlodsb\n\t"
	"testb %%al,%%al\n\t"
	"je " LF(2) "\n\t"
	"movl %4,%%edi\n\t"
	"movl %%edx,%%ecx\n\t"
	"repne\n\t"
	"scasb\n\t"
	"jne " LB(1) "\n\t"
	"decl %0\n\t"
	"jmp " LF(3) "\n"
	LL(2) "\txorl %0,%0\n"
	LL(3) 
	:"=S" (__res):"a" (0),"c" (0xffffffff),"0" (cs),"g" (ct)
	:"ax","cx","dx","di");
return __res;
}
