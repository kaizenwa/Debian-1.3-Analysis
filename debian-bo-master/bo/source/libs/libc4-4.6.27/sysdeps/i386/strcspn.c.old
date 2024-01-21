#include <string.h>

#include "asm-ops.h"

size_t strcspn(const char * cs, const char * ct)
{
register char * __res;
__asm__ __volatile__ ("cld\n\t"
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
	"jne " LB(1) "\n"
	LL(2) "\tdecl %0"
	:"=S" (__res):"a" (0),"c" (0xffffffff),"0" (cs),"g" (ct)
	:"ax","cx","dx","di");
return (size_t) (__res-cs);
}
