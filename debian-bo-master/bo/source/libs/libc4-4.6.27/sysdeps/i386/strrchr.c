#include <string.h>
#include "asm-ops.h"

char * strrchr(const char * s,int c)
{
register char * __res;
__asm__("cld\n\t"
	"movb %%al,%%ah\n"
	LL(1) "\tlodsb\n\t"
	"cmpb %%ah,%%al\n\t"
	"jne " LF(2) "\n\t"
	"leal -1(%%esi),%0\n"
	LL(2) "\ttestb %%al,%%al\n\t"
	"jne " LB(1)
	:"=d" (__res):"0" (0),"S" (s),"a" (c):"ax","si");
return __res;
}


#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (strrchr, rindex);
#endif
