#include <string.h>

#include "asm-ops.h"

char * strchr(const char * s,int c)
{
register char * __res;
__asm__("cld\n\t"
	"movb %%al,%%ah\n"
	LL(1) "\tlodsb\n\t"
	"cmpb %%ah,%%al\n\t"
	"je " LF(2) "\n\t"
	"testb %%al,%%al\n\t"
	"jne " LB(1) "\n\t"
	"movl $1,%1\n"
	LL(2) "\tleal -1(%1),%0\n"
	:"=a" (__res):"S" (s),"0" (c):"si");
return __res;
}

#include <gnu-stabs.h>
#ifdef elf_alias
elf_alias (strchr, index);
#endif
