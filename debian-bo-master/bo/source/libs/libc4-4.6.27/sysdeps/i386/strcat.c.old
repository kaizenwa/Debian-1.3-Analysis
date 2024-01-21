#include <string.h>

#include "asm-ops.h"

char * strcat(char * dest,const char * src)
{
__asm__("cld\n\t"
	"repne\n\t"
	"scasb\n\t"
	"decl %1\n"
	LL(1) "\tlodsb\n\t"
	"stosb\n\t"
	"testb %%al,%%al\n\t"
	"jne " LB(1)
	::"S" (src),"D" (dest),"a" (0),"c" (0xffffffff):"si","di","ax","cx");
return dest;
}
