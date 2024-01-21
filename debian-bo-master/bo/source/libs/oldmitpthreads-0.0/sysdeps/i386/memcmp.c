#include <string.h>

#ifdef isbcmp
#define	memcmp	bcmp
#define size_t	int
#endif

#include "asm-ops.h"

int
memcmp(const void * cs,const void * ct,size_t count)
{
  register int __res;

#ifdef isbcmp
  if (count <= 0) return 0;
#endif

  __asm__ __volatile__ ("cld\n\t"
	"repe\n\t"
	"cmpsb\n\t"
	"je " LF(1) "\n\t"
	"sbbl %%eax,%%eax\n\t"
	"orb $1,%%al\n"
	LL(1) 
	:"=a" (__res):"0" (0),"S" (cs),"D" (ct),"c" (count)
	:"si","di","cx");
  return __res;
}
