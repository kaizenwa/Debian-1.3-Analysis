#include "kiss.h"

void *xrealloc (void *mem, int newsz)
{
    register void
	*ret;

    if (! (ret = realloc (mem, newsz)) )
	error ("can't realloc to %d bytes", newsz);
    return (ret);
}
    
