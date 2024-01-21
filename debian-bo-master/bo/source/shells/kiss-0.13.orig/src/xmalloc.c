#include "kiss.h"

void *xmalloc (int sz)
{
    register void
	*ret;

    if (! (ret = malloc (sz)) )
	error ("can't allocate %d bytes", sz);
    return (ret);
}
