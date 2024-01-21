#include "kiss.h"

Stringstack setstring (char *newstring)
{
    Stringstack
	ret;

    ret.nstr = 1;
    ret.str = xmalloc (sizeof (char *));
    ret.str [0] = xstrdup (newstring);

    return (ret);
}
