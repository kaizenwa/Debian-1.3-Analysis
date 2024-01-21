#include "kiss.h"

char *xstrdup (char *s)
{
    register char
	*ret;

    if (! (ret = strdup (s)))
	error ("can't stringdup %d bytes", strlen (s));

    return (ret);
}
