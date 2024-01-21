#include "kiss.h"

Stringstack copystringstack (Stringstack s, int first, int last)
{
    Stringstack
	ret = { NULL, 0 };
    register int
	i;

    for (i = first; i <= last; i++)
	addstringtostack (&ret, s.str [i]);

    return (ret);
}
