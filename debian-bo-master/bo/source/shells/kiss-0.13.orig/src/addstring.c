#include "kiss.h"

Stringstack addstring (Stringstack a, Stringstack b)
{
    Stringstack
	ret;
    int
	i;

    ret = copystringstack (a, 0, a.nstr - 1);
    for (i = 0; i < b.nstr; i++)
	addstringtostack (&ret, b.str [i]);

    return (ret);
}
