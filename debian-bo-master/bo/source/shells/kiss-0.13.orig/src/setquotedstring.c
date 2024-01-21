#include "kiss.h"

Stringstack setquotedstring (char *str, int quote)
{
    register int
	len = strlen (str) - 1;
    Stringstack
	ret;

    if (len > 0 && str [len] == quote)
    {
	str [len] = '\0';
	ret = setstring (str + 1);
	str [len] = quote;
	return (ret);
    }
    else
	return (setstring (""));
}
