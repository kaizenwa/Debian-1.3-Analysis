#include "kiss.h"

Stringstack reexpand (Stringstack s)
{
    register int
	i;
    char
	buf [FILENAMELEN];
    Stringstack
	exp,
	ret;
    
    /* expand variables */
    for (i = 0; i < s.nstr; i++)
	if (expandvars (s.str [i], buf))
	{
	    free (s.str [i]);
	    s.str [i] = xstrdup (buf);
	}

    /* expand wildcards if any */
    ret = copystringstack (s, 0, 0);
    
    for (i = 1; i < s.nstr; i++)
    {
	exp = setexpandedstring (s.str [i]);
	addstringstack (&ret, exp);
	clearstack (&exp);
    }
    
    return (ret);
}

