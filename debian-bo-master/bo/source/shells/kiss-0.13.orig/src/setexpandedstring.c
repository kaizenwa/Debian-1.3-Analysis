#include "kiss.h"

Stringstack setexpandedstring (char *s)
{
    glob_t
	globres;
    register int
	i;
    Stringstack
	ret;

    /* no wildcards? just a string. */
    if (! strchr (s, '*') && ! strchr (s, '?'))
	return (setstring (s));

    /* initialize retval */
    ret.nstr = 0;
    ret.str = NULL;

    if (glob (s, GLOB_NOCHECK, NULL, &globres))
	warning ("file globbing failure");
    else
    {
	ret.nstr = globres.gl_pathc;
	ret.str = xmalloc ( (ret.nstr + 1) * sizeof (char *) );
	for (i = 0; i < globres.gl_pathc; i++)
	    ret.str [i] = xstrdup (globres.gl_pathv [i]);
	globfree (&globres);
    }

    return (ret);
}
