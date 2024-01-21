#include "kiss.h"

void addstringstack (Stringstack *what, Stringstack add)
{
    register int
	i;
    
    what->str = xrealloc (what->str,
			  (what->nstr + add.nstr) * sizeof (char *));
    for (i = 0; i < add.nstr; i++)
	what->str [what->nstr + i] = xstrdup (add.str [i]);
    what->nstr += add.nstr;
}
