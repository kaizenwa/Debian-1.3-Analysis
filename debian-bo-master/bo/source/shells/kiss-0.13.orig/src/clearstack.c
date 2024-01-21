#include "kiss.h"

void clearstack (Stringstack *w)
{
    register int
	i;
    
    for (i = 0; i < w->nstr; i++)
	free (w->str [i]);
    free (w->str);

    w->nstr = 0;
    w->str = NULL;
}
