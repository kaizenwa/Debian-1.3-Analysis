#include "kiss.h"

int dohistory (Stringstack s)
{
    register int
	i,
	j;
    
    if (s.nstr > 2)
	return (warning ("history command takes one optional argument"));

    for (i = 0; i < nhislist; i++)
	if (s.nstr == 1 ||
	    ! strncmp (s.str [1], hislist [i].str [0], strlen (s.str [1]))
	   )
	{
	    for (j = 0; j < hislist [i].nstr; j++)
		printf (" %s", hislist [i].str [j]);
	    putchar ('\n');
	}

    return (0);
}
