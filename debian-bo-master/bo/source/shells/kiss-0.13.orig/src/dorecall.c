#include "kiss.h"

int dorecall (Stringstack s)
{
    register int
	j,
	i;
    
    if (s.nstr != 2)
	error ("recall command needs one argument");

    for (i = nhislist - 1; i >= 0; i--)
	if (! strncmp (s.str [1], hislist [i].str [0], strlen (s.str [1])))
	{
	    for (j = 0; j < hislist [i].nstr; j++)
		printf ("%s ", hislist [i].str [j]);
	    putchar ('\n');
	    runcmd (hislist [i]);
	    return (laststatus);
	}

    return (warning ("\"%s\": no such command in history list",
		     s.str [1]));
}
