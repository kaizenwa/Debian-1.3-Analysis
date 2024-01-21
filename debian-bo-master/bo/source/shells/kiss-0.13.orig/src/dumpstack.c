#include "kiss.h"

void dumpstack (char *msg, Stringstack s)
{
    register int
	i;
    
    if (flags.debug)
    {
	printf ("%s ", msg);
	for (i = 0; i < s.nstr; i++)
	    printf ("[%s] ", s.str [i]);
	putchar ('\n');
    }
}
