#include "kiss.h"

int morefile (FILE *f, char *name, int showname, FILE *mystdin)
{
    register int
	ch,
	lines = 0;
    
    if (showname)
	printf ("--------- %s --------\n", name);

    while (1)
    {
	ch = fgetc (f);
	if (feof (f))
	{
	    printf ("more: q=quit, n=next file: ");
	    return (getinput (mystdin) != 'q');
	}

	putchar (ch);
	if (ch == '\n')
	{
	    if (++lines >= 23)
	    {
		printf ("more: q=quit, n=next file, enter=next page: ");
		if ( (ch = getinput (mystdin)) == 'q')
		    return (0);
		else if (ch == 'n')
		    return (1);
		lines = 0;
	    }
	}
    }
}
