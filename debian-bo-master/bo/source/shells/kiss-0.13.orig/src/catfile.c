#include "kiss.h"

void catfile (FILE *inf, char *name, int showname, int printonly)
{
    int
	ch;
    
    if (showname)
	printf ("\n%s\n", name);

    while (1)
    {
	ch = fgetc (inf);
	if (feof (inf))
	    break;
	if (! isprint (ch) && printonly)
	    printf ("\\%2.2x", ch);
	else
	    putchar (ch);
    }
}
