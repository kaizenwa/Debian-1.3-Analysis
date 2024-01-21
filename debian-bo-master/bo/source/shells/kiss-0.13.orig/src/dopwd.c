#include "kiss.h"

int dopwd (Stringstack s)
{
    register char
	*cp;
    
    if (s.nstr != 1)
	error ("Bad commandline.\n"
	       "Usage: %s      to print current working directory\n",
	       progname);

    if (! (cp = getcwd (NULL, 0)) )
	return (warning ("failure retrieving current working directory\n"));
    printf ("%s\n", cp);
    free (cp);

    return (0);
}
		
