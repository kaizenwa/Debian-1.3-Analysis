#include "kiss.h"

int doread (Stringstack s)
{
    char
	buf [FILENAMELEN];
    register char
	*cp;
    
    if (s.nstr > 2 || getopt (s.nstr, s.str, "h") != -1)
	return (warning ("Bad commandline.\n"
			 "Usage: read VAR - get input and set VAR to input\n"
			 "       read     - wait for input\n"));

    if (s.nstr == 2)
    {
	for (cp = s.str [1]; *cp; cp++)
	    if (! isupper (*cp))
		return (warning ("variable must be in upper case"));
	gets (buf);
	addtoenv (s.str [1], buf);
    }
    else
	gets (buf);
    
    return (0);
}
