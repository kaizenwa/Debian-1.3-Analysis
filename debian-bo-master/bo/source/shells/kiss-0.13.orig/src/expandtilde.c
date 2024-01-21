#include "kiss.h"

void expandtilde (Stringstack s)
{
    char
	buf [FILENAMELEN],    
	rest [FILENAMELEN];
    register char
	*cp;
    register int
	i;

    for (i = 1; i < s.nstr; i++)
    {
	strcpy (buf, s.str [i]);
	while ( (cp = strstr (buf, "~/")) )
	{
	    strcpy (rest, cp + 2);
	    strcpy (cp, homedir);
	    strcat (buf, "/");
	    strcat (buf, rest);
	}
	free (s.str [i]);
	s.str [i] = xstrdup (buf);
    }
}
