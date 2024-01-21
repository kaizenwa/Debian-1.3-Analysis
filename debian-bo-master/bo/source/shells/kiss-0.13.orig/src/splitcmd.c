#include "kiss.h"

int splitcmd (Stringstack series, Stringstack *dest, int from)
{
    register int
	done = 0,
	i;
    char
	buf [FILENAMELEN];
    register char
	*cp;
    
    if (from >= series.nstr)
	return (-1);

    for (i = from; i < series.nstr; i++)
    {
	strcpy (buf, series.str [i]);
	if ( (cp = strchr (buf, ';')) && cp > buf && *(cp - 1) != '\\' )
	{
	    *cp = '\0';
	    while (isspace (*cp) && *cp)
		cp++;
	    free (series.str [i]);
	    series.str [i] = xstrdup (cp + 1);
	    done = 1;
	}
	dest->str = xrealloc (dest->str, (dest->nstr + 1) * sizeof (char *));
	dest->str [(dest->nstr)++] = xstrdup (buf);
	if (done)
	    return (i);
    }
    
    return (i + 1);
}
