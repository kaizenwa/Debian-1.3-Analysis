#include "kiss.h"

int expandvars (char *name, char *buf)
{
    char
	varname [FILENAMELEN],
	twobuf [2] = { 0, 0 };
    register char
	*env,
	*cp;
    register int
	varfound = 0;

    cp = name;
    *buf = '\0';
    
    while (*cp)
    {
	if (*cp == '$' && isupper (* (cp + 1)) &&
	    (cp > name || *(cp - 1) != '\\')
	   )
	{
	    cp++;
	    varname [0] = '\0';
	    varfound = 1;
	    while (isupper (*cp))
	    {
		twobuf [0] = *cp;
		strcat (varname, twobuf);
		cp++;
	    }
	    if ( (env = getenv (varname)) )
		strcat (buf, env);
	    cp--;
	}
	else
	{
	    twobuf [0] = *cp;
	    strcat (buf, twobuf);
	}

	if (*cp)
	    cp++;
    }

    return (varfound);
}
