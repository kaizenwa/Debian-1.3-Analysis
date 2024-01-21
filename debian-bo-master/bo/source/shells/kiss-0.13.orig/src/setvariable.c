#include "kiss.h"

Stringstack setvariable (char *varname)
{
    char
	buf [LINELEN];
    register char
	*cp;

    /* check for the pseudo-variables $$, $? and $! */
    if (! strcmp (varname, "$$"))
	sprintf (buf, "%d", getpid ());
    else if (! strcmp (varname, "$?"))
	sprintf (buf, "%d", laststatus);
    else if (! strcmp (varname, "$!"))
	sprintf (buf, "%d", lastchildpid);
    else
    {
	/* try to get the variable */
	if ( (cp = getenv (varname + 1)) )
	    return (setstring (cp));
	else
	    buf [0] = '\0';
    }

    return (setstring (buf));
}
