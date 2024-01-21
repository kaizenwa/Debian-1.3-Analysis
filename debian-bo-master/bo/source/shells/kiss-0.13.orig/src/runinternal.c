#include "kiss.h"

int runinternal (Stringstack s)
{
    register int
	i;
    Stringstack
	tmp = { NULL, 0 };
    register char
	*cp;

    /* is command flagged as immediate? */
    if ( ( (i = isinternal (s.str [0])) != -1 ) &&
	 (cmdtable [i].firstlevel)
       )
    {
	laststatus = cmdtable [i].cmd (s);
	return (1);
    }

    /* "VAR = value" is a special case: remap to "setenv VAR value" */
    if ( (s.nstr == 3 || s.nstr == 2) && !strcmp (s.str [1], "=") )
    {
	addstringtostack (&tmp, "setenv");
	addstringtostack (&tmp, s.str [0]);
	
	if (s.nstr == 3)
	{
	    addstringtostack (&tmp, s.str [2]);
	    laststatus = dosetenv (tmp);
	}
	else
	    laststatus = dounsetenv (tmp);
	
 	clearstack (&tmp); 
	return (1);
    }

    /* "VAR=value" is also a special case, see above */
    if ( s.nstr == 1 && (cp = strchr (s.str [0], '=')) )
    {
	addstringtostack (&tmp, "setenv");
	*cp = '\0';
	addstringtostack (&tmp, s.str [0]);

	cp++;
	if (*cp)
	{
	    addstringtostack (&tmp, cp);
	    laststatus = dosetenv (tmp);
	}
	else
	    laststatus = dounsetenv (tmp);

	clearstack (&tmp);
	return (1);
    }

    /* fall thru: not an immediate command */
    return (0);
}
    
