#include "kiss.h"

Stringstack expandalias (Stringstack s)
{
    Stringstack
	ret;
    register int
	i,
	j,
	arg,
	appended = 0;
    register char
	*space,
	*cp;
    char
	buf [FILENAMELEN],
	twobuf [2] = { 0, 0 };

    ret.str = NULL;
    ret.nstr = 0;

    if ( (i = isalias (s.str [0])) != -1 )
    {
	/* split off first  ; separated part */
	if (strstr (alias [i].str [1], " ; "))
	{
	    strcpy (buf, alias [i].str [1]);
	    cp = strstr (buf, " ; ");
	    *cp = '\0';
	    if (! (space = strtok (buf, " ")) )
		addstringtostack (&ret, buf);
	    else
	    {
		addstringtostack (&ret, space);
		while ( (space = strtok (NULL, " ")) )
		addstringtostack (&ret, space);
	    }
	    
	    strcpy (bufferedinput, cp + 3);
	}
	else
	    ret = copystringstack (alias [i], 1, 1);

	/* loop thru all arguments */
	for (j = 2; j < alias [i].nstr; j++)  
	{
	    /* copy alias parts to buf, expanding $1 $2 etc. */
	    buf [0] = '\0';
	    for (cp = alias [i].str [j]; *cp; cp++)
		if (*cp == '$')
		{
		    if ( (arg = atoi (cp + 1)) && arg < s.nstr && arg > 0)
			strcat (buf, s.str [arg]);
		    appended++;
		    while (isdigit (* (cp + 1)) )
			cp++;
		}
		else
		{
		    twobuf [0] = *cp;
		    strcat (buf, twobuf);
		}
	    if (buf [0])
		addstringtostack (&ret, buf);
	}

	/* if no $1 etc. expanded: add stuff from the command */
	if (! appended)
	    for (i = 1; i < s.nstr; i++)
		addstringtostack (&ret, s.str [i]);
    }
    else
	ret = copystringstack (s, 0, s.nstr - 1);

    return (ret);
}
    
