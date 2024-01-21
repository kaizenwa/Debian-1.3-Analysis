#include "kiss.h"

static void addoutput (Stringstack *what, char *cmd)
{
    FILE
	*f;
    register char
	*info = 0,
	*cp;
    char
	buf [FILENAMELEN];

    if (! (f = popen (cmd, "r")) )
    {
	warning ("cannot start \"%s\" for backquote expansion", cmd);
	return;
    }
    while (fgets (buf, FILENAMELEN - 1, f))
    {
	if (! info)
	    info = xstrdup (buf);
	else
	{
	    info = xrealloc (info, strlen (info) + 4 + strlen (buf));
	    strcat (info, " ");
	    strcat (info, buf);
	}
    }
    if ( (laststatus = pclose (f)) && ! flags.supressstat)
	printf ("[%s: exited with %d]\n", cmd, laststatus);

    if (! info || ! *info)
	return;

    if (! (cp = strtok (info, " \n\t")) )
	addstringtostack (what, info);
    else
    {
	addstringtostack (what, cp);
	while ( (cp = strtok (NULL, " \n\t")) )
	    addstringtostack (what, cp);
    }

    free (info);
}

Stringstack expandbackquotes (Stringstack s)
{
    Stringstack
	ret = { NULL, 0 };
    register int
	len,
	i;
    char
	cmd [FILENAMELEN];

    for (i = 0; i < s.nstr; i++)
    {
	if (*s.str [i] != '`')
	{
	    addstringtostack (&ret, s.str [i]);
	    continue;
	}
	strcpy (cmd, s.str [i] + 1);
	len = strlen (cmd);
	if (len < 2 || cmd [len - 1] != '`')
	{
	    warning ("bad backquote syntax in \"%s\"", s.str [i]);
	    continue;
	}
	cmd [len - 1] = '\0';
	addoutput (&ret, cmd);
    }

    return (ret);
}
	
