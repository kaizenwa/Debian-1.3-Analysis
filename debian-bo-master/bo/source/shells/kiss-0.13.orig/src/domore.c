#include "kiss.h"

int domore (Stringstack s)
{
    register int
	opt,
	ret = 0,
	morefiles,
	i;
    FILE
	*altstdin,
	*f;

    while ( (opt = getopt (s.nstr, s.str, "h")) != -1 )
	switch (opt)
	{
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s file(s)\n",
		       progname);
	}
    
    if (! isatty (fileno (stdout)))
	return (docat (s));

    if (s.nstr == 1)
    {
	if (! (altstdin = fopen (DEVTTY, "r")) )
	    error ("cannot open \"%s\" as alternate stdin", DEVTTY);
	morefile (stdin, "stdin", 0, altstdin);
    }
    else
    {
	for (i = 1; i < s.nstr; i++)
	{
	    if (! (f = fopen (s.str [i], "r")) )
		ret += warning ("cannot open \"%s\"", s.str [i]);
	    else
	    {
		morefiles = morefile (f, s.str [i], s.nstr > 2, stdin);
		fclose (f);
		if (! morefiles)
		    break;
	    }
	}
    }
    
    return (ret);
}
    

	
