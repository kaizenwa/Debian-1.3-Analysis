#include "kiss.h"

int doprintenv (Stringstack s)
{
    register int
	i,
	ret = 0;
    register char
	*cp;

    if (getopt (s.nstr, s.str, "h") != -1)
	error ("Bad commandline.\n"
	       "Usage: %s -h          this text\n"
	       "       %s variable(s) show environment setting of variable(s)\n"
	       "       %s             show entire environment\n"
	       , progname, progname, progname);

    if (s.nstr > 1)
	for (i = 1; i < s.nstr; i++)
	    if (! (cp = getenv (s.str [i])) )
	    {
		warning ("no environment variable \"%s\" defined", s.str [i]);
		ret++;
	    }
	    else
		printf ("%s=%s\n", s.str [i], cp);
    else
	for (i = 0; environ [i]; i++)
	    printf ("%s\n", environ [i]);

    return (ret);
}
