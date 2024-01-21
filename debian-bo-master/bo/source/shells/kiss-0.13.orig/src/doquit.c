#include "kiss.h"

int doquit (Stringstack s)
{
    if (s.nstr > 2)
	return (warning ("Bad commandline.\n"
			 "Usage: exit [exitstatus]\n"
			 "Where:\n"
			 "    exitstatus: (optional) return value; "
				    "default: exit with result\n"
			 "    of last child process\n"));

    if (s.nstr > 1)
	exit (atoi (s.str [1]));
    else
    {
	exit (laststatus);
	return (0);	/* to satisfy return value */
    }
}
