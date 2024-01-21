#include "kiss.h"

int dosetenv (Stringstack s)
{
    register int
	i;

    if (getopt (s.nstr, s.str, "h") != -1)
    {
	warning ("Bad commandline.\n"
		 "Usage: setenv -h         this text\n"
		 "       setenv VAR value  set enviroment variable to value\n"
		 "       VAR=value         set environment "
				    "variable to value\n");
	return (1);
    }

    if (s.nstr != 3)
	return (warning ("command needs VARIABLE and value\n"));

    for (i = 0; i < strlen (s.str [1]); i++)
	if (! isupper (s.str [1][i]))
	    return (warning ("variable \"%s\" is not all upper case\n",
			     s.str [1]));

    addtoenv (s.str [1], s.str [2]);

    return (0);
}
