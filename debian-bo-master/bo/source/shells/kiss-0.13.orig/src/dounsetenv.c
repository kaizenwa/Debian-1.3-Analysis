#include "kiss.h"

int dounsetenv (Stringstack s)
{
    if (getopt (s.nstr, s.str, "h") != -1 || s.nstr != 2)
    {
	warning ("Bad commandline.\n"
		 "Usage: unsetenv variable\n");
	return (1);
    }

    addtoenv (s.str [1], NULL);

    return (0);
}
