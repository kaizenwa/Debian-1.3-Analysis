#include "kiss.h"

int dosource (Stringstack s)
{
    if (s.nstr != 2 || getopt (s.nstr, s.str, "h") != -1)
	return (warning ("Bad comandline.\n"
			 "Usage: source file\n"));

    sourcefile (s.str [1]);
    return (0);
}
			 
