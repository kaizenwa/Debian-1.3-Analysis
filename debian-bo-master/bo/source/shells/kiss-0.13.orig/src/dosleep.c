#include "kiss.h"

int dosleep (Stringstack s)
{
    int
	nsec;
    
    if (s.nstr != 2 || getopt (s.nstr, s.str, "h") != -1)
	error ("Bad commandline.\n"
	       "Usage: %s seconds\n"
	       "Where:\n"
	       "    seconds - the number of seconds to sleep\n"
	       , progname);

    if (! (nsec = atoi (s.str [1])) || nsec < 0 )
	error ("bad seconds count \"%s\"", s.str [1]);

    sleep (nsec);

    return (0);
}
