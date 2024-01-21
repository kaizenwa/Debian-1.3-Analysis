#include "kiss.h"

static int touchfile (char *name)
{
    struct stat
	statbuf;
    register int
	handle;

    if (stat (name, &statbuf))
    {
	if ( (handle = open (name, O_RDWR | O_CREAT, CREATEFLAGS)) < 0)
	    return (warning ("can't create \"%s\"", name));
	close (handle);
    }
    else
    {
	if (utime (name, NULL))
	    return (warning ("problem setting time for \"%s\"", name));
    }

    return (0);
}


int dotouch (Stringstack s)
{
    register int
	i,
	ret = 0;
    
    if (s.nstr == 1 || getopt (s.nstr, s.str, "h") != -1)
	error ("Bad commandline.\n"
	       "Usage: %s file(s)\n"
	       , progname);

    for (i = 0; i < s.nstr; i++)
	ret += touchfile (s.str [i]);

    return (ret);
}
