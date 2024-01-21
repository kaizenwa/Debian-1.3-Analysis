#include "kiss.h"

int domknod (Stringstack s)
{
    int
	mode =  S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH,
	major,
	minor;
    
    if ( s.nstr < 3 || getopt (s.nstr, s.str, "h") != -1 )
	error ("Bad commandline.\n"
	       "Usage: %s filename b majorno minorno (create block device)\n"
	       "   or: %s filename c majorno minorno (create character "
							    "device)\n"
	       "   or: %s filename p                 (create pipe)\n"
	       , progname, progname, progname);

    if (s.nstr != 3 && s.nstr != 5)
	error ("need 2 or 4 arguments");

    if (s.nstr == 3 && ! strcmp (s.str [2], "p"))
    {
	if (mkfifo (s.str [1], mode))
	    error ("problem making pipe \"%s\"", s.str [1]);
	return (0);
    }

    if (! strcmp (s.str [2], "c"))
	mode |= S_IFCHR;
    else if (! strcmp (s.str [2], "b"))
	mode |= S_IFBLK;
    else
	error ("bad creation parameter \"%s\", [bc] supported", s.str [2]);

    if (! sscanf (s.str [3], "%d", &major))
	error ("bad major number \"%s\"", s.str [3]);
    if (! sscanf (s.str [4], "%d", &minor))
	error ("bad minor number \"%s\"", s.str [4]);

    if (mknod (s.str [1], mode, major * 256 + minor))
	error ("problem making device \"%s\"", s.str [1]);

    return (0);
}
