#include "kiss.h"

int dorm (Stringstack s)
{
    register int
	ret = 0,
	i,
	opt;
    RmFlags
	fl = { 0, 0, 0, 0 };

    while ( (opt = getopt (s.nstr, s.str, "virfh")) != -1 )
	switch (opt)
	{
	    case 'v':
		fl.verbose = 1;
		break;
	    case 'i':
		fl.interactive = 1;
		break;
	    case 'r':
		fl.recursive = 1;
		break;
	    case 'f':
		fl.forced = 1;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-virf] file(s)\n"
		       "       %s -h\n"
		       "Where:\n"
		       "    -h: this text\n"
		       "    -v: verbose, show what's happening\n"
		       "    -i: interactive, ask confirmation\n"
		       "    -r: recursive: remove subdirs and contents\n"
		       "    -f: forced: override read-only permissions\n"
		       , progname, progname);
	}

    if (s.nstr - optind < 1)
	error ("need at least one argument");

    for (i = optind; i < s.nstr; i++)
	ret += removefile (s.str [i], fl);

    return (ret);
}
