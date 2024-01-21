#include "kiss.h"

int domv (Stringstack s)
{
    register int
	opt,
	i,
	ret = 0;
    MvFlags
	fl = { 0, 0, 0 };
    struct stat
	destbuf;

    while ( (opt = getopt (s.nstr, s.str, "ivph")) != -1 )
	switch (opt)
	{
	    case 'i':
		fl.interactive = 1;
		break;
	    case 'v':
		fl.verbose = 1;
		break;
	    case 'p':
		fl.protect = 1;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-ivp] sourcefile destfile\n"
		       "       %s [-ivp] sourcefile(s) destdir\n"
		       "Where:\n"
		       "    -i: interactive mode, ask confirmation before "
				    "overwriting\n"
		       "    -v: verbose mode: show what's happening\n"
		       "    -p: protect existing files, never overwrite them\n"
		       , progname, progname);
	}

    /* need at least 2 args */
    if (s.nstr - optind < 2)
	error ("need at least 2 arguments");

    /* is last entry a dir? */
    if (! stat (s.str [s.nstr - 1], &destbuf) && S_ISDIR (destbuf.st_mode))
	for (i = optind; i < s.nstr - 1; i++)
	    ret += movefiletodir (s.str [i], s.str [s.nstr - 1], fl);
    else if (s.nstr - optind > 2)
	error ("more than 1 file can only be moved to destination directory");
    else
	ret += movefiletofile (s.str [optind], s.str [s.nstr - 1], fl);

    return (ret);
}
