#include "kiss.h"

int docp (Stringstack s)
{
    CpFlags
	fl = { 0, 0, 0, 0, 0 };
    register int
	ret = 0,
	i,
	opt;
    struct stat
	statbuf;

    while ( (opt = getopt (s.nstr, s.str, "vrpdh")) != -1 )
	switch (opt)
	{
	    case 'v':
		fl.verbose = 1;
		break;
	    case 'r':
		fl.recursive = 1;
		break;
	    case 'p':
		fl.preserve = 1;
		break;
	    case 'd':
		fl.noderef = 1;
		break;
	    case 'i':
		fl.interactive = 1;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-vpdi]  sourcefile destfile\n"
		       "       %s [-vpdi]  sourcfile(s) destdir\n"
		       "       %s [-vpdri] sourcedir destdir\n"
		       "       %s -h\n"
		       "Where:\n"
		       "    -d: no-dereference: copy symlinks as symlinks\n"
		       "    -h: this message\n"                            
		       "    -i: interactive, ask before overwriting\n"
		       "    -p: preserve file timestamps\n"
		       "    -r: copy recursively sourcedir to destdir\n"
		       "    -v: verbose output, show what's going on\n"
		       , progname, progname, progname, progname);
	}

    /* need at least 2 args */
    if (s.nstr - optind < 2)
	error ("need at least 2 arguments");

    /* for multiple files: destination must be directory */
    if (s.nstr - optind > 2)
    {
	if (fl.recursive)
	    error ("recursive copying only applies from one dir to another");
	if (stat (s.str [s.nstr - 1], &statbuf) ||
	    ! S_ISDIR (statbuf.st_mode)
	   )
	    error ("\"%s\" is not a directory\n"
		   "multiple files must be copied to an existing directory",
		   s.str [s.nstr - 1]);
	for (i = optind; i < s.nstr - 1; i++)
	    ret += copyfiletodir (s.str [i], s.str [s.nstr - 1], fl);
	return (ret);
    }

    /* exactly two files */
    if (! stat (s.str [s.nstr - 1], &statbuf) &&
	S_ISDIR (statbuf.st_mode)
       )
    {
	if (! stat (s.str [optind], &statbuf) &&
	    S_ISDIR (statbuf.st_mode)
	   )
	{
	    if (! fl.recursive)
		error ("dir to dir copying can only be with recursive flag");
	    return (copydirtodir (s.str [optind], s.str [s.nstr - 1], fl, 0));
	}
	return (copyfiletodir (s.str [optind], s.str [s.nstr - 1], fl));
    }
    return (copyfiletofile (s.str [optind], s.str [s.nstr - 1], fl));
    
}
