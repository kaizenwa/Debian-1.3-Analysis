#include "kiss.h"

int dols (Stringstack s)
{
    register int
	i,
	opt;
    LsFlags
	fl = { 0, 0, 0, 0, 0 };
    register int
	ret = 0;
    struct stat
	statbuf;

    /* column-wise is default for display, otherwise 1 per line is def */
    if (isatty (STDOUT_FILENO))
	fl.column = 1;
    else
	fl.oneperline = 1;

    while ( (opt = getopt (s.nstr, s.str, "hl1CFa")) != -1 )
	switch (opt)
	{
	    case 'l':
		fl.longoutput = 1;
		/* fall thru to one per line output */
	    case '1':
		fl.oneperline = 1;
		fl.column = 0;
		break;
	    case 'C':
		fl.column = 1;
		fl.longoutput = 0;
		fl.oneperline = 0;
		break;
	    case 'F':
		fl.listtype = 1;
		break;
	    case 'a':
		fl.showall = 1;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-1aCFlh] [files or directories]\n"
		       "Where:\n"
		       "    -1   one entry per line\n"
		       "    -a   show all, includes .*\n"
		       "    -C   column-wise listing\n"
		       "    -F   append type-character: / for dir,"
					    " = for pipe, @ for link\n"
		       "    -l   long listing format\n"
		       , progname);
	}

    if (s.nstr == optind)
    {
	ret += listdir (".", fl);
	listoutputflush ();
	return (ret);
    }

    /* list directories first */
    for (i = optind; i < s.nstr; i++)
    {
	if (stat (s.str [i], &statbuf))
	    ret += warning ("can't stat \"%s\"", s.str [i]);
	else if (S_ISDIR (statbuf.st_mode))
	{
	    if (s.nstr - optind > 1)
		printf ("%s:\n", s.str [i]);
	    ret += listdir (s.str [i], fl);
	    listoutputflush ();
	}
    }
    
    /* list ordinary files next */
    for (i = optind; i < s.nstr; i++)
	if (! stat (s.str [i], &statbuf) && ! S_ISDIR (statbuf.st_mode))
	    ret += listfile (s.str [i], fl);

    listoutputflush ();
    
    return (ret);
}
