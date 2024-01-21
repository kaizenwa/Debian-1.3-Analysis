#include "kiss.h"

int docat (Stringstack s)
{
    register int
	showall = 0,
	printonly = 0,
	i,
	ret = 0,
	opt;
    FILE
	*inf;

    while ( (opt = getopt (s.nstr, s.str, "hsp")) != -1 )
	switch (opt)
	{
	    case 'p':
		printonly++;
		break;
	    case 's':
		showall++;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-hps] [file(s)]\n"
		       "Where:\n"
		       "    -h: this message\n"
		       "    -p: show only printable chars\n"
		       "    -s: show filenames while cat-ting\n"
		       "    file(s): files to copy to stdout\n",
		       progname);
	}

    if (optind >= s.nstr)
	catfile (stdin, "stdin", showall, printonly);
    else for (i = optind; i < s.nstr; i++)
    {
	if (! (inf = fopen (s.str [i], "r")) )
	    ret += warning ("cannot open \"%s\"", s.str [i]);
	else
	{
	    catfile (inf, s.str [i], showall, printonly);
	    fclose (inf);
	}
    }

    return (ret);
}
