#include "kiss.h"

int domount (Stringstack s)
{
    register int
	opt;
    register char
	*type = NULL;

    while ( (opt = getopt (s.nstr, s.str, "t:h")) != -1 )
	switch (opt)
	{
	    case 't':
		if (! (type = optarg))
		    error ("missing type after \"-t\"");
		break;
	    default:
	    case 'h':
		error ("Bad commandline.\n"
		       "Usage: %s\n"
		       "   or: %s [-t type] [-r] device directory\n"
		       "Where:\n"
		       "    -t type   : mount as type, e.g. ext2, minix\n"
		       "    device    : device to mount\n"
		       "    directory : mount point\n"
		       , progname, progname);
	}

    if (s.nstr - optind == 0)			/* no more arguments */
    {
	char
	    buf [BUFSIZ];
	FILE
	    *mtab = fopen (MTAB, "r");

	if (! mtab)
	    return (warning ("can't open \"%s\" for reading", MTAB));

	while (fgets (buf, sizeof (buf), mtab))
	    fputs (buf, stdout);
	fclose (mtab);
	return (0);
    }
    else if (s.nstr - optind == 2)		/* 2 args: device point */
    {
	FILE
	    *mtab;

	if (! type)
	    error ("need \"-t type\" specification");

	if (mount (s.str [optind], s.str [optind + 1], type, 1, 0))
	    error ("problem mounting \"%s\" on \"%s\" (type \"%s\")",
		   s.str [optind + 1], s.str [optind], type);

	if (! (mtab = fopen (MTAB, "a")) )
	    return (warning ("\"%s\" not updated", MTAB));
	fprintf(mtab, "%s %s %s rw 1 0\n",
		s.str [optind], s.str [optind + 1], type);
	
	fclose(mtab);
    }
    else					/* other argument count */
	error ("bad argument count");

    return (0);
}
