#include "kiss.h"

int dormdir (Stringstack s)
{
    register int
	ret = 0,
	i;
    struct stat
	statbuf;

    if (getopt (s.nstr, s.str, "h") != -1 || s.nstr < 2)
	error ("Bad commandline.\n"
	       "Usage: %s [-h] directory(~ies)\n"
	       "Where:\n"
	       "    -h: this text\n"
	       "    directories: directories to remove, which must be empty\n"
	       , progname);

    for (i = 1; i < s.nstr; i++)
    {
	if (stat (s.str [i], &statbuf))
	    ret += warning ("cannot stat \"%s\"", s.str [i]);
	else if (! S_ISDIR (statbuf.st_mode))
	    ret += warning ("\"%s\" is no directory", s.str [i]);
	else if (rmdir (s.str [i]))
	    ret += warning ("problem while removing \"%s\"", s.str [i]);
    }

    return (ret);
}
