#include "kiss.h"

int domkdir (Stringstack s)
{
    register int
	makeparts = 0,
	i,
	opt,
	ret = 0;
    char
	buf [FILENAMELEN];
    struct stat
	statbuf;
    register char
	*cp;

    while ( (opt = getopt (s.nstr, s.str, "ph")) != -1 )
	if (opt == 'p')
	    makeparts++;
	else
	    error ("Bad commandline.\n"
		   "Usage: %s [-p] directory(~ies)\n"
		   "Where:\n"
		   "    -p: make full path, including intermediate dirs\n"
		   "    directories: dirs to create\n", progname);

    for (i = optind; i < s.nstr; i++)
    {
	if (! stat (s.str [i], &statbuf))
	    ret += warning ("\"%s\" already exists", s.str [i]);
	else if (makeparts)
	{
	    strcpy (buf, s.str [i]);
	    if ( (cp = strtok (buf, "/")) )
	    {
		if (stat (cp, &statbuf) && mkdir (cp, CREATEFLAGS))
		{
		    ret += warning ("failure while creating subpath \"%s\"",
				    cp);
		    break;
		}
		chdir (cp);
		while ( (cp = strtok (NULL, "/")) )
		{
		    if (stat (cp, &statbuf) && mkdir (cp, CREATEFLAGS))
		    {
			ret += warning ("failure while creating subpath \"%s\"",
					cp);
			break;
		    }
		    chdir (cp);
		}
	    }
		    
	}
	else
	{
	    if (mkdir (s.str [i], CREATEFLAGS))
		ret += warning ("failure while creating dir \"%s\"",
				s.str [i]);
	}
    }

    return (ret);
}
