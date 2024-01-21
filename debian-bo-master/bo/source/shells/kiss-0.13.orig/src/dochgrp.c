#include "kiss.h"

int dochgrp (Stringstack s)
{
    struct group
	*gp;
    register int
	ret = 0,
	i,
	gid;
    struct stat
	statbuf;
    
    if (getopt (s.nstr, s.str, "h") != -1 || s.nstr < 3)
	error ("Bad commandline.\n"
	       "Usage: %s group-id file(s)\n"\
	       "Where:\n"
	       "    group-id: numerical ID or group name\n"
	       "    file(s): files to change group\n"
	       , progname);

    if (! isdigit (s.str [1][1]))
    {
	if (! (gp = getgrnam (s.str [1])) )
	    error ("no group with name \"%s\" in database", s.str [1]);
	gid = gp->gr_gid;
    }
    else
    {
	gid = atoi (s.str [1]);
	if (! (gp = getgrgid (gid)) )
	    error ("no group with number \"%d\" in database", gid);
    }

    for (i = 2; i < s.nstr; i++)
    {
	if (stat (s.str [i], &statbuf))
	    ret += warning ("cannot stat \"%s\"", s.str [i]);
	else if (chown (s.str [i], statbuf.st_uid, gid))
	    ret += warning ("problem changing group id to %d for file \"%s\"",
			    gid, s.str [i]);
    }

    return (ret);
}
