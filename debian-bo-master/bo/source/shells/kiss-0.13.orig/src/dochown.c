#include "kiss.h"

int dochown (Stringstack s)
{
    struct passwd
	*pwd;
    register int
	ret = 0,
	i,
	uid;
    struct stat
	statbuf;
    
    if (getopt (s.nstr, s.str, "h") != -1 || s.nstr < 3)
	error ("Bad commandline.\n"
	       "Usage: %s user-id file(s)\n"\
	       "Where:\n"
	       "    user-id: numerical ID or username\n"
	       "    file(s): files to change user ownership\n"
	       , progname);

    if (! isdigit (s.str [1][1]))
    {
	if (! (pwd = getpwnam (s.str [1])) )
	    error ("no user with name \"%s\" in database", s.str [1]);
	uid = pwd->pw_uid;
    }
    else
    {
	uid = atoi (s.str [1]);
	if (! (pwd = getpwuid (uid)) )
	    error ("no user with number \"%d\" in database", uid);
    }

    for (i = 2; i < s.nstr; i++)
    {
	if (stat (s.str [i], &statbuf))
	    ret += warning ("cannot stat \"%s\"", s.str [i]);
	else if (chown (s.str [i], uid, statbuf.st_gid))
	    ret += warning ("problem changing user id to %d for file \"%s\"",
			    uid, s.str [i]);
    }

    return (ret);
}
