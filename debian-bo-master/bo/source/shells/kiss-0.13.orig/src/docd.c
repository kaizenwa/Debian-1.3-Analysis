#include "kiss.h"

int docd (Stringstack s)
{
    register char
	*cp;

    if (s.nstr > 2)
	return (warning ("Bad commandline.\n"
			 "Usage: cd dir   change to directory \"dir\"\n"
			 "       cd       change to homedirectory \"%s\"\n",
			 homedir));
    if (s.nstr != 2)
	cp = homedir;
    else
	cp = s.str [1];

    if (chdir (cp))
	return (warning ("no such directory \"%s\"", s.str [1]));
    
    return (0);
}
