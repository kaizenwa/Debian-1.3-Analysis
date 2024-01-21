#include "kiss.h"

int doln (Stringstack s)
{
    register int
	sym = 0,
	forced = 0,
	opt;
    struct stat
	statbuf;
    register char
	*src,
	*dest;
    
    while ( (opt = getopt (s.nstr, s.str, "sfh")) != -1 )
	if (opt == 's')
	    sym++;
	else if (opt == 'f')
	    forced++;
	else
	    error ("Bad commandline.\n"
		   "Usage: %s [-sf] srcfile linkname\n"
		   "Where:\n"
		   "    -s: make symbolic link (default: make hard link)\n"
		   "    -f: force symlink even if srcfile doesn't exist\n"
		   "    srcfile: file to make link to\n"
		   "    linkname: link to create, may not exist (yet)\n"
		   , progname);

    if (s.nstr - optind != 2)
	error ("need two arguments");

    src = s.str [optind];
    dest = s.str [s.nstr - 1];

    if (stat (src, &statbuf))
	if (! forced || ! sym)
	    error ("can't stat \"%s\"", src);
    if (! stat (dest, &statbuf))
	error ("\"%s\" already exists, can't create that link",
	       dest);

    if (sym)
    {
	if (symlink (src, dest))
	    error ("can't create symlink from \"%s\" to \"%s\"",  src, dest);
    }
    else
    {
	if (link (src, dest))
	    error ("can't create hard link from \"%s\" to \"%s\"", src, dest);
    }
    
    return (0);
}
