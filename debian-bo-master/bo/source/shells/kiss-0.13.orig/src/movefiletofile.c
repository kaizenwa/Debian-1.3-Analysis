#include "kiss.h"

int movefiletofile (char *src, char *dest, MvFlags fl)
{
    struct stat
	srcbuf,
	destbuf;
    
    if (fl.verbose)
	printf ("%s -> %s\n", src, dest);

    if (stat (src, &srcbuf))
	return (warning ("cannot stat \"%s\"", src));

    if (! stat (dest, &destbuf))
    {
	if (srcbuf.st_ino == destbuf.st_ino)
	    return (warning ("\"%s\" and \"%s\" are identical files",
			     src, dest));
	if (fl.interactive)
	{
	    printf ("%s: move \"%s\" to \"%s\" [y/?] ", progname, src, dest);
	    if (getinput (stdin) != 'y')
		return (0);
	}
	else if (fl.protect)
	{
	    warning ("not overwriting \"%s\"", dest);
	    return (0);
	}

	if (unlink (dest))
	    return (warning ("cannot remove dest \"%s\"", dest));
    }

    if (rename (src, dest))
	return (warning ("problems renaming \"%s\" to \"%s\"", src, dest));

    return (0);
}
		
	
