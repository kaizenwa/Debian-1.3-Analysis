#include "kiss.h"

int removefile (char *name, RmFlags fl)
{
    struct stat
	statbuf;
    register int
	namelen;

    if (stat (name, &statbuf) && lstat (name, &statbuf))
	return (warning ("cannot stat \"%s\"", name));

    namelen = strlen (name);
    if (! strcmp (name, ".") ||
	! strcmp (name, "..") ||
	(namelen >= 2 && ! strcmp (name + namelen - 2, "/.")) ||
	(namelen >= 3 && ! strcmp (name + namelen - 3, "/.."))
       )
	return (warning ("won't remove \".\" or \"..\" directories"));
		
    if (fl.interactive)
    {
	printf ("%s: remove \"%s\" [y/?] ", progname, name);
	if (getinput (stdin) != 'y')
	    return (0);
    }
    else if (fl.verbose)
	printf ("%s\n", name);

    if (S_ISDIR (statbuf.st_mode))
    {
	if (fl.recursive)
	    return (recursiveremove (name, fl));
	else
	    return (warning ("\%s\" is a directory", name));
    }

    if (! (statbuf.st_mode & S_IWUSR))
    {
	if (! fl.forced)
	    return (warning ("\"%s\" is non-writable", name));
	if (chmod (name, statbuf.st_mode | S_IWUSR))
	    return (warning ("failure removing read-only mode on \"%s\"",
			     name));
    }

    if (unlink (name))
	return (warning ("failure removing \"%s\"", name));

    return (0);
}
    
