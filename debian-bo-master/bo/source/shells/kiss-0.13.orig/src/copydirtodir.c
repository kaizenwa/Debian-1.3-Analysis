#include "kiss.h"

int copydirtodir (char *srcdir, char *destdir, CpFlags fl, int makeit)
{
    register int
	res = 0;
    glob_t
	globvec;
    register int
	i;
    register char
	*entry,
	*curdir = getcwd (NULL, 0),
	*ddir;
    struct stat
	statbuf;
    char
	fulldest [FILENAMELEN];

    /* expand destdir to full path */
    if (makeit)
    {
	strcpy (fulldest, destdir);
	if (fulldest [strlen (fulldest) - 1] != '/')
	    strcat (fulldest, "/");
	strcat (fulldest, srcdir);

	if (stat (fulldest, &statbuf))
	{
	    if (mkdir (fulldest, CREATEFLAGS))
		return (warning ("failure creating dir \"%s\"", fulldest));
	}
	if (chdir (fulldest))
	    return (warning ("cannot access dest dir \"%s\"", fulldest));
    }
    else if (chdir (destdir))
	return (warning ("cannot access dest dir \"%s\"", destdir));

    ddir = getcwd (NULL, 0);

    chdir (curdir);
    if (chdir (srcdir))
	return (warning ("cannot access source dir \"%s\"", srcdir));
	
    if (! glob (".*", 0, NULL, &globvec))
    {
	for (i = 0; i < globvec.gl_pathc; i++)
	{
	    entry = globvec.gl_pathv [i];
	    if (strcmp (entry, ".") && strcmp (entry, ".."))
	    {
		if (! stat (entry, &statbuf) && S_ISDIR (statbuf.st_mode))
		    res += copydirtodir (entry, ddir, fl, 1);
		else
		    res += copyfiletodir (entry, ddir, fl);
	    }
	}
    }
    globfree (&globvec);

    if (! glob ("*", 0, NULL, &globvec))
    {
	for (i = 0; i < globvec.gl_pathc; i++)
	{
	    entry = globvec.gl_pathv [i];
	    if (! stat (entry, &statbuf) && S_ISDIR (statbuf.st_mode))
		res += copydirtodir (entry, ddir, fl, 1);
	    else
		res += copyfiletodir (entry, ddir, fl);
	}
    }
    globfree (&globvec);

    chdir (curdir);
    free (curdir);

    return (res);
}
