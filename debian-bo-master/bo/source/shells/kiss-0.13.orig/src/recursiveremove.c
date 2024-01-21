#include "kiss.h"

int recursiveremove (char *dir, RmFlags fl)
{
    glob_t
	globres;
    register int
	i,
	res = 0;
    struct stat
	statbuf;
    char
	name [FILENAMELEN];

    /* first kill files starting with a dot */
    strcpy (name, dir);
    if (name [strlen (name) - 1] != '/')
	strcat (name, "/");
    strcat (name, ".*");
    if (! glob (name, 0, NULL, &globres))
	/* note: start at index 2, first are . and .. */
	for (i = 2; i < globres.gl_pathc; i++)
	    res += removefile (globres.gl_pathv [i], fl);
    globfree (&globres);

    /* now kill all other files */
    strcpy (name, dir);
    if (name [strlen (name) - 1] != '/')
	strcat (name, "/");
    strcat (name, "*");
    if (! glob (name, 0, NULL, &globres))
	for (i = 0; i < globres.gl_pathc; i++)
	    res += removefile (globres.gl_pathv [i], fl);

    /* is dir itself removable? */
    if (stat (dir, &statbuf))
	return (warning ("suddenly can't stat \"%\" no more", dir));

    if (! (statbuf.st_mode & S_IWUSR))
    {
	if (! fl.forced)
	    return (warning ("dir \"%s\" is non-writeable", dir));
	if (chmod (dir, statbuf.st_mode | S_IWUSR))
	    return (warning ("failure removing read-only on dir \"%s\"", dir));
    }

    if (rmdir (dir))
	return (warning ("failure removing dir \"%s\"", dir));

    return (res);
}
