#include "kiss.h"

static int listdirfiles (char *path, LsFlags fl)
{
    glob_t
	globres;
    register int
	i;
    
    if (! glob (path, 0, NULL, &globres))
	for (i = 0; i < globres.gl_pathc; i++)
	    if (listfile (globres.gl_pathv [i], fl))
	    {
		globfree (&globres);
		return (1);
	    }

    globfree (&globres);
    return (0);
}
		
int listdir (char *dir, LsFlags fl)
{
    struct stat
	statbuf;
    char
	path [FILENAMELEN];
    register int
	ret = 0;
    
    if (stat (dir, &statbuf) || ! S_ISDIR (statbuf.st_mode))
	return (warning ("\"%s\" is not an accessible directory", dir));

    if (strcmp (dir, "."))
    {
	if (fl.showall)
	{
	    strcpy (path, dir);
	    strcat (path, "/.*");
	    ret += listdirfiles (path, fl);
	}
	strcpy (path, dir);
	strcat (path, "/*");
	ret += listdirfiles (path, fl);
    }
    else
    {
	if (fl.showall)
	    ret += listdirfiles (".*", fl);
	ret += listdirfiles ("*", fl);
    }

    return (ret);
}
