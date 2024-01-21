#include "kiss.h"

int copyfiletodir (char *file, char *dir, CpFlags fl)
{
    char
	destname [FILENAMELEN],
	base [FILENAMELEN];

    /* make destfilename */
    strcpy (destname, dir);
    if (destname [strlen (destname) - 1] != '/')
	strcat (destname, "/");
    getbasename (file, base);
    strcat (destname, base);

    /* copy it */
    return (copyfiletofile (file, destname, fl));
}
    
