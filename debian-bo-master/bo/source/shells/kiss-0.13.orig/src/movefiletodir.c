#include "kiss.h"

int movefiletodir (char *src, char *dir, MvFlags fl)
{
    char
	destbuf [FILENAMELEN],
	base [FILENAMELEN];

    strcpy (destbuf, dir);
    if (destbuf [strlen (destbuf) - 1] != '/')
	strcat (destbuf, "/");
    getbasename (src, base);
    strcat (destbuf, base);

    return (movefiletofile (src, destbuf, fl));
}
