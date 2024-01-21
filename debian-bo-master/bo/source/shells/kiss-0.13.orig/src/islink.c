#include "kiss.h"

int islink (char *fname, char *whereto)
{
    register int
	res;

    if ( (res = readlink (fname, whereto, FILENAMELEN)) > 0 )
    {
	whereto [res] = '\0';
	return (1);
    }
    return (0);
}
