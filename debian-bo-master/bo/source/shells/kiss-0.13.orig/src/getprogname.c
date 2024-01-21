#include "kiss.h"

void getprogname (char *av0)
{
    register char
	*cp;

    cp = strrchr (av0, '/');
    if (cp && *(cp + 1))
	progname = cp + 1;
    else
	progname = av0;
}
	
