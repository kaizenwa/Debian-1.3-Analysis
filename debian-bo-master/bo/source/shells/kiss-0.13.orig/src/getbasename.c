#include "kiss.h"

void getbasename (char *file, char *base)
{
    register char
	*cp;

    if ( (cp = strrchr (file, '/')) && *(cp + 1) )
	strcpy (base, cp + 1);
    else
	strcpy (base, file);
}
