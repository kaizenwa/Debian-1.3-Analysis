#include "kiss.h"

void addtoenv (char *var, char *setting)
{
    char
	buf [FILENAMELEN];

    if (! setting)
	putenv (var);
    else
    {
	strcpy (buf, var);
	strcat (buf, "=");
	strcat (buf, setting);
	putenv (xstrdup (buf));
    }
}
