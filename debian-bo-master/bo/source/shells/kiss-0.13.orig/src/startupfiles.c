#include "kiss.h"

void startupfiles ()
{
    char
	userrc [FILENAMELEN];
    FILE
	*inf;

    strcpy (userrc, homedir);
    strcat (userrc, "/" USERRC);

    if ( (inf = fopen (userrc, "r")) )
	yypushfile (inf);

    if ( (inf = fopen (SYSTEMRC, "r")) )
	yypushfile (inf);
}
