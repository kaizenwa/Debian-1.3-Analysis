#include "kiss.h"

void sourcefile (char *name)
{
    FILE
	*inf;

    if (! (inf = fopen (name, "r")) )
    {
	warning ("cannot open \"%s\" for reading", name);
	return;
    }

    yypushfile (inf);
}
