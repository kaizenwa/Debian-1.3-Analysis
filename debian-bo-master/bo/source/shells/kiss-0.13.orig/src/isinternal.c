#include "kiss.h"

int isinternal (char *cmd)
{
    register int
	i;

    for (i = 0; cmdtable [i].cmdname; i++)
	if (! strcmp (cmd, cmdtable [i].cmdname))
	    return (i);

    return (-1);
}
