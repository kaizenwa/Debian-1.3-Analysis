#include "kiss.h"

int isalias (char *cmd)
{
    register int
	i;

    for (i = 0; i < nalias; i++)
	if (! strcmp (cmd, alias [i].str [0]))
	    return (i);

    return (-1);
}
