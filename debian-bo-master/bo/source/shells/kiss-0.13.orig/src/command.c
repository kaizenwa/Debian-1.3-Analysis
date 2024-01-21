#include "kiss.h"

void command (int index)
{
    FILE
	*f;
    register int
	i;

    if (! (f = popen (orgargv [0], "w")) )
	error ("could not restart \"%s\"", orgargv [0]);

    for (i = index; i < orgargc; i++)
	fprintf (f, "%s ", orgargv [i]);
    fputc ('\n', f);

    laststatus = pclose (f);
}
