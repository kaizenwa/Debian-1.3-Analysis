#include "kiss.h"

int doexec (Stringstack s)
{
    register int
	i;

    /* need at least one arg */
    if (s.nstr == 1)
	return (warning ("exec requires program to execute"));

    for (i = 0; i < s.nstr - 1; i++)
	s.str [i] = s.str [i + 1];
    s.str [s.nstr - 1] = NULL;

    execvp (s.str [0], s.str);

    return (warning ("%s: command not found", s.str [0]));
}
