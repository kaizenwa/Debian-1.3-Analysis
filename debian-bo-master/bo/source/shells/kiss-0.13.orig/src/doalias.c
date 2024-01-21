#include "kiss.h"

static void dumpalias (int i)
{
    register int
	j;

    printf ("%s -> ", alias [i].str [0]);
    for (j = 1; j < alias [i].nstr; j++)
	printf ("%s ", alias [i].str [j]);
    putchar ('\n');
}

static void listaliases ()
{
    register int
	i;

    for (i = 0; i < nalias; i++)
	dumpalias (i);
}

static void listalias (char *name)
{
    register int
	i;

    if ( (i = isalias (name)) != -1 )
	dumpalias (i);
}

int doalias (Stringstack s)
{
    register int
	i;

    if (s.nstr == 1)
    {
	listaliases ();
	return (0);
    }

    if (s.nstr == 2)
    {
	listalias (s.str [1]);
	return (0);
    }

    if (! strcmp (s.str [1], "-h"))
	return (warning ("Bad commandline.\n"
			 "Usage: alias           : list all aliases\n"
			 "       alias cmd       : list alias \"cmd\"\n"
			 "       alias cmd redef : create alias\n"
			 "Use $1, $2 etc. in the \"redef\" to access "
				    "arguments.\n"));

    if ( (i = isalias (s.str [1])) != -1 )
    {
	clearstack (&alias [i]);
	alias [i] = copystringstack (s, 1, s.nstr - 1);
	return (0);
    }

    alias = xrealloc (alias, (nalias + 1) * sizeof (Stringstack));
    alias [nalias++] = copystringstack (s, 1, s.nstr - 1);

    return (0);
}
