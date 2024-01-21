#include "kiss.h"

static int findexecutable (char *dir, Stringstack what)
{
    register int
	i,
	hits = 0;
    char
	buf [FILENAMELEN];
    struct stat
	statbuf;

    for (i = 1; i < what.nstr; i++)
    {
	strcpy (buf, dir);
	if (buf [strlen (buf) - 1] != '/')
	    strcat (buf, "/");
	strcat (buf, what.str [i]);

	if (! stat (buf, &statbuf) && (statbuf.st_mode & S_IXUSR))
	{
	    printf ("%s\n", buf);
	    hits++;
	}
    }

    return (hits);
}
    

int dowhere (Stringstack s)
{
    char
	buf [FILENAMELEN];
    register char
	*cp;
    register int
	aliasnr,
	j,
	i,
	res = 0;

    if (getopt (s.nstr, s.str, "h") != -1 || s.nstr == 1)
	error ("Bad commandline.\n"
	       "Usage: %s [-h] program(s)\n"
	       "Where:\n"
	       "    -h: this text\n"
	       "    program(s): programs to locate on PATH or as built-in\n"
	       , progname, progname);

    for (i = 1; i < s.nstr; i++)
    {
	if ( (aliasnr = isalias (s.str [i])) != -1 )
	{
	    printf ("%s is aliased to ", s.str [i]);
	    for (j = 1; j < alias [aliasnr].nstr; j++)
		printf ("%s ", alias [aliasnr].str [j]);
	    putchar ('\n');
	    res++;
	}
	if (isinternal (s.str [i]) != -1)
	{
	    printf ("%s is a built-in command\n", s.str [i]);
	    res++;
	}
    }

    if (! (cp = getenv ("PATH")) )
	return (warning ("cannot determine PATH setting"));

    strcpy (buf, cp);

    if ( (cp = strtok (buf, ":")) )
    {
	res += findexecutable (cp, s);
	while ( (cp = strtok (NULL, ":")) )
	    res += findexecutable (cp, s);
    }

    return (res == 0);
}
