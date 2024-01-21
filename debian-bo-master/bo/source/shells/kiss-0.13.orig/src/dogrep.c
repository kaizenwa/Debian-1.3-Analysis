#include "kiss.h"

static int match (FILE *inf, regex_t *regp, GrepFlags fl, char *filename)
{
    char
	buf [FILENAMELEN];
    int
	nmatches = 0,
	found;

    while (1)
    {
	if (! (fgets (buf, FILENAMELEN - 1, inf)) )
	    break;
	found = ! regexec (regp, buf, 0, NULL, 0);
	if ( (found && !fl.reverse) || (!found && fl.reverse) )
	{
	    nmatches++;
	    if (filename)
		printf ("%s: ", filename);
	    printf ("%s", buf);
	}
    }
    return (nmatches);
}

int dogrep (Stringstack s)
{
    GrepFlags
	fl = { 0, 0 };
    register int
	i,
	ret = 0,
	nout = 0,
	opt,
	regflags = REG_EXTENDED | REG_NEWLINE | REG_NOSUB;
    regex_t
	regex;
    FILE
	*inf;

    while ( (opt = getopt (s.nstr, s.str, "hiv")) != -1 )
	switch (opt)
	{
	    case 'i':
		fl.ignorecase = 1;
		regflags |= REG_ICASE;
		break;
	    case 'v':
		fl.reverse = 1;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-iv] expression [file(s)]\n"
		       "Where:\n"
		       "    -i: ignore case when matching\n"
		       "    -v: show non-matching lines of information\n"
		       "    file(s): files to scan, stdin when absent\n"
		       , progname);
	}
    if (s.nstr < 2)
	error ("needs an expression to match");

    if (regcomp (&regex, s.str [optind], regflags))
	error ("bad expression \"%s\"", s.str [optind]);

    if (s.nstr > optind + 1)
	for (i = optind + 1; i < s.nstr; i++)
	{
	    if (! (inf = fopen (s.str [i], "r")) )
		ret += warning ("cannot open \"%s\" for reading", s.str [i]);
	    else
	    {
		nout += match (inf, &regex, fl, s.str [i]);
		fclose (inf);
	    }
	}
    else
	nout = match (stdin, &regex, fl, NULL);

    regfree (&regex);

    if (! nout)
	ret++;

    return (ret);
}
    
