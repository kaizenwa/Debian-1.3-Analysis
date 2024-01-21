#include "kiss.h"

static void echostring (char *s, int escape)
{
    register char
	*cp;

    for (cp = s; *cp; cp++)
	if (! escape || *cp != '\\' || ! *(cp + 1))
	    putchar (*cp);
	else
	{
	    cp++;
	    switch (*cp)
	    {
		case 'a':
		    putchar ('\a');
		    break;
		case 'b':
		    putchar ('\b');
		    break;
		case 'n':
		    putchar ('\n');
		    break;
		case 'r':
		    putchar ('\r');
		    break;
		case 't':
		    putchar ('\t');
		    break;
		case 'v':
		    putchar ('\v');
		    break;
		default:
		    putchar (*cp);
		    break;
	    }
	}
}

int doecho (Stringstack s)
{
    register int
	opt,
	i;
    EchoFlags
	fl = { 0, 0 };

    while ( (opt = getopt (s.nstr, s.str, "neh")) != -1 )
	switch (opt)
	{
	    case 'e':
		fl.escape = 1;
		break;
	    case 'n':
		fl.nonewline = 1;
		break;
	    case 'h':
	    default:
		error ("Bad commandline.\n"
		       "Usage: %s [-ne] strings\n"
		       "Where:\n"
		       "    -n: don't print terminating newline character\n"
		       "    -e: interpret escape sequences "
					    "\\a \\b \\n \\r \\t \\v \n"
		       "    strings: strings to display\n"
		       , progname);
	}

    for (i = optind; i < s.nstr; i++)
    {
	echostring (s.str [i], fl.escape);
	if (i < s.nstr - 1)
	    putchar (' ');
    }
    if (! fl.nonewline)
	putchar ('\n');

    return (0);
}
