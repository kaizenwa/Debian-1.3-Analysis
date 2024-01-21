#include "kiss.h"

static void wordcount (FILE *f, char *name, int *tlines, int *twords,
		       int *tchars)
{
    register int
	lastchar = -1,
	ch,
	lines = 0,
	chars = 0,
	words = 0;
    
    while (1)
    {
	ch = fgetc (f);
	if (feof (f))
	    break;

	chars++;
	if (ch == '\n')
	    lines++;

	if (lastchar != -1)
	    if (isalnum (ch) && ! isalnum (lastchar))
		words++;

	lastchar = ch;
    }

    printf ("%10d %10d %10d %s\n",
	    lines, words, chars,
	    name ? name : "");

    *tlines += lines;
    *twords += words;
    *tchars += chars;
}

int dowc (Stringstack s)
{
    register int
	ret = 0,
	i;
    int
	totlines = 0,
	totwords = 0,
	totchars = 0;
    FILE
	*f;
    
    if (getopt (s.nstr, s.str, "h") != -1)
	error ("Bad commandline.\n"
	       "Usage: %s [file(s)]\n", progname);

    if (s.nstr == 1)
	wordcount (stdin, NULL, &totlines, &totwords, &totchars);

    for (i = 1; i < s.nstr; i++)
    {
	if (! (f = fopen (s.str [i], "r")) )
	    ret += warning ("cannot open \"%s\" for reading");
	else
	    wordcount (f, s.str [i], &totlines, &totwords, &totchars);
    }

    if (s.nstr > 2)
	printf ("%10d %10d %10d total\n",
		totlines, totwords, totchars);
    
    return (ret);
}
