#include "kiss.h"

int
    nentries = 0;
char
    **entry = NULL;

void listoutput (char *buf)
{
    entry = xrealloc (entry, (nentries + 1) * sizeof (char *));
    entry [nentries++] = xstrdup (buf);
}

void listoutputflush ()
{
    /* we can assume that we're in multicolumn mode now */
    register int
	maxlen = 0,
	nperline,
	printed = 0,
	j,
	i;

    /* nothing to do? nogo. */
    if (! nentries)
	return;

    /* find longest entry */
    for (i = 0; i < nentries; i++)
    {
	register int
	    len = strlen (entry [i]);
	if (len > maxlen)
	    maxlen = len;
    }

    /* allow for extra space */
    maxlen++;

    nperline = 80 / maxlen;
    if (! nperline)
	nperline++;

    for (i = 0; i < nentries; i++)
    {
	printf ("%s", entry [i]);
	for (j = strlen (entry [i]); j < maxlen; j++)
	    putchar (' ');
	printed++;

	if (printed == nperline)
	{
	    putchar ('\n');
	    printed = 0;
	}
    }

    if (printed)
	putchar ('\n');

    for (i = 0; i < nentries; i++)
	free (entry [i]);
    free (entry);

    entry = NULL;
    nentries = 0;
}
