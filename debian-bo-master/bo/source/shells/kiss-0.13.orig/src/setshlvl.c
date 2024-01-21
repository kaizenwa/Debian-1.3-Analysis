#include "kiss.h"

/* for debugging */
#define DUMPENV { int i; for (i = 0; environ[i]; i++) puts (environ[i]); }

void setshlvl ()
{
    char
	buf [30];

    if (! flags.noenviron)
    {
	shlvl++;
	sprintf (buf, "%d", shlvl);

	addtoenv ("SHLVL", buf);
    }
}
