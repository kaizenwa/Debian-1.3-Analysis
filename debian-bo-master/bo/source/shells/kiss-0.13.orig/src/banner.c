#include "kiss.h"
#include "version.h"
#include "subversion.h"

void banner ()
{
    static char
	*msg [] =
	{
	    "Keep It Simple Stupid!",
	    "the Keep It Simple Shell",
	    "Karel's Interactive Simple Shell",
	    "Karel's Incredibly Stupid Shell",
	};

    srand ( (int) time (NULL) );
    
    printf ("\n"
	    "KISS V" VERSION " [" SUBVERSION "]\n"
	    "Copyright (c) Karel Kubat / ICCE 1995. All rights reserved.\n"
	    "Another MegaHard production!\n"
	    "\n"
	    "%s by karel Kubat (karel@icce.rug.nl).\n"
	    "%s\n\n",
	    progname, msg [rand () % (sizeof (msg) / sizeof (char *))]);

    exit (1);
}
