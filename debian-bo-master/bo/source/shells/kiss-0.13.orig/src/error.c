#include "kiss.h"

void error (char *fmt, ...)
{
    va_list
	args;
    
    fprintf (stderr, "%s: ", progname);
    va_start (args, fmt);
    vfprintf (stderr, fmt, args);
    fputc ('\n', stderr);

    exit (1);
}
