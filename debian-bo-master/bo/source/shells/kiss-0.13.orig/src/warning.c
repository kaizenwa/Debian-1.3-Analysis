#include "kiss.h"

int warning (char *fmt, ...)
{
    va_list
	args;
    
    fprintf (stderr, "%s: ", progname);
    va_start (args, fmt);
    vfprintf (stderr, fmt, args);
    fputc ('\n', stderr);

    return (1);
}
