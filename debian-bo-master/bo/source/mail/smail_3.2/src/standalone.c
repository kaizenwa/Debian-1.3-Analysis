#include "varargs.h"

/*
 * get_local_year stub for get_local_year in sysdep.c
 */
int
get_local_year()
{
      return 1915;
}

/*
 * define panic, fatal and write_log here, rather than
 * using the external routines.  We are testing and just want
 * the information displayed, not logged.
 */
/*VARARGS2*/
void
panic(exitcode, fmt, va_alist)
    int exitcode;			/* call exit(exitcode) */
    char *fmt;				/* printf(3) format */
    va_dcl                              /* arguments for printf */
{
    va_list ap;

    va_start(ap);
    (void)fprintf(stderr, "PANIC(%s): ", exitcode);
    (void)vfprintf(stderr, fmt, ap);
    putc('\n', stderr);			/* fatal messages not \n terminated */
    va_end(ap);

    return_to_sender = TRUE;
    exit(exitcode);
}

/*VARARGS2*/
void
write_log(log, fmt, va_alist)
    int log;				/* TRUE if to write global log file */
    char *fmt;				/* printf(3) format */
    va_dcl                              /* arguments for printf */
{
    va_list ap;

    va_start(ap);
    (void)vfprintf(stderr, fmt, ap);
    putc('\n', stderr);
    va_end(ap);
}
