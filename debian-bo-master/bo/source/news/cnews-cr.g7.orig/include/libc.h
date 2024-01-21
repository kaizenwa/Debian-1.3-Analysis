#ifndef LIBC_H
#define LIBC_H
/*
 * declarations of (supposedly) standard C library functions and types.
 */

extern time_t time();			/* sys/timeb.h? time.h sez POSIX */

extern FILE *popen();			/* stdio.h */

/* these unfortunately cannot be relied upon to be in the right header */
extern struct passwd *getpwnam();	/* pwd.h */
extern char *ctime();			/* time.h */

extern char *mktemp();

extern int optind;
extern char *optarg;

#include "alloc.h"
#include <stdlib.h>
#endif					/* LIBC_H */
