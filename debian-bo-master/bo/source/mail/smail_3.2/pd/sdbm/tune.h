/* @(#) tune.h,v 1.1 1992/05/12 11:03:55 tron Exp */
/*
 * sdbm - ndbm work-alike hashed database library
 * tuning and portability constructs [not nearly enough]
 * author: oz@nexus.yorku.ca
 */

/*
 * ANSI?
 */

#ifdef ANSI_C

#include <stdlib.h>
#include <stddef.h>
#include <limits.h>

#define BYTESIZ CHAR_BIT

#else	/* ! ANSI_C */

extern int errno;
extern char *malloc();
extern void free();

#define BYTESIZ 8

#endif	/* ! ANSI_C */

/*
 * POSIX?
 */

#ifdef POSIX_OS
#include <unistd.h>
#else
extern long lseek();
#endif

/*
 * BSD?
 */

#ifndef SEEK_SET
#define SEEK_SET L_SET
#endif

/*
 * important tuning parms (hah)
 */

#define SEEDUPS			/* always detect duplicates */
#define BADMESS			/* generate a message for worst case:
				   cannot make room after SPLTMAX splits */

/*
 * misc
 */

#ifndef NULL
#define NULL	0
#endif

#ifdef DEBUG
#define debug(x)	printf x
#else
#define debug(x)
#endif
