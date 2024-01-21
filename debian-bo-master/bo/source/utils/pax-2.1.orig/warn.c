/* $Source: /usr/local/src/pax/RCS/warn.c,v $
 *
 * $Revision: 2.2 $
 *
 * warn.c - miscellaneous user warning routines
 *
 * DESCRIPTION
 *
 *	These routines provide the user with various forms of warning
 *	and informational messages.
 *
 * AUTHOR
 *
 *     Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
 *
 * COPYRIGHT
 *
 *	Copyright (c) 1989 Mark H. Colburn.  All rights reserved.
 *
 *	Redistribution and use in source and binary forms are permitted
 *	provided that the above copyright notice and this paragraph are
 *	duplicated in all such forms and that any documentation,
 *	advertising materials, and other materials related to such
 *	distribution and use acknowledge that the software was developed
 *	by Mark H. Colburn.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * $Log: warn.c,v $
 * Revision 2.2  1996/10/20  12:10:20  istewart
 * Fix some definitions, add strftime & strcasecmp
 *
 * Revision 2.1  1996/10/18  21:36:18  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char        *ident = "$Id: warn.c,v 2.2 1996/10/20 12:10:20 istewart Exp $";
static char        *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif /* ! lint */


/* Headers */

#include "pax.h"


/* Function Prototypes */

static void	    prsize __P ((FILE *, off_t));

/* warnarch - print an archive-related warning message and offset
 *
 * DESCRIPTION
 *
 *	Present the user with an error message and an archive offset at
 *	which the error occured.   This can be useful for diagnosing or
 *	fixing damaged archives.
 *
 * PARAMETERS
 *
 *	char 	*msg	- A message string to be printed for the user.
 *	off_t 	adjust	- An adjustment which is added to the current
 *			  archive position to tell the user exactly where
 *			  the error occurred.
 */

#ifdef __STDC__
void		warnarch (char *msg, off_t adjust)
#else
void		warnarch (msg, adjust)
    char	*msg;
    off_t	 adjust;
#endif
{
    fprintf(stderr, "%s: [offset ", myname);
    prsize(stderr, total - adjust);
    fprintf(stderr, "]: %s\n", msg);
}


/* prsize - print a file offset on a file stream
 *
 * DESCRIPTION
 *
 *	Prints a file offset to a specific file stream.  The file offset is
 *	of the form "%dm+%dk+%d", where the number preceeding the "m" and
 *	the "k" stand for the number of Megabytes and the number of
 *	Kilobytes, respectivley, which have been processed so far.
 *
 * PARAMETERS
 *
 *	FILE  *stream	- Stream which is to be used for output
 *	off_t  size	- Current archive position to be printed on the output
 *			  stream in the form: "%dm+%dk+%d".
 *
 */

#ifdef __STDC__
static void	prsize (FILE * stream, off_t size)
#else
static void	prsize (stream, size)
    FILE	*stream;	/* stream which is used for output */
    off_t	 size;	/* current archive position to be printed */
#endif

{
    off_t               n;

    if ((n = (size / (1024L * 1024L)))) {
	fprintf(stream, "%ldm+", n);
	size -= n * 1024L * 1024L;
    }
    if ((n = (size / 1024L))) {
	fprintf(stream, "%ldk+", n);
	size -= n * 1024L;
    }
    fprintf(stream, "%ld", size);
}


/* fatal - print fatal message and exit
 *
 * DESCRIPTION
 *
 *	Fatal prints the program's name along with an error message, then
 *	exits the program with a non-zero return code.
 *
 * PARAMETERS
 *
 *	char 	*why	- description of reason for termination
 *
 * RETURNS
 *
 *	Returns an exit code of 1 to the parent process.
 */

#ifdef __STDC__
void		fatal (char *why)
#else
void		fatal (why)
    char	*why;	/* description of reason for termination */
#endif
{
    fprintf(stderr, "%s: %s\n", myname, why);
    exit(1);
}


/* warn - print a warning message
 *
 * DESCRIPTION
 *
 *	Print an error message listing the program name, the actual error
 *	which occurred and an informational message as to why the error
 *	occurred on the standard error device.  The standard error is
 *	flushed after the error is printed to assure that the user gets
 *	the message in a timely fasion.
 *
 * PARAMETERS
 *
 *	char *what	- Pointer to string describing what failed.
 *	char *why	- Pointer to string describing why did it failed.
 */

#ifdef __STDC__
void		warn (char *what, char *why)
#else
void		warn (what, why)
    char	*what;	/* message as to what the error was */
    char	*why;	/* explanation why the error occurred */
#endif
{
    fprintf(stderr, "%s: %s : %s\n", myname, what, why);
    fflush(stderr);
}
