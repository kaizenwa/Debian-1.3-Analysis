/*
 * version.c: the ver() and atexit()/on_exit() routines.
 *  
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * Sat Oct 29 13:09:31 GMT 1994  Wilf. (G.Wilford@ee.surrey.ac.uk) 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>

#define NLS_SET	versionSet
#include "nls/nls.h"

#include "manconfig.h"
#include "lib/error.h"

/* print the version message, then exit */
void ver(void)
{
	printf(CATGETS(version_STRING,
	               "%s, version %s, db %s, %s (G.Wilford@ee.surrey.ac.uk)\n"),
	       program_name, VERSION, VER_ID, DATE);
	exit(OK);
}

/* If we are using message cats, make sure we close the cat before we leave */
#if defined(NLS) && ( defined(HAVE_ATEXIT) || defined(HAVE_ON_EXIT) )

extern int debug;

/* close the message catalogue on any exit */
#  if defined(HAVE_ATEXIT)
void close_catalogue(void)
{
	NLS_CLOSE;
	if (debug)
		fprintf(stderr, "close_catalogue()\n");
}
#  elif defined(HAVE_ON_EXIT)
void close_catalogue( int status, char *arg )
{
	NLS_CLOSE;
	if (debug)
		fprintf(stderr, "close_catalogue(%d)\n", status);
}
#  endif /* HAVE_ATEXIT */

#endif /* NLS && (HAVE_ATEXIT || HAVE_ON_EXIT) */
