/* Getopt.c - Parse command line options.
   Copyright (C) 1996 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
   
/* Handle which string header we include */

#ifdef HAVE_STRING_H
#include <string.h>
#else /* ! HAVE_STRING_H */
#include <strings.h>
#ifndef strchr
#define strchr(s, c) index(s, c)
#endif /* ! strchr */
#endif /* ! HAVE_STRING_H */

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: getopt.c,v 1.4 1996/08/28 17:35:38 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
/* The character returned by getopt on failure */

#define GETOPT_FAIL	'?'

/****************************************************************************/
/* These error messages are defined by POSIX */

#define	GETOPT_ILLEGAL	"%s: illegal option -- %c\n"
#define GETOPT_NOARG	"%s: option requires an argument -- %c\n"

/****************************************************************************/
/* Global variables defined by getopt */

char *optarg = NULL;		/* The value of an option's argument */
int optind = 1;			/* The next argument in argv[] to scan */
int opterr = 1;			/* Print error messages only if set to 1 */
int optopt = GETOPT_FAIL;	/* The value of any unmatched option */

/****************************************************************************/
/* Global function declarations */

extern int strcmp();

/****************************************************************************/
int getopt(argc, argv, optstring)
int argc;
char **argv, *optstring;
{
	/* Return the next option in argv, or EOF if none */

	static char *option = NULL;
	char *c;

	/* Check that we have options to process */

	if (option == NULL) {
		/* Try to find the next option argument */

		option = (optind < argc && *argv[optind] == '-')
			? argv[optind] + 1 : NULL;

		/* Check for a terminating '--' argument */

		if (option != NULL && !strcmp(argv[optind], "--")) {
			/* Clear the option and skip the argument */

			option = NULL;
			optind++;
		}

		/* Return end-of-options if we didn't find one */

		if (option == NULL) {
			return(EOF);
		}
	}

	/* Store the option found in optopt */

	optopt = *option;

	/* Now search for the option in optstring */

	if ((c = strchr(optstring, *option++)) == NULL && opterr) {
		/* Bad option, print an error message */

		fprintf(stderr, GETOPT_ILLEGAL, argv[0], optopt);
	}

	/* Update the position in options and arguments */

	option = (*option != '\0') ? option : NULL;
	optind = (option == NULL && optind < argc) ? optind + 1 : optind;

	/* If we have an option, do we need an argument? */

	if (c != NULL && *(c + 1) == ':') {
		/* Get the argument and update the positions */

		optarg = (option != NULL) ? option :
			(optind < argc) ? argv[optind] : NULL;
		optind = (optarg != NULL) ? optind + 1 : optind;
		option = NULL;

		/* Handle an error extracting the argument */

		if (optarg == NULL && opterr) {
			/* No argument, print an error message */

			fprintf(stderr, GETOPT_NOARG, argv[0], optopt);
			return(GETOPT_FAIL);
		} else if (optarg == NULL) {
			/* Just silently return failure */

			return(GETOPT_FAIL);
		}
	}

	/* And return the option character */

	return((c != NULL) ? optopt : GETOPT_FAIL);
}
/****************************************************************************/
