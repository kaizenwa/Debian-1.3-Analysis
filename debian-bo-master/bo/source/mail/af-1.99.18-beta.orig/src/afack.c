/* Afack.c - Main module for the af alias file checker.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Malc Arnold.

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
#include <varargs.h>
#include "af.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: afack.c,v 1.13 1996/08/28 17:44:08 malc Exp $";
static char *HeaderId = HEADERID;
#endif /* ! lint */

/****************************************************************************/
/* The command line switches valid for afack */

#define AFACKOPTS	"v"

/****************************************************************************/
/* Global function declarations */

extern int getopt();
extern void exit(), read_afile();
extern void list_aliases();

/* Local function declarations */

static int getargs();
static void usage();

/****************************************************************************/
/* This flag indicates whether the user has quit or not */

int user_quit = FALSE;

/****************************************************************************/
/* Import the index and error values for getopt() */

extern int optind, opterr;

/****************************************************************************/
int main(argc, argv)
int argc;
char **argv;
{
	/* Process and handle the arguments */

	int i, first_file, verbose;

	/* Process the command line */

	first_file = getargs(argc, argv, &verbose);

	/* Now check each file */

	for (i = first_file; i < argc; i++) {
		if (access(argv[i], 00) != 0) {
			(void) fprintf(stderr, "%s: Can't open %s.\n",
				       argv[0], argv[i]);
		} else {
			read_afile(argv[i], FALSE);
		}
	}

	if (verbose) {
		list_aliases();
	}

	return(0);
}
/****************************************************************************/
static int getargs(argc, argv, verbose)
int argc, *verbose;
char *argv[];
{
	/*
	 * Process the command line.  Set the verbose flag, and
	 * return the position of the first file name in argv.
	 */

	int opt;

	/* Turn off getopt() messages */

	opterr = 0;

	/* Initialize the user options */

	*verbose = FALSE;

	/* Now parse the command line */

	while ((opt = getopt(argc, argv, AFACKOPTS)) != EOF) {
		switch (opt) {
		case 'v':
			*verbose = TRUE;
			break;
		case '?':
			usage(argv[0]);		/* Print usage and quit */
		}

	}

	/* Check that files have been specified */

	if (optind == argc) {
		usage(argv[0]);
	}

	return(optind);
}
/****************************************************************************/
void panic(text)
char *text;
{
	/* Internal panic - shut down and exit, displaying text */

	(void) fprintf(stderr, "%s\n", text);
	exit(2);
}
/****************************************************************************/
/*VARARGS*/
void emsgl(va_alist)
va_dcl
{
	/*
	 * Display a list of strings to the standard error.
	 * The calling sequence is :
	 * 	emsgl(arg1, arg2, ... , NULL)
	 *
	 * All arguments must be strings.  This could be replaced
	 * by a function calling vsprintf, but there is little
	 * need, and vsprintf is not universally available.
	 */

	char *arg;
	va_list arglist;

	/* Initialise the varargs handling */

	va_start(arglist);

	/* Now loop through the arguments, printing them */

	while ((arg = va_arg(arglist, char *)) != NULL) {
		(void) fputs(arg, stderr);
	}

	/* End the line and clean up varargs handling */

	(void) putc('\n', stderr);
	va_end(arglist);

	return;
}
/****************************************************************************/
void table(name, entry)
char *name, *entry;
{
	/* For afack, this tabulates the entry to stdout */

	char *spaces = "                                ";	/* Indent */

	(void) fputs(name, stdout);

	/* Ignore entry if it is NULL */

	if (entry != NULL) {
		if (strlen(name) < strlen(spaces)) {
			(void) fputs(spaces + strlen(name), stdout);
		}
		(void) fputs(entry, stdout);
	}

	(void) putchar('\n');
	return;
}
/****************************************************************************/
static void usage(prognam)
char *prognam;
{
	/* Print a usage message and exit abnormally */

	(void) fprintf(stderr, "Usage : %s file ...\n", prognam);
	exit(1);
}
/****************************************************************************/
