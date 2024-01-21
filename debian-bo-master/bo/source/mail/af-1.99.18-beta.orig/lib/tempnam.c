/* Tempnam.c - Get a temporary file name.
   Copyright (C) 1992, 1996 Malc Arnold.

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
#include <sys/types.h>
#include <sys/stat.h>

/****************************************************************************/
/* RCS info. */

#ifndef lint
static char *RcsId = "$Id: tempnam.c,v 1.3 1996/03/17 01:08:15 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/*LINTLIBRARY*/
/****************************************************************************/
/* Global function declarations */

extern char *malloc(), *getenv(), *mktemp();
extern int stat(), access(), strlen();
extern void free();

/****************************************************************************/
/* The template for use with mktemp */

#define TFILETEMPLATE	"%s%c%sXXXXXX"

/****************************************************************************/
char *tempnam(tmp_dir, tmp_prefix)
char *tmp_dir, *tmp_prefix;
{
	/*
	 * Return an allocated string containing a unique file name.
	 * Only a partial implementation - relies on tmp_dir being
	 * correctly defined.
	 */

	char *dir, *tfile;
	struct stat buf;

	/* Use TMPDIR if defined and valid */

	if ((dir = getenv("TMPDIR")) == NULL
	    || stat(dir, &buf) < 0 || (buf.st_mode & S_IFMT) != S_IFDIR
	    || !access(dir, 03)) {
		/* TMPDIR not defined or valid; use the default */

			dir = tmp_dir;
	}

	/* Make the template for mktemp */

	if ((tfile = malloc(strlen(dir) + strlen(tmp_prefix) + 8)) == NULL) {
		return(NULL);
	}
	(void) sprintf(tfile, TFILETEMPLATE, dir, '/', tmp_prefix);

	/* Now call mktemp and check for error */

	if (mktemp(tfile) != tfile) {
		free(tfile);
		tfile = NULL;
	}

	/* Return the file name */

	return(tfile);
}
/****************************************************************************/
