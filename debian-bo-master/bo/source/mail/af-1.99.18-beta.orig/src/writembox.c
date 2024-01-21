/* Writembox.c - Program to write folder for af.
   Copyright (C) 1991, 1992, 1996 Malc Arnold.

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
#include <sys/errno.h>
#include "af.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: writembox.c,v 1.8 1996/08/28 17:44:08 malc Exp $";
static char *HeaderId = HEADERID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *malloc(), *lock();
extern int move_lockfile(), unlock();
extern long atol();
extern void free(), exit();

/* Local function declarations */

static int write_ok();
static long filesize();
static void usage();

/****************************************************************************/
/* Import the the system error flag */

extern int errno;

/****************************************************************************/
int main(argc, argv)
int argc;
char *argv[];
{
	/* Write stdin to the specified folder */

	char *folder, *lfile;
	int c, status;
	long size;
	FILE *fp;

	/* Check the arguments */

	if (argc != 2 && argc != 3) {
		usage(argv[0]);
	}

	folder = argv[1];
	size = (argc == 3) ? atol(argv[2]) : 0L;

	/* If folder isn't an incoming mailbox then reset the uid and gid */

	if (!MAILBOX(folder)) {
		(void) setuid(getuid());
		(void) setgid(getgid());
	}

	/* Check the user is allowed to write folder */

	if (!write_ok(folder)) {
		return(errno);
	}

	/* Lock the folder */

	if ((lfile = lock(folder)) == NULL) {
		return(ETXTBSY);
	}

	/* And double-check the size hasn't changed */

	if (size >= 0 && filesize(folder) > size) {
		(void) unlock(lfile);
		return(ETXTBSY);
	}

	/* Open the lock file */

	if ((fp = fopen(lfile, "w")) == NULL) {
		(void) unlock(lfile);
		return(errno);
	}

	/* Write stdin to the lock file */

	while ((c = getchar()) != EOF) {
		if (putc(c, fp) == EOF) {
			(void) fclose(fp);
			(void) unlock(lfile);
			return(errno);
		}
	}

	/* Close the file */

	(void) fclose(fp);

	/* Move the lock file onto the folder */

	status = move_lockfile(folder, lfile);

	/* Unlock the folder */

	(void) unlock(lfile);
	return(status);
}
/****************************************************************************/
static int write_ok(folder)
char *folder;
{
	/* Return TRUE if writing folder is permissible */

	char *dirnam, *p;
	int ok;

	/* Check access the easy way if the file exists */

	if (access(folder, 00) == 0) {
		return(access(folder, 02) == 0);
	}

	/* No file; set up the directory name */

	if ((dirnam = malloc(strlen(folder) + 1)) == NULL) {
		return(FALSE);
	}
	(void) strcpy(dirnam, folder);

	/* Trim the file name from the directory name */

	if ((p = strrchr(dirnam, DIRSEP)) == NULL) {
		/* No directory; use current dir */

		(void) strcpy(dirnam, ".");
	} else if (p == dirnam) {
		/* Special case for '/' */

		*(p + 1) = '\0';
	} else {
		/* Strip the file name */

		*p = '\0';
	}

	/* Check we can write in the directory */

	ok = (access(dirnam, 03) == 0);

	/* Clean up and return status */

	free(dirnam);
	return(ok);
}
/****************************************************************************/
static long filesize(folder)
char *folder;
{
	/* Return the last seek position of the file */

	struct stat buf;

	/* Return the size of the file in bytes */

	return((stat(folder, &buf) < 0) ? 0L : (long) buf.st_size);
}
/****************************************************************************/
static void usage(progname)
char *progname;
{
	/* Print a usage message to stderr and expire */

	(void) fputs("Usage: ", stderr);
	(void) fputs(progname, stderr);
	(void) fputs(" file [ size ]\n", stderr);
	exit(1);
}
/****************************************************************************/
