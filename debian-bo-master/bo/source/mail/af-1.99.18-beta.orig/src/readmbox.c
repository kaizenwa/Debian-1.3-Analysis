/* Readmbox.c - Program to read mailbox for af.
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
#include <sys/errno.h>
#include "af.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: readmbox.c,v 1.9 1996/08/28 17:44:08 malc Exp $";
static char *HeaderId = HEADERID;
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *lock();
extern int unlock();
extern long atol();
extern void exit();

/* Local function declarations */

static int read_ok();
static void usage();

/****************************************************************************/
/* Import the system error flag */

extern int errno;

/****************************************************************************/
int main(argc, argv)
int argc;
char *argv[];
{
	/* Read the specified folder and print it to stdout */

	char *folder, *lfile;
	int c;
	long offset;
	FILE *fp;

	/* Check the arguments */

	if (argc != 2 && argc != 3) {
		usage(argv[0]);
	}
	folder = argv[1];
	offset = (argc == 3) ? atol(argv[2]) : 0L;

	/* If folder isn't an incoming mailbox then reset the uid and gid */

	if (!MAILBOX(folder)) {
		(void) setuid(getuid());
		(void) setgid(getgid());
	}

	/* Check the user is allowed to read folder */

	if (!read_ok(folder)) {
		return(errno);
	}

	/* Lock the folder */

	if ((lfile = lock(folder)) == NULL) {
		return(ETXTBSY);
	}

	/* Open the folder */

	if ((fp = fopen(folder, "r")) == NULL) {
		(void) unlock(lfile);
		return(errno);
	}

	/* Seek to any specified offset */

	if (fseek(fp, offset, 0) < 0) {
		(void) fclose(fp);
		(void) unlock(lfile);
		return(errno);
	}

	/* Print the folder to stdout */

	while ((c = getc(fp)) != EOF) {
		if (putchar((c == '\0') ? '\n' : c) == EOF) {
			(void) fclose(fp);
			(void) unlock(lfile);
			return(errno);
		}
	}

	/* Close and unlock the folder */

	(void) fclose(fp);
	(void) unlock(lfile);

	return(0);
}
/****************************************************************************/
static int read_ok(folder)
char *folder;
{
	/* Return TRUE if reading folder is permissible */

	return(access(folder, 00) || !access(folder, 04));
}
/****************************************************************************/
static void usage(progname)
char *progname;
{
	/* Print a usage message to stderr and expire */

	(void) fputs("Usage: ", stderr);
	(void) fputs(progname, stderr);
	(void) fputs(" file [ offset ]\n", stderr);
	exit(1);
}
/****************************************************************************/
