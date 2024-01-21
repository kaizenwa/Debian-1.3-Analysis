/* Mailbox.c - Handle mailbox I/O for af.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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
static char *RcsId = "$Id: mailbox.c,v 1.20 1997/05/06 16:16:26 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *xmalloc(), *strerror(), *utos();
extern int wait(), unlock(), nonempty();
extern void free(), _exit(), emsgl();

#ifndef READ_VIA_PIPES
extern char *lock();
extern int move_lockfile();
extern long filesize();
#endif /* ! READ_VIA_PIPES */

/* Local function declarations */

static int is_reg_file(), read_ok(), write_ok();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
#ifdef READ_VIA_PIPES
/****************************************************************************/
FILE *open_folder(folder, towrite, offset)
char *folder;
int towrite;
long offset;
{
	/* Open folder in the mode specified */

	char off_str[15];
	int fds[2];
	FILE *fp;

	/* Check that access to the folder is permitted */

	if (!towrite && !read_ok(folder) || towrite && !write_ok(folder)) {
		emsgl("Can't open ", folder, ": ", strerror(errno), NULL);
		return(NULL);
	}

	/* Check that folder is a regular file */

	if (!is_reg_file(folder)) {
		emsgl("Can't open ", folder, ": Not a regular file", NULL);
		return(NULL);
	}

	/* Generate a pipe */

	if (pipe(fds) < 0) {
		emsgl("Can't create pipe to access ", folder,
		      ": ", strerror(errno), NULL);
		return(NULL);
	}

	/* Now fork */

	switch(fork()) {
	case -1:					/* Failed */
		emsgl("Can't fork to access ", folder,
		      ": ", strerror(errno), NULL);
		(void) close(fds[0]);
		(void) close(fds[1]);
		return(NULL);
	case 0:						/* Child */
		/* Set up the offset for the folder */

		(void) sprintf(off_str, "%ld", offset);

		/* Now set up the pipe for reading or writing */

		if (towrite) {
			/* Set up the read end of the pipe */

			(void) close(fds[1]);
			(void) dup2(fds[0], 0);

			/* And execute the mailbox writer */

			(void) execl(WRITEMBOX, WRITEMBOX,
				     folder, off_str, NULL);
		} else {
			/* Set up the write end of the pipe */

			(void) close(fds[0]);
			(void) dup2(fds[1], 1);

			/* And execute the mailbox reader */

			(void) execl(READMBOX, READMBOX,
				     folder, off_str, NULL);
		}

		/* Give up at this stage */

		_exit(errno);
	default:					/* Parent */
		/* Set up the af ends of the pipe */

		if (towrite) {
			(void) close(fds[0]);
			fp = fdopen(fds[1], "w");
		} else {
			(void) close(fds[1]);
			fp = fdopen(fds[0], "r");
		}
		if (fp == NULL) {
			emsgl("Can't open pipe to access ", folder,
			      ": ", strerror(errno), NULL);
		}
		return(fp);
	}
	/*NOTREACHED*/
}
/****************************************************************************/
int close_folder(folder, fp)
char *folder;
FILE *fp;
{
	/* Close fp, which is a file pointer to folder */

	int status;

	/* Close the file */

	(void) fclose(fp);

	/* Wait for the child to terminate and report errors */

	(void) wait(&status);
	if (RETURNSIG(status)) {
		emsgl("Error accessing ", folder,
		      ": process terminated by signal ",
		      utos(RETURNVAL(status)), NULL);
	} else if (RETURNVAL(status)) {
		emsgl("Error accessing ", folder, ": ",
		      strerror(RETURNVAL(status)), NULL);
	}
	return(status);
}
/****************************************************************************/
void abort_folder(folder, fp)
char *folder;
FILE *fp;
{
	/*
	 * Close fp, which is a file pointer to folder, after an
	 * error writing the output.  This can be assumed to have
	 * been caused by the write process dying, and so we can
	 * do normal cleanup here, and let writembox handle the
	 * error.  The only difference is we suppress any error
	 * messages, since we've already had an error.
	 */

	int status;

	/* Close the file and wait for the child */

	(void) fclose(fp);
	(void) wait(&status);

	/* That's the best we can do */

	return;
}
/****************************************************************************/
#else /* ! READ_VIA_PIPES */
/****************************************************************************/
/* The saved uid and gid, recovered to access incoming mailboxes */

static uid_t saved_uid = -1;
static gid_t saved_gid = -1;

/* Are we writing the current folder? */

static int writing = FALSE;

/* The lock file associated with the current folder */

static char *lock_file = NULL;

/****************************************************************************/
void save_ids()
{
	/* Save the effective uid and gid and revert to the real ones */

	saved_uid = geteuid();
	saved_gid = getegid();

	(void) setuid(getuid());
	(void) setgid(getgid());

	return;
}
/****************************************************************************/
FILE *open_folder(folder, towrite, offset)
char *folder;
int towrite;
long offset;
{
	/* Lock and open the folder in the mode specified */

	FILE *fp;

	/* Are we reading or writing? */

	writing = (towrite);

	/* Check that access to the folder is permitted */

	if (!writing && !read_ok(folder) || writing && !write_ok(folder)) {
		emsgl("Can't open ", folder, ": ", strerror(errno), NULL);
		return(NULL);
	}

	/* Check that folder is a regular file */

	if (!is_reg_file(folder)) {
		emsgl("Can't open ", folder, ": Not a regular file", NULL);
		return(NULL);
	}

	/* If folder is an incoming mailbox then set the uid and gid */

	if (MAILBOX(folder)) {
		(void) setuid(saved_uid);
		(void) setgid(saved_gid);
	}

	/* Lock the folder and check the file size if writing */

	if ((lock_file = lock(folder)) == NULL || writing
	    && offset >= 0 && filesize(folder) > offset) {
		emsgl("Can't open ", folder, ": ", strerror(ETXTBSY), NULL);
		if (lock_file != NULL) {
			unlock(lock_file);
		}
		return(NULL);
	}

	/* Open the folder if reading, or the lockfile if writing */

	fp = (writing) ? fopen(lock_file, "w") : fopen(folder, "r");

	if (fp == NULL) {
		emsgl("Can't open ", folder, ": ", strerror(errno), NULL);
		(void) unlock(lock_file);
		return(NULL);
	}

	/* Seek to any specified offset if reading */

	if (!writing && fseek(fp, offset, 0) < 0) {
		emsgl("Error reading ", folder,
		      ": ", strerror(errno), NULL);
		(void) fclose(fp);
		(void) unlock(lock_file);
		return(NULL);
	}

	return(fp);
}
/****************************************************************************/
int close_folder(folder, fp)
char *folder;
FILE *fp;
{
	/* Close fp, which is a file pointer to folder */

	int status = 0;

	/* Close the file */

	(void) fclose(fp);

	/* If writing, move the lock file onto the folder */

	if (writing && (status = move_lockfile(folder, lock_file))) {
		emsgl("Error accessing ", folder,
		      ": ", strerror(status), NULL);
	}

	/* Unlock the folder */

	(void) unlock(lock_file);

	/* If folder is an incoming mailbox then reset the uid and gid */

	if (MAILBOX(folder)) {
		(void) setuid(getuid());
		(void) setgid(getgid());
	}

	return(status);
}
/****************************************************************************/
void abort_folder(folder, fp)
char *folder;
FILE *fp;
{
	/*
	 * Close fp, which is a file pointer to folder, after an
	 * error writing the output.  This means we don't want to
	 * copy the lock file onto the original file, since the
	 * lock file is truncated.
	 */

	/* Close the file and unlock the folder */

	(void) fclose(fp);
	(void) unlock(lock_file);

	/* If folder is an incoming mailbox then reset the uid and gid */

	if (MAILBOX(folder)) {
		(void) setuid(getuid());
		(void) setgid(getgid());
	}

	/* That's the best we can do */

	return;
}
/****************************************************************************/
#endif /* ! READ_VIA_PIPES */
/****************************************************************************/
int empty_folder(folder)
char *folder;
{
	/* Return TRUE if folder is readable but empty */

	return(access(folder, 00) || !access(folder, 04)
	       && is_reg_file(folder) && !nonempty(folder));
}
/****************************************************************************/
static int read_ok(folder)
char *folder;
{
	/* Return TRUE if reading folder is permissible */

	return(access(folder, 00) && errno == ENOENT || !access(folder, 04));
}
/****************************************************************************/
static int write_ok(folder)
char *folder;
{
	/* Return TRUE if writing folder is permissible */

	char *dirnam, *p;
	int ok;

	/* Check access the easy way if the file exists */

	if (!access(folder, 00)) {
		return(!access(folder, 02));
	}

	/* No file, default the directory name */

	dirnam = xmalloc(strlen(folder) + 1);
	(void) strcpy(dirnam, folder);

	/* Now set the directory the folder's in */

	if ((p = strrchr(dirnam, DIRSEP)) == NULL) {
		(void) strcpy(dirnam, ".");
	} else if (p == dirnam) {
		*(p + 1) = '\0';
	} else {
		*p = '\0';
	}

	/* Check we can write in the directory */

	ok = (!access(dirnam, 03));

	/* Free the directory space */

	free(dirnam);
	return(ok);
}
/****************************************************************************/
static int is_reg_file(folder)
char *folder;
{
	/* Report whether folder is a regular file */

	struct stat sbuf;

	/* Return TRUE if no such file or is a regular file */

	return (stat(folder, &sbuf) < 0 ||
		(sbuf.st_mode & S_IFMT) == S_IFREG);
}
/****************************************************************************/
