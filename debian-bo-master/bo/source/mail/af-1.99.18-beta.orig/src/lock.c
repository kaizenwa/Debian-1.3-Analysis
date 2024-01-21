/* Lock.c - Handle file locking and backup files for af.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1997 Malc Arnold.

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
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "af.h"
#include STRING_HDR

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifndef READ_VIA_PIPES
#include "misc.h"
#endif /* ! READ_VIA_PIPES */

#ifdef HAVE_FLOCK
#include <sys/file.h>
#endif /* HAVE_FLOCK */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *RcsId = "$Id: lock.c,v 1.23 1997/03/31 18:32:19 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* Global function declarations */

extern char *malloc(), *tempnam();
extern unsigned sleep();
extern void free();

#ifndef READ_VIA_PIPES
extern unsigned save_atimer();
extern unsigned restore_atimer();
#endif /* ! READ_VIA_PIPES */

/* Local function declarations */

static char *lockfile(), *backupfile();
static int create_lockfile(), copy_backup();

/****************************************************************************/
/* Import the system error number */

extern int errno;

/****************************************************************************/
/* This static records whether we made a lock file or a backup file */

static int lock_made = FALSE;

/****************************************************************************/
char *lock(folder)
char *folder;
{
	/*
	 * Lock the specified folder, or fail if it is already
	 * locked and remains so for too long.
	 *
	 * If we can't create a lockfile in the target directory
	 * then we need to create one in directory BFILEDIR, since
	 * lockfiles are also used to create a backup copy of a
	 * file when writing it.
	 */

	char *lfile;
	int try;

#ifndef READ_VIA_PIPES
	ATIMER tbuf;
#endif /* ! READ_VIA_PIPES */

	/* Get the name of the lock file */

	lfile = lockfile(folder);

	/* Now wait for the lock file to be nonexistent */

	try = 0;
	while (access(lfile, 00) == 0) {
		/* Lock file exists, retry? */

		if (try++ > MAXLTRY) {
			return(NULL);
		}

		/* Turn off any alarm timers and sleep */

#ifdef READ_VIA_PIPES
		(void) sleep(1);
#else /* ! READ_VIA_PIPES */
		(void) save_atimer(&tbuf);
		(void) sleep(1);
		(void) restore_atimer(&tbuf);
#endif /* ! READ_VIA_PIPES */
	}

	/* Now set our own lock file */

	if (!(lock_made = create_lockfile(lfile))) {
		/* Can't lock file; use a backup */

		lfile = backupfile();
	}

	/* And return the lockfile name */

	return(lfile);
}
/****************************************************************************/
int unlock(lfile)
char *lfile;
{
	/* Unlock folder by removing the lock file */

	return((unlink(lfile) < 0) ? errno : 0);
}
/****************************************************************************/
static char *lockfile(folder)
char *folder;
{
        /* Return (in a static buffer) the lock file for folder */

        /* The static buffer for the lock file name */

        static char *lfile = NULL;

	char *slash;
	int len;

        /* If lfile is not null then free it */

        if (lfile != NULL) {
                free(lfile);
        }

        /*
	 * Generate the lock file name.  The names for incoming mailboxes
	 * and folders are different to avoid problems with file name
	 * lengths on SysV.
	 */

	if (MAILBOX(folder)) {
		lfile = malloc(strlen(folder) + strlen(LOCKFILE) + 1);
		(void) strcpy(lfile, folder);
		(void) strcat(lfile, LOCKFILE);
	} else if ((slash = strrchr(folder, '/')) != NULL) {
		len = (slash - folder) + 1;
		lfile = malloc(strlen(folder) + strlen(LFILEPFX) + 1);
		(void) strncpy(lfile, folder, len);
		lfile[len] = '\0';
		(void) strcat(lfile, LFILEPFX);
		(void) strcat(lfile, slash + 1);
	} else {
		lfile = malloc(strlen(folder) + strlen(LFILEPFX) + 1);
		(void) strcpy(lfile, LFILEPFX);
		(void) strcat(lfile, folder);
	}

        return(lfile);
}
/****************************************************************************/
static char *backupfile()
{
	/* Create a backup file if we can't manage a lock file */

	static char *bfile = NULL;		/* The buffer we return */
	int fd;

	/* If bfile is not null then free it */

	if (bfile != NULL) {
		free(bfile);
	}

	/* Create the backup file if possible */

	if ((bfile = tempnam(BFILEDIR, BFILEPFX)) != NULL) {
		if ((fd = creat(bfile, 0666)) < 0) {
			/* Error creating the file */

			free(bfile);
			bfile = NULL;
		} else {
			/* Close the file we created */

			(void) close(fd);
		}
	}
	return(bfile);
}
/****************************************************************************/
static int create_lockfile(lfile)
char *lfile;
{
	/* Create a lockfile in an NFS-friendly way */

	char *slash;
	char *tdir, *tfile;
	int fd;
	struct stat buf;

	/* Split the lockfile path from the file name */

	if ((slash = strrchr(lfile, '/')) != NULL) {
		/* Use the file path as the temp dir */

		tdir = malloc(slash - lfile + 1);
		(void) strncpy(tdir, lfile, slash - lfile);
		tdir[slash - lfile] = '\0';
	} else {
		tdir = strdup(".");
	}

	/* Generate the temporary file name */

	if ((tfile = tempnam(tdir, TFILEPFX)) == NULL) {
		free(tdir);
		return(FALSE);
	}
	
	/* Now create the temporary file */

	if ((fd = creat(tfile, 0660)) < 0) {
		free(tfile);
		free(tdir);
		return(FALSE);
	}

	/* Close the temp file and link it to the lock file */

	(void) close(fd);
	if (link(tfile, lfile) < 0 || stat(tfile, &buf) < 0
	    || buf.st_nlink < 2) {
		/* Error creating the lock file */

		(void) unlink(lfile);
		(void) unlink(tfile);
		free(tfile);
		free(tdir);
		return(FALSE);
	}

	/* Got the lock file, remove the temporary */

	(void) unlink(tfile);
	free(tfile);
	free(tdir);

	/* And return success */

	return(TRUE);
}
/****************************************************************************/
int move_lockfile(folder, lfile)
char *folder, *lfile;
{
	/* Move or copy the lockfile onto the folder */

	uid_t uid;
	gid_t gid;
	struct stat buf;

	/* If we don't have a lock file then copy backup to destination */

	if (!lock_made) {
		return(copy_backup(folder, lfile));
	}

	/* Get information about the target file */

	if (!stat(folder, &buf)) {
		/* Check the number of links to the folder */

		if (buf.st_nlink > 1) {
			return(copy_backup(folder, lfile));
		}

		/* Try and recreate the original ownership */

		uid = (buf.st_uid == geteuid()) ? -1 : buf.st_uid;
		gid = (buf.st_gid == getegid()) ? -1 : buf.st_gid;

		if (chown(lfile, uid, gid) < 0) {
			return(copy_backup(folder, lfile));
		}

		/* Try and recreate the original modes */

		if (chmod(lfile, buf.st_mode) < 0) {
			return(copy_backup(folder, lfile));
		}

		/* Remove the folder */

		if (unlink(folder) < 0) {
			return(errno);
		}
	}

	/* Move the lock file onto the folder */

	return((link(lfile, folder) < 0) ? errno : 0);
}
/****************************************************************************/
static int copy_backup(folder, lfile)
char *folder, *lfile;
{
	/* Copy the backup file into the destination folder */

	FILE *ffp, *lfp;
	int c;

	/* Open the lock file */

	if ((lfp = fopen(lfile, "r")) == NULL) {
		return(errno);
	}

	/* Open the folder */

	if ((ffp = fopen(folder, "w")) == NULL) {
		(void) fclose(lfp);
		return(errno);
	}

#ifdef HAVE_LOCKF
	/* Lock the folder with lockf while we write */

	if (lockf(fileno(ffp), 1, 0) < 0) {
		(void) fclose(lfp);
		(void) fclose(ffp);
		return(errno);
	}
#endif /* ! HAVE_LOCKF */

#ifdef HAVE_FLOCK
	/* Lock the folder with flock while we write */

	if (flock(fileno(ffp), LOCK_EX) < 0) {
		(void) fclose(lfp);
		(void) fclose(ffp);
		return(errno);
	}
#endif /* HAVE_FLOCK */

	/* Now do the copy */

	while ((c = getc(lfp)) != EOF) {
		if (putc(c, ffp) == EOF) {
			(void) fclose(lfp);
			(void) fclose(ffp);
			return(errno);
		}
	}

	/* Close the files, removing the lock, and return ok */

	(void) fclose(lfp);
	(void) fclose(ffp);

	return(0);
}
/****************************************************************************/
