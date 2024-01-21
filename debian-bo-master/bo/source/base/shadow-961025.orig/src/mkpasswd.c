/*
 * Copyright 1990 - 1994, John F. Haugh II
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by John F. Haugh, II
 *      and other contributors.
 * 4. Neither the name of John F. Haugh, II nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY JOHN HAUGH AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL JOHN HAUGH OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <config.h>

#include "rcsid.h"
RCSID("$Id: mkpasswd.c,v 1.1.1.1 1996/08/10 07:59:51 marekm Exp $")

#include <sys/stat.h>
#include "prototypes.h"
#include "defines.h"
#include <stdio.h>

#if !defined(DBM) && !defined(NDBM) /*{*/
int
main(argc, argv)
	int argc;
	char **argv;
{
	fprintf(stderr, "%s: no DBM database on system - no action performed\n",
		argv[0]);
	return 0;
}

#else /*} defined(DBM) || defined(NDBM) {*/

#include <fcntl.h>
#include <pwd.h>

#ifdef	DBM
#include <dbm.h>
#endif
#ifdef	NDBM
#include <ndbm.h>
#include <grp.h>

extern	DBM	*pw_dbm;
extern	DBM	*gr_dbm;
#ifdef	SHADOWPWD
extern	DBM	*sp_dbm;
#endif
#ifdef	SHADOWGRP
extern	DBM	*sg_dbm;
#endif
char	*fgetsx();
#endif	/* NDBM */

char	*CANT_OPEN =	"%s: cannot open file %s\n";
char	*CANT_OVERWRITE = "%s: cannot overwrite file %s\n";
#ifdef	DBM
char	*CANT_CREATE =	"%s: cannot create %s\n";
#endif
char	*DBM_OPEN_ERR =	"%s: cannot open DBM files for %s\n";
char	*PARSE_ERR =	"%s: error parsing line\n\"%s\"\n";
char	*LINE_TOO_LONG = "%s: the beginning with \"%.16s ...\" is too long\n";
char	*ADD_REC =	"adding record for name \"%s\"\n";
char	*ADD_REC_ERR =	"%s: error adding record for \"%s\"\n";
char	*INFO =		"added %d entries, longest was %d\n";
#ifdef	NDBM
#ifdef	SHADOWPWD
#ifdef	SHADOWGRP
char	*USAGE =	"Usage: %s [ -vf ] [ -p|g|sp|sg ] file\n";
#else	/* !SHADOWGRP */
char	*USAGE =	"Usage: %s [ -vf ] [ -p|g|sp ] file\n";
#endif	/* SHADOWGRP */
#else	/* !SHADOWPWD */
char	*USAGE =	"Usage: %s [ -vf ] [ -p|g ] file\n";
#endif	/* SHADOWPWD */
#else	/* !NDBM */
char	*USAGE =	"Usage: %s [ -vf ] file\n";
#endif	/* NDBM */

char	*Progname;
int	vflg = 0;
int	fflg = 0;
#ifdef	NDBM
int	gflg = 0;
int	sflg = 0;		/* -s flag -- leave in, makes code nicer */
int	pflg = 0;
#endif

void	usage();

extern	char	*xmalloc();
extern	struct	passwd	*sgetpwent();
extern	int	pw_dbm_update();
#ifdef	NDBM
extern	struct	group	*sgetgrent();
extern	int	gr_dbm_update();
#ifdef	SHADOWPWD
extern	struct	spwd	*sgetspent();
extern	int	sp_dbm_update();
#endif
#ifdef	SHADOWGRP
extern	struct	sgrp	*sgetsgent();
extern	int	sg_dbm_update();
#endif
#endif	/* NDBM */

/*
 * mkpasswd - create DBM files for /etc/passwd-like input file
 *
 * mkpasswd takes an an argument the name of a file in /etc/passwd format
 * and creates a DBM file keyed by user ID and name.  The output files have
 * the same name as the input file, with .dir and .pag appended.
 *
 * if NDBM is defined this command will also create look-aside files for
 * /etc/group, /etc/shadow, and /etc/gshadow.
 */

int
main (argc, argv)
int	argc;
char	**argv;
{
	extern	int	optind;
	extern	char	*optarg;
	FILE	*fp;			/* File pointer for input file       */
	char	*file;			/* Name of input file                */
	char	*dir;			/* Name of .dir file                 */
	char	*pag;			/* Name of .pag file                 */
	char	*cp;			/* Temporary character pointer       */
	int	flag;			/* Flag for command line option      */
#ifdef	DBM
	int	fd;			/* File descriptor of open DBM file  */
#endif
	int	cnt = 0;		/* Number of entries in database     */
	int	longest = 0;		/* Longest entry in database         */
	int	len;			/* Length of input line              */
	int	errors = 0;		/* Count of errors processing file   */
	char	buf[BUFSIZ*8];		/* Input line from file              */
	struct	passwd	*passwd=NULL;	/* Pointer to password file entry    */
#ifdef	NDBM
	struct	group	*group=NULL;   	/* Pointer to group file entry       */
#ifdef	SHADOWPWD
	struct	spwd	*shadow=NULL;	/* Pointer to shadow passwd entry    */
#endif
#ifdef	SHADOWGRP
	struct	sgrp	*gshadow=NULL;	/* Pointer to shadow group entry     */
#endif
	DBM	*dbm;			/* Pointer to new NDBM files         */
	DBM	*dbm_open();		/* Function to open NDBM files       */
#endif

	/*
	 * Figure out what my name is.  I will use this later ...
	 */

	Progname = Basename(argv[0]);

	/*
	 * Figure out what the flags might be ...
	 */

#ifdef	NDBM
	while ((flag = getopt (argc, argv, "fvpgs")) != EOF)
#else
	while ((flag = getopt (argc, argv, "fv")) != EOF)
#endif
	{
		switch (flag) {
			case 'v':
				vflg++;
				break;
			case 'f':
				fflg++;
				break;
#ifdef	NDBM
			case 'g':
				gflg++;
#ifndef	SHADOWGRP
				if (sflg)
					usage ();
#endif
				if (pflg)
					usage ();

				break;
#if defined(SHADOWPWD) || defined(SHADOWGRP)
			case 's':
				sflg++;
#ifndef	SHADOWGRP
				if (gflg)
					usage ();
#endif
				break;
#endif
			case 'p':
				pflg++;
				if (gflg)
					usage ();

				break;
#endif
			default:
				usage ();
		}
	}
#ifdef NDBM
	/*
	 * Backwards compatibility fix for -p flag ...
	 */

#ifdef SHADOWPWD
	if (! sflg && ! gflg)
#else
	if (! gflg)
#endif
		pflg++;
#endif	/* NDBM */

	/*
	 * The last and only remaining argument must be the file name
	 */

	if (argc - 1 != optind)
		usage ();

	file = argv[optind];

	if (! (fp = fopen (file, "r"))) {
		fprintf (stderr, CANT_OPEN, Progname, file);
		exit (1);
	}

	/*
	 * Make the filenames for the two DBM files.
	 */

	dir = xmalloc (strlen (file) + 5);	/* space for .dir file */
	strcat (strcpy (dir, file), ".dir");

	pag = xmalloc (strlen (file) + 5);	/* space for .pag file */
	strcat (strcpy (pag, file), ".pag");

	/*
	 * Remove existing files if requested.
	 */

	if (fflg) {
		(void) unlink (dir);
		(void) unlink (pag);
	}

	/*
	 * Create the two DBM files - it is an error for these files
	 * to have existed already.
	 */

	if (access (dir, 0) == 0) {
		fprintf (stderr, CANT_OVERWRITE, Progname, dir);
		exit (1);
	}
	if (access (pag, 0) == 0) {
		fprintf (stderr, CANT_OVERWRITE, Progname, pag);
		exit (1);
	}

#ifdef	NDBM
	if (sflg)
		umask (077);
	else
#endif
	umask (0);
#ifdef	DBM
	if ((fd = open (dir, O_RDWR|O_CREAT|O_EXCL, 0644)) == -1) {
		fprintf (stderr, CANT_CREATE, Progname, dir);
		exit (1);
	} else
		close (fd);

	if ((fd = open (pag, O_RDWR|O_CREAT|O_EXCL, 0644)) == -1) {
		fprintf (stderr, CANT_CREATE, Progname, pag);
		unlink (dir);
		exit (1);
	} else
		close (fd);
#endif

	/*
	 * Now the DBM database gets initialized
	 */

#ifdef	DBM
	if (dbminit (file) == -1) {
		fprintf (stderr, DBM_OPEN_ERR, Progname, file);
		exit (1);
	}
#endif
#ifdef	NDBM
	if (! (dbm = dbm_open (file, O_RDWR|O_CREAT, 0644))) {
		fprintf (stderr, DBM_OPEN_ERR, Progname, file);
		exit (1);
	}
	if (gflg) {
#ifdef	SHADOWGRP
		if (sflg)
			sg_dbm = dbm;
		else
#endif
			gr_dbm = dbm;
	} else {
#ifdef	SHADOWPWD
		if (sflg)
			sp_dbm = dbm;
		else
#endif
			pw_dbm = dbm;
	}
#endif

	/*
	 * Read every line in the password file and convert it into a
	 * data structure to be put in the DBM database files.
	 */

#ifdef	NDBM
	while (fgetsx (buf, BUFSIZ, fp) != NULL)
#else
	while (fgets (buf, BUFSIZ, fp) != NULL)
#endif
	{

		/*
		 * Get the next line and strip off the trailing newline
		 * character.
		 */

		buf[sizeof buf - 1] = '\0';
		if (! (cp = strchr (buf, '\n'))) {
			fprintf (stderr, LINE_TOO_LONG, Progname, buf);
			exit (1);
		}
		*cp = '\0';
		len = strlen (buf);

#ifdef	USE_NIS
		/*
		 * Parse the password file line into a (struct passwd).
		 * Erroneous lines cause error messages, but that's
		 * all.  YP lines are ignored completely.
		 */

		if (buf[0] == '-' || buf[0] == '+')
			continue;
#endif
#ifdef	DBM
		if (! (passwd = sgetpwent (buf)))
#endif
#ifdef	NDBM
		if (! (((! sflg && pflg) && (passwd = sgetpwent (buf)))
#ifdef	SHADOWPWD
			|| ((sflg && pflg) && (shadow = sgetspent (buf)))
#endif
			|| ((! sflg && gflg) && (group = sgetgrent (buf)))
#ifdef	SHADOWGRP
			|| ((sflg && gflg) && (gshadow = sgetsgent (buf)))
#endif
		))
#endif	/* NDBM */
		{
			fprintf (stderr, PARSE_ERR, Progname, buf);
			errors++;
			continue;
		}
#ifdef	DBM
		if (vflg)
			printf (ADD_REC, passwd->pw_name);

		if (! pw_dbm_update (passwd))
			fprintf (stderr, ADD_REC_ERR,
				Progname, passwd->pw_name);
#endif
#ifdef	NDBM
		if (vflg) {
			if (!sflg && pflg) printf (ADD_REC, passwd->pw_name);
#ifdef	SHADOWPWD
			if (sflg && pflg) printf (ADD_REC, shadow->sp_namp);
#endif
			if (!sflg && gflg) printf (ADD_REC, group->gr_name);
#ifdef	SHADOWGRP
			if (sflg && gflg) printf (ADD_REC, gshadow->sg_name);
#endif
		}
		if (! sflg && pflg && ! pw_dbm_update (passwd))
			fprintf (stderr, ADD_REC_ERR,
				Progname, passwd->pw_name);

#ifdef	SHADOWPWD
		if (sflg && pflg && ! sp_dbm_update (shadow))
			fprintf (stderr, ADD_REC_ERR,
				Progname, shadow->sp_namp);
#endif
		if (! sflg && gflg && ! gr_dbm_update (group))
			fprintf (stderr, ADD_REC_ERR,
				Progname, group->gr_name);
#ifdef	SHADOWGRP
		if (sflg && gflg && ! sg_dbm_update (gshadow))
			fprintf (stderr, ADD_REC_ERR,
				Progname, gshadow->sg_name);
#endif	/* SHADOWGRP */
#endif	/* NDBM */

		/*
		 * Update the longest record and record count
		 */

		if (len > longest)
			longest = len;
		cnt++;
	}

	/*
	 * Tell the user how things went ...
	 */

	if (vflg)
		printf (INFO, cnt, longest);

	exit (errors);
	/*NOTREACHED*/
}

/*
 * usage - print error message and exit
 */

void
usage ()
{
	fprintf (stderr, USAGE, Progname);
	exit (1);
	/*NOTREACHED*/
}
#endif /*} defined(DBM) || defined(NDBM) */
