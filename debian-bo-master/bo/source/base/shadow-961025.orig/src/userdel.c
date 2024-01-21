/*
 * Copyright 1991 - 1994, John F. Haugh II
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
RCSID("$Id: userdel.c,v 1.3 1996/09/25 03:20:05 marekm Exp $")

#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <fcntl.h>
#include <utmp.h>

#include "prototypes.h"
#include "defines.h"
#include "getdef.h"
#include "pwauth.h"

static char	user_name[BUFSIZ];
static uid_t	user_id;
static char	user_home[BUFSIZ];

static char	*Prog;
static int fflg = 0, rflg = 0;

#ifdef	NDBM
extern	int	pw_dbm_mode;
#ifdef	SHADOWPWD
extern	int	sp_dbm_mode;
#endif
extern	int	gr_dbm_mode;
#ifdef	SHADOWGRP
extern	int	sg_dbm_mode;
#endif
#endif

#include "groupio.h"
#include "pwio.h"

#ifdef	SHADOWPWD
#include "shadowio.h"
#endif

#ifdef	SHADOWGRP
#include "sgroupio.h"
#endif

#ifdef SHADOWPWD
static int is_shadow_pwd;
#endif
#ifdef SHADOWGRP
static int is_shadow_grp;
#endif

/*
 * usage - display usage message and exit
 */

static void
usage()
{
	fprintf(stderr, "usage: %s [-r] name\n", Prog);
	exit(2);
}

/*
 * update_groups - delete user from secondary group set
 *
 *	update_groups() takes the user name that was given and searches
 *	the group files for membership in any group.
 */

static void
update_groups()
{
	const struct group *grp;
	struct group *ngrp;
#ifdef	SHADOWGRP
	const struct sgrp *sgrp;
	struct sgrp *nsgrp;
#endif	/* SHADOWGRP */

	/*
	 * Scan through the entire group file looking for the groups that
	 * the user is a member of.
	 */

	for (gr_rewind (), grp = gr_next ();grp;grp = gr_next ()) {

		/*
		 * See if the user specified this group as one of their
		 * concurrent groups.
		 */

		if (!is_on_list(grp->gr_mem, user_name))
			continue;

		/* 
		 * Delete the username from the list of group members and
		 * update the group entry to reflect the change.
		 */

		ngrp = __gr_dup(grp);
		if (!ngrp) {
			exit(13);  /* XXX */
		}
		ngrp->gr_mem = del_list (ngrp->gr_mem, user_name);
		if (! gr_update (ngrp))
			fprintf (stderr, "%s: error updating group entry\n",
				Prog);

		/*
		 * Update the DBM group file with the new entry as well.
		 */

#ifdef	NDBM
		if (! gr_dbm_update (ngrp))
			fprintf (stderr, "%s: cannot update dbm group entry\n",
				Prog);
#endif	/* NDBM */
		SYSLOG((LOG_INFO, "delete `%s' from group `%s'\n",
			user_name, ngrp->gr_name));
	}
#ifdef	NDBM
	endgrent ();
#endif	/* NDBM */
#ifdef	SHADOWGRP
	if (!is_shadow_grp)
		return;

	/*
	 * Scan through the entire shadow group file looking for the groups
	 * that the user is a member of.  Both the administrative list and
	 * the ordinary membership list is checked.
	 */

	for (sgr_rewind (), sgrp = sgr_next ();sgrp;sgrp = sgr_next ()) {
		int was_member, was_admin;

		/*
		 * See if the user specified this group as one of their
		 * concurrent groups.
		 */

		was_member = is_on_list(sgrp->sg_mem, user_name);
		was_admin = is_on_list(sgrp->sg_adm, user_name);

		if (!was_member && !was_admin)
			continue;

		nsgrp = __sgr_dup(sgrp);
		if (!nsgrp) {
			exit(13);  /* XXX */
		}

		if (was_member)
			nsgrp->sg_mem = del_list (nsgrp->sg_mem, user_name);

		if (was_admin)
			nsgrp->sg_adm = del_list (nsgrp->sg_adm, user_name);

		if (! sgr_update (nsgrp))
			fprintf (stderr, "%s: error updating group entry\n",
				Prog);
#ifdef	NDBM
		/*
		 * Update the DBM group file with the new entry as well.
		 */

		if (! sg_dbm_update (nsgrp))
			fprintf (stderr, "%s: cannot update dbm group entry\n",
				Prog);
#endif	/* NDBM */
		SYSLOG((LOG_INFO, "delete `%s' from shadow group `%s'\n",
			user_name, nsgrp->sg_name));
	}
#ifdef	NDBM
	endsgent ();
#endif	/* NDBM */
#endif	/* SHADOWGRP */
}

/*
 * close_files - close all of the files that were opened
 *
 *	close_files() closes all of the files that were opened for this
 *	new user.  This causes any modified entries to be written out.
 */

static void
close_files()
{
	if (! pw_close ())
		fprintf (stderr, "%s: cannot rewrite password file\n", Prog);
#ifdef	SHADOWPWD
	if (is_shadow_pwd && ! spw_close ())
		fprintf (stderr, "%s: cannot rewrite shadow password file\n",	
			Prog);
#endif
	if (! gr_close ())
		fprintf (stderr, "%s: cannot rewrite group file\n",
			Prog);

	(void) gr_unlock ();
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_close ())
		fprintf (stderr, "%s: cannot rewrite shadow group file\n",
			Prog);

	if (is_shadow_grp)
		(void) sgr_unlock ();
#endif
#ifdef	SHADOWPWD
	if (is_shadow_pwd)
		(void) spw_unlock ();
#endif
	(void) pw_unlock ();
}

/*
 * fail_exit - exit with a failure code after unlocking the files
 */

static void
fail_exit(code)
	int code;
{
	(void) pw_unlock ();
	(void) gr_unlock ();
#ifdef	SHADOWPWD
	if (is_shadow_pwd)
		spw_unlock ();
#endif
#ifdef	SHADOWGRP
	if (is_shadow_grp)
		sgr_unlock ();
#endif
	exit(code);
}

/*
 * open_files - lock and open the password files
 *
 *	open_files() opens the two password files.
 */

static void
open_files()
{
	if (! pw_lock ()) {
		fprintf (stderr, "%s: unable to lock password file\n", Prog);
		exit (10);
	}
	if (! pw_open (O_RDWR)) {
		fprintf (stderr, "%s: unable to open password file\n", Prog);
		fail_exit (10);
	}
#ifdef	SHADOWPWD
	if (is_shadow_pwd && ! spw_lock ()) {
		fprintf (stderr, "%s: cannot lock shadow password file\n", Prog);
		fail_exit (10);
	}
	if (is_shadow_pwd && ! spw_open (O_RDWR)) {
		fprintf (stderr, "%s: cannot open shadow password file\n", Prog);
		fail_exit (10);
	}
#endif
	if (! gr_lock ()) {
		fprintf (stderr, "%s: unable to lock group file\n", Prog);
		fail_exit (10);
	}
	if (! gr_open (O_RDWR)) {
		fprintf (stderr, "%s: cannot open group file\n", Prog);
		fail_exit (10);
	}
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_lock ()) {
		fprintf (stderr, "%s: unable to lock shadow group file\n", Prog);
		fail_exit (10);
	}
	if (is_shadow_grp && ! sgr_open (O_RDWR)) {
		fprintf (stderr, "%s: cannot open shadow group file\n", Prog);
		fail_exit (10);
	}
#endif
}

/*
 * update_user - delete the user entries
 *
 *	update_user() deletes the password file entries for this user
 *	and will update the group entries as required.
 */

static void
update_user()
{
#if defined(AUTH_METHODS) || defined(DBM) || defined(NDBM)
	struct	passwd	*pwd;
#endif
#ifdef AUTH_METHODS
#ifdef	SHADOWPWD
	struct	spwd	*spwd;

	if (is_shadow_pwd && (spwd = spw_locate (user_name)) &&
	    spwd->sp_pwdp[0] == '@') {
		if (pw_auth (spwd->sp_pwdp + 1, user_name, PW_DELETE, (char *) 0)) {
			SYSLOG((LOG_ERR,
				"failed deleting auth `%s' for user `%s'\n",
				spwd->sp_pwdp + 1, user_name));
			fprintf(stderr, "%s: error deleting authentication\n",
				Prog);
		} else {
			SYSLOG((LOG_INFO,
				"delete auth `%s' for user `%s'\n",
				spwd->sp_pwdp + 1, user_name));
		}
	}
#endif	/* SHADOWPWD */
	if ((pwd = pw_locate (user_name)) && pwd->pw_passwd[0] == '@') {
		if (pw_auth (pwd->pw_passwd + 1, user_name, PW_DELETE, (char *) 0)) {
			SYSLOG((LOG_ERR,
				"failed deleting auth `%s' for user `%s'\n",
				pwd->pw_passwd + 1, user_name));
			fprintf(stderr, "%s: error deleting authentication\n",
				Prog);
		} else {
			SYSLOG((LOG_INFO, "delete auth `%s' for user `%s'\n",
				pwd->pw_passwd + 1, user_name);
		}
	}
#endif  /* AUTH_METHODS */
	if (! pw_remove (user_name))
		fprintf (stderr, "%s: error deleting password entry\n", Prog);
#ifdef	SHADOWPWD
	if (is_shadow_pwd && ! spw_remove (user_name))
		fprintf (stderr, "%s: error deleting shadow password entry\n",
			Prog);
#endif
#if defined(DBM) || defined(NDBM)
	if (pw_dbm_present()) {
		if ((pwd = getpwnam (user_name)) && ! pw_dbm_remove (pwd))
			fprintf (stderr,
				"%s: error deleting password dbm entry\n",
				Prog);
	}

	/*
	 * If the user's UID is a duplicate the duplicated entry needs
	 * to be updated so that a UID match can be found in the DBM
	 * files.
	 */

	for (pw_rewind (), pwd = pw_next ();pwd;pwd = pw_next ()) {
		if (pwd->pw_uid == user_id) {
			pw_dbm_update (pwd);
			break;
		}
	}
#endif
#if defined(NDBM) && defined(SHADOWPWD)
	if (is_shadow_pwd && sp_dbm_present() && !sp_dbm_remove(user_name))
		fprintf (stderr, "%s: error deleting shadow passwd dbm entry\n",
			Prog);

	endspent ();
#endif
#if defined(DBM) || defined(NDBM)
	endpwent ();
#endif
	SYSLOG((LOG_INFO, "delete user `%s'\n", user_name));
}

/*
 * user_busy - see if user is logged in.
 */

static void
user_busy(name, uid)
	const char *name;
	uid_t uid;
{
	struct	utmp	*utent;

	/*
	 * We see if the user is logged in by looking for the user name
	 * in the utmp file.
	 */

	setutent ();

	while ((utent = getutent ())) {
#ifdef USER_PROCESS
		if (utent->ut_type != USER_PROCESS)
			continue;
#else
		if (utent->UT_USER[0] == '\0')
			continue;
#endif
		if (strncmp (utent->UT_USER, name, sizeof utent->UT_USER))
			continue;

		fprintf(stderr, "%s: user %s is currently logged in\n",
			Prog, name);
		exit(8);
	}
}

/* 
 * user_cancel - cancel cron and at jobs
 *
 *	user_cancel removes the crontab and any at jobs for a user
 */

static void
user_cancel(user)
	const char *user;
{
	char	buf[BUFSIZ];

#ifdef CRONTAB_COMMAND
	/* 
	 * Remove the crontab if there is one.
	 */

#ifdef CRONTAB_FILE
	sprintf (buf, CRONTAB_FILE, user);
	if (access (buf, 0) == 0)
#endif
	{
		sprintf (buf, CRONTAB_COMMAND, user);
		system (buf);
	}
#endif

#ifdef ATRM_COMMAND
	/*
	 * Remove any at jobs as well.
	 */

	sprintf (buf, ATRM_COMMAND, user);
	system (buf);
#endif
}

#ifdef EXTRA_CHECK_HOME_DIR
static int
path_prefix(s1, s2)
	const char *s1;
	const char *s2;
{
	return (strncmp(s2, s1, strlen(s1)) == 0);
}
#endif

static int
is_owner(uid, path)
	uid_t uid;
	const char *path;
{
	struct stat st;

	if (stat(path, &st))
		return -1;
	return (st.st_uid == uid);
}

#ifndef NO_REMOVE_MAILBOX
static void
remove_mailbox()
{
	char *maildir;
	char mailfile[1024];
	int i;

	maildir = getdef_str("MAIL_DIR");
#ifdef MAIL_SPOOL_DIR
	if (!maildir && !getdef_str("MAIL_FILE"))
		maildir = MAIL_SPOOL_DIR;
#endif
	if (!maildir)
		return;

	sprintf(mailfile, "%s/%s", maildir, user_name);
	if (fflg) {
		unlink(mailfile);  /* always remove, ignore errors */
		return;
	}
	i = is_owner(user_id, mailfile);
	if (i == 0) {
		fprintf(stderr,
			"%s: warning: %s not owned by %s, not removing\n",
			Prog, mailfile, user_name);
		return;
	} else if (i == -1)
		return;  /* mailbox doesn't exist */
	if (unlink(mailfile)) {
		fprintf(stderr, "%s: warning: can't remove ", Prog);
		perror(mailfile);
	}
}
#endif

/*
 * main - userdel command
 */

int
main(argc, argv)
	int argc;
	char **argv;
{
	struct	passwd	*pwd;
	int	arg;
	int	errors = 0;
	extern	int	optind;

	/*
	 * Get my name so that I can use it to report errors.
	 */

	Prog = Basename(argv[0]);

	openlog(Prog, LOG_PID|LOG_CONS|LOG_NOWAIT, LOG_AUTH);

#ifdef SHADOWPWD
	is_shadow_pwd = (access(SHADOW_FILE, 0) == 0);
#endif

#ifdef SHADOWGRP
	is_shadow_grp = (access(SGROUP_FILE, 0) == 0);
#endif

	/*
	 * The open routines for the DBM files don't use read-write
	 * as the mode, so we have to clue them in.
	 */

#ifdef	NDBM
	pw_dbm_mode = O_RDWR;
#ifdef	SHADOWPWD
	sp_dbm_mode = O_RDWR;
#endif
	gr_dbm_mode = O_RDWR;
#ifdef	SHADOWGRP
	sg_dbm_mode = O_RDWR;
#endif
#endif
	while ((arg = getopt (argc, argv, "fr")) != EOF) {
		switch (arg) {
		case 'f':  /* force remove even if not owned by user */
			fflg++;
			break;
		case 'r':  /* remove home dir and mailbox */
			rflg++;
			break;
		default:
			usage();
		}
	}
	
	if (optind == argc)
		usage ();

	/*
	 * Start with a quick check to see if the user exists.
	 */

	strncpy (user_name, argv[argc - 1], BUFSIZ);

	if (! (pwd = getpwnam (user_name))) {
		fprintf(stderr, "%s: user %s does not exist\n",
			Prog, user_name);
		exit(6);
	}
#ifdef	USE_NIS

	/*
	 * Now make sure it isn't an NIS user.
	 */

	if (__ispwNIS ()) {
		char	*nis_domain;
		char	*nis_master;

		fprintf (stderr, "%s: user %s is a NIS user\n",
			Prog, user_name);

		if (! yp_get_default_domain (&nis_domain) &&
				! yp_master (nis_domain, "passwd.byname",
				&nis_master)) {
			fprintf (stderr, "%s: %s is the NIS master\n",
				Prog, nis_master);
		}
		exit(6);
	}
#endif
	user_id = pwd->pw_uid;
	strcpy (user_home, pwd->pw_dir);

	/*
	 * Check to make certain the user isn't logged in.
	 */

	user_busy (user_name, user_id);

	/*
	 * Do the hard stuff - open the files, create the user entries,
	 * create the home directory, then close and update the files.
	 */

	open_files ();

	update_user ();
	update_groups ();

#ifndef NO_REMOVE_MAILBOX
	if (rflg)
		remove_mailbox();
#endif

	if (rflg && !fflg && !is_owner(user_id, user_home)) {
		fprintf(stderr, "%s: %s not owned by %s, not removing\n",
			Prog, user_home, user_name);
		rflg = 0;
		errors++;
	}

/* This may be slow, the above should be good enough.  */
#ifdef EXTRA_CHECK_HOME_DIR
	if (rflg && !fflg) {
		/*
		 * For safety, refuse to remove the home directory
		 * if it would result in removing some other user's
		 * home directory.  Still not perfect so be careful,
		 * but should prevent accidents if someone has /home
		 * or / as home directory...  --marekm
		 */
		setpwent();
		while ((pwd = getpwent())) {
			if (strcmp(pwd->pw_name, user_name) == 0)
				continue;

			if (path_prefix(user_home, pwd->pw_dir)) {
				fprintf(stderr,
	"%s: not removing directory %s (would remove home of user %s)\n",
					Prog, user_home, pwd->pw_name);

				rflg = 0;
				errors++;
				break;
			}
		}
	}
#endif

	if (rflg) {
		if (remove_tree(user_home) || rmdir(user_home)) {
			fprintf(stderr, "%s: error removing directory %s\n",
				Prog, user_home);

			errors++;
		}
	}

	/*
	 * Cancel any crontabs or at jobs.  Have to do this before we
	 * remove the entry from /etc/passwd.
	 */

	user_cancel(user_name);

	close_files ();

	exit(errors ? 12:0);
	/*NOTREACHED*/
}
