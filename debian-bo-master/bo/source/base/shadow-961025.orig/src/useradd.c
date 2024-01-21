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
RCSID("$Id: useradd.c,v 1.3 1996/09/25 03:20:05 marekm Exp $")

#include "prototypes.h"
#include "defines.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <fcntl.h>
#include <time.h>

#include "pwauth.h"
#if HAVE_LASTLOG_H
#include <lastlog.h>
#else
#include "lastlog_.h"
#endif
#include "faillog.h"

#ifndef SKEL_DIR
#define SKEL_DIR "/etc/skel"
#endif

#ifndef USER_DEFAULTS_FILE
#define USER_DEFAULTS_FILE "/etc/default/useradd"
#define NEW_USER_FILE "/etc/default/nuaddXXXXXX"
#endif

static gid_t	def_group;
static char *def_gname = "";
static char *def_home = "";
static char *def_shell = "";
static char *def_template = SKEL_DIR;
static long	def_inactive;
static long	def_expire;

static char	def_file[] = USER_DEFAULTS_FILE;

#define	VALID(s)	(strcspn (s, ":\n") == strlen (s))

static char *user_name;
static uid_t	user_id;
static gid_t	user_gid;
static char *user_comment = "";
static char *user_home = "";
static char *user_shell = "";
#ifdef	SHADOWPWD
static long user_expire = -1;
static int is_shadow_pwd;
#endif
#ifdef SHADOWGRP
static int is_shadow_grp;
#endif
static char *user_groups[NGROUPS_MAX+1];  /* NULL-terminated list */
static int do_grp_update = 0;  /* group files need to be updated */

static char	*Prog;

static int
	uflg = 0, /* specify user ID for new account */
	oflg = 0, /* permit non-unique user ID to be specified with -u */
	gflg = 0, /* primary group ID for new account */
	Gflg = 0, /* secondary group set for new account */
	dflg = 0, /* home directory for new account */
	bflg = 0, /* new default root of home directory */
	sflg = 0, /* shell program for new account */
	cflg = 0, /* comment (GECOS) field for new account */
	mflg = 0, /* create user's home directory if it doesn't exist */
	kflg = 0, /* specify a directory to fill new user directory */
	fflg = 0, /* days until account with expired password is locked */
	eflg = 0, /* days after password changed before it becomes expired */
	Dflg = 0; /* set/show new user default values */

#ifdef AUTH_METHODS
static int Aflg = 0; /* specify authentication method for user */
static char user_auth[BUFSIZ];
static char *auth_arg;
#endif

#ifdef NDBM
extern	int	pw_dbm_mode;
#ifdef	SHADOWPWD
extern	int	sp_dbm_mode;
#endif
extern	int	gr_dbm_mode;
#ifdef	SHADOWGRP
extern	int	sg_dbm_mode;
#endif
#endif

static int	home_added;
#if defined(DBM) || defined(NDBM)
static int	pw_dbm_added;
#endif
#ifdef	NDBM
static int	gr_dbm_added;
#ifdef	SHADOWPWD
static int	sp_dbm_added;
#endif
#ifdef	SHADOWGRP
static int	sg_dbm_added;
#endif
#endif	/* NDBM */

static	void	fail ();

#include "groupio.h"

#ifdef	SHADOWGRP
#include "sgroupio.h"
#endif

#include "pwio.h"

#ifdef	SHADOWPWD
#include "shadowio.h"
#endif

#include "getdef.h"

/*
 * get_defaults - read the defaults file
 *
 *	get_defaults() reads the defaults file for this command.  It sets
 *	the various values from the file, or uses built-in default values
 *	if the file does not exist.
 */

static void
get_defaults()
{
	FILE	*fp;
	char	buf[BUFSIZ];
	char	*cp;
	const struct group *grp;

	/*
	 * Open the defaults file for reading.
	 */

	if (! (fp = fopen (def_file, "r"))) {

		/*
		 * No defaults file - set up the defaults that are given
		 * in the documentation.
		 */

		def_group = 1;
		def_gname = "other";
		def_home = "/home";
		def_inactive = -1;
		def_expire = -1;
		return;
	}

	/*
	 * Read the file a line at a time.  Only the lines that have
	 * relevant values are used, everything else can be ignored.
	 */

	while (fgets (buf, sizeof buf, fp)) {
		if ((cp = strrchr (buf, '\n')))
			*cp = '\0';

		if (! (cp = strchr (buf, '=')))
			continue;

		cp++;

		/*
		 * Primary GROUP identifier
		 */

#ifdef	SVR4
		if (strncmp ("defgroup=", buf, 9) == 0)
#else
		if (strncmp ("GROUP=", buf, 6) == 0)
#endif
		{
			if (isdigit (*cp)) {
				def_group = atoi (cp);
				if (! (grp = getgrgid (def_group))) {
					fprintf (stderr, "%s: unknown gid %s\n",
						Prog, cp);
				}
				def_gname = xstrdup(grp->gr_name);
			} else if ((grp = getgrnam (cp))) {
				def_group = grp->gr_gid;
				def_gname = xstrdup(cp);
			} else {
				fprintf (stderr, "%s: unknown group %s\n",
					Prog, cp);
			}
		}
		
		/*
		 * Default HOME filesystem
		 */
		 
#ifdef	SVR4
		else if (strncmp ("defparent=", buf, 10) == 0)
#else
		else if (strncmp ("HOME=", buf, 5) == 0)
#endif
		{
			def_home = xstrdup(cp);
		}

		/*
		 * Default Login Shell command
		 */

#ifdef	SVR4
		else if (strncmp ("defshell=", buf, 9) == 0)
#else
		else if (strncmp ("SHELL=", buf, 6) == 0)
#endif
		{
			def_shell = xstrdup(cp);
		}

#ifdef	SHADOWPWD
		/*
		 * Default Password Inactive value
		 */

#ifdef	SVR4
		else if (strncmp ("definact=", buf, 9) == 0)
#else
		else if (strncmp ("INACTIVE=", buf, 9) == 0)
#endif
		{
			def_inactive = atoi (cp);
		}
#endif
		
		/*
		 * Default Password Expiration value
		 */

#ifdef	SVR4
		else if (strncmp ("defexpire=", buf, 10) == 0)
#else
		else if (strncmp ("EXPIRE=", buf, 7) == 0)
#endif
		{
			if (*cp == '\0')
				def_expire = -1;
			else
				def_expire = atoi (cp);
		}

		/*
		 * Default Skeleton information
		 */

#ifdef	SVR4
		else if (strncmp ("defskel=", buf, 8) == 0)
#else
		else if (strncmp ("SKEL=", buf, 5) == 0)
#endif
		{
			if (*cp == '\0')
				cp = SKEL_DIR;
			
			def_template = xstrdup(cp);
		}
	}
}

/*
 * show_defaults - show the contents of the defaults file
 *
 *	show_defaults() displays the values that are used from the default
 *	file and the built-in values.
 */

static void
show_defaults()
{
#ifdef	SVR4
	struct	tm	*tm;
	time_t	time;

	time = def_expire * (3600L*24L);
	tm = gmtime (&time);

	printf ("group=%s,%d  basedir=%s  skel=%s\n",
		def_gname, def_group, def_home, def_template);

#ifdef	SHADOWPWD
	printf ("shell=%s  inactive=%d  ", def_shell, def_inactive);
#else
	printf ("shell=%s  ", def_shell);
#endif

	if (def_expire >= 0)
		printf ("expire=%d/%d/%d\n",
			tm->tm_mon + 1, tm->tm_mday,
			tm->tm_year >= 100 ? tm->tm_year + 1900:tm->tm_year);
	else
		printf ("expire=\n");
#else
	printf ("GROUP=%d\n", def_group);
	printf ("HOME=%s\n", def_home);
	printf ("INACTIVE=%ld\n", def_inactive);
	printf ("EXPIRE=%ld\n", def_expire);
	printf ("SHELL=%s\n", def_shell);
	printf ("SKEL=%s\n", def_template);
#endif

}

/*
 * set_defaults - write new defaults file
 *
 *	set_defaults() re-writes the defaults file using the values that
 *	are currently set.  Duplicated lines are pruned, missing lines are
 *	added, and unrecognized lines are copied as is.
 */

static int
set_defaults()
{
	FILE	*ifp;
	FILE	*ofp;
	char	buf[BUFSIZ];
	static	char	new_file[] = NEW_USER_FILE;
	char	*cp;
	int	out_group = 0;
	int	out_home = 0;
	int	out_inactive = 0;
	int	out_expire = 0;
	int	out_shell = 0;
	int	out_skel = 0;
#ifdef	SVR4
	int	out_gname = 0;
#endif

	/*
	 * Create a temporary file to copy the new output to.
	 */

	mktemp (new_file);
	if (! (ofp = fopen (new_file, "w"))) {
		fprintf (stderr, "%s: cannot create new defaults file\n", Prog);
		return -1;
	}

	/*
	 * Open the existing defaults file and copy the lines to the
	 * temporary files, using any new values.  Each line is checked
	 * to insure that it is not output more than once.
	 */

	if ((ifp = fopen (def_file, "r"))) {
		while (fgets (buf, sizeof buf, ifp)) {
			if ((cp = strrchr (buf, '\n')))
				*cp = '\0';

#ifdef	SVR4
			if (strncmp ("defgroup=", buf, 9) == 0)
#else
			if (strncmp ("GROUP=", buf, 6) == 0)
#endif
			{
				if (! out_group)
#ifdef	SVR4
					fprintf (ofp, "defgroup=%d\n", def_group);
#else
					fprintf (ofp, "GROUP=%d\n", def_group);
#endif
				out_group++;
			}
#ifdef	SVR4
			else if (strncmp ("defgname=", buf, 9) == 0)
			{
				if (! out_gname)
					fprintf (ofp, "defgname=%s\n", def_gname);
				out_gname++;
			}
#endif
#ifdef	SVR4
			else if (strncmp ("defparent=", buf, 10) == 0)
#else
			else if (strncmp ("HOME=", buf, 5) == 0)
#endif
			{
				if (! out_home)
#ifdef	SVR4
					fprintf (ofp, "defparent=%s\n", def_home);
#else
					fprintf (ofp, "HOME=%s\n", def_home);
#endif
				out_home++;
#ifdef	SHADOWPWD
			}
#ifdef	SVR4
			else if (strncmp ("definact=", buf, 9) == 0)
#else
			else if (strncmp ("INACTIVE=", buf, 9) == 0)
#endif
			{
				if (! out_inactive)
#ifdef	SVR4
					fprintf (ofp, "definact=%d\n",
						def_inactive);
#else
					fprintf (ofp, "INACTIVE=%ld\n",
						def_inactive);
#endif
				out_inactive++;
#endif
			}
#ifdef	SVR4
			else if (strncmp ("defexpire=", buf, 10) == 0)
#else
			else if (strncmp ("EXPIRE=", buf, 7) == 0)
#endif
			{
				if (! out_expire)
#ifdef	SVR4
					if (def_expire >= 0)
						fprintf (ofp, "defexpire=%d\n",
							def_expire);
					else
						fprintf (ofp, "defexpire=\n");
#else
					fprintf (ofp, "EXPIRE=%ld\n",
						def_expire);
#endif
				out_expire++;
			}
#ifdef	SVR4
			else if (strncmp ("defshell=", buf, 9) == 0)
#else
			else if (strncmp ("SHELL=", buf, 6) == 0)
#endif
			{
				if (! out_shell)
#ifdef	SVR4
					fprintf (ofp, "defshell=%s\n",
						def_shell);
#else
					fprintf (ofp, "SHELL=%s\n",
						def_shell);
#endif
				out_shell++;
			}
#ifdef	SVR4
			else if (strncmp ("defskel=", buf, 8) == 0)
#else
			else if (strncmp ("SKEL=", buf, 5) == 0)
#endif
			{
				if (! out_skel)
#ifdef	SVR4
					fprintf (ofp, "defskel=%s\n",
						def_template);
#else
					fprintf (ofp, "SKEL=%s\n",
						def_template);
#endif
				out_skel++;
			}
			else
				fprintf (ofp, "%s\n", buf);
		}
		fclose ((FILE *) ifp);
	}

	/*
	 * Check each line to insure that every line was output.  This
	 * causes new values to be added to a file which did not previously
	 * have an entry for that value.
	 */

	if (! out_group)
#ifdef	SVR4
		fprintf (ofp, "defgroup=%d\n", def_group);
#else
		fprintf (ofp, "GROUP=%d\n", def_group);
#endif
	if (! out_home)
#ifdef	SVR4
		fprintf (ofp, "defparent=%s\n", def_home);
#else
		fprintf (ofp, "HOME=%s\n", def_home);
#endif
#ifdef	SHADOWPWD
	if (! out_inactive)
#ifdef	SVR4
		fprintf (ofp, "definact=%ld\n", def_inactive);
#else
		fprintf (ofp, "INACTIVE=%ld\n", def_inactive);
#endif
#endif
	if (! out_expire)
#ifdef	SVR4
		fprintf (ofp, "defexpire=%ld\n", def_expire);
#else
		fprintf (ofp, "EXPIRE=%ld\n", def_expire);
#endif
	if (! out_shell)
#ifdef	SVR4
		fprintf (ofp, "defshell=%s\n", def_shell);
#else
		fprintf (ofp, "SHELL=%s\n", def_shell);
#endif
	if (! out_skel)
#ifdef	SVR4
		fprintf (ofp, "defskel=%s\n", def_template);
#else
		fprintf (ofp, "SKEL=%s\n", def_template);
#endif

	/*
	 * Flush and close the file.  Check for errors to make certain
	 * the new file is intact.
	 */

	(void) fflush (ofp);
	if (ferror (ofp) || fclose ((FILE *) ofp)) {
		unlink (new_file);
		return -1;
	}

	/*
	 * Rename the current default file to its backup name.
	 */

	sprintf (buf, "%s-", def_file);
	if (rename (def_file, buf) && errno != ENOENT) {
		sprintf (buf, "%s: rename: %s", Prog, def_file);
		perror (buf);
		unlink (new_file);
		return -1;
	}

	/*
	 * Rename the new default file to its correct name.
	 */

	if (rename (new_file, def_file)) {
		sprintf (buf, "%s: rename: %s", Prog, new_file);
		perror (buf);
		return -1;
	}
#ifdef	SHADOWPWD
	SYSLOG((LOG_INFO,
		"defaults: group=%d, home=%s, inactive=%ld, expire=%ld\n",
		def_group, def_home, def_inactive, def_expire));
#else
	SYSLOG((LOG_INFO,
		"defaults: group=%d, home=%s, expire=%ld\n",
		def_group, def_home, def_expire));
#endif
	return 0;
}

/*
 * get_groups - convert a list of group names to an array of group IDs
 *
 *	get_groups() takes a comma-separated list of group names and
 *	converts it to a NULL-terminated array.  Any unknown group
 *	names are reported as errors.
 */

static int
get_groups(list)
	char *list;
{
	char *cp;
	const struct group *grp;
	int errors = 0;
	int ngroups = 0;

	/*
	 * Initialize the list to be empty
	 */

	user_groups[0] = (char *) 0;

	if (! *list)
		return 0;

	/*
	 * So long as there is some data to be converted, strip off
	 * each name and look it up.  A mix of numerical and string
	 * values for group identifiers is permitted.
	 */

	do {
		/*
		 * Strip off a single name from the list
		 */

		if ((cp = strchr (list, ',')))
			*cp++ = '\0';

		/*
		 * Names starting with digits are treated as numerical
		 * GID values, otherwise the string is looked up as is.
		 */

		if (isdigit (*list))
			grp = getgrgid (atoi (list));
		else
			grp = getgrnam (list);

		/*
		 * There must be a match, either by GID value or by
		 * string name.
		 */

		if (! grp) {
			fprintf (stderr, "%s: unknown group %s\n", Prog, list);
			errors++;
		}
		list = cp;

		/*
		 * If the group doesn't exist, don't dump core...
		 * Instead, try the next one.  --marekm
		 */
		if (! grp)
			continue;

#ifdef	USE_NIS
		/*
		 * Don't add this group if they are an NIS group.  Tell
		 * the user to go to the server for this group.
		 */

		if (__isgrNIS ()) {
			fprintf (stderr, "%s: group `%s' is a NIS group.\n",
				Prog, grp->gr_name);
			continue;
		}
#endif

		if (ngroups == NGROUPS_MAX) {
			fprintf(stderr,
				"%s: too many groups specified (max %d).\n",
				Prog, ngroups);
			break;
		}

		/*
		 * Add the group name to the user's list of groups.
		 */

		user_groups[ngroups++] = xstrdup(grp->gr_name);
	} while (list);

	user_groups[ngroups] = (char *) 0;

	/*
	 * Any errors in finding group names are fatal
	 */

	if (errors)
		return -1;

	return 0;
}

/*
 * usage - display usage message and exit
 */

static void
usage()
{
	fprintf (stderr,
		"usage: %s [-u uid [-o]] [-g group] [-G group,...] \n", Prog);
	fprintf (stderr,
		"\t\t[-d home] [-s shell] [-c comment] [-m [-k template]]\n");
	fprintf (stderr, "\t\t");
#ifdef SHADOWPWD
	if (is_shadow_pwd)
		fprintf(stderr, "[-f inactive] [-e expire %s ] ",
			DATE_FORMAT_DESCR);
#endif
#ifdef AUTH_METHODS
	fprintf (stderr, "[ -A program ] ");
#endif
	fprintf (stderr, "name\n");

	fprintf (stderr,
#ifdef	SHADOWPWD
		"\t%s -D [-g group] [-b base] [-f inactive] [-e expire]\n",
#else
		"\t%s -D [-g group] [-b base] [-e expire]\n",
#endif
			Prog);

	exit(2);
}

/*
 * new_pwent - initialize the values in a password file entry
 *
 *	new_pwent() takes all of the values that have been entered and
 *	fills in a (struct passwd) with them.
 */

static void
new_pwent(pwent)
	struct passwd *pwent;
{
	bzero((char *) pwent, sizeof *pwent);
	pwent->pw_name = user_name;

#ifdef SHADOWPWD
	if (is_shadow_pwd) {
		pwent->pw_passwd = "x";
#ifdef ATT_AGE
		pwent->pw_age = "";
#endif
	} else
#endif
	{
#ifdef ATT_AGE
		static char age[3];

		age[0] = i64c (def_expire + 6 / 7);
		age[1] = i64c (0);
		age[2] = '\0';
		pwent->pw_age = age;
#endif

#ifdef AUTH_METHODS
		if (Aflg)
			pwent->pw_passwd = user_auth
		else
#endif
			pwent->pw_passwd = "!";
	}

	pwent->pw_uid = user_id;
	pwent->pw_gid = user_gid;
	pwent->pw_gecos = user_comment;
#ifdef	ATT_COMMENT
	pwent->pw_comment = "";
#endif
#ifdef BSD_QUOTA
	pwent->pw_quota = "";
#endif
	pwent->pw_dir = user_home;
	pwent->pw_shell = user_shell;
}

#ifdef	SHADOWPWD
/*
 * new_spent - initialize the values in a shadow password file entry
 *
 *	new_spent() takes all of the values that have been entered and
 *	fills in a (struct spwd) with them.
 */

static void
new_spent(spent)
	struct spwd *spent;
{
	bzero((char *) spent, sizeof *spent);
	spent->sp_namp = user_name;

#ifdef AUTH_METHODS
	if (Aflg)
		spent->sp_pwdp = user_auth;
	else
#endif
		spent->sp_pwdp = "!";

	spent->sp_lstchg = -1;
	spent->sp_min = getdef_num("PASS_MIN_DAYS", -1);
	if (spent->sp_min > 0)
		spent->sp_min *= DAY/SCALE;
#if 0  /* XXX - what is def_expire: sp_max or sp_expire???  --marekm */
	spent->sp_max = def_expire;
#else
	spent->sp_max = getdef_num("PASS_MAX_DAYS", -1);
#endif
	if (spent->sp_max > 0)
		spent->sp_max *= DAY/SCALE;
	spent->sp_warn = getdef_num("PASS_WARN_AGE", -1);
	if (spent->sp_warn > 0)
		spent->sp_warn *= DAY/SCALE;
	spent->sp_inact = def_inactive;
	if (spent->sp_inact > 0)
		spent->sp_inact *= DAY/SCALE;
	spent->sp_expire = user_expire;
}
#endif

/*
 * grp_update - add user to secondary group set
 *
 *	grp_update() takes the secondary group set given in user_groups
 *	and adds the user to each group given by that set.
 */

static void
grp_update()
{
	const struct group *grp;
	struct group *ngrp;
#ifdef	SHADOWGRP
	const struct sgrp *sgrp;
	struct sgrp *nsgrp;
#endif

	/*
	 * Lock and open the group file.  This will load all of the group
	 * entries.
	 */

	if (! gr_lock ()) {
		fprintf (stderr, "%s: error locking group file\n", Prog);
		exit (1);
	}
	if (! gr_open (O_RDWR)) {
		fprintf (stderr, "%s: error opening group file\n", Prog);
		exit (1);
	}
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_lock ()) {
		fprintf (stderr, "%s: error locking shadow group file\n", Prog);
		exit (1);
	}
	if (is_shadow_grp && ! sgr_open (O_RDWR)) {
		fprintf (stderr, "%s: error opening shadow group file\n", Prog);
		exit (1);
	}
#endif

	/*
	 * Scan through the entire group file looking for the groups that
	 * the user is a member of.
	 */

	for (gr_rewind (), grp = gr_next ();grp;grp = gr_next ()) {

		/*
		 * See if the user specified this group as one of their
		 * concurrent groups.
		 */

		if (!is_on_list(user_groups, grp->gr_name))
			continue;

		/*
		 * Make a copy - gr_update() will free() everything
		 * from the old entry, and we need it later.
		 */

		ngrp = __gr_dup(grp);
		if (!ngrp) {
			exit(13);  /* XXX */
		}

		/* 
		 * Add the username to the list of group members and
		 * update the group entry to reflect the change.
		 */

		ngrp->gr_mem = add_list (ngrp->gr_mem, user_name);
		if (! gr_update (ngrp)) {
			fprintf (stderr, "%s: error adding new group entry\n",
				Prog);
			fail (1);
		}
#ifdef	NDBM
		/*
		 * Update the DBM group file with the new entry as well.
		 */

		if (! gr_dbm_update (ngrp)) {
			fprintf (stderr, "%s: cannot add new dbm group entry\n",
				Prog);
			fail (1);
		} else
			gr_dbm_added++;
#endif
		SYSLOG((LOG_INFO, "add `%s' to group `%s'\n",
			user_name, ngrp->gr_name));
	}
#ifdef NDBM
	endgrent ();
#endif

#ifdef	SHADOWGRP
	if (!is_shadow_grp)
		return;

	/*
	 * Scan through the entire shadow group file looking for the groups
	 * that the user is a member of.  The administrative list isn't
	 * modified.
	 */

	for (sgr_rewind (), sgrp = sgr_next ();sgrp;sgrp = sgr_next ()) {

		/*
		 * See if the user specified this group as one of their
		 * concurrent groups.
		 */

		if (!gr_locate(sgrp->sg_name))
			continue;

		if (!is_on_list(user_groups, sgrp->sg_name))
			continue;

		/*
		 * Make a copy - sgr_update() will free() everything
		 * from the old entry, and we need it later.
		 */

		nsgrp = __sgr_dup(sgrp);
		if (!nsgrp) {
			exit(13);  /* XXX */
		}

		/* 
		 * Add the username to the list of group members and
		 * update the group entry to reflect the change.
		 */

		nsgrp->sg_mem = add_list (nsgrp->sg_mem, user_name);
		if (! sgr_update (nsgrp)) {
			fprintf (stderr, "%s: error adding new group entry\n",
				Prog);
			fail (1);
		}
#ifdef	NDBM
		/*
		 * Update the DBM group file with the new entry as well.
		 */

		if (! sg_dbm_update (nsgrp)) {
			fprintf (stderr, "%s: cannot add new dbm group entry\n",
				Prog);
			fail (1);
		} else
			sg_dbm_added++;
#endif	/* NDBM */
		SYSLOG((LOG_INFO, "add `%s' to shadow group `%s'\n",
			user_name, nsgrp->sg_name));
	}
#ifdef NDBM
	endsgent ();
#endif	/* NDBM */
#endif	/* SHADOWGRP */
}

/*
 * find_new_uid - find the next available UID
 *
 *	find_new_uid() locates the next highest unused UID in the password
 *	file, or checks the given user ID against the existing ones for
 *	uniqueness.
 */

static void
find_new_uid()
{
	const struct passwd *pwd;
	uid_t uid_min, uid_max;

	uid_min = getdef_num("UID_MIN", 100);
	uid_max = getdef_num("UID_MAX", 60000);

	/*
	 * Start with some UID value if the user didn't provide us with
	 * one already.
	 */

	if (! uflg)
		user_id = uid_min;

	/*
	 * Search the entire password file, either looking for this
	 * UID (if the user specified one with -u) or looking for the
	 * largest unused value.
	 */

#ifdef NO_GETPWENT
	pw_rewind();
	while ((pwd = pw_next())) {
#else  /* using getpwent() we can check against NIS users etc.  */
	setpwent();
	while ((pwd = getpwent())) {
#endif
		if (strcmp (user_name, pwd->pw_name) == 0) {
			fprintf (stderr, "%s: name %s is not unique\n",
				Prog, user_name);
			exit(9);
		}
		if (uflg && user_id == pwd->pw_uid) {
			fprintf (stderr, "%s: uid %d is not unique\n",
				Prog, user_id);
			exit(4);
		}
		if (! uflg && pwd->pw_uid >= user_id) {
			if (pwd->pw_uid > uid_max)
				continue;
			user_id = pwd->pw_uid + 1;
		}
	}
	/*
	 * If a user with uid equal to UID_MAX exists, the above algorithm
	 * will give us UID_MAX+1 even if not unique.  Search for the first
	 * free uid starting with UID_MIN (it's O(n*n) but can be avoided
	 * by not having users with uid equal to UID_MAX).  --marekm
	 */
	if (!uflg && user_id == uid_max + 1) {
		for (user_id = uid_min; user_id < uid_max; user_id++) {
#ifdef NO_GETPWENT
			pw_rewind();
			while ((pwd = pw_next()) && pwd->pw_uid != user_id)
				;
			if (!pwd)
				break;
#else
			if (!getpwuid(user_id))
				break;
#endif
		}
		if (user_id == uid_max) {
			fprintf(stderr, "%s: can't get unique uid\n",
				Prog);
			exit(1);
		}
	}
}

#ifdef AUTH_METHODS
/*
 * convert_auth - convert the argument list to a authentication list
 */

static void
convert_auth(auths, list)
	char *auths;
	const char *list;
{
	char	*cp, *end;
	char	buf[257];

	/*
	 * Copy each method.  DEFAULT is replaced by an encrypted string
	 * if one can be found in the current authentication list.
	 */

	strcpy (buf, list);
	auths[0] = '\0';
	for (cp = buf;cp;cp = end) {
		if (auths[0])
			strcat (auths, ";");

		if ((end = strchr (cp, ',')))
			*end++ = '\0';

		if (strcmp (cp, "DEFAULT") == 0) {
			strcat (auths, "!");
		} else {
			strcat (auths, "@");
			strcat (auths, cp);
		}
	}
}

/*
 * valid_auth - check authentication list for validity
 */

static int
valid_auth(methods)
	const char *methods;
{
	char	*cp, *end;
	char	buf[257];
	int	default_cnt = 0;

	/*
	 * Cursory checks, length and illegal characters
	 */

	if ((int) strlen (methods) > 256)
		return 0;

	if (! VALID (methods))
		return 0;

	/*
	 * Pick each method apart and check it.
	 */

	strcpy (buf, methods);
	for (cp = buf;cp;cp = end) {
		if ((end = strchr (cp, ',')))
			*end++ = '\0';

		if (strcmp (cp, "DEFAULT") == 0) {
			if (default_cnt++ > 0)
				return 0;
		}
	}
	return 1;
}
#endif  /* AUTH_METHODS */

/*
 * process_flags - perform command line argument setting
 *
 *	process_flags() interprets the command line arguments and sets
 *	the values that the user will be created with accordingly.  The
 *	values are checked for sanity.
 */

static void
process_flags(argc, argv)
	int argc;
	char **argv;
{
	extern	int	optind;
	extern	char	*optarg;
	const struct group *grp;
	int	anyflag = 0;
	int	arg;

#ifdef SHADOWPWD
#define FLAGS "A:Du:og:G:d:s:c:mk:f:e:b:"
#else
#define FLAGS "A:Du:og:G:d:s:c:mk:e:b:"
#endif
	while ((arg = getopt(argc, argv, FLAGS)) != EOF) {
#undef FLAGS
		switch (arg) {
#ifdef AUTH_METHODS
			case 'A':
				if (! valid_auth (optarg)) {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				auth_arg = optarg;
				Aflg++;
				break;
#endif
			case 'b':
				if (! VALID (optarg) || optarg[0] != '/') {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				bflg++;
				if (! Dflg)
					usage ();

				def_home = optarg;
				break;
			case 'c':
				if (! VALID (optarg)) {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				cflg++;
				user_comment = optarg;
				break;
			case 'd':
				if (! VALID (optarg) || optarg[0] != '/') {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				dflg++;
				user_home = optarg;
				break;
			case 'D':
				if (anyflag)
					usage ();

				Dflg++;
				break;
			case 'e':
				eflg++;
				if (Dflg) {
					def_expire = atoi (optarg);
#ifdef	SHADOWPWD
				} else if (is_shadow_pwd) {
					user_expire = strtoday (optarg);
					user_expire *= (DAY/SCALE);
#endif
				} else
					usage();
				break;
#ifdef	SHADOWPWD
			case 'f':
				if (!is_shadow_pwd)
					usage();
				fflg++;
				def_inactive = atoi (optarg);
				break;
#endif
			case 'g':
				gflg++;
				if (isdigit (optarg[0]))
					grp = getgrgid (atoi (optarg));
				else
					grp = getgrnam (optarg);

				if (! grp) {
					fprintf (stderr,
						"%s: unknown group %s\n",
						Prog, optarg);
					exit(6);
				}
				if (Dflg)
					def_group = grp->gr_gid;
				else
					user_gid = grp->gr_gid;
				break;
			case 'G':
				Gflg++;
				if (get_groups(optarg))
					exit(6);
				if (user_groups[0])
					do_grp_update++;
				break;
			case 'k':
				if (! mflg)
					usage ();

				def_template = optarg;
				kflg++;
				break;
			case 'm':
				mflg++;
				break;
			case 'o':
				if (! uflg)
					usage ();

				oflg++;
				break;
			case 's':
				if (! VALID (optarg) || (optarg[0] &&
				    (optarg[0] != '/' && optarg[0] != '*'))) {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				sflg++;
				user_shell = optarg;
				def_shell = optarg;
				break;
			case 'u':
				uflg++;
				user_id = atoi (optarg);
				break;
			default:
				usage ();
		}
		anyflag++;
	}

	/*
	 * Get the user name if there is one.
	 */

	if (Dflg) {
		if (optind != argc)
			usage();

		if (uflg || oflg || Gflg || dflg || cflg || mflg)
			usage();
	} else {
		if (optind != argc - 1)
			usage();

		user_name = argv[optind];
		if (!check_name(user_name)) {
			fprintf(stderr, "%s: invalid user name `%s'\n",
				Prog, user_name);
			exit(3);
		}
	}
	if (! dflg) {
		user_home = xmalloc(strlen(def_home) + strlen(user_name) + 2);
		sprintf(user_home, "%s/%s", def_home, user_name);
	}

	if (! gflg)
		user_gid = def_group;

	if (! sflg)
		user_shell = def_shell;
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
	if (! pw_close ()) {
		fprintf (stderr, "%s: cannot rewrite password file\n", Prog);
		fail (1);
	}
#ifdef	SHADOWPWD
	if (is_shadow_pwd && ! spw_close ()) {
		fprintf (stderr, "%s: cannot rewrite shadow password file\n",	
			Prog);
		fail (1);
	}
#endif
	if (do_grp_update) {
		if (! gr_close ()) {
			fprintf (stderr, "%s: cannot rewrite group file\n",
				Prog);
			fail (1);
		}
		(void) gr_unlock ();
#ifdef	SHADOWGRP
		if (is_shadow_grp && ! sgr_close ()) {
			fprintf (stderr, "%s: cannot rewrite shadow group file\n",
				Prog);
			fail (1);
		}
		if (is_shadow_grp)
			sgr_unlock ();
#endif
	}
#ifdef	SHADOWPWD
	if (is_shadow_pwd)
		spw_unlock ();
#endif
	(void) pw_unlock ();
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
		exit (1);
	}
	if (! pw_open (O_RDWR)) {
		fprintf (stderr, "%s: unable to open password file\n", Prog);
		exit (1);
	}
#ifdef	SHADOWPWD
	if (is_shadow_pwd && ! spw_lock ()) {
		fprintf (stderr, "%s: cannot lock shadow password file\n", Prog);
		exit (1);
	}
	if (is_shadow_pwd && ! spw_open (O_RDWR)) {
		fprintf (stderr, "%s: cannot open shadow password file\n", Prog);
		exit (1);
	}
#endif
}


static void
faillog_reset(uid)
	uid_t uid;
{
	struct faillog fl;
	int fd;

	fd = open(FAILFILE, O_RDWR);
	if (fd >= 0) {
		bzero((char *) &fl, sizeof(fl));
		lseek(fd, (off_t) sizeof(fl) * uid, SEEK_SET);
		write(fd, &fl, sizeof(fl));
		close(fd);
	}
}

static void
lastlog_reset(uid)
	uid_t uid;
{
	struct lastlog ll;
	int fd;

	fd = open(LASTLOG_FILE, O_RDWR);
	if (fd >= 0) {
		bzero((char *) &ll, sizeof(ll));
		lseek(fd, (off_t) sizeof(ll) * uid, SEEK_SET);
		write(fd, &ll, sizeof(ll));
		close(fd);
	}
}

/*
 * usr_update - create the user entries
 *
 *	usr_update() creates the password file entries for this user
 *	and will update the group entries if required.
 */

static void
usr_update()
{
	struct	passwd	pwent;
#ifdef	SHADOWPWD
	struct	spwd	spent;
#endif

	if (! oflg)
		find_new_uid ();

#ifdef AUTH_METHODS
	if (Aflg)
		convert_auth (user_auth, auth_arg);
#endif

	/*
	 * Fill in the password structure with any new fields, making
	 * copies of strings.
	 */

	new_pwent (&pwent);
#ifdef	SHADOWPWD
	new_spent (&spent);
#endif

	/*
	 * Create a syslog entry.  We need to do this now in case anything
	 * happens so we know what we were trying to accomplish.
	 */

#ifdef AUTH_METHODS
	SYSLOG((LOG_INFO,
	    "new user: name=%s, uid=%d, gid=%d, home=%s, shell=%s, auth=%s\n",
		user_name, user_id, user_gid, user_home, user_shell,
		Aflg ? auth_arg : "DEFAULT"));
#else
	SYSLOG((LOG_INFO,
		"new user: name=%s, uid=%d, gid=%d, home=%s, shell=%s\n",
		user_name, user_id, user_gid, user_home, user_shell));
#endif

#ifdef AUTH_METHODS
	/*
	 * Attempt to add the new user to any authentication programs
	 * which have been requested.  Since this is more likely to fail
	 * than the update of the password file, we do this first.
	 */

	if (Aflg && pw_auth (user_auth, pwent.pw_name, PW_ADD, (char *) 0)) {
		fprintf (stderr, "%s: error adding authentication method\n",
			Prog);
		fail (1);
	}
#endif  /* AUTH_METHODS */

	/*
	 * Initialize faillog and lastlog entries for this UID in case
	 * it belongs to a previously deleted user.  We do it only if
	 * no user with this UID exists yet (entries for shared UIDs
	 * are left unchanged).  --marekm
	 */

	if (!getpwuid(user_id)) {
		faillog_reset(user_id);
		lastlog_reset(user_id);
	}

	/*
	 * Put the new (struct passwd) in the table.
	 */

	if (! pw_update (&pwent)) {
		fprintf (stderr, "%s: error adding new password entry\n", Prog);
		exit (1);
	}
#if defined(DBM) || defined(NDBM)

	/*
	 * Update the DBM files.  This creates the user before the flat
	 * files are updated.  This is safe before the password field is
	 * either locked, or set to a valid authentication string.
	 */

	if (pw_dbm_present()) {
		if (! pw_dbm_update (&pwent)) {
			fprintf (stderr,
				"%s: error updating password dbm entry\n",
				Prog);
			exit (1);
		} else
			pw_dbm_added = 1;
	}
	endpwent ();
#endif
#ifdef	SHADOWPWD

	/*
	 * Put the new (struct spwd) in the table.
	 */

	if (is_shadow_pwd && ! spw_update (&spent)) {
		fprintf (stderr, "%s: error adding new shadow password entry\n",
			Prog);
		exit (1);
	}
#ifdef	NDBM

	/* 
	 * Update the DBM files for the shadow password.  This entry is
	 * output before the entry in the flat file, but this is safe as
	 * the password is locked or the authentication string has the
	 * proper values.
	 */

	if (is_shadow_pwd && sp_dbm_present()) {
		if (! sp_dbm_update (&spent)) {
			fprintf (stderr,
				"%s: error updating shadow passwd dbm entry\n",
				Prog);
			fail (1);
		} else
			sp_dbm_added++;
		endspent ();
	}
#endif
#endif	/* SHADOWPWD */

	/*
	 * Do any group file updates for this user.
	 */

	if (do_grp_update)
		grp_update ();
}

/*
 * create_home - create the user's home directory
 *
 *	create_home() creates the user's home directory if it does not
 *	already exist.  It will be created mode 755 owned by the user
 *	with the user's default group.
 */

static void
create_home()
{
	if (access (user_home, 0)) {
		/* XXX - create missing parent directories.  --marekm */
		if (mkdir (user_home, 0)) {
			fprintf(stderr, "%s: cannot create directory %s\n",
				Prog, user_home);
			fail(12);
		}
		chown (user_home, user_id, user_gid);
#if 1
		chmod(user_home, 0777 & ~getdef_num("UMASK", 077));
#else
		chmod (user_home, 0755);
#endif
		home_added++;
	}
}

/*
 * fail - undo as much as possible
 */

static void
fail(code)
	int code;
{
#if defined(DBM) || defined(NDBM)
	struct	passwd	pwent;

	if (pw_dbm_added) {
		pwent.pw_name = user_name;
		pwent.pw_uid = user_id;
		(void) pw_dbm_remove (&pwent);
	}
#endif
#ifdef	NDBM
	if (gr_dbm_added)
		fprintf (stderr, "%s: rebuild the group database\n", Prog);
#ifdef	SHADOWPWD
	if (sp_dbm_added)
		(void) sp_dbm_remove (user_name);
#endif
#ifdef	SHADOWGRP
	if (sg_dbm_added)
		fprintf (stderr, "%s: rebuild the shadow group database\n",
			Prog);
#endif
#endif	/* NDBM */
	if (home_added)
		rmdir (user_home);

	SYSLOG((LOG_INFO, "failed adding user `%s', data deleted\n",
		user_name));
	exit (code);
}

/*
 * main - useradd command
 */

int
main(argc, argv)
	int argc;
	char **argv;
{
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
	 * The open routines for the NDBM files don't use read-write
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
	get_defaults();

	process_flags(argc, argv);

	/*
	 * See if we are messing with the defaults file, or creating
	 * a new user.
	 */

	if (Dflg) {
		if (gflg || bflg || fflg || eflg || sflg)
			exit (set_defaults () ? 1:0);

		show_defaults ();
		exit (0);
	}

	/*
	 * Start with a quick check to see if the user exists.
	 */

	if (getpwnam(user_name)) {
		fprintf(stderr, "%s: user %s exists\n", Prog, user_name);
		exit(9);
	}

	/*
	 * Do the hard stuff - open the files, create the user entries,
	 * create the home directory, then close and update the files.
	 */

	open_files ();

	usr_update ();

	if (mflg) {
		create_home ();
		copy_tree (def_template, user_home, user_id, user_gid);
	}
	close_files ();
	exit (0);
	/*NOTREACHED*/
}
