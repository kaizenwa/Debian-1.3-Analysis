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
RCSID("$Id: usermod.c,v 1.4 1996/09/25 03:20:06 marekm Exp $")

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <fcntl.h>
#include <time.h>

#include "prototypes.h"
#include "defines.h"
#include "faillog.h"
#if HAVE_LASTLOG_H
#include <lastlog.h>
#else
#include "lastlog_.h"
#endif
#include "pwauth.h"
#include "getdef.h"

#define	VALID(s)	(strcspn (s, ":\n") == strlen (s))

static char	*user_name;
static char	*user_newname;
static uid_t	user_id;
static uid_t	user_newid;
static gid_t	user_gid;
static gid_t	user_newgid;
static char	user_comment[BUFSIZ];
static char	user_home[BUFSIZ];
static char	user_newhome[BUFSIZ];
static char	user_shell[BUFSIZ];
#ifdef	SHADOWPWD
static long	user_expire;
static long	user_inactive;
#endif
static char *user_groups[NGROUPS_MAX+1];  /* NULL-terminated list */
static int do_grp_update = 0;	/* group files need to be updated */

static char *Prog;

#ifdef AUTH_METHODS
static char *auth_arg;
static char user_auth[BUFSIZ];
static int Aflg = 0; /* specify user defined authentication method */
#else
#define Aflg 0
#endif

static int
	uflg = 0, /* specify user ID for new account */
	oflg = 0, /* permit non-unique user ID to be specified with -u */
	gflg = 0, /* primary group ID for new account */
	Gflg = 0, /* secondary group set for new account */
	dflg = 0, /* home directory for new account */
	sflg = 0, /* shell program for new account */
	cflg = 0, /* comment (GECOS) field for new account */
	mflg = 0, /* create user's home directory if it doesn't exist */
	fflg = 0, /* days until account with expired password is locked */
	eflg = 0, /* days after password changed before it becomes expired */
	lflg = 0; /* new user name for user */

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

#ifdef SHADOWPWD
static int is_shadow_pwd;
#endif
#ifdef SHADOWGRP
static int is_shadow_grp;
#endif

#include "groupio.h"

#ifdef	SHADOWGRP
#include "sgroupio.h"
#endif

#include "pwio.h"

#ifdef	SHADOWPWD
#include "shadowio.h"
#endif

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
			fprintf(stderr, "%s: unknown group %s\n", Prog, list);
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
			fprintf(stderr, "%s: group `%s' is a NIS group.\n",
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
	fprintf(stderr,
		"usage: %s [-u uid [-o]] [-g group] [-G group,...] \n", Prog);
	fprintf(stderr,
		"\t\t[-d home [-m]] [-s shell] [-c comment] [-l new_name]\n");
	fprintf(stderr, "\t\t");
#ifdef SHADOWPWD
	if (is_shadow_pwd)
		fprintf(stderr, "[-f inactive ] [-e expire %s ] ",
			DATE_FORMAT_DESCR);
#endif
#ifdef AUTH_METHODS
	fprintf(stderr, "[-A {DEFAULT|program},... ] ");
#endif
	fprintf(stderr, "name\n");
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
	if (lflg) {
		SYSLOG((LOG_INFO, "change user name `%s' to `%s'\n",
			pwent->pw_name, user_newname));
		pwent->pw_name = xstrdup (user_newname);
	}
	if (uflg) {
		SYSLOG((LOG_INFO, "change user `%s' UID from `%d' to `%d'\n",
			pwent->pw_name, pwent->pw_uid, user_newid));
		pwent->pw_uid = user_newid;
	}
	if (gflg) {
		SYSLOG((LOG_INFO, "change user `%s' GID from `%d' to `%d'\n",
			pwent->pw_name, pwent->pw_gid, user_newgid));
		pwent->pw_gid = user_newgid;
	}
	if (cflg)
		pwent->pw_gecos = xstrdup (user_comment);

	if (dflg) {
		SYSLOG((LOG_INFO, "change user `%s' home from `%s' to `%s'\n",
			pwent->pw_name, pwent->pw_dir, user_newhome));
		pwent->pw_dir = xstrdup (user_newhome);
	}
	if (sflg) {
		SYSLOG((LOG_INFO, "change user `%s' shell from `%s' to `%s'\n",
			pwent->pw_name, pwent->pw_shell, user_shell));
		pwent->pw_shell = xstrdup (user_shell);
	}
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
	if (lflg)
		spent->sp_namp = xstrdup (user_newname);

	if (fflg) {
		SYSLOG((LOG_INFO,
			"change user `%s' inactive from `%ld' to `%ld'\n",
			spent->sp_namp, spent->sp_inact, user_inactive));
		spent->sp_inact = user_inactive;
	}
	if (eflg) {
		SYSLOG((LOG_INFO,
			"change user `%s' expiration from `%ld' to `%ld'\n",
			spent->sp_namp, spent->sp_expire, user_expire));
		spent->sp_expire = user_expire;
	}
}
#endif	/* SHADOWPWD */

/*
 * fail_exit - exit with an error code after unlocking files
 */

static void
fail_exit(code)
	int code;
{
	(void) gr_unlock ();
#ifdef	SHADOWGRP
	if (is_shadow_grp)
		sgr_unlock ();
#endif
#ifdef	SHADOWPWD
	if (is_shadow_pwd)
		spw_unlock ();
#endif
	(void) pw_unlock ();
	exit (code);
}

/*
 * grp_update - add user to secondary group set
 *
 *	grp_update() takes the secondary group set given in user_groups
 *	and adds the user to each group given by that set.
 */

static void
grp_update()
{
	int is_member;
	int was_member;
	int changed;
	const struct group *grp;
	struct group *ngrp;
#ifdef	SHADOWGRP
	int was_admin;
	const struct sgrp *sgrp;
	struct sgrp *nsgrp;
#endif

	/*
	 * Lock and open the group file.  This will load all of the group
	 * entries.
	 */

	if (! gr_lock ()) {
		fprintf(stderr, "%s: error locking group file\n", Prog);
		SYSLOG((LOG_ERR, "error locking group file"));
		exit (1);
	}
	if (! gr_open (O_RDWR)) {
		fprintf(stderr, "%s: error opening group file\n", Prog);
		SYSLOG((LOG_ERR, "error opening group file"));
		fail_exit (1);
	}
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_lock ()) {
		fprintf(stderr, "%s: error locking shadow group file\n", Prog);
		SYSLOG((LOG_ERR, "error locking shadow group file"));
		fail_exit (1);
	}
	if (is_shadow_grp && ! sgr_open (O_RDWR)) {
		fprintf(stderr, "%s: error opening shadow group file\n", Prog);
		SYSLOG((LOG_ERR, "error opening shadow group file"));
		fail_exit (1);
	}
#endif	/* SHADOWGRP */

	changed = 0;

	/*
	 * Scan through the entire group file looking for the groups that
	 * the user is a member of.
	 */

	for (gr_rewind (), grp = gr_next ();grp;grp = gr_next ()) {

		/*
		 * See if the user specified this group as one of their
		 * concurrent groups.
		 */

		was_member = is_on_list(grp->gr_mem, user_name);
		is_member = Gflg && is_on_list(user_groups, grp->gr_name);

		if (!was_member && !is_member)
			continue;

		ngrp = __gr_dup(grp);
		if (!ngrp) {
			exit(13);  /* XXX */
		}

		if (was_member && (!Gflg || is_member)) {
			if (lflg) {
				ngrp->gr_mem = del_list(ngrp->gr_mem,
							user_name);
				ngrp->gr_mem = add_list(ngrp->gr_mem,
							user_newname);
				changed = 1;
				SYSLOG((LOG_INFO,
					"change `%s' to `%s' in group `%s'\n",
					user_name, user_newname,
					ngrp->gr_name));
			}
		} else if (was_member && Gflg && !is_member) {
			ngrp->gr_mem = del_list (ngrp->gr_mem, user_name);
			changed = 1;
			SYSLOG((LOG_INFO, "delete `%s' from group `%s'\n",
				user_name, ngrp->gr_name));
		} else if (!was_member && Gflg && is_member) {
			ngrp->gr_mem = add_list (ngrp->gr_mem,
				lflg ? user_newname:user_name);
			changed = 1;
			SYSLOG((LOG_INFO, "add `%s' to group `%s'\n",
				lflg ? user_newname:user_name, ngrp->gr_name));
		}
		if (!changed)
			continue;

		changed = 0;
		if (! gr_update (ngrp)) {
			fprintf(stderr, "%s: error adding new group entry\n",
				Prog);
			SYSLOG((LOG_ERR, "error adding group entry"));
			fail_exit (1);
		}
#ifdef	NDBM
		/*
		 * Update the DBM group file with the new entry as well.
		 */

		if (! gr_dbm_update (ngrp)) {
			fprintf(stderr, "%s: cannot add new dbm group entry\n",
				Prog);
			SYSLOG((LOG_ERR, "error adding dbm group entry"));
			fail_exit (1);
		}
#endif	/* NDBM */
	}
#ifdef NDBM
	endgrent ();
#endif

#ifdef	SHADOWGRP
	if (!is_shadow_grp)
		return;

	changed = 0;

	/*
	 * Scan through the entire shadow group file looking for the groups
	 * that the user is a member of.
	 */

	for (sgr_rewind (), sgrp = sgr_next ();sgrp;sgrp = sgr_next ()) {

		/*
		 * See if the user was a member of this group
		 */

		was_member = is_on_list(sgrp->sg_mem, user_name);

		/*
		 * See if the user was an administrator of this group
		 */

		was_admin = is_on_list(sgrp->sg_adm, user_name);

		/*
		 * See if the user specified this group as one of their
		 * concurrent groups.
		 */

		is_member = Gflg && is_on_list(user_groups, sgrp->sg_name);

		if (!was_member && !was_admin && !is_member)
			continue;

		nsgrp = __sgr_dup(sgrp);
		if (!nsgrp) {
			exit(13);  /* XXX */
		}

		if (was_admin && lflg) {
			nsgrp->sg_adm = del_list (nsgrp->sg_adm, user_name);
			nsgrp->sg_adm = add_list (nsgrp->sg_adm, user_newname);
			changed = 1;
			SYSLOG((LOG_INFO,
			    "change admin `%s' to `%s' in shadow group `%s'\n",
			    user_name, user_newname, nsgrp->sg_name));
		}
		if (was_member && (!Gflg || is_member)) {
			if (lflg) {
				nsgrp->sg_mem = del_list (nsgrp->sg_mem,
					user_name);
				nsgrp->sg_mem = add_list (nsgrp->sg_mem,
					user_newname);
				changed = 1;
				SYSLOG((LOG_INFO,
				  "change `%s' to `%s' in shadow group `%s'\n",
				  user_name, user_newname, nsgrp->sg_name));
			}
		} else if (was_member && Gflg && !is_member) {
			nsgrp->sg_mem = del_list (nsgrp->sg_mem, user_name);
			changed = 1;
			SYSLOG((LOG_INFO,
				"delete `%s' from shadow group `%s'\n",
				user_name, nsgrp->sg_name));
		} else if (!was_member && Gflg && is_member) {
			nsgrp->sg_mem = add_list (nsgrp->sg_mem,
				lflg ? user_newname:user_name);
			changed = 1;
			SYSLOG((LOG_INFO, "add `%s' to shadow group `%s'\n",
				lflg ? user_newname:user_name,nsgrp->sg_name));
		}
		if (!changed)
			continue;

		changed = 0;

		/* 
		 * Update the group entry to reflect the changes.
		 */

		if (! sgr_update (nsgrp)) {
			fprintf(stderr, "%s: error adding new group entry\n",
				Prog);
			SYSLOG((LOG_ERR, "error adding shadow group entry\n"));
			fail_exit (1);
		}
#ifdef	NDBM
		/*
		 * Update the DBM group file with the new entry as well.
		 */

		if (! sg_dbm_update (nsgrp)) {
			fprintf(stderr, "%s: cannot add new dbm group entry\n",
				Prog);
			SYSLOG((LOG_ERR,
				"error adding dbm shadow group entry\n"));
			fail_exit (1);
		}
#endif	/* NDBM */
	}
#ifdef NDBM
	endsgent ();
#endif	/* NDBM */
#endif	/* SHADOWGRP */
}

#ifdef AUTH_METHODS
/*
 * get_password - locate encrypted password in authentication list
 */

static char *
get_password(list)
	char *list;
{
	char	*cp, *end;
	static	char	buf[257];

	strcpy (buf, list);
	for (cp = buf;cp;cp = end) {
		if ((end = strchr (cp, ';')))
			*end++ = 0;

		if (cp[0] == '@')
			continue;

		return cp;
	}
	return (char *) 0;
}

/*
 * split_auths - break up comma list into (char *) array
 */

static void
split_auths(list, array)
	char *list;
	char **array;
{
	char	*cp, *end;
	int	i = 0;

	for (cp = list;cp;cp = end) {
		if ((end = strchr (cp, ';')))
			*end++ = '\0';

		array[i++] = cp;
	}
	array[i] = 0;
}

/*
 * update_auths - find list of methods to update
 */

static void
update_auths(old, new, update)
	char *old;
	char *new;
	char *update;
{
	char	oldbuf[257], newbuf[257];
	char	*oldv[32], *newv[32], *updatev[32];
	int	i, j, k;

	strcpy (oldbuf, old);
	split_auths (oldbuf, oldv);

	strcpy (newbuf, new);
	split_auths (newbuf, newv);

	for (i = j = k = 0;oldv[i];i++) {
		for (j = 0;newv[j];j++)
			if (strcmp (oldv[i], newv[j]) != 0)
				break;

		if (newv[j] != (char *) 0)
			updatev[k++] = oldv[i];
	}
	updatev[k] = 0;

	update[0] = '\0';
	for (i = 0;updatev[i];i++) {
		if (i)
			strcat (update, ";");

		strcat (update, updatev[i]);
	}
}

/*
 * add_auths - find list of methods to add
 */

static void
add_auths(old, new, add)
	char *old;
	char *new;
	char *add;
{
	char	oldbuf[257], newbuf[257];
	char	*oldv[32], *newv[32], *addv[32];
	int	i, j, k;

	strcpy (oldbuf, old);
	split_auths (oldbuf, oldv);

	strcpy (newbuf, new);
	split_auths (newbuf, newv);

	for (i = j = k = 0;newv[i];i++) {
		for (j = 0;oldv[j];j++)
			if (strcmp (oldv[i], newv[j]) == 0)
				break;

		if (oldv[j] == (char *) 0)
			addv[k++] = newv[i];
	}
	addv[k] = 0;

	add[0] = '\0';
	for (i = 0;addv[i];i++) {
		if (i)
			strcat (add, ";");

		strcat (add, addv[i]);
	}
}

/*
 * delete_auths - find list of methods to delete
 */

static void
delete_auths(old, new, remove)
	char *old;
	char *new;
	char *remove;
{
	char	oldbuf[257], newbuf[257];
	char	*oldv[32], *newv[32], *removev[32];
	int	i, j, k;

	strcpy (oldbuf, old);
	split_auths (oldbuf, oldv);

	strcpy (newbuf, new);
	split_auths (newbuf, newv);

	for (i = j = k = 0;oldv[i];i++) {
		for (j = 0;newv[j];j++)
			if (strcmp (oldv[i], newv[j]) == 0)
				break;

		if (newv[j] == (char *) 0)
			removev[k++] = oldv[i];
	}
	removev[k] = 0;

	remove[0] = '\0';
	for (i = 0;removev[i];i++) {
		if (i)
			strcat (remove, ";");

		strcat (remove, removev[i]);
	}
}

/*
 * convert_auth - convert the argument list to a authentication list
 */

static void
convert_auth(auths, oldauths, list)
	char *auths;
	char *oldauths;
	char *list;
{
	char	*cp, *end;
	char	*old;
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
			if ((old = get_password (oldauths)))
				strcat (auths, old);
			else
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
#endif

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
	const struct passwd *pwd;
#ifdef SHADOWPWD
	const struct spwd *spwd = NULL;
#endif
	long	l;
	int	anyflag = 0;
	int	arg;

	if (argc == 1 || argv[argc - 1][0] == '-')
		usage ();

	if (! (pwd = getpwnam (argv[argc - 1]))) {
		fprintf(stderr, "%s: user %s does not exist\n",
			Prog, argv[argc - 1]);
		exit(6);
	}
	user_name = argv[argc - 1];

#ifdef	USE_NIS

	/*
	 * Now make sure it isn't an NIS user.
	 */

	if (__ispwNIS ()) {
		char	*nis_domain;
		char	*nis_master;

		fprintf(stderr, "%s: user %s is a NIS user\n",
			Prog, user_name);

		if (! yp_get_default_domain (&nis_domain) &&
				! yp_master (nis_domain, "passwd.byname",
				&nis_master)) {
			fprintf(stderr, "%s: %s is the NIS master\n",
				Prog, nis_master);
		}
		exit(6);
	}
#endif
	user_id = pwd->pw_uid;
	user_gid = pwd->pw_gid;
	strcpy (user_comment, pwd->pw_gecos);
	strcpy (user_home, pwd->pw_dir);
	strcpy (user_shell, pwd->pw_shell);

#ifdef	SHADOWPWD
	if (is_shadow_pwd && (spwd = getspnam (user_name))) {
		user_expire = spwd->sp_expire;
		user_inactive = spwd->sp_inact;
	}
#endif
#ifdef	SHADOWPWD
#define FLAGS "A:u:og:G:d:s:c:mf:e:l:"
#else
#define FLAGS "A:u:og:G:d:s:c:ml:"
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
			case 'c':
				if (! VALID (optarg)) {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				strncpy (user_comment, optarg, BUFSIZ);
				cflg++;
				break;
			case 'd':
				if (! VALID (optarg)) {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				dflg++;
				strncpy (user_newhome, optarg, BUFSIZ);
				break;
#ifdef	SHADOWPWD
			case 'e':
				if (!is_shadow_pwd)
					usage();
				l = strtoday (optarg) * (DAY/SCALE);
				user_expire = l;
				eflg++;
				break;
			case 'f':
				if (!is_shadow_pwd)
					usage();
				user_inactive = atoi (optarg);
				fflg++;
				break;
#endif	/* SHADOWPWD */
			case 'g':
				if (isdigit (optarg[0]))
					grp = getgrgid (atoi (optarg));
				else
					grp = getgrnam (optarg);

				if (! grp) {
					fprintf(stderr,
						"%s: unknown group %s\n",
						Prog, optarg);
					exit(6);
				}
				user_newgid = grp->gr_gid;
				gflg++;
				break;
			case 'G':
				if (get_groups (optarg))
					exit(6);
				Gflg++;
				break;
			case 'l':
				if (!check_name(optarg)) {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}

				/*
				 * If the name does not really change, we
				 * mustn't set the flag as this will cause
				 * rather serious problems later!
				 */

				if (strcmp (user_name, optarg))
					lflg++;

				user_newname = optarg;
				break;
			case 'm':
				if (! dflg)
					usage ();

				mflg++;
				break;
			case 'o':
				if (! uflg)
					usage ();

				oflg++;
				break;
			case 's':
				if (! VALID (optarg)) {
					fprintf(stderr,
						"%s: invalid field `%s'\n",
						Prog, optarg);
					exit(3);
				}
				strncpy (user_shell, optarg, BUFSIZ);
				sflg++;
				break;
			case 'u':
				uflg++;
				user_newid = atoi (optarg);
				break;
			default:
				usage ();
		}
		anyflag++;
	}
	if (anyflag == 0) {
		fprintf(stderr, "%s: no flags given\n", Prog);
		exit(2);
	}
	if (optind != argc - 1)
		usage ();

	if (dflg && strcmp (user_home, user_newhome) == 0)
		dflg = mflg = 0;

	if (uflg && user_id == user_newid)
		uflg = oflg = 0;

	if (lflg && getpwnam (user_newname)) {
		fprintf(stderr, "%s: user %s exists\n", Prog, user_newname);
		exit(9);
	}

	if (uflg && !oflg && getpwuid(user_newid)) {
		fprintf(stderr, "%s: uid %d is not unique\n",
			Prog, user_newid);
		exit(4);
	}

	do_grp_update = Gflg || lflg;
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
		fprintf(stderr, "%s: cannot rewrite password file\n", Prog);
		fail_exit (1);
	}
#ifdef	SHADOWPWD
	if (is_shadow_pwd && ! spw_close ()) {
		fprintf(stderr, "%s: cannot rewrite shadow password file\n",	
			Prog);
		fail_exit (1);
	}
#endif
	if (do_grp_update) {
		if (! gr_close ()) {
			fprintf(stderr, "%s: cannot rewrite group file\n",
				Prog);
			fail_exit (1);
		}
		gr_unlock();
#ifdef	SHADOWGRP
		if (is_shadow_grp && ! sgr_close ()) {
			fprintf(stderr, "%s: cannot rewrite shadow group file\n",
				Prog);
			fail_exit (1);
		}
		if (is_shadow_grp)
			sgr_unlock();
#endif
	}
#ifdef	SHADOWPWD
	if (is_shadow_pwd)
		spw_unlock ();
#endif
	(void) pw_unlock ();

	/*
	 * Close the DBM and/or flat files
	 */

	endpwent ();
#ifdef	SHADOWPWD
	endspent ();
#endif
	endgrent ();
#ifdef	SHADOWGRP
	endsgent ();
#endif
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
		fprintf(stderr, "%s: unable to lock password file\n", Prog);
		exit (1);
	}
	if (! pw_open (O_RDWR)) {
		fprintf(stderr, "%s: unable to open password file\n", Prog);
		fail_exit (1);
	}
#ifdef	SHADOWPWD
	if (is_shadow_pwd && ! spw_lock ()) {
		fprintf(stderr, "%s: cannot lock shadow password file\n",
			Prog);
		fail_exit (1);
	}
	if (is_shadow_pwd && ! spw_open (O_RDWR)) {
		fprintf(stderr, "%s: cannot open shadow password file\n",
			Prog);
		fail_exit (1);
	}
#endif
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
	struct passwd pwent;
	const struct passwd *pwd;
#ifdef	SHADOWPWD
	struct spwd spent;
	const struct spwd *spwd = NULL;
#endif
#ifdef AUTH_METHODS
	char	old_auth[BUFSIZ];
	char	auth_buf[BUFSIZ];
#endif

	/*
	 * Locate the entry in /etc/passwd, which MUST exist.
	 */

	pwd = pw_locate(user_name);
	if (!pwd) {
		fprintf(stderr, "%s: %s not found in /etc/passwd\n",
			Prog, user_name);
		fail_exit(6);
	}
	pwent = *pwd;
	new_pwent (&pwent);

#ifdef	SHADOWPWD

	/* 
	 * Locate the entry in /etc/shadow.  It doesn't have to
	 * exist, and won't be created if it doesn't.
	 */

	if (is_shadow_pwd && (spwd = spw_locate(user_name))) {
		spent = *spwd;
		new_spent (&spent);
	}
#endif

#ifdef AUTH_METHODS

#ifdef	SHADOWPWD
	strcpy (old_auth, spwd ? spent.sp_pwdp : pwent.pw_passwd);
#else
	strcpy (old_auth, pwent.pw_passwd);
#endif

	if (Aflg)
		convert_auth (user_auth, old_auth, auth_arg);

	/*
	 * XXX - this code needs some checking, changing the user name with
	 * "usermod -l new old" clears the password for this user :-(.
	 * For now, just don't define AUTH_METHODS and all will be well.
	 * Most programs don't support "administrator defined authentication
	 * methods" and PAM (when done) will be better anyway :-).  --marekm
	 */
	if (lflg || (Aflg && strcmp (old_auth, user_auth) != 0)) {
		delete_auths (old_auth, user_auth, auth_buf);
		if (auth_buf[0] && pw_auth (auth_buf, user_name,
				PW_DELETE, (char *) 0)) {
			fprintf(stderr,
				"%s: error deleting authentication method\n",
				Prog);
			SYSLOG((LOG_ERR, "error deleting auth for `%s'\n",
				user_name));
			fail_exit (1);
		}
		add_auths (old_auth, user_auth, auth_buf);
		if (auth_buf[0] == '@' && pw_auth (auth_buf,
			lflg ? user_newname:user_name, PW_ADD, (char *) 0)) {
			fprintf(stderr,
				"%s: error adding authentication method\n",
				Prog);
			SYSLOG((LOG_ERR, "error adding auth for `%s'\n",
				lflg ? user_newname:user_name));
			fail_exit (1);
		}
		update_auths (old_auth, user_auth, auth_buf);
		if (lflg && auth_buf[0] == '@' && pw_auth (auth_buf,
				user_newname, PW_CHANGE, user_name)) {
			fprintf(stderr,
				"%s: error changing authentication method\n",
				Prog);
			SYSLOG((LOG_ERR, "error changing auth for `%s'\n",
				lflg ? user_newname:user_name));
			fail_exit (1);
		}
#ifdef	SHADOWPWD
		if (spwd)
			spent.sp_pwdp = user_auth;
		else
#endif
			pwent.pw_passwd = user_auth;
	}
#endif  /* AUTH_METHODS */
	if (lflg || uflg || gflg || cflg || dflg || sflg || Aflg) {
		if (! pw_update (&pwent)) {
			fprintf(stderr, "%s: error changing password entry\n",
				Prog);
			fail_exit (1);
		}
		if (lflg && ! pw_remove (user_name)) {
			fprintf(stderr, "%s: error removing password entry\n",
				Prog);
			fail_exit (1);
		}
#if defined(DBM) || defined(NDBM)
		if (pw_dbm_present()) {
			if (! pw_dbm_update (&pwent)) {
				fprintf(stderr,
					"%s: error adding password dbm entry\n",
					Prog);
				fail_exit (1);
			}
			if (lflg && (pwd = getpwnam (user_name)) &&
					! pw_dbm_remove (pwd)) {
				fprintf(stderr,
					"%s: error removing passwd dbm entry\n",
					Prog);
				fail_exit (1);
			}
		}
#endif
	}
#ifdef	SHADOWPWD
	if (spwd && (lflg || eflg || fflg || Aflg)) {
		if (! spw_update (&spent)) {
			fprintf(stderr,
				"%s: error adding new shadow password entry\n",
				Prog);
			fail_exit (1);
		}
		if (lflg && ! spw_remove (user_name)) {
			fprintf(stderr,
				"%s: error removing shadow password entry\n",
				Prog);
			fail_exit (1);
		}
	}
#ifdef	NDBM
	if (spwd && sp_dbm_present()) {
		if (! sp_dbm_update (&spent)) {
			fprintf(stderr,
				"%s: error updating shadow passwd dbm entry\n",
				Prog);
			fail_exit (1);
		}
		if (lflg && ! sp_dbm_remove (user_name)) {
			fprintf(stderr,
				"%s: error removing shadow passwd db entry\n",
				Prog);
			fail_exit (1);
		}
	}
#endif	/* NDBM */
#endif	/* SHADOWPWD */
	if (do_grp_update)
		grp_update ();
}

/*
 * move_home - move the user's home directory
 *
 *	move_home() moves the user's home directory to a new location.
 *	The files will be copied if the directory cannot simply be
 *	renamed.
 */

static void
move_home()
{
	struct	stat	sb;

	if (mflg && stat (user_home, &sb) == 0) {
		/*
		 * Don't try to move it if it is not a directory
		 * (but /dev/null for example).  --marekm
		 */
		if (!S_ISDIR(sb.st_mode))
			return;

		if (access (user_newhome, 0) == 0) {
			fprintf(stderr, "%s: directory %s exists\n",
				Prog, user_newhome);
			fail_exit(12);
		} else if (rename (user_home, user_newhome)) {
			if (errno == EXDEV) {
				if (mkdir (user_newhome, sb.st_mode & 0777)) {
					fprintf(stderr,
						"%s: can't create %s\n",
						Prog, user_newhome);
				}
				if (chown (user_newhome,
						sb.st_uid, sb.st_gid)) {
					fprintf(stderr, "%s: can't chown %s\n",
						Prog, user_newhome);
					rmdir (user_newhome);
					fail_exit(12);
				}
				if (copy_tree (user_home, user_newhome,
						uflg ? user_newid:-1,
						gflg ? user_newgid:-1) == 0 &&
					remove_tree (user_home) == 0 &&
						rmdir (user_home) == 0)
					return;

				(void) remove_tree (user_newhome);
				(void) rmdir (user_newhome);
			}
			fprintf(stderr,
				"%s: cannot rename directory %s to %s\n",
				Prog, user_home, user_newhome);
			fail_exit(12);
		}
	}
	if (uflg || gflg)
		chown (dflg ? user_newhome:user_home,
			uflg ? user_newid:user_id,
			gflg ? user_newgid:user_gid);
}

/*
 * update_files - update the lastlog and faillog files
 */

static void
update_files()
{
	struct	lastlog	ll;
	struct	faillog	fl;
	int	fd;

	/*
	 * Relocate the "lastlog" entries for the user.  The old entry
	 * is left alone in case the UID was shared.  It doesn't hurt
	 * anything to just leave it be.
	 */

	if ((fd = open (LASTLOG_FILE, O_RDWR)) != -1) {
		lseek (fd, (off_t) user_id * sizeof ll, SEEK_SET);
		if (read (fd, (char *) &ll, sizeof ll) == sizeof ll) {
			lseek (fd, (off_t) user_newid * sizeof ll, SEEK_SET);
			write (fd, (char *) &ll, sizeof ll);
		}
		close (fd);
	}

	/*
	 * Relocate the "faillog" entries in the same manner.
	 */

	if ((fd = open (FAILFILE, O_RDWR)) != -1) {
		lseek (fd, (off_t) user_id * sizeof fl, SEEK_SET);
		if (read (fd, (char *) &fl, sizeof fl) == sizeof fl) {
			lseek (fd, (off_t) user_newid * sizeof fl, SEEK_SET);
			write (fd, (char *) &fl, sizeof fl);
		}
		close (fd);
	}
}

#ifndef NO_MOVE_MAILBOX
/*
 * This is the new and improved code to carefully chown/rename the user's
 * mailbox.  Maybe I am too paranoid but the mail spool dir sometimes
 * happens to be mode 1777 (this practice is NOT recommended, but makes
 * mail user agents work without setgid mail).  --marekm
 */
static void
move_mailbox()
{
	char *maildir;
	char mailfile[1024], newmailfile[1024];
	int fd;
	struct stat st;

	maildir = getdef_str("MAIL_DIR");
#ifdef MAIL_SPOOL_DIR
	if (!maildir && !getdef_str("MAIL_FILE"))
		maildir = MAIL_SPOOL_DIR;
#endif
	if (!maildir)
		return;

	/*
	 * O_NONBLOCK is to make sure open won't hang on mandatory locks.
	 * We do fstat/fchown to make sure there are no races (someone
	 * replacing /var/spool/mail/luser with a hard link to /etc/passwd
	 * between stat and chown).  --marekm
	 */

	sprintf(mailfile, "%s/%s", maildir, user_name);
	fd = open(mailfile, O_RDONLY | O_NONBLOCK, 0);
	if (fd < 0) {
		/* no need for warnings if the mailbox doesn't exist */
		if (errno != ENOENT)
			perror(mailfile);
		return;
	}
	if (fstat(fd, &st) < 0) {
		perror("fstat");
		close(fd);
		return;
	}
	if (st.st_uid != user_id) {
		/* better leave it alone */
		fprintf(stderr, "%s: warning: %s not owned by %s\n",
			Prog, mailfile, user_name);
		close(fd);
		return;
	}
	if (uflg && fchown(fd, user_newid, (gid_t) -1) < 0)
		perror("failed to change mailbox owner");

	close(fd);

	if (lflg) {
		sprintf(newmailfile, "%s/%s", maildir, user_newname);
		if (link(mailfile, newmailfile) || unlink(mailfile))
			perror("failed to rename mailbox");
	}
}
#endif

/*
 * main - usermod command
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
#endif	/* NDBM */
	process_flags (argc, argv);

	/*
	 * Do the hard stuff - open the files, change the user entries,
	 * change the home directory, then close and update the files.
	 */

	open_files ();

	usr_update ();

	close_files ();

	if (mflg)
		move_home ();

#ifndef NO_MOVE_MAILBOX
	if (lflg || uflg)
		move_mailbox();
#endif

	if (uflg) {
		update_files ();

		/*
		 * Change the UID on all of the files owned by `user_id'
		 * to `user_newid' in the user's home directory.
		 */

		chown_tree (dflg ? user_newhome:user_home,
			user_id, user_newid,
			user_gid, gflg ? user_newgid:user_gid);
	}
	exit(0);
	/*NOTREACHED*/
}
