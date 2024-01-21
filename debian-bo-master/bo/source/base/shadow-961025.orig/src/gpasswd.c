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
RCSID("$Id: gpasswd.c,v 1.3 1996/09/25 03:20:01 marekm Exp $")

#include <sys/types.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>

#include "prototypes.h"
#include "defines.h"

#include "groupio.h"
#ifdef	SHADOWGRP
#include "sgroupio.h"
#endif

static char name[BUFSIZ];
static char pass[BUFSIZ];

static char *Prog;
static char *user;
static char *group;
#ifdef SHADOWGRP
static int is_shadowgrp;
static char *admins;
#endif
static char *members;

static int
	aflg = 0,
	Aflg = 0,
	dflg = 0,
	Mflg = 0,
	rflg = 0,
	Rflg = 0;

#ifndef	RETRIES
#define	RETRIES	3
#endif

extern char *crypt_make_salt();
extern	int	errno;
#ifdef	NDBM
#ifdef	SHADOWGRP
extern	int	sg_dbm_mode;
#endif
extern	int	gr_dbm_mode;
#endif

/*
 * usage - display usage message
 */

static void
usage()
{
	fprintf(stderr, "usage: %s [ -r|R ] group\n", Prog);
	fprintf(stderr, "       %s [ -a user ] group\n", Prog);
	fprintf(stderr, "       %s [ -d user ] group\n", Prog);
#ifdef	SHADOWGRP
	if (is_shadowgrp)
	fprintf(stderr, "       %s [ -A user[,user] ] [ -M user[,user] ] group\n",
		Prog);
	else
#else
	fprintf(stderr, "       %s [ -M user[,user] ] group\n", Prog);
#endif
	exit (1);
}

/*
 * die - set or reset termio modes.
 *
 *	die() is called before processing begins.  signal() is then
 *	called with die() as the signal handler.  If signal later
 *	calls die() with a signal number, the terminal modes are
 *	then reset.
 */

static RETSIGTYPE
die(killed)
	int killed;
{
	static TERMIO sgtty;

	if (killed)
		STTY(0, &sgtty);
	else
		GTTY(0, &sgtty);

	if (killed) {
		putchar ('\n');
		fflush (stdout);
		exit (killed);
	}
}
/*
 * check_list - check a comma-separated list of user names for validity
 *
 *	check_list scans a comma-separated list of user names and checks
 *	that each listed name exists.
 */

static int
check_list(users)
	const char *users;
{
	const char *start, *end;
	char	user[32];
	int	errors = 0;
	int	len;

	for (start = users;start && *start;start = end) {
		if ((end = strchr (start, ','))) {
			len = end - start;
			end++;
		} else {
			len = strlen(start);
		}

		if (len > sizeof(user) - 1)
			len = sizeof(user) - 1;
		strncpy(user, start, len);
		user[len] = '\0';

		/*
		 * This user must exist.
		 */

		if (! getpwnam (user)) {
			fprintf (stderr, "%s: unknown user %s\n", Prog, user);
			errors++;
		}
	}
	return errors;
}

/*
 * gpasswd - administer the /etc/group file
 *
 *	-a user		add user to the named group
 *	-d user		remove user from the named group
 *	-r		remove password from the named group
 *	-R		restrict access to the named group
 *	-A user,...	make list of users the administrative users
 *	-M user,...	make list of users the group members
 */

int
main(argc, argv)
	int argc;
	char **argv;
{
	extern	int	optind;
	extern	char	*optarg;
	int	flag;
	char	*cp;
	int	amroot;
	int	retries;
	int	ruid = getuid();
	struct	group	*gr = 0;
	struct	group	grent;
#ifdef	SHADOWGRP
	struct	sgrp	*sg = 0;
	struct	sgrp	sgent;
#endif
	struct	passwd	*pw = 0;

	/*
	 * Make a note of whether or not this command was invoked
	 * by root.  This will be used to bypass certain checks
	 * later on.  Also, set the real user ID to match the
	 * effective user ID.  This will prevent the invoker from
	 * issuing signals which would interfer with this command.
	 */

	amroot = getuid () == 0;
#ifdef	NDBM
#ifdef	SHADOWGRP
	sg_dbm_mode = O_RDWR;
#endif
	gr_dbm_mode = O_RDWR;
#endif
	setuid (geteuid ());

	Prog = Basename(argv[0]);

	openlog("gpasswd", LOG_PID|LOG_CONS|LOG_NOWAIT, LOG_AUTH);
	setbuf (stdout, (char *) 0);
	setbuf (stderr, (char *) 0);

#ifdef SHADOWGRP
	is_shadowgrp = (access(SGROUP_FILE, 0) == 0);
#endif
	while ((flag = getopt (argc, argv, "a:d:grRA:M:")) != EOF) {
		switch (flag) {
			case 'a':	/* add a user */
				aflg++;
				user = optarg;
				break;
#ifdef SHADOWGRP
			case 'A':
				if (!is_shadowgrp)
					usage();
				Aflg++;
				admins = optarg;
				break;
#endif
			case 'd':	/* delete a user */
				dflg++;
				user = optarg;
				break;
			case 'g':	/* no-op from normal password */
				break;
			case 'M':
				Mflg++;
				members = optarg;
				break;
			case 'r':	/* remove group password */
				rflg++;
				break;
			case 'R':	/* restrict group password */
				Rflg++;
				break;
			default:
				usage ();
		}
	}

	/*
	 * Make sure exclusive flags are exclusive
	 */

	if (aflg + dflg + rflg + Rflg + (Aflg || Mflg) > 1)
		usage ();

	/*
	 * If the password is being changed, the input and output must
	 * both be a tty.  The typical keyboard signals are caught
	 * so the termio modes can be restored.
	 */

	if (! aflg && ! dflg && ! rflg && ! Rflg && ! Aflg && ! Mflg) {
		if (! isatty (0) || ! isatty (1))
			exit (1);

		die (0);			/* save tty modes */

		signal (SIGHUP, die);
		signal (SIGINT, die);
		signal (SIGQUIT, die);
		signal (SIGTERM, die);
#ifdef	SIGTSTP
		signal (SIGTSTP, die);
#endif
	}

	/*
	 * Determine the name of the user that invoked this command.
	 * This is really hit or miss because there are so many ways
	 * that command can be executed and so many ways to trip up
	 * the routines that report the user name.
	 */

	if ((cp = getlogin ()) && (pw = getpwnam (cp)) && pw->pw_uid == ruid) {
					/* need user name */
		STRFCPY(name, cp);
	} else if ((pw = getpwuid (ruid))) /* get it from password file */
		STRFCPY(name, pw->pw_name);
	else {				/* can't find user name! */
		fprintf (stderr, "Who are you?\n");
		exit (1);
	}
	if (! (pw = getpwnam (name)))
		goto failure;		/* can't get my name ... */
		
	/*
	 * Get the name of the group that is being affected.  The group
	 * entry will be completely replicated so it may be modified
	 * later on.
	 */

	if (! (group = argv[optind]))
		usage ();

	if (! (gr = getgrnam (group))) {
		fprintf (stderr, "unknown group: %s\n", group);
		exit (1);
	}
	grent = *gr;
	grent.gr_name = xstrdup (gr->gr_name);
	grent.gr_passwd = xstrdup (gr->gr_passwd);

	grent.gr_mem = dup_list(gr->gr_mem);
#ifdef	SHADOWGRP
	if ((sg = getsgnam (group))) {
		sgent = *sg;
		sgent.sg_name = xstrdup (sg->sg_name);
		sgent.sg_passwd = xstrdup (sg->sg_passwd);

		sgent.sg_mem = dup_list(sg->sg_mem);
		sgent.sg_adm = dup_list(sg->sg_adm);
	} else {
		sgent.sg_name = xstrdup (group);
		sgent.sg_passwd = grent.gr_passwd;
		grent.gr_passwd = "!";

		sgent.sg_mem = dup_list(grent.gr_mem);

		sgent.sg_adm = (char **) xmalloc (sizeof (char *) * 2);
#ifdef FIRST_MEMBER_IS_ADMIN
		if (sgent.sg_mem[0]) {
			sgent.sg_adm[0] = xstrdup (sgent.sg_mem[0]);
			sgent.sg_adm[1] = 0;
		} else
#endif
			sgent.sg_adm[0] = 0;

		sg = &sgent;
	}

	/*
	 * The policy here for changing a group is that 1) you must be
	 * root or 2). you must be listed as an administrative member.
	 * Administrative members can do anything to a group that the
	 * root user can.
	 */

	if (!amroot && !is_on_list(sgent.sg_adm, name))
		goto failure;
#else	/* ! SHADOWGRP */

#ifdef FIRST_MEMBER_IS_ADMIN
	/*
	 * The policy here for changing a group is that 1) you must bes
	 * root or 2) you must be the first listed member of the group.
	 * The first listed member of a group can do anything to that
	 * group that the root user can.  The rationale for this hack is
	 * that the FIRST user is probably the most important user in
	 * this entire group.
	 */

	if (! amroot) {
		if (grent.gr_mem[0] == (char *) 0)
			goto failure;

		if (strcmp (grent.gr_mem[0], name) != 0)
			goto failure;
	}
#else
	/*
	 * This feature enabled by default could be a security problem
	 * when installed on existing systems where the first group
	 * member might be just a normal user...  --marekm
	 */

	if (!amroot)
		goto failure;
#endif

#endif	/* SHADOWGRP */

	/*
	 * Removing a password is straight forward.  Just set the
	 * password field to a "".
	 */

	if (rflg) {
		grent.gr_passwd = "";
#ifdef	SHADOWGRP
		sgent.sg_passwd = "";
#endif
		SYSLOG((LOG_INFO, "remove password from group %s\n", group));
		goto output;
	} else if (Rflg) {
		/*
		 * Same thing for restricting the group.  Set the password
		 * field to "!".
		 */

		grent.gr_passwd = "!";
#ifdef	SHADOWGRP
		sgent.sg_passwd = "!";
#endif
		SYSLOG((LOG_INFO, "restrict access to group %s\n", group));
		goto output;
	}

	/*
	 * Adding a member to a member list is pretty straightforward
	 * as well.  Call the appropriate routine and split.
	 */

	if (aflg) {
		if (getpwnam (user) == (struct passwd *) 0) {
			fprintf (stderr, "%s: unknown user %s\n", Prog, user);
			exit (1);
		}
		printf ("Adding user %s to group %s\n", user, group);
		grent.gr_mem = add_list (grent.gr_mem, user);
#ifdef	SHADOWGRP
		sgent.sg_mem = add_list (sgent.sg_mem, user);
#endif
		SYSLOG((LOG_INFO, "add member %s to group %s\n", user, group));
		goto output;
	}

	/*
	 * Removing a member from the member list is the same deal
	 * as adding one, except the routine is different.
	 */

	if (dflg) {
		int	removed = 0;

		printf ("Removing user %s from group %s\n", user, group);

		if (is_on_list(grent.gr_mem, user)) {
			removed = 1;
			grent.gr_mem = del_list (grent.gr_mem, user);
		}
#ifdef	SHADOWGRP
		if (is_on_list(sgent.sg_mem, user)) {
			removed = 1;
			sgent.sg_mem = del_list (sgent.sg_mem, user);
		}
#endif
		if (! removed) {
			fprintf (stderr, "%s: unknown member %s\n", Prog, user);
			exit (1);
		}
		SYSLOG((LOG_INFO, "remove member %s from group %s\n",
			user, group));
		goto output;
	}

	/*
	 * Replacing the entire list of members is simple.  Check the list
	 * to make sure everyone is a real user.  Then slap the new list
	 * in place.
	 */

	if (Mflg) {

		/*
		 * Only root can replace the entire list.
		 */

		if (! amroot)
			goto failure;

		/*
		 * Check the list for validity, then put it in.
		 */

		if (check_list (members))
			exit (1);

		SYSLOG((LOG_INFO,"set members of %s to %s\n",group,members));
#ifdef	SHADOWGRP
		sgent.sg_mem = comma_to_list (members);
		grent.gr_mem = comma_to_list (members);

		if (! Aflg)
			goto output;
#else
		grent.gr_mem = comma_to_list (members);

		goto output;
#endif
	}

#ifdef	SHADOWGRP

	/*
	 * Replacing the entire list of administators is simple.  Check the
	 * list to make sure everyone is a real user.  Then slap the new
	 * list in place.
	 */

	if (Aflg) {

		/*
		 * Only root can replace the entire list.
		 */

		if (! amroot)
			goto failure;

		/*
		 * Check the list for validity, then put it in.
		 */

		if (check_list (admins))
			exit (1);

		SYSLOG((LOG_INFO, "set administrators of %s to %s\n",
				group, admins));
		sgent.sg_adm = comma_to_list (admins);

		goto output;
	}
#endif

	/*
	 * A new password is to be entered and it must be encrypted,
	 * etc.  The password will be prompted for twice, and both
	 * entries must be identical.  There is no need to validate
	 * the old password since the invoker is either the group
	 * owner, or root.
	 */

	printf ("Changing the password for group %s\n", group);

	for (retries = 0;retries < RETRIES;retries++) {
		if (! (cp = getpass ("New Password:")))
			exit (1);

		STRFCPY(pass, cp);
		bzero(cp, strlen (cp));
		if (! (cp = getpass ("Re-enter new password:")))
			exit (1);

		if (strcmp(pass, cp) == 0) {
			bzero(cp, strlen(cp));
			break;
		}

		bzero(cp, strlen (cp));
		bzero(pass, sizeof pass);

		if (retries + 1 < RETRIES)
			puts ("They don't match; try again");
	}

	if (retries == RETRIES) {
		fprintf (stderr, "%s: Try again later\n", Prog);
		exit (1);
	}

	cp = pw_encrypt(pass, crypt_make_salt());
#ifdef	SHADOWGRP
	sgent.sg_passwd = cp;
#else
	grent.gr_passwd = cp;
#endif
	bzero (pass, sizeof pass);
	SYSLOG((LOG_INFO, "change the password for group %s\n", group));

	/*
	 * This is the common arrival point to output the new group
	 * file.  The freshly crafted entry is in allocated space.
	 * The group file will be locked and opened for writing.  The
	 * new entry will be output, etc.
	 */

output:
	signal (SIGHUP, SIG_IGN);
	signal (SIGINT, SIG_IGN);
	signal (SIGQUIT, SIG_IGN);
#ifdef	SIGTSTP
	signal (SIGTSTP, SIG_IGN);
#endif

	if (! gr_lock ()) {
		fprintf (stderr, "%s: can't get lock\n", Prog);
		SYSLOG((LOG_WARN, "failed to get lock for /etc/group\n"));
		exit (1);
	}
#ifdef	SHADOWGRP
	if (is_shadowgrp && ! sgr_lock ()) {
		fprintf (stderr, "%s: can't get shadow lock\n", Prog);
		SYSLOG((LOG_WARN, "failed to get lock for /etc/gshadow\n"));
		exit (1);
	}
#endif
	if (! gr_open (O_RDWR)) {
		fprintf (stderr, "%s: can't open file\n", Prog);
		SYSLOG((LOG_WARN, "cannot open /etc/group\n"));
		exit (1);
	}
#ifdef	SHADOWGRP
	if (is_shadowgrp && ! sgr_open (O_RDWR)) {
		fprintf (stderr, "%s: can't open shadow file\n", Prog);
		SYSLOG((LOG_WARN, "cannot open /etc/gshadow\n"));
		exit (1);
	}
#endif
	if (! gr_update (&grent)) {
		fprintf (stderr, "%s: can't update entry\n", Prog);
		SYSLOG((LOG_WARN, "cannot update /etc/group\n"));
		exit (1);
	}
#ifdef	SHADOWGRP
	if (is_shadowgrp && ! sgr_update (&sgent)) {
		fprintf (stderr, "%s: can't update shadow entry\n", Prog);
		SYSLOG((LOG_WARN, "cannot update /etc/gshadow\n"));
		exit (1);
	}
#endif
	if (! gr_close ()) {
		fprintf (stderr, "%s: can't re-write file\n", Prog);
		SYSLOG((LOG_WARN, "cannot re-write /etc/group\n"));
		exit (1);
	}
#ifdef	SHADOWGRP
	if (is_shadowgrp && ! sgr_close ()) {
		fprintf (stderr, "%s: can't re-write shadow file\n", Prog);
		SYSLOG((LOG_WARN, "cannot re-write /etc/gshadow\n"));
		exit (1);
	}
	if (is_shadowgrp)
		sgr_unlock ();
#endif
	if (! gr_unlock ()) {
		fprintf (stderr, "%s: can't unlock file\n", Prog);
		exit (1);
	}
#ifdef	NDBM
	if (gr_dbm_present() && ! gr_dbm_update (&grent)) {
		fprintf (stderr, "%s: can't update DBM files\n", Prog);
		SYSLOG((LOG_WARN, "cannot update /etc/group DBM files\n"));
		exit (1);
	}
	endgrent ();
#ifdef	SHADOWGRP
	if (is_shadowgrp && sg_dbm_present() && ! sg_dbm_update (&sgent)) {
		fprintf (stderr, "%s: can't update DBM shadow files\n", Prog);
		SYSLOG((LOG_WARN, "cannot update /etc/gshadow DBM files\n"));
		exit (1);
	}
	endsgent ();
#endif
#endif
	exit (0);
	/*NOTREACHED*/

failure:
	fprintf (stderr, "Permission denied.\n");
	exit (1);
	/*NOTREACHED*/
}
