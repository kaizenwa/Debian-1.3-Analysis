/*
 * Copyright 1992 - 1994, John F. Haugh II
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
RCSID("$Id: pwck.c,v 1.3 1996/09/25 03:20:03 marekm Exp $")

#include <stdio.h>
#include <fcntl.h>
#include <grp.h>

#include "prototypes.h"
#include "defines.h"
#include <pwd.h>

#define NEED_PW_FILE_ENTRY
#include "pwio.h"

#ifdef	SHADOWPWD
#define NEED_SPW_FILE_ENTRY
#include "shadowio.h"
#endif

/*
 * Exit codes
 */

#define	E_OKAY		0
#define	E_USAGE		1
#define	E_BADENTRY	2
#define	E_CANTOPEN	3
#define	E_CANTLOCK	4
#define	E_CANTUPDATE	5

/*
 * Message strings
 */

#define CANTOPEN	"%s: cannot open file %s\n"
#define CANTLOCK	"%s: cannot lock file %s\n"
#define CANTUPDATE	"%s: cannot update file %s\n"
#if defined(DBM) || defined(NDBM)
#define CHANGES		"%s: the files have been updated; run mkpasswd\n"
#else
#define CHANGES		"%s: the files have been updated\n"
#endif
#define NOCHANGES	"%s: no changes\n"
#define BADUID		"user %s: bad UID (%d)\n"
#define NOPGROUP	"user %s: no group %d\n"
#define NOHOME		"user %s: directory %s does not exist\n"
#define NOSHELL		"user %s: program %s does not exist\n"
#define BADENTRY	"invalid password file entry\n"
#define PWDUP		"duplicate password entry\n"
#define DELETE		"delete line `%s'? "
#define NO		"No"
#define BADSENTRY	"invalid shadow password file entry\n"
#define SPWDUP		"duplicate shadow password entry\n"
#define SPWNOMATCH	"no matching password file entry\n"

/*
 * Global variables
 */

extern	int	optind;
extern	char	*optarg;
extern	struct	pw_file_entry	*__pwf_head;
extern	int	__pw_changed;

#ifdef	SHADOWPWD
extern	struct	spw_file_entry	*__spwf_head;
extern	int	__sp_changed;
#endif

/*
 * Local variables
 */

char	*Prog;
char	*pwd_file = PASSWD_FILE;
#ifdef	SHADOWPWD
char	*spw_file = SHADOW_FILE;
#endif
char	read_only;

/*
 * usage - print syntax message and exit
 */

static void
usage()
{
#ifdef	SHADOWPWD
	fprintf (stderr, "Usage: %s [ -r ] [ passwd [ shadow ] ]\n", Prog);
#else
	fprintf (stderr, "Usage: %s [ -r ] [ passwd ]\n", Prog);
#endif
	exit (E_USAGE);
}

/*
 * yes_or_no - get answer to question from the user
 */

static int
yes_or_no()
{
	char	buf[BUFSIZ];

	/*
	 * In read-only mode all questions are answered "no".
	 */

	if (read_only) {
		puts (NO);
		return 0;
	}

	/*
	 * Get a line and see what the first character is.
	 */

	if (fgets (buf, BUFSIZ, stdin))
		return buf[0] == 'y' || buf[0] == 'Y';

	return 0;
}

/*
 * pwck - verify password file integrity
 */

int
main(argc, argv)
	int argc;
	char **argv;
{
	int	arg;
	int	errors = 0;
	int	deleted = 0;
	struct	pw_file_entry	*pfe, *tpfe;
	struct	passwd	*pwd;
#ifdef	SHADOWPWD
	struct	spw_file_entry	*spe, *tspe;
	struct	spwd	*spw;
	int	is_shadow = 0;
#endif

	/*
	 * Get my name so that I can use it to report errors.
	 */

	Prog = Basename(argv[0]);

	openlog(Prog, LOG_PID|LOG_CONS|LOG_NOWAIT, LOG_AUTH);

	/*
	 * Parse the command line arguments
	 */

	while ((arg = getopt (argc, argv, "r")) != EOF) {
		if (arg == 'r')
			read_only = 1;
		else if (arg != EOF)
			usage ();
	}

	/*
	 * Make certain we have the right number of arguments
	 */

#ifdef	SHADOWPWD
	if (optind != argc && optind + 1 != argc && optind + 2 != argc)
#else
	if (optind != argc && optind + 1 != argc)
#endif
		usage ();

	/*
	 * If there are two left over filenames, use those as the
	 * password and shadow password filenames.
	 */

	if (optind != argc) {
		pwd_file = argv[optind];
		pw_name (pwd_file);
	}
#ifdef SHADOWPWD
	if (optind + 2 == argc) {
		spw_file = argv[optind + 1];
		spw_name(spw_file);
		is_shadow = 1;
	} else if (optind == argc)
		is_shadow = (access(spw_file, 0) == 0);
#endif

	/*
	 * Lock the files if we aren't in "read-only" mode
	 */

	if (! read_only) {
		if (! pw_lock ()) {
			fprintf (stderr, CANTLOCK, Prog, pwd_file);
			if (optind == argc)
				SYSLOG((LOG_WARN,"cannot lock %s\n",pwd_file));
			closelog();
			exit (E_CANTLOCK);
		}
#ifdef	SHADOWPWD
		if (is_shadow && !spw_lock()) {
			fprintf (stderr, CANTLOCK, Prog, spw_file);
			if (optind == argc)
				SYSLOG((LOG_WARN,"cannot lock %s\n",spw_file));
			closelog();
			exit (E_CANTLOCK);
		}
#endif
	}

	/*
	 * Open the files.  Use O_RDONLY if we are in read_only mode,
	 * O_RDWR otherwise.
	 */

	if (! pw_open (read_only ? O_RDONLY:O_RDWR)) {
		fprintf (stderr, CANTOPEN, Prog, pwd_file);
		if (optind == argc)
			SYSLOG((LOG_WARN, "cannot open %s\n", pwd_file));
		closelog();
		exit (E_CANTOPEN);
	}
#ifdef	SHADOWPWD
	if (is_shadow && !spw_open(read_only ? O_RDONLY:O_RDWR)) {
		fprintf (stderr, CANTOPEN, Prog, spw_file);
		if (optind == argc)
			SYSLOG((LOG_WARN, "cannot open %s\n", spw_file));
		closelog();
		exit (E_CANTOPEN);
	}
#endif

	/*
	 * Loop through the entire password file.
	 */

	for (pfe = __pwf_head;pfe;pfe = pfe->pwf_next) {
#ifdef	USE_NIS
		/*
		 * If this is a NIS line, skip it.  You can't "know" what
		 * NIS is going to do without directly asking NIS ...
		 */

		if (pfe->pwf_line[0] == '+' || pfe->pwf_line[0] == '-')
			continue;
#endif
		/*
		 * Start with the entries that are completely corrupt.
		 * They have no (struct passwd) entry because they couldn't
		 * be parsed properly.
		 */

		if (pfe->pwf_entry == (struct passwd *) 0) {

			/*
			 * Tell the user this entire line is bogus and
			 * ask them to delete it.
			 */

			printf (BADENTRY);
			printf (DELETE, pfe->pwf_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (! yes_or_no ())
				continue;

			/*
			 * All password file deletions wind up here.  This
			 * code removes the current entry from the linked
			 * list.  When done, it skips back to the top of
			 * the loop to try out the next list element.
			 */

delete_pw:
			SYSLOG((LOG_INFO, "delete passwd line `%s'\n",
				pfe->pwf_line));
			deleted++;
			__pw_changed = 1;

			/*
			 * Simple case - delete from the head of the
			 * list.
			 */

			if (pfe == __pwf_head) {
				__pwf_head = pfe->pwf_next;
				continue;
			}

			/*
			 * Hard case - find entry where pwf_next is
			 * the current entry.
			 */

			for (tpfe = __pwf_head;tpfe->pwf_next != pfe;
					tpfe = tpfe->pwf_next)
				;

			tpfe->pwf_next = pfe->pwf_next;
			continue;
		}

		/*
		 * Password structure is good, start using it.
		 */

		pwd = pfe->pwf_entry;

		/*
		 * Make sure this entry has a unique name.
		 */

		for (tpfe = __pwf_head;tpfe;tpfe = tpfe->pwf_next) {

			/*
			 * Don't check this entry
			 */

			if (tpfe == pfe)
				continue;

			/*
			 * Don't check invalid entries.
			 */

			if (tpfe->pwf_entry == (struct passwd *) 0)
				continue;

			if (strcmp (pwd->pw_name, tpfe->pwf_entry->pw_name))
				continue;

			/*
			 * Tell the user this entry is a duplicate of
			 * another and ask them to delete it.
			 */

			puts (PWDUP);
			printf (DELETE, pfe->pwf_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (yes_or_no ())
				goto delete_pw;
		}

		/*
		 * Check for a Slackware bug.  Make sure UID is not -1
		 * (it has special meaning for some syscalls).  --marekm
		 */

		if (pwd->pw_uid == (uid_t) -1) {
			printf(BADUID, pwd->pw_name, (int) pwd->pw_uid);
			errors++;
		}

		/*
		 * Make sure the primary group exists
		 */

		if (! getgrgid (pwd->pw_gid)) {

			/*
			 * No primary group, just give a warning
			 */

			printf (NOPGROUP, pwd->pw_name, (int) pwd->pw_gid);
			errors++;
		}

		/*
		 * Make sure the home directory exists
		 */

		if (access (pwd->pw_dir, 0)) {

			/*
			 * Home directory doesn't exist, give a warning
			 */

			printf (NOHOME, pwd->pw_name, pwd->pw_dir);
			errors++;
		}

		/*
		 * Make sure the login shell is executable
		 */

		if (pwd->pw_shell[0] && access (pwd->pw_shell, 0)) {

			/*
			 * Login shell doesn't exist, give a warning
			 */
			
			printf (NOSHELL, pwd->pw_name, pwd->pw_shell);
			errors++;
		}
	}

#ifdef	SHADOWPWD
	if (!is_shadow)
		goto shadow_done;

	/*
	 * Loop through the entire shadow password file.
	 */

	for (spe = __spwf_head;spe;spe = spe->spwf_next) {
#ifdef	USE_NIS
		/*
		 * If this is a NIS line, skip it.  You can't "know" what
		 * NIS is going to do without directly asking NIS ...
		 */

		if (spe->spwf_line[0] == '+' || spe->spwf_line[0] == '-')
			continue;
#endif

		/*
		 * Start with the entries that are completely corrupt.
		 * They have no (struct spwd) entry because they couldn't
		 * be parsed properly.
		 */

		if (spe->spwf_entry == (struct spwd *) 0) {

			/*
			 * Tell the user this entire line is bogus and
			 * ask them to delete it.
			 */

			printf (BADSENTRY);
			printf (DELETE, spe->spwf_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (! yes_or_no ())
				continue;

			/*
			 * All shadow file deletions wind up here.  This
			 * code removes the current entry from the linked
			 * list.  When done, it skips back to the top of
			 * the loop to try out the next list element.
			 */

delete_spw:
			SYSLOG((LOG_INFO, "delete shadow line `%s'\n",
				spe->spwf_line));
			deleted++;
			__sp_changed = 1;

			/*
			 * Simple case - delete from the head of the
			 * list.
			 */

			if (spe == __spwf_head) {
				__spwf_head = spe->spwf_next;
				continue;
			}

			/*
			 * Hard case - find entry where spwf_next is
			 * the current entry.
			 */

			for (tspe = __spwf_head;tspe->spwf_next != spe;
					tspe = spe->spwf_next)
				;

			tspe->spwf_next = spe->spwf_next;
			continue;
		}

		/*
		 * Shadow password structure is good, start using it.
		 */

		spw = spe->spwf_entry;

		/*
		 * Make sure this entry has a unique name.
		 */

		for (tspe = __spwf_head;tspe;tspe = tspe->spwf_next) {

			/*
			 * Don't check this entry
			 */

			if (tspe == spe)
				continue;

			/*
			 * Don't check invalid entries.
			 */

			if (tspe->spwf_entry == (struct spwd *) 0)
				continue;

			if (strcmp (spw->sp_namp, tspe->spwf_entry->sp_namp))
				continue;

			/*
			 * Tell the user this entry is a duplicate of
			 * another and ask them to delete it.
			 */

			puts (SPWDUP);
			printf (DELETE, spe->spwf_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (yes_or_no ())
				goto delete_spw;
		}

		/*
		 * Make sure this entry exists in the /etc/passwd
		 * file.
		 */

		if (! pw_locate (spw->sp_namp)) {

			/*
			 * Tell the user this entry has no matching
			 * /etc/passwd entry and ask them to delete it.
			 */

			puts (SPWNOMATCH);
			printf (DELETE, spe->spwf_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (yes_or_no ())
				goto delete_spw;
		}
	}

shadow_done:
#endif

	/*
	 * All done.  If there were no deletions we can just abandon any
	 * changes to the files.
	 */

	if (deleted) {
		if (! pw_close ()) {
			fprintf (stderr, CANTUPDATE, Prog, pwd_file);
			SYSLOG((LOG_WARN, "cannot update %s\n", pwd_file));
			closelog();
			exit (E_CANTUPDATE);
		}
#ifdef	SHADOWPWD
		if (is_shadow && !spw_close()) {
			fprintf (stderr, CANTUPDATE, Prog, spw_file);
			SYSLOG((LOG_WARN, "cannot update %s\n", spw_file));
			closelog();
			exit (E_CANTUPDATE);
		}
#endif
	}

	/*
	 * Don't be anti-social - unlock the files when you're done.
	 */

#ifdef	SHADOWPWD
	if (is_shadow)
		spw_unlock ();
#endif
	(void) pw_unlock ();

	/*
	 * Tell the user what we did and exit.
	 */

	if (errors)
		printf (deleted ? CHANGES:NOCHANGES, Prog);

	closelog();
	exit (errors ? E_BADENTRY:E_OKAY);
}
