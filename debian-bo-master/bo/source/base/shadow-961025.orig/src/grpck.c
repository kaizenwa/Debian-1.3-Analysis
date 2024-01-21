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
RCSID("$Id: grpck.c,v 1.3 1996/09/25 03:20:02 marekm Exp $")

#include <stdio.h>
#include <fcntl.h>
#include <grp.h>

#include "prototypes.h"
#include "defines.h"
#include <pwd.h>

#define NEED_GR_FILE_ENTRY
#include "groupio.h"

#ifdef	SHADOWGRP
#define NEED_SG_FILE_ENTRY
#include "sgroupio.h"
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
#define BADGID		"group %s: bad GID (%d)\n"
#define NOUSER		"group %s: no user %s\n"
#define BADENTRY	"invalid group file entry\n"
#define GRDUP		"duplicate group entry\n"
#define DELETE		"delete line `%s'? "
#define DELMEM		"delete member `%s'? "
#define NO		"No"
#define NOSUSER		"shadow group %s: no user %s\n"
#define NOSADMUSER	"shadow group %s: no administrative user %s\n"
#define BADSENTRY	"invalid shadow group file entry\n"
#define SGRDUP		"duplicate shadow group entry\n"
#define DELADM		"delete administrative member `%s'? "

/*
 * Global variables
 */

extern	int	optind;
extern	char	*optarg;
extern	struct	gr_file_entry	*__grf_head;
extern	int	__gr_changed;
#ifdef	SHADOWGRP
extern	struct	sg_file_entry	*__sgr_head;
extern	int	__sg_changed;
#endif

/*
 * Local variables
 */

static char *Prog;
static char *grp_file = GROUP_FILE;
#ifdef	SHADOWGRP
static char *sgr_file = SGROUP_FILE;
#endif
static char read_only = 0;

/*
 * usage - print syntax message and exit
 */

static void
usage()
{
#ifdef	SHADOWGRP
	fprintf (stderr, "Usage: %s [ -r ] [ group [ gshadow ] ]\n", Prog);
#else
	fprintf (stderr, "Usage: %s [ -r ] [ group ]\n", Prog);
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

	if (fgets (buf, sizeof buf, stdin))
		return buf[0] == 'y' || buf[0] == 'Y';

	return 0;
}

/*
 * delete_member - delete an entry in a list of members
 */

static void
delete_member(list, member)
	char **list;
	char *member;
{
	int	i;

	for (i = 0;list[i];i++)
		if (list[i] == member)
			break;

	if (list[i])
		for (;list[i];i++)
			list[i] = list[i + 1];
}

/*
 * grpck - verify group file integrity
 */

int
main(argc, argv)
	int argc;
	char **argv;
{
	int	arg;
	int	errors = 0;
	int	deleted = 0;
	int	i;
	struct	gr_file_entry	*gre, *tgre;
	struct	group	*grp;
#ifdef	SHADOWGRP
	struct	sg_file_entry	*sge, *tsge;
	struct	sgrp	*sgr;
	int is_shadow = 0;
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

#ifdef	SHADOWGRP
	if (optind != argc && optind + 1 != argc && optind + 2 != argc)
#else
	if (optind != argc && optind + 1 != argc)
#endif
		usage ();

	/*
	 * If there are two left over filenames, use those as the
	 * group and group password filenames.
	 */

	if (optind != argc) {
		grp_file = argv[optind];
		gr_name (grp_file);
	}
#ifdef	SHADOWGRP
	if (optind + 2 == argc) {
		sgr_file = argv[optind + 1];
		sgr_name (sgr_file);
		is_shadow = 1;
	} else if (optind == argc)
		is_shadow = (access(sgr_file, 0) == 0);
#endif

	/*
	 * Lock the files if we aren't in "read-only" mode
	 */

	if (! read_only) {
		if (! gr_lock ()) {
			fprintf (stderr, CANTLOCK, Prog, grp_file);
			if (optind == argc)
				SYSLOG((LOG_WARN,"cannot lock %s\n",grp_file));
			closelog();
			exit (E_CANTLOCK);
		}
#ifdef	SHADOWGRP
		if (is_shadow && !sgr_lock()) {
			fprintf (stderr, CANTLOCK, Prog, sgr_file);
			if (optind == argc)
				SYSLOG((LOG_WARN,"cannot lock %s\n",sgr_file));
			closelog();
			exit (E_CANTLOCK);
		}
#endif
	}

	/*
	 * Open the files.  Use O_RDONLY if we are in read_only mode,
	 * O_RDWR otherwise.
	 */

	if (! gr_open (read_only ? O_RDONLY:O_RDWR)) {
		fprintf (stderr, CANTOPEN, Prog, grp_file);
		if (optind == argc)
			SYSLOG((LOG_WARN, "cannot open %s\n", grp_file));
		closelog();
		exit (E_CANTOPEN);
	}
#ifdef	SHADOWGRP
	if (is_shadow && !sgr_open(read_only ? O_RDONLY:O_RDWR)) {
		fprintf (stderr, CANTOPEN, Prog, sgr_file);
		if (optind == argc)
			SYSLOG((LOG_WARN, "cannot open %s\n", sgr_file));
		closelog();
		exit (E_CANTOPEN);
	}
#endif

	/*
	 * Loop through the entire group file.
	 */

	for (gre = __grf_head;gre;gre = gre->grf_next) {
#ifdef	USE_NIS
		/*
		 * Skip all NIS entries.
		 */

		if (gre->grf_line[0] == '+' || gre->grf_line[0] == '-')
			continue;
#endif
		/*
		 * Start with the entries that are completely corrupt.
		 * They have no (struct group) entry because they couldn't
		 * be parsed properly.
		 */

		if (gre->grf_entry == (struct group *) 0) {

			/*
			 * Tell the user this entire line is bogus and
			 * ask them to delete it.
			 */

			printf (BADENTRY);
			printf (DELETE, gre->grf_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (! yes_or_no())
				continue;

			/*
			 * All group file deletions wind up here.  This
			 * code removes the current entry from the linked
			 * list.  When done, it skips back to the top of
			 * the loop to try out the next list element.
			 */

delete_gr:
			SYSLOG((LOG_INFO, "delete group line `%s'\n",
				gre->grf_line));
			deleted++;
			__gr_changed = 1;

			/*
			 * Simple case - delete from the head of the
			 * list.
			 */

			if (gre == __grf_head) {
				__grf_head = gre->grf_next;
				continue;
			}

			/*
			 * Hard case - find entry where grf_next is
			 * the current entry.
			 */

			for (tgre = __grf_head;tgre->grf_next != gre;
					tgre = tgre->grf_next)
				;

			tgre->grf_next = gre->grf_next;
			continue;
		}

		/*
		 * Group structure is good, start using it.
		 */

		grp = gre->grf_entry;

		/*
		 * Make sure this entry has a unique name.
		 */

		for (tgre = __grf_head;tgre;tgre = tgre->grf_next) {

			/*
			 * Don't check this entry
			 */

			if (tgre == gre)
				continue;

			/*
			 * Don't check invalid entries.
			 */

			if (tgre->grf_entry == (struct group *) 0)
				continue;

			if (strcmp (grp->gr_name, tgre->grf_entry->gr_name))
				continue;

			/*
			 * Tell the user this entry is a duplicate of
			 * another and ask them to delete it.
			 */

			puts (GRDUP);
			printf (DELETE, gre->grf_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (yes_or_no ())
				goto delete_gr;
		}

		/*
		 * Check for a Slackware bug.  Make sure GID is not -1
		 * (it has special meaning for some syscalls).  --marekm
		 */

		if (grp->gr_gid == (gid_t) -1) {
			errors++;
			printf(BADGID, grp->gr_name, (int) grp->gr_gid);
		}

		/*
		 * Make sure each member exists
		 */

		for (i = 0;grp->gr_mem[i];i++) {
			if (getpwnam(grp->gr_mem[i]))
				continue;
			/*
			 * Can't find this user.  Remove them
			 * from the list.
			 */

			errors++;
			printf (NOUSER, grp->gr_name, grp->gr_mem[i]);
			printf (DELMEM, grp->gr_mem[i]);

			if (! yes_or_no())
				continue;

			SYSLOG((LOG_INFO, "delete member `%s' group `%s'\n",
				grp->gr_mem[i], grp->gr_name));
			deleted++;
			delete_member(grp->gr_mem, grp->gr_mem[i]);
			gre->grf_changed = 1;
			__gr_changed = 1;
		}
	}

#ifdef	SHADOWGRP
	if (!is_shadow)
		goto shadow_done;

	/*
	 * Loop through the entire shadow group file.
	 */

	for (sge = __sgr_head;sge;sge = sge->sgr_next) {

		/*
		 * Start with the entries that are completely corrupt.
		 * They have no (struct sgrp) entry because they couldn't
		 * be parsed properly.
		 */

		if (sge->sgr_entry == (struct sgrp *) 0) {

			/*
			 * Tell the user this entire line is bogus and
			 * ask them to delete it.
			 */

			printf (BADSENTRY);
			printf (DELETE, sge->sgr_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (! yes_or_no ())
				continue;

			/*
			 * All shadow group file deletions wind up here.
			 * This code removes the current entry from the
			 * linked list.  When done, it skips back to the
			 * top of the loop to try out the next list element.
			 */

delete_sg:
			SYSLOG((LOG_INFO, "delete shadow line `%s'\n",
				sge->sgr_line));
			deleted++;
			__sg_changed = 1;

			/*
			 * Simple case - delete from the head of the
			 * list.
			 */

			if (sge == __sgr_head) {
				__sgr_head = sge->sgr_next;
				continue;
			}

			/*
			 * Hard case - find entry where sgr_next is
			 * the current entry.
			 */

			for (tsge = __sgr_head;tsge->sgr_next != sge;
					tsge = tsge->sgr_next)
				;

			tsge->sgr_next = sge->sgr_next;
			continue;
		}

		/*
		 * Shadow group structure is good, start using it.
		 */

		sgr = sge->sgr_entry;

		/*
		 * Make sure this entry has a unique name.
		 */

		for (tsge = __sgr_head;tsge;tsge = tsge->sgr_next) {

			/*
			 * Don't check this entry
			 */

			if (tsge == sge)
				continue;

			/*
			 * Don't check invalid entries.
			 */

			if (tsge->sgr_entry == (struct sgrp *) 0)
				continue;

			if (strcmp (sgr->sg_name, tsge->sgr_entry->sg_name))
				continue;

			/*
			 * Tell the user this entry is a duplicate of
			 * another and ask them to delete it.
			 */

			puts (SGRDUP);
			printf (DELETE, sge->sgr_line);
			errors++;

			/*
			 * prompt the user to delete the entry or not
			 */

			if (yes_or_no ())
				goto delete_sg;
		}

		/*
		 * Make sure each administrator exists
		 */

		for (i = 0;sgr->sg_adm[i];i++) {
			if (getpwnam(sgr->sg_adm[i]))
				continue;
			/*
			 * Can't find this user.  Remove them
			 * from the list.
			 */

			errors++;
			printf (NOSADMUSER, sgr->sg_name, sgr->sg_adm[i]);
			printf (DELADM, sgr->sg_adm[i]);

			if (! yes_or_no())
				continue;

			SYSLOG((LOG_INFO,
				"delete admin `%s' from shadow group `%s'\n",
				sgr->sg_adm[i], sgr->sg_name));
			deleted++;
			delete_member(sgr->sg_adm, sgr->sg_adm[i]);
			sge->sgr_changed = 1;
			__sg_changed = 1;
		}

		/*
		 * Make sure each member exists
		 */

		for (i = 0;sgr->sg_mem[i];i++) {
			if (getpwnam(sgr->sg_mem[i]))
				continue;

			/*
			 * Can't find this user.  Remove them
			 * from the list.
			 */

			errors++;
			printf (NOUSER, sgr->sg_name, sgr->sg_mem[i]);
			printf (DELMEM, sgr->sg_mem[i]);

			if (! yes_or_no())
				continue;

			SYSLOG((LOG_INFO,
				"delete member `%s' from shadow group `%s'\n",
				sgr->sg_mem[i], sgr->sg_name));
			deleted++;
			delete_member(sgr->sg_mem, sgr->sg_mem[i]);
			sge->sgr_changed = 1;
			__sg_changed = 1;
		}
	}

shadow_done:
#endif	/* SHADOWGRP */

	/*
	 * All done.  If there were no deletions we can just abandon any
	 * changes to the files.
	 */

	if (deleted) {
		if (! gr_close ()) {
			fprintf (stderr, CANTUPDATE, Prog, grp_file);
			exit (E_CANTUPDATE);
		}
#ifdef	SHADOWGRP
		if (is_shadow && !sgr_close()) {
			fprintf (stderr, CANTUPDATE, Prog, sgr_file);
			exit (E_CANTUPDATE);
		}
#endif
	}

	/*
	 * Don't be anti-social - unlock the files when you're done.
	 */

#ifdef	SHADOWGRP
	if (is_shadow)
		sgr_unlock ();
#endif
	(void) gr_unlock ();

	/*
	 * Tell the user what we did and exit.
	 */

	if (errors)
		printf (deleted ? CHANGES:NOCHANGES, Prog);

	exit (errors ? E_BADENTRY:E_OKAY);
}
