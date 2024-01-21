/*
 * Copyright 1991 - 1993, John F. Haugh II
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
RCSID("$Id: groupadd.c,v 1.3 1996/09/25 03:20:01 marekm Exp $")

#include <sys/types.h>
#include <stdio.h>
#include <grp.h>
#include <ctype.h>
#include <fcntl.h>

#include "defines.h"
#include "prototypes.h"

#include "getdef.h"

#include "groupio.h"

#ifdef	SHADOWGRP
#include "sgroupio.h"

static int is_shadow_grp;
#endif

static char	*group_name;
static gid_t	group_id;
static char *empty_list = NULL;

static char	*Prog;

static int
	oflg = 0, /* permit non-unique group ID to be specified with -g */
	gflg = 0; /* ID value for the new group */

#ifdef	NDBM
extern	int	gr_dbm_mode;
extern	int	sg_dbm_mode;
#endif

static	void	fail_exit ();

/*
 * usage - display usage message and exit
 */

static void
usage()
{
	fprintf (stderr, "usage: groupadd [-g gid [-o]] group\n");
	exit (2);
}

/*
 * new_grent - initialize the values in a group file entry
 *
 *	new_grent() takes all of the values that have been entered and
 *	fills in a (struct group) with them.
 */

static void
new_grent(grent)
	struct group *grent;
{
	bzero ((char *) grent, sizeof *grent);
	grent->gr_name = group_name;
	grent->gr_passwd = "x";
	grent->gr_gid = group_id;
	grent->gr_mem = &empty_list;
}

#ifdef	SHADOWGRP
/*
 * new_sgent - initialize the values in a shadow group file entry
 *
 *	new_sgent() takes all of the values that have been entered and
 *	fills in a (struct sgrp) with them.
 */

static void
new_sgent(sgent)
	struct sgrp *sgent;
{
	bzero ((char *) sgent, sizeof *sgent);
	sgent->sg_name = group_name;
	sgent->sg_passwd = "!";
	sgent->sg_adm = &empty_list;
	sgent->sg_mem = &empty_list;
}
#endif	/* SHADOWGRP */

/*
 * grp_update - add new group file entries
 *
 *	grp_update() writes the new records to the group files.
 */

static void
grp_update()
{
	struct	group	grp;
#ifdef	SHADOWGRP
	struct	sgrp	sgrp;
#endif	/* SHADOWGRP */

	/*
	 * Create the initial entries for this new group.
	 */

	new_grent (&grp);
#ifdef	SHADOWGRP
	new_sgent (&sgrp);
#endif	/* SHADOWGRP */

	/*
	 * Write out the new group file entry.
	 */

	if (! gr_update (&grp)) {
		fprintf (stderr, "%s: error adding new group entry\n", Prog);
		fail_exit (10);
	}
#ifdef	NDBM

	/*
	 * Update the DBM group file with the new entry as well.
	 */

	if (gr_dbm_present() && ! gr_dbm_update (&grp)) {
		fprintf (stderr, "%s: cannot add new dbm group entry\n", Prog);
		fail_exit (10);
	}
	endgrent ();
#endif	/* NDBM */

#ifdef	SHADOWGRP

	/*
	 * Write out the new shadow group entries as well.
	 */

	if (is_shadow_grp && ! sgr_update (&sgrp)) {
		fprintf (stderr, "%s: error adding new group entry\n", Prog);
		fail_exit (10);
	}
#ifdef	NDBM

	/*
	 * Update the DBM group file with the new entry as well.
	 */

	if (is_shadow_grp && sg_dbm_present() && ! sg_dbm_update (&sgrp)) {
		fprintf (stderr, "%s: cannot add new dbm group entry\n", Prog);
		fail_exit (10);
	}
	endsgent ();
#endif	/* NDBM */
#endif	/* SHADOWGRP */
	SYSLOG((LOG_INFO, "new group: name=%s, gid=%d\n",
		group_name, group_id));
}

/*
 * find_new_gid - find the next available GID
 *
 *	find_new_gid() locates the next highest unused GID in the group
 *	file, or checks the given group ID against the existing ones for
 *	uniqueness.
 */

static void
find_new_gid()
{
	const struct group *grp;
	gid_t gid_min, gid_max;

	gid_min = getdef_num("GID_MIN", 100);
	gid_max = getdef_num("GID_MAX", 60000);

	/*
	 * Start with some GID value if the user didn't provide us with
	 * one already.
	 */

	if (! gflg)
		group_id = gid_min;

	/*
	 * Search the entire group file, either looking for this
	 * GID (if the user specified one with -g) or looking for the
	 * largest unused value.
	 */

	for (gr_rewind (), grp = gr_next ();grp;grp = gr_next ()) {
		if (strcmp (group_name, grp->gr_name) == 0) {
			fprintf (stderr, "%s: name %s is not unique\n",
				Prog, group_name);
			fail_exit (9);
		}
		if (gflg && group_id == grp->gr_gid) {
			fprintf (stderr, "%s: gid %d is not unique\n",
				Prog, group_id);
			fail_exit (4);
		}
		if (! gflg && grp->gr_gid >= group_id) {
			if (grp->gr_gid > gid_max)
				continue;
			group_id = grp->gr_gid + 1;
		}
	}
	if (!gflg && group_id == gid_max + 1) {
		group_id = gid_min;
		gr_rewind();
		while ((grp = gr_next())) {
			if (grp->gr_gid != group_id)
				continue;
			if (group_id == gid_max) {
				fprintf(stderr, "%s: can't get unique gid\n",
					Prog);
				fail_exit(4);
			}
			group_id++;
			gr_rewind();
		}
	}
}

/*
 * check_new_name - check the new name for validity
 *
 *	check_new_name() insures that the new name doesn't contain
 *	any illegal characters.
 */

static void
check_new_name()
{
	if (check_name(group_name))
		return;

	/*
	 * All invalid group names land here.
	 */

	fprintf (stderr, "%s: %s is a not a valid group name\n",
		Prog, group_name);

	exit (3);
}

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
	char	*end;
	int	arg;

	while ((arg = getopt (argc, argv, "og:")) != EOF) {
		switch (arg) {
			case 'g':
				gflg++;
				if (! isdigit (optarg[0]))
					usage ();

				group_id = strtol (optarg, &end, 10);
				if (*end != '\0') {
					fprintf (stderr, "%s: invalid group %s\n",
						Prog, optarg);
					fail_exit (3);
				}
				break;
			case 'o':
				if (! gflg)
					usage ();

				oflg++;
				break;
			default:
				usage ();
		}
	}
	if (optind != argc - 1)
		usage ();

	group_name = argv[argc - 1];
	check_new_name ();
}

/*
 * close_files - close all of the files that were opened
 *
 *	close_files() closes all of the files that were opened for this
 *	new group.  This causes any modified entries to be written out.
 */

static void
close_files()
{
	if (! gr_close ()) {
		fprintf (stderr, "%s: cannot rewrite group file\n", Prog);
		fail_exit (10);
	}
	(void) gr_unlock ();
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_close ()) {
		fprintf (stderr, "%s: cannot rewrite shadow group file\n",
			Prog);
		fail_exit (10);
	}
	if (is_shadow_grp)
		sgr_unlock ();
#endif	/* SHADOWGRP */
}

/*
 * open_files - lock and open the group files
 *
 *	open_files() opens the two group files.
 */

static void
open_files()
{
	if (! gr_lock ()) {
		fprintf (stderr, "%s: unable to lock group file\n", Prog);
		exit (10);
	}
	if (! gr_open (O_RDWR)) {
		fprintf (stderr, "%s: unable to open group file\n", Prog);
		fail_exit (10);
	}
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_lock ()) {
		fprintf (stderr, "%s: unable to lock shadow group file\n",
			Prog);
		fail_exit (10);
	}
	if (is_shadow_grp && ! sgr_open (O_RDWR)) {
		fprintf (stderr, "%s: unable to open shadow group file\n",
			Prog);
		fail_exit (10);
	}
#endif	/* SHADOWGRP */
}

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
	exit (code);
}

/*
 * main - groupadd command
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

#ifdef SHADOWGRP
	is_shadow_grp = (access(SGROUP_FILE, 0) == 0);
#endif

	/*
	 * The open routines for the DBM files don't use read-write
	 * as the mode, so we have to clue them in.
	 */

#ifdef	NDBM
	gr_dbm_mode = O_RDWR;
#ifdef	SHADOWGRP
	sg_dbm_mode = O_RDWR;
#endif	/* SHADOWGRP */
#endif	/* NDBM */
	process_flags (argc, argv);

	/*
	 * Start with a quick check to see if the group exists.
	 */

	if (getgrnam (group_name)) {
		fprintf (stderr, "%s: group %s exists\n", Prog, group_name);
		exit (9);
	}

	/*
	 * Do the hard stuff - open the files, create the group entries,
	 * then close and update the files.
	 */

	open_files ();

	if (! gflg || ! oflg)
		find_new_gid ();

	grp_update ();

	close_files ();
	exit (0);
	/*NOTREACHED*/
}
