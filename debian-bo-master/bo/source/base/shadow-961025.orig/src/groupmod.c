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
RCSID("$Id: groupmod.c,v 1.3 1996/09/25 03:20:01 marekm Exp $")

#include <sys/types.h>
#include <stdio.h>
#include <grp.h>
#include <ctype.h>
#include <fcntl.h>

#include "prototypes.h"
#include "defines.h"

#include "groupio.h"

#ifdef	SHADOWGRP
#include "sgroupio.h"

static int is_shadow_grp;
#endif

static char	*group_name;
static char	*group_newname;
static gid_t	group_id;
static gid_t	group_newid;

static char	*Prog;

static int
	oflg = 0, /* permit non-unique group ID to be specified with -g */
	gflg = 0, /* new ID value for the group */
	nflg = 0; /* a new name has been specified for the group */

#ifdef	NDBM
extern	int	gr_dbm_mode;
extern	int	sg_dbm_mode;
#endif

/*
 * usage - display usage message and exit
 */

static void
usage()
{
	fprintf (stderr, "usage: groupmod [-g gid [-o]] [-n name] group\n");
	exit (2);
}

/*
 * new_grent - updates the values in a group file entry
 *
 *	new_grent() takes all of the values that have been entered and
 *	fills in a (struct group) with them.
 */

static void
new_grent(grent)
	struct group *grent;
{
	if (nflg)
		grent->gr_name = xstrdup (group_newname);

	if (gflg)
		grent->gr_gid = group_newid;
}

#ifdef	SHADOWGRP
/*
 * new_sgent - updates the values in a shadow group file entry
 *
 *	new_sgent() takes all of the values that have been entered and
 *	fills in a (struct sgrp) with them.
 */

static void
new_sgent(sgent)
	struct sgrp *sgent;
{
	if (nflg)
		sgent->sg_name = xstrdup (group_newname);
}
#endif	/* SHADOWGRP */

/*
 * grp_update - update group file entries
 *
 *	grp_update() writes the new records to the group files.
 */

static void
grp_update()
{
	struct group grp;
	const struct group *ogrp;
#ifdef	SHADOWGRP
	struct sgrp sgrp;
	const struct sgrp *osgrp = NULL;
#endif	/* SHADOWGRP */

	/*
	 * Get the current settings for this group.
	 */

	ogrp = gr_locate(group_name);
	if (!ogrp) {
		fprintf(stderr, "group not found\n");
		exit(10);
	}
	grp = *ogrp;
	new_grent (&grp);
#ifdef	SHADOWGRP
	if (is_shadow_grp && (osgrp = sgr_locate(group_name))) {
		sgrp = *osgrp;
		new_sgent (&sgrp);
	}
#endif	/* SHADOWGRP */

	/*
	 * Write out the new group file entry.
	 */

	if (! gr_update (&grp)) {
		fprintf (stderr, "%s: error adding new group entry\n", Prog);
		exit (10);
	}
	if (nflg && ! gr_remove (group_name)) {
		fprintf (stderr, "%s: error removing group entry\n", Prog);
		exit (10);
	}
#ifdef	NDBM

	/*
	 * Update the DBM group file with the new entry as well.
	 */

	if (gr_dbm_present()) {
		if (! gr_dbm_update (&grp)) {
			fprintf (stderr, "%s: cannot add new dbm group entry\n",
				Prog);
			exit (10);
		}
		if (nflg && (ogrp = getgrnam (group_name)) &&
				! gr_dbm_remove (ogrp)) {
			fprintf (stderr, "%s: error removing group dbm entry\n",
				Prog);
			exit (10);
		}
		endgrent ();
	}
#endif	/* NDBM */

#ifdef	SHADOWGRP

	/*
	 * Make sure there was a shadow entry to begin with.  Skip
	 * down to "out" if there wasn't.  Can't just return because
	 * there might be some syslogging to do.
	 */

	if (! osgrp)
		goto out;

	/*
	 * Write out the new shadow group entries as well.
	 */

	if (! sgr_update (&sgrp)) {
		fprintf (stderr, "%s: error adding new group entry\n", Prog);
		exit (10);
	}
	if (nflg && ! sgr_remove (group_name)) {
		fprintf (stderr, "%s: error removing group entry\n", Prog);
		exit (10);
	}
#ifdef	NDBM

	/*
	 * Update the DBM shadow group file with the new entry as well.
	 */

	if (sg_dbm_present()) {
		if (! sg_dbm_update (&sgrp)) {
			fprintf (stderr,
				"%s: cannot add new dbm shadow group entry\n",
				Prog);
			exit (10);
		}
		if (nflg && ! sg_dbm_remove (group_name)) {
			fprintf (stderr,
				"%s: error removing shadow group dbm entry\n",
				Prog);
			exit (10);
		}
		endsgent ();
	}
#endif	/* NDBM */
out:
#endif	/* SHADOWGRP */

	if (nflg)
		SYSLOG((LOG_INFO, "change group `%s' to `%s'\n",
			group_name, group_newname));

	if (gflg)
		SYSLOG((LOG_INFO, "change gid for `%s' to %d\n",
			nflg ? group_newname:group_name, group_newid));
}

/*
 * check_new_gid - check the new GID value for uniqueness
 *
 *	check_new_gid() insures that the new GID value is unique.
 */

static void
check_new_gid()
{
	/*
	 * First, the easy stuff.  If the ID can be duplicated, or if
	 * the ID didn't really change, just return.  If the ID didn't
	 * change, turn off those flags.  No sense doing needless work.
	 */

	if (group_id == group_newid) {
		gflg = 0;
		return;
	}

	if (oflg || ! getgrgid (group_newid))
		return;

	/*
	 * Tell the user what they did wrong.
	 */

	fprintf (stderr, "%s: %d is not a unique gid\n", Prog, group_newid);
	exit (4);
}

/*
 * check_new_name - check the new name for uniqueness
 *
 *	check_new_name() insures that the new name does not exist
 *	already.  You can't have the same name twice, period.
 */

static void
check_new_name()
{
	/*
	 * Make sure they are actually changing the name.
	 */

	if (strcmp (group_name, group_newname) == 0) {
		nflg = 0;
		return;
	}

	if (check_name(group_newname)) {

		/*
		 * If the entry is found, too bad.
		 */

		if (getgrnam (group_newname)) {
			fprintf (stderr, "%s: %s is not a unique name\n",
				Prog, group_newname);
			exit (9);
		}
		return;
	}

	/*
	 * All invalid group names land here.
	 */

	fprintf (stderr, "%s: %s is a not a valid group name\n",
		Prog, group_newname);
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

	while ((arg = getopt (argc, argv, "og:n:")) != EOF) {
		switch (arg) {
			case 'g':
				gflg++;
				group_newid = strtol (optarg, &end, 10);
				if (*end != '\0') {
					fprintf (stderr, "%s: invalid group %s\n",
						Prog, optarg);
					exit (3);
				}
				break;
			case 'n':
				if (strcmp (group_name, optarg)) {
					nflg++;
					group_newname = optarg;
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
		exit (10);
	}
	(void) gr_unlock ();
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_close ()) {
		fprintf (stderr, "%s: cannot rewrite shadow group file\n",
			Prog);
		exit (10);
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
	if (! gr_lock()) {
		fprintf (stderr, "%s: unable to lock group file\n", Prog);
		exit (10);
	}
	if (! gr_open(O_RDWR)) {
		fprintf (stderr, "%s: unable to open group file\n", Prog);
		exit (10);
	}
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_lock()) {
		fprintf (stderr, "%s: unable to lock shadow group file\n",
			Prog);
		exit (10);
	}
	if (is_shadow_grp && ! sgr_open(O_RDWR)) {
		fprintf (stderr, "%s: unable to open shadow group file\n",
			Prog);
		exit (10);
	}
#endif	/* SHADOWGRP */
}

/*
 * main - groupmod command
 *
 *	The syntax of the groupmod command is
 *	
 *	groupmod [ -g gid [ -o ]] [ -n name ] group
 *
 *	The flags are
 *		-g - specify a new group ID value
 *		-o - permit the group ID value to be non-unique
 *		-n - specify a new group name
 */

int
main(argc, argv)
	int argc;
	char **argv;
{
	struct	group	*grp;

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

	if (! (grp = getgrnam (group_name))) {
		fprintf (stderr, "%s: group %s does not exist\n",
			Prog, group_name);
		exit (6);
	} else
		group_id = grp->gr_gid;

#ifdef	USE_NIS

	/*
	 * Now make sure it isn't an NIS group.
	 */

	if (__isgrNIS ()) {
		char	*nis_domain;
		char	*nis_master;

		fprintf (stderr, "%s: group %s is a NIS group\n",
			Prog, group_name);

		if (! yp_get_default_domain (&nis_domain) &&
				! yp_master (nis_domain, "group.byname",
				&nis_master)) {
			fprintf (stderr, "%s: %s is the NIS master\n",
				Prog, nis_master);
		}
		exit (6);
	}
#endif

	if (gflg)
		check_new_gid ();

	if (nflg)
		check_new_name ();

	/*
	 * Do the hard stuff - open the files, create the group entries,
	 * then close and update the files.
	 */

	open_files ();

	grp_update ();

	close_files ();
	exit (0);
	/*NOTREACHED*/
}
