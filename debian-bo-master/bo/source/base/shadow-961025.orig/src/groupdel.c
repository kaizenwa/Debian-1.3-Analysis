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
RCSID("$Id: groupdel.c,v 1.3 1996/09/25 03:20:01 marekm Exp $")

#include <sys/types.h>
#include <stdio.h>
#include <grp.h>
#include <ctype.h>
#include <fcntl.h>
#include <pwd.h>

#include "prototypes.h"
#include "defines.h"

static char	*group_name;
static char	*Prog;
static int	errors;

#ifdef	NDBM
extern	int	gr_dbm_mode;
extern	int	sg_dbm_mode;
#endif

#include "groupio.h"

#ifdef	SHADOWGRP
#include "sgroupio.h"

static int is_shadow_grp;
#endif

/*
 * usage - display usage message and exit
 */

static void
usage()
{
	fprintf (stderr, "usage: groupdel group\n");
	exit (2);
}

/*
 * grp_update - update group file entries
 *
 *	grp_update() writes the new records to the group files.
 */

static void
grp_update()
{
#ifdef	NDBM
	struct	group	*ogrp;
#endif

	if (! gr_remove (group_name)) {
		fprintf (stderr, "%s: error removing group entry\n", Prog);
		errors++;
	}
#ifdef	NDBM

	/*
	 * Update the DBM group file
	 */

	if (gr_dbm_present()) {
		if ((ogrp = getgrnam (group_name)) &&
				! gr_dbm_remove (ogrp)) {
			fprintf (stderr, "%s: error removing group dbm entry\n",
				Prog);
			errors++;
		}
	}
	endgrent ();
#endif	/* NDBM */

#ifdef	SHADOWGRP

	/*
	 * Delete the shadow group entries as well.
	 */

	if (is_shadow_grp && ! sgr_remove (group_name)) {
		fprintf (stderr, "%s: error removing shadow group entry\n",
			Prog);
		errors++;
	}
#ifdef	NDBM

	/*
	 * Update the DBM shadow group file
	 */

	if (is_shadow_grp && sg_dbm_present()) {
		if (! sg_dbm_remove (group_name)) {
			fprintf (stderr,
				"%s: error removing shadow group dbm entry\n",
				Prog);
			errors++;
		}
	}
	endsgent ();
#endif	/* NDBM */
#endif	/* SHADOWGRP */
	SYSLOG((LOG_INFO, "remove group `%s'\n", group_name));
	return;
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
		errors++;
	}
	(void) gr_unlock ();
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_close ()) {
		fprintf (stderr, "%s: cannot rewrite shadow group file\n",
			Prog);
		errors++;
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
		exit (10);
	}
#ifdef	SHADOWGRP
	if (is_shadow_grp && ! sgr_lock ()) {
		fprintf (stderr, "%s: unable to lock shadow group file\n",
			Prog);
		exit (10);
	}
	if (is_shadow_grp && ! sgr_open (O_RDWR)) {
		fprintf (stderr, "%s: unable to open shadow group file\n",
			Prog);
		exit (10);
	}
#endif	/* SHADOWGRP */
}

/*
 * group_busy - check if this is any user's primary group
 *
 *	group_busy verifies that this group is not the primary group
 *	for any user.  You must remove all users before you remove
 *	the group.
 */

static void
group_busy(gid)
	gid_t gid;
{
	struct	passwd	*pwd;

	/*
	 * Nice slow linear search.
	 */

	setpwent ();

	while ((pwd = getpwent ()) && pwd->pw_gid != gid)
		;

	endpwent ();

	/*
	 * If pwd isn't NULL, it stopped becaues the gid's matched.
	 */

	if (pwd == (struct passwd *) 0)
		return;

	/*
	 * Can't remove the group.
	 */

	fprintf (stderr, "%s: cannot remove user's primary group.\n", Prog);
	exit (1);
}

/*
 * main - groupdel command
 *
 *	The syntax of the groupdel command is
 *	
 *	groupdel group
 *
 *	The named group will be deleted.
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

	if (argc != 2)
		usage ();

	group_name = argv[1];

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

	/*
	 * Start with a quick check to see if the group exists.
	 */

	if (! (grp = getgrnam (group_name))) {
		fprintf (stderr, "%s: group %s does not exist\n",
			Prog, group_name);
		exit (6);
	}
#ifdef	USE_NIS

	/*
	 * Make sure this isn't a NIS group
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

	/*
	 * Now check to insure that this isn't the primary group of
	 * anyone.
	 */

	group_busy (grp->gr_gid);

	/*
	 * Do the hard stuff - open the files, delete the group entries,
	 * then close and update the files.
	 */

	open_files ();

	grp_update ();

	close_files ();
	exit (errors == 0 ? 0:1);
	/*NOTREACHED*/
}
