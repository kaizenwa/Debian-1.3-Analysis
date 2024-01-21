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

/*
 * id - print current process user identification information
 *
 *	Print the current process identifiers.  This includes the
 *	UID, GID, effective-UID and effective-GID.  Optionally print
 *	the concurrent group set if the current system supports it.
 */

#include <config.h>

#include "rcsid.h"
RCSID("$Id: id.c,v 1.1.1.1 1996/08/10 07:59:52 marekm Exp $")

#include <sys/types.h>
#include <stdio.h>
#include <grp.h>
#include <pwd.h>
#include "defines.h"

static void
usage()
{
#ifdef HAVE_GETGROUPS
	fprintf (stderr, "usage: id [ -a ]\n");
#else
	fprintf (stderr, "usage: id\n");
#endif
	exit (1);
}

/*ARGSUSED*/
int
main(argc, argv)
	int argc;
	char **argv;
{
	int	id;

/*
 * This block of declarations is particularly strained because of several
 * different ways of doing concurrent groups.  Old BSD systems used int
 * for gid's, but short for the type passed to getgroups().  Newer systems
 * use gid_t for everything.  Some systems have a small and fixed NGROUPS,
 * usually about 16 or 32.  Others use bigger values.
 */

#ifdef HAVE_GETGROUPS
	GETGROUPS_T groups[NGROUPS_MAX];
	int	ngroups;
	int	aflg = 0;
#endif
	struct	passwd	*pw;
	struct	group	*gr;

#ifdef HAVE_GETGROUPS
	/*
	 * See if the -a flag has been given to print out the
	 * concurrent group set.
	 */

	if (argc > 1) {
		if (argc > 2 || strcmp (argv[1], "-a"))
			usage ();
		else
			aflg = 1;
	}
#else
	if (argc > 1)
		usage ();
#endif

	/*
	 * Print out the real user ID and group ID.  If the user or
	 * group does not exist, just give the numerical value.
	 */

	if ((pw = getpwuid (id = getuid ())))
		printf ("uid=%d(%s)", id, pw->pw_name);
	else
		printf ("uid=%d", id);

	if ((gr = getgrgid (id = getgid ())))
		printf (" gid=%d(%s)", id, gr->gr_name);
	else
		printf (" gid=%d", id);

	/*
	 * Print out the effective user ID and group ID if they are
	 * different from the real values.
	 */

	if (getuid () != geteuid ()) {
		if ((pw = getpwuid (id = geteuid ())))
			printf (" euid=%d(%s)", id, pw->pw_name);
		else
			printf (" euid=%d", id);
	}
	if (getgid () != getegid ()) {
		if ((gr = getgrgid (id = getegid ())))
			printf (" egid=%d(%s)", id, gr->gr_name);
		else
			printf (" egid=%d", id);
	}

#ifdef HAVE_GETGROUPS
	/*
	 * Print out the concurrent group set if the user has requested
	 * it.  The group numbers will be printed followed by their
	 * names.
	 */

	if (aflg && (ngroups = getgroups (NGROUPS_MAX, groups)) != -1) {
		int	i;

		/*
		 * Start off the group message.  It will be of the format
		 *
		 *	groups=###(aaa),###(aaa),###(aaa)
		 *
		 * where "###" is a numerical value and "aaa" is the
		 * corresponding name for each respective numerical value.
		 */

		printf (" groups=");
		for (i = 0;i < ngroups;i++) {
			if (i)
				putchar (',');

			if ((gr = getgrgid (groups[i])))
				printf ("%d(%s)", (int) groups[i], gr->gr_name);
			else
				printf ("%d", (int) groups[i]);
		}
	}
#endif

	/*
	 * Finish off the line.
	 */

	putchar ('\n');
	exit (0);
	/*NOTREACHED*/
}
