#ifndef lint
static char	sccsid[] = "@(#)group.c	1.12	(Berkeley) 5/11/89";
#endif

#include "common.h"

#ifdef XTHREAD
extern char *thread_name();
#endif
#if defined(XOVER) || defined(XROVER)
extern void close_xfiles();
#endif

extern char *malloc();

/*
 * GROUP newsgroup
 *
 * Change the current group to the specified newsgroup.
 * We also change our current directory to that newsgroup if
 * a spool directory for it exists.
 * If the newsgroup specified is invalid, the old newsgroup
 * remains selected.
 */

void
group(argc, argv)
	int	argc;
	char	*argv[];
{
	char	temp_dir[256];
	int	high_msg, low_msg;
	char	*cp;
	char	*reqlist[2];

	if (argc != 2) {
		printf("%d Usage: GROUP newsgroup.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	if (index(argv[1], '/') != (char *) NULL) {
		printf("%d Invalid group name (bad format).\r\n", ERR_NOGROUP);
		(void) fflush(stdout);
		return;
	}

	if (find_group(argv[1], &low_msg, &high_msg) < 0) {
		printf("%d Invalid group name (not in active).\r\n",
			ERR_NOGROUP);
		(void) fflush(stdout);
		return;
	}

	reqlist[0] = argv[1];
	reqlist[1] = NULL;

	if (ngpermcount) {
		if (ngmatch(s1strneql, ALLBUT,
		    ngpermlist, ngpermcount, reqlist, 1) == 0) {
			printf("%d You're not allowed to read %s, sorry.\r\n",
				ERR_ACCESS, argv[1]);
			(void) fflush(stdout);
			return;
		}
	} else if (ALLBUT == 0) {
		printf("%d You're not allowed to read %s, sorry.\r\n",
			ERR_ACCESS, argv[1]);
		(void) fflush(stdout);
		return;
	}

#if defined(XOVER) || defined(XROVER)
	close_xfiles();
#endif
	close_crnt();
	if (group_name)
		free(group_name);
	(void) chdir(spooldir);
	if ((group_name = malloc(strlen(argv[1])+1)) != NULL)
		strcpy(group_name, argv[1]);

#ifdef LOG
	syslog(LOG_INFO, "%s group %s", hostname, argv[1]);
#endif

	while ((cp = index(argv[1], '.')) != (char *) NULL)
		*cp = '/';

	(void) strcpy(temp_dir, spooldir);
	(void) strcat(temp_dir, "/");
	(void) strcat(temp_dir, argv[1]);

	/*
	 * (void) because a group can be in the active file
	 * but not have a spool directory.  Just leave us
	 * chdired to base spool directory if this fails.
	 */
	(void) chdir(temp_dir);

#ifdef LOG
	++grps_acsd;
#endif

	num_arts = scan_dir(low_msg, high_msg);
	art_ptr = 0;

	ingroup = 1;

#ifdef	XTHREAD
	threadfile = thread_name(argv[1]);
#endif

	while ((cp = index(argv[1], '/')) != (char *) NULL)
		*cp = '.';

	printf("%d %d %d %d %s\r\n",
		OK_GROUP,
		num_arts,
		(num_arts > 0 ? art_array[0] : 0),
		(num_arts > 0 ? art_array[num_arts-1] : 0),
		argv[1]);
	(void) fflush(stdout);
}

#ifdef LISTGROUP
/*
 * LISTGROUP [group]
 *
 * Lists all article numbers (filenames) in the given group. Used by
 * newsreaders such as nn and trn for fast validation of a database.
 * If a group name is given it becomes the current group.
 *
 * This command is an extension, and not included in RFC 977.
 */

void
xlistgroup(argc, argv)
	int		argc;
	char		*argv[];
{
	register int i;

	if (argc == 2) {
		ingroup = 0;
		/* This will output a success or failure message */
		group(argc, argv);
		if (!ingroup) {
			return;
		}
	} else if (argc > 2) {
		printf("%d Usage: LISTGROUP [newsgroup].\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	} else if (!ingroup) {
		printf("%d You are not currently in a newsgroup.\r\n",
			ERR_NCING);
		(void) fflush(stdout);
		return;
	} else if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	} else {
		/* output a success message when no group name is given */
		printf("%d %d %d %d %s\r\n",
			OK_GROUP,
			num_arts,
			(num_arts > 0 ? art_array[0] : 0),
			(num_arts > 0 ? art_array[num_arts-1] : 0),
			group_name? group_name : "(current group)");
	}

#ifdef LOG
	syslog(LOG_INFO, "%s listgroup", hostname);
#endif
	for (i = 0; i < num_arts; i++) {
		printf("%d\r\n", art_array[i]);
	}
	putline(".");
	(void) fflush(stdout);
}

#endif /* LISTGROUP */
