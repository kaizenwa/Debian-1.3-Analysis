/* This file (and only this file - not the entire nntp distribution) is
 * hereby placed in the public domain.  Use it as you see fit, but if you
 * manage to find some wonderful use for this code elsewhere, it would be
 * nice to receive mention for it.
 *
 * - Tim Iverson
 *   iverson@xstor.com -/- uunet!xstor!iverson
 *   3/28/91
 *   modified by Wayne Davison (davison@borland.com) to work with trn 2.0
 *   10/6/91
 */

#include "common.h"
#ifndef lint
static char sccsid[] = "$Id: xthread.c,v 1.2 1994/11/01 06:08:21 sob Exp sob $";
#endif
#ifdef XTHREAD

# ifdef __GNUC__
#  define alloca __builtin_alloca
# endif

char *thread_name();

/* Usage: XTHREAD [DBINIT|THREAD]
 *
 * DBINIT	dump the contents of the db.init file to stdout
 * THREAD	dump the contents of the thread file for the current
 *		newsgroup to stdout (default if no argument).
 *
 * N.B. These two files are not ascii and no attempt is made at converting
 *	native byte size to any type of standard whatsoever.  This'll have
 *	to be fixed if this command is to be integrated into the protocol.
 *
 * This command is not documented in rfc977.
 */

void
xthread(argc, argv)
int	argc;
char	*argv[];
{
	register FILE	*fp;
	struct stat	s;
	char		*buf, *file, *what;

	/* can't transfer threads, only read 'em */
	if (!canread)
	{
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	/* "parse" the argument */
	if (argc == 2 && !strcasecmp(argv[1], "dbinit"))
	{
		file = thread_name("*******");
		what = "db.init";
		strcpy(index(file, '*'), what);
	}
	else if (argc == 1 || argc == 2 && !strcasecmp(argv[1], "thread"))
	{
		if (!threadfile)
		{
			printf("%d You are not currently in a newsgroup.\r\n",
				ERR_NCING);
			(void) fflush(stdout);
			return;
		}
		file = threadfile;
		what = "thread";
	}
	else
	{
		printf("%d Usage: XTHREAD [DBINIT|THREAD]\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	/* try to open the file to be transfered */
	if (!(fp = fopen(file, "r")))
	{
		printf("%d %s file is not available.\r\n", ERR_FAULT, what);
		(void) fflush(stdout);
#ifdef SYSLOG
		if (!strcmp(what, "db.init"))
		    syslog(LOG_ERR, "xthread: fopen %s: %m", file);
#endif
		return;
	}

	/* tell 'em how much binary data is coming down the pike */
	fstat(fileno(fp), &s);
	printf("%d %u bytes of %s file follows verbatim (binary!)\r\n",
		OK_BIN, s.st_size, what);

	/* copy the file verbatim to stdout */
#ifdef __GNUC__
	if (buf = alloca(s.st_size))
	{
		/* ah-so! got lotsa memoree */
		read(fileno(fp), buf, s.st_size);
		fwrite(buf, s.st_size, 1, stdout);
	}
	else
#endif
	{
		int		bytes;
		char		buf[BUFSIZ];

		while (bytes = fread(buf, 1, sizeof buf, fp))
			fwrite(buf, bytes, 1, stdout);
	}

	fputs("\r\n.\r\n", stdout);
	fflush(stdout);
	fclose(fp);
}

/* Change a newsgroup name into the name of the thread data file.  We
** subsitute any '.'s in the group name into '/'s (unless LONG_THREAD_NAMES
** is defined), prepend the path, and append the '/.thread' (or '.th') on to
** the end.
*/
char *
thread_name(group)
char *group;
{
	static char name_buff[MAXPATHLEN];
#ifdef LONG_THREAD_NAMES
	char group_buff[512];
	register char *ptr;

	strcpy(group_buff, group);
	ptr = group = group_buff;
	while ((ptr = index(ptr, '/'))) {
		*ptr = '.';
	}
#endif
#ifdef SUFFIX
	sprintf(name_buff, "%s/%s%s", threaddir, group, SUFFIX);
#else
	sprintf(name_buff, "%s/%s", threaddir, group);
#endif

	return name_buff;
}

#endif /* not XTHREAD */
