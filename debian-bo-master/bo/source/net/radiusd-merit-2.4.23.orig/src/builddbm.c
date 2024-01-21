/*
 *
 *	RADIUS
 *	Remote Authentication Dial In User Service
 *
 *
 *	Livingston Enterprises, Inc.
 *	6920 Koll Center Parkway
 *	Pleasanton, CA   94566
 *
 *	Copyright 1992 Livingston Enterprises, Inc.
 *
 *	Permission to use, copy, modify, and distribute this software for any
 *	purpose and without fee is hereby granted, provided that this
 *	copyright and permission notice appear on all copies and supporting
 *	documentation, the name of Livingston Enterprises, Inc. not be used
 *	in advertising or publicity pertaining to distribution of the
 *	program without specific prior permission, and notice be given
 *	in supporting documentation that copying and distribution is by
 *	permission of Livingston Enterprises, Inc.
 *
 *	Livingston Enterprises, Inc. makes no representations about
 *	the suitability of this software for any purpose.  It is
 *	provided "as is" without express or implied warranty.
 *
 */

static char     rcsid[] =
		"$Id: builddbm.c,v 1.10 1995/10/21 19:24:30 web Exp $";

static char     sccsid[] =
		"@(#)builddbm.c	1.1 Copyright 1992 Livingston Enterprises Inc";

#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<netdb.h>
#include	<time.h>
#include	<ctype.h>

#include	"radius.h"

char           *radius_dir;

#define FIND_MODE_NAME	0
#define FIND_MODE_REPLY	1
#define FIND_MODE_SKIP	2
#define FIND_MODE_FLUSH	3

FILE           *userfd;
static int      user_read PROTO((char *, char *));

main (argc, argv)

int             argc;
char           *argv[];

{
	char            name[128];
	char            content[2048];
	char           *progname;
	int             fd;
	datum           named;
	datum           contentd;
#ifdef	USE_NDBM
	DBM            *db;
#endif	/* USE_NDBM */

	progname = *argv;

#ifdef	USE_NDBM
	if ((db = dbm_open ("users", O_RDWR | O_CREAT | O_TRUNC, 0600)) == 0)
	{
		fprintf (stderr, "%s: Couldn't init dbm\n", progname);
		exit (-1);
	}
#else	/* USE_NDBM */
	if ((fd = open ("users.pag", O_WRONLY | O_CREAT | O_TRUNC, 0600)) < 0)
	{
		fprintf (stderr, "%s: Couldn't open users.pag for writing\n",
			progname);
		exit (-1);
	}
	close (fd);
	if ((fd = open ("users.dir", O_WRONLY | O_CREAT | O_TRUNC, 0600)) < 0)
	{
		fprintf (stderr, "%s: Couldn't open users.dir for writing\n",
			progname);
		exit (-1);
	}
	close (fd);
#endif	/* USE_NDBM */

	radius_dir = ".";

#ifdef	USE_DBM
	if (dbminit ("users") != 0)
	{
		fprintf (stderr, "%s: Couldn't init dbm\n",
			progname);
		exit (-1);
	}
#endif	/* USE_DBM */

	while (user_read (name, content) == 0)
	{
		named.dptr = name;
		named.dsize = strlen (name);
		contentd.dptr = content;
		contentd.dsize = strlen (content);
#ifdef	USE_NDBM
		if (dbm_store (db, named, contentd, DBM_INSERT) != 0)
#else	/* USE_NDBM */
		if (store (named, contentd) != 0)
#endif	/* USE_NDBM */
		{
			fprintf (stderr, "%s: Couldn't store datum\n",
				progname);
			exit (-1);
		}
	}
#ifdef	USE_NDBM
	dbm_close (db);
#else	/* USE_NDBM */
	dbmclose ();
#endif	/* USE_NDBM */
	exit (0);
} /* end of main () */

/*************************************************************************
 *
 *	Function: user_read
 *
 *	Purpose: Return each user in the database: name is key, content is
 *		 two strings - check items, and reply items separated by a
 *		 newline.
 *
 *************************************************************************/

static int
user_read (name, content)

char           *name;
char           *content;

{
	char            buffer[256];
	char           *ptr;
	int             namelen;
	int             mode;
	VALUE_PAIR     *check_first;
	VALUE_PAIR     *reply_first;
	char           *func = "user_name";

	/*
	 * Open the user table
	 */
	if (userfd == (FILE *) NULL)
	{
		sprintf (buffer, "%s/%s", radius_dir, RADIUS_USERS);
		if ((userfd = fopen (buffer, "r")) == (FILE *) NULL)
		{
			fprintf (stderr, "%s: Couldn't open %s for reading\n",
				 func, buffer);
			exit (-1);
		}
	}

	mode = FIND_MODE_NAME;

	while (fgets (buffer, sizeof (buffer), userfd) != (char *) NULL)
	{
		if (mode == FIND_MODE_NAME)
		{

			/*
			 * Find the entry starting with the users name
			 */
			if (*buffer != COMMENT &&
				*buffer != '\t' && *buffer != '\n')
			{
				ptr = buffer;
				while (*ptr != ' ' && *ptr != '\t' &&
						*ptr != '\0')
				{
					*name++ = *ptr++;
				}
				*name = '\0';
				if (*ptr == '\0')
				{
					continue;
				}
				ptr++;
				while (*ptr == ' ' || *ptr == '\t')
				{
					ptr++;
				} strcpy (content, ptr);
				content += strlen (content);
				mode = FIND_MODE_REPLY;
			}
		}
		else
		{
			if (*buffer == ' ' || *buffer == '\t')
			{
				ptr = buffer;
				while (*ptr == ' ' || *ptr == '\t')
				{
					ptr++;
				}
				strcpy (content, ptr);
				content += strlen (content);
				content -= 2;
				while (*content == ' ' || *content == '\t')
				{
					content--;
				}
				content++;
				*content = '\0';
				if (*(content - 1) != ',')
				{
					return (0);
				}
			}
			else
			{
				/* We are done */
				return (0);
			}
		}
	}
	fclose (userfd);
	return (-1);
} /* end of user_read () */
