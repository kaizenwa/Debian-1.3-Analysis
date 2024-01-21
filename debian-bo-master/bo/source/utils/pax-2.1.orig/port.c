/* $Source: /usr/local/src/pax/RCS/port.c,v $
 *
 * $Revision: 2.2 $
 *
 * port.c - These are routines not available in all environments.
 *
 * DESCRIPTION
 *
 *	The routines contained in this file are provided for portability to
 *	other versions of UNIX or other operating systems.  Not all systems
 *	have the same functions or the same semantics, these routines
 *	attempt to bridge the gap as much as possible.
 *
 * AUTHOR
 *
 *	Mark H. Colburn, Open Systems Architects, Inc. (mark@@jhereg.mn.org)
 *	John Gilmore (gnu@@hoptoad)
 *
 * COPYRIGHT
 *
 *	Copyright (c) 1989 Mark H. Colburn.  All rights reserved.
 *
 *	Redistribution and use in source and binary forms are permitted
 *	provided that the above copyright notice and this paragraph are
 *	duplicated in all such forms and that any documentation,
 *	advertising materials, and other materials related to such
 *	distribution and use acknowledge that the software was developed
 *	by Mark H. Colburn.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * $Log: port.c,v $
 * Revision 2.2  1996/10/20  12:10:20  istewart
 * Fix some definitions, add strftime & strcasecmp
 *
 * Revision 2.1  1996/10/18  21:36:18  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char        *ident = "$Id: port.c,v 2.2 1996/10/20 12:10:20 istewart Exp $";
static char        *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif /* ! lint */


/* Headers */

#include "pax.h"

#ifdef HAVE_VFORK_H 
#  include <vfork.h>
#else
#  define vfork fork
#endif


#ifndef HAVE_MKDIR 

/* mkdir - make a directory
 *
 * DESCRIPTION
 *
 * 	Mkdir will make a directory of the name "dpath" with a mode of
 *	"dmode".  This is consistent with the BSD mkdir() function and the
 *	P1003.1 definitions of MKDIR.
 *
 * PARAMETERS
 *
 *	dpath		- name of directory to create
 *	dmode		- mode of the directory
 *
 * RETURNS
 *
 *	Returns 0 if the directory was successfully created, otherwise a
 *	non-zero return value will be passed back to the calling function
 *	and the value of errno should reflect the error.
 */

#ifdef __STDC__
int		mkdir (char *dpath, int dmode)
#else
int		mkdir (dpath, dmode)
    char	*dpath;		/* Directory to make */
    int		 dmode;		/* mode to be used for new directory */
#endif
{
    int                 cpid;
    int                 status;
    Stat                statbuf;
    extern int          errno;

    if (PAX_STAT(dpath, &statbuf) == 0) {
	errno = EEXIST;		/* Stat worked, so it already exists */
	return (-1);
    }
    /* If stat fails for a reason other than non-existence, return error */
    if (errno != ENOENT)
	return (-1);

    switch (cpid = vfork()) {

    case -1:			/* Error in fork() */
	return (-1);	/* Errno is set already */

    case 0:			/* Child process */

	(void) umask(mask | (0777 & ~dmode));	/* Set for mkdir */
	execl("/bin/mkdir", "mkdir", dpath, (char *) 0);
	_exit(-1);		/* Can't exec /bin/mkdir */

    default:			/* Parent process */
	while (cpid != wait (&status)) {
	    /* Wait for child to finish */
	}
    }

    if (TERM_SIGNAL(status) != 0 || TERM_VALUE(status) != 0) {
	errno = EIO;		/* We don't know why, but */
	return (-1);	/* /bin/mkdir failed */
    }
    return (0);
}

#endif 


#ifndef HAVE_RMDIR

/* rmdir - remove a directory
 *
 * DESCRIPTION
 *
 *	Rmdir will remove the directory specified by "dpath".  It is
 *	consistent with the BSD and POSIX rmdir functions.
 *
 * PARAMETERS
 *
 *	dpath		- name of directory to remove
 *
 * RETURNS
 *
 *	Returns 0 if the directory was successfully deleted, otherwise a
 *	non-zero return value will be passed back to the calling function
 *	and the value of errno should reflect the error.
 */

#ifdef __STDC__
int		rmdir (char *dpath)
#else
int		rmdir (dpath)
    char	*dpath;	/* directory to remove */
#endif
{
    int                 cpid,
                        status;
    Stat                statbuf;
    extern int          errno;

    /* check to see if it exists */
    if (PAX_STAT(dpath, &statbuf) == -1) {
	return (-1);
    }
    switch (cpid = vfork()) {

    case -1:			/* Error in fork() */
	return (-1);	/* Errno is set already */

    case 0:			/* Child process */
	execl("/bin/rmdir", "rmdir", dpath, (char *) 0);
	_exit(-1);		/* Can't exec /bin/rmdir */

    default:			/* Parent process */
	while (cpid != wait(&status)) {
	    /* Wait for child to finish */
	}
    }

    if (TERM_SIGNAL(status) != 0 || TERM_VALUE(status) != 0) {
	errno = EIO;		/* We don't know why, but */
	return (-1);	/* /bin/rmdir failed */
    }
    return (0);
}

#endif


#ifndef HAVE_STRERROR

/* strerror - return pointer to appropriate system error message
 *
 * DESCRIPTION
 *
 *	Get an error message string which is appropriate for the setting
 *	of the errno variable.
 *
 * RETURNS
 *
 *	Returns a pointer to a string which has an appropriate error
 *	message for the present value of errno.  The error message
 *	strings are taken from sys_errlist[] where appropriate.  If an
 *	appropriate message is not available in sys_errlist, then a
 *	pointer to the string "Unknown error (errno <errvalue>)" is
 *	returned instead.
 */

#ifdef __STDC__
char		*strerror (int num)
#else
char		*strerror (num)
    int		num
#endif
{
    static char         msg[40];/* used for "Unknown error" messages */

    if (num > 0 && num < sys_nerr) {
	return (sys_errlist[num]);
    }
    sprintf(msg, "Unknown error (errno %d)", num);
    return (msg);
}

#endif 


#ifndef HAVE_GETOPT

/*
 * getopt - parse command line options
 *
 * DESCRIPTION
 *
 *	This is a slightly modified version of the AT&T Public Domain
 *	getopt().  It is included for those systems that do not already
 *	have getopt() available in their C libraries.
 */

int                 opterr = 1;
int                 optind = 1;
int                 optopt;
char               *optarg;

#ifdef __STDC__
int		getopt (int argc, char **argv, char *opts)
#else
int		getopt (argc, argv, opts)
    int		  argc;
    char	**argv;
    char	 *opts;
#endif
{
    static int          sp = 1;
    register int        c;
    register char      *cp;

    if (sp == 1)
	if (optind >= argc ||
	    argv[optind][0] != '-' || argv[optind][1] == '\0')
	    return (EOF);
	else if (strcmp(argv[optind], "--") == NULL) {
	    optind++;
	    return (EOF);
	}
    optopt = c = argv[optind][sp];
    if (c == ':' || (cp = index(opts, c)) == NULL) {
	if (opterr)
	    fprintf (stderr, "%s: illegal option -- %c\n", argv[0], c);

	if (argv[optind][++sp] == '\0') {
	    optind++;
	    sp = 1;
	}
	return ('?');
    }
    if (*++cp == ':') {
	if (argv[optind][sp + 1] != '\0')
	    optarg = &argv[optind++][sp + 1];
	else if (++optind >= argc) {
	    if (opterr)
		fprintf (stderr, "%s: option requires an argument -- %c\n", argv[0], c);

	    sp = 1;
	    return ('?');
	} else
	    optarg = argv[optind++];
	sp = 1;
    } else {
	if (argv[optind][++sp] == '\0') {
	    sp = 1;
	    optind++;
	}
	optarg = NULL;
    }
    return (c);
}

#endif /* !GETOPT */


#ifndef HAVE_MEMCPY

/*
 * memcpy - copy a block of memory from one place to another
 *
 * DESCRIPTION
 *
 *	This is an implementation of the System V/ANSI memcpy() function.
 *	Although Berkeley has bcopy which could be used here, use of this
 *	function does make the source more ANSI compliant.  This function
 *	is only used if it is not in the C library.
 */

#ifdef __STDC__
void			memcpy (void *s1, void *s2, unsigned int n)
#else
char			*memcpy (s1, s2, n)
    char		*s1;	/* destination block */
    char		*s2;	/* source block */
    unsigned int	n;	/* number of bytes to copy */
#endif
{
    char               *p;

    p = s1;
    while (n--) {
	*(unsigned char) s1++ = *(unsigned char) s2++;
    }
    return (p);
}

#endif


#ifndef HAVE_MEMSET
/*
 * memset - set a block of memory to a particular value
 *
 * DESCRIPTION
 *
 *	This is an implementation of the System V/ANSI memset function.
 *	Although Berkeley has the bzero() function, which is often what
 *	memset() is used for, the memset function is more general.  This
 *	function is only used if it is not in the C library.
 */

#ifdef __STDC__
void			*memset (void *s1, int c, unsigned int n)
#else
char			*memset (s1, c, n)
    char		*s1;	/* pointer to buffer to fill */
    int			 c;	/* character to fill buffer with */
    unsigned int	 n;	/* number of characters to fill */
#endif
{
    char               *p;

    p = s1;
    while (n--) {
	*(unsigned char) s1++ = (unsigned char) c;
    }
    return (p);
}

#endif /* !MEMSET */

/*
 * Stricmp
 */

#ifndef HAVE_STRICMP
#  ifdef __STDC__
int			stricmp (const char *s1, const char *s2)
#  else
int			stricmp (s1, s2)
const char		*s1;
const char		*s2;
#  endif
{
    int 	diff;

    if ((unsigned char *)s1 == (unsigned char *)s2)
	return 0;

    while ((diff = (toupper (*s1) - toupper (*s2))) == 0)
    {
	if (*s1++ == 0)
	    return 0;
	
	s2++;
    }

    return diff;
}
#endif

/*
 * Password
 */

#ifndef HAVE_GETPWNAM
static struct passwd npwd = {"", "", 0, 0, 0, "", "", "", ""};

struct passwd	*getpwuid (x)
uid_t		x;
{
    return (&npwd);
}


struct passwd	*getpwnam(s)
char		*s;
{
    return (&npwd);
}
#endif

/*
 * group
 */

#ifndef HAVE_GETGRNAM
static char		gmem1[] = "";
static char		*gmem2[] = {gmem1, (char *) NULL};
static struct group	ngrp = {"", "", 0, gmem2};

struct group	*getgrgid(x)
gid_t		x;
{
    return (&ngrp);
}


struct group	*getgrnam(s)
char		*s;
{
    return (&ngrp);
}


int	setgrent (void)
{
    return 0;
}
#endif

/*
 * Process UID
 */

#ifndef HAVE_GETUID
uid_t	getuid (void)
{
    return 0;
}
#endif

#ifndef HAVE_GETGID
gid_t	getgid (void)
{
    return 0;
}
#endif

/*
 * Link files
 */

#ifndef HAVE_LINK
int	link (from, to)
char	*from;
char	*to;
{
    return(-1);
}
#endif


#ifndef HAVE_CHOWN 
int	chown (name, uid, gid)
char    *name;
uid_t	uid;
gid_t	gid;
{
    return 0;
}
#endif
