/* $Source: /usr/local/src/pax/RCS/tar.c,v $
 *
 * $Revision: 2.2 $
 *
 * tar.c - tar specific functions for archive handling
 *
 * DESCRIPTION
 *
 *	These routines provide a tar conforming interface to the pax
 *	program.
 *
 * AUTHOR
 *
 *	Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
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
 * $Log: tar.c,v $
 * Revision 2.2  1996/10/20  12:10:20  istewart
 * Fix some definitions, add strftime & strcasecmp
 *
 * Revision 2.1  1996/10/18  21:36:18  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char        *ident = "$Id: tar.c,v 2.2 1996/10/20 12:10:20 istewart Exp $";
static char        *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.";
#endif /* not lint */

/* Headers */

#include "pax.h"


/* Defines */

#define DEF_BLOCKING	20	/* default blocking factor for extract */


/* Function Prototypes */

static int	    taropt __P ((int, char **, char *));
static void 	    usage __P ((void));

/* do_tar - main routine for tar.
 *
 * DESCRIPTION
 *
 *	Provides a tar interface to the PAX program.  All tar standard
 *	command line options are supported.
 *
 * PARAMETERS
 *
 *	int argc	- argument count (argc from main)
 *	char **argv	- argument list (argv from main)
 *
 * RETURNS
 *
 *	zero
 */

#ifdef __STDC__
void		do_tar (int argc, char **argv)
#else
void		do_tar (argc, argv)
    int		  argc;	/* argument count (argc from main) */
    char	**argv;	/* argument list (argv from main) */
#endif
{
    int                 c;	/* Option letter */

    /* Set default option values */
    names_from_stdin = 0;
    ar_file = getenv("TAPE");	/* From environment, or */
    if (ar_file == NULL) {	/* from Makefile */
	ar_file = DEF_TAR_FILE;
    }
    /*
     * set up the flags to reflect the default pax inteface.  Unfortunately
     * the pax interface has several options which are completely opposite of
     * the tar and/or cpio interfaces...
     */
    f_unconditional = 1;
    f_mtime = 1;
    f_dir_create = 1;
    blocking = 0;
    ar_interface = TAR;
    ar_format = TAR;
    msgfile = stderr;

    /* Parse options */
    while ((c = taropt(argc, argv, "#:b:cf:hlmortuvwx")) != EOF) {
	switch (c) {

	case '#':
	    break;

	case 'b':		/* specify blocking factor */
	    /*
	     * FIXME - we should use a conversion routine that does some kind
	     * of reasonable error checking, but...
	     */
	    blocking = atoi(optarg);
	    break;

	case 'c':		/* create a new archive */
	    f_create = 1;
	    break;

	case 'f':		/* specify input/output file */
	    ar_file = optarg;
	    break;

	case 'h':
	    f_follow_links = 1;	/* follow symbolic links */
	    break;

	case 'l':		/* report unresolved links */
	    f_unresolved = 1;
	    break;

	case 'm':		/* don't restore modification times */
	    f_modified = 1;
	    break;

	case 'o':		/* take on user's group rather than archives */
	    break;

	case 'r':		/* named files are appended to archive */
	    f_append = 1;
	    break;

	case 't':
	    f_list = 1;		/* list files in archive */
	    break;

	case 'u':		/* named files are added to archive */
	    f_newer = 1;
	    break;

	case 'v':		/* verbose mode */
	    f_verbose = 1;
	    break;

	case 'w':		/* user interactive mode */
	    f_disposition = 1;
	    break;

	case 'x':		/* named files are extracted from archive */
	    f_extract = 1;
	    break;

	case '?':
	    usage();
	    exit(EX_ARGSBAD);
	}
    }

#ifdef MSDOS
    setmode(fileno(msgfile), O_TEXT);
    if (names_from_stdin) {
        setmode(fileno(stdin), O_TEXT);
    }
#endif /* MSDOS */

    /* check command line argument sanity */
    if (f_create + f_extract + f_list + f_append + f_newer != 1) {
#ifdef MSDOS
        printf(
        "\r\n%s: you must specify exactly one of the c, t, r, u or x options\r\n",
                       myname);
#else
	(void) fprintf(stderr,
	"%s: you must specify exactly one of the c, t, r, u or x options\n",
                       myname);
#endif
	usage();
	exit(EX_ARGSBAD);
    }
    /* set the blocking factor, if not set by the user */
    if (blocking == 0) {
	if (f_extract || f_list) {
	    blocking = DEF_BLOCKING;
	    fprintf(stderr, "Tar: blocksize = %d\n", blocking);
	} else {
	    blocking = 1;
	}
    }
    blocksize = blocking * BLOCKSIZE;
    buf_allocate((off_t) blocksize);

    if (f_create) {
	open_archive(AR_WRITE);
	create_archive();	/* create the archive */
    } else if (f_extract) {
	open_archive(AR_READ);
	read_archive();		/* extract files from archive */
    } else if (f_list) {
	open_archive(AR_READ);
	read_archive();		/* read and list contents of archive */
    } else if (f_append) {
	open_archive(AR_APPEND);
	append_archive();	/* append files to archive */
    }
    if (f_unresolved) {
	linkleft();		/* report any unresolved links */
    }
}


/* taropt -  tar specific getopt
 *
 * DESCRIPTION
 *
 * 	Plug-compatible replacement for getopt() for parsing tar-like
 * 	arguments.  If the first argument begins with "-", it uses getopt;
 * 	otherwise, it uses the old rules used by tar, dump, and ps.
 *
 * PARAMETERS
 *
 *	int argc	- argument count (argc from main)
 *	char **argv	- argument list (argv from main)
 *	char *optstring	- sring which describes allowable options
 *
 * RETURNS
 *
 *	Returns the next option character in the option string(s).  If the
 *	option requires an argument and an argument was given, the argument
 *	is pointed to by "optarg".  If no option character was found,
 *	returns an EOF.
 *
 */
#ifdef __STDC__
static int	taropt (int argc, char **argv, char *optstring)
#else
static int	taropt (argc, argv, optstring)
    int		  argc;		/* argument count from cmd line */
    char	**argv;		/* argument vector from cmd line */
    char	 *optstring;	/* option string ala getopt */
#endif
{
    extern char        *optarg;	/* Points to next arg */
    extern int          optind;	/* Global argv index */
    static char        *key;	/* Points to next keyletter */
    static char         use_getopt;	/* !=0 if argv[1][0] was '-' */
    char                c;
    char               *place;

    optarg = (char *) NULL;

    if (key == (char *) NULL) {	/* First time */
	if (argc < 2) {
	    return (EOF);
	}
	key = argv[1];
	if (*key == '-') {
	    use_getopt++;
	} else {
	    optind = 2;
	}

    }
    if (use_getopt) {
	return (getopt(argc, argv, optstring));
    }
    c = *key++;
    if (c == '\0') {
	key--;
	return (EOF);
    }
    place = index(optstring, c);

    if (place == (char *) NULL || c == ':') {
	fprintf(stderr, "%s: unknown option %c\n", argv[0], c);
	return ('?');
    }
    place++;
    if (*place == ':') {
	if (optind < argc) {
	    optarg = argv[optind];
	    optind++;
	} else {
	    fprintf(stderr, "%s: %c argument missing\n", argv[0], c);
	    return ('?');
	}
    }
    return (c);
}


/* usage - print a helpful message and exit
 *
 * DESCRIPTION
 *
 *	Usage prints out the usage message for the TAR interface and then
 *	exits with a non-zero termination status.  This is used when a user
 *	has provided non-existant or incompatible command line arguments.
 *
 * RETURNS
 *
 *	Returns an exit status of 1 to the parent process.
 *
 */

#ifdef __STDC__
static void	usage (void)
#else
static void	usage ()
#endif
{
#ifdef MSDOS
    printf("\r\nPAX version 2.1 - POSIX conforming tar and cpio archiver for DOS and OS/2\r\n");

    printf("\r\nUsage: %s -c[bfvw] device block filename..\r\n", myname);
    printf("       %s -r[bvw] device block [filename...]\r\n", myname);
    printf("       %s -t[vf] device\r\n", myname);
    printf("       %s -u[bvw] device block [filename...]\r\n", myname);
    printf("       %s -x[flmovw] device [filename...]\r\n", myname);

    printf("\r\nRename TAR.EXE to PAX.EXE or CPIO.EXE to get the PAX or CPIO user interface.\r\n");

#ifdef DISKACC
    printf("\r\nUse the option f with the special device names 0: and 1: to access Unix floppy"
           "\r\ndisks with tar archives. The disk type is automatically detected.\r\n");
#endif
#else
    fprintf(stderr, "Usage: %s -c[bfvw] device block filename..\n", myname);
    fprintf(stderr, "       %s -r[bvw] device block [filename...]\n", myname);
    fprintf(stderr, "       %s -t[vf] device\n", myname);
    fprintf(stderr, "       %s -u[bvw] device block [filename...]\n", myname);
    fprintf(stderr, "       %s -x[flmovw] device [filename...]\n", myname);
#endif
    exit(1);
}
