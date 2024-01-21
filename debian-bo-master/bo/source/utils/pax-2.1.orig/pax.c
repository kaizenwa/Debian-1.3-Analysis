/* $Source: /usr/local/src/pax/RCS/pax.c,v $
 *
 * $Revision: 2.2 $
 *
 * DESCRIPTION
 *
 *	Pax is the archiver described in IEEE P1003.2.  It is an archiver
 *	which understands both tar and cpio archives and has a new interface.
 *
 * SYNOPSIS
 *
 *	pax -[cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]
 *	pax -r [-cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]
 *	pax -w [-adimuvy] [-b blocking] [-f archive] [-s replstr]...]
 *	       [-t device][-x format][pathname...]
 *	pax -r -w [-ilmopuvy][-s replstr][pathname...] directory
 *
 * DESCRIPTION
 *
 * 	PAX - POSIX conforming tar and cpio archive handler.  This
 *	program implements POSIX conformant versions of tar, cpio and pax
 *	archive handlers for UNIX.  These handlers have defined befined
 *	by the IEEE P1003.2 commitee.
 *
 * COMPILATION
 *
 *	A number of different compile time configuration options are
 *	available, please see the Makefile and config.h for more details.
 *
 * AUTHOR
 *
 *     Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
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
 * $Log: pax.c,v $
 * Revision 2.2  1996/10/20  12:10:20  istewart
 * Fix some definitions, add strftime & strcasecmp
 *
 * Revision 2.1  1996/10/18  21:36:18  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char        *ident = "$Id: pax.c,v 2.2 1996/10/20 12:10:20 istewart Exp $";
static char        *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif /* ! lint */


/* Headers */

#define NO_EXTERN
#include "pax.h"


/* Globally Available Identifiers */

char               *ar_file;		/* File containing name of archive */
char               *bufend;		/* End of data within archive buffer */
char               *bufstart;		/* Archive buffer */
char               *bufidx;		/* Archive buffer index */
char               *myname;		/* name of executable (argv[0]) */
char              **n_argv;		/* Argv used by name routines */
int                 n_argc;		/* Argc used by name routines */
int                 archivefd;		/* Archive file descriptor */
int                 blocking;		/* Size of each block, in records */
gid_t	            gid;		/* Group ID */
int                 head_standard;	/* true if archive is POSIX format */
int                 ar_interface;	/* defines interface we are using */
int                 ar_format;		/* defines current archve format */
int                 mask;		/* File creation mask */
int                 ttyf;		/* For interactive queries */
uid_t	            uid;		/* User ID */
int                 names_from_stdin;	/* names for files are from stdin */
off_t               total;		/* Total number of bytes transferred */
short               f_access_time;	/* Reset access times of input files */
short               areof;		/* End of input volume reached */
short               f_dir_create;	/* Create missing directories */
short               f_append;		/* Add named files to end of archive */
short               f_create;		/* create a new archive */
short               f_extract;		/* Extract named files from archive */
short               f_follow_links;	/* follow symbolic links */
short               f_interactive;	/* Interactivly extract files */
short               f_unresolved;	/* Report on unresolved links */
short               f_list;		/* List files on the archive */
short               f_modified;		/* Don't restore modification times */
short               f_verbose;		/* Turn on verbose mode */
short               f_link;		/* link files where possible */
short               f_owner;		/* extract files as the user */
short               f_pass;		/* pass files between directories */
short               f_newer;		/* append files to archive if newer */
short               f_disposition;	/* ask for file disposition */
short               f_reverse_match;	/* Reverse sense of pattern match */
short               f_mtime;		/* Retain file modification time */
short               f_unconditional;	/* Copy unconditionally */
short               tar_interface = 0;	/* using the tar interface */
short               cpio_interface = 0;	/* using the cpio interface */
short               pax_interface = 0;	/* using the pax interface */
time_t              now = 0;		/* Current time */
uint                arvolume;		/* Volume number */
off_t               blocksize = BLOCKSIZE;	/* Archive block size */
FILE               *msgfile;		/* message outpu file stdout/stderr */
Replstr            *rplhead = (Replstr *) NULL;	/* head of replstr list */
Replstr            *rpltail;		/* pointer to tail of replstr list */


/* Function Prototypes */

static void	   usage __P ((void));
static off_t       pax_optsize __P ((char *));

/* main - main routine for handling all archive formats.
 *
 * DESCRIPTION
 *
 * 	Set up globals and call the proper interface as specified by the user.
 *
 * PARAMETERS
 *
 *	int argc	- count of user supplied arguments
 *	char **argv	- user supplied arguments
 *
 * RETURNS
 *
 *	Returns an exit code of 0 to the parent process.
 */

#ifdef __STDC__
int		main (int argc, char **argv)
#else
int		main (argc, argv)
    int		  argc;
    char	**argv;
#endif
{
#ifdef MSDOS
    char 	       *tmp;
#endif /* MSDOS */

#ifdef MSDOS
    _fmode = O_BINARY;
    setmode(fileno(stdin), O_BINARY);
    setmode(fileno(stdout), O_BINARY);
    /* strip the pathname off of the name of the executable */
#ifdef DIO
    dio_str(argv[0]);
#endif
    if ((myname = strrchr(argv[0], '/')) != (char *)NULL) {
 	myname++;
    } else if ((myname = strrchr(argv[0], '\\')) != (char *)NULL) {
	myname++;
    } else {
	myname = argv[0];
    }
    if ((tmp = strrchr(myname, '.')) != (char *) NULL) {
	*tmp = '\0';
    }
#else /* !MSDOS */
    /* strip the pathname off of the name of the executable */
    if ((myname = rindex(argv[0], '/')) != (char *) NULL) {
	myname++;
    } else {
	myname = argv[0];
    }
#endif /* MSDOS */

    /* set up for collecting other command line arguments */
    name_init(argc, argv);

    /* get all our necessary information */
    mask = umask(0);
    uid = getuid();
    gid = getgid();
    now = time((time_t *) 0);

    /* open terminal for interactive queries */
    ttyf = open_tty();

    if (stricmp(myname, "tar") == 0) {
	tar_interface = 1;
	do_tar(argc, argv);
    } else if (stricmp(myname, "cpio") == 0) {
	cpio_interface = 1;
	do_cpio(argc, argv);
    } else {
	pax_interface = 1;
	do_pax(argc, argv);
    }
    return (0);
}


/* do_pax - provide a PAX conformant user interface for archive handling
 *
 * DESCRIPTION
 *
 *	Process the command line parameters given, doing some minimal sanity
 *	checking, and then launch the specified archiving functions.
 *
 * PARAMETERS
 *
 *    int ac		- A count of arguments in av.  Should be passed argc
 *			  from main
 *    char **av		- A pointer to an argument list.  Should be passed
 *			  argv from main
 *
 * RETURNS
 *
 *    Normally returns 0.  If an error occurs, -1 is returned
 *    and state is set to reflect the error.
 *
 */

#ifdef __STDC__
void		do_pax (int ac, char **av)
#else
void		do_pax (ac, av)
    int		  ac;	/* argument counter */
    char	**av;	/* arguments */
#endif
{
    int                 c;
    char               *dirname;
    Stat                st;

    /* default input/output file for PAX is STDIN/STDOUT */
    ar_file = "-";

    /*
     * set up the flags to reflect the default pax inteface.  Unfortunately
     * the pax interface has several options which are completely opposite of
     * the tar and/or cpio interfaces...
     */
    f_unconditional = 1;
    f_mtime = 1;
    f_dir_create = 1;
    f_list = 1;
    blocksize = 0;
    blocking = 0;
    ar_interface = PAX;
    ar_format = TAR;		/* default interface if none given for -w */
    msgfile = stdout;

    while ((c = getopt(ac, av, "#:ab:cdf:ilmoprs:t:uvwx:y")) != EOF) {
	switch (c) {

	case '#':
	    break;

	case 'a':
	    f_append = 1;
	    f_list = 0;
	    break;

	case 'b':
	    if ((blocksize = pax_optsize(optarg)) == 0) {
		fatal("Bad block size");
	    }
	    break;

	case 'c':
	    f_reverse_match = 1;
	    break;

	case 'd':
	    f_dir_create = 0;
	    break;

	case 'f':
	    if (blocksize == 0) {
		blocking = 1;
		blocksize = 1 * BLOCKSIZE;
	    }
	    ar_file = optarg;
	    break;

	case 'i':
	    f_interactive = 1;
	    break;

	case 'l':
	    f_link = 1;
	    break;

	case 'm':
	    f_mtime = 0;
	    break;

	case 'o':
	    f_owner = 1;
	    break;

	case 'p':
	    f_access_time = 1;
	    break;

	case 'r':
	    if (f_create) {
		f_create = 0;
		f_pass = 1;
	    } else {
		f_list = 0;
		f_extract = 1;
	    }
	    msgfile = stderr;
	    break;

	case 's':
	    add_replstr(optarg);
	    break;

	case 't':
	    if (blocksize == 0) {
		blocking = 1;
		blocksize = 10 * BLOCKSIZE;
	    }
	    ar_file = optarg;
	    break;

	case 'u':
	    f_unconditional = 1;
	    break;

	case 'v':
	    f_verbose = 1;
	    break;

	case 'w':
	    if (f_extract) {
		f_extract = 0;
		f_pass = 1;
	    } else {
		f_list = 0;
		f_create = 1;
	    }
	    msgfile = stderr;
	    break;

	case 'x':
            if (stricmp(optarg, "ustar") == 0) {
		ar_format = TAR;
            } else if (stricmp(optarg, "cpio") == 0) {
		ar_format = CPIO;
	    } else {
		usage();
	    }
	    break;

	case 'y':
	    f_disposition = 1;
	    break;

	default:
	    usage();
	}
    }

    if ( strcmp(ar_file, "-") == 0
         && isatty(fileno(stdin))
         && isatty(fileno(stdout)) )
      usage();

#ifdef MSDOS
    setmode(fileno(msgfile), O_TEXT);
#endif /* MSDOS */

    if (blocksize == 0) {
	blocking = 1;
	blocksize = blocking * BLOCKSIZE;
    }
    buf_allocate((off_t) blocksize);

    if (f_extract || f_list) {
	open_archive(AR_READ);
	get_archive_type();
	read_archive();
    } else if (f_create) {
	if (optind >= n_argc) {
	    names_from_stdin++;	/* args from stdin */
	}

#ifdef MSDOS
        if (names_from_stdin) {
            setmode(fileno(stdin), O_TEXT);
	}
#endif /* MSDOS */

	open_archive(AR_WRITE);
	create_archive();
    } else if (f_append) {
	open_archive(AR_APPEND);
	get_archive_type();
	append_archive();
    } else if (f_pass && optind < n_argc) {
	dirname = n_argv[--n_argc];
	if (PAX_LSTAT(dirname, &st) < 0) {
	    fatal(strerror (errno));
	}
	if ((st.sb_mode & S_IFMT) != S_IFDIR) {
	    fatal("Not a directory");
	}
	if (optind >= n_argc) {
	    names_from_stdin++;	/* args from stdin */
	}

#ifdef MSDOS
        if (names_from_stdin) {
            setmode(fileno(stdin), O_TEXT);
	}
#endif /* MSDOS */

	pass(dirname);
    } else {
	usage();
    }
}


/* get_archive_type - determine input archive type from archive header
 *
 * DESCRIPTION
 *
 * 	reads the first block of the archive and determines the archive
 *	type from the data.  If the archive type cannot be determined,
 *	processing stops, and a 1 is returned to the caller.  If verbose
 *	mode is on, then the archive type will be printed on the standard
 *	error device as it is determined.
 *
 * FIXME
 *
 *	be able to understand TAR and CPIO magic numbers
 */

#ifdef __STDC__
void		get_archive_type (void)
#else
void		get_archive_type ()
#endif
{
    if (ar_read() != 0) {
	fatal("Unable to determine archive type.");
    }
    if (strncmp(bufstart, "070707", 6) == 0) {
	ar_format = CPIO;
	if (f_verbose) {
	    fputs("CPIO format archive\n", stderr);
	}
    } else if ((((uint)(bufstart[0]&0x0FF)<<8) | (uint)(bufstart[1]&0x0FF)) == 070707) {
        /* big-endian (not byte swapped, might be word swapped) */
	ar_format = CPIO;
	if (f_verbose) {
	    fputs("CPIO format archive\n", stderr);
	}
    } else if ((((uint)(bufstart[1]&0x0FF)<<8) | (uint)(bufstart[0]&0x0FF)) == 070707) {
        /* big-endian (not byte swapped, might be word swapped) */
	ar_format = CPIO;
	if (f_verbose) {
	    fputs("CPIO format archive\n", stderr);
	}
    } else if (strncmp(&bufstart[257], "ustar", 5) == 0) {
	ar_format = TAR;
	if (f_verbose) {
	    fputs("USTAR format archive\n", stderr);
	}
    } else {
	ar_format = TAR;
	if (f_verbose) {
	    fputs("USTAR format archive assumed\n", stderr);
	}
    }
}


/* pax_optsize - interpret a size argument
 *
 * DESCRIPTION
 *
 * 	Recognizes suffixes for blocks (512-bytes), k-bytes and megabytes.
 * 	Also handles simple expressions containing '+' for addition.
 *
 * PARAMETERS
 *
 *    char 	*str	- A pointer to the string to interpret
 *
 * RETURNS
 *
 *    Normally returns the value represented by the expression in the
 *    the string.
 *
 * ERRORS
 *
 *	If the string cannot be interpretted, the program will fail, since
 *	the buffering will be incorrect.
 *
 */

#ifdef __STDC__
static off_t	pax_optsize (char *str)
#else
static off_t	pax_optsize (str)
    char	*str;		/* pointer to string to interpret */
#endif
{
    char               *idx;
    off_t               number;	/* temporary storage for current number */
    off_t               result;	/* cumulative total to be returned to caller */

    result = 0;
    idx = str;
    for (;;) {
	number = 0;
	while (*idx >= '0' && *idx <= '9')
	    number = number * 10 + *idx++ - '0';
	switch (*idx++) {

	case 'b':
	    result += number * 512L;
	    continue;

	case 'k':
	    result += number * 1024L;
	    continue;

	case 'm':
	    result += number * 1024L * 1024L;
	    continue;

	case '+':
	    result += number;
	    continue;

	case '\0':
	    result += number;
	    break;

	default:
	    break;
	}
	break;
    }
    if (*--idx) {
	fatal("Unrecognizable value");
    }
    return (result);
}


/* usage - print a helpful message and exit
 *
 * DESCRIPTION
 *
 *	Usage prints out the usage message for the PAX interface and then
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

    printf("\r\nUsage: %s -[cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]\r\n", myname);
    printf("       %s -r [-cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]\r\n", myname);
    printf("       %s -w [-adimuvy] [-b blocking] [-f archive] [-s replstr]\r\n"
           "              [-t device] [-x format] [pathname...]\r\n", myname);
    printf("       %s -r -w [-ilmopuvy] [-s replstr] [pathname...] directory\r\n", myname);

    printf("\r\nRename PAX.EXE to TAR.EXE or CPIO.EXE to get the TAR or CPIO user interface.\r\n");

#ifdef DISKACC
    printf("\r\nUse the option -t with the special device names 0: and 1: to access Unix floppy"
           "\r\ndisks with tar or cpio archives. The disk type is automatically detected.\r\n");
#endif
#else
    fprintf(stderr, "Usage: %s -[cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]\n",
	    myname);
    fprintf(stderr, "       %s -r [-cimopuvy] [-f archive] [-s replstr] [-t device] [pattern...]\n",
	    myname);
    fprintf(stderr, "       %s -w [-adimuvy] [-b blocking] [-f archive] [-s replstr]\n"
                    "              [-t device] [-x format] [pathname...]\n",
	    myname);
    fprintf(stderr, "       %s -r -w [-ilmopuvy] [-s replstr] [pathname...] directory\n",
            myname);
#endif
    exit(1);
}
