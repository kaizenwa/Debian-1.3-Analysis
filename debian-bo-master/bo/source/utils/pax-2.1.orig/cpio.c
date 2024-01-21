/* $Source: /usr/local/src/pax/RCS/cpio.c,v $
 *
 * $Revision: 2.2 $
 *
 * cpio.c - Cpio specific functions for archive handling
 *
 * DESCRIPTION
 *
 *	These function provide a cpio conformant interface to the pax
 *	program.
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
 * $Log: cpio.c,v $
 * Revision 2.2  1996/10/20  12:10:20  istewart
 * Fix some definitions, add strftime & strcasecmp
 *
 * Revision 2.1  1996/10/18  21:35:01  istewart
 * Initial 2.1 port
 *
 */

#ifndef lint
static char        *ident = "$Id: cpio.c,v 2.2 1996/10/20 12:10:20 istewart Exp $";
static char        *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif /* ! lint */


/* Headers */

#include "pax.h"


/* Function Prototypes */

static void	    usage __P ((void));

/* do_cpio - handle cpio format archives
 *
 * DESCRIPTION
 *
 *	Do_cpio provides a standard CPIO interface to the PAX program.  All
 *	of the standard cpio flags are available, and the behavior of the
 *	program mimics traditonal cpio.
 *
 * PARAMETERS
 *
 *	int	argc	- command line argument count
 *	char	**argv	- pointer to command line arguments
 *
 * RETURNS
 *
 *	Nothing.
 */

#ifdef __STDC__
void		do_cpio (int argc, char **argv)
#else
void		do_cpio(argc, argv)
    int		  argc;
    char	**argv;
#endif
{
    int                 c;
    char               *dirname = "";
    Stat                st;

    /* default input/output file for CPIO is STDIN/STDOUT */
    ar_file = "-";
    names_from_stdin = 1;

    /* set up the flags to reflect the default CPIO inteface. */
    blocksize = BLOCKSIZE;
    ar_interface = CPIO;
    ar_format = CPIO;
    msgfile = stderr;

    while ((c = getopt(argc, argv, "#:D:Bacdfilmoprtuv")) != EOF) {
	switch (c) {

	case '#':
	    break;

	case 'i':
	    f_extract = 1;
	    break;

	case 'o':
	    f_create = 1;
	    f_mtime = 1;	/* automatically save mtime on write */
	    break;

	case 'p':
	    f_pass = 1;
	    dirname = argv[--argc];

	    /* check to make sure that the argument is a directory */
	    if (PAX_LSTAT(dirname, &st) < 0) {
		fatal(strerror (errno));
	    }
	    if ((st.sb_mode & S_IFMT) != S_IFDIR) {
		fatal("Not a directory");
	    }
	    break;

	case 'B':
	    blocksize = BLOCK;
	    break;

	case 'a':
	    f_access_time = 1;
	    break;

	case 'c':
	    break;

	case 'D':
	    ar_file = optarg;
	    break;

	case 'd':
	    f_dir_create = 1;
	    break;

	case 'f':
	    f_reverse_match = 1;
	    break;

	case 'l':
	    f_link = 1;
	    break;

	case 'm':
	    f_mtime = 1;
	    break;

	case 'r':
	    f_interactive = 1;
	    break;

	case 't':
	    f_list = 1;
	    break;

	case 'u':
	    f_unconditional = 1;
	    break;

	case 'v':
	    f_verbose = 1;
	    break;

	default:
	    usage();
	}
    }

#ifdef MSDOS
    setmode(fileno(msgfile), O_TEXT);
    if (names_from_stdin) {
        setmode(fileno(stdin), O_TEXT);
    }
#endif /* MSDOS */

    if (f_create + f_pass + f_extract != 1) {
	usage();
    }
    if (!f_pass) {
	buf_allocate((off_t) blocksize);
    }
    if (f_extract) {
	open_archive(AR_READ);	/* Open for reading */
	read_archive();
    } else if (f_create) {
	open_archive(AR_WRITE);
	create_archive();
    } else if (f_pass) {
	pass(dirname);
    }
    /* print out the total block count transfered */
    fprintf(stderr, "%ld Blocks\n", ROUNDUP(total, BLOCKSIZE) / BLOCKSIZE);
}


/* usage - print a helpful message and exit
 *
 * DESCRIPTION
 *
 *	Usage prints out the usage message for the CPIO interface and then
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

    printf("\r\nUsage: %s -o[Bacv]\r\n", myname);
    printf("       %s -i[Bcdmrtuvf] [pattern...]\r\n", myname);
    printf("       %s -p[adlmruv] directory\r\n", myname);

    printf("\r\nUse the nonstandard option \"-D file\" for files as input/output archives."
           "\r\nRename CPIO.EXE to PAX.EXE or TAR.EXE to get the PAX or TAR user interface.\r\n");

#ifdef DISKACC
    printf("\r\nUse the option -D with the special device names 0: and 1: to access Unix floppy"
           "\r\ndisks with cpio archives. The disk type is automatically detected.\r\n");
#endif
#else
    fprintf(stderr, "Usage: %s -o[Bacv]\n", myname);
    fprintf(stderr, "       %s -i[Bcdmrtuvf] [pattern...]\n", myname);
    fprintf(stderr, "       %s -p[adlmruv] directory\n", myname);
#endif
    exit(1);
}
