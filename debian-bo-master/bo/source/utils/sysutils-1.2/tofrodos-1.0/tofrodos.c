/*
	tofrodos.c	Converts text files between DOS and Unix formats.
	Copyright (c) 1996 by Christopher S L Heng. All rights reserved.

	$Id: tofrodos.c 1.2 1996/06/11 21:53:07 chris Exp $
*/

/* this should always be first */
#include "config.h"

/* standard headers */
#include <signal.h>	/* signal() */
#include <stdio.h>	/* FILE functions */
#include <stdlib.h>	/* EXIT_SUCCESS */
#include <string.h>	/* strrchr(), strlen(), strcpy(), strcat() */
#include <sys/stat.h>	/* stat() */

/* conditionally included headers */
#if defined(MSDOS)
#include <fcntl.h>	/* O_BINARY */
#include <io.h>		/* chmod(), setmode(), isatty() */
#endif

#if defined(HAVE_GETOPT_H)
#include <getopt.h>	/* optind */
#endif

#if defined(HAVE_MKTEMP_H)
#include MKTEMP_HEADER
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h>	/* chmod(), mktemp(), isatty() */
#endif

/* our headers */
#include "emsg.h"
#include "tofrodos.h"
#include "utility.h"
#include "version.h"

/* macros */
#define	BAKEXT		".bak"	/* backup file extension */
#define	DIRSLASH	'/'	/* works for both MSDOS and Unix */
#define	MKTEMP_TEMPL	"XXXXXX"
#define	NEWBUFSIZ	16384	/* buffer size for the files */

/* conditional macros */
#if defined(MSDOS)
#if !defined(_MAX_DIR) || (_MAX_DIR < 260)	/* MAXDIRSIZE */
#define MAXDIRSIZE	260
#else
#define	MAXDIRSIZE	_MAX_DIR
#endif
#if !defined(_MAX_NAME) || (_MAX_NAME < 260)	/* MAXFILESIZE */
#define MAXFILESIZE	260
#else
#define	MAXFILESIZE	_MAX_NAME
#endif
#if !defined(_MAX_PATH) || (_MAX_PATH < 260)	/* MAXPATHSIZE */
#define	MAXPATHSIZE	260
#else
#define	MAXPATHSIZE	_MAX_PATH
#endif
#endif

#if defined(MSDOS)
#define	INFILEMODE	"rb"
#define	OUTFILEMODE	"wb"
#else
#define	INFILEMODE	"r"
#define	OUTFILEMODE	"w"
#endif

#if !defined(MSDOS)
#define	CURRENTDIR	"./"
#endif

/* global variables */
int abortonerr ; /* 1 if should abort when there is error in any file */
		/* in a list of files, 0 carry on (default) */
int alwaysconvert ; /* convert all \r\n to \r\r\n when direction */
		/* is UNIXTODOS, and delete all \r when direction is */
		/* DOSTOUNIX */
int direction = DEFDIRECTION ; /* UNIXTODOS or DOSTOUNIX */
int forcewrite ; /* convert even if file is not writeable */
char * progname = VERSN_PROGNAME ;/* name of binary (ie, argv[0]) */
int overwrite = 1 ;	/* 1 = overwrite original file, 0 = make backup */
int verbose ;

/* local variables */
static char * infilename = "stdin" ;
static unsigned short origfilemode ;	/* file mode of original file */
static FILE * tempfp ;
static char * tempfilename ;

/* local functions */
static int checkmode ( char * filename );
static int convert ( FILE * infp, FILE * outfp );
static int openandconvert ( char * filename );

/*
	main

	tofrodos converts ASCII text files to/from a DOS CR-LF deliminated
	form from/to a Unix LF deliminated form.

	Usage: tofrodos [options] [file...]

	Exit codes:
		EXIT_SUCCESS	success	(stdlib.h)
		EXIT_ERROR	error	(tofrodos.h)
*/
int main ( int argc, char ** argv )
{
	/* initialise and parse the options */
	if (init( argv[0] ) || parseargs( argc, argv ))
		return EXIT_ERROR ;

	/* check if we are to convert from stdin */
	if (argc == optind) {
	    if (isatty( fileno( stdin ) )) {
		/* stdin must be redirected else you should supply a */
		/* filename. */
		fprintf( stdout, EMSG_NOFILENAME, progname );
		return EXIT_ERROR ;
	    }
	    /* otherwise stdin has been redirected */
#if defined(MSDOS)
	    /* need to make sure the input and output files are binary */
	    /* on MSDOS */
	    setmode( fileno( stdin ), O_BINARY );
	    setmode( fileno( stdout ), O_BINARY );
#endif
	    return openandconvert( NULL );
	}

	/* if we reach here, we have a (list?) of files to convert */
	/* (ignore stdin) */
	while (optind < argc) {
	    if (verbose)
		puts( argv[optind] );
	    if (openandconvert( argv[optind] ) != 0 && abortonerr)
		return EXIT_ERROR ;
	    optind++ ;
	}

	return EXIT_SUCCESS ;
}

/*
	sighandler

	Handles SIGINT and SIGTERM. Prints a message, closes and
	deletes the temporary files and quits with EXIT_ERROR.

        It never returns (and Watcom C knows it).
*/
void sighandler ( int sig )
{
	/* restore signal handler, in case we have the old unsafe */
        /* behaviour */
	signal( sig, sighandler );

	/* print error message if verbose */
	if (verbose)
		fprintf( stderr, EMSG_SIGNAL, progname );

	/* close the temporary file and delete it */
	if (tempfp != NULL) {
		fclose( tempfp );
		tempfp = NULL ;
	}
	if (tempfilename != NULL) {
		remove( tempfilename );
                tempfilename = NULL ;
	}

        exit( EXIT_ERROR );
}

/* ---------------------------- local functions --------------------- */
/*
	checkmode

	Checks that the file we are supposed to convert is indeed
	writeable. We don't really need for it to be writeable, since
	we actually open a new file and eventually delete the current
	file.

	However, if a file is marked not-writeable, we should at least
	respect the user's choice and abort unless he flags the
	forcewrite flag.

	At the same time we also save the current mode of the file
	so that we can set the converted file to the same mode. The
	value is saved in origfilemode.

	Returns: 0 on success, -1 on error.

	If -1 is returned, it could mean one of few things:
	1) some component of the path was not valid (directory or the file
	itself) (DOS/Unix) or search permission was denied (Unix)
	2) the file is not readable
	3) the file is not writeable and forcewrite is zero.
	An error message is displayed on error.
*/
static int checkmode ( char * filename )
{
	struct stat statbuf ;

	/* get the file information */
	if (stat( filename, &statbuf )) {
		/* couldn't stat the file. */
		fprintf( stdout, EMSG_ACCESSFILE, progname, filename );
		return -1 ;
	}
	/* save the mode */
	origfilemode = statbuf.st_mode ;
	/* check if file can be read - this is actually redundant for */
	/* DOS systems. */
	if (!(origfilemode & S_IRUSR)) { /* not readable */
		fprintf( stdout, EMSG_NOTREADABLE, progname, filename );
		return -1 ;
	}
	/* check if file can be written to, if forcewrite is 0 */
	if (!forcewrite && !(origfilemode & S_IWUSR)) { /* not writeable */
		fprintf( stdout, EMSG_NOTWRITEABLE, progname, filename );
		return -1 ;
	}
	return 0 ;
}

/*
	convert

	Does the actual work of converting infp to outfp.

	If direction is DOSTOUNIX, "\r\n" pairs will be converted to
	'\n'. However, standalone '\r' without a '\n' immediately
	following will not be eliminated unless alwaysconvert is
	nonzero.

	If direction is UNIXTODOS, '\n' will be converted to "\r\n".
	However "\r\n" pairs are not converted to '\r\r\n' unless
	alwaysconvert is nonzero.

        Returns 0 on success, -1 on error.
*/
static int convert ( FILE * infp, FILE * outfp )
{
	int prevch ;
	int c ;

	/* actually it is very simple to do the conversion in DOS */
	/* because the stdio library does this work automatically for */
	/* us. But because we want this program to work on Linux as */
	/* well, a little bit of work stands before us (but only a little). */

	prevch = EOF ;

	if (direction == UNIXTODOS) {
		/* basically we convert all newlines to "\r\n" unless */
		/* the file is already in "\r\n" format. The problem here */
		/* is when you have special situations like a Unix */
		/* text file with lines that have a '\r' just */
		/* before a '\n'. These lines will */
		/* not be converted to "\r\r\n" since the function */
                /* below assumes the line has already been converted. */
                /* To force the conversion of all \n to \r\n regardless */
		/* of preceding characters, set alwaysconvert to 1. */
		while ( (c = getc( infp )) != EOF ) {
			if (c == '\n' && (!alwaysconvert && prevch != '\r')) {
				if (putc( '\r', outfp ) == EOF)
					break ;
			}
                        /* always emit the current character */
			if (putc( c, outfp ) == EOF)
                        	break ;
			prevch = c ;                        	 
                }
	}
	else if (direction == DOSTOUNIX) {
	    if (!alwaysconvert) {
		/* basically we withhold emitting any '\r' until we */
		/* are sure that the next character is not a '\n'. */
		/* If it is not, we emit the '\r', if it is, we */
                /* only emit the '\n'. */
		while ( (c = getc( infp )) != EOF ) {
			if (prevch == '\r') {
				/* '\r' is a special case because we don't */
				/* emit a '\r' until the next character */
				/* has been read */
				if (c == '\n') { /* a "\r\n" pair */
					/* discard previous '\r' and */
					/* just put the '\n' */
					if (putc( c, outfp ) == EOF)
						break ;
				}
				else {	/* prevch was a standalone '\r' */
                                	/* emit the standalone '\r' */
					if (putc( '\r', outfp ) == EOF)
						break ;
					/* emit the current character if */
                                        /* it is not a '\r' */
					if (c != '\r') {
						if (putc( c, outfp ) == EOF)
							break ;
                                        }
				}
			}
			else { /* prevch was not '\r' */
				/* emit current character if it is not */
				/* a '\r' */
				if (c != '\r') {
					if (putc( c, outfp ) == EOF)
						break ;
				}
			}
			prevch = c ;
		}
	    }	/* alwaysconvert == 0 */
	    else { /* eliminate all '\r' */
		while ((c = getc( infp )) != EOF) {
			if (c != '\r') {
				if (putc( c, outfp ) == EOF)
					break ;
			}
			/* else skip all carriage returns */
		}
	    }
	}
	else {
		fprintf( stderr, EMSG_INTERNAL, progname, EINTNL_DIRECTION );
		return -1 ;
	}

	/* if we reach here, either we've reached an EOF or an error */
	/* occurred. */
	if (!feof( infp )) { /* error */
		fprintf( stderr, EMSG_CONVERT, progname, infilename );
                return -1 ;
	}
	return 0 ;
}

/*
	openandconvert

	Called to open the files and convert the contents. If you want
	it to convert stdin to stdout, call it with NULL as the filename
	argument; otherwise pass the function the name of the input file.

	Returns: 0 on success, -1 on error. Error messages will be
        displayed on error before returning.
*/
static int openandconvert ( char * filename )
{
	FILE * infp ;
        FILE * outfp ;
	int err ;
	char * bakfilename ;
#if defined(MSDOS)
	char drv[_MAX_DRIVE];
	char dir[MAXDIRSIZE];
	char fname[MAXFILESIZE];
	char tempname[MAXPATHSIZE];
#else
	char * s ;
	char * t ;
	size_t len ;
	int replacech ;
#endif
#if NEWBUFSIZ > BUFSIZ
	char * inbufptr ;
	char * outbufptr ;
#endif

	/* make sure we initialise */
	bakfilename = NULL ;

	if (filename != NULL) { /* stdin is not redirected */

	    /* check for appropriate permissions on the file */
            /* also saves the mode in origfilemode */
	    if (checkmode( filename ))
            	return -1 ;

	    /* we need to create a temporary and backup filename (if */
	    /* applicable) in the same directory */
	    /* as our file. This is easy to do for DOS since we have the */
	    /* _splitpath(), _makepath() functions. */
#if defined(MSDOS)
	    _splitpath( filename, drv, dir, fname, NULL );
	    _makepath( tempname, drv, dir, MKTEMP_TEMPL, NULL );
	    tempfilename = xstrdup( tempname );
	    if (!overwrite) {
		_makepath( tempname, drv, dir, fname, BAKEXT );
		if (!strcmp( tempname, filename )) {
			fprintf( stderr, EMSG_BAKFILENAME, filename );
			goto err_freetempfn ;
		}
		bakfilename = xstrdup( tempname );
	    }
#else	/* not MSDOS */
	    /* check if there is a path prefix */
	    if ((s = strrchr( filename, DIRSLASH )) != NULL) {
		*s = '\0';
		replacech = 1 ;
		len = strlen( filename ) ;
		t = filename ;
	    }
	    else {
	    	replacech = 0 ;
	    	len = sizeof(CURRENTDIR) - 1 ;
	    	t = CURRENTDIR ;
	    }
	    tempfilename = xmalloc( len + sizeof(MKTEMP_TEMPL) );
	    strcpy( tempfilename, t );
	    strcat( tempfilename, MKTEMP_TEMPL );
	    if (!overwrite) {
		/* for the backup file in Unix systems, just append a */
		/* .bak to the existing filename */
		bakfilename = xmalloc( len + sizeof(BAKEXT) );
		strcpy( bakfilename, filename );
		strcat( bakfilename, BAKEXT );
	    }
	    if (replacech)
		*s = DIRSLASH ;
#endif
	    /* make the temp filename */
	    if (mktemp( tempfilename ) == NULL) {
		fprintf( stderr, EMSG_NOTEMPNAME, progname );
err_freebakfn:
		if (!overwrite && bakfilename != NULL)
			free( bakfilename );
#if defined(MSDOS)
err_freetempfn:
#endif
		free( tempfilename );
		tempfilename = NULL ;
		return -1 ;
	    }

	    /* open the filename as the input file */
	    if ((infp = fopen( filename, INFILEMODE )) == NULL) {
		fprintf( stderr, EMSG_OPENFILE, progname, filename );
		goto err_freebakfn ;
	    }
	    /* associate the infilename with the filename for error */
	    /* messages */
	    infilename = filename ;

	    /* open the temp file as the output file */
	    if ((tempfp = fopen( tempfilename, OUTFILEMODE )) == NULL) {
		fprintf( stderr, EMSG_OPENFILE, progname, tempfilename );
		fclose( infp );
		goto err_freebakfn ;
	    }
	    outfp = tempfp ;

	} /* if filename != NULL */
	else { /* filename == NULL, ie stdin is redirected */
		infp = stdin ;
		outfp = stdout ;
	}

#if NEWBUFSIZ > BUFSIZ
	/* make sure that we have a big input and output buffer */
	inbufptr = outbufptr = NULL ;
	/* (don't use xmalloc() because it we can't get what we want, */
	/* we just don't bother, and go ahead with the minimum) */
	if ((inbufptr = malloc( NEWBUFSIZ )) != NULL)
		setvbuf( infp, inbufptr, _IOFBF, NEWBUFSIZ );
	if ((outbufptr = malloc( NEWBUFSIZ )) != NULL)
		setvbuf( outfp, outbufptr, _IOFBF, NEWBUFSIZ );
#endif
	/* do the conversion */
	err = convert( infp, outfp );

	/* close the files */
	fclose( infp );
	fclose( outfp );

	if (filename == NULL) {
	    /* remove the output file handle from the global to avoid */
	    /* double attempts to close the same file */
	    tempfp = NULL ;
	}

#if NEWBUFSIZ > BUFSIZ
	/* got to free buffers we allocated first */
	if (inbufptr != NULL)
		free( inbufptr );
	if (outbufptr != NULL)
		free( outbufptr );
#endif

	if (filename != NULL) { /* stdin was not redirected */

	    if (err) { /* there was an error */
            	/* delete the temp file since we've already created it */
            	remove ( tempfilename );
		goto err_freebakfn ;
	    }

	    /* if we need to back up, delete any backup files of the */
	    /* same name first (in case we are running on DOS where */
	    /* a rename does not delete the file automatically) */
	    if (!overwrite) {
		/* remove existing backup files of same name */
		remove( bakfilename );
		/* rename the original file to the back up name */
		rename( filename, bakfilename );
	    }
	    else { /* if we do not need to back up delete the original file */
#if defined(MSDOS)
		/* for MSDOS, we need to make sure the file is writeable */
		/* otherwise we cannot delete! Under Unix, this is not */
		/* necessary, since a file can be deleted if we */
		/* have write permissions for the directory */
                chmod( filename, S_IRUSR|S_IWUSR );
#endif
		remove( filename );
	    }

	    /* rename the temp file to the original file name */
	    rename( tempfilename, filename );

	    /* remove the temp file name from the global for our */
	    /* signal handler*/
	    tempfilename = NULL ;

	    /* free memory we allocated */
	    if (!overwrite && bakfilename != NULL)
		free( bakfilename );

	    /* change the file mode to reflect the original file mode */
            chmod( filename, origfilemode );

	}	/* stdin was not redirected */

	return err ;
}
