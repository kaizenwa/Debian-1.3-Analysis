/*
 * main.c
 * Copyright (c) 1994,1995 by Christopher Heng. All rights reserved.
 * You may not remove or alter the copyright notice and/or the conditions for
 * use and distribution. See COPYING for additional conditions for use and
 * distribution. (The file COPYING contains the GNU General Public License
 * version 2.)
 *
 * Program to undigest a file comprising one or more digest files into
 * separate digests.
 *
 * Usage: splitdigest [options] [file...]
 * Options include
 *	-C <config>	Use configuration file specified.
 *	-c		Compress output file. (Obsolete)
 *	-h		Display usage.
 *	-l		Suppress the removal of the Content-Length: header.
 *	-o <outdir>	Output directory to place files (will be created
 *			if it does not exist).
 *	-s		Separate the digests only; do not undigest. (Obsolete)
 *	-t		Leave output file as uncompressed text file (default).
 *			(Obsolete)
 *	-u		Undigest while separating digests (default). (Obsolete)
 *	-v		Display version number.
 *
 * $Id: main.c,v 2.6 1995/07/08 10:04:45 chris Released $
 */

#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include "cftables.h"
#include "splitdigest.h"
#include "version.h"

/* the following are for easy identification for bug reports and */
/* debugging purposes */
static char rcsid[] = "$Id: main.c,v 2.6 1995/07/08 10:04:45 chris Released $";
static char verstr[] = VERSTR;
static char shusagestr[] = SHUSAGESTR ;

/* global variables */
int compression = DEFTOCOMPRESS ;	/* whether to compress or not */
					/* (obsolete) */
#if (OLDMETHOD == 1)
int undigest = 0;			/* (obsolete) */
#else
int undigest = 1;	/* whether to undigest or not (by default) */
			/* (obsolete) */
#endif
int kill_contentlen = 1 ;	/* kill Content-Length lines */
char * pgmname ;	/* program name */
char * configfile = DEFCONFIG;	/* configuration filename */
char * infilename = "stdin" ;
char * newdir = NULL ;	/* directory to extract the files */

/* local functions */
static void cleanuponsig ( int sig );
static void cleanuponexit( void );
static int changedir( char * dir );

int main ( int argc, char ** argv )
{

	static char optstring[] = "C:chlo:stuv" ; /* options for getopt() */
	char * currdir ;
	FILE * infile ;
	int i, c ;

	/* form pgmname without path prefix */
	if ((pgmname = strrchr( argv[0], '/' )) == NULL)
		pgmname = argv[0] ;
	else pgmname++;

	/* trap SIGINT so that we can clean up temporary files */
	if (signal( SIGINT, SIG_IGN ) != SIG_IGN) {
		signal( SIGINT, cleanuponsig );
	}
	/* register an atexit function */
	atexit( cleanuponexit );

	while ( (c = getopt(argc, argv, optstring)) != EOF ) {
		switch( c ) {
			case 'C':	/* specify another config file */
				configfile = optarg ;
				break;
			case 'c':	/* compress output file */
				compression = 1; /* obsolete */
				break ;
			case 'h':	/* usage help requested */
				fprintf( stdout, "%s\n%s", verstr, USAGESTR );
				return EXIT_SUCCESS;
					/* quit immediately */
				break ;
			case 'l':	/* preserve Content-Length header */
				kill_contentlen = 0;
				break ;
			case 'o':	/* output directory specified */
				newdir = optarg;
				break ;
			case 's':	/* separate digests only (obsolete) */
				undigest = 0;
				break;
			case 't':	/* switch off compression (obsolete) */
				compression = 0;
				break ;
			case 'u':	/* undigest (obsolete) */
				undigest = 1;
				break;
			case 'v':	/* version number request */
				fprintf( stdout, "%s\n", verstr );
				return EXIT_SUCCESS;
					/* quit immediately */
				break ;
			default:	/* bad option */
				fprintf( stderr, "\n%s\n", shusagestr);
				return EXIT_FAILURE;
				break ;
		}
	}
	/* get current directory and save it */
	for (currdir = NULL, i = 128; currdir == NULL ; i+=128) {
		if ((currdir = getcwd( NULL, i )) == NULL) {
			if (errno == ERANGE)
				continue ;
			else
				goto nomemleft ;
		}
	}

	/* make directory and change to it if applicable */
	if (newdir != NULL && changedir(newdir))
		return EXIT_FAILURE ;
	/* load the configuration tables */
	if (inittables())
		return EXIT_FAILURE ;
	/* if no args assume stdin is the input */
	if (optind >= argc) {
		if (isatty(fileno(stdin))) {
			fprintf( stderr, "%s\n", shusagestr );
			return EXIT_FAILURE ;
		}
		uncatfile( stdin );
	}
	else {	/* invoked uncatfile for each arg */
		for (i = optind; i < argc; i++) {
			if (argv[i][0] == '/') {
				if ((infilename = malloc(strlen(argv[i])+1))
					== NULL)
					goto nomemleft ;
				strcpy(infilename, argv[i]);
			}
			else {
			/* we need to make infilename a full path because */
			/* we may have changed directory if the user */
			/* has specified -o xxx */
			    if ((infilename = malloc(
				strlen(currdir)+strlen(argv[i])+2 ))==NULL) {
nomemleft:				
				fprintf( stderr, "%s: out of memory.\n",
					pgmname );
				return EXIT_FAILURE ;
			    }
			    strcpy(infilename, currdir);
			    strcat(infilename, "/");
			    strcat(infilename, argv[i]);
			}
			if ((infile = fopen ( infilename, READMODE ))==NULL) {
				fprintf( stderr, "%s: cannot open %s. "
					"Skipped.\n", pgmname, argv[i] );
				continue;
			}
			uncatfile( infile );
			fclose(infile);
			free(infilename);
		}
	}
	return EXIT_SUCCESS;

}

/* clean up on SIGINT */
static void cleanuponsig ( int sig )
{
	exit(EXIT_SIGINT) ;	/* the atexit() function will do the cleaning*/
}

/* atexit cleanup function */
static void cleanuponexit( void )
{
	realcleanup();
}

/* change directory to specified directory. Create if it does not exist */
static int changedir ( char * dir )
{
	struct stat buf ;
	int mask, mode ;

	/* we stat to see if the "dir" string is a directory. If */
	/* it doesn't exist we'll create it. If it is a file */
	/* we'll abort. We'll also abort if the directory cannot */
	/* be created. The directory is created with the default */
	/* umask */

	if (!access(dir, F_OK)) { /* directory/file exists */
		/* now make sure it is a directory */
		if (stat(dir, &buf)) {
			fprintf( stderr, "%s: Error - can't stat %d.\n",
					pgmname, dir );
			return -1 ;
		}
		if (! S_ISDIR(buf.st_mode)) {
			fprintf( stderr, "%s: Error - %s is not a directory.\n",
					pgmname, dir );
			return -1 ;
		}
	}
	else {
		/* get umask */
		mask = umask( 0 );
		umask ( mask ); /* restore to original */
		mode = S_IRUSR|S_IWUSR|S_IXUSR|
			((mask&S_IRGRP)?0:S_IRGRP)|
			((mask&S_IWGRP)?0:S_IWGRP)|
			((mask&S_IXGRP)?0:S_IXGRP)|
			((mask&S_IROTH)?0:S_IROTH)|
			((mask&S_IWOTH)?0:S_IWOTH)|
			((mask&S_IXOTH)?0:S_IXOTH);
		if (mkdir(dir, mode)) { /* make dir if it doesn't exist */
			/* couldn't make directory. exit with error message */
			fprintf ( stderr, "%s: Error - Unable to make %s.\n",
				pgmname, dir );
			return -1 ;
		}
	}
	/* now change to the directory */
	if (chdir(dir)) {
		fprintf ( stderr, "%s: Error - Unable to change to directory %s\n",
			pgmname, dir );
		return -1 ;
	}
	return 0;
}
