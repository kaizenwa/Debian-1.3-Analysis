/*
 * uncatfile.c	Unconcatenates a file with multiple digests.
 * Copyright (c) 1994 by Christopher Heng. All rights reserved.
 * You may not remove any of the copyright notices and/or conditions for use
 * and distribution. See COPYING for additional conditions for use and
 * distribution.
 *
 * $Id: uncatfile.c,v 2.3 1995/07/08 10:04:45 chris Released $
 */

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cftables.h"
#include "splitdigest.h"
#include "version.h"

/* macros */
#define	CONTENTLENHDR	"Content-Length: "	/* content-length header */

/* local variables */
static char rcsid[] = "$Id: uncatfile.c,v 2.3 1995/07/08 10:04:45 chris Released $";	/* for debugging purposes */
static char tempfilename[TEMPFILELEN];	/* temporary filename */
static int tempisused ;			/* flag that temp filename is opened */
static char targetname[MAXFILELEN] = "dummy";	/* target file name */
static char buffer[MAXBUFFERLEN];	/* buffer for line */
static FILE * outfile ;			/* output file pointer */
static char fromfield[] = FROMFIELD ;	/* beginning of next mail msg */

/* local functions */
static void compressfile ( char * file );
static int getline ( char * buffer, int maxlen, FILE * fp );
static void putline ( char * buffer, FILE *fp );
static int iscontentlen ( char * buffer );

/* does the dirty job of splitting the file */
void uncatfile ( FILE * infile )
{
	int gotheader, notfirst ;
	int inhdr ;
	char tempbuf[MAXFILELEN];
	char endstring[MAXBUFFERLEN];
	char savedfrom[MAXBUFFERLEN];
	size_t len ;
	cftable_t * cfp ;
	fpos_t lastpos ;
	char * s;
	
	while (!feof(infile)) {
	
	    /* open output file to a temp name - warning: the file is */
	    /* created in the current directory so that it can be */
	    /* simply renamed to the final file name - so that we */
	    /* don't need to worry about linking across different file */
	    /* systems and what not. (I told you this was a primitive */
	    /* program!) */
	    strcpy ( tempfilename, TEMPLATE );
	    if (mktemp( tempfilename ) == NULL) {
		fprintf( stderr, "%s: cannot generate temporary file"
			"name. Aborting.\n", pgmname );
		exit(EXIT_FAILURE); /* quite unlikely to happen, right? */
		/* (Warning: infile still open - exit() will close it) */
	    }

	    /* open the output file */
	    if ( (outfile = fopen ( tempfilename, WRITEMODE )) == NULL ) {
	    	fprintf( stderr, "%s: cannot open temporary file %s. Skipped.\n",
	    		pgmname, tempfilename );
		return ; /* return and better luck next time! */
	   }	
	   tempisused = 1;	/* flag that temporary file is opened */

	    /* keep reading lines to find the digest name from the header*/
	    gotheader = notfirst = 0 ;
	    inhdr = 1;	/* flag that we're in the header of the email */
	    while (getline(buffer, MAXBUFFERLEN, infile)) {
   		/* WARNING: the following code is duplicated later as well. */
   		/* Should consider putting this into a function! */
		if (!strcmp( buffer, "\n" ))
			inhdr = 0;	/* end of headers - blank line */
		/* write the buffer to output file unless: 1) we need to */
		/* kill the Content-Length header; 2) we're currently */
		/* processing a mail header; *AND* 3) the header matches */
		/* a Content-Length header */
		if (!kill_contentlen || !inhdr || !iscontentlen( buffer ))
			putline( buffer, outfile );
		if (!notfirst) {
			strcpy (savedfrom, buffer); /* save the "From " line*/
			notfirst = 1;
		}
		if (isheader( buffer, targetname, endstring, &cfp )) {
			/* found header */
			gotheader = 1;
			break ; /* break out */
		}
	   }
	   if (!gotheader) { /* if no header it's either an error or eof */
	   	fprintf(stderr, "%s: Unexpected end-of-file in %s.\n",
	   		pgmname, infilename );
	   	goto errreturn ;
	   }
	   /* now read and copy till the hdrsep is encountered */
	   while (getline(buffer, MAXBUFFERLEN, infile)) {
		if (!strcmp( buffer, "\n" ))
			inhdr = 0;	/* end of headers - blank line */
	   	if (ishdrsep( buffer, cfp )) {
	   		putline( savedfrom, outfile);
	   		getline(buffer, MAXBUFFERLEN, infile);
	   			/* get rid of blank line */
	   		break ;
	   	}
   		/* WARNING: the following code is duplicated above. */
   		/* should consider putting this into a function! */
		/* write the buffer to output file unless: 1) we */
		/* need to kill the Content-Length header; */
		/* 2) we're currently processing a mail header; */
		/* *AND* 3) the header matches a Content-Length */
		/* header */
	   	else if (!kill_contentlen || !inhdr ||
			!iscontentlen( buffer ))
			putline( buffer, outfile );
	   }
	   /* at this point, we assume all mail headers have been processed */
	   /* (ie after the hdrsep is encountered) and so we don't bother */
	   /* to check for any more Content-Length headers */

	   /* now read and copy till msgsep or end of digest marker */
	   while (getline(buffer, MAXBUFFERLEN, infile)) {
		if (ismsgsep( buffer, cfp )) {
			putline( savedfrom, outfile ); /* output "From " hdr*/
			getline(buffer, MAXBUFFERLEN, infile);
				/* get rid of blank line */
			continue ;
		}
	   	else putline( buffer, outfile ); /* output line */ 
		s = buffer + (strlen(buffer)-1);
		if (*s == '\n')
			*s = '\0'; /* get rid of new line */
		if (isend( buffer, endstring )) {
			if (fgetpos( infile, &lastpos )) {/*for repositioning*/
getposerr:			fprintf(stderr, "%s: Error reading %s.\n",
					pgmname, infilename );
				goto errreturn ;
			}
			/* just copy till next mail message */
			while (getline(buffer, MAXBUFFERLEN, infile)) {
				/* compare before we write so we won't */
				/* overshoot in writing */
				if (!strncmp(buffer, fromfield, sizeof(fromfield)-1)) {
					/* oops, overshot. Reposition */
					fsetpos( infile, &lastpos );
					break ;
				}
				/* copy */
				putline( buffer, outfile);
				/* get current position for repositioning */
				if (fgetpos( infile, &lastpos ))
					goto getposerr; /* sorry, another goto! */
			}
			break ; /* finished for now */
		}	/* end of isend() */
      
	   } /* end of while - read till msgsep or end of digest markers */

	   /* now we got to close the files and rename the output file */
	   tempisused = 0;	/* flag to prevent cleanuponsig() deleting */
	   fclose(outfile);	/* tempfile */
	   rename( tempfilename, targetname );
	   if (compression)	/* if compression is needed */
	    	compressfile(targetname);
	}

	return;

errreturn:	/* clean up on error */
	tempisused = 0;
	fclose( outfile );
	unlink( tempfilename );
	return ;
}

/* does the real cleanup - called by cleanuponsig() and the atexit function */
void realcleanup( void )
{
	if (tempisused) {	/* if temp file opened */
		fclose( outfile );
		unlink ( tempfilename ); /* just trying to be tidy, old chap */
		tempisused = 0;	/* flag done in case we get called twice */
	}
	return ;
}

/* compress file name specified */
static void compressfile ( char * file )
{
	int childpid ;	/* child process id */
	if ( (childpid = fork()) == 0 ) {	/* child */
		if ( execl( DEFCOMPRESS, DEFCOMPRESS, COMPRESSARG, file,
			NULL ) == -1 ) {
			fprintf( stderr, "%s: Cannot execute %s.\n",
				pgmname, DEFCOMPRESS );
			exit(EXIT_FAILURE);
		}
	}
	else if (childpid == -1) { /* error from fork() */
		fprintf( stderr, "%s: Cannot fork (not enough memory).\n",
			pgmname );
		return ;	/* just return */
	}
	else waitpid ( childpid, NULL, 0 ); /* wait for child to complete */
	return ;
}

/* get line from input file */
static int getline( char * buffer, int maxlen, FILE * infile )
{
	if (fgets( buffer, maxlen, infile ) == NULL) {
		if (ferror(infile) ) {
			fprintf( stderr, "%s: error reading %s.\n",
				pgmname, infilename );
			exit( EXIT_FAILURE );
		}
		return 0 ;
	}
	return 1;
}

/* put a line into the output file */
static void putline ( char * buffer, FILE * outfp )
{
	if (fputs( buffer, outfile) == EOF) {
		fprintf( stderr, "%s: error writing %s.\n",
			pgmname, tempfilename ); /* always write to tempfile */
		exit( EXIT_FAILURE );
	}
	return ;
}

/* check if the start of the buffer matches the Content-Length header */
static int iscontentlen ( char * buffer )
{
	char *s ;
	if (!strncmp ( buffer, CONTENTLENHDR, sizeof(CONTENTLENHDR)-1 ))
		return 1 ;
	return 0;
}
