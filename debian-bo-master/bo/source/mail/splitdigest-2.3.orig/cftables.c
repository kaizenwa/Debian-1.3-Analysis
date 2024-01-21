/*
 * cftables.c	- functions to handle the configuration tables.
 * Copyright (c) 1994 by Christopher Heng. All rights reserved.
 * You may not remove or alter the copyright notice and/or conditions for use
 * and the conditions for use and distribution.
 * Additional terms and conditions for use and distribution are included
 * in the file COPYING, which contains the GNU General Public License
 * version 2.
 *
 * $Id: cftables.c,v 2.3 1995/07/08 10:04:45 chris Released $
 */

/* Real programmers don't comment their code. It was hard to write; it */
/* should be hard to read! :-) */

#include <stdio.h>
#include <stdlib.h>
#include "cftables.h"
#include "splitdigest.h"

static char rcsid[] = "$Id: cftables.c,v 2.3 1995/07/08 10:04:45 chris Released $" ;	/* for debugging purposes */
static cftable_t cftable[MAXCFTABLE] ;

/* local functions */
static char * getline ( FILE * fp );
static char * getlaterfield ( FILE * fp );
static int countnumargs ( char * s );
static void expandfmt ( char * buf, char * pattern, arg_t * args, int maxlen );

/* initialise the table */
int inittables ( void )
{
	FILE * fp;
	int i, errcode ;
	char * s;
	char * headersep, * messagesep ;
	
	/* open the config file */
	if ((fp = fopen( configfile, READMODE )) == NULL)
		return -1;
		
	/* read in each items, initialising cftable */
	i = errcode = 0;
	while (i < (MAXCFTABLE-1)) {
		if (((cftable[i].headerstring = getline( fp )) == NULL) ||
		    ((cftable[i].endstring = getlaterfield(fp))==NULL) ||
		    ((cftable[i].fnpattern = getlaterfield(fp))==NULL) ||
		    ((headersep = getlaterfield(fp))==NULL) ||
		    ((messagesep = getlaterfield(fp))==NULL) ) {
		    	if (feof(fp))
		    		break ;
			errcode = -1 ;
			fprintf( stderr, "%s: Missing field in %s. Aborting.\n",
				pgmname, configfile );
			break ;	/* missing field - abort */
		    }
		    if ((cftable[i].numspec = countnumargs( cftable[i].headerstring ))
		    	> MAXSCANFARGS || cftable[i].numspec < 1 ) {
		    	fprintf( stderr, "%s: Too many or too little conversion "
		    		"specifiers in %s (max %d).\n",
		    		pgmname, cftable[i].headerstring, MAXSCANFARGS );
			errcode = -1;	/* flag error */
		    	break ;	/* can't manage so many args to sscanf(), abort */
		    }
		    if ((cftable[i].numhdrsep = atoi(headersep)) <= 0 ||
		    	(cftable[i].nummsgsep = atoi(messagesep)) <= 0 ) {
		    	fprintf( stderr, "%s: Invalid number of separator"
		    		" character specified: %s %s.\n",
		    		pgmname, headersep, messagesep );
		    	errcode = -1;
		    	break ;
		    }
		    free(headersep);	/* free unneeded memory */
		    free(messagesep);
		    i++ ;	/* next entry */
	}

	/* close */
	fclose ( fp );
	if (!i)	/* force an error if nothing was successfully read */
		errcode = -1 ;
	return (errcode);
}

static int countnumargs ( char * s )
{
	int count ;

	/* count the number of "%x" where x is not '%' or '*' */
	for (count = 0; *s; s++) {
		if (*s == '%' && *++s != '%' && *s != '*' )
			count++;
	}
	return count ;
}

static char * getlaterfield ( FILE * fp )
{
	char * s ;
	if ( (s = getline(fp)) != NULL ) {
		if (*s == '\t')	/* kill preceding tab if any */
			memmove( s, s+1, strlen(s) );
	}
	return s ;
}

static char * getline ( FILE * fp )
{
	char buffer[MAXBUFFERLEN];
	char * s ;
	int len ;

	while (1) {
		if (fgets( buffer, MAXBUFFERLEN, fp ) != NULL) {
			if (buffer[0] == '#' || buffer[0] == '\n' )
				continue ; /* skip comments and blank lines */
			s = buffer + (len = strlen (buffer));
			if (*--s == '\n') {
				*s = '\0';
				len--;
			}
			if ((s = malloc(len+1)) == NULL) {
				return NULL ;
			}
			strcpy ( s, buffer );
			return s ;
		}
		else break ;
	}
	return NULL ;
}

/* compare the string against all the header values in the table */
/* returns 1 if a header was found, 0 otherwise */
int isheader ( char * s, char * fname, char *endstring, cftable_t ** cfp )
{
	cftable_t * cfptr ;	/* temp ptr */
	int rtn ;
	arg_t args[MAXSCANFARGS];	/* max number of conversion specs */

	cfptr = cftable ;	/* point to table */

	for ( ; cfptr->headerstring != NULL ; cfptr++ ) {
		if ((rtn = sscanf ( s, cfptr->headerstring,
			&args[0], &args[1], &args[2], &args[3], &args[4],
			&args[5] )) < cfptr->numspec)
			continue ; /* nope - didn't match */
		else if (rtn == EOF)
			break ;
		else {	/* matched (I hope; I don't check for >cfptr.numspec)*/
			*cfp = cfptr ;	/* save pointer */
			expandfmt( fname, cfptr->fnpattern, args, MAXFILELEN );
			expandfmt ( endstring, cfptr->endstring, args, MAXBUFFERLEN-1 );
			return 1; /* flag found */
		}
	}
	return 0 ;	/* not found! */
}

/* used to sprintf both the final output filename as well as the end string */
static void expandfmt ( char * buf, char * pattern, arg_t * args, int maxlen )
{
	char * pat ;
	char * startbuf ;

	pat = pattern ;	/* save orig pattern for error message */
	startbuf = buf ; /* save start of buffer */
	*buf = '\0';	/* make sure there's a null byte to begin with */
			/* just in case nothing matches... */

	while (*pat) {

	    while (*pat && *pat != '%') {
		*buf++ = *pat++ ;	/* copy characters */
		*buf = '\0';	/* terminate with null character */
	    }
	    if (*pat) {
		if (*++pat == '%') {
			*buf++ = '%';
			*buf = '\0';	/* terminate string */
			pat++;
		}
		else if (*pat == 's')
			strcpy(buf, args->sptr); /* copies \0 byte as well */
		else if (*pat == 'd')
			sprintf(buf, "%d", args->iptr); /* will term string itself */
		else {
			fprintf( stderr, "%s: Bad config rule: %s",
				pgmname, pattern );
			exit (EXIT_FAILURE) ;
		}
		pat++;	/* advance to next character */
		buf = buf + strlen(buf); /* point to end of string */
		args++;	/* advance to next arg to interpret */
	    }
	}	/* while (*pat) */ /* loop till no more pattern to interp */
	if (strlen(startbuf) >= maxlen) {
		fprintf (stderr, "%s: Oops - config string too long: %s.\n",
			pgmname, startbuf );
		exit (EXIT_FAILURE);
	}
	return ;
}

/* compare for the end of digest - s cannot have any newline affixed */
/* returns 1 if the end string was detected, 0 if there is no match */
int isend ( char * s, char * endstring )
{
	if (!strcmp(s, endstring))
		return 1;	/* found */
	return 0;
}

/* this function will check if undigestion is required, if not, it'll */
/* just return 0 (fail). If undigestion is required, it'll return 1 */
/* if it's a header separator and 0 otherwise */
int ishdrsep ( char *s, cftable_t * cfp )
{
	if (!undigest)	/* don't bother to work if undigestion not required */
		return 0;
	return issep( s, cfp->numhdrsep );
}

/* this function will check if undigestion is required. If not, it'll */
/* just fail, ie return 0. Otherwise, it'll return 1 if it is a message */
/* separator, and 0 if it is not */
int ismsgsep ( char * s, cftable_t * cfp )
{
	if (!undigest)
		return 0;
	return issep( s, cfp->nummsgsep );
}

/* the real separator comparison function */
int issep ( char * s, int numsep )
{
	/* while within valid length, and the character is SEPCHAR */
	for ( ; numsep && *s == SEPCHAR; numsep--, s++ )
		;
	if (*s == '\n' && !numsep) /* end of line and compared successfully */
		return 1;
	return 0;
}
