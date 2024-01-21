/*==========================================================================
 =                                                                         =
 =                        Project CTI-Print                                =
 =                                                                         =
 = Author:                                                                 =
 =   Panos Dimakopoulos, Systems Programmer,                               =
 =   Computer Technology Institute,                                        =
 =   Division of Computing Facilities,                                     =
 =   P.O. Box 1122,                                                        =
 =   261 10  Patras,                                                       =
 =   Greece                                                                =
 =   (e-mail: dimakop@cti.gr)                                              =
 =   Tel: +30 61 992061                                                    =
 =   Fax: +30 61 993973                                                    =
 =                                                                         =
 = Created by Patrick Powell  <papowell@sdsu.edu>                          =
 =   for LPRng software Sat Aug 26 06:54:25 PDT 1995                       =
 ==========================================================================*/


/*==========================================================================
  Version CTI-LPRng-1.0
  =========================================================================*/


/*
 * $Id: readpipe.c,v 1.6 1996/07/24 21:11:00 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

#define UNSOL(msg,X) (!strncmp(msg,X,strlen(X)))
char CODE[]  = "CODE=";
char PAGECOUNT[]  = "PAGECOUNT";

/* set if pagecount found on previous line */
static int pagecount_found;

/*
	readpipe will read a line of input,
	and then decode it for status and other information
	int readpipe(int *inptr, int timeout)
		inptr - pointer to location for status value
		timeout - time for the read
	returns:
	  n	> 0  number of characters read
	  n = 0  timeout on input
	  n < 0  EOF on input
*/

int readpipe(int *inptr, int timeout)
{
	int   n, cd;
	char *s, *start, *end;
	if( inptr ) *inptr = -1;

	cd = 0;
	if ( (n=port_readline(STDOUT,readprin,sizeof(readprin)-1,timeout)) <0 ) {
		logerr(2,"Could not read from STDOUT");
		return( n );
	} else if( n > 0 ){
		log(3,"readpipe: read %d, '%s'", n, readprin);
		for( start = readprin; (cd = *start) && isspace( cd ); ++start );
	}
	if ( n == 0 || cd == 0 ) return n;

	for( start = readprin; start && *start; start = end ){
		end = strpbrk( start, "\n\r\004" );
		if( end ){
			*end++ = 0;
		}
		while( isspace(*start) ) ++start;
		if( *start == 0 ) continue;
		log(3,"readpipe: checking '%s'", start);
		if( pagecount_found ){
			log(3,"readpipe: pagecount_found %d", pagecount_found );
			pagecount_found = 0;
			npages = get_num( start );
			log(3,"readpipe: npages now %d", npages );
		}
		for( s = start; (s = getcrstr(s, CODE, sizeof(CODE)-1 ));
			s += sizeof(CODE) ) {
			cd = get_num( s );
			if( inptr ){
				*inptr = cd;
			}
			check_code( cd );
		}
		for( s = start; (s = getcrstr(s, PAGECOUNT, sizeof(PAGECOUNT)-1 ));
			s += sizeof(PAGECOUNT) ) {
			pagecount_found = 0;
			/* get_num skips only white spaces to next number */
			npages = get_num( s );
			log(4,"Pagecounter = %d", npages);
			/* if at the end of input, find it on the next line */
			if( npages < 0 ){
				pagecount_found = 1;
			}
		}
		if( getcrstr( start, job_start, strlen( job_start )-2) ) {
			job_started	= TRUE;
			log(3,"readpipe: job started");
		} else if( getcrstr( start, job_end, strlen( job_end )-2) ) {
			log(3,"readpipe: job ended");
			job_ended	= TRUE;
		}
		if( strstr( start, "Error:" ) || logall ){
			log(1,"STATUS: %s", start );
		}
	}
	return n;
}
