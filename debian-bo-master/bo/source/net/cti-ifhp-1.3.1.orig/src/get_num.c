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
 * $Id: get_num.c,v 1.2 1996/04/21 19:14:14 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 * The passed argument is supposed to be a string starting off with any
 * pattern (such as "PAGES=" or "CODE=") and followed by a number.
 * The number is returned as this is the one of major concern.
 */
int get_num(s)
char *s;
{
	int i = -1;
	int c;

	if( !s ) return(i);

	/* skip over whitespace */
	while( (c = *s) && isspace(c) ) s++;
	/* now we have either a number or bad info */
	/* return 0 if bad info, -1 if end of line */
	if( c ){
		i = atoi( s );
	}
	return( i );
}

