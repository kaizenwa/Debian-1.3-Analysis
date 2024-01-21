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
 * $Id: getcrstr.c,v 1.1 1995/09/02 15:12:40 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 * return the start of the line matching the string
 * line is delineate by \r\n or \n sequences
 * return the first non-blank character position
 */

char * getcrstr(s, match, len)
char *s;
char *match;
int len;
{
	/* count the number of \r's */
	log(7,"getcrstr: want '%s' in '%s'", match, s );
	if( (s = strstr(s, match )) ){
		return(s+strlen(match));
	}
	return(0);
}
