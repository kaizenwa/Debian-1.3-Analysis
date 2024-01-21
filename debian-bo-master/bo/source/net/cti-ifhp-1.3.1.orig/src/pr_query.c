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
 * $Id: pr_query.c,v 1.2 1996/05/06 04:34:13 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void pr_query(fd, n)
int  fd, n;
{
	int i, len ;
	char *s;

	for (i=0; i<n; i++) {
		s = query[i];
		log(3,"Writing query string[%d] '%s'", i, s);
		len = strlen(s);
		if( writecn(fd, s, len) != len ){
			logerr(1,"Cannot write query string <%s>", s);
			fexit(FILTABORT);
		}
	}
}
