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
 * $Id: pr_endjob.c,v 1.2 1995/09/04 14:58:06 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void pr_endjob(fd)
int fd;
{
	char buf[MAXLINE+1];
	int i = 0;


	query[i++]= UELPJL ;
	plp_snprintf(buf,sizeof(buf)-1,"@PJL EOJ %s\r\n", job_end);
	query[i++] = buf;
	query[i++]= UEL ;

	log(4, "pr_endjob: sending query");
	pr_query(fd, i);
}

