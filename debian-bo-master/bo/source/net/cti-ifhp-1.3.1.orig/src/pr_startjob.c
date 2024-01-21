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
 * $Id: pr_startjob.c,v 1.1 1995/09/02 15:12:48 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void pr_startjob(fd)
int fd;
{
	char buf[MAXLINE+1];
	int i = 0;

	plp_snprintf(buf,sizeof(buf)-1,"@PJL JOB %s\r\n", job_start);
	query[i++] = buf;

	log(4, "pr_startjob: starting");
	pr_query(fd, i);
}
