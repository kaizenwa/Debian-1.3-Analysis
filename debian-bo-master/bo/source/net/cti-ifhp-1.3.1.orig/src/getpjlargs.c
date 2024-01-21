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
 * $Id: getpjlargs.c,v 1.1 1995/09/02 15:12:40 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void getpjlargs()
{
	plp_snprintf(job_start,sizeof(job_start)-1,"NAME=\"Start of CTI-Print Job: %d\"",getpid());
	plp_snprintf(job_end,sizeof(job_start)-1,"NAME=\"End of CTI-Print Job: %d\"", getpid() );
}

