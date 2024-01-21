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
 * $Id: fexit.c,v 1.2 1996/05/06 04:33:59 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void fexit(i)
int i;
{
	log(4,"Exit value = %d", i);
	if ( monitpid > 0 )	{
		log(3,"Father murders %d", monitpid);
		kill(monitpid,SIGTERM);
		kill(monitpid,SIGCONT);
	}
	exit(i);
}
