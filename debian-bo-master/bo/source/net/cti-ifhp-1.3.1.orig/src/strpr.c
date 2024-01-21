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
 * $Id: strpr.c,v 1.1 1995/09/02 15:12:52 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

void resetprinter()
{
	log(4,"resetprinter: '%s'", UEL );
	write_check(STDOUT, UEL, "Cannot UEL reset printer", 0);
}

void pclresetprinter()
{
	log(4,"pclresetprinter: '%s'", PCLRESETSTR );
	write_check(STDOUT, PCLRESETSTR, "Cannot PCL reset printer", 0);
}

