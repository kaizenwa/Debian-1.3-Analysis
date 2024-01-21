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
 * $Id: cleanup.c,v 1.3 1996/05/06 04:33:54 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

static int exitval;

void cleanup(sig)
int sig;
{
	log(4,"Signal %s received-entering cleanup", Sigstr(sig) );

	if( monitpid > 0 ) {
        log(3,"Father murders %d", monitpid);
        kill(monitpid,SIGTERM);
        kill(monitpid,SIGCONT);
	}
	if( exitval ){
		fexit(exitval);
	} else if( getpid() != filterpid ){
		/* we are the child */
		exit(0);
    }
	exitval = FILTRETRY;
	log(3,"Job Interrupted");
	pr_endjob(STDOUT);
	write_check(STDOUT, USTPJLOFF, "Cannot send USTPJLOFF", 0);
	write_check(STDOUT, UELPJL, "Cannot send UELPJL", 0);
	exit(exitval);
}
