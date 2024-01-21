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
 * $Id: check_job.c,v 1.5 1996/07/19 23:40:08 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void check_job_end()
{
	int fftries = 0;
	int timeout = 1;
	int n, err;

	log(2,"Checking printer for job end");

	/*
	 * the printer may be tied up doing postscript or offline;
	 * it is also possible that somebody turned the printer off;
	 * Looking for the end of job string may not work if
	 * somebody screwed up by sending a bad PJL language print job
	 * We look for idle printer OR echo job string;  in fact we may not
	 * get either of these if somebody sent a bad 'change personality'
	 * command to the printer.  I suspect that by clever use of
	 * embedded printer job language commands in a file somebody could
	 * appear to have terminated their job while in actual fact it
	 * was still printing.
	 * we try to end the job when the printer is idle
	 */

	do{
		pr_endjob(STDOUT);
		if( timeout < 256 ){
			timeout += timeout; /* double it each time */
		}
		err = readpipe(&n,timeout);
		if( err < 0 ){
			log(2, "EOF reading from printer");
			fexit(FILTABORT);
		}
		log(4, "check_job_end: fftries=%d, code %d, job_ended %d",
			++fftries, n, job_ended );
	} while( !job_ended );
}
