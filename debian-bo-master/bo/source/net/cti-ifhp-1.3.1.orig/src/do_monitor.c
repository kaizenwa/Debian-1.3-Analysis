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
 * $Id: do_monitor.c,v 1.8 1996/11/14 19:56:26 papowell Exp papowell $
 */

#include "portable.h"
#include "hp4.h"
#include "common.h"

/*
 * set up the child process to do the monitoring and synchronizaiton
 */


void pr_ustatus(fd,pjlstr)
int fd;
char *pjlstr;
{
	int i = 0;
	query[i++]=pjlstr;
	pr_query(fd, i);
}

void do_monitor()
{
	int printer_code;
	int n, err;

   	/*
	* Try to synchronise with printer; use 60 secs as good default
	*/
	if( pr_synch(STDOUT, 60 )==FALSE ) {
		log(2,"Failed to synchronise with printer");
		fexit(FILTRETRY);
	} else {
		log(2,"Synchronised with printer");
	}

	/* send the start of job string */
	pr_startjob(STDOUT);

   /*
	* Check printer status
	*/

	do {
		printer_ok = FALSE;
		printer_code = 0;
		pr_ustatus(STDOUT, INFOSTATUS);	/* job status on */
		err = readpipe( &printer_code,4);
		n = printer_code/1000; /* code group */
		if( err < 0 ){
			log (2, "EOF reading from printer");
			fexit(FILTABORT);
		} else if( err != 0
			&& (n == 10 || n == 11 || printer_code == 35078 ) ){
			/*
			* These printer codes indicate that it is
			* safe to print:
			*
			* 10xxx: Informational messages.
			* 11xyy: Background Paper Mount (paper is out in some
			*        tray, but the same format is available in
			*        another tray).
			* 35078: Entered powersave mode.
			*/
			printer_ok = TRUE;
		}
	} while( printer_ok==FALSE );

	/* get initial page count */
	if( pagecount ){
		initialpagecount = pr_pagecount(STDOUT);
	}
	if (initialpagecount == 0) {
		log (2, "Pagecount 0 from PJL INFO. Trying PostScript");
		initialpagecount = pr_pspagecount(STDOUT);
		if (initialpagecount == 0)  {
			log (2, "Pagecount 0 from PostScript!");
		}
	}

	log(2,"Initial page count %d", initialpagecount );
	doaccnt(1);

	/* pr_ustatus(STDOUT,"@PJL USTATUS DEVICE = VERBOSE\r\n"); */
	monitpid = fork();
	log(4,"Right after fork with monitpid=%d", monitpid);
	if( monitpid == -1 ){
		logerr(1,"Cannot fork process");
		fexit(FILTABORT);
	} else if( monitpid == 0 ){
		log(4,"Child with pid=%d",getpid());
		do{
			n = readpipe((int *)0, 30 );
		}while( n >= 0 );
		exit(0);
	}
	log(4,"Father pid=%d, child %d",getpid(), monitpid);
}
