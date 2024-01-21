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

#define EXTERN

/*
 * $Id: ifhp.c,v 1.10 1996/09/10 13:44:54 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

static const char *const idallf = "@(#) Computer Technology Institute, CTI-Print Project";
static const char *const idhp4 = "@(#)       Filter for HP LaserJet 4, V 1.2 \n" ;
static const char *const idifhp4 = "@(#) ifhp4.c 95/08/23 - V 1.15" ;

int main(argc,argv,envp)
int argc;
char *argv[];
char *envp[];
{
	int acnt_fd;
	unsigned long starttime, endtime;
	int elapsed;

	Envp = envp;
	debug = 2;

	/* save the PID */
	starttime = time( (void *)0 );
	filterpid = getpid();

	acnt_fd = Accounting_fd = dup(3);
	if( Accounting_fd >= 0 ){
		close( Accounting_fd );
		Accounting_fd = 3;
	}
	getargs( argc, argv );
	get_info();
	errorcode = FILTABORT;
	getpjlargs();

	(void)signal(SIGCHLD, SIG_IGN);
	(void)signal(SIGINT, cleanup);
	(void)signal(SIGHUP, cleanup);
	(void)signal(SIGTERM, cleanup);
	(void)signal(SIGQUIT, cleanup);
	(void)signal(SIGPIPE, SIG_IGN);

	/* open device if specified */
	if( device ) open_device();

	log(4,"ifhp: get_status %d", get_status );
#if defined(F_GETFL)
	if( get_status ){
		int status;
		log(4,"ifhp: trying fcntl" );
		status = fcntl( 1, F_GETFL );
		log(4,"ifhp: fcntl returned 0x%x, O_RDWR = 0x%x", status, O_RDWR );
		if( (status & O_RDWR) == 0 ){
			log(4,"ifhp: cannot read stdout, not getting status" );
			get_status = 0;
		}
	}
#endif

	if( get_status ){
		do_monitor();
	} else {
		/* send the necessary PJL commands to simply terminate
		 * current job, if any */
		header_info();
	}


	if( of_filter ){
		log(2,"Starting of_filter actions");
		do_of_stream(stdin, STDOUT );
	} else {
		sendjob(0, STDOUT );
	}

	/* terminate job */

	if( get_status ){
		if( monitpid > 0 ) {
			int pid, status;
			log(3,"stopping monitor %d", monitpid);
			kill(monitpid,SIGTERM);
			kill(monitpid,SIGCONT);
			pid = waitpid( monitpid, &status, 0 );
			log(3,"ifhp: monitor %d exit status %d", pid, status );
		}
		check_job_end();
		if( pagecount ){
			npages = pr_pagecount(STDOUT);
			if (npages == 0) {
				log (2, "Pagecount 0 from PJL INFO. Trying PostScript");
				npages = pr_pspagecount(STDOUT);
				if (npages == 0)  {
					log (2, "Pagecount 0 from PostScript!");
				}
			}
		}
		endtime = time( (void *)0 );
		elapsed = endtime - starttime;
		log(2,"Initial page count %d, final %d, Total pages = %d, elapsed time %d secs",
		initialpagecount, npages, npages - initialpagecount, elapsed );
		write_check(STDOUT, USTPJLOFF, "Cannot send USTPJLOFF", 0);
		write_check(STDOUT, UELPJL, "Cannot send UELPJL", 0);
		doaccnt(0);
	} else {
		header_info();
	}
	
	fexit(FILTSUCC);
	return( 0 );
}
