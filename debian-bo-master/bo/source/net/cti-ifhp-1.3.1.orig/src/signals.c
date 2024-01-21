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
 * $Id: signals.c,v 1.6 1996/09/10 13:45:01 papowell Exp $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


void slay(sig)
int sig;
{
	log(4,"slay: signal %s received", Sigstr(sig));
	fexit(FILTSUCC);
}

void nochildren( int sig )
{
	int pid, status;
	status = 0;

	log(4,"nochildren: SIGCHLD received by father");
#if defined(HAVE_WAITPID)
	pid = waitpid( -1,&status,WNOHANG );
#else
	pid = wait3( &status,WNOHANG,(void *)0 );
#endif
	log(4,"nochildren: pid  %d, status 0x%x", pid, status );
	if( pid <= 0 || WIFSTOPPED(status)){
		log(4,"nochildren: ignoring status" );
		return;
	}

	/* check to see if the child has simply stopped or started */
	if( (WIFEXITED(status) && WEXITSTATUS(status))|| WIFSIGNALED(status) ){
		if( job_ended == 0 ){
			log(3,"Child died, status 0x%x", status);
			cleanup(SIGCHLD);
			fexit(FILTRETRY);
		}
	}
	/* reset the signal handler - necessary for SVR4 */
	(void)signal(SIGCHLD, nochildren);
}

void pr_late( int sig )
{
	log(5,"pr_late: started");
	alrmflag = 1;
	(void)signal(SIGALRM,pr_late);
}
