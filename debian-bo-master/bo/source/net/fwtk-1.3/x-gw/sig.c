/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

 /*
  *      Author: Wei Xu, Trusted Information Systems, Inc.
*/
static  char    RcsId[] = "Header: ";

#include "ulib.h"
#include <sys/wait.h>
#include <signal.h>


  /****************************************
   * return value == -3 the pid alive 
   *        otherwise it died.
   * return: -1: WIFSIGNALED
   *         -2: no status
   *         -3: alive
   *         >=0: WEXITSTATUS
   * when >= -1 the child died
   ****************************************/
int handle_sigpid( sigpid )
pid_t sigpid;
{
        pid_t              temppid;
#ifdef __386BSD__   
	int                wstatus;
#else
	union  wait        wstatus; 
#endif
	int                exitstat, wopts=WNOHANG;

	temppid = waitpid(sigpid,&wstatus,wopts);

	if ( !temppid ) { /* there's no status */
		exitstat= -2;
		goto out;
	}

        if (WIFSIGNALED(wstatus) || WIFSTOPPED(wstatus) ) {
		pmsg("child died abnormally or stopped\n",0);
		exitstat= -1;
		goto out;
	 }

         if( !WIFEXITED(wstatus) ) {
		exitstat== -3;
		goto out;
	}

	 exitstat = WEXITSTATUS(wstatus);
out:
#ifdef WEI_DBG
fprintf(stderr,"handle_sigpid pid=%d: exit_status= %d\n",
                sigpid, exitstat );
#endif  
	 return exitstat; /* must >= 0 */
}

/* ******* chldsigs ***********************************************
 * list_t *pidlist must store the id as the child pid previously.
 * Check (wait) through the list of the pidlist of children. 
 * Callback when the child is exit or signaled.
 * Delete the pid on the list if the child is exited.
 ****************************************************************/
list_t	*chldsigs(pidlist,cb,data)
list_t	*pidlist;
int	(*cb)();	/* return < -1 in order not to delete 
			 * the pid on the list.
			 */
void	*data;
{
	int	ret;
	list_t	*p=pidlist;

	while(p) { 
		if( (ret=handle_sigpid(p->id))>= -1 && cb) 
			ret=cb(p->id,data,ret);
		if( ret>= -1) 
			pidlist=(list_t*)deleteListItem(pidlist,p->id);
	p=p->next;
	} 
	return pidlist;
}

void	sigexit(funct)
void	(*funct)();
{
	signal(SIGINT, funct); 	/* control c	*/
	signal(SIGQUIT,funct);	/* control /	*/
	signal(SIGTERM,funct);	/* kill(1)	*/
	signal(SIGXCPU,funct);	/* exceeds upu 	*/
}

