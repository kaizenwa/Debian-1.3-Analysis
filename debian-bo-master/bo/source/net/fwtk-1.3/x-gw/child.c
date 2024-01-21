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
#include <unistd.h>


pid_t become_child()
{
	pid_t	pid = fork(); 

	if( pid< 0) {  
		pmsg("Fork of child failed",errno);
		return -1;
	}
           /* the child */
	if( !pid ) return;
	exit(1);
}

int  dup_stdio( rfd, wfd )
int	rfd, wfd;
{
	if( (rfd != STDIN_FILENO) ) {
		if( dup2(rfd,STDIN_FILENO) != STDIN_FILENO ) {
			pmsg("dup2",1);
			return(errno);
		}
		close(rfd);
	}

	if( wfd != STDOUT_FILENO ) {
		if( dup2(wfd,STDOUT_FILENO) != STDOUT_FILENO ) {
			pmsg("dup2",1);
			return(errno);
		}
		close(wfd);
	}
	return 0;
}

pid_t pipe_sync( pr, pw, child_cb, data )
int     *pr,     /* parent read fd */
	*pw;     /* parent write df */
void  (*child_cb)();
void   *data;
{
	int	pfd[2];
	int     cfd[2];
	int     cr;     /* child read fd */
	int     cw;     /* child write fd */
	pid_t   pid;

        if( !pr || !pw ) return -1;

	if( pipe(pfd)<0 || pipe(cfd)<0 ) {
		pmsg( "pipe",1 );
		return -1;
	}

	*pr=pfd[0]; cr=cfd[0];
	*pw=cfd[1]; cw=pfd[1];

	if( (pid=fork()) < 0 ) {
		pmsg( "fork",1 );
		return -1;
	}
        if( !pid ) { /* close parent side fds */
		if( close(*pr) < 0 || close(*pw) < 0 ) {
			pmsg("close",1);
			exit(errno);
		}
		if( dup_stdio(cr,cw) ) exit(errno);

		(*child_cb)(data);
		exit(1);
	}
		/* close child side fd's */
	if( close(cr) < 0  || close(cw) ) {
		pmsg("close",1);
		return -1;
	}
	return pid;
}

