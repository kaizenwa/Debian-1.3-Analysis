/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef SERVER_H
#define SERVER_H

/*
 * $Id: server.h,v 1.2 1995/09/10 15:42:38 chuck Exp $
 */

#include <sys/types.h>
#include <sys/wait.h>

#include "defs.h"
#ifdef BSD
#include "service.h"
#endif
#include "connection.h"

#ifdef NO_POSIX_TYPES
typedef int pid_t ;
#endif


/*
 * This struct describes running servers
 */
struct server
{
	pid_t 			svr_pid ;
	time_t 			svr_start_time ;
	connection_s	*svr_conn ;
	struct service *svr_sp ;				/* service that owns this server 	*/
	int 				svr_fork_failures ;	/* number of fork(2) failures			*/
	int 				svr_exit_status ;
	bool_int 		svr_log_remote_user ;
	bool_int 		svr_writes_to_log ;	/* needed because a service may be	*/
													/* reconfigured between server 		*/
													/*	forking and exit						*/
} ;

#define SERP( p )                		((struct server *)(p))

#define SERVER_SERVICE( serp )			(serp)->svr_sp
#define SERVER_CONNECTION( serp)			(serp)->svr_conn
#define SERVER_CONNSERVICE( serp )		CONN_SERVICE( SERVER_CONNECTION( serp ) )
#define SERVER_FD( serp )					CONN_DESCRIPTOR( (serp)->svr_conn )
#define SERVER_PID( serp )					(serp)->svr_pid
#define SERVER_EXITSTATUS( serp )		(serp)->svr_exit_status
#define SERVER_STARTTIME( serp )			(serp)->svr_start_time
#define SERVER_LOGUSER( serp )			(serp)->svr_log_remote_user

#define SERVER_FORKLIMIT( serp )			\
						( (serp)->svr_fork_failures >= MAX_FORK_FAILURES )

#define server_set_pid( serp, pid )		(serp)->svr_pid = (pid)
#define server_set_exit_status( serp, status )	\
						(serp)->svr_exit_status = (status)

status_e 		server_init() ;
void				server_release() ;
status_e			server_run() ;
status_e			server_start() ;
void				server_dump() ;
void				server_end() ;
struct server	*server_lookup() ;


/*
 * Macros for compatibility
 */
#ifndef OLD_WAIT
#define PROC_EXITED( s )         WIFEXITED( s )
#define PROC_SIGNALED( s )       WIFSIGNALED( s )
#define PROC_STOPPED( s )        WIFSTOPPED( s )
#define PROC_EXITSTATUS( s )     WEXITSTATUS( s )
#define PROC_TERMSIG( s )        WTERMSIG( s )
#else
#define PROC_EXITED( s )         WIFEXITED( *(union wait *)&(s) )
#define PROC_SIGNALED( s )       WIFSIGNALED( *(union wait *)&(s) )
#define PROC_STOPPED( s )        WIFSTOPPED( *(union wait *)&(s) )
#define PROC_EXITSTATUS( s )     (((union wait *)&(s))->w_T.w_Retcode)
#define PROC_TERMSIG( s )        (((union wait *)&(s))->w_T.w_Termsig)
#endif   /* OLD_WAIT */

#endif	/* SERVER_H */

