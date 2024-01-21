/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: int.c,v 1.2 1995/09/10 15:42:38 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <syslog.h>
#include <errno.h>
#include <signal.h>

#include "config.h"
#include "connection.h"
#ifdef BSD
#include "service.h"
#endif
#include "server.h"
#include "int.h"

void msg() ;
char *inet_ntoa() ;
unsigned long inet_addr() ;

typedef struct intercept *(*initfunc)() ;

struct lookup_table
{
	initfunc	initializer ;
	int		socket_type ;
} ;

extern struct intercept *di_init() ;
extern struct intercept *si_init() ;

static struct lookup_table intercept_lookup_table[] =
	{
		{ di_init,			SOCK_DGRAM },
		{ si_init,			SOCK_STREAM },
		{ NULL }
	} ;

			
/*
 * This variable has file scope for the benefit of the signal handler
 */
static struct intercept *intp ;



PRIVATE initfunc find_initializer( type )
	int type ;
{
	struct lookup_table *ltp ;

	for ( ltp = intercept_lookup_table ; ltp->initializer ; ltp++ )
		if ( ltp->socket_type == type )
			return( ltp->initializer ) ;
	msg( LOG_ERR, "find_initializer", "No initializer for type %d", type ) ;
	_exit( 0 ) ;
	/* NOTREACHED */
}


/*
 * This function is the interface of the intercept code with the rest of 
 * the program. 
 */
void intercept( serp )
	struct server *serp ;
{
	struct service *sp = SERVER_SERVICE( serp ) ;
	initfunc initializer ;
	PRIVATE void start_server() ;
	PRIVATE void terminate_server() ;

#ifdef DEBUG_INTERCEPTOR
	if ( debug.on )
	{
		msg( LOG_DEBUG, "intercept", "%d is sleeping", getpid() ) ;
		sleep( 10 ) ;
	}
#endif

	initializer = find_initializer( SVC_SOCKET_TYPE( sp ) ) ;
	intp = (*initializer)( serp ) ;
	start_server( intp ) ;
	(*intp->int_ops->mux)() ;
	terminate_server( intp ) ;
	/*
	 * the terminate_server function should not return but even if it
	 * does, child_process will do the _exit.
	 */ 
}



/*
 * Create a socket and bind it to (INADDR_LOOPBACK,0)
 */
PRIVATE int get_server_socket( ip )
	struct intercept *ip ;
{
	struct service *sp = SERVER_SERVICE( INT_SERVER( ip ) ) ;
	struct sockaddr_in *sinp = INT_LOCALADDR( ip ) ;
	int sd ;
	int size ;
	char *func = "get_server_socket" ;

	if ( ( sd = socket( AF_INET, SVC_SOCKET_TYPE( sp ), 0 ) ) == -1 )
		int_fail( ip, "socket creation" ) ;
	
	sinp->sin_family = AF_INET ;
#ifdef INADDR_LOOPBACK
	sinp->sin_addr.s_addr = htonl( INADDR_LOOPBACK ) ;
#else
	sinp->sin_addr.s_addr = inet_addr( "127.0.0.1" ) ;
#endif
	sinp->sin_port = 0 ;

	if ( bind( sd, SA( sinp ), sizeof( *sinp ) ) == -1 )
		int_fail( ip, "bind" ) ;
	
	size = sizeof( *sinp ) ;
	if ( getsockname( sd, SA( sinp ), &size ) == -1 )
		int_fail( ip, "getsockname" ) ;
	
	if ( debug.on )
		msg( LOG_DEBUG, func, "address = %s, port = %d",
			inet_ntoa( sinp->sin_addr ), ntohs( sinp->sin_port ) ) ;
		
	if ( ip->int_socket_type == SOCK_STREAM )
		(void) listen( sd, LISTEN_BACKLOG ) ;
	
	return( sd ) ;
}


PRIVATE void start_server( ip )
	struct intercept *ip ;
{
	struct server		*serp = INT_SERVER( ip ) ;
	struct service		*sp = SERVER_SERVICE( serp ) ;
	int					server_socket ;
	pid_t					pid ;
	void					child_process() ;

	server_socket = get_server_socket( ip ) ;
	
	pid = fork() ;

	switch ( pid )
	{
		case -1:
			int_fail( ip, "fork" ) ;
			/* NOTREACHED */
		
		case 0:
			conn_set_descriptor( SERVER_CONNECTION( serp ), server_socket ) ;
			svc_make_external( sp ) ;				/* avoid looping */
			child_process( serp ) ;
			/* NOTREACHED */
		
		default:
			server_set_pid( serp, pid ) ;
			(void) close( server_socket ) ;
	}
}



/*
 * Return value:
 *			OK 			if the server died
 *			FAILED 		otherwise
 */
PRIVATE status_e wait_child( ip )
	struct intercept *ip ;
{
	char *func = "wait_child" ;

	for ( ;; )
	{
		int status ;
		pid_t pid = wait( &status ) ;

		if ( pid == -1 )
		{
			if ( errno != EINTR )
			{
				msg( LOG_ERR, func, "wait: %m" ) ;
				return( FAILED ) ;
			}
		}
		else if ( pid == SERVER_PID( INT_SERVER( ip ) ) )
		{
			if ( PROC_STOPPED( status ) )
				return( FAILED ) ;
			server_set_exit_status( INT_SERVER( ip ), status ) ;
			return( OK ) ;
		}
		else
		{
			msg( LOG_ERR, func,
				"wait returned pid of unknown process: %d", pid ) ;
			return( FAILED ) ;
		}
	}
}



PRIVATE void terminate_server( ip )
	struct intercept *ip ;
{
	pid_t pid = SERVER_PID( INT_SERVER( intp ) ) ;

	if ( pid > 0 )
		(void) kill( pid, SIGKILL ) ;

	/*
	 * Normally, wait_child should never return since a SIGCHLD will 
	 * invoke the signal handler which will then call the exit function.
	 */
	if ( wait_child( ip ) == OK )
		(*intp->int_ops->exit)() ;
}


void int_sighandler( sig )
	int sig ;
{
	char *func = "int_sighandler" ;
	char *sig_name() ;

	if ( debug.on )
		msg( LOG_DEBUG, func, "Received signal %s", sig_name( sig ) ) ;

	if ( sig == SERVER_EXIT_SIG )
	{
		if ( wait_child( intp ) == OK )
			(*intp->int_ops->exit)() ;
	}
	else if ( sig == INTERCEPT_SIG )
		INTERCEPT( intp ) = FALSE ;
	else if ( sig == SIGTERM )
		terminate_server( intp ) ;
}
