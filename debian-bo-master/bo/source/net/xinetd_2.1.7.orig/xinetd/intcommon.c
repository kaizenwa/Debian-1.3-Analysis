/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: intcommon.c,v 1.3 1995/09/10 18:41:13 chuck Exp $" ;

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#ifdef linux
#include <netinet/in.h>
#endif
#include <signal.h>
#include <syslog.h>
#include <errno.h>

void exit() ;

#include "int.h"
#include "defs.h"
#include "service.h"
#include "server.h"
#include "config.h"
#include "state.h"

void msg() ;

void int_fail( ip, syscall )
	struct intercept *ip ;
	char *syscall ;
{
	msg( LOG_ERR, "fail", "%s failed: %m", syscall ) ;
	(*ip->int_ops->exit)() ;
	/* NOTREACHED */
}


/*
 * Returns either a positive number or -1
 */
int int_select( max, read_mask )
	int max ;
	fd_set *read_mask ;
{
	char *func = "int_select" ;

	for ( ;; )
	{
		int n_ready ;

		n_ready = select( max+1, read_mask,
											FD_SET_NULL, FD_SET_NULL, TIMEVAL_NULL ) ;
		if ( n_ready > 0 )
			return( n_ready ) ;
		else if ( n_ready == -1 )
			if ( errno == EINTR )
				continue ;
			else
			{
				msg( LOG_ERR, func, "select: %m" ) ;
				return( -1 ) ;
			}
	}
}


void int_exit( ip )
	struct intercept *ip ;
{
	int status = SERVER_EXITSTATUS( INT_SERVER( ip ) ) ;
	char *func = "int_exit" ;
	char *sig_name() ;

	if ( debug.on )
	{
		if ( PROC_EXITED( status ) )
			msg( LOG_DEBUG, func, "intercepted server died" ) ;
		else if ( PROC_SIGNALED( status ) )
			msg( LOG_DEBUG, func, "intercepted server received signal %s",
					sig_name( (int) PROC_TERMSIG( status ) ) ) ;
	}
	_exit( (int) PROC_EXITSTATUS( status ) ) ;
}

#ifdef BSD
/*
 * handle child signals, straight from Stevens
 */
void child_sighandler()
{
	int		pid;
	union wait	status;

	while((pid = wait3((int *)&status, WNOHANG, (struct rusage *) 0)) > 0)
		;
}
#endif	/* BSD */

/*
 * The ops vector must be installed before invoking this function
 */
void int_init( ip, serp )
	struct intercept *ip ;
	struct server *serp ;
{
	register unsigned u ;
	char *func = "int_init" ;
	void int_sighandler() ;

	/*
	 * Sanity test
	 */
	if ( SERVER_SERVICE( serp ) != SERVER_CONNSERVICE( serp ) )
	{
		msg( LOG_ERR, "server service (%s) != connection service (%s)",
									SVC_ID( SERVER_SERVICE( serp ) ),
										SVC_ID( SERVER_CONNSERVICE( serp ) ) ) ;
		exit( 1 ) ;
	}

   /*
    * Close all unneeded descriptors
    */
   for ( u = 0 ; u < pset_count( SERVICES( ps ) ) ; u++ )
   {
      struct service *sp = SP( pset_pointer( SERVICES( ps ), u ) ) ;

      if ( sp == SERVER_SERVICE( serp ) )
         continue ;
      if ( log_get_type( SC_LOG( SVC_CONF( sp ) ) ) == L_FILE )
         xlog_destroy( sp->svc_log ) ;
      (void) close( SVC_FD( sp ) ) ;
   }

	/*
	 * Setup signal handling
	 */
#ifdef BSD
	if ( (int) signal( SERVER_EXIT_SIG, child_sighandler ) == -1 )
#else
	if ( (int) signal( SERVER_EXIT_SIG, int_sighandler ) == -1 )
#endif	/* BSD */
		int_fail( ip, "signal" ) ;
	if ( (int) signal( INTERCEPT_SIG, int_sighandler ) == -1 )
		int_fail( ip, "signal" ) ;
	if ( (int) signal( SIGTERM, int_sighandler ) == -1 )
		int_fail( ip, "signal" ) ;
	
	/*
	 * Initialize state
	 */
	INTERCEPT( ip ) = TRUE ;
	*INT_SERVER( ip ) = *serp ;
	INT_REMOTE( ip ) = SERVER_FD( serp ) ;

	INT_CONNECTIONS( ip ) = pset_create( 0, 0 ) ;
	if ( INT_CONNECTIONS( ip ) == NULL )
	{
		msg( LOG_ERR, func, ES_NOMEM ) ;
		(*ip->int_ops->exit)() ;
	}
}



/*
 * Make a new connection to the local server
 */
channel_s *int_newconn( ip, sinp, remote_socket )
	struct intercept		*ip ;
	struct sockaddr_in	*sinp ;
	int						remote_socket ;
{
	struct service			*sp			= SERVER_SERVICE( INT_SERVER( ip ) ) ;
	int						socket_type = SVC_SOCKET_TYPE( sp ) ;
	struct sockaddr_in	*local		= INT_LOCALADDR( ip ) ;
	char 						*sid			= SVC_ID( sp ) ;
   channel_s				*chp ;
   int						sd ;
   char						*func = "int_newconn" ;

   /*
    * Get a socket and connect it to the local address
	 *
	 * XXX:	should we use SC_PROTOVAL to explicitly specify the protocol ?
    */
   if ( ( sd = socket( AF_INET, socket_type, 0 ) ) == -1 )
   {
      msg( LOG_ERR, func,"(intercepting %s) socket creation failed: %m", sid ) ;
      return( CHANNEL_NULL ) ;
   }

   if ( connect( sd, SA( local ), sizeof( *local ) ) == -1 )
   {
      msg( LOG_ERR, func, "(intercepting %s) connect failed: %m", sid ) ;
      (void) close( sd ) ;
      return( CHANNEL_NULL ) ;
   }

	chp = NEW_CHANNEL() ;
   if ( chp == CHANNEL_NULL )
   {
      msg( LOG_ERR, func, ES_NOMEM ) ;
      (void) close( sd ) ;
      return( CHANNEL_NULL ) ;
   }

   if ( pset_add( INT_CONNECTIONS( ip ), chp ) == NULL )
   {
      msg( LOG_ERR, func, ES_NOMEM ) ;
		FREE_CHANNEL( chp ) ;
      (void) close( sd ) ;
      return( CHANNEL_NULL ) ;
   }

	chp->ch_state = GOOD_CHANNEL ;
	chp->ch_from = *sinp ;
   chp->ch_local_socket = sd ;
	chp->ch_remote_socket = remote_socket ;
   return( chp ) ;
}



/*
 * Check if the (address,port) in sinp is already in the connection table.
 * Return value:
 *    a connection pointer if the address is found
 *    NULL if the address if not found
 *
 * *addr_checked is set to TRUE of FALSE depending on whether there
 * is already a connection from the same IP address in the table.
 */
channel_s *int_lookupconn( ip, sinp, addr_checked )
	struct intercept		*ip ;
   struct sockaddr_in	*sinp ;
   bool_int					*addr_checked ;
{
   register unsigned		u ;
	register pset_h		conntab = INT_CONNECTIONS( ip ) ;

   *addr_checked = FALSE ;

   for ( u = 0 ; u < pset_count( conntab ) ; u++ )
   {
      register channel_s *chp = CHP( pset_pointer( conntab, u ) ) ;

      if ( chp->ch_from.sin_addr.s_addr == sinp->sin_addr.s_addr )
      {
         *addr_checked = TRUE ;
         if ( chp->ch_from.sin_port == sinp->sin_port )
            return( chp ) ;
      }
   }
   return( CHANNEL_NULL ) ;
}

