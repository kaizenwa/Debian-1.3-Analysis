/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: service.c,v 1.5 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#ifdef linux
#include <sys/time.h>
#endif
#if defined(BSD) || defined linux
#include <netinet/in.h>
#endif
#include <netdb.h>
#include <syslog.h>
#include <fcntl.h>

#include "sio.h"

#include "options.h"

#include "access.h"
#include "attr.h"
#include "service.h"
#include "server.h"
#include "state.h"
#include "config.h"
#include "connection.h"

#define NEW_SVC()					NEW( struct service )
#define FREE_SVC( sp )			FREE( sp )

#define SUSPEND( sp )			(sp)->svc_state = SVC_SUSPENDED
#define RESUME( sp )				(sp)->svc_state = SVC_ACTIVE
#define DISABLE( sp )			(sp)->svc_state = SVC_DISABLED

void msg() ;
void out_of_memory() ;

time_t time() ;
char *malloc() ;

static struct name_value service_states[] =
   {
      { "Not started",        (int) SVC_NOT_STARTED   },
      { "Active",             (int) SVC_ACTIVE        },
      { "Disabled",           (int) SVC_DISABLED      },
		{ "Suspended",				(int) SVC_SUSPENDED		},
      { NULL,                 1                       },
		{ "BAD STATE",				0								}
   } ;



/*
 * Allocate a new struct service and initialize it from scp 
 */
struct service *svc_new( scp )
	struct service_config *scp ;
{
	struct service *sp ;
	char *func = "svc_new" ;

	sp = NEW_SVC() ;
   if ( sp == NULL )
   {
      out_of_memory( func ) ;
      return( NULL ) ;
   }
	CLEAR( *sp ) ;

	sp->svc_conf = scp ;
	return( sp ) ;
}


struct service *svc_make_special( scp, handler )
	struct service_config	*scp ;
	statfunc						handler ;
{
	struct service		*sp ;
	char					*func = "svc_make_special" ;

	if ( ( sp = svc_new( scp ) ) == NULL )
	{
		out_of_memory( func ) ;
		return( NULL ) ;
	}

	sp->svc_handler_func = handler ;
	sp->svc_log = ps.rws.program_log ;

	sp->svc_ref_count = 1 ;
	sp->svc_state = SVC_ACTIVE ;
	return( sp ) ;
}


void svc_free( sp )
   struct service *sp ;
{
   sc_free( sp->svc_conf ) ;
	FREE_SVC( sp ) ;
}

/*
 * Currently, address control works by looking at the fields 
 * only_from/no_access of struct service.
 * In order to enable address control, we copy to these fields either
 * the explicitly specified values (which come from the service_config)
 * or the default ones (if there are any).
 */
void svc_setup_address_control( sp )
	register struct service *sp ;
{
	register struct service_config *scp = SVC_CONF( sp ) ;

	if ( SC_IS_PRESENT( scp, A_ONLY_FROM ) )
		sp->svc_only_from = SC_ONLY_FROM( scp ) ;
	else if ( SC_SPECIFIED( DEFAULTS( ps ), A_ONLY_FROM ) )
		sp->svc_only_from = SC_ONLY_FROM( DEFAULTS( ps ) ) ;
	else
		sp->svc_only_from = NULL ;

	if ( SC_IS_PRESENT( scp, A_NO_ACCESS ) )
		sp->svc_no_access = SC_NO_ACCESS( scp ) ;
	else if ( SC_SPECIFIED( DEFAULTS( ps ), A_NO_ACCESS ) )
		sp->svc_no_access = SC_NO_ACCESS( DEFAULTS( ps ) ) ;
	else
		sp->svc_no_access = NULL ;
}


PRIVATE status_e set_fd_modes( sp )
	register struct service *sp ;
{
	int sd = SVC_FD( sp ) ;
	char *func = "set_fd_modes" ;

   /*
    * There is a possibility of blocking on a send/write if
    *
	 *		the service does not require forking (==> is internal) AND
    *    it does not accept connections
    *
    * To avoid this, we put the descriptor in FNDELAY mode.
    * (if the service accepts connections, we still need to put the
    * 'accepted' connection in FNDELAY mode but this is done elsewhere)
    */
	if ( ! SVC_FORKS( sp ) && ! SVC_ACCEPTS_CONNECTIONS( sp ) &&
                              fcntl( sd, F_SETFL, FNDELAY ) == -1 )
   {
      msg( LOG_ERR, func,
         "fcntl failed (%m) for FNDELAY. service = %s", SVC_ID( sp ) ) ;
		return( FAILED ) ;
   }

   /*
    * Always set the close-on-exec flag
    */
   if ( fcntl( sd, F_SETFD, 1 ) == -1 )
   {
      msg( LOG_ERR, func,
         "fcntl failed (%m) for close-on-exec. service = %s", SVC_ID( sp ) ) ;
      return( FAILED ) ;
   }
	return( OK ) ;
}


#ifndef NO_RPC

PRIVATE status_e activate_rpc( sp )
   register struct service *sp ;
{
	struct sockaddr_in		sin ;
	int						sin_len ;
	unsigned long			vers ;
	struct service_config	*scp						= SVC_CONF( sp ) ;
	struct rpc_data			*rdp						= SC_RPCDATA( scp ) ;
	char					*sid						= SC_ID( scp ) ;
	unsigned				registered_versions			= 0 ;
	int						sd							= SVC_FD( sp ) ;
	char					*func						= "activate_rpc" ;

	CLEAR( sin ) ;

    /*
     * N Verdon (5/6/95) changed the sin.sin_addr.s_addr to
     * use the variable scp->sc_interface.s_addr to allow
     * binding to a specific IP address.
     */
	sin.sin_family      = AF_INET ;

#ifdef BIND_IF
	sin.sin_addr.s_addr = scp->sc_interface.s_addr;
#else
	sin.sin_addr.s_addr = htonl( INADDR_ANY ) ;
#endif

	sin.sin_port        = 0 ; /* let the system give us a port */

	if ( bind( sd, SA( &sin ), sizeof( sin ) ) == -1 )
	{
		msg( LOG_ERR, func, "bind failed (%m). service = %s", sid ) ;
		return( FAILED ) ;
	}

	/*
	 * Find the port number that was assigned to the socket
	 */
	sin_len = sizeof( sin ) ;
	if ( getsockname( sd, SA( &sin ), &sin_len ) == -1 )
	{
		msg( LOG_ERR, func,
				"getsockname failed (%m). service = %s", sid ) ;
		return( FAILED ) ;
	}
	sc_set_port( scp, ntohs( sin.sin_port ) ) ;

	/*
	 * Try to register as many versions as possible
	 */
	for ( vers = RD_MINVERS( rdp ) ; vers <= RD_MAXVERS( rdp ) ; vers++ )
		if ( pmap_set( RD_PROGNUM( rdp ), vers,
										SC_PROTOVAL( scp ), SC_PORT( scp ) ) )
			registered_versions++ ;
		else
			msg( LOG_ERR, func,
				"pmap_set failed. service=%s program=%ld version=%ld",
					sid, RD_PROGNUM( rdp ), vers ) ;

	if ( debug.on )
		msg( LOG_DEBUG, func,
				"Registered %d versions of %s", registered_versions, sid ) ;

	return( ( registered_versions == 0 ) ? FAILED : OK ) ;
}

#endif	/* ! NO_RPC */


PRIVATE status_e activate_normal( sp )
	register struct service *sp ;
{
	struct sockaddr_in		sin ;
	int						sd				= SVC_FD( sp ) ;
	struct service_config	*scp			= SVC_CONF( sp ) ;
	unsigned short			service_port	= SC_PORT( scp ) ;
	char					*sid			= SC_ID( scp ) ;
	char					*func			= "activate_normal" ;

	CLEAR( sin ) ;

	/*
	 * N Verdon (5/6/95) changed the sin.sin_addr.s_addr to
     * use the variable scp->sc_interface.s_addr to allow
     * binding to a specific IP address.
	 */
	sin.sin_family      = AF_INET ;
#ifdef BIND_IF
	sin.sin_addr.s_addr = scp->sc_interface.s_addr;
#else
	sin.sin_addr.s_addr = htonl( INADDR_ANY ) ;
#endif
	sin.sin_port        = htons( service_port ) ;

	if ( reuse_option || SC_REUSE_ADDRESS( scp ) )
	{
		int on = 1 ;

		if ( setsockopt( sd, SOL_SOCKET, SO_REUSEADDR, 
                         (char *) &on, sizeof( on ) ) == -1 
           )
		{
			msg( LOG_WARNING, func, 
                 "setsockopt SO_REUSEADDR failed (%m). service = %s", sid ) ;
		}
	}

	if ( bind( sd, SA( &sin ), sizeof( sin ) ) == -1 )
	{
		msg( LOG_ERR, func, "bind failed (%m). service = %s", sid ) ;
		return( FAILED ) ;
	}

	return( OK ) ;
}


/*
 * Activate a service. 
 */
status_e svc_activate( sp )
   register struct service *sp ;
{
	struct service_config	*scp = SVC_CONF( sp ) ;
	status_e						status ;
   char							*func = "svc_activate" ;
	voidfunc						get_shutdown_by_name() ;
	PRIVATE status_e			svc_generic_handler() ;
	PRIVATE void				deactivate() ;

	sp->svc_fd = socket( AF_INET, 
								SC_SOCKET_TYPE( scp ), SC_PROTOVAL( scp ) ) ;

   if ( sp->svc_fd == -1 )
   {
      msg( LOG_ERR, func,
						"socket creation failed (%m). service = %s", SC_ID( scp ) ) ;
		return( FAILED ) ;
   }

	if ( set_fd_modes( sp ) == FAILED )
	{
		(void) close( sp->svc_fd ) ;
		return( FAILED ) ;
	}

#ifndef NO_RPC
   if ( SC_IS_RPC( scp ) )
		status = activate_rpc( sp ) ;
   else
#endif   /* ! NO_RPC */
		status = activate_normal( sp ) ;
	
	if ( status == FAILED )
	{
		(void) close( sp->svc_fd ) ;
		return( FAILED ) ;
	}

	if ( log_start( sp, &sp->svc_log ) == FAILED )
	{
		deactivate( sp ) ;
		return( FAILED ) ;
	}

	/*
	 * Initialize the service data
	 */
	sp->svc_running_servers	= sp->svc_retry_servers = 0 ;
	sp->svc_shutdown_func	= get_shutdown_by_name( SC_NAME( scp ) ) ;
	sp->svc_handler_func		= svc_generic_handler ;
	svc_setup_address_control( sp ) ;

   if ( SC_MUST_LISTEN( scp ) )
      (void) listen( sp->svc_fd, LISTEN_BACKLOG ) ;

   ps.rws.descriptors_free-- ;

	sp->svc_state = SVC_ACTIVE ;

	FD_SET( sp->svc_fd, &ps.rws.socket_mask ) ;
	if ( sp->svc_fd > ps.rws.mask_max )
		ps.rws.mask_max = sp->svc_fd ;

	ps.rws.active_services++ ;
	ps.rws.available_services++ ;

   return( OK ) ;
}


PRIVATE void deactivate( sp )
	register struct service *sp ;
{
	(void) close( SVC_FD( sp ) ) ;

#ifndef NO_RPC
	if ( SC_IS_RPC( SVC_CONF( sp ) ) )
	{
		register unsigned long vers ;
		register struct rpc_data *rdp = SC_RPCDATA( SVC_CONF( sp ) ) ;

		for ( vers = RD_MINVERS( rdp ) ; vers <= RD_MAXVERS( rdp ) ; vers++ )
			(void) pmap_unset( RD_PROGNUM( rdp ), vers ) ;
	}
#endif	/* ! NO_RPC */
}


/*
 * Close the service descriptor.
 * If this is an RPC service, deregister it.
 * Close the log.
 */
void svc_deactivate( sp )
   register struct service *sp ;
{
	if ( ! SVC_IS_AVAILABLE( sp ) )
		return ;

	deactivate( sp ) ;
   ps.rws.descriptors_free++ ;

	if ( SVC_IS_ACTIVE( sp ) )
	{
		FD_CLR( SVC_FD( sp ), &ps.rws.socket_mask ) ;
		ps.rws.active_services-- ;
	}

	ps.rws.available_services-- ;

	DISABLE( sp ) ;
}



/*
 * Suspend a service
 */
void svc_suspend( sp )
	register struct service *sp ;
{
	char *func = "svc_suspend" ;

	if ( ! SVC_IS_ACTIVE( sp ) )
	{
		msg( LOG_ERR, func, "service %s is not active", SVC_ID( sp ) ) ;
		return ;
	}

	FD_CLR( SVC_FD( sp ), &ps.rws.socket_mask ) ;
	ps.rws.active_services-- ;
	if ( debug.on )
		msg( LOG_DEBUG, func, "Suspended service %s", SVC_ID( sp ) ) ;
	
	SUSPEND( sp ) ;
}


/*
 * Resume a suspended service.
 */
void svc_resume( sp )
	register struct service *sp ;
{
	char *func = "svc_resume" ;

	FD_SET( SVC_FD( sp ), &ps.rws.socket_mask ) ;
	ps.rws.active_services++ ;
	if ( debug.on )
		msg( LOG_DEBUG, func, "Resumed service %s", SVC_ID( sp ) ) ;
	RESUME( sp ) ;
}


/*
 * Steps:
 *		1. Deactivate the service
 *		2. Free all memory used by the service and free the service itself
 *
 * Since this function may free all memory associated with the service as
 * well as the memory pointed by sp, only the value of sp should be used
 * after this call if the return value is 0 (i.e. no dereferencing of sp).
 *
 * Special services are never deactivated.
 */
int svc_release( sp )
   register struct service *sp ;
{
	char *sid = SVC_ID( sp ) ;
	char *func = "svc_release" ;

	if ( sp->svc_ref_count == 0 )
	{
		msg( LOG_ERR, func, "%s: svc_release with 0 count", sid ) ;
		return( 0 ) ;
	}
	
	sp->svc_ref_count-- ;
	if ( sp->svc_ref_count == 0 )
	{
		if ( debug.on )
			msg( LOG_DEBUG, func, "ref count of service %s dropped to 0", sid ) ;
		if ( ! SC_IS_SPECIAL( SVC_CONF( sp ) ) )
		{
			if ( sp->svc_log )
				log_end( SC_LOG( SVC_CONF( sp ) ), sp->svc_log ) ;
			svc_deactivate( sp ) ;
			svc_free( sp ) ;
		}
		else		/* this shouldn't happen */
			msg( LOG_WARNING, func,
				"ref count of special service %s dropped to 0", sid ) ;
		return( 0 ) ;
	}
	else
		return( sp->svc_ref_count ) ;
}



void svc_dump( sp, fd )
	register struct service *sp ;
	int fd ;
{
	char *get_shutdown_by_addr() ;
	void tabprint() ;

	tabprint( fd, 0, "Service = %s\n", SC_NAME( SVC_CONF( sp ) ) ) ;
	tabprint( fd, 1, "State = %s\n",
								nv_get_name( service_states, (int) sp->svc_state ) ) ;

	sc_dump( SVC_CONF( sp ), fd, 1, FALSE ) ;

	if ( sp->svc_state == SVC_ACTIVE )
	{
		tabprint( fd, 1, "running servers = %d\n", sp->svc_running_servers ) ;
		tabprint( fd, 1, "retry servers = %d\n", sp->svc_retry_servers ) ;
		tabprint( fd, 1, "attempts = %d\n", sp->svc_attempts ) ;
		tabprint( fd, 1, "service fd = %d\n", sp->svc_fd ) ;
		tabprint( fd, 1, "shutdown function = %s\n",
							get_shutdown_by_addr( sp->svc_shutdown_func ) ) ;
	}
	Sputchar( fd, '\n' ) ;
}


/*
 * Returns TRUE if the server instantiation rate is over the limit
 */
bool_int svc_looping( sp )
	register struct service *sp ;
{
	time_t				current_time ;
	register time_t	time_diff ;
	char					*func = "svc_looping" ;

	(void) time( &current_time ) ;

	if ( sp->svc_attempts == 0 )
	{
		sp->svc_attempts++ ;
		sp->svc_start_time = current_time ;
		return( FALSE ) ;
	}

	time_diff = current_time - sp->svc_start_time ;
	if ( time_diff <= LOOP_INTERVAL )
	{
		sp->svc_attempts++ ;
		if ( time_diff == 0 )
			time_diff = 1 ;
		if ( sp->svc_attempts/time_diff > ps.ros.loop_rate )
		{
			FD_CLR( SVC_FD( sp ), &ps.rws.socket_mask ) ;
			svc_deactivate( sp ) ;
			msg( LOG_ERR, func,
				"%s service was deactivated because of looping", SVC_ID( sp ) ) ;
			return( TRUE ) ;
		}
	}
	else
	{
		sp->svc_start_time = current_time ;
		sp->svc_attempts = 1 ;
	}
	return( FALSE ) ;
}



void svc_request( sp )
	register struct service *sp ;
{
	register connection_s *cp ;

	cp = conn_new( sp ) ;
	if ( cp == CONN_NULL )
		return ;

	if ( svc_handle( sp, cp ) == OK )
		return ;
	else
	{
		conn_cleanup( cp ) ;
		if ( conn_start_alternative( cp ) == FAILED )
			conn_free( cp ) ;
	}
}



PRIVATE status_e svc_generic_handler( sp, cp )
	register struct service		*sp ;
	register connection_s		*cp ;
{
	register struct service_config *scp = SVC_CONF( sp ) ;

   /*
    * There is no loop check for services that accept connections because
    * the connections are accepted by xinetd, therefore the queue will get
    * empty eventually even if the server is faulty.
    * Services for which no fork(2) occurs are not checked because
    *    a) they are internal and we assume they are not faulty
    *    b) we have no way of determining if something is going wrong
    *       for example, UDP discard can do its job very quickly
    */
	if ( !SC_ACCEPTS_CONNECTIONS( scp ) && SVC_FORKS( sp ) && svc_looping( sp ) )
		return( FAILED ) ;

	if ( svc_access_control( sp, cp ) == OK )
		return( server_run( sp, cp ) ) ;

	/*
	 * Add alternative services to handle the connection attempt.
	 * The services must be added in the proper order because they
	 * are invoked in that order:
	 *			logging
	 *			shutdown
	 */

	if ( SVC_LOGS_USERID_ON_FAILURE( sp ) )
		(void) conn_add_alternative( cp, LOG_SERVICE( ps ) ) ;

	if ( sp->svc_shutdown_func != NULL )
	{
		if ( ! SVC_RECORDS( sp ) || 
					conn_add_alternative( cp, SHUTDOWN_SERVICE( ps ) ) == FAILED )
			conn_shutdown( cp ) ;
	}

	return( FAILED ) ;
}


status_e svc_access_control( sp, cp )
	register struct service		*sp ;
	connection_s					*cp ;
{
	access_e result ;

	result = access_control( sp, cp, MASK_NULL ) ;
	if ( result != AC_OK )
	{
		bool_int report_failure = TRUE ;

		/*
		 * Try to avoid reporting multiple times a failed attempt to access
		 * a datagram-based service from a bad address. We do this because
		 * the clients of such services usually send multiple datagrams 
		 * before reporting a timeout (we have no way of telling them that
		 * their request has been denied).
		 */
		if ( result == AC_ADDRESS && SVC_SOCKET_TYPE( sp ) == SOCK_DGRAM )
		{
			register struct sockaddr_in *sinp = conn_address( cp ) ;
			register struct sockaddr_in *last = &sp->svc_last_dgram_addr ;
			time_t current_time ;

			(void) time( &current_time ) ;

			if ( sinp->sin_addr.s_addr == last->sin_addr.s_addr &&
														sinp->sin_port == last->sin_port )
			{
				if ( current_time - sp->svc_last_dgram_time <= DGRAM_IGNORE_TIME )
					report_failure = FALSE ;
				else
					sp->svc_last_dgram_time = current_time ;
			}
			else
			{
				sp->svc_last_dgram_addr = *sinp ;
				sp->svc_last_dgram_time = current_time ;
			}
		}

		if ( report_failure )
			svc_log_failure( sp, cp, result ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}


/*
 * Implement shutdown protocol (usually sends an error indication).
 */
void svc_shutdown( sp, cp, msgp )
	register struct service		*sp ;
	register connection_s		*cp ;
	char								**msgp ;
{
	int						old_flags ;
	register bool_int		no_block ;
	int						fd		= CONN_DESCRIPTOR( cp ) ;
	char						*func = "svc_shutdown" ;

#ifdef DEBUG
	if ( sp != CONN_SERVICE( cp ) )
	{
		msg( LOG_CRIT, func, "service = %s, service of connection = %s",
			SVC_ID( sp ), SVC_ID( CONN_SERVICE( cp ) ) ) ;
		return ;
	}
#endif

	if ( sp->svc_shutdown_func == NULL )
		return ;

	no_block = ( msgp == NULL ) ;

	/*
	 * Ensure that we won't block in the shutdown function
	 */
	if ( no_block )
	{
		if ( ( old_flags = fcntl( fd, F_GETFL, 0 ) ) == -1 )
		{
			msg( LOG_ERR, func, "fcntl-getflags failed: %m" ) ;
			return ;
		}

		if ( fcntl( fd, F_SETFL, FNDELAY ) == -1 )
		{
			msg( LOG_ERR, func, "fcntl-setflags failed: %m" ) ;
			return ;
		}
	}

	(*sp->svc_shutdown_func)( fd, msgp ) ;

	/*
	 * Don't bother with restoring the flags if this is a new descriptor
	 */
	if ( no_block && SC_ACCEPTS_CONNECTIONS( SVC_CONF( sp ) ) )
		(void) fcntl( fd, F_SETFL, old_flags ) ;
}


/*
 * Invoked when a server of the specified service dies
 */
void svc_postmortem( sp, serp )
	register struct service		*sp ;
	register struct server		*serp ;
{
	struct service				*co_sp	= SERVER_CONNSERVICE( serp ) ;
	register connection_s	*cp		= SERVER_CONNECTION( serp ) ;
	char							*func		= "svc_postmortem" ;

	svc_dec_running_servers( sp ) ;

	/*
	 * Log information about the server that died
	 */
	if ( SVC_IS_LOGGING( sp ) )
	{
		if ( serp->svr_writes_to_log )
		{
			if ( debug.on )
				msg( LOG_DEBUG, func,
								"Checking log size of %s service", SVC_ID( sp ) ) ;
			xlog_control( SVC_LOG( sp ), XLOG_SIZECHECK ) ;
		}
		svc_log_exit( sp, serp ) ;
	}

	/*
	 * Check for alternative services to handle this connection
	 */
	if ( conn_start_alternative( cp ) == OK )
		return ;
	
	/*
	 * Now check if we have to check the log size of the service that owns
	 * the connection
	 */
	if ( co_sp != sp && SVC_IS_LOGGING( co_sp ) )
		xlog_control( SVC_LOG( co_sp ), XLOG_SIZECHECK ) ;

	conn_free( cp ) ;
}

