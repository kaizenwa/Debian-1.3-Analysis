/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: special.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <syslog.h>
#include <signal.h>

#include "misc.h"

#include "options.h"

#include "state.h"
#include "conf.h"
#include "sconst.h"
#include "server.h"
#include "config.h"
#include "builtin.h"
#include "connection.h"

void msg() ;
void out_of_memory() ;

extern struct name_value socket_types[] ;

PRIVATE void stream_logging() ;
PRIVATE void stream_shutdown() ;
void intercept() ;

static struct builtin_service special_services[] =
	{
		{ LOG_SERVICE_NAME, 			SOCK_STREAM,	{ stream_logging,		FORK	} },
		{ SHUTDOWN_SERVICE_NAME,	SOCK_STREAM,   { stream_shutdown,  	FORK  } },
		{ INTERCEPT_SERVICE_NAME,	SOCK_STREAM,	{ intercept,			FORK	} },
		{ INTERCEPT_SERVICE_NAME,	SOCK_DGRAM,		{ intercept,			FORK	} },
		{ NULL }
	} ;


builtin_s *spec_find( service_name, type )
   char *service_name ;
   int type ;
{
	builtin_s *bp ;
	struct name_value *nvp ;
	char *func = "spec_find" ;

	if ( bp = builtin_lookup( special_services, service_name, type ) )
		return( bp ) ;

	nvp = nv_find_name( socket_types, type ) ;
	if ( nvp == NULL )
	{
		msg( LOG_ERR, func, "unknown socket type: %d", type ) ;
		return( NULL ) ;
	}

	msg( LOG_ERR, func,
				"special service %s,%s not supported", service_name, nvp->name ) ;
   return( NULL ) ;
}


PRIVATE status_e spec_service_handler( sp, cp )
	struct service *sp ;
	connection_s *cp ;
{
	if ( svc_access_control( sp, cp ) == FAILED ||
											server_run( sp, cp ) == FAILED )
	{
		/*
		 * If we couldn't start a server for the shutdown service, mark
		 * the connection for shutdown, so that we will do a simple shutdown
		 * (i.e. we won't try to collect information from the remote end).
		 */
		if ( sp == SHUTDOWN_SERVICE( ps ) )
			conn_shutdown( cp ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}



PRIVATE struct service *spec_setup( name, socket_type, instances )
	char *name ;
	int socket_type ;
	int instances ;
{
	builtin_s *bp ;
	struct service_config *scp ;

	bp = spec_find( name, socket_type ) ;
	if ( bp == NULL )
		return( NULL ) ;

	if ( ( scp = sc_make_special( name, bp, instances ) ) == NULL )
		return( NULL ) ;

	return( svc_make_special( scp, spec_service_handler ) ) ;
}


/*
 * Initialize the special services and the corresponding entries in
 * the program state structure.
 */
void spec_include()
{
	int instances ;

	instances = logprocs_option ? logprocs_option_arg : DEFAULT_LOGPROCS ;
	LOG_SERVICE( ps ) = spec_setup( LOG_SERVICE_NAME, SOCK_STREAM, instances ) ;

	instances = shutdownprocs_option ? shutdownprocs_option_arg
												: DEFAULT_SHUTDOWNPROCS ;
	SHUTDOWN_SERVICE( ps ) = spec_setup( SHUTDOWN_SERVICE_NAME,
																SOCK_STREAM, instances ) ;
}


PRIVATE void stream_logging( serp )
	struct server *serp ;
{
	char *func = "stream_logging" ;
	idresult_e result ;
	idresult_e log_remote_user() ;
	char *idresult_explain() ;

#ifdef DEBUG_LOGGING
	if ( debug.on )
	{
		msg( LOG_DEBUG, func, "%d is sleeping", getpid() ) ;
		sleep( 10 ) ;
	}
#endif

	result = log_remote_user( serp, LOGUSER_FAILURE_TIMEOUT ) ;
	if ( result != IDR_OK && result != IDR_NOSERVER )
		msg( LOG_ERR, func, "Failed to contact identity server at %s: %s",
			conn_addrstr( SERVER_CONNECTION( serp ) ),
				idresult_explain( result ) ) ;
}



PRIVATE void stream_shutdown( serp )
	struct server *serp ;
{
	struct service *sp = SERVER_CONNSERVICE( serp ) ;
   char *str ;
	char *func = "stream_shutdown" ;

	if ( debug.on )
		msg( LOG_DEBUG, func, "shutdown for service %s, (fd=%d)",
			SVC_ID( sp ), SERVER_FD( serp ) ) ;

#ifdef DEBUG_SHUTDOWN
   /*
    * The reason for the sleep is that single stepping through this
    * code is impossible (at least on the Sun IPC I am using).
    */
   if ( debug.on )
   {
      msg( LOG_DEBUG, func, "%d is sleeping", getpid() ) ;
      sleep( 10 ) ;
   }
#endif

   /*
    * Everything must happen within 10 seconds.
    * Since there is no alarm handler the process will be terminated if the
    * timer runs out.
    */
   if ( ! debug.on )
   {
      (void) alarm( 10 ) ;
      (void) signal( SIGALRM, SIG_DFL ) ;
   }

	str = NULL ;				/* must be initialized to NULL */
	svc_shutdown( sp, SERVER_CONNECTION( serp ), &str ) ;

   (void) alarm( 0 ) ;

   if ( str != NULL )
      svc_logprint( sp, DATA_ENTRY, "%s", str ) ;
}


