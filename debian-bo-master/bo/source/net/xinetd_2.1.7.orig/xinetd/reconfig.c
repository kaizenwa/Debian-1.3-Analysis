/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: reconfig.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <syslog.h>
#include <signal.h>
#include <memory.h>

#include "state.h"
#include "access.h"
#include "defs.h"
#include "service.h"
#include "server.h"
#include "conf.h"
#include "config.h"

void exit() ;

void msg() ;
void out_of_memory() ;

typedef enum { RECONFIG_SOFT, RECONFIG_HARD } reconfig_e ;


#define SWAP( x, y, temp )			(temp) = (x), (x) = (y), (y) = (temp)


PRIVATE void do_reconfig() ;


void soft_reconfig()
{
	do_reconfig( RECONFIG_SOFT ) ;
}


void hard_reconfig()
{
	do_reconfig( RECONFIG_HARD ) ;
}


/*
 * Reconfigure the server by rereading the configuration file.
 * Services may be added, deleted or have their attributes changed.
 * All syslog output uses the LOG_NOTICE priority level (except for
 * errors).
 */
PRIVATE void do_reconfig( reconfig_type )
	reconfig_e reconfig_type ;
{
	struct service				*osp ;
	struct service_config	*nscp ;
	struct configuration		new_conf ;
	psi_h							iter ;
	unsigned						new_services ;
	unsigned						old_services		= 0 ;
	unsigned						dropped_services	= 0 ;
	char							*func					= "do_reconfig" ;
	PRIVATE status_e			readjust() ;
	PRIVATE void				check_servers() ;
	PRIVATE void				swap_defaults() ;
	void							terminate_servers() ;
	void							cancel_service_retries() ;

	msg( LOG_NOTICE, func, "Starting %s reconfiguration",
				( reconfig_type == RECONFIG_SOFT ) ? "soft" : "hard" ) ;

	if ( cnf_get( &new_conf, (long)CONF_TIMEOUT ) == FAILED )
	{
		msg( LOG_WARNING, func, "reconfiguration failed" ) ;
		return ;
	}

	iter = psi_create( SERVICES( ps ) ) ;
	if ( iter == NULL )
	{
		out_of_memory( func ) ;
		cnf_free( &new_conf ) ;
		return ;
	}

	swap_defaults( &new_conf ) ;

	/*
	 * Glossary:
	 *		Sconf: service configuration
	 *		Lconf: list of service configurations
	 *
	 * Iterate over all existing services. If the service is included in the 
	 * new Lconf, readjust its attributes (as a side-effect, the new service 
	 * Sconf is removed from the new Lconf).
	 *	Services not in the new Lconf are deactivated.
	 */
	for ( osp = SP( psi_start( iter ) ) ; osp ; osp = SP( psi_next( iter ) ) )
	{
		char *sid = SVC_ID( osp ) ;
		boolean_e drop_service ;

		if ( ! SVC_IS_AVAILABLE( osp ) )
			continue ;

		/*
		 * Check if this service is in the new Lconf
		 * Notice that the service Sconf is removed from the new Lconf
		 * if it is found there.
		 */
		if ( nscp = cnf_extract( &new_conf, SVC_CONF( osp ) ) )
		{
			/*
			 * The first action of readjust is to swap the service configuration
			 * with nscp. This is the reason for passing the address of nscp
			 * (so that on return nscp will *always* point to the old service
			 * configuration).
			 */
			if ( readjust( osp, &nscp, reconfig_type ) == OK )
			{
				/*
				 * Do the access control again
				 */
				if ( reconfig_type == RECONFIG_HARD )
					check_servers( osp ) ;

				old_services++ ;
				drop_service = NO ;
			}
			else	/* the readjustment failed */
				drop_service = YES ;
			sc_free( nscp ) ;
		}
		else
			drop_service = YES ;

		if ( drop_service == YES )
		{
			/*
			 * Procedure for disabling a service:
			 *
			 *		a. Terminate running servers and cancel retry attempts, in case
			 *			of hard reconfiguration
			 *		b. Deactivate the service
			 */
			if ( reconfig_type == RECONFIG_HARD )
			{
				terminate_servers( osp ) ;
				cancel_service_retries( osp ) ;
			}

			/*
			 * Deactivate the service; the service will be deleted only
			 * if its reference count drops to 0.
			 */
			svc_deactivate( osp ) ;
			msg( LOG_NOTICE, func, "service %s deactivated", sid ) ;
			if ( SVC_RELE( osp ) == 0 )
				psi_remove( iter ) ;
			dropped_services++ ;
		}
	}

	psi_destroy( iter ) ;

	/*
	 * At this point the new Lconf only contains services that were not
	 * in the old Lconf.
	 */
	new_services = cnf_start_services( &new_conf ) ;

	msg( LOG_NOTICE, func,
		"Reconfigured: new=%d old=%d dropped=%d (services)",
			new_services, old_services, dropped_services ) ;

	if ( ps.rws.available_services == 0 )
	{
		msg( LOG_CRIT, func, "No available services. Exiting" ) ;
		exit( 1 ) ;
	}

	cnf_free( &new_conf ) ;
}


PRIVATE void swap_defaults( confp )
	struct configuration *confp ;
{
	struct service_config *temp ;

	/*
	 * Close the previous common log file, if one was specified
	 */
	if ( DEFAULT_LOG( ps ) != NULL )
	{
		log_end( SC_LOG( DEFAULTS( ps ) ), DEFAULT_LOG( ps ) ) ;
		DEFAULT_LOG( ps ) = NULL ;
	}
	DEFAULT_LOG_ERROR( ps ) = FALSE ;

	SWAP( DEFAULTS( ps ), CNF_DEFAULTS( confp ), temp ) ;
}



PRIVATE void sendsig( serp, sig )
	register struct server	*serp ;
	int							sig ;
{
	char		*sid	= SVC_ID( SERVER_SERVICE( serp ) ) ;
	pid_t		pid	= SERVER_PID( serp ) ;
	char		*func = "sendsig" ;

	/*
	 * Always use a positive pid, because of the semantics of kill(2)
	 */
	if ( pid > 0 )
	{
		msg( LOG_WARNING, func, "Sending signal %d to %s server %d",
												sig, sid, pid ) ;
		(void) kill( pid, sig ) ;
	}
	else
		msg( LOG_ERR, func, "Negative server pid = %d. Service %s", pid, sid ) ;
}


/*
 * Send signal sig to all running servers of service sp
 */
PRIVATE void deliver_signal( sp, sig )
	register struct service *sp ;
	int sig ;
{
	register unsigned u ;

	for ( u = 0 ; u < pset_count( SERVERS( ps ) ) ; u++ )
	{
		register struct server *serp ;

		serp = SERP( pset_pointer( SERVERS( ps ), u ) ) ;
		if ( SERVER_SERVICE( serp ) == sp )
			sendsig( serp, sig ) ;
	}
}


/*
 * Terminate all servers of the specified service
 */
void terminate_servers( sp )
	register struct service *sp ;
{
	int sig = SC_IS_INTERNAL( SVC_CONF( sp ) ) ? SIGTERM : SIGKILL ;

	deliver_signal( sp, sig ) ;
}


PRIVATE void stop_interception( sp )
	struct service *sp ;
{
	deliver_signal( sp, INTERCEPT_SIG ) ;
}


/*
 * Do access control on all servers of the specified service.
 * If "limit" is 0, we don't mind if the service limit is exceeded.
 * If "limit" is non-zero, we kill servers that exceed the service limit
 * (but at most "limit" servers are killed because of exceeding the 
 * instances limit)
 *
 * Return value: number of servers that failed the check (and were killed)
 */
PRIVATE int server_check( sp, limit )
	register struct service		*sp ;
	int								limit ;
{
	register unsigned		u ;
	int						killed_servers	= 0 ;
	pset_h					server_set		= SERVERS( ps ) ;
	char						*func				= "server_check" ;

	if ( SVC_RUNNING_SERVERS( sp ) == 0 )
		return( 0 ) ;

	for ( u = 0 ; u < pset_count( server_set ) ; u++ )
	{
		register struct server *serp = SERP( pset_pointer( server_set, u ) ) ;

		if ( SERVER_SERVICE( serp ) == sp )
		{
			access_e result ;
			mask_t check_mask = MASK( CF_TIME ) ;

			if ( SVC_SOCKET_TYPE( sp ) != SOCK_DGRAM )
				M_OR( check_mask, check_mask, MASK( CF_ADDRESS ) ) ;
			if ( limit != 0 && killed_servers < limit )
				M_OR( check_mask, check_mask, MASK( CF_SERVICE_LIMIT ) ) ;
			
			result = access_control( sp, SERVER_CONNECTION( serp ), &check_mask );

			if ( result == AC_OK )
				continue ;
		
			msg( LOG_NOTICE, func, "%s server %d failed %s check",
				SVC_ID( sp ), SERVER_PID( serp ), access_explain( result ) ) ;
			sendsig( serp, SIGKILL ) ;
			killed_servers++ ;
		}
	}
	return( killed_servers ) ;
}


/*
 * Check if all the running servers of the specified service fit the 
 * new access control specifications. The ones that don't fit are killed.
 * 
 * We go through the server table twice. During the first
 * pass we ignore overruns of the server limit. During the second pass
 * we don't ignore such overruns until a certain number of servers are
 * terminated.
 */
PRIVATE void check_servers( sp )
	register struct service *sp ;
{
	int existant_servers = SVC_RUNNING_SERVERS( sp ) ;
	int limit ;

	existant_servers -= server_check( sp, 0 ) ;

	limit = existant_servers - SC_INSTANCES( SVC_CONF( sp ) ) ;
	if ( limit < 0 )
		limit = 0 ;
	(void) server_check( sp, limit ) ;
}



/*
 * Stop any logging and restart if necessary.
 * Note that this has the side-effect of using the new common log
 * handle as it should.
 */
PRIVATE status_e restart_log( sp, old_conf )
	struct service *sp ;
	struct service_config *old_conf ;
{
	struct log *lp = SC_LOG( old_conf ) ;

	if ( log_get_type( lp ) != L_NONE && SVC_IS_LOGGING( sp ) )
		log_end( lp, SVC_LOG( sp ) ) ;
	SVC_LOG( sp ) = NULL ;
	
	return( log_start( sp, &SVC_LOG( sp ) ) ) ;
}


/*
 * Unregister past versions, register new ones
 * We do it the dumb way: first unregister; then register
 * We try to be a little smart by checking if there has
 * been any change in version numbers (if not, we do nothing).
 * Also, we save the port number
 */
PRIVATE status_e readjust_rpc_service( old_scp, new_scp )
	register struct service_config *old_scp ;
	register struct service_config *new_scp ;
{
	unsigned				long vers ;
	unsigned short		port						= SC_PORT( old_scp ) ;
	struct rpc_data	*new_rdp					= SC_RPCDATA( new_scp ) ;
	struct rpc_data	*old_rdp					= SC_RPCDATA( old_scp ) ;
	unsigned				registered_versions	= 0 ;
	char					*func						= "readjust_rpc_service" ;

#ifndef NO_RPC
	SC_PORT( new_scp ) = SC_PORT( old_scp ) ;

	if ( RD_MINVERS( old_rdp ) == RD_MINVERS( new_rdp ) &&
				RD_MAXVERS( old_rdp ) == RD_MAXVERS( new_rdp ) )
		return( OK ) ;

	for ( vers = RD_MINVERS( old_rdp ) ; vers <= RD_MAXVERS( old_rdp ) ; vers++ )
		 (void) pmap_unset( RD_PROGNUM( old_rdp ), vers ) ;

	for ( vers = RD_MINVERS( new_rdp ) ; vers <= RD_MAXVERS( new_rdp ) ; vers++ )
		if ( pmap_set( RD_PROGNUM( new_rdp ),
										vers, SC_PROTOVAL( new_scp ), port ) )
			registered_versions++ ;
		else
			msg( LOG_ERR, func, 
				"pmap_set failed. service=%s, program=%ld, version = %ld",
					SC_ID( new_scp ), RD_PROGNUM( new_rdp ), vers ) ;

	if ( registered_versions == 0 )
	{
		msg( LOG_ERR, func,
				"No versions registered for RPC service %s", SC_ID( new_scp ) ) ;
		/*
		 * Avoid the pmap_unset
		 */
		RD_MINVERS( new_rdp ) = RD_MAXVERS( new_rdp ) + 1 ;
		return( FAILED ) ;
	}
#endif	/* ! NO_RPC */
	return( OK ) ;
}


/*
 * Readjust service attributes. 
 *
 * We assume that the following attributes are the same:
 *			wait
 *			socket_type
 *			type
 *			protocol
 *
 * Readjustment happens in 3 steps:
 *		1) We swap the svc_conf fields
 *				This has the side-effect of free'ing the memory associated
 *				with the old service configuration when the new configuration
 *				is destroyed.
 *		2) We readjust the fields that require some action to be taken:
 *				RPC mapping
 *				log file open
 *		3) We update the address control fields.
 */
PRIVATE status_e readjust( sp, new_conf_ptr, type )
	register struct service				*sp ;
	register struct service_config	**new_conf_ptr ;
	reconfig_e								type ;
{
	struct service_config	*temp_conf ;
	struct service_config	*old_conf	= SVC_CONF( sp ) ;
	struct service_config	*new_conf	= *new_conf_ptr ;
	char							*sid			= SVC_ID( sp ) ;
	char							*func			= "readjust" ;

	msg( LOG_NOTICE, func, "readjusting service %s", sid ) ;

	SWAP( SVC_CONF( sp ), *new_conf_ptr, temp_conf ) ;

	if ( SC_IS_RPC( old_conf ) &&
						readjust_rpc_service( old_conf, new_conf ) == FAILED )
		return( FAILED ) ;
	
	/*
	 * This is what happens if the INTERCEPT flag is toggled and an
	 * interceptor is running:
	 *
	 * Case 1: clear->set
	 *		Wait until the server dies (soft reconfig) or
	 *		terminate the server (hard reconfig)
	 *
	 * Case 2: set->clear
    *    Send a signal to the interceptor to tell it to stop intercepting
	 */
	if ( SC_IS_INTERCEPTED( old_conf ) != SC_IS_INTERCEPTED( new_conf ) )
	{
		if ( SC_IS_INTERCEPTED( new_conf ) )			/* case 1 */
		{
			if ( type == RECONFIG_HARD )
				terminate_servers( sp ) ;
		}
		else													/* case 2 */
		{
			stop_interception( sp ) ;
			msg( LOG_NOTICE, func, "Stopping interception for %s", sid ) ;
		}
	}
	svc_setup_address_control( sp ) ;
	return( restart_log( sp, old_conf ) ) ;
}

