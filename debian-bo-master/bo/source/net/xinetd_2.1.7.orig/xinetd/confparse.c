/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#include <sys/types.h>
#include <sys/param.h>
#include <syslog.h>
#include <netdb.h>
#include <string.h>

#if defined(BSD) || defined(linux)
#include <netinet/in.h>
#endif

#if defined(BSD)
#include <rpc/rpc.h>
#else
char *malloc() ;
#endif

void endservent() ;
void endprotoent() ;
void endrpcent() ;

#include "pset.h"
#include "misc.h"

#include "attr.h"
#include "config.h"
#include "conf.h"
#include "defs.h"
#include "parse.h"
#include "sconst.h"
#include "sconf.h"
#include "state.h"

void out_of_memory() ;
void msg() ;

/*
 * Pset iterator used by functions in this file.
 * It lives only when get_configuration is called (i.e. it is created and
 * destroyed each time). This is because the pset it is iterating on
 * changes.
 */
static psi_h iter ;


PRIVATE status_e fix_server_argv( scp )
	struct service_config *scp ;
{
	char *server_name ;
	char *func = "fix_server_argv" ;

	/*
	 * Check if the user specified any server arguments.
	 * If not, then the server_argv has not been allocated yet,
	 * so malloc it (size 2)
	 * Put in argv[ 0 ] the last component of the server pathname
	 */
	if ( ! SC_SPECIFIED( scp, A_SERVER_ARGS ) )
	{
		scp->sc_server_argv = (char **) malloc( 2 * sizeof( char * ) ) ;
		if ( scp->sc_server_argv == NULL )
		{
			out_of_memory( func ) ;
			return( FAILED ) ;
		}
		scp->sc_server_argv[ 1 ] = NULL ;
		SC_PRESENT( scp, A_SERVER_ARGS ) ;
	}

	/*
	 * Determine server name
	 */
	server_name = strrchr( scp->sc_server, '/' ) ;
	if ( server_name == NULL )
		server_name = scp->sc_server ;
	else
		server_name++ ;      /* skip the '/' */

	/*
	 * Place it in argv[ 0 ]
	 */
	scp->sc_server_argv[ 0 ] = make_string( 1, server_name ) ;
	if ( scp->sc_server_argv[ 0 ] == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}



#define USE_DEFAULT( scp, def, attr_id )	\
			( ! SC_SPECIFIED( scp, attr_id ) && SC_SPECIFIED( def, attr_id ) )

/*
 * Fill the service configuration with attributes that were not
 * explicitly specified. These can be:
 *		1) implied attributes (like the server name in argv[0])
 *		2) attributes from 'defaults' so that we won't need to check
 *			'defaults' anymore.
 *		3) default values (like the service instance limit)
 */
PRIVATE status_e attr_fill( scp, def )
	struct service_config *scp ;
	struct service_config *def ;
{
	char *func = "attr_fill" ;
	status_e setup_environ() ;

	if ( ! SC_IS_INTERNAL( scp ) && fix_server_argv( scp ) == FAILED )
		return( FAILED ) ;

	if ( ! SC_SPECIFIED( scp, A_INSTANCES ) )
	{
		scp->sc_instances = SC_SPECIFIED( def, A_INSTANCES ) ? def->sc_instances
																	  : DEFAULT_INSTANCE_LIMIT ;
		SC_PRESENT( scp, A_INSTANCES ) ;
	}

	if ( USE_DEFAULT( scp, def, A_LOG_ON_SUCCESS ) )
	{
		scp->sc_log_on_success = def->sc_log_on_success ;
		SC_PRESENT( scp, A_LOG_ON_SUCCESS ) ;
	}

	if ( USE_DEFAULT( scp, def, A_LOG_ON_FAILURE ) )
	{
		scp->sc_log_on_failure = def->sc_log_on_failure ;
		SC_PRESENT( scp, A_LOG_ON_FAILURE ) ;
	}

	if ( USE_DEFAULT( scp, def, A_LOG_TYPE ) )
	{
		struct log *dlp = SC_LOG( def ) ;
		struct log *slp = SC_LOG( scp ) ;

		switch ( log_get_type( dlp ) )
		{
			case L_NONE:
				log_set_type( slp, L_NONE ) ;
				break ;
			
			case L_SYSLOG:
				*slp = *dlp ;
				break ;
			
			case L_FILE:
				log_set_type( slp, L_COMMON_FILE ) ;
				break ;

			default:
				msg( LOG_ERR, func,
								"bad log type: %d", (int) log_get_type( dlp ) ) ;
				return( FAILED ) ;
		}
		SC_PRESENT( scp, A_LOG_TYPE ) ;
	}
	if ( setup_environ( scp, def ) == FAILED )
		return( FAILED ) ;
	return( OK ) ;
}


PRIVATE void remove_disabled_services( confp )
	struct configuration *confp ;
{
	pset_h disabled_services ;
	register struct service_config *scp ;
	struct service_config *defaults = confp->cnf_defaults ;

	if ( ! SC_SPECIFIED( defaults, A_DISABLED ) )
		return ;
	
	disabled_services = defaults->sc_disabled ;

	for ( scp = SCP( psi_start( iter ) ) ; scp ; scp = SCP( psi_next( iter ) ) )
	{
		register char *sid = SC_ID( scp ) ;
		register unsigned u ;

		for ( u = 0 ; u < pset_count( disabled_services ) ; u++ )
			if ( EQ( sid, (char *) pset_pointer( disabled_services, u ) ) )
			{
				sc_free( scp ) ;
				psi_remove( iter ) ;
				break ;
			}
	}
}


/*
 * Check if all required attributes have been specified
 */
PRIVATE status_e attr_check( scp )
	register struct service_config *scp ;
{
	mask_t			necessary_and_specified ;
	mask_t			necessary_and_missing ;
	mask_t			must_specify = NECESSARY_ATTRS ;
	register int	attr_id ;
	char				*attr_name ;
	char				*func = "attr_check" ;
	char				*attr_name_lookup() ;

	/*
	 * Determine what attributes must be specified
	 */
	if ( ! SC_IS_INTERNAL( scp ) )
		M_OR( must_specify, must_specify, NECESSARY_ATTRS_EXTERNAL ) ;

	if ( SC_IS_UNLISTED( scp ) )
		M_OR( must_specify, must_specify, NECESSARY_ATTRS_UNLISTED ) ;
	else
		M_OR( must_specify, must_specify, NECESSARY_ATTRS_LISTED ) ;

	if ( SC_IS_RPC( scp ) )
		if ( SC_IS_UNLISTED( scp ) )
		{
			M_OR( must_specify, must_specify, NECESSARY_ATTRS_RPC_UNLISTED ) ;
			M_CLEAR( must_specify, A_PORT ) ;
		}
		else
			M_OR( must_specify, must_specify, NECESSARY_ATTRS_RPC_LISTED ) ;

	/*
	 * Check if all necessary attributes have been specified
	 *
	 * NOTE: None of the necessary attributes can belong to "defaults"
	 *			This is why we use the sc_attributes_specified mask instead
	 *			of the sc_attributes_present mask.
	 */
	M_AND( necessary_and_specified,
						scp->sc_specified_attributes, must_specify ) ;
	M_XOR( necessary_and_missing, necessary_and_specified, must_specify ) ;

	if ( M_ARE_ALL_CLEAR( necessary_and_missing ) )
		return( OK ) ;
	
	/*
	 * Print names of missing attributes
	 */
	for ( attr_id = 0 ; attr_id < SERVICE_ATTRIBUTES ; attr_id++ )
		if ( M_IS_SET( necessary_and_missing, attr_id ) && 
						( attr_name = attr_name_lookup( attr_id ) ) != NULL )
			msg( LOG_ERR, func,
				"Service %s missing attribute %s", scp->sc_id, attr_name ) ;
	return( FAILED ) ;
}


/*
 * Perform validity checks on the specified entry
 *
 * Also does the following:
 *		1. If this is an internal service, it finds the function that
 *			implements it
 *		2. For RPC services, it finds the program number
 *		3. For non-RPC services, it finds the port number.
 */
PRIVATE status_e check_entry( scp )
	register struct service_config *scp ;
{
	char *func = "check_entry" ;

	if ( attr_check( scp ) == FAILED )
		return( FAILED ) ;

	/*
	 * Currently, we cannot intercept:
	 *		1) internal services
	 *		2) multi-threaded services
	 * We clear the INTERCEPT flag without disabling the service.
	 */
	if ( SC_IS_INTERCEPTED( scp ) )
	{
		if ( SC_IS_INTERNAL( scp ) )
		{
			msg( LOG_ERR, func,
				"Internal services cannot be intercepted: %s ", scp->sc_id ) ;
			M_CLEAR( scp->sc_flags, SF_INTERCEPT ) ;
		}
		if ( scp->sc_wait == NO )
		{
			msg( LOG_ERR, func,
				"Multi-threaded services cannot be intercepted: %s", scp->sc_id ) ;
			M_CLEAR( scp->sc_flags, SF_INTERCEPT ) ;
		}
	}

	if ( SC_IS_INTERNAL( scp ) &&
		( scp->sc_builtin = 
				builtin_find( scp->sc_name, scp->sc_socket_type ) ) == NULL )
			return( FAILED ) ;

	/*
	 * If this is an unlisted service we are done.
	 * attr_check() guaranteed that we have all the information we need
	 * to start the service.
	 */
	if ( SC_IS_UNLISTED( scp ) )
		return( OK ) ;

#ifndef NO_RPC
	if ( SC_IS_RPC( scp ) )
	{
		struct rpcent *rep = getrpcbyname( scp->sc_name ) ;

		if ( rep == NULL )
		{
			msg( LOG_ERR, func, "unknown RPC service: %s", scp->sc_name ) ;
			return( FAILED ) ;
		}
		SC_RPCDATA( scp )->rd_program_number = rep->r_number ;
	}
	else
#endif	/* ! NO_RPC */
	{
		struct servent *sep ;
		unsigned short service_port ;

		/*
		 * Check if a protocol was specified.
		 * If so, verify it is a proper protocol for the given service.
		 * If no protocol was specified, find the protocol for the service
		 * and its number.
		 */
		if ( SC_SPECIFIED( scp, A_PROTOCOL ) )
		{
			sep = getservbyname( scp->sc_name, scp->sc_protocol.name ) ;
			if ( sep == NULL )
			{
				msg( LOG_ERR, func, 
					"service/protocol combination not in /etc/services: %s/%s",
						scp->sc_name, scp->sc_protocol.name ) ;
				return( FAILED ) ;
			}
		}
		else
		{
			struct protoent *pep ;

			sep = getservbyname( scp->sc_name, CHAR_NULL ) ;
			if ( sep == NULL )
			{
				msg( LOG_ERR, func, "Unknown service: %s", scp->sc_name ) ;
				return( FAILED ) ;
			}
			scp->sc_protocol.name = make_string( 1, sep->s_proto ) ;
			if ( scp->sc_protocol.name == NULL )
			{
				out_of_memory( func ) ;
				return( FAILED ) ;
			}
			pep = getprotobyname( sep->s_proto ) ;
			if ( pep == NULL )
			{
				msg( LOG_ERR, func,
					"Protocol '%s' for service '%s' is not in /etc/protocols",
						sep->s_proto, scp->sc_name ) ;
				return( FAILED ) ;
			}
			scp->sc_protocol.value = pep->p_proto ;
		}

		service_port = sep->s_port ;		/* s_port is in network-byte-order */

		/*
		 * If a port was specified, it must be the right one
		 */
		if ( SC_SPECIFIED( scp, A_PORT ) && 
											scp->sc_port != ntohs( service_port ) )
		{
			msg( LOG_ERR, func, "Service %s expects port %d, not %d",
								scp->sc_name, ntohs( service_port ), scp->sc_port ) ;
			return( FAILED ) ;
		}
		scp->sc_port = ntohs( service_port ) ;
	}
	return( OK ) ;
}


void parse_conf_file() ;
void parse_end() ;

#if CONF_TIMEOUT != 0 && ! defined( NO_TIMERS )

#include <sys/time.h>
#include "timer.h"
#include "timemacros.h"

static timer_h conf_timer ;

status_e create_conf_timer()
{
	conf_timer = timer_create( TIMER_REAL, TIMER_RETURN_ERROR, INT_NULL ) ;
	return( ( conf_timer == NULL ) ? FAILED : OK ) ;
}

#endif	/* CONF_TIMEOUT != 0 && ! NO_TIMERS */


/*
 * Get a configuration from the specified file.
 * In case of timeout, do cleanup
 */
PRIVATE status_e get_conf( fd, confp, timeout )
	int fd ;
	struct configuration *confp ;
	long timeout ;
{
#if CONF_TIMEOUT != 0 && ! defined( NO_TIMERS )

	struct itimerval itv ;
	struct timer_action ta ;
	char *func = "get_conf" ;
	
	if ( timeout )
	{
		if ( setjmp( ta.ta_env ) )
		{
			parse_end() ;
			return( FAILED ) ;
		}

		/*
		 * Start the timer
		 */
		TV_ZERO( itv.it_interval ) ;
		itv.it_value.tv_sec	= timeout ;
		itv.it_value.tv_usec	= 0 ;
		ta.ta_flags				= TIMER_LONGJMP ;
		ta.ta_func				= NULL ;
		ta.ta_arg				= VOID_NULL ;
		if ( timer_start( conf_timer, &itv, TIMER_RELATIVE, &ta ) == TIMER_ERR )
		{
			msg( LOG_ERR, func, "timer creation failed" ) ;
			return( FAILED ) ;
		}
	}

	parse_conf_file( fd, confp ) ;

	if ( timeout )
		timer_stop( conf_timer ) ;
	parse_end() ;
	return( OK ) ;

#else		/* CONF_TIMEOUT == 0 || NO_TIMERS */

#ifdef lint
	timeout = timeout ;
#endif
	parse_conf_file( fd, confp ) ;
	parse_end() ;
	return( OK ) ;
#endif	/* CONF_TIMEOUT != 0 && ! NO_TIMERS */

}



#define CHECK_AND_CLEAR( scp, mask, mask_name )											\
	if ( M_IS_SET( mask, LO_USERID ) )														\
	{																									\
		msg( LOG_WARNING, func,																	\
		"%s service: clearing USERID option from %s", scp->sc_id, mask_name ) ;	\
		M_CLEAR( mask, LO_USERID ) ;															\
	}



/*
 * Get a configuration by reading the configuration file.
 */
status_e cnf_get( confp, timeout )
	struct configuration *confp ;
	long timeout ;
{
	int config_fd ;
	struct service_config *scp ;
	builtin_s *spec_find() ;
	char *func = "get_configuration" ;

	if ( cnf_init( confp, &config_fd, &iter ) == FAILED )
		return( FAILED ) ;

	if ( get_conf( config_fd, confp, timeout ) == FAILED )
	{
		Sclose( config_fd ) ;
		cnf_free( confp ) ;
		psi_destroy( iter ) ;
		return( FAILED ) ;
	}

	Sclose( config_fd ) ;

	remove_disabled_services( confp ) ;

	for ( scp = SCP( psi_start( iter ) ) ; scp ; scp = SCP( psi_next( iter ) ) )
	{
		if ( check_entry( scp ) == FAILED )
		{
			sc_free( scp ) ;
			psi_remove( iter ) ;
			continue ;
		}

		/*
		 * Fill the service configuration from the defaults.
		 * We do this so that we don't have to look at the defaults any more.
		 */
		if ( attr_fill( scp, confp->cnf_defaults ) == FAILED )
		{
			sc_free( scp ) ;
			psi_remove( iter ) ;
			continue ;
		}

		/*
		 * If the INTERCEPT flag is set, change this service to an internal 
		 * service using the special INTERCEPT builtin.
		 */
		if ( SC_IS_INTERCEPTED( scp ) )
		{
			builtin_s *bp ;

			bp = spec_find( INTERCEPT_SERVICE_NAME, scp->sc_socket_type ) ;
			if ( bp == NULL )
			{
				msg( LOG_ERR, func, "removing service %s", SC_ID( scp ) ) ;
				sc_free( scp ) ;
				psi_remove( iter ) ;
				continue ;
			}

			scp->sc_builtin = bp ;
			M_SET( scp->sc_type, ST_INTERNAL ) ;
		}

		/*
		 * Clear the USERID flag for the identity service because
		 * it may lead to loops (for example, remote xinetd issues request,
		 * local xinetd issues request to remote xinetd etc.)
		 * We identify the identity service by its (protocol,port) combination.
		 */
		if ( scp->sc_port == IDENTITY_SERVICE_PORT && 
													scp->sc_protocol.value == IPPROTO_TCP )
		{
			CHECK_AND_CLEAR( scp, scp->sc_log_on_success, "log_on_success" ) ;
			CHECK_AND_CLEAR( scp, scp->sc_log_on_failure, "log_on_failure" ) ;
		}
	}

	psi_destroy( iter ) ;

	if ( debug.on && debug.fd != -1 )
		cnf_dump( confp, debug.fd ) ;

	endservent() ;
	endprotoent() ;
#ifndef NO_RPC
	endrpcent() ;
#endif
	return( OK ) ;
}


