/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: sconf.c,v 1.2 1995/09/10 14:26:36 chuck Exp $" ;

#include "misc.h"

#include "state.h"
#include "defs.h"
#include "attr.h"
#include "sconf.h"
#include "addr.h"

char *malloc() ;
int free() ;

#define NEW_SCONF()					NEW( struct service_config )
#define FREE_SCONF( scp )			FREE( scp )

/*
 * Conditional free; checks if the pointer is NULL
 */
#define COND_FREE( x )           if ( x )                   \
                                 {                          \
                                    free( (char *) x ) ;    \
                                    x = NULL ;              \
                                 }

extern struct name_value success_log_options[] ;
extern struct name_value failure_log_options[] ;
extern struct name_value service_types[] ;
extern struct name_value service_flags[] ;
extern struct name_value socket_types[] ;
extern struct name_value syslog_facilities[] ;
extern struct name_value syslog_levels[] ;

void tabprint() ;
void out_of_memory() ;


/*
 * Allocate a new service_config and initialize the service name field
 * with 'name'; the rest of the fields are set to 0 which gives them
 * their default values.
 */
struct service_config *sc_alloc( name )
	char *name ;
{
	struct service_config *scp ;
	char *func = "sc_alloc" ;

	scp = NEW_SCONF() ;
	if ( scp == NULL )
	{
		out_of_memory( func ) ;
		return( NULL ) ;
	}
	CLEAR( *scp ) ;
	scp->sc_name = name ;
	return( scp ) ;
}


PRIVATE void release_string_pset( pset )
	pset_h pset ;
{
	pset_apply( pset, free, NULL ) ;
	pset_destroy( pset ) ;
}



/*
 * Free all malloc'ed memory for the specified service
 */
void sc_free( scp )
   register struct service_config *scp ;
{
	void ti_free() ;

   COND_FREE( scp->sc_name ) ;
   COND_FREE( scp->sc_id ) ;
   COND_FREE( scp->sc_protocol.name ) ;
   COND_FREE( scp->sc_server ) ;
   if ( scp->sc_server_argv )
   {
      register char **pp ;

		/*
		 * argv[ 0 ] is a special case because it may not have been allocated yet
		 */
		if ( scp->sc_server_argv[ 0 ] != NULL)
			free( scp->sc_server_argv[ 0 ] ) ;
      for ( pp = &scp->sc_server_argv[ 1 ] ; *pp != NULL ; pp++ )
         free( *pp ) ;
      free( (char *) scp->sc_server_argv ) ;
   }
   COND_FREE( log_filelog( SC_LOG( scp ) )->fl_filename ) ;

	if ( scp->sc_access_times != NULL )
	{
		ti_free( scp->sc_access_times ) ;
		pset_destroy( scp->sc_access_times ) ;
	}

	if ( scp->sc_only_from != NULL )
	{
		addrlist_free( scp->sc_only_from ) ;
		pset_destroy( scp->sc_only_from ) ;
	}

	if ( scp->sc_no_access != NULL )
	{
		addrlist_free( scp->sc_no_access ) ;
		pset_destroy( scp->sc_no_access ) ;
	}

   if ( scp->sc_env_var_defs != NULL )
		release_string_pset( scp->sc_env_var_defs ) ;
	if ( scp->sc_pass_env_vars != NULL )
		release_string_pset( scp->sc_pass_env_vars ) ;
	if ( SC_ENV( scp )->env_type == CUSTOM_ENV && 
												SC_ENV( scp )->env_handle != ENV_NULL )
		env_destroy( SC_ENV( scp )->env_handle ) ;
	
	FREE_SCONF( scp ) ;
}


/*
 * Create a configuration for one of the special services
 */
struct service_config *sc_make_special( service_name, bp, instances )
	char *service_name ;
	builtin_s *bp ;
	int instances ;
{
	char *name ;
	struct service_config *scp ;
	char *func = "sc_make" ;

	name = make_string( 1, service_name ) ;
	if ( name == NULL )
	{
		out_of_memory( func ) ;
		return( NULL ) ;
	}

	if ( ( scp = sc_alloc( name ) ) == NULL )
	{
		free( name ) ;
		return( NULL ) ;
	}

   scp->sc_id = make_string( 1, scp->sc_name ) ;
   if ( scp->sc_id == NULL )
   {
      free( name ) ;
      out_of_memory( func ) ;
      return( NULL ) ;
   }
   SC_SPECIFY( scp, A_ID ) ;

	/*
	 * All special services are internal
	 */
   M_SET( scp->sc_type, ST_SPECIAL ) ;
   M_SET( scp->sc_type, ST_INTERNAL ) ;
	scp->sc_builtin = bp ;
   SC_SPECIFY( scp, A_TYPE ) ;

   M_SET( scp->sc_flags, SF_NORETRY ) ;
   SC_SPECIFY( scp, A_FLAGS ) ;

   scp->sc_instances = instances ;
   SC_SPECIFY( scp, A_INSTANCES ) ;

   scp->sc_wait = NO ;
   SC_SPECIFY( scp, A_WAIT ) ;

	return( scp ) ;
}



PRIVATE void dump_log_data( fd, scp, tab_level )
	int fd ;
	register struct service_config *scp ;
	int tab_level ;
{
	register struct log *lp = SC_LOG( scp ) ;
	register struct filelog *flp ;
	int i ;

	switch ( log_get_type( lp ) )
	{
		case L_NONE:
			tabprint( fd, tab_level, "No logging\n" ) ;
			return ;

		case L_COMMON_FILE:
			tabprint( fd, tab_level, "Logging to common log file\n" ) ;
			break ;

		case L_FILE:
			flp = log_filelog( lp ) ;
			tabprint( fd, tab_level, "Logging to file: %s", flp->fl_filename ) ;

			if ( FILELOG_SIZE_CONTROL( flp ) )
				Sprint( fd, " (soft=%d hard=%d)\n",
								flp->fl_soft_limit, flp->fl_hard_limit ) ;
			else
				Sprint( fd, " (no limits)\n" ) ;
			break ;
		
		case L_SYSLOG:
			tabprint( fd, tab_level,
				"Logging to syslog. Facility = %s, level = %s\n",
					nv_get_name( syslog_facilities, log_syslog( lp )->sl_facility ),
					nv_get_name( syslog_levels, log_syslog( lp )->sl_level ) ) ;
			break ;
	}

	tabprint( fd, tab_level, "Log_on_success flags =" ) ;
	for ( i = 0 ; success_log_options[ i ].name != NULL ; i++ )
		if ( M_IS_SET( scp->sc_log_on_success, success_log_options[ i ].value ) )
			Sprint( fd, " %s", success_log_options[ i ].name ) ;
	Sputchar( fd, '\n' ) ;

	tabprint( fd, tab_level, "Log_on_failure flags =" ) ;
	for ( i = 0 ; failure_log_options[ i ].name != NULL ; i++ )
		if ( M_IS_SET( scp->sc_log_on_failure, failure_log_options[ i ].value ) )
			Sprint( fd, " %s", failure_log_options[ i ].name ) ;
	Sputchar( fd, '\n' ) ;
}


/*
 * Print info about service scp to file descriptor fd
 */
void sc_dump( scp, fd, tab_level, is_defaults )
	register struct service_config *scp ;
	register int fd ;
	int tab_level ;
	bool_int is_defaults ;
{
	register struct name_value		*nvp ;
	register unsigned 				u ;
	register char 						**pp ;
	void 									ti_dump() ;

	if ( is_defaults )
		tabprint( fd, tab_level, "Service defaults\n" ) ;
	else
		tabprint( fd, tab_level, "Service configuration: %s\n", scp->sc_name ) ;

	if ( ! is_defaults )
	{
		tabprint( fd, tab_level+1, "id = %s\n", scp->sc_id ) ;

		if ( ! M_ARE_ALL_CLEAR( scp->sc_flags ) )
		{
			tabprint( fd, tab_level+1, "flags =" ) ;
			for ( nvp = &service_flags[ 0 ] ; nvp->name != NULL ; nvp++ )
				if ( M_IS_SET( scp->sc_flags, nvp->value ) )
					Sprint( fd, " %s", nvp->name ) ;
			Sputchar( fd, '\n' ) ;
		}

		if ( ! M_ARE_ALL_CLEAR( scp->sc_type ) )
		{
			tabprint( fd, tab_level+1, "type =" ) ;
			for ( nvp = &service_types[ 0 ] ; nvp->name != NULL ; nvp++ )
				if ( M_IS_SET( scp->sc_type, nvp->value ) )
					Sprint( fd, " %s", nvp->name ) ;
			Sputchar( fd, '\n' ) ;
		}

		tabprint( fd, tab_level+1, "socket_type = %s\n",
			nv_get_name( socket_types, scp->sc_socket_type ) ) ;

		if ( SC_SPECIFIED( scp, A_PORT ) )
			tabprint( fd, tab_level+1, "port = %d\n", scp->sc_port ) ;

#ifdef BIND_IF
		tabprint( fd, tab_level+1, "Interface = %s\n",
                  inet_ntoa( scp->sc_interface ) ) ;
#endif

		tabprint( fd, tab_level+1, "Protocol (name,number) = (%s,%d)\n",
				scp->sc_protocol.name, scp->sc_protocol.value ) ;
	}

	if ( SC_SPECIFIED( scp, A_INSTANCES ) )
		if ( scp->sc_instances == UNLIMITED )
			tabprint( fd, tab_level+1, "Instances = UNLIMITED\n" ) ;
		else
			tabprint( fd, tab_level+1, "Instances = %d\n", scp->sc_instances ) ;
		
	if ( SC_SPECIFIED( scp, A_NICE ) )
		tabprint( fd, tab_level+1, "Nice = %d\n", scp->sc_nice ) ;

	if ( ! is_defaults )
	{
		if ( ! SC_IS_INTERNAL( scp ) )
		{
			tabprint( fd, tab_level+1, "Server = %s\n", scp->sc_server ) ;
			tabprint( fd, tab_level+1, "Server argv =" ) ;
			for ( pp = scp->sc_server_argv ; *pp ; pp++ )
				Sprint( fd, " %s", *pp ) ;
			Sputchar( fd, '\n' ) ;
		} 

		if ( SC_IS_RPC( scp ) )
		{
			struct rpc_data *rdp = SC_RPCDATA( scp ) ;

			tabprint( fd, tab_level+1, "RPC data\n" ) ;
			tabprint( fd, tab_level+2,
									"program number = %ld\n", rdp->rd_program_number ) ;
			tabprint( fd, tab_level+2, "rpc_version = " ) ;
			if ( rdp->rd_min_version == rdp->rd_max_version )
				Sprint( fd, "%ld\n", rdp->rd_min_version ) ;
			else
				Sprint( fd, "%ld-%ld\n",
									rdp->rd_min_version, rdp->rd_max_version ) ;
		}

		if ( SC_SPECIFIED( scp, A_ACCESS_TIMES ) )
		{
			tabprint( fd, tab_level+1, "Access times =" ) ;
			ti_dump( scp->sc_access_times, fd ) ;
			Sputchar ( fd, '\n' ) ;
		}
	}

	if ( SC_SPECIFIED( scp, A_ONLY_FROM ) )
	{
		tabprint( fd, tab_level+1, "Only from: " ) ;
		addrlist_dump( scp->sc_only_from, fd ) ;
		Sputchar( fd, '\n' ) ;
	}

	if ( SC_SPECIFIED( scp, A_NO_ACCESS ) )
	{
		tabprint( fd, tab_level+1, "No access: " ) ;
		addrlist_dump( scp->sc_no_access, fd ) ;
		Sputchar( fd, '\n' ) ;
	}
	
	dump_log_data( fd, scp, tab_level+1 ) ;

	if ( SC_SPECIFIED( scp, A_PASSENV ) )
	{
		tabprint( fd, tab_level+1, "Passenv =" ) ;
		for ( u = 0 ; u < pset_count( scp->sc_pass_env_vars ) ; u++ )
			Sprint( fd, " %s",
						(char *) pset_pointer( scp->sc_pass_env_vars, u ) ) ;
		Sputchar ( fd, '\n' ) ;
	}

	if ( ! is_defaults )
		if ( SC_SPECIFIED( scp, A_ENV ) )
		{
			tabprint( fd, tab_level+1, "Environment additions:\n" ) ;
			for ( u = 0 ; u < pset_count( scp->sc_env_var_defs ) ; u++ )
				tabprint( fd, tab_level+2,
						"%s\n", (char *) pset_pointer( scp->sc_env_var_defs, u ) ) ;
		}
	
	if ( SC_ENV( scp )->env_type == CUSTOM_ENV )
	{
		tabprint( fd, tab_level+1, "Environment strings:\n" ) ;
		for ( pp = env_getvars( SC_ENV( scp )->env_handle ) ; *pp ; pp++ )
			tabprint( fd, tab_level+2, "%s\n", *pp ) ;
	}
	Sflush( fd ) ;
}


#define SC_RPCPROGNUM( s )    RD_PROGNUM( SC_RPCDATA( s ) )
#define SAME_RPC( s1, s2 )    ( SC_RPCPROGNUM( s1 ) == SC_RPCPROGNUM( s2 ) )
#define SAME_NONRPC( s1, s2 ) ( (s1)->sc_socket_type == (s2)->sc_socket_type \
                                 && (s1)->sc_port == (s2)->sc_port )

/*
 * Two service configurations are considered different if any of the
 * following is TRUE:
 *		1) only one is unlisted
 *		2) only one is internal
 *		3) only one is RPC
 *		4) they have different values for the 'wait' attribute
 *		5) they use different protocols
 *		6) they are both RPC services but have different program numbers
 *		7) neither is an RPC service and they have different socket_types or
 *			use diffent ports
 *
 * This function returns TRUE if the specified configurations are different.
 *
 * Note that this function is closely related to the 'readjust' function
 * that is invoked on reconfiguration; that function will not change 
 * attributes that this function checks to determine if two configurations
 * are different.
 */
bool_int sc_different_confs( scp1, scp2 )
	struct service_config *scp1, *scp2 ;
{
   if ( SC_IS_UNLISTED( scp1 ) != SC_IS_UNLISTED( scp2 ) ||
				SC_IS_INTERNAL( scp1 ) != SC_IS_INTERNAL( scp2 ) ||
					SC_IS_RPC( scp1 ) != SC_IS_RPC( scp2 ) )
      return( TRUE ) ;

   if ( scp1->sc_wait != scp2->sc_wait )
      return( TRUE ) ;
  
	if ( scp1->sc_protocol.value != scp2->sc_protocol.value )
      return( TRUE ) ;

#ifdef BIND_IF
	/* NV - added to check if IP addresses are different.
     * by default we stop all instances of a server if it
     * is to rebind to another interface or if it is 
     * disabled from an interface
     */
	if ( scp1->sc_interface.s_addr != scp2->sc_interface.s_addr )
		return( TRUE ) ;
#endif

   if ( SC_IS_RPC( scp1 ) )
   {
		if ( ! SAME_RPC( scp1, scp2 ) )
         return( TRUE ) ;
   }
   else
   {
		if ( ! SAME_NONRPC( scp1, scp2 ) )
         return( TRUE ) ;
   }
   return( FALSE ) ;
}

