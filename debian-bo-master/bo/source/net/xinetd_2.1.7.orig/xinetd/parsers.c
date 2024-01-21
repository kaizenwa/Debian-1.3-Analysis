/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: parsers.c,v 1.3 1995/09/10 18:41:13 chuck Exp $" ;

#ifdef linux
#include <unistd.h>
#endif
#include <sys/file.h>
#include <syslog.h>
#include <string.h>
#include <netdb.h>
#include <pwd.h>
#include <grp.h>

#ifdef BIND_IF
#include <netinet/in.h>  /* NV */
#endif

int free() ;

#include "pset.h"
#include "misc.h"

#include "defs.h"
#include "parse.h"
#include "sconf.h"
#include "config.h"
#include "addr.h"

#define NEW_SET( set, v1, v2 )												\
				if ( (set) == NULL && 										\
					( (set) = pset_create( (set), (v1), (v2) ) ) == NULL )	\
				{															\
					out_of_memory( func ) ;									\
					return( FAILED ) ;										\
				}


void msg() ;
void parsemsg() ;
void out_of_memory() ;
char **argv_alloc() ;

extern struct name_value success_log_options[] ;
extern struct name_value failure_log_options[] ;
extern struct name_value service_types[] ;
extern struct name_value socket_types[] ;
extern struct name_value service_flags[] ;
extern struct name_value syslog_facilities[] ;
extern struct name_value syslog_levels[] ;

extern env_h std_env ;


/*
 * Find the flags corresponding to strings in "values" and apply
 * them to "*maskp" (apply means add or remove depending on "op")
 * "description" describes the type of flags.
 */
PRIVATE status_e parse_value_list( values, maskp, list, op, description )
	pset_h					values ;
	mask_t					*maskp ;
	struct name_value		list[] ;
	enum assign_op			op ;
	char						*description ;
{
	register unsigned				u ;
	register struct name_value *nvp ;
	char								*func = "parse_value_list" ;

	for ( u = 0 ; u < pset_count( values ) ; u++ )
	{
		char *name = (char *) pset_pointer( values, u ) ;

		nvp = nv_find_value( list, name ) ;
		if ( nvp != NULL )
			if ( op == PLUS_EQ )
				M_SET( *maskp, nvp->value ) ;
			else
				M_CLEAR( *maskp, nvp->value ) ;
		else
			parsemsg( LOG_WARNING, func, "Bad %s: %s", description, name ) ;
	}
	return( OK ) ;
}


status_e type_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	return( parse_value_list( values,
						&scp->sc_type, service_types, PLUS_EQ, "service type" ) ) ;
}


status_e flags_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	return( parse_value_list( values,
						&scp->sc_flags, service_flags, PLUS_EQ, "service flag" ) ) ;
}


status_e socket_type_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	register struct name_value *nvp ;
	register char *type = (char *) pset_pointer( values, 0 ) ;
	char *func = "socket_type_parser" ;

	nvp = nv_find_value( socket_types, type ) ;
	if ( nvp != NULL )
	{
		scp->sc_socket_type = nvp->value ;
		return( OK ) ;
	}
	else
	{
		parsemsg( LOG_ERR, func, "Bad socket type: %s", type ) ;
		return( FAILED ) ;
	}
}


status_e rpc_version_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	struct rpc_data *rdp = SC_RPCDATA( scp ) ;
	char *version = (char *) pset_pointer( values, 0 ) ;
	char *p = strchr( version, '-' ) ;
	char *func = "rpc_version_parser" ;

	if ( p == NULL )
		rdp->rd_min_version = rdp->rd_max_version = atoi( version ) ;
	else
	{
		*p = NUL ;
		rdp->rd_min_version = atoi( version ) ;
		rdp->rd_max_version = atoi( p+1 ) ;
		if ( rdp->rd_min_version > rdp->rd_max_version )
		{
			parsemsg( LOG_ERR, func, "bad version range: %s", version ) ;
			return( FAILED ) ;
		}
	}
	return( OK ) ;
}


status_e rpc_number_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	SC_RPCDATA( scp )->rd_program_number = 
									atoi( (char *) pset_pointer( values, 0 ) ) ;
	return( OK ) ;
}


status_e protocol_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char *proto_name = (char *) pset_pointer( values, 0 ) ;
	struct protoent *pep ;
	char *func = "protocol_parser" ;

	if ( ( pep = getprotobyname( proto_name ) ) == NULL )
	{
		parsemsg( LOG_ERR, func, 
					"Protocol %s not in /etc/protocols", proto_name ) ;
		return( FAILED ) ;
	}

	scp->sc_protocol.name = make_string( 1, proto_name ) ;
	if ( scp->sc_protocol.name == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}
	scp->sc_protocol.value = pep->p_proto ;
	return( OK ) ;
}


status_e wait_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char *val = (char *) pset_pointer( values, 0 ) ;
	char *func = "wait_parser" ;

	if ( EQ( val, "yes" ) )
		scp->sc_wait = YES ;
	else if ( EQ( val, "no" ) )
		scp->sc_wait = NO ;
	else
		parsemsg( LOG_ERR, func, "Bad value for wait: %s", val ) ;
	return( OK ) ;
}


status_e user_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char *user = (char *) pset_pointer( values, 0 ) ;
	struct passwd *pw ;
	char *func = "user_parser" ;

	pw = getpwnam( user ) ;
	if ( pw == NULL )
	{
		parsemsg( LOG_ERR, func, "Unknown user: %s", user ) ;
		return( FAILED ) ;
	}
	
	scp->sc_uid = pw->pw_uid ;
	scp->sc_user_gid = pw->pw_gid ;
	return( OK ) ;
}



status_e group_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char *group = (char *) pset_pointer( values, 0 ) ;
	struct group *grp ;
	char *func = "group_parser" ;

	grp = getgrnam( group ) ;
	if ( grp == NULL )
	{
		parsemsg( LOG_ERR, func, "Unknown group: %s", group ) ;
		return( FAILED ) ;
	}
	
	scp->sc_gid = grp->gr_gid ;
	return( OK ) ;
}



status_e server_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char *server = (char *) pset_pointer( values, 0 ) ;
	char *func = "server_parser" ;

	if ( access( server, X_OK ) == -1 )
	{
		parsemsg( LOG_ERR, func, "Server %s is not executable", server ) ;
		return( FAILED ) ;
	}

	scp->sc_server = make_string( 1, server ) ;
	if ( scp->sc_server == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}



status_e server_args_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	register char **argv ;
	register unsigned u ;
	unsigned i ;
	register unsigned argv_index ;
	register unsigned n_args = pset_count( values ) ;
	char *func = "server_args_parser" ;

	/*
	 * Create the argv for a future exec call
	 * Reserve space for the server. We cannot use scp->sc_server
	 * since it may not have a value yet.
	 */
	argv = argv_alloc( n_args+1 ) ;
	
	for ( u = 0, argv_index = 1 ; u < pset_count( values ) ; u++, argv_index++ )
	{
		register char *s = make_string( 1, (char *) pset_pointer( values, u ) ) ;

		if ( s == NULL )
		{
			for ( i = 1 ; i < argv_index ; i++ )
				free( argv[ i ] ) ;
			free( (char *) argv ) ;
			out_of_memory( func ) ;
			return( FAILED ) ;
		}
		argv[ argv_index ] = s ;
	}

	argv[ argv_index ] = argv[ 0 ] = NULL ;
	scp->sc_server_argv = argv ;
	return( OK ) ;
}

#ifdef BIND_IF
/* 
 * NV - parsing routine for interface IP address 
 */
status_e interface_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char   *interface = (char *) pset_pointer( values, 0 ) ;
	char   *func      = "interface_parser" ;

    /* assign an IP address */
	scp->sc_interface.s_addr = inet_addr( interface );

	/* trap badly formed IP addresses */
	if ( scp->sc_interface.s_addr == -1 )
	{
		parsemsg( LOG_ERR, func, "Badly formed interface IP address: %s", 
                  interface ) ;
		return( FAILED ) ;
	}
}
#endif

status_e instances_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char *instances = (char *) pset_pointer( values, 0 ) ;
	char *func = "instances_parser" ;

	if ( EQ( instances, "UNLIMITED" ) )
		scp->sc_instances = UNLIMITED ;
	else
	{
		scp->sc_instances = atoi( instances ) ;
		if ( scp->sc_instances < 0 )
		{
			parsemsg( LOG_ERR, func,
				"Number of instances is negative: %s", instances ) ;
			return( FAILED ) ;
		}
	}
	return( OK ) ;
}


status_e id_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	char *func = "id_parser" ;

	scp->sc_id = make_string( 1, (char *) pset_pointer( values, 0 ) ) ;
	if ( scp->sc_id != NULL )
		return( OK ) ;
	out_of_memory( func ) ;
	return( FAILED ) ;
}



#define PORT_BITS				16
#define PORT_MAX				( 1 << PORT_BITS )

status_e port_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	unsigned long port = atoi( (char *) pset_pointer( values, 0 ) ) ;
	char *func = "port_parser" ;

	if ( port >= PORT_MAX )
	{
		parsemsg( LOG_ERR, func, "port number exceeds %d", PORT_MAX-1 ) ;
		return( FAILED ) ;
	}
	scp->sc_port = port ;
	return( OK ) ;
}


PRIVATE status_e add_new_string( set, str )
	pset_h set ;
	char *str ;
{
	char *p = make_string( 1, str ) ;
	char *func = "add_new_string" ;

	if ( p == NULL )
	{
		parsemsg( LOG_CRIT, func, ES_NOMEM ) ;
		return( FAILED ) ;
	}
	if ( pset_add( set, p ) == NULL )
	{
		free( p ) ;
		parsemsg( LOG_CRIT, func, ES_NOMEM ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}


status_e env_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	unsigned u ;
	char *func = "env_parser" ;

	if ( op == MINUS_EQ )
	{
		parsemsg( LOG_WARNING, func,
			"operator '-=' not supported for env atribute" ) ;
		return( FAILED ) ;
	}

	NEW_SET( scp->sc_env_var_defs, 5, 5 ) ;

	if ( op == SET_EQ && pset_count( scp->sc_env_var_defs ) > 0 )
	{
		pset_apply( scp->sc_env_var_defs, free, NULL ) ;
		pset_clear( scp->sc_env_var_defs ) ;
	}

	for ( u = 0 ; u < pset_count( values ) ; u++ )
	{
		char *str = (char *) pset_pointer( values, u ) ;

		/*
		 * Check if the string contains an '='
		 */
		if ( strchr( str, '=' ) == NULL )
		{
			parsemsg( LOG_ERR, func, "%s has no '='", str ) ;
			continue ;
		}

		if ( add_new_string( scp->sc_env_var_defs, str ) == FAILED )
			break ;
	}
	return( OK ) ;
}


status_e passenv_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	pset_h var_set ;
	unsigned u ;
	char *func = "passenv_parser" ;

	NEW_SET( scp->sc_pass_env_vars, 0, 0 ) ;

	var_set = scp->sc_pass_env_vars ;

	if ( op == SET_EQ )
	{
		pset_apply( var_set, free, NULL ) ;
		pset_clear( var_set ) ;
		op = PLUS_EQ ;
	}

	for ( u = 0 ; u < pset_count( values ) ; u++ )
	{
		char *env_var = (char *) pset_pointer( values, u ) ;
		unsigned v ;
		boolean_e found ;

		/*
		 * Check if it is already there
		 */
		for ( found = NO, v = 0 ; v < pset_count( var_set ) ; v++ )
			if ( EQ( env_var, (char *) pset_pointer( var_set, v ) ) )
			{
				found = YES ;
				break ;
			}
		
		if ( op == MINUS_EQ && found == NO || op != MINUS_EQ && found == YES )
			continue ;
		
		if ( op == MINUS_EQ )
		{
			free( (char *) pset_pointer( var_set, v ) ) ;
			pset_remove_index( var_set, v ) ;
		}
		else
		{
			if ( env_lookup( std_env, env_var ) == CHAR_NULL )
			{
				parsemsg( LOG_WARNING, func,
					"undefined environment variable: %s", env_var ) ;
				continue ;
			}

			if ( add_new_string( var_set, env_var ) == FAILED )
				return( FAILED ) ;
		}
	}
	return( OK ) ;
}



status_e disabled_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	register unsigned u ;
	char *func = "disabled_parser" ;

	NEW_SET( scp->sc_disabled, pset_count( values ), 0 ) ;
	
	for ( u = 0 ; u < pset_count( values ) ; u++ )
	{
		char *name = (char *) pset_pointer( values, u ) ;

		if ( add_new_string( scp->sc_disabled, name ) == FAILED )
			return( OK ) ;
	}
	return( OK ) ;
}


/*
 * Interpret a number of the form: <num>[m|M|k|K]
 * m and M mean megabytes, k and K mean kilobytes, nothing means bytes
 */
PRIVATE unsigned get_limit( limitstr )
	char *limitstr ;
{
	int multiplier ;

	switch ( limitstr[ strlen( limitstr ) - 1 ] )
	{
		case 'k':
		case 'K':
			multiplier = 1024 ;
			break ;
		
		case 'm':
		case 'M':
			multiplier = 1024 * 1024 ;
			break ;
		
		default:
			multiplier = 1 ;
	}
	return( (unsigned) atoi( limitstr ) * multiplier ) ;
}


PRIVATE status_e parse_filelog( flp, values )
	struct filelog		*flp ;
	pset_h				values ;
{
	unsigned		soft_limit ;
	unsigned		hard_limit ;
	char			*file ;
	unsigned		count = pset_count( values ) ;
	char			*func = "parse_filelog" ;

	if ( count < 2 || count > 4 )
	{
		parsemsg( LOG_ERR, func, "wrong number of arguments" ) ;
		return( FAILED ) ;
	}

	file = make_string( 1, (char *) pset_pointer( values, 1 ) ) ;
	if ( file == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}

	/*
	 * Get the limits, if any
	 */
	if ( count > 2 )
	{
		soft_limit = get_limit( (char *) pset_pointer( values, 2 ) ) ;
		if ( soft_limit == 0 )
		{
			parsemsg( LOG_ERR, func, "soft limit is 0" ) ;
			free( file ) ;
			return( FAILED ) ;
		}

		/*
		 * If a hard limit was specified check that it is at least equal 
		 * to the soft limit. If no hard limit was specified, determine
		 * it from the formula:
		 *		hard = soft + x
		 * where 
		 *		min( 1%soft,LOG_EXTRA_MIN ) <= x <= max( 1%soft,LOG_EXTRA_MAX )
		 */
		if ( count == 4 )
		{
			hard_limit = get_limit( (char *) pset_pointer( values, 3 ) ) ;
			if ( hard_limit < soft_limit )
			{
				parsemsg( LOG_ERR, func,
					"hard limit (%d) is less than soft limit (%d)",
							hard_limit, soft_limit ) ;
				free( file ) ;
				return( FAILED ) ;
			}
		}
		else
		{
			unsigned extra = soft_limit / 100 ;		/* 1% of soft limit */

			if ( extra < LOG_EXTRA_MIN )
				extra = LOG_EXTRA_MIN ;
			else if ( extra > LOG_EXTRA_MAX )
				extra = LOG_EXTRA_MAX ;
			hard_limit = soft_limit + extra ;
		}
		flp->fl_soft_limit = soft_limit ;
		flp->fl_hard_limit = hard_limit ;
	}
	flp->fl_filename = file ;
	return( OK ) ;
}


PRIVATE status_e parse_syslog( slp, values )
	struct syslog	*slp ;
	pset_h			values ;
{
	char					*facility ;
	char					*level ;
	struct name_value *nvp ;
	unsigned				count = pset_count( values ) ;
	char					*func = "parse_syslog" ;

	if ( count < 2 || count > 3 )
	{
		parsemsg( LOG_ERR, func, "wrong number of arguments" ) ;
		return( FAILED ) ;
	}

	facility = (char *) pset_pointer( values, 1 ) ;
	if ( ( nvp = nv_find_value( syslog_facilities, facility ) ) == NULL )
	{
		parsemsg( LOG_ERR, func, "Unknown syslog facility: %s", facility ) ;
		return( FAILED ) ;
	}
	slp->sl_facility = nvp->value ;

	if ( count == 3 )
	{
		level = (char *) pset_pointer( values, 2 ) ;
		if ( ( nvp = nv_find_value( syslog_levels, level ) ) == NULL )
		{
			parsemsg( LOG_ERR, func, "Unknown syslog level: %s", level ) ;
			return( FAILED ) ;
		}
		slp->sl_level = nvp->value ;
	}
	else
		slp->sl_level = DEFAULT_SERVICE_SYSLOG_LEVEL ;

	return( OK ) ;
}


status_e log_type_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	struct log *lp = SC_LOG( scp ) ;
	char *type ;
	char *func = "parse_log_type" ;

	type = (char *) pset_pointer( values, 0 ) ;

	if ( EQ( type, "FILE" ) )
	{
		if ( parse_filelog( log_filelog( lp ), values ) == FAILED )
			return( FAILED ) ;
		lp->l_type = L_FILE ;
	}
	else if ( EQ( type, "SYSLOG" ) )
	{
		if ( parse_syslog( log_syslog( lp ), values ) == FAILED )
			return( FAILED ) ;
		lp->l_type = L_SYSLOG ;
	}
	else
	{
		parsemsg( LOG_ERR, func, "Unknown log type: %s", type ) ;
		return( FAILED ) ;
	}
	return( OK ) ;
}



PRIVATE status_e parse_log_flags( values, op, maskp, options, name )
	pset_h				values ;
	enum assign_op		op ;
	mask_t				*maskp ;
	struct name_value options[] ;
	char					*name ;
{
	if ( op == SET_EQ )
	{
		M_CLEAR_ALL( *maskp ) ;
		op = PLUS_EQ ;
	}

	return( parse_value_list( values, maskp, options, op, name ) ) ;
}


status_e log_on_success_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	return( parse_log_flags( values, op,
		&scp->sc_log_on_success, success_log_options, "log-on-success flag" ) ) ;
}


status_e log_on_failure_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	return( parse_log_flags( values, op,
		&scp->sc_log_on_failure, failure_log_options, "log-on_failure flag" ) ) ;
}



PRIVATE status_e parse_inet_addresses( values, op, addr_list )
	pset_h			values ;
	enum assign_op op ;
	pset_h			*addr_list ;
{
	register unsigned		u ;
	pset_h					addr_set ;
	statfunc					addrlist_func ;
	char						*func = "parse_inet_addresses" ;
	
	NEW_SET( *addr_list, 0, 0 ) ;

	addr_set = *addr_list ;

	/*
	 * If the op was '=' clear the existing list of addresses
	 */
	if ( op == SET_EQ )
	{
		op = PLUS_EQ ;
		addrlist_free( addr_set ) ;
		pset_clear( addr_set ) ;
	}

	addrlist_func = ( op == PLUS_EQ ) ? addrlist_add : addrlist_remove ;

	for ( u = 0 ; u < pset_count( values ) ; u++ )
	{
		register char *str_addr = (char *) pset_pointer( values, u ) ;

		if ( (*addrlist_func)( addr_set, str_addr ) == FAILED )
			break ;
	}
	return( OK ) ;
}



status_e only_from_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	return( parse_inet_addresses( values, op, &scp->sc_only_from ) ) ;
}


status_e no_access_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	return( parse_inet_addresses( values, op, &scp->sc_no_access ) ) ;
}


status_e access_times_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	register unsigned u ;
	char *func = "access_times_parser" ;
	status_e ti_add() ;

	NEW_SET( scp->sc_access_times, 0, 0 ) ;

	for ( u = 0 ; u < pset_count( values ) ; u++ )
	{
		register char *interval = (char *) pset_pointer( values, u ) ;

		if ( ti_add( scp->sc_access_times, interval ) == FAILED )
			break ;
	}
	return( OK ) ;
}


status_e nice_parser( values, scp, op )
	pset_h values ;
	struct service_config *scp ;
	enum assign_op op ;
{
	scp->sc_nice = atoi( (char *) pset_pointer( values, 0 ) ) ;
	return( OK ) ;
}

