/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id" ;

#include <sys/types.h>
#include <netdb.h>
#include <string.h>
#include <syslog.h>
#include <memory.h>

#include "misc.h"
#include "str.h"
#include "pset.h"
#include "sio.h"

#include "defs.h"
#include "sconf.h"
#include "conf.h"
#include "attr.h"
#include "parse.h"
#include "addr.h"

char *malloc() ;

void parsemsg() ;
void msg() ;
void out_of_memory() ;

char *next_line() ;
int line_has_only_1_char() ;
void skip_entry() ;

/*
 * Parser functions
 */
status_e service_parser() ;
status_e socket_type_parser() ;
status_e protocol_parser() ;
status_e wait_parser() ;
status_e user_parser() ;
status_e group_parser() ;
status_e server_parser() ;
status_e server_args_parser() ;
status_e instances_parser() ;
status_e log_on_success_parser() ;
status_e log_on_failure_parser() ;
status_e log_type_parser() ;
status_e only_from_parser() ;
status_e no_access_parser() ;
status_e access_times_parser() ;
status_e type_parser() ;
status_e id_parser() ;
status_e env_parser() ;
status_e port_parser() ;
status_e rpc_version_parser() ;
status_e passenv_parser() ;
status_e flags_parser() ;
status_e disabled_parser() ;
status_e rpc_number_parser() ;
status_e nice_parser() ;
#ifdef BIND_IF
status_e interface_parser(); /* NV */
#endif

/*
 * A NULL value for the name field marks the end of the table
 *
 * The 3rd value is the number of attribute values.
 * If the number is positive, exactly that many values must be specified.
 * If the number is -1, 0 or more values may be specified.
 * If the number is -2, 0 or more values may be specified and the operators
 * '+=' and '-=' may be used.
 */
static struct attribute service_attributes[] =
	{
		{ "socket_type", 		A_SOCKET_TYPE,	 1,	socket_type_parser 		},
		{ "protocol",			A_PROTOCOL,		 1,	protocol_parser			},
		{ "wait",				A_WAIT,			 1,	wait_parser				},
		{ "user", 				A_USER, 		 1,	user_parser				},
		{ "group", 				A_GROUP,		 1,	group_parser 			},
		{ "server",				A_SERVER,		 1,	server_parser			},
		{ "server_args",		A_SERVER_ARGS,	-1,	server_args_parser		},
		{ "instances",			A_INSTANCES,	 1,	instances_parser		},
		{ "log_on_success",	A_LOG_ON_SUCCESS,	-2,	log_on_success_parser	},
		{ "log_on_failure",	A_LOG_ON_FAILURE,	-2,	log_on_failure_parser	},
		{ "log_type",			A_LOG_TYPE,		-1,	log_type_parser			},
		{ "only_from",			A_ONLY_FROM,	-2,	only_from_parser	},
		{ "no_access",			A_NO_ACCESS,	-2,	no_access_parser	},
		{ "access_times",		A_ACCESS_TIMES,	-1,	access_times_parser	},
		{ "type",				A_TYPE,			-1,	type_parser			},
#ifndef NO_RPC
		{ "rpc_version",		A_RPC_VERSION,	 1,	rpc_version_parser	},
		{ "rpc_number",			A_RPC_NUMBER,	 1,	rpc_number_parser	},
#endif
		{ "id",					A_ID,			 1,	id_parser			},
		{ "env",					A_ENV,		-2,	env_parser			},
		{ "port",				A_PORT,			 1,	port_parser			},
		{ "passenv",			A_PASSENV,		-2,	passenv_parser		},
		{ "flags",				A_FLAGS,		-1,	flags_parser		},
		{ "nice",				A_NICE,			 1,	nice_parser			},
#ifdef BIND_IF
		{ "interface",			A_INTERFACE,	 1,	interface_parser	},
#endif
		{ NULL,					A_NONE,			-1,	NULL				}
	} ;

static struct attribute default_attributes[] =
   {
      { "log_type",        A_LOG_TYPE,       -2,   log_type_parser       },
      { "log_on_success",  A_LOG_ON_SUCCESS, -2,   log_on_success_parser },
      { "log_on_failure",  A_LOG_ON_FAILURE, -2,   log_on_failure_parser },
      { "disabled",        A_DISABLED,       -2,   disabled_parser       },
      { "no_access",       A_NO_ACCESS,      -2,   no_access_parser      },
      { "only_from",       A_ONLY_FROM,      -2,   only_from_parser      },
      { "instances",       A_INSTANCES,       1,   instances_parser      },
      { "passenv",         A_PASSENV,        -2,   passenv_parser        },
#ifdef BIND_IF
      { "interface",       A_INTERFACE,       1,   interface_parser      },
#endif
      { NULL,              A_NONE,            0,   NULL                  }
   } ;


#define MODIFIABLE( ap )				( (ap)->a_nvalues == -2 )
#define VAR_VALUES( ap )				( (ap)->a_nvalues < 0 )
#define FIXED_VALUES( ap )				( (ap)->a_nvalues > 0 )

int line_count ;

PRIVATE void		get_service_entry() ;
PRIVATE entry_e	find_next_entry() ;
PRIVATE status_e 	parse_entry() ;


/*
 * Given the id, return the name (only the service attributes are searched)
 */
char *attr_name_lookup( id )
	register int id ;
{
	register struct attribute *ap ;

	for ( ap = &service_attributes[ 0 ] ; ap->a_name ; ap++ )
		if ( id == ap->a_id )
			return( ap->a_name ) ;
	return( CHAR_NULL ) ;
}


void parse_end()
{
	endprotoent() ;
	endpwent() ;
	endgrent() ;
	endnetent() ;
	endhostent() ;
}


/*
 * Parsing rules and rationale
 *
 * The parse_conf_file function parses a configuration file identified
 * by a file descriptor and fills the service table and defaults of
 * the configuration argument.
 *
 * The configuration information for a service comes from 2 sources: the
 * service entry and, possibly, the defaults entry.
 * Attributes specified in the defaults entry can be overriden or
 * modified by the service entry. Modifiable attributes can be identified
 * by the value -2 for the 'a_nvalues' field of the struct attribute. Those
 * attributes with a different value for 'a_nvalues' are overridable ones.
 * The modifiable attributes are filled in only if the entry tries to modify
 * them.
 */

/*
 * Read the configuration file (descriptor fd) and place all
 * services found there in the configuration.
 */
void parse_conf_file( fd, confp )
	int fd ;
	struct configuration *confp ;
{
	pset_h						sconfs				= CNF_SERVICE_CONFS( confp ) ;
	struct service_config	*default_config	= CNF_DEFAULTS( confp ) ;
	boolean_e					found_defaults		= NO ;
	struct service_config	default_default_config ;
	char							*func					= "parse_conf_file" ;

	line_count = 0 ;
	CLEAR( default_default_config ) ;

	for ( ;; )
	{
		entry_e	entry_type ;
		char		*service_name ;

		/*
		 * if find_next_entry is successful, service_name
		 * will point to malloc'ed memory
		 */
		entry_type = find_next_entry( fd, &service_name ) ;

		switch ( entry_type )
		{
			case SERVICE_ENTRY:
				get_service_entry( fd, sconfs, service_name, default_config ) ;
				break ;

			case DEFAULTS_ENTRY:
				if ( found_defaults == YES )
				{
					parsemsg( LOG_ERR, func,
				 "only 1 defaults entry is allowed. This entry will be ignored" ) ;
					skip_entry( fd ) ;
				}
				else if ( parse_entry( DEFAULTS_ENTRY, fd,
										default_config, &default_default_config ) == OK )
					found_defaults = YES ;
				break ;
			
			case BAD_ENTRY:
				skip_entry( fd ) ;
				break ;

			case NO_ENTRY:
				return ;
		}
	}
}



/*
 * Find the next service entry.
 * Look for a line of the form:
 *
 *		<white-space> service <white-space> <service_name>
 *
 * followed by a line containing only the ENTRY_BEGIN character
 */
PRIVATE entry_e find_next_entry( fd, snamep )
	int	fd ;
	char	**snamep ;						/* service name pointer */
{
	register char	*p ;
	str_h				strp ;
	char				*sname ;
	entry_e			entry_type ;
	register char	*line = next_line( fd ) ;
	char				*func = "find_next_entry" ;

	if ( line == CHAR_NULL )
		return( NO_ENTRY ) ;
	
	strp = str_parse( line, " \t", STR_RETURN_ERROR, INT_NULL ) ;
	if ( strp == NULL )
	{
		parsemsg( LOG_CRIT, func, "str_parse failed" ) ;
		return( BAD_ENTRY ) ;
	}

	if ( ( p = str_component( strp ) ) == CHAR_NULL )
	{
		/*
		 * This shouldn't happen since it implies that there is a bug
		 * in next_line
		 */
		parsemsg( LOG_WARNING, func, "empty line" ) ;
		str_endparse( strp ) ;
		return( BAD_ENTRY ) ;
	}

	/*
	 * Look for a keyword
	 */
	if ( EQ( p, KW_SERVICE ) )
	{
		/*
		 * Now get the service name
		 */
		if ( ( p = str_component( strp ) ) == CHAR_NULL )
		{
			parsemsg( LOG_ERR, func, "service name missing" ) ;
			str_endparse( strp ) ;
			return( BAD_ENTRY ) ;
		}
	
		sname = make_string( 1, p ) ;
		if ( sname == CHAR_NULL )
		{
			out_of_memory( func ) ;
			str_endparse( strp ) ;
			return( BAD_ENTRY ) ;
		}
		str_endparse( strp ) ;
		entry_type = SERVICE_ENTRY ;
	}
	else if ( EQ( p, KW_DEFAULTS ) )
	{
		str_endparse( strp ) ;
		entry_type = DEFAULTS_ENTRY ;
	}
	else
	{
		parsemsg( LOG_ERR, func, "missing service keyword" ) ;
		str_endparse( strp ) ;
		return( BAD_ENTRY ) ;
	}

	/*
	 * Now look for ENTRY_BEGIN
	 */
	line = next_line( fd ) ;
	if ( line == NULL || ! line_has_only_1_char( line, ENTRY_BEGIN ) )
	{
		parsemsg( LOG_ERR, func,
			"Service %s: missing '%c'", sname, ENTRY_BEGIN ) ;
		if ( entry_type == SERVICE_ENTRY )
			free( sname ) ;
		return( BAD_ENTRY ) ;
	}
	*snamep = sname ;
	return( entry_type ) ;
}



/*
 * Get a service entry. Steps:
 *
 *		1. Parse entry attributes
 *		2. Determine service id
 *		3. Insert entry in table
 */
PRIVATE void get_service_entry( fd, sconfs, name, defaults )
	int							fd ;
	pset_h						sconfs ;
	char							*name ;
	struct service_config	*defaults ;
{
	register struct service_config	*scp ;
	unsigned									u ;
	char										*func = "get_service_entry" ;

	scp = sc_alloc( name ) ;
	if ( scp == NULL )
	{
		free( name ) ;
		skip_entry( fd ) ;
		return ;
	}

	if ( parse_entry( SERVICE_ENTRY, fd, scp, defaults ) == FAILED )
	{
		sc_free( scp ) ;
		skip_entry( fd ) ;
		return ;
	}

   /*
    * If no service id was specified, set it equal to the service name
    */
   if ( ! SC_SPECIFIED( scp, A_ID ) )
      if ( scp->sc_id = make_string( 1, scp->sc_name ) )
         SC_PRESENT( scp, A_ID ) ;
      else
      {
			out_of_memory( func ) ;
         sc_free( scp ) ;
         return ;
      }

	/*
	 * Make sure the service id is unique
	 */
	for ( u = 0 ; u < pset_count( sconfs ) ; u++ )
		if ( EQ( SCP( pset_pointer( sconfs, u ) )->sc_id, scp->sc_id ) )
		{
         parsemsg( LOG_ERR, func, "id not unique: %s", scp->sc_id ) ;
			sc_free( scp ) ;
			return ;
		}

	if ( ! pset_add( sconfs, scp ) )
	{
		out_of_memory( func ) ;
		sc_free( scp ) ;
		return ;
	}
}




/*
 * Fill in scp the value of the modifiable attribute attr from def.
 * These modifiable attributes are:
 *		log_on_{success,failure}
 *		only_from
 *		no_access
 *		passenv
 */
PRIVATE void fill_attribute( attr_id, scp, def )
	unsigned						attr_id ;
	struct service_config	*scp ;
	struct service_config	*def ;
{
	status_e copy_pset() ;

	switch ( attr_id )
	{
		case A_LOG_ON_SUCCESS:
			M_ASSIGN( scp->sc_log_on_success, def->sc_log_on_success ) ;
			SC_PRESENT( scp, A_LOG_ON_SUCCESS ) ;
			break ;

		case A_LOG_ON_FAILURE:
			M_ASSIGN( scp->sc_log_on_failure, def->sc_log_on_failure ) ;
			SC_PRESENT( scp, A_LOG_ON_FAILURE ) ;
			break ;

		case A_ONLY_FROM:
			if ( addrlist_copy( def->sc_only_from, &scp->sc_only_from ) == OK )
				SC_PRESENT( scp, A_ONLY_FROM ) ;
			break ;

		case A_NO_ACCESS:
			if ( addrlist_copy( def->sc_no_access, &scp->sc_no_access ) == OK )
				SC_PRESENT( scp, A_NO_ACCESS ) ;
			break ;
		
		case A_PASSENV:
			if ( copy_pset( def->sc_pass_env_vars,
									&scp->sc_pass_env_vars, 0 ) == OK )
				SC_PRESENT( scp, A_PASSENV ) ;
			break ;
#ifdef BIND_IF
		case A_INTERFACE:
			def->sc_interface.s_addr =  scp->sc_interface.s_addr ;
			SC_PRESENT( scp, A_INTERFACE ) ;
			break ;
#endif
	}
}



/*
 * Find the attribute with the specified name
 */
PRIVATE struct attribute *attr_lookup( attr_array, attr_name )
	struct attribute attr_array[] ;
	char *attr_name ;
{
	register struct attribute *ap ;
	char *func = "attr_lookup" ;

	for ( ap = &attr_array[ 0 ] ; ap->a_name ; ap++ )
		if ( EQ( attr_name, ap->a_name ) )
			return( ap ) ;
	parsemsg( LOG_WARNING, func, "bad attribute: %s", attr_name ) ;
	return( NULL ) ;
}


/*
 * Identify the attribute in <attr_name>.
 *
 * Check if
 *		1) the attribute has been defined already
 *		2) the value count is correct
 *		3) the assign op is appropriate
 *
 * Invoke appropriate parser
 */
PRIVATE void identify_attribute( entry_type, scp, defaults,
								 attr_name, op, attr_values )
	entry_e					entry_type ;
	struct service_config	*scp ;
	struct service_config	*defaults ;
	register char			*attr_name ;
	enum assign_op			op ;
	pset_h					attr_values ;
{
	register struct attribute	*ap ;
	char						*func = "identify_attribute" ;


	if ( entry_type == SERVICE_ENTRY )
		ap = attr_lookup( service_attributes, attr_name ) ;
	else
		ap = attr_lookup( default_attributes, attr_name ) ;
	
	if ( ap == NULL )
		return ;

	if ( ! MODIFIABLE( ap ) )
	{

		if ( SC_SPECIFIED( scp, ap->a_id ) )
		{
			parsemsg( LOG_WARNING, func, "Service %s: attribute already set: %s",
						scp->sc_name, attr_name ) ;
			return ;
		}

		if ( op != SET_EQ )
		{
			parsemsg( LOG_WARNING, func,
				"Service %s: operator '%s' cannot be used for attribute '%s'",
					scp->sc_name, ( op == PLUS_EQ ) ? "+=" : "-=", attr_name ) ;
			return ;
		}
	}
	else		/* modifiable attribute */
	{
		/*
		 * For the defaults entry, '=' and '+=' have the same meaning
		 */
		if ( entry_type == DEFAULTS_ENTRY && op == SET_EQ )
			op = PLUS_EQ ;

		/*
		 * If this is the first time we see this attribute, and a default
		 * for it is available, copy that default.
		 */
		if ( ! SC_IS_PRESENT( scp, ap->a_id ) &&
											SC_SPECIFIED( defaults, ap->a_id ) )
			fill_attribute( ap->a_id, scp, defaults ) ;
	}

	if ( FIXED_VALUES( ap ) && ap->a_nvalues != pset_count( attr_values ) )
	{
		parsemsg( LOG_WARNING, func,
			"attribute %s expects %d values and %d values were specified",
			attr_name, ap->a_nvalues, pset_count( attr_values ) ) ;
		return ;
	}


	if ( (*ap->a_parser)( attr_values, scp, op ) == OK )
		SC_SPECIFY( scp, ap->a_id ) ;

}


/*
 * Read the entry line-by-line and add the information in scp
 * Use defaults to initialize modifiable entry fields.
 */
PRIVATE status_e parse_entry( entry_type, fd, scp, defaults )
	entry_e						entry_type ;
	int							fd ;
	struct service_config	*scp ;
	struct service_config	*defaults ;
{
   static pset_h		attr_values ;
   register char		*line ;
   char					*attr_name ;
   enum assign_op		op ;
   void					identify_attribute() ;
   status_e				parse_line() ;
   char					*func = "get_attributes" ;

	if ( ! attr_values && ( attr_values = pset_create( 10, 10 ) ) == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}

   for ( ;; )
   {
      line = next_line( fd ) ;
      if ( line == CHAR_NULL )
      {
         parsemsg( LOG_ERR, func, "incomplete entry" ) ;
         return( FAILED ) ;
      }

      if ( line_has_only_1_char( line, ENTRY_END ) )
         return( OK ) ;

      if ( parse_line( line, &attr_name, &op, attr_values ) == FAILED )
      {
         pset_clear( attr_values ) ;
         return( FAILED ) ;
      }

		identify_attribute( entry_type,
					scp, defaults, attr_name, op, attr_values ) ;
      pset_clear( attr_values ) ;
   }
}

