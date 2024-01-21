/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: access.c,v 1.2 1995/09/10 15:42:38 chuck Exp $" ;

#include <syslog.h>
#include <time.h>

#include "connection.h"
#ifdef BSD
#include "service.h"
#endif
#include "state.h"
#include "addr.h"
#include "access.h"

char *inet_ntoa() ;
time_t time() ;

void msg() ;

struct name_value access_code_names[] =
	{
		{ "address",				(int) AC_ADDRESS			},
		{ "time",					(int) AC_TIME				},
		{ "fork",					(int) AC_FORK				},
		{ "service_limit",		(int) AC_SERVICE_LIMIT	},
		{ "process_limit",		(int) AC_PROCESS_LIMIT	},
		{ CHAR_NULL,				1								},
		{ "UNKNOWN",				0								}
	} ;


/*
 * Returns OK if the IP address in sinp is acceptable to the access control
 * lists of the specified service.
 */
PRIVATE status_e remote_address_check( sp, sinp )
	register struct service		*sp ;
	struct sockaddr_in			*sinp ;
{
	/*
	 * of means only_from, na means no_access
	 */
	unsigned long			of_match ;
	unsigned long			na_match ;
	register bool_int		of_matched ;
	register bool_int		na_matched ;
	struct in_addr			*addr ;

	addr = &sinp->sin_addr ;

	if ( sp->svc_no_access != NULL )
		na_matched = addrlist_match( sp->svc_no_access, addr, &na_match ) ;
	else
		na_matched = FALSE ;

	if ( sp->svc_only_from != NULL )
		of_matched = addrlist_match( sp->svc_only_from, addr, &of_match ) ;
	else
		of_matched = FALSE ;

	/*
	 * Check if the specified address is in both lists
	 */
	if ( na_matched && of_matched )
	{
		/*
		 * The greater match wins.
		 * If the matches are equal, this is an error in the service entry
		 * and we cannot allow a server to start.
		 * We do not disable the service entry (not our job).
		 */
		if ( na_match == of_match )
			msg( LOG_ERR, "remote_address_check",
"Service=%s: only_from list and no_access list match equally the address %s",
				SVC_ID( sp ), inet_ntoa( sinp->sin_addr ) ) ;
		return( ( of_match > na_match ) ? OK : FAILED ) ;
	}

	if ( sp->svc_no_access != NULL && na_matched )
		return( FAILED ) ;
	if ( sp->svc_only_from != NULL && ! of_matched )
		return( FAILED ) ;

	/*
	 * If no lists were specified, the default is to allow starting a server
	 */
	return( OK ) ;
}


/*
 * mp is the mask pointer, t is the check type
 */
#define CHECK( mp, t )		( ( (mp) == NULL ) || M_IS_SET( *(mp), t ) )

/*
 * Perform the access controls specified by check_mask.
 * If check_mask is NULL, perform all access controls
 */
access_e access_control( sp, cp, check_mask )
	register struct service		*sp ;
	register connection_s		*cp ;
	register mask_t				*check_mask ;
{
	register struct service_config	*scp = SVC_CONF( sp ) ;
	struct sockaddr_in					*sinp ;

	if ( CHECK( check_mask, CF_ADDRESS ) && 
				( sinp = conn_address( cp ) ) != SOCKADDRIN_NULL &&
				remote_address_check( sp, sinp ) == FAILED )
		return( AC_ADDRESS ) ;

	if ( CHECK( check_mask, CF_TIME ) &&
			SC_ACCESS_TIMES( scp ) != NULL && 
				! ti_current_time_check( SC_ACCESS_TIMES( scp ) ) )
		return( AC_TIME ) ;
	
	if ( CHECK( check_mask, CF_SERVICE_LIMIT ) &&
								SVC_RUNNING_SERVERS( sp ) >= SC_INSTANCES( scp ) )
		return( AC_SERVICE_LIMIT ) ;
	
	if ( CHECK( check_mask, CF_PROCESS_LIMIT ) && ps.ros.process_limit )
	{
		unsigned processes_to_create = SC_IS_INTERCEPTED( scp ) ? 2 : 1 ;

		if ( pset_count( SERVERS( ps ) ) + processes_to_create >
																ps.ros.process_limit )
		return( AC_PROCESS_LIMIT ) ;
	}
	return( AC_OK ) ;
}

