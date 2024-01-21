/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: conf.c,v 1.3 1995/09/10 18:41:13 chuck Exp $" ;

#include <sys/types.h>
#if defined(BSD) || defined(linux)
#include <netinet/in.h>
#endif
#include <syslog.h>
#include <fcntl.h>
#include <string.h>
#include <netdb.h>

#include "pset.h"
#include "misc.h"
#include "xlog.h"

#include "attr.h"
#include "config.h"
#include "conf.h"
#include "defs.h"
#include "service.h"
#include "state.h"

void msg() ;
void out_of_memory() ;


void cnf_free( confp )
	struct configuration *confp ;
{
	register unsigned u ;
	pset_h sconfs = confp->cnf_service_confs ;

	for ( u = 0 ; u < pset_count( sconfs ) ; u++ )
		sc_free( SCP( pset_pointer( sconfs, u ) ) ) ;
	pset_destroy( sconfs ) ;
	if ( confp->cnf_defaults )
		sc_free( confp->cnf_defaults ) ;
}


/*
 * Extract from 'confp' the service that matches 'scp'
 */
struct service_config *cnf_extract( confp, scp )
	struct configuration *confp ;
	struct service_config *scp ;
{
	pset_h stab = confp->cnf_service_confs ;
   register unsigned u ;

   for ( u = 0 ; u < pset_count( stab ) ; u++ )
   {
      struct service_config *iscp = SCP( pset_pointer( stab, u ) ) ;

      if ( ! EQ( scp->sc_id, iscp->sc_id ) || sc_different_confs( scp, iscp ) )
         continue ;

		pset_remove_index( stab, u ) ;
		return( iscp ) ;
   }
   return( NULL ) ;
}



void cnf_dump( confp, fd )
	struct configuration *confp ;
	int fd ;
{
	pset_h stab = confp->cnf_service_confs ;
	register unsigned u ;
	void tabprint() ;

	sc_dump( confp->cnf_defaults, fd, 0, TRUE ) ;
	tabprint( fd, 0, "\n" ) ;

	for ( u = 0 ; u < pset_count( stab ) ; u++ )
	{
		sc_dump( SCP( pset_pointer( stab, u ) ), fd, 0, FALSE ) ;
		Sputchar( fd, '\n' ) ;
	}
}


status_e cnf_init( confp, fdp, iterp )
	struct configuration *confp ;
	int *fdp ;
	psi_h *iterp ;
{
	int fd ;
	pset_h pset ;
	psi_h pset_iter ;
	struct service_config *scp ;
	char *func = "cnf_init" ;

	/*
	 * Open configuration file
	 */
   fd = open( ps.ros.config_file, O_RDONLY ) ;

   if ( fd == -1 )
   {
      msg( LOG_ERR, func, "open( %s ) failed: %m", ps.ros.config_file ) ;
		return( FAILED ) ;
   }

	if ( ( pset = pset_create( 0, 0 ) ) == NULL )
	{
		msg( LOG_CRIT, func, "can't create service table" ) ;
		(void) close( fd ) ;
		return( FAILED ) ;
	}

	if ( ( scp = sc_alloc( CHAR_NULL ) ) == NULL )
	{
		msg( LOG_ERR, func, "can't allocate defaults service" ) ;
		pset_destroy( pset ) ;
		(void) close( fd ) ;
		return( FAILED ) ;
	}

	if ( ( pset_iter = psi_create( pset ) ) == NULL )
	{
		msg( LOG_ERR, func, "can't create service table iterator" ) ;
		sc_free( scp ) ;
		pset_destroy( pset ) ;
		(void) close( fd ) ;
		return( FAILED ) ;
	}

	*fdp = fd ;
	confp->cnf_service_confs = pset ;
	confp->cnf_defaults = scp ;
	*iterp = pset_iter ;
	return( OK ) ;
}


PRIVATE void destroy_service( sp )
	struct service *sp ;
{
	svc_deactivate( sp ) ;
	svc_free( sp ) ;
}


/*
 * Try to start all services.
 * Return the # of services started.
 */
unsigned cnf_start_services( confp )
	struct configuration *confp ;
{
	pset_h sconfs = confp->cnf_service_confs ;
	unsigned services_started = 0 ;
	register unsigned u ;
	char *func = "cnf_start_services" ;

	for ( u = 0 ; u < pset_count( sconfs ) ; u++ )
	{
		register struct service_config *scp = SCP( pset_pointer( sconfs, u ) ) ;
		register struct service *sp ;

		if ( ( sp = svc_new( scp ) ) == NULL )
		{
			sc_free( scp ) ;
			continue ;
		}

		if ( svc_activate( sp ) == FAILED )
		{
			svc_free( sp ) ;
			continue ;
		}

		/*
		 * descriptors_free can be negative without a descriptor-allocating
		 * system call failing because some of the descriptors we reserve
		 * are transient
		 */
		if ( ps.rws.descriptors_free < 0 )
		{
			msg( LOG_ERR, func,
				"Service %s disabled because of lack of file descriptors",
					SVC_ID( sp ) ) ;
			destroy_service( sp ) ;
			continue ;
		}

		/*
		 * Activation successful; add service to service table
		 */
		if ( pset_add( SERVICES( ps ), sp ) == NULL )
		{
			out_of_memory( func ) ;
			destroy_service( sp ) ;
			break ;
		}

		SVC_HOLD( sp ) ;

		services_started++ ;

		if ( debug.on )
			msg( LOG_DEBUG, func, "Started service: %s", SVC_ID( sp ) ) ;
	}

	/*
	 * All the configurations have been linked to their services
	 * so we don't need to hold references to them in the pset.
	 * We need to clear the pset so that the cnf_free will not free the memory.
	 */
	pset_clear( sconfs ) ;

	if ( debug.on )
		msg( LOG_DEBUG, func, "mask_max = %d, services_started = %d",
				ps.rws.mask_max, services_started ) ;
			
	return( services_started ) ;
}



