/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: retry.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/time.h>
#include <syslog.h>

#include "pset.h"

#include "state.h"
#include "access.h"
#include "config.h"
#include "server.h"
#include "connection.h"
#include "flags.h"

void msg() ;
void out_of_memory() ;

static bool_int retry_timer_running ;

PRIVATE void stop_retry_timer() ;
PRIVATE void start_retry_timer() ;


/*
 * Attempt to start all servers in the retry table
 */
void server_retry()
{
	unsigned				servers_started = 0 ;
	register unsigned u ;
	char					*func = "server_retry" ;
	PRIVATE void		cancel_retry() ;

	for ( u = 0 ; u < pset_count( RETRIES( ps ) ) ; u++ )
	{
		register struct server *retry = SERP( 
													pset_pointer( RETRIES( ps ), u ) ) ;
		register struct service *sp = SERVER_SERVICE( retry ) ;
		connection_s *cp = SERVER_CONNECTION( retry ) ;

		/*
		 * Drop the retry if access control fails or we have
		 * a memory allocation problem
		 */
		if ( svc_access_control( sp, cp ) == FAILED ||
					  pset_add( SERVERS( ps ), retry ) == NULL )
		{
			cancel_retry( retry ) ;
			pset_pointer( RETRIES( ps ), u ) = NULL ;
			continue ;
		}

		if ( server_start( retry ) == OK )
		{
			servers_started++ ;
			svc_dec_retries( sp ) ;
			conn_close( cp ) ;
			pset_pointer( RETRIES( ps ), u ) = NULL ;
			continue ;
		}
		else
		{
			pset_remove( SERVERS( ps ), retry ) ;
			if ( SERVER_FORKLIMIT( retry ) )
			{
				/*
				 * give up retrying
				 */
				msg( LOG_ERR, func,
					"service %s: too many consecutive fork failures", SVC_ID(sp) ) ;
				svc_log_failure( sp, cp, AC_FORK ) ;
				cancel_retry( retry ) ;
				pset_pointer( RETRIES( ps ), u ) = NULL ;
				continue ;
			}
			else
			{
				if ( debug.on )
					msg( LOG_DEBUG, func,
						"fork failed for service %s. Retrying...", SVC_ID( sp ) ) ;
			}
		}
	}

	pset_compact( RETRIES( ps ) ) ;

	if ( debug.on )
		msg( LOG_DEBUG, func,
			"%d servers started, %d left to retry",
				servers_started, pset_count( RETRIES( ps ) ) ) ;

	if ( pset_count( RETRIES( ps ) ) == 0 )
		stop_retry_timer() ;
}


/*
 * Schedule a retry by inserting the struct server in the retry table
 * and starting the timer if necessary
 */
status_e schedule_retry( serp )
	struct server *serp ;
{
	struct service *sp = SERVER_SERVICE( serp ) ;
	char *func = "schedule_retry" ;

	if ( pset_add( RETRIES( ps ), serp ) == NULL )
	{
		out_of_memory( func ) ;
		return( FAILED ) ;
	}
	svc_inc_retries( sp ) ;
	start_retry_timer() ;
	if ( debug.on )
		msg( LOG_DEBUG, func, "Scheduled retry attempt for %s", SVC_ID( sp ) ) ;
	return( OK ) ;
}


/*
 * This function should not be called for servers that correspond to
 * services not in the service table because server_release will result
 * in releasing all memory associated with the service (since the ref
 * count will drop to 0).
 */
PRIVATE void cancel_retry( serp )
	struct server *serp ;
{
	struct service *sp = SERVER_SERVICE( serp ) ;

	conn_free( SERVER_CONNECTION( serp ) ) ;
	svc_dec_retries( sp ) ;
	server_release( serp ) ;
}



/*
 * Cancel all retry attempts for the specified service
 */
void cancel_service_retries( sp )
   register struct service *sp ;
{
   register unsigned u ;
   char *func = "cancel_service_retries" ;

	if ( SVC_RETRIES( sp ) == 0 )
      return ;

	u = 0 ;
	while ( u < pset_count( RETRIES( ps ) ) )
	{
		register struct server *serp ;

		serp = SERP( pset_pointer( RETRIES( ps ), u ) ) ;
		if ( SERVER_SERVICE( serp ) == sp )
		{
			msg( LOG_NOTICE, func,
            "dropping retry attempt for service %s", SVC_ID( sp ) ) ;
			cancel_retry( serp ) ;
			pset_remove_index( RETRIES( ps ), u ) ;
			continue ;
		}
		u++ ;
   }

	if ( pset_count( RETRIES( ps ) ) == 0 )
		stop_retry_timer() ;
}


#ifndef NO_TIMERS

#include "timer.h"

static timer_h retry_timer ;

status_e create_retry_timer()
{
	retry_timer = timer_create( TIMER_REAL, TIMER_RETURN_ERROR, INT_NULL ) ;
	return( ( retry_timer == NULL ) ? FAILED : OK ) ;
}


PRIVATE void retry_action( tp, arg )
	timer_h tp ;
	void *arg ;
{
#ifdef lint
	tp = tp ;
	arg = arg ;
#endif
	M_SET( ps.flags, RETRY_FLAG ) ;
}

#endif		/* ! NO_TIMERS */


PRIVATE void start_retry_timer()
{
	/*
	 * The retry itimerval is set so that the timer expires every 
	 * RETRY_INTERVAL seconds when it is enabled.
	 */
	static struct itimerval itv =
		{
			{ RETRY_INTERVAL, 0 },
			{ RETRY_INTERVAL, 0 }
		} ;
	char *func = "start_retry_timer" ;

	/*
	 * Enable timer if necessary.
	 */

#ifndef NO_TIMERS
	if ( ! retry_timer_running )
	{
		struct timer_action ta ;

		ta.ta_flags = TIMER_NOFLAGS ;
		ta.ta_func = retry_action ;
		ta.ta_arg = VOID_NULL ;
		if ( timer_start( retry_timer, &itv, TIMER_RELATIVE, &ta ) == TIMER_ERR )
		{
			msg( LOG_ERR, func, "failed to start retry timer" ) ;
			return ;
		}
		retry_timer_running = TRUE ;
	}
#else
	if ( ! retry_timer_running )
		if ( setitimer( ITIMER_REAL, &itv, ITIMERVAL_NULL ) == -1 )
			msg( LOG_ERR, func, "setitimer: %m" ) ;
		else
			retry_timer_running = TRUE ;
#endif
}


PRIVATE void stop_retry_timer()
{
	if ( retry_timer_running )
	{
#ifndef NO_TIMERS
		timer_stop( retry_timer ) ;
#else
		(void) setitimer( ITIMER_REAL, ITIMERVAL_NULL, ITIMERVAL_NULL ) ;
#endif
		retry_timer_running = FALSE ;
	}
}

