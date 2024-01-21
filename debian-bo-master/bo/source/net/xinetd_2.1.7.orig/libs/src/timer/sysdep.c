/*
 * (c) Copyright 1993 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "defs.h"
#include "impl.h"
#include "ostimer.h"
#include "timemacros.h"


PRIVATE void sigalrm_handler() ;
PRIVATE void sigvtalrm_handler() ;
PRIVATE void sigprof_handler() ;

PRIVATE void get_real_time() ;
PRIVATE void get_virtual_time() ;
PRIVATE void get_prof_time() ;

#define SIGNOSIG							0

static struct os_timer os_timers[] =
   {
#ifdef ITIMER_REAL
		{ AVAILABLE,	 ITIMER_REAL,
#else
		{ UNAVAILABLE,	0,
#endif
			TIMER_REAL,			SIGALRM,    sigalrm_handler,     get_real_time     },

#ifdef ITIMER_VIRTUAL
		{ AVAILABLE,	ITIMER_VIRTUAL,
#else
		{ UNAVAILABLE,	0,
#endif
        TIMER_VIRTUAL,		SIGVTALRM,  sigvtalrm_handler,   get_virtual_time  },

#ifdef ITIMER_PROF
		{ AVAILABLE,	ITIMER_PROF,
#else
		{ UNAVAILABLE,	0,
#endif
        TIMER_PROF,     	SIGPROF,    sigprof_handler,     get_prof_time     },

		{ UNAVAILABLE,	0,
        TIMER_REAL,			SIGNOSIG,	NULL,                NULL,             }
   } ;


/*
 * The timer_block_mask blocks all timer signals when a timer signal
 * happens (using the sa_mask field of struct sigaction). This is necessary
 * when the user function associated with the timer does not return.
 * Consider the following scenario:
 *    The user creates 2 timers, one TIMER_REAL (signal: SIGALRM), one
 *    TIMER_VIRTUAL (signal: SIGVTALRM).
 *    SIGALRM occurs first but before it is handled, SIGVTALRM happens.
 *    At this point both SIGARLM and SIGVTALRM are blocked.
 *    SIGVTALRM gets unblocked and the function for the TIMER_VIRTUAL is
 *    invoked and never returns. The function for the TIMER_REAL is never
 *    invoked (and any TIMER_REAL timers never expire).
 */
#ifndef NO_POSIX_SIGS
static sigset_t timer_block_mask ;
#else
static int timer_block_mask ;
#endif

static int timer_block_mask_set ;         /* flag */


/*
 * Initialize the timer_block_mask.
 * As a side-effect it also initializes the block_mask of each ostimer.
 */
PRIVATE void set_timer_block_mask()
{
   ostimer_s *otp ;

#ifndef NO_POSIX_SIGS
   (void) sigemptyset( &timer_block_mask ) ;
#else
    /* timer_block_mask is a global variable so it is initialized to 0 */
#endif

   for ( otp = &os_timers[ 0 ] ; otp->ost_handler ; otp++ )
   {
#ifndef NO_POSIX_SIGS
      (void) sigemptyset( &otp->ost_block_mask ) ;
      (void) sigaddset( &otp->ost_block_mask, otp->ost_signal ) ;
      (void) sigaddset( &timer_block_mask, otp->ost_signal ) ;
#else
      otp->ost_block_mask = sigmask( otp->ost_signal ) ;
      timer_block_mask |= otp->ost_block_mask ;
#endif
   }

   timer_block_mask_set = TRUE ;
}


PRIVATE ostimer_s *ostimer_find( type )
	enum timer_types	type ;
{
	register ostimer_s *otp ;

	for ( otp = os_timers ; otp->ost_handler ; otp++ )
		if ( otp->ost_timertype == type )
			return( otp->ost_availability == AVAILABLE ? otp : OSTIMER_NULL ) ;
	return( OSTIMER_NULL ) ;
}


PRIVATE int time_compare( p1, p2 )
   pq_obj p1, p2 ;
{
   return( TV_LT( TP( p1 )->t_expiration, TP( p2 )->t_expiration ) ) ;
}


/*
 * Initialize an OS timer. The initialization steps are:
 *
 *    create priority queue
 *    install signal handler
 *
 * We also initialize the timer_block_mask if it has not been initialized yet.
 */
ostimer_s *__ostimer_init( tp, type )
	timer_s				*tp ;
	enum timer_types	type ;
{
#ifndef NO_POSIX_SIGS
   struct sigaction  sa ;
#else
   struct sigvec     sv ;
#endif
   ostimer_s   		*otp ;
   struct timer_q    *tqp ;

	/*
	 * Find the corresponding ostimer
	 */
	if ( ( otp = ostimer_find( type ) ) == OSTIMER_NULL )
		HANDLE_ERROR( tp->t_flags, OSTIMER_NULL,
			tp->t_errnop, TIMER_ENOTAVAILABLE,
				"TIMER __ostimer_init: requested timer type not available\n" ) ;

	/*
	 * We use the value of ost_timerq to determine if the os_timer
	 * has been initialized.
	 */
   tqp = &otp->ost_timerq ;
	if ( tqp->tq_handle )
		return( otp ) ;
	
   tqp->tq_handle = pq_create( time_compare,
				tp->t_flags & TIMER_RETURN_ERROR ? PQ_RETURN_ERROR : PQ_NOFLAGS,
										&tqp->tq_errno ) ;
   if ( tqp->tq_handle == NULL )
   {
      *tp->t_errnop = TIMER_ENOMEM ;
      return( OSTIMER_NULL ) ;
   }

   if ( ! timer_block_mask_set )
      set_timer_block_mask() ;

#ifndef NO_POSIX_SIGS
   sa.sa_handler = otp->ost_handler ;
   sa.sa_mask = timer_block_mask ;
   sa.sa_flags = 0 ;
   if ( sigaction( otp->ost_signal, &sa, SIGACTION_NULL ) == -1 )
#else
   sv.sv_handler = otp->ost_handler ;
   sv.sv_mask = timer_block_mask ;
   sv.sv_flags = 0 ;
   if ( sigvec( otp->ost_signal, &sv, SIGVEC_NULL ) == -1 )
#endif
      HANDLE_ERROR( tp->t_flags, OSTIMER_NULL, tp->t_errnop, TIMER_ESIGPROBLEM,
         "TIMER __ostimer_init: signal handler installation failed\n" ) ;
   return( otp ) ;
}


/*
 * timer_* functions that need access to private data of ostimer
 */
void timer_block_type( type )
   enum timer_types	type ;
{
   ostimer_s			*otp = ostimer_find( type ) ;

	if ( otp == OSTIMER_NULL )
		return ;

#ifndef NO_POSIX_SIGS
   (void) sigprocmask( SIG_BLOCK, &otp->ost_block_mask, SIGSET_NULL ) ;
#else
   (void) sigblock( otp->ost_block_mask ) ;
#endif
}


void timer_unblock_type( type )
   enum timer_types	type ;
{
   ostimer_s			*otp = ostimer_find( type ) ;

	if ( otp == OSTIMER_NULL )
		return ;

#ifndef NO_POSIX_SIGS
   (void) sigprocmask( SIG_UNBLOCK, &otp->ost_block_mask, SIGSET_NULL ) ;
#else
	{
		int old_mask = sigblock( ~0 ) ;

		(void) sigsetmask( old_mask & ~otp->ost_block_mask ) ;
	}
#endif
}


void __ostimer_blockall()
{
#ifndef NO_POSIX_SIGS
   (void) sigprocmask( SIG_BLOCK, &timer_block_mask, SIGSET_NULL ) ;
#else
   (void) sigblock( timer_block_mask ) ;
#endif
}


void __ostimer_unblockall()
{
#ifndef NO_POSIX_SIGS
   (void) sigprocmask( SIG_UNBLOCK, &timer_block_mask, SIGSET_NULL ) ;
#else
   int old_mask = sigblock( ~0 ) ;

   (void) sigsetmask( old_mask & ~timer_block_mask ) ;
#endif
}


void __ostimer_unblockall_except( otp )
   ostimer_s *otp ;
{
#ifndef NO_POSIX_SIGS
   sigset_t new_mask = timer_block_mask ;

   (void) sigdelset( &new_mask, otp->ost_signal ) ;
   (void) sigprocmask( SIG_UNBLOCK, &new_mask, SIGSET_NULL ) ;
#else
   int old_mask = sigblock( ~0 ) ;

   (void) sigsetmask( ( old_mask & ~timer_block_mask )
                                          | otp->ost_block_mask ) ;
#endif
}


PRIVATE void sigalrm_handler()
{
#ifdef DEBUG_MSGS
   printf( "\tSIGALRM happened\n" ) ;
#endif
   __ostimer_interrupt( &os_timers[ (int)TIMER_REAL ] ) ;
}


PRIVATE void sigvtalrm_handler()
{
#ifdef DEBUG_MSGS
   printf( "\tSIGVTALRM happened\n" ) ;
#endif
   __ostimer_interrupt( &os_timers[ (int)TIMER_VIRTUAL ] ) ;
}


PRIVATE void sigprof_handler()
{
#ifdef DEBUG_MSGS
   printf( "\tSIGPROF happened\n" ) ;
#endif
   __ostimer_interrupt( &os_timers[ (int)TIMER_PROF ] ) ;
}


PRIVATE void get_real_time( tvp )
   struct timeval *tvp ;
{
#ifdef ITIMER_REAL
   (void) gettimeofday( tvp, TIMEZONE_NULL ) ;
#endif
}


PRIVATE void get_virtual_time( tvp )
   struct timeval *tvp ;
{
#ifdef ITIMER_VIRTUAL
   struct rusage ru ;

   (void) getrusage( RUSAGE_SELF, &ru ) ;
   *tvp = ru.ru_utime ;
#endif
}


PRIVATE void get_prof_time( tvp )
   struct timeval *tvp ;
{
#ifdef ITIMER_PROF
   struct rusage ru ;

   (void) getrusage( RUSAGE_SELF, &ru ) ;
   TV_ADD( *tvp, ru.ru_utime, ru.ru_stime ) ;
#endif
}


