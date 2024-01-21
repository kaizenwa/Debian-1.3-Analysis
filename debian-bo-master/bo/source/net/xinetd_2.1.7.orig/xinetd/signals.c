/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: signals.c,v 1.7 1995/09/11 18:18:21 chuck Exp $" ;

#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <syslog.h>
#include <errno.h>
#include <string.h>

#include "str.h"

#include "state.h"
#include "defs.h"
#include "config.h"
#include "flags.h"

extern int errno ;

void msg() ;

void exit() ;
time_t time() ;

#define SIGSET_NULL							((sigset_t *)0)
#define SIGVEC_NULL							((struct sigvec *)0)
#define SIGACTION_NULL						((struct sigaction *)0)

#ifdef NO_POSIX_SIGS

#ifdef NO_POSIX_TYPES
/*
 * XXX:	here we assume that in the case that NO_POSIX_TYPES is not defined
 *			(i.e. the system has posix types) the sigset_t is also typedef'd
 *			to 'int'. Our goal is to work with systems that have defined
 *			sigset_t but do not yet support the posix signal interface.
 */
typedef int sigset_t ;

struct sigaction
{
	void			(*sa_handler)() ;
	sigset_t		sa_mask ;
	int			sa_flags ;
} ;
#endif	/* NO_POSIX_TYPES */

#ifdef NO_SIGVEC
#define sigmask( sig )						( 1 << ( (sig) -1 ) )
typedef int (*sighandler_type)() ;
#define sigpause( x )
#define sigsetmask( x )
#endif	/* NO_SIGVEC */


#define sigsuspend( set )					sigpause( *set )
#define sigemptyset( set )					(*set) = 0
#define sigaddset( set, sig )				( ( (*set) |= sigmask( sig ) ), 0 )
#define sigismember( set, sig )			( ( (*set) & sigmask( sig ) ) != 0 )


/*
 * Only works for SIG_SETMASK and SIG_UNBLOCK. Also oset must be NULL.
 */
int sigprocmask( how, set, oset )
	int how ;
	sigset_t *set ;
	sigset_t *oset ;
{
	if ( how == SIG_BLOCK || oset != NULL )
	{
		msg( LOG_ERR, "sigprocmask",
									"Bad args: how = %d, oset = %p", how, oset ) ;
		return( -1 ) ;
	}

	if ( how == SIG_SETMASK )
	{
		(void) sigsetmask( *set ) ;
		return( 0 ) ;
	}

	if ( how == SIG_UNBLOCK )
	{
		int current_mask = sigblock( 0 ) ;

		(void) sigsetmask( current_mask & ~*set ) ;
		return( 0 ) ;
	}
	/* NOTREACHED */
}


/*
 * NOTE: This is not a complete imitation of sigaction; in particular it
 *			expects that sap is never NULL and that osap is always NULL.
 */
int sigaction( sig, sap, osap )
	int sig ;
	struct sigaction *sap ;
	struct sigaction *osap ;
{
	if ( sap == NULL || osap != NULL )
	{
		msg( LOG_ERR, "sigaction", "Bad args: sap = %p, osap = %p", sap, osap ) ;
		return( -1 ) ;
	}

#ifndef NO_SIGVEC
	{
		struct sigvec sv ;

		sv.sv_handler = sap->sa_handler ;
		sv.sv_mask = sap->sa_mask ;
		sv.sv_flags = sap->sa_flags ;

		return( sigvec( sig, &sv, SIGVEC_NULL ) ) ;
	}
#else		/* NO_SIGVEC */
	{
		sighandler_type new_handler ;

		new_handler = sa.sa_handler ;
		return( signal( sig, new_handler ) ? 0 : -1 ) ;
	}
#endif	/* ! NO_SIGVEC */
}

#endif	/* NO_POSIX_SIGS */


/*
 * reset_sigs is the list of signals that we need to reset to SIG_DFL.
 * Currently, these are the signals whose actions we set to SIG_IGN.
 * In general, we should also include any signals that have a handler
 * that does anything other than setting a flag. We need to do this
 * in case such a signal occurs while a forked process is providing
 * an internal service.
 */
static sigset_t reset_sigs ;

/*
 * nsig is equal to the greatest signal number supported plus 1
 */
static int nsig ;



/*
 * When this function returns FAILED, we check the errno to determine
 * if it failed because the signal number specified was invalid.
 * This allows us to determine the number of supported signals.
 */
PRIVATE status_e handle_signal( sig )
	register int sig ;
{
	struct sigaction		sa ;
	voidfunc					sig_handler ;
	PRIVATE void			my_handler() ;				/* for the signals we are 	*/
																/* interested in				*/
	PRIVATE void			general_handler() ;		/* for everything else		*/

	switch ( sig )
	{
		case RECONFIG_SOFT_SIG:
		case RECONFIG_HARD_SIG:
		case TERMINATION_SIG:
		case STATE_DUMP_SIG:
		case CONSISTENCY_CHECK_SIG:
		case SERVER_EXIT_SIG:
#ifdef NO_TIMERS
		case RETRY_SIG:
#endif
		case QUIT_SIG:
			sig_handler = my_handler ;
			break ;

		case SIGTTIN:
		case SIGTTOU:
		case SIGTSTP:
		case SIGCONT:
			if ( debug.on )
				return( OK ) ;
			/* FALL THROUGH */
			 
		/*
		 * We may receive a SIGPIPE when handling an internal stream 
		 * service and the other end closes the connection.
		 * We only care about internal services that don't require forking.
		 */
		case SIGPIPE:
			sig_handler = SIG_IGN ;
			sigaddset( &reset_sigs, sig ) ;
			break ;

		case SIGKILL:
		case SIGSTOP:
			return( OK ) ;			/* we can't catch these two */
		
		/*
		 * If the following two cases are included, SIGSEGV and SIGBUS will
		 * cause core dumps. We want that to happen when we are debugging
		 * xinetd (i.e. DEBUG is defined) and we are not debugging the
		 * signal recovery code (i.e. DEBUG_SIGNALS is not defined).
		 */
#if defined( DEBUG ) && !defined( DEBUG_SIGNALS )
		case SIGSEGV:
		case SIGBUS:
			return( OK ) ;
#endif

		case SIGTRAP:
			if ( debug.on )
				return( OK ) ;
		
		default:
			sig_handler = general_handler ;
	}

	sa.sa_flags = 0 ;
	sigemptyset( &sa.sa_mask ) ;
	sa.sa_handler = sig_handler ;
	return( ( sigaction( sig, &sa, SIGACTION_NULL ) == -1 ) ? FAILED : OK ) ;
}



/*
 * Install signal handlers for all signals that can be caught.
 * This implies that no core dumps are generated by default.
 */
status_e signal_init()
{
	register int		sig ;
	char					*func = "install_signal_handlers" ;
	char					*sig_name() ;

	sigemptyset( &reset_sigs ) ;
#ifndef NO_TIMERS
	sigaddset( &reset_sigs, SIGALRM ) ;
#endif

	for ( sig = 1 ;; sig++ )
		if ( handle_signal( sig ) == FAILED )
			if ( errno == EINVAL )
			{
				nsig = sig ;
				break ;
			}
			else
			{
				msg( LOG_CRIT, func,
					"Failed to install signal handler for signal %s: %m",
						sig_name( sig ) ) ;
				return( FAILED ) ;
			}
	return( OK ) ;
}


void signal_wait()
{
	sigset_t masked_sigs ;

	sigemptyset( &masked_sigs ) ;
	(void) sigsuspend( &masked_sigs ) ;
}


#define MAX_SIGNAL_COUNT					50
#define MAX_INTERVAL_SIGNAL_COUNT		10
#define SIGNAL_INTERVAL						1		/* second */

/*
 * This function handles SIGSEGV and SIGBUS.
 * Emergency action is taken if a certain number (MAX_SIGNAL_COUNT) of 
 * these signals is received over the lifetime of the program OR 
 * if a certain number (MAX_INTERVAL_SIGNAL_COUNT) of these signals 
 * is received within a certain time interval (SIGNAL_INTERVAL).
 *
 * The action depends on the type of the emergency:
 *		Case 1: MAX_INTERVAL_SIGNAL_COUNT is exceeded
 *			If a setjmp environment is available, do a longjmp, otherwise exit
 *		Case 2: MAX_SIGNAL_COUNT is exceeded
 *			Exit
 *
 * NOTE: We try to send a message to the log only once to avoid
 *			looping in this function (in case there is a bug in msg())
 */
PRIVATE void bad_signal()
{
	static time_t	interval_start ;
	static int		interval_signal_count ;
	static int		total_signal_count ;
	time_t			current_time ;
	char				*func = "bad_signal" ;

	total_signal_count++ ;
	if ( total_signal_count == MAX_SIGNAL_COUNT )
	{
		msg( LOG_CRIT, func,
				"Received %d bad signals. Exiting...", total_signal_count ) ;
		exit( 1 ) ;
	}
	else if ( total_signal_count > MAX_SIGNAL_COUNT )
		_exit( 1 ) ;		/* in case of a problem in exit(3) */
	
	(void) time( &current_time ) ;

	if ( interval_signal_count > 0 &&
				current_time - interval_start <= SIGNAL_INTERVAL )
	{
		interval_signal_count++ ;
		if ( interval_signal_count == MAX_INTERVAL_SIGNAL_COUNT )
		{
			if ( ps.rws.env_is_valid )
			{
				interval_start = current_time ;
				interval_signal_count = 1 ;
				msg( LOG_ERR, func, "Resetting..." ) ;
				longjmp( ps.rws.env, 1 ) ;
				/* NOTREACHED */
			}
			msg( LOG_CRIT, func,
				"Received %d signals in %d seconds. Exiting...",
					interval_signal_count, SIGNAL_INTERVAL ) ;
			exit( 1 ) ;
		}
		else if ( interval_signal_count > MAX_INTERVAL_SIGNAL_COUNT )
			_exit( 1 ) ;			/* shouldn't happen */
	}
	else
	{
		interval_start = current_time ;
		interval_signal_count = 1 ;
	}
}


#if defined( sun ) && defined( sparc )
#undef SP

#include <machine/reg.h>
#include <machine/frame.h>

PRIVATE void stack_trace( scp )
	struct sigcontext *scp ;
{
	struct frame		*fp ;
	struct rwindow		*rwp ;
	char					tracebuf[ 1000 ] ;
	int					len	= 0 ;
	unsigned				size	= sizeof( tracebuf ) - 1 ;

	tracebuf[ size ] = NUL ;

	if ( scp->sc_wbcnt != 0 )
		return ;
	rwp = (struct rwindow *) scp->sc_sp ;
	len = strx_nprint( tracebuf, size, "%#x %#x", scp->sc_pc, rwp->rw_rtn ) ;
	size -= len ;

	for ( fp = (struct frame *) rwp->rw_fp ; fp != NULL ; fp = fp->fr_savfp ) 
	{
		int cc ;

		cc = strx_nprint( &tracebuf[ len ], size,
												" %#x", (unsigned) fp->fr_savpc ) ;
		len += cc ;
		size -= cc ;
		fp = fp->fr_savfp ;
	}

	msg( LOG_CRIT, "stack_trace", "%s", tracebuf ) ;
}
#endif	/* sun && sparc */



char *sig_name( sig )
	int sig ;
{
	static char signame_buf[ 30 ] ;

#if !defined( NO_SIGLIST ) && defined( NSIG )

#if !defined(linux) && !(BSD > 199300) && !defined(__FreeBSD__) && !defined(__NetBSD__)
	extern char *sys_siglist[] ;
#endif	/* linux */

	if ( sig < NSIG )
		return( strx_sprint( signame_buf, sizeof( signame_buf ),
							"%d (%s)", sig, sys_siglist[ sig ] ) ) ;
#endif
	return( strx_sprint( signame_buf, sizeof( signame_buf ), "%d", sig ) ) ;
}


/*
 * For SIGSEGV and SIGBUS we invoke the bad_signal() function (if this is 
 * SunOS, we log the address where the problem occured).
 *
 * If we are not running under SunOS, we use the NO_SIGVEC flag to determine 
 * if we have a struct sigcontext (since struct sigcontext is documented in 
 * the sigvec man page).
 *
 * For other signals, we just log the fact that they occured.
 * SIGINT is a special case since in debug.on mode, it will 
 * cause termination.
 */

#ifdef sun

PRIVATE void general_handler( sig, code, scp, addr )
	int sig, code ;
	struct sigcontext *scp;
	char *addr;
{
	int pc = scp->sc_pc ;

#else		/* not a Sun */

#if !defined(NO_SIGVEC) && !defined(linux)	/* this is weird */

PRIVATE void general_handler( sig, code, scp )
	int sig, code ;
	struct sigcontext *scp;
{
	char *addr = NULL ;
	int pc = scp->sc_pc ;

#else		/* defined( NO_SIGVEC ) */

PRIVATE void general_handler( sig )
	int sig ;
{
	char *addr = NULL ;
	int pc = 0 ;

#endif	/* ! NO_SIGVEC */
#endif	/* sun */

	sigset_t badsigs ;
	char *func = "general_handler" ;

	/*
	 * Do this here to catch problems like SIGSEGV in msg()
	 */
	sigemptyset( &badsigs ) ;
	sigaddset( &badsigs, sig ) ;
	(void) sigprocmask( SIG_UNBLOCK, &badsigs, SIGSET_NULL ) ;

	switch ( sig )
	{
		case SIGBUS:
		case SIGSEGV:
			msg( LOG_CRIT, func,
					"(%d) Unexpected signal: %s, pc = %#x, address = %#x.",
						getpid(), sig_name( sig ), pc, addr ) ;
#if defined( sun ) && defined( sparc )
			stack_trace( scp ) ;
#endif
			bad_signal() ;
			break ;
		
		default:
			msg( LOG_NOTICE, func, "Unexpected signal %s (addr=%#x)",
						sig_name( sig ), addr ) ;
			if ( debug.on && sig == SIGINT )
				exit( 1 ) ;
	}
}


/*
 * The job of this function is to set the flag that corresponds to the
 * received signal. No other action is taken.
 */
PRIVATE void my_handler( sig )
	int sig ;
{
	/*
	 * Signals are listed most-frequent-first
	 */
	switch( sig )
	{
		case SERVER_EXIT_SIG:
			M_SET( ps.flags, CHILD_FLAG ) ;
			break ;
		
		case RETRY_SIG:
			M_SET( ps.flags, RETRY_FLAG ) ;
			break ;
		
		case RECONFIG_SOFT_SIG:
			M_SET( ps.flags, SOFT_RECONFIG_FLAG ) ;
			break ;
		
		case RECONFIG_HARD_SIG:
			M_SET( ps.flags, HARD_RECONFIG_FLAG ) ;
			break ;
		
		case TERMINATION_SIG:
			M_SET( ps.flags, TERMINATE_FLAG ) ;
			break ;
		
		case STATE_DUMP_SIG:
			M_SET( ps.flags, DUMP_FLAG ) ;
			break ;
		
		case CONSISTENCY_CHECK_SIG:
			M_SET( ps.flags, CONSISTENCY_FLAG ) ;
			break ;
		
		case QUIT_SIG:
			M_SET( ps.flags, QUIT_FLAG ) ;
			break ;
	}
}


/*
 * Reset all signals to default action. Reset the signal mask
 *
 * This function is invoked from a forked process. That is why we
 * invoke _exit instead of exit (to avoid the possible stdio buffer flushes)
 */
void signal_default_state()
{
	register int sig ;
	sigset_t empty ;

	for ( sig = 1 ; sig < nsig ; sig++ )
		if ( sigismember( &reset_sigs, sig ) == 1 )
			if ( (int) signal( sig, SIG_DFL ) == -1 )
			{
				msg( LOG_ERR, "reset_signals",
					"signal(3) failed for signal %s: %m", sig_name( sig ) ) ;
				if ( debug.on )
					_exit( 1 ) ;
			}
		
	sigemptyset( &empty ) ;
	(void) sigprocmask( SIG_SETMASK, &empty, SIGSET_NULL ) ;
}


