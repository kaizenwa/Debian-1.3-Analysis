/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: init.c,v 1.2 1995/09/10 15:42:38 chuck Exp $" ;

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <syslog.h>
#include <fcntl.h>

#include "sio.h"
#include "str.h"
#include "pset.h"
#include "xlog.h"

#include "options.h"

#include "state.h"
#include "defs.h"
#include "config.h"
#include "conf.h"

#ifdef BSD		/* fix for getrlimit() problem in BSDI */
#undef RLIMIT_NOFILE
#include <unistd.h>
#endif

struct module
{
	char *name ;
	status_e (*initializer)() ;
} ;


char		*msg_init() ;
status_e signal_init() ;
status_e initenv() ;
status_e create_conf_timer() ;
status_e create_retry_timer() ;
status_e create_cc_timer() ;

static struct module program_modules[] = 
	{
		{ "signal",							signal_init				},
		{ "environment",					initenv					},
#ifndef NO_TIMERS
		{ "conf timer",					create_conf_timer		},
		{ "retry timer",					create_retry_timer	},
		{ "consistency check timer",	create_cc_timer		},
#endif
		{ CHAR_NULL }
	} ;


static bool_int have_stderr ;

extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;

void msg() ;

void exit() ;

#define STDERR_FD						2


/*
 * This function is invoked when a system call fails during initialization.
 * A message is printed to stderr, and the program is terminated
 */
PRIVATE void syscall_failed( call )
	char *call ;
{
	char errno_buf[ 40 ] ;
	char *err ;

	if ( have_stderr )
	{
		if ( errno < sys_nerr )
			err = sys_errlist[ errno ] ;
		else
			err = strx_sprint( errno_buf, sizeof( errno_buf ),
																"errno = %d", errno ) ;
		Sprint( STDERR_FD, "%s: %s failed: %s\n", program_name, call, err ) ;
	}
	exit( 1 ) ;
}



/*
 * Close all descriptors except STDERR_FD. We need this to report
 * errors and the process pid of the daemon.
 * Open all descriptors in the range 0..MAX_PASS_FD (except STDERR_FD)
 * to the root directory.
 * STDERR_FD should not be 0.
 */
PRIVATE void setup_file_descriptors()
{
	int	fd ;
	int	new ;
	int	root_fd ;
	int	n_descriptors = getdtablesize() ;

	/*
	 * Close all unneeded descriptors
	 */
	for ( fd = 0 ; fd < n_descriptors ; fd++ )
		if ( fd != STDERR_FD )
			(void) close( fd ) ;
	
	/*
	 * Check if the STDERR_FD descriptor is open.
	 */
	new = dup( STDERR_FD ) ;
	if ( new != -1 )
	{
		have_stderr = TRUE ;
		(void) close( new ) ;
	}

	if ( ( root_fd = open( "/", O_RDONLY ) ) == -1 )
		syscall_failed( "open of '/'" ) ;

	for ( fd = 0 ; fd <= MAX_PASS_FD ; fd++ )
	{
		if ( have_stderr && fd == STDERR_FD )
			continue ;
		if ( fd != root_fd && dup2( root_fd, fd ) == -1 )
			syscall_failed( "dup2" ) ;
	}

	if ( root_fd > MAX_PASS_FD )
		(void) close( root_fd ) ;
}



PRIVATE void set_fd_limit()
{
#ifdef RLIMIT_NOFILE

	struct rlimit rl ;
	char *func = "set_fd_limit" ;

	/*
	 * Set the soft file descriptor limit to the hard limit.
	 */
	if ( getrlimit( RLIMIT_NOFILE, &rl ) == -1 )
	{
		msg( LOG_CRIT, func, "getrlimit: %m" ) ;
		exit( 1 ) ;
	}

	ps.ros.orig_max_descriptors = rl.rlim_cur ;
   ps.ros.max_descriptors = rl.rlim_max ;

	rl.rlim_cur = rl.rlim_max ;
	if ( setrlimit( RLIMIT_NOFILE, &rl ) == -1 )
	{
		ps.ros.max_descriptors = ps.ros.orig_max_descriptors ;
		return ;
	}

	if ( Smorefds() == SIO_ERR )
	{
		msg( LOG_CRIT, func, "Smorefds: %m" ) ;
		exit( 1 ) ;
	}

#else		/* ! RLIMIT_NOFILE */

   ps.ros.max_descriptors = getdtablesize() ;

#endif	/* RLIMIT_NOFILE */
}


PRIVATE void init_common( argc, argv )
	int argc ;
	char *argv[] ;
{
	struct module *mp ;
	char *func = "init_common" ;

	/*
	 * Initialize the program state
	 */
	set_fd_limit() ;

	ps.ros.Argv = argv ;
	ps.ros.Argc = argc ;
   ps.ros.config_file = f_option ? f_option_arg : DEFAULT_CONFIG_FILE ;
	ps.ros.is_superuser = ( geteuid() == 0 ) ;
	if ( limit_option )
		ps.ros.process_limit = limit_option_arg ;
	ps.ros.loop_rate = ( loop_option ) ? loop_option_arg : DEFAULT_LOOP_RATE ;

	/*
	 * Initialize the program modules
	 */
	for ( mp = program_modules ; mp->name ; mp++ )
		if ( (*mp->initializer)() == FAILED )
		{
			msg( LOG_CRIT, func,
				"Initialization of %s facility failed. Exiting...", mp->name ) ;
			exit( 1 ) ;
		}
	(void) umask( 0 ) ;
}


/*
 * Become a daemon by forking a new process. The parent process exits.
 */
PRIVATE void become_daemon()
{
	int	tries ;
	int	pid ;
	char	*func = "become_daemon" ;
	void	no_control_tty() ;

	/*
	 * First fork so that the parent will think we have exited
	 */
	for ( tries = 0 ;; tries++ )
	{
		if ( tries == 5 )
		{
			msg( LOG_CRIT, func, "fork: %m. Exiting..." ) ;
			exit( 0 ) ;
		}

		pid = fork() ;

		if ( pid == -1 )
		{
			sleep( 1 ) ;	/* wait for a second */
			continue ;		/* and then retry		*/
		}
		else if ( pid == 0 )
			break ;
		else
		{
			if ( pid_option && have_stderr )
				Sprint( STDERR_FD, "%d\n", pid ) ;
#ifndef DEBUG_DAEMON
			sleep( 3 ) ;	/* give some time to the daemon to initialize */
#endif
			exit( 0 ) ;
		}
	}

	(void) dup2( 0, STDERR_FD ) ;
	no_control_tty() ;

#ifdef DEBUG_DAEMON
	sleep( 20 ) ; 		/* XXX: timers will probably not work after this */
#endif
}



PRIVATE pset_h new_table( size )
	unsigned size ;
{
	char *func = "new_table" ;
	pset_h tab = pset_create( size, 0 ) ;

	if ( tab == NULL )
	{
		msg( LOG_CRIT, func, "Failed to create table" ) ;
		exit( 1 ) ;
	}
	return( tab ) ;
}


/*
 * Create tables
 */
PRIVATE void init_rw_state()
{
   SERVERS( ps ) = new_table( 0 ) ;
	RETRIES( ps ) = new_table( 0 ) ;
	SERVICES( ps ) = new_table( 0 ) ;

	ps.rws.descriptors_free = ps.ros.max_descriptors - DESCRIPTORS_RESERVED ;

	FD_ZERO( &ps.rws.socket_mask ) ;
	ps.rws.mask_max = 0 ;

}


/*
 * Perform all necessary initializations
 */
void init_daemon( argc, argv )
	int argc ;
	char *argv[] ;
{
	char *fail ;

	setup_file_descriptors() ;

	(void) opt_recognize( argc, argv ) ;
	debug.on = d_option ;

	/*
	 * XXX: we only use xlog_parms on XLOG_SYSLOG-type logs but in general
	 *		  we should do it for all types of xlog's we may use. We can get
	 *		  away with this now, because xlog_parms for XLOG_FILELOG is a noop.
	 */
	(void) xlog_parms( XLOG_SYSLOG,
					program_name, LOG_PID + LOG_NOWAIT, LOG_DAEMON ) ;

	/*
	 * Initialize the message facility; after this everything can use the
	 * msg() interface
	 */
	if ( fail = msg_init() )
	{
		if ( have_stderr )
			Sprint( STDERR_FD, "%s: msg_init failed: %s\n", program_name, fail ) ;
		exit( 1 ) ;
	}

	init_common( argc, argv ) ;

	if ( ! debug.on )
		become_daemon() ;
	
	init_rw_state() ;
}


/*
 * Initialize all services
 *
 * This function is either successful in starting some services 
 * or it terminates the program.
 */
void init_services()
{
	struct configuration conf ;
	char *func = "init_services" ;
	void spec_include() ;

	if ( cnf_get( &conf, (long)0 ) == FAILED )
	{
		msg( LOG_CRIT, func, "couldn't get configuration. Exiting..." ) ;
		exit( 1 ) ;
	}

	DEFAULTS( ps ) = CNF_DEFAULTS( &conf ) ;
	(void) cnf_start_services( &conf ) ;
	CNF_DEFAULTS( &conf ) = NULL ;		/* to avoid the free by cnf_free */
	cnf_free( &conf ) ;

	/*
	 * The number of available/active services is kept by the service functions
	 */
	if ( ps.rws.available_services == 0 )
	{
		msg( LOG_CRIT, func, "no services. Exiting..." ) ;
		exit( 1 ) ;
	}

	spec_include() ;		/* include special services */
}

