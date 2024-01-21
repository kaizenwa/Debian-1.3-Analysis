/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: child.c,v 1.10 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/param.h>
#ifndef BSD
#include <sys/wait.h>
#include <netinet/in.h>
#endif
#include <syslog.h>
#include <errno.h>
#include <pwd.h>
#include <fcntl.h>

#include "str.h"
#include "pset.h"

#include "options.h"

#include "attr.h"
#include "server.h"
#include "state.h"
#include "sconst.h"
#include "config.h"

char *inet_ntoa() ;

void msg() ;
void msg_suspend() ;
void msg_resume() ;

#ifdef BSD  		/* fix for getrlimit() problem in BSDI */
#undef RLIMIT_NOFILE
#include <unistd.h>
#endif

/*
 * This function is running in the new process
 */
PRIVATE void exec_server( serp )
	struct server *serp ;
{
	struct service_config *scp = SVC_CONF( SERVER_SERVICE( serp ) ) ;
	int fd ;
	int descriptor = SERVER_FD( serp ) ;
	char *server = SC_SERVER( scp ) ;
	char *func = "exec_server" ;
	void no_control_tty() ;

	/*
	 * The following code solves a problem with post-version-4.3
	 * Ultrix systems (the bug was reported, and a fix was provided by
	 * doug@seas.smu.edu; a slightly modified version of this
	 * fix is included here).
	 *
	 * If this is a 'nowait' service, we pass the service descriptor
	 * to the server. Note that we have set the close-on-exec flag
	 * on all service descriptors. It is unclear whether the dup2()
	 * will create a descriptor with the close-on-exec flag set,
	 * so we explicitly clear the flag (since we are doing this
	 * after the fork, it does not affect the descriptor of the
	 * parent process).
	 */
	if ( fcntl( descriptor, F_SETFD, 0 ) == -1 )
		msg( LOG_WARNING, func,
			"fcntl( %d, clear close-on-exec ) failed: %m", descriptor ) ;

	if ( debug.on )
		msg( LOG_DEBUG, func, "duping %d", descriptor ) ;

	for ( fd = 0 ; fd <= MAX_PASS_FD ; fd++ )
	{
		if ( dup2( descriptor, fd ) == -1 )
		{
			msg( LOG_ERR, func,
					"dup2( %d, %d ) failed: %m", descriptor, fd ) ;
			_exit( 1 ) ;
		}
	}

	(void) close( descriptor ) ;

	if ( debug.on )
	{
		msg( LOG_DEBUG, func, "exec( %s ). no control terminal", server ) ;
		no_control_tty() ;
	}

#ifdef RLIMIT_NOFILE
	{
		struct rlimit rl ;

		rl.rlim_cur = ps.ros.orig_max_descriptors ;
		rl.rlim_max = ps.ros.max_descriptors ;
		(void) setrlimit( RLIMIT_NOFILE, &rl ) ;
	}
#endif

	msg_suspend() ;

	(void) execve( server, SC_SERVER_ARGV( scp ),
											env_getvars( SC_ENV( scp )->env_handle ) ) ;

	/*
	 * The exec failed. Log the error and exit.
	 */
	msg_resume() ;
	msg( LOG_ERR, func, "execv( %s ) failed: %m", server ) ;
	_exit( 0 ) ;
}


/*
 * Rename this process by changing the ps.ros.Argv vector
 * Try to put the name of the service in ps.ros.Argv[0], Argv[1]
 * until either the service name is exhausted or we run out
 * of ps.ros.Argv's. 
 * The rest of ps.ros.Argv is cleared to spaces
 */
PRIVATE void rename_process( name )
	char *name ;
{
	register char *from = name ;
	register char *to = ps.ros.Argv[ 0 ] ;
	register int index = 1 ;

	while ( *from != NUL )
	{
		if ( *to != NUL )
			*to++ = *from++ ;
		else
			if ( index < ps.ros.Argc )
				to = ps.ros.Argv[ index++ ] ;
			else
				break ;
	}
	str_fill( to, ' ' ) ;
	while ( index < ps.ros.Argc )
		str_fill( ps.ros.Argv[ index++ ], ' ' ) ;
}


PRIVATE void set_credentials( scp )
	register struct service_config *scp ;
{
	char *func = "set_credentials" ;

	if ( SC_SPECIFIED( scp, A_GROUP ) || SC_SPECIFIED( scp, A_USER ) )
		if ( ps.ros.is_superuser )
		{
			int gid = sc_getgid( scp ) ;

			if ( setgid( gid ) == -1 )
			{
				msg( LOG_ERR, func, "setgid failed: %m" ) ;
				_exit( 1 ) ;
			}

#ifndef NO_INITGROUPS
			/*
			 * Bug discovered by maf+@osu.edu (a bug fix was also provided;
			 * a slightly modified version is included here):
			 *		initgroups was not being invoked to set the remaining
			 *		groups appropriately
			 */
			if ( SC_SPECIFIED( scp, A_USER ) )
			{
				struct passwd *pwd ;

				/*
				 * Invoke getpwuid() to get the user's name.
				 *
				 * XXX:	we should not need to invoke getpwuid(); we should
				 *			remember the user name in the configuration file.
				 */
				if ( ( pwd = getpwuid( SC_UID( scp ) ) ) == NULL )
				{
					msg( LOG_ERR, func, "getpwuid( %d ) (service=%s) failed: %m",
						SC_UID( scp ), SC_ID( scp ) ) ;
					_exit( 1 ) ;
				}

				if ( initgroups( pwd->pw_name, pwd->pw_gid ) == -1 )
				{
					msg( LOG_ERR, func, "initgroups( %s, %d ) failed: %m",
						pwd->pw_name, pwd->pw_gid ) ;
					_exit( 1 ) ;
				}
			}
#endif	/* ! NO_INITGROUPS */
		}
		else
			msg( LOG_WARNING, func, "can't change gid; not superuser" ) ;

	if ( SC_SPECIFIED( scp, A_USER ) )
		if ( ps.ros.is_superuser )
		{
			if ( setuid( SC_UID( scp ) ) == -1 )
			{
				msg( LOG_ERR, func, "setuid failed: %m" ) ;
				_exit( 1 ) ;
			}
		}
		else
			msg( LOG_WARNING, func, "can't change uid; not superuser" ) ;
}



/*
 * This function is invoked in a forked process to run a server. 
 * If the service is internal the appropriate function is invoked
 * otherwise the server program is exec'ed.
 * This function also logs the remote user id if appropriate
 */
void child_process( serp )
	register struct server *serp ;
{
	struct service							*sp = SERVER_SERVICE( serp ) ;
	register struct service_config	*scp = SVC_CONF( sp ) ;
	idresult_e								log_remote_user() ;
	char										*idresult_explain() ;
	void										signal_default_state() ;

#ifdef DEBUG_SERVER
	if ( debug.on )
	{
		msg( LOG_DEBUG, "child_process", "Process %d is sleeping", getpid() ) ;
		sleep( 10 ) ;
	}
#endif

	if ( SERVER_LOGUSER( serp ) )
	{
		unsigned timeout ;
		idresult_e result ;
		
		/*
		 * We use LOGUSER_SUCCESS_TIMEOUT unless the service requires
		 * identification, in which case we use an infinite timeout
		 */
		timeout = SC_MUST_IDENTIFY( scp ) ? 0 : LOGUSER_SUCCESS_TIMEOUT ;
		result = log_remote_user( serp, timeout ) ;

		if ( result != IDR_OK && SC_MUST_IDENTIFY( scp ) )
		{
			svc_logprint( sp, NOID_ENTRY, "%s %s",
						conn_addrstr( SERVER_CONNECTION( serp ) ),
							idresult_explain( result ) ) ;
			_exit( 0 ) ;
		}
	}

	if ( ! SC_IS_INTERCEPTED( scp ) )
	{
		set_credentials( scp ) ;
		signal_default_state() ;
		if ( SC_SPECIFIED( scp, A_NICE ) )
			(void) nice( SC_NICE( scp ) ) ;
	}

	if ( ! SC_IS_INTERNAL( scp ) )
		exec_server( serp ) ;
	else
	{
		char name[ 180 ] ;

		/*
		 * We don't bother to disassociate from the controlling terminal
		 *	(we have a controlling terminal only if debug.on is TRUE)
		 *
		 * Also, for interceptor processes, we give them the name:
		 *				<program_name> <service-id> interceptor
		 */
		if ( SC_IS_INTERCEPTED( scp ) )
			strx_print( INT_NULL, name, sizeof( name ),
									"%s %s interceptor", program_name, SC_ID( scp ) ) ;
		else
		{
			int namelen = sizeof( name ) - 1 ;		/* leave space for the NUL */
			struct sockaddr_in *sinp = conn_address( SERVER_CONNECTION( serp ) ) ;
			int len = strx_nprint( name, namelen,
										"(%s service) %s", program_name, SC_ID( scp ) ) ;
			
			if ( SC_ACCEPTS_CONNECTIONS( scp ) && sinp != SOCKADDRIN_NULL )
				strx_print( INT_NULL, &name[ len ], namelen - len,
													" %s", inet_ntoa( sinp->sin_addr ) ) ;
		}
		rename_process( name ) ;
		svc_internal( sp, serp ) ;
	}
	_exit( 0 ) ;
	/* NOTREACHED */
}


/*
 * This function is invoked when a SIGCLD is received
 */
void child_exit()
{
	char *func = "child_exit" ;

	for ( ;; )				/* Find all children that exited */
	{
		int status ;
		register int pid ;
		struct server *serp ;
		
#if defined( sun ) && defined( lint )
		pid = wait3( (union wait *)&status, WNOHANG, RUSAGE_NULL ) ;
#else
		pid = wait3( &status, WNOHANG, RUSAGE_NULL ) ;
#endif

		if ( debug.on )
			msg( LOG_DEBUG, func, "wait3 returned = %d", pid ) ;
		
		if ( pid == -1 )
			if ( errno == EINTR )
				continue ;
			else
				break ;

		if ( pid == 0 )
			break ;
		
		if ( ( serp = server_lookup( pid ) ) != NULL )
		{
			serp->svr_exit_status = status ;
			server_end( serp ) ;
		}
		else
			msg( LOG_NOTICE, func, "unknown child process %d %s", pid,
				PROC_STOPPED( status ) ? "stopped" : "died" ) ;
	}
}

