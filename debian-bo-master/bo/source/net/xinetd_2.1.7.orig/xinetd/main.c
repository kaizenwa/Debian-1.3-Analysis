/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: main.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;
char program_version[] = VERSION ;

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include <syslog.h>

#include "sio.h"

#include "options.h"

#include "service.h"
#include "state.h"


/*
 * The following are the only global variables of this program
 */
struct program_state ps ;
struct debug debug ;

extern int errno ;

void exit() ;
void msg() ;

/*
 * This is where the story starts...
 */
int main( argc, argv )
	int argc ;
	char *argv[] ;
{
	char				*func = "main" ;
	PRIVATE void	main_loop() ;
	void				init_services() ;
	void				init_daemon() ;
	void				enable_periodic_check() ;

	init_daemon( argc, argv ) ;
	init_services() ;

	msg( LOG_NOTICE, func, "Started working: %d available service%s",
		ps.rws.available_services,
			( ps.rws.available_services > 1 ) ? "s" : "" ) ;

	if ( cc_option )
#ifndef NO_TIMERS
		enable_periodic_check( cc_option_arg ) ;
#else
		msg( LOG_WARNING, func, "-cc option not supported" ) ;
#endif
	
	/*
	 * The reason for doing the setjmp here instead of in main_loop is
	 * that setjmp is not guaranteed to restore register values which
	 * can cause a problem for register variables
	 */
	if ( setjmp( ps.rws.env ) == 0 )
		ps.rws.env_is_valid = TRUE ;

	main_loop() ;

	/* NOTREACHED */
}


/*
 * What main_loop does:
 *
 *		check if any flags are set
 *		select on all active services
 *		for each socket where a request is pending
 *			try to start a server
 */
PRIVATE void main_loop()
{
	char				*func = "main_loop" ;
	PRIVATE void	find_bad_fd() ;
	void				check_flags() ;
	void				signal_wait() ;

	for ( ;; )
	{
		fd_set read_mask ;
		register int n_active ;
		register unsigned u ;

		check_flags() ;

		if ( debug.on )
			msg( LOG_DEBUG, func,
					"active_services = %d", ps.rws.active_services ) ;

		if ( ps.rws.active_services == 0 )
		{
			signal_wait() ;
			continue ;
		}

		read_mask = ps.rws.socket_mask ;
		n_active = select( ps.rws.mask_max+1, &read_mask,
								FD_SET_NULL, FD_SET_NULL, TIMEVAL_NULL ) ;
		if ( n_active == -1 )
		{
			if ( errno == EINTR )
				continue ;
			else if ( errno == EBADF )
				find_bad_fd() ;
			else
				msg( LOG_NOTICE, func, "select error: %m" ) ;
			continue ;
		}
		else if ( n_active == 0 )
			continue ;

		if ( debug.on )
			msg( LOG_DEBUG, func, "select returned %d", n_active ) ;

		for ( u = 0 ; u < pset_count( SERVICES( ps ) ) ; u++ )
		{
			register struct service *sp ;

			sp = SP( pset_pointer( SERVICES( ps ), u ) ) ;

			if ( ! SVC_IS_ACTIVE( sp ) )
				continue ;

			if ( FD_ISSET( SVC_FD( sp ), &read_mask ) )
			{
				svc_request( sp ) ;
				if ( --n_active == 0 )
					break ;
			}
		}
		if ( n_active > 0 )
			msg( LOG_ERR, func, "%d descriptors still set", n_active ) ;
	}
}



/*
 * This function identifies if any of the fd's in the socket mask
 * is bad. We use it in case select(2) returns EBADF
 * When we identify such a bad fd, we remove it from the mask
 * and deactivate the service.
 */
PRIVATE void find_bad_fd()
{
	register int fd ;
	struct stat st ;
	unsigned bad_fd_count = 0 ;
	char *func = "find_bad_fd" ;

	for ( fd = 0 ; fd < ps.ros.max_descriptors ; fd++ )
		if ( FD_ISSET( fd, &ps.rws.socket_mask ) && fstat( fd, &st ) == -1 )
		{
			int found = FALSE ;
			register unsigned u ;

			for ( u = 0 ; u < pset_count( SERVICES( ps ) ) ; u++ )
			{
				register struct service *sp ;

				sp = SP( pset_pointer( SERVICES( ps ), u ) ) ;

				if ( ! SVC_IS_AVAILABLE( sp ) )
					continue ;

				if ( SVC_FD( sp ) == fd )
				{
					msg( LOG_ERR, func,
						"file descriptor of service %s has been closed",
										SVC_ID( sp ) ) ;
					svc_deactivate( sp ) ;
					found = TRUE ;
					break ;
				}
			}
			if ( ! found )
			{
				FD_CLR( fd, &ps.rws.socket_mask ) ;
				msg( LOG_ERR, func,
					"No active service for file descriptor %d\n", fd ) ;
				bad_fd_count++ ;
			}
		}
	if ( bad_fd_count == 0 )
		msg( LOG_NOTICE, func,
			"select reported EBADF but no bad file descriptors were found" ) ;
}


/*
 * Deactivates all active processes.
 * The real reason for doing this instead of just exiting is
 * to deregister the RPC services
 */
void quit_program()
{
	register unsigned u ;
	char *func = "quit_program" ;

	for ( u = 0 ; u < pset_count( SERVICES( ps ) ) ; u++ )
		svc_deactivate( SP( pset_pointer( SERVICES( ps ), u ) ) ) ;
	msg( LOG_WARNING, func, "Exiting..." ) ;
	exit( 0 ) ;
}


void terminate_program()
{
	register unsigned u ;
	void terminate_servers() ;

	for ( u = 0 ; u < pset_count( SERVICES( ps ) ) ; u++ )
		terminate_servers( SP( pset_pointer( SERVICES( ps ), u ) ) ) ;
	quit_program() ;
}

