/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: ident.c,v 1.4 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#if defined(BSD) || defined(linux)
#include <netinet/in.h>
#endif
#include <netdb.h>
#include <signal.h>
#include <syslog.h>
#include <setjmp.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>

#include "str.h"

#include "defs.h"
#include "sconst.h"
#include "server.h"

void msg() ;
status_e write_buf() ;


#define BUFSIZE					1024		/* RFC-1413 suggests 1000 */

#define START_TIMER( t )		(void) alarm( t )
#define STOP_TIMER()				(void) alarm( 0 )

char *inet_ntoa() ;


static jmp_buf env ;


PRIVATE void sigalrm_handler()
{
	longjmp( env, 1 ) ;
}


/*
 * This function always runs in a forked process.
 */
idresult_e log_remote_user( serp, timeout )
	struct server *serp ;
	unsigned timeout ;				/* in seconds */
{
	static char				buf[ BUFSIZE ] ;
	int						cc ;
	struct sockaddr_in	sin_local ;
	struct sockaddr_in	sin_remote ;
	struct sockaddr_in	sin_contact ;
	unsigned					local_port ;
	unsigned					remote_port ;
	int						sd ;
	int						sin_len ;
	char						*p ;
	char						*func = "log_remote_user" ;
	PRIVATE char			*get_line() ;
	PRIVATE char			*verify_line() ;

#ifndef NO_TIMERS
	/*
	 * Although expiring timers are harmless, they still take time to process
	 */
	STOP_TIMER() ;
#endif

	if ( timeout && (int) signal( SIGALRM, sigalrm_handler ) == -1 )
	{
		msg( LOG_ERR, func, "signal: %m" ) ;
		return( IDR_ERROR ) ;
	}

	/*
	 * Determine local and remote addresses
	 */
	sin_len = sizeof( sin_local ) ;
	if ( getsockname( SERVER_FD( serp ), SA( &sin_local ), &sin_len ) == -1 )
	{
		msg( LOG_ERR, func, "getsockname: %m" ) ;
		return( IDR_ERROR ) ;
	}

	if ( ! conn_address( SERVER_CONNECTION( serp ) ) )
	{
		/*
		 * This shouldn't happen since identification only works for
		 * connection-based services.
		 */
		msg( LOG_ERR, func, "connection has no address" ) ;
		return( IDR_ERROR ) ;
	}

	sin_remote = *conn_address( SERVER_CONNECTION( serp ) ) ;
	local_port = ntohs( sin_local.sin_port ) ;
	remote_port = ntohs( sin_remote.sin_port ) ;

	/*
	 * Create a socket and set the close-on-exec flag on the descriptor.
	 * We set the flag in case we are called as part of a successful
	 * attempt to start a server (i.e. execve will follow).
	 */
	sd = socket( AF_INET, SOCK_STREAM, 0 ) ;
	if ( sd == -1 )
	{
		msg( LOG_ERR, func, "socket creation: %m" ) ;
		return( IDR_ERROR ) ;
	}
	if ( fcntl( sd, F_SETFD, 1 ) == -1 )
	{
		msg( LOG_ERR, func, "fcntl F_SETFD: %m" ) ;
		(void) close( sd ) ;
		return( IDR_ERROR ) ;
	}

	CLEAR( sin_contact ) ;
	sin_contact.sin_family = sin_remote.sin_family ;
	sin_contact.sin_addr = sin_remote.sin_addr ;
	sin_contact.sin_port = htons( IDENTITY_SERVICE_PORT ) ;

	if ( timeout )
		if ( setjmp( env ) == 0 )
			START_TIMER( timeout ) ;
		else
			return( IDR_TIMEDOUT ) ;

	if ( connect( sd, SA( &sin_contact ), sizeof( sin_contact ) ) == -1 )
	{
		if ( timeout )
			STOP_TIMER() ;
		return( IDR_NOSERVER ) ;
	}

	cc = strx_nprint( buf, sizeof( buf ),
										"%d,%d\r\n", remote_port, local_port ) ;
	if ( write_buf( sd, buf, cc ) == FAILED )
	{
		if ( timeout )
			STOP_TIMER() ;
		return( IDR_ERROR ) ;
	}

	p = get_line( sd, buf, sizeof( buf ) ) ;

	if ( timeout )
		STOP_TIMER() ;

	if ( p == NULL )
		return( IDR_RESPERR ) ;
	
	/*
	 * Verify that the received line is OK
	 */
	if ( ( p = verify_line( buf, local_port, remote_port ) ) == NULL )
	{
		msg( LOG_ERR, func, "Bad line received from identity server at %s: %s",
										inet_ntoa( sin_remote.sin_addr ), buf ) ;
		return( IDR_BADRESP ) ;
	}

	svc_logprint( SERVER_CONNSERVICE( serp ), USERID_ENTRY, "%s", p ) ;
	return( IDR_OK ) ;
}


PRIVATE char *verify_line( line, local_port, remote_port )
	char			*line ;
	unsigned 	local_port ;
	unsigned 	remote_port ;
{
	register char	*p ;
	register char	*start = line ;

	/*
	 * Verify port numbers
	 */
	p = strchr( start, ',' ) ;
	if ( p == NULL )
		return( NULL ) ;
	*p = NUL ;
	if ( atoi( start ) != remote_port )
		return( NULL ) ;
	
	start = p+1 ;
	p = strchr( start, ':' ) ;
	if ( p == NULL )
		return( NULL ) ;
	*p = NUL ;
	if ( atoi( start ) != local_port )
		return( NULL ) ;
	
	/*
	 * Look for the 'USERID' string
	 */
	{
		char *line_id = "USERID" ;
		int line_id_len = strlen( line_id ) ;

		start = p+1 ;
		for ( p = start ; isspace( *p ) ; p++ ) ;
		if ( *p == NUL )
			return( NULL ) ;
		start = p ;
		if ( strncmp( start, line_id, line_id_len ) != 0 )
			return( NULL ) ;
		start += line_id_len ;		/* skip it */
	}

	for ( p = start ; isspace( *p ) ; p++ ) ;		/* skip any white-space */
	if ( *p != ':' )
		return( NULL ) ;
	for ( p++ ; isspace( *p ) ; p++ ) ;
	if ( *p == NUL )
		return( NULL ) ;
	return( p ) ;
}

	

/*
 * Get a line terminated by CR-LF.
 * Replace the CR-LF with NUL.
 */
PRIVATE char *get_line( sd, buf, bufsize )
	int				sd ;
	register char	*buf ;
	unsigned 		bufsize ;
{
	int				size ;
	register int	cc ;
	register char	*p ;
	register char	*s ;
	char				*func = "get_line" ;

	for ( p = buf, size = bufsize ; size > 0 ; p += cc, size -= cc )
	{
		cc = read( sd, p, size ) ;
		if ( cc == -1 )
			if ( errno == EINTR )
			{
				cc = 0 ;
				continue ;
			}
			else
			{
				msg( LOG_ERR, func, "read: %m" ) ;
				return( CHAR_NULL ) ;
			}
		if ( cc == 0 )
		{
			msg( LOG_ERR, func, "identd server reply missing ending CR-LF" ) ;
			return( CHAR_NULL ) ;
		}
		for ( s = p ; s < p + cc ; s++ )
			if ( *s == '\n' && s != buf && s[-1] == '\r' )
			{
				s[-1] = NUL ;
				return( buf ) ;
			}
	}
	msg( LOG_ERR, func, "Too much input from identity server" ) ;
	return( CHAR_NULL ) ;
}


char *idresult_explain( result )
	idresult_e result ;
{
	char *reason = "UNKNOWN" ;

	switch ( result )
	{
		case IDR_OK:
			reason = "no error" ;
			break ;

		case IDR_NOSERVER:
			reason = "no server" ;
			break ;

		case IDR_TIMEDOUT:
			reason = "timeout" ;
			break ;
		
		case IDR_ERROR:
			reason = "system error" ;
			break ;
		
		case IDR_RESPERR:
			reason = "error while receiving response" ;
			break ;
		
		case IDR_BADRESP:
			reason = "bad response" ;
			break ;
	}
	return( reason ) ;
}

