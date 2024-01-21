/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: connection.c,v 1.2 1995/09/10 15:42:38 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#ifdef BSD
#include <netinet/in.h>
#endif
#include <syslog.h>

#include "sio.h"

#include "connection.h"
#include "service.h"
#include "state.h"

char *inet_ntoa() ;
char *malloc() ;

void msg() ;
void out_of_memory() ;

#define NEW_CONN()					NEW( connection_s )
#define FREE_CONN( cop )			FREE( cop )


/*
 * Get a new connection request and initialize 'cp' appropriately
 */
PRIVATE status_e get_connection( sp, cp )
	register struct service *sp ;
	register connection_s *cp ;
{
	int sin_len = sizeof( cp->co_remote_address ) ;
	char *func = "get_connection" ;

	if ( SVC_ACCEPTS_CONNECTIONS( sp ) )
	{
		cp->co_descriptor = accept( SVC_FD( sp ),
												SA( &cp->co_remote_address ), &sin_len ) ;
		if ( cp->co_descriptor == -1 )
		{
			msg( LOG_ERR, func, "service %s, accept: %m", SVC_ID( sp ) ) ;
			return( FAILED ) ;
		}
		M_SET( cp->co_flags, COF_HAVE_ADDRESS ) ;
		M_SET( cp->co_flags, COF_NEW_DESCRIPTOR ) ;
	}
	else
	{
		if ( SVC_SOCKET_TYPE( sp ) == SOCK_DGRAM )
		{
			char t_ch ;

			/*
			 * This trick is done to get the remote address.
			 * select(2) guaranteed that we won't block on the recvfrom
			 */
			if ( recvfrom( SVC_FD( sp ), &t_ch, 1, MSG_PEEK,
										SA( &cp->co_remote_address ), &sin_len ) == -1 )
			{
				msg( LOG_ERR, func, "service %s, recvfrom: %m", SVC_ID( sp ) ) ;
				return( FAILED ) ;
			}
			M_SET( cp->co_flags, COF_HAVE_ADDRESS ) ;
		}
		cp->co_descriptor = SVC_FD( sp ) ;
	}
	return( OK ) ;
}



/*
 * Get a connection for the specified service and return a pointer
 * to a new connection_s
 */
connection_s *conn_new( sp )
	register struct service *sp ;
{
	connection_s				new_conn ;
	register connection_s	*cp ;
	char							*func = "conn_new" ;

	CLEAR( new_conn ) ;

	/*
	 * The reason we first get the connection and then allocate a
	 * 'connection_s' is because we want to always consume some input.
	 */
	if ( get_connection( sp, &new_conn ) == FAILED )
		return( NULL ) ;

	new_conn.co_state = CONN_OPEN ;
	new_conn.co_sp = sp ;
	SVC_HOLD( sp ) ;
	if ( SVC_WAITS( sp ) )
		svc_suspend( sp ) ;

	cp = NEW_CONN() ;
	if ( cp == CONN_NULL )
	{
		out_of_memory( func ) ;
		conn_cleanup( &new_conn ) ;
		conn_shutdown( &new_conn ) ;
		conn_free( &new_conn ) ;
		return( CONN_NULL ) ;
	}
	*cp = new_conn ;
	return( cp ) ;
}


/*
 * Close the connection descriptor if it is a new one
 */
void conn_close( cp )
	register connection_s *cp ;
{
	if ( cp->co_state == CONN_OPEN && 
										M_IS_SET( cp->co_flags, COF_NEW_DESCRIPTOR ) )
	{
		(void) close( cp->co_descriptor ) ;
		cp->co_state = CONN_CLOSED ;
	}
}


/*
 * Release the specified connection.
 * Certain actions may be performed before doing this:
 *		- invocation of the service shutdown function
 *		- drain of a single UDP packet if the socket type is SOCK_DGRAM
 */
void conn_free( cp )
	register connection_s *cp ;
{
	register struct service *sp = cp->co_sp ;
	int i ;
	void drain() ;

	if ( SVC_IS_SUSPENDED( sp ) )
		svc_resume( sp ) ;

	if ( cp->co_state == CONN_OPEN )
	{
		if ( M_IS_SET( cp->co_flags, COF_SHUTDOWN ) )
			svc_shutdown( sp, cp, (char **)NULL ) ;
		
		if ( M_IS_SET( cp->co_flags, COF_CLEANUP ) && 
									SVC_SOCKET_TYPE( sp ) == SOCK_DGRAM )
			drain( cp->co_descriptor ) ;
	}

	if ( SVC_RELE( sp ) == 0 )
		pset_remove( SERVICES( ps ), sp ) ;

	for ( i = 0 ; i < cp->co_alternative_count ; i++ )
	{
		struct service *asp = cp->co_alternatives[ i ] ;

		if ( SVC_RELE( asp ) == 0 )
			pset_remove( SERVICES( ps ), asp ) ;
	}

	conn_close( cp ) ;

	FREE_CONN( cp ) ;
}


status_e conn_add_alternative( cp, sp )
	register connection_s *cp ;
	register struct service *sp ;
{
	char *func = "conn_add_alternative" ;

	if ( sp == NULL )
		return( FAILED ) ;

	if ( cp->co_alternative_count >= MAX_ALTERNATIVES )
	{
		msg( LOG_ERR, func,
			"Cannot add alternative service %s to connection for service %s",
				SVC_ID( sp ), SVC_ID( cp->co_sp ) ) ;
		return( FAILED ) ;
	}

	if ( debug.on )
		msg( LOG_DEBUG, func,
			"Adding alternative service %s to connection of service %s",
				SVC_ID( sp ), SVC_ID( cp->co_sp ) ) ;

	cp->co_alternatives[ cp->co_alternative_count++ ] = sp ;
	SVC_HOLD( sp ) ;
	return( OK ) ;
}


/*
 * Start invoking alternative services starting from the next alternative one
 * until either we get a successful invocation or we run out of services
 */
status_e conn_start_alternative( cp )
	register connection_s *cp ;
{
	char *func = "conn_start_alternative" ;

	while ( cp->co_next_alternative < cp->co_alternative_count )
	{
		struct service *asp = cp->co_alternatives[ cp->co_next_alternative++ ] ;
		
		if ( svc_handle( asp, cp ) == OK )
		{
			if ( debug.on )
				msg( LOG_DEBUG, func,
					"Started alternative service %s", SVC_ID( asp ) ) ;
			return( OK ) ;
		}
	}
	return( FAILED ) ;
}


void conn_dump( cp, fd )
	register connection_s *cp ;
	int fd ;
{
	register unsigned u ;
	void tabprint() ;

	tabprint( fd, 1, "state = %s\n",
			( cp->co_state == CONN_CLOSED ) ? "CLOSED" : "OPEN" ) ;

	tabprint( fd, 1, "service = %s\n", SVC_ID( cp->co_sp ) ) ;
	tabprint( fd, 1, "descriptor = %d\n", cp->co_descriptor ) ;
	tabprint( fd, 1, "flags = %#x\n", cp->co_flags ) ;
	tabprint( fd, 1, "remote_address = %s,%d\n",
										inet_ntoa( cp->co_remote_address.sin_addr ),
										ntohs( cp->co_remote_address.sin_port ) ) ;
	tabprint( fd, 1, "Alternative services = " ) ;
	for ( u = 0 ; u < cp->co_alternative_count ; u++ )
		Sprint( fd, " %s", SVC_ID( cp->co_alternatives[ u ] ) ) ;
	Sputchar( fd, '\n' ) ;
	if ( cp->co_alternative_count > 0 )
		tabprint( fd, 1, "next alternative service = %s\n",
				SVC_ID( cp->co_alternatives[ cp->co_next_alternative ] ) ) ;
}

