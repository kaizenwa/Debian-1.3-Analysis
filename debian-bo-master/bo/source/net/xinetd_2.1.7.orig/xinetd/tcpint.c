/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: tcpint.c,v 1.2 1995/09/10 18:41:13 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#ifdef linux
#include <netinet/in.h>
#endif
#include <sys/time.h>
#include <syslog.h>
#include <signal.h>
#include <errno.h>

#include "config.h"
#include "int.h"
#include "access.h"
#include "defs.h"

char *inet_ntoa() ;
void msg() ;

typedef enum { S_OK, S_SERVER_ERR, S_CLIENT_ERR } stream_status_e ;

struct istream_private
{
	unsigned accepted_connections ;
} ;

#define SIP( p )						((struct istream_private *)(p))

static struct istream_private istream ;

PRIVATE void si_mux() ;
PRIVATE void si_exit() ;

static struct intercept_ops istream_ops =
	{
		si_mux,
		si_exit
	} ;

static struct intercept stream_intercept_state ;


struct intercept *si_init( serp )
	struct server *serp ;
{
	register struct intercept *ip = &stream_intercept_state ;

	ip->int_socket_type = SOCK_STREAM ;
	ip->int_priv = (void *) &istream ;
	ip->int_ops = &istream_ops ;
	int_init( ip, serp ) ;
	if ( (int) signal( SIGPIPE, SIG_IGN ) == -1 )
		int_fail( ip, "signal" ) ;
	return( ip ) ;
}


PRIVATE void si_exit()
{
	register struct intercept *ip = &stream_intercept_state ;
	
	if ( SIP( ip->int_priv )->accepted_connections == 0 )
		(void) accept( INT_REMOTE( ip ), SA( NULL ), INT_NULL ) ;
	int_exit( ip ) ;
}


PRIVATE void si_mux()
{
	register struct intercept	*ip = &stream_intercept_state ;
	fd_set							socket_mask ;
	int								mask_max ;
	psi_h								iter ;
	char								*func = "si_mux" ;
	PRIVATE status_e				handle_io() ;
	PRIVATE stream_status_e		tcp_local_to_remote() ;
	PRIVATE stream_status_e		tcp_remote_to_local() ;
	PRIVATE void					connection_request() ;

	FD_ZERO( &socket_mask ) ;
	FD_SET( INT_REMOTE( ip ), &socket_mask ) ;
	mask_max = INT_REMOTE( ip ) ;

	iter = psi_create( INT_CONNECTIONS( ip ) ) ;
	if ( iter == NULL )
	{
		msg( LOG_ERR, func, ES_NOMEM ) ;
		return ;
	}

	for ( ;; )
	{
		channel_s *chp ;
		fd_set read_mask ;
		int n_ready ;

		read_mask = socket_mask ;
		n_ready = int_select( mask_max+1, &read_mask ) ;

		if ( n_ready == -1 )
			return ;
		
      if ( FD_ISSET( INT_REMOTE( ip ), &read_mask ) )
      {
         connection_request( ip, &chp ) ;
			if ( chp != NULL )
			{
				FD_SET( chp->ch_local_socket, &socket_mask ) ;
				if ( chp->ch_local_socket > mask_max )
					mask_max = chp->ch_local_socket ;
				FD_SET( chp->ch_remote_socket, &socket_mask ) ;
				if ( chp->ch_remote_socket > mask_max )
					mask_max = chp->ch_remote_socket ;
			}
         if ( --n_ready == 0 )
            continue ;
      }

		for ( chp = CHP( psi_start(iter) ) ; chp ; chp = CHP( psi_next(iter) ) )
		{
			if ( FD_ISSET( chp->ch_local_socket, &read_mask ) )
			{
#ifdef DEBUG_TCPINT
				if ( debug.on )
					msg( LOG_DEBUG, func, "Input available on local socket %d", 
																			chp->ch_local_socket ) ;
#endif
				if ( handle_io( iter, chp,
								&socket_mask, tcp_local_to_remote ) == FAILED )
					return ;
				if ( --n_ready == 0 )
					break ;
			}

			if ( FD_ISSET( chp->ch_remote_socket, &read_mask ) )
			{
#ifdef DEBUG_TCPINT
				msg( LOG_DEBUG, func, "Input available on remote socket %d", 
																		chp->ch_remote_socket ) ;
#endif
				if ( handle_io( iter, chp,
								&socket_mask, tcp_remote_to_local ) == FAILED )
					return ;
				if ( --n_ready == 0 )
					break ;
			}
		}
	}
}


PRIVATE status_e handle_io( iter, chp, maskp, iofunc )
	psi_h					iter ;
	channel_s			*chp ;
	fd_set				*maskp ;
	stream_status_e	(*iofunc)() ;
{
	char *func = "handle_io" ;

	switch ( (*iofunc)( chp ) )
	{
		case S_SERVER_ERR:
			return( FAILED ) ;
		
		case S_CLIENT_ERR:

			if ( debug.on )
				msg( LOG_DEBUG, func,
					"Closing channel to %s,%d using sockets %d(l),%d(r)",
						inet_ntoa( chp->ch_from.sin_addr ),
							ntohs( chp->ch_from.sin_port ),
								chp->ch_local_socket, chp->ch_remote_socket ) ;

			FD_CLR( chp->ch_local_socket, maskp ) ;
			FD_CLR( chp->ch_remote_socket, maskp ) ;
			(void) close( chp->ch_remote_socket ) ;
			(void) close( chp->ch_local_socket ) ;
			psi_remove( iter ) ;
			FREE_CHANNEL( chp ) ;
			break ;
	}
	return( OK ) ;
}


PRIVATE void connection_request( ip, chpp )
	struct intercept	*ip ;
	channel_s			**chpp ;
{
	struct sockaddr_in	sin ;
	int						sin_len = sizeof( sin ) ;
	channel_s				*chp ;
	int						sd ;
	bool_int					addr_checked ;
	char						*func = "connection_request" ;

	*chpp = NULL ;

	if ( ( sd = accept( INT_REMOTE( ip ), SA( &sin ), &sin_len ) ) == -1 )
		return ;
	
	SIP( ip->int_priv )->accepted_connections++ ;

	if ( debug.on )
		msg( LOG_DEBUG, func, "connection request from %s,%d",
			inet_ntoa( sin.sin_addr ), ntohs( sin.sin_port ) ) ;

	chp = int_lookupconn( ip, &sin, &addr_checked ) ;
	if ( chp == NULL )
	{
		struct server	*serp	= INT_SERVER( ip ) ;
		struct service *sp	= SERVER_SERVICE( serp ) ;
		connection_s	*cop	= SERVER_CONNECTION( serp ) ;

		conn_setaddr( cop, &sin ) ;

		if ( INTERCEPT( ip ) )
		{
			mask_t check_mask ;
			access_e result ;
			
			M_OR( check_mask, MASK( CF_ADDRESS ), MASK( CF_TIME ) ) ;
			result = access_control( sp, cop, &check_mask ) ;

			if ( result != AC_OK )
			{
				svc_log_failure( sp, cop, result ) ;
				(void) close( sd ) ;
				return ;
			}
		}

		if ( ( chp = int_newconn( ip, &sin, sd ) ) == NULL )
		{
			(void) close( sd ) ;
			return ;
		}
		
		if ( ! addr_checked )
			svc_log_success( sp, cop, SERVER_PID( serp ) ) ;

#if defined( TCP_NODELAY )
		{
			int on = 1 ;

			(void) setsockopt( chp->ch_local_socket, IPPROTO_TCP,
										TCP_NODELAY, (char *) &on, sizeof( on ) ) ;
			(void) setsockopt( chp->ch_remote_socket, IPPROTO_TCP,
										TCP_NODELAY, (char *) &on, sizeof( on ) ) ;
		}
#endif	/* TCP_NODELAY */
		
		*chpp = chp ;
	}
	else
		msg( LOG_ERR, func,
			"Received another connection request from %s,%d",
				inet_ntoa( sin.sin_addr ), ntohs( sin.sin_port ) ) ;
}


PRIVATE stream_status_e tcp_local_to_remote( chp )
	channel_s *chp ;
{
	char buf[ DATAGRAM_SIZE ] ;
	int rcc, wcc ;
	char *p ;
	int left ;
	char *func = "tcp_local_to_remote" ;

	for ( ;; )
	{
		rcc = recv( chp->ch_local_socket, buf, sizeof( buf ), 0 ) ;
		if ( rcc == 0 )
			return( S_SERVER_ERR ) ;
		else if ( rcc == -1 )
		{
			if ( errno != EINTR )
			{
				msg( LOG_ERR, func, "recv: %m" ) ;
				return( S_SERVER_ERR ) ;
			}
		}
		else
			break ;
	}

	for ( p = buf, left = rcc ; left ; p += wcc, left -= wcc )
	{
		wcc = send( chp->ch_remote_socket, p, left, 0 ) ;
		if ( wcc == 0 )
			return( S_CLIENT_ERR ) ;
		else if ( wcc == -1 )
		{
			if ( errno == EINTR )
				wcc = 0 ;
			else
			{
				msg( LOG_ERR, func, "send: %m" ) ;
				return( S_CLIENT_ERR ) ;
			}
		}
	}

#ifdef DEBUG_TCPINT
	if ( debug.on )
		msg( LOG_DEBUG, func,
			"Transferred %d bytes from local socket %d to remote socket %d",
				rcc, chp->ch_local_socket, chp->ch_remote_socket ) ;
#endif

	return( S_OK ) ;
}


PRIVATE stream_status_e tcp_remote_to_local( chp )
	channel_s *chp ;
{
	char buf[ DATAGRAM_SIZE ] ;
	int rcc, wcc ;
	int left ;
	char *p ;
	char *func = "tcp_remote_to_local" ;

	for ( ;; )
	{
		rcc = recv( chp->ch_remote_socket, buf, sizeof( buf ), 0 ) ;
		if ( rcc == 0 )
			return( S_CLIENT_ERR ) ;
		else if ( rcc == -1 )
		{
			if ( errno != EINTR )
			{
				msg( LOG_ERR, func, "recv: %m" ) ;
				return( S_CLIENT_ERR ) ;
			}
		}
		else
			break ;
	}

	for ( p = buf, left = rcc ; left ; p += wcc, left -= wcc )
	{
		wcc = send( chp->ch_local_socket, p, left, 0 ) ;
		if ( wcc == 0 )
			return( S_SERVER_ERR ) ;
		else if ( wcc == -1 )
			if ( errno == EINTR )
				rcc = 0 ;
			else
			{
				msg( LOG_ERR, func, "send: %m" ) ;
				return( S_SERVER_ERR ) ;
			}
	}

#ifdef DEBUG_TCPINT
	if ( debug.on )
		msg( LOG_DEBUG, func,
			"Transferred %d bytes from remote socket %d to local socket %d",
				rcc, chp->ch_remote_socket, chp->ch_local_socket ) ;
#endif

	return( S_OK ) ;
}

