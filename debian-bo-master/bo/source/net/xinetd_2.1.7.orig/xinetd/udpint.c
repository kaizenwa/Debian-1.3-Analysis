/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: udpint.c,v 1.3 1995/09/10 18:41:13 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#if defined(BSD) || defined(linux)
#include <netinet/in.h>
#endif
#include <netdb.h>
#include <sys/time.h>
#include <syslog.h>
#include <errno.h>

#include "access.h"
#include "defs.h"
#include "int.h"

/*
 * Datagrams greater than this will be truncated
 */
#define MAX_DATAGRAM_SIZE			( 32 * 1024 )

char *inet_ntoa() ;

void msg() ;

struct packet
{
	struct sockaddr_in	from ;
	char						*data ;
	int						size ;
} ;

typedef struct packet packet_s ;


struct idgram_private
{
	unsigned received_packets ;
} ;

#define IDP( p )					((struct idgram_private *)(p))


static struct idgram_private idgram ;

PRIVATE void di_mux() ;
PRIVATE void di_exit() ;

static struct intercept_ops idgram_ops =
	{
		di_mux,
		di_exit
	} ;


static struct intercept dgram_intercept_state ;


struct intercept *di_init( serp )
	struct server *serp ;
{
	register struct intercept *ip = &dgram_intercept_state ;
	
	ip->int_socket_type = SOCK_DGRAM ;
	ip->int_priv = (void *) &idgram ;
	ip->int_ops = &idgram_ops ;
	int_init( ip, serp ) ;
	return( ip ) ;
}


PRIVATE void di_exit()
{
	register struct intercept *ip = &dgram_intercept_state ;
	void drain() ;

	if ( IDP( ip->int_priv )->received_packets == 0 )
		drain( INT_REMOTE( ip ) ) ;
	int_exit( ip ) ;
}


/*
 * Returns only if there is an I/O error while communicating with the server
 */
PRIVATE void di_mux()
{
	register struct intercept	*ip = &dgram_intercept_state ;
	fd_set							socket_mask ;
	int								mask_max ;
	PRIVATE void					udp_remote_to_local() ;
	PRIVATE status_e				udp_local_to_remote() ;

	FD_ZERO( &socket_mask ) ;
	FD_SET( INT_REMOTE( ip ), &socket_mask ) ;
	mask_max = INT_REMOTE( ip ) ;

	for ( ;; )
	{
		register unsigned u ;
		channel_s *chp ;
		fd_set read_mask ;
		int n_ready ;

		read_mask = socket_mask ;
		n_ready = int_select( mask_max+1, &read_mask ) ;

		if ( n_ready == -1 )
			return ;
		
		if ( FD_ISSET( INT_REMOTE( ip ), &read_mask ) )
		{
			udp_remote_to_local( ip, &chp ) ;
			if ( chp != NULL )
			{
				FD_SET( chp->ch_local_socket, &socket_mask ) ;
				if ( chp->ch_local_socket > mask_max )
					mask_max = chp->ch_local_socket ;
			}
			if ( --n_ready == 0 )
				continue ;
		}

		for ( u = 0 ; u < pset_count( INT_CONNECTIONS( ip ) ) ; u++ )
		{
			chp = CHP( pset_pointer( INT_CONNECTIONS( ip ), u ) ) ;

			if ( FD_ISSET( chp->ch_local_socket, &read_mask ) )
			{
				if ( udp_local_to_remote( chp ) == FAILED )
					return ;
				if ( --n_ready == 0 )
					break ;
			}
		}
	}
}


/*
 * Read data from the remote socket and send it to the appropriate local 
 * socket.
 * If this is a new connection, insert it in the connection table and
 * place its handle in *chpp.
 */
PRIVATE void udp_remote_to_local( ip, chpp )
	struct intercept	*ip ;
	channel_s			**chpp ;
{
	char					buf[ MAX_DATAGRAM_SIZE ] ;
	packet_s				packet ;
	channel_s			*chp ;
	bool_int				addr_checked ;
	PRIVATE void		send_data() ;
	PRIVATE status_e	get_incoming_packet() ;

	*chpp = CHANNEL_NULL ;

	packet.data = buf ;
	packet.size = sizeof( buf ) ;
	if ( get_incoming_packet( ip, &packet ) == FAILED )
		return ;

	chp = int_lookupconn( ip, &packet.from, &addr_checked ) ;
	if ( chp == CHANNEL_NULL )
	{
		struct server		*serp = INT_SERVER( ip ) ;
		struct service 	*sp = SERVER_SERVICE( serp ) ;
		connection_s		*cop = SERVER_CONNECTION( serp ) ;

		if ( ( chp = int_newconn( ip, &packet.from, INT_REMOTE( ip ) ) ) == NULL )
			return ;

		conn_setaddr( cop, &packet.from ) ;		/* for logging */

		if ( INTERCEPT( ip ) )
		{
			mask_t check_mask ;
			access_e result ;

			M_OR( check_mask, MASK( CF_ADDRESS ), MASK( CF_TIME ) ) ;
			result = access_control( sp, cop, &check_mask ) ;

			if ( result != AC_OK )
			{
				svc_log_failure( sp, cop, result ) ;
				chp->ch_state = BAD_CHANNEL ;
				return ;
			}
		}
		
		/*
		 * Since we don't distinguish ports, there is no point to log
		 * another successful attempt from the same address
		 */
		if ( ! addr_checked )
			svc_log_success( sp, cop, SERVER_PID( serp ) ) ;
			
		*chpp = chp ;
	}
	else if ( chp->ch_state == BAD_CHANNEL )
		return ;
	
#ifdef DEBUG_UDPINT
	if ( debug.on )
		msg( LOG_DEBUG, "udp_remote_to_local",
					"sending %d bytes to server on port %d",
							packet.size, ntohs( INT_LOCALADDR( ip )->sin_port ) ) ;
#endif

	send_data( chp->ch_local_socket,
			packet.data, packet.size, SOCKADDRIN_NULL ) ;
}


/*
 * Send the data in buf to destination addr using the socket sd.
 * If addr is NULL, use the default socket destination
 */
PRIVATE void send_data( sd, buf, len, addr )
	int						sd ;
	char						*buf ;
	int						len ;
	struct sockaddr_in	*addr ;
{
	char	*p ;
	int	left ;
	int	cc ;
	char	*func = "send_data" ;

	for ( p = buf, left = len ; left > 0 ; left -= cc, p+= cc )
	{
		if ( addr == SOCKADDRIN_NULL )
			cc = send( sd, p, left, 0 ) ;
		else
			cc = sendto( sd, p, left, 0, SA( addr ), sizeof( *addr ) ) ;

		if ( cc == -1 )
			if ( errno == EINTR )
			{
				cc = 0 ;
				continue ;
			}
			else
			{
				msg( LOG_ERR, func, "%s: %m", addr ? "sendto" : "send" ) ;
				return ;
			}
	}
}


PRIVATE status_e get_incoming_packet( ip, pp )
	struct intercept *ip ;
	packet_s *pp ;
{
	int from_len ;
	char *func = "get_incoming_packet" ;

	for ( ;; )
	{
		int cc ;

		from_len = sizeof( pp->from ) ;
		cc = recvfrom( INT_REMOTE( ip ), pp->data, pp->size,
												0, SA( &pp->from ), &from_len ) ;
		if ( cc == -1 )
		{
			if ( errno != EINTR )
			{
				msg( LOG_ERR, func, "recvfrom error: %m" ) ;
				return( FAILED ) ;
			}
		}
		else if ( cc == 0 )
			return( FAILED ) ;
		else
		{
			pp->size = cc ;
			IDP( ip->int_priv )->received_packets++ ;
			break ;
		}
	}

	if ( from_len == 0 )
	{
		msg( LOG_ERR, func, "incoming packet had 0 length address" ) ;
		return( FAILED ) ;
	}
	
#ifdef DEBUG_UDPINT
	if ( debug.on )
		msg( LOG_DEBUG, func, "Received %d bytes from address: %s,%d",
			pp->size,
				inet_ntoa( pp->from.sin_addr ), ntohs( pp->from.sin_port ) ) ;
#endif

	return( OK ) ;
}



PRIVATE status_e udp_local_to_remote( chp )
	channel_s *chp ;
{
	char	buf[ MAX_DATAGRAM_SIZE ] ;
	int	cc ;
	char	*func = "udp_local_to_remote" ;

	for ( ;; )
	{
		cc = recv( chp->ch_local_socket, buf, sizeof( buf ), 0 ) ;
	
		if ( cc == -1 )
		{
			if ( errno != EINTR ) 
			{
				msg( LOG_ERR, func, "recv from daemon: %m" ) ;
				return( FAILED ) ;
			}
		}
		else if ( cc == 0 )
			return( FAILED ) ;
		else
			break ;
	}
	
#ifdef DEBUG_UDPINT
	if ( debug.on )
		msg( LOG_DEBUG, func, "sending %d bytes to address %s,%d",
			cc, inet_ntoa( chp->ch_from.sin_addr ), ntohs( chp->ch_from.sin_port ) ) ;
#endif

	send_data( chp->ch_remote_socket, buf, cc, &chp->ch_from ) ;
	return( OK ) ;
}

