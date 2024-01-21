/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: builtins.c,v 1.5 1995/09/10 18:41:13 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <time.h>
#include <syslog.h>

#include "str.h"

#include "config.h"
#include "state.h"
#include "defs.h"
#include "builtin.h"
#include "sconf.h"
#include "server.h"

extern int errno ;

time_t time() ;

void msg() ;
status_e write_buf() ;

#define BUFFER_SIZE					1024

PRIVATE void stream_echo() ;
PRIVATE void dgram_echo() ;
PRIVATE void stream_discard() ;
PRIVATE void dgram_discard() ;
PRIVATE void stream_time() ;
PRIVATE void dgram_time() ;
PRIVATE void stream_daytime() ;
PRIVATE void dgram_daytime() ;
PRIVATE void stream_chargen() ;
PRIVATE void dgram_chargen() ;
PRIVATE void stream_servers() ;
PRIVATE void stream_services() ;


static struct builtin_service builtin_services[] =
	{
		{ "echo",		SOCK_STREAM,	{ stream_echo, 		FORK  	} },
		{ "echo",		SOCK_DGRAM,		{ dgram_echo,			NO_FORK	} },
		{ "discard",	SOCK_STREAM,	{ stream_discard,		FORK		} },
		{ "discard",	SOCK_DGRAM,		{ dgram_discard,		NO_FORK	} },
		{ "time",		SOCK_STREAM,	{ stream_time,			NO_FORK	} },
		{ "time",		SOCK_DGRAM,		{ dgram_time,			NO_FORK	} },
		{ "daytime",	SOCK_STREAM,	{ stream_daytime,		NO_FORK	} },
		{ "daytime",	SOCK_DGRAM,		{ dgram_daytime,		NO_FORK	} },
		{ "chargen",	SOCK_STREAM,	{ stream_chargen,		FORK		} },
		{ "chargen",	SOCK_DGRAM,		{ dgram_chargen,		NO_FORK	} },
		{ "servers",	SOCK_STREAM,	{ stream_servers,		FORK		} },
		{ "services",	SOCK_STREAM,	{ stream_services,	FORK		} },
		{ NULL }
	} ;


builtin_s *builtin_find( service_name, type )
	char	*service_name ;
	int	type ;
{
	register builtin_s	*bsp ;
	char						*func = "builtin_find" ;

	if ( bsp = builtin_lookup( builtin_services, service_name, type ) )
		return( bsp ) ;
	
	msg( LOG_ERR, func, "No such internal service: %s", service_name ) ;
	return( NULL ) ;
}


builtin_s *builtin_lookup( services, service_name, type )
	struct builtin_service	services[] ;
	register char				*service_name ;
	register int				type ;
{
	register struct builtin_service *bsp ;

	for ( bsp = services ; bsp->bs_name != NULL ; bsp++ )
		if ( EQ( bsp->bs_name, service_name ) && bsp->bs_socket_type == type )
			return( &bsp->bs_handle ) ;
	return( NULL ) ;
}


/*
 * The rest of this file contains the functions that implement the 
 * builtin services
 */


PRIVATE void stream_echo( serp )
	struct server *serp ;
{
	char				buf[ BUFFER_SIZE ] ;
	register int	cc ;
	int				descriptor = SERVER_FD( serp ) ;

	for ( ;; )
	{
		cc = read( descriptor, buf, sizeof( buf ) ) ;
		if ( cc == 0 )
			break ;
		if ( cc == -1 )
			if ( errno == EINTR )
				continue ;
			else
				break ;

		if ( write_buf( descriptor, buf, cc ) == FAILED )
			break ;
	}
}


PRIVATE void dgram_echo( serp )
	struct server *serp ;
{
	char						buf[ DATAGRAM_SIZE ] ;
	struct sockaddr_in	sin ;
	int						cc ;
	int						sin_len = sizeof( sin ) ;
	int						descriptor = SERVER_FD( serp ) ;

	cc = recvfrom( descriptor, buf, sizeof( buf ), 0, SA( &sin ), &sin_len ) ;
	if ( cc != -1 )
		(void) sendto( descriptor, buf, cc, 0, SA( &sin ), sizeof( sin ) ) ;
}


PRIVATE void stream_discard( serp )
	struct server *serp ;
{
	char				buf[ BUFFER_SIZE ] ;
	register int	cc ;

	for ( ;; )
	{
		cc = read( SERVER_FD( serp ), buf, sizeof( buf ) ) ;
		if ( cc == 0 || cc == -1 && errno != EINTR )
			break ;
	}
}


PRIVATE void dgram_discard( serp )
	struct server *serp ;
{
	char buf[ 1 ] ;

	(void) recv( SERVER_FD( serp ), buf, sizeof( buf ), 0 ) ;
}



/*
 * Generate the current time using the SMTP format:
 *		02 FEB 1991 12:31:42 MST
 *
 * The result is placed in buf.
 * buflen is a value-result parameter. It indicates the size of
 * buf and on exit it has the length of the string placed in buf.
 */
PRIVATE void daytime_protocol( buf, buflen )
	char	*buf ;
	int	*buflen ;
{
	static char *month_name[] =
		{
			"JAN", "FEB", "MAR", "APR", "MAY", "JUN",
			"JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
		} ;
	time_t		now ;
	struct tm	*tmp ;
	int			size = *buflen ;
	
#ifdef linux
	char tzname[2];

	(void) time( &now ) ;
	tmp = localtime( &now ) ;
	mktime(tmp);
        strx_print( buflen, buf, size,
                "%02d %s %d %02d:%02d:%02d %c%cT\r\n",
                tmp->tm_mday, month_name[ tmp->tm_mon ], 1900 + tmp->tm_year,
                tmp->tm_hour, tmp->tm_min, tmp->tm_sec, tzname[0], tzname[1] ) ;
#else
	(void) time( &now ) ;
	tmp = localtime( &now ) ;
	strx_print( buflen, buf, size,
		"%02d %s %d %02d:%02d:%02d %s\r\n",
		tmp->tm_mday, month_name[ tmp->tm_mon ], 1900 + tmp->tm_year,
		tmp->tm_hour, tmp->tm_min, tmp->tm_sec, tmp->tm_zone ) ;
#endif	/* linux */
}


PRIVATE void stream_daytime( serp )
	struct server *serp ;
{
	char	time_buf[ BUFFER_SIZE ] ;
	int	buflen = sizeof( time_buf ) ;

	daytime_protocol( time_buf, &buflen ) ;
	(void) write_buf( SERVER_FD( serp ), time_buf, buflen ) ;
}


PRIVATE void dgram_daytime( serp )
	struct server *serp ;
{
	char						time_buf[ BUFFER_SIZE ] ;
	struct sockaddr_in	sin ;
	int						sin_len		= sizeof( sin ) ;
	int						buflen		= sizeof( time_buf ) ;
	int						descriptor	= SERVER_FD( serp ) ;

	if ( recvfrom( descriptor, time_buf, sizeof( time_buf ), 0,
				SA( &sin ), &sin_len ) == -1 )
		return ;

	daytime_protocol( time_buf, &buflen ) ;
	
	(void) sendto( descriptor, time_buf, buflen, 0, SA(&sin), sizeof( sin ) ) ;
}

#if defined(linux) || defined(__FreeBSD__) || defined(__bsdi__) || defined(__NetBSD__)
#define TIME_OFFSET			2208988800UL
#else
#define TIME_OFFSET			2208988800
#endif

/*
 * We always report the time using network-byte-order
 */
PRIVATE void time_protocol( timep )
	register unsigned long *timep ;
{
	time_t now ;

	(void) time( &now ) ;
	*timep = now + TIME_OFFSET ;
	*timep = htonl( *timep ) ;
}


PRIVATE void stream_time( serp )
	struct server *serp ;
{
	unsigned long now ;

	time_protocol( &now ) ;
	(void) write_buf( SERVER_FD( serp ), (char *) &now, sizeof( now ) ) ;
}


PRIVATE void dgram_time( serp )
	struct server *serp ;
{
	char						buf[ 1 ] ;
	unsigned long			now ;
	struct sockaddr_in	sin ;
	int						sin_len	= sizeof( sin ) ;
	int						fd			= SERVER_FD( serp ) ;

	if ( recvfrom( fd, buf, sizeof( buf ), 0, SA( &sin ), &sin_len ) == -1 )
		return ;

	time_protocol( &now ) ;
	(void) sendto( fd, (char *) &now, sizeof( now ), 0, SA( &sin ), sin_len ) ;
}


#define ASCII_PRINTABLE_CHARS				94
#define LINE_LENGTH							72

#define RING_BUF_SIZE						ASCII_PRINTABLE_CHARS + LINE_LENGTH

static char ring_buf[ RING_BUF_SIZE ] ;
static char *ring ;


#define ASCII_START				( ' ' + 1 )
#define ASCII_END					126

#define min( a, b )				((a)<(b) ? (a) : (b))

PRIVATE char *generate_line( buf, len )
	char	*buf ;
	int	len ;
{
	int line_len = min( LINE_LENGTH, len-2 ) ;

	if ( line_len < 0 )
		return( NULL ) ;

	if ( ring == NULL )
	{
		register char ch ;
		register char *p ;

		for ( p = ring_buf, ch = ASCII_START ;
							p < &ring_buf[ RING_BUF_SIZE ] ; p++ )
		{
			*p = ch++ ;
			if ( ch == ASCII_END )
				ch = ASCII_START ;
		}
		ring = ring_buf ;
	}
	(void) memcpy( buf, ring, line_len ) ;
	buf[ line_len   ] = '\r' ;
	buf[ line_len+1 ] = '\n' ;

	ring++ ;
	if ( &ring_buf[ RING_BUF_SIZE ] - ring < LINE_LENGTH )
		ring = ring_buf ;
	return( buf ) ;
}


PRIVATE void stream_chargen( serp )
	struct server *serp ;
{
	char	line_buf[ LINE_LENGTH+2 ] ;
	int	descriptor = SERVER_FD( serp ) ;

	(void) shutdown( descriptor, 0 ) ;
	for ( ;; )
	{
		if ( generate_line( line_buf, sizeof( line_buf ) ) == NULL )
			break ;
		if ( write_buf( descriptor, line_buf, sizeof( line_buf ) ) == FAILED )
			break ;
	}
}


PRIVATE void dgram_chargen( serp )
	struct server *serp ;
{
	char						buf[ BUFFER_SIZE ] ;
	register char			*p ;
	register int			len ;
	struct sockaddr_in	sin ;
	int						sin_len	= sizeof( sin ) ;
	int						fd			= SERVER_FD( serp ) ;
	register int			left		= sizeof( buf ) ;

	if ( recvfrom( fd, buf, sizeof( buf ), 0, SA( &sin ), &sin_len ) == -1 )
		return ;

#if BUFFER_SIZE < LINE_LENGTH+2
	bad_variable = 1 ;		/* this will cause a compilation error */
#endif

	for ( p = buf ; left > 2 ; left -= len, p += len )
	{
		len = min( LINE_LENGTH+2, left ) ;
		if ( generate_line( p, len ) == NULL )
			break ;
	}
	(void) sendto( fd, buf, p-buf, 0, SA( &sin ), sin_len ) ;
}


PRIVATE void stream_servers( this_serp )
	struct server *this_serp ;
{
	register unsigned		u ;
	int						descriptor = SERVER_FD( this_serp ) ;

	for ( u = 0 ; u < pset_count( SERVERS( ps ) ) ; u++ )
	{
		struct server *serp = SERP( pset_pointer( SERVERS( ps ), u ) ) ;

		/*
		 * We cannot report any useful information about this server because
		 * the data in the server struct are filled by the parent.
		 */
		if ( serp == this_serp )
			continue ;

		server_dump( serp, descriptor ) ;
	}
}


PRIVATE void stream_services( serp )
	struct server *serp ;
{
	register unsigned u ;
	int fd = SERVER_FD( serp ) ;

	for ( u = 0 ; u < pset_count( SERVICES( ps ) ) ; u++ )
	{
		int cc ;
		char buf[ BUFFER_SIZE ] ;
		struct service_config *scp ;
		
		scp = SVC_CONF( SP( pset_pointer( SERVICES( ps ), u ) ) ) ;

		strx_print( &cc, buf, sizeof( buf ), "%s %s %d\n",
			SC_NAME( scp ), SC_PROTONAME( scp ), SC_PORT( scp ) ) ;
			
		if ( write_buf( fd, buf, cc ) == FAILED )
			break ;
	}
}

