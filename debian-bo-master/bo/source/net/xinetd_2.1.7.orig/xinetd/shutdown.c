/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: shutdown.c,v 1.1 1996/01/24 19:29:19 chuck Exp $" ;

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <pwd.h>
#include <string.h>
#include <syslog.h>

#include "sio.h"
#include "str.h"

#include "defs.h"

char *malloc() ;
char *crypt() ;

void msg() ;

typedef enum { SD_LOGIN, SD_SHELL, SD_EXEC, SD_FINGER } shutdown_e ;

struct shutdown_function
{
	char			*sf_service ;
	voidfunc 	sf_func ;
	char			*sf_function_name ;
} ;


PRIVATE void rlogin_shutdown() ;
PRIVATE void rexec_shutdown() ;
PRIVATE void rsh_shutdown() ;
PRIVATE void finger_shutdown() ;

static struct shutdown_function shutdown_functions[] =
   {
      { "login",		rlogin_shutdown,	"rlogin_shutdown"	},
		{ "shell",		rsh_shutdown,		"rsh_shutdown"		},
		{ "exec",		rexec_shutdown,	"rexec_shutdown"	},
		{ "finger",		finger_shutdown,	"finger_shutdown"	},
      { CHAR_NULL }
   } ;


typedef enum
	{
		RS_OK, RS_EOF, RS_IOERR, RS_NOMEM, RS_TOOLONG, RS_BADCONN
	} read_status_e ;

static struct name_value read_status_names[] =
	{
		{ "Error: out of memory",		(int) RS_NOMEM		},
		{ "Error: input too long",		(int) RS_TOOLONG	},
		{ "Error: I/O",					(int) RS_IOERR  	},
		{ "Error: end-of-file",			(int) RS_EOF		},
		{ "Error: bad connection",		(int) RS_BADCONN	},
		{ CHAR_NULL,						1						},
		{ "UNKNOWN",						0						}
	} ;

#define rs_explain( s )			nv_get_name( read_status_names, (int) (s) )


/*
 * NOTE: All shutdown functions assume that the process will exit
 * 		very soon, so they don't bother to deallocate any malloc'ed
 *			memory. In particular, it is not guaranteed that the memory 
 *			returned by these functions will point to malloc'ed memory
 */


/*
 * Locate and return the shutdown function for the given service
 */
voidfunc get_shutdown_by_name( service ) 
	register char *service ;
{
	register struct shutdown_function *sfp ;

	for ( sfp = &shutdown_functions[ 0 ] ; sfp->sf_service ; sfp++ )
		if ( EQ( service, sfp->sf_service ) )
			return( sfp->sf_func ) ;
	return( NULL ) ;
}


char *get_shutdown_by_addr( func )
	register voidfunc func ;
{
	register struct shutdown_function *sfp ;

	for ( sfp = &shutdown_functions[ 0 ] ; sfp->sf_service ; sfp++ )
		if ( func == sfp->sf_func )
			break ;
	return( sfp->sf_function_name ) ;
}


/*
 * Read a string of length at most max_len from socket sd.
 * The string is placed in buf (if buf is not NULL). If buf
 * is NULL, the input is drained.
 * The input string is always read even if the allowed length
 * is exceeded (the extra characters are ignored).
 * max_len is the maximum length of the input string excluding the
 * terminating NUL.
 * The actual length is returned in lenp.
 */
PRIVATE read_status_e read_string( sd, max_len, buf, lenp )
	int					sd ;
	register unsigned max_len ;
	char					*buf ;
	unsigned				*lenp ;
{
	char						c ;
	register char			*p ;
	int						cc ;
	read_status_e			read_status ;
	register status_e		status		= OK ;
	register int			len			= 0 ;
	int						drain_input = ( buf == NULL ) ;

	if ( drain_input )
	{
		status = FAILED ;
		p = &c ;
	}
	else
	{
		p = buf ;
		status = OK ;
	}

	for ( ;; )
	{
		/*
		 * Keep reading characters one by one (ugly !!!)
		 * If status == FAILED, don't store them
		 * Interesting side-effect: since we stop increasing p in
		 * this case, *p will be used for storing all subsequent input
		 * until the NUL. So we get a NUL terminated string with
		 * the extra characters ignored.
		 */
		if ( len > max_len )
		{
			status = FAILED ;
			read_status = RS_TOOLONG ;
		}

		/*
		 * If an I/O error occurs, end the loop
		 */
		if ( ( cc = read( sd, p, 1 ) ) != 1 )
		{
			if ( cc == -1 )
				if ( errno == EINTR )
					continue ;
				else
					read_status = RS_IOERR ;
			else
				read_status = RS_EOF ;
			status = FAILED ;
			break ;
		}
		if ( *p == NUL )
			break ;
		if ( status == OK )
		{
			len++ ;
			p++ ;
		}
	}
	if ( status == OK )
	{
		*lenp = len ;
		read_status = RS_OK ;
	}
	return( drain_input ? RS_OK : read_status ) ;
}


/*
 * Allocates a single buffer big enough to hold <count> strings whose
 * maximum lengths are given in the <limits> array
 * The pointers in the <strings> array will point to the part
 * of the buffer for each string.
 */
PRIVATE char *setup( count, limits, strings )
	register unsigned		count ;
	unsigned					limits[] ;
	char						*strings[] ;
{
	register unsigned		total_len ;
	register unsigned		index ;
	register unsigned		u ;
	char						*buf ;

	for ( u = 0, total_len = 0 ; u < count ; u++ )
		total_len += limits[ u ] ;
	
	buf = malloc( total_len + count ) ;		/* count the NULs */
	if ( buf == NUL )
		return( NULL ) ;
	
	for ( u = 0, index = 0 ; u < count ; u++ )
	{
		strings[ u ] = &buf[ index ] ;
		index += limits[ u ] + 1 ;
	}
	return( buf ) ;
}



PRIVATE int connect_back( id, sd, port )
	shutdown_e			id ;
	int					sd ;
	unsigned short 	port ;
{
	int						new_sd ;
	struct sockaddr_in	sin ;
	int						sin_len	= sizeof( sin ) ;
	char						*func		= "connect_back" ;

	/*
	 * Get the remote address
	 */
	if ( getpeername( sd, SA( &sin ), &sin_len ) == -1 )
	{
		if ( debug.on )
			msg( LOG_DEBUG, func, "id=%d, getpeername: %m", (int) id ) ;
		return( -1 ) ;
	}

	/*
	 * Get a socket for the new connection.
	 * For the shell service, the socket must have a proviliged local port
	 */
	if ( id == SD_SHELL )
	{
		int local_port = IPPORT_RESERVED - 1 ;
		unsigned short client_port = ntohs( sin.sin_port ) ;

		if ( ! ( client_port >= IPPORT_RESERVED/2 &&
											client_port < IPPORT_RESERVED ) )
		return( -1 ) ;

		if ( ( new_sd = rresvport( &local_port ) ) == -1 )
			return( -1 ) ;
	}
	else if ( id == SD_EXEC )
	{
		if ( ( new_sd = socket( AF_INET, SOCK_STREAM, 0 ) ) == -1 )
			return( -1 ) ;
	}
	else
		return( -1 ) ;

	sin.sin_port = htons( port ) ;
	sin.sin_family = AF_INET ;
	if ( connect( new_sd, SA( &sin ), sizeof( sin ) ) == -1 )
	{
		if ( debug.on )
			msg( LOG_DEBUG, func, "connect: %m (port=%d)", port ) ;
		(void) close( new_sd ) ;
		return( -1 ) ;
	}
	return( new_sd ) ;
}


PRIVATE status_e rservices_common( id, sd, nargs, 
										limits, strings, total_lenp, pp )
	shutdown_e	id ;				/* service id 											*/
	unsigned		nargs ;			/* number of expected args. args are 			*/
										/* NUL-terminated strings							*/
	unsigned		limits[] ;		/* max length of each arg							*/
	char			*strings[] ;	/* array of arg pointers							*/
	unsigned		*total_lenp ;	/* total length of args								*/
	char			**pp ;			/* pointer to string to put error message		*/
{
	read_status_e	rs ;
	char				*store_buf ;
	unsigned			total_len ;
	unsigned			len ;
	int				i ;
	char				*func = "rservices_common" ;

	store_buf = setup( nargs, limits, strings ) ;
	if ( store_buf == NULL )
	{
		*pp = rs_explain( RS_NOMEM ) ;
		return( FAILED ) ;
	}

	for ( i = 0, total_len = 0 ; i < nargs ; i++ )
	{
		rs = read_string( sd, limits[ i ], strings[ i ], &len ) ;
		if ( rs != RS_OK )
		{
			*pp = rs_explain( rs ) ;
			return( FAILED ) ;
		}

		if ( i == 0 && ( id == SD_SHELL || id == SD_EXEC ) )
		{
			unsigned short port = atoi( strings[ 0 ] ) ;

			if ( debug.on )
				msg( LOG_DEBUG, func, "port for new connection = %d", port ) ;

			if ( port != 0 && connect_back( id, sd, port ) == -1 )
			{
				*pp = rs_explain( RS_BADCONN ) ;
				return( FAILED ) ;
			}
		}
		else
			total_len += len ;
	}
	*total_lenp = total_len ;
	return( OK ) ;
}


#define RLOGIN_ARGS 		4

PRIVATE void rlogin_shutdown( sd, pp )
	int sd ;
	char **pp ;
{
	char					*print_buf ;
	unsigned				print_buf_size ;
	unsigned				total_len ;
	char					*strings[ RLOGIN_ARGS ] ;
	static unsigned	limits[ RLOGIN_ARGS ] = { 0, 16, 16, 4096 } ;

	(void) write( sd, "", 1 ) ;

	if ( pp == NULL )
		return ;

	if ( rservices_common( SD_LOGIN, sd,
				RLOGIN_ARGS, limits, strings, &total_len, pp ) == FAILED )
		return ;

	print_buf_size = total_len + 100 ;
	print_buf = malloc( print_buf_size ) ;
	if ( print_buf != NULL )
		*pp = strx_sprint( print_buf, print_buf_size,
						"remote_user=%s local_user=%s tty=%s",
							strings[ 1 ], strings[ 2 ], strings[ 3 ] ) ;
	else
		*pp = rs_explain( RS_NOMEM ) ;
}


	
#define REXEC_ARGS			4
#define SALT_LEN				2

PRIVATE void rexec_shutdown( sd, pp )
	int sd ;
	char **pp ;
{
	char					*print_buf ;
	unsigned				print_buf_size ;
	char					*password ;
	char					salt[ SALT_LEN ] ;
	struct passwd		*pw ;
	char					*verify ;
	unsigned				total_len ;
	char					*strings[ REXEC_ARGS ] ;
	char					error_indication		= '\1' ;
	char					*error_message			= "Permission denied.\n" ;
	static unsigned	limits[ REXEC_ARGS ] = { 5, 16, 16, 4096 } ;

	(void) write( sd, &error_indication, 1 ) ;
	(void) write( sd, error_message, strlen( error_message ) + 1 ) ;
	
	if ( pp == NULL )
		return ;

	if ( rservices_common( SD_EXEC, sd,
				REXEC_ARGS, limits, strings, &total_len, pp ) == FAILED )
		return ;

	/*
	 * Verify the password
	 */
	password = strings[ 2 ] ;
	pw = getpwnam( strings[ 1 ] ) ;
	if ( pw != NULL )
	{
		strncpy( salt, pw->pw_passwd, SALT_LEN )[ SALT_LEN ] = NUL ;
		if ( EQ( crypt( password, salt ), pw->pw_passwd ) )
			verify = "ok" ;
		else
			verify = "failed" ;
	}
	else
		verify = "baduser" ;

	str_fill( password, ' ' ) ;		/* clear the password */

	print_buf_size = total_len + 100 ;
	print_buf = malloc( print_buf_size ) ;
	if ( print_buf != NULL )
		*pp = strx_sprint( print_buf, print_buf_size, 
							"remote_user=%s verify=%s command=%s",
											strings[ 1 ], verify, strings[ 3 ] ) ;
	else
		*pp = rs_explain( RS_NOMEM ) ;
}



#define RSH_ARGS			4

PRIVATE void rsh_shutdown( sd, pp )
	int sd ;
	char **pp ;
{
	char					*print_buf ;
	unsigned				print_buf_size ;
	unsigned				total_len ;
	char					*strings[ RSH_ARGS ] ;
	char					error_indication		= '\1' ;
	char					*error_message			= "Permission denied.\n" ;
	static unsigned	limits[ RSH_ARGS ]	= { 5, 16, 16, 4096 } ;

	(void) write( sd, &error_indication, 1 ) ;
	(void) write( sd, error_message, strlen( error_message ) + 1 ) ;

	if ( pp == NULL )
		return ;
	
	if ( rservices_common( SD_SHELL, sd,
					RSH_ARGS, limits, strings, &total_len, pp ) == FAILED )
		return ;
	
	print_buf_size = total_len + 100 ;
	print_buf = malloc( print_buf_size ) ;
	if ( print_buf != NULL )
		*pp = strx_sprint( print_buf, print_buf_size,
					"remote_user=%s local_user=%s command=%s",
							strings[ 1 ], strings[ 2 ], strings[ 3 ] ) ;
	else
		*pp = rs_explain( RS_NOMEM ) ;
}


PRIVATE void finger_shutdown( sd, pp )
	int sd ;
	char **pp ;
{
	char *line ;
	int line_len ;

	if ( pp == NULL )
		return ;
	
	line = Srdline( sd ) ;
	if ( line == NULL )
	{
		*pp = "EMPTY-LINE" ;
		return ;
	}

	line_len = SIOLINELEN( sd ) ;

	if ( line_len > 0 && line[ line_len-1 ] == '\r' )
		line[ --line_len ] = NUL ;

	*pp = ( line_len == 0 ) ? "EMPTY-LINE" : line ;
}

