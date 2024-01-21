/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: itox.c,v 1.5 1995/09/10 18:41:13 chuck Exp $" ;

#define EQ( s1, s2 )					( strcmp( s1, s2 ) == 0 )

#define NUL								'\0'
#define NULL							0
#define PRIVATE						static

#define FIELD_WIDTH					15
#define DAEMON_DIR_OPTION			"-daemon_dir"
#define TCPD_NAME						"tcpd"

#include <sys/param.h>
#include "sio.h"
#include "str.h"
#include "misc.h"

#if defined(linux) || defined(BSD)
PRIVATE void print_line( char *name, char *value ) ;
PRIVATE char *next_word( char *description ) ;
#endif

char *strchr() ;

str_h strp ;
int line_count ;

/*
 * This program works only as a filter.
 * Options:
 * 	-daemon_dir <dir_name>	:	if you use tcpd, this option specifies the
 *											directory where all the daemons are.
 *											You must specify this option if you use tcpd
 *
 * Note that we don't bother to free the memory we malloc.
 */
int main( argc, argv )
	int argc ;
	char *argv[] ;
{
	char *s ;
	int uses_tcpd ;
	char *daemon_dirpath ;
	void print_line() ;
	char *next_word() ;

	if ( argc != 1 && argc != 3 )
	{
		Sprint( 2, "Usage: %s [%s dir_path]\n",
				basename( argv[ 0 ] ), DAEMON_DIR_OPTION ) ;
		exit( 1 ) ;
	}

	uses_tcpd = ( argc == 3 ) ;

	if ( uses_tcpd )
	{
		int len ;

		daemon_dirpath = argv[ 2 ] ;
		len = strlen( daemon_dirpath ) ;
		if ( daemon_dirpath[ len-1 ] == '/' )
			daemon_dirpath[ --len ] = NUL ;
	}

	strp = str_parse( (char *)0, " \t", STR_NOFLAGS, (int *)0 ) ;

	while ( s = Srdline( 0 ) )
	{
		char *word ;
		char *p ;
		char *socket_type, *protocol ;
		char *service ;
		int is_rpc ;

		line_count++ ;

		if ( SIOLINELEN( 0 ) == 0 || s[ 0 ] == '#' )
			continue ;

		str_setstr( strp, s ) ;

		service = word = next_word( "service name" ) ;

		/*
		 * Check if it is an RPC service
		 */
		p = strchr( word, '/' ) ;
		if ( p != NULL )
			*p = 0 ;
		Sprint( 1, "service %s\n{\n", word ) ;
		if ( is_rpc = ( p != NULL ) )
		{
			print_line( "type", "RPC" ) ;
			print_line( "rpc_version", p+1 ) ;
		}

		socket_type = word = next_word( "socket type" ) ;
		print_line( "socket_type", socket_type ) ;

		word = next_word( "protocol" ) ;
		p = strchr( word, '/' ) ;
		protocol = ( p == NULL ) ? word : p+1 ;

		print_line( "protocol", protocol ) ;

		word = next_word( "wait/nowait" ) ;
		print_line( "wait", EQ( word, "wait" ) ? "yes" : "no" ) ;

		word = next_word( "user" ) ;
		print_line( "user", word ) ;

		word = next_word( "server" ) ;
		if ( EQ( word, "internal" ) )
		{
			/*
			 * We are in trouble if this is an RPC service
			 */
			if ( is_rpc )
			{
				Sprint( 2,
					"The entry for service %s will be wrong because\n", service ) ;
				Sprint( 2, "we can't handle internal RPC services\n" ) ;
			}
			else
			{
				print_line( "type", "INTERNAL" ) ;
				print_line( "id", make_string( 3, service, "-", socket_type ) ) ;
			}
		}
		else
		{
			char *server_path = word ;		/* from inetd.conf */
			char *server_of_server_path = basename( server_path ) ;
			char *server_name = next_word( "server name" ) ;
			char *server ;						/* for xinetd config file */

			if ( EQ( server_of_server_path, TCPD_NAME ) )
			{
				if ( ! uses_tcpd )
				{
					Sprint( 2, "You must use option %s if you use %s\n",
						DAEMON_DIR_OPTION, TCPD_NAME ) ;
					exit( 1 ) ;
				}
				if ( server_name[ 0 ] == '/' )
					server = server_name ;
				else
					server = make_pathname( 2, daemon_dirpath, server_name ) ;
			}
			else
				server = server_path ;

			print_line( "server", server ) ;

			word = str_component( strp ) ;			/* 1st arg */
			if ( word != NULL )
			{
				Sprint( 1, "\t%-*s = %s", FIELD_WIDTH, "server_args", word ) ;
				while ( word = str_component( strp ) )
					Sprint( 1, " %s", word ) ;
				Sputchar( 1, '\n' ) ;
			}
		}

		Sprint( 1, "}\n\n" ) ;
	}
	Sflush( 1 ) ;
	exit( 0 ) ;
}


PRIVATE void print_line( name, value )
	char *name, *value ;
{
	Sprint( 1, "\t%-*s = %s\n", FIELD_WIDTH, name, value ) ;
}


PRIVATE char *next_word( description )
	char *description ;
{
	char *word = str_component( strp ) ;

	if ( word == NULL )
	{
		Sprint( 2, "Line %d: %s missing \n", line_count, description ) ;
		exit( 1 ) ;
	}
	return( word ) ;
}

