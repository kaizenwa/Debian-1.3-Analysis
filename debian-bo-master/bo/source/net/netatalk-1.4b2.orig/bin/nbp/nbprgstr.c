/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <stdio.h>
#include <strings.h>

Usage( av0 )
    char	*av0;
{
    char	*p;

    if (( p = rindex( av0, '/' )) == 0 ) {
	p = av0;
    } else {
	p++;
    }

    fprintf( stderr, "Usage: %s obj:type@zone\n", p );
    exit( 1 );
}

main( ac, av )
    int		ac;
    char	**av;
{
    struct sockaddr_at	addr;
    char		*Obj = 0, *Type = 0, *Zone = 0;
    int			s, namelen, c, port = 0;
    extern char		*optarg;
    extern int		optind, opterr;

    while (( c = getopt( ac, av, "p:" )) != EOF ) {
	switch ( c ) {
	case 'p' :
	    port = atoi( optarg );
	    break;

	default :
	    Usage( av[ 0 ] );
	    exit( 1 );
	}
    }

    if ( ac - optind != 1 ) {
	Usage( av[ 0 ] );
    }

    /*
     * Get the name. If Type or Obj aren't specified, error.
     */
    if ( nbp_name( av[ optind ], &Obj, &Type, &Zone ) || !Obj || !Type ) {
	Usage( av[ 0 ] );
    }

    if (( s = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	return( -1 );
    }

    bzero( &addr, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    addr.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    addr.sat_family = AF_APPLETALK;
    addr.sat_addr.s_net = ATADDR_ANYNET;
    addr.sat_addr.s_node = ATADDR_ANYNODE;
    if ( bind( s, (struct sockaddr *) &addr, sizeof( struct sockaddr_at )) < 0 ) {
	perror( "bind" );
	exit( 1 );
    }

    namelen = sizeof( struct sockaddr_at );
    if ( getsockname( s, (struct sockaddr *) &addr, &namelen ) < 0 ) {
	perror( "getsockname" );
	exit( 1 );
    }
    if ( port ) {
	addr.sat_port = port;
    }

    if ( nbp_rgstr( &addr, Obj, Type, Zone ) < 0 ) {
	perror( "nbp_rgstr" );
	fprintf( stderr, "Can't register %s:%s@%s\n", Obj, Type,
		Zone ? Zone : "*" );
	exit( 1 );
    }
}
