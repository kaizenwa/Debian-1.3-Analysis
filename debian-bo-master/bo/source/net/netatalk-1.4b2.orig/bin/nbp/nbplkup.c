/*
 * Copyright (c) 1990,1991 Regents of The University of Michigan.
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation, and that the name of The University
 * of Michigan not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. This software is supplied as is without expressed or
 * implied warranties of any kind.
 *
 *	Research Systems Unix Group
 *	The University of Michigan
 *	c/o Mike Clark
 *	535 W. William Street
 *	Ann Arbor, Michigan
 *	+1-313-763-0525
 *	netatalk@itd.umich.edu
 */

#include <sys/types.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/nbp.h>
#include <strings.h>
#include <stdio.h>
#if !defined( sun ) || !defined( i386 )
#include <stdlib.h>
#endif sun i386

char *Obj = "=";
char *Type = "=";
char *Zone = "*";

Usage( av0 )
    char	*av0;
{
    char	*p;

    if (( p = rindex( av0, '/' )) == 0 ) {
	p = av0;
    } else {
	p++;
    }

    printf( "Usage:\t%s [ -r responses] [ obj:type@zone ]\n", p );
    exit( 1 );
}

main( ac, av )
    int		ac;
    char	**av;
{
    struct nbpnve	*nn;
    char		*name;
    int			i, c, nresp = 1000;
    extern char		*optarg;
    extern int		optind, opterr;

    while (( c = getopt( ac, av, "r:" )) != EOF ) {
	switch ( c ) {
	case 'r' :
	    nresp = atoi( optarg );
	    break;

	default :
	    Usage( av[ 0 ] );
	    exit( 1 );
	}
    }

    if (( nn = (struct nbpnve *)malloc( nresp * sizeof( struct nbpnve )))
	    == NULL ) {
	perror( "malloc" );
	exit( 1 );
    }

    if ( ac - optind > 1 ) {
	Usage( av[ 0 ] );
	exit( 1 );
    }

    /*
     * Get default values from the environment. We need to copy out
     * the results, here, since nbp_name returns it's parameters
     * in static space, and we'll clobber them when we call it again
     * later.
     */
    if (( name = getenv( "NBPLKUP" )) != NULL ) {
	if ( nbp_name( name, &Obj, &Type, &Zone )) {
	    fprintf( stderr,
		    "Environment variable syntax error: NBPLKUP = %s\n",
		    name );
	    exit( 1 );
	}

	if (( name = (char *)malloc( strlen( Obj ) + 1 )) == NULL ) {
	    perror( "malloc" );
	    exit( 1 );
	}
	strcpy( name, Obj );
	Obj = name;

	if (( name = (char *)malloc( strlen( Type ) + 1 )) == NULL ) {
	    perror( "malloc" );
	    exit( 1 );
	}
	strcpy( name, Type );
	Type = name;

	if (( name = (char *)malloc( strlen( Zone ) + 1 )) == NULL ) {
	    perror( "malloc" );
	    exit( 1 );
	}
	strcpy( name, Zone );
	Zone = name;

    }

    if ( ac - optind == 1 ) {
	if ( nbp_name( av[ optind ], &Obj, &Type, &Zone )) {
	    Usage( av[ 0 ] );
	    exit( 1 );
	}
    }

    if (( c = nbp_lookup( Obj, Type, Zone, nn, nresp )) < 0 ) {
	perror( "nbp_lookup" );
	exit( -1 );
    }
    for ( i = 0; i < c; i++ ) {
	asciize( nn[ i ].nn_objlen, nn[ i ].nn_obj );
	asciize( nn[ i ].nn_typelen, nn[ i ].nn_type );

	printf( "%31.*s:%-34.*s %u.%u:%u\n",
		nn[ i ].nn_objlen, nn[ i ].nn_obj,
		nn[ i ].nn_typelen, nn[ i ].nn_type,
		ntohs( nn[ i ].nn_sat.sat_addr.s_net ),
		nn[ i ].nn_sat.sat_addr.s_node,
		nn[ i ].nn_sat.sat_port );
    }

}

#include <ctype.h>

asciize( i, p )
    int		i;
    char	*p;
{
    for ( ; i > 0; p++, i-- ) {
	if ( !isascii( *p ))
	    *p = toascii( *p );
    }
}

#define BPLEN	48
char	hexdig[] = "0123456789abcdef";

bprint( data, len )
    char	*data;
    int		len;
{
    char	out[ BPLEN ];
    int		i = 0;

    bzero( out, BPLEN );
    for ( ;; ) {
	if ( len < 1 ) {
	    printf( "%s\n", ( i == 0 ) ? "(end)" : out );
	    break;
	}

	if ( isgraph((unsigned char) *data )) {
	    out[ i ] = ' ';
	    out[ i+1 ] = *data;
	} else {
	    out[ i ] = hexdig[ ( *data & 0xf0 ) >> 4 ];
	    out[ i+1 ] = hexdig[ *data & 0x0f ];
	}
	i += 2;
	len--;
	data++;

	if ( i > BPLEN - 2 ) {
	    printf( "%s\n", out );
	    bzero( out, BPLEN );
	    i = 0;
	    continue;
	}
	out[ i++ ] = ' ';
    }
}
