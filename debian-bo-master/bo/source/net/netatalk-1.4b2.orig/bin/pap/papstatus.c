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
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/file.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/atp.h>
#include <atalk/pap.h>
#include <atalk/nbp.h>
#include <stdio.h>
#include <strings.h>

#define _PATH_PAPRC	".paprc"

usage( path )
    char	*path;
{
    char	*p;

    if (( p = rindex( path, '/' )) == NULL ) {
	p = path;
    } else {
	p++;
    }
    fprintf( stderr,
	"Usage:\t%s [ -p printername ]\n", p );
    exit( 1 );
}

char *
paprc()
{
    static char	s[ 32 + 1 + 32 + 1 + 32 ];
    char	*name = NULL;
    FILE	*f;

    if (( f = fopen( _PATH_PAPRC, "r" )) == NULL ) {
	return( NULL );
    }
    while ( fgets( s, sizeof( s ), f ) != NULL ) {
	s[ strlen( s ) - 1 ] = '\0';	/* remove trailing newline */
	if ( *s == '#' ) {
	    continue;
	}
	name = s;
	break;
    }
    fclose( f );
    return( name );
}

char			*printer = NULL;

char			cbuf[ 8 ];
struct nbpnve		nn;

main( ac, av )
    int		ac;
    char	**av;
{
    ATP			atp;
    int			wait, c, err = 0;
    char		*obj = NULL, *type = "LaserWriter", *zone = "*";
    extern char		*optarg;
    extern int		optind, errno;

    while (( c = getopt( ac, av, "p:s:" )) != EOF ) {
	switch ( c ) {
	case 'p' :
	    printer = optarg;
	    break;

	default :
	    fprintf( stderr, "Unknown option: '%c'\n", c );
	    err++;
	}
    }
    if ( err ) {
	usage( *av );
    }
    if ( printer == NULL && (( printer = paprc()) == NULL )) {
	usage( *av );
    }

    /*
     * Open connection.
     */
    if ( nbp_name( printer, &obj, &type, &zone ) < 0 ) {
	fprintf( stderr, "%s: Bad name\n", printer );
	exit( 1 );
    }
    if ( obj == NULL ) {
	fprintf( stderr, "%s: Bad name\n", printer );
	exit( 1 );
    }
    if ( nbp_lookup( obj, type, zone, &nn, 1 ) <= 0 ) {
	if ( errno != 0 ) {
	    perror( "nbp_lookup" );
	} else {
	    fprintf( stderr, "%s:%s@%s: NBP Lookup failed\n", obj, type, zone );
	}
	exit( 1 );
    }

    if (( atp = atp_open( 0 )) == NULL ) {
	perror( "atp_open" );
	exit( 1 );
    }

    if ( optind == ac ) {
	getstatus( atp, &nn.nn_sat );
	exit( 0 );
    }
    if ( optind - ac > 1 ) {
	usage( *av );
    }
    wait = atoi( av[ optind ] );
    for (;;) {
	getstatus( atp, &nn.nn_sat );
	sleep( wait );
    }
}

getstatus( atp, sat )
    ATP			atp;
    struct sockaddr_at	*sat;
{
    struct iovec	iov;
    struct atp_block	atpb;
    char		rbuf[ ATP_MAXDATA ];

    cbuf[ 0 ] = 0;
    cbuf[ 1 ] = PAP_SENDSTATUS;
    cbuf[ 2 ] = cbuf[ 3 ] = 0;

    atpb.atp_saddr = sat;
    atpb.atp_sreqdata = cbuf;
    atpb.atp_sreqdlen = 4;		/* bytes in SendStatus request */
    atpb.atp_sreqto = 2;		/* retry timer */
    atpb.atp_sreqtries = 5;		/* retry count */
    if ( atp_sreq( atp, &atpb, 1, ATP_XO ) < 0 ) {
	perror( "atp_sreq" );
	exit( 1 );
    }

    iov.iov_base = rbuf;
    iov.iov_len = sizeof( rbuf );
    atpb.atp_rresiov = &iov;
    atpb.atp_rresiovcnt = 1;
    if ( atp_rresp( atp, &atpb ) < 0 ) {
	perror( "atp_rresp" );
	exit( 1 );
    }

    /* sanity */
    if ( iov.iov_len < 8 ||
	    rbuf[ 1 ] != PAP_STATUS ) {
	fprintf( stderr, "Bad response!\n" );
	return;	/* This is weird, since TIDs must match... */
    }

    printf( "%.*s\n", iov.iov_len - 9, iov.iov_base + 9 );
}
