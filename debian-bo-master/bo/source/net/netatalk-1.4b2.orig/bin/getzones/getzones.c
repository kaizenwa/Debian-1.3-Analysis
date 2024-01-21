#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/atp.h>
#include <atalk/zip.h>

usage( s )
    char *s;
{
    fprintf( stderr, "usage:\t%s [-m | -l] [address]\n", s );
    exit( 1 );
}

main( argc, argv )
    int		argc;
    char	*argv[];
{
    struct atp_handle	*ah;
    struct atp_block	atpb;
    struct sockaddr_at	saddr;
    struct servent	*se;
    char		reqdata[4], buf[ ATP_MAXDATA ];
    struct iovec	iov;
    short		temp, index = 0;
    int			c, myzoneflg = 0, localzonesflg = 0, errflg = 0;
    extern int		optind;

    reqdata[ 0 ] = ZIPOP_GETZONELIST;

    while (( c = getopt( argc, argv, "ml" )) != EOF ) {
	switch (c) {
	case 'm':
	    if ( localzonesflg ) {
		++errflg;
	    }
	    ++myzoneflg;
	    reqdata[ 0 ] = ZIPOP_GETMYZONE;
	    break;
	case 'l':
	    if ( myzoneflg ) {
		++errflg;
	    }
	    ++localzonesflg;
	    reqdata[ 0 ] = ZIPOP_GETLOCALZONES;
	    break;
	default:
	    ++errflg;
	}
    }

    if ( errflg || argc - optind > 1 ) {
	usage( argv[ 0 ] );
    }

    if (( ah = atp_open( 0 )) == NULL ) {
	perror( "atp_open" );
	exit( 1 );
    }

    bzero( (char *) &saddr, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    saddr.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    saddr.sat_family = AF_APPLETALK;
    if (( se = getservbyname( "zip", "ddp" )) == NULL ) {
	fprintf( stderr, "Unknown service.\n" );
	exit( 1 );
    }
    saddr.sat_port = ntohs( se->s_port );

    if ( argc == optind ) {
	saddr.sat_addr.s_net = ATADDR_ANYNET;
	saddr.sat_addr.s_node = ATADDR_ANYNODE;
    } else {
	if ( !atalk_aton( argv[ optind ], &saddr.sat_addr )) {
	    fprintf( stderr, "Bad address.\n" );
	    exit( 1 );
	}
    }

    index = ( myzoneflg ? 0 : 1 );
    reqdata[1] = 0;

    do {
	atpb.atp_saddr = &saddr;
	temp = htons( index );
	bcopy( &temp, &reqdata[2], 2 );
	atpb.atp_sreqdata = reqdata;
	atpb.atp_sreqdlen = 4;
	atpb.atp_sreqto = 2;
	atpb.atp_sreqtries = 5;

	/* send getzone request zones (or get my zone)
	*/
	if ( atp_sreq( ah, &atpb, 1, 0 ) < 0 ) {
	    perror( "atp_sreq" );
	    exit( 1 );
	}

	iov.iov_base = buf;
	iov.iov_len = ATP_MAXDATA;
	atpb.atp_rresiov = &iov;
	atpb.atp_rresiovcnt = 1;

	if ( atp_rresp( ah, &atpb ) < 0 ) {
	    perror( "atp_rresp" );
	    exit( 1 );
	}

	bcopy( &((char *)iov.iov_base)[2], &temp, 2 );
	temp = ntohs( temp );
	print_zones( temp, iov.iov_base+4 );
	index += temp;
    } while ( !myzoneflg && !((char *)iov.iov_base)[ 0 ] );

    if ( atp_close( ah ) != 0 ) {
	perror( "atp_close" );
	exit( 1 );
    }

    exit( 0 );
}


print_zones( n, buf )
    short	n;	/* number of zones in this packet */
    char	*buf;	/* zone length/name pairs */
{
    for ( ; n--; buf += (*buf) + 1 ) {
	printf( "%.*s\n", *buf, buf+1 );
    }
}
