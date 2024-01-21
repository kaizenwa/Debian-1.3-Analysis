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

/*
 * AppleTalk Echo Protocol Client
 */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/errno.h>

#include <signal.h>
#include <stdio.h>
#include <netdb.h>

#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/compat.h>
#include <atalk/aep.h>
#include <atalk/nbp.h>
#include <atalk/ddp.h>

struct sockaddr_at	target;
int			s, nsent = 0, nrecv = 0;
long			totalms = 0, minms = -1, maxms = -1;

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
aep_send()
{
    struct timeval	tv;
    char		*p, buf[ 1024 ];
    static unsigned int	seq = 0;

    p = buf;
    *p++ = DDPTYPE_AEP;
    *p++ = AEPOP_REQUEST;
    bcopy( &seq, p, sizeof( unsigned int ));
    p += sizeof( unsigned int );
    seq++;

    if ( gettimeofday( &tv, (struct timezone *)0 ) < 0 ) {
	perror( "gettimeofday" );
	exit( 1 );
    }
    bcopy( &tv, p, sizeof( struct timeval ));
    p += sizeof( struct timeval );

    if ( sendto( s, buf, p - buf, 0, (struct sockaddr *) &target,
	    sizeof( struct sockaddr_at )) < 0 ) {
	perror( "sendto" );
	exit( 1 );
    }
    nsent++;
}

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
done()
{
    if ( nsent > 0 ) {
	printf( "\n----%d.%d AEP Statistics----\n",
		ntohs( target.sat_addr.s_net ), target.sat_addr.s_node );
	printf( "%d packets sent, %d packets received, %d%% packet loss\n",
		nsent, nrecv, (( nsent - nrecv ) * 100 ) / nsent );
	if ( nrecv > 0 ) {
	    printf( "round-trip (ms)  min/avg/max = %d/%d/%d\n",
		    minms, totalms / nrecv, maxms );
	}	
    }
    exit( 0 );
}

main( ac, av )
    int		ac;
    char	**av;
{
    struct servent	*se;
    struct sigaction	sv;
    struct itimerval	it;
    struct sockaddr_at	sat;
    struct timeval	tv, atv;
    struct nbpnve	nn;
    char		*obj = NULL, *type = "Workstation", *zone = "*";
    int			satlen, cc;
    unsigned int	seq;
    long		ms;
    char		buf[ 1024 ], *p;
    unsigned char	port;
    extern int		errno;

    if ( ac != 2 ) usage( av[ 0 ] );

    /*
     * Save the port, since nbp_lookup calls getservbyname() to get the
     * nbp port.
     */
    if (( se = getservbyname( "echo", "ddp" )) == NULL ) {
	fprintf( stderr, "Unknown service.\n" );
	exit( 1 );
    }
    port = ntohs( se->s_port );

    bzero( &target, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    target.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    target.sat_family = AF_APPLETALK;
    if ( !atalk_aton( av[ 1 ], &target.sat_addr )) {
	if ( nbp_name( av[ 1 ], &obj, &type, &zone ) || !obj ) {
	    fprintf( stderr, "Bad name: %s\n", av[ 1 ] );
	    exit( 1 );
	}
	if ( nbp_lookup( obj, type, zone, &nn, 1 ) <= 0 ) {
	    fprintf( stderr, "Can't find: %s\n", av[ 1 ] );
	    exit( 1 );
	}
	bcopy( &nn.nn_sat, &target, sizeof( struct sockaddr_at ));
    }
    target.sat_port = port;

    if (( s = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	perror( "socket" );
	exit( 1 );
    }

    sv.sa_handler = aep_send;
    sigemptyset( &sv.sa_mask );
    sigaddset( &sv.sa_mask, SIGINT );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGALRM, &sv, (struct sigaction *)0 ) < 0 ) {
	perror( "sigaction" );
	exit( 1 );
    }

    sv.sa_handler = done;
    sigemptyset( &sv.sa_mask );
    sigaddset( &sv.sa_mask, SIGALRM );
    sv.sa_flags = SA_RESTART;
    if ( sigaction( SIGINT, &sv, (struct sigaction *)0 ) < 0 ) {
	perror( "sigaction" );
	exit( 1 );
    }

    it.it_interval.tv_sec = 1L;
    it.it_interval.tv_usec = 0L;
    it.it_value.tv_sec = 1L;
    it.it_value.tv_usec = 0L;

    if ( setitimer( ITIMER_REAL, &it, (struct itimerval *)0 ) < 0 ) {
	perror( "setitimer" );
	exit( 1 );
    }

    for (;;) {
	satlen = sizeof( struct sockaddr_at );
	if (( cc = recvfrom( s, buf, sizeof( buf ), 0, (struct sockaddr *) &sat,
		&satlen )) < 0 ) {
	    if ( errno == EINTR ) {
		errno = 0;
		continue;
	    } else {
		perror( "recvfrom" );
		exit( 1 );
	    }
	}
	p = buf;
	if ( *p++ != DDPTYPE_AEP || *p++ != AEPOP_REPLY ) {
	    fprintf( stderr, "%s: bad packet!\n", av[ 0 ] );
	    continue;
	}
	if ( gettimeofday( &tv, (struct timezone *)0 ) < 0 ) {
	    perror( "gettimeofday" );
	    exit( 1 );
	}
	bcopy( p, &seq, sizeof( unsigned int ));
	p += sizeof( unsigned int );
	bcopy( p, &atv, sizeof( struct timeval ));
	nrecv++;
	ms = ( tv.tv_sec - atv.tv_sec ) * 1000 +
		( tv.tv_usec - atv.tv_usec ) / 1000;
	totalms += ms;
	if ( ms > maxms ) {
	    maxms = ms;
	}
	if ( ms < minms || minms == -1 ) {
	    minms = ms;
	}
	printf( "%d bytes from %u.%u: aep_seq=%d. time=%d. ms\n",
		cc, ntohs( sat.sat_addr.s_net ), sat.sat_addr.s_node,
		seq, ms );
    }
}

usage( av0 )
{
    fprintf( stderr, "Usage:\t%s ( addr | nbpname )\n", av0 );
    exit( 1 );
}
