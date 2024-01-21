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
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/compat.h>
#include <atalk/nbp.h>
#include <atalk/ddp.h>

#include <netdb.h>
#include  "nbp_conf.h"

extern int errno;

#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
nbp_alrm()
{
    return;
}

nbp_lookup( obj, type, zone, nn, nncnt )
    char		*obj, *type, *zone;
    struct nbpnve	*nn;
    int			nncnt;
{
    struct sockaddr_at	addr;
    struct itimerval	it, oit;
    struct sigaction	sv, osv;
    struct nbpnve	nve;
    struct nbphdr	nh;
    struct nbptuple	nt;
    struct servent	*se;
    char		*data;
    int			s, namelen, cnt, tries, sc, cc, i;

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
    addr.sat_port = ATADDR_ANYPORT;
    if ( bind( s, (struct sockaddr *)&addr,
	      sizeof( struct sockaddr_at )) < 0 ) {
	return( -1 );
    }

    namelen = sizeof( struct sockaddr_at );
    if ( getsockname( s, (struct sockaddr *)&addr, &namelen ) < 0 ) {
	return( -1 );
    }

    data = nbp_send;
    *data++ = DDPTYPE_NBP;
    nh.nh_op = NBPOP_BRRQ;
    nh.nh_cnt = 1;
    nh.nh_id = 0;
    bcopy( &nh, data, SZ_NBPHDR );
    data += SZ_NBPHDR;

    nt.nt_net = addr.sat_addr.s_net;
    nt.nt_node = addr.sat_addr.s_node;
    nt.nt_port = addr.sat_port;
    bcopy( &nt, data, SZ_NBPTUPLE);
    data += SZ_NBPTUPLE;

    if ( obj ) {
	if (( cc = strlen( obj )) > NBPSTRLEN ) return( -1 );
	*data++ = cc;
	bcopy( obj, data, cc );
	data += cc;
    } else {
	*data++ = 0;
    }

    if ( type ) {
	if (( cc = strlen( type )) > NBPSTRLEN ) return( -1 );
	*data++ = cc;
	bcopy( type, data, cc );
	data += cc;
    } else {
	*data++ = 0;
    }

    if ( zone ) {
	if (( cc = strlen( zone )) > NBPSTRLEN ) return( -1 );
	*data++ = cc;
	bcopy( zone, data, cc );
	data += cc;
    } else {
	*data++ = 0;
    }

    bzero( &addr, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    addr.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    addr.sat_family = AF_APPLETALK;
    addr.sat_addr.s_net = ATADDR_ANYNET;
    addr.sat_addr.s_node = ATADDR_ANYNODE;
    if ( nbp_port == 0 ) {
	if (( se = getservbyname( "nbp", "ddp" )) == NULL ) {
	    nbp_port = 2;
	} else {
	    nbp_port = ntohs( se->s_port );
	}
    }
    addr.sat_port = nbp_port;

    it.it_interval.tv_sec = 2L;
    it.it_interval.tv_usec = 0L;
    it.it_value.tv_sec = 2L;
    it.it_value.tv_usec = 0L;

    if ( setitimer( ITIMER_REAL, &it, &oit ) < 0 ) {
	return( -1 );
    }

    sv.sa_handler = nbp_alrm;
    sigemptyset( &sv.sa_mask );
    sv.sa_flags = SA_INTERRUPT;

    if ( sigaction( SIGALRM, &sv, &osv ) < 0 ) {
	return( -1 );
    }

    cnt = 0;
    tries = 3;
    sc = data - nbp_send;
    while ( tries > 0 ) {
	if ( sendto( s, nbp_send, sc, 0, (struct sockaddr *)&addr,
		sizeof( struct sockaddr_at )) < 0 ) {
	    return( -1 );
	}
	while (( cc = recvfrom( s, nbp_recv, sizeof( nbp_recv ), 0, 0,
		&namelen )) > 0 ) {
	    data = nbp_recv;
	    if ( *data++ != DDPTYPE_NBP ) {
		continue;
	    }
	    cc--;

	    bcopy( data, &nh, SZ_NBPHDR );
	    data += SZ_NBPHDR;
	    if ( nh.nh_op != NBPOP_LKUPREPLY ) {
		continue;
	    }
	    cc -= SZ_NBPHDR;

	    while (( i = nbp_parse( data, &nve, cc )) >= 0 ) {
		data += cc - i;
		cc = i;
		/*
		 * Check to see if nve is already in nn. If not,
		 * put it in, and increment cnt.
		 */
		for ( i = 0; i < cnt; i++ ) {
		    if ( nbp_match( &nve, &nn[ i ],
			    NBPMATCH_NOZONE|NBPMATCH_NOGLOB )) {
			break;
		    }
		}
		if ( i == cnt ) {
		    nn[ cnt++ ] = nve;
		}
		if ( cnt == nncnt ) {
		    tries = 0;
		    break;
		}
	    }
	    if ( cnt == nncnt ) {
		tries = 0;
		break;
	    }
	}
	tries--;
    }

    if ( setitimer( ITIMER_REAL, &oit, 0 ) < 0 ) {
	return( -1 );
    }

    if ( sigaction( SIGALRM, &osv, 0 ) < 0 ) {
	return( -1 );
    }

    close( s );
    errno = 0;
    return( cnt );
}
