/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved. See COPYRIGHT.
 */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <netatalk/at.h>
#include <netatalk/endian.h>
#include <atalk/nbp.h>
#include <atalk/ddp.h>

#include <netdb.h>
#include  "nbp_conf.h"

nbp_rgstr( sat, obj, type, zone )
    struct sockaddr_at	*sat;
    char		*obj, *type, *zone;
{
    struct sockaddr_at	to;
    struct nbpnve	nn;
    struct nbphdr	nh;
    struct nbptuple	nt;
    struct timeval	timeout;
    fd_set		readfd;
    struct servent	*se;
    char		*data;
    int			s, cc, namelen;
    extern int		errno;

    if ( nbp_lookup( obj, type, zone, &nn, 1 ) > 0 ) {
	return( -1 );
    }

    if (( s = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	return( -1 );
    }

    data = nbp_send;
    *data++ = DDPTYPE_NBP;
    nh.nh_op = NBPOP_RGSTR;
    nh.nh_cnt = 1;
    nh.nh_id = 0;
    bcopy( &nh, data, SZ_NBPHDR );
    data += SZ_NBPHDR;

    nt.nt_net = sat->sat_addr.s_net;
    nt.nt_node = sat->sat_addr.s_node;
    nt.nt_port = sat->sat_port;
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

    bzero( (struct sockaddr *) &to, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    to.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    to.sat_family = AF_APPLETALK;;
    to.sat_addr.s_net = ATADDR_ANYNET;
    to.sat_addr.s_node = ATADDR_ANYNODE;
    if ( nbp_port == 0 ) {
	if (( se = getservbyname( "nbp", "ddp" )) == NULL ) {
	    nbp_port = 2;
	} else {
	    nbp_port = ntohs( se->s_port );
	}
    }
    to.sat_port = nbp_port;

    if ( sendto( s, nbp_send, data - nbp_send, 0, (struct sockaddr *)&to,
	    sizeof( struct sockaddr_at )) < 0 ) {
	return( -1 );
    }

    FD_ZERO( &readfd );
    FD_SET( s, &readfd );
    timeout.tv_sec = 2;
    timeout.tv_usec = 0;
    if (( cc = select( s + 1, &readfd, 0, 0, &timeout )) < 0 ) {
	return( -1 );
    }
    if ( cc == 0 ) {
	errno = ETIMEDOUT;
	return( -1 );
    }

    namelen = sizeof( struct sockaddr_at );
    if (( cc = recvfrom( s, nbp_recv, sizeof( nbp_recv ), 0,
			(struct sockaddr *)&to, &namelen )) < 0 ) {
	return( -1 );
    }
    close( s );

    data = nbp_recv;
    if ( *data++ != DDPTYPE_NBP ) {
	return( -1 );
    }
    bcopy( data, &nh, SZ_NBPHDR );
    if ( nh.nh_op != NBPOP_OK ) {
	return( -1 );
    }
    return( 0 );
}
