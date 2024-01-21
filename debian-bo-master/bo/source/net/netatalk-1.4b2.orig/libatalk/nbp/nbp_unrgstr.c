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
#include <sys/errno.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/nbp.h>
#include <atalk/ddp.h>

#include <netdb.h>
#include  "nbp_conf.h"

nbp_unrgstr( obj, type, zone )
    char		*obj, *type, *zone;
{
    struct sockaddr_at	to;
    struct nbphdr	nh;
    struct nbptuple	nt;
    struct timeval	timeout;
    fd_set		readfd;
    struct servent	*se;
    char		*data;
    int			s, cc, namelen;
    extern int		errno;

    if (( s = socket( AF_APPLETALK, SOCK_DGRAM, 0 )) < 0 ) {
	return( -1 );
    }

    bzero( &to, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
    to.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    to.sat_family = AF_APPLETALK;
    if ( bind( s, (struct sockaddr *)&to, sizeof( struct sockaddr_at )) < 0 ) {
	return( -1 );
    }

    data = nbp_send;
    *data++ = DDPTYPE_NBP;
    nh.nh_op = NBPOP_UNRGSTR;
    nh.nh_cnt = 1;
    nh.nh_id = 0;
    bcopy( &nh, data, SZ_NBPHDR );
    data += SZ_NBPHDR;

    nt.nt_net = 0;
    nt.nt_node = 0;
    nt.nt_port = 0;
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

    bzero( &to, sizeof( struct sockaddr_at ));
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
