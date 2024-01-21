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
#include <atalk/nbp.h>
#include <atalk/ddp.h>

#include <sys/syslog.h>

#include <netdb.h>
#include <strings.h>
#include  "nbp_conf.h"

char		nbp_send[ 1024 ];
char		nbp_recv[ 1024 ];
u_char		nbp_port = 0;

nbp_parse( data, nn, len )
    char		*data;
    struct nbpnve	*nn;
    int			len;
{
    struct nbptuple	nt;

    bcopy( data, &nt, SZ_NBPTUPLE);
    data += SZ_NBPTUPLE;
    len -= SZ_NBPTUPLE;
    if ( len < 0 ) {
	return( -1 );
    }

#ifdef BSD4_4
    nn->nn_sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
    nn->nn_sat.sat_family = AF_APPLETALK;
    nn->nn_sat.sat_addr.s_net = nt.nt_net;
    nn->nn_sat.sat_addr.s_node = nt.nt_node;
    nn->nn_sat.sat_port = nt.nt_port;

    nn->nn_objlen = *data++;
    len -= nn->nn_objlen + 1;
    if ( len < 0 ) {
	return( -1 );
    }
    if ( nn->nn_objlen > NBPSTRLEN ) {
	return( -1 );
    }
    bcopy( data, nn->nn_obj, nn->nn_objlen );
    data += nn->nn_objlen;

    nn->nn_typelen = *data++;
    len -= nn->nn_typelen + 1;
    if ( len < 0 ) {
	return( -1 );
    }
    if ( nn->nn_typelen > NBPSTRLEN ) {
	return( 1 );
    }
    bcopy( data, nn->nn_type, nn->nn_typelen );

    data += nn->nn_typelen;
    nn->nn_zonelen = *data++;
    len -= nn->nn_zonelen + 1;
    if ( len < 0 ) {
	return( -1 );
    }
    if ( nn->nn_zonelen > NBPSTRLEN ) {
	return( 1 );
    }
    bcopy( data, nn->nn_zone, nn->nn_zonelen );

    return( len );
}

#define NBPM_OBJ	(1<<1)
#define NBPM_TYPE	(1<<2)
#define NBPM_ZONE	(1<<3)

nbp_match( n1, n2, flags )
    struct nbpnve	*n1, *n2;
    int			flags;
{
    int			match = 0;

    if ( flags & NBPMATCH_NOZONE ) {
	match |= NBPM_ZONE;
    }

    if ( !( flags & NBPMATCH_NOGLOB )) {
	if ( n1->nn_objlen == 1 && n1->nn_obj[0] == '=' ) {
	    match |= NBPM_OBJ;
	}
	if ( n1->nn_typelen == 1 && n1->nn_type[0] == '=' ) {
	    match |= NBPM_TYPE;
	}
    }

    if ( !( match & NBPM_OBJ )) {
	if ( n1->nn_objlen != n2->nn_objlen ||
		strndiacasecmp( n1->nn_obj, n2->nn_obj, n1->nn_objlen )) {
	    return( 0 );
	}
    }
    if ( !( match & NBPM_TYPE )) {
	if ( n1->nn_typelen != n2->nn_typelen ||
		strndiacasecmp( n1->nn_type, n2->nn_type, n1->nn_typelen )) {
	    return( 0 );
	}
    }
    if ( !( match & NBPM_ZONE )) {
	if ( n1->nn_zonelen != n2->nn_zonelen ||
		strndiacasecmp( n1->nn_zone, n2->nn_zone, n1->nn_zonelen )) {
	    return( 0 );
	}
    }

    return( 1 );
}

nbp_name( name, objp, typep, zonep )
    char	*name, **objp, **typep, **zonep;
{
    static char	buf[ 32 + 1 + 32 + 1 + 32 + 1 ];
    char	*p;

    if ( name ) {
	if ( strlen( name ) + 1 > sizeof( buf )) {
	    return( -1 );
	}
	strcpy( buf, name );

	if (( p = rindex( buf, '@' )) != NULL ) {
	    *p++ = '\0';
	    *zonep = p;
	}
	if (( p = rindex( buf, ':' )) != NULL ) {
	    *p++ = '\0';
	    *typep = p;
	}
	if ( *buf != '\0' ) {
	    *objp = buf;
	}
    }

    return( 0 );
}
