/*
 * Copyright (c) 1996 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/atp.h>
#include <atalk/asp.h>

asp_shutdown( asp )
    ASP			asp;
{
    struct atp_block	atpb;
    struct iovec	iov;
    char		*p;
    char		buf[ ATP_MAXDATA ];
    int			iovcnt = 8;
    u_short		blen, seq;
    u_char		oport;

    p = buf;
    *p++ = ASPFUNC_CLOSE;
    *p++ = asp->asp_sid;
    seq = 0;
    bcopy( &seq, p, sizeof( u_short ));
    p += sizeof( u_short );

    oport = asp->asp_sat.sat_port;
    atpb.atp_saddr = &asp->asp_sat;
    atpb.atp_saddr->sat_port = asp->asp_wss;
    atpb.atp_sreqdata = buf;
    atpb.atp_sreqdlen = p - buf;
    atpb.atp_sreqto = 2;
    atpb.atp_sreqtries = 5;

    if ( atp_sreq( asp->asp_atp, &atpb, 1, ATP_XO ) < 0 ) {
	asp->asp_sat.sat_port = oport;
	return( -1 );
    }

    iov.iov_base = buf;
    iov.iov_len = sizeof( buf );
    atpb.atp_rresiov = &iov;
    atpb.atp_rresiovcnt = 1;

    if ( atp_rresp( asp->asp_atp, &atpb ) < 0 ) {
	asp->asp_sat.sat_port = oport;
	return( -1 );
    }
    asp->asp_sat.sat_port = oport;

    return( 0 );
}
