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
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/atp.h>
#include <atalk/asp.h>

char		rbuf[ 8 ][ ATP_MAXDATA ];
struct iovec	iov[ 8 ] = {
    { rbuf[ 0 ], 0 },
    { rbuf[ 1 ], 0 },
    { rbuf[ 2 ], 0 },
    { rbuf[ 3 ], 0 },
    { rbuf[ 4 ], 0 },
    { rbuf[ 5 ], 0 },
    { rbuf[ 6 ], 0 },
    { rbuf[ 7 ], 0 },
};

asp_wrtcont( asp, buf, buflen )
    ASP			asp;
    char		*buf;
    int			*buflen;
{
    struct atp_block	atpb;
    char		*p;
    int			iovcnt = 8;
    u_short		blen, seq;
    u_char		oport;

    p = buf;
    *p++ = ASPFUNC_WRTCONT;
    *p++ = asp->asp_sid;
    seq = htons( asp->asp_seq );
    bcopy( &seq, p, sizeof( u_short ));
    p += sizeof( u_short );
    blen = htons( *buflen );
    bcopy( &blen, p, sizeof( u_short ));
    p += sizeof( u_short );

    for ( iovcnt = 0; iovcnt < 8; iovcnt++ ) {
	iov[ iovcnt ].iov_len = ATP_MAXDATA;
    }

    oport = asp->asp_sat.sat_port;
    atpb.atp_saddr = &asp->asp_sat;
    atpb.atp_saddr->sat_port = asp->asp_wss;
    atpb.atp_sreqdata = buf;
    atpb.atp_sreqdlen = p - buf;
    atpb.atp_sreqto = 2;
    atpb.atp_sreqtries = 5;

    if ( atp_sreq( asp->asp_atp, &atpb, iovcnt, ATP_XO ) < 0 ) {
	asp->asp_sat.sat_port = oport;
	return( -1 );
    }
    atpb.atp_rresiov = iov;
    atpb.atp_rresiovcnt = iovcnt;
    if ( atp_rresp( asp->asp_atp, &atpb ) < 0 ) {
	asp->asp_sat.sat_port = oport;
	return( -1 );
    }

    asp->asp_sat.sat_port = oport;

    p = buf;
    for ( iovcnt = 0; iovcnt < atpb.atp_rresiovcnt; iovcnt++ ) {
	bcopy( iov[ iovcnt ].iov_base + 4, p, iov[ iovcnt ].iov_len - 4 );
	p += ( iov[ iovcnt ].iov_len - 4 );
    }
    *buflen = p - buf;
    return( 0 );
}

asp_wrtreply( asp, result, buf, buflen )
    ASP		asp;
    int		result;
    char	*buf;
    int		buflen;
{
    return( asp_cmdreply( asp, result, buf, buflen ));
}
