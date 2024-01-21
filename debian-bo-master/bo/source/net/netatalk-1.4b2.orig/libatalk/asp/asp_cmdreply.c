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
#include <netatalk/at.h>
#include <atalk/atp.h>
#include <atalk/asp.h>

#include <stdio.h>

#ifdef ATP_MAXDATA
#undef ATP_MAXDATA
#endif
#define ATP_MAXDATA	578

char	rbuf[ 8 ][ ATP_MAXDATA + 4 ];

asp_cmdreply( asp, result, buf, buflen )
    ASP		asp;
    int		result;
    char	*buf;
    int		buflen;
{
    struct iovec	iov[ 8 ];
    struct atp_block	atpb;
    int			iovcnt;

    iovcnt = 0;
    do {
	iov[ iovcnt ].iov_base = rbuf[ iovcnt ];

	if ( iovcnt == 0 ) {
	    bcopy( (char *)&result, iov[ iovcnt ].iov_base, 4 );
	} else {
	    bzero( iov[ iovcnt ].iov_base, 4 );
	}

	if ( buflen > ATP_MAXDATA ) {
	    bcopy( buf, iov[ iovcnt ].iov_base + 4, ATP_MAXDATA );
	    buf += ATP_MAXDATA;
	    buflen -= ATP_MAXDATA;
	    iov[ iovcnt ].iov_len = ATP_MAXDATA + 4;
	} else {
	    bcopy( buf, iov[ iovcnt ].iov_base + 4, buflen );
	    iov[ iovcnt ].iov_len = buflen + 4;
	    buflen = 0;
	}
	iovcnt++;
    } while ( buflen > 0 );

    atpb.atp_saddr = &asp->asp_sat;
    atpb.atp_sresiov = iov;
    atpb.atp_sresiovcnt = iovcnt;
    if ( atp_sresp( asp->asp_atp, &atpb ) < 0 ) {
	return( -1 );
    }
    asp->asp_seq++;
    return( 0 );
}
