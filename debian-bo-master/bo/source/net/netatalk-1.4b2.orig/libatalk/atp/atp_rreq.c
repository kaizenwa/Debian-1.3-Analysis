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
#include <sys/socket.h>
#include <sys/errno.h>
#include <netinet/in.h>
#undef s_net
#include <netatalk/at.h>
#include <atalk/atp.h>
#include "atp_internals.h"

extern int	errno;

/* wait for a tranasaction service request
*/
int atp_rreq( ah, atpb )
    ATP			ah;		/* open atp handle */
    struct atp_block	*atpb;		/* parameter block */
{
    struct atpbuf	*req_buf;	/* for receiving request packet */
    struct atphdr	req_hdr;	/* request header overlay */
    struct sockaddr_at	faddr;		/* sender's address */
    int			recvlen;	/* length of received packet */
    u_short		tid;
    int			rc, trelcode;
    u_char		func;

#ifdef EBUG
    print_bufuse( ah, "atp_rreq" );
#endif

    while (( rc = atp_rsel( ah, atpb->atp_saddr, ATP_TREQ )) == 0 ) {
	;
    }

    if ( rc != ATP_TREQ ) {
#ifdef EBUG
	printf( "<%d> atp_rreq: atp_rsel returns err %d\n", getpid(), rc );
#endif EBUG
	return( rc );
    }

    /* allocate a buffer for receiving request
    */
    if (( req_buf = alloc_buf()) == NULL ) {
	return -1;
    }

    bcopy( (char *)atpb->atp_saddr, (char *)&faddr,
	    sizeof( struct sockaddr_at ));
    func = ATP_TREQ;
    if (( recvlen = recv_atp( ah, &faddr, &func, ATP_TIDANY,
	  req_buf->atpbuf_info.atpbuf_data, 1 )) < 0 ) {
	free_buf( req_buf );
	return -1;
    }

    bcopy( req_buf->atpbuf_info.atpbuf_data + 1, (char *)&req_hdr,
	    sizeof( struct atphdr ));
    tid = ntohs( req_hdr.atphd_tid );

    ah->atph_rtid = tid;
    if (( ah->atph_rxo = req_hdr.atphd_ctrlinfo & ATP_XO ) != 0 ) {
	ah->atph_rreltime = ATP_RELTIME *
		( 1 << ( req_hdr.atphd_ctrlinfo & ATP_TRELMASK ));
    }

    bcopy( (char *)&faddr, (char *)atpb->atp_saddr,
	    sizeof( struct sockaddr_at ));

    if ( recvlen - ATP_HDRSIZE > atpb->atp_rreqdlen ) {
	free_buf( req_buf );
	errno = EMSGSIZE;
	return -1;
    }

    atpb->atp_rreqdlen = recvlen - ATP_HDRSIZE;
    bcopy( (char *)req_buf->atpbuf_info.atpbuf_data + ATP_HDRSIZE,
	    (char *)atpb->atp_rreqdata, recvlen - ATP_HDRSIZE );
    atpb->atp_bitmap = req_hdr.atphd_bitmap;
    free_buf( req_buf );
    return( 0 );
}
