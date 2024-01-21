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
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <sys/uio.h>
#include <signal.h>

#include <netinet/in.h>
#undef s_net
#include <netatalk/at.h>
#include <atalk/atp.h>

extern int	errno;

int
atp_sreq( ah, atpb, respcount, flags )
    ATP			ah;		/* open atp handle */
    struct atp_block	*atpb;		/* parameter block */
    int			respcount;	/* buffers available for response */
    u_char		flags;		/* ATP_XO, ATP_TREL?? */
{
    struct atpbuf	*req_buf;
    int			i;

#ifdef EBUG
    print_bufuse( ah, "atp_sreq" );
#endif

    /* check parameters
    */
    if ( atpb->atp_sreqdlen < 4 || atpb->atp_sreqdlen > ATP_MAXDATA
	    || ( respcount < 0 ) || ( respcount > 8 )
	    || ( atpb->atp_sreqto < 0 ) || (( atpb->atp_sreqtries < 1 )
	    && ( atpb->atp_sreqtries != ATP_TRIES_INFINITE ))) {
	errno = EINVAL;
	return -1;
    }
    /* clean up any packet fragments left from last request
    */
    for ( i = 0; i < 8; ++i ) {
	if ( ah->atph_resppkt[ i ] != NULL ) {
	    free_buf( ah->atph_resppkt[ i ] );
	    ah->atph_resppkt[ i ] = NULL;
	}
    }

    /* generate bitmap, tid and ctrlinfo
    */
    atpb->atp_bitmap = ( 1 << respcount ) - 1;

    /* allocate a new buffer and build request packet
    */
    if (( req_buf = alloc_buf()) == NULL ) {
	return( -1 );
    }
    build_req_packet( req_buf, ah->atph_tid++, flags | ATP_TREQ, atpb );
    bcopy( (char *)atpb->atp_saddr, &req_buf->atpbuf_addr,
	    sizeof( struct sockaddr_at ));

    /* send the initial request
    */
#ifdef EBUG
    printf( "\n<%d> atp_sreq: sending a %d byte packet ", getpid(),
	    req_buf->atpbuf_dlen );
    print_addr( " to", atpb->atp_saddr );
    putchar( '\n' );
    bprint( req_buf->atpbuf_info.atpbuf_data, req_buf->atpbuf_dlen );
#endif

    gettimeofday( &ah->atph_reqtv, (struct timezone *)0 );
#ifdef DROPPACKETS
if (( random() % 3 ) != 2 ) {
#endif
    if ( sendto( ah->atph_socket, req_buf->atpbuf_info.atpbuf_data,
	    req_buf->atpbuf_dlen, 0, (struct sockaddr *) atpb->atp_saddr,
	    sizeof( struct sockaddr_at )) != req_buf->atpbuf_dlen ) {
	free_buf( req_buf );
	return( -1 );
    }
#ifdef DROPPACKETS
} else printf( "<%d> atp_sreq: dropped request\n", getpid() );
#endif

    if ( atpb->atp_sreqto != 0 ) {
	if ( ah->atph_reqpkt != NULL ) {
	    free_buf( ah->atph_reqpkt );
	}
	ah->atph_reqto = atpb->atp_sreqto;
	if ( atpb->atp_sreqtries == ATP_TRIES_INFINITE ) {
	    ah->atph_reqtries = ATP_TRIES_INFINITE;
	} else {
	    /* we already sent one */
	    ah->atph_reqtries = atpb->atp_sreqtries - 1;
	}
	ah->atph_reqpkt = req_buf;
	ah->atph_rbitmap = ( 1 << respcount ) - 1;
	ah->atph_rrespcount = respcount;
    } else {
	free_buf( req_buf );
	ah->atph_rrespcount = 0;
    }

    return( 0 );
}
