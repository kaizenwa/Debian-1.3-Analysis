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
#include <netatalk/at.h>
#include <atalk/atp.h>
#include "atp_internals.h"
#ifdef EBUG
#include <stdio.h>
#endif EBUG

int atp_close( ah )
    ATP		ah;
{
    struct atpbuf	*cq;
    int			i;

    /* remove from list of open atp sockets & discard queued data
    */
#ifdef EBUG
    print_bufuse( ah, "atp_close");
#endif EBUG

    if ( close( ah->atph_socket ) < 0 ) {
	return -1;
    }

    while ( ah->atph_queue != NULL ) {
	cq = ah->atph_queue;
	ah->atph_queue = cq->atpbuf_next;
	free_buf( cq );
    }

    while ( ah->atph_sent != NULL ) {
	cq = ah->atph_sent;
	for ( i = 0; i < 8; ++i ) {
	    if ( cq->atpbuf_info.atpbuf_xo.atpxo_packet[ i ] != NULL ) {
		free_buf( cq->atpbuf_info.atpbuf_xo.atpxo_packet[ i ] );
	    }
	}
	ah->atph_sent = cq->atpbuf_next;
	free_buf( cq );
    }

    if ( ah->atph_reqpkt != NULL ) {
	free_buf( ah->atph_reqpkt );
	ah->atph_reqpkt = NULL;
    }

    for ( i = 0; i < 8; ++i ) {
	if ( ah->atph_resppkt[ i ] != NULL ) {
	    free_buf( ah->atph_resppkt[ i ] );
	    ah->atph_resppkt[ i ] = NULL;
	}
    }

#ifdef EBUG
    print_bufuse( ah, "atp_close end");
#endif EBUG

    free_buf( (struct atpbuf *) ah );

    return 0;
}
