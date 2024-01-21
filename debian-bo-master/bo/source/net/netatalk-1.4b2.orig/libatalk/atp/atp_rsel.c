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

#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <atalk/compat.h>
#include <atalk/atp.h>
#include "atp_internals.h"

extern int	errno;

static int	got_alarm = 0;
#ifdef DROP_ATPTREL
static int	release_count = 0;
#endif DROP_ATPTREL


static int
resend_request( ah )
    ATP		ah;
{
    /*
     * update bitmap and send request packet
     */
    struct atphdr	req_hdr;

#ifdef EBUG
    printf( "\n<%d> resend_request: resending %d byte request packet",
	    getpid(), ah->atph_reqpkt->atpbuf_dlen );
    print_addr( " to", &ah->atph_reqpkt->atpbuf_addr );
    putchar( '\n' );
    bprint( ah->atph_reqpkt->atpbuf_info.atpbuf_data,
	    ah->atph_reqpkt->atpbuf_dlen );
#endif

    bcopy( ah->atph_reqpkt->atpbuf_info.atpbuf_data + 1, (char *)&req_hdr,
	sizeof( struct atphdr ));
    req_hdr.atphd_bitmap = ah->atph_rbitmap;
    bcopy( (char *)&req_hdr, ah->atph_reqpkt->atpbuf_info.atpbuf_data + 1,
	    sizeof( struct atphdr ));

    gettimeofday( &ah->atph_reqtv, (struct timezone *)0 );
    if ( sendto( ah->atph_socket,
	    ah->atph_reqpkt->atpbuf_info.atpbuf_data,
	    ah->atph_reqpkt->atpbuf_dlen, 0,
	    (struct sockaddr *)&ah->atph_reqpkt->atpbuf_addr,
	    sizeof( struct sockaddr_at )) != ah->atph_reqpkt->atpbuf_dlen ) {
	return( -1 );
    }

    if ( ah->atph_reqtries > 0 ) {
	--(ah->atph_reqtries);
    }

    return( 0 );
}

int
atp_rsel( ah, faddr, func )
    ATP			ah;		/* open atp handle */
    struct sockaddr_at	*faddr;		/* address to receive from */
    int			func;		/* which function(s) to wait for;
					   0 means request or response */
{
    struct atpbuf	*abuf, *pb, *cb;
    struct atphdr	req_hdr, resp_hdr;
    int			i, recvlen, requesting, mask;
    u_char		rfunc;
    u_short		tid;
    struct timeval	tv;
    struct sockaddr_at	saddr;

#ifdef EBUG
    print_bufuse( ah, "atp_rsel at top" );
#endif
    if ( func == 0 ) {
	func = ATP_FUNCANY;
    }

    requesting = ( func & ATP_TRESP ) && ah->atph_rrespcount > 0 &&
	( ah->atph_reqtries > 0 || ah->atph_reqtries == ATP_TRIES_INFINITE );

    if ( requesting && ah->atph_rbitmap == 0 ) {
	/*
	 * we already have a complete atp response; just return
	 */
	return( ATP_TRESP );
    }

    if (( abuf = alloc_buf()) == NULL ) {
	return( -1 );
    }

    if ( requesting ) {
#ifdef EBUG
	printf( "<%d> atp_rsel: request pending\n", getpid());
#endif
	gettimeofday( &tv, (struct timezone *)0 );
	if ( tv.tv_sec - ah->atph_reqtv.tv_sec > ah->atph_reqto ) {
	    if ( resend_request( ah ) < 0 ) {
		free_buf( abuf );
		return( -1 );
	    }
	}
	save_timer();
	set_alrm_catcher();
    }

    for ( ;; ) {
	rfunc = func;
	if ( requesting ) {
	    set_timer( ah->atph_reqto );
	}
	bcopy( (char *)faddr, (char *)&saddr, sizeof( struct sockaddr_at ));
#ifdef EBUG
	printf( "<%d> atp_rsel calling recv_atp,", getpid());
	print_addr( " accepting from: ", &saddr );
	putchar( '\n' );
#endif EBUG
	if (( recvlen = recv_atp( ah, &saddr, &rfunc, ATP_TIDANY,
		abuf->atpbuf_info.atpbuf_data, 0 )) >= 0 ) {
	    break;	/* we received something */
	}

	if ( !requesting || errno != EINTR || !got_alarm ) {
	    break;	/* error */
	}
	    
	if ( ah->atph_reqtries <= 0 &&
		ah->atph_reqtries != ATP_TRIES_INFINITE ) {
	    errno = ETIMEDOUT;
	    break;
	}
	
	if ( resend_request( ah ) < 0 ) {
	    break;	/* error */
	}
    }

    if ( requesting ) {
	cancel_timer();
	restore_timer();
	reset_alrm_catcher();
    }

    if ( recvlen <= 0 ) {	/* error */
	free_buf( abuf );
	return( recvlen );
    }

#ifdef EBUG
    printf( "<%d> atp_rsel: rcvd %d bytes", getpid(), recvlen );
    print_addr( " from: ", &saddr );
    putchar( '\n' );
    bprint( abuf->atpbuf_info.atpbuf_data, recvlen );
#endif

    abuf->atpbuf_dlen = recvlen;
    bcopy( abuf->atpbuf_info.atpbuf_data + 1, (char *)&resp_hdr,
	    sizeof( struct atphdr ));

    if ( rfunc == ATP_TREQ ) {
	/*
	 * we got a request: check to see if it is a duplicate (XO)
	 * while we are at it, we expire old XO responses from sent list
	 */
	bcopy( abuf->atpbuf_info.atpbuf_data + 1, (char *)&req_hdr,
		sizeof( struct atphdr ));
	tid = ntohs( req_hdr.atphd_tid );
	gettimeofday( &tv, (struct timezone *)0 );
	for ( pb = NULL, cb = ah->atph_sent; cb != NULL;
		pb = cb, cb = cb->atpbuf_next ) {
#ifdef EBUG
	    printf( "<%d>", getpid());
	    print_addr( " examining", &cb->atpbuf_addr );
	    printf( " %hu", cb->atpbuf_info.atpbuf_xo.atpxo_tid );
	    print_addr( " (looking for", &saddr );
	    printf( " %hu)\n", tid );
#endif
	    if ( tv.tv_sec - cb->atpbuf_info.atpbuf_xo.atpxo_tv.tv_sec
		    > cb->atpbuf_info.atpbuf_xo.atpxo_reltime ) {
		/* discard expired response */
#ifdef EBUG
		printf( "<%d> expiring tid %hu\n", getpid(),
			cb->atpbuf_info.atpbuf_xo.atpxo_tid );
#endif
		if ( pb == NULL ) {
		    ah->atph_sent = cb->atpbuf_next;
		} else {
		    pb->atpbuf_next = cb->atpbuf_next;
		}

		for ( i = 0; i < 8; ++i ) {
		    if ( cb->atpbuf_info.atpbuf_xo.atpxo_packet[ i ]
			    != NULL ) {
			free_buf( cb->atpbuf_info.atpbuf_xo.atpxo_packet[ i ] );
		    }
		}
		free_buf( cb );

		if (( cb = pb ) == NULL )
		    break;

	    } else if ( at_addr_eq( &saddr, &cb->atpbuf_addr )) {
		if ( cb->atpbuf_info.atpbuf_xo.atpxo_tid == tid ) {
		    break;
		}
	    }
	}

	if ( cb != NULL ) {
#ifdef EBUG
	    printf( "<%d> duplicate request -- re-sending XO resp\n",
		    getpid());
#endif
	    /* matches an old response -- just re-send and reset expire */
	    cb->atpbuf_info.atpbuf_xo.atpxo_tv = tv;
	    for ( i = 0; i < 8; ++i ) {
		if ( cb->atpbuf_info.atpbuf_xo.atpxo_packet[i] != NULL &&
			req_hdr.atphd_bitmap & ( 1 << i )) {
		    sendto( ah->atph_socket,
			    cb->atpbuf_info.atpbuf_xo.atpxo_packet[i]->atpbuf_info.atpbuf_data,
			    cb->atpbuf_info.atpbuf_xo.atpxo_packet[i]->atpbuf_dlen,
			    0, (struct sockaddr *)&saddr,
			    sizeof( struct sockaddr_at ));
		}
	    }
	}

	if ( cb == NULL ) {
	    /* new request -- queue it and return */
	    bcopy( (char *)&saddr, (char *)&abuf->atpbuf_addr,
		    sizeof( struct sockaddr_at ));
	    bcopy( (char *)&saddr, (char *)faddr,
		    sizeof( struct sockaddr_at ));
	    abuf->atpbuf_next = ah->atph_queue;
	    ah->atph_queue = abuf;
	    return( ATP_TREQ );
	} else {
	    free_buf( abuf );
	    return( 0 );
	}
    }

    /*
     * we got a response: update bitmap
     */
    bcopy( ah->atph_reqpkt->atpbuf_info.atpbuf_data + 1, (char *)&req_hdr,
	    sizeof( struct atphdr ));
    if ( requesting && ah->atph_rbitmap & ( 1<<resp_hdr.atphd_bitmap )
		&& req_hdr.atphd_tid == resp_hdr.atphd_tid ) {
	ah->atph_rbitmap &= ~( 1<<resp_hdr.atphd_bitmap );

	if ( ah->atph_resppkt[ resp_hdr.atphd_bitmap ] != NULL ) {
	    free_buf( ah->atph_resppkt[ resp_hdr.atphd_bitmap ] );
	}
	ah->atph_resppkt[ resp_hdr.atphd_bitmap ] = abuf;

	/* if End Of Message, clear all higher bitmap bits
	*/
	if ( resp_hdr.atphd_ctrlinfo & ATP_EOM ) {
#ifdef EBUG
	    printf( "<%d> EOM -- seq num %d  current bitmap %d\n",
		getpid(), resp_hdr.atphd_bitmap, ah->atph_rbitmap );
#endif
	    mask = 1 << resp_hdr.atphd_bitmap;
	    ah->atph_rbitmap &= ( mask | mask-1 );
	}

	/* if Send Trans. Status, send updated request
	*/
	if ( resp_hdr.atphd_ctrlinfo & ATP_STS ) {
#ifdef EBUG
	    puts( "STS" );
#endif
	    req_hdr.atphd_bitmap = ah->atph_rbitmap;
	    bcopy( (char *)&req_hdr,
		ah->atph_reqpkt->atpbuf_info.atpbuf_data + 1,
		sizeof( struct atphdr ));
	    if ( sendto( ah->atph_socket,
		    ah->atph_reqpkt->atpbuf_info.atpbuf_data,
		    ah->atph_reqpkt->atpbuf_dlen, 0,
		    (struct sockaddr *) &ah->atph_reqpkt->atpbuf_addr,
		    sizeof( struct sockaddr_at )) !=
		    ah->atph_reqpkt->atpbuf_dlen ) {
		free_buf( abuf );
		return( -1 );
	    }
	}
    } else {
	/*
	 * we are not expecting this response -- toss it
	 */
	free_buf( abuf );
#ifdef EBUG
	printf( "atp_rsel: ignoring resp bm=%x tid=%d (expected %x/%d)\n",
		resp_hdr.atphd_bitmap, ntohs( resp_hdr.atphd_tid ),
		ah->atph_rbitmap, ah->atph_tid );
#endif EBUG
    }

    if ( !ah->atph_rbitmap && ( req_hdr.atphd_ctrlinfo & ATP_XO )) {
	/*
	 * successful completion - send release
	 * the release consists of DDP type byte + ATP header + 4 user bytes
	 */
	req_hdr.atphd_ctrlinfo = ATP_TREL;
	bcopy( (char *)&req_hdr, ah->atph_reqpkt->atpbuf_info.atpbuf_data + 1,
	    sizeof( struct atphdr ));
	ah->atph_reqpkt->atpbuf_dlen = sizeof( struct atphdr ) + 5;
#ifdef EBUG
	printf( "<%d> sending TREL", getpid() );
	bprint( ah->atph_reqpkt->atpbuf_info.atpbuf_data,
		ah->atph_reqpkt->atpbuf_dlen );
#endif
#ifdef DROP_ATPTREL
	if (( ++release_count % 10 ) != 0 ) {
#endif DROP_ATPTREL
	sendto( ah->atph_socket, ah->atph_reqpkt->atpbuf_info.atpbuf_data,
		ah->atph_reqpkt->atpbuf_dlen, 0,
		(struct sockaddr *) &ah->atph_reqpkt->atpbuf_addr,
		sizeof( struct sockaddr_at));
#ifdef DROP_ATPTREL
	}
#endif DROP_ATPTREL
    }

    if ( ah->atph_rbitmap != 0 ) {
	if ( ah->atph_reqtries > 0
		|| ah->atph_reqtries == ATP_TRIES_INFINITE ) {
	    return( 0 );
	} else {
	    errno = ETIMEDOUT;
	    return( -1 );
	}
    }

    bcopy( (char *)&saddr, (char *)faddr, sizeof( struct sockaddr_at ));
    return( ATP_TRESP );
}


#if !defined( ibm032 ) && !defined( _IBMR2 )
    void
#endif ibm032 _IBMR2
catch_alrm()
{
    got_alarm = 1;
}


struct sigaction	old_alrm_sv;

set_alrm_catcher()
{
    struct sigaction	sv;

    sv.sa_handler = catch_alrm;
    sigemptyset( &sv.sa_mask );
    sv.sa_flags = SA_INTERRUPT;

    if ( sigaction( SIGALRM, &sv, &old_alrm_sv ) < 0 ) {
	perror( "sigaction in set_alrm_catcher" );
	exit( -1 );
    }
}


reset_alrm_catcher()
{
    if ( sigaction( SIGALRM, &old_alrm_sv, (struct sigaction *) 0 ) < 0 ) {
	perror( "sigaction in reset_alrm_catcher" );
	exit( -1 );
    }
}


struct itimerval old_itv;

#ifdef _IBMR2
#define getitimer(a,b)		(setitimer( a, (struct itimerval *)0, b ))
#endif _IBMR2

save_timer()
{
    if ( getitimer( ITIMER_REAL, &old_itv ) < 0 ) {
	perror( "getitimer" );
	exit( -1 );
    }
}


restore_timer()
{
    if ( setitimer( ITIMER_REAL, &old_itv, (struct itimerval *) 0 ) < 0 ) {
	perror( "setitimer" );
	exit( -1 );
    }
}

set_timer( secs )
    int		secs;
{
    struct itimerval itv;

    itv.it_interval.tv_sec = 0L;
    itv.it_interval.tv_usec = 0L;
    itv.it_value.tv_sec = secs;
    itv.it_value.tv_usec = 0L;

    got_alarm = 0;
    
    if ( setitimer( ITIMER_REAL, &itv, (struct itimerval *) 0 ) < 0 ) {
	perror( "setitimer" );
	exit( -1 );
    }
}


cancel_timer()
{
    set_timer( 0 );
}
