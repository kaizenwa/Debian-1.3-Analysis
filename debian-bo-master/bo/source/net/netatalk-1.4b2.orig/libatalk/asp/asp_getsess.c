/*
 * Copyright (c) 1990,1996 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/errno.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <sys/syslog.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netatalk/at.h>
#include <atalk/compat.h>
#include <atalk/atp.h>
#include <atalk/asp.h>
#include <stdio.h>

#include "asp_child.h"

struct asp_child	**asp_ac;
ASP			asp_asp;
int			asp_sessions;

/*
 * Routine called every 30 seconds to send tickles to all open sessions,
 * and to check that tickles have been received from all clients. If
 * a client hasn't been sending tickles, the child processes is sent a
 * SIGTERM signal -- this is expected to terminate the child within
 * ~30 seconds, or the child will be sent another signal. DO NOT IGNORE
 * THIS SIGNAL!
 */
    void
asp_alrm()
{
    struct atp_block	atpb;
    char		rdata[ ATP_MAXDATA ];
    int			sid;

    for ( sid = 0; sid < asp_sessions; sid++ ) {
	if ( asp_ac[ sid ] == NULL ||
		asp_ac[ sid ]->ac_state == ACSTATE_DEAD ) {
	    continue;
	}

	if ( ++asp_ac[ sid ]->ac_state >= ACSTATE_BAD ) {
	    if ( kill( asp_ac[ sid ]->ac_pid, SIGTERM ) < 0 ) {
		syslog( LOG_ERR, "kill: %m" );
		/*
		 * Child is gone (for some reason), mark it to
		 * be clean up.
		 */
		asp_ac[ sid ]->ac_state = ACSTATE_DEAD;
	    }
	    syslog( LOG_INFO, "asp_alrm: %d timed out",
		    asp_ac[ sid ]->ac_pid );
	    continue;
	}
	/*
	 * Send a tickle.
	 */
	rdata[ 0 ] = ASPFUNC_TICKLE;
	rdata[ 1 ] = sid;
	rdata[ 2 ] = rdata[ 3 ] = 0;
	atpb.atp_saddr = &asp_ac[ sid ]->ac_sat;
	atpb.atp_sreqdata = rdata;
	atpb.atp_sreqdlen = 4;
	atpb.atp_sreqto = 0;
	atpb.atp_sreqtries = 1;
	if ( atp_sreq( asp_asp->asp_atp, &atpb, 0, 0 ) < 0 ) {
	    syslog( LOG_ERR, "atp_sreq: %m" );
	}
    }
    return;
}

#ifndef WEXITSTATUS
#define WEXITSTATUS(x)	((x).w_status)
#endif
/*
 * Routine called when a child exits to clean up the data structures
 * associated with the child.
 */
    void
asp_chld()
{
    int		status;
    int		sid, pid;

    while (( pid = wait3( &status, WNOHANG, 0 )) > 0 ) {
	for ( sid = 0; sid < asp_sessions; sid++ ) {
	    if ( asp_ac[ sid ] != NULL && asp_ac[ sid ]->ac_pid == pid ) {
		break;
	    }
	}
	if ( sid >= asp_sessions ) {
	    syslog( LOG_INFO, "asp_chld spurious child %d", pid );
	    continue;
	}

	if ( WIFEXITED( status )) {
	    if ( WEXITSTATUS( status )) {
		syslog( LOG_INFO, "asp_chld %d exited %d", pid,
			WEXITSTATUS( status ));
		asp_attention( htons((short)0x8000), sid );
	    } else {
		syslog( LOG_INFO, "asp_chld %d done", pid );
	    }
	} else {
	    asp_attention( htons((short)0x8000), sid );
	    if ( WIFSIGNALED( status )) {
		syslog( LOG_INFO, "asp_chld %d killed", pid );
	    } else {
		syslog( LOG_INFO, "asp_chld %d died", pid );
	    }
	}

	asp_ac[ sid ]->ac_state = ACSTATE_DEAD;
    }
    return;
}

/*
 * This call handles open, tickle, and getstatus requests. On a
 * successful open, it forks a child process. This call will never
 * return to the parent, except on an error, NULL. It returns an ASP
 * to the child.
 */
ASP
asp_getsession( asp, sessions )
    ASP		asp;
    int		sessions;
{
    struct sockaddr_at	sat;
    struct sigaction	asv, oasv, csv, ocsv;
    struct itimerval	it, oit;
    struct atp_block	atpb;
    struct iovec	iov[ 8 ];
    ATP			atp;
    ASP			aspret;
    char		rdata[ ATP_MAXDATA ];
    int			sid, csid, pid;
    u_short		asperr;
    extern int		errno;

    if (( asp_ac =
	    (struct asp_child **)calloc( sessions, sizeof( struct asp_child *)))
	    == NULL ) {
	return( NULL );
    }
    asp_sessions = sessions;
    asp_asp = asp;

    asv.sa_handler = asp_alrm;
    sigemptyset( &asv.sa_mask );
    asv.sa_flags = SA_RESTART;

    if ( sigaction( SIGALRM, &asv, &oasv ) < 0 ) {
	return( NULL );
    }

    csv.sa_handler = asp_chld;
    sigemptyset( &csv.sa_mask );
    csv.sa_flags = 0;

    if ( sigaction( SIGCHLD, &csv, &ocsv ) < 0 ) {
	return( NULL );
    }

    it.it_interval.tv_sec = 30;
    it.it_interval.tv_usec = 0;
    it.it_value.tv_sec = 30;
    it.it_value.tv_usec = 0;

    if ( setitimer( ITIMER_REAL, &it, &oit ) < 0 ) {
	return( NULL );
    }

    for (;;) {
	bzero( &sat, sizeof( struct sockaddr_at ));
#ifdef BSD4_4
	sat.sat_len = sizeof( struct sockaddr_at );
#endif BSD4_4
	sat.sat_family = AF_APPLETALK;
	sat.sat_addr.s_net = ATADDR_ANYNET;
	sat.sat_addr.s_node = ATADDR_ANYNODE;
	sat.sat_port = ATADDR_ANYPORT;
	atpb.atp_saddr = &sat;
	atpb.atp_rreqdata = rdata;
	atpb.atp_rreqdlen = sizeof( rdata );
	if ( atp_rreq( asp->asp_atp, &atpb ) < 0 ) {
	    if ( errno == EINTR ) {
		continue;
	    }
	    return( NULL );
	}

	switch ( rdata[ 0 ] ) {
	case ASPFUNC_STAT :
#ifdef EBUG
printf( "asp stat\n" );
#endif EBUG
	    if ( asp->asp_slen > 0 ) {
		rdata[ 0 ] = 0;
		bcopy( asp->asp_status, &rdata[ 4 ], asp->asp_slen );
		iov[ 0 ].iov_base = rdata;
		iov[ 0 ].iov_len = 4 + asp->asp_slen;
		atpb.atp_sresiov = iov;
		atpb.atp_sresiovcnt = 1;
		atp_sresp( asp->asp_atp, &atpb );
	    }
	    break;
	case ASPFUNC_OPEN :
	    for ( csid = 0; csid < sessions; csid++ ) {
		if ( asp_ac[ csid ] == NULL ) {
		    break;
		}
		/* check for stuff to free */
		if ( asp_ac[ csid ]->ac_state == ACSTATE_DEAD ) {
		    free( asp_ac[ csid ] );
		    asp_ac[ csid ] = NULL;
		    break;
		}
	    }
	    if ( csid == sessions ) {				/* Too many */
		rdata[ 0 ] = 0;
		rdata[ 1 ] = 0;
		asperr = ASPERR_SERVBUSY;
	    } else {
		if (( atp = atp_open( 0 )) == NULL ) {
		    break;
		}
		switch ( pid = fork()) {
		case 0 :					/* child */
		    for ( sid = 0; sid < sessions; sid++ ) {
			if ( asp_ac[ sid ] != NULL ) {
			    free( asp_ac[ sid ] );
			}
		    }
		    free( asp_ac );
		    if ( sigaction( SIGALRM, &oasv, 0 ) < 0 ) {
			return( NULL );
		    }
		    if ( sigaction( SIGCHLD, &ocsv, 0 ) < 0 ) {
			return( NULL );
		    }
		    if ( setitimer( ITIMER_REAL, &oit, 0 ) < 0 ) {
			return( NULL );
		    }
		    if (( aspret = (struct ASP *)malloc( sizeof( struct ASP )))
			    == NULL ) {
			return( NULL );
		    }
		    aspret->asp_atp = atp;
		    aspret->asp_sat = sat;
		    aspret->asp_wss = rdata[ 1 ];
		    aspret->asp_seq = 0;
		    aspret->asp_flags = ASPFL_SSS;
		    aspret->asp_sid = csid;
		    return( aspret );

		case -1 :					/* error */
		    rdata[ 0 ] = 0;
		    rdata[ 1 ] = 0;
		    asperr = ASPERR_SERVBUSY;
		    break;
		default :
		    if (( asp_ac[ csid ] =
			(struct asp_child *)malloc( sizeof( struct asp_child )))
			== NULL ) {
			break;
		    }
		    asp_ac[ csid ]->ac_pid = pid;
		    asp_ac[ csid ]->ac_state = ACSTATE_OK;
		    asp_ac[ csid ]->ac_sat = sat;
		    asp_ac[ csid ]->ac_sat.sat_port = rdata[ 1 ];
		    rdata[ 0 ] = atp_sockaddr( atp )->sat_port;
		    rdata[ 1 ] = csid;
		    asperr = ASPERR_OK;
		    atp_close( atp );
		    break;
		}
	    }
	    bcopy( &asperr, &rdata[ 2 ], sizeof( u_short ));
	    iov[ 0 ].iov_base = rdata;
	    iov[ 0 ].iov_len = 4;
	    atpb.atp_sresiov = iov;
	    atpb.atp_sresiovcnt = 1;
	    atp_sresp( asp->asp_atp, &atpb );
	    break;

	case ASPFUNC_TICKLE :
	    sid = rdata[ 1 ];
	    if ( asp_ac[ sid ] != NULL &&
		    asp_ac[ sid ]->ac_state != ACSTATE_DEAD ) {
		asp_ac[ sid ]->ac_state = ACSTATE_OK;
	    }
	    break;
	}
    }
}

/*
 * Routine called to send a signal to all of a processed asp children.
 * Presumably, this routine can only be called from a signal handler
 * itself, since once asp_getsess() gets into control, it will never
 * return.
 */
asp_kill( signal )
    int		signal;
{
    int		sid;

    for ( sid = 0; sid < asp_sessions; sid++ ) {
	if ( asp_ac[ sid ] != NULL &&
		asp_ac[ sid ]->ac_state != ACSTATE_DEAD ) {
	    if ( kill( asp_ac[ sid ]->ac_pid, signal ) < 0 ) {
		syslog( LOG_ERR, "asp_kill %d: %m", asp_ac[ sid ]->ac_pid );
		asp_ac[ sid ]->ac_state = ACSTATE_DEAD;
	    }
	}
    }
    return( 0 );
}

asp_attentionall( code )
    short	code;
{
    int			sid;

    for ( sid = 0; sid < asp_sessions; sid++ ) {
	if ( asp_ac[ sid ] == NULL ||
		asp_ac[ sid ]->ac_state == ACSTATE_DEAD ) {
	    continue;
	}
	if ( asp_attention( code, sid ) < 0 ) {
	    continue;
	}
    }
    return( 0 );
}

/*
 * Note that code is passed in network byte-order (foolishly).
 */
asp_attention( code, sid )
    short	code, sid;
{
    struct atp_block	atpb;
    struct iovec	iov[ 1 ];
    char		sdata[ 4 ], rdata[ 4 ];

    sdata[ 0 ] = ASPFUNC_ATTN;
    bcopy( &code, &sdata[ 2 ], sizeof( short ));
    iov[ 0 ].iov_base = rdata;
    iov[ 0 ].iov_len = sizeof( rdata );

    if ( asp_ac[ sid ] == NULL || asp_ac[ sid ]->ac_state == ACSTATE_DEAD ) {
	return( -1 );
    }
    sdata[ 1 ] = sid;
    atpb.atp_saddr = &asp_ac[ sid ]->ac_sat;
    atpb.atp_sreqdata = sdata;
    atpb.atp_sreqdlen = sizeof( sdata );
    atpb.atp_sreqto = 2;
    atpb.atp_sreqtries = 5;
syslog( LOG_ERR, "atp_sreq to (%d) %d.%d:%d",
	atpb.atp_saddr->sat_family,
	atpb.atp_saddr->sat_addr.s_net,
	atpb.atp_saddr->sat_addr.s_node,
	atpb.atp_saddr->sat_port );
    if ( atp_sreq( asp_asp->asp_atp, &atpb, 1, 0 ) < 0 ) {
	syslog( LOG_ERR, "atp_sreq: %m" );
	return( -1 );
    }
    atpb.atp_rresiov = iov;
    atpb.atp_rresiovcnt = sizeof( iov )/sizeof( iov[ 0 ] );

    if ( atp_rresp( asp_asp->asp_atp, &atpb ) < 0 ) {
	syslog( LOG_ERR, "atp_rresp: %m" );
	return( -1 );
    }

    return( 0 );
}
