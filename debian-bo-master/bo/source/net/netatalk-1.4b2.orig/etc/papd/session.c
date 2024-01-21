/*
 * Copyright (c) 1990,1994 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

#include <sys/types.h>
#include <sys/syslog.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <netatalk/at.h>
#include <atalk/atp.h>
#include <atalk/pap.h>

#include "file.h"

extern unsigned char	connid, quantum, oquantum;

char		buf[ PAP_MAXQUANTUM ][ 4 + PAP_MAXDATA ];
struct iovec	niov[ PAP_MAXQUANTUM ] = {
    { buf[ 0 ],	0 },
    { buf[ 1 ],	0 },
    { buf[ 2 ],	0 },
    { buf[ 3 ],	0 },
    { buf[ 4 ],	0 },
    { buf[ 5 ],	0 },
    { buf[ 6 ],	0 },
    { buf[ 7 ],	0 },
};
struct iovec	iov[ PAP_MAXQUANTUM ] = {
    { buf[ 0 ] + 4,	0 },
    { buf[ 1 ] + 4,	0 },
    { buf[ 2 ] + 4,	0 },
    { buf[ 3 ] + 4,	0 },
    { buf[ 4 ] + 4,	0 },
    { buf[ 5 ] + 4,	0 },
    { buf[ 6 ] + 4,	0 },
    { buf[ 7 ] + 4,	0 },
};

/*
 * Accept files until the client closes the connection.
 * Read lines of a file, until the client sends eof, after
 * which we'll send eof also.
 */
session( atp, sat )
    ATP			atp;
    struct sockaddr_at	*sat;
{
    struct timeval	tv;
    struct atp_block	atpb;
    struct sockaddr_at	ssat;
    struct papfile	infile, outfile;
    fd_set		fds;
    char		cbuf[ 578 ];
    int			i, cc, timeout = 0, readpending = 0;
    u_short		seq = 0, rseq, netseq;
    u_char		readport;

    infile.pf_state = PF_BOT;
    infile.pf_len = 0;
    infile.pf_buf = 0;
    infile.pf_cur = 0;
    infile.pf_end = 0;

    outfile.pf_state = PF_BOT;
    outfile.pf_len = 0;
    outfile.pf_buf = 0;
    outfile.pf_cur = 0;
    outfile.pf_end = 0;

    /*
     * Ask for data.
     */
    cbuf[ 0 ] = connid;
    cbuf[ 1 ] = PAP_READ;
    if ( ++seq == 0xffff ) seq = 1;
    netseq = htons( seq );
    bcopy( &netseq, &cbuf[ 2 ], sizeof( netseq ));
    atpb.atp_saddr = sat;
    atpb.atp_sreqdata = cbuf;
    atpb.atp_sreqdlen = 4;		/* bytes in SendData request */
    atpb.atp_sreqto = 5;		/* retry timer */
    atpb.atp_sreqtries = -1;		/* infinite retries */
    if ( atp_sreq( atp, &atpb, oquantum, ATP_XO )) {
	syslog( LOG_ERR, "atp_sreq: %m" );
	exit( 1 );
    }

    for (;;) {
	/*
	 * Time between tickles.
	 */
	tv.tv_sec = 60;
	tv.tv_usec = 0;

	/*
	 * If we don't get anything for a while, time out.
	 */
	FD_ZERO( &fds );
	FD_SET( atp_fileno( atp ), &fds );

	if (( cc = select( FD_SETSIZE, &fds, 0, 0, &tv )) < 0 ) {
	    syslog( LOG_ERR, "select: %m" );
	    exit( 1 );
	}
	if ( cc == 0 ) {
	    if ( timeout++ > 2 ) {
		syslog( LOG_ERR, "connection timed out" );
		lp_cancel();
		exit( 1 );
	    }

	    /*
	     * Send a tickle.
	     */
	    cbuf[ 0 ] = connid;
	    cbuf[ 1 ] = PAP_TICKLE;
	    cbuf[ 2 ] = cbuf[ 3 ] = 0;
	    atpb.atp_saddr = sat;
	    atpb.atp_sreqdata = cbuf;
	    atpb.atp_sreqdlen = 4;		/* bytes in Tickle request */
	    atpb.atp_sreqto = 0;		/* best effort */
	    atpb.atp_sreqtries = 1;		/* try once */
	    if ( atp_sreq( atp, &atpb, 0, 0 )) {
		syslog( LOG_ERR, "atp_sreq: %m" );
		exit( 1 );
	    }
	    continue;
	} else {
	    timeout = 0;
	}

	bzero( &ssat, sizeof( struct sockaddr_at ));
	switch( atp_rsel( atp, &ssat, ATP_TRESP | ATP_TREQ )) {
	case ATP_TREQ :
	    atpb.atp_saddr = &ssat;
	    atpb.atp_rreqdata = cbuf;
	    atpb.atp_rreqdlen = sizeof( cbuf );
	    if ( atp_rreq( atp, &atpb ) < 0 ) {
		syslog( LOG_ERR, "atp_rreq: %m" );
		exit( 1 );
	    }
	    /* sanity */
	    if ( (unsigned char)cbuf[ 0 ] != connid ) {
		syslog( LOG_ERR, "Bad ATP request!" );
		continue;
	    }

	    switch( cbuf[ 1 ] ) {
	    case PAP_READ :
		/*
		 * Other side is ready for some data.
		 */
		readpending = 1;
		readport = ssat.sat_port;
		bcopy( &cbuf[ 2 ], &rseq, sizeof( rseq ));
		break;

	    case PAP_CLOSE :
		/*
		 * Respond to the close request.
		 * If we're in the middle of a file, clean up.
		 */
		if (( infile.pf_state & PF_BOT ) ||
			( PF_BUFSIZ( &infile ) == 0 &&
			( infile.pf_state & PF_EOF ))) {
		    lp_print();
		} else {
		    lp_cancel();
		}

		niov[ 0 ].iov_len = 4;
		((char *)niov[ 0 ].iov_base)[ 0 ] = connid;
		((char *)niov[ 0 ].iov_base)[ 1 ] = PAP_CLOSEREPLY;
		((char *)niov[ 0 ].iov_base)[ 2 ] =
			((char *)niov[ 0 ].iov_base)[ 3 ] = 0;
		atpb.atp_sresiov = niov;
		atpb.atp_sresiovcnt = 1;
		if ( atp_sresp( atp, &atpb ) < 0 ) {
		    syslog( LOG_ERR, "atp_sresp: %m" );
		    exit( 1 );
		}
		exit( 0 );
		break;

	    case PAP_TICKLE :
		break;
	    default :
		syslog( LOG_ERR, "Bad PAP request!" );
	    }

	    break;

	case ATP_TRESP :
	    atpb.atp_saddr = &ssat;
	    for ( i = 0; i < oquantum; i++ ) {
		niov[ i ].iov_len = PAP_MAXDATA + 4;
	    }
	    atpb.atp_rresiov = niov;
	    atpb.atp_rresiovcnt = oquantum;
	    if ( atp_rresp( atp, &atpb ) < 0 ) {
		syslog( LOG_ERR, "atp_rresp: %m" );
		exit( 1 );
	    }

	    /* sanity */
	    if ( ((unsigned char *)niov[ 0 ].iov_base)[ 0 ] != connid ||
		    ((char *)niov[ 0 ].iov_base)[ 1 ] != PAP_DATA ) {
		syslog( LOG_ERR, "Bad data response!" );
		continue;
	    }

	    for ( i = 0; i < atpb.atp_rresiovcnt; i++ ) {
		APPEND( &infile,
			niov[ i ].iov_base + 4, niov[ i ].iov_len - 4 );
		if (( infile.pf_state & PF_EOF ) == 0 &&
			((char *)niov[ 0 ].iov_base)[ 2 ] ) {
		    infile.pf_state |= PF_EOF;
		}
	    }

	    /* move data */
	    if ( ps( &infile, &outfile ) < 0 ) {
		syslog( LOG_ERR, "parse: bad return" );
		exit( 1 );	/* really?  close? */
	    }

	    /*
	     * Ask for more data.
	     */
	    cbuf[ 0 ] = connid;
	    cbuf[ 1 ] = PAP_READ;
	    if ( ++seq == 0xffff ) seq = 1;
	    netseq = htons( seq );
	    bcopy( &netseq, &cbuf[ 2 ], sizeof( netseq ));
	    atpb.atp_saddr = sat;
	    atpb.atp_sreqdata = cbuf;
	    atpb.atp_sreqdlen = 4;		/* bytes in SendData request */
	    atpb.atp_sreqto = 5;		/* retry timer */
	    atpb.atp_sreqtries = -1;		/* infinite retries */
	    if ( atp_sreq( atp, &atpb, oquantum, ATP_XO )) {
		syslog( LOG_ERR, "atp_sreq: %m" );
		exit( 1 );
	    }
	    break;

	case 0:
	    break;

	default :
	    syslog( LOG_ERR, "atp_rsel: %m" );
	    exit( 1 );
	}

	/* send any data that we have */
	if ( readpending &&
		( PF_BUFSIZ( &outfile ) || ( outfile.pf_state & PF_EOF ))) {
	    for ( i = 0; i < quantum; i++ ) {
		((char *)niov[ i ].iov_base)[ 0 ] = connid;
		((char *)niov[ i ].iov_base)[ 1 ] = PAP_DATA;
		((char *)niov[ i ].iov_base)[ 2 ] =
			((char *)niov[ i ].iov_base)[ 3 ] = 0;

		if ( PF_BUFSIZ( &outfile ) > PAP_MAXDATA  ) {
		    cc = PAP_MAXDATA;
		} else {
		    cc = PF_BUFSIZ( &outfile );
		    if ( outfile.pf_state & PF_EOF ) {
			((char *)niov[ 0 ].iov_base)[ 2 ] = 1;	/* eof */
			outfile.pf_state = PF_BOT;
			infile.pf_state = PF_BOT;
		    }
		}

		niov[ i ].iov_len = 4 + cc;
		bcopy( outfile.pf_cur, niov[ i ].iov_base + 4, cc );
		CONSUME( &outfile, cc );
		if ( PF_BUFSIZ( &outfile ) == 0 ) {
		    i++;
		    break;
		}
	    }
	    ssat.sat_port = readport;
	    atpb.atp_saddr = &ssat;
	    atpb.atp_sresiov = niov;
	    atpb.atp_sresiovcnt = i;	/* reported by stevebn@pc1.eos.co.uk */
	    if ( atp_sresp( atp, &atpb ) < 0 ) {
		syslog( LOG_ERR, "atp_sresp: %m" );
		exit( 1 );
	    }
	    readpending = 0;
	}
    }
}
