/*
** natali/natali_pap.c
** Copyright 1995, 1996, Trinity College Computing Center.
** Written by David Chappell.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software and documentation are provided "as is" without
** express or implied warranty.
**
** This module contains internal routines for PAP.
** 
** Last modified 30 January 1996.
*/

#include "natali.h"

#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "appletalk.h"
#include "pap.h"

/*
** This routine is in libatalk/atp/atp_rsel.c.  The word
** "static" must be removed from it and Netatalk recompiled.
*/
int resend_request(ATP atp);

/*
** If given a file descriptor which refers to a socket which
** is currently being used for a PAP connection, this function
** will return a pointer to the PAP structure.  If this 
** function fails it will set pap_errno and return a NULL pointer.
*/
struct PAP *natali_fd_to_pap(int fd)
    {
    if( fd < 0 || fd > NATALI_MAXFDS || natali_fds[fd] == (void*)NULL
    		|| *( (int*)natali_fds[fd] ) != SIGNITURE_PAP )
    	{
	DODEBUG(("natali_fd_to_pap(): bad fd"));

	pap_errno = PAPBADPARM;
	errno = EBADF;			/* is this overkill? */
	return (struct PAP *)NULL;
    	}

    return (struct PAP *)natali_fds[fd];    
    } /* end of natali_fd_to_pap() */

/*
** Dispatch a send credit if the other end does not have one yet.
*/
int natali_pap_sendcredit(struct PAP *pap)
    {
    struct atp_block atpb;			/* ATP block */
    BYTE cbuf[4];				/* PAP command buffer */

    DODEBUG(("natali_pap_sendcredit()"));

    if( ! pap->sent_sendcredit )
    	{
	DODEBUG(("natali_pap_sendcredit(): dispatching send credit to socket %d, connid=%d", pap->sat.sat_port, pap->connid));

	cbuf[0] = pap->connid;				/* connexion id */
	cbuf[1] = PAP_SendData;				/* command ("please send data") */

	if( ++(pap->my_seq) == 0xFFFF ) pap->my_seq = 1;/* next sequence number */

	cbuf[2] = (pap->my_seq & 0xFF00) >> 8;		/* store sequence number in */
	cbuf[3] = pap->my_seq & 0x00FF;			/* big endian order */

	atpb.atp_saddr = &(pap->sat);		/* address other end should reply to */
	atpb.atp_sreqdata = cbuf;		/* pointer to message buffer */
	atpb.atp_sreqdlen = 4;			/* size of message, 4 bytes */
	atpb.atp_sreqto = 15;			/* 15 seconds (Inside AppleTalk II p 10-10) */
	atpb.atp_sreqtries = -1;		/* infinite retries */
	
	if( atp_sreq(pap->atp, &atpb, pap->nearquantum, ATP_XO) < 0 )
	    {
	    DODEBUG(("natali_pap_sendcredit(): can't send sendcredit: atp_sreq() failed, errno=%d (%s)", errno, strerror(errno) ));
	    pap_errno = PAPSYSERR;
	    return -1;
	    }
    	
    	pap->sent_sendcredit = TRUE;
    	}

    return 0;
    } /* end of natali_pap_sendcredit() */

/*
** This is the PAP event handler.  It is called frequently from
** the various PAP library routines including pap_look().
**
** A program should use select() to wait for something to happened 
** on the socket.  It should then call pap_look() to find out what happened.
** It should not do any _harm_ to call pap_look() when nothing has happened.  
**
** This routine handles the retransmission of the tickle packet every
** 60 seconds because Netatalk's lame ATP can't do it.  It also
** calls an internal routine in the ATP library to give it a chance to
** retransmit the SendData packet without danger of getting blocked in
** a Netatalk library routine such as atp_rsel().   The select() in the
** program which calls this library should have a 5 second timeout so
** that this routine will be called often enought to perform these duties.
**
** There will not always be something to do when select() returns.
** For instance, select() has that timeout.  For this reason, this code
** calls select() again to make sure there really is something going on
** on the endpoint before it calls atp_rsel().
*/
int natali_pap_eventhandler(struct PAP *pap)
    {
    struct atp_block atpb;		/* ATP block */
    struct iovec iov;
    BYTE cbuf[8];			/* PAP command buffer */
    struct sockaddr_at ssat;
    time_t now;				/* current time */
    int seq;				/* a sequence number */
    struct timeval selto;		/* select() timeout */
    fd_set selfd;			/* select() file descriptor set */
    int selret;				/* select() return value */
    
    DODEBUG(("natali_pap_eventhandler()"));

    time( &now );

    /*
    ** This stuff does not apply to server endpoints waiting
    ** for a connexion.
    */
    if( ! pap->server )
	{
	/* Check for session timeout */
	if( (now - pap->last_recv_time) > PAP_TIMEOUT )
    	    {
    	    DODEBUG(("natali_pap_eventhandler(): session timed out, connid=%d", pap->connid));
	    pap->closed = TRUE;		/* mark the connexion as closed to prevent furthur use */
	    pap_errno = PAPHANGUP;	/* say other end has hung up */
	    return -1;			/* this is an error */
	    }

	/* If we have not sent the other end a send credit, do it now. */
	if(natali_pap_sendcredit(pap) == -1)
	    return -1;

	/* Perhaps it is time to send a tickle packet? */
	if( now - pap->last_send_time >= PAP_TICKLE_INTERVAL )
	    {
	    DODEBUG(("natali_pap_eventhandler(): sending tickle, time since last is %d seconds",now - pap->last_send_time));

	    if( sendto( pap->atp->atph_socket,
			pap->tickle_packet->atpbuf_info.atpbuf_data, pap->tickle_packet->atpbuf_dlen, 0,
			(struct sockaddr *)&pap->tickle_packet->atpbuf_addr,
			sizeof(struct sockaddr_at)) != pap->tickle_packet->atpbuf_dlen )
		{
		pap_errno = PAPSYSERR;
		return -1;
		}

	    pap->last_send_time = now;
	    }
	} /* end of if( ! pap->server ) */

    /*
    ** Now it is time to see if we have received anything.
    **
    ** If this is a server endpoint, we are interested in connexions
    ** from any socket on any node on any network.  If this is an open
    ** connexion, we are interested in stuff received from any socket
    ** on the machine we are connected to.
    */
    if( pap->server )
    	{
    	memset(&ssat, 0, sizeof(struct sockaddr_at));
    	ssat.sat_family = AF_APPLETALK;
    	ssat.sat_addr.s_net = ATADDR_ANYNET;
    	ssat.sat_addr.s_node = ATADDR_ANYNODE;
    	}
    else
    	{
	ssat = pap->sat;
	}
    ssat.sat_port = ATADDR_ANYPORT;    
    
    DODEBUG(("natali_pap_eventhandler(): calling select()"));
    FD_ZERO( &selfd );
    FD_SET( atp_fileno(pap->atp), &selfd );
    selto.tv_sec = 0;
    selto.tv_usec = 0;
    while( (selret=select(FD_SETSIZE, &selfd, (fd_set*)NULL, (fd_set*)NULL, &selto)) == -1 )
    	{
    	if( errno == EINTR )
    	    continue;
    	
	DODEBUG(("natali_pap_eventhandler(): select() failed, errno=%d (%s)", errno, strerror(errno) ));

    	pap_errno = PAPSYSERR;
    	return -1;
    	}

    DODEBUG(("natali_pap_eventhandler(): select() returned %d", selret));

    /* If something was received, */
    if( selret == 1 )
      {
      int saved_tid;

      DODEBUG(("natali_pap_eventhandler(): calling atp_rsel()"));

      switch( atp_rsel( pap->atp, &ssat, ATP_TRESP | ATP_TREQ ) )
    	{
    	case ATP_TREQ:			/* a request */
	    DODEBUG(("natali_pap_eventhandler(): request on socket for connexion %d", pap->connid));

	    /* Update our end of the connexion timeout */
	    time( &(pap->last_recv_time) );

	    /*
	    ** Save the record of last received TID so we can restore it
	    ** if this is a tickle.
	    */
	    saved_tid = pap->atp->atph_rtid;

	    /* read the request */
	    atpb.atp_saddr = &ssat;			/* once again, from any socket */
	    atpb.atp_rreqdata = cbuf;			/* read into our buffer */
	    atpb.atp_rreqdlen = sizeof(cbuf);		/* allow use of whole buffer */
	    if( atp_rreq(pap->atp, &atpb) < 0 )
	    	{
		DODEBUG(("natali_pap_eventhandler(): atp_rreq() failed!"));
		pap_errno = PAPSYSERR;
		return -1;
		}

	    DODEBUG(("natali_pap_eventhandler(): request TID is %d, saved_tid=%d",pap->atp->atph_rtid, saved_tid));

	    /*
	    ** Compare the connexion id in the request to
	    ** the connexion id of this session.
	    */
	    if( ! pap->server && cbuf[0] != pap->connid )
	    	{
	    	DODEBUG(("natali_pap_eventhandler(): request has wrong connid!"));
	    	pap_errno = PAPSYSERR;	/* unspecified sort of error */
		errno = 0;		/* do this because PAPSYSERR often result of errno set */
	    	return -1;
	    	}

	    /* Interpret the command byte in the request. */
	    switch( cbuf[1] )
	    	{
		case PAP_SendData:
		    seq = cbuf[3] + (cbuf[2] << 8);	/* read the sequence number */
		    DODEBUG(("natali_pap_eventhandler(): request is SendData, seq=%d, port=%d", seq, ssat.sat_port));
		    if( seq==0 || seq > pap->his_seq || (seq==1 && pap->his_seq==0xFFFF) )
			{
			pap->port = ssat.sat_port;
			pap->his_seq = seq;
			pap->received_sendcredit = TRUE;
			}
		    else
		    	{
		    	DODEBUG(("natali_pap_eventhandler(): duplicate SendData request"));

			if( ! pap->received_sendcredit )
			    {
			    natali_debug("*** response already sent, network error! ***");
			    /* pap->closed = TRUE;
			    pap_errno = PAPHANGUP;
			    return -1; */
			    }
		    	}
		    break;
		
		case PAP_CloseConn:	/* close connexion */
		    DODEBUG(("natali_pap_eventhandler(): request is CloseConn"));
		    cbuf[0] = pap->connid;
		    cbuf[1] = PAP_CloseConnReply;
		    cbuf[2] = 0;
		    cbuf[3] = 0;
		    iov.iov_base = cbuf;
		    iov.iov_len = 4;
		    atpb.atp_sresiov = &iov;
		    atpb.atp_sresiovcnt = 1;
		    
		    /* Respond to the close request. */
		    if( atp_sresp(pap->atp, &atpb) < 0 )
		    	{
			DODEBUG(("natali_pap_eventhandler(): PAP_CloseConn: atp_sresp() failed"));
			pap_errno = PAPSYSERR;
			return -1;		    	
		    	}

		    pap->closed = TRUE;		/* mark as closed to prevent furthur operations */
		    pap_errno = PAPHANGUP;
		    return -1;
		
		case PAP_Tickle:		/* tickle packet, just ignore it */
		    DODEBUG(("natali_pap_eventhandler(): request is Tickle, remote socket is %d, restoring saved_tid (%d)", (int)ssat.sat_port,saved_tid));
		    pap->atp->atph_rtid = saved_tid;
		    break;
		
		case PAP_SendStatus:		/* request for status */
		    DODEBUG(("natali_pap_eventhandler(): request is SendStatus"));
		    {
		    BYTE rbuf[8+256];
		    rbuf[0] = 0;			/* dummy connexion id */
		    rbuf[1] = PAP_Status;
		    rbuf[2] = 0;
		    rbuf[3] = 0;
		    rbuf[4] = rbuf[5] = rbuf[6] = rbuf[7] = 0;
		    rbuf[8] = pap->status[0];
		    memcpy( &rbuf[9], &(pap->status[1]), pap->status[0] );
		    iov.iov_base = rbuf;
		    iov.iov_len = 9 + pap->status[0];
		    atpb.atp_sresiov = &iov;
		    atpb.atp_sresiovcnt = 1;
		    
		    /* Send the response to the status request. */
		    if( atp_sresp(pap->atp, &atpb) < 0 )
		    	{
			DODEBUG(("natali_pap_eventhandler(): PAP_SendStatus: atp_sresp() failed"));
			pap_errno = PAPSYSERR;
			return -1;		    	
		    	}
		    }
		    break;
		    
		case PAP_OpenConn:		/* open connexion request */
		    DODEBUG(("natali_pap_eventhandler(): request is OpenConn"));
		    {
		    BYTE rbuf[8+256];

		    rbuf[0] = cbuf[0];			/* copy connexion id from query to answer */
		    rbuf[1] = PAP_OpenConnReply;	/* set code number */
		    rbuf[2] = 0;
		    rbuf[3] = 0;

		    if( pap->connexion_request )	/* if request already received, */
		    	{
			rbuf[4] = 0;		/* dummy ATP responding socket */
			rbuf[5] = 0;		/* dummy flow quantum */
			rbuf[6] = 0xFF;		/* result: busy */
			rbuf[7] = 0xFF;		/* result: busy */
			rbuf[8] = 4;
			strcpy(&rbuf[9],"Busy");
			iov.iov_base = rbuf;
			iov.iov_len = 8 + 1 + 4;
		    	}
		    else
		    	{
			pap->connid = cbuf[0];
			
			if( (pap->newatp = atp_open(0)) == (ATP)NULL )
			    {
			    DODEBUG(("natali_pap_eventhandler(): atp_open() failed, errno=%d (%s)",errno,strerror(errno)));
			    pap_errno = PAPSYSERR;
			    return -1;
			    }

			rbuf[4] = atp_sockaddr(pap->newatp)->sat_port;	/* reply with socket of new ATP endpoint */
			pap->farquantum = cbuf[5];			/* save his flow quantum */
			rbuf[5] = pap->nearquantum;			/* reply with our flow quantum */
			rbuf[6] = 0;					/* result: ok */
			rbuf[7] = 0;					/* result: ok */
			rbuf[8] = pap->status[0];			/* length of status */
			memcpy( &rbuf[9], &(pap->status[1]), pap->status[0] );

			pap->sat = ssat;				/* address of other end */
			pap->sat.sat_port = cbuf[4];
			
			pap->connexion_request = TRUE;		    	

			iov.iov_base = rbuf;
			iov.iov_len = 8 + 1 + pap->status[0];
		    	}
		    
		    /* Send the response. */
		    atpb.atp_sresiov = &iov;
		    atpb.atp_sresiovcnt = 1;
		    if( atp_sresp( pap->atp, &atpb ) < 0 )
		    	{
			DODEBUG(("natali_pap_eventhandler(): atp_sresp() failed, errno=%d (%s)",errno,strerror(errno)));
			pap_errno = PAPSYSERR;
			return -1;			
		    	}

		    }
		    break;

		default:
		    DODEBUG(("natali_pap_eventhandler(): unrecognized request: %d",(int)cbuf[1]));
		    pap_errno = PAPSYSERR;	/* generic error, */
		    errno = 0;			/* but not a lower level function error */
		    return -1;	    	
	    	}

	    break;
    	
    	case ATP_TRESP:			/* A response to a request or our's. */
					/* (This ought to be data.) */
	    DODEBUG(("natali_pap_eventhandler(): data received"));
 	    pap->data_received = TRUE;   	
    	    break;

	case 0:				/* nothing ready */
	    DODEBUG(("natali_pap_eventhandler(): nothing"));
	    break;

	default:			/* unknown event */
	    DODEBUG(("natali_pap_eventhandler(): atp_rsel() failed, errno=%d (%s)",errno, strerror(errno) ));
	    pap_errno = PAPSYSERR;
	    return -1;
    	}
      }

    /*
    ** This code is executed if select() returns 0.  It works
    ** around a bug in the Netatalk ATP library which prevents
    ** requests from being resent until a packet is received.
    **
    ** In order for this code to link properly, the word "static"
    ** must be removed from the definition of resend_request()
    ** in libatalk/atp/atp_rsel.c.
    */
    else
      {
      struct timeval tv;
      gettimeofday(&tv,(struct timezone *)NULL);     
      if( (tv.tv_sec - pap->atp->atph_reqtv.tv_sec) >= pap->atp->atph_reqto )
      	{
	DODEBUG(("natali_pap_eventhandler(): manually resending request"));
	if( resend_request(pap->atp) < 0 )
	    {
	    DODEBUG(("natali_pap_eventhandler(): resend_request() failed"));
	    pap_errno = PAPSYSERR;
	    return -1;
	    }      	
      	}
      }

    DODEBUG(("natali_pap_eventhandler(): normal termination"));

    return 0;    
    } /* end of natali_pap_eventhandler() */

/*
** This is called by pap_open() and paps_get_next_job().
**
** It finishes setting up the endpoint state and then
** sends the first tickle packet to the other end.  After that,
** it saves the tickle packet for future re-sending.
*/
int natali_pap_start_tickling(struct PAP *pap)
    {
    struct atp_block atpb;	/* ATP block */
    BYTE cbuf[4];		/* PAP command buffer */

    DODEBUG(("natali_pap_start_tickling()"));

    /* Set some of the initial state */
    pap->sent_sendcredit = FALSE;
    pap->received_sendcredit = FALSE;
    pap->my_seq = pap->his_seq = 0;
    pap->last_send_time = pap->last_recv_time = time((time_t*)NULL);
    pap->closed = FALSE;
    pap->data_received = FALSE;
    pap->server = FALSE;		/* This is a connexion endpoint, not a server endpoint */
    pap->write_call_blocked = FALSE;
    pap->connexion_request = FALSE;	/* prevent spurious pap_look() responses */

    /* Start tickling */
    cbuf[0] = pap->connid;			/* connexion id */
    cbuf[1] = PAP_Tickle;			/* command */
    cbuf[2] = 0;				/* these should */
    cbuf[3] = 0;				/* be zero */
    atpb.atp_saddr = &(pap->sat);		/* address to send to */
    atpb.atp_sreqdata = cbuf;			/* data to send */
    atpb.atp_sreqdlen = 4;			/* length of data, 4 bytes */
    atpb.atp_sreqto = PAP_TICKLE_INTERVAL;	/* retry timer */
    atpb.atp_sreqtries = -1;			/* retry count */
    if( atp_sreq(pap->atp, &atpb, 0, 0) < 0 )
	{
	DODEBUG(("natali_pap_start_tickling(): can't start tickle: atp_sreq() failed, errno=%d (%s)", errno, strerror(errno) ));
    	pap_errno = PAPSYSERR;
	return -1;
    	}

    /* Save the tickle packet so we can send it again. */
    if( (pap->tickle_packet = (struct atpbuf*)malloc( sizeof(struct atpbuf) )) == (struct atpbuf*)NULL )
	{
	DODEBUG(("natali_pap_start_tickling(): malloc() failed, errno=%d (%s)",errno,strerror(errno)));
	pap_errno = PAPSYSERR;
	return -1;
	}
    memcpy( pap->tickle_packet, pap->atp->atph_reqpkt, sizeof(struct atpbuf) );

    return 0;
    } /* end of natali_pap_start_tickling() */

/* end of file */

