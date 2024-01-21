/*
** natali/pap_close.c
** Copyright 1995, Trinity College Computing Center.
** Writen by David.Chappell.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software and documentation are provided "as is" without
** express or implied warranty.
**
** This file is part of an AppleTalk Library Interface compatible library
** for Netatalk. 
**
** Last modified 22 December 1995.
*/

#include "natali.h"

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "appletalk.h"
#include "pap.h"

/*
** Send a disconnect indication to the other end, wait for a 
** response, and then destroy the endpoint together with
** its data structures.
*/
int pap_close(int fd)
    {
    struct PAP *pap;
    BYTE cbuf[8];
    BYTE rbuf[ATP_MAXDATA];
    struct iovec iov;
    struct atp_block atpb;

    #ifdef DEBUG
    natali_debug("pap_close(%d)",fd);
    #endif
    
    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	DODEBUG(("pap_close(): bad file descriptor"));
    	return -1;	/* pap_errno is already set, just return */
    	}

    pap_errno = 0;

    /* If the session had not already been terminated and this is not */
    /* a server endpoint, send a close request to the other end.      */
    if( ! pap->closed && ! pap->server )
	{
	/* Set up a connexion close request packet. */
	cbuf[0] = pap->connid;
	cbuf[1] = PAP_CloseConn;
	cbuf[2] = 0;
	cbuf[3] = 0;

	/* Set up the structure to describe the command. */
	atpb.atp_saddr = &(pap->sat);
	atpb.atp_sreqdata = cbuf;
	atpb.atp_sreqdlen = 4;    
	atpb.atp_sreqto = 2;		/* timeout */
	atpb.atp_sreqtries = 5;		/* retries */

	/* Send it. */
	if( atp_sreq(pap->atp, &atpb, 1, ATP_XO) < 0 )
	    {
	    DODEBUG(("pap_close(): atp_sreq() failed, errno=%d",errno));
	    pap_errno = PAPSYSERR;	/* not technically legal */
	    }

	else				/* no error sending response */
	    {
	    /* Read the response. */
	    iov.iov_base = rbuf;
	    iov.iov_len = sizeof(rbuf);
	    atpb.atp_rresiov = &iov;
	    atpb.atp_rresiovcnt = 1;
	    if( atp_rresp(pap->atp, &atpb) < 0 )
		{
		DODEBUG(("pap_close(): atp_rresp() failed, errno=%d (%s)", errno, strerror(errno) ));
		pap_errno = PAPSYSERR;		/* not technically legal */
		}
	    else
		{
		/* Make sure the response is correct. */
		if( iov.iov_len != 4 )
		    {
       	    	    DODEBUG(("pap_close(): response is of incorrect length"));
		    pap_errno = PAPSYSERR;
       	    	    }
		else if( rbuf[0] != pap->connid )
		    {
           	    DODEBUG(("pap_close(): response has incorrect connid"));
           	    pap_errno = PAPSYSERR;
           	    }
       	    	else if( rbuf[1] != PAP_CloseConnReply )
           	    {
           	    DODEBUG(("pap_close(): response is not CloseConnReply"));
           	    pap_errno = PAPSYSERR;
		    }
		}
	    }

	} /* end of if session close request should be sent to other end  */

    atp_close(pap->atp);		/* ATP endpoint */

    natali_fds[fd] = (void*)NULL;	/* clear entry in fd array */

    free( (void*)pap );			/* free memory occupied by PAP structure */

    if(pap_errno)
    	return -1;
    else
	return 0;    
    } /* end of pap_close() */

/*
** This is like the function above except that it does not
** send a disconnect message to the other end.  This is not 
** a standard part of ALI.  Under System V the equivelent may
** be acomplished with close().
*/
int pap_abrupt_close(int fd)
    {
    struct PAP *pap;

    DODEBUG(("pap_abrupt_close(%d)",fd));
    
    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	DODEBUG(("pap_abrupt_close(): bad file descriptor"));
    	return -1;			/* pap_errno is already set, just return */
    	}

    atp_close(pap->atp);		/* ATP endpoint */

    natali_fds[fd] = (void*)NULL;	/* clear entry in fd array */

    if( ! pap->server )
    	free(pap->tickle_packet);

    free( (void*)pap );			/* free memory occupied by PAP structure */

    return 0;    
    } /* end of pap_abrupt_close() */

/* end of file */
