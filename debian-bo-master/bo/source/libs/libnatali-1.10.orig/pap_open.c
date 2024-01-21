/*
** natali/pap_open.c
** Copyright 1995, 1996, Trinity College Computing Center.
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
** This file was last modified 15 February 1996.
*/

#include "natali.h"

#include <stdlib.h>
#include <unistd.h>
#include <memory.h>

#include "appletalk.h"
#include "nbp.h"
#include "pap.h"

/*
** pap_open()
**
** When this function is called, tuple should hold the address of the print
** server to connect to, quantum should point to a variable which holds the 
** number of 512 byte buffers which we will be providing when we call pap_read()
** (the maximum is 8), status will point to a location where a pascal string with 
** the server status should be stored, and retry should be a retry structure.
**
** ^^^ incomplete ^^^
**
** Notice that the AT&T documentation says that the third argument should
** be of type char *, but that does not make sense since the first character
** which will be placed in it is a length between 0 and 255.
*/
int pap_open(at_nbptuple_t *tuple, u_short *quantum, unsigned char *status, short retry)
    {
    struct PAP *pap;
    ATP atp;
    struct atp_block atpb;	/* ATP block */
    struct iovec iov;
    BYTE cbuf[8];		/* PAP command buffer */
    BYTE rbuf[ATP_MAXDATA];	/* response buffer */
    int fd;
    time_t start_time;
    time_t now_time;
    time_t elapsed_time;

    DODEBUG(("pap_open(tuple={%d:%d:%d}, *quantum, *status, retry=%d)",
	(int)tuple->enu_addr.net, (int)tuple->enu_addr.node,
	(int)tuple->enu_addr.socket, retry));

    /* Make sure natali initialization has been done. */
    if( ! natali_started )
        natali_init();

    /* Check for bad parameters. */
    if( tuple == (at_nbptuple_t*)NULL || quantum == (u_short*)NULL
    		|| *quantum < 1 || *quantum > PAP_MAXQUANTUM
    		|| status == (unsigned char*)NULL || retry < -1 )
    	{
    	DODEBUG(("pap_open(): invalid parameters"));
    	pap_errno = PAPBADPARM;
    	return -1;
    	}
    
    /* Open an ATP endpoint. */
    if( (atp=atp_open(0)) == (ATP)NULL )
    	{
	DODEBUG(("pap_open(): atp_open() failed"));
	
	pap_errno = PAPSYSERR;
	return -1;
	}

    /*
    ** Make sure the file descriptor of the socket
    ** associated with this ATP endpoint is not too high.
    */
    if( (fd=atp_fileno(atp)) > NATALI_MAXFDS )
    	{
    	DODEBUG(("pap_open(): atp->atp_socket is %d, NATALI_MAXFDS is %d",fd,NATALI_MAXFDS));
	atp_close(atp);
    	pap_errno = PAPTOOMANY;
    	return -1;
    	}

    DODEBUG(("pap_open(): fd = %d",fd));
    	
    /* Create an entry for this connexion in the natali_fds[] array. */
    if( (pap=(struct PAP*)malloc(sizeof(struct PAP))) == (struct PAP*)NULL )
    	{
	DODEBUG(("pap_open(): malloc() failed"));
	pap_errno = PAPSYSERR;
	atp_close(atp);
	return -1;
	}
    pap->signiture = SIGNITURE_PAP;
    pap->atp = atp;			/* save ATP endpoint pointer */
    natali_fds[fd] = (void*)pap;	/* store a pointer to the structure for ref by fd */
    
    /* Fill in the connection request packet. */
    cbuf[0] = pap->connid = getpid() & 0xff;
    cbuf[1] = PAP_OpenConn;
    cbuf[2] = 0;
    cbuf[3] = 0;
    cbuf[4] = atp_sockaddr(atp)->sat_port;		/* port to respond to */
    cbuf[5] = pap->nearquantum = *quantum;		/* this end flow quantum */

    /* Fill in the destination socket address. */
    memset(&(pap->sat),0,sizeof(struct sockaddr_at));
    pap->sat.sat_family = AF_APPLETALK;
    pap->sat.sat_addr.s_net = htons(tuple->enu_addr.net);
    pap->sat.sat_addr.s_node = tuple->enu_addr.node;
    pap->sat.sat_port = tuple->enu_addr.socket;

    /* Get the start time. */
    time(&start_time);
    
    /* The connect loop. */
    while(TRUE)
    	{
	/* Fill in the current wait time in the connection request packet. */
	time(&now_time);
	elapsed_time = now_time - start_time;
	cbuf[6] = (elapsed_time & 0xFF00) >> 8;	/* big endian */
	cbuf[7] = elapsed_time & 0x00FF;

	/* Fill in the ATP block. */
	atpb.atp_saddr = &(pap->sat);
	atpb.atp_sreqdata = cbuf;
	atpb.atp_sreqdlen = 8;		/* size of data */
	atpb.atp_sreqto = 2;		/* retry timer */
	atpb.atp_sreqtries = 5;		/* retry count */

	/* Send the request. */
	DODEBUG(("pap_open(): sending request")); 
	if( atp_sreq( pap->atp, &atpb, 1, ATP_XO ) < 0 )
	    {
	    DODEBUG(("pap_open(): atp_sreq() failed, errno=%d",errno));
	    
	    pap_errno = PAPSYSERR;
	    atp_close(atp);		/* free the ATP endpoint */
	    free(pap);			/* throw away the PAP structure */
	    return -1;
	    }

	/* Read the response. */
	iov.iov_base = rbuf;
	iov.iov_len = sizeof(rbuf);
	atpb.atp_rresiov = &iov;	/* set of buffers */
	atpb.atp_rresiovcnt = 1;	/* number of buffers */
	DODEBUG(("pap_open(): reading response"));
	if( atp_rresp(pap->atp,&atpb) < 0 )
	    {
	    /*
	    ** This probably indicates that the printer has
	    ** gone off the network.  Should we retry in 
	    ** that case?  I don't know.
	    */
	    DODEBUG(("pap_open(): atp_rresp() failed, errno=%d (%s)",errno,strerror(errno)));
	    
	    if( retry != -1 && retry-- <= 0 )
	    	{
		DODEBUG(("pap_open(): retries exhausted"));
		pap_errno = PAPTIMEOUT;
		atp_close(atp);
		free(pap);
		return -1;
		}

	    continue;
	    }

	/*
	** We got a reply, what can we make of it?
	*/
	if( iov.iov_len < 9 )	/* If it is too short, */
	    {			/* ignore it. */
	    DODEBUG(("pap_open(): bad response, too short, ignoring"));
	    continue;
	    }
	if( rbuf[0] != pap->connid )
	    {
	    DODEBUG(("pap_open(): bad response, wrong connid, ignoring")); 
	    continue;		/* ignore it */
	    }
	if( rbuf[1] != PAP_OpenConnReply )
	    {
	    DODEBUG(("pap_open(): bad response, not OpenConnReply, ignoring"));
	    continue;		/* ignore it */
	    }

	/*
	** Well, it is not clearly invalid, that is something.
	** Let's copy the status string.
	*/
	{
	unsigned char *si, *di;
	int len;
	
	len = rbuf[8];			/* get the length */
	DODEBUG(("pap_open(): %d bytes of status: \"%.*s\"", len, len, &rbuf[9]));
	si = &rbuf[9];			/* point si to the start of the status string */
	di = status;			/* point di to the destination */
	*(di++) = len;			/* store the length */
	while(len--)			/* copy the string */
	    *(di++) = *(si++);	    
	}

	/*
	** Now, were we connected, or is the server busy?
	*/
	if( rbuf[6] != 0 || rbuf[7] != 0 )
	    {
	    DODEBUG(("pap_open(): server busy"));
	    
	    if( retry != -1 && retry-- <= 0 )
	    	{
		DODEBUG(("pap_open(): retries exhausted"));
		pap_errno = PAPBUSY;
		atp_close(atp);
		free(pap);
		return -1;
		}

	    DODEBUG(("sleeping for 2 seconds"));
	    sleep(2);
	    }
	else			/* connected */
	    {
	    DODEBUG(("pap_open(): connected, Rs=%d, farquantum=%d",rbuf[4],rbuf[5]));
	    pap->sat.sat_port = rbuf[4];
	    *quantum = pap->farquantum = rbuf[5];
	    break;	    
	    }

	} /* end of open loop */

    /*
    ** Finish initializing the PAP structure and start ticling.
    */
    if( natali_pap_start_tickling(pap) == -1 )
	{
	atp_close(pap->atp);
    	return -1;
    	}

    /* Return the file descriptor of the socket for this connection. */
    return fd;
    } /* end of pap_open() */

/* end of file */
