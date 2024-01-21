/*
** natali/pap_write.c
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
** Last modified 19 January 1996.
*/

#include "natali.h"

#include <errno.h>
#include <string.h>

#include "appletalk.h"
#include "pap.h"

int pap_write(int fd, const char *data, int len, u_char eof_flag, u_char mode)
    {
    struct PAP *pap;			/* pointer to the PAP structure */
    int x;
    const char *ptr;
    int lenleft;
    int thislen;
    struct sockaddr_at ssat;
    struct atp_block atpb;

    BYTE buffer[PAP_MAXQUANTUM][512+4];	/* buffer to hold data to be sent */

    struct iovec iov[PAP_MAXQUANTUM] = 	/* pointers to the sections of the buffer */
    	{
	{ buffer[0], 0 },
	{ buffer[1], 0 },
	{ buffer[2], 0 },
	{ buffer[3], 0 },
	{ buffer[4], 0 },
	{ buffer[5], 0 },
	{ buffer[6], 0 },
	{ buffer[7], 0 }
    	} ;

    DODEBUG(("pap_write(fd=%d, data=\"%.*s\", len=%d, eof_flag=%d, mode=%d)", fd,
    	len < 16 ? len : 16, data != (char*)NULL ? data : "<NULL>", len, eof_flag, mode));

    /* Find the PAP structure which is associated with this file descriptor. */
    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	DODEBUG(("pap_write(): bad file descriptor"));
    	return -1;	/* pap_errno is already set, just return */
    	}

    /* Check the arguments for sanity. */
    if( data == (char*)NULL || len < 0 
    		|| ( mode != PAP_NOWAIT && mode != PAP_WAIT ) )
    	{
    	DODEBUG(("pap_write(): invalid parameter"));
    	pap_errno = PAPBADPARM;
    	return -1;
    	}

    /*
    ** pap_write() will not accept a block of data whose
    ** size is greater than the remote flow quantum allows.
    */
    if( len > ( pap->farquantum * 512 ) )
    	{
    	DODEBUG(("pap_write(): data size too large"));
    	pap_errno = PAPSIZERR;
    	return -1;
    	}

    /* See if this endpoint is already closed. */
    if( pap->closed )
    	{
    	DODEBUG(("pap_write(): PAP endpoint is closed"));
    	pap_errno = PAPHANGUP;
    	return -1;
    	}

    /*
    ** Clear the pap_write() was blocked flag, we will
    ** set it again if necessary.
    */
    pap->write_call_blocked = FALSE;

    /*
    ** Wait mode
    */
    if( mode == PAP_WAIT )
    	{
	struct timeval select_timeout;
	fd_set fds;
	
	DODEBUG(("pap_write(): mode is PAP_WAIT"));

	while( ! pap->received_sendcredit )
	    {
	    select_timeout.tv_sec = PAP_TICKLE_INTERVAL;
	    select_timeout.tv_usec = 0;
	    FD_ZERO( &fds );
	    FD_SET( fd, &fds );
	
	    DODEBUG(("pap_write(): waiting for send credit, select()"));

	    if( select(FD_SETSIZE, &fds, (fd_set*)NULL, (fd_set*)NULL, &select_timeout) == -1 ) 
		{
		DODEBUG(("pap_write(): select() failed, errno=%d (%s)", errno, strerror(errno) ));
		pap_errno = PAPSYSERR;
		return -1;
		}
	    
	    if( natali_pap_eventhandler(pap) == -1 )
	    	{
	    	DODEBUG(("pap_write(): natali_pap_eventhandler() failed, pap_errno=%d",pap_errno));
	    	return -1;
	    	}

	    if( pap->data_received )		/* If data has been received, */
	    	{				/* interupt pap_write(). */
	    	DODEBUG(("pap_write(): data received"));
	    	pap_errno = PAPDATARECD;
	    	return -1;
	    	}
	    }

	DODEBUG(("pap_write(): got send credit"));
    	}

    /*
    ** PAP_NOWAIT mode
    */
    else
	{
	DODEBUG(("pap_write(): mode is PAP_NOWAIT"));

	if( natali_pap_eventhandler(pap) == -1 )
	    {
	    DODEBUG(("pap_write(): natali_pap_eventhandler() failed, pap_errno=%d",pap_errno));
	    return -1;
	    }

	if( ! pap->received_sendcredit )
	    {
	    DODEBUG(("pap_write(): no send credit"));
	    pap_errno = PAPBLOCKED;
	    pap->write_call_blocked = TRUE;
	    return -1;
	    }    	
	}

    /*
    ** Copy the data into the buffer.
    */
    ptr = data;			/* roving pointer to the data */
    lenleft = len;		/* decreasing count of data to be buffered */
    x = 0;			/* buffer index */
    do	{
	thislen = lenleft <= 512 ? lenleft : 512;	/* take no more than 512 bytes at a gulp */

	DODEBUG(("pap_write(): block %d, thislen=%d, lenleft=%d, connid=%d", x, thislen, lenleft, pap->connid));

	buffer[x][0] = pap->connid;	/* connexion id */
	buffer[x][1] = PAP_Data;	/* this is a data block */
	buffer[x][2] = 0;		/* end of file */
	buffer[x][3] = 0;		/* this byte should be zero */

	memcpy( &buffer[x][4], ptr, thislen );	/* copy this gulp */
	iov[x].iov_len = thislen + 4;		/* store the length of this gulp */

	DODEBUG(("-->%.*s",thislen,&buffer[x][4]));

	ptr += thislen;
	lenleft -= thislen;	/* reduce record of work left to do */

	/*
	** If we have been asked to send EOF and this is the last block,
	** set the EOF bit.
	**
	** (Inside AppleTalk page 10-10, paragraph 3 seems to say that
	** the EOF bit should be set only on the last block, but a Mac
	** sets it in all blocks.)
	*/
	if(lenleft==0 && eof_flag)
	    {
	    DODEBUG(("pap_write(): ^^^ setting EOF on block %d ^^^",x));
	    buffer[x][2] = 1;
	    }

	x++;
    	} while( lenleft > 0 );

    /*
    ** Do the actual sending.
    */
    DODEBUG(("pap_write(): sending %d block(s), %d byte(s) to remote socket %d in response to seq %d", x, len, pap->port, pap->his_seq));
    ssat = pap->sat;			/* Use address of other end */
    ssat.sat_port = pap->port;		/* with port number that SendData came from. */
    atpb.atp_saddr = &ssat;
    atpb.atp_sresiov = iov;
    atpb.atp_sresiovcnt = x;
    
    if( atp_sresp(pap->atp, &atpb) < 0 )
    	{
	DODEBUG(("pap_write(): atp_sresp() failed, errno=%d (%s)", errno, strerror(errno)));
	pap_errno = PAPSYSERR;
	return -1;    	
    	}    
    
    pap->received_sendcredit = FALSE;	/* we have used up our sendcredit */

    DODEBUG(("pap_write() done"));
    return 0;				/* return sucess indicashun */
    } /* end of pap_write() */

/* end of file */
