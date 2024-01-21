/*
** natali/pap_read.c
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

int pap_read(int fd, char *data, int len, u_char *eof_flag)
    {
    struct PAP *pap;
    struct timeval select_timeout;
    struct sockaddr_at ssat;
    int retval;
    struct atp_block atpb;
    int x;
    char *ptr;
    int blocklen;
    int selret;

    BYTE buffer[PAP_MAXQUANTUM][512+4];

    struct iovec iov[PAP_MAXQUANTUM] = 
    	{
	{ buffer[0], 512+4 },
	{ buffer[1], 512+4 },
	{ buffer[2], 512+4 },
	{ buffer[3], 512+4 },
	{ buffer[4], 512+4 },
	{ buffer[5], 512+4 },
	{ buffer[6], 512+4 },
	{ buffer[7], 512+4 }
    	} ;

    DODEBUG(("pap_read(fd=%d, data=?, len=%d, *eof_flag=?)", fd, len));
    
    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	DODEBUG(("pap_read(): bad file descriptor"));
    	return -1;	/* pap_errno is already set, just return */
    	}

    if( data == (char*)NULL || eof_flag == (u_char*)NULL )
    	{
	DODEBUG(("pap_read(): invalid parameter"));
	pap_errno = PAPBADPARM;
	return -1;
    	}

    if( len < (pap->nearquantum * 512) ) 
    	{
	DODEBUG(("pap_read(): buffer is too small"));
	pap_errno = PAPSIZERR;
	return -1;
    	}

    if( pap->closed )
    	{
    	DODEBUG(("PAP endpoint is closed"));
    	pap_errno = PAPHANGUP;
    	return -1;
    	}

    /* If no data has been received, wait for it. */
    while( ! pap->data_received )
	{
	fd_set fds;
	
	select_timeout.tv_sec = 60;
	select_timeout.tv_usec = 0;

	FD_ZERO( &fds );
	FD_SET( fd, &fds );
	
	DODEBUG(("pap_read(): waiting for data"));

	while( (selret=select(FD_SETSIZE, &fds, (fd_set*)NULL, (fd_set*)NULL, &select_timeout)) == -1 )
	    {
	    if( errno == EINTR )
	    	continue;

	    DODEBUG(("pap_read(): select() failed, errno=%d",errno));
	    
	    pap_errno = PAPSYSERR;
	    return -1;
	    }
	
	DODEBUG(("pap_read(): select returned %d", selret));

	if( natali_pap_eventhandler(pap) == -1 )
	    {
	    DODEBUG(("pap_read(): natali_pap_eventhandler() failed, pap_errno=%d",pap_errno));
	    return -1;
	    }
	}

    /* Receive the data */
    DODEBUG(("pap_read(): reading data"));
    ssat = pap->sat;
    ssat.sat_port = ATADDR_ANYPORT;    
    atpb.atp_saddr = &ssat;
    atpb.atp_rresiov = iov;
    atpb.atp_rresiovcnt = pap->nearquantum;
    if( atp_rresp(pap->atp, &atpb) < 0 )
    	{
	DODEBUG(("pap_read(): atp_rresp() failed, errno=%d (%s)",errno,strerror(errno)));
	pap_errno = PAPSYSERR;
	return -1;    	
    	}

    /* Copy the data into the caller's buffer. */
    *eof_flag = 0;		/* set to zero for now */
    retval = 0;			/* bytes read so far */
    ptr = data;			/* roving pointer to the buffer */
    for(x=0; x < atpb.atp_rresiovcnt; x++)
	{
	DODEBUG(("pap_read(): block %d, length = (%d - 4)", x, iov[x].iov_len));

	/* Check the received data for validity. */
	if( iov[x].iov_len < 4 )
	    {
	    DODEBUG(("pap_read(): received data block %d is too short to be valid",x));
	    pap_errno = PAPSYSERR;
	    return -1;	    
	    }
	if( buffer[x][0] != pap->connid )
	    {
	    DODEBUG(("pap_read(): received data block %d has wrong connid",x));
	    pap_errno = PAPSYSERR;
	    return -1;
    	    }
	if( buffer[x][1] != PAP_Data )
    	    {
	    DODEBUG(("pap_read(): received data block %d is in unrecognized format",x));
	    pap_errno = PAPSYSERR;
	    return -1;
    	    }

	blocklen = iov[x].iov_len - 4;			/* length of this block */
	memcpy(ptr, &buffer[x][4], blocklen);		/* copy the block to the buffer */
	ptr += blocklen;				/* advance pointer */
	retval += blocklen;				/* add byte count to return value */
	
	DODEBUG(("-->%.*s", blocklen, &buffer[x][4]));

	if( buffer[x][2] )
	    {
	    DODEBUG(("pap_read(): *** EOF in block %d ***", x));
	    *eof_flag = 1;
	    /* break; */
	    }
	}

    /*
    ** Send another send credit right away.
    ** If we don't do this, things slow to a crawl.
    */
    DODEBUG(("pap_read(): calling natali_pap_sendcredit()"));
    pap->data_received = FALSE;
    pap->sent_sendcredit = FALSE;
    if( natali_pap_sendcredit(pap) == -1 )
	{
	DODEBUG(("pap_read(): natali_pap_sendcredit() failed, pap_errno=%d, errno=%d (%s)",
	    pap_errno, errno, strerror(errno) ));
	return -1;
	}

    DODEBUG(("pap_read(): %d bytes read, *eof_flag=%d", retval, *eof_flag));

    return retval;
    } /* end of pap_read() */

/* end of file */
