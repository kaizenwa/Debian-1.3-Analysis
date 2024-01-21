/*
** natali/paps_open.c
** Copyright 1995, 1996, Trinity College Computing Center.
** Written by David.Chappell.
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
** Last modified 30 January 1996.
*/

#include "natali.h"

#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "appletalk.h"
#include "pap.h"

int paps_open(u_short quantum)
    {
    ATP atp;
    int fd;
    struct PAP *pap;
    
    /* Make sure the parameter is valid. */
    if( quantum < 1 || quantum > 8 )
    	{
    	#ifdef DEBUG
    	natali_debug("paps_open(): quantum is out of range");
    	#endif
    	pap_errno = PAPBADPARM;
    	return -1;
    	}

    /* Make sure natali initialization has been done. */
    if( ! natali_started )
        natali_init();

    /* Create a PAP endpoint to accept connexions on */
    if( (atp=atp_open(0)) == (ATP)NULL )
    	{
	#ifdef DEBUG
	natali_debug("paps_open(): atp_open() failed, errno=%d (%s)",errno,strerror(errno));
	#endif
	pap_errno = PAPSYSERR;
	return -1;
    	}

    /* Make sure the file descriptor of the socket        */
    /* associated with this ATP endpoint is not too high. */
    if( (fd=atp_fileno(atp)) > NATALI_MAXFDS )
    	{
    	#ifdef DEBUG
    	natali_debug("paps_open(): atp->atp_socket is %d, NATALI_MAXFDS is %d",fd,NATALI_MAXFDS);
    	#endif
	atp_close(atp);			/* free data structure and socket */
    	pap_errno = PAPTOOMANY;
    	return -1;
    	}

    #ifdef DEBUG
    natali_debug("paps_open(): fd = %d",fd);
    #endif
    	
    /* Create an entry for this connection in the natali_fds[] array. */
    if( (pap=(struct PAP*)malloc(sizeof(struct PAP))) == (struct PAP*)NULL )
    	{
	#ifdef DEBUG
	natali_debug("paps_open(): malloc() failed");
	#endif
	pap_errno = PAPSYSERR;
	atp_close(atp);
	return -1;
	}

    pap->signiture = SIGNITURE_PAP;	/* mark this as a PAP structure */
    pap->atp = atp;			/* save ATP endpoint pointer */
    pap->nearquantum = quantum;		/* save our quantum */
    pap->connexion_request = FALSE;
    pap->status[0] = 7;			/* install a dummy */
    strcpy(&pap->status[1],"<unset>");	/* status value */
    pap->closed = FALSE;
    pap->server = TRUE;			/* is a server endpoint */

    pap->data_received = FALSE;		/* Prevent spurious responses in */
    pap->sent_sendcredit = FALSE;	/* pap_look().  (Suggested by */
    pap->received_sendcredit = FALSE;	/* Damian Ivereigh <damien@cisco.com>.) */
    pap->write_call_blocked = FALSE;

    natali_fds[fd] = (void*)pap;	/* store a pointer to the structure for ref by fd */
    
    return fd;				/* success!  return the file descriptor */
    } /* end of paps_open() */

/* end of file */
