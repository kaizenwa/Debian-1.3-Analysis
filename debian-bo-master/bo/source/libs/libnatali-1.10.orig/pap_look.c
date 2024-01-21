/*
** natali/pap_look.c
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
** Last modified 1 February 1996.
*/

#include "natali.h"

#include "appletalk.h"
#include "nbp.h"
#include "pap.h"

/*
** This routine indicates what is going on on a PAP endpoint.
** If there is an error it returns -1 and pap_errno is set to
** one of the following values:
**
** PAPSYSERR		who knows
** PAPBADPARM		fd is not a PAP endpoint
**
** If there is no error, it returns one of the following values:
**
** PAP_CONNECT_RECVD	Connexion recieved on a server endpoint
** PAP_DATA_RECVD	Data has been recieved and should be 
**			accepted with pap_read() as soon as possible,
**			certainly before calling pap_write() with the
**			mode set to PAP_WAIT
** PAP_WRITE_ENABLED	Indicates that it is now ok to call pap_write()
**			with mode set to PAP_WAIT.  Paplook() will only
**			return this value if a previous call to pap_write()
**			return -1 and set pap_errno to PAPBLOCKED
** PAP_DISCONNECT	The connexion has been broken either by the other
**			end executing pap_close() or due to a timeout
** 0			Nothing remarkable.  If pap_write() hasn't been
**			blocked previously, it may ore may not be possible
**			to write.  You will just have to try and see what
**			happens.
*/
int pap_look(int fd)
    {
    struct PAP *pap;

    #ifdef DEBUG
    natali_debug("pap_look(%d)",fd);
    #endif

    /* Try to get a pointer to the PAP structure for this fd. */
    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	#ifdef DEBUG
	natali_debug("pap_look(): bad file descriptor");
	#endif
    	return -1;	/* pap_errno is already set, just return */
    	}
    
    /* Handle any outstanding events. */
    if( natali_pap_eventhandler(pap) == -1 && pap_errno != PAPHANGUP )
    	{
    	#ifdef DEBUG
    	natali_debug("pap_look(): natali_pap_eventhandler() returned -1, pap_errno=%d",pap_errno);
	#endif
	return -1;
	}

    /* If endpoint is closed, */
    if( pap->closed )
    	return PAP_DISCONNECT;
    	
    /* If a connexion request has been received (server endpoints), */
    if( pap->connexion_request )
	{
	if( ! pap->server )
	    {
	    DODEBUG(("pap_look(): connexion request but no a server endpoint"));
	    pap_errno = PAPSYSERR;
	    return -1;
	    }
    	return PAP_CONNECT_RECVD;
    	}

    /* If data has been received, */
    if( pap->data_received )
    	return PAP_DATA_RECVD;
    
    /* If we have a sendcredit and pap_write() was blocked, */
    if( pap->received_sendcredit && pap->write_call_blocked )
    	return PAP_WRITE_ENABLED;
    	
    /* Nothing worth noteing, */
    return 0;
    } /* end of pap_look() */

/* end of file */
