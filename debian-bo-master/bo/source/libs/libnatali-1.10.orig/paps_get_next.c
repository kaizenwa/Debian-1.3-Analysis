/*
** natali/paps_get_next.c
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
#include <time.h>
#include <stdlib.h>
#include <unistd.h>

#include "appletalk.h"
#include "pap.h"

/*
** This function should be called when a server endpoint reports
** a connexion request has arrived.  (Such an even is reported by
** pap_look().)
**
** The argument fd is the file descriptor of the server endpoint.
** The argument quantum is a pointer to a variable which contains 
** the size in 512 byte blocks of the buffer this side will use 
** when calling pap_read().  On return this same variable will be
** set to the buffer size of the remote end expressed in the same
** units.
**
*/
int paps_get_next_job(int fd, u_short *quantum, at_inet_t *src)
    {
    int nfd;
    struct PAP *srv_pap, *pap;
    ATP atp;

    DODEBUG(("paps_get_next_job(%d, quantum, src)",fd));

    /* Find the PAP structure which describes the server endpoint. */
    if( (srv_pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	DODEBUG(("paps_get_next_job(): bad file descriptor"));
    	return -1;	/* pap_errno is already set, just return */
    	}

    /* Check for a bad flow quantum parameter. */
    if( quantum == (u_short*)NULL )
    	{
	DODEBUG(("paps_get_next_job(): invalid quantum pointer"));
    	pap_errno = PAPBADPARM;
    	return -1;
    	}

    /* Make sure there really is a connexion request. */
    if( ! srv_pap->connexion_request )
    	{
    	DODEBUG(("paps_get_next_job(): no connexion request"));
	pap_errno = PAPBUSY;	/* unofficial response !!! */    	
	return -1;    	
    	}

    /* We have used this one up */
    srv_pap->connexion_request = FALSE;

    /* main endpoint is currently stored in server tickle */
    atp = srv_pap->newatp;

    /* Create an entry for this connection in the natali_fds[] array. */
    if( (pap=(struct PAP*)malloc(sizeof(struct PAP))) == (struct PAP*)NULL )
    	{
	DODEBUG(("paps_get_next_job(): malloc() failed"));
	pap_errno = PAPSYSERR;
	atp_close(atp);
	return -1;
	}
    pap->signiture = SIGNITURE_PAP;
    pap->atp = atp;			/* save ATP endpoint pointer */

    /*
    ** The new file descriptor which we will return is the one which 
    ** identifies the socket associated with this ATP endpoint.
    */
    nfd = atp_fileno(atp);

    pap->sat = srv_pap->sat;		/* address of other end */
    pap->connid = srv_pap->connid;	/* connexion id */	
    pap->nearquantum = srv_pap->nearquantum;
    pap->farquantum = srv_pap->farquantum;

    /*
    ** Finish initializing the PAP structure and start tickling.
    */
    if( natali_pap_start_tickling(pap) == -1 )
	{
	atp_close(pap->atp);
    	return -1;
    	}

    /* Put our new PAP structure in the fd table. */
    natali_fds[nfd] = (void*)pap;

    /* Feed the far quantum to our caller. */
    *quantum = pap->farquantum;

    /* If wanted, feed back the address of the caller. */
    if( src != (at_inet_t*)NULL )
    	{
	src->net = ntohs(pap->sat.sat_addr.s_net);
	src->node = pap->sat.sat_addr.s_node;
	src->socket = pap->sat.sat_port;    	
    	}

    DODEBUG(("paps_get_next_job(): returning fd %d",nfd));

    return nfd;
    } /* end of paps_get_next_job() */

/* end of file */
