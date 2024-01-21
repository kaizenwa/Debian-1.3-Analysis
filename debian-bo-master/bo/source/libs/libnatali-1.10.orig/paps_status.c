/*
** natali/paps_status.c
** Copyright 1995, Trinity College Computing Center.
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
** Last modified 22 December 1995.
*/

#include "natali.h"

#include <memory.h>

#include "appletalk.h"
#include "pap.h"

int paps_status(int fd, char *status)
    {
    struct PAP *pap;
    int len;

    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	#ifdef DEBUG
	natali_debug("paps_status(): bad file descriptor");
	#endif
    	return -1;	/* pap_errno is already set, just return */
    	}

    if( status == (char*)NULL )
    	{
	#ifdef DEBUG
	natali_debug("paps_status(): invalid parameter");
	#endif
	pap_errno = PAPBADPARM;
	return -1;
    	}

    len = pap->status[0] = status[0];
    memcpy( &pap->status[1], &status[1], len);

    return 0;    
    } /* end of paps_status() */

/* end of file */
