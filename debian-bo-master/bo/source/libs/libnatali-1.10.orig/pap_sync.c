/*
** natali/pap_sync.c
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

#include "appletalk.h"
#include "pap.h"

/*
** This function is a noop.  The AT&T documentation does not
** clearly indicated what it should do.
*/
int pap_sync(int fd)
    {
    struct PAP *pap;

    DODEBUG(("pap_sync(%d)",fd));
    
    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	DODEBUG(("pap_sync(): bad file descriptor"));
    	return -1;	/* pap_errno is already set, just return */
    	}

    return 0;    
    } /* end of pap_sync() */
    
/* end of file */
