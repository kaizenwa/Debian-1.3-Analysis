/*
** natali/nbp_register.c
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

#include <errno.h>
#include <string.h>

#include "appletalk.h"
#include "nbp.h"

int nbp_register(at_entity_t *entity, int fd, at_retry_t *retry)
    {
    struct PAP *pap;		/* a kludge */

    char obj[33];		/* buffer for object name to look up */
    char type[33];		/* buffer for object type to look up */
    char zone[33];
    
    #ifdef DEBUG
    natali_debug("nbp_register()");
    #endif

    if( entity == (at_entity_t *)NULL )
    	{
	#ifdef DEBUG
	natali_debug("nbp_register(): invalid parameters");
	#endif
    	nbp_errno = NBPBADPARM;
    	return -1;
    	}

    memcpy(obj,entity->object.str,entity->object.len);
    obj[(int)entity->object.len] = (char)NULL;

    memcpy(type,entity->type.str,entity->type.len);
    type[(int)entity->type.len] = (char)NULL;

    memcpy(zone,entity->zone.str,entity->zone.len);
    zone[(int)entity->zone.len] = (char)NULL;

    /* This will work only for PAP */
    if( (pap=natali_fd_to_pap(fd)) == (struct PAP *)NULL )
    	{
	#ifdef DEBUG
	natali_debug("pap_read(): bad file descriptor");
	#endif
    	return -1;	/* pap_errno is already set, just return */
    	}

    #ifdef DEBUG
    natali_debug("nbp_register(): nbp_rgstr(sock, \"%s\", \"%s\", \"%s\")", obj, type, zone);
    #endif

    if( nbp_rgstr( atp_sockaddr(pap->atp), obj, type, zone ) < 0 )
    	{
	#ifdef DEBUG
	natali_debug("errno=%d",errno);
	natali_debug("nbp_register: nbp_rgstr() failed, errno=%d (%s)", errno, strerror(errno));
	#endif
	if( errno == EEXIST )			/* name already exists */
	    nbp_errno = NBPDUPNAME;
	else if( errno == EINVAL )		/* name component has wrong length */
	    nbp_errno = NBPBADNAME;
	else					/* other */
	    nbp_errno = NBPSYSERR;
	return -1;    	
    	}

    return 0;
    } /* end of nbp_register() */
    
/* end of file */
