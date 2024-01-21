/*
** natali/nbp_remove.c
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
** Last modified 12 February 1996.
*/

#include "natali.h"

#include <string.h>
#include <errno.h>

#include "appletalk.h"
#include "nbp.h"

int nbp_remove(at_entity_t *entity, int fd)
    {
    char obj[33];		/* buffer for object name to look up */
    char type[33];		/* buffer for object type to look up */
    char zone[33];
    
    #ifdef DEBUG
    natali_debug("nbp_remove()");
    #endif

    if( entity == (at_entity_t *)NULL )
    	{
	#ifdef DEBUG
	natali_debug("nbp_remove(): invalid parameters");
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

    #ifdef DEBUG
    natali_debug("nbp_remove(): %s %s %s",obj,type,zone);
    #endif

#ifndef BUG
    if( nbp_unrgstr( obj, type, zone ) < 0 )
#else
    if( nbp_unrgstr( obj, type, "*" ) < 0 )
#endif
    	{
	#ifdef DEBUG
	natali_debug("nbp_remove: nbp_unrgstr() failed, errno=%d (%s)", errno, strerror(errno));
	#endif
	nbp_errno = NBPSYSERR;
	return -1;    	
    	}

    return 0;
    } /* end of nbp_remove() */

/* end of file */
