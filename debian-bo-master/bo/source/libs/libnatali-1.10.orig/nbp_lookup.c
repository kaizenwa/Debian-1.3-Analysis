/*
** natali/nbp_lookup.c
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
** Last modified 30 October 1995.
*/

#include "natali.h"

#include <unistd.h>
#include <errno.h>
#include <stdlib.h>

#include "appletalk.h"
#include "nbp.h"

/*
** This is not called npb_lookup() because the Netatalk library
** already contains a function with that name.  We fix this
** in nbp.h with a macro.
*/
int natali_nbp_lookup(at_entity_t *entity,	/* name to look up */
		 at_nbptuple_t *buf,		/* buffer for returned data */
		 int max, 			/* slots in buffer */
		 at_retry_t *retry,		/* retry info structure */
		 u_char *more)			/* set to non-zero if not all fit in buffer */
    {
    char obj[33];		/* buffer for object name to look up */
    char type[33];		/* buffer for object type to look up */
    char zone[33];
    struct nbpnve *nn;
    int result;			/* result of calling Netatalk function */
    int retries;		/* number of retries to do */
    int delaytime;		/* delay between retries in seconds */
    int x;
    
    #ifdef DEBUG
    natali_debug("nbp_lookup()");
    #endif

    /* Look for various types of bad parameter. */
    if(max < 1 || entity == (at_entity_t *)NULL 
    		|| buf == (at_nbptuple_t *)NULL || more == (u_char*)NULL )
    	{
	#ifdef DEBUG
	natali_debug("nbp_lookup(): bad parameter");
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
    natali_debug("\"%s\" \"%s\" \"%s\"",obj,type,zone);
    #endif

    /* Allocate enough Netatalk Name Binding Protocol */
    /* Network Visible Entity records. */
    if( (nn = (struct nbpnve*)malloc( (max+1) * sizeof(struct nbpnve))) == (struct nbpnve*)NULL )
    	{
    	#ifdef DEBUG
    	natali_debug("nbp_lookup() malloc() failed, errno=%d",errno);
    	#endif

	nbp_errno = NBPSYSERR;
	return -1;
	}

    /* If the retry structure is not NULL, pull out the values. */
    if(retry != (at_retry_t *)NULL)
    	{
	retries = retry->retries;
	delaytime = retry->interval;
    	}
    else	/* If it is NULL, use default values. */
    	{
	retries = 8;
	delaytime = 1;
    	}

    /* Call Netatalk's nbp_lookup() function, calling it again */
    /* and again if we should do retries.                      */
    do	{
	#ifdef DEBUG
	natali_debug("trying...");
	#endif

	result = nbp_lookup(obj,type,zone,nn,max+1);

    	if(result < 0)
    	    {
	    #ifdef DEBUG
	    natali_debug("nbp_lookup() failed, result=%d, errno=%d",result,errno);
	    #endif

    	    nbp_errno = NBPSYSERR;
    	    return -1;
	    }

	if(result == 0 && retries)
	    {
	    #ifdef DEBUG
	    natali_debug("sleep(%d)",delaytime);
	    #endif

	    sleep(delaytime);
	    }
	    
	} while(result == 0 && retries-- );
    
    /* Determine if we got more entries than the */
    /* caller provided buffer space for.         */
    if(result > max)
    	{
    	*more = 1;
    	result = max;
    	}
    else
    	{
    	*more = 0;
    	}

    /* Convert the result from Netatalk format to ALI format. */
    for(x=0; x < result; x++)
	{
	#ifdef DEBUG
	natali_debug("-> %d %.*s:%.*s@%.*s %u.%d:%d",x,
		nn[x].nn_objlen,nn[x].nn_obj,
		nn[x].nn_typelen,nn[x].nn_type,
		nn[x].nn_zonelen,nn[x].nn_zone,
		ntohs(nn[x].nn_sat.sat_addr.s_net),
		nn[x].nn_sat.sat_addr.s_node,
		nn[x].nn_sat.sat_port);
	#endif

	buf[x].enu_entity.object.len = nn[x].nn_objlen;
	memcpy( buf[x].enu_entity.object.str, nn[x].nn_obj, nn[x].nn_objlen );

	buf[x].enu_entity.type.len = nn[x].nn_typelen;
	memcpy( buf[x].enu_entity.type.str, nn[x].nn_type, nn[x].nn_typelen );

	buf[x].enu_entity.zone.len = nn[x].nn_zonelen;
	memcpy( buf[x].enu_entity.zone.str, nn[x].nn_zone, nn[x].nn_zonelen );

	buf[x].enu_addr.net = ntohs(nn[x].nn_sat.sat_addr.s_net);
	buf[x].enu_addr.node = nn[x].nn_sat.sat_addr.s_node;
	buf[x].enu_addr.socket = nn[x].nn_sat.sat_port;
	
	buf[x].enu_enum = x;
	}

    /* Free our temporary buffer. */
    free(nn);

    return result;
    } /* end of nbp_lookup() */
    
/* end of file */
