/*
** nbp_parse.c
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
** Last modified 31 October 1995.
*/

#include "natali.h"

#include <string.h>

#include "appletalk.h"
#include "nbp.h"

/*
** Parse an AppleTalk name in "name:type@zone" format and store the
** result in an at_entity_t structure.
**
** Note that there is a Netatalk function which does this but
** we don't use it becuase what it does is not close enough
** to what we want to make it worth while.
*/
int nbp_parse_entity(at_entity_t *entity, const char *str)
    {
    int namelen;
    const char *typestart;
    int typelen;
    const char *zonestart;
    int zonelen;

    #ifdef DEBUG
    natali_debug("nbp_parse_entity(*entity), \"%s\")",str);
    #endif

    namelen = strcspn(str,":");
    
    if( str[namelen] != ':' )
    	return -1;
    
    typestart = &str[namelen+1];
    typelen = strcspn(typestart,"@");
    
    if( typestart[typelen] != '@' )
    	return -1;
    	
    zonestart = &typestart[typelen+1];
    zonelen = strlen(zonestart);
    
    if( namelen > 32 || typelen > 32 || zonelen > 32 )
        return -1;	

    memcpy(entity->object.str,str,namelen);
    entity->object.len = namelen;
    
    memcpy(entity->type.str,typestart,typelen);
    entity->type.len = typelen;
    
    memcpy(entity->zone.str,zonestart,zonelen);
    entity->zone.len = zonelen;
    
    return 0;    
    } /* end of nbp_parse_entity() */

/* end of file */
