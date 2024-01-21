/*
** yp_gshadow.c              YP GShadow "map" access routines
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/

#include "config.h"

#ifdef ENABLE_YP


#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <gshadow.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"

static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;


static char **yp_parse_grp_members(char *buf)
{
   static char **members = NULL;
   int nmembers = 0;
   int member_index = 0;
   char *cp;
   

   if (Xalloc(&members, nmembers = 32) == NULL)
       return NULL;

   cp = _yp_xstrtok(buf, ',');
   while (cp)
   {
       if (member_index + 1 >= nmembers)
	   if (Xalloc(&members, nmembers += 32) == NULL)
	       return NULL;
       
       members[member_index++] = cp;

       cp = _yp_xstrtok(NULL,',');
   }

   members[member_index] = NULL;
   return (members);
}



static struct sgrp *grent_parse(char *str, int len)
{
    static struct sgrp sgrp;

    
    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;
    
    strncpy(pbuf, str, len);
    pbuf[len] = '\0';
    
    sgrp.sg_name   = _yp_xstrtok(pbuf, ':');
    if (sgrp.sg_name == NULL)
	return NULL;
    
    sgrp.sg_passwd = _yp_xstrtok(NULL, ':');
    if (sgrp.sg_passwd == NULL)
	return NULL;

    sgrp.sg_adm = NULL;

    /* Skip gid number */
    if (_yp_xstrtok(NULL, ':') == NULL)
	return NULL;

    sgrp.sg_mem = yp_parse_grp_members(_yp_xstrtok(NULL, ':'));
    if (sgrp.sg_mem == NULL)
	return NULL;
    
    return &sgrp;
}

void _yp_setsgent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void _yp_endsgent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct sgrp *_yp_getsgent(void)
{
    struct sgrp *sg;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("group", ".byname", &domain);
    if (map == NULL)
	return NULL;

    sg = NULL;
    
    if (rewind_flag)
    {
	if (yp_first(domain, map,
		     &outkey, &keylen,
		     &result, &len))
	    goto error;
	
	rewind_flag = 0;
	savekey = outkey;
	savekeylen = keylen;
    }
    else
    {
	if (yp_next(domain, map,
		    savekey, savekeylen, &outkey, &keylen,
		    &result, &len))
	    goto error;
	
	free(savekey);
	savekey = outkey;
	savekeylen = keylen;
    }

    /*
    ** Loop, fetching the next entry if there is an incorrectly
    ** formatted entry.
    */
    errno = 0;
    while ((sg = grent_parse(result, len)) == NULL && errno == 0)
    {
#ifdef DEBUG
	fprintf(stderr, "yp_gshadow: Invalid group entry: %.*s\n",
		len, result);
#endif
	free(result);
	
	if (yp_next(domain, map,
		    savekey, savekeylen, &outkey, &keylen,
		    &result, &len))
	    goto error;
	
	free(savekey);
	savekey = outkey;
	savekeylen = keylen;
    }
    
    free(result);

  error:
    free(map);
    free(domain);
    
    return sg;
}


struct sgrp *_yp_getsgnam(const char *name)
{
    struct sgrp *sg;
    char *map;
    char *domain;
    char *result;
    int len;

    map = _ypopts_getmd("group", ".byname", &domain);
    if (map == NULL)
	return NULL;

    sg = NULL;
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	sg = grent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    return sg;
}

#endif /* ENABLE_YP */
