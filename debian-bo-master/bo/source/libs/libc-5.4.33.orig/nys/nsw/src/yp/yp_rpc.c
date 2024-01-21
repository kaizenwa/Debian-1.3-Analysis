/*
** yp_rpc.c              YP Rpc map access routines
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
*/

#include "config.h"

#ifdef ENABLE_YP

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <netdb/rpc.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"


static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;


static struct rpcent *
rpcent_parse(char *str, int len)
{
    static struct rpcent rpe;
    char *cp;
    int alen;
    int i;


    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;
    
    strncpy(pbuf, str, len);
    pbuf[len] = '\0';
    
    rpe.r_name   = _yp_xstrtok(pbuf, ' ');
    if (rpe.r_name == NULL)
	return NULL;
    
    cp = _yp_xstrtok(NULL, ' ');
    if (cp == NULL)
	return NULL;
    rpe.r_number = atoi(cp);

    /* Build alias list */
    alen = 1;
    if (Xalloc(&rpe.r_aliases, alen+1) == NULL)
	return NULL;

    i = 0;
    while ((cp = _yp_xstrtok(NULL, ' ')) && cp[0] != '#')
    {
	if (i >= alen)
	{
	    alen += 10;
	    if (Xalloc(&rpe.r_aliases, alen+1) == NULL)
		return NULL;
	}
	rpe.r_aliases[i++] = cp;
    }

    rpe.r_aliases[i] = NULL;

    return &rpe;
}



void
_yp_setrpcent(int stayopen)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void
_yp_endrpcent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct rpcent *
_yp_getrpcent(void)
{
    struct rpcent *rpe;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("rpc", ".byname", &domain);
    if (map == NULL)
	return NULL;

    rpe = NULL;
    
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
    while ((rpe = rpcent_parse(result, len)) == NULL && errno == 0)
    {
#ifdef DEBUG
	fprintf(stderr, "yp_rpc: Invalid rpc entry: %.*s\n",
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
    
    return rpe;
}


struct rpcent *
_yp_getrpcbyname(const char *name)
{
    struct rpcent *rpp;
    char *map;
    char *domain;
    char *result;
    int len;


    map = _ypopts_getmd("rpc", ".byname", &domain);
    if (map == NULL)
	return NULL;

    rpp = NULL;
    
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	rpp = rpcent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return rpp;
}


struct rpcent *
_yp_getrpcbynumber(int number)
{
    char buf[32];
    struct rpcent *rpp;
    char *map;
    char *domain;
    char *result;
    int len;


    map = _ypopts_getmd("rpc", ".bynumber", &domain);
    if (map == NULL)
	return NULL;

    sprintf(buf, "%d", number);
	    
    rpp = NULL;
    
    if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0)
    {
	rpp = rpcent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return rpp;
}

#endif /* ENABLE_YP */
