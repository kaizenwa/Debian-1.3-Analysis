/*
** yp_protocols.c              YP Protocols map access routines
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
#include <netdb/protocols.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"


static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;


static struct protoent *
protoent_parse(char *str, int len)
{
    static struct protoent poe;
    char *cp;
    int alen;
    int i;
    
    
    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;
    
    strncpy(pbuf, str, len);
    pbuf[len] = '\0';
    
    poe.p_name   = _yp_xstrtok(pbuf, ' ');
    if (poe.p_name == NULL)
	return NULL;
    
    cp = _yp_xstrtok(NULL, ' ');
    if (cp == NULL)
	return NULL;
    poe.p_proto = atoi(cp);

    /* Build alias list */
    alen = 1;
    if (Xalloc(&poe.p_aliases, alen+1) == NULL)
	return NULL;

    i = 0;
    while ((cp = _yp_xstrtok(NULL, ' ')) && cp[0] != '#')
    {
	if (i >= alen)
	{
	    alen += 10;
	    if (Xalloc(&poe.p_aliases, alen+1) == NULL)
		return NULL;
	}
	
	poe.p_aliases[i++] = cp;
    }

    poe.p_aliases[i] = NULL;

    return &poe;
}



void
_yp_setprotoent(int stayopen)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void
_yp_endprotoent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct protoent *
_yp_getprotoent(void)
{
    struct protoent *poe;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("protocols", ".byname", &domain);
    if (map == NULL)
	return NULL;

    poe = NULL;
    
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
    while ((poe = protoent_parse(result, len)) == NULL && errno == 0)
    {
#ifdef DEBUG
	fprintf(stderr, "yp_protocols: Invalid protocols entry: %.*s\n",
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
    
    return poe;
}


struct protoent *
_yp_getprotobyname(const char *name)
{
    struct protoent *pep;
    char *map;
    char *domain;
    char *result;
    int len;


    map = _ypopts_getmd("protocols", ".byname", &domain);
    if (map == NULL)
	return NULL;

    pep = NULL;
    
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	pep = protoent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return pep;
}


struct protoent *
_yp_getprotobynumber(int number)
{
    char buf[32];
    struct protoent *pep;
    char *map;
    char *domain;
    char *result;
    int len;


    map = _ypopts_getmd("protocols", ".bynumber", &domain);
    if (map == NULL)
	return NULL;

    sprintf(buf, "%d", number);
	    
    pep = NULL;
    
    if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0)
    {
	pep = protoent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return pep;
}

#endif /* ENABLE_YP */



