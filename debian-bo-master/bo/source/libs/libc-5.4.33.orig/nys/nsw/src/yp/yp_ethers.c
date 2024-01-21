/*
** yp_ethers.c              YP Ethers map access routines
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
#include <ethers.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"

static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;


static struct ether *ethent_parse(char *str, int len)
{
    char buf[1024];
    static struct ether eth;
    int v0, v1, v2, v3, v4, v5;
    char *cp;
    

    strncpy(buf, str, len);
    buf[len] = '\0';

    cp = strchr(buf, '#');
    if (cp)
	*cp = '\0';

    cp = strtok(buf, " \t");
    if (sscanf(cp, "%x:%x:%x:%x:%x:%x", &v0, &v1, &v2, &v3, &v4, &v5) != 6)
    {
	errno = 0;
	return NULL;
    }

    cp = strtok(NULL, " \t");
    if (cp == NULL || strlen(cp) >= sizeof(eth.name))
    {
        errno = 0;
        return NULL;
    }
    
    strcpy(eth.name, cp);
	   
    eth.addr.ether_addr_octet[0] = (unsigned char) v0;
    eth.addr.ether_addr_octet[1] = (unsigned char) v1;
    eth.addr.ether_addr_octet[2] = (unsigned char) v2;
    eth.addr.ether_addr_octet[3] = (unsigned char) v3;
    eth.addr.ether_addr_octet[4] = (unsigned char) v4;
    eth.addr.ether_addr_octet[5] = (unsigned char) v5;
    
    return &eth;
}


void _yp_setethent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void _yp_endethent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct ether *_yp_getethent(void)
{
    struct ether *eth;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;

    
    map = _ypopts_getmd("ethers", ".byaddr", &domain);
    if (map == NULL)
	return NULL;

    eth = NULL;
    
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
    while ((eth = ethent_parse(result, len)) == NULL && errno == 0)
    {
#ifdef DEBUG
	fprintf(stderr, "yp_ethers: Invalid ethers entry: %.*s\n",
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
    
    return eth;
}


struct ether *_yp_getethbyname(const char *name)
{
    struct ether *eth;
    char *map;
    char *domain;
    char *result;
    int len;


    map = _ypopts_getmd("ethers", ".byname", &domain);
    if (map == NULL)
	return NULL;

    eth = NULL;
    
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	eth = ethent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return eth;
}


struct ether *_yp_getethbyaddr(const struct ether_addr *addr)
{
    char buf[32];
    struct ether *eth;
    char *map;
    char *domain;
    char *result;
    int len;


    map = _ypopts_getmd("ethers", ".byaddr", &domain);
    if (map == NULL)
	return NULL;

    sprintf(buf, "%x:%x:%x:%x:%x:%x",
	    (int) addr->ether_addr_octet[0],
	    (int) addr->ether_addr_octet[1],
	    (int) addr->ether_addr_octet[2],
	    (int) addr->ether_addr_octet[3],
	    (int) addr->ether_addr_octet[4],
	    (int) addr->ether_addr_octet[5]);
	    
    eth = NULL;
    
    if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0)
    {
	eth = ethent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return eth;
}

#endif /* ENABLE_YP */
