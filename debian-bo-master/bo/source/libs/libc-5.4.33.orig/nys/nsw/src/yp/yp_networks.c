/*
** yp_networks.c           NIS Version 2 Network map access routines
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
** Authors: Peter Eriksson <pen@signum.se>
**          Michael A. Griffith <grif@cs.ucr.edu>
*/

#include "config.h"

#ifdef ENABLE_YP


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb/networks.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"


static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;


static struct netent *
netent_parse(char *str, int len)
{
    static struct netent ne;
    char *cp;
    int alen;
    int i;
    
    
    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;
    
    strncpy(pbuf, str, len);
    pbuf[len] = '\0';
    
    ne.n_name = _yp_xstrtok(pbuf, ' ');
    if (ne.n_name == NULL)
	return NULL;

    ne.n_addrtype = AF_INET;
    
    cp = _yp_xstrtok(NULL, ' ');
    if (cp == NULL)
	return NULL;

    ne.n_net = inet_network(cp);

    /* Build alias list */
    alen = 1;
    if (Xalloc(&ne.n_aliases, alen+1) == NULL)
	return NULL;

    i = 0;
    while ((cp = _yp_xstrtok(NULL, ' ')) && cp[0] != '#')
    {
	if (i >= alen)
	{
	    alen += 10;
	    if (Xalloc(&ne.n_aliases, alen+1) == NULL)
		return NULL;
	}
	
	ne.n_aliases[i++] = cp;
    }


    ne.n_aliases[i] = NULL;

    return &ne;
}



void
_yp_setnetent(int stayopen)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void
_yp_endnetent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct netent *
_yp_getnetent(void)
{
    struct netent *net;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("networks", ".byaddr", &domain);
    if (map == NULL)
	return NULL;

    net = NULL;
    
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

    net = netent_parse(result, len);
    free(result);

  error:
    free(map);
    free(domain);
    
    return net;
}


struct netent *
_yp_getnetbyaddr(unsigned long net, int type)
{
    struct netent *nep;
    char *map;
    char *domain;
    char *result;
    int len;
    char buf[256];
    int blen;
    struct in_addr in;
    

    map = _ypopts_getmd("networks", ".byaddr", &domain);
    if (map == NULL)
	return NULL;

    in = inet_makeaddr(net, 0);
    strcpy(buf, inet_ntoa(in));
    blen = strlen(buf);
    
    nep = 0;
    
  Retry:
    if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0)
    {
	nep = netent_parse(result, len);
	free(result);
    }
    else if (buf[blen-2] == '.' && buf[blen-1] == '0')
    {
	/* Try again, but with trailing dot(s) removed (one by one) */
	buf[blen-2] = '\0';
	blen -= 2;
	goto Retry;
    }

    free(map);
    free(domain);
    
    return nep;
}


struct netent *
_yp_getnetbyname(const char *name)
{
    struct netent *net;
    char *map;
    char *domain;
    char *result;
    int len;

    map = _ypopts_getmd("networks", ".byname", &domain);
    if (map == NULL)
	return NULL;

    net = NULL;
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	net = netent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    return net;
}

#endif /* ENABLE_YP */


