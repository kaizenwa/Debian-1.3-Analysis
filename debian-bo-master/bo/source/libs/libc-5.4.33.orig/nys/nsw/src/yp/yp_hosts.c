/*
** yp_hosts.c           NIS Version 2 Hosts map access routines
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
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb/hosts.h>
#include "xalloc.h"
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;


static struct hostent *
hostent_parse(char *str, int len)
{
    static struct hostent heb;
    char *cp;
    int alen;
    int i;
    

    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;
    
    strncpy(pbuf, str, len);
    pbuf[len] = '\0';

    if (Xalloc(&heb.h_addr_list, 2) == NULL)
	return NULL;

    if (Xalloc(((long **) &heb.h_addr_list[0]), 1) == NULL)
	return NULL;
	
    cp = _yp_xstrtok(pbuf, ' ');
    if (cp == NULL)
	return NULL;

    heb.h_length = sizeof(long);
    heb.h_addrtype = AF_INET;

    * (long *) heb.h_addr_list[0] = inet_addr(cp);

    heb.h_addr_list[1] = NULL;
    
    /* There could be more then one whitespace before the hostname ! */
    heb.h_name = _yp_xstrtok(NULL, ' ');
    while(heb.h_name && (strlen(heb.h_name) == 0))
   	heb.h_name = _yp_xstrtok(NULL, ' ');
        	   
    /* Build alias list */
    alen = 1;
    if (Xalloc(&heb.h_aliases, alen+1) == NULL)
	return NULL;

    i = 0;
    while ((cp = _yp_xstrtok(NULL, ' ')) && cp[0] != '#')
    {
	/* There could be more then one whitespece between hostnames ! */
        if(strlen(cp) == 0) continue;
	if (i >= alen)
	{
	    alen += 10;
	    if (Xalloc(&heb.h_aliases, alen+1) == NULL)
		return NULL;
	}
	heb.h_aliases[i++] = cp;
    }

    heb.h_aliases[i] = NULL;

    return &heb;
}


void
_yp_sethostent(int stayopen)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void
_yp_endhostent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct hostent *
_yp_gethostent(void)
{
    struct hostent *host;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("hosts", ".byname", &domain);
    if (map == NULL)
	return NULL;

    host = NULL;
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

    host = hostent_parse(result, len);
    free(result);

  error:
    free(map);
    free(domain);
    
    return host;
}


struct hostent *
_yp_gethostbyaddr(const char *addr,
                  int alen, int type)
{
    struct hostent *host;
    char *map;
    char *domain;
    char *result;
    int len;
    char *buf;

 
    map = _ypopts_getmd("hosts", ".byaddr", &domain);
    if (map == NULL)
	return NULL;

    buf = inet_ntoa(* (struct in_addr *) addr);
                
    host = NULL;
    
    if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0)
    {
	host = hostent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return host;
}


struct hostent *
_yp_gethostbyname(const char *name)
{
    struct hostent *host;
    char *map;
    char *domain;
    char *result;
    int len;

    
    map = _ypopts_getmd("hosts", ".byname", &domain);
    if (map == NULL)
	return NULL;

    host = NULL;
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	host = hostent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    return host;
}

#endif /* ENABLE_YP */

