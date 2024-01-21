/*
** yp_services.c              YP Services map access routines
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
#include <netdb/services.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"

#ifdef __linux__
#include <netinet/in.h> 
#else
extern unsigned short int htons(unsigned short int);
#endif

static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;


static struct servent *
servent_parse(char *str, int len)
{
    static struct servent sb;
    char *cp;
    int alen;
    int i;
    
    
    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;
    
    strncpy(pbuf, str, len);
    pbuf[len] = '\0';
    
    sb.s_name   = _yp_xstrtok(pbuf, ' ');
    if (sb.s_name == NULL)
	return NULL;
    
    cp = _yp_xstrtok(NULL, '/');
    if (cp == NULL)
	return NULL;
    sb.s_port = htons(atoi(cp));

    sb.s_proto = _yp_xstrtok(NULL, ' ');
    if (sb.s_proto == NULL)
	return NULL;
 
    /* Build alias list */
    alen = 1;
    if (Xalloc(&sb.s_aliases, alen+1) == NULL)
	return NULL;

    i = 0;
    while ((cp = _yp_xstrtok(NULL, ' ')) && cp[0] != '#')
    {
	if (i >= alen)
	{
	    alen += 10;
	    if (Xalloc(&sb.s_aliases, alen+1) == NULL)
		return NULL;
	}
	sb.s_aliases[i++] = cp;
    }

    sb.s_aliases[i] = NULL;

    return &sb;
}

void
_yp_setservent(int stayopen)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}

void
_yp_endservent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct servent *
_yp_getservent(void)
{
    struct servent *sp;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("services", ".byname", &domain);
    if (map == NULL)
	return NULL;

    sp = NULL;
    
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
    while ((sp = servent_parse(result, len)) == NULL && errno == 0)
    {
#ifdef DEBUG
	fprintf(stderr, "yp_services: Invalid services entry: %.*s\n",
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
    
    return sp;
}


/*
** HACK WARNING. Sun key'ed services.byname on number/protocol ->
** so there is no way to directly ask an YP server what port
** a certain service is using... So we loop over all entries.
**
** This is *not* an efficient way to do it...
*/
struct servent *
_yp_getservbyname(const char *name,
                  const char *protocol)
{
	struct servent *sp;
	int i, found;
    
	_yp_setservent(0);

	found = 0;
	while ( !found && (sp = _yp_getservent()) ) {
		if ( strcmp(sp->s_proto, protocol) == 0 ) {
			if (strcmp(sp->s_name, name) == 0 ) {
				found = 1;
			}
			else {
				for( i = 0 ; !found && sp->s_aliases[i] != NULL ; i++ ) {
					if (strcmp(sp->s_aliases[i], name) == 0 ) {
						found = 1;
					}
				}
			}
		}
	}

	_yp_endservent();
	return sp;
}


struct servent *
_yp_getservbyport(int port, const char *protocol)
{
	struct servent *sp;
	char buf[256];
	char *map;
	char *domain;
	char *result;
	int len;

	map = _ypopts_getmd("services", ".byname", &domain);
	if (map == NULL)
			return NULL;

	sprintf(buf, "%d/%s", port, protocol);
	    
	sp = NULL;
    
	if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0) {
		sp = servent_parse(result, len);
		free(result);
	}

	free(map);
	free(domain);
    
	return sp;
}

#endif /* ENABLE_YP */
