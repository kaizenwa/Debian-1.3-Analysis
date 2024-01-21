/*
** yp_shadow.c              YP Shadow "map" access routines
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
#include <shadow.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"

static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;
static char parse_aging;

static struct spwd *pwent_parse(char *str, int len)
{
    static struct spwd spwd;
    char *cp;
    
    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;

    strncpy(pbuf, str, len);
    pbuf[len] = '\0';

    spwd.sp_namp   = _yp_xstrtok(pbuf, ':');
    if (spwd.sp_namp == NULL)
	return NULL;

    spwd.sp_pwdp = _yp_xstrtok(NULL, ':');
    if (spwd.sp_pwdp == NULL)
	return NULL;

    if (parse_aging) {
	cp = _yp_xstrtok(NULL, ':');
	if (cp == NULL)
	    return NULL;
	if (!isdigit(*cp))
	    spwd.sp_lstchg = -1;
	else
	    spwd.sp_lstchg = atoi(cp);

	cp = _yp_xstrtok(NULL, ':');
	if (cp == NULL)
	    return NULL;
	if (!isdigit(*cp))
	    spwd.sp_min = -1;
	else
	    spwd.sp_min = atoi(cp);

	cp = _yp_xstrtok(NULL, ':');
	if (cp == NULL)
	    return NULL;
	if (!isdigit(*cp))
	    spwd.sp_max = -1;
	else
	    spwd.sp_max = atoi(cp);

	cp = _yp_xstrtok(NULL, ':');
	if (cp == NULL)
	    return NULL;
	if (!isdigit(*cp))
	    spwd.sp_warn = -1;
	else
	    spwd.sp_warn = atoi(cp);

	cp = _yp_xstrtok(NULL, ':');
	if (cp == NULL)
	    return NULL;
	if (!isdigit(*cp))
	    spwd.sp_inact = -1;
	else
	    spwd.sp_inact = atoi(cp);

	cp = _yp_xstrtok(NULL, ':');
	if (cp == NULL)
	    return NULL;
	if (!isdigit(*cp))
	    spwd.sp_expire = -1;
	else
	    spwd.sp_expire = atoi(cp);

	cp = _yp_xstrtok(NULL, ':');
	if (cp == NULL)
	    return NULL;
	if (!isdigit(*cp))
	    spwd.sp_flag = -1;
	else
	    spwd.sp_flag = atoi(cp);
    } else
    {
	spwd.sp_lstchg = -1;
	spwd.sp_min = -1;
	spwd.sp_max = -1;
	spwd.sp_warn = -1;
	spwd.sp_inact = -1;
	spwd.sp_expire = -1;
	spwd.sp_flag = -1;
    }
    return &spwd;
}

void _yp_setspent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void _yp_endspent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct spwd *_yp_getspent(void)
{
    struct spwd *pw;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("shadow", ".byname", &domain);
    if (map == NULL)
    {
	map = _ypopts_getmd("passwd", ".byname", &domain);
	if (map == NULL)
	    return NULL;
	parse_aging=0;
    } else parse_aging=1;

    pw = NULL;
    
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
    while ((pw = pwent_parse(result, len)) == NULL && errno == 0)
    {
#ifdef DEBUG
	fprintf(stderr, "yp_shadow: Invalid passwd entry: %.*s\n",
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
    
    return pw;
}


struct spwd *_yp_getspnam(const char *name)
{
    struct spwd *pw;
    char *map;
    char *domain;
    char *result;
    int len;

    map = _ypopts_getmd("shadow", ".byname", &domain);
    if (map == NULL)
    {
	map = _ypopts_getmd("passwd", ".byname", &domain);
	if (map == NULL)
	    return NULL;
	parse_aging=0;
    } else parse_aging=1;

    pw = NULL;
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	pw = pwent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    return pw;
}

#endif /* ENABLE_YP */
