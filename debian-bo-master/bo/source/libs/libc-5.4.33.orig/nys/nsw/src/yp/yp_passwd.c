/*
** yp_passwd.c           NIS Version 2 Passwd map access routines
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
#include <pwd.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"

#include "xalloc.h"

static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;
static char *pbuf = NULL;


static struct passwd *pwent_parse(char *str, int len)
{
    static struct passwd pwd;
    char *cp;

    
    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;
    
    strncpy(pbuf, str, len);
    pbuf[len] = '\0';
    
    pwd.pw_name   = _yp_xstrtok(pbuf, ':');
    if (pwd.pw_name == NULL)
	return NULL;
    
    pwd.pw_passwd = _yp_xstrtok(NULL, ':');
    if (pwd.pw_passwd == NULL)
	return NULL;
    
    cp = _yp_xstrtok(NULL, ':');
    if (cp == NULL || !isdigit(*cp))
	return NULL;
    pwd.pw_uid    = atoi(cp);

    cp = _yp_xstrtok(NULL, ':');
    if (cp == NULL || !isdigit(*cp))
	return NULL;
    pwd.pw_gid    = atoi(cp);
    
    pwd.pw_gecos  = _yp_xstrtok(NULL, ':');
    if (pwd.pw_gecos == NULL)
	return NULL;
    
    pwd.pw_dir    = _yp_xstrtok(NULL, ':');
    if (pwd.pw_dir == NULL)
	return NULL;
    
    pwd.pw_shell  = _yp_xstrtok(NULL, ':');
    if (pwd.pw_dir == NULL)
	return NULL;

    return &pwd;
}


void _yp_setpwent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void _yp_endpwent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct passwd *_yp_getpwent(void)
{
    struct passwd *pw;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("passwd", ".byname", &domain);
    if (map == NULL)
	return NULL;

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
	fprintf(stderr, "yp_passwd: Invalid passwd entry: %.*s\n",
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


struct passwd *_yp_getpwuid(uid_t uid)
{
    struct passwd *pw;
    char *map;
    char *domain;
    char *result;
    int len;
    char buf[16];


    map = _ypopts_getmd("passwd", ".byuid", &domain);
    if (map == NULL)
	return NULL;

    sprintf(buf, "%u", uid);

    pw = NULL;
    
    if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0)
    {
	pw = pwent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return pw;
}


struct passwd *_yp_getpwnam(const char *name)
{
    struct passwd *pw;
    char *map;
    char *domain;
    char *result;
    int len;

    map = _ypopts_getmd("passwd", ".byname", &domain);
    if (map == NULL)
	return NULL;

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
