/*
** yp_group.c           NIS Version 2 group map access routines
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
#include <grp.h>
#include "yp_misc.h"
#include "rpcsvc/ypclnt.h"
#include "xalloc.h"


static char *pbuf = NULL;
static int rewind_flag = 1;
static char *savekey = NULL;
static int savekeylen = 0;



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



static struct group *grent_parse(char *str, int len)
{
    static struct group grd;
    char *cp;
    

    if (Xalloc(&pbuf, len+8) == NULL)
	return NULL;

    strncpy(pbuf, str, len);
    pbuf[len] = '\0';

    grd.gr_name = _yp_xstrtok(pbuf, ':');
    if (grd.gr_name == NULL)
	return NULL;
    
    grd.gr_passwd = _yp_xstrtok(NULL, ':');
    if (grd.gr_passwd == NULL)
	return NULL;

    cp = _yp_xstrtok(NULL, ':');
    if (cp == NULL)
	return NULL;
    grd.gr_gid = atoi(cp);
    
    grd.gr_mem = yp_parse_grp_members(_yp_xstrtok(NULL, ':'));
    if (grd.gr_mem == NULL)
	return NULL;
    return &grd;
}


void _yp_setgrent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


void _yp_endgrent(void)
{
    rewind_flag = 1;
    if (savekey) {
	free(savekey);
	savekey=NULL;
    }
}


struct group *_yp_getgrent(void)
{
    struct group *gr;
    char *map;
    char *domain;
    char *result;
    int len;
    char *outkey;
    int keylen;


    map = _ypopts_getmd("group", ".byname", &domain);
    if (map == NULL)
	return NULL;

    gr = NULL;
    
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

    gr = grent_parse(result, len);
    free(result);

  error:
    free(map);
    free(domain);
    
    return gr;
}


struct group *_yp_getgrgid(gid_t gid)
{
    struct group *gr;
    char *map;
    char *domain;
    char *result;
    int len;
    char buf[256];


    map = _ypopts_getmd("group", ".bygid", &domain);
    if (map == NULL)
	return NULL;

    sprintf(buf, "%d", gid);

    gr = NULL;
    
    if (yp_match(domain, map, buf, strlen(buf), &result, &len) == 0)
    {
	gr = grent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    
    return gr;
}


struct group *_yp_getgrnam(const char *name)
{
    struct group *gr;
    char *map;
    char *domain;
    char *result;
    int len;

    map = _ypopts_getmd("group", ".byname", &domain);
    if (map == NULL)
	return NULL;

    gr = NULL;
    if (yp_match(domain, map, name, strlen(name), &result, &len) == 0)
    {
	gr = grent_parse(result, len);
	free(result);
    }

    free(map);
    free(domain);
    return gr;
}

#endif /* ENABLE_YP */


