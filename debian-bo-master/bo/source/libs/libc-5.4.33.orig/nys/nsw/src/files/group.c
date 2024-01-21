/*
** group.c                           /etc/group access functions
**
** This file is part of the NYS Library.
**
**      Copyright (c) 1993 Signum Support AB
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grp.h>
#include "misc.h"
#include "xalloc.h"


static FILE *gfp = NULL;


void _setgrent(void)
{
    if (gfp)
	rewind(gfp);
    else
	gfp = fopen(_PATH_GROUP, "r");
}


void _endgrent(void)
{
    if (gfp)
	fclose(gfp);
    gfp = NULL;
}


static int add_member(struct group *grp, char *name)
{
    static int size = 0;
    static int pos = 0;
    int i;

    
    if (grp->gr_mem == NULL)
    {
	size = 32;
	pos = 0;
	grp->gr_mem = malloc(size * sizeof(char *));
	if (grp->gr_mem == NULL)
	    return -1;

	for (i = 0; i < size; i++)
	    grp->gr_mem[i] = NULL;
    }

    if (pos+1 >= size)
    {
	size += 32;
	grp->gr_mem = realloc(grp->gr_mem, size * sizeof(char *));
	if (grp->gr_mem == NULL)
	    return -1;

	for (i = pos; i < size; i++)
	    grp->gr_mem[i] = NULL;
    }

    grp->gr_mem[pos++] = name;
    
    return pos;
}


struct group *sgetgrent(const char *str)
{
    static struct group grp;
    static char *buf = NULL;
    char *cp, *sp;


    if (Xalloc(&buf, strlen(str)+1) == NULL)
	return NULL;
    strcpy(buf, str);

    sp = buf;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;

    *cp++ = '\0';
    grp.gr_name = sp;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;

    *cp++ = '\0';
    grp.gr_passwd = sp;


    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;
    *cp++ = '\0';
    if (*sp)
	grp.gr_gid = atoi(sp);
    else
	grp.gr_gid = -1;

    sp = cp;
    if (grp.gr_mem)
    {
	free(grp.gr_mem);
	grp.gr_mem = NULL;
    }
    
    while ((cp = strchr(sp, ',')) != NULL)
    {
	*cp++ = '\0';

	if (add_member(&grp, sp) < 0)
	    return NULL;
	sp = cp;
    }

    if (add_member(&grp, sp) < 0)
	return NULL;

    return &grp;
}


struct group *fgetgrent(FILE *fp)
{
    static char *buf = NULL;
    static int size = 0;
    int len;

    len = _nsw_getline(&buf, &size, fp);
    if (len < 0)
	return NULL;
    
    return sgetgrent(buf);
}


struct group *_getgrent(void)
{
    if (gfp == NULL)
	_setgrent();
    
    if (gfp == NULL)
	return NULL;

    return fgetgrent(gfp);
}


struct group *_getgrnam(const char *name)
{
    struct group *gp;

    _setgrent();
    
    while ((gp = _getgrent()) != NULL &&
	   strcmp(gp->gr_name, name) != 0)
	;

    _endgrent();
    
    return gp;
}


struct group *_getgrgid(gid_t gid)
{
    struct group *gp;


    _setgrent();
    
    while ((gp = _getgrent()) != NULL &&
	   gp->gr_gid != gid)
	;

    _endgrent();
    
    return gp;
}
