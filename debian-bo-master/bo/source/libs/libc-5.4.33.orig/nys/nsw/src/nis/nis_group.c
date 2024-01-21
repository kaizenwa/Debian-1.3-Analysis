/*
** nis_group.c              NIS+ Group map access routines
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

#ifdef ENABLE_NIS


#include <stdio.h>
#include <errno.h>
#include <grp.h>
#include <rpcsvc/nis.h>
#include "xalloc.h"


static nis_result *res = NULL;
static nis_name *names = NULL;


#define NISENTRYCOL(idx,col,res) \
	((res)->objects.objects_val[(idx)].zo_data.objdata_u.en_data.en_cols.en_cols_val[(col)].ec_value.ec_value_val)



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


static struct group *parse_result(nis_result *res)
{
    static struct group grp;
    char *cp, *sp;

    
    if (res == NULL)
	return NULL;
    
    /*
    ** Validate the returned data. Must be a successful lookup,
    ** the number of objects returned must be exactly one,
    ** the returned object must be of type ENTRY,
    ** the ENTRY table must be of type 'group_tbl',
    ** and it must contain 4 slots.
    **
    ** This might be overkill...
    **
    ** Should probably set errno to some sane value
    */
    if ((res->status != NIS_SUCCESS &&
	 res->status != NIS_S_SUCCESS) ||
	res->objects.objects_len != 1 ||
	res->objects.objects_val[0].zo_data.zo_type != ENTRY_OBJ ||
	strcmp(res->objects.objects_val[0].zo_data.objdata_u.en_data.en_type,
	       "group_tbl") != 0 ||
	res->objects.objects_val[0].zo_data.objdata_u.en_data.en_cols.en_cols_len != 4)
	
	return NULL;

    if (grp.gr_name)
	free(grp.gr_name);
    
    if (grp.gr_passwd)
	free(grp.gr_passwd);

    if (grp.gr_mem)
    {
	free(grp.gr_mem);
	grp.gr_mem = NULL;
    }
    
    grp.gr_name   = xstrdup(NISENTRYCOL(0, 0, res));
    grp.gr_passwd = xstrdup(NISENTRYCOL(0, 1, res));
    grp.gr_gid = atoi(NISENTRYCOL(0, 2, res));

    sp = NISENTRYCOL(0, 3, res);
    while ((cp = strchr(sp, ',')) != NULL)
    {
	*cp++ = '\0';

	if (add_member(&grp, xstrdup (sp)) < 0)
	    return NULL;
	sp = cp;
    }

    if (add_member(&grp, xstrdup (sp)) < 0)
	return NULL;

    return &grp;
}


void _nis_setgrent(void)
{
    res = NULL;
}


void _nis_endgrent(void)
{
    res = NULL;

    if (names)
    {
	nis_freenames(names);
	names = NULL;
    }
}


struct group *_nis_getgrent(void)
{
    if (res == NULL)
    {
	if (names)
	    nis_freenames(names);

	names = nis_getnames("group.org_dir");
	if (names == NULL || names[0] == NULL)
	    return NULL;
	
	res = nis_first_entry(names[0]);
    }
    else
	res = nis_next_entry(names[0], &res->cookie);

    return parse_result(res);
}


struct group *_nis_getgrgid(gid_t gid)
{
    nis_result *res;
    char buf[81];


    sprintf(buf, "[gid=%d],group.org_dir", gid);
    
    res = nis_list(buf, EXPAND_NAME, NULL, NULL);
    return parse_result(res);
}


struct group *_nis_getgrnam(const char *name)
{
    nis_result *res;
    char buf[81];


    if (name == NULL || strlen(name) > 8)
	return NULL;
    
    sprintf(buf, "[name=%s],group.org_dir", name);
    
    res = nis_list(buf, EXPAND_NAME, NULL, NULL);
    
    return parse_result(res);
}

#endif /* ENABLE_NIS */
