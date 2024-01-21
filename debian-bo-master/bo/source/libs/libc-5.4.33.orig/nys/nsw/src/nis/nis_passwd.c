/*
** nis_passwd.c              NIS+ Passwd map access routines
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
#include <pwd.h>
#include <rpcsvc/nis.h>
#include "xalloc.h"


static nis_result *res = NULL;
static nis_name *names = NULL;


#define NISENTRYCOL(idx,col,res) \
	((res)->objects.objects_val[(idx)].zo_data.objdata_u.en_data.en_cols.en_cols_val[(col)].ec_value.ec_value_val)



static struct passwd *parse_result(nis_result *res)
{
    static struct passwd pwd;

    
    if (res == NULL)
	return NULL;
    
    /*
    ** Validate the returned data. Must be a successful lookup,
    ** the number of objects returned must be exactly one,
    ** the returned object must be of type ENTRY,
    ** the ENTRY table must be of type 'passwd_tbl',
    ** and it must contain atleast 7 slots.
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
	       "passwd_tbl") != 0 ||
	res->objects.objects_val[0].zo_data.objdata_u.en_data.en_cols.en_cols_len < 7)
	
	return NULL;

    if (pwd.pw_name)
	free(pwd.pw_name);
    if (pwd.pw_passwd)
	free(pwd.pw_passwd);
    if (pwd.pw_gecos)
	free(pwd.pw_gecos);
    if (pwd.pw_dir)
	free(pwd.pw_dir);
    if (pwd.pw_shell)
	free(pwd.pw_shell);
    
    pwd.pw_name   = xstrdup(NISENTRYCOL(0, 0, res));
    pwd.pw_passwd = xstrdup(NISENTRYCOL(0, 1, res));
    pwd.pw_uid = atoi(NISENTRYCOL(0, 2, res));
    pwd.pw_gid = atoi(NISENTRYCOL(0, 3, res));
    pwd.pw_gecos = xstrdup(NISENTRYCOL(0, 4, res));
    pwd.pw_dir = xstrdup(NISENTRYCOL(0, 5, res));
    pwd.pw_shell = xstrdup(NISENTRYCOL(0, 6, res));

    return &pwd;
}

void _nis_setpwent(void)
{
    res = NULL;
}


void _nis_endpwent(void)
{
    res = NULL;

    if (names)
    {
	nis_freenames(names);
	names = NULL;
    }
}

struct passwd *_nis_getpwent(void)
{
    if (res == NULL)
    {
	if (names)
	    nis_freenames(names);

	names = nis_getnames("passwd.org_dir");
	if (names == NULL || names[0] == NULL)
	    return NULL;
	
	res = nis_first_entry(names[0]);
    }
    else
	res = nis_next_entry(names[0], &res->cookie);

    return parse_result(res);
}


struct passwd *_nis_getpwuid(uid_t uid)
{
    nis_result *res;
    char buf[81];


    sprintf(buf, "[uid=%d],passwd.org_dir", uid);
    
    res = nis_list(buf, EXPAND_NAME, NULL, NULL);
    return parse_result(res);
}


struct passwd *_nis_getpwnam(const char *name)
{
    nis_result *res;
    char buf[81];


    if (name == NULL || strlen(name) > 8)
	return NULL;
    
    sprintf(buf, "[name=%s],passwd.org_dir", name);
    
    res = nis_list(buf, EXPAND_NAME, NULL, NULL);
    
    return parse_result(res);
}

#endif /* ENABLE_NIS */
