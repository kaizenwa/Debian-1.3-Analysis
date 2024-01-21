/*
** nis_shadow.c              NIS+ Shadow "map" access routines
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
#include <shadow.h>
#include <rpcsvc/nis.h>
#include "xalloc.h"


static nis_result *res = NULL;
static nis_name *names = NULL;


#define NISENTRYCOL(idx,col,res) \
	((res)->objects.objects_val[(idx)].zo_data.objdata_u.en_data.en_cols.en_cols_val[(col)].ec_value.ec_value_val)


static struct spwd *parse_result(nis_result *res)
{
    static struct spwd spb;
    char *cp, *sp;

    
    if (res == NULL)
	return NULL;
    
    /*
    ** Validate the returned data. Must be a successful lookup,
    ** the number of objects returned must be exactly one,
    ** the returned object must be of type ENTRY,
    ** the ENTRY table must be of type 'passwd_tbl',
    ** and it must contain atleast 8 slots.
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
	res->objects.objects_val[0].zo_data.objdata_u.en_data.en_cols.en_cols_len < 8)
	
	return NULL;

    if (spb.sp_namp)
	free(spb.sp_namp);
    if (spb.sp_pwdp)
	free(spb.sp_pwdp);

    spb.sp_namp = xstrdup(NISENTRYCOL(0, 0, res));
    spb.sp_pwdp = xstrdup(NISENTRYCOL(0, 1, res));
    sp = NISENTRYCOL(0, 7, res);


    spb.sp_lstchg = spb.sp_min = spb.sp_max = spb.sp_warn = -1;
    spb.sp_inact = spb.sp_expire = -1;
    spb.sp_flag = -1;
        
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';
    
    if (*sp)
	spb.sp_lstchg = atoi(sp);
    else
	spb.sp_lstchg = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';
    if (*sp)
	spb.sp_min = atoi(sp);
    else
	spb.sp_min = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_max = atoi(sp);
    else
	spb.sp_max = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_warn = atoi(sp);
    else
	spb.sp_warn = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_inact = atoi(sp);
    else
	spb.sp_inact = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_expire = atoi(sp);
    else
	spb.sp_expire = -1;
	
    if (*cp)
	spb.sp_flag = atoi(cp);
    else
	spb.sp_flag = -1;


    return &spb;
}



void _nis_setspent(void)
{
    res = NULL;
}


void _nis_endspent(void)
{
    res = NULL;

    if (names)
    {
	nis_freenames(names);
	names = NULL;
    }
}


struct spwd *_nis_getspent(void)
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


struct spwd *_nis_getspnam(const char *name)
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
