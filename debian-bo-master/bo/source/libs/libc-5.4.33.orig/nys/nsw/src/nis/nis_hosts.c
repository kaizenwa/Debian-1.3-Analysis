/*
** nis_hosts.c              NIS+ Hosts map access routines
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

#ifdef ENABLE_NIS

#include <stdio.h>
#include <errno.h>
#include <netdb/hosts.h>
#include <rpcsvc/nis.h>
#include "xalloc.h"

static nis_result *res = NULL;
static nis_name *names = NULL;

#define NISENTRYCOL(idx,col,res) \
        ((res)->objects.objects_val[(idx)].zo_data.objdata_u.en_data.en_cols.en_cols_val[(col)].ec_value.ec_value_val)

static struct hostent *parse_result(nis_result *res)
{
    static struct hostent host;
    int i, alen;
    
    if (res == NULL)
        return NULL;
    
    /*
    ** Validate the returned data. Must be a successful lookup,
    ** the returned object must be of type ENTRY,
    ** the ENTRY table must be of type 'hosts_tbl',
    ** and it must contain atleast 4 slots.
    **
    ** This might be overkill...
    **
    ** Should probably set errno to some sane value
    */
    if ((res->status != NIS_SUCCESS &&
         res->status != NIS_S_SUCCESS) ||
        res->objects.objects_val[0].zo_data.zo_type != ENTRY_OBJ ||
        strcmp(res->objects.objects_val[0].zo_data.objdata_u.en_data.en_type,
               "hosts_tbl") != 0 ||
        res->objects.objects_val[0].zo_data.objdata_u.en_data.en_cols.en_cols_len < 4)
        
        return NULL;

    if (Xalloc(&host.h_addr_list, 2) == NULL)
        return NULL;

    if (Xalloc(((long **) &host.h_addr_list[0]), 1) == NULL)
        return NULL;
        
    host.h_length = sizeof(long);
    host.h_addrtype = AF_INET;

    * (long *) host.h_addr_list[0] = inet_addr(NISENTRYCOL(0, 2, res));

    host.h_addr_list[1] = NULL;

    if (host.h_name)
      free (host.h_name);
    host.h_name = xstrdup(NISENTRYCOL(0, 0, res));
    
    alen =res->objects.objects_len;
    if (Xalloc(&host.h_aliases, alen+1) == NULL)
      return NULL;
    
    for (i = 0; i < alen; i++)
      host.h_aliases = NISENTRYCOL(i,1,res);

    host.h_aliases[i] = NULL;

    return &host;
}


void
_nis_sethostent(int stayopen)
{
  res = NULL;
}


void
_nis_endhostent(void)
{
  res = NULL;

  if (names)
    {
      nis_freenames(names);
      names = NULL;
    }
}


struct hostent *
_nis_gethostent(void)
{
  if (res == NULL)
    {
      if (names)
	nis_freenames(names);
      
      names = nis_getnames("hosts.org_dir");
      if (names == NULL || names[0] == NULL)
	return NULL;
      
      res = nis_first_entry(names[0]);
    }
  else
    res = nis_next_entry(names[0], &res->cookie);
  
  return parse_result(res);
}


struct hostent *
_nis_gethostbyname(const char *name)
{
  nis_result *res;
  char buf[81];

  if (name == NULL || strlen(name) > 8)
    return NULL;

  /*
  ** Search at first in the alias list, and use the correct name
  ** for the next search
  */
  sprintf(buf, "[name=%s],hosts.org_dir", name);
  res = nis_list(buf, EXPAND_NAME, NULL, NULL);
  if (res == NULL)
    return NULL;

  /*
  ** If we do not find it, try it as original name. But if the
  ** database is correct, we should find it in the first case, too
  */
  if ((res->status != NIS_SUCCESS &&
       res->status != NIS_S_SUCCESS) ||
      res->objects.objects_val[0].zo_data.zo_type != ENTRY_OBJ ||
      strcmp(res->objects.objects_val[0].zo_data.objdata_u.en_data.en_type,
	     "hosts_tbl") != 0 ||
      res->objects.objects_val[0].zo_data.objdata_u.en_data.en_cols.en_cols_len < 4)
    sprintf(buf, "[cname=%s],hosts.org_dir", name);
  else
    sprintf(buf, "[cname=%s],hosts.org_dir", NISENTRYCOL(0, 0, res));

  res = nis_list(buf, EXPAND_NAME, NULL, NULL);

  return parse_result(res);
}


struct hostent *
_nis_gethostbyaddr(const char *addr, int len, int type)
{
  nis_result *res;
  char buf[81];

  sprintf(buf, "[addr=%s],hosts.org_dir", addr);

  res = nis_list(buf, EXPAND_NAME, NULL, NULL);

  return parse_result(res);
}

#endif /* ENABLE_NIS */
