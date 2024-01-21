/*
** nis_if.c                Cover routines for NIS+
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
**
** 22.9.1996 : Added nis_add, nis_remove, nis_modify
**             Thorsten Kukuk <kukuk@uni-paderborn.de>
*/

#include "config.h"

#ifdef ENABLE_NISEMU

#include <stdio.h>
#include <string.h>
#include <rpc/rpc.h>
#include "rpcsvc/nis.h"
#include "nis_alloc.h"
#include "nis_conf.h"
#include "xalloc.h"



static int nissetup_flag = 0;
nis_name nis_splitname(const nis_name, int *, nis_attr **);
typedef struct nis_result * (*_nis_cbf_t)(void *, CLIENT *);
nis_result * _nis_call(_nis_cbf_t, void *, int);
struct ib_request * _nis_build_ib_request(const nis_name,
                                          struct ib_request *,
                                          u_long);


int nis_setup(void)
{
    if (nissetup_flag)
	return 0;
    
    if (nisconf_set())
	return -1;

    nissetup_flag = 1;
    return 0;
}



nis_name nis_splitname(const nis_name name, int *len, nis_attr **val)
{
    static nis_name rname = NULL;
    char *cp, *tp1, *tp2, *tp3;
    int size;


    if (name == NULL)
	return NULL;

    if (rname)
    {
	free(rname);
	rname = NULL;
    }
    
    rname = strdup(name);
    if (rname == NULL)
	return NULL;
    
    if (len)
	*len = 0;
    if (val)
	*val = NULL;

    size = 0;

    
    /* Not of "[key=value,key=value,...],foo.." format? */
    if  (*rname != '[')
	return strdup(rname);
    
    cp = strchr(rname, ']');
    if (cp == NULL || cp[1] != ',')
	return NULL;

    
    *cp = '\0';
    cp += 2;
    
    if (len == NULL || val == NULL)
	return strdup(cp);

    tp1 = strchr(tp2 = (rname+1), ',');
    do
    {
	if (tp1)
	    *tp1++ = '\0';

	tp3 = strchr(tp2, '=');
	if (!tp3)
	    return NULL;
	
	*tp3++ = '\0';

	if ((*len) + 1 >= size)
	{
	    size += 10;
	    if (Xalloc(val, size) == NULL)
		return NULL;
	}

	(*val)[*len].zattr_ndx = xstrdup(tp2);
	if (((*val)[*len].zattr_ndx) == NULL)
	    return NULL;

	(*val)[*len].zattr_val.zattr_val_len = strlen(tp3)+1;
	
	(*val)[*len].zattr_val.zattr_val_val = xstrdup(tp3);
	if ((*val)[*len].zattr_val.zattr_val_val == NULL)
	    return NULL;
	
	(*len)++;
    } while (tp1 && ((tp1 = strchr(tp2 = tp1, ',')), 1));

    return strdup(cp);
}




nis_result *
_nis_call(struct nis_result * (*cfh)(void *req, CLIENT *clh),
	    void *req,
	    int tcp_flag)
			     
{
    struct nis_result *res, *nres;
    int nresp_flag = 0;
    CLIENT *clh;
    char *address;
    
    if (nis_setup())
	return NULL;

    clh = nisconf_domain2client(nis_local_directory(),
				NULL, &address,
				tcp_flag);
    if (clh == NULL)
	return NULL;
 
    while ((res = (*cfh)(req, clh)) == NULL)
    {
	nresp_flag = 1;
	fprintf(stderr,
    "NIS: server \"%s\" not responding, still trying...\n",
		address);
    }

    if (nresp_flag == 1)
	fprintf(stderr, "NIS: server \"%s\" OK.\n",
		address);

    if (res)
    {
	nres = nis_dupresult(res);

	xdr_free((xdrproc_t) xdr_nis_result, (char *) res);
    }
    else
	nres = NULL;
    
    if (tcp_flag)
    {
	auth_destroy(clh->cl_auth);
	clnt_destroy(clh);
    }

    return nres;
}


nis_result *nis_lookup(nis_name name, u_long flags)
{
    nis_result *res;
    struct ns_request nsreq;
    nis_name *names;
    int i;

    
    if (nis_setup())
	return NULL;

    if (flags & EXPAND_NAME)
    {
        names = nis_getnames(name);
        if (names == NULL)
            return NULL;

        res = NULL;
	i = 0;
	while (names[i] != NULL && (res == NULL || (res->status >= 2)))
        {
            nsreq.ns_name = names[i];
      
            nsreq.ns_object.ns_object_len = 0;
            nsreq.ns_object.ns_object_val = NULL;
	
            res = _nis_call((_nis_cbf_t) nis_lookup_3, &nsreq, 0);

	    i++;
        }

        if (res && res->status == NIS_NOT_ME)
            res->status = NIS_NOTFOUND;

	nis_freenames(names);
    }
    else
    {
        nsreq.ns_name = name;
      
	nsreq.ns_object.ns_object_len = 0;
	nsreq.ns_object.ns_object_val = NULL;
	
	res = _nis_call((_nis_cbf_t) nis_lookup_3, &nsreq, 0);
    }

    return res;
}


nis_result *nis_add(const nis_name name, const nis_object *obj)
{
  nis_result *res;
  struct ns_request nsreq;
    
  if (nis_setup())
    return NULL;
  
  nsreq.ns_name = name;
  
  nsreq.ns_object.ns_object_len = 1;
  nsreq.ns_object.ns_object_val = nis_clone_object(obj,NULL);
  
  res = _nis_call((_nis_cbf_t) nis_add_3, &nsreq, 0);
  
  nis_destroy_object(nsreq.ns_object.ns_object_val);
  
  return res;
}


nis_result *nis_remove(const nis_name name, const nis_object *obj)
{
  nis_result *res;
  struct ns_request nsreq;
    
  if (nis_setup())
    return NULL;
  
  nsreq.ns_name = name;
  
  if (obj != NULL)
    {
       nsreq.ns_object.ns_object_len = 1;
       nsreq.ns_object.ns_object_val = nis_clone_object(obj,NULL);
    }
  else
    {
       nsreq.ns_object.ns_object_len = 0;
       nsreq.ns_object.ns_object_val = NULL;
    }
    
  res = _nis_call((_nis_cbf_t) nis_remove_3, &nsreq, 0);
  
  nis_destroy_object(nsreq.ns_object.ns_object_val);
  
  return res;
}


nis_result *nis_modify(const nis_name name, const nis_object *obj)
{
  nis_result *res;
  struct ns_request nsreq;
    
  if (nis_setup())
    return NULL;
  
  nsreq.ns_name = name;
  
  nsreq.ns_object.ns_object_len = 1;
  nsreq.ns_object.ns_object_val = nis_clone_object(obj,NULL);
  
  res = _nis_call((_nis_cbf_t) nis_modify_3, &nsreq, 0);
  
  nis_destroy_object(nsreq.ns_object.ns_object_val);
  
  return res;
}


struct ib_request *_nis_build_ib_request(const nis_name name, 
					 struct ib_request *ibreq,
					 u_long flags)
{
    ibreq->ibr_name = nis_splitname(name,
			 &ibreq->ibr_srch.ibr_srch_len,
			 &ibreq->ibr_srch.ibr_srch_val);

    if (ibreq->ibr_name == NULL)
	return NULL;

    if (flags & EXPAND_NAME)
    {
	nis_name *names;

	names = nis_getnames(ibreq->ibr_name);
	
	free(ibreq->ibr_name);
	ibreq->ibr_name = NULL;

	if (names == NULL)
	    return NULL;

	ibreq->ibr_name = strdup(names[0]);
	
	nis_freenames(names);
    }
    
    ibreq->ibr_flags = 0;
    
    ibreq->ibr_obj.ibr_obj_len = 0;
    ibreq->ibr_obj.ibr_obj_val = NULL;
    
    ibreq->ibr_cbhost.ibr_cbhost_len = 0;
    ibreq->ibr_cbhost.ibr_cbhost_val = NULL;

    ibreq->ibr_bufsize = 0;

    ibreq->ibr_cookie.n_len = 0;
    ibreq->ibr_cookie.n_bytes = NULL;
    
    return ibreq;
}


static void _nis_clean_ib_request(struct ib_request *ibreq)
{
    if (ibreq->ibr_srch.ibr_srch_len)
    {
	nis_freeattrs(ibreq->ibr_srch.ibr_srch_val,
		      ibreq->ibr_srch.ibr_srch_len);

	ibreq->ibr_srch.ibr_srch_val = NULL;
	ibreq->ibr_srch.ibr_srch_len = 0;
    }

    if (ibreq->ibr_name)
    {
	free(ibreq->ibr_name);
	ibreq->ibr_name = NULL;
    }

    if (ibreq->ibr_cookie.n_bytes)
    {
	free(ibreq->ibr_cookie.n_bytes);
	ibreq->ibr_cookie.n_bytes = NULL;
	ibreq->ibr_cookie.n_len = 0;
    }
}


/* Common save area for nis_first_entry()/nis_next_entry() */
static nis_result *fn_res = NULL;


nis_result *nis_first_entry(const nis_name name)
{
    struct ib_request ibreq;
    nis_result *res;

    
    if (nis_setup() == -1)
	return NULL;

    if (_nis_build_ib_request(name, &ibreq, 0) == NULL)
      return NULL;

    res = _nis_call((_nis_cbf_t) nis_ibfirst_3, &ibreq, 0);

    _nis_clean_ib_request(&ibreq);
    
    if (fn_res)
	nis_freeresult(fn_res);

    fn_res = nis_dupresult (res);

    return res;
}




nis_result *nis_next_entry(const nis_name name, netobj *cookie)
{
    nis_result *res;
    struct ib_request ibreq;


    if (nis_setup() == -1)
	return NULL;
    
    if (_nis_build_ib_request(name, &ibreq, 0) == NULL)
      return NULL;

    if (cookie)
      nis_copynetobj(&ibreq.ibr_cookie, cookie);

    res = _nis_call((_nis_cbf_t) nis_ibnext_3, &ibreq, 0);

    _nis_clean_ib_request(&ibreq);

    if (fn_res)
	nis_freeresult(fn_res);

    fn_res = nis_dupresult (res);

    return res;
}



/*
** Poor mans callback variant, someday I'll implement a real
** callback server. This will eat *lots* of memory if the
** map transfered is big. It'll also block until all the
** map has been moved over to our side...
*/
nis_result *nis_list(const nis_name name,
		     const u_long flags,
		     int (*callback)(const nis_name name,
				     const nis_object *object,
				     const void *userdata),
		     const void *userdata)
{
    nis_result *res;
    struct ib_request ibreq;
    

    if (nis_setup() == -1)
	return NULL;

    if (_nis_build_ib_request(name, &ibreq, flags) == NULL)
      return NULL;

    res = _nis_call((_nis_cbf_t) nis_iblist_3, &ibreq, 1);

    _nis_clean_ib_request(&ibreq);
    
    if (fn_res)
	nis_freeresult(fn_res);

    fn_res = nis_dupresult (res);
    
    if (callback != NULL)
    {
	unsigned int i;
	
	if (res->status > 1)
	    return res;

	for (i = 0; i < res->objects.objects_len; i++)
	    if ( (*callback)(name,
			     &res->objects.objects_val[i],
			     userdata) != 0)
		break;
			     
	return res;
    }

    return res;
}


#endif /* ENABLE_NISEMU */
