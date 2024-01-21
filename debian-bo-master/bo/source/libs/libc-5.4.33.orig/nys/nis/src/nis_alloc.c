/*
** nis_alloc.c           NIS+ object allocation / freeing routines.
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

#include <stdio.h>
#include <rpcsvc/nis.h>
#include "xalloc.h"
#include "nis_alloc.h"


/*
** Functions dealing with "netobj" objects
*/
netobj *
nis_copynetobj(netobj *nno, netobj *no)
{
    if (nno == NULL || no == NULL)
	return NULL;

    memset(nno, 0, sizeof(*nno));

    if (no->n_len > 0)
    {
	nno->n_bytes = xdup(no->n_bytes, no->n_len);
	if (nno->n_bytes == NULL)
	    return NULL;
	
	nno->n_len = no->n_len;
    }

    return nno;
}

netobj *
nis_dupnetobjs(netobj *no, int len)
{
    netobj *nno = NULL;
    int i;

    
    if (no == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&nno, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copynetobj(&nno[i], &no[i]) == NULL)
	{
	    nis_freenetobjs(nno, i-1);
	    return NULL;
	}

    return nno;
}

netobj *
nis_dupnetobj(netobj *no)
{
    return nis_dupnetobjs(no, 1);
}


void
nis_cleannetobj(netobj *no)
{
    if (no == NULL)
	return;

    if (no->n_bytes && no->n_len > 0)
    {
	free(no->n_bytes);
	no->n_bytes = NULL;
	no->n_len = 0;
    }
}

void
nis_freenetobjs(netobj *no, int len)
{
    int i;


    if (no == NULL)
	return;
    
    for (i = 0; i < len; i++)
	nis_cleannetobj(&no[i]);

    free(no);
}

void
nis_freenetobj(netobj *no)
{
    nis_freenetobjs(no, 1);
}



/*
** Functions dealing with "endpoint" objects
*/
endpoint *
nis_copyendpoint(endpoint *nep, endpoint *ep)
{
    if (nep == NULL || ep == NULL)
	return NULL;

    memset(nep, 0, sizeof(*nep));

    if (ep->uaddr)
	if ((nep->uaddr = xstrdup(ep->uaddr)) == NULL)
	    return NULL;

    if (ep->family)
	if ((nep->family = xstrdup(ep->family)) == NULL)
	    return NULL;

    if (ep->proto)
	if ((nep->proto = xstrdup(ep->proto)) == NULL)
	    return NULL;

    return nep;
}

endpoint *
nis_dupendpoints(endpoint *ep, int len)
{
    endpoint *nep = NULL;
    int i;

    
    if (ep == NULL || len <= 0)
	return NULL;
    
    if (Xalloc(&nep, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copyendpoint(&nep[i], &ep[i]) == NULL)
	{
	    nis_freeendpoints(nep, i-1);
	    return NULL;
	}

    return nep;
}

endpoint *
nis_dupendpoint(endpoint *ep)
{
    return nis_dupendpoints(ep, 1);
}


void
nis_cleanendpoint(endpoint *ep)
{
    if (ep == NULL)
	return;

    if (ep->uaddr)
    {
	free(ep->uaddr);
	ep->uaddr = NULL;
    }
    if (ep->family)
    {
	free(ep->family);
	ep->family = NULL;
    }
    if (ep->proto)
    {
	free(ep->proto);
	ep->proto = NULL;
    }
}

void
nis_freeendpoints(endpoint *ep, int len)
{
    int i;


    if (ep == NULL)
	return;
    
    for (i = 0; i < len; i++)
	nis_cleanendpoint(&ep[i]);

    free(ep);
}

void
nis_freeendpoint(endpoint *ep)
{
    nis_freeendpoints(ep, 1);
}



/*
** Functions dealing with "nis_server" objects
*/
nis_server *
nis_copyserver(nis_server *nns, nis_server *ns)
{
    if (nns == NULL || ns == NULL)
	return NULL;

    memset(nns, 0, sizeof(*nns));

    if (ns->name)
	if ((nns->name = xstrdup(ns->name)) == NULL)
	    return NULL;


    if (ns->ep.ep_len > 0)
    {
	nns->ep.ep_val = nis_dupendpoints(ns->ep.ep_val, ns->ep.ep_len);
	if (nns->ep.ep_val == NULL)
	    return NULL;
	nns->ep.ep_len = ns->ep.ep_len;
    }
    else
    {
	nns->ep.ep_val = NULL;
	nns->ep.ep_len = 0;
    }
    
    nns->key_type = ns->key_type;
    if (nis_copynetobj(&nns->pkey, &ns->pkey) == NULL)
	return NULL;

    return nns;
}

nis_server *
nis_dupservers(nis_server *ns, int len)
{
    nis_server *nns = NULL;
    int i;


    if (ns == NULL || len <= 0)
	return NULL;
    
    if (Xalloc(&nns, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copyserver(&nns[i], &ns[i]) == NULL)
	{
	    nis_freeservers(nns, i-1);
	    return NULL;
	}

    return nns;
}

nis_server *
nis_dupserver(nis_server *ns)
{
    return nis_dupservers(ns, 1);
}


void
nis_cleanserver(nis_server *ns)
{
    if (ns == NULL)
	return;

    if (ns->name)
    {
	free(ns->name);
	ns->name = NULL;
    }

    if (ns->ep.ep_val)
    {
	nis_freeendpoints(ns->ep.ep_val, ns->ep.ep_len);
	ns->ep.ep_val = NULL;
	ns->ep.ep_len = 0;
    }

    nis_cleannetobj(&ns->pkey);
}

void
nis_freeservers(nis_server *ns, int len)
{
    int i;


    if (ns == NULL)
	return;
    
    for (i = 0; i < len; i++)
	nis_cleanserver(&ns[i]);

    free(ns);
}

void
nis_freeserver(nis_server *ns)
{
    nis_freeservers(ns, 1);
}



/*
** Functions to deal with "directory_obj" objects
*/
directory_obj *
nis_copydirectory(directory_obj *ndob, directory_obj *dob)
{
    if (ndob == NULL || dob == NULL)
	return NULL;

    memset(ndob, 0, sizeof(*ndob));

    if (dob->do_name)
	if ((ndob->do_name = xstrdup(dob->do_name)) == NULL)
	    return NULL;

    ndob->do_type = dob->do_type;

    if (dob->do_servers.do_servers_len > 0)
    {
	ndob->do_servers.do_servers_val =
	    nis_dupservers(dob->do_servers.do_servers_val,
			   dob->do_servers.do_servers_len);
	if (ndob->do_servers.do_servers_val == NULL)
	    return NULL;

	ndob->do_servers.do_servers_len = dob->do_servers.do_servers_len;
    }
    else
    {
	ndob->do_servers.do_servers_len = 0;
	ndob->do_servers.do_servers_val = NULL;
    }
	
    
    ndob->do_ttl  = dob->do_ttl;

    if (dob->do_armask.do_armask_len > 0)
    {
	ndob->do_armask.do_armask_val = 
	    xdup(dob->do_armask.do_armask_val,
		 dob->do_armask.do_armask_len);
	
	if (ndob->do_armask.do_armask_val)
	    return NULL;
	
	ndob->do_armask.do_armask_len = dob->do_armask.do_armask_len;
    }
    else
    {
	ndob->do_armask.do_armask_val = NULL;
	ndob->do_armask.do_armask_len = 0;
    }
    
    return ndob;
}

directory_obj *
nis_dupdirectories(directory_obj *dob, int len)
{
    directory_obj *ndo = NULL;
    int i;


    if (dob == NULL || len <= 0)
	return NULL;
    
    if (Xalloc(&ndo, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copydirectory(&ndo[i], &dob[i]) == NULL)
	{
	    nis_freedirectories(ndo, i-1);
	    return NULL;
	}

    return ndo;
}

directory_obj *
nis_dupdirectory(directory_obj *dob)
{
    return nis_dupdirectories(dob, 1);
}


void
nis_cleandirectory(directory_obj *dob)
{
    if (dob == NULL)
	return;
    
    if (dob->do_name)
	free(dob->do_name);

    if (dob->do_servers.do_servers_len > 0)
    {
	nis_freeservers(dob->do_servers.do_servers_val,
			dob->do_servers.do_servers_len);
	dob->do_servers.do_servers_val = NULL;
	dob->do_servers.do_servers_len = 0;
    }

    if (dob->do_armask.do_armask_len > 0)
    {
	free(dob->do_armask.do_armask_val);
	dob->do_armask.do_armask_val = NULL;
	dob->do_armask.do_armask_len = 0;
    }
}

void
nis_freedirectories(directory_obj *dob, int len)
{
    int i;


    for (i = 0; i < len; i++)
	nis_cleandirectory(&dob[i]);

    free(dob);
}

void
nis_freedirectory(directory_obj *dob)
{
    nis_freedirectories(dob, 1);
}


group_obj *
nis_copygroup(group_obj *ngo, group_obj *go)
{
    unsigned int i;

    
    if (ngo == NULL || go == NULL)
	return NULL;

    memset(ngo, 0, sizeof(*ngo));

    ngo->gr_flags = go->gr_flags;
    ngo->gr_members.gr_members_len = go->gr_members.gr_members_len;
    if (Xalloc(&ngo->gr_members.gr_members_val,
	       go->gr_members.gr_members_len) == NULL)
	return NULL;
    
    for (i = 0; i < go->gr_members.gr_members_len; i++)
	if ((ngo->gr_members.gr_members_val[i] =
	     xstrdup(go->gr_members.gr_members_val[i])) == NULL)
	    return NULL;
    
    return ngo;
}

group_obj *
nis_dupgroups(group_obj *go, int len)
{
    group_obj *ngo = NULL;
    int i;


    if (go == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&ngo, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copygroup(&ngo[i], &go[i]) == NULL)
	{
	    nis_freegroups(ngo, i-1);
	    return NULL;
	}

    return ngo;
}

group_obj *
nis_dupgroup(group_obj *go)
{
    return nis_dupgroups(go, 1);
}


void
nis_cleangroup(group_obj *go)
{
    unsigned int i;
    
    if (go->gr_members.gr_members_val)
    {
	for (i = 0; i < go->gr_members.gr_members_len; i++)
	    if (go->gr_members.gr_members_val[i])
		free(go->gr_members.gr_members_val[i]);

	free(go->gr_members.gr_members_val);
	go->gr_members.gr_members_val = NULL;
	go->gr_members.gr_members_len = 0;
    }
}

void
nis_freegroups(group_obj *go, int len)
{
    int i;


    for (i = 0; i < len; i++)
	nis_cleangroup(&go[i]);

    free(go);
}

void
nis_freegroup(group_obj *go)
{
    nis_freegroups(go, 1);
}



static void
nis_freetable_cols(table_col *tc, int len)
{
    int i;

    for (i = 0; i < len; i++)
	if (tc[i].tc_name)
	    free(tc[i].tc_name);
    free(tc);
}

static table_col *
nis_duptable_cols(table_col *tc, int len)
{
    table_col *ntc = NULL;
    int i;


    if (tc == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&ntc, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
    {
	if ((ntc[i].tc_name = xstrdup(tc[i].tc_name)) == NULL)
	    goto error;

	ntc[i].tc_flags = tc[i].tc_flags;
	ntc[i].tc_rights = tc[i].tc_rights;
    }

    return ntc;

  error:
    nis_freetable_cols(ntc, len);
    return NULL;
}


    

/*
** Functions dealing with "table_obj" objects
*/
table_obj *
nis_copytable(table_obj *nto, table_obj *to)
{
    if (nto == NULL || to == NULL)
	return NULL;

    memset(nto, 0, sizeof(*nto));

    if ((nto->ta_type = xstrdup(to->ta_type)) == NULL)
	return NULL;

    nto->ta_maxcol = to->ta_maxcol;
    nto->ta_sep = to->ta_sep;

    if (to->ta_cols.ta_cols_len > 0)
    {
	nto->ta_cols.ta_cols_val = nis_duptable_cols(to->ta_cols.ta_cols_val,
						     to->ta_cols.ta_cols_len);
	if (nto->ta_cols.ta_cols_val == NULL)
	    goto Error;

	nto->ta_cols.ta_cols_len = to->ta_cols.ta_cols_len;
    }

    if (to->ta_path)
      if ((nto->ta_path = xstrdup(to->ta_path)) == NULL)
	  goto Error;

    return nto;

  Error:
    nis_cleantable(nto);
    return NULL;
}

table_obj *
nis_duptables(table_obj *to, int len)
{
    table_obj *nto = NULL;
    int i;


    if (to == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&nto, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copytable(&nto[i], &to[i]) == NULL)
	{
	    nis_freetables(nto, i-1);
	    return NULL;
	}

    return nto;
}

table_obj *
nis_duptable(table_obj *to)
{
    return nis_duptables(to, 1);
}



void
nis_cleantable(table_obj *to)
{
    if (to == NULL)
	return;
    
    if (to->ta_type)
    {
	free(to->ta_type);
	to->ta_type = NULL;
    }
    
    if (to->ta_cols.ta_cols_val)
    {
	nis_freetable_cols(to->ta_cols.ta_cols_val,
			   to->ta_cols.ta_cols_len);
	to->ta_cols.ta_cols_val = NULL;
	to->ta_cols.ta_cols_len = 0;
    }
    
    if (to->ta_path)
    {
	free(to->ta_path);
	to->ta_path = NULL;
    }
}

void
nis_freetables(table_obj *to, int len)
{
    int i;


    for (i = 0; i < len; i++)
	nis_cleantable(&to[i]);
    
    free(to);
}

void
nis_freetable(table_obj *to)
{
    nis_freetables(to, 1);
}


static void
nis_freeentry_cols(entry_col *ec, int len)
{
    int i;


    if (ec == NULL)
	return;
    
    for (i = 0; i < len; i++)
	if (ec[i].ec_value.ec_value_val)
	{
	    free(ec[i].ec_value.ec_value_val);
        }

    free(ec);
}

static entry_col *
nis_dupentry_cols(entry_col *ec, int len)
{
    entry_col *nec = NULL;
    int i;


    if (ec == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&nec, len) == NULL)
	return NULL;

    nec->ec_flags = ec->ec_flags;
    
    for (i = 0; i < len; i++)
    {
	if (ec[i].ec_value.ec_value_len > 0)
	{
	    nec[i].ec_value.ec_value_val = xdup(ec[i].ec_value.ec_value_val,
						ec[i].ec_value.ec_value_len);

	    if (nec[i].ec_value.ec_value_val == NULL)
		goto error;
	    
	    nec[i].ec_value.ec_value_len = ec[i].ec_value.ec_value_len;
	}
	else
	{
	    nec[i].ec_value.ec_value_val = NULL;
	    nec[i].ec_value.ec_value_len = 0;
	}
    }

    return nec;
    
  error:
    nis_freeentry_cols(nec, len);
    return NULL;
}



/*
** Functions dealing with "entry_obj" objects
*/
entry_obj *
nis_copyentry(entry_obj *neo, entry_obj *eo)
{
    if (neo == NULL || eo == NULL)
	return NULL;

    memset(neo, 0, sizeof(*neo));
    
    if ((neo->en_type = xstrdup(eo->en_type)) == NULL)
	return NULL;

    if (eo->en_cols.en_cols_len > 0)
    {
	neo->en_cols.en_cols_val = nis_dupentry_cols(eo->en_cols.en_cols_val,
						     eo->en_cols.en_cols_len);
	if (neo->en_cols.en_cols_val == NULL)
	    return NULL;
    }
    neo->en_cols.en_cols_len = eo->en_cols.en_cols_len;
    
    return neo;
}

entry_obj *
nis_dupentries(entry_obj *eo, int len)
{
    entry_obj *neo = NULL;
    int i;


    if (eo == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&neo, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copyentry(&neo[i], &eo[i]) == NULL)
	{
	    nis_freeentries(neo, i-1);
	    return NULL;
	}

    return neo;
}

entry_obj *
nis_dupentry(entry_obj *eo)
{
    return nis_dupentries(eo, 1);
}

void
nis_cleanentry(entry_obj *eo)
{
    if (eo == NULL)
	return;
    
    if (eo->en_type)
    {
	free(eo->en_type);
	eo->en_type = NULL;
    }
    
    if (eo->en_cols.en_cols_val)
    {
	nis_freeentry_cols(eo->en_cols.en_cols_val,
			   eo->en_cols.en_cols_len);
	eo->en_cols.en_cols_val = NULL;
	eo->en_cols.en_cols_len = 0;
    }
}

void
nis_freeentries(entry_obj *eo, int len)
{
    int i;

    if (eo == NULL)
	return;

    for (i = 0; i < len; i++)
	nis_cleanentry(&eo[i]);

    free(eo);
}

void
nis_freeentry(entry_obj *eo)
{
    nis_freeentries(eo, 1);
}



/*
** Functions dealing with "nis_attr" objects
*/
nis_attr *
nis_copyattr(nis_attr *nat, nis_attr *at)
{
    if (nat == NULL || at == NULL)
	return NULL;

    memset(nat, 0, sizeof(*nat));
    
    if ((nat->zattr_ndx = xstrdup(at->zattr_ndx)) == NULL)
	return NULL;
    
    nat->zattr_val.zattr_val_len = at->zattr_val.zattr_val_len;

    if (at->zattr_val.zattr_val_val)
    {
	nat->zattr_val.zattr_val_val = xdup(at->zattr_val.zattr_val_val,
					    at->zattr_val.zattr_val_len);
	
	if (nat->zattr_val.zattr_val_val == NULL)
	    return NULL;
    }
    else
    {
	nat->zattr_val.zattr_val_val = NULL;
	nat->zattr_val.zattr_val_len = 0;
    }
    
    return nat;
}

nis_attr *
nis_dupattrs(nis_attr *at, int len)
{
    nis_attr *nat = NULL;
    int i;


    if (at == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&nat, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copyattr(&nat[i], &at[i]) == NULL)
	{
	    nis_freeattrs(nat, i-1);
	    return NULL;
	}
    
    return nat;
}

nis_attr *
nis_dupattr(nis_attr *at)
{
    return nis_dupattrs(at, 1);
}


void
nis_cleanattr(nis_attr *at)
{
    if (at == NULL)
	return;
    
    if (at->zattr_ndx)
    {
	free(at->zattr_ndx);
	at->zattr_ndx = NULL;
    }
    
    if (at->zattr_val.zattr_val_val)
    {
	free(at->zattr_val.zattr_val_val);
	at->zattr_val.zattr_val_val = NULL;
	at->zattr_val.zattr_val_len = 0;
    }
}

void
nis_freeattrs(nis_attr *at, int len)
{
    int i;


    if (at == NULL)
	return;
    
    for (i = 0; i < len; i++)
	nis_cleanattr(&at[i]);

    free(at);
}

void
nis_freeattr(nis_attr *at)
{
    nis_freeattrs(at, 1);
}



/*
** Functions dealing with "link_obj" objects
*/
link_obj *
nis_copylink(link_obj *nlo, link_obj *lo)
{
    if (nlo == NULL || lo == NULL)
	return NULL;
    
    memset(nlo, 0, sizeof(*nlo));
    
    nlo->li_rtype = lo->li_rtype;
    
    if ((nlo->li_name = xstrdup(lo->li_name)) == NULL)
	return NULL;
    
    if (lo->li_attrs.li_attrs_len > 0)
    {
	nlo->li_attrs.li_attrs_val = nis_dupattrs(lo->li_attrs.li_attrs_val,
						  lo->li_attrs.li_attrs_len);

	if (nlo->li_attrs.li_attrs_val == NULL)
	    return NULL;
    }
    nlo->li_attrs.li_attrs_len = lo->li_attrs.li_attrs_len;

    return nlo;
}

link_obj *
nis_duplinks(link_obj *lo, int len)
{
    link_obj *nlo = NULL;
    int i;


    if (lo == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&nlo, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copylink(&nlo[i], &lo[i]) == NULL)
	{
	    nis_freelinks(nlo, i-1);
	    return NULL;
	}

    return nlo;
}

link_obj *
nis_duplink(link_obj *lo)
{
    return nis_duplinks(lo, 1);
}


void
nis_cleanlink(link_obj *lo)
{
    if (lo == NULL)
	return;

    if (lo->li_attrs.li_attrs_val)
    {
	nis_freeattrs(lo->li_attrs.li_attrs_val, lo->li_attrs.li_attrs_len);
	lo->li_attrs.li_attrs_val = NULL;
	lo->li_attrs.li_attrs_len = 0;
    }
    
    if (lo->li_name)
    {
	free(lo->li_name);
	lo->li_name = NULL;
    }
}

void
nis_freelinks(link_obj *lo, int len)
{
    int i;
    
    if (lo == NULL)
	return;

    for (i = 0; i < len; i++)
	nis_cleanlink(&lo[i]);
    
    free(lo);
}

void
nis_freelink(link_obj *lo)
{
    nis_freelinks(lo, 1);
}



/*
** Functions dealing with "objdata" objects
*/
objdata *
nis_copyobjdata(objdata *nobd, objdata *obd)
{
    memset(nobd, 0, sizeof(*nobd));
	   
    nobd->zo_type = obd->zo_type;

    switch (obd->zo_type)
    {
      case BOGUS_OBJ:
	break;
	
      case NO_OBJ:
	break;
	
      case DIRECTORY_OBJ:
	if (nis_copydirectory(&nobd->objdata_u.di_data,
			      &obd->objdata_u.di_data) == NULL)
	    return NULL;
	break;

      case GROUP_OBJ:
	if (nis_copygroup(&nobd->objdata_u.gr_data,
			  &obd->objdata_u.gr_data) == NULL)
	    return NULL;
	break;

      case TABLE_OBJ:
	if (nis_copytable(&nobd->objdata_u.ta_data,
			  &obd->objdata_u.ta_data) == NULL)
	    return NULL;
	break;

      case ENTRY_OBJ:
	if (nis_copyentry(&nobd->objdata_u.en_data,
			  &obd->objdata_u.en_data) == NULL)
	    return NULL;
	break;

      case LINK_OBJ:
	if (nis_copylink(&nobd->objdata_u.li_data,
			 &obd->objdata_u.li_data) == NULL)
	    return NULL;
	break;

      case PRIVATE_OBJ:
	nobd->objdata_u.po_data.po_data_len =
	    obd->objdata_u.po_data.po_data_len;

	if (obd->objdata_u.po_data.po_data_val)
	{
	    nobd->objdata_u.po_data.po_data_val =
		xdup(obd->objdata_u.po_data.po_data_val,
		     obd->objdata_u.po_data.po_data_len);
	
	    if (nobd->objdata_u.po_data.po_data_val == NULL)
		return NULL;
	}
	else
	{
	    nobd->objdata_u.po_data.po_data_val = NULL;
	    nobd->objdata_u.po_data.po_data_len = 0;
	}
	break;

      default:
	return NULL;
    }

    return nobd;
}

objdata *
nis_dupobjdatas(objdata *obd, int len)
{
    objdata *nobd = NULL;
    int i;
    

    if (obd == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&nobd, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copyobjdata(&nobd[i], &obd[i]) == NULL)
	{
	    nis_freeobjdatas(nobd, i-1);
	    return NULL;
	}

    return nobd;
}

objdata *
nis_dupobjdata(objdata *obd)
{
    return nis_dupobjdatas(obd, 1);
}


void
nis_cleanobjdata(objdata *obd)
{
    switch (obd->zo_type)
    {
      case BOGUS_OBJ:
	break;
	
      case NO_OBJ:
	break;
	
      case DIRECTORY_OBJ:
	nis_cleandirectory(&obd->objdata_u.di_data);
	break;

      case GROUP_OBJ:
	nis_cleangroup(&obd->objdata_u.gr_data);
	break;

      case TABLE_OBJ:
	nis_cleantable(&obd->objdata_u.ta_data);
	break;

      case ENTRY_OBJ:
	nis_cleanentry(&obd->objdata_u.en_data);
	break;

      case LINK_OBJ:
	nis_cleanlink(&obd->objdata_u.li_data);
	break;

      case PRIVATE_OBJ:
	if (obd->objdata_u.po_data.po_data_val)
	{
	    free(obd->objdata_u.po_data.po_data_val);
	    obd->objdata_u.po_data.po_data_val = NULL;
	    obd->objdata_u.po_data.po_data_len = 0;
	}
	break;

      default:
	break;
    }
}

void
nis_freeobjdatas(objdata *obd, int len)
{
    int i;
    
    if (obd == NULL)
	return;

    for (i = 0; i < len; i++)
	nis_cleanobjdata(&obd[i]);
    
    free(obd);
}

void
nis_freeobjdata(objdata *obd)
{
    nis_freeobjdatas(obd, 1);
}



/*
** Functions dealing with "nis_object" objects
*/
nis_object *
nis_copyobject(nis_object *nob, nis_object *ob)
{
    if (nob == NULL | ob == NULL)
	return NULL;

    memset(nob, 0, sizeof(*nob));
	   
    nob->zo_oid = ob->zo_oid;
    
    if ((nob->zo_name = xstrdup(ob->zo_name)) == NULL)
	return NULL;
    if ((nob->zo_owner = xstrdup(ob->zo_owner)) == NULL)
	return NULL;
    if ((nob->zo_group = xstrdup(ob->zo_group)) == NULL)
	return NULL;
    if ((nob->zo_domain = xstrdup(ob->zo_domain)) == NULL)
	return NULL;

    nob->zo_access = ob->zo_access;
    nob->zo_ttl = ob->zo_ttl;
    
    if (nis_copyobjdata(&nob->zo_data, &ob->zo_data) == NULL)
	return NULL;

    return nob;
}

nis_object *
nis_dupobjects(nis_object *ob, int len)
{
    nis_object *nob = NULL;
    int i;


    if (ob == NULL || len == 0)
	return NULL;
    
    if (Xalloc(&nob, len) == NULL)
	return NULL;

    for (i = 0; i < len; i++)
	if (nis_copyobject(&nob[i], &ob[i]) == NULL)
	{
	    nis_freeobjects(nob, i-1);
	    return NULL;
	}

    return nob;
}

nis_object *
nis_dupobject(nis_object *ob)
{
    return nis_dupobjects(ob, 1);
}


nis_object *
nis_clone_object(nis_object *src, nis_object *dest)
{
  if(dest == NULL)
    return nis_dupobject(src);
  else
    return nis_copyobject(dest,src);
}


void
nis_cleanobject(nis_object *ob)
{
    if (ob == NULL)
	return;

    if (ob->zo_name)
    {
	free(ob->zo_name);
	ob->zo_name = NULL;
    }
    if (ob->zo_owner)
    {
	free(ob->zo_owner);
	ob->zo_owner = NULL;
    }
    if (ob->zo_group)
    {
	free(ob->zo_group);
	ob->zo_group = NULL;
    }
    if (ob->zo_domain)
    {
	free(ob->zo_domain);
	ob->zo_domain = NULL;
    }

    nis_cleanobjdata(&ob->zo_data);
}

void
nis_freeobjects(nis_object *ob, int len)
{
    int i;

    
    if (ob == NULL)
	return;

    for (i = 0; i < len; i++)
	nis_cleanobject(&ob[i]);
    
    free(ob);
}

void
nis_freeobject(nis_object *ob)
{
    nis_freeobjects(ob, 1);
}

void 
nis_destroy_object(nis_object *obj) 
{
   nis_freeobjects(obj,1);
}



/*
** Functions dealing with "nis_result" objects
*/
nis_result *
nis_copyresult(nis_result *nres, nis_result *res)
{
    if (res == NULL || nres == NULL)
	return NULL;
    
    memset(nres, 0, sizeof(*nres));
    
    nres->status = res->status;

    if (res->objects.objects_len > 0)
    {
        nres->objects.objects_val = nis_dupobjects(res->objects.objects_val,
	  				           res->objects.objects_len);

        if (nres->objects.objects_val == NULL)
	    return NULL;
    }
    nres->objects.objects_len = res->objects.objects_len;

    if (nis_copynetobj(&nres->cookie, &res->cookie) == NULL)
	return NULL;

    nres->zticks = res->zticks;
    nres->dticks = res->dticks;
    nres->aticks = res->aticks;
    nres->cticks = res->cticks;

    return nres;
}

nis_result *
nis_dupresult(nis_result *res)
{
    nis_result *nres = NULL;


    if (res == NULL)
	return NULL;
    
    if (Xnew(nres) == NULL)
	return NULL;

    if (nis_copyresult(nres, res) == NULL)
    {
	nis_freeresult(nres);
	return NULL;
    }

    return nres;
}


void
nis_cleanresult(nis_result *nsres)
{
    if (nsres == NULL)
	return;

    if (nsres->objects.objects_val)
    {
	nis_freeobjects(nsres->objects.objects_val,
			nsres->objects.objects_len);
	nsres->objects.objects_val = NULL;
	nsres->objects.objects_len = 0;
    }

    nis_cleannetobj(&nsres->cookie);
}

void
nis_freeresults(nis_result *res, int len)
{
    int i;

    
    if (res == NULL)
	return;

    for (i = 0; i < len; i++)
	nis_cleanresult(&res[i]);
    
    free(res);
}

void
nis_freeresult(nis_result *res)
{
    nis_freeresults(res, 1);
}
