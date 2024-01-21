/*
 * Copyright (c) 1992/3 Theo de Raadt <deraadt@fsa.ca>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote
 *    products derived from this software without specific prior written
 *    permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 *  Modifications for linux by Swen Thuemmler <swen@uni-paderborn.de>
 */

#include "yp.h"
#include <rpcsvc/yp_prot.h>
#include <string.h>

#if 0
bool_t
xdr_ypxfrstat(XDR *xdrs, enum ypxfrstat *objp)
{
  if (!xdr_enum(xdrs, (enum_t *)objp))
    {
      return (FALSE);
    }
  return (TRUE);
}
#endif

bool_t
xdr_domainname(XDR *xdrs, char *objp)
{
  if (!xdr_string(xdrs, &objp, YPMAXDOMAIN))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_mapname(XDR *xdrs, mapname *objp)
{
  if (!xdr_string(xdrs, objp, YPMAXMAP))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_peername(XDR *xdrs, peername *objp)
{
  if (!xdr_string(xdrs, objp, YPMAXPEER))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_datum(XDR *xdrs, datum *objp)
{
  if (!xdr_bytes(xdrs,
                 (char **)&objp->dptr,
                 (u_int *)&objp->dsize,
                 YPMAXRECORD))
    {
      return (FALSE);
    }
  return (TRUE);
}

#if 0
bool_t
xdr_ypmap_parms(XDR *xdrs, struct ypmap_parms *objp)
{
  if (!xdr_domainname(xdrs, objp->domain))
    {
      return (FALSE);
    }
  if (!xdr_mapname(xdrs, &objp->map))
    {
      return (FALSE);
    }
  if (!xdr_u_long(xdrs, &objp->ordernum))
    {
      return (FALSE);
    }
  if (!xdr_peername(xdrs, &objp->owner))
    {
      return (FALSE);
    }
  return (TRUE);
}
#endif

bool_t
xdr_ypreq_key(XDR *xdrs, struct ypreq_key *objp)
{
  if (!xdr_domainname(xdrs, objp->domain))
    {
      return (FALSE);
    }
  if (!xdr_mapname(xdrs, &objp->map))
    {
      return (FALSE);
    }
  if (!xdr_datum(xdrs, &objp->keydat))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypreq_nokey(XDR *xdrs, struct ypreq_nokey *objp)
{
  if (!xdr_domainname(xdrs, objp->domain))
    {
      return (FALSE);
    }
  if (!xdr_mapname(xdrs, &objp->map))
    {
      return (FALSE);
    }
  return (TRUE);
}

#if 0
bool_t
xdr_ypreq_xfr(XDR *xdrs, struct ypreq_xfr *objp)
{
  if (!xdr_ypmap_parms(xdrs, &objp->map_parms))
    {
      return (FALSE);
    }
  if (!xdr_u_long(xdrs, &objp->transid))
    {
      return (FALSE);
    }
  if (!xdr_u_long(xdrs, &objp->proto))
    {
      return (FALSE);
    }
  if (!xdr_u_short(xdrs, &objp->port))
    {
      return (FALSE);
    }
  return (TRUE);
}
#endif

bool_t
xdr_ypresp_val(XDR *xdrs, struct ypresp_val *objp)
{
  if (!xdr_u_long(xdrs, &objp->status))
    {
      return (FALSE);
    }
  if (!xdr_datum(xdrs, &objp->valdat))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_key_val(XDR *xdrs, struct ypresp_key_val *objp)
{
  if (!xdr_u_long(xdrs, &objp->status))
    {
      return (FALSE);
    }
/* Silly. The server responds in wrong order !? */
  if (!xdr_datum(xdrs, &objp->valdat))
    {
      return (FALSE);
    }
  if (!xdr_datum(xdrs, &objp->keydat))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_master(XDR *xdrs, struct ypresp_master *objp)
{
  if (!xdr_u_long(xdrs, &objp->status))
    {
      return (FALSE);
    }
  if (!xdr_peername(xdrs, &objp->master))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_order(XDR *xdrs, struct ypresp_order *objp)
{
  if (!xdr_u_long(xdrs, &objp->status))
    {
      return (FALSE);
    }
  if (!xdr_u_long(xdrs, &objp->ordernum))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_all(XDR *xdrs, struct ypresp_all *objp)
{
  if (!xdr_bool(xdrs, &objp->more))
    {
      return (FALSE);
    }
  switch (objp->more) {
  case TRUE:
    if (!xdr_ypresp_key_val(xdrs, &objp->ypresp_all_u.val))
      {
        return (FALSE);
      }
    break;
  case FALSE:
    break;
  default:
    return (FALSE);
  }
  return (TRUE);
}

bool_t
xdr_ypmaplist_str (XDR *xdrs, char *objp)
{
  if (!xdr_mapname(xdrs, &objp))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypmaplist(XDR *xdrs, struct ypmaplist *objp)
{
  if (!xdr_ypmaplist_str(xdrs, objp->ypml_name))
    {
      return (FALSE);
    }
  if (!xdr_pointer(xdrs,
                   (char **)&objp->ypml_next,
                   sizeof(struct ypmaplist),
                   (xdrproc_t)xdr_ypmaplist))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_maplist(XDR *xdrs, struct ypresp_maplist *objp)
{
  if (!xdr_u_long(xdrs, &objp->status))
    {
      return (FALSE);
    }
  if (!xdr_pointer(xdrs, (char **)&objp->list,
                   sizeof(struct ypmaplist), (xdrproc_t)xdr_ypmaplist))
    {
      return (FALSE);
    }
  return (TRUE);
}

#if 0
bool_t
xdr_yppushresp_xfr(XDR *xdrs, struct yppushresp_xfr *objp)
{
  if (!xdr_u_long(xdrs, &objp->transid))
    {
      return (FALSE);
    }
  if (!xdr_u_long(xdrs, &objp->status))
    {
      return (FALSE);
    }
  return (TRUE);
}
#endif

bool_t
xdr_ypbind_resptype(XDR *xdrs, enum ypbind_resptype *objp)
{
  if (!xdr_enum(xdrs, (enum_t *)objp))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypbind_binding(XDR *xdrs, struct ypbind_binding *objp)
{
  if (!xdr_opaque(xdrs, (caddr_t) &objp->ypbind_binding_addr, 4))
    {
      return (FALSE);
    }
  if (!xdr_opaque(xdrs, (caddr_t) &objp->ypbind_binding_port, 2))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypbind_resp(XDR *xdrs, struct ypbind_resp *objp)
{
  if (!xdr_ypbind_resptype(xdrs, &objp->ypbind_status))
    {
      return (FALSE);
    }
  switch (objp->ypbind_status) {
  case YPBIND_FAIL_VAL:
    if (!xdr_u_long(xdrs, &objp->ypbind_respbody.ypbind_error))
      {
        return (FALSE);
      }
    break;
  case YPBIND_SUCC_VAL:
    if (!xdr_ypbind_binding(xdrs, &objp->ypbind_respbody.ypbind_bindinfo))
      {
        return (FALSE);
      }
    break;
  default:
    return (FALSE);
  }
  return (TRUE);
}

bool_t
xdr_ypbind_setdom(XDR *xdrs, struct ypbind_setdom *objp)
{
  if (!xdr_domainname(xdrs, objp->ypsetdom_domain))
    {
      return (FALSE);
    }
  if (!xdr_ypbind_binding(xdrs, &objp->ypsetdom_binding))
    {
      return (FALSE);
    }
  if (!xdr_u_short(xdrs, &objp->ypsetdom_vers))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_all_seq (XDR *xdrs, u_long *objp)
{
  struct ypresp_all out;
  u_long status;
  char *key, *val;
  int r;
  
  bzero (&out, sizeof out);
  while (1)
    {
      if (!xdr_ypresp_all (xdrs, &out))
        {
          xdr_free (xdr_ypresp_all, (char *) &out);
          *objp = YP_YPERR;
          return FALSE;
        }
      if (out.more == 0)
        {
          xdr_free (xdr_ypresp_all, (char *) &out);
          return FALSE;
        }
      status = out.ypresp_all_u.val.status;
      switch (status)
        {
        case YP_TRUE:
          key = (char *) malloc (out.ypresp_all_u.val.keydat.dsize + 1);
          bcopy (out.ypresp_all_u.val.keydat.dptr, key,
                 out.ypresp_all_u.val.keydat.dsize);
          key[out.ypresp_all_u.val.keydat.dsize] = '\0';
          val = (char *) malloc (out.ypresp_all_u.val.valdat.dsize + 1);
          bcopy (out.ypresp_all_u.val.valdat.dptr, val,
                 out.ypresp_all_u.val.valdat.dsize);
          val[out.ypresp_all_u.val.valdat.dsize] = '\0';
          xdr_free (xdr_ypresp_all, (char *) &out);
          
          r = (*ypresp_allfn) (status,
                               key, out.ypresp_all_u.val.keydat.dsize,
                               val, out.ypresp_all_u.val.valdat.dsize,
                               ypresp_data);
          *objp = status;
          free (key);
          free (val);
          if (r)
            return TRUE;
          break;
        case YP_NOMORE:
          xdr_free (xdr_ypresp_all, (char *) &out);
          return TRUE;
        default:
          xdr_free (xdr_ypresp_all, (char *) &out);
          *objp = status;
          return TRUE;
        }
    }
}

bool_t
xdr_yp_inaddr (XDR *xdrs, struct in_addr *objp)
{
  if (!xdr_opaque (xdrs, (caddr_t) & objp->s_addr, sizeof objp->s_addr))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypstat (XDR *xdrs, enum ypbind_resptype *objp)
{
  if (!xdr_enum (xdrs, (enum_t *) objp))
    {
      return (FALSE);
    }
  return (TRUE);
}
