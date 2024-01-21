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

#include <rpcsvc/yp.h>
#include <string.h>

/*
 *  Prototypes
 */
bool_t xdr_ypxfrstat         (XDR *, ypxfrstat *);
bool_t xdr_domainname        (XDR *, domainname *);
bool_t xdr_mapname           (XDR *, mapname *);
bool_t xdr_peername          (XDR *, peername *);
bool_t xdr_ypmap_parms       (XDR *, struct ypmap_parms *);
bool_t xdr_ypreq_key         (XDR *, struct ypreq_key *);
bool_t xdr_ypreq_nokey       (XDR *, struct ypreq_nokey *);
bool_t xdr_ypreq_xfr         (XDR *, struct ypreq_xfr *);
bool_t xdr_ypresp_val        (XDR *, struct ypresp_val *);
bool_t xdr_ypresp_key_val    (XDR *, struct ypresp_key_val *);
bool_t xdr_ypresp_master     (XDR *, struct ypresp_master *);
bool_t xdr_ypresp_order      (XDR *, struct ypresp_order *);
bool_t xdr_ypresp_all        (XDR *, struct ypresp_all *);
bool_t xdr_ypmaplist_str     (XDR *, char *);
bool_t xdr_ypresp_maplist    (XDR *, struct ypresp_maplist *);
bool_t xdr_yppushresp_xfr    (XDR *, struct yppushresp_xfr *);
bool_t xdr_ypbind_resptype   (XDR *, enum ypbind_resptype *);
bool_t xdr_ypbind_binding    (XDR *, struct ypbind_binding *);
bool_t xdr_ypbind_resp       (XDR *, struct ypbind_resp *);
bool_t xdr_ypbind_setdom     (XDR *, struct ypbind_setdom *);
bool_t xdr_ypresp_all_seq    (XDR *, u_long *);
bool_t xdr_yp_inaddr         (XDR *, struct in_addr *);
bool_t xdr_ypstat            (XDR *, ypstat *);

/*
 * Begin functions
 */
bool_t
xdr_ypxfrstat(XDR *xdrs, ypxfrstat *objp)
{
  if (!xdr_enum(xdrs, (enum_t *)objp))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_domainname(XDR *xdrs, domainname *objp)
{
  if (!xdr_string(xdrs, objp, YPMAXDOMAIN))
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
xdr_keydat(XDR *xdrs, keydat *objp)
{
  if (!xdr_bytes(xdrs, (char **)&objp->keydat_val,
	(u_int *)&objp->keydat_len, YPMAXRECORD))
  {
    return (FALSE);
  }
  return (TRUE);
}

bool_t
xdr_valdat(XDR *xdrs, valdat *objp)
{
  if (!xdr_bytes(xdrs, (char **)&objp->valdat_val,
	(u_int *)&objp->valdat_len, YPMAXRECORD)) 
  {
    return (FALSE);
  }
  return (TRUE);
}

bool_t
xdr_ypmap_parms(XDR *xdrs, struct ypmap_parms *objp)
{
  if (!xdr_domainname(xdrs, &objp->domain))
    {
      return (FALSE);
    }
  if (!xdr_mapname(xdrs, &objp->map))
    {
      return (FALSE);
    }
  if (!xdr_u_int (xdrs, &objp->ordernum))
    {
      return (FALSE);
    }
  if (!xdr_peername(xdrs, &objp->peer))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypreq_key(XDR *xdrs, struct ypreq_key *objp)
{
  if (!xdr_domainname(xdrs, &objp->domain))
    {
      return (FALSE);
    }
  if (!xdr_mapname(xdrs, &objp->map))
    {
      return (FALSE);
    }
  if (!xdr_keydat (xdrs, &objp->key))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypreq_nokey(XDR *xdrs, struct ypreq_nokey *objp)
{
  if (!xdr_domainname(xdrs, &objp->domain))
    {
      return (FALSE);
    }
  if (!xdr_mapname(xdrs, &objp->map))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypreq_xfr(XDR *xdrs, struct ypreq_xfr *objp)
{
  if (!xdr_ypmap_parms(xdrs, &objp->map_parms))
    {
      return (FALSE);
    }
  if (!xdr_u_int (xdrs, &objp->transid))
    {
      return (FALSE);
    }
  if (!xdr_u_int (xdrs, &objp->prog))
    {
      return (FALSE);
    }
  if (!xdr_u_int (xdrs, &objp->port))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_val(XDR *xdrs, struct ypresp_val *objp)
{
  if (!xdr_ypstat (xdrs, &objp->stat))
    {
      return (FALSE);
    }
  if (!xdr_valdat (xdrs, &objp->val))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_key_val(XDR *xdrs, struct ypresp_key_val *objp)
{
  if (!xdr_ypstat (xdrs, &objp->stat))
    {
      return (FALSE);
    }
/* Silly. The server responds in wrong order !? */
  if (!xdr_valdat (xdrs, &objp->val))
    {
      return (FALSE);
    }
  if (!xdr_keydat (xdrs, &objp->key))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_master(XDR *xdrs, struct ypresp_master *objp)
{
  if (!xdr_ypstat (xdrs, &objp->stat))
    {
      return (FALSE);
    }
  if (!xdr_peername(xdrs, &objp->peer))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_ypresp_order(XDR *xdrs, struct ypresp_order *objp)
{
  if (!xdr_ypstat (xdrs, &objp->stat))
    {
      return (FALSE);
    }
  if (!xdr_u_int (xdrs, &objp->ordernum))
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

#if 0
bool_t
xdr_ypmaplist_str (XDR *xdrs, char *objp)
{
  if (!xdr_mapname(xdrs, &objp))
    {
      return (FALSE);
    }
  return (TRUE);
}
#endif

bool_t
xdr_ypmaplist(XDR *xdrs, struct ypmaplist *objp)
{
#if 0
  if (!xdr_ypmaplist_str(xdrs, objp->map))
#else
  if (!xdr_mapname(xdrs, &objp->map))
#endif
    {
      return (FALSE);
    }
  if (!xdr_pointer(xdrs,
                   (char **)&objp->next,
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
  if (!xdr_ypstat (xdrs, &objp->stat))
    {
      return (FALSE);
    }
  if (!xdr_pointer(xdrs, (char **)&objp->maps,
                   sizeof(struct ypmaplist), (xdrproc_t)xdr_ypmaplist))
    {
      return (FALSE);
    }
  return (TRUE);
}

bool_t
xdr_yppush_status(XDR *xdrs, yppush_status *objp)
{
  if (!xdr_enum(xdrs, (enum_t *)objp)) {
    return (FALSE);
  }
  return (TRUE);
}

bool_t
xdr_yppushresp_xfr(XDR *xdrs, struct yppushresp_xfr *objp)
{
  if (!xdr_u_int(xdrs, &objp->transid))
    {
      return (FALSE);
    }
  if (!xdr_yppush_status (xdrs, &objp->status))
    {
      return (FALSE);
    }
  return (TRUE);
}

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
    if (!xdr_u_int (xdrs, &objp->ypbind_resp_u.ypbind_error))
      {
        return (FALSE);
      }
    break;
  case YPBIND_SUCC_VAL:
    if (!xdr_ypbind_binding(xdrs, &objp->ypbind_resp_u.ypbind_bindinfo))
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
  if (!xdr_domainname(xdrs, &objp->ypsetdom_domain))
    {
      return (FALSE);
    }
  if (!xdr_ypbind_binding(xdrs, &objp->ypsetdom_binding))
    {
      return (FALSE);
    }
  if (!xdr_u_int (xdrs, &objp->ypsetdom_vers))
    {
      return (FALSE);
    }
  return (TRUE);
}

extern int (*ypresp_allfn) (int, char *, int, char *, int, char *);
extern char *ypresp_data;

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
          xdr_free((xdrproc_t) xdr_ypresp_all, (char *) &out);
          *objp = YP_YPERR;
          return FALSE;
        }
      if (out.more == 0)
        {
          xdr_free((xdrproc_t) xdr_ypresp_all, (char *) &out);
          return FALSE;
        }
      status = out.ypresp_all_u.val.stat;
      switch (status)
        {
        case YP_TRUE:
          key = (char *) malloc (out.ypresp_all_u.val.key.keydat_len + 1);
          bcopy (out.ypresp_all_u.val.key.keydat_val, key,
                 out.ypresp_all_u.val.key.keydat_len);
          key[out.ypresp_all_u.val.key.keydat_len] = '\0';
          val = (char *) malloc (out.ypresp_all_u.val.val.valdat_len + 1);
          bcopy (out.ypresp_all_u.val.val.valdat_val, val,
                 out.ypresp_all_u.val.val.valdat_len);
          val[out.ypresp_all_u.val.val.valdat_len] = '\0';
          xdr_free((xdrproc_t) xdr_ypresp_all, (char *) &out);
          
          r = (*ypresp_allfn) (status, key,
                            out.ypresp_all_u.val.key.keydat_len, val,
                            out.ypresp_all_u.val.val.valdat_len, ypresp_data);
          *objp = status;
          free (key);
          free (val);
          if (r)
            return TRUE;
          break;
        case YP_NOMORE:
          xdr_free((xdrproc_t) xdr_ypresp_all, (char *) &out);
          return TRUE;
        default:
          xdr_free((xdrproc_t) xdr_ypresp_all, (char *) &out);
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
xdr_ypstat (XDR *xdrs, ypstat *objp)
{
  if (!xdr_enum (xdrs, (enum_t *) objp))
    {
      return (FALSE);
    }
  return (TRUE);
}
