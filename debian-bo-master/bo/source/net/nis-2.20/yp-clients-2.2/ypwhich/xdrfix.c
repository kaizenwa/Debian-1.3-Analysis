#include <rpcsvc/yp_prot.h>
#include <string.h>

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
