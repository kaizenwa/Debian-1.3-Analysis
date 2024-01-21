#include "config.h"

#ifdef ENABLE_YPEMU

#include <rpcsvc/ypupdate_prot.h>

bool_t xdr_yp_buf(XDR *xdrs, yp_buf *objp)
{
	 if (!xdr_bytes(xdrs, (char **)&objp->yp_buf_val, (u_int *)&objp->yp_buf_len, MAXYPDATALEN)) {
		 return (FALSE);
	 }
	return (TRUE);
}

bool_t xdr_ypupdate_args(XDR *xdrs, ypupdate_args *objp)
{
	 if (!xdr_string(xdrs, &objp->mapname, MAXMAPNAMELEN)) {
		 return (FALSE);
	 }
	 if (!xdr_yp_buf(xdrs, &objp->key)) {
		 return (FALSE);
	 }
	 if (!xdr_yp_buf(xdrs, &objp->datum)) {
		 return (FALSE);
	 }
	return (TRUE);
}

bool_t
xdr_ypdelete_args(XDR *xdrs, ypdelete_args *objp)
{
	 if (!xdr_string(xdrs, &objp->mapname, MAXMAPNAMELEN)) {
		 return (FALSE);
	 }
	 if (!xdr_yp_buf(xdrs, &objp->key)) {
		 return (FALSE);
	 }
	return (TRUE);
}

#endif
