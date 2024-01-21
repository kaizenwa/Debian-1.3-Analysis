/*
 * $Log: xdr_yp.h,v $
 * Revision 1.1.1.1  1996/08/11 17:35:22  kukuk
 * NYS ypserv package
 *
 * Revision 2.2  1995/07/27 17:30:04  swen
 * Removed __P() macros. We're using ANSI anyway.
 *
 * Revision 2.1  1995/01/24  12:24:30  swen
 * Added RCS keywords.
 *
 */

extern bool_t xdr_domainname (XDR *, char *);
extern bool_t xdr_peername (XDR *, char *);
extern bool_t xdr_datum (XDR *, datum *);
extern bool_t xdr_mapname (XDR *, char *);
extern bool_t xdr_ypreq_key (XDR *, struct ypreq_key *);
extern bool_t xdr_ypreq_nokey (XDR *, struct ypreq_nokey *);
extern bool_t xdr_yp_inaddr (XDR *, struct in_addr *);
extern bool_t xdr_ypbind_binding (XDR *, struct ypbind_binding *);
extern bool_t xdr_ypbind_resptype (XDR *, enum ypbind_resptype *);
extern bool_t xdr_ypbind_resp (XDR *, struct ypbind_resp *);
extern bool_t xdr_ypresp_val (XDR *, struct ypresp_val *);
extern bool_t xdr_ypbind_setdom (XDR *, struct ypbind_setdom *);
extern bool_t xdr_ypresp_key_val (XDR *, struct ypresp_key_val *);
extern bool_t xdr_ypresp_all_seq (XDR *, u_long *);
extern bool_t xdr_ypresp_master (XDR *, struct ypresp_master *);
extern bool_t xdr_ypmaplist_str (XDR *, char *);
extern bool_t xdr_ypmaplist (XDR *, struct ypmaplist *);
extern bool_t xdr_ypresp_maplist (XDR *, struct ypresp_maplist *);
extern bool_t xdr_ypresp_order (XDR *, struct ypresp_order *);
