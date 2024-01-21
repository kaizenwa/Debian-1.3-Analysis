#include "config.h"

#ifdef ENABLE_YPEMU

#include <rpc/rpc.h>
#include <rpcsvc/yp.h>


/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

void *
ypproc_null_2(void *argp, CLIENT *clnt)
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_NULL, (xdrproc_t) xdr_void, argp, (xdrproc_t) xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((void *)&res);
}


bool_t *
ypproc_domain_2(domainname *argp, CLIENT *clnt)
{
	static bool_t res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_DOMAIN, (xdrproc_t) xdr_domainname, argp, (xdrproc_t) xdr_bool, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


bool_t *
ypproc_domain_nonack_2(domainname *argp, CLIENT *clnt)
{
	static bool_t res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_DOMAIN_NONACK, (xdrproc_t) xdr_domainname, argp, (xdrproc_t) xdr_bool, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ypresp_val *
ypproc_match_2(ypreq_key *argp, CLIENT *clnt)
{
	static ypresp_val res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_MATCH, (xdrproc_t) xdr_ypreq_key, argp, (xdrproc_t) xdr_ypresp_val, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ypresp_key_val *
ypproc_first_2(ypreq_key *argp, CLIENT *clnt)
{
	static ypresp_key_val res;

	bzero((char *)&res, sizeof(res));
	
	if (clnt_call(clnt, YPPROC_FIRST, (xdrproc_t) xdr_ypreq_key, argp, (xdrproc_t) xdr_ypresp_key_val, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}

	return (&res);
}


ypresp_key_val *
ypproc_next_2(ypreq_key *argp, CLIENT *clnt)
{
	static ypresp_key_val res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_NEXT, (xdrproc_t) xdr_ypreq_key, argp, (xdrproc_t) xdr_ypresp_key_val, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ypresp_xfr *
ypproc_xfr_2(ypreq_xfr *argp, CLIENT *clnt)
{
	static ypresp_xfr res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_XFR, (xdrproc_t) xdr_ypreq_xfr, argp, (xdrproc_t) xdr_ypresp_xfr, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


void *
ypproc_clear_2(void *argp, CLIENT *clnt)
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_CLEAR, (xdrproc_t) xdr_void, argp, (xdrproc_t) xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((void *)&res);
}


ypresp_master *
ypproc_master_2(ypreq_nokey *argp, CLIENT *clnt)
{
	static ypresp_master res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_MASTER, (xdrproc_t) xdr_ypreq_nokey, argp, (xdrproc_t) xdr_ypresp_master, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ypresp_order *
ypproc_order_2(ypreq_nokey *argp, CLIENT *clnt)
{
	static ypresp_order res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_ORDER, (xdrproc_t) xdr_ypreq_nokey, argp, (xdrproc_t) xdr_ypresp_order, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ypresp_maplist *
ypproc_maplist_2(domainname *argp, CLIENT *clnt)
{
	static ypresp_maplist res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPROC_MAPLIST, (xdrproc_t) xdr_domainname, argp, (xdrproc_t) xdr_ypresp_maplist, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


void *
yppushproc_null_1(void *argp, CLIENT *clnt)
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPUSHPROC_NULL, (xdrproc_t) xdr_void, argp, (xdrproc_t) xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((void *)&res);
}


void *
yppushproc_xfrresp_1(yppushresp_xfr *argp, CLIENT *clnt)
{
	static yppushresp_xfr res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPPUSHPROC_XFRRESP, (xdrproc_t) xdr_void, argp, (xdrproc_t) xdr_yppushresp_xfr, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


void *
ypbindproc_null_2(void *argp, CLIENT *clnt)
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPBINDPROC_NULL, (xdrproc_t) xdr_void, argp, (xdrproc_t) xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((void *)&res);
}


ypbind_resp *
ypbindproc_domain_2(domainname *argp, CLIENT *clnt)
{
	static ypbind_resp res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPBINDPROC_DOMAIN, (xdrproc_t) xdr_domainname, argp, (xdrproc_t) xdr_ypbind_resp, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


void *
ypbindproc_setdom_2(ypbind_setdom *argp, CLIENT *clnt)
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, YPBINDPROC_SETDOM, (xdrproc_t) xdr_ypbind_setdom, argp, (xdrproc_t) xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((void *)&res);
}

#endif /* ENABLE_YPEMU */
