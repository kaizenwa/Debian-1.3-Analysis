#include <rpc/rpc.h>
#include "rquota.h"

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

getquota_rslt *
rquotaproc_getquota_1(argp, clnt)
	getquota_args *argp;
	CLIENT *clnt;
{
	static getquota_rslt res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, RQUOTAPROC_GETQUOTA, xdr_getquota_args, argp, xdr_getquota_rslt, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


getquota_rslt *
rquotaproc_getactivequota_1(argp, clnt)
	getquota_args *argp;
	CLIENT *clnt;
{
	static getquota_rslt res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, RQUOTAPROC_GETACTIVEQUOTA, xdr_getquota_args, argp, xdr_getquota_rslt, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


getquota_rslt *
rquotaproc_getquota_2(argp, clnt)
	ext_getquota_args *argp;
	CLIENT *clnt;
{
	static getquota_rslt res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, RQUOTAPROC_GETQUOTA, xdr_ext_getquota_args, argp, xdr_getquota_rslt, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


getquota_rslt *
rquotaproc_getactivequota_2(argp, clnt)
	ext_getquota_args *argp;
	CLIENT *clnt;
{
	static getquota_rslt res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, RQUOTAPROC_GETACTIVEQUOTA, xdr_ext_getquota_args, argp, xdr_getquota_rslt, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

