#include "config.h"
#ifdef ENABLE_NISEMU


#include <rpc/rpc.h>
#include "rpcsvc/nis.h"

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

nis_result *
nis_lookup_3(ns_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_LOOKUP, (xdrproc_t) xdr_ns_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_add_3(ns_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_ADD, (xdrproc_t) xdr_ns_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_modify_3(ns_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_MODIFY, (xdrproc_t) xdr_ns_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_remove_3(ns_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_REMOVE, (xdrproc_t) xdr_ns_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_iblist_3(ib_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_IBLIST, (xdrproc_t) xdr_ib_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_ibadd_3(ib_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_IBADD, (xdrproc_t) xdr_ib_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_ibmodify_3(ib_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_IBMODIFY, (xdrproc_t) xdr_ib_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_ibremove_3(ib_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_IBREMOVE, (xdrproc_t) xdr_ib_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_ibfirst_3(ib_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_IBFIRST, (xdrproc_t) xdr_ib_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_result *
nis_ibnext_3(ib_request *argp, CLIENT *clnt)
{
	static nis_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_IBNEXT, (xdrproc_t) xdr_ib_request, argp, (xdrproc_t) xdr_nis_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

fd_result *
nis_finddirectory_3(fd_args *argp, CLIENT *clnt)
{
	static fd_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_FINDDIRECTORY, (xdrproc_t) xdr_fd_args, argp, (xdrproc_t) xdr_fd_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_taglist *
nis_status_3(nis_taglist *argp, CLIENT *clnt)
{
	static nis_taglist res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_STATUS, (xdrproc_t) xdr_nis_taglist, argp, (xdrproc_t) xdr_nis_taglist, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

log_result *
nis_dumplog_3(dump_args *argp, CLIENT *clnt)
{
	static log_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_DUMPLOG, (xdrproc_t) xdr_dump_args, argp, (xdrproc_t) xdr_log_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

log_result *
nis_dump_3(dump_args *argp, CLIENT *clnt)
{
	static log_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_DUMP, (xdrproc_t) xdr_dump_args, argp, (xdrproc_t) xdr_log_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

bool_t *
nis_callback_3(netobj *argp, CLIENT *clnt)
{
	static bool_t res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_CALLBACK, (xdrproc_t) xdr_netobj, argp, (xdrproc_t) xdr_bool, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

u_long *
nis_cptime_3(nis_name *argp, CLIENT *clnt)
{
	static u_long res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_CPTIME, (xdrproc_t) xdr_nis_name, argp, (xdrproc_t) xdr_u_long, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

cp_result *
nis_checkpoint_3(nis_name *argp, CLIENT *clnt)
{
	static cp_result res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_CHECKPOINT, (xdrproc_t) xdr_nis_name, argp, (xdrproc_t) xdr_cp_result, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

void *
nis_ping_3(ping_args *argp, CLIENT *clnt)
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_PING, (xdrproc_t) xdr_ping_args, argp, (xdrproc_t) xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((void *)&res);
}

nis_taglist *
nis_servstate_3(nis_taglist *argp, CLIENT *clnt)
{
	static nis_taglist res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_SERVSTATE, (xdrproc_t) xdr_nis_taglist, argp, (xdrproc_t) xdr_nis_taglist, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_error *
nis_mkdir_3(nis_name *argp, CLIENT *clnt)
{
	static nis_error res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_MKDIR, (xdrproc_t) xdr_nis_name, argp, (xdrproc_t) xdr_nis_error, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_error *
nis_rmdir_3(nis_name *argp, CLIENT *clnt)
{
	static nis_error res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_RMDIR, (xdrproc_t) xdr_nis_name, argp, (xdrproc_t) xdr_nis_error, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

nis_error *
nis_updkeys_3(nis_name *argp, CLIENT *clnt)
{
	static nis_error res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, NIS_UPDKEYS, (xdrproc_t) xdr_nis_name, argp, (xdrproc_t) xdr_nis_error, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

#endif /* ENABLE_NISEMU */
