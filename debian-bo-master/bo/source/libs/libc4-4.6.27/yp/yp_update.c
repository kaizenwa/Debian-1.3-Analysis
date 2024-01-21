#include <rpc/rpc.h>
#include <rpcsvc/ypclnt.h>
#include <rpcsvc/ypupdate_prot.h>


#define WINDOW 60

static struct timeval TIMEOUT = { 25, 0 };

	
int
yp_update(char *domain, char *map, unsigned ypop, 
	  char *key, int keylen, char *data, int datalen)
{
    union {
	ypupdate_args update_args;
	ypdelete_args delete_args;
    } args;
    xdrproc_t xdr_argument;
    unsigned res = 0;
    CLIENT *clnt;
    char *master;
    struct sockaddr saddr;
    char servername[MAXNETNAMELEN + 1];
    int r;
  
    if (!domain || !map || !key || (ypop != YPOP_DELETE && !data))
      return YPERR_BADARGS;

    args.update_args.mapname = map;
    args.update_args.key.yp_buf_len = keylen; 
    args.update_args.key.yp_buf_val = key;
    args.update_args.datum.yp_buf_len = datalen;
    args.update_args.datum.yp_buf_val = data;

    if ((r = yp_master(domain, map, &master)) != 0)
      return r;

    if (!host2netname(servername, master, domain)) {
	fprintf(stderr, "yp_update: cannot convert host to netname\n");
	return YPERR_YPERR;
    }
    if ((clnt = clnt_create(master, YPU_PROG, YPU_VERS, "tcp")) == NULL) {
	clnt_pcreateerror ("yp_update: clnt_create");
	return YPERR_RPC;
    }
    if (!clnt_control(clnt, CLGET_SERVER_ADDR, (char *)&saddr)) {
	fprintf(stderr, "yp_update: cannot get server address\n");
	return YPERR_RPC;
    }
    switch (ypop) {
      case YPOP_CHANGE:
      case YPOP_INSERT:
      case YPOP_STORE:
	xdr_argument = (xdrproc_t) xdr_ypupdate_args;
	break;

      case YPOP_DELETE:
	xdr_argument = (xdrproc_t) xdr_ypdelete_args;
	break;

      default:
	return YPERR_BADARGS;
	break;
    }
    clnt->cl_auth = authdes_create(servername, WINDOW, &saddr, NULL);

    if (clnt->cl_auth == NULL)
      clnt->cl_auth = authunix_create_default();

  again:    
    r = clnt_call (clnt, ypop, xdr_argument, &args,
		   xdr_u_int, &res, TIMEOUT);

    if (r == RPC_AUTHERROR) {

	if (clnt->cl_auth->ah_cred.oa_flavor == AUTH_DES) { 
	    clnt->cl_auth = authunix_create_default();
	    goto again;
	} else
	  return YPERR_ACCESS;
    }
    if (r != RPC_SUCCESS) {
	clnt_perror(clnt, "yp_update: clnt_call");
	return YPERR_RPC;
    }
    return res;
}
