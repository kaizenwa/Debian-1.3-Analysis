/*
** yp_if.c                Cover routines for YP (NIS)
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

#include "config.h"

#ifdef ENABLE_YPEMU


#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include <rpcsvc/yp.h>
#include "yp_conf.h"
#include "rpcsvc/ypclnt.h"
#include <rpcsvc/ypupdate_prot.h>

static int ypsetup_flag = 0;

void *ypall_data;
typedef int (*ypall_foreach_t)(int, char *, int, char *, int, void *);
ypall_foreach_t ypall_foreach;
void yp_prnick2real(FILE *);
char *yp_nick2real(char *);
int yp_bind(char *);
void yp_unbind(char *);
bool_t xdr_ypresp_all_seq(XDR *, u_long *);

static struct
{
    char *nickname;
    char *realname;
} transtab[] = {
    { "passwd",		"passwd.byname" },
    { "group",		"group.byname" },
    { "networks",	"networks.byaddr" },
    { "hosts",		"hosts.byname" },
    { "protocols",	"protocols.bynumber" },
    { "services",	"services.byname" },
    { "aliases",	"mail.aliases" },
    { "ethers",		"ethers.byname" },
	  
    { "rpc",		"rpc.bynumber" },
    { "netmasks",	"netmasks.byaddr" },
    { "publickey",	"publickey.byname" },
    { "netid",		"netid.byname" },
    { "passwd.adjunct", "passwd.adjunct.byname" },
    { "group.adjunct",	"group.adjunct.byname" },
    { "timezone", 	"timezone.byname" },
    
    { NULL, NULL }
};



static char *xstrdup(char *str, int len)
{
    char *cp;


    cp = malloc(len+2);
    memcpy(cp, str, len);
    cp[len++] = '\n';
    cp[len] = '\0';

    return cp;
}


int ypprot_err(int code)
{
    switch (code)
    {
      case YP_TRUE:
	return 0;

      case YP_NOMORE:
	return YPERR_NOMORE;
	
      case YP_FALSE:
	return YPERR_YPERR;

      case YP_NOMAP:
	return YPERR_MAP;

      case YP_NODOM:
	return YPERR_DOMAIN;

      case YP_NOKEY:
	return YPERR_KEY;

      case YP_BADOP:
	return YPERR_YPERR;

      case YP_BADDB:
	return YPERR_BADDB;

      case YP_YPERR:
	return YPERR_YPERR;

      case YP_BADARGS:
	return YPERR_BADARGS;

      case YP_VERS:
	return YPERR_VERS;
    }

    return -1;
}


char *yperr_string(int code)
{
    switch (code)
    {
      case 0:
	return "NIS operation succeeded";
	
      case YPERR_BADARGS:
	return "args to NIS function are bad";

      case YPERR_RPC:
	return "RPC failure on NIS operation";

      case YPERR_DOMAIN:
	return "can't bind to a server which serves domain";

      case YPERR_MAP:
	return "no such map in server's domain";

      case YPERR_KEY:
	return "no such key in map";

      case YPERR_YPERR:
	return "internal NIS server or client error";

      case YPERR_RESRC:
	return "local resource allocation failure";

      case YPERR_NOMORE:
	return "no more records in map database";

      case YPERR_PMAP:
	return "can't communicate with portmapper";

      case YPERR_YPBIND:
	return "can't communicate with ypbind";

      case YPERR_YPSERV:
	return "can't communicate with ypserv";

      case YPERR_NODOM:
	return "local domain name not set";

      case YPERR_BADDB:
	return "NIS map database is bad";

      case YPERR_VERS:
	return "NIS client/server version mismatch";

      case YPERR_ACCESS:
	return "permission denied";

      case YPERR_BUSY:
	return "database busy";

      default:
	return "unknown NIS client error code";
    }
}

static int yp_setup(void)
{
    if (ypsetup_flag)
	return 0;
    
    if (ypconf_set())
	return YPERR_NODOM;

    ypsetup_flag = 1;
    
    return 0;
}


static void *do_ypcall(const char *domain,
		       void * (*cfh)(void *req, CLIENT *clh),
		       void *req,
		       int *ecode)
{
    void *res;
    CLIENT *clh;
    char *address;
    int tries = 0;
    int const __yplib_maxtries = 5;

    again:
    if ((*ecode = yp_setup()) != 0)
	return NULL;

    clh = ypconf_domain2client(domain, NULL, &address);
    if (!clh)
    {
	*ecode = YPERR_DOMAIN;
	return NULL;
    }

    if ((res = (*cfh)(req, clh)) == NULL)
    {
	*ecode = YPERR_RPC;
	/* rebind in case of server reboot! */
	ypconf_unbinddomain(domain);
	if (tries++ < __yplib_maxtries) goto again;
	return NULL;
    }

    return res;
}


int
yp_get_default_domain(char **outdomain)
{
    int code;
    char buf[1025];
    char *cp;
    
    code = yp_setup();
    if (code!= 0)
	return code;

    /*
     * If you could set this for a setuid program, you could potentially
     * cause /bin/su to read a hacker-supplied password file with lots of 
     * extra uid 0 accounts. That would be bad.
     */
    cp = __libc_secure_getenv("YPDOMAIN");
    if (cp)
    {
	*outdomain = strdup(cp);
	return 0;
    }
    
    if (_yp_config->domainname)
    {
	*outdomain = strdup(_yp_config->domainname);
	return 0;
    }

    if (getdomainname(buf, sizeof(buf)) == 0)
    {
	*outdomain = strdup(buf);
	return 0;
    }

    return -1;
}


void
yp_prnick2real(FILE *fp)
{
    int i;

    
    fprintf(fp, "NIS map nickname translation table:\n");
    for (i = 0; transtab[i].nickname; i++)
	fprintf(fp, "\t\"%s\" -> \"%s\"\n",
		transtab[i].nickname,
		transtab[i].realname);
}


char
*yp_nick2real(char *mname)
{
    int i;


    for (i = 0; transtab[i].nickname; i++)
	if (strcmp(transtab[i].nickname, mname) == 0)
	    return transtab[i].realname;

    return mname;
}


int yp_match(const char *indomain,
	     const char *inmap,
	     const char *inkey,
	     int inkeylen,
	     char **outval,
	     int *outvallen)
{
    ypreq_key ypreq;
    ypresp_val *ypres;
    int code;

    if (NULL == inkey || 0 == inkeylen || '\0' == *inkey ||
	NULL == inmap || '\0' == *inmap ||
	NULL == indomain || '\0' == *indomain)
        return YPERR_BADARGS;

    ypreq.domain = (char *) indomain;
    ypreq.map = (char *) inmap;
    ypreq.key.keydat_val = (char *) inkey;
    ypreq.key.keydat_len = inkeylen;

    ypres = (ypresp_val *) do_ypcall(indomain,
				     (void * (*)(void *req, CLIENT *clh))
				     &ypproc_match_2, &ypreq,
				     &code);

    if (ypres == NULL)
	return code;
    
    if (ypres->stat != YP_TRUE)
	return ypprot_err(ypres->stat);

    *outval = xstrdup(ypres->val.valdat_val, ypres->val.valdat_len);
    *outvallen = ypres->val.valdat_len;

    xdr_free((xdrproc_t)xdr_ypresp_val, (char *) ypres);
    
    return 0;
}



int yp_first(const char *indomain,
	     const char *inmap,
	     char **outkey,
	     int *outkeylen,
	     char **outval,
	     int *outvallen)
{
    ypreq_key ypreq;
    ypresp_key_val *ypres;
    int code;
    

    if (NULL == indomain || '\0' == *indomain ||
	NULL == inmap || '\0' == *inmap)
        return YPERR_BADARGS;

    ypreq.domain = (char *) indomain;
    ypreq.map = (char *) inmap;
    ypreq.key.keydat_val = "";
    ypreq.key.keydat_len = 0;

    ypres = (ypresp_key_val *) do_ypcall(indomain,
					 (void * (*)(void *req, CLIENT *clh))
					 &ypproc_first_2, &ypreq,
					 &code);
    if (ypres == NULL)
	return code;

    if (ypres->stat != YP_TRUE)
	return ypprot_err(ypres->stat);

/* XXX What's going on here??? outval and outkey arrive in the wrong order! */
#ifdef HAVE_BUGGY_YP_X
    *outval = xstrdup(ypres->key.keydat_val, ypres->key.keydat_len);
    *outvallen = ypres->key.keydat_len;
    
    *outkey = xstrdup(ypres->val.valdat_val, ypres->val.valdat_len);
    *outkeylen = ypres->val.valdat_len;
#else
    *outkey = xstrdup(ypres->key.keydat_val, ypres->key.keydat_len);
    *outkeylen = ypres->key.keydat_len;
    
    *outval = xstrdup(ypres->val.valdat_val, ypres->val.valdat_len);
    *outvallen = ypres->val.valdat_len;
#endif
    
    xdr_free((xdrproc_t) xdr_ypresp_key_val, (char *) ypres);

    return 0;
}


int yp_next(const char *indomain,
	    const char *inmap,
	    const char *inkey,
	    int inkeylen,
	    char **outkey,
	    int *outkeylen,
	    char **outval,
	    int *outvallen)
{
    ypreq_key ypreq;
    ypresp_key_val *ypres;
    int code;
    
    if (NULL == indomain || '\0' == *indomain ||
	NULL == inmap || '\0' == *inmap ||
	NULL == inkey || 0 == inkeylen || '\0' == *inkey)
        return YPERR_BADARGS;


    ypreq.domain = (char *) indomain;
    ypreq.map = (char *) inmap;
    ypreq.key.keydat_val = (char *) inkey;
    ypreq.key.keydat_len = inkeylen;

    ypres = (ypresp_key_val *) do_ypcall(indomain,
					 (void * (*)(void *req, CLIENT *clh))
					 &ypproc_next_2, &ypreq,
					 &code);
    if (ypres == NULL)
	return code;
    
    if (ypres->stat != YP_TRUE)
	return ypprot_err(ypres->stat);

/* What's going on here??? outval and outkey arrive in the wrong order! */
#ifdef HAVE_BUGGY_YP_X
    *outval = xstrdup(ypres->key.keydat_val, ypres->key.keydat_len);
    *outvallen = ypres->key.keydat_len;
    
    *outkey = xstrdup(ypres->val.valdat_val, ypres->val.valdat_len);
    *outkeylen = ypres->val.valdat_len;
#else
    *outkey = xstrdup(ypres->key.keydat_val, ypres->key.keydat_len);
    *outkeylen = ypres->key.keydat_len;
    
    *outval = xstrdup(ypres->val.valdat_val, ypres->val.valdat_len);
    *outvallen = ypres->val.valdat_len;
#endif
    
    xdr_free((xdrproc_t) xdr_ypresp_key_val, (char *) ypres);

    return 0;
}



int yp_bind(char *indomain)
{
    CLIENT *clh;
    char *address;
    int ecode;

    if ((ecode = yp_setup()) != 0)
	return ecode;

    clh = ypconf_domain2client(indomain, NULL, &address);
    if (clh)
	return 0;
    else
	return YPERR_DOMAIN;
}

void yp_unbind(char *indomain)
{
    if (yp_setup())
	return;

    if (ypconf_unbinddomain(indomain) < 0)
	return;
}


bool_t
xdr_ypresp_all_seq(XDR *xdrs, u_long *objp)
{
  struct ypresp_all response;
  char *inkey, *inval;
  int inkeylen, invallen;
  int result;
  u_long status;
  
  bzero(&response, sizeof(struct ypresp_all));
  *objp = YP_FALSE;
  for (;;)
    {
      if (!xdr_ypresp_all(xdrs, &response))
	{
	  xdr_free((xdrproc_t) xdr_ypresp_all, (char *)&response);
	  *objp = YP_YPERR;
	  return (FALSE);
	}
      if (0 == response.more)
	{
	  xdr_free((xdrproc_t) xdr_ypresp_all, (char *)&response);
	  return (FALSE);
	}
      status = response.ypresp_all_u.val.stat;
      switch (status)
	{
	 case YP_TRUE:
	  inkeylen = response.ypresp_all_u.val.key.keydat_len;
	  invallen = response.ypresp_all_u.val.val.valdat_len;
	  inkey = malloc(inkeylen + 1);
	  memcpy(inkey, response.ypresp_all_u.val.key.keydat_val, inkeylen);
	  inkey[inkeylen] = '\0';
	  inval = malloc(invallen + 1);
	  memcpy(inval, response.ypresp_all_u.val.val.valdat_val, invallen);
	  inval[invallen] = '\0';
	  xdr_free((xdrproc_t) xdr_ypresp_all, (char *)&response);
	  result = (*ypall_foreach)(status, 
				    inkey, inkeylen, 
				    inval, invallen,
				    ypall_data);
	  *objp = status;
	  free(inkey);
	  free(inval);
	  if (result)
	    return TRUE;
	  break;
	 case YP_NOMORE:
	  xdr_free ((xdrproc_t) xdr_ypresp_all, (char *)&result);
	  return TRUE;
	  break;
	 default:
	  xdr_free ((xdrproc_t) xdr_ypresp_all, (char *)&result);
	  *objp = status;
	  return TRUE;
	}
    }
}



int yp_all(char *indomain,
	   char *inmap,
	   struct ypall_callback *incbp)
{
  struct ypreq_nokey yprnk;
  u_long status;
  struct timeval timeout;
  CLIENT *clnt_handle;
  char *address;

  if (NULL == indomain || '\0' == *indomain ||
      NULL == inmap || '\0' == *inmap)
    return YPERR_BADARGS;

  if (0 != yp_setup())
    return 1;

  address = ypconf_domain2address(indomain, NULL);
  if (NULL == address)
    return YPERR_DOMAIN;

  clnt_handle = clnt_create(address, YPPROG, YPVERS, "tcp");
  if (NULL == clnt_handle)
    return YPERR_RPC;

  yprnk.domain = indomain;
  yprnk.map = inmap;
  ypall_foreach = (ypall_foreach_t)incbp->foreach;
  ypall_data = incbp->data;
  timeout.tv_sec = 5;
  timeout.tv_usec = 0;
  
  (void) clnt_call(clnt_handle, YPPROC_ALL,
		   (xdrproc_t) xdr_ypreq_nokey, &yprnk,
		   (xdrproc_t) xdr_ypresp_all_seq, &status,
		   timeout);
  
  clnt_destroy(clnt_handle);
  
  return (status == YP_FALSE ? 0 : ypprot_err(status));
}

int yp_order(char *indomain,
	     char *inmap,
	     int *outorder)
{
    ypreq_nokey ypreq;
    ypresp_order *ypres;
    int code;
    
    if (NULL == indomain || '\0' == *indomain ||
	NULL == inmap || '\0' == *inmap)
        return YPERR_BADARGS;

    ypreq.domain = (char *) indomain;
    ypreq.map = (char *) inmap;

    ypres = (ypresp_order *) do_ypcall(indomain,
				       (void * (*)(void *req, CLIENT *clh))
				       &ypproc_order_2, &ypreq,
				       &code);
    if (ypres == NULL)
	return code;
    
    if (ypres->stat != YP_TRUE)
	return ypprot_err(ypres->stat);
    
    *outorder = ypres->ordernum;
    
    xdr_free((xdrproc_t) xdr_ypresp_order, (char *) ypres);
    
    return 0;
}

int yp_master(char *indomain,
	      char *inmap,
	      char **outname)
{
    ypreq_nokey ypreq;
    ypresp_master *ypres;
    int code;
    
    if (NULL == indomain || '\0' == *indomain ||
	NULL == inmap || '\0' == *inmap)
        return YPERR_BADARGS;

    ypreq.domain = (char *) indomain;
    ypreq.map = (char *) inmap;

    ypres = (ypresp_master *) do_ypcall(indomain,
					(void * (*)(void *req, CLIENT *clh))
					&ypproc_master_2, &ypreq,
					&code);
    if (ypres == NULL)
	return code;
    
    if (ypres->stat != YP_TRUE)
	return ypprot_err(ypres->stat);
    
    *outname = strdup(ypres->peer);

    xdr_free((xdrproc_t) xdr_ypresp_master, (char *) ypres);

    return 0;
}

int yp_maplist(char *indomain,
	      ypmaplist **outname)
{
    ypresp_maplist *ypres;
    int code;
    
    if (NULL == indomain || '\0' == *indomain)
        return YPERR_BADARGS;

    ypres = (ypresp_maplist *) do_ypcall(indomain,
					(void * (*)(void *req, CLIENT *clh))
					&ypproc_maplist_2, indomain,
					&code);
    if (ypres == NULL)
	return code;
    
    if (ypres->stat != YP_TRUE)
	return ypprot_err(ypres->stat);
    
    *outname = ypres->maps;

    return 0;
}


#define WINDOW 60

int yp_update(char *domain, char *map, unsigned ypop, 
          char *key, int keylen, char *data, int datalen)
{
  static struct timeval TIMEOUT = { 25, 0 };
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

#endif /* ENABLE_YPEMU */
