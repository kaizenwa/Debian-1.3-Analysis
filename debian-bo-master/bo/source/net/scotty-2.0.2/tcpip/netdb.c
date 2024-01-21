/*
 * netdb.c
 *
 * This file contains the source of the netdb command that provides
 * access to local network configuration information. Its mostly just
 * a wrapper around the C interface defined in netdb.h.
 *
 * Copyright (c) 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 */

#include "scotty.h"

#include <rpc/rpc.h>

/*
 * Some machines have no rpcent structure. Here is definition that
 * seems to work in this cases.
 */

#ifndef HAVE_RPCENT
struct rpcent {
        char    *r_name;        /* name of server for this rpc program */
        char    **r_aliases;    /* alias list */
        int     r_number;       /* rpc program number */
};
#endif

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
DumpHosts		_ANSI_ARGS_((Tcl_Interp *interp, char *dummy));

static int
LookupHostName		_ANSI_ARGS_((Tcl_Interp *interp, char *address));

static int
LookupHostAddress	_ANSI_ARGS_((Tcl_Interp *interp, char *name));

static int
DumpNetworks		_ANSI_ARGS_((Tcl_Interp *interp, char *dummy));

static int
LookupNetworkName	_ANSI_ARGS_((Tcl_Interp *interp, char *address));

static int
LookupNetworkAddress	_ANSI_ARGS_((Tcl_Interp *interp, char *name));

static int
DumpProtocols		_ANSI_ARGS_((Tcl_Interp *interp, char *dummy));

static int
LookupProtoName		_ANSI_ARGS_((Tcl_Interp *interp, char *number));

static int
LookupProtoNumber	_ANSI_ARGS_((Tcl_Interp *interp, char *name));

static int
DumpServices		_ANSI_ARGS_((Tcl_Interp *interp, char *dummy));

static int
LookupServiceName	_ANSI_ARGS_((Tcl_Interp *interp, char *service));

static int
LookupServiceNumber	_ANSI_ARGS_((Tcl_Interp *interp, char *name));

static int
DumpSunrpcs		_ANSI_ARGS_((Tcl_Interp *interp, char *dummy));

static int
LookupSunrpcName	_ANSI_ARGS_((Tcl_Interp *interp, char *number));

static int
LookupSunrpcNumber	_ANSI_ARGS_((Tcl_Interp *interp, char *name));

/*
 * Below are the defines for all known databases and for every 
 * query operation.
 */

#define NETDB_HOSTS	1
#define NETDB_NETWORKS	2
#define NETDB_PROTOCOLS	3
#define NETDB_SERVICES	4
#define NETDB_SUNRPCS	5

#define NETDB_DUMP	    1
#define NETDB_LOOKUP	    2
#define NETDB_LOOKUPNAME    3
#define NETDB_LOOKUPADDRESS 4
#define NETDB_LOOKUPNUMBER  5

/*
 * All queries are dispatched using the following table. 
 */

struct NetdbQuery {
    int db;
    int query;
    int (*fnx)(); /* (Tcl_Interp*, tkined_object*, int, char**) */
};

static struct NetdbQuery queryTable[] = {

    { NETDB_HOSTS,	NETDB_DUMP,		DumpHosts },
    { NETDB_NETWORKS,	NETDB_DUMP,		DumpNetworks },
    { NETDB_PROTOCOLS,	NETDB_DUMP,		DumpProtocols },
    { NETDB_SERVICES,	NETDB_DUMP,		DumpServices },
    { NETDB_SUNRPCS,	NETDB_DUMP,		DumpSunrpcs },

    { NETDB_HOSTS,	NETDB_LOOKUPNAME,	LookupHostName },
    { NETDB_HOSTS,	NETDB_LOOKUPADDRESS,	LookupHostAddress },

    { NETDB_NETWORKS,	NETDB_LOOKUPNAME,	LookupNetworkName },
    { NETDB_NETWORKS,	NETDB_LOOKUPADDRESS,	LookupNetworkAddress },

    { NETDB_PROTOCOLS,	NETDB_LOOKUPNAME,	LookupProtoName },
    { NETDB_PROTOCOLS,	NETDB_LOOKUPNUMBER,	LookupProtoNumber },

    { NETDB_SERVICES,	NETDB_LOOKUPNAME,	LookupServiceName },
    { NETDB_SERVICES,	NETDB_LOOKUPNUMBER,	LookupServiceNumber },

    { NETDB_SUNRPCS,	NETDB_LOOKUPNAME,	LookupSunrpcName },
    { NETDB_SUNRPCS,	NETDB_LOOKUPNUMBER,	LookupSunrpcNumber },

    { 0, 0, 0 }
};

static int
DumpHosts (interp, dummy)
     Tcl_Interp *interp;
     char *dummy;
{
#ifdef HAVE_GETHOSTENT
    struct hostent *host;
    unsigned long addr;
    struct in_addr *paddr;
    char buffer[20];
    int i = 0;

    sethostent(0);
    while ((host = gethostent())) {	
	Tcl_AppendResult(interp, i++ ? " {" : "{", host->h_name, 
			 (char *) NULL);
	if (*host->h_addr_list) {
	    paddr = (struct in_addr *) *host->h_addr_list++;
            addr = ntohl(paddr->s_addr);
            sprintf(buffer, "%lu.%lu.%lu.%lu",
		    (addr >> 24) & 0xff, (addr >> 16) & 0xff,
		    (addr >> 8) & 0xff, addr & 0xff);
	    Tcl_AppendResult(interp, " ", buffer, "}", (char *) NULL);
	}
    }
    endhostent();
#endif
    return TCL_OK;
}

static int
LookupHostName (interp, address)
     Tcl_Interp *interp;
     char *address;
{
    struct hostent *host;
    unsigned long addr;

    if ((addr = inet_addr(address)) == -1) {
      errorExit:
        Tcl_AppendResult(interp, "can not lookup \"", 
			 address, "\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (! (host = gethostbyaddr((char *) &addr, 4, AF_INET))) {
        goto errorExit;
    }
    Tcl_SetResult(interp, (char *) host->h_name, TCL_VOLATILE);
    return TCL_OK;
}

static int
LookupHostAddress (interp, name)
     Tcl_Interp *interp;
     char *name;
{
    struct hostent *host;
    unsigned long addr;
    struct in_addr *paddr;

    if ((host = gethostbyname(name)) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", 
			 name, "\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (*host->h_addr_list) {
	paddr = (struct in_addr *) *host->h_addr_list++;
	addr = ntohl(paddr->s_addr);
	sprintf(interp->result, "%lu.%lu.%lu.%lu",
		(addr >> 24) & 0xff, (addr >> 16) & 0xff,
		(addr >> 8) & 0xff, addr & 0xff);
    }
    return TCL_OK;
}

static int
DumpNetworks (interp, dummy)
     Tcl_Interp *interp;
     char *dummy;
{
#ifdef HAVE_GETNETENT
    struct netent *net;
    char buf[20];
    int i = 0;

    setnetent(0);
    while ((net = getnetent())) {

	while (net->n_net && ! ((net->n_net >> 24) & 0xff)) {
	    net->n_net <<= 8;
	}

	sprintf(buf, "%lu.%lu.%lu.%lu",
		(net->n_net >> 24) & 0xff, (net->n_net >> 16) & 0xff,
		(net->n_net >> 8) & 0xff, net->n_net & 0xff);

	Tcl_AppendResult(interp, i++ ? " {" : "{", net->n_name, 
			 " ", buf, "}", (char *) NULL);
    }
    endnetent();
#endif
    return TCL_OK;
}

static int
LookupNetworkName (interp, address)
     Tcl_Interp *interp;
     char *address;
{
    struct netent *net;
    unsigned long addr;

    if ((addr = inet_addr(address)) == -1) {
      errorExit:
        Tcl_AppendResult(interp, "can not lookup \"", 
			 address, "\"", (char *) NULL);
	return TCL_ERROR;
    }

    while (addr && ! (addr & 0xff)) {
	addr >>= 8;
    }

    if (! (net = getnetbyaddr(addr, AF_INET))) {
        goto errorExit;
    }
    Tcl_SetResult(interp, (char *) net->n_name, TCL_VOLATILE);
    return TCL_OK;
}

static int
LookupNetworkAddress (interp, name)
     Tcl_Interp *interp;
     char *name;
{
    struct netent *net;

    if ((net = getnetbyname(name)) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", name, "\"",
			 (char *) NULL);
	return TCL_ERROR;
    }

    while (net->n_net && ! ((net->n_net >> 24) & 0xff)) {
	net->n_net <<= 8;
    }

    sprintf(interp->result, "%lu.%lu.%lu.%lu",
	    (net->n_net >> 24) & 0xff, (net->n_net >> 16) & 0xff,
	    (net->n_net >> 8) & 0xff, net->n_net & 0xff);

    return TCL_OK;
}

static int
DumpProtocols (interp, dummy)
     Tcl_Interp *interp;
     char *dummy;
{
#ifdef HAVE_GETPROTOENT
    struct protoent *proto;
    char buf[20];
    int i = 0;

    setprotoent(0);
    while ((proto = getprotoent())) {	
	sprintf(buf, "%d", proto->p_proto);
	Tcl_AppendResult(interp, i++ ? " {" : "{", proto->p_name, 
			 " ", buf, "}", (char *) NULL);
    }
    endprotoent();
#endif
    return TCL_OK;
}

static int
LookupProtoName (interp, number)
     Tcl_Interp *interp;
     char *number;
{
    struct protoent *proto;
    int num;

    if (Tcl_GetInt(interp, number, &num) != TCL_OK) {
        return TCL_ERROR;
    }

    if ((proto = getprotobynumber(num)) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", number, "\"",
			 (char *) NULL);
	return TCL_ERROR;
    }

    Tcl_SetResult(interp, proto->p_name, TCL_STATIC);
    return TCL_OK;
}

static int
LookupProtoNumber (interp, name)
     Tcl_Interp *interp;
     char *name;
{
    struct protoent *proto;

    if ((proto = getprotobyname(name)) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", name, "\"",
			 (char *) NULL);
	return TCL_ERROR;
    }

    sprintf(interp->result, "%d", proto->p_proto);
    return TCL_OK;
}

static int
DumpServices (interp, dummy)
     Tcl_Interp *interp;
     char *dummy;
{
#ifdef HAVE_GETSERVENT
    struct servent *serv;
    char buf[20];
    int i = 0;

    setservent(0);
    while ((serv = getservent())) {	
	sprintf(buf, "%d", ntohs(serv->s_port));
	Tcl_AppendResult(interp, i++ ? " {" : "{", serv->s_name, 
			 " ", buf, " ", serv->s_proto, "}", (char *) NULL);
    }
    endservent();
#endif
    return TCL_OK;
}

static int
LookupServiceName (interp, service)
     Tcl_Interp *interp;
     char *service;
{
    struct servent *serv;
    int argc;
    char **argv;
    int port;

    if (Tcl_SplitList(interp, service, &argc, &argv) != TCL_OK) {
        return TCL_ERROR;
    }
    if (argc != 2) {
        Tcl_SetResult(interp, "service should contain number and protocol",
		      TCL_STATIC);
	goto errorExit;
    }
    if (Tcl_GetInt(interp, argv[0], &port) != TCL_OK) {
        goto errorExit;
    }

    if ((serv = getservbyport(htons(port), argv[1])) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", service, "\"",
			 (char *) NULL);
        goto errorExit;
    }

    Tcl_SetResult(interp, serv->s_name, TCL_STATIC);
    return TCL_OK;

  errorExit:
    ckfree((char *) argv);
    return TCL_ERROR;
}

static int
LookupServiceNumber (interp, name)
     Tcl_Interp *interp;
     char *name;
{
    struct servent *serv;
    int argc;
    char **argv;

    if (Tcl_SplitList(interp, name, &argc, &argv) != TCL_OK) {
        return TCL_ERROR;
    }
    if (argc != 2) {
        Tcl_SetResult(interp, "name should contain name and protocol",
		      TCL_STATIC);
	goto errorExit;
    }

    if ((serv = getservbyname(argv[0], argv[1])) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", name, "\"",
			 (char *) NULL);
        goto errorExit;
    }

    sprintf(interp->result, "%d", ntohs(serv->s_port));
    ckfree((char *) argv);
    return TCL_OK;

  errorExit:
    ckfree((char *) argv);
    return TCL_ERROR;
}

static int
DumpSunrpcs (interp, dummy)
     Tcl_Interp *interp;
     char *dummy;
{
#ifdef HAVE_GETRPCENT
    struct rpcent *rpc;
    char buf[20];
    int i = 0;

    setrpcent(0);
    while ((rpc = getrpcent())) {	
	sprintf(buf, "%d", rpc->r_number);
	Tcl_AppendResult(interp, i++ ? " {" : "{", rpc->r_name, 
			 " ", buf, "}", (char *) NULL);
    }
    endrpcent();
#endif
    return TCL_OK;
}

static int
LookupSunrpcName (interp, number)
     Tcl_Interp *interp;
     char *number;
{
    struct rpcent *rpc;
    int num;

    if (Tcl_GetInt(interp, number, &num) != TCL_OK) {
        return TCL_ERROR;
    }

    if ((rpc = getrpcbynumber(num)) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", number, "\"",
			 (char *) NULL);
	return TCL_ERROR;
    }

    Tcl_SetResult(interp, rpc->r_name, TCL_STATIC);
    return TCL_OK;
}

static int
LookupSunrpcNumber (interp, name)
     Tcl_Interp *interp;
     char *name;
{
    struct rpcent *rpc;

    if ((rpc = getrpcbyname(name)) == NULL) {
        Tcl_AppendResult(interp, "can not lookup \"", name, "\"",
			 (char *) NULL);
	return TCL_ERROR;
    }

    sprintf(interp->result, "%d", rpc->r_number);
    return TCL_OK;
}

/*
 * This is the netdb command as described in the scotty documentation.
 */

int
Scotty_NetdbCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int length;
    int query = 0;
    int db = 0;
    char *arg = NULL;
    struct NetdbQuery *queryPtr;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " db ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 2) {
	query = NETDB_DUMP;
    } else {
	query = NETDB_LOOKUP;
    }

    length = strlen(argv[1]);
    if (strncmp(argv[1], "hosts", length) == 0) {
	db = NETDB_HOSTS;
    } else if (strncmp(argv[1], "networks", length) == 0) {
	db = NETDB_NETWORKS;
    } else if (strncmp(argv[1], "protocols", length) == 0) {
	db = NETDB_PROTOCOLS;
    } else if (strncmp(argv[1], "services", length) == 0) {
	db = NETDB_SERVICES;
    } else if (strncmp(argv[1], "sunrpcs", length) == 0) {
	db = NETDB_SUNRPCS;
    } else {
        Tcl_AppendResult(interp, "bad database \"", argv[1], "\": should be ",
			 "hosts, networks, protocols, services, or sunrpcs",
			 (char *) NULL);
	return TCL_ERROR;
    }

    if ((query == NETDB_DUMP) && (argc != 2)) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", 
			 argv[0], " ", argv[1], (char *) NULL);
	return TCL_ERROR;
    }

    if (query == NETDB_LOOKUP) {

        if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " ", argv[1], " query arg\"", 
			     (char *) NULL);
	    return TCL_ERROR;
	}

	length = strlen(argv[2]);
	if (strncmp(argv[2], "name", length) == 0) {
	    query = NETDB_LOOKUPNAME;
	} else if (strncmp(argv[2], "address", length) == 0) {
	    query = NETDB_LOOKUPADDRESS;
	} else if (strncmp(argv[2], "number", length) == 0) {
	    query = NETDB_LOOKUPNUMBER;
	} else {
	    Tcl_AppendResult(interp, "bad query \"", argv[2], 
			     "\": should be name, address, or number",
			     (char *) NULL);
	    return TCL_ERROR;
	}

	arg = argv[3];
    }

    for (queryPtr = queryTable; queryPtr->fnx; queryPtr++) {
        if (queryPtr->db == db && queryPtr->query == query) {
	    return (queryPtr->fnx)(interp, arg);
	}
    }

    Tcl_SetResult(interp, "can not answer query", TCL_STATIC);
    return TCL_ERROR;
}
