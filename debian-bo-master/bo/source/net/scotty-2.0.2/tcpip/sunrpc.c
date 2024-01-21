/*
 * sunrpc.c
 *
 * Extend a tcl command interpreter with some sunrpc commands.
 *
 * Copyright (c) 1993, 1994, 1995
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
 */

#include "scotty.h"

#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>

#include "rstat.h"

/*
 * Some machines have incompatible definitions for h_addr. This
 * fix was suggested by Erik Schoenfelder.
 */

#undef h_addr
#include "ether.h"
#define h_addr h_addr_list[0]

#include "mount.h"

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

static char str[1024];

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
SunrpcCreateError	_ANSI_ARGS_((Tcl_Interp *interp));

static void
SunrpcError		_ANSI_ARGS_((Tcl_Interp *interp, int res));

static char*
SunrpcGetHostname	_ANSI_ARGS_((Tcl_Interp *interp,  char *str));

static int
SunrpcGetAddr		_ANSI_ARGS_((Tcl_Interp *interp, char *host,
				     struct sockaddr_in *addr));
static int 
SunrpcOpenEtherd	_ANSI_ARGS_((Tcl_Interp *interp, char *host));

static int
SunrpcCloseEtherd	_ANSI_ARGS_((Tcl_Interp *interp, char *host));

static int
SunrpcEtherd		_ANSI_ARGS_((Tcl_Interp *interp, char *host));

static int
SunrpcRstat		_ANSI_ARGS_((Tcl_Interp *interp, char *host));

static int
SunrpcInfo		_ANSI_ARGS_((Tcl_Interp *interp, char *host));

static int
SunrpcMount		_ANSI_ARGS_((Tcl_Interp *interp, char *host));

static int
SunrpcExports		_ANSI_ARGS_((Tcl_Interp *interp, char *host));

static int
SunrpcProbe		_ANSI_ARGS_((Tcl_Interp *interp, char *host, 
				     unsigned long prognum,
				     unsigned long version, 
				     unsigned protocol));

/*
 * Set an error result string into the Tcl interpreter. These functions
 * clean up the standard Sun error messages so that we get rid of the
 * RPC: prefix.
 */

static void
SunrpcCreateError (interp)
     Tcl_Interp *interp;
{
    char *p = clnt_spcreateerror ("");
    if (strncmp (p, ": RPC: ", 7) == 0) p += 7;
    if (isspace (p[strlen(p)-1])) p[strlen(p)-1] = '\0';
    Tcl_SetResult (interp, p, TCL_STATIC);
}

static void
SunrpcError (interp, res)
     Tcl_Interp *interp;
     int res;
{
    char *p = clnt_sperrno(res);
    if (strncmp(p, "RPC: ", 5) == 0) p += 5;
    Tcl_SetResult(interp, p, TCL_STATIC);
}

/*
 * Make sure that str points to a hostname and not to an ip address.
 * We return the host name in static memory so the caller need not
 * worry about freeing the host name.
 */

static char* 
SunrpcGetHostname (interp, str)
    Tcl_Interp *interp;
    char *str;
{
    static char *host = NULL;
    struct hostent *hostent;
    int hostaddr;

    hostent = gethostbyname (str);
    if (hostent != NULL) {
	if (host) ckfree(host);
	host = ckstrdup (str);
	return host;
    }
    
    /*
     * this may be an IP address in numerical form 
     */
    
    if ((hostaddr = inet_addr(str)) == -1) {
        Tcl_AppendResult (interp, "unknown host name ", str, (char *) NULL);
        return NULL;    
    }
    hostent = gethostbyaddr ((char *) &hostaddr, sizeof(hostaddr), AF_INET);
    if (hostent == NULL) {
        Tcl_AppendResult (interp, "unknown host name ", str, (char *) NULL);
        return NULL;
    }
    
    if (host) ckfree(host);
    host = ckstrdup ((char *) hostent->h_name);
    return host;
}

/*
 * Initialize the sockaddr structure needed for some RPC calls.
 */

static int 
SunrpcGetAddr (interp, host, addr)
    Tcl_Interp *interp;
    struct sockaddr_in *addr;
    char* host;
{
    struct hostent *hp;

    if ((host = SunrpcGetHostname (interp, host)) == NULL) {
        return TCL_ERROR;
    }

    addr->sin_family = AF_INET;
    addr->sin_port = 0;
    addr->sin_addr.s_addr = INADDR_ANY;

    if ((hp = gethostbyname (host)) == NULL) {
	Tcl_AppendResult (interp, "unknown host name ", host, (char *) NULL);
	return TCL_ERROR;
    }
    addr->sin_addr.s_addr = * (unsigned long *) hp->h_addr;

    return TCL_OK;
}

/*
 * Get some network monitoring information from suns etherd.
 */

struct ether_handle {
    char   *name;
    CLIENT *clnt;
    etherstat estat;
    struct ether_handle *next;
};
typedef struct ether_handle ether_handle;

static ether_handle *eh_list = NULL;

static int 
SunrpcOpenEtherd (interp, host)
    Tcl_Interp *interp;
    char *host;
{
    int dummy;
    ether_handle *p;
    etherstat *res;
    struct sockaddr_in _addr;
    struct sockaddr_in *addr = &_addr;
    struct timeval timeout;
    int socket = RPC_ANYSOCK;

    timeout.tv_sec = 5; timeout.tv_usec = 0;

    if ((host = SunrpcGetHostname (interp, host)) == NULL) {
        return TCL_ERROR;
    }

    if (SunrpcGetAddr(interp, host, addr) != TCL_OK) {
        return TCL_ERROR;
    }

    for (p = eh_list; p != NULL; p = p->next) {
	if (!strcmp (host, p->name)) {
	    return TCL_OK;
	}
    }

    p = (ether_handle *) ckalloc (sizeof(ether_handle));

    p->clnt = clntudp_create(addr, ETHERPROG, ETHERVERS, timeout, &socket);
    if (p->clnt == NULL) {
	Tcl_AppendResult (interp, "can not connect to ", host, (char *) NULL);
        return TCL_ERROR;
    }

    etherproc_on_1 (&dummy, p->clnt);

    res = etherproc_getdata_1 (&dummy, p->clnt);
    if (res == NULL) {
	Tcl_AppendResult (interp, "can not connect to ", host, (char *) NULL);
	return TCL_ERROR;
    }
    p->estat = *res;
    p->name = ckstrdup (host);
    p->next = eh_list;
    eh_list = p;
    
    return TCL_OK;
}

static int 
SunrpcCloseEtherd (interp, host)
    Tcl_Interp *interp;
    char *host;
{
    int dummy;
    ether_handle *p;
    ether_handle *q = NULL;

    if ((host = SunrpcGetHostname (interp, host)) == NULL) {
        return TCL_ERROR;
    }

    for (p = eh_list; p != NULL; p = p->next) {
        if (strcmp (host, p->name) == 0) {
	    etherproc_off_1 (&dummy, p->clnt);
	    if (q == NULL) {
		eh_list = p->next;
	    } else {
		q->next = p->next;
	    }
	    ckfree (p->name);
	    ckfree ((char *) p);
        }
	q = p;
    }
    
    return TCL_OK;
}

static int
SunrpcEtherd (interp, host)
    Tcl_Interp *interp;
    char *host;
{
    int dummy, tdiff, i;
    ether_handle *p;
    etherstat *res;

    if ((host = SunrpcGetHostname (interp, host)) == NULL) {
        return TCL_ERROR;
    }

    for (p = eh_list; p != NULL; p = p->next) {
	if (strcmp (host, p->name) == 0) {
	    res = etherproc_getdata_1 (&dummy, p->clnt);
	    if (res == NULL) {
		Tcl_AppendResult (interp, "can not connect to ", host,
				  (char *) NULL);
		return TCL_ERROR;
	    }
	    tdiff = res->e_time.tv_useconds>p->estat.e_time.tv_useconds ?
		    (res->e_time.tv_useconds - p->estat.e_time.tv_useconds) :
	            (1000000 - p->estat.e_time.tv_useconds - res->e_time.tv_useconds);
	    tdiff = tdiff/1000 + 1000 * 
		    (res->e_time.tv_seconds - p->estat.e_time.tv_seconds);

	    sprintf (str,"time TimeTicks %u", tdiff);
	    Tcl_AppendElement (interp, str);
	    sprintf (str, "bytes Gauge %u", res->e_bytes - p->estat.e_bytes);
	    Tcl_AppendElement (interp, str);
	    sprintf (str, "packets Gauge %u", 
		     res->e_packets - p->estat.e_packets);
	    Tcl_AppendElement (interp, str);
	    sprintf (str, "bcast Gauge %u", res->e_bcast - p->estat.e_bcast);
	    Tcl_AppendElement (interp, str);

#define SUNRPC_PROTO_ND		0
#define SUNRPC_PROTO_ICMP	1
#define SUNRPC_PROTO_UDP	2
#define SUNRPC_PROTO_TCP	3
#define SUNRPC_PROTO_ARP	4
#define SUNRPC_PROTO_OTHER	5

	    sprintf (str, "nd Gauge %u", 
		     res->e_proto[SUNRPC_PROTO_ND] 
		     - p->estat.e_proto[SUNRPC_PROTO_ND]);
	    Tcl_AppendElement (interp, str);

	    sprintf (str, "icmp Gauge %u", 
		     res->e_proto[SUNRPC_PROTO_ICMP] 
		     - p->estat.e_proto[SUNRPC_PROTO_ICMP]);
	    Tcl_AppendElement (interp, str);

	    sprintf (str, "udp Gauge %u", 
		     res->e_proto[SUNRPC_PROTO_UDP] 
		     - p->estat.e_proto[SUNRPC_PROTO_UDP]);
	    Tcl_AppendElement (interp, str);

	    sprintf (str, "tcp Gauge %u", 
		     res->e_proto[SUNRPC_PROTO_TCP] 
		     - p->estat.e_proto[SUNRPC_PROTO_TCP]);
	    Tcl_AppendElement (interp, str);

	    sprintf (str, "arp Gauge %u", 
		     res->e_proto[SUNRPC_PROTO_ARP] 
		     - p->estat.e_proto[SUNRPC_PROTO_ARP]);
	    Tcl_AppendElement (interp, str);

	    sprintf (str, "other Gauge %u", 
		     res->e_proto[SUNRPC_PROTO_OTHER] 
		     - p->estat.e_proto[SUNRPC_PROTO_OTHER]);
	    Tcl_AppendElement (interp, str);

#define MINPACKETLEN 60
#define MAXPACKETLEN 1514
#define BUCKETLNTH ((MAXPACKETLEN - MINPACKETLEN + NBUCKETS - 1)/NBUCKETS)

	    for (i = 0; i < NBUCKETS; i++) {
		sprintf (str, "%d-%d Gauge %u", 
			 MINPACKETLEN + i * BUCKETLNTH,
			 MINPACKETLEN + (i + 1) * BUCKETLNTH - 1,
			 res->e_size[i] - p->estat.e_size[i]);
		Tcl_AppendElement (interp, str);
	    }
	    
	    p->estat = *res;
	    return TCL_OK;
	}
    }

    Tcl_AppendResult (interp, "no connection to ", host, (char*) NULL);
    return TCL_ERROR;
}

/* 
 * Get some information about the status of a host using the rstat RPC.
 * The result is a list of name type value triples as returned by the
 * snmp interface.
 */

static int
SunrpcRstat (interp, host)
    Tcl_Interp *interp;
    char *host;
{
    int res, dummy;
    struct statstime statp;
    
    if ((host = SunrpcGetHostname (interp, host)) == NULL) {
        return TCL_ERROR;
    }
    
    res = callrpc (host, RSTATPROG, RSTATVERS_TIME, RSTATPROC_STATS,
		   xdr_void, (char *) &dummy,
		   xdr_statstime, (char *) &statp);

    if (res != 0) {
	SunrpcError (interp, res);
	return TCL_ERROR;
    }

    sprintf (str,"cp_user Counter %d", statp.cp_time[0]);
    Tcl_AppendElement (interp, str);
    sprintf (str,"cp_nice Counter %d", statp.cp_time[1]);
    Tcl_AppendElement (interp, str);
    sprintf (str, "cp_system Counter %d", statp.cp_time[2]);
    Tcl_AppendElement (interp, str);
    sprintf (str, "cp_idle Counter %d", statp.cp_time[3]);
    Tcl_AppendElement (interp, str);

    sprintf (str, "dk_xfer_0 Counter %d", statp.dk_xfer[0]);
    Tcl_AppendElement (interp, str);
    sprintf (str, "dk_xfer_1 Counter %d", statp.dk_xfer[1]);
    Tcl_AppendElement (interp, str);
    sprintf (str, "dk_xfer_2 Counter %d", statp.dk_xfer[2]);
    Tcl_AppendElement (interp, str);
    sprintf (str, "dk_xfer_3 Counter %d", statp.dk_xfer[3]);
    Tcl_AppendElement (interp, str);

    sprintf (str, "v_pgpgin Counter %d", statp.v_pgpgin);
    Tcl_AppendElement (interp, str);
    sprintf (str, "v_pgpgout Counter %d", statp.v_pgpgout);
    Tcl_AppendElement (interp, str);
    sprintf (str, "v_pswpin Counter %d", statp.v_pswpin);
    Tcl_AppendElement (interp, str);
    sprintf (str, "v_pswpout Counter %d", statp.v_pswpout);
    Tcl_AppendElement (interp, str);

    sprintf (str, "v_intr Counter %d", statp.v_intr);
    Tcl_AppendElement (interp, str);
    sprintf (str, "v_swtch Counter %d", statp.v_swtch);
    Tcl_AppendElement (interp, str);

    sprintf (str, "if_ipackets Counter %d", statp.if_ipackets);
    Tcl_AppendElement (interp, str);
    sprintf (str, "if_ierrors Counter %d", statp.if_ierrors);
    Tcl_AppendElement (interp, str);
    sprintf (str, "if_opackets Counter %d", statp.if_opackets);
    Tcl_AppendElement (interp, str);
    sprintf (str, "if_oerrors Counter %d", statp.if_oerrors);
    Tcl_AppendElement (interp, str);

    sprintf (str, "avenrun_0 Gauge %d", statp.avenrun[0]);
    Tcl_AppendElement (interp, str);
    sprintf (str, "avenrun_1 Gauge %d", statp.avenrun[1]);
    Tcl_AppendElement (interp, str);
    sprintf (str, "avenrun_2 Gauge %d", statp.avenrun[2]);
    Tcl_AppendElement (interp, str);

    sprintf (str, "boottime TimeTicks %d", statp.boottime.tv_sec);
    Tcl_AppendElement (interp, str);
    sprintf (str, "curtime TimeTicks %d", statp.curtime.tv_sec);
    Tcl_AppendElement (interp, str);

    return TCL_OK;
}

/*
 * Ask the portmapper about registered RPC services.
 */

static int
SunrpcInfo (interp, host)
    Tcl_Interp *interp;
    char *host;
{
    struct sockaddr_in _addr;
    struct sockaddr_in *addr = &_addr;
    struct pmaplist *portmapperlist;

    if (SunrpcGetAddr(interp, host, addr) != TCL_OK) {
        return TCL_ERROR;
    }

    portmapperlist = pmap_getmaps(addr);
    while (portmapperlist) {
	int prog = portmapperlist->pml_map.pm_prog;
	struct rpcent *re = getrpcbynumber(prog);
	sprintf (str, "%lu %2lu %s %5lu %s",
		 portmapperlist->pml_map.pm_prog,
		 portmapperlist->pml_map.pm_vers,
		 portmapperlist->pml_map.pm_prot==IPPROTO_UDP ? "udp" : "tcp",
		 portmapperlist->pml_map.pm_port,
		 (re!=NULL) ? re->r_name : "(unknown)" );
	Tcl_AppendElement(interp, str);
	portmapperlist = portmapperlist->pml_next;
    }
    return TCL_OK;
}

/*
 * Ask the mount daemon about mounted filesystems.
 */

static int
SunrpcMount (interp, host)
    Tcl_Interp *interp;
    char *host;
{
    mountlist ml = NULL;
    struct timeval timeout;
    enum clnt_stat res;
    CLIENT *clnt;
    struct sockaddr_in _addr;
    struct sockaddr_in *addr = &_addr;
    int socket = RPC_ANYSOCK;

    timeout.tv_sec = 5; timeout.tv_usec = 0;

    if (SunrpcGetAddr(interp, host, addr) != TCL_OK) {
	return TCL_ERROR;
    }

    clnt = clnttcp_create(addr, MOUNTPROG, MOUNTVERS, &socket, 0, 0);
    if (! clnt) {
	SunrpcCreateError(interp);
	return TCL_ERROR;
    }
	
    res = clnt_call(clnt, MOUNTPROC_DUMP, xdr_void, (char*) 0, 
		    xdr_mountlist, (char *) &ml, timeout);
    clnt_destroy(clnt);
    if (res != RPC_SUCCESS) {
	SunrpcError (interp, res);
	return TCL_ERROR;
    }

    while (ml) {
	sprintf (str, "%s %s", ml->ml_directory, ml->ml_hostname);
	Tcl_AppendElement (interp, str);
	ml = ml->ml_next;
    }

    return TCL_OK;
}

/*
 * Ask the mount daemon about exported filesystems.
 */

static int
SunrpcExports (interp, host)
    Tcl_Interp *interp;
    char *host;
{
    exports ex = NULL;
    groups gr;
    struct timeval timeout;
    enum clnt_stat res;
    CLIENT *clnt;
    char *grstr;
    char buf[512];
    int size = 0;
    struct sockaddr_in _addr;
    struct sockaddr_in *addr = &_addr;
    int socket = RPC_ANYSOCK;

    timeout.tv_sec = 5; timeout.tv_usec = 0;

    if (SunrpcGetAddr(interp, host, addr) != TCL_OK) {
	return TCL_ERROR;
    }

    clnt = clnttcp_create(addr, MOUNTPROG, MOUNTVERS, &socket, 0, 0);
    if (! clnt) {
	SunrpcCreateError(interp);
	return TCL_ERROR;
    }
	
    res = clnt_call(clnt, MOUNTPROC_EXPORT, xdr_void, (char*) 0,
		    xdr_exports, (char *) &ex, timeout);
    clnt_destroy(clnt);
    if (res != RPC_SUCCESS) {
	SunrpcError (interp, res);
	return TCL_ERROR;
    }

    while (ex) {
	size = 1;
	gr = ex->ex_groups;
	while (gr) {
	    size += strlen (gr->gr_name);
	    size++;
	    gr = gr->gr_next;
	}
	gr = ex->ex_groups;
	grstr = (char *) ckalloc(size);
	strcpy (grstr, "");
	while (gr) {
	    sprintf (buf, "%s ", gr->gr_name);
	    strcat (grstr, buf);
	    gr = gr->gr_next;
	}
	sprintf (str, "%s {%s}",
		 ex->ex_dir ? ex->ex_dir : "\"\"",
		 ex->ex_groups ? grstr : "");
	Tcl_AppendElement (interp, str);
	ckfree (grstr);
	ex = ex->ex_next;
    }

    return TCL_OK;
}

/*
 * SunrpcProbe() probes a registered RPC service by calling 
 * procedure 0. This should work with every decent RPC service.
 */

static int
SunrpcProbe (interp, host, prognum, version, protocol)
    Tcl_Interp *interp;
    char *host;
    unsigned long prognum;
    unsigned long version;
    unsigned protocol;
{
    struct sockaddr_in _addr;
    struct sockaddr_in *addr = &_addr;
    CLIENT *clnt;
    int socket = RPC_ANYSOCK;    
    struct timeval timeout;
    enum clnt_stat res;
    u_short port;
    struct timeval tvs, tve;
    int time;
    char buf[40];
    char *p;

    timeout.tv_sec = 5; timeout.tv_usec = 0;

    if (SunrpcGetAddr(interp, host, addr) != TCL_OK) {
	return TCL_ERROR;
    }

    if ((protocol != IPPROTO_UDP) && (protocol != IPPROTO_TCP)) {
	interp->result = "unknown protocol";
	return TCL_ERROR;
    }

    port = pmap_getport (addr, prognum, version, protocol);
    addr->sin_port = htons(port);
    
    if (protocol == IPPROTO_TCP) {
	clnt = clnttcp_create (addr, prognum, version, &socket, 0, 0); 
    } else {
	struct timeval wait;
	wait.tv_sec = 1; wait.tv_usec = 0;
	clnt = clntudp_create (addr, prognum, version, wait, &socket);
    }
    if (clnt == NULL) {
	SunrpcCreateError(interp);
	return TCL_ERROR;
    }

    (void) gettimeofday(&tvs, (struct timezone *) NULL);

    res = clnt_call(clnt, NULLPROC, xdr_void, (char *)NULL, 
		    xdr_void, (char *) NULL, timeout);

    (void) gettimeofday(&tve, (struct timezone *) NULL);

    clnt_destroy (clnt);

    time = (tve.tv_sec - tvs.tv_sec) * 1000;
    time += (tve.tv_usec - tvs.tv_usec) / 1000;
    sprintf (buf, "%d ", time);

    p = clnt_sperrno(res);
    if (strncmp(p, "RPC: ", 5) == 0) p += 5;
    Tcl_AppendResult (interp, buf, p, (char *) NULL);
    return TCL_OK;
}

/* 
 * Extend a tcl command interpreter with a sunrpc command.
 */

int
Scotty_SunrpcCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    if (argc < 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0], 
			  " option host ?args?\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    if ((argc == 3) && (strcmp (argv[1], "info") == 0)) {

	return SunrpcInfo (interp, argv[2]);

    } else if ((argc == 3) && (strcmp (argv[1], "stat") == 0)) {

	return SunrpcRstat (interp, argv[2]);

    } else if ((argc == 3) && (strcmp (argv[1], "mount") == 0)) {
	
	return SunrpcMount (interp, argv[2]);
	
    } else if ((argc == 3) && (strcmp (argv[1], "exports") == 0)) {
	
	return SunrpcExports (interp, argv[2]);
	
    } else if (strcmp (argv[1], "probe") == 0) {

        int program, version;
	unsigned protocol = 0;

	if (argc < 6) { 
	    Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			      " probe host program version protocol\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetInt (interp, argv[3], &program) != TCL_OK) 
		return TCL_ERROR;
	if (Tcl_GetInt (interp, argv[4], &version) != TCL_OK) 
		return TCL_ERROR;
	if (!strcmp (argv[5], "tcp")) {
	    protocol = IPPROTO_TCP; 
	} else if (!strcmp (argv[5], "udp")) {
	    protocol = IPPROTO_UDP; 
	} else {
	    Tcl_AppendResult (interp, "unknown protocol ",
			      argv[5], (char *) NULL);
	    return TCL_ERROR;
	}

	return SunrpcProbe (interp, argv[2], program, version, protocol);

    } else if (!strcmp (argv[1], "ether")) {
	
	if (argc == 4) {
	    if (!strcmp (argv[3], "open"))
		    return SunrpcOpenEtherd (interp, argv[2]);
	    if (!strcmp (argv[3], "close"))
		    return SunrpcCloseEtherd (interp, argv[2]);
	    Tcl_AppendResult (interp, "bad arg \"", argv[3],
			      "\": should be open or close",
			      (char *) NULL);
	    return TCL_ERROR;
	}
	if (argc == 3) {
	    return SunrpcEtherd (interp, argv[2]);
	} else {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      argv[0], " ether host ?open? ?close?\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}
	
    }

    if (argc > 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0], 
			  " option host\"", (char *) NULL);
    } else {
	Tcl_AppendResult (interp, "bad option \"", argv[1], 
			  "\": should be info, probe, ",
			  "mount, exports, stat, or ether", 
			  (char *) NULL);
    }

    return TCL_ERROR;
}
