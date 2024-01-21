/*
 * ntp.c
 *
 * Extend a tcl command interpreter with a command to query
 * NTP server for timestat.
 *
 * Copyright (c) 1994, 1995
 *
 * E. Schoenfelder, J. Schoenwaelder
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

/* 
 * todo: 
 *    * check about `more' flag.
 *    * cache hostlookups.
 *    * make better error return strings.
 */

#include "scotty.h"

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

struct ntp_control {
    unsigned char mode;			/* version and mode */
    unsigned char op;			/* opcode */
    unsigned short sequence;		/* sequence # */
    unsigned short status;		/* status */
    unsigned short associd;		/* association id */
    unsigned short offset;		/* data offset */
    unsigned short len;			/* data len */
    unsigned char data[(480 + 20)];	/* data + auth */
};

#ifdef __alpha
/* dec alpha needs an extra cookie... */
typedef unsigned int ipaddr_t;
#else
typedef unsigned long ipaddr_t;
#endif

/* communication socket */
static int sock = -1;

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
NtpSocket		_ANSI_ARGS_((Tcl_Interp *interp));

static int
NtpReady		_ANSI_ARGS_((int sock, int timeout));

static void
NtpMakePkt		_ANSI_ARGS_((struct ntp_control *pkt, 
				     int op, int assoc, int seq));
static int
NtpFetch		_ANSI_ARGS_((Tcl_Interp *interp,
				     struct sockaddr_in *daddr, 
				     int op, int retries, int timeo,
				     char *buf, int assoc));
static int
NtpSplit		_ANSI_ARGS_((Tcl_Interp *interp, char *varname,
				     char *pfix, char *buf));
static int 
NtpGetPeer		_ANSI_ARGS_((char *data, int *assoc));


/*
 * create the communication socket:
 */

static int
NtpSocket (interp)
    Tcl_Interp *interp;
{
    struct sockaddr_in maddr;
    
    if (sock != -1) {
	close(sock);
    }

    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	Tcl_AppendResult(interp, "could not create socket: ", 
			 Tcl_PosixError(interp), (char *) NULL);
        return TCL_ERROR;
    }

    maddr.sin_family = AF_INET;
    maddr.sin_addr.s_addr = htonl(INADDR_ANY);
    maddr.sin_port = htons(0);

    if (bind(sock, (struct sockaddr *) &maddr, sizeof(maddr)) < 0) {
	Tcl_AppendResult(interp, "can not bind: ", Tcl_PosixError(interp), 
			 (char *) NULL);
        return TCL_ERROR;
    }

    return TCL_OK;
}


/*
 * return != 1 if sock is ready.
 */

static int
NtpReady (sock, timeout)
     int sock, timeout;
{
    fd_set rfd;
    struct timeval tv;
    int rc;
    
    FD_ZERO(&rfd);
    FD_SET(sock, &rfd);
    tv.tv_sec = timeout / 1000;
    tv.tv_usec = (timeout % 1000) * 1000;
    
    do {
	rc = select(sock + 1, &rfd, (fd_set *) 0, (fd_set *) 0, &tv);
	if (rc == -1 && errno == EINTR)
		continue;
	if (rc == -1) {
	    perror("* select failed; reason");
	    return 0;
	}
    } while (rc < 0);
    
    return rc > 0;
}


static void
NtpMakePkt (pkt, op, assoc, seq)
     struct ntp_control *pkt;
     int op, assoc, seq;
{
    pkt->mode = 0x18 | 6;			/* version 3 | MODE_CONTROL */
    pkt->op = op;				/* CTL_OP_... */
    pkt->sequence = htons(seq);
    pkt->status = 0;
    pkt->associd = htons(assoc);
    pkt->offset = htons(0);

    if (! assoc) {
	sprintf((char *) pkt->data, 
	      "precision,peer,system,stratum,rootdelay,rootdispersion,refid");
    } else  {
	sprintf((char *) pkt->data, 
	      "srcadr,stratum,precision,reach,valid,delay,offset,dispersion");
    }
    pkt->len = htons(strlen(pkt->data));
}


/*
 * Fetch op from server, append result to buf.
 * Return a standard tcl result.
 */

static int
NtpFetch (interp, daddr, op, retries, timeo, buf, assoc)
    Tcl_Interp *interp;
    struct sockaddr_in *daddr;
    int op;
    int retries;
    int timeo;
    char *buf;
    int assoc;
{
    struct ntp_control qpkt, pkt;
    struct sockaddr_in saddr;
    int i, rc, slen = sizeof(saddr);
    int timeout = (timeo * 1000) / (retries + 1);

    static int seq = 1;				/* sequence number */

    /* 
     * increment to a new sequence number: 
     */

    seq++;
    
    for (i = 0; i < retries + 1; i++) {
	NtpMakePkt(&qpkt, op, assoc, seq);		/* CTL_OP_READVAR */
	memset((char *) &pkt, 0, sizeof(pkt));
	
	rc = sendto(sock, (char *) &qpkt, sizeof(qpkt), 0, 
		    (struct sockaddr *) daddr, sizeof(*daddr));
	if (rc < 0) {
	    Tcl_AppendResult(interp, "udp sendto failed: ",
			     Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}
	
	while (NtpReady(sock, timeout)) {
	    memset((char *) &pkt, 0, sizeof(pkt));
	    rc = recvfrom(sock, (char *) &pkt, sizeof(pkt), 0, 
			  (struct sockaddr *) &saddr, &slen);
	    if (rc < 0) {
		Tcl_AppendResult(interp, "recvfrom failed: ",
				 Tcl_PosixError(interp), (char *) NULL);
		return TCL_ERROR;
	    }

	    /*
	     * Ignore short packets < (ntp_control + 1 data byte)
	     */
	    
	    if (rc < 12 + 1) {
		continue;
	    }
	    
	    if ((pkt.op & 0x80) 
		&& saddr.sin_addr.s_addr == daddr->sin_addr.s_addr
		&& saddr.sin_port == daddr->sin_port
		&& pkt.sequence == qpkt.sequence)
	    {
		strcat(buf, pkt.data);
		return TCL_OK;
	    }
	}
    }
    
    Tcl_SetResult(interp, "no ntp response", TCL_STATIC);
    return TCL_ERROR;
}

static int
NtpSplit (interp, varname, pfix, buf)
    Tcl_Interp *interp;
    char *varname;
    char *pfix;
    char *buf;
{
    char *d, *s, *g, *r;
    char var [256];

    for (s = buf, d = buf; *s; s++) {
	if (*s == ',') {
	    *s = '\0';
	    for (g = d; *g && (*g != '='); g++) ;
	    if (*g) {
		*g++ = '\0';
		sprintf(var, "%s.%s", pfix, d);
		r = Tcl_SetVar2(interp, varname, var, g, TCL_LEAVE_ERR_MSG);
		if (!r) return TCL_ERROR;
	    }
	    for (d = s+1; *d && isspace(*d); d++) ;
	}
    }

    if (d != s) {
	if (isspace(*--s)) *s = '\0';
	if (isspace(*--s)) *s = '\0';
	for (g = d; *g && (*g != '='); g++) ;
	if (*g) {
	    *g++ = '\0';
	    sprintf(var, "%s.%s", pfix, d);
	    r = Tcl_SetVar2(interp, varname, var, g, TCL_LEAVE_ERR_MSG);
	    if (!r) return TCL_ERROR;
	}
    }

    return TCL_OK;
}


/*
 * scan data for a peer=... entry;
 * return the peer entry in peer and rc = 1, if found
 */
 
static int 
NtpGetPeer (data, assoc)
     char *data;
     int *assoc;
{
    int i;

    for (i = 0; i < strlen(data); i++) {
        if (1 == sscanf(data + i, "peer=%d,", assoc)) {
	    return 1;
	}
    }

    return 0;
}

/*
 * This procedure is invoked to process the "ntp" command.
 * See the user documentation for details on what it does.
 */

int
Scotty_NtpCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    char *cmd = argv[0];
    struct sockaddr_in daddr;
    int rc, assoc, a, b, c, d;
    ipaddr_t addr;
    struct hostent *hp;
    char *hname, *varname, data1 [1024], data2 [1024];

    static int retries = 2;	/* default # of retries: */
    static int timeout = 2;	/* default timeout in s */

    int actRetries = -1;	/* actually used retries */
    int actTimeout = -1;	/* actually used timeout */

    if (argc == 1) {
      ntpWrongArgs:
	Tcl_AppendResult(interp, "wrong # args: should be \"", 
			 cmd, " ?-timeout t? ?-retries r? ",
			 "host arrayName\"", (char *) NULL);
	return TCL_ERROR;
    }

    argc--; argv++;

    while (argc > 0 && (*argv[0] == '-')) {
        int length = strlen(argv[0]);
	if (strncmp(argv [0], "-retries", length) == 0) {
	    argc--, argv++;
	    if (argc < 1) {
	        sprintf(interp->result, "%d", retries);
		return TCL_OK;
	    }
	    if (Tcl_GetInt(interp, argv[0], &actRetries) != TCL_OK)
		    return TCL_ERROR;
	    if (actRetries < 0) {
		Tcl_SetResult(interp, "negative retries", TCL_STATIC);
		return TCL_ERROR;
	    }
	} else if (strncmp(argv [0], "-timeout", length) == 0) {
	    argc--, argv++;
	    if (argc < 1) {
	        sprintf(interp->result, "%d", timeout);
		return TCL_OK;
	    }
	    if (Tcl_GetInt(interp, argv [0], &actTimeout) != TCL_OK)
                    return TCL_ERROR;
	    if (actTimeout < 1) {
		Tcl_SetResult(interp, "negative timeout", TCL_STATIC);
                return TCL_ERROR;
	    }
	} else {
	    Tcl_AppendResult(interp, "unknown option \"", argv [0], "\"", 
			     (char *) NULL);
	    return TCL_ERROR;
	}
	argc--, argv++;
    }

    /*
     * No arguments left? Set the default values and return.
     */

    if (argc == 0) {
	if (actRetries > 0) {
	    retries = actRetries;
	}
	if (actTimeout > 0) {
	    timeout = actTimeout;
	}
        return TCL_OK;
    }

    /*
     * Now we should have two arguments left: host and arrayName.
     */
    
    if (argc != 2) {
	goto ntpWrongArgs;
    }

    hname = argv[0];
    varname = argv[1];
    actRetries = actRetries < 0 ? retries : actRetries;
    actTimeout = actTimeout < 0 ? timeout : actTimeout;
    
    if (sock < 0) {
	if (NtpSocket(interp) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    
    while (hname && (*hname == ' '|| *hname == '\t')) {
	hname++;
    }
    if (hname[0] >= '0' && hname[0] <= '9'
	&& 4 == sscanf(hname, "%d.%d.%d.%d", &a, &b, &c, &d))
	    addr = htonl(a << 24 | b << 16 | c << 8 | d);
    else if ((hp = gethostbyname(hname)))
	    addr = * (ipaddr_t *) hp->h_addr;
    else {
	Tcl_AppendResult(interp, "no such host \"", hname, 
			 "\"", (char *) NULL);
        return TCL_ERROR;
    }
    
    daddr.sin_family = AF_INET;
    daddr.sin_addr.s_addr = addr;
    daddr.sin_port = htons(123);			/* ntp service */
    
    /*
     * CTL_OP_READVAR
     */

    data1 [0] = data2 [0] = 0;
    if (NtpFetch(interp, &daddr, 2, actRetries, actTimeout, data1, 0) 
	!= TCL_OK) {
	return TCL_ERROR;
    }
    
    /*
     * Try to get additional info: 
     */

    if (NtpGetPeer(data1, &assoc)) {
	if (NtpFetch(interp, &daddr, 2, actRetries, actTimeout, data2, assoc)
	    != TCL_OK) {
	    return TCL_ERROR;
	}
    }

    /*
     * split response buffer:
     */

    rc = NtpSplit(interp, varname, "sys", data1);
    if (rc == TCL_OK) {
	return NtpSplit(interp, varname, "peer", data2);
    }

    return rc;
}

/* end of ntp.c */
