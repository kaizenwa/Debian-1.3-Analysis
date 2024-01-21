/*
 * udp.c
 *
 * This is the implementation of the udp command that allows you
 * to send and receive udp datagrams.
 *
 * Copyright (c) 1994, 1995
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

#include <tcl.h>

/*
 * A structure to describe an open UDP socket.
 */

typedef struct Socket {
    char *fileId;                /* The tcl file name of this socket  */
    struct sockaddr_in client;   /* The client we are connected to    */
    struct Socket *nextPtr;      /* Next Socket in queue, or NULL for */
			         /* end of queue.                     */
} Socket;

static Socket *socketList = NULL;

/*
 * Forward declarations for procedures defined later in this file:
 */

static struct servent*
UdpGetServent	_ANSI_ARGS_((char *name));

static struct hostent*
UdpGetHostent	_ANSI_ARGS_((char *name));

static Socket*
UdpSocket	_ANSI_ARGS_((Tcl_Interp *interp, char *fileId));

static int
UdpOpen		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
UdpConnect	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
UdpSend		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
UdpReceive	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
UdpClose	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

#ifdef HAVE_MULTICAST
static int
UdpMulticast	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));
#endif

static int
UdpInfo		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));


/*
 * Get a pointer to a servent structure. Used to map service names
 * and numbers. According to the assigned numbers RFC, a service name
 * may not start with a digit. So it should be save to look at the first
 * byte to decide if its a service name or not.
 */

static struct servent *
UdpGetServent(name)
    char *name;
{
    struct servent *servent;
    static struct servent _servent;

    if (isdigit(*name)) {
        _servent.s_port = htons(atoi(name));
        _servent.s_proto = "udp";
	servent = (_servent.s_port == -1) ? NULL : &_servent;
    } else {
	servent = getservbyname(name, "udp");
    }

    return servent;
}

/*
 * Get a pointer to a hostent structure. First try gethostbyname.
 * If this fails, try inet_addr and fake a hostent structure.
 */

static struct hostent *
UdpGetHostent(name)
    char *name;
{
    struct hostent *hostent;
    static struct hostent _hostent;
    static int hostaddr, hostaddrPtr[2];
	    
    hostent = gethostbyname(name);
    if (hostent != NULL) return hostent;

    hostaddr = inet_addr(name);
    if (hostaddr == -1) return NULL;

    _hostent.h_addr_list = (char **) hostaddrPtr;
    _hostent.h_addr_list[0] = (char *) &hostaddr;
    _hostent.h_addr_list[1] = NULL;
    _hostent.h_length = sizeof(hostaddr);
    _hostent.h_addrtype = AF_INET;
    return &_hostent;
}

/*
 * Returns a Socket * pointer if fileId is a valid socket. We leave 
 * an error message in interp if fileId is not a valid socket.
 */

static Socket *
UdpSocket (interp, fileId)
    Tcl_Interp *interp;
    char *fileId;
{
    Socket *usock;

    for (usock = socketList; usock != NULL; usock = usock->nextPtr) {
	if (strcmp(usock->fileId, fileId) == 0) break;
    }

    if (usock == NULL) {
	Tcl_AppendResult(interp, "bad udp handle \"", fileId, 
			  "\"", (char *) NULL);
	return (Socket *) NULL;
    }

    return usock;
}

/*
 * Create a udp socket and create a tcl file handle for it.
 */

static int
UdpOpen (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int sock;
    FILE *f;
    struct servent *servent;
    struct sockaddr_in name;
    Socket *usock;
    char *port = "0";

    if (argc < 2 || argc > 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " open ?port?\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (argc == 3) port = argv[2];

    servent = UdpGetServent(port);
    if (servent == NULL) {
	Tcl_AppendResult(interp, "no such service \"", port, "\"", 
			 (char *) NULL);
	return TCL_ERROR;
    }
	
    sock = socket(PF_INET, SOCK_DGRAM, 0);
    if (sock < 0) {
	Tcl_AppendResult(interp, "could not create socket: ", 
			 Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }

    name.sin_family = AF_INET;
    name.sin_addr.s_addr = htonl(INADDR_ANY);
    name.sin_port = servent->s_port;

    if (bind(sock, (struct sockaddr *) &name, sizeof(name)) < 0) {
	Tcl_AppendResult(interp, "couldn't create socket on port \"",
			 port, "\": ", Tcl_PosixError(interp),
			 (char *) NULL);
	close(sock);
	return TCL_ERROR;
    }
    
    if ((f = fdopen(sock, "r+")) == NULL) {
        Tcl_AppendResult(interp, "couldn't open file: ",
			 Tcl_PosixError(interp), (char *) NULL);
	close(sock);
        return TCL_ERROR;
    }
    
    Tcl_EnterFile(interp, f, TCL_FILE_READABLE | TCL_FILE_WRITABLE);

    usock = (Socket *) ckalloc(sizeof(Socket));
    usock->fileId = ckstrdup(interp->result);
    usock->client.sin_addr.s_addr = htonl(INADDR_ANY);
    usock->client.sin_port = 0;
    usock->nextPtr = socketList;
    socketList = usock;

    return TCL_OK;
}

/*
 * Connect a udp socket to a remote server. This allows us to use tcl's
 * read, write and puts commands instead of the send and receive options
 * of the udp command.
 */

static int
UdpConnect (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int sock;
    FILE *f;
    Socket *usock;
    struct sockaddr_in name;
    struct hostent *hp;
    struct servent *servent;

    if (argc != 4) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " connect host port\"", (char *) NULL);
        return TCL_ERROR;
    }

    hp = UdpGetHostent(argv[2]);
    if (hp == NULL) {
	Tcl_AppendResult(interp, "no such host \"", argv[2], 
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }

    servent = UdpGetServent(argv[3]);
    if (servent == NULL) {
	Tcl_AppendResult(interp, "no such service \"", argv[3], 
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    sock = socket(PF_INET, SOCK_DGRAM, 0);
    if (sock < 0) {
	Tcl_AppendResult(interp, "could not create socket: ", 
			 Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }

    memcpy((char *) &name.sin_addr, (char *) hp->h_addr, hp->h_length);
    name.sin_family = AF_INET;
    name.sin_port = servent->s_port;
    
    if (connect(sock, (struct sockaddr *) &name, sizeof(name)) < 0) {
	Tcl_AppendResult(interp, "can not connect to host \"", argv[2],
			 "\" using port \"", argv[3], "\": ", 
			 Tcl_PosixError(interp), (char *) NULL);
	close(sock);
	return TCL_ERROR;
    }

    if ((f = fdopen(sock, "r+")) == NULL) {
        Tcl_AppendResult(interp, "couldn't open file: ",
			 Tcl_PosixError(interp), (char *) NULL);
	close(sock);
        return TCL_ERROR;
    }
    
    Tcl_EnterFile(interp, f, TCL_FILE_READABLE | TCL_FILE_WRITABLE);

    usock = (Socket *) ckalloc(sizeof(Socket));
    usock->fileId = ckstrdup(interp->result);
    usock->client.sin_addr = name.sin_addr;
    usock->client.sin_port = servent->s_port;
    usock->nextPtr = socketList;
    socketList = usock;

    return TCL_OK;
}

/*
 * Send a message using the udp socket file to a given host and port.
 */

static int
UdpSend (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *f;
    struct sockaddr_in name;
    struct hostent *hp;
    struct servent *servent;
    Socket *usock;

    if (argc != 6 && argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " send file ?host port? string\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[2], 1, 1, &f) != TCL_OK)
	    return TCL_ERROR;

    if ((usock = UdpSocket(interp, argv[2])) == (Socket *) NULL)
	    return TCL_ERROR;

    if (argc == 6 && usock->client.sin_addr.s_addr != htonl(INADDR_ANY)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " send file string\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (argc == 4 && usock->client.sin_addr.s_addr == htonl(INADDR_ANY)) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " send file host port string\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (argc == 6) {
	
	hp = UdpGetHostent(argv[3]);
	if (hp == NULL) {
	    Tcl_AppendResult(interp, "so such host \"", argv[3], 
			     "\"", (char *) NULL);
	    return TCL_ERROR;
	}
	
	servent = UdpGetServent(argv[4]);
	if (servent == NULL) {
	    Tcl_AppendResult(interp, "no such service \"", argv[4], 
			     "\"", (char *) NULL);
	    return TCL_ERROR;
	}
	
	memcpy((char *) &name.sin_addr, (char *) hp->h_addr, hp->h_length);
	name.sin_family = AF_INET;
	name.sin_port = servent->s_port;

	if (sendto(fileno(f), argv[5], strlen(argv[5]), 0, 
		    (struct sockaddr *) &name, sizeof(name)) < 0) {
	    Tcl_AppendResult(interp, "udp send to host \"", argv[3], 
			     "\" port \"", argv[4], "\" failed: ",
			     Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}

    } else {

	if (send(fileno(f), argv[3], strlen(argv[3]), 0) < 0) {
	    Tcl_AppendResult(interp, "udp send failed: ", 
			     Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}

    }
    

    return TCL_OK;
}

/*
 * Receive a message from a udp handle and return the sending hostname,
 * its port number and the message in a tcl list.
 */

static int
UdpReceive (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *f;
#define BUFSIZE 8192
    u_char msg[BUFSIZE];
    u_char *startPtr, *scanPtr;
    char buf[80];
    int clen, len;
    struct sockaddr_in client;
    Tcl_DString tclString;

    if (argc != 3) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " receive file\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[2], 0, 1, &f) != TCL_OK)
	    return TCL_ERROR;

    if (UdpSocket(interp, argv[2]) == (Socket *) NULL)
	    return TCL_ERROR;

    clen = sizeof(client);
    len = recvfrom(fileno(f), msg, BUFSIZE, 0, 
		   (struct sockaddr *) &client, &clen);
    if (len < 0) {
	Tcl_AppendResult(interp, "receive failed on \"", argv[2], "\": ", 
			 Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }

    Tcl_DStringInit(&tclString);
    startPtr = msg;
    for (scanPtr = startPtr; scanPtr < msg + len; scanPtr++) {
	if ((isalnum(*scanPtr) || ispunct(*scanPtr) || isspace(*scanPtr))
	    && *scanPtr != '\\') {
	    continue;
	}
	Tcl_DStringAppend(&tclString, startPtr, scanPtr - startPtr);
        startPtr = scanPtr + 1;
	if (*scanPtr == '\\') {
	    Tcl_DStringAppend(&tclString, "\\\\", 2);
	} else {
	    Tcl_DStringAppend(&tclString, "\\x", 2);
	    sprintf(buf, "%02x", *scanPtr);
	    Tcl_DStringAppend(&tclString, buf, -1);
	}
    }
    Tcl_DStringAppend(&tclString, startPtr, scanPtr - startPtr);

    sprintf(buf, "%d", (int) ntohs(client.sin_port));
    Tcl_AppendElement(interp, inet_ntoa(client.sin_addr));
    Tcl_AppendElement(interp, buf);
    Tcl_AppendElement(interp, Tcl_DStringValue(&tclString));
    Tcl_DStringFree(&tclString);

    return TCL_OK;
}

/*
 * Close the udp socket. This done by removing the entry in our socket 
 * list and calling the tcl close command to do the dirty job.
 */

static int
UdpClose (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *f;
    Socket *p, *q;

    if (argc != 3) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " close file\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[2], 0, 0, &f) != TCL_OK)
	    return TCL_ERROR;

    for (p = socketList, q = NULL; p != NULL; q = p, p = p->nextPtr) {
	if (strcmp(p->fileId, argv[2]) == 0) break;
    }

    if (p == NULL) {
        Tcl_AppendResult(interp, "bad udp handle \"", argv[2], 
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
	
    if (q == NULL) {
        socketList = p->nextPtr;
    } else {
        q->nextPtr = p->nextPtr;
    }
    ckfree(p->fileId);
    ckfree((char *) p);

    return Tcl_VarEval(interp, "close ", argv[2], (char *) NULL);
}

/*
 * Get some information about all open udp sockets. If called with
 * no argument, return a list of all opened udp sockets. If called
 * with a tcl file handle, return detailed information regarding
 * this handle.
 */

static int
UdpInfo (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *f;
    Socket *usock;

    if (argc < 2 || argc > 3) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " info ?file?\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (argc == 2) {

	for (usock = socketList; usock != NULL; usock = usock->nextPtr) {
	    if (Tcl_GetOpenFile(interp, usock->fileId, 0, 0, &f) == TCL_OK)
		    Tcl_AppendElement(interp, usock->fileId);
	}

    } else {
	
	int rc;
	struct sockaddr_in server;
	int length = sizeof(server);

	if (Tcl_GetOpenFile(interp, argv[2], 0, 0, &f) != TCL_OK)
		return TCL_ERROR;

	if ((usock = UdpSocket(interp, argv[2])) == (Socket *) NULL)
		return TCL_ERROR;

	rc = getsockname(fileno(f), (struct sockaddr *) &server, &length);
	if (rc == 0) {
	    sprintf(interp->result, "%s %d ", 
		    inet_ntoa(server.sin_addr), 
		    (int) ntohs(server.sin_port));
	} else {
	    sprintf(interp->result, "{} {} ");
	}
	sprintf(interp->result+strlen(interp->result), "%s %d",
		inet_ntoa(usock->client.sin_addr),
		(int) ntohs(usock->client.sin_port));
    }

    return TCL_OK;
}

#ifdef HAVE_MULTICAST
static int
UdpMulticast (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    FILE *f;
    Socket* usock;
    struct hostent *hp;
    struct servent *servent;
    struct ip_mreq mreq;
    struct sockaddr_in name;
    int len, tmp, optlen, sock;
    unsigned char ttl;
    
    if (argc < 3) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " multicast option ?args ...?\"",
			 (char *) NULL);
        return TCL_ERROR;
    }

    len = strlen(argv[2]);
    if (*argv[2] == 't' && !strncmp(argv[2], "ttl", len)) {

	if (argc < 4 || argc > 5) {
	    Tcl_AppendResult(interp, "wrong # args, should be \"",
                             argv[0], " multicast ttl file ?value?\"", 
			     (char *) NULL);
	    return TCL_ERROR;
	}

	if (Tcl_GetOpenFile(interp, argv[3], 1, 1, &f) != TCL_OK)
	    return TCL_ERROR;
	
	sock = fileno(f);

	if ((usock = UdpSocket(interp, argv[3])) == (Socket *) NULL)
	    return TCL_ERROR;

	if (argc == 5) {
            if (Tcl_GetInt(interp, argv[4], &tmp) != TCL_OK)
                return TCL_ERROR;
            ttl = tmp;
            if (setsockopt(sock, IPPROTO_IP, IP_MULTICAST_TTL, (char*) &ttl,
			   sizeof(ttl)) == -1) {
                Tcl_AppendResult(interp, "can't set multicast ttl: ",
                                 Tcl_PosixError(interp), (char *) NULL);
                return TCL_ERROR;
            }
            return TCL_OK;
        }

	optlen = sizeof(ttl);
	if (getsockopt(sock, IPPROTO_IP, IP_MULTICAST_TTL, (char*) &ttl,
		       &optlen) == -1) {
	    Tcl_AppendResult(interp, "can't get multicast ttl: ",
			     Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}
	tmp = ttl;
	sprintf(interp->result, "%d", tmp);
	return TCL_OK;

    } else if (*argv[2] == 'o' && !strncmp(argv[2], "open", len)) {

	if (argc < 5 || argc > 6) {
	    Tcl_AppendResult(interp, "wrong # args, should be \"", argv[0],
	      " multicast open multicast-address port ?interface-address?\"",
			     (char *) NULL);
	    return TCL_ERROR;
	}

	hp = UdpGetHostent(argv[3]);
	if (hp == NULL) {
	    Tcl_AppendResult(interp, "so such host \"", argv[3],
			     "\"", (char *) NULL);
            return TCL_ERROR;
	}

	servent = UdpGetServent(argv[4]);
	if (servent == NULL) {
	    Tcl_AppendResult(interp, "no such service \"", argv[4], 
			     "\"", (char *) NULL);
	    return TCL_ERROR;
	}
	
	sock = socket(PF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
	    Tcl_AppendResult(interp, "could not create socket: ", 
			     Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}

	memcpy((char *) &mreq.imr_multiaddr, 
	       (char *) hp->h_addr, hp->h_length);

	if ((mreq.imr_multiaddr.s_addr == -1 ||
	     !IN_MULTICAST(ntohl(mreq.imr_multiaddr.s_addr)))) {
	    Tcl_AppendResult(interp, "bad multicast address \"",
			     argv[3], "\"", (char *) NULL);
	    close(sock);
	    return TCL_ERROR;
	}
	if (argc == 6) {
	    hp = UdpGetHostent(argv[5]);
	    if (hp == NULL) {
		Tcl_AppendResult(interp, "bad interface address \"", argv[5],
				 "\"", (char *) NULL);
		close(sock);
		return TCL_ERROR;
	    }
	    memcpy((char *) &mreq.imr_interface, 
		   (char *) hp->h_addr, hp->h_length);
	} else {
	    mreq.imr_interface.s_addr = htonl(INADDR_ANY);
	}

	if (setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*) &mreq,
		       sizeof(mreq)) == -1) {
	    Tcl_AppendResult(interp, "multicast IP membership add failed: ",
			     Tcl_PosixError(interp), (char *) NULL);
	    close(sock);
	    return TCL_ERROR;
	}

#ifdef SO_REUSEADDR

	/*
	 * Allow others to bind to the same UDP port.
	 */

	{
	    int one = 1;
	    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
		       (char *) &one, sizeof(one));
	}
#endif

	name.sin_family = AF_INET;
	name.sin_addr.s_addr = htonl(INADDR_ANY);
	name.sin_port = servent->s_port;
	if (bind(sock, (struct sockaddr*) &name, sizeof(name)) == -1) {
 	    Tcl_AppendResult(interp, "bind failed: ",
			     Tcl_PosixError(interp), (char *) NULL);
	    close(sock);
	    return TCL_ERROR;
	}

	if ((f = fdopen(sock, "r+")) == NULL) {
	    Tcl_AppendResult(interp, "couldn't open file: ",
			     Tcl_PosixError(interp), (char *) NULL);
	    close(sock);
	    return TCL_ERROR;
	}

	Tcl_EnterFile(interp, f, TCL_FILE_READABLE | TCL_FILE_WRITABLE);

	usock = (Socket *) ckalloc(sizeof(Socket));
	usock->fileId = ckstrdup(interp->result);
	usock->client.sin_addr.s_addr = htonl(INADDR_ANY);
	usock->client.sin_port = 0;
	usock->nextPtr = socketList;
	socketList = usock;

	return TCL_OK;
    }

    Tcl_AppendResult(interp, "bad multicast option \"", argv[2],
		     "\": should be ttl, or open", (char *) NULL);
    return TCL_ERROR;
}
#endif

/*
 * This is the udp command as described in the scotty documentation.
 * It simply dispatches to the C functions implementing the options
 * understood by the udp command.
 */

int
Scotty_UdpCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int length;
    char c;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'o') && (strncmp(argv[1], "open", length) == 0)) {
	return UdpOpen(interp, argc, argv);
    } else if ((c == 'c') && (strncmp(argv[1], "connect", length) == 0)) {
        return UdpConnect(interp, argc, argv);
    } else if ((c == 's') && (strncmp(argv[1], "send", length) == 0)) {
        return UdpSend(interp, argc, argv);
    } else if ((c == 'r') && (strncmp(argv[1], "receive", length) == 0)) {
        return UdpReceive(interp, argc, argv);
    } else if ((c == 'c') && (strncmp(argv[1], "close", length) == 0)) {
        return UdpClose(interp, argc, argv);
    } else if ((c == 'i') && (strncmp(argv[1], "info", length) == 0)) {
        return UdpInfo(interp, argc, argv);
    }
#ifdef HAVE_MULTICAST
    else if ((c == 'm') && (strncmp(argv[1], "multicast", length) == 0)) {
	return UdpMulticast(interp, argc, argv);
    }
#endif

    Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be ",
		     "open, connect, send, receive, close, ",
#ifdef HAVE_MULTICAST
		     "multicast, ",
#endif
		     "or info", (char *) NULL);
    return TCL_ERROR;
}
