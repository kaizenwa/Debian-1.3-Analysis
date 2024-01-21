/*
 * tcp.c
 *
 * This file contains a simple Tcl "connect" command that returns a
 * standard Tcl File descriptor. You can accept connections and 
 * shutdown parts of full duplex connections. This file was heavily 
 * inspired by the tclRawTCP extension written by Pekka Nikander 
 * <pnr@innopoli.ajk.tele.fi> and Tim MacKenzie 
 * <tym@dibbler.cs.monash.edu.au) for TCL 6.7 and TK 3.2.
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
 *
 * Copyright 1992 Telecom Finland
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  Telecom Finland
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 */

#include "scotty.h"

/*
 * A structure to describe a TCP connection.
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

static struct servent *
TcpGetServent	_ANSI_ARGS_((char *name));

static struct hostent *
TcpGetHostent	_ANSI_ARGS_((char *name));

static Socket*
TcpSocket	_ANSI_ARGS_((Tcl_Interp *interp, char *fileId));

static int
TcpConnect	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
TcpListen	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
TcpAccept	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
TcpShutdown	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
TcpClose	_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
TcpInfo		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

/*
 * Get a pointer to a servent structure. Used to map service names
 * and numbers. According to the assigned numbers RFC, a service name
 * may not start with a digit. So it should be save to look at the first
 * byte to decide if its a service name or not.
 */

static struct servent *
TcpGetServent(name)
    char *name;
{
    struct servent *servent;
    static struct servent _servent;

    if (isdigit(*name)) {
        _servent.s_port = htons(atoi(name));
        _servent.s_proto = "tcp";
	servent = (_servent.s_port == -1) ? NULL : &_servent;
    } else {
	servent = getservbyname(name, "tcp");
    }

    return servent;
}

/*
 * Get a pointer to a hostent structure. First try gethostbyname.
 * If this fails, try inet_addr and fake a hostent structure.
 */

static struct hostent *
TcpGetHostent(name)
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
TcpSocket (interp, fileId)
    Tcl_Interp *interp;
    char *fileId;
{
    Socket *tsock;

    for (tsock = socketList; tsock != NULL; tsock = tsock->nextPtr) {
	if (strcmp(tsock->fileId, fileId) == 0) break;
    }

    if (tsock == NULL) {
	Tcl_AppendResult(interp, "bad tcp handle \"", fileId, 
			 "\"", (char *) NULL);
	return (Socket *) NULL;
    }

    return tsock;
}

/*
 * Open a socket connection to a given host and service.
 */

static int
TcpConnect (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int sock;
    FILE *f;
    struct hostent *hp;
    struct servent *servent;
    struct sockaddr_in name;
    Socket *tsock;
    
    if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " connect host port\"", (char *) NULL);
	return TCL_ERROR;
    }

    hp = TcpGetHostent(argv[2]);
    if (hp == NULL) {
	Tcl_AppendResult(interp, "no such host \"", argv[2], 
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }

    servent = TcpGetServent(argv[3]);
    if (servent == NULL) {
	Tcl_AppendResult(interp, "no such service \"", argv[3], 
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }

    sock = socket(PF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        Tcl_AppendResult(interp, "could not create socket: ", 
			 Tcl_PosixError (interp), (char *) NULL);
        return TCL_ERROR;
    }
    
    memcpy ((char *) &name.sin_addr, (char *) hp->h_addr, hp->h_length);
    name.sin_family = AF_INET;
    name.sin_port = servent->s_port;
     
    if (connect(sock, (struct sockaddr *) &name, sizeof(name)) < 0) {
	Tcl_AppendResult(interp, "can not connect to host \"", 
			 argv[2], "\" using port \"", argv[3], "\": ", 
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

    /*
     * Turn off buffering.  Otherwise, we run into nasty interaction
     * problems with gets/puts/read and our transmission commands below.
     */

    setbuf(f, NULL);

    Tcl_EnterFile(interp, f, TCL_FILE_READABLE | TCL_FILE_WRITABLE);

    tsock = (Socket *) ckalloc(sizeof(Socket));
    tsock->fileId = ckstrdup(interp->result);
    tsock->client.sin_addr = name.sin_addr;
    tsock->client.sin_port = name.sin_port;
    tsock->nextPtr = socketList;
    socketList = tsock;

    return TCL_OK;
}

/*
 * Create a listening socket on the given port number.
 */

static int
TcpListen (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int sock;
    FILE *f;
    struct servent *servent;
    struct sockaddr_in name;
    Socket *tsock;
    char *port = "0";
    
    if (argc != 2 && argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " listen ?port?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 3) port = argv[2];

    servent = TcpGetServent(port);
    if (servent == NULL) {
	Tcl_AppendResult(interp, "no such service \"", port, "\"", 
			 (char *) NULL);
	return TCL_ERROR;
    }

    sock = socket(PF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        Tcl_AppendResult(interp, "could not create socket: ", 
			 Tcl_PosixError(interp), (char *) NULL);
        return TCL_ERROR;
    }
    
    name.sin_addr.s_addr = INADDR_ANY;
    name.sin_family = AF_INET;
    name.sin_port = servent->s_port;
     
    if (bind(sock, (struct sockaddr *) &name, sizeof(name)) < 0) {
	Tcl_AppendResult(interp, "can not listen on port \"", port,
			 "\": ", Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }
    listen(sock, 5);

    if ((f = fdopen(sock, "r+")) == NULL) {
        Tcl_AppendResult(interp, "couldn't open file: ",
			 Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }

    Tcl_EnterFile(interp, f, TCL_FILE_READABLE | TCL_FILE_WRITABLE);

    tsock = (Socket *) ckalloc(sizeof(Socket));
    tsock->fileId = ckstrdup(interp->result);
    tsock->client.sin_addr.s_addr = INADDR_ANY;
    tsock->client.sin_port = 0;
    tsock->nextPtr = socketList;
    socketList = tsock;

    return TCL_OK;
}

/*
 * Accept a connection on a listening socket. Return a new file
 * if a new connection is established.
 */

static int
TcpAccept (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    struct sockaddr_in sockaddr;
    int len = sizeof sockaddr;
    FILE *f;
    int fd;
    Socket *tsock;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " accept file\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[2], 1, 0, &f) != TCL_OK) {
	return TCL_ERROR;
    }
    
    fd = fileno(f);
    
    fd = accept(fd, (struct sockaddr *)&sockaddr, &len);

    if (fd < 0) {
	Tcl_AppendResult(interp, "can not accept: ",
			 Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }

    if ((f = fdopen(fd, "r+")) == NULL) {
        Tcl_AppendResult(interp, "couldn't open file: ",
			 Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Turn off buffering.  Otherwise, we run into nasty interaction
     * problems with gets/puts/read and our transmission commands below.
     */

    setbuf(f, NULL);

    Tcl_EnterFile(interp, f, TCL_FILE_READABLE | TCL_FILE_WRITABLE);

    tsock = (Socket *) ckalloc(sizeof(Socket));
    tsock->fileId = ckstrdup(interp->result);
    tsock->client.sin_addr = sockaddr.sin_addr;
    tsock->client.sin_port = sockaddr.sin_port;
    tsock->nextPtr = socketList;
    socketList = tsock;

    return TCL_OK;
}

/*
 * Shutdown a a tcp connection. It is possible to shutdown a socket
 * for reading writing or both, although tcl will not know about it.
 * An attempt to write to a socket that has been closed for writing
 * will not fail. This is ugly but we keep independed from tcl 
 * internals, so I choose this way.
 */

static int
TcpShutdown (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *f;
    int fd;
    int perms;

    if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " shutdown file how\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[2], 1, 0, &f) != TCL_OK) {
	return TCL_ERROR;
    }
    
    fd = fileno(f);
#if (TCL_MINOR_VERSION == 4)
    perms = Tcl_FilePermissions(f);
#else
    perms = Tcl_FilePermissions(interp, f);
#endif
    if (perms == -1) {
	interp->result = "can not lookup file access mode";
	return TCL_ERROR;
    }
    
    if (!strcmp(argv[3], "0") || !strcmp(argv[3], "receives") || 
	!strcmp(argv[3], "read")) {
	if (perms & TCL_FILE_READABLE != TCL_FILE_READABLE) {
	    Tcl_AppendResult(interp, "File is not readable", (char *) NULL);
            return TCL_ERROR;
	}
	if (shutdown(fd, 0)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_PosixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
    } else if (!strcmp(argv[3], "1") || !strcmp(argv[3], "sends") ||
	       !strcmp(argv[3], "write")) {
	if (perms & TCL_FILE_WRITABLE != TCL_FILE_WRITABLE) {
	    Tcl_AppendResult(interp, "File is not writeable", (char *) NULL);
            return TCL_ERROR;
	}
	if (shutdown(fd, 1)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_PosixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
    } else if (!strcmp(argv[3], "2") || !strcmp(argv[3], "all") ||
	    !strcmp(argv[3], "both")) {
	if (shutdown(fd, 2)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_PosixError (interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
    } else {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " shutdown file how\"", (char *) NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}
	
/*
 * Close the tcp socket. This done by removing the entry in our socket 
 * list and calling the tcl close command to do the dirty job.
 */

static int
TcpClose (interp, argc, argv)
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

    for (p = socketList, q = NULL; p != NULL; q = p, p = p->nextPtr) {
	if (strcmp(p->fileId, argv[2]) == 0) break;
    }

    if (p == NULL) {
        Tcl_AppendResult(interp, "bad tcp handle \"", argv[2], 
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
	
    if (Tcl_GetOpenFile(interp, argv[2], 1, 0, &f) != TCL_OK) {
	return TCL_ERROR;
    }
    
    shutdown(fileno(f), 2);
    
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
 * Get some information about all open tcp connections. If called with
 * no argument, return a list of all opened tcp sockets. If called
 * with a tcl file handle, return detailed information regarding
 * this handle.
 */

static int
TcpInfo (interp, argc, argv)
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *f;
    Socket *tsock;

    if (argc < 2 || argc > 3) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " info ?file?\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (argc == 2) {

	for (tsock = socketList; tsock != NULL; tsock = tsock->nextPtr) {
	    if (Tcl_GetOpenFile(interp, tsock->fileId, 0, 0, &f) == TCL_OK)
		    Tcl_AppendElement(interp, tsock->fileId);
	}

    } else {
	
	int rc;
	struct sockaddr_in server;
	int length = sizeof(server);

	if (Tcl_GetOpenFile(interp, argv[2], 0, 0, &f) != TCL_OK)
		return TCL_ERROR;

	if ((tsock = TcpSocket(interp, argv[2])) == (Socket *) NULL)
		return TCL_ERROR;


	rc = getsockname(fileno (f), (struct sockaddr *) &server, &length);
	if (rc == 0) {
	    sprintf(interp->result, "%s %d ", 
		    inet_ntoa(server.sin_addr), 
		    (int) ntohs(server.sin_port));
	} else {
	    sprintf(interp->result, "{} {} ");
	}
	sprintf(interp->result+strlen(interp->result), "%s %d",
		inet_ntoa(tsock->client.sin_addr),
		(int) ntohs(tsock->client.sin_port));
    }

    return TCL_OK;
}

/*
 * This is the tcp command as described in the scotty documentation.
 * It simply dispatches to the C functions implementing the options
 * understood by the tcp command.
 */

int
Scotty_TcpCmd (clientData, interp, argc, argv)
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

    if ((c == 'c') && (strncmp(argv[1], "connect", length) == 0)) {
        return TcpConnect(interp, argc, argv);
    } else if ((c == 'l') && (strncmp(argv[1], "listen", length) == 0)) {
        return TcpListen(interp, argc, argv);
    } else if ((c == 'a') && (strncmp(argv[1], "accept", length) == 0)) {
        return TcpAccept(interp, argc, argv);
    } else if ((c == 's') && (strncmp(argv[1], "shutdown", length) == 0)) {
        return TcpShutdown(interp, argc, argv);
    } else if ((c == 'c') && (strncmp(argv[1], "close", length) == 0)) {
        return TcpClose(interp, argc, argv);
    } else if ((c == 'i') && (strncmp(argv[1], "info", length) == 0)) {
        return TcpInfo(interp, argc, argv);
    }

    Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be ",
		     "connect, accept, shutdown, or info",
		     (char *) NULL);
    return TCL_ERROR;
}
