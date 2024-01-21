/*
 * snmpNet.c
 *
 * This file contains all functions that handle transport over UDP.
 * There is also some code here to talk to the straps daemon which is
 * used to receive and forward SNMP traps send to the privileged 162
 * port.
 *
 * Copyright (c) 1994, 1995
 *
 * Sven Schmidt, J. Schoenwaelder
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

#include "snmp.h"

#include <sys/un.h>
#include <arpa/inet.h>
#include <netdb.h>

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/*
 * Local variables:
 */

extern int	hexdump;		/* flag that controls hexdump */
static int	sock = -1;		/* socket to send/receive messages */
static int	trap_sock = -1;		/* socket to receive traps */

static char	*serv_path = "/tmp/.straps";

/*
 * The default filename where we will find the straps binary. This
 * is normally overwritten in the Makefile.
 */

#ifndef STRAPS
#define STRAPS "/usr/local/bin/straps"
#endif

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
xread			_ANSI_ARGS_((int fd, char *buf, int len));

static int
straps			_ANSI_ARGS_((Tcl_Interp *interp));

static void
DumpPacket		_ANSI_ARGS_((u_char *packet, int packetlen,
				     char *msg, struct sockaddr_in *from));

static void
ResponseProc		_ANSI_ARGS_((ClientData clientData, int mask));

static void
TrapProc		_ANSI_ARGS_((ClientData clientData, int mask));

static void
AgentProc		_ANSI_ARGS_((ClientData clientData, int mask));

static int
TrapRecv		_ANSI_ARGS_((Tcl_Interp *interp, 
				     u_char *packet, int *packetlen, 
				     struct sockaddr_in *from));
static int
AgentRecv		_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
				     u_char *packet, int *packetlen,
				     struct sockaddr_in *from));


#ifdef SIGPIPE
static void
ign_pipe		_ANSI_ARGS_((void));
#endif

/*
 * Ignore borken pipe signals. Restart signalhandler for all these 
 * bozo's outside.
 */

#ifdef SIGPIPE
static void
ign_pipe ()
{
    signal (SIGPIPE, ign_pipe);
}
#endif

/*
 * Read a buffer from a file descriptor. This wrapper is needed on
 * broken SYS V machines to handle interrupted system calls.
 */

static int
xread (fd, buf, len)
     int fd;
     char *buf;
     int len;
{
    int rc;
    
    while ((rc = read (fd, buf, len)) < 0
           && (errno == EINTR || errno == EAGAIN))
            continue;

    return rc;
}

/*
 * straps() starts the straps forwarder daemon and returns a 
 * standard Tcl result.
 */

static int
straps (interp)
     Tcl_Interp *interp;
{
    int *pidArray, code, argc = 1;
    static char *argv[2] = { NULL, 0 };

    if (! argv[0]) {
        argv[0] = getenv("SCOTTY_STRAPS");
	if (! argv[0]) {
	    argv[0] = STRAPS;
	}
	argv[0] = ckstrdup(argv[0]);
    }

    code = Tcl_CreatePipeline(interp, argc, argv, &pidArray, 
			      NULL, NULL, NULL);
    if (code != 1) {
        ckfree(argv[0]);
        argv[0] = NULL;
	return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 * DumpPacket() prints a hex dump of a packet. Useful for debugging 
 * this code.  The message given in the third parameter should be used 
 * to identify the received packet. The fourth parameter identifies 
 * the address and port of the sender.
 */

static void
DumpPacket (packet, packetlen, msg, addr)
     u_char *packet;
     int packetlen;
     char *msg;
     struct sockaddr_in *addr;
{
    u_char *cp = packet;
    int	i = 0;

    if (msg) {
	if (addr->sin_addr.s_addr)
	  printf ("%s %3d bytes [%s:%u]:\n  ", msg, packetlen,
		  inet_ntoa (addr->sin_addr), ntohs (addr->sin_port));
	else
	  printf ("%s %3d bytes:\n  ", msg, packetlen);
    }

    while (i++ < packetlen) {
	printf ("%02x", *cp++);
	
	if (i++ < packetlen)
	  printf ("%02x ", *cp++);
	
	if ((i % 16) == 0 && i < packetlen)
	  printf ("\n  ");
    }
    printf ("\n");
}

/*
 * SNMP_ManagerSocket() opens a UDP-Port for the sessions and
 * associates the processing function with that file handle for
 * asychronous response handling.
 */

int
SNMP_ManagerSocket (interp)
     Tcl_Interp *interp;
{
    struct sockaddr_in name;

    if (sock > 0) {
	return TCL_OK;
    }

    if ((sock = socket (AF_INET, SOCK_DGRAM, 0)) < 0) {
        Tcl_AppendResult (interp, "can not create socket: ", 
			  Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }
    
    name.sin_family = AF_INET;
    name.sin_port = 0;
    name.sin_addr.s_addr = INADDR_ANY;
    
    if (bind (sock, (struct sockaddr *) &name, sizeof(name)) < 0) {
        Tcl_AppendResult (interp, "can not bind socket: ", 
			  Tcl_PosixError (interp), (char *) NULL);
	close (sock);
	sock = -1;
	return TCL_ERROR;
    }
    
    Tk_CreateFileHandler (sock, TK_READABLE, ResponseProc, 
			  (ClientData *) interp);
    return TCL_OK;
}

/*
 * SNMP_ManagerClose() closes the socket used to handle manager
 * initiated SNMP communication.
 */

void
SNMP_ManagerClose ()
{
    Tk_DeleteFileHandler (sock);
    close (sock);
    sock = -1;
}

/*
 * SNMP_Wait() waits for an answer. This is used to implement 
 * synchronous operations. The argument defines the number of
 * milliseconds to wait.
 */

int
SNMP_Wait (ms)
     int ms;
{
    struct timeval wait;
    int width = sock + 1;
    fd_set readfds;

    wait.tv_sec  = ms / 1000;
    wait.tv_usec = (ms % 1000) * 1000;
    FD_ZERO (&readfds);
    FD_SET (sock, &readfds);

    return select (width, &readfds, (fd_set *)0, (fd_set *)0, &wait);
}

/*
 * SNMP_TrapSocket() creates a socket to the straps trap daemon. 
 */

int
SNMP_TrapSocket (interp)
     Tcl_Interp *interp;
{
    int i, rc;
    struct sockaddr_un saddr;
    int slen;

    if (trap_sock >= 0) {
	return TCL_OK;
    }
    
    if ((trap_sock = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
	Tcl_AppendResult (interp, "can not create straps socket: ",
			  Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }
    
    memset ((char *) &saddr, 0, sizeof(saddr));
    
    saddr.sun_family = AF_UNIX;
    strcpy (saddr.sun_path, serv_path);
    slen = sizeof(saddr) - sizeof(saddr.sun_path) + strlen(saddr.sun_path);
    
    if (connect (trap_sock, (struct sockaddr *) &saddr, slen) < 0) {
	
	if (straps (interp) != TCL_OK) return TCL_ERROR;
	
	for (i = 0; i < 5; i++) {
	    sleep (1);
	    rc = connect (trap_sock, (struct sockaddr *) &saddr, slen);
	    if (rc >= 0) break;
	}
	
	if (rc < 0) {
	    Tcl_AppendResult (interp, "can not connect straps socket: ",
			      Tcl_PosixError (interp), (char *) NULL);
	    close (trap_sock);
	    trap_sock = -1;    
	    return TCL_ERROR;
	}
    }

#ifdef SIGPIPE
    signal (SIGPIPE, ign_pipe);
#endif

    Tk_CreateFileHandler (trap_sock, TK_READABLE, TrapProc,
			  (ClientData *) interp);
    return TCL_OK;
}

/*
 * SNMP_TrapClose() closes the connection to the straps daemon.
 */

void
SNMP_TrapClose ()
{
    Tk_DeleteFileHandler (trap_sock);
    close (trap_sock);
    trap_sock = -1;    
}

/*
 * SNMP_AgentSocket() creates a socket for the agent part on a given 
 * port. If an agent socket is already created, we close the socket
 * and open a new one.
 */

int
SNMP_AgentSocket (interp, session)
     Tcl_Interp *interp;
     SNMP_Session *session;
{
    struct sockaddr_in name;
    SNMP_Session *s;
    
    if (session->agentSocket > 0) {
	Tk_DeleteFileHandler(session->agentSocket);
	close(session->agentSocket);
    }

    switch (session->version) {
#ifdef SNMPv2CLASSIC
      case SNMPv2CLASSIC:
	name.sin_port = htons(session->srcParty.TPort);
	break;
#endif
      default:
	name = session->tAddr;
	break;
    }
    name.sin_family = AF_INET;
    name.sin_addr.s_addr = INADDR_ANY;
    
    /*
     * Check if we can reuse a socket already opened by another
     * session. This allows to have more than one agent session
     * listening on a single transport endpoint.
     */

    for (s = sessionList; s && s->agentSocket > 0; s = s->nextPtr) {
	struct sockaddr_in sname;
	int namelen = sizeof(sname);
	if (getsockname(s->agentSocket, 
			(struct sockaddr *) &sname, &namelen) == -1) {
	    continue;
	}
	if (sname.sin_port == name.sin_port) {
	    session->agentSocket = s->agentSocket;
	    return TCL_OK;
	}
    }

    /*
     * Create a new socket for this transport endpoint and set up
     * a Tk filehandler to handle incoming messages.
     */

    if ((session->agentSocket = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        Tcl_AppendResult(interp, "can not create socket: ",
			 Tcl_PosixError(interp), (char *) NULL);
	session->agentSocket = 0; 
        return TCL_ERROR;
    }
    
    if (bind(session->agentSocket, (struct sockaddr *) &name, 
	     sizeof(name)) < 0) {
        Tcl_AppendResult(interp, "can not bind socket: ",
                         Tcl_PosixError(interp), (char *) NULL);
	close(session->agentSocket);
	session->agentSocket = 0;
        return TCL_ERROR;
    }

    Tk_CreateFileHandler(session->agentSocket, TK_READABLE, 
			 AgentProc, (ClientData *) session);

    return TCL_OK;
}

/*
 * SNMP_AgentClose() closes the socket for incoming requests.
 */

void
SNMP_AgentClose (session)
     SNMP_Session *session;
{
    struct sockaddr_in name;
    SNMP_Session *s;

    switch (session->version) {
#ifdef SNMPv2CLASSIC
      case SNMPv2CLASSIC:
	name.sin_port = htons(session->srcParty.TPort);
	break;
#endif
      default:
	name = session->tAddr;
	break;
    }
    name.sin_family = AF_INET;
    name.sin_addr.s_addr = INADDR_ANY;

    /*
     * Check if the socket is still in use by some other sessions.
     */

    for (s = sessionList; s && s->agentSocket > 0; s = s->nextPtr) {
	struct sockaddr_in sname;
	int namelen = sizeof(sname);
	if (getsockname(s->agentSocket, 
			(struct sockaddr *) &sname, &namelen) == -1) {
	    continue;
	}
	if (s != session && sname.sin_port == name.sin_port) {
	    session->agentSocket = 0;
	    return;
	}
    }

    /*
     * Still here? So we are the only one and should de-install the
     * file handler and close the socket.
     */

    Tk_DeleteFileHandler(session->agentSocket);
    close(session->agentSocket);
    session->agentSocket = 0;
}

/*
 * SNMP_Send() takes a serialized packet pointed to by packet
 * and sends it to the destination defined in the socket descriptor of
 * this session.
 */

int
SNMP_Send (interp, packet, packetlen, to)
     Tcl_Interp	*interp;
     u_char *packet;
     int packetlen;
     struct sockaddr_in *to;
{
    if (sendto (sock, packet, packetlen, 0, 
		(struct sockaddr *) to, sizeof(*to)) < 0) {
        Tcl_AppendResult (interp, "sendto failed: ", 
			  Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    snmpStats.snmpOutPkts++;

    if (hexdump) {
	DumpPacket (packet, packetlen, "send", to);
    }

    return TCL_OK;
}

/*
 * SNMP_Recv() reads from the socket, if an incoming response is expected.
 */

int
SNMP_Recv (interp, packet, packetlen, from)
     Tcl_Interp	*interp;
     u_char	*packet;
     int	*packetlen;
     struct sockaddr_in *from;
{
    int	fromlen = sizeof (*from);

    if ((*packetlen = recvfrom (sock, packet, *packetlen, 0,
				(struct sockaddr *) from, &fromlen)) < 0) {
	Tcl_AppendResult (interp, "recvfrom failed: ",
			  Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    if (hexdump) {
	DumpPacket (packet, *packetlen, "recv", from);
    }

    return TCL_OK;
}

/*
 * TrapRecv() reads a trap message from the straps daemon socket
 * and returns the packet.
 */

static int
TrapRecv (interp, packet, packetlen, from)
     Tcl_Interp *interp;
     u_char     *packet;
     int        *packetlen;
     struct sockaddr_in *from;
{
    int len, rlen, four;
    char c;

    /* read the length of the trap packet and then the packet itself */

    if ((four = xread (trap_sock, (char *) &from->sin_addr.s_addr, 4)) < 0
	|| (four = xread (trap_sock, (char *) &from->sin_port, 2)) < 0
	|| (four = xread (trap_sock, (char *) &len, 4)) < 0
        || xread (trap_sock, packet, 
		  rlen = (len < *packetlen ? len : *packetlen)) < 0) {
	Tcl_AppendResult (interp, "read failed: ",
			  Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    if (four != 4 && four != 2) {
        Tk_DeleteFileHandler (trap_sock);
        close (trap_sock);
        trap_sock = -1;
	Tcl_AppendResult (interp, "lost connection to straps daemon");
	return TCL_ERROR;
    }

    /* eat up remaining data-bytes */

    while (len > *packetlen) {
        if (xread (trap_sock, &c, 1) != 1) {
	    Tcl_AppendResult (interp, "lost connection to straps daemon");
	    return TCL_ERROR;
	}
        len--;
    }

    *packetlen = rlen;

    if (hexdump) {
	DumpPacket (packet, *packetlen, "recv", from);
    }

    return TCL_OK;
}

/*
 * AgentRecv() reads from the socket, if an incoming request 
 * is expected.
 */

static int
AgentRecv (interp, session, packet, packetlen, from)
     Tcl_Interp	*interp;
     SNMP_Session *session;
     u_char *packet;
     int *packetlen;
     struct sockaddr_in *from;
{
    int	fromlen = sizeof (*from);

    if ((*packetlen = recvfrom (session->agentSocket, packet, *packetlen, 0,
				(struct sockaddr *) from, &fromlen)) < 0) {
	Tcl_AppendResult (interp, "recvfrom failed: ",
			  Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    if (hexdump) {
	DumpPacket (packet, *packetlen, "recv", from);
    }

    return TCL_OK;
}

/*
 * SNMP_TimeoutProc() handles timeouts by retransmitting the message
 * or calling the error callback function if the number of retries
 * are already done.
 */

void
SNMP_TimeoutProc (clientData)
     ClientData	clientData;
{
    SNMP_Request *request = (SNMP_Request *) clientData;
    SNMP_Session *session = request->session;
    Tcl_Interp *interp = request->interp;
    
    if (request->retr_nr < session->retries) {
	
	/* 
	 * Reinstall TimerHandler for this request and retransmit
	 * this request (keeping the original oid).
	 */
	
        request->retr_nr += 1;
	request->timerToken = Tk_CreateTimerHandler(
				( session->timeout * 1000 ) / session->retries,
			 	SNMP_TimeoutProc,
				(ClientData *) request);
#ifdef SNMPv2USEC
	if (session->version == SNMPv2USEC && session->qos & USEC_QOS_AUTH) {
	    SNMP_UsecAuthPacket (session, request->packet, request->packetlen);
	}
#endif
	SNMP_Send (interp, request->packet, request->packetlen, 
		   &session->tAddr);

    } else {

	/*
	 * # of retransmissions reached: Evaluate the callback to
	 * notify the application and delete this request. We fake
	 * an empty pdu structure to conform to the callback 
	 * conventions.
	 */
    
	SNMP_PDU _pdu;
	SNMP_PDU *pdu = &_pdu;

	memset ((char *) pdu, 0, sizeof(SNMP_PDU));
	pdu->request_id = request->reqid;
	pdu->error_status = -1;
	Tcl_DStringInit (&pdu->varbind);
	SNMP_EvalCallback (interp, session, pdu, request->cmd, 
			   NULL, NULL, NULL);

	SNMP_DeleteRequest (session, request);
	Tcl_ResetResult (interp);
    }
}

/*
 * ResponseProc() is called from the event dispatcher whenever a
 * response to a management message is received.
 */

static void
ResponseProc (clientData, mask)
     ClientData	clientData;
     int mask;
{
    Tcl_Interp *interp = (Tcl_Interp *) clientData;
    u_char packet[BUFSIZE];
    int code, packetlen = BUFSIZE;
    struct sockaddr_in from;

    Tcl_ResetResult (interp);
    code = SNMP_Recv (interp, packet, &packetlen, &from);
    if (code != TCL_OK) return;

    code = SNMP_Decode (interp, packet, packetlen, &from, NULL, NULL);
    if (code == TCL_ERROR) {
	Tcl_AddErrorInfo (interp, "\n    (snmp response event)");
        Tk_BackgroundError (interp);
    }
}

/*
 * TrapProc() is called from the event dispatcher when a
 * trap message is received.
 */

static void
TrapProc (clientData, mask)
     ClientData clientData;
     int mask;
{
    Tcl_Interp *interp = (Tcl_Interp *) clientData;
    u_char packet[BUFSIZE];
    int code, packetlen = BUFSIZE;
    struct sockaddr_in from;

    Tcl_ResetResult (interp);
    code = TrapRecv (interp, packet, &packetlen, &from);
    if (code != TCL_OK) return;

    code = SNMP_Decode (interp, packet, packetlen, &from, NULL, NULL);
    if (code == TCL_ERROR) {
	Tcl_AddErrorInfo (interp, "\n    (snmp trap event)");
        Tk_BackgroundError (interp);
    }
}

/*
 * AgentProc() is called whenever a request to the SNMP agent
 * side is received.
 */

static void
AgentProc (clientData, mask)
     ClientData clientData;
     int mask;
{
    SNMP_Session *session = (SNMP_Session *) clientData;
    Tcl_Interp *interp = session->agentInterp;
    u_char packet[BUFSIZE];
    int code, packetlen = BUFSIZE;
    struct sockaddr_in from;

    if (! interp) return;

    Tcl_ResetResult (interp);
    code = AgentRecv (interp, session, packet, &packetlen, &from);
    if (code != TCL_OK) return;
    
    code = SNMP_Decode (interp, packet, packetlen, &from, NULL, NULL);
    if (code == TCL_ERROR) {
	  Tcl_AddErrorInfo (interp, "\n    (snmp agent event)");
	  Tk_BackgroundError (interp);
    }
}
