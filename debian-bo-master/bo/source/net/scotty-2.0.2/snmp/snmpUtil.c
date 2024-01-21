/*
 * snmpUtil.c
 *
 * This file contains some utilities to manipulate request lists
 * that keep track of outstanding requests. It also contains a
 * wrapper around the MD5 digest algorithm and soem stuff to handle
 * SNMP bindings.
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
#include "md5.h"
#include <arpa/inet.h>

/*
 * FreeBSD doesn't define INADDR_LOOPBACK - only in rpc/types.h. That's
 * strange. So I provide the fallthrough definition below.
 */

#ifndef INADDR_LOOPBACK
#define INADDR_LOOPBACK		(u_long)0x7F000001
#endif

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
FreeRequest			_ANSI_ARGS_((SNMP_Request *request));


/* 
 * Flag that controls hexdump. See the watch command for its use.
 */

extern int	hexdump;

/*
 * The error codes as defined in RFC 1448.
 */

/*
 * The following table is used to convert error codes as defined in RFC 1448
 * to error strings and back. Access to the table is done by 
 * SNMP_Str2Err() and SNMP_Err2Str().
 */

struct ErrorType {
   char *name;
   int errno;
};

static struct ErrorType errorTable[] = 
{
    { "noError",		E_NOERROR		},
    { "tooBig",			E_TOOBIG		},
    { "noSuchName",		E_NOSUCHNAME		},
    { "badValue",		E_BADVALUE		},
    { "readOnly",		E_READONLY		},
    { "genErr",			E_GENERR		},
    { "noAccess",		E_NOACCESS		},
    { "wrongType",		E_WRONGTYPE		},
    { "wrongLength",		E_WRONGLENGTH		},
    { "wrongEncoding",		E_WRONGENCODING		},
    { "wrongValue",		E_WRONGVALUE		},
    { "noCreation",		E_NOCREATION		},
    { "inconsistentValue",	E_INCONSISTENTVALUE	},
    { "resourceUnavailable",	E_RESOURCEUNAVAILABLE	},
    { "commitFailed",		E_COMMITFAILED		},
    { "undoFailed",		E_UNDOFAILED		},
    { "authorizationError",	E_AUTHORIZATIONERROR	},
    { "notWritable",		E_NOTWRITABLE		},
    { "inconsistentName",	E_INCONSISTENTNAME	},
    { NULL }
};

/*
 * SNMP_Str2Err() converts an error message in str to the appropriate
 * SNMP error code. SNMP_Str2Err() returns -1 if str contains an unknown
 * error message.
 */

int
SNMP_Str2Err (str)
     char *str;
{
    struct ErrorType *typePtr;

    for (typePtr = errorTable; typePtr->name; typePtr++) {
	if (strcmp (str, typePtr->name) == 0) {
	    return typePtr->errno;
	}
    }
    return -1;
}

/*
 * SNMP_Err2Str() converts an SNMP error code into a readable message.
 * Unknown error code are mapped to the message `unknown'.
 */

char *
SNMP_Err2Str (err)
     int err;
{
    struct ErrorType *typePtr;

    for (typePtr = errorTable; typePtr->name; typePtr++) {
	if (typePtr->errno == err) {
	    return typePtr->name;
	}
    }

    return "unknown";
}

/*
 * The following table is used to convert event names to event token.
 * Access to the table is done by SNMP_Str2Event() and SNMP_Event2Str().
 */

struct EventType {
   char *name;
   int event;
};

static struct EventType eventTable[] =
{
   { "get",		SNMP_GET_EVENT 		},
   { "set",		SNMP_SET_EVENT 		},
   { "create",		SNMP_CREATE_EVENT	},
   { "trap",		SNMP_TRAP_EVENT		},
   { "inform",		SNMP_INFORM_EVENT	},
   { "check",		SNMP_CHECK_EVENT	},
   { "commit",		SNMP_COMMIT_EVENT	},
   { "rollback",	SNMP_ROLLBACK_EVENT 	},
   { "begin",		SNMP_BEGIN_EVENT 	},
   { "end",		SNMP_END_EVENT		},
   { "recv",		SNMP_RECV_EVENT		},
   { "send",		SNMP_SEND_EVENT		},
   { NULL }
};


/*
 * SNMP_Str2Event() converts an event name in string to the appropriate
 * SNMP event code. SNMP_Str2Event() returns 0 if string contains an 
 * unknown error message.
 */

int
SNMP_Str2Event (string)
     char *string;
{
    struct EventType *typePtr;

    for (typePtr = eventTable; typePtr->name; typePtr++) {
	if (strcmp (string, typePtr->name) == 0) {
	    return typePtr->event;
	}
    }
    
    return 0;
}


/*
 * SNMP_Event2Str() converts an SNMP event code into the event name.
 * Unknown event codes will result in a NULL pointer.
 */

char *
SNMP_Event2Str (event)
     int event;
{
    struct EventType *typePtr;

    for (typePtr = eventTable; typePtr->name; typePtr++) {
	if (typePtr->event == event) {
	    return typePtr->name;
	}
    }
    
    return NULL;
}


/*
 * SNMP_EvalCallback() evaluates a Tcl callback. The command string
 * is modified according to the % escapes before evaluation. The list
 * of supported escapes is %R = request id, %S = session name, %E =
 * error status, %I = error index, %V = varbindlist and %A the agent 
 * address. There are three more escapes for instance bindings: 
 * %o = object identifier of instance, %i = instance identifier, 
 * %v = value.
 */

int
SNMP_EvalCallback (interp, session, pdu, cmd, instance, instOid, value)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *pdu;
     char *cmd;
     char *instance;
     char *instOid;
     char *value;
{
    char buf[20];
    int	code;
    Tcl_DString tclCmd;
    char *startPtr, *scanPtr;

    Tcl_DStringInit (&tclCmd);
    startPtr = cmd;
    for (scanPtr = startPtr; *scanPtr != '\0'; scanPtr++) {
	if (*scanPtr != '%') {
	    continue;
	}
	Tcl_DStringAppend (&tclCmd, startPtr, scanPtr - startPtr);
	scanPtr++;
	startPtr = scanPtr + 1;
	switch (*scanPtr) {
	  case 'R':  
	    sprintf (buf, "%d", pdu->request_id);
	    Tcl_DStringAppend (&tclCmd, buf, -1);
	    break;
	  case 'S':
	    if (session) {
		Tcl_DStringAppend (&tclCmd, session->name, -1);
	    }
	    break;
	  case 'V':
	    Tcl_DStringAppend (&tclCmd, Tcl_DStringValue (&pdu->varbind), -1);
	    break;
	  case 'E':
	    if (pdu->error_status < 0) {
		Tcl_DStringAppend (&tclCmd, "noResponse", 10);
	    } else {
		Tcl_DStringAppend (&tclCmd, 
				   SNMP_Err2Str (pdu->error_status), -1);
	    }
	    break;
	  case 'I':
	    sprintf (buf, "%d", pdu->error_index);
	    Tcl_DStringAppend (&tclCmd, buf, -1);
	    break;
	  case 'A':
	    Tcl_DStringAppend (&tclCmd, inet_ntoa(pdu->addr.sin_addr), -1);
	    break;
	  case 'P':
	    sprintf (buf, "%u", ntohs (pdu->addr.sin_port));
	    Tcl_DStringAppend (&tclCmd, buf, -1);
	    break;
	  case 'T':
	    switch (pdu->type) {
	      case SNMP_GET:
		Tcl_DStringAppend (&tclCmd, "get", -1);
		break;
	      case SNMP_GETNEXT:
		Tcl_DStringAppend (&tclCmd, "getnext", -1);
		break;
	      case SNMP_RESPONSE:
		Tcl_DStringAppend (&tclCmd, "response", -1);
		break;
	      case SNMPv1_TRAP:
	      case SNMPv2_TRAP:
		Tcl_DStringAppend (&tclCmd, "trap", -1);
		break;
	      case SNMP_SET:
		Tcl_DStringAppend (&tclCmd, "set", -1);
		break;
	      case SNMPv2_GETBULK:
		Tcl_DStringAppend (&tclCmd, "getbulk", -1);
		break;
	      case SNMPv2_INFORM:
		Tcl_DStringAppend (&tclCmd, "inform", -1);
		break;
	      case SNMPv2_REPORT:
		Tcl_DStringAppend (&tclCmd, "report", -1);
		break;
	    }
            break;
	  case 'o':
	    if (instance) {
		Tcl_DStringAppend (&tclCmd, instance, -1);
	    }
	    break;
	  case 'i':
	    if (instOid) {
		Tcl_DStringAppend (&tclCmd, instOid, -1);
	    }
	    break;
	  case 'v':
	    if (value) {
		Tcl_DStringAppend (&tclCmd, value, -1);
	    }
	    break;
	  case '%':
	    Tcl_DStringAppend (&tclCmd, "%", -1);
	    break;
	  default:
	    sprintf (buf, "%%%c", *scanPtr);
	    Tcl_DStringAppend (&tclCmd, buf, -1);
	}
    }
    Tcl_DStringAppend (&tclCmd, startPtr, scanPtr - startPtr);
    
    /*
     * Now evaluate the callback function and issue a background
     * error if the callback fails for some reason. Return the
     * original error message and code to the caller.
     */
    
    Tcl_AllowExceptions (interp);
    code = Tcl_GlobalEval (interp, Tcl_DStringValue (&tclCmd));
    Tcl_DStringFree (&tclCmd);

    /*
     * Call the usual error handling proc if we have evaluated
     * a binding not bound to a specific instance. Bindings 
     * bound to an instance are usually called during PDU 
     * processing where it is important to get the error message
     * back.
     */

    if (code == TCL_ERROR && instOid == NULL) {
	char *errorMsg = ckstrdup (interp->result);
	Tcl_AddErrorInfo (interp, "\n    (snmp callback)");
        Tk_BackgroundError (interp);
	Tcl_SetResult (interp, errorMsg, TCL_DYNAMIC);
    }
    
    return code;
}


/*
 * SNMP_EvalBinding() checks for events that are not bound to an
 * instance, such as SNMP_BEGIN_EVENT and SNMP_END_EVENT.
 */

int
SNMP_EvalBinding (interp, session, pdu, event)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *pdu;
     int event;
{
    int code = TCL_OK;
    SNMP_Binding *bindPtr = session->bindPtr;
    
    while (bindPtr) {
	if (bindPtr->event == event) break;
	bindPtr = bindPtr->nextPtr;
    }

    if (bindPtr && bindPtr->command) {
	code = SNMP_EvalCallback(interp, session, pdu, bindPtr->command,
				 NULL, NULL, NULL);
    }

    return code;
}


/*
 * SNMP_DumpPDU() dumps the contents of a pdu to standard output. This
 * is just a debugging aid.
 */

void
SNMP_DumpPDU (interp, pdu)
     Tcl_Interp *interp;
     SNMP_PDU *pdu;
{
    if (hexdump) {

        int i, code, argc;
	char **argv;
	char *name;

	switch (pdu->type) {
	  case SNMP_GET:
	    name = "get-request"; break;
	  case SNMP_GETNEXT:
	    name = "get-next-request"; break;
	  case SNMP_RESPONSE:
	    name = "response"; break;
	  case SNMPv1_TRAP:
	    name = "snmpV1-trap"; break;
	  case SNMP_SET :
	    name = "set-request"; break;
	  case SNMPv2_GETBULK:
	    name = "get-bulk-request"; break;
	  case SNMPv2_INFORM:
	    name = "inform-request"; break;
	  case SNMPv2_TRAP:
	    name = "snmpV2-trap"; break;
	  case SNMPv2_REPORT:
	    name = "report"; break;
	  default:
	    name = "unknown-request";
	}
	
	if (pdu->type == SNMPv2_GETBULK) {
	    printf ("%s %d non-repeaters %d max-repetitions %d\n", 
		    name, pdu->request_id,
		    pdu->error_status, pdu->error_index);
	} else if (pdu->type == SNMPv1_TRAP) {
	    printf ("%s\n", name);
	} else if (pdu->error_status == E_NOERROR) {
	    printf ("%s %d %s\n", name, pdu->request_id,
		    SNMP_Err2Str(pdu->error_status));
	} else {
	    printf ("%s %d %s at %d\n", name, pdu->request_id, 
		    SNMP_Err2Str(pdu->error_status), pdu->error_index);
	}

	code = Tcl_SplitList (interp, Tcl_DStringValue(&pdu->varbind), 
			      &argc, &argv);
	if (code == TCL_OK) {
	    for (i = 0; i < argc; i++) {
		printf ("%4d.\t%s\n", i+1, argv[i]);
	    }
	    ckfree ((char *) argv);
	}
	Tcl_ResetResult (interp);
    }
}


/*
 * SNMP_MD5_digest() computes the message digest of the given packet.
 * It is based on the MD5 implementation of RFC 1321.
 */

void
SNMP_MD5_digest (packet, length, digest)
     u_char *packet;
     int length;
     u_char *digest;
{
    MDstruct MD;
    int i, j;
    u_char buf[BUFSIZE], *cp;

    /*
     * As the computation has side effects on machines with LOWBYTEFIRST,
     * do the computation in a static array.
     */

    memcpy ((char *) buf, (char *) packet, length);
    cp = buf;

    MDbegin (&MD);
    while (length > 64) {
        MDupdate (&MD, cp, 512);
	cp += 64;
	length -= 64;
    }
    MDupdate (&MD, cp, length * 8);

    if (hexdump) {
	printf ("MD5 Digest: "); MDprint(&MD); printf ("\n");
    }

    cp = digest;
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 32; j += 8)
	  *cp++ = (MD.buffer[i] >> j) & 0xFF;
    }
}


#ifdef SNMPv2USEC
/*
 * SNMP_Passwd2Key() converts a 0 terminated password string into
 * a 16 byte MD5 key. This is a slighly modified version taken from
 * the USEC proposal.
 */

void 
SNMP_Passwd2Key (passwd, key)
     u_char *passwd;
     u_char *key;
{
    MDstruct    MD;
    u_char      *cp, password_buf[64];
    u_long      password_index = 0;
    u_long      i, j, count = 0;
    int		len = strlen (passwd);

    if (len == 0) {
	memset (key, '\0', 16);
	return;
    }
    
    MDbegin(&MD);   /* initialize MD5 */
    
    /* loop until we've done 1 Megabyte */
    while (count < 1048576) {
        cp = password_buf;
        for(i = 0; i < 64; i++){
            *cp++ = passwd[ password_index++ % len ];
            /*
             * Take the next byte of the password, wrapping to the
             * beginning of the password as necessary.
             */
        }
	
        MDupdate(&MD, password_buf, 64 * 8);
        /*
         * 1048576 is divisible by 64, so the last MDupdate will be
         * aligned as well.
         */
        count += 64;
    }
    MDupdate(&MD, password_buf, 0); /* tell MD5 we're done */

    if (hexdump) {
	printf ("MD5 Digest: "); MDprint(&MD); printf ("\n");
    }

    cp = key;
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 32; j += 8)
	  *cp++ = (MD.buffer[i] >> j) & 0xFF;
    }
    *cp = '\0';
}
#endif


/*
 * SNMP_MallocSession() allocates a SNMP_Session structure and 
 * initializes all pointers. The functions returns a pointer to 
 * the SNMP_Session structure.
 */

SNMP_Session*
SNMP_MallocSession ()
{
    SNMP_Session *session;
    
    session = (SNMP_Session *) ckalloc (sizeof (SNMP_Session));
    memset ((char *) session, '\0', sizeof (SNMP_Session));

    session->tAddr.sin_family	   = AF_INET;
    session->tAddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    session->tAddr.sin_port        = htons (SNMP_PORT);
    session->version		= SNMPv1;
    session->community		= ckstrdup( "public" );
#ifdef SNMPv2CLASSIC
    session->dstParty.TAddress	= ckstrdup ("127.0.0.1");
    session->dstParty.TPort	= SNMP_PORT;
    session->srcParty.TAddress	= ckstrdup ("127.0.0.1");
    session->srcParty.TPort	= SNMP_PORT;
#endif
    session->reqid		= rand();
    session->retries		= SNMP_RETRIES;
    session->timeout		= SNMP_TIMEOUT;
    session->window 		= SNMP_WINDOW;

    return session;
}


/*
 * SNMP_FreeSession() frees the memory allocated by a SNMP_Session
 * and all it's associated structures.
 */

void
SNMP_FreeSession (session)
     SNMP_Session *session;
{
    SNMP_Request *request;
    
    if (session != NULL) {

	while (session->requestList != NULL) {
	    request = session->requestList;
	    session->requestList = session->requestList->nextPtr;
	    FreeRequest (request);
	}

	if (session->community) ckfree (session->community);

#ifdef SNMPv2CLASSIC
	if (session->dstParty.Identity) {
	    ckfree ((char *) session->dstParty.Identity);
	}
	if (session->dstParty.TDomain) ckfree (session->dstParty.TDomain);
	if (session->dstParty.TAddress) ckfree (session->dstParty.TAddress);

	if (session->srcParty.Identity) {
	    ckfree ((char *) session->srcParty.Identity);
	}
	if (session->srcParty.TDomain) ckfree (session->srcParty.TDomain);
	if (session->srcParty.TAddress) ckfree (session->srcParty.TAddress);
#endif

	while (session->bindPtr) {
	    SNMP_Binding *bindPtr = session->bindPtr;
	    session->bindPtr = bindPtr->nextPtr;
	    if (bindPtr->command) {
		ckfree (bindPtr->command);
	    }
	    ckfree ((char *) bindPtr);
	}

	if (session->agentSocket) SNMP_AgentClose (session);
	ckfree ((char *) session);
    }
}


/*
 * FreeRequest() frees the memory allocated by a SNMP_Request structure.
 */

static void
FreeRequest (request)
     SNMP_Request *request;
{

    if (request != NULL) {
	if (request->timerToken) {
	    Tk_DeleteTimerHandler (request->timerToken);
	}
	ckfree ((char *) request);
    }
}


/*
 * SNMP_RecordRequest() saves this message in the request list for the
 * given session, together with it's callback function, which is
 * executed when the response request is received from the agent.
 */

SNMP_Request*
SNMP_RecordRequest (interp, session, reqid, packet, packetlen, cmd)
     Tcl_Interp *interp;
     SNMP_Session *session;
     int reqid;
     u_char *packet;
     int packetlen;
     char *cmd;
{
    SNMP_Request *request;
    int cmdLen = cmd ? strlen (cmd) + 1 : 0;

    /*
     * Allocate a SNMP_Request structure together with some space to
     * hold the encoded packet and the command to evaluate. Allocating 
     * this in one ckalloc call simplyfies and thus speeds up memory 
     * management.
     */

    request = (SNMP_Request *) ckalloc (sizeof (SNMP_Request) 
					+ cmdLen + packetlen);
    memset ((char *) request, '\0', sizeof (SNMP_Request));
    if (cmd) {
	request->cmd = (char *) request + sizeof (SNMP_Request);
	strcpy (request->cmd, cmd);
    }
    request->packet = (u_char *) request + sizeof (SNMP_Request) + cmdLen;
    request->reqid = reqid;
    memcpy (request->packet, packet, packetlen);
    request->packetlen = packetlen;
    request->interp = interp;
    request->session = session;
    request->timerToken = Tk_CreateTimerHandler((session->timeout * 1000) 
						/ (session->retries + 1),
						SNMP_TimeoutProc,
						(ClientData *) request);
    
    /*
     * Append request to the list of requests for this session.
     */

    request->nextPtr = session->requestList;
    session->requestList = request;
    
    return request;
}


/*
 * SNMP_LookupRequest() walks through the request lists of the open
 * sessions and tries to find the given request_id. Returns a pointer
 * to the request structure, if the request is found. The session that
 * invoked the request, is returnd in req_sess.  If there is no
 * corresponding request to this id, NULL is returned. The calling
 * function has to decide how to handle this situation.
 */

SNMP_Request*
SNMP_LookupRequest (reqid)
     int reqid;
{
    SNMP_Session *session;
    SNMP_Request *request;

    for (session = sessionList; session; session = session->nextPtr) {
	request = session->requestList;
	while (request != NULL) {
	    if (request->reqid == reqid) {
		return request;
	    }
	    request = request->nextPtr;
	}
    }
    return NULL;
}


/*
 * SNMP_DeleteRequest() deletes the request pointed to by request from
 * the given session pointed to by session.
 */

void
SNMP_DeleteRequest (session, request)
     SNMP_Session *session;
     SNMP_Request *request;
{
    SNMP_Session *s;
    SNMP_Request *p, *q;
    
    /* 
     * Check whether the session is still in the session list.
     * We sometimes get called when the session has already been
     * destroyed as a side effect of evaluating callbacks.
     */
    
    for (s = sessionList; s; s = s->nextPtr) {
	if (s == session) break;
    }
    
    if (s != session) return;

    /*
     * Find the request in the requestList and remove it.
     */

    for (p = session->requestList, q = NULL; p != NULL; q = p,p = p->nextPtr) {
	if (p == request) break;
    }

    if (p == NULL) return;

    if (q == NULL) {
	session->requestList = p->nextPtr;
    } else {
	q->nextPtr = p->nextPtr;
    }
    
    FreeRequest (request); 
}


/*
 * SNMP_SplitVarBindList() splits a list of Tcl lists containing
 * varbinds (again Tcl lists) and returns an array of SNMP_VarBind
 * structures.
 */

int
SNMP_SplitVarBindList (interp, list, varBindSizePtr, varBindPtrPtr)
     Tcl_Interp *interp;
     char *list;
     int *varBindSizePtr;
     SNMP_VarBind **varBindPtrPtr;
{
    int code, vblc, i;
    char **vblv;
    int varBindSize;
    SNMP_VarBind *varBindPtr;

    code = Tcl_SplitList (interp, list, &vblc, &vblv);
    if (code != TCL_OK) {
        return TCL_ERROR;
    }

    /*
     * Allocate space for the varbind table. Note, we could reuse space
     * allocated from previous runs to avoid all the malloc and free
     * operations. For now, we go the simple way.
     */

    varBindSize = vblc;
    varBindPtr = (SNMP_VarBind *) ckalloc (varBindSize * sizeof(SNMP_VarBind));
    memset (varBindPtr, 0, varBindSize * sizeof(SNMP_VarBind));

    for (i = 0; i < varBindSize; i++) {
        int vbc;
        char **vbv;
        code = Tcl_SplitList (interp, vblv[i], &vbc, &vbv);
	if (code != TCL_OK) {
	    SNMP_FreeVarBindList (varBindSize, varBindPtr);
	    ckfree ((char *) vblv);
	    return TCL_ERROR;
	}
	if (vbc > 0) {
	    varBindPtr[i].soid = vbv[0];
	    if (vbc > 1) {
		varBindPtr[i].syntax = vbv[1];
		if (vbc > 2) {
		    varBindPtr[i].value = vbv[2];
		}
	    }
	}
	varBindPtr[i].freePtr = (char *) vbv;
    }

    *varBindSizePtr = varBindSize;
    *varBindPtrPtr = varBindPtr;
    ckfree ((char *) vblv);
    return TCL_OK;
}


/*
 * SNMP_MergeVarBindList() merges an array of SNMP_VarBind structures
 * into a corresponding Tcl list. It returns a pointer to a malloced
 * buffer.  
 */

char*
SNMP_MergeVarBindList (varBindSize, varBindPtr)
     int varBindSize;
     SNMP_VarBind *varBindPtr;
{
    static Tcl_DString list;
    int i;

    Tcl_DStringInit (&list);

    for (i = 0; i < varBindSize; i++) {
        Tcl_DStringStartSublist (&list);
	Tcl_DStringAppendElement (&list, 
			     varBindPtr[i].soid ? varBindPtr[i].soid : "");
	Tcl_DStringAppendElement (&list, 
			     varBindPtr[i].syntax ? varBindPtr[i].syntax : "");
	Tcl_DStringAppendElement (&list, 
			     varBindPtr[i].value ? varBindPtr[i].value : "");
	Tcl_DStringEndSublist (&list);
    }

    return ckstrdup (Tcl_DStringValue (&list));
}


/*
 * SNMP_FreeVarBindList() frees the array of SNMP_VarBind structures
 * and all the varbinds stored in the array.
 */

void
SNMP_FreeVarBindList (varBindSize, varBindPtr)
     int varBindSize;
     SNMP_VarBind *varBindPtr;
{
    int i;
    
    for (i = 0; i < varBindSize; i++) {
	if (varBindPtr[i].freePtr) {
	    ckfree (varBindPtr[i].freePtr);
	}
    }

    ckfree ((char *) varBindPtr);
}


/*
 * Return the uptime of this agent in hundreds of seconds. Should
 * be initialzed when registering the SNMP extension.
 */

int
SNMP_SysUpTime ()
{
    static time_t boottime = 0;

    if (!boottime) {
	boottime = time ((time_t *) NULL);
	return 0;
    } else {
	return (time ((time_t *) NULL) - boottime) * 100;
    }
}


/*
 * SNMP_BinToHex() converts the binary buffer s with len n into readable
 * format (eg: 1A:2B:3D). The result is returned in d (with trailing
 * '\0') 
 */

void
SNMP_BinToHex (s, n, d)
     char *s;
     int n;
     char *d;
{
    while (n-- > 0) {
	char c = *s++;
	int c1 = (c & 0xf0) >> 4;
	int c2 = c & 0x0f;
	if ((c1 += '0') > '9') c1 += 7;
	if ((c2 += '0') > '9') c2 += 7;
	*d++ = c1, *d++ = c2;
	if (n > 0)
	  *d++ = ':';
    }
    *d = 0;
}


/*
 * SNMP_HexToBin() converts the readable hex buffer s to pure binary
 * octets into buffer d and return the length in n.
 */

int
SNMP_HexToBin (s, d, n)
     char *s, *d;
     int *n;
{
    int v;
    char c;

    *n = 0;

    while (s[0] && s[1]) {
	c = *s++ & 0xff;
	if (! isxdigit(c)) return -1;
	v = c >= 'a' ?  c - 87 : (c >= 'A' ? c - 55 : c - 48);
	c = *s++ & 0xff;
	if (! isxdigit(c)) return -1;
	v = (v << 4) + (c >= 'a' ?  c - 87 : (c >= 'A' ? c - 55 : c - 48));
	*d++ = v;
	(*n)++;
	if (*s && *s++ != ':') return -1;
    }

    return *n;
}
