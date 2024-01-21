/*
 * snmp.h
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

#ifndef _SNMP_H
#define _SNMP_H

#include <scotty.h>

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <tcl.h>
#include <tk.h>

#include "asn1.h"

/*
 * Some general defines. Note, that all SNMPv2 versions that we wil
 * ever implement should have the some SNMPv2 bit set, so that we
 * can and a version number with SNMPv2 to check if we allow SNMPv2
 * protocol operations.
 *
 * You can turn off support for SNMPv2CLASSIC or SNMPv2USEC by 
 * undefining these symbols. This will make the code a bit smaller,
 * but you won't gain much speed. So you might want to let these
 * defines as they are.
 */

#define SNMPv1			0x11
#define SNMPv2			0x20
#define SNMPv2CLASSIC		0x21
#define SNMPv2USEC		0x22
#define SNMPv2C			0x23

#define	SNMP_PORT		161
#define SNMP_RETRIES		3
#define SNMP_TIMEOUT		5
#define SNMP_WINDOW		8

#define MD5_SIZE		16

#ifdef SNMPv2CLASSIC
#define NO_AUTH			0
#define MD5_AUTH		1
#define NO_PRIV			0
#define DES_PRIV		1
#endif

/*
 * The PDU formats as defined in RFC 1157 and RFC 1448. I also
 * added the REPORT PDU current under discussion in the SNMPv2 WG.
 */

#define SNMP_GET		0
#define SNMP_GETNEXT		1
#define SNMP_RESPONSE		2
#define	SNMP_SET		3
#define SNMPv1_TRAP		4
#define SNMPv2_GETBULK		5
#define SNMPv2_INFORM		6
#define SNMPv2_TRAP		7
#define SNMPv2_REPORT		8

/*
 * Error status definitions as of RFC 1448 and RFC 1157. Note, that
 * RFC 1157 only defines the errors NOERROR - GENERR. Some of the
 * version 1 error codes are only for V1/V2 proxy compatibility.
 */

#define E_NOERROR		0	/* v1/v2 error code */
#define E_TOOBIG		1	/* v1/v2 error code */
#define E_NOSUCHNAME		2	/* v1    error code */
#define E_BADVALUE		3	/* v1    error code */
#define E_READONLY		4	/* v1    error code */
#define E_GENERR		5	/* v1/v2 error code */
#define E_NOACCESS		6	/*    v2 error code */
#define E_WRONGTYPE		7	/*    v2 error code */
#define E_WRONGLENGTH		8	/*    v2 error code */
#define E_WRONGENCODING		9	/*    v2 error code */
#define E_WRONGVALUE		10	/*    v2 error code */
#define E_NOCREATION		11	/*    v2 error code */
#define E_INCONSISTENTVALUE	12	/*    v2 error code */
#define E_RESOURCEUNAVAILABLE	13	/*    v2 error code */
#define E_COMMITFAILED		14	/*    v2 error code */
#define E_UNDOFAILED		15	/*    v2 error code */
#define E_AUTHORIZATIONERROR	16	/*    v2 error code */
#define E_NOTWRITABLE		17	/*    v2 error code */
#define E_INCONSISTENTNAME	18	/*    v2 error code */

/*
 * The following functions provide a way to map SNMP error codes
 * to readable names and vice versa.
 */

EXTERN int
SNMP_Str2Err		_ANSI_ARGS_((char *str));

EXTERN char *
SNMP_Err2Str		_ANSI_ARGS_((int err));

/*
 * Some definitions for the User based Security model (USEC).
 */

#ifdef SNMPv2USEC
#define USEC_QOS_NULL		(0x00)
#define USEC_QOS_AUTH		(0x01)
#define USEC_QOS_PRIV		(0x02)
#define USEC_QOS_MAINT		(0x80)
#define USEC_QOS_AUTHPRIV	(0x03)

#define USEC_MODEL		0
#define USEC_MAX_USER		32
#define USEC_MAX_CONTEXT	64
#define USEC_MAX_AGENTID	12
#define USEC_MAX_MMS		65507
#define USEC_MIN_MMS		484
#endif

#ifdef SNMPv2CLASSIC

/*
 * Party and context structure based on RFC 1445 and RFC 1446.
 */

typedef struct SNMP_Party {
   ASN1_OID		*Identity;
   int			IdentityLen;
   char			*TDomain;
   char			*TAddress;
   int			TPort;
   int			MaxMessageSize;
   int			AuthProtocol;
   u_int		AuthClock;
   u_char		AuthPrivate[MD5_SIZE];
   u_char		AuthPublic[33];
   u_int		AuthLifetime;
   int			PrivProtocol;
   u_char		PrivPrivate[MD5_SIZE];
   u_char		PrivPublic[33];
} SNMP_Party;

typedef struct SNMP_Context {
   ASN1_OID		*Identity;
   int			IdentityLen;
} SNMP_Context;

#endif

/*
 * A session structure contains all infomation needed for agent < - >
 * manager communication.  
 */

typedef struct SNMP_Session {
   char			name[20];       /* session name                     */
   struct sockaddr_in	tAddr;		/* the target address		    */
   int			version;	/* SNMP version bits		    */
   char			*community;	/* community string (SNMPv1)	    */
#ifdef SNMPv2CLASSIC
   SNMP_Party		dstParty;	/* party that agent acts at	    */
   SNMP_Party		srcParty;	/* party that manager acts at	    */
   SNMP_Context		context;	/* chosen context for session	    */
#endif
#ifdef SNMPv2USEC
   u_char		qos;
   u_char		agentID[USEC_MAX_AGENTID];
   u_int		agentBoots;
   u_int		agentTime;
   int			userNameLen;
   char			userName[USEC_MAX_USER];
   u_char		authKey[MD5_SIZE];
   u_char		privKey[MD5_SIZE];
   int			cntxtLen;
   char			cntxt[USEC_MAX_CONTEXT];
   int			maxSize;
#endif
   int			retries;	/* Number of retries		    */
   u_int		timeout;	/* Length to wait for timeout	    */
   int			window;		/* The max. # of async. requests    */
   u_int		reqid;		/* next request to use		    */
   struct SNMP_Binding	*bindPtr;	/* commands bound to this session   */
   struct SNMP_Request	*requestList; 	/* outstanding requests		    */
   int			agentSocket;	/* socket used if we are an agent   */
   Tcl_Interp		*agentInterp;   /* the interp used by the agent	    */
   struct SNMP_Instance *instPtr;       /* the tree of MIB instances        */
   struct SNMP_Session	*nextPtr;	/* pointer to next session	    */
} SNMP_Session;

/*
 * Global variable that points to a list of all known sessions.
 */

EXTERN SNMP_Session *sessionList;

/*
 * Structure to hold a varbind. Lists of varbinds are mapped to 
 * arrays of varbinds as this simplifies memory management.
 */

typedef struct SNMP_VarBind {
    char *soid;
    char *syntax;
    char *value;
    char *freePtr;
    ClientData clientData;
} SNMP_VarBind;


EXTERN void
SNMP_FreeVarBindList	_ANSI_ARGS_((int varBindSize, 
				     SNMP_VarBind *varBindPtr));
EXTERN int
SNMP_SplitVarBindList	_ANSI_ARGS_((Tcl_Interp *interp, char *list,
				     int *varBindSizePtr, 
				     SNMP_VarBind **varBindPtrPtr));
EXTERN char*
SNMP_MergeVarBindList	_ANSI_ARGS_((int varBindSize, 
				     SNMP_VarBind *varBindPtr));

/*
 * Structure for the SNMP PDU format as defined in RFC 1445.
 */

typedef struct SNMP_PDU {
    struct sockaddr_in addr;	/* The address from/to it is sent.     */
    int type;			/* The type of this PDU.               */
    int request_id;		/* A unique request id for this PDU.   */
    int error_status;		/* The SNMP error status field.        */
    int error_index;		/* The SNMP error index field.         */
    char *trapOID;		/* Trap object identifier.             */
#if 1
    Tcl_DString varbind;	/* The list of varbinds as Tcl string. */
#else
    SNMP_VarBind *varbind;	/* The list of varbinds as an array.   */
#endif
} SNMP_PDU;

/*
 * Structure to describe an outstanding asynchronous request.
 */

typedef struct SNMP_Request {
   int			reqid;		/* Request id			    */
   int			retr_nr;        /* Number of retransmissions        */
   u_char		*packet;
   int			packetlen;
   Tk_TimerToken	timerToken;	/* Token of the Tk Timer Handler    */
   Tcl_Interp		*interp;	/* Interpreter to eval callback     */
   char			*cmd;		/* Tcl command to process response  */
   struct SNMP_Session	*session;	/* The session for this request	    */
   struct SNMP_Request	*nextPtr;	/* next pending request		    */
} SNMP_Request;


EXTERN SNMP_Request*
SNMP_RecordRequest	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *sess,
				     int reqid, u_char *packet, int packetlen,
				     char *cmd));
EXTERN SNMP_Request*
SNMP_LookupRequest	_ANSI_ARGS_((int reqid));

EXTERN void
SNMP_DeleteRequest	_ANSI_ARGS_((SNMP_Session *session, 
				     SNMP_Request *request));


/*
 * The event types currently supported for SNMP bindings.
 * The functions below provide a way to map SNMP event codes
 * to readable names and vice versa.
 */

#define SNMP_NO_EVENT		0x0000
#define SNMP_GET_EVENT		0x0001
#define SNMP_SET_EVENT		0x0002
#define SNMP_CREATE_EVENT	0x0004
#define SNMP_TRAP_EVENT		0x0008
#define SNMP_INFORM_EVENT	0x0010
#define SNMP_CHECK_EVENT	0x0020
#define SNMP_COMMIT_EVENT	0x0040
#define SNMP_ROLLBACK_EVENT	0x0080
#define SNMP_BEGIN_EVENT	0x0100
#define SNMP_END_EVENT		0x0200
#define SNMP_RECV_EVENT		0x0400
#define SNMP_SEND_EVENT		0x0800

EXTERN int
SNMP_Str2Event		_ANSI_ARGS_((char *string));

EXTERN char *
SNMP_Event2Str		_ANSI_ARGS_((int event));

/*
 * A structure to describe a binding. Multiple bindings are organized
 * in a simple list as it is not too likely to have many bindings for
 * an object.
 */

typedef struct SNMP_Binding {
    int event;				/* Event that triggers binding. */
    char *command;			/* Tcl command to evaluate.     */
    struct SNMP_Binding *nextPtr;	/* Next binding in our list.    */
} SNMP_Binding;


EXTERN int
SNMP_EvalBinding	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
                                     SNMP_PDU *pdu, int event));

/*
 * Structure to describe an instance known to an agent.
 */

typedef struct SNMP_Instance {
    char *label;			/* The complete OID.	    */
    int offset;				/* Offset to instance id.   */
    int syntax;				/* Syntax string from MIB.  */
    int	access;				/* Access mode from MIB.    */
    char *tclVarName;			/* Tcl variable name.	    */
    SNMP_Binding *bindings;		/* List of bindings.        */ 
    u_int subid;			/* Sub identifier in Tree.  */
    struct SNMP_Instance *childPtr;	/* List of child nodes.	    */
    struct SNMP_Instance *nextPtr;	/* List of peer node.	    */
} SNMP_Instance;


EXTERN int
SNMP_CreateInst		_ANSI_ARGS_((Tcl_Interp *interp, char *id,
				     char *varName, char *defval));
EXTERN SNMP_Instance*
SNMP_FindInst		_ANSI_ARGS_((SNMP_Session *session,
				     ASN1_OID *oid, int len));

EXTERN SNMP_Instance*
SNMP_FindNextInst	_ANSI_ARGS_((SNMP_Session *session,
				     ASN1_OID *oid, int len));

EXTERN int
SNMP_CreateInstBinding	_ANSI_ARGS_((SNMP_Session *session,
				     ASN1_OID *oid, int len,
				     int event, char *command));

EXTERN char*
SNMP_GetInstBinding	_ANSI_ARGS_((SNMP_Session *session,
				     ASN1_OID *oid, int len, int event));

EXTERN int
SNMP_EvalInstBinding	_ANSI_ARGS_((SNMP_Session *session,
				     SNMP_PDU *pdu, SNMP_Instance *inst,
				     int operation, char *value));


/*
 * Structures to keep SNMP related statistics. See RFC 1213 and RFC 1450
 * for more details when these counters must be incremented.
 */

typedef struct SNMP_Statistics {
    /* RFC 1213 */
    u_int snmpInPkts;
    u_int snmpOutPkts;
    u_int snmpInBadVersions;
    u_int snmpInBadCommunityNames;
    u_int snmpInBadCommunityUses;
    u_int snmpInASNParseErrs;
    u_int snmpInTooBigs;
    u_int snmpInNoSuchNames;
    u_int snmpInBadValues;
    u_int snmpInReadOnlys;
    u_int snmpInGenErrs;
    u_int snmpInTotalReqVars;
    u_int snmpInTotalSetVars;
    u_int snmpInGetRequests;
    u_int snmpInGetNexts;
    u_int snmpInSetRequests;
    u_int snmpInGetResponses;
    u_int snmpInTraps;
    u_int snmpOutTooBigs;
    u_int snmpOutNoSuchNames;
    u_int snmpOutBadValues;
    u_int snmpOutGenErrs;
    u_int snmpOutGetRequests;
    u_int snmpOutGetNexts;
    u_int snmpOutSetRequests;
    u_int snmpOutGetResponses;
    u_int snmpOutTraps;
    u_int snmpEnableAuthenTraps;
    /* RFC 1450 XXX not yet implemented */
    u_int snmpStatsPackets;
    u_int snmpStats30Something;
    u_int snmpStatsEncodingErrors;
    u_int snmpStatsUnknownDstParties;
    u_int snmpStatsDstPartyMismatches;
    u_int snmpStatsUnknownSrcParties;
    u_int snmpStatsBadAuths;
    u_int snmpStatsNotInLifetimes;
    u_int snmpStatsWrongDigestValues;
    u_int snmpStatsUnknownContexts;
    u_int snmpStatsBadOperations;
    u_int snmpStatsSilentDrops;
    /* snmpV1BadCommunityNames is the same as snmpInBadCommunityNames */
    /* snmpV1BadCommunityUses  is the same as snmpInBadCommunityUses  */
#ifdef SNMPv2USEC
    u_int usecStatsUnsupportedQoS;
    u_int usecStatsNotInWindows;
    u_int usecStatsUnknownUserNames;
    u_int usecStatsWrongDigestValues;
    u_int usecStatsUnknownContextSelectors;
#endif
} SNMP_Statistics;

/*
 * Global variable used to gather the agent SNMP statistics.
 */

EXTERN SNMP_Statistics snmpStats;

/*
 * Exported SNMP procedures:
 */

EXTERN int
SNMP_Init		_ANSI_ARGS_((Tcl_Interp *interp));

EXTERN SNMP_Session*
SNMP_MallocSession	_ANSI_ARGS_((void));

EXTERN void
SNMP_FreeSession	_ANSI_ARGS_((SNMP_Session *session));

EXTERN int
SNMP_Encode		_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
				     SNMP_PDU *pdu, char *cmd));
EXTERN int
SNMP_Decode		_ANSI_ARGS_((Tcl_Interp *interp, 
				     u_char *packet, int packetlen,
				     struct sockaddr_in *from,
				     SNMP_Session *session, int *reqid));
EXTERN void
SNMP_TimeoutProc	_ANSI_ARGS_((ClientData clientData));

EXTERN void
SNMP_AgentInit		_ANSI_ARGS_((Tcl_Interp *interp));

EXTERN int
SNMP_AgentRequest	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
				     SNMP_PDU *pdu));
EXTERN int
SNMP_AgentWalk		_ANSI_ARGS_((Tcl_Interp *interp, char *varName,
				     char *body));
EXTERN int
SNMP_EvalCallback	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
				     SNMP_PDU *pdu,
				     char *cmd, char *instance,
				     char *instOid, char *value));
/*
 * The size of the internal buffer used to decode or assemble
 * SNMP packets.
 */

#define BUFSIZE	2048

/*
 * The following function is used to create a socket used for
 * all manager initiated communication. The Close function
 * is used to close this socket if all SNMP sessions have been
 * destroyed.
 */

EXTERN int
SNMP_ManagerSocket	_ANSI_ARGS_((Tcl_Interp	*interp));

EXTERN void
SNMP_ManagerClose	_ANSI_ARGS_((void));

/*
 * Management communication initiated by an agent is normally
 * done using traps. Because traps are sent to port 162 (which
 * requires special privileges, we read the from the straps
 * process via a unix domain socket.
 */

EXTERN int
SNMP_TrapSocket		_ANSI_ARGS_((Tcl_Interp *interp));

EXTERN void
SNMP_TrapClose		_ANSI_ARGS_((void));

/*
 * Create and close a socket used for the agent part. This socket
 * used to process messages received from other agents.
 */

EXTERN int
SNMP_AgentSocket	_ANSI_ARGS_((Tcl_Interp *interp, 
				     SNMP_Session *session));

EXTERN void
SNMP_AgentClose		_ANSI_ARGS_((SNMP_Session *session));

/*
 * Functions used to send and receive SNMP messages. The SNMP_Wait
 * function is used to wait for an answer.
 */

EXTERN int
SNMP_Send		_ANSI_ARGS_((Tcl_Interp *interp,
				     u_char *packet, int packetlen,
				     struct sockaddr_in *to));
EXTERN int
SNMP_Recv		_ANSI_ARGS_((Tcl_Interp *interp, 
				     u_char *packet, int *packetlen,
				     struct sockaddr_in *from));
EXTERN int 
SNMP_Wait		_ANSI_ARGS_((int ms));

/*
 * Some more utility functions.
 */

EXTERN void
SNMP_MD5_digest		_ANSI_ARGS_((u_char *packet, int packetlen,
				     u_char *digest));

#ifdef SNMPv2USEC
EXTERN void
SNMP_Passwd2Key		_ANSI_ARGS_((u_char *passwd, u_char *key));

EXTERN void
SNMP_UsecAuthPacket	_ANSI_ARGS_((SNMP_Session *session, 
				     u_char *packet, int packetlen));
#endif

EXTERN int
SNMP_SysUpTime		_ANSI_ARGS_((void));

/*
 * Some conversion functions used to make binary syntax usable in tcl.
 */

EXTERN void
SNMP_BinToHex		_ANSI_ARGS_((char *s, int n, char *d));

EXTERN int
SNMP_HexToBin		_ANSI_ARGS_((char *s, char *d, int *n));

/*
 * Little utilities to debug the SNMP message handling code.
 */

EXTERN void
SNMP_DumpPDU		_ANSI_ARGS_((Tcl_Interp *interp, SNMP_PDU *pdu));

#endif /* _SNMP_H */
