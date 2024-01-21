/*
 * snmpRequest.c
 *
 * This file contains all functions that take a request to send a
 * SNMP packet over the network.
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
#include "mib.h"

#include <netdb.h>
#include <arpa/inet.h>

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
EncodeV1Message		_ANSI_ARGS_((Tcl_Interp *interp,
				     SNMP_Session *sess, SNMP_PDU *pdu,
				     u_char *packet, int *packetlen));
#ifdef SNMPv2USEC
static int
EncodeUsecParameter	_ANSI_ARGS_((SNMP_Session *session, SNMP_PDU *pdu, 
				     u_char *parameter));
#endif

#ifdef SNMPv2CLASSIC
static int
EncodeV2Message		_ANSI_ARGS_((Tcl_Interp *interp,
				     SNMP_Session *sess, SNMP_PDU *pdu,
				     u_char *packet, int *packetlen));
#endif

static u_char*
EncodePDU		_ANSI_ARGS_((Tcl_Interp *interp, 
				     SNMP_Session *sess, SNMP_PDU *pdu,
				     u_char *packet, int *packetlen));

/*
 * SNMP_Encode() converts the pdu into BER transfer syntax and sends it 
 * from the management party on this system to a remote agent.
 */

int
SNMP_Encode (interp, session, pdu, cmd)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *pdu;
     char *cmd;
{
    int	retry = 0, packetlen = 0, rc = 0;
    u_char packet[BUFSIZE];

    memset ((char *) packet, '\0', sizeof (packet));
    packetlen = 0;

    /*
     * Some special care must be taken to conform to SNMPv1 sessions:
     * SNMPv2 getbulk requests must be turned into getnext request 
     * and SNMPv1 error codes must be mapped on SNMPv1 error codes
     * (e.g. genErr as nothing more appropriate is available).
     *
     * This is based on the mapping presented in Marhall Rose and
     * Keith McCloghrie: "How to Manage your Network using SNMP"
     * page 95.
     */

    if (session->version == SNMPv1) {
        if (pdu->type == SNMPv2_GETBULK) {
	    pdu->type = SNMP_GETNEXT;
	    pdu->error_status = E_NOERROR;
	    pdu->error_index  = 0;
	}
	if (pdu->error_status > E_GENERR) {
	    switch (pdu->error_status) {
	      case E_NOACCESS:
	      case E_NOCREATION:
	      case E_NOTWRITABLE:
	      case E_INCONSISTENTNAME:
		pdu->error_status = E_NOSUCHNAME; break;
	      case E_WRONGTYPE:
	      case E_WRONGLENGTH:
	      case E_WRONGENCODING:
	      case E_WRONGVALUE:
		pdu->error_status = E_BADVALUE; break;	
	      case E_RESOURCEUNAVAILABLE:
	      case E_COMMITFAILED:
	      case E_UNDOFAILED:
		pdu->error_status = E_GENERR; break;
	      default:
		pdu->error_status = E_GENERR; break;
	    }
	}
    }

    /*
     * Encode message into ASN1 BER transfer syntax. Authentication or
     * encryption is done within the following procedures if it is an
     * authentic or private message.
     */

    switch (session->version) {
#ifdef SNMPv2CLASSIC
      case SNMPv2CLASSIC: 
	rc = EncodeV2Message (interp, session, pdu, packet, &packetlen);
	break;
#endif
      default:
	rc = EncodeV1Message (interp, session, pdu, packet, &packetlen);
        break;
    }

    if (rc != TCL_OK) {
	return TCL_ERROR;
    }

    switch (pdu->type) {
      case SNMP_GET:		snmpStats.snmpOutGetRequests++; break;
      case SNMP_GETNEXT:	snmpStats.snmpOutGetNexts++; break;
      case SNMP_SET:		snmpStats.snmpOutSetRequests++; break;
      case SNMP_RESPONSE:	snmpStats.snmpOutGetResponses++; break;
      case SNMPv1_TRAP:		snmpStats.snmpOutTraps++; break;
    }

    /*
     * Show the contents of the PDU - mostly for debugging.
     */

    SNMP_EvalBinding (interp, session, pdu, SNMP_SEND_EVENT);

    SNMP_DumpPDU (interp, pdu);

    /*
     * A trap message or a response? - send it and we are done!
     */
    
    if (pdu->type == SNMPv1_TRAP || pdu->type == SNMPv2_TRAP 
	|| pdu->type == SNMP_RESPONSE || pdu->type == SNMPv2_REPORT) {
#ifdef SNMPv2USEC
	if (session->version == SNMPv2USEC 
	    && session->qos & USEC_QOS_AUTH) {
	    SNMP_UsecAuthPacket (session, packet, packetlen);
	}
#endif
	if (SNMP_Send (interp, packet, packetlen, &pdu->addr) != TCL_OK) {
	    return TCL_ERROR;
	}
	Tcl_ResetResult (interp);
	return TCL_OK;
    }
  
   /*
    * Asychronous request: record request, send packet to the
    * target an we are done.
    */

    if (cmd) {

	int count = 0;
	SNMP_Request *rPtr;

	for (rPtr = session->requestList; rPtr; rPtr = rPtr->nextPtr) {
	    count++;
	}
	if (session->window && count >= session->window) {
	    /* XXX -- not finished yet */
#if 0
	    fprintf (stderr, "** window %d exceeded (%d) -- waiting\n",
		     session->window, count);
#endif
	}

	rPtr = SNMP_RecordRequest(interp, session, pdu->request_id, 
				  packet, packetlen, cmd);
	
#ifdef SNMPv2USEC
	if (session->version == SNMPv2USEC && session->qos & USEC_QOS_AUTH) {
	    SNMP_UsecAuthPacket(session, packet, packetlen);
	}
#endif
	if (SNMP_Send(interp, packet, packetlen, &pdu->addr) == TCL_ERROR) {
	    SNMP_DeleteRequest(session, rPtr);
	    return TCL_ERROR;
	}
	sprintf(interp->result, "%d", (int) pdu->request_id);
	return TCL_OK;
    }

   /*
    * Synchronous request: send packet and wait for response.
    */

    for (retry = 0; retry <= session->retries; retry++) {
	int id;

      repeat:
#ifdef SNMPv2USEC
	if (session->version == SNMPv2USEC && session->qos & USEC_QOS_AUTH) {
	    SNMP_UsecAuthPacket (session, packet, packetlen);
	}
#endif
	if (SNMP_Send (interp, packet, packetlen, &pdu->addr) == TCL_ERROR) {
	    return TCL_ERROR;
	}

	while (SNMP_Wait (session->timeout*1000 / (session->retries+1)) > 0) {
	    u_char packet[BUFSIZE];
	    int packetlen = BUFSIZE;
	    int rc;
	    struct sockaddr_in from;
	    
	    if (SNMP_Recv (interp, packet, &packetlen, &from) != TCL_OK) {
		return TCL_ERROR;
	    }
	    
	    rc = SNMP_Decode (interp, packet, packetlen, &from, session, &id);
	    if (rc == TCL_BREAK) {
		if (retry++ <= session->retries + 1) {
		    goto repeat;
		}
	    }
	    if (rc == TCL_OK) {
		if (id == pdu->request_id) {
		    return TCL_OK;
		}
		rc = TCL_CONTINUE;
	    }
	    
	    if (rc == TCL_CONTINUE) continue;
	    if (rc == TCL_ERROR) return TCL_ERROR;
	}
    }
    
    Tcl_SetResult (interp, "noResponse", TCL_STATIC);
    return TCL_ERROR;
}


/*
 * EncodeV1Message() takes a session and a PDU and serializes the
 * ASN1 pdu as an octet string into the buffer pointed to by packet
 * using the "Basic Encoding Rules". See RFC 1157 for the Message 
 * header description. The main parts are the version number, the 
 * community string and the SNMP PDU.
 */

static int
EncodeV1Message (interp, session, pdu, packet, packetlen)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *pdu;
     u_char *packet;
     int *packetlen;
{
    u_char *messageLen;
    u_char *p = packet;
#ifdef SNMPv2USEC
#define PARAM_MAX_LENGTH 370
    u_char buffer[PARAM_MAX_LENGTH], *parameter = NULL;
    int version = 0, parameterLen = 0;

    if (session->qos & USEC_QOS_PRIV) {
	Tcl_SetResult (interp, "encryption not supported", TCL_STATIC);
	return TCL_ERROR;
    }
#endif

    *p++ = (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE);
    messageLen = p++;
    *packetlen += 2;

    switch (session->version) {
      case SNMPv1:
	version = 0;
	parameter = (u_char *) session->community;
	parameterLen = strlen (parameter);
	break;
#ifdef SNMPv2C
      case SNMPv2C:
	version = 1;
	parameter = (u_char *) session->community;
	parameterLen = strlen (parameter);
        break;
#endif
#ifdef SNMPv2USEC
      case SNMPv2USEC:
	version = 2;
	parameter = buffer;
	parameterLen = EncodeUsecParameter (session, pdu, parameter);
	break;
    }
#endif
    p = ASN1_EncodeInt (p, packetlen, ASN1_INTEGER, version);
    p = ASN1_EncodeOctetString (p, packetlen, ASN1_OCTET_STRING,
				parameter, parameterLen);

    p = EncodePDU (interp, session, pdu, p, packetlen);
    if (p == NULL) {
	if (*interp->result == '\0') {
	    Tcl_SetResult (interp, ASN1_ErrorString(), TCL_STATIC);
	}
	return TCL_ERROR;
    }

    p = ASN1_EncodeLength (p, packetlen, messageLen, p - (messageLen + 1));
    return TCL_OK;
}


#ifdef SNMPv2USEC
/*
 * UsecParameter() builds the parameters string. Note, some fields
 * are left blank as they are filled in later in SNMP_UsecAuthPacket().
 * This way we can patch in new agentTime or agentBoot values in case
 * our clock drifts away.
 */

static int
EncodeUsecParameter (session, pdu, parameter)
     SNMP_Session *session;
     SNMP_PDU *pdu;
     u_char *parameter;
{
    u_char *p = parameter;
    u_int maxSize = BUFSIZE;

    /* 
     * The first byte is the model indicator, which is 0 for the USEC 
     * model. The second byte is the quality of service (qos) field.
     */

    *p++ = USEC_MODEL;
    *p++ = session->qos;

    /*
     * The following 12 bytes contain the agent identifier. This field
     * will contain 12 bytes of 0 if we send unsecure messages. The next 
     * 4 bytes contain the number of agent boots followed by the current
     * agent time which is calculated by using the time offset saved
     * in the session structure. Note, these fields are patched later.
     */

    if (pdu->type == SNMPv2_REPORT) {
	u_int boots = session->agentBoots;
	u_int clock = time ((time_t *) NULL) - session->agentTime;
	memcpy (p, session->agentID, USEC_MAX_AGENTID);
	p += USEC_MAX_AGENTID;
	*p++ = (boots >> 24) & 0xff;
	*p++ = (boots >> 16) & 0xff;
	*p++ = (boots >> 8) & 0xff;
	*p++ = boots & 0xff;
	*p++ = (clock >> 24) & 0xff;
	*p++ = (clock >> 16) & 0xff;
	*p++ = (clock >> 8) & 0xff;
	*p++ = clock & 0xff;
    } else {
	memset (p, 0, USEC_MAX_AGENTID + 4 + 4);
	p += 20;
    }

    /*
     * The next variable length field contains the user name. The first
     * byte is the length of the user name.
     */

    *p++ = session->userNameLen;
    memcpy (p, session->userName, session->userNameLen);
    p += session->userNameLen;

    /*
     * The next variable length field is the authentication digest. Its
     * length is 0 for unauthenticated messages or 16 for the MD5 digest
     * algorithm. Note, this field is patched later.
     */
    
    if (session->qos & USEC_QOS_AUTH) {
	*p++ = MD5_SIZE;
	p += MD5_SIZE;
    } else {
	*p++ = 0;
    }

    /*
     * The following two bytes contain the max message size we accept.
     * followed by the context identifier. Note, the context identifier
     * is variable length but the length is not given as it is implicit
     * contained in the length of the octet string.
     */
    
    *p++ = (maxSize >> 8) & 0xff;
    *p++ = maxSize & 0xff;
    
    memcpy (p, session->cntxt, session->cntxtLen);
    p += session->cntxtLen;
    return (p - parameter);
}


/*
 * SNMP_UsecAuthPacket() patches the USEC authentication information
 * into a BER encoded SNMP USEC packet. We therefore decode the packet
 * until we have found the parameter string. This is still a lot faster
 * than doing BER encodings every time the packet is sent.
 */

void
SNMP_UsecAuthPacket (session, packet, packetlen)
     SNMP_Session *session;
     u_char *packet;
     int packetlen;
{
    u_char *parm, *p = packet;
    int dummy = packetlen;
    u_int boots = session->agentBoots;
    u_int clock = time ((time_t *) NULL) - session->agentTime;
    u_char digest[MD5_SIZE];

    /*
     * Get the parameter section out of the message header.
     */
    
    if (*p ++ != (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE)) return;
    p = ASN1_DecodeLength (p, &dummy, (u_int *) &dummy);
    p = ASN1_DecodeInt (p, &dummy, ASN1_INTEGER, &dummy);
    p = ASN1_DecodeOctetString (p, &dummy, ASN1_OCTET_STRING, 
				&parm, &dummy);
    if (! p) return;

    /*
     * Set the agentID, the agentBoots and the agentTime values.
     */

    p = parm + 2;
    memcpy (p, session->agentID, USEC_MAX_AGENTID);
    p += USEC_MAX_AGENTID;
    *p++ = (boots >> 24) & 0xff;
    *p++ = (boots >> 16) & 0xff;
    *p++ = (boots >> 8) & 0xff;
    *p++ = boots & 0xff;
    *p++ = (clock >> 24) & 0xff;
    *p++ = (clock >> 16) & 0xff;
    *p++ = (clock >> 8) & 0xff;
    *p++ = clock & 0xff;

    /*
     * Skip the user name field, check the digest len (should be 16),
     * copy the key into the packet, compute the digest and copy the
     * result back into the packet.
     */

    p += *p++;
    if (*p++ != MD5_SIZE) return;
    memcpy (p, session->authKey, MD5_SIZE);
    SNMP_MD5_digest (packet, packetlen, digest);
    memcpy (p, digest, MD5_SIZE);
}
#endif


#ifdef SNMPv2CLASSIC
/*
 * EncodeV2Message() takes a session and a PDU and serializes the
 * ASN1 pdu as an octet string into the buffer pointed to by packet
 * using the "Basic Encoding Rules".
 */

static int
EncodeV2Message (interp, session, pdu, packet, packetlen)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *pdu;
     u_char *packet;
     int *packetlen;
{
    int dummy;
    
    u_char *PrivMsg_len	= NULL,	*PrivData_len = NULL;
    u_char *AuthMsg_len = NULL, *AuthData_len = NULL;
    u_char *AuthInformation = NULL;

    u_char md5Digest[17], *digest_begin = NULL;

    if (session->srcParty.PrivProtocol != NO_PRIV
	|| session->dstParty.PrivProtocol != NO_PRIV) {
	Tcl_SetResult (interp, "encryption not supported", TCL_STATIC);
        return TCL_ERROR;
    }
    
    /*
     * encode "SnmpPrivMsg" ( tag: [1] IMPLICIT SEQUENCE )
     */

    *packet++ = (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1);
    PrivMsg_len = packet++;
    *packetlen += 2;   

    /*
     * encode the "privDst" field
     */

    packet = ASN1_EncodeOID (packet, packetlen, session->dstParty.Identity,
			     session->dstParty.IdentityLen);

    /*
     * encode the "privData" field ( tag: [1] IMPLICIT OCTET STRING )
     */

    *packet++ = (CONTEXT_SPECIFIC | PRIMITIVE | TAG_1);
    PrivData_len = packet++;
    *packetlen += 2;

    /*
     * encode the "SnmpAuthMsg" ( tag: [1] IMPLICIT SEQUENCE )
     */

    AuthInformation = packet;

    *packet++ = (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1);
    AuthMsg_len = packet++;
    *packetlen += 2;

    if (session->srcParty.AuthProtocol == MD5_AUTH) {

	u_char *AuthInfo_len;

	*packet++ = (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_2);
	AuthInfo_len  = packet++;
	*packetlen += 2;
	
	digest_begin = packet;

	packet = ASN1_EncodeOctetString (packet, packetlen, ASN1_OCTET_STRING,
					 session->srcParty.AuthPrivate, 
					 MD5_SIZE);
	packet = ASN1_EncodeInt (packet, packetlen, ASN1_UInteger32,
				 session->dstParty.AuthClock);
	packet = ASN1_EncodeInt (packet, packetlen, ASN1_UInteger32,
				 session->srcParty.AuthClock);
	packet = ASN1_EncodeLength (packet, packetlen,
				    AuthInfo_len, packet - (AuthInfo_len + 1));

    } else {
	packet = ASN1_EncodeOctetString (packet, packetlen, ASN1_OCTET_STRING,
					 NULL, 0);
    }

    /*
     * encode the "SnmpMgmtCom" ( tag: [1] IMPLICIT SEQUENCE )
     */

    *packet++ = (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_2);
    AuthData_len = packet++;
    *packetlen += 2;

    /*
     * encode the "dstParty", "srcParty" and "context" field
     */

    packet = ASN1_EncodeOID (packet, packetlen, 
			     session->dstParty.Identity, 
			     session->dstParty.IdentityLen);

    packet = ASN1_EncodeOID (packet, packetlen,
			     session->srcParty.Identity,
			     session->srcParty.IdentityLen);
    
    packet = ASN1_EncodeOID (packet, packetlen,
			     session->context.Identity,
			     session->context.IdentityLen);
    /*
     * encode PDU
     */

    packet = EncodePDU (interp, session, pdu, packet, packetlen);
    if (packet == NULL) {
	if (*interp->result == '\0') {
	    Tcl_SetResult (interp, ASN1_ErrorString(), TCL_STATIC);
	}
	return TCL_ERROR;
    }

    /*
     * Set the various length fields with their values. If one field
     * is longer than it's reserved space, make sure to shift the
     * other pointers. We do this by removing the packetlen before
     * setting the length fields. We add the possibly corrected 
     * packetlen later so we have updated our values.
     */

    packet = ASN1_EncodeLength (packet, packetlen,
				AuthData_len, packet - (AuthData_len + 1));
    digest_begin -= *packetlen;
    packet = ASN1_EncodeLength (packet, packetlen,
				AuthMsg_len, packet - (AuthMsg_len + 1));
    AuthInformation -= *packetlen;
    packet = ASN1_EncodeLength (packet, packetlen, 
				PrivData_len, packet - (PrivData_len + 1));
    packet = ASN1_EncodeLength (packet, packetlen,
				PrivMsg_len, packet - (PrivMsg_len + 1));
    digest_begin += *packetlen;
    AuthInformation += *packetlen;

    /*
     * call message digest computation if it's an authenticated message
     */

    if (session->srcParty.AuthProtocol == MD5_AUTH) {
	SNMP_MD5_digest (AuthInformation, packet - AuthInformation, md5Digest);
	ASN1_EncodeOctetString (digest_begin, &dummy, ASN1_OCTET_STRING, 
				md5Digest, MD5_SIZE);
    }

    return TCL_OK;
}
#endif


/*
 * EncodePDU() takes a session and a PDU and serializes the ASN1 PDU
 * as an octet string into the buffer pointed to by packet using the
 * "Basic Encoding Rules".
 */

static u_char*
EncodePDU (interp, session, pdu, packet, packetlen)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU	*pdu;
     u_char	*packet;
     int	*packetlen;
{    
    u_char *PDU_len = NULL, *VarBind_len = NULL, *VarBindList_len = NULL;
    
    int i, vblc, vbc;
    char **vblv, **vbv;

    ASN1_OID *oid;
    int oidlen;

    /*
     * encode pdu type ( tag: [pdu_type] IMPLICIT PDU )
     */
    
    *packet++  = (CONTEXT_SPECIFIC | CONSTRUCTED | pdu->type);
    PDU_len    = packet++;
    *packetlen += 2;

    if (pdu->type == SNMPv1_TRAP) {

	int generic = 0, specific = 0, len;
	char hostname[64];
	struct hostent *he;

	oid = ASN1_Str2Oid (pdu->trapOID, &oidlen);
	if (strncmp (pdu->trapOID, "1.3.6.1.6.3.1.1.5", 17) == 0) {
	    ASN1_OID *tmp;
	    generic = oid[oidlen-1] - 1;
	    specific = 0;
	    tmp = ASN1_Str2Oid ("1.3.6.1.4.1.1701", &len);
	    packet = ASN1_EncodeOID (packet, packetlen, tmp, len);
	} else {
	    generic = 6;
	    specific = oid[oidlen-1];
	    packet = ASN1_EncodeOID (packet, packetlen, oid, oidlen-2);
	}
	if (packet == NULL) {
	    Tcl_SetResult (interp, 
			   "failed to encode enterprise object identifier", 
			   TCL_STATIC);
	    return NULL;
	}
 
	if (gethostname (hostname, 64) < 0) {
	    strcpy (hostname, "localhost");
	}
	he = gethostbyname (hostname);

	packet = ASN1_EncodeOctetString (packet, packetlen, ASN1_IpAddress,
					 *he->h_addr_list, he->h_length);

	packet = ASN1_EncodeInt (packet, packetlen, ASN1_INTEGER, generic);
	packet = ASN1_EncodeInt (packet, packetlen, ASN1_INTEGER, specific);
	packet = ASN1_EncodeInt (packet, packetlen, ASN1_TimeTicks, 
				 SNMP_SysUpTime());

    } else {
    
	packet = ASN1_EncodeInt (packet, packetlen,
				 ASN1_INTEGER, pdu->request_id);
	packet = ASN1_EncodeInt (packet, packetlen,
				 ASN1_INTEGER, pdu->error_status);
	switch (pdu->error_status) {
	  case E_TOOBIG:	snmpStats.snmpOutTooBigs++; break;
	  case E_NOSUCHNAME:	snmpStats.snmpOutNoSuchNames++; break;
	  case E_BADVALUE:	snmpStats.snmpOutBadValues++; break;
	  case E_READONLY:	break; /* not used */
	  case E_GENERR:	snmpStats.snmpOutGenErrs++; break;
	}
	packet = ASN1_EncodeInt (packet, packetlen,
				 ASN1_INTEGER, pdu->error_index);
    }

    /*
     * encode VarBindList ( SEQUENCE of VarBind )
     */
    
    *packet++       = (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE);
    VarBindList_len = packet++;
    *packetlen      += 2;
    
    /*
     * split the varbind list and loop over all elements
     */
    
    if (Tcl_SplitList (interp, Tcl_DStringValue (&pdu->varbind), &vblc, &vblv)
	!= TCL_OK) {
	return NULL;
    }

    if (pdu->type == SNMPv2_TRAP || pdu->type == SNMPv2_INFORM) {

	/* 
	 * encode two default var binds: sysUpTime.0 and snmpTrapOID.0
	 * as defined in RFC 1448
	 */
	   
	*packet++   = (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE);
	VarBind_len = packet++;
	*packetlen  += 2;

	oid = ASN1_Str2Oid ("1.3.6.1.2.1.1.3.0", &oidlen);
	packet = ASN1_EncodeOID (packet, packetlen, oid, oidlen);

	packet = ASN1_EncodeInt (packet, packetlen, ASN1_TimeTicks, 
				 SNMP_SysUpTime());
	packet = ASN1_EncodeLength (packet, packetlen,
				    VarBind_len,
				    packet - (VarBind_len + 1));

	*packet++   = (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE);
	VarBind_len = packet++;
	*packetlen  += 2;

	oid = ASN1_Str2Oid ("1.3.6.1.6.3.1.1.4.1.0", &oidlen);
	packet = ASN1_EncodeOID (packet, packetlen, oid, oidlen);

	oid = ASN1_Str2Oid (pdu->trapOID, &oidlen);
	packet = ASN1_EncodeOID (packet, packetlen, oid, oidlen);
	
	packet = ASN1_EncodeLength (packet, packetlen,
				    VarBind_len,
				    packet - (VarBind_len + 1));
    }
    
    for (i = 0; i < vblc; i++) {
	
	char *value;
	int asn1_type = ASN1_OTHER;
	
	/*
	 * split a single varbind into its components
	 */
	
	if (Tcl_SplitList (interp, vblv[i], &vbc, &vbv) != TCL_OK) {
	    ckfree ((char *) vblv);
	    return NULL;
	}

	if (vbc == 0) {
	    Tcl_SetResult (interp, "missing OBJECT IDENTIFIER", TCL_STATIC);
	    ckfree ((char *) vblv);
            return NULL;
	}
	
	/*
	 * encode each VarBind ( SEQUENCE name, value )
	 */
	
	*packet++   = (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE);
	VarBind_len = packet++;
	*packetlen  += 2;

	/*
	 * encode the object identifier, perhaps consulting the MIB
	 */
	
	if (ASN1_IsOid (vbv[0])) {
	    oid = ASN1_Str2Oid (vbv[0], &oidlen);
	} else {
	    char *tmp = MIB_Oid (vbv[0], 0);
	    if (! tmp) {
		Tcl_ResetResult (interp);
		Tcl_AppendResult (interp, "invalid arg \"", vbv[0],
				  ": expecting OBJECT IDENTIFIER",
				  (char *) NULL);
		ckfree ((char *) vbv);
		ckfree ((char *) vblv);
		return NULL;
	    }
	    oid = ASN1_Str2Oid (tmp, &oidlen);
	}
	packet = ASN1_EncodeOID (packet, packetlen, oid, oidlen);

	/*
	 * guess the asn1 type field and the value
	 */

	switch (vbc) {
	  case 1:
	    value = "";
	    asn1_type = ASN1_NULL;
	    break;
	  case 2:
	    value = vbv[1];
	    asn1_type = MIB_ASN1 (vbv[0], 0);
	    break;
	  default:
	    value = vbv[2];

	    /*
	     * Check if there is an exception in the asn1 type field.
	     * Convert this into an appropriate NULL type if we create
	     * a response PDU. Otherwise, ignore this stuff and use
	     * the type found in the MIB.
	     */

	    if (pdu->type == SNMP_RESPONSE) {
		if (strcmp (vbv[1], "noSuchObject") == 0) {
		    asn1_type = ASN1_NO_SUCH_OBJECT;
		} else if (strcmp (vbv[1], "noSuchInstance") == 0) {
		    asn1_type = ASN1_NO_SUCH_INSTANCE;
		} else if (strcmp (vbv[1], "endOfMibView") == 0) {
		    asn1_type = ASN1_END_OF_MIB_VIEW;
		} else {
		    asn1_type = ASN1_Str2Sntx (vbv[1]);
		}
	    } else {
		asn1_type = ASN1_Str2Sntx (vbv[1]);
		if (! asn1_type) {
		    asn1_type = ASN1_NULL;
		}
	    }
	    break;
	}
	
	/*
         * If it's a SET request (or something equivalent), we'll 
	 * have to encode the VALUE.
	 */

	if (pdu->type == SNMP_SET || pdu->type == SNMP_RESPONSE
	    || pdu->type == SNMPv1_TRAP || pdu->type == SNMPv2_TRAP
	    || pdu->type == SNMPv2_INFORM || pdu->type == SNMPv2_REPORT) {

	    switch (asn1_type) {
	      case ASN1_INTEGER:
	      case ASN1_Counter32:
	      case ASN1_Gauge32:
	      case ASN1_UInteger32:
		{   int int_val, rc;
		    rc = Tcl_GetInt (interp, value, &int_val);
		    if (rc != TCL_OK) {
			char *tmp = MIB_Scan (vbv[0], 0, value);
			if (tmp && *tmp) {
			    Tcl_ResetResult (interp);
			    rc = Tcl_GetInt (interp, tmp, &int_val);
			}
			if (rc != TCL_OK) return NULL;
		    }
		    packet = ASN1_EncodeInt (packet, packetlen,
					     asn1_type, int_val);
		}
		break;
	      case ASN1_Counter64:
		{   int int_val, rc;
		    if (sizeof (int) >= 8) {
			rc = Tcl_GetInt (interp, value, &int_val);
			if (rc != TCL_OK) {
			    char *tmp = MIB_Scan (vbv[0], 0, value);
			    if (tmp && *tmp) {
				Tcl_ResetResult (interp);
				rc = Tcl_GetInt (interp, tmp, &int_val);
			    }
			    if (rc != TCL_OK) return NULL;
			}
			packet = ASN1_EncodeInt (packet, packetlen,
						 ASN1_Counter64, int_val);
		    } else {
			double d;
			rc = Tcl_GetDouble (interp, value, &d);
			if (rc != TCL_OK) {
			    return NULL;
			}
			if (d < 0) {
			    Tcl_SetResult (interp, "negativ counter value",
					   TCL_STATIC);
			    return NULL;
			}
			packet = ASN1_EncodeCounter64 (packet, packetlen, d);
		    }
		}
		break;
	      case ASN1_BIT_STRING:
		packet = ASN1_EncodeNull (packet, packetlen, ASN1_NULL);
		break;
	      case ASN1_TimeTicks:
		{   u_int d, h, m, s, f, n, val;
		    n = sscanf (value, "%dd %d:%d:%d.%d", 
				&d, &h, &m, &s, &f);
		    if (n == 5) {
			val = d * 8640000 + h * 360000 
					+ m * 6000 + s * 100 + f;
		    } else {
			val = 0;
			while (isdigit (*value)) {
			    val = 10 * val + *value - '0';
			    value++;
			}
		    }
		    packet = ASN1_EncodeInt (packet, packetlen,
					     ASN1_TimeTicks, val);
		}
                break;
	      case ASN1_IpAddress:
		{   int addr = inet_addr (value);
		    packet = ASN1_EncodeOctetString (packet, packetlen, 
						     ASN1_IpAddress, 
						     (char *) &addr, 4);
		}
		break;
	      case ASN1_OCTET_STRING:
		{   char *hex = value;
		    int len;
		    static char *bin = NULL;
		    static int binLen = 0;
		    char *scan = MIB_Scan (vbv[0], 0, value);
		    if (scan) hex = scan;
		    if (*hex) {
		        len = strlen (hex);
		        if (binLen < len + 1) {
			    if (bin) ckfree (bin);
			    binLen = len + 1;
			    bin = ckalloc (binLen);
			}
			if (SNMP_HexToBin (hex, bin, &len) < 0) {
			    Tcl_SetResult (interp, 
					   "illegal octet string value",
					   TCL_STATIC);
			    return NULL;
			}
		    } else {
			len = 0;
		    }
		    packet = ASN1_EncodeOctetString (packet, packetlen,
						     ASN1_OCTET_STRING,
						     bin, len);
		}
		break;
	      case ASN1_OBJECT_IDENTIFIER:
		if (! ASN1_IsOid (value)) {
		    char *tmp = MIB_Oid (value, 0);
		    if (!tmp) {
			Tcl_AppendResult (interp, 
					  "illegal object identifier \"",
					  value, "\"", (char *) NULL);
			return NULL;
		    }
		    oid = ASN1_Str2Oid (tmp, &oidlen);
		} else {
		    oid = ASN1_Str2Oid (value, &oidlen);
		}
		packet = ASN1_EncodeOID (packet, packetlen, oid, oidlen);
		break;
	      case ASN1_NO_SUCH_OBJECT:
	      case ASN1_NO_SUCH_INSTANCE:
	      case ASN1_END_OF_MIB_VIEW:
	      case ASN1_NULL:
		packet = ASN1_EncodeNull (packet, packetlen, asn1_type);
		break;
	      default:
		sprintf (interp->result, "unknown asn1 type 0x%.2x",
			 asn1_type);
		return NULL;
	    }
	} else {
	    packet = ASN1_EncodeNull (packet, packetlen, ASN1_NULL);
	}

	packet = ASN1_EncodeLength (packet, packetlen,
				    VarBind_len,
				    packet - (VarBind_len + 1));

	ckfree ((char *) vbv);
    }

    ckfree ((char *) vblv);

    packet = ASN1_EncodeLength (packet, packetlen, VarBindList_len,
				packet - (VarBindList_len + 1));
    packet = ASN1_EncodeLength (packet, packetlen, PDU_len,
				packet - (PDU_len + 1));
    return packet;
}
