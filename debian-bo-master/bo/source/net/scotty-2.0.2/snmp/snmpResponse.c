/*
 * snmpResponse.c
 *
 * This file contains all functions that decode a received SNMP packet
 * and do the appropriate actions. 
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

/*
 * A structure to keep the important parts of the message header 
 * while processing incoming SNMP messages.
 */

typedef struct Message {
    int		version;
    int		comLen;
    u_char	*com;
#ifdef SNMPv2CLASSIC
    ASN1_OID	srcParty[OID_MAXLEN];
    int		srcPartyLen;
    ASN1_OID	dstParty[OID_MAXLEN];
    int		dstPartyLen;
    ASN1_OID	context[OID_MAXLEN];
    int		contextLen;
    u_int	authDstTimestamp;
    u_int	authSrcTimestamp;
#endif
    u_char	*authDigest;
    int		authDigestLen;
#ifdef SNMPv2USEC
    u_char	qos;
    u_char	agentID[USEC_MAX_AGENTID];
    u_int	agentBoots;
    u_int	agentTime;
    int		userNameLen;
    char	userName[USEC_MAX_USER];
    int		cntxtLen;
    char	cntxt[USEC_MAX_CONTEXT];
    int		maxSize;
#endif
} Message;

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
Authentic		_ANSI_ARGS_((SNMP_Session *session, Message *msg,
				     u_char *packet, int packetlen,
				     u_int **snmpStatPtr));

static int
DecodeV1Message		_ANSI_ARGS_((Tcl_Interp	*interp, 
				     Message *msg, SNMP_PDU *pdu,
				     u_char *packet, int packetlen));

#ifdef SNMPv2USEC
static int
DecodeUsecParameter	_ANSI_ARGS_((Message *msg));

static void
SendUsecReport		_ANSI_ARGS_((Tcl_Interp *interp, 
				     SNMP_Session *session, 
				     struct sockaddr_in *to, 
				     int reqid, u_int *statPtr));
#endif

#ifdef SNMPv2CLASSIC
static int
DecodeV2Message		_ANSI_ARGS_((Tcl_Interp *interp,
				     Message *msg, SNMP_PDU *pdu,
				     u_char *packet, int packetlen));
#endif

static int
DecodePDU		_ANSI_ARGS_((Tcl_Interp *interp, struct SNMP_PDU *pdu,
				     u_char *packet, int *packetlen));


/*
 * SNMP_Decode() decode a complete SNMP packet and does all required
 * actions (mostly executing callbacks or doing gets/sets on the
 * agent module).
 */

int
SNMP_Decode(interp, packet, packetlen, from, session, reqid)
     Tcl_Interp	*interp;
     u_char	*packet;
     int	packetlen;
     struct sockaddr_in *from;
     SNMP_Session *session;
     int	  *reqid;
{
    SNMP_PDU _pdu, *pdu = &_pdu;
    Message _msg, *msg = &_msg;
    SNMP_Request *request = NULL;
    int code, delivered = 0;

    if (reqid) {
	*reqid = 0;
    }
    memset((char *) msg, 0, sizeof(Message));
    Tcl_DStringInit(&pdu->varbind);
    pdu->addr = *from;

    /*
     * read the version from the received packet.
     */

    if (*packet == (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE)) {
	snmpStats.snmpInPkts++;
	code = DecodeV1Message(interp, msg, pdu, packet, packetlen);
	if (code == TCL_ERROR) {
	    return TCL_CONTINUE;
	}
#ifdef SNMPv2CLASSIC
    } else if (*packet == (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1)) {
	snmpStats.snmpStatsPackets++;
	code = DecodeV2Message(interp, msg, pdu, packet, packetlen);
	if (code == TCL_ERROR) {
	    return TCL_CONTINUE;
	}
#endif
    } else {
	snmpStats.snmpInBadVersions++;
	Tcl_SetResult(interp, "received packet with unknown SNMP version",
		      TCL_STATIC);
	return TCL_CONTINUE;
    }

    /*
     * Show the contents of the PDU - mostly for debugging.
     */

    SNMP_DumpPDU(interp, pdu);

    /*
     * First check for REPORT PDUs. They are used internally to handle
     * SNMPv2 time synchronization aka maintenance functions.
     */

#ifdef SNMPv2USEC
    if (msg->version == SNMPv2USEC && pdu->type == SNMPv2_REPORT) {
	SNMP_Session *s = session;
	time_t clock = time ((time_t *) NULL);
	request = SNMP_LookupRequest (pdu->request_id);
	if (request) {
	    s = request->session;
	}
	if (! s) return TCL_CONTINUE;
	s->agentTime = clock - msg->agentTime;
	s->agentBoots = msg->agentBoots;
	memcpy (s->agentID, msg->agentID, 12);
	SNMP_EvalBinding (interp, s, pdu, SNMP_RECV_EVENT);
	if (request) {
	    if (s->version == SNMPv2USEC && s->qos & USEC_QOS_AUTH) {
		SNMP_UsecAuthPacket (s, request->packet, request->packetlen);
	    }
	    SNMP_Send (interp, request->packet, request->packetlen, &s->tAddr);
	}
	return TCL_BREAK;
    }
#endif

    /*
     * Next, handle RESPONSES as we should be able to find a session 
     * for the RESPONSE PDU. 
     */


    if (pdu->type == SNMP_RESPONSE) {

	snmpStats.snmpInGetResponses++;
    
	/* 
	 * Lookup the request for this response and evaluate the callback
	 * or return the result if we can not find an async request and
	 * we already have the session pointer.
	 */
	
	request = SNMP_LookupRequest (pdu->request_id);

	if (! request) {
	    if (! session) {
		return TCL_CONTINUE;
	    }
	    
	    if (reqid) {
		*reqid = pdu->request_id;
	    }

	    if (! Authentic(session, msg, packet, packetlen, NULL)) {
		Tcl_SetResult (interp, "authentication failure", TCL_STATIC);
		return TCL_CONTINUE;
	    }

	    SNMP_EvalBinding (interp, session, pdu, SNMP_RECV_EVENT);
	    
	    if (pdu->error_status) {
		char buf[20];
		Tcl_ResetResult (interp);
		Tcl_AppendResult (interp, SNMP_Err2Str (pdu->error_status),
				  " ", (char *) NULL);
		sprintf (buf, "%d ", pdu->error_index);
		Tcl_AppendResult (interp, buf, 
				  Tcl_DStringValue (&pdu->varbind),
				  (char *) NULL);
		return TCL_ERROR;
	    }
	    Tcl_ResetResult (interp);
	    Tcl_DStringResult (interp, &pdu->varbind);
	    return TCL_OK;

	} else {

	    session = request->session;

	    if (! Authentic(session, msg, packet, packetlen, NULL)) {
		Tcl_SetResult (interp, "authentication failure", TCL_STATIC);
		return TCL_CONTINUE;
	    }
	    
	    SNMP_EvalBinding (interp, session, pdu, SNMP_RECV_EVENT);

	    if (request->cmd) {
		SNMP_EvalCallback (interp, session, pdu,
				   request->cmd, NULL, NULL, NULL);
	    }

	    /*
	     * Free response message structure and delete request.
	     */
	    
	    SNMP_DeleteRequest (session, request);
	    return TCL_OK;
	}
    }


    for (session = sessionList; session; session = session->nextPtr) {

	SNMP_Binding *bindPtr = session->bindPtr;

	if (session->version != msg->version) continue;

	switch (pdu->type) {
	  case SNMPv1_TRAP: 
	    while (bindPtr && bindPtr->event != SNMP_TRAP_EVENT) {
		bindPtr = bindPtr->nextPtr;
	    }
	    if (session->version == SNMPv1 && bindPtr && bindPtr->command
		&& (! session->agentSocket)
		&& Authentic (session, msg, packet, packetlen, NULL)) {
		delivered++;
		SNMP_EvalCallback (interp, session, pdu,
                                   bindPtr->command, NULL, NULL, NULL);
		snmpStats.snmpInTraps++;
	    }
	    break;
	  case SNMPv2_TRAP:
	    while (bindPtr && bindPtr->event != SNMP_TRAP_EVENT) {
		bindPtr = bindPtr->nextPtr;
	    }
	    if ((session->version & SNMPv2) && bindPtr && bindPtr->command
		&& (! session->agentSocket)
		&& Authentic (session, msg, packet, packetlen, NULL)) {
		delivered++;
		SNMP_EvalCallback (interp, session, pdu,
                                   bindPtr->command, NULL, NULL, NULL);
		snmpStats.snmpInTraps++;
	    }
	    break;
	  case SNMPv2_INFORM:
	    while (bindPtr && bindPtr->event != SNMP_INFORM_EVENT) {
                bindPtr = bindPtr->nextPtr;
            }
	    if ((session->version & SNMPv2) && bindPtr && bindPtr->command
                && session->agentSocket
                && Authentic (session, msg, packet, packetlen, NULL)) {
                delivered++;
		if (SNMP_AgentRequest (interp, session, pdu) != TCL_OK) {
                    return TCL_ERROR;
                }
                SNMP_EvalCallback (interp, session, pdu,
                                   bindPtr->command, NULL, NULL, NULL);
            }
	    break;
	  case SNMPv2_GETBULK:
	    if (session->version == SNMPv1) break;
	  case SNMP_GET:
	  case SNMP_GETNEXT:
	  case SNMP_SET: 
	    {
		u_int *statPtr;
		if (! session->agentSocket) break;
		if (Authentic (session, msg, packet, packetlen, &statPtr)) {
		    if (SNMP_AgentRequest(interp, session, pdu) != TCL_OK) {
			return TCL_ERROR;
		    }
		    delivered++;
		} else {
#ifdef SNMPv2USEC
		    if (session->version == SNMPv2USEC) {
			SendUsecReport(interp, session, from, 
				       pdu->request_id, statPtr);
		    }
#endif
		}
	    }
	    break;
	}
    }

    if (! delivered && msg->version == SNMPv1) {
	snmpStats.snmpInBadCommunityNames++;
    }

    return TCL_CONTINUE;
}


/*
 * Authentic() verifies the authentication information contained
 * in the message header against the information associated with 
 * the given session handle.
 */

static int
Authentic (session, msg, packet, packetlen, snmpStatPtr)
     SNMP_Session *session;
     Message *msg;
     u_char *packet;
     int packetlen;
     u_int **snmpStatPtr;
{
    u_char recvDigest[16], md5Digest[16];
    int authentic = 0;

    if (msg->version != session->version) {
	return authentic;
    }

    switch (msg->version) {

      case SNMPv1:
	if (strlen(session->community) != msg->comLen) break;
	authentic = (memcmp (session->community, msg->com, msg->comLen) == 0);
	break;

#ifdef SNMPv2C
      case SNMPv2C:
	if (strlen(session->community) != msg->comLen) break;
	authentic = (memcmp (session->community, msg->com, msg->comLen) == 0);
	break;
#endif

#ifdef SNMPv2CLASSIC
      case SNMPv2CLASSIC:

	/* 
	 * XXX STILL LEFT TO BE DONE (SEE RFC 1450)
	 *
	 *  snmpStatsDstPartyMismatches
	 *  snmpStatsUnknownSrcParties
	 *  snmpStatsBadAuths
	 *  snmpStatsNotInLifetimes
	 *  snmpStatsWrongDigestValues
	 *  snmpStatsUnknownContexts
	 *  snmpStatsBadOperations
	 *  snmpStatsSilentDrops
	 */

	authentic = 1;
	
	if (msg->contextLen != session->context.IdentityLen
	    || memcmp (msg->context, session->context.Identity, 
		       msg->contextLen) != 0) {
            authentic = 0;
            break;
        }

	if (msg->dstPartyLen != session->srcParty.IdentityLen
	    || memcmp (msg->dstParty, session->srcParty.Identity, 
		       msg->dstPartyLen) != 0) {
            authentic = 0;
            break;
        }

	if (msg->srcPartyLen != session->dstParty.IdentityLen
	    || memcmp (msg->srcParty, session->dstParty.Identity, 
		       msg->srcPartyLen) != 0) {
            authentic = 0;
            break;
        }

	if (session->srcParty.AuthProtocol == MD5_AUTH) {

	    if (msg->authDigestLen != MD5_SIZE) {
		authentic = 0;
		break;
	    }
	    
	    memcpy (recvDigest, msg->authDigest, MD5_SIZE);
	    memcpy (msg->authDigest, session->srcParty.AuthPrivate, MD5_SIZE);
	    SNMP_MD5_digest (packet, packetlen, md5Digest);
	    if (memcmp (recvDigest, md5Digest, MD5_SIZE) != 0) {
		authentic = 0;
	    }
	    memcpy (msg->authDigest, recvDigest, MD5_SIZE);
	}
	break;
#endif

#ifdef SNMPv2USEC
      case SNMPv2USEC:

	authentic = 1;
	if (snmpStatPtr) {
	    *snmpStatPtr = NULL;
	}

	if (msg->userNameLen != session->userNameLen 
	    || memcmp (msg->userName, session->userName, 
		       msg->userNameLen) != 0) {
	    snmpStats.usecStatsUnknownUserNames++;
	    if (snmpStatPtr) {
		*snmpStatPtr = &snmpStats.usecStatsUnknownUserNames;
	    }
	    authentic = 0;
	    break;
	}

	if (msg->qos & USEC_QOS_PRIV || msg->qos != session->qos) {
	    snmpStats.usecStatsUnsupportedQoS++;
	    if (snmpStatPtr) {
		*snmpStatPtr = &snmpStats.usecStatsUnsupportedQoS;
	    }
	    authentic = 0;
	    break;
	}

	if (msg->cntxtLen != session->cntxtLen
            || memcmp(msg->cntxt, session->cntxt, msg->cntxtLen) != 0) {
	    snmpStats.usecStatsUnknownContextSelectors++;
	    if (snmpStatPtr) {
		*snmpStatPtr = &snmpStats.usecStatsUnknownContextSelectors;
	    }
            authentic = 0;
            break;
        }

	if (msg->qos & USEC_QOS_AUTH) {

	    if (memcmp(msg->agentID, session->agentID,USEC_MAX_AGENTID) != 0) {
		snmpStats.usecStatsNotInWindows++;
		if (snmpStatPtr) {
		    *snmpStatPtr = &snmpStats.usecStatsNotInWindows;
		}
		authentic = 0;
		break;
	    }
	    
	    if (session->agentSocket) {
		int clock = time ((time_t *) NULL) - session->agentTime;
		if (msg->agentBoots != session->agentBoots
		    || (int) msg->agentTime < clock - 150
		    || (int) msg->agentTime > clock + 150) {
		    snmpStats.usecStatsNotInWindows++;
		    if (snmpStatPtr) {
			*snmpStatPtr = &snmpStats.usecStatsNotInWindows;
		    }
		    authentic = 0;
		    break;
		}
	    }

	    if (msg->authDigestLen != MD5_SIZE) {
		snmpStats.usecStatsWrongDigestValues++;
		if (snmpStatPtr) {
		    *snmpStatPtr = &snmpStats.usecStatsWrongDigestValues;
		}
		authentic = 0;
		break;
	    }

	    memcpy(recvDigest, msg->authDigest, MD5_SIZE);
	    memcpy(msg->authDigest, session->authKey, MD5_SIZE);
	    SNMP_MD5_digest(packet, packetlen, md5Digest);

	    if (memcmp(recvDigest, md5Digest, MD5_SIZE) != 0) {
		snmpStats.usecStatsWrongDigestValues++;
		if (snmpStatPtr) {
		    *snmpStatPtr = &snmpStats.usecStatsWrongDigestValues;
		}
		authentic = 0;
	    }
	    memcpy(msg->authDigest, recvDigest, MD5_SIZE);
	}
	break;
#endif
    }

    return authentic;
}


/*
 * DecodeV1Message() takes a serialized packet and decodes the SNMPv1
 * message header.
 */

static int
DecodeV1Message (interp, msg, pdu, packet, packetlen)
     Tcl_Interp *interp;
     Message *msg;
     SNMP_PDU *pdu;
     u_char *packet;
     int packetlen;
{
    int version;
    int buflen = 0;
    u_int msglen = 0;
    u_char *p = packet;
    
    /*
     * Decode "Packet Header" header ( SEQUENCE 0x30 msglen )
     */
    
    if (*p++ != (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE)) {
	sprintf (interp->result,
		 "Message header: invalid value 0x%.2x; expecting 0x%.2x",
		 *--p, (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE));
	snmpStats.snmpInASNParseErrs++;
	return TCL_ERROR;
    }
    buflen += 1;
    
    p = ASN1_DecodeLength (p, &buflen, &msglen);
    if (p == NULL) goto asn1Error;
    
    if ((buflen + msglen) != packetlen) {
	interp->result = "invalid length field in message header";
	return TCL_ERROR;
    }
    
    buflen = 0;		/* buflen from here must be the same as msglen */

    /*
     * Decode Version field of this message ( must be 0 for RFC1157, 1
     * for community based SNMPv2 or 2 for USEC ).
     */
    
    p = ASN1_DecodeInt (p, &buflen, ASN1_INTEGER, &version);
    if (p == NULL) goto asn1Error;

    switch (version) {
      case 0:
	msg->version = SNMPv1;
	break;
#ifdef SNMPv2C
      case 1:
	msg->version = SNMPv2C;
	break;
#endif
#ifdef SNMPv2USEC
      case 2:
	msg->version = SNMPv2USEC;
	break;
#endif
      default:
	snmpStats.snmpInBadVersions++;
	sprintf (interp->result,
                 "received packet with unknown SNMP version %d", version);
        return TCL_ERROR;
    }
    
    /*
     * Decode "community" string.
     */

    if (*p != ASN1_OCTET_STRING) {
        sprintf (interp->result,
		 "Parameter string: invalid value 0x%.2x; expecting 0x%.2x",
		 *p, ASN1_OCTET_STRING);
	snmpStats.snmpInASNParseErrs++;
	return TCL_ERROR;
    }
    p = ASN1_DecodeOctetString (p, &buflen, ASN1_OCTET_STRING,
				&msg->com, &msg->comLen);
    if (p == NULL) goto asn1Error;

#ifdef SNMPv2USEC
    if (version == 2) {
	if (DecodeUsecParameter (msg) != TCL_OK) {
	    Tcl_SetResult (interp, "encoding error in USEC parameter", 
			   TCL_STATIC);
	    return TCL_ERROR;
	}
    }
#endif

    /*
     * Decode PDU and validate total message length.
     */

    if (DecodePDU (interp, pdu, p, &buflen) != TCL_OK) {
	return TCL_ERROR;
    }

    if (buflen != msglen) {
	sprintf (interp->result,
		 "Message sequence length (%d) differs from real length (%d).",
		 buflen, (int) msglen);
	snmpStats.snmpInASNParseErrs++;
	return TCL_ERROR;
    }

    return TCL_OK;

  asn1Error:
    Tcl_SetResult (interp, ASN1_ErrorString(), TCL_STATIC);
    snmpStats.snmpInASNParseErrs++;
    return TCL_ERROR;
}


#ifdef SNMPv2USEC
/*
 * DecodeUsecParameter() decodes the USEC parameter field and updates
 * the message structure pointed to by msg.
 */

static int
DecodeUsecParameter (msg)
     Message *msg;
{
    u_char *p = msg->com;

    /*
     * Skip over the model number and get the qos value.
     */

    if (*p++ != USEC_MODEL) {
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    msg->qos = *p++;

    /*
     * Copy the agentID, the agentBoots and the agentTime into 
     * the message structure.
     */

    memcpy (msg->agentID, p, USEC_MAX_AGENTID);
    p += USEC_MAX_AGENTID;
	
    msg->agentBoots = *p++;
    msg->agentBoots = (msg->agentBoots << 8) + *p++;
    msg->agentBoots = (msg->agentBoots << 8) + *p++;
    msg->agentBoots = (msg->agentBoots << 8) + *p++;
    
    msg->agentTime = *p++;
    msg->agentTime = (msg->agentTime << 8) + *p++;
    msg->agentTime = (msg->agentTime << 8) + *p++;
    msg->agentTime = (msg->agentTime << 8) + *p++;

    /*
     * Get the user name, the authentication digest, the max message 
     * size and finally the context identifier.
     */
	
    msg->userNameLen = *p++;
    if (msg->userNameLen > USEC_MAX_USER) {
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    memcpy (msg->userName, p, msg->userNameLen);
    p += msg->userNameLen;
    
    msg->authDigestLen = *p++;
    msg->authDigest = p;
    p += msg->authDigestLen;
    
    msg->maxSize = *p++;
    msg->maxSize = (msg->maxSize << 8) + *p++;
    if (msg->maxSize < USEC_MIN_MMS || msg->maxSize > USEC_MAX_MMS) {
	snmpStats.snmpStatsEncodingErrors++;
        return TCL_ERROR;
    }

    msg->cntxtLen = msg->comLen - (p - msg->com);
    if (msg->cntxtLen < 0 || msg->cntxtLen > USEC_MAX_CONTEXT) {
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    memcpy(msg->cntxt, p, msg->comLen - (p - msg->com));

    return TCL_OK;
}


/*
 * Send a REPORT PDU to let the receiver synchronize itself. Make sure
 * that we never send a REPORT PDU if we are not an agent. (We could get
 * into nasty loops otherwise. We create a copy of the session and adjust
 * it to become the well-known usec user.
 */

static void
SendUsecReport (interp, session, to, reqid, statPtr)
     Tcl_Interp *interp;
     SNMP_Session *session;
     struct sockaddr_in *to;
     int reqid;
     u_int *statPtr;
{
    SNMP_PDU _pdu, *pdu = &_pdu;
    SNMP_Session _usecSession, *usecSession = &_usecSession;
    char varbind[80];

    if (! session->agentSocket || ! statPtr) return;

    *usecSession = *session;
    usecSession->tAddr = *to;
    usecSession->userNameLen = 4;
    memcpy (usecSession->userName, "usec", 4);
    usecSession->cntxtLen = 0;
    usecSession->qos = USEC_QOS_MAINT;
    
    pdu->addr         = usecSession->tAddr;
    pdu->type         = SNMPv2_REPORT;
    pdu->request_id   = reqid;
    pdu->error_status = E_NOERROR;
    pdu->error_index  = 0;    
    pdu->trapOID      = NULL;
    Tcl_DStringInit (&pdu->varbind);
    
    if (statPtr > &snmpStats.usecStatsUnsupportedQoS) {
	sprintf (varbind, "{1.3.6.1.6.3.6.1.2.%d %u}", 
		 statPtr - &snmpStats.usecStatsUnsupportedQoS + 1,  *statPtr);
    } else if (statPtr > &snmpStats.snmpStatsPackets) {
	sprintf (varbind, "{1.3.6.1.6.3.1.1.1.%d %u}", 
		 statPtr -  &snmpStats.snmpStatsPackets + 1, *statPtr);
    } else {
	*varbind = '\0';
    }

    Tcl_DStringAppend (&pdu->varbind, varbind, -1);
    SNMP_Encode (interp, usecSession, pdu, NULL);
    Tcl_DStringFree (&pdu->varbind);
}
#endif


#ifdef SNMPv2CLASSIC
/*
 * DecodeV2Message() takes a serialized packet and decodes the SNMPv2
 * message header. This is more complicated since we must handle the
 * authentication information.
 */

static int
DecodeV2Message (interp, msg, pdu, packet, packetlen)
     Tcl_Interp *interp;
     Message *msg;
     SNMP_PDU *pdu;
     u_char *packet;
     int packetlen;
{
    int oidlen = 0, buflen = 0;
    u_int asnlen = 0, msglen = 0;
    ASN1_OID oid[OID_MAXLEN];

    msg->version = SNMPv2CLASSIC;
    
    /*
     * decode tag & length field of this message
     */
    
    if (*packet++ != (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1)) {
	sprintf (interp->result,
		 "SnmpPrivMsg: invalid tag 0x%.2x; expecting 0x%.2x",
		 *--packet, (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1));
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    buflen += 1;
    
    packet = ASN1_DecodeLength (packet, &buflen, &msglen);
    if (packet == NULL) goto asn1Error;
    
    if ((buflen + msglen) != packetlen) {
	interp->result = "invalid length field in message header";
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    
    buflen = 0;		/* buflen from here must be the same as msglen */

    /*
     * decode "privDst" field and set buflen.
     */
    
    packet = ASN1_DecodeOID (packet, &buflen, oid, &oidlen);
    if (packet == NULL) goto asn1Error;

    /*
     * decode "privData" header
     */
    
    if (*packet++ != (CONTEXT_SPECIFIC | PRIMITIVE | TAG_1)) {
	sprintf (interp->result,
		 "privData: invalid tag 0x%.2x; expecting 0x%.2x",
		 *--packet, (CONTEXT_SPECIFIC | PRIMITIVE | TAG_1));
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    buflen += 1;

    packet = ASN1_DecodeLength (packet, &buflen, &asnlen);
    if (packet == NULL) goto asn1Error;

    if ((asnlen + buflen) != msglen) {
	interp->result = "privData: invalid length field";
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }

    /*
     * decode "SnmpAuthMsg" header
     */

    if (*packet++ != (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1)) {
	sprintf (interp->result,
		 "SnmpAuthMsg: invalid tag 0x%.2x; expecting 0x%.2x",
		 *--packet, (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1));
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    buflen += 1;
    
    packet = ASN1_DecodeLength (packet, &buflen, &asnlen);
    if (packet == NULL) goto asn1Error;

    if ((asnlen + buflen) != msglen) {
	interp->result = "SnmpAuthMsg: invalid length field";
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }

    /*
     * Decode "authInformation" field - see RFC 1446 for more details.
     */

    if (*packet == (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_2)) {

	packet++;
	buflen += 1;

	packet = ASN1_DecodeLength (packet, &buflen, &asnlen);
	packet = ASN1_DecodeOctetString (packet, &buflen, ASN1_OCTET_STRING, 
					 &msg->authDigest, 
					 &msg->authDigestLen);
	packet = ASN1_DecodeInt (packet, &buflen, 
				 ASN1_UInteger32, &msg->authDstTimestamp);
	packet = ASN1_DecodeInt (packet, &buflen, 
				 ASN1_UInteger32, &msg->authSrcTimestamp);
	if (packet == NULL) goto asn1Error;

    } else if (*packet == ASN1_OCTET_STRING) {
	packet = ASN1_DecodeOctetString (packet, &buflen, ASN1_OCTET_STRING, 
					 NULL, 0);
	if (packet == NULL) goto asn1Error;
    } else {
	sprintf (interp->result,
		 "SnmpAuthMsg: invalid tag 0x%.2x; expecting 0x%.2x or 0x%.2x",
		 *--packet, (CONTEXT_SPECIFIC | CONSTRUCTED | TAG_1),
		 ASN1_OCTET_STRING);
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;	
    }


    /*
     * decode "SnmpMgmtCom" header
     */

    pdu->type = *packet++;

    if (pdu->type != (CONTEXT_SPECIFIC | CONSTRUCTED | SNMP_RESPONSE)) {
	sprintf (interp->result,
		 "SnmpMgmtCom: invalid tag 0x%.2x; expecting 0x%.2x",
		 pdu->type,
		 (CONTEXT_SPECIFIC | CONSTRUCTED | SNMP_RESPONSE));
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    buflen += 1;
    
    packet = ASN1_DecodeLength (packet, &buflen, &asnlen);
    if (packet == NULL) goto asn1Error;
    
    if ((asnlen + buflen) != msglen) {
        interp->result = "SnmpMgmtCom: invalid length field";
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    
    /*
     * Decode "dstParty", "srcParty" and "context" fields.
     */    
    
    packet = ASN1_DecodeOID (packet, &buflen, 
			     msg->dstParty, &msg->dstPartyLen);
    packet = ASN1_DecodeOID (packet, &buflen, 
			     msg->srcParty, &msg->srcPartyLen);
    packet = ASN1_DecodeOID (packet, &buflen, 
			     msg->context, &msg->contextLen);
    if (packet == NULL) goto asn1Error;

    /*
     * decode PDU and validate total message length
     */

    if (DecodePDU (interp, pdu, packet, &buflen) != TCL_OK) {
	return TCL_ERROR;
    }

    if (buflen != msglen) {
	sprintf (interp->result,
		 "Message sequence length (%d) differs from real length (%d).",
		 buflen, (int) msglen);
	snmpStats.snmpStatsEncodingErrors++;
	return TCL_ERROR;
    }
    
    return TCL_OK;

  asn1Error:
    Tcl_SetResult (interp, ASN1_ErrorString(), TCL_STATIC);
    snmpStats.snmpStatsEncodingErrors++;
    return TCL_ERROR;
}
#endif


/*
 * DecodePDU() takes a serialized packet and decodes the PDU. The result
 * is written to the pdu structure and varbind list is converted to a
 * TCL list contained in pdu->varbind.
 */

static int
DecodePDU (interp, pdu, packet, packetlen)
     Tcl_Interp	*interp;
     SNMP_PDU	*pdu;
     u_char	*packet;
     int	*packetlen;
{
    int		oidlen	= 0,		/* */
		pdulen	= 0;		/* # of bytes read parsing the PDU */
    
    u_int	asnlen	= 0,		/* asn sequence length		   */
		deflen	= 0;		/* pdu total length without header */
    
    ASN1_OID	oid[OID_MAXLEN];

    int		int_val;

    char buf[20];
    char *freeme;
    static char *vboid;
    static int vboidLen = 0;
    char *snmpTrapEnterprise = NULL;

    Tcl_DStringInit (&pdu->varbind);

    /*
     * first decode PDU tag -- let's see what we have to deal with
     */
    
    switch (*packet++) {
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMP_RESPONSE:
	pdu->type = SNMP_RESPONSE;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMP_GET:
	pdu->type = SNMP_GET;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMP_GETNEXT:
	pdu->type = SNMP_GETNEXT;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMPv2_GETBULK:
	pdu->type = SNMPv2_GETBULK;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMP_SET:
	pdu->type = SNMP_SET;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMPv2_INFORM:
	pdu->type = SNMPv2_INFORM;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMPv1_TRAP:
	pdu->type = SNMPv1_TRAP;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMPv2_TRAP:
	pdu->type = SNMPv2_TRAP;
	break;
      case CONTEXT_SPECIFIC | CONSTRUCTED | SNMPv2_REPORT:
	pdu->type = SNMPv2_REPORT;
	break;
      default:
	sprintf (interp->result,
		 "Response-PDU: invalid tag 0x%.2x.", *--packet);
	snmpStats.snmpInASNParseErrs++;
	return TCL_ERROR;
    }
    pdulen = 1;

    packet = ASN1_DecodeLength (packet, &pdulen, &deflen);
    if (packet == NULL) goto asn1Error;
    
    *packetlen += pdulen;
    pdulen = 0;

    if (pdu->type == SNMPv1_TRAP) {

	int generic, specific;
	char *toid = NULL;

	/*
	 * V1 trap PDUs require special attention - sad story
	 */

	pdu->request_id = 0;
	pdu->error_status = 0;
	pdu->error_index = 0;

	packet = ASN1_DecodeOID (packet, &pdulen, oid, &oidlen);
        if (packet == NULL) goto asn1Error;

	/*
	 * Save the enterprise object identifier so we can add it
	 * at the end of the varbind list. See the definition of
	 * snmpTrapEnterprise for details.
	 */

	{
	    char *tmp;
	    snmpTrapEnterprise = ASN1_Oid2Str(oid, oidlen);
	    tmp = MIB_Name(snmpTrapEnterprise, 0);
	    if (tmp) {
		snmpTrapEnterprise = ckstrdup(tmp);
	    } else {
		snmpTrapEnterprise = ckstrdup(snmpTrapEnterprise);
	    }
	}

	packet = ASN1_DecodeOctetString (packet, &pdulen, ASN1_IpAddress, 
					 NULL, NULL);
	if (packet == NULL) goto asn1Error;

	packet = ASN1_DecodeInt (packet, &pdulen, ASN1_INTEGER, &generic);
        if (packet == NULL) goto asn1Error;

	packet = ASN1_DecodeInt (packet, &pdulen, ASN1_INTEGER, &specific);
        if (packet == NULL) goto asn1Error;

	/* 
	 * Ignore errors here to accept bogus trap messages.
	 */

	packet = ASN1_DecodeInt (packet, &pdulen, ASN1_TimeTicks, &int_val);
	if (packet == NULL) goto asn1Error;
	{   u_int d, h, m, s, f;
	    d = int_val;
	    f = d % 100; d = d /100;
	    s = d % 60;  d = d / 60;
	    m = d % 60;  d = d / 60;
	    h = d % 24;  d = d / 24;
	    sprintf (buf, "%3dd %2d:%02d:%02d.%02d", d, h, m, s, f);
	    Tcl_DStringStartSublist (&pdu->varbind);
	    Tcl_DStringAppendElement (&pdu->varbind, "1.3.6.1.2.1.1.3.0");
	    Tcl_DStringAppendElement (&pdu->varbind, "TimeTicks");
	    Tcl_DStringAppendElement (&pdu->varbind, buf);
	    Tcl_DStringEndSublist (&pdu->varbind);
	}

	switch (generic) {
	  case 0:				/* coldStart*/
	    toid = "1.3.6.1.6.3.1.1.5.1";
	    break;
	  case 1:				/* warmStart */
	    toid = "1.3.6.1.6.3.1.1.5.2";
	    break;
	  case 2:				/* linkDown */
	    toid = "1.3.6.1.6.3.1.1.5.3";
	    break;
	  case 3:				/* linkUp */
	    toid = "1.3.6.1.6.3.1.1.5.4";
	    break;
	  case 4:				/* authenticationFailure */
	    toid = "1.3.6.1.6.3.1.1.5.5";
	    break;
	  case 5:				/* egpNeighborLoss */
	    toid = "1.3.6.1.6.3.1.1.5.6";
	    break;
	  default:
	    oid[oidlen++] = 0;
	    oid[oidlen++] = specific;		/* enterpriseSpecific */
	    toid = ckstrdup (ASN1_Oid2Str (oid, oidlen));
	    break;
	}

	Tcl_DStringStartSublist (&pdu->varbind);
	Tcl_DStringAppendElement (&pdu->varbind, "1.3.6.1.6.3.1.1.4.1.0");
	Tcl_DStringAppendElement (&pdu->varbind, "OBJECT IDENTIFIER");
	{
	    char *tmp = MIB_Name (toid, 0);
	    if (tmp) {
		Tcl_DStringAppendElement (&pdu->varbind, tmp);
	    } else {
		Tcl_DStringAppendElement (&pdu->varbind, toid);
	    }
	}

	if (((generic < 0) || (generic > 5)) && toid) {
	    ckfree (toid);
	    toid = NULL;
	}
	Tcl_DStringEndSublist (&pdu->varbind);

	if (packet == NULL) {
	    goto trapError;
	}

    } else {

	/*
	 * decode "request-id", "error-status", & "error index" field
	 */
	
	packet = ASN1_DecodeInt (packet, &pdulen, ASN1_INTEGER, 
				 &pdu->request_id);
	if (packet == NULL) goto asn1Error;
	
	packet = ASN1_DecodeInt (packet, &pdulen, 
				 ASN1_INTEGER, &pdu->error_status);
	if (packet == NULL) goto asn1Error;

	switch (pdu->error_status) {
	  case E_TOOBIG:	snmpStats.snmpInTooBigs++; break;
	  case E_NOSUCHNAME:	snmpStats.snmpInNoSuchNames++; break;
	  case E_BADVALUE:	snmpStats.snmpInBadValues++; break;
	  case E_READONLY:	snmpStats.snmpInReadOnlys++; break;
	  case E_GENERR:	snmpStats.snmpInGenErrs++; break;
	}
	
	packet = ASN1_DecodeInt (packet, &pdulen, ASN1_INTEGER, 
				 &pdu->error_index);
	if (packet == NULL) goto asn1Error;
	
    }
    
    /*
     * decode "VarBindList" header ( 0x30 asnlen )
     */
    
    if (*packet++ != (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE)) {
	if (pdu->type == SNMPv1_TRAP) {
	    goto trapError;
	}
	sprintf (interp->result,
		 "VarBindList: invalid tag 0x%.2x; expecting 0x%.2x",
		 *--packet, (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE));
	snmpStats.snmpInASNParseErrs++;
	return TCL_ERROR;
    }
    pdulen += 1;
    
    packet = ASN1_DecodeLength (packet, &pdulen, &asnlen);
    if (packet == NULL) goto asn1Error;
	
    if ((pdulen + asnlen) != deflen) {
	interp->result = "VarBindList: invalid length field";
	snmpStats.snmpInASNParseErrs++;
	return TCL_ERROR;
    }

    /* 
     * decode each "VarBind" 
     */

    while (packet && pdulen < deflen) {
	
	/*
	 * decode "VarBind" header ( 0x30 asnlen )
	 */
	
	if (*packet++ !=  (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE)) {
	    sprintf (interp->result,
		     "VarBind: invalid tag 0x%.2x; expecting 0x%.2x",
		     *--packet, (UNIVERSAL | CONSTRUCTED | ASN1_SEQUENCE));
	    snmpStats.snmpInASNParseErrs++;
	    return TCL_ERROR;
	}
	pdulen += 1;
	
	packet = ASN1_DecodeLength (packet, &pdulen, &asnlen);
	if (packet == NULL) goto asn1Error;
	
	/* Start a new element in the VarBindList */

	Tcl_DStringStartSublist (&pdu->varbind);
	
	/*
	 * decode OBJECT-IDENTIFIER
	 */
	
	packet = ASN1_DecodeOID (packet, &pdulen, oid, &oidlen);
	if (packet == NULL) goto asn1Error;

	{
	  char *soid = ASN1_Oid2Str (oid, oidlen);
	  int len = strlen (soid);
	  if (vboidLen < len + 1)  {
	      if (vboid) ckfree (vboid);
	      vboidLen = len + 1;
	      vboid = ckstrdup (soid);
	  } else {
	      strcpy (vboid, soid);
	  }
	  Tcl_DStringAppendElement (&pdu->varbind, vboid);
	}

	/*
	 * Handle exceptions that are coded in the SNMP varbind. We
	 * should create a type conforming null value if possible.
	 */

	switch (*packet) {
	  case ASN1_NO_SUCH_OBJECT:
	    Tcl_DStringAppendElement (&pdu->varbind, "noSuchObject");
	    Tcl_DStringAppendElement (&pdu->varbind, 
	        MIB_ASN1 (vboid, 0) == ASN1_OCTET_STRING ? "" : "0");
	    packet = ASN1_DecodeNull (packet, &pdulen, ASN1_NO_SUCH_OBJECT);
	    goto nextVarBind;
	  case ASN1_NO_SUCH_INSTANCE:
	    Tcl_DStringAppendElement (&pdu->varbind, "noSuchInstance");
	    Tcl_DStringAppendElement (&pdu->varbind, 
	        MIB_ASN1 (vboid, 0) == ASN1_OCTET_STRING ? "" : "0");
	    packet = ASN1_DecodeNull (packet, &pdulen, ASN1_NO_SUCH_INSTANCE);
	    goto nextVarBind;
	  case ASN1_END_OF_MIB_VIEW:
	    Tcl_DStringAppendElement (&pdu->varbind, "endOfMibView");
	    Tcl_DStringAppendElement (&pdu->varbind, 
	        MIB_ASN1 (vboid, 0) == ASN1_OCTET_STRING ? "" : "0");
	    packet = ASN1_DecodeNull (packet, &pdulen, ASN1_END_OF_MIB_VIEW);
	    goto nextVarBind;
	}

	/*
	 * Decode the ASN.1 type.
	 */

	Tcl_DStringAppendElement (&pdu->varbind, ASN1_Sntx2Str (*packet));

	/*
	 * decode value for object
	 */

	switch (*packet) {
	  case ASN1_Counter32:
	  case ASN1_Gauge32:
	  case ASN1_UInteger32:
            packet = ASN1_DecodeInt (packet, &pdulen, *packet, &int_val);
            if (packet == NULL) goto asn1Error;
	    sprintf (buf, "%u", int_val);
            Tcl_DStringAppendElement (&pdu->varbind, buf);
            break;
	  case ASN1_INTEGER:
            packet = ASN1_DecodeInt (packet, &pdulen, *packet, &int_val);
            if (packet == NULL) goto asn1Error;
	    {   char *tmp;
		sprintf (buf, "%d", int_val);
		tmp = MIB_Format (vboid, 0, buf);
		if (tmp) {
		    Tcl_DStringAppendElement (&pdu->varbind, tmp);
		} else {
		    Tcl_DStringAppendElement (&pdu->varbind, buf);
		}
	    }
            break;
	  case ASN1_TimeTicks:
	    {  
		u_int d, h, m, s, f;
		packet = ASN1_DecodeInt (packet, &pdulen, 
					 ASN1_TimeTicks, &d);
		if (packet == NULL) goto asn1Error;
		f = d % 100; d = d /100;
		s = d % 60;  d = d / 60;
		m = d % 60;  d = d / 60;
		h = d % 24;  d = d / 24;
		sprintf (buf, "%3dd %2d:%02d:%02d.%02d", d, h, m, s, f);
		Tcl_DStringAppendElement (&pdu->varbind, buf);
            }
            break;
	  case ASN1_Counter64:
	    {
		if (sizeof (int) >= 8) {
		    packet = ASN1_DecodeInt (packet, &pdulen,
					     ASN1_Counter64, &int_val);
		    if (packet == NULL) goto asn1Error;
		    sprintf (buf, "%u", int_val);
		    Tcl_DStringAppendElement (&pdu->varbind, buf);
		} else {
		    double d;
		    packet = ASN1_DecodeCounter64 (packet, &pdulen, &d);
		    if (packet == NULL) goto asn1Error;
		    Tcl_PrintDouble (interp, d, buf);
		    Tcl_DStringAppendElement (&pdu->varbind, buf);
		}
	    }
	    break;
	  case ASN1_NULL:
            packet += 2;
            pdulen += 2;
	    Tcl_DStringAppendElement (&pdu->varbind, "");
            break;
	  case ASN1_OBJECT_IDENTIFIER:
            packet = ASN1_DecodeOID (packet, &pdulen, oid, &oidlen);
            if (packet == NULL) goto asn1Error;
	    {   char *soid = ASN1_Oid2Str (oid, oidlen);
		char *tmp = MIB_Name (soid, 0);
		if (tmp) {
		    Tcl_DStringAppendElement (&pdu->varbind, tmp);
		} else {
		    Tcl_DStringAppendElement (&pdu->varbind, soid);
		}
            }
            break;
	  case ASN1_IpAddress:
            packet = ASN1_DecodeOctetString (packet, &pdulen, ASN1_IpAddress, 
					     (u_char **) &freeme, &int_val);
            if (packet == NULL) goto asn1Error;
	    if (int_val != 4) goto asn1Error;
            Tcl_DStringAppend (&pdu->varbind, " ", 1);
	    {
		struct sockaddr_in addr;
		memcpy (&addr.sin_addr, freeme, 4);
		Tcl_DStringAppendElement (&pdu->varbind, 
					  inet_ntoa(addr.sin_addr));
	    }
            break;
	  case ASN1_OCTET_STRING:
            packet = ASN1_DecodeOctetString (packet, &pdulen, 
					     ASN1_OCTET_STRING, 
					     (u_char **) &freeme, &int_val);
            if (packet == NULL) goto asn1Error;
	    {   char *tmp;
		static char *hex = NULL;
		static int hexLen = 0;
		if (hexLen < int_val * 3 + 1) {
		    if (hex) ckfree (hex);
		    hexLen = int_val * 3 + 1;
		    hex = ckalloc (hexLen);
		}
		SNMP_BinToHex (freeme, int_val, hex);
		tmp = MIB_Format (vboid, 0, hex);
		if (tmp) {
		    Tcl_DStringAppendElement (&pdu->varbind, tmp);
		} else {
		    Tcl_DStringAppendElement (&pdu->varbind, hex);
		}
	    }
            break;
	  default:
            sprintf (interp->result, "unknown asn1 type 0x%.2x", *packet);
	    snmpStats.snmpInASNParseErrs++;
            return TCL_ERROR;
	}
	
      nextVarBind:
	Tcl_DStringEndSublist (&pdu->varbind);
    }

    /*
     * Add the enterprise object identifier to the varbind list.
     * See the definition of snmpTrapEnterprise of details.
     */

    if (pdu->type == SNMPv1_TRAP && snmpTrapEnterprise) {
	Tcl_DStringStartSublist (&pdu->varbind);
	Tcl_DStringAppendElement (&pdu->varbind, "1.3.6.1.6.3.1.1.4.3.0");
        Tcl_DStringAppendElement (&pdu->varbind, "OBJECT IDENTIFIER");
	Tcl_DStringAppendElement (&pdu->varbind, snmpTrapEnterprise);
	Tcl_DStringEndSublist (&pdu->varbind);
	ckfree (snmpTrapEnterprise);
    }
    
    *packetlen += pdulen;
    
    if (pdulen != deflen) {
	sprintf (interp->result,
		 "PDU sequence length (%d) differs from real length (%d).",
		 pdulen, (int) deflen);
	snmpStats.snmpInASNParseErrs++;
	return TCL_ERROR;
    }

    return TCL_OK;
    
  asn1Error:
    Tcl_SetResult (interp, ASN1_ErrorString(), TCL_STATIC);
    snmpStats.snmpInASNParseErrs++;
    return TCL_ERROR;

  trapError:
    return TCL_OK;
}

