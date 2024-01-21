/*
 * snmpAgent.c
 *
 * This is the SNMP agent interface of scotty.
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

#include "snmp.h"
#include "mib.h"

/*
 * The following structure is used to implement a cache that
 * is used to remember queries so that we can respond to
 * retries quickly. This is needed because side effects can
 * break the agent down if we do them for each retry.
 */

typedef struct CacheElement {
    SNMP_Session *session;
    SNMP_PDU pdu;
} CacheElement;

#define CACHE_SIZE 64
static CacheElement cache[CACHE_SIZE];

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
CacheInit		_ANSI_ARGS_((void));

static SNMP_PDU*
CacheGet		_ANSI_ARGS_((SNMP_Session *session));

static int
CacheHit		_ANSI_ARGS_((Tcl_Interp *interp,
				     SNMP_Session *session, int id));
static char*
TraceSysUpTime		_ANSI_ARGS_((ClientData clientData,
				     Tcl_Interp *interp,
				     char *name1, char *name2, int flags));
static char*
TraceUnsignedInt	_ANSI_ARGS_((ClientData clientData,
				     Tcl_Interp *interp,
				     char *name1, char *name2, int flags));
static int
GetRequest		_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
				     SNMP_PDU *request, SNMP_PDU *response));
static int
SetRequest		_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
				     SNMP_PDU *request, SNMP_PDU *response));


/*
 * The global variable to keep the snmp statistics is defined here.
 */

SNMP_Statistics snmpStats;

/*
 * The following table is used to register build-in instances
 * bound to counter inside of the protocol stack.
 */

struct StatReg {
    char *name;
    unsigned int *value;
};

static struct StatReg statTable[] = {
    { "snmpInPkts.0",		      &snmpStats.snmpInPkts },
    { "snmpOutPkts.0",		      &snmpStats.snmpOutPkts },
    { "snmpInBadVersions.0",	      &snmpStats.snmpInBadVersions },
    { "snmpInBadCommunityNames.0",    &snmpStats.snmpInBadCommunityNames },
    { "snmpInBadCommunityUses.0",     &snmpStats.snmpInBadCommunityUses },
    { "snmpInASNParseErrs.0",	      &snmpStats.snmpInASNParseErrs },
    { "snmpInTooBigs.0",	      &snmpStats.snmpInTooBigs },
    { "snmpInNoSuchNames.0",	      &snmpStats.snmpInNoSuchNames },
    { "snmpInBadValues.0",	      &snmpStats.snmpInBadValues },
    { "snmpInReadOnlys.0",	      &snmpStats.snmpInReadOnlys },
    { "snmpInGenErrs.0",	      &snmpStats.snmpInGenErrs },
    { "snmpInTotalReqVars.0",	      &snmpStats.snmpInTotalReqVars },
    { "snmpInTotalSetVars.0",	      &snmpStats.snmpInTotalSetVars },
    { "snmpInGetRequests.0",	      &snmpStats.snmpInGetRequests },
    { "snmpInGetNexts.0",	      &snmpStats.snmpInGetNexts },
    { "snmpInSetRequests.0",	      &snmpStats.snmpInSetRequests },
    { "snmpInGetResponses.0",	      &snmpStats.snmpInGetResponses },
    { "snmpInTraps.0",		      &snmpStats.snmpInTraps },
    { "snmpOutTooBigs.0",	      &snmpStats.snmpOutTooBigs },
    { "snmpOutNoSuchNames.0",	      &snmpStats.snmpOutNoSuchNames },
    { "snmpOutBadValues.0",	      &snmpStats.snmpOutBadValues },
    { "snmpOutGenErrs.0",	      &snmpStats.snmpOutGenErrs },
    { "snmpOutGetRequests.0",	      &snmpStats.snmpOutGetRequests },
    { "snmpOutGetNexts.0",	      &snmpStats.snmpOutGetNexts },
    { "snmpOutSetRequests.0",	      &snmpStats.snmpOutSetRequests },
    { "snmpOutGetResponses.0",	      &snmpStats.snmpOutGetResponses },
    { "snmpOutTraps.0",		      &snmpStats.snmpOutTraps },
    { "snmpStatsPackets.0",	      &snmpStats.snmpStatsPackets },
    { "snmpStats30Something.0",	      &snmpStats.snmpStats30Something },
    { "snmpStatsUnknownDstParties.0", &snmpStats.snmpStatsUnknownDstParties },
    { "snmpStatsDstPartyMismatches.0",&snmpStats.snmpStatsDstPartyMismatches },
    { "snmpStatsUnknownSrcParties.0", &snmpStats.snmpStatsUnknownSrcParties},
    { "snmpStatsBadAuths.0",	      &snmpStats.snmpStatsBadAuths },
    { "snmpStatsNotInLifetimes.0",    &snmpStats.snmpStatsNotInLifetimes },
    { "snmpStatsWrongDigestValues.0", &snmpStats.snmpStatsWrongDigestValues },
    { "snmpStatsUnknownContexts.0",   &snmpStats.snmpStatsUnknownContexts },
    { "snmpStatsBadOperations.0",     &snmpStats.snmpStatsBadOperations },
    { "snmpStatsSilentDrops.0",	      &snmpStats.snmpStatsSilentDrops },
    { "snmpV1BadCommunityNames.0",    &snmpStats.snmpInBadCommunityNames },
    { "snmpV1BadCommunityUses.0",     &snmpStats.snmpInBadCommunityUses },
#ifdef SNMPv2USEC
    { "usecStatsUnsupportedQoS.0",    &snmpStats.usecStatsUnsupportedQoS },
    { "usecStatsNotInWindows.0",      &snmpStats.usecStatsNotInWindows },
    { "usecStatsUnknownUserNames.0",  &snmpStats.usecStatsUnknownUserNames },
    { "usecStatsWrongDigestValues.0", &snmpStats.usecStatsWrongDigestValues },
    { "usecStatsUnknownContextSelectors.0", 
	&snmpStats.usecStatsUnknownContextSelectors },
#endif
    { 0, 0 }
};


/*
 * CacheInit() initializes the cache of answered requests. This is 
 * called only once when we become an agent.
 */

static void
CacheInit ()
{
    int i;
    memset ((char *) cache, '\0', sizeof (cache));
    for (i = 0; i <  CACHE_SIZE; i++) {
	Tcl_DStringInit (&cache[i].pdu.varbind);
    }
}


/*
 * CacheGet() gets a free cache element. The cache is actually a
 * FIFO ring buffer, so we just return the next element.
 */

static SNMP_PDU*
CacheGet (session)
     SNMP_Session *session;
{
    static last = 0;
    last = (last + 1 ) % CACHE_SIZE;
    Tcl_DStringFree (&cache[last].pdu.varbind);
    cache[last].session = session;
    return &(cache[last].pdu);
}


/*
 * CacheHit() checks if the request identified by session and
 * request id is in the cache so we can send the answer without
 * further processing. It returns a standard Tcl result if found
 * and -1 if no hit was made.
 */

static int
CacheHit (interp, session, id)
     Tcl_Interp *interp;
     SNMP_Session *session;
     int id;
{
    int i, code;
    for (i = 0; i < CACHE_SIZE; i++) {
	if (cache[i].pdu.request_id == id && cache[i].session == session) {
	    code = SNMP_Encode (interp, cache[i].session, &cache[i].pdu, NULL);
	    return code;
	}
    }
    return -1;
}


/*
 * TraceSysUpTime() is a trace callback which is called by the
 * tcl interpreter whenever the sysUpTime variable is read.
 */

static char*
TraceSysUpTime (clientData, interp, name1, name2, flags)
     ClientData clientData;
     Tcl_Interp *interp;
     char *name1;
     char *name2;
     int flags;
{
    char buf[20];
    sprintf (buf, "%u", SNMP_SysUpTime());
    Tcl_SetVar (interp, name1, buf, TCL_GLOBAL_ONLY);
    return NULL;
}


/*
 * TraceUnsignedInt writes the unsigned value pointed to by
 * clientData into the tcl variable under trace. Used to implement 
 * snmp statistics.
 */

static char*
TraceUnsignedInt (clientData, interp, name1, name2, flags)
     ClientData clientData;
     Tcl_Interp *interp;
     char *name1;
     char *name2;
     int flags;
{
    char buf[20];
    sprintf (buf, "%u", *(unsigned *) clientData);
    Tcl_SetVar (interp, name1, buf, TCL_GLOBAL_ONLY);
    return NULL;    
}

/*
 * SNMP_AgentInit() initializes the agent by registering some 
 * default variables.  
 */

void
SNMP_AgentInit (interp)
     Tcl_Interp *interp;
{
    static done = 0;
    char tclvar[80];
    struct StatReg *p;

    if (done) return;
    done = 1;

    CacheInit();

    SNMP_CreateInst (interp, "sysDescr.0", "sysDescr", 
		     "scotty midlevel agent");
    SNMP_CreateInst (interp, "sysObjectID.0", "sysObjectID",
		     "1.3.6.1.4.1.1701.1");
    SNMP_CreateInst (interp, "sysUpTime.0", "sysUpTime", "0");
    Tcl_TraceVar (interp, "sysUpTime", TCL_TRACE_READS | TCL_GLOBAL_ONLY, 
		  TraceSysUpTime, (ClientData) NULL);
    SNMP_CreateInst (interp, "sysContact.0", "sysContact", "");
    SNMP_CreateInst (interp, "sysName.0", "sysName", "");
    SNMP_CreateInst (interp, "sysLocation.0", "sysLocation", "");
    SNMP_CreateInst (interp, "sysServices.0", "sysServices", "72");
    SNMP_CreateInst (interp, "scottyVersion.0", "scotty_version", NULL);

    for (p = statTable; p->name; p++) {
	strcpy (tclvar, p->name);
	SNMP_CreateInst (interp, p->name, tclvar, "");
	Tcl_TraceVar (interp, tclvar, TCL_TRACE_READS | TCL_GLOBAL_ONLY,
		      TraceUnsignedInt, (ClientData) p->value);
    }

    /* XXX snmpEnableAuthenTraps.0 should be implemented */

    Tcl_ResetResult (interp);
}

/*
 * GetRequest() is called to process get, getnext and getbulk requests.
 */

static int
GetRequest (interp, session, request, response)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *request;
     SNMP_PDU *response;
{
    int i, code, oidlen;
    ASN1_OID *oid;
    SNMP_VarBind *inVarBindPtr;
    int inVarBindSize;
    SNMP_Instance *inst;

    code = SNMP_SplitVarBindList (interp, Tcl_DStringValue(&request->varbind),
				  &inVarBindSize, &inVarBindPtr);
    if (code != TCL_OK) {
	return TCL_ERROR;
    }

    for (i = 0; i < inVarBindSize; i++) {

	char *value;

	oid = ASN1_Str2Oid (inVarBindPtr[i].soid, &oidlen);
	if (request->type == SNMP_GETNEXT || request->type == SNMPv2_GETBULK) {
	    inst = SNMP_FindNextInst (session, oid, oidlen);
	} else {
	    inst = SNMP_FindInst (session, oid, oidlen);
	}

	if (! inst) {
	    
	    /*
	     * SNMPv1 handles this case by sending back an error PDU
	     * while SNMPv2 uses exceptions. We create an exception
	     * varbind to reflect the error types defined in RFC 1448.
	     */

	    if (session->version == SNMPv1) {
		response->error_status = E_NOSUCHNAME;
		snmpStats.snmpOutNoSuchNames++;
		goto varBindError;
	    }

	    Tcl_DStringStartSublist (&response->varbind);
	    Tcl_DStringAppendElement (&response->varbind, inVarBindPtr[i].soid);
	    if (request->type == SNMP_GET) {
		MIB_Node *nodePtr = MIB_FindNode (inVarBindPtr[i].soid, 
						  NULL, 0);
		if (!nodePtr || nodePtr->childPtr) {
		    Tcl_DStringAppendElement (&response->varbind,
					      "noSuchObject");
		} else {
		    Tcl_DStringAppendElement (&response->varbind, 
					      "noSuchInstance");
		}
	    } else {
		Tcl_DStringAppendElement (&response->varbind, "endOfMibView");
	    }
	    Tcl_DStringAppendElement (&response->varbind, "");
	    Tcl_DStringEndSublist (&response->varbind);
	    continue;
	}

	Tcl_DStringStartSublist (&response->varbind);
	Tcl_DStringAppendElement (&response->varbind, inst->label);
	Tcl_DStringAppendElement (&response->varbind, 
				  ASN1_Sntx2Str (inst->syntax));
	code = SNMP_EvalInstBinding (session, request, inst, SNMP_GET_EVENT, 
				     inVarBindPtr[i].value);
	if (code == TCL_ERROR) {
	    goto varBindTclError;
	}
	value = Tcl_GetVar (interp, inst->tclVarName, 
			    TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
	if (!value) {
	    response->error_status = E_GENERR;
	    goto varBindError;
	}
	Tcl_DStringAppendElement (&response->varbind, value);
	Tcl_ResetResult (interp);

	snmpStats.snmpInTotalReqVars++;

	Tcl_DStringEndSublist (&response->varbind);
	
	continue;

      varBindTclError:
	response->error_status = SNMP_Str2Err (interp->result);
	if (response->error_status < 0) {
	    response->error_status = E_GENERR;
	}
	snmpStats.snmpOutGenErrs += (response->error_status == E_GENERR);
       
      varBindError:
	response->error_index = i+1;
	break;
    }

    /*
     * We check here if the varbind length in string representation
     * exceeds our buffer used to build the packet. This is not always
     * correct, but we should be on the safe side in most cases.
     */

    if (Tcl_DStringLength (&response->varbind) >= BUFSIZE) {
	response->error_status = E_TOOBIG;
	response->error_index = 0;
    }

    SNMP_FreeVarBindList (inVarBindSize, inVarBindPtr);
    return TCL_OK;
}


/*
 * SetRequest() is called to process set requests, which is much more
 * difficult to do than get* requests.
 */

static int
SetRequest (interp, session, request, response)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *request;
     SNMP_PDU *response;
{
    int i, code, oidlen;
    ASN1_OID *oid;
    SNMP_VarBind *inVarBindPtr;
    int inVarBindSize;
    SNMP_Instance *inst;	

    code = SNMP_SplitVarBindList (interp, Tcl_DStringValue(&request->varbind),
				  &inVarBindSize, &inVarBindPtr);
    if (code != TCL_OK) {
	return TCL_ERROR;
    }

    for (i = 0; i < inVarBindSize; i++) {

	char *value;
	int setAlreadyDone = 0;
	
	oid = ASN1_Str2Oid (inVarBindPtr[i].soid, &oidlen);
	inst = SNMP_FindInst (session, oid, oidlen);

	if (! inst) {
	    char *access = MIB_Access (inVarBindPtr[i].soid, 0);
	    if (access && (strcmp (access, "read-create") == 0)) {
		char *name = MIB_Name (inVarBindPtr[i].soid, 0);
		char *tmp = ckalloc (strlen(name) + 2);
		char *c = tmp;
		strcpy (tmp, name);
		for (c = tmp; *c && *c != '.'; c++);
		if (*c) *c = '(';
		while (*c) c++;
		*c++ = ')';
		*c = '\0';
		SNMP_CreateInst (interp, inVarBindPtr[i].soid, tmp, "");
		ckfree (tmp);
		inst = SNMP_FindInst (session, oid, oidlen);
		code = SNMP_EvalInstBinding (session, request, inst, 
					     SNMP_CREATE_EVENT,
					     inVarBindPtr[i].value);
		if (code == TCL_ERROR) {
		    goto varBindTclError;
		}
		if (code != TCL_BREAK) {
		    Tcl_SetVar (interp, inst->tclVarName, 
				inVarBindPtr[i].value, TCL_GLOBAL_ONLY);
		}
		setAlreadyDone = 1;
	    }
	}

	if (! inst) {
	    
	    /*
	     * SNMPv1 handles this case by sending back an error PDU
	     * while SNMPv2 uses exceptions. We create an exception
	     * varbind to reflect the error types defined in RFC 1448.
	     */

	    if (session->version == SNMPv1) {
		response->error_status = E_NOSUCHNAME;
		snmpStats.snmpOutNoSuchNames++;
		goto varBindError;
	    }

	    Tcl_DStringStartSublist (&response->varbind);
	    Tcl_DStringAppendElement (&response->varbind, 
				      inVarBindPtr[i].soid);
	    /* XXX what about notWritable ? */
	    Tcl_DStringAppendElement (&response->varbind, "noAccess");
	    Tcl_DStringAppendElement (&response->varbind, "");
	    Tcl_DStringEndSublist (&response->varbind);
	    continue;
	}

	if (!setAlreadyDone) {

	    /*
	     * Check if the instance is writable.
	     */

	    if (inst->access == M_READONLY) {
		response->error_status = E_NOTWRITABLE;
		goto varBindError;
	    }

	    /*
	     * Check if the received value is of approriate type.
	     */
	    
	    if (ASN1_Str2Sntx(inVarBindPtr[i].syntax) != inst->syntax) {
		response->error_status = E_WRONGTYPE;
		goto varBindError;
	    }

	    code = SNMP_EvalInstBinding (session, request, inst, 
				       SNMP_SET_EVENT, inVarBindPtr[i].value);
	    if (code == TCL_ERROR) {
		goto varBindTclError;
	    }
	    if (code != TCL_BREAK) {
		value = Tcl_GetVar(interp, inst->tclVarName, 
				   TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
		inVarBindPtr[i].clientData = (ClientData) ckstrdup(value);
	        Tcl_SetVar(interp, inst->tclVarName, 
			   inVarBindPtr[i].value, TCL_GLOBAL_ONLY);
	    }
	    snmpStats.snmpInTotalSetVars++;
	}
	
	Tcl_DStringStartSublist (&response->varbind);
	Tcl_DStringAppendElement (&response->varbind, inst->label);
	Tcl_DStringAppendElement (&response->varbind, 
				  ASN1_Sntx2Str (inst->syntax));
	value = Tcl_GetVar (interp, inst->tclVarName, 
			    TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG);
	if (!value) {
	    response->error_status = E_GENERR;
	    goto varBindError;
	}
	Tcl_DStringAppendElement (&response->varbind, value);
	Tcl_ResetResult (interp);

	Tcl_DStringEndSublist (&response->varbind);
	
	continue;

      varBindTclError:
	response->error_status = SNMP_Str2Err (interp->result);
	if (response->error_status < 0) {
	    response->error_status = E_GENERR;
	}
	snmpStats.snmpOutGenErrs += (response->error_status == E_GENERR);
       
      varBindError:
	response->error_index = i+1;
	break;
    }

    /*
     * We check here if the varbind length in string representation
     * exceeds our buffer used to build the packet. This is not always
     * correct, but we should be on the safe side in most cases.
     */
    
    if (Tcl_DStringLength (&response->varbind) >= BUFSIZE) {
	response->error_status = E_TOOBIG;
	response->error_index = 0;
    }

    /*
     * Another check for consistency errors before we start 
     * the commit/rollback phase. This additional check was 
     * suggested by Peter.Polkinghorne@gec-hrc.co.uk.
     */
    
    if (response->error_status == E_NOERROR) {
	for (i = 0; i < inVarBindSize; i++) {
	    oid = ASN1_Str2Oid (inVarBindPtr[i].soid, &oidlen);
	    inst = SNMP_FindInst (session, oid, oidlen);
	    if (inst) {
		code = SNMP_EvalInstBinding (session, request, inst, 
					     SNMP_CHECK_EVENT,
					     inVarBindPtr[i].value);
	    } else {
	        Tcl_ResetResult (interp);
	        code = TCL_ERROR;
	    }

	    if (code != TCL_OK) {
		response->error_status = SNMP_Str2Err (interp->result);
		if (response->error_status < 0) {
		    response->error_status = E_GENERR;
		}
		snmpStats.snmpOutGenErrs +=
		    (response->error_status == E_GENERR);
		response->error_index = i+1;
		break;
	    }
	}
    }

    /*
     * We now start the commit/rollback phase. Note, we must be
     * careful to do rollbacks in the correct order and only
     * on those instances that were actually processed.
     */
	
    if (response->error_status == E_NOERROR) {

        /*
	 * Evaluate commit bindings if we have no error yet.
	 * Ignore all errors now since we have already decided 
	 * that this PDU has been processed successfully.
	 */
      
        for (i = 0; i < inVarBindSize; i++) {
	    oid = ASN1_Str2Oid (inVarBindPtr[i].soid, &oidlen);
	    inst = SNMP_FindInst (session, oid, oidlen);
	    if (inst) {
	        SNMP_EvalInstBinding (session, request, inst, 
				      SNMP_COMMIT_EVENT, 
				      inVarBindPtr[i].value);
	    }
	    if (inVarBindPtr[i].clientData) {
	        char *oldValue = (char *) inVarBindPtr[i].clientData;
	        ckfree(oldValue);
	        inVarBindPtr[i].clientData = NULL;
	    }
	}

    } else {

        /*
	 * Evaluate the rollback bindings for all the instances
	 * that have been processed so far. Ignore all errors now
	 * as they would overwrite the error that caused us to
	 * rollback the set request processing.
	 */

        int maxIndex = (inVarBindSize > response->error_index) 
	    ? inVarBindSize : response->error_index;

        for (i = maxIndex - 1; i >= 0; i--) {
	    oid = ASN1_Str2Oid (inVarBindPtr[i].soid, &oidlen);
	    inst = SNMP_FindInst (session, oid, oidlen);
	    if (inst) {
	        SNMP_EvalInstBinding (session, request, inst, 
				      SNMP_ROLLBACK_EVENT,
				      inVarBindPtr[i].value);
		if (inVarBindPtr[i].clientData) {
		    char *oldValue = (char *) inVarBindPtr[i].clientData;
		    Tcl_SetVar (interp, inst->tclVarName, 
				oldValue, TCL_GLOBAL_ONLY);
		    ckfree(oldValue);
		    inVarBindPtr[i].clientData = NULL;
		}
	    }
	}
    }

    SNMP_FreeVarBindList (inVarBindSize, inVarBindPtr);
    return TCL_OK;
}


/*
 * SNMP_AgentRequest() gets called when the agent receives a get or
 * set message. It splits the varbind, looks up the variables and
 * assembles an answer.
 */

int
SNMP_AgentRequest (interp, session, pdu)
     Tcl_Interp *interp;
     SNMP_Session *session;
     SNMP_PDU *pdu;
{
    int rc;
    SNMP_PDU *reply;

    switch (pdu->type) {
      case SNMP_GET:		snmpStats.snmpInGetRequests++; break;
      case SNMP_GETNEXT:	snmpStats.snmpInGetNexts++; break;
      case SNMPv2_GETBULK:	break;
      case SNMP_SET:		snmpStats.snmpInSetRequests++; break;
      case SNMPv2_INFORM:	break;
    }

    if ((rc = CacheHit (interp, session, pdu->request_id)) >= 0) {
	return rc;
    }

    SNMP_EvalBinding (interp, session, pdu, SNMP_BEGIN_EVENT);

    reply = CacheGet(session);
    reply->addr = pdu->addr;
    reply->request_id = 0;		/* set if everything is ok */
    reply->error_status = E_NOERROR;
    reply->error_index  = 0;

    if (pdu->type == SNMP_SET) {
	rc = SetRequest(interp, session, pdu, reply);
    } else {
	rc = GetRequest(interp, session, pdu, reply);
    }
    if (rc != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Throw the reply varbind away and re-use the one in the original
     * request if we had an error.
     */

    if (reply->error_status != E_NOERROR) {
	Tcl_DStringFree (&reply->varbind);
	Tcl_DStringAppend (&reply->varbind,
			   Tcl_DStringValue (&pdu->varbind),
			   Tcl_DStringLength (&pdu->varbind));
    }
 
    reply->type = SNMP_RESPONSE;
    reply->request_id = pdu->request_id;

    SNMP_EvalBinding (interp, session, reply, SNMP_END_EVENT);

    if (SNMP_Encode (interp, session, reply, NULL) != TCL_OK) {
	Tcl_AddErrorInfo (interp, "\n    (snmp send reply)");
        Tk_BackgroundError (interp);
	Tcl_ResetResult (interp);
	reply->error_status = E_GENERR;
	Tcl_DStringFree (&reply->varbind);
        Tcl_DStringAppend (&reply->varbind,
                           Tcl_DStringValue (&pdu->varbind),
                           Tcl_DStringLength (&pdu->varbind));
	return SNMP_Encode (interp, session, reply, NULL);
    } else {
	return TCL_OK;
    }
}
