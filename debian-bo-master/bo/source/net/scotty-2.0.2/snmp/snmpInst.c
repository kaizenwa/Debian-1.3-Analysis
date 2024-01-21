/*
 * snmpTree.c
 *
 * Utilities to organize the tree of SNMP instances maintained by
 * agent sessions.
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
 * The root of the tree containing all MIB instances.
 */

static SNMP_Instance *instTree = NULL;

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
WalkInstances			_ANSI_ARGS_((Tcl_Interp *interp, 
					     SNMP_Instance *root, 
					     char *varName, char *body));
static SNMP_Instance*
MallocInstance			_ANSI_ARGS_((void));

static void
FreeInstance			_ANSI_ARGS_((SNMP_Instance *inst));

static SNMP_Instance*
TreeAdd				_ANSI_ARGS_((char *id, int offset, 
					     int syntax, int access,
					     char *tclVarName));
static void
TreeRemove			_ANSI_ARGS_((SNMP_Instance *tree,
					     char *varname));
static SNMP_Instance*
TreeFind			_ANSI_ARGS_((SNMP_Instance *tree,
					     ASN1_OID *oid, int len));
static SNMP_Instance*
TreeFindNext			_ANSI_ARGS_((SNMP_Instance *tree,
					     ASN1_OID *oid, int len));
static char*
TraceDeleteInstance		_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp,
					     char *name1, char *name2, 
					     int flags));


static int
WalkInstances (interp, root, varName, body)
     Tcl_Interp *interp;
     SNMP_Instance *root;
     char *varName;
     char *body;
{
    SNMP_Instance *instPtr;
    int result = TCL_OK;
    char *value;
    
    for (instPtr = root; instPtr; instPtr = instPtr->nextPtr) {

	value = Tcl_SetVar (interp, varName, 
			    instPtr->label ? instPtr->label : "", 
			    TCL_LEAVE_ERR_MSG);
	if (value && instPtr->tclVarName) {
	    value = Tcl_SetVar (interp, varName, instPtr->tclVarName,
				TCL_APPEND_VALUE | TCL_LIST_ELEMENT 
				| TCL_LEAVE_ERR_MSG);
	}
        if (! value) {
            result = TCL_ERROR;
            goto loopDone;
        }

        result = Tcl_Eval (interp, body);

	if ((result == TCL_OK || result == TCL_CONTINUE) 
	    && instPtr->childPtr) {
	    WalkInstances (interp, instPtr->childPtr, varName, body);
	}
	
	if (result != TCL_OK) {
	    if (result == TCL_CONTINUE) {
		result = TCL_OK;
	    } else if (result == TCL_BREAK) {
		goto loopDone;
	    } else if (result == TCL_ERROR) {
		char msg[100];
		sprintf (msg, "\n    (\"snmp walk\" body line %d)",
			 interp->errorLine);
		Tcl_AddErrorInfo (interp, msg);
		goto loopDone;
	    } else {
		goto loopDone;
	    }
	}
    }

  loopDone:

    if (result == TCL_OK) {
	Tcl_ResetResult (interp);
    }
    return result;
}


/*
 * SNMP_AgentWalk() walks through all the instances in the MIB 
 * tree and evaluates the body for each node in the tree.
 */

int
SNMP_AgentWalk (interp, varName, body)
     Tcl_Interp *interp;
     char *varName;
     char *body;
{
    return WalkInstances (interp, instTree, varName, body);
}


/*
 * MallocInstance() allocates memory for a new instance.
 */

static SNMP_Instance *
MallocInstance ()
{
    SNMP_Instance *instPtr;
    instPtr = (SNMP_Instance *) ckalloc (sizeof (SNMP_Instance));
    memset ((char *) instPtr, '\0', sizeof (SNMP_Instance));
    return instPtr;
}


/*
 * FreeInstance() frees an instance and all associated memory.
 */

static void
FreeInstance (instPtr)
     SNMP_Instance *instPtr;
{

    if (instPtr->label) {
	ckfree (instPtr->label);
    }
    if (instPtr->tclVarName) {
	ckfree (instPtr->tclVarName);
    }
    while (instPtr->bindings) {
	SNMP_Binding *bindPtr = instPtr->bindings;
	instPtr->bindings = instPtr->bindings->nextPtr;
	if (bindPtr->command) {
	    ckfree (bindPtr->command);
	}
	ckfree ((char *) bindPtr);
    }
    ckfree ((char *) instPtr);
}


/*
 * TreeAdd() adds a new instance to the tree of instances.
 */

static SNMP_Instance*
TreeAdd (soid, offset, syntax, access, tclVarName)
     char *soid;
     int offset;
     int syntax;
     int access;
     char *tclVarName;
{
    ASN1_OID *oid;
    int i, oidlen;
    SNMP_Instance *p, *q = NULL;

    if (instTree == NULL) {
	instTree = MallocInstance ();
	instTree->label = "1";
	instTree->subid = 1;
    }

    oid = ASN1_Str2Oid (soid, &oidlen);
    if (oid[0] != 1 || oidlen < 1) {
	return NULL;
    }
    if (oidlen == 1 && oid[0] == 1) {
        return instTree;
    }

    for (p = instTree, i = 1; i < oidlen; p = q, i++) {
	for (q = p->childPtr; q; q = q->nextPtr) {
	    if (q->subid == oid[i]) break;
	}
	if (! q) {

	    /*
	     * Create new intermediate nodes.
	     */

	    SNMP_Instance *n;
	    char *s = ASN1_Oid2Str (oid, i+1);
	    
	    n = MallocInstance ();
	    n->label = ckstrdup (s);
	    n->subid = oid[i];

	    if (! p->childPtr) {			/* first node  */
		p->childPtr = n;

	    } else if (p->childPtr->subid > oid[i]) {	/* insert head */
		n->nextPtr = p->childPtr;
		p->childPtr = n;

	    } else {					/* somewhere else */
		for (q = p->childPtr; q->nextPtr && q->nextPtr->subid < oid[i]; 
		     q = q->nextPtr) ;
		if (q->nextPtr && q->nextPtr->subid == oid[i]) {
		    continue;
		}
		n->nextPtr = q->nextPtr;
		q->nextPtr = n;
	    }

	    q = n;
	}
    }

    if (q) {
	if (q->label)      ckfree (q->label);
	if (q->tclVarName) ckfree (q->tclVarName);
	
	q->label  = soid;
	q->offset = offset;
	q->syntax = syntax;
	q->access = access;
	q->tclVarName = tclVarName;
    }
  
    return q;
}


/*
 * TreeFindNext() locates the lexikographic next instance in the tree.
 */

static SNMP_Instance*
TreeFindNext (tree, oid, len)
     SNMP_Instance *tree;
     ASN1_OID *oid;
     int len;
{
    SNMP_Instance *p, *inst;
    static int force = 0;

    /*
     * Reset the force flag if we start a new search from the root of
     * the instance tree. The flag will be set whenever we decide that
     * the next instance we find will be a good candidate.
     */

    if (tree == instTree) {
	force = 0;
    }

    /*
     * Skip over all subidentifier that are lower than the
     * subidentifier of interest.
     */

    p = tree;
    if (len && oid) {
	while (p && p->subid < oid[0]) p = p->nextPtr;
    }

    /*
     * Loop over all neighbours at this tree level. Decend if possible.
     */

    while (p) {
	if (p->childPtr) {
	    if (len > 0 && p->subid == oid[0]) {
		inst = TreeFindNext (p->childPtr, oid + 1, len - 1);
	    } else {
		inst = TreeFindNext (p->childPtr, NULL, 0);
	    }
	    if (inst) return inst;
	} else {
	    if (len == 0) {
		return p;
	    } else if ((len != 1 || p->subid != oid[0]) && force) {
		return p;
	    } else {
		force = 1;
	    }
	}
	p = p->nextPtr;
    }

    return NULL;
}


/*
 * TreeFind() locates an instance in the tree.
 */

static SNMP_Instance*
TreeFind (tree, oid, len)
     SNMP_Instance *tree;
     ASN1_OID *oid;
     int len;
{
    SNMP_Instance *p, *q = NULL;
    int i;
    
    if (oid[0] != 1) return NULL;
    for (p = tree, i = 1; p && i < len; p = q, i++) {
	for (q = p->childPtr; q; q = q->nextPtr) {
	    if (q->subid == oid[i]) break;
	}
	if (!q) {
	    return NULL; 
	}
    }
    return q;
}


/*
 * TreeRemove() deletes all nodes from the tree that are associated
 * with a given tcl variable.
 */

static void
TreeRemove (tree, varName)
     SNMP_Instance *tree;
     char *varName;
{
    SNMP_Instance *p, *q;

    if (!tree) return;

    for (p = tree; p; p = p->nextPtr) {
	if (p->childPtr) {
	    q = p->childPtr;
	    TreeRemove (q, varName);
	    if (q->tclVarName && (strcmp (q->tclVarName, varName) == 0)) {
		p->childPtr = q->nextPtr;
		FreeInstance (q);
	    }
	}
	if (p->nextPtr) {
	    q = p->nextPtr;
	    if (q->tclVarName && (strcmp (q->tclVarName, varName) == 0)) {
		p->nextPtr = q->nextPtr;
		FreeInstance (q);
	    }
	}
    }
}


/*
 * TraceDeleteInstance() is a trace callback which is called by the
 * Tcl interpreter whenever a MIB variable is removed. We have to 
 * run through the whole tree to discard these variables.
 */

static char*
TraceDeleteInstance (clientData, interp, name1, name2, flags)
     ClientData clientData;
     Tcl_Interp *interp;
     char *name1;
     char *name2;
     int flags;
{
    int len = strlen (name1);
    char *varName;
			 
    if (name2) {
	len += strlen (name2);
    }
    varName = ckalloc (len + 3);
    strcpy (varName, name1);
    if (name2) {
	strcat (varName,"(");
	strcat (varName, name2);
	strcat (varName,")");
    }

    TreeRemove (instTree, varName);
    ckfree (varName);
    return NULL;
}


/*
 * SNMP_CreateInst() creates a new instance in the instance tree
 * and a Tcl array variable that will be used to access and modify the
 * instance from within tcl.  
 */

int
SNMP_CreateInst (interp, label, tclVarName, defval)
     Tcl_Interp *interp;
     char *label;
     char *tclVarName;
     char *defval;
{
    char *soid = NULL;
    MIB_Node *nodePtr = MIB_FindNode (label, NULL, 0);
    int access, offset = 0, syntax = 0;

    if (!nodePtr || nodePtr->childPtr) {
	Tcl_AppendResult (interp, "unknown object type \"", label, "\"", 
			  (char *) NULL);
	return TCL_ERROR;
    }

    soid = ckstrdup (MIB_Oid (label, 0));

    /*
     * Calculate the instance identifier. Return an error if we
     * have no real instance. Otherwise save a pointer to the
     * instance identifier so that we can access it later.
     */

    {
	int oidLen;
	ASN1_OID *oid;
	MIB_Node *basePtr = NULL;
	char *freeme = NULL;

	for (oid = ASN1_Str2Oid (soid, &oidLen); oidLen; oidLen--) {
	    freeme = ASN1_Oid2Str (oid, oidLen);
	    basePtr = MIB_FindNode (freeme, NULL, 1);
	    if (basePtr) break;
	}

	if (! basePtr || strlen (soid) <= strlen (freeme)) {
	    Tcl_AppendResult (interp, "no instance identifier in \"",
			      label, "\"", (char *) NULL);
	    return TCL_ERROR;
	}

	if (freeme) {
	    offset = strlen(freeme)+1;
	}
    }

    syntax = MIB_ASN1 (label, 0);

    access = nodePtr->access;
    if (access == M_NOACCESS) {
	Tcl_AppendResult (interp, "object \"", label, "\" is not accessible",
			  TCL_STATIC);
	goto errorExit;
    }

    if (defval) {
	if (Tcl_SetVar (interp, tclVarName, defval, 
			TCL_GLOBAL_ONLY | TCL_LEAVE_ERR_MSG) == NULL) {
	    goto errorExit;
	}
    }

    TreeAdd (soid, offset, syntax, access, ckstrdup (tclVarName));
    Tcl_TraceVar (interp, tclVarName, TCL_TRACE_UNSETS | TCL_GLOBAL_ONLY, 
		  TraceDeleteInstance, (ClientData) NULL);
    Tcl_ResetResult (interp);
    return TCL_OK;

  errorExit:
    if (soid) ckfree (soid);
    return TCL_ERROR;
}


/*
 * SNMP_FindInst() locates the instance given by the oid in the 
 * instance tree. Ignore all tree nodes without a valid syntax. 
 * They are only used internally as non leaf nodes.
 */

SNMP_Instance*
SNMP_FindInst (session, oid, oidlen)
     SNMP_Session *session;
     ASN1_OID *oid;
     int oidlen;
{
    SNMP_Instance *inst = TreeFind (instTree, oid, oidlen);
    return (inst && inst->syntax) ? inst : NULL;
}

/*
 * SNMP_FindNextInst() locates the next instance given by the oid in 
 * the instance tree. Ignore all tree nodes without a valid syntax. 
 * They are only used internally as non leaf nodes.
 */


SNMP_Instance*
SNMP_FindNextInst (session, oid, oidlen)
     SNMP_Session *session;
     ASN1_OID *oid;
     int oidlen;
{
    SNMP_Instance *inst = TreeFindNext (instTree, oid, oidlen);
    return (inst && inst->syntax) ? inst : NULL;
}


/*
 * SNMP_CreateInstBinding() creates and modifies SNMP event 
 * bindings for instances.
 */

int
SNMP_CreateInstBinding (session, oid, oidlen, event, command)
     SNMP_Session *session;
     ASN1_OID *oid;
     int oidlen;
     int event;
     char *command;
{
    SNMP_Instance *inst = NULL;
    SNMP_Binding *bindPtr = NULL;

    /*
     * Create an anonymous node if there is no instance known yet.
     */
	
    inst = TreeFind (instTree, oid, oidlen);
    if (!inst) {
	inst = TreeAdd (ckstrdup (ASN1_Oid2Str(oid, oidlen)), 0, 0, 0, NULL);
	if (! inst) {
	    return TCL_ERROR;
	}
    }

    /*
     * Check if we already have a binding for this event type.
     */

    for (bindPtr = inst->bindings; bindPtr; bindPtr = bindPtr->nextPtr) {
	if (bindPtr->event == event) break;
    }

    /*
     * Create a new binding if necessary. Overwrite already
     * existing bindings.
     */

    if (command) {
	if (!bindPtr) {
	    bindPtr = (SNMP_Binding *) ckalloc (sizeof (SNMP_Binding));
	    memset ((char *) bindPtr, '\0', sizeof (SNMP_Binding));
	    bindPtr->event = event;
	    bindPtr->nextPtr = inst->bindings;
	    inst->bindings = bindPtr;
	}
	if (bindPtr->command) {
	    ckfree (bindPtr->command);
	    bindPtr->command = NULL;
	}
	if (*command != '\0') {
	    bindPtr->command = ckstrdup (command);
	}
    }

    return TCL_OK;
}

/*
 * SNMP_GetInstBinding() retrieves the current binding for the given
 * MIB instance and event type. We return a NULL pointer if there
 * is either no instance of if there is no binding for an instance.
 */

char*
SNMP_GetInstBinding (session, oid, oidlen, event)
     SNMP_Session *session;
     ASN1_OID *oid;
     int oidlen;
     int event;
{
    SNMP_Instance *inst = NULL;
    SNMP_Binding *bindPtr = NULL;

    inst = TreeFind (instTree, oid, oidlen);
    if (! inst) {
	return NULL;
    }

    for (bindPtr = inst->bindings; bindPtr; bindPtr = bindPtr->nextPtr) {
	if (bindPtr->event == event) break;
    }
    if (! bindPtr) {
	return NULL;
    }

    return bindPtr->command;
}

/*
 * SNMP_EvalInstBinding() evaluates a binding for this object. We
 * start at the given instance and follow the oid to the top of the
 * instance tree. We evaluate all bindings during our walk up the
 * tree. A break return code can be used to stop this process.
 */

int
SNMP_EvalInstBinding (session, pdu, inst, event, value)
     SNMP_Session *session;
     SNMP_PDU *pdu;
     SNMP_Instance *inst;
     int event;
     char *value;
{
    int oidlen;
    ASN1_OID *oid;
    int code = TCL_OK;
    char *instOid;
    
    oid = ASN1_Str2Oid (inst->label, &oidlen);
    instOid = inst->label+inst->offset;

    for (; oidlen > 0; oidlen--) {
	SNMP_Binding *bindPtr;
	
	inst = TreeFind (instTree, oid, oidlen);
	if (!inst) continue;

	for (bindPtr = inst->bindings; bindPtr; bindPtr = bindPtr->nextPtr) {
	    if (bindPtr->event == event) break;
	}

	if (bindPtr) {

	    /*
	     * Evaluate the binding and check if the instance is still
	     * there after we complete the binding. It may have been
	     * deleted in the callback. Also make sure that we have an
	     * error status of E_NOERROR and error index 0 during the
	     * callback.
	     */

	    int error_status = pdu->error_status;
	    int error_index  = pdu->error_index;
	    pdu->error_status = E_NOERROR;
	    pdu->error_index  = 0;
	    code = SNMP_EvalCallback (session->agentInterp, session,
				      pdu, bindPtr->command,
				      inst->label, instOid, value);
	    pdu->error_status = error_status;
	    pdu->error_index  = error_index;

	    if (code == TCL_OK && !TreeFind (instTree, oid, oidlen)) {
	        code = TCL_ERROR;
	    }
	    if (code == TCL_BREAK || code == TCL_ERROR) break;
	}
    }

    return code;
}

