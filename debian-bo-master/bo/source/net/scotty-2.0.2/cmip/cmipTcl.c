/*
 * cmipTcl.c
 *
 * Implementation of the "cmip" command, with options like:
 * connect, release, get, set, action, create and delete,
 * that implement a CMIP protocol
 *
 * Copyright (c) 1994
 *
 * M. Kernchen
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
 * See cmip.h for an explanation of this ugly define.
 */

#define INCLUDE_SYNTAXES

#include "cmip.h"

typedef struct CMIP_Token {
    Tcl_Interp		*interp;
    CMIP_Handle		*cmiph;
} CMIP_Token;

/*
 * A hash table used to hold all cmip handles.
 */

static Tcl_HashTable   cmipTable;

/*
 * Forward declarations for procedures defined later in this file:
 */

static CMIP_Handle*
MallocHandle	_ANSI_ARGS_((int msd, char *agent, char *host));

static void
FreeHandle	_ANSI_ARGS_((ClientData clientData));

static void
ReceiveProc	_ANSI_ARGS_((ClientData clientData, int mask));

static int
Terminate	_ANSI_ARGS_((CMIP_Handle *cmiph, Tcl_Interp *interp));

static int
Abort		_ANSI_ARGS_((CMIP_Handle *cmiph, Tcl_Interp *interp));

static void
WaitRequests	_ANSI_ARGS_((char *name));

static int
HandleCmd	_ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
			     int argc, char **argv));
static int
Connect		_ANSI_ARGS_((Tcl_Interp *interp, int argc, char **argv));

static int
CmipCmd		_ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
			     int argc, char **argv));


/*===========================================================================*/
/*
 * handling everything once a request has been sent
 */
int
CMIP_WaitResponse(interp, cmiph, type, callback)
     Tcl_Interp		*interp;
     CMIP_Handle	*cmiph;
     int		 type;
     char		*callback;
{
    CMIP_Request *rh;

    Tcl_HashEntry *ht_entry;

    char	buf[28];
    int		status, flag;

    /* compose the request handle */

    rh = CMIP_MallocRequest(cmiph->req_nr, type, callback);

    ht_entry = Tcl_CreateHashEntry (cmiph->req_table, rh->name, &flag);
    Tcl_SetHashValue (ht_entry, (ClientData) rh);

    if (callback) { /* asynchron */
	Tcl_SetResult (interp, rh->name, TCL_STATIC);
	status = TCL_OK;
    } else {
	/* synchron: do while there are outstanding responses */
	do {
	    status = CMIP_Wait(cmiph, interp, rh);
	    switch (status) {
	    case TCL_OK:
		Tcl_DStringResult(interp, &rh->dStrResult);
	    case TCL_ERROR:
	    case TCL_CONTINUE:
		break;
	    default:
		sprintf(buf, "\"%d\"", status);
		Tcl_AppendResult(interp,
				 "unexpected return-value from wait() ",
				 buf, (char*)NULL);
		return TCL_ERROR;
	    }
	} while (status == TCL_CONTINUE);

	Tcl_DeleteHashEntry(ht_entry);
	CMIP_FreeRequest(rh);
    }

    return status;
}


/*===========================================================================*/
/*
 * return an allocated request-handle structure
 */

CMIP_Request*
CMIP_MallocRequest(id, type, callback)
     int	 id;
     int	 type;
     char	*callback;
{
    char		buf[256];

    CMIP_Request *rh = (CMIP_Request *) ckalloc(sizeof(CMIP_Request));
    sprintf(buf, "req%d", id);
    rh->name	     = ckstrdup(buf);
    rh->request_id   = id;
    rh->type	     = type;
    rh->reqcallback  = callback ? ckstrdup(callback) : NULLCP;
    Tcl_DStringInit (&rh->dStrResult);

    return rh;
}


/*===========================================================================*/
/*
 * free a request-handle structure
 */

void
CMIP_FreeRequest(rh)
     CMIP_Request	*rh;
{
    Tcl_DStringFree(&rh->dStrResult);
    ckfree((char *) rh->name);
    if (rh->reqcallback) ckfree((char *) rh->reqcallback);
    ckfree((char *) rh);
}


/*===========================================================================*/
/*
 * return an allocated cmip-handle structure
 */

static CMIP_Handle*
MallocHandle(msd, agent, host)
     int msd;
     char *agent;
     char *host;
{
    char		 buf[256];
    CMIP_Handle		*cmiph;
    Tcl_HashEntry	*ht_entry;
    int			 flag;
    static unsigned lastcmipId = 0;	/* id number for the cmip handle */

    cmiph = (CMIP_Handle *) ckalloc(sizeof(CMIP_Handle));
    sprintf(buf, "cmip%d", lastcmipId++);
    cmiph->name		= ckstrdup(buf);
    cmiph->msd		= msd;
    cmiph->agent	= ckstrdup(agent);
    cmiph->host		= ckstrdup(host);
    cmiph->req_table	= (Tcl_HashTable *) ckalloc(sizeof(Tcl_HashTable));
    Tcl_InitHashTable(cmiph->req_table, TCL_STRING_KEYS);
    cmiph->req_nr	= 0;
    
    ht_entry = Tcl_CreateHashEntry(&cmipTable, cmiph->name, &flag);
    Tcl_SetHashValue(ht_entry, (ClientData) cmiph);

    return cmiph;
}


/*===========================================================================*/
/*
 * free a cmip-handle structure
 */

static void
FreeHandle(clientData)
     ClientData clientData;
{
    CMIP_Handle	*cmiph = (CMIP_Handle *) clientData;

    Tcl_HashEntry	*ht_entry;
    Tcl_HashSearch	 search;

    Tk_DeleteFileHandler(cmiph->msd); 

    ht_entry = Tcl_FindHashEntry (&cmipTable, cmiph->name);
    if (ht_entry) {
	Tcl_DeleteHashEntry (ht_entry);
    
	/*
	 * Delete structures of all outstanding request.
	 */
	
	for (ht_entry = Tcl_FirstHashEntry(cmiph->req_table, &search);
	     ht_entry != NULL;
	     ht_entry = Tcl_NextHashEntry(&search)) {
	    CMIP_FreeRequest((CMIP_Request *) Tcl_GetHashValue(ht_entry));
	}
    }

    ckfree(cmiph->name);
    ckfree(cmiph->agent);
    ckfree(cmiph->host);
    ckfree((char *) cmiph->req_table);
    ckfree((char *) cmiph);
}


/*===========================================================================*/
/*
 * Trigger responses on a msd (session descriptor), i.e. a file descriptor
 * for a management association to an agent at a host.
 * if the descriptor gets readable, i.e. there is an incomming response, call
 * the wait() routine, to catch the response and evaluate it.
 */

static void
ReceiveProc(clientdata, mask)
    ClientData	clientdata;
    int		mask;
{
    CMIP_Token	*token = (CMIP_Token *) clientdata;
    int code;

    code = CMIP_Wait(token->cmiph, token->interp, (CMIP_Request *) NULL);

    if (code == TCL_ERROR) {
	Tcl_AddErrorInfo (token->interp, "\n    (cmip event)");
	Tk_BackgroundError (token->interp);
    }
}


/*===========================================================================*/
/*
 * release the handle-command and depending structures
 */

static int
Terminate(cmiph, interp)
     CMIP_Handle *cmiph;
     Tcl_Interp *interp;
{
    MSAPIndication  mi;

    if (M_TerminateReq(cmiph->msd, ACF_NORMAL, NULLPE, &mi) != OK) {
	Tcl_AppendResult (interp, "terminate failed: ", mi.mi_abort.ma_data,
			  (char *) NULL);
	Tcl_DeleteCommand(interp, cmiph->name);
	return TCL_ERROR;
    }

    Tcl_DeleteCommand(interp, cmiph->name);
    return TCL_OK;
} /* Terminate() */


/*===========================================================================*/
/*
 * abort a cmip association
 */

static int
Abort(cmiph, interp)
     CMIP_Handle *cmiph;
     Tcl_Interp *interp;
{
    MSAPIndication  mi;

    if ((M_AbortReq (cmiph->msd, NULLEXTERN, &mi) != OK) && interp) {
	Tcl_AppendResult (interp, "abort failed: ", mi.mi_abort.ma_data,
			  (char *) NULL);
	Tcl_DeleteCommand(interp, cmiph->name);
	return TCL_ERROR;
    }

    Tcl_DeleteCommand(interp, cmiph->name);
    return TCL_OK;
} /* Abort() */


/*===========================================================================*/
/*
 * release the handle-command and depending structures
 */

static void
WaitRequests(name)
     char *name;
{
    /* wait until all outstanding requests are queued
     * and allow other events to take place -- we have 
     * to be careful because events can modify the hash 
     * table. That's why we start from the beginning if
     * we have done one event.
     */
    Tcl_HashEntry	*ht_entry;
    Tcl_HashSearch	 handle_search;

  repeat:
    if (name) {
	ht_entry = Tcl_FindHashEntry(&cmipTable, name);
    } else {
	ht_entry = Tcl_FirstHashEntry(&cmipTable, &handle_search);
    }
    while (ht_entry != NULL) {
	Tcl_HashSearch	 request_search;
	CMIP_Handle		*cmiph;
	
	cmiph = (CMIP_Handle *) Tcl_GetHashValue(ht_entry);
	
	/* wait for all the asynchronous requests to be queued and
	 * allow other events to take place
	 */
	
	if (Tcl_FirstHashEntry(cmiph->req_table,
			       &request_search) != NULL) {
	    Tk_DoOneEvent (0);
	    goto repeat;
	}

	/* we have synced all requests if there are no more request 
	 * handles and we search for a handle given by name.
	 */

	if (name) return;

	/* go to the next cmip handle as the current has no 
	 * outstanding requests
	 */

	ht_entry = Tcl_NextHashEntry(&handle_search);	
    }
}


/*===========================================================================*/

static int
HandleCmd(clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
    CMIP_Handle *cmiph = (CMIP_Handle *) clientData;

    if (argc < 2) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
                          " option ?arg arg ...?\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (strcmp(argv[1], "get") == 0) {
	return CMIP_Get(cmiph, interp, argc, argv);
    } else if (strcmp(argv[1], "set") == 0) {
	return CMIP_Set(cmiph, interp, argc, argv);
    } else if (strcmp(argv[1], "action") == 0) {
	return CMIP_Action(cmiph, interp, argc, argv);
    } else if (strcmp(argv[1], "create") == 0) {
	return CMIP_Create(cmiph, interp, argc, argv);
    } else if (strcmp(argv[1], "delete") == 0) {
	return CMIP_Delete(cmiph, interp, argc, argv);
    } else if (strcmp(argv[1], "cancelGet") == 0) {
	return CMIP_CancelGet(cmiph, interp, argc, argv);
    } else if (strcmp(argv[1], "eventSink") == 0) {
        return CMIP_EventReport(cmiph, interp, argc, argv);
    } else if (strcmp(argv[1], "release") == 0) {
	return Terminate(cmiph, interp);
    } else if (strcmp(argv[1], "abort") == 0) {
	return Abort(cmiph, interp);
    } else if (strcmp(argv[1], "wait") == 0) {
	char *name = ckstrdup(cmiph->name);
	WaitRequests (name);
	ckfree (name);
	return TCL_OK;
    } else if (strcmp(argv[1], "requests") == 0) {
	Tcl_HashEntry	*ht_entry;
	Tcl_HashSearch	 search;
	CMIP_Request	*rh;

	for (ht_entry = Tcl_FirstHashEntry(cmiph->req_table, &search);
	     ht_entry != NULL;
	     ht_entry = Tcl_NextHashEntry(&search)) {
	    rh = (CMIP_Request *) Tcl_GetHashValue(ht_entry);
	    Tcl_AppendElement(interp,
			      Tcl_GetHashKey(cmiph->req_table, ht_entry));
	}
	return TCL_OK;
    }

    Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be ",
		     "get, set, action, create, delete, cancelGet, ",
		     "eventSink, requests, wait, release, abort",
		     (char *) NULL);
    return TCL_ERROR;
} /* HandleCmd() */


/*===========================================================================*/
/*
 * Connect to a given agent running on host.
 */

static int
Connect(interp, argc, argv)
    Tcl_Interp  *interp;
    int          argc;
    char       **argv;
{
    CMIP_Handle *cmiph;
    CMIP_Token	*cmip_sess;

    MSAPConnect      mcs;
    MSAPIndication   mis;
    AEI		     aei;
    struct PSAPaddr *pa;

    if (argc != 4) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0], " ",
                          argv[1], " agent host\"", (char *) NULL);
        return TCL_ERROR;
    }

    if ((aei = str2aei(argv[3], argv[2])) == NULLAEI) {
	Tcl_AppendResult (interp, "unknown application entity title \"",
			  argv[3], "-", argv[2], "\"", (char *) NULL);
	return TCL_ERROR;
    }

    if ((pa = aei2addr(aei)) == NULLPA) {
	Tcl_SetResult (interp, "aei translation failed", TCL_STATIC);
	return TCL_ERROR;
    }

#ifdef OSIMIS_3
    if (M_InitialiseReq(NULLAEI,  NULLPA, aei, pa, "management",
		CMIP_VERSION2, 0xd, NULLEXTERN, NULLEXTERN, &mcs, &mis)
	    == NOTOK) {
#else
    if (M_InitialiseReq(NULLAEI,  NULLPA, aei, pa, "management",
		CMIP_VERSION2, 0x17, NULLEXTERN, NULLEXTERN, &mcs, &mis)
	    == NOTOK) {
#endif
	if (mis.mi_abort.ma_reason == ACS_TRANSIENT) {
	    Tcl_SetResult (interp, "remote SMA is busy, try later", 
			   TCL_STATIC);
	} else {
	    Tcl_SetResult (interp, mis.mi_abort.ma_data, TCL_VOLATILE);
	}
	mi_free(&mis);
	return TCL_ERROR;
    }
    mc_free(&mcs);

    cmiph = MallocHandle(mcs.mc_sd, argv[2], argv[3]);

    cmip_sess = (CMIP_Token *) ckalloc(sizeof(CMIP_Token));
    cmip_sess->interp = interp;
    cmip_sess->cmiph = cmiph;

    Tk_CreateFileHandler(cmiph->msd, TK_READABLE, ReceiveProc,
			 (ClientData) cmip_sess);

    Tcl_CreateCommand(interp, cmiph->name, HandleCmd,
		      (ClientData) cmiph, FreeHandle);

    Tcl_SetResult(interp, cmiph->name, TCL_STATIC);

    return TCL_OK;
} /* Connect() */


/*===========================================================================*/
/*
 * This is the cmip command as described in the scotty documentation.
 * It simply dispatches to the C functions implementing the options
 * understood by the cmip command.
 */

static int
CmipCmd(clientData, interp, argc, argv)
    ClientData   clientData;
    Tcl_Interp  *interp;
    int          argc;
    char       **argv;
{
    if (argc < 2) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
                          " option ?arg arg ...?\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (strcmp(argv[1], "connect") == 0) {
	return Connect(interp, argc, argv);
    } else if (strcmp(argv[1], "wait") == 0) {
	WaitRequests(NULL);
	return TCL_OK;
    } else if (strcmp(argv[1], "info") == 0) {
	/* list all associations, so the new commands */
	Tcl_HashEntry	*ht_entry;
	Tcl_HashSearch	search;
	CMIP_Handle	*cmiph;
	for (ht_entry = Tcl_FirstHashEntry(&cmipTable, &search);
	     ht_entry != NULL;
	     ht_entry = Tcl_NextHashEntry(&search)) {
	    cmiph = (CMIP_Handle *) Tcl_GetHashValue(ht_entry);
	    Tcl_AppendElement(interp, Tcl_GetHashKey(&cmipTable, ht_entry));
	}
	return TCL_OK;
    }

    Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be ",
		      "connect, wait, info",
		      (char *) NULL);
    return TCL_ERROR;
} /* CmipCmd() */


/*===========================================================================*/

int
Cmip_Init(interp)
    Tcl_Interp *interp;
{
    /* load syntaxes */

    if (initialiseSyntaxes(NULLCP, attrSyntaxes) == NOTOK) {
        Tcl_AppendResult(interp, "CMIP_Init (): cannot initialise syntaxes",
                          (char *) NULL);
	return TCL_ERROR;
    }

    Tcl_InitHashTable(&cmipTable, TCL_STRING_KEYS);

    Tcl_CreateCommand(interp, "cmip", CmipCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;
} /* CMIP_Init() */
