/*
 * rpc.c
 *
 * Implementation of commands that implement a simple RPC protocol
 * on top of a (reliable) tcp file handle.
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
 */

#include "scotty.h"

/*
 * Data structure used to describe a RPC handle.
 */

typedef struct RpcHandle {
    char *fileId;               /* the TCL file id of the connection */
    char *rpcId;                /* the rpc handle */
    int  pargc;                 /* number of exported proc names */
    char **pargv;               /* the array of exported proc names */
    struct RpcHandle *server;   /* a pointer to the server handle. it is
			           used by handles representing connection
				   to a server */
} RpcHandle;

/* 
 * The hash table used to map rpc handle ids to RpcHandle pointer. 
 */

static Tcl_HashTable rpcTable;
static int initialized = 0;

/* 
 * The types of the RPC messages send between clients and server. 
 */

#define RPC_NONE  0
#define RPC_CALL  1
#define RPC_REPLY 2
#define RPC_EVAL  3
#define RPC_ERROR 4

/*
 * The clientData used in the callback functions.
 */

typedef struct RpcToken {
    char *rpcId;
    Tcl_Interp *interp;
} RpcToken;

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
xread			_ANSI_ARGS_((int fd, char *buf, int len));

static int
xwrite			_ANSI_ARGS_((int fd, char *buf, int len));

static RpcHandle*
CreateRpcHandle		_ANSI_ARGS_((char *name));

static void 
DeleteRpcHandle		_ANSI_ARGS_((ClientData clientData));

static int
ReadMessage		_ANSI_ARGS_((char **msg, FILE *file, int *type));

static int
WriteMessage		_ANSI_ARGS_((char *msg, FILE *file, int type));

static int
RpcClient		_ANSI_ARGS_((ClientData clientData, 
				     Tcl_Interp *interp, 
				     int argc, char **argv));
static int
RpcServer		_ANSI_ARGS_((ClientData clientData, 
				     Tcl_Interp *interp, 
				     int argc, char **argv));
static int
RpcDelete		_ANSI_ARGS_((ClientData clientData, 
				     Tcl_Interp *interp, 
				     int argc, char **argv));
static int 
RpcMakeCall		_ANSI_ARGS_((ClientData clientData, 
				     Tcl_Interp *interp,
				     int argc, char **argv));
static int
RpcRegister		_ANSI_ARGS_((ClientData clientData, 
				     Tcl_Interp *interp, 
				     int argc, char **argv));
static int
RpcUnregister		_ANSI_ARGS_((ClientData clientData,
				     Tcl_Interp *interp, 
				     int argc, char **argv));
static int
RpcInfo			_ANSI_ARGS_((ClientData clientData,
				     Tcl_Interp *interp, 
				     int argc, char **argv));
static void
RpcAcceptProc		_ANSI_ARGS_((ClientData clientData, int mask));

static void
RpcProcessProc		_ANSI_ARGS_((ClientData clientData, int mask));

/*
 * This wrapper for write() is needed to fix broken SYS V semantics.
 */

static int
xwrite (fd, buf, len)
     int fd;
     char *buf;
     int len;
{
    int rc;

    do {
	while ((rc = write (fd, buf, len)) < 0
	       && (errno == EINTR || errno == EAGAIN))
		continue;
	len -= rc;
	buf += rc;
    } while ((len > 0) && (rc > 0));

    return rc;
}

/*
 * This wrapper for read() is needed to fix broken SYS V semantics.
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
 * Allocate a new rpc handle. Rpc client handles may be used to
 * make rpc calls and rpc server handles are used to register
 * commands for a server port. The new handle is registered in
 * rpcTable hash table.
 */

static RpcHandle*
CreateRpcHandle (file)
     char *file;
{
    char buffer[20];
    static unsigned lastid = 0;
    RpcHandle *rh;
    Tcl_HashEntry *entryPtr;
    int flag;

    sprintf(buffer, "rpc%d", lastid++);

    rh = (RpcHandle *) ckalloc (sizeof (RpcHandle));

    rh->fileId = ckstrdup (file);
    rh->rpcId  = ckstrdup (buffer);
    rh->pargc = 0;
    rh->pargv = (char **) (ckalloc(1));
    rh->server = (RpcHandle *) NULL;

    entryPtr = Tcl_CreateHashEntry (&rpcTable, rh->rpcId, &flag);
    Tcl_SetHashValue (entryPtr, (ClientData) rh);
    
    return rh;
}

/*
 * Delete an rpc handle object. Free everything allocated before 
 * destroying the structure.
 */

static void 
DeleteRpcHandle (clientData)
     ClientData clientData;
{
    RpcHandle *rh = (RpcHandle *) clientData;
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch search;
    RpcHandle *crh;

    /*
     * scan through all rpc handles that point to 
     * this server rpc handle and set the link to NULL
     */

    entryPtr = Tcl_FirstHashEntry (&rpcTable, &search);
    while (entryPtr != NULL) {
	crh = (RpcHandle *) Tcl_GetHashValue (entryPtr);
	if (crh->server == rh) {
	    crh->server = (RpcHandle *) NULL;
	}
	entryPtr = Tcl_NextHashEntry (&search);
    }

    ckfree (rh->fileId);
    ckfree (rh->rpcId);

    ckfree ((char *) rh->pargv);
    
    ckfree ((char *) rh);
}

/*
 * Read a message from the stream given by file and return
 * the type and the message.
 */

static int
ReadMessage (msg, file, type)
     char **msg;
     FILE *file;
     int *type;
{
    Tcl_DString dst;
    char buffer[256];
    int len;
    char *p;
    char *b;

    Tcl_DStringInit (&dst);
    do {
	if ((len = xread (fileno (file), buffer, 256)) < 0) {
	    Tcl_DStringFree (&dst);
	    return TCL_ERROR;
	}
	Tcl_DStringAppend (&dst, buffer, len);
    } while (! Tcl_CommandComplete (Tcl_DStringValue (&dst)));
	
    /* 
     * Eliminate the leading and the trailing braces, extract the 
     * type information (encoded as the first number) and allocate 
     * a copy of the message.
     */

    p = Tcl_DStringValue (&dst);
    if (*p != '{') {
	Tcl_DStringFree (&dst);
	return TCL_ERROR;
    }

    p++; while (isspace(*p)) p++;        /* remove leading white space */

    b = p; while (isdigit(*p)) p++;      /* scan over the type field */

    *p = '\0'; *type = atoi(b);          /* convert to an integer value */

    p++;
    *msg = ckstrdup (p);
    (*msg)[strlen(*msg)-1] = '\0';

    Tcl_DStringFree (&dst);

    return TCL_OK;
}

/*
 * Write a message on the stream given by file.
 */

static int
WriteMessage (msg, file, type)
     char *msg;
     FILE *file;
     int type;
{
    char buffer[80];
    Tcl_DString dst;
    int rc, fd = fileno (file);

    sprintf (buffer, "%d", type);

    Tcl_DStringInit (&dst);
    Tcl_DStringStartSublist (&dst);
    Tcl_DStringAppendElement (&dst, buffer);
    Tcl_DStringAppend (&dst, " ", 1);
    Tcl_DStringAppend (&dst, msg, -1);
    Tcl_DStringEndSublist (&dst);

    rc = xwrite (fd, Tcl_DStringValue (&dst), Tcl_DStringLength (&dst));

    Tcl_DStringFree (&dst);

    if (rc < 0) return TCL_ERROR;

    return TCL_OK;
}

/*
 * Create a client rpc handle. The parameter defines the tcl file to use.
 */

static int
RpcClient (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    char *fileId;
    RpcHandle *rh;
    FILE *filePtr;

    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", 
			  argv[0], " client host port\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (Tcl_VarEval (interp, "tcp connect ", argv[2], " ", argv[3], 
		     (char *) NULL) != TCL_OK) {
	return TCL_ERROR;
    }
    fileId = ckstrdup (interp->result);

    Tcl_ResetResult (interp);

    if (Tcl_GetOpenFile(interp, fileId, 1, 1, &filePtr) != TCL_OK) {
	ckfree (fileId);
 	return TCL_ERROR;
    }

    /* 
     * Create a handle and a new tcl command for the new object.
     */

    rh = CreateRpcHandle (fileId);

    Tcl_CreateCommand (interp, rh->rpcId, RpcMakeCall, 
		       (ClientData) rh, DeleteRpcHandle);

    Tcl_SetResult (interp, rh->rpcId, TCL_STATIC);

    ckfree (fileId);

    return TCL_OK;
}

/*
 * Create a new rpc server. Return a rpc server handle that can
 * be used to register or unregister commands for this service.
 */

static int
RpcServer (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    RpcHandle *rh;
    char *fileId;
    int res;

    if (argc != 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
                          argv[0], " server port\"", (char *) NULL);
        return TCL_ERROR;
    }

    res = Tcl_VarEval (interp, "tcp listen ", argv[2], (char *) NULL);
    if (res != TCL_OK) 	return res;

    fileId = ckstrdup(interp->result);

    Tcl_ResetResult (interp);

    rh = CreateRpcHandle (fileId);

    {
	RpcToken *rcd;
	FILE *filePtr;

	if (Tcl_GetOpenFile (interp, rh->fileId, 1, 1, &filePtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	rcd = (RpcToken *) ckalloc (sizeof (RpcToken));
	rcd->rpcId = ckstrdup (rh->rpcId);
	rcd->interp = interp;

	Tk_CreateFileHandler (fileno(filePtr), TK_READABLE, 
			      RpcAcceptProc, (ClientData) rcd);
    }

    Tcl_SetResult (interp, rh->rpcId, TCL_STATIC);

    ckfree (fileId);

    return TCL_OK;
}

/*
 * Delete an existing rpc handle. No further parameters are expected.
 */

static int
RpcDelete (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashEntry *entryPtr;
    Tcl_CmdInfo info;
    RpcHandle *rh;
    FILE *filePtr;

    if (argc != 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", 
			  argv[0], " delete handle\"", (char *) NULL);
        return TCL_ERROR;
    }

    entryPtr = Tcl_FindHashEntry(&rpcTable, argv[2]);
    if (entryPtr == NULL) {
	Tcl_AppendResult (interp, "no rpc handle \"", argv[2], "\"",
			  (char *) NULL);
	return TCL_ERROR;
    }
    
    rh = (RpcHandle *) Tcl_GetHashValue (entryPtr);

    Tcl_VarEval (interp, "tcp close ", rh->fileId, (char *) NULL);
    if (Tcl_GetOpenFile (interp, rh->fileId, 1, 1, &filePtr) == TCL_OK) {
	Tk_DeleteFileHandler (fileno (filePtr));
    }
    Tcl_ResetResult (interp);

    if (Tcl_GetCommandInfo (interp, argv[2], &info) == 0) {
	DeleteRpcHandle ((RpcHandle *) (Tcl_GetHashValue (entryPtr)));
    } else {
	Tcl_DeleteCommand (interp, argv[2]);
    }

    Tcl_DeleteHashEntry (entryPtr);

    return TCL_OK;
}

/*
 * Make an rpc call to the server. The parameters are send
 * to a server proc given by the first mandatory argument.
 *
 * NOTE: Don't change the name to rpc_call! At least solaris
 * uses this name in its C library.
 */

static int 
RpcMakeCall (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    RpcHandle *rh = (RpcHandle *) clientData;
    char *msg;
    FILE *filePtr;
    int type;

    if (argc < 2) {	
	Tcl_AppendResult (interp, "wrong # args: should be \"", 
			  argv[0], " ?-async? cmd ?args?\"", 
			  (char *) NULL);
        return TCL_ERROR;
    }

    if (strcmp (argv[1], "-async") == 0) {
	type = RPC_EVAL;
	if (argc < 3) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      argv[0], " ?-async? cmd ?args?\"", 
			      (char *) NULL);
	    return TCL_ERROR;
	}
	argc--;
	argv++;
    } else {
	type = RPC_CALL;
    }

    if (Tcl_GetOpenFile(interp, rh->fileId, 1, 1, &filePtr) != TCL_OK) {
 	return TCL_ERROR;
    }

    /*
     * Write the command to the stream.
     */

    msg = Tcl_Concat (argc-1, argv+1);
    if (WriteMessage (msg, filePtr, type) != TCL_OK) {
	Tcl_ResetResult (interp);
	Tcl_AppendResult (interp, "can not send RPC message to \"",
			  rh->fileId, "\": ",Tcl_PosixError (interp),
			  (char *) NULL);
        return TCL_ERROR;
    }
    ckfree (msg);

    /*
     * Fetch the answer from the stream.
     */

    do {
	if (ReadMessage (&msg, filePtr, &type) != TCL_OK) {
	    Tcl_SetResult (interp, "lost connection to rpc server", 
			   TCL_STATIC);
	    return TCL_ERROR;
	}
    } while ((type != RPC_REPLY) && (type != RPC_ERROR));

    Tcl_SetResult (interp, msg, TCL_DYNAMIC);

    return (type == RPC_REPLY) ? TCL_OK : TCL_ERROR;
}

/*
 * Export a proc on a rpc handle. The parameter defines the
 * proc to export.
 */

static int
RpcRegister (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashEntry *entryPtr;
    RpcHandle *rh;
    char *list;
    Tcl_DString dstr;

    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " register handle proc\"", (char *) NULL);
        return TCL_ERROR;
    }

    entryPtr = Tcl_FindHashEntry(&rpcTable, argv[2]);
    if (entryPtr == NULL) {
	Tcl_AppendResult (interp, "no rpc handle \"", argv[2], "\"",
			  (char *) NULL);
	return TCL_ERROR;
    }
    rh = (RpcHandle *) Tcl_GetHashValue(entryPtr);

    Tcl_DStringInit (&dstr);
    list = Tcl_Merge (rh->pargc, rh->pargv);
    Tcl_DStringAppend (&dstr, list, -1);
    Tcl_DStringAppendElement (&dstr, argv[3]);
    ckfree (list);
    ckfree ((char *) rh->pargv);
    Tcl_SplitList (interp, Tcl_DStringValue (&dstr), 
		   &(rh->pargc), &(rh->pargv));
    Tcl_DStringFree (&dstr);

    return TCL_OK;
}

/*
 * Remove an exported proc from a rpc handle. The parameter 
 * defines the proc to remove.
 */

static int
RpcUnregister (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashEntry *entryPtr;
    RpcHandle *rh;
    int i;
    Tcl_DString dstr;

    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " unregister handle proc\"", (char *) NULL);
        return TCL_ERROR;
    }

    entryPtr = Tcl_FindHashEntry(&rpcTable, argv[2]);
    if (entryPtr == NULL) {
	Tcl_AppendResult (interp, "no rpc handle \"", argv[2], "\"",
			  (char *) NULL);
	return TCL_ERROR;
    }
    rh = (RpcHandle *) Tcl_GetHashValue(entryPtr);

    Tcl_DStringInit (&dstr);
    for (i = 0; i < rh->pargc; i++) {
	if (strcmp(argv[3], rh->pargv[i]) != 0) {
	    Tcl_DStringAppendElement (&dstr, rh->pargv[i]);
	}
    }
    ckfree ((char *) rh->pargv);
    Tcl_SplitList (interp, Tcl_DStringValue (&dstr),
                   &(rh->pargc), &(rh->pargv));
    Tcl_DStringFree (&dstr);

    return TCL_OK;
}

/*
 * Return some information about existing rpc handles.
 */

static int
RpcInfo (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch search;

    RpcHandle *rh;

    if (argc < 2 || argc > 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
                          " info ?handle?\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (argc == 3) {

	char *regcmds;
	Tcl_DString dst;
	RpcHandle *arh;

	entryPtr = Tcl_FindHashEntry(&rpcTable, argv[2]);
	if (entryPtr == NULL) {
	    Tcl_AppendResult (interp, "no rpc handle \"", argv[2], "\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}

	rh = (RpcHandle *) Tcl_GetHashValue(entryPtr);

	if (rh->server) {
	    Tcl_AppendResult (interp, "no rpc handle \"", argv[2], "\"",
                              (char *) NULL);
            return TCL_ERROR;
	}

	Tcl_DStringInit (&dst);
	Tcl_DStringAppendElement (&dst, rh->fileId);

	regcmds = Tcl_Merge (rh->pargc, rh->pargv);
	Tcl_DStringAppendElement (&dst, regcmds);
	ckfree (regcmds);

	Tcl_DStringStartSublist (&dst);
	entryPtr = Tcl_FirstHashEntry(&rpcTable, &search);
        while (entryPtr != NULL) {
            arh = (RpcHandle *) Tcl_GetHashValue(entryPtr);
            if (arh->server == rh) {
		Tcl_DStringAppendElement (&dst, arh->fileId);
	    }
            entryPtr = Tcl_NextHashEntry (&search);
        }
	Tcl_DStringEndSublist (&dst);

	Tcl_DStringResult (interp, &dst);
	
    } else {

	/*
	 * scan through all rpc handles that point to 
	 * this server rpc handle
	 */
	
	entryPtr = Tcl_FirstHashEntry(&rpcTable, &search);
	while (entryPtr != NULL) {
	    rh = (RpcHandle *) Tcl_GetHashValue(entryPtr);
	    if (! rh->server) Tcl_AppendElement (interp, rh->rpcId);
	    entryPtr = Tcl_NextHashEntry (&search);
	}
    }

    return TCL_OK;
}

/*
 * This command procedure is called whenever a new connection on
 * a rpc file handle is accepted.
 */

static void
RpcAcceptProc (clientData, mask)
     ClientData clientData;
     int mask;
{
    RpcToken *rcd = (RpcToken *) clientData;
    Tcl_Interp *interp = rcd->interp;
    FILE *filePtr;
    Tcl_HashEntry *entryPtr;
    RpcHandle *s_rh;
    RpcHandle *c_rh;

    /*
     * Get the rpc handle if it is vaild.
     */

    entryPtr = Tcl_FindHashEntry (&rpcTable, rcd->rpcId);
    if (entryPtr == NULL) return;

    s_rh = (RpcHandle *) Tcl_GetHashValue (entryPtr);

    Tcl_ResetResult (interp);
    if (Tcl_VarEval(interp, "tcp accept ", s_rh->fileId, 
		    (char *) NULL) != TCL_OK) {
	return;
    }
    
    c_rh = CreateRpcHandle (interp->result);
    c_rh->server = s_rh;

    {
	RpcToken *rcd;

	if (Tcl_GetOpenFile (interp, c_rh->fileId, 1, 1, &filePtr) != TCL_OK) {
	    return;
	}
	rcd = (RpcToken *) ckalloc (sizeof (RpcToken));
	rcd->rpcId = ckstrdup (c_rh->rpcId);
	rcd->interp = interp;

	Tk_CreateFileHandler (fileno(filePtr), TK_READABLE, 
			      RpcProcessProc, (ClientData) rcd);
    }
}

/*
 * This command procedure is called whenever a rpc request can be 
 * read from the underlying transport connection. The call will be 
 * evaluated and send a return msg depending of the message type.
 */

static void
RpcProcessProc (clientData, mask)
     ClientData clientData;
     int mask;
{
    RpcToken *rcd = (RpcToken *) clientData;
    Tcl_Interp *interp = rcd->interp;
    Tcl_HashEntry *entryPtr;
    RpcHandle *rh;
    FILE *filePtr;
    int type;
    char *cmd, *msg = NULL;
    int i, argc, result;
    char **argv = NULL;
    RpcHandle *server;
    Tcl_CmdInfo cmdInfo;

    /*
     * Get the rpc handle if it is vaild.
     */

    entryPtr = Tcl_FindHashEntry (&rpcTable, rcd->rpcId);
    if (entryPtr == NULL) return;

    rh = (RpcHandle *) Tcl_GetHashValue (entryPtr);

    /*
     * Check if we can get the open file structure. Make sure to reset
     * the result in the interpreter as there might be some old results
     * pendings.
     */

    Tcl_ResetResult (interp);
    if (Tcl_GetOpenFile (interp, rh->fileId, 1, 1, &filePtr) != TCL_OK) {
	return;
    }

    /*
     * Read in the call message.
     */

    do {
	if (ReadMessage (&msg, filePtr, &type) != TCL_OK) {
	    goto error;
	}
    } while ((type != RPC_CALL) && (type != RPC_EVAL));

    /*
     * Check if the command is valid for the rpc handle.
     */

    if (Tcl_SplitList (interp, msg, &argc, &argv) != TCL_OK) {
	if (WriteMessage ("illegal rpc call", filePtr, RPC_ERROR) != TCL_OK) {
	    goto error;
	}
	ckfree (msg);
	return;
    }

    cmd = NULL;
    server = rh->server;
    if (server != (RpcHandle *) NULL) {
	for (i = 0; i < server->pargc; i++) {
	    if (strcmp (argv[0], server->pargv[i]) == 0) {
		cmd = argv[0];
		break;
	    }
	}
    }

    if (!cmd || !Tcl_GetCommandInfo(interp, cmd, &cmdInfo)) {
	if (WriteMessage ("illegal rpc call", filePtr, RPC_ERROR) != TCL_OK) {
	    goto error;
	}
	ckfree (msg);
	ckfree ((char *) argv);
	return;
    }

    /*
     * Process the call either synchronous or asynchronous. Do not call
     * Tcl_Eval() because of possible security holes with $ substitution 
     * and bracketed command evaluation.
     */

    if (type == RPC_EVAL) {
	if (WriteMessage ("", filePtr, RPC_REPLY) != TCL_OK) {
	    goto error;
	}
    }

    result = (cmdInfo.proc)(cmdInfo.clientData, interp, argc, argv);

    if (type == RPC_CALL) {
	int msgtype = (result == TCL_OK) ? RPC_REPLY : RPC_ERROR;
	if (WriteMessage (interp->result, filePtr, msgtype) != TCL_OK) {
	    goto error;
	}
    }

    ckfree (msg);
    ckfree ((char *) argv);
    return;

 error:
    if (msg  != NULL) ckfree (msg);
    if (argv != NULL) ckfree ((char *) argv);
    if (Tcl_GetOpenFile (interp, rh->fileId, 1, 1, &filePtr) == TCL_OK) {
	Tk_DeleteFileHandler (fileno (filePtr));
    }
    Tcl_ResetResult (interp);
    Tcl_AppendResult (interp, "can not send RPC message to \"",
		      rh->fileId, "\": ", Tcl_PosixError (interp),
		      (char *) NULL);
    return;
}

/*
 * This is the rpc command as described in the scotty documentation.
 * It simply dispatches to the C functions implementing the options
 * understood by the rpc command.
 */

int
Scotty_RpcCmd (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    int len;

    if (argc < 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (! initialized) {
	Tcl_InitHashTable (&rpcTable, TCL_STRING_KEYS);
	initialized = 1;
    }

    len = strlen (argv[1]);

    if (strncmp(argv[1], "client", len) == 0) {
	return RpcClient (clientData, interp, argc, argv);
    } else if (strncmp(argv[1], "server", len) == 0) {
        return RpcServer (clientData, interp, argc, argv);
    } else if (strncmp(argv[1], "delete", len) == 0) {
        return RpcDelete (clientData, interp, argc, argv);
    } else if (strncmp(argv[1], "register", len) == 0) {
        return RpcRegister (clientData, interp, argc, argv);
    } else if (strncmp(argv[1], "unregister", len) == 0) {
        return RpcUnregister (clientData, interp, argc, argv);
    } else if (strncmp(argv[1], "info", len) == 0) {
        return RpcInfo (clientData, interp, argc, argv); 
    }

    Tcl_AppendResult (interp, "bad option \"", argv[1], "\": should be ",
		      "client, server, register, unregister, delete, or info",
		      (char *) NULL);
    return TCL_ERROR;
}
