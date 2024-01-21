/*
 * mibTcl.c
 *
 * The Tcl interface to the MIB parser.
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
 * Forward declarations for procedures defined later in this file:
 */

static int	
MibCmd			_ANSI_ARGS_((ClientData	clientData, Tcl_Interp *interp,
				     int argc, char **argv));

static int
LoadFile		_ANSI_ARGS_((Tcl_Interp *interp, char *file));

static int
WalkTree		_ANSI_ARGS_((Tcl_Interp *interp, char *name, 
				     char *label, char *body, int exact,
				     MIB_Node* nodePtr, ASN1_OID *oid, 
				     int len));

/*
 * Initialize the mib command to the SNMP mib parser extension.
 */

int
MIB_Init (interp)
     Tcl_Interp *interp;
{
    Tcl_CreateCommand (interp, "mib", MibCmd, (ClientData *) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;
}

/*
 * MibCmd() implements the mib tcl command. See the documentation
 * for details about its operation.
 */

static int
MibCmd (clientData, interp, argc, argv)
     ClientData	clientData;
     Tcl_Interp	*interp;
     int	argc;
     char	**argv;
{
    int exact = 0;
    int len = 0;
    char *cmd, *name, *arg;
    char *result = NULL;

    if (argc > 1 && strcmp (argv[1], "-exact") == 0) {
	exact = 1;
	argc--;
	argv++;
    }

    cmd  = argc > 1 ? argv[1] : NULL;
    name = argc > 2 ? argv[2] : NULL;
    arg  = argc > 3 ? argv[3] : NULL;

    if (argc < 2) {
      wrongArgs:
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  "mib ?-exact? option args\"", (char *) NULL);
	return TCL_ERROR;
    }

    len = cmd ? strlen (cmd) : 0;

    if ((strncmp (cmd, "walk", len) == 0)) {
	ASN1_OID *oid = NULL;
	ASN1_OID _oid[OID_MAXLEN];
	int len, code;
	if (argc != 5) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      "mib ?-exact? walk varName label command\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}
	if (ASN1_IsOid (arg)) {

	    int i;
	    oid = ASN1_Str2Oid (arg, &len);

	    /*
	     * Make a private copy so that we do not run into problem
	     * if the buffer returned by ASN1_Str2Oid() is reused for
	     * other tasks.
	     */

	    for (i = 0; i < len; i++) {
		_oid[i] = oid[i];
	    }
	    oid = _oid;
	}
	code = WalkTree (interp, name, arg, argv[4], exact, NULL, oid, len);
	return (code == TCL_BREAK) ? TCL_OK : code;
    }

    if (strncmp (cmd, "format", len) == 0) {
	if (argc == 4) {
	    result = MIB_Format (name, exact, arg);
	} else {
	    Tcl_AppendResult (interp, "wrong # args: should be \"mib ",
			      "?-exact? format name value\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (! result) goto notFound;
        Tcl_SetResult (interp, result, TCL_VOLATILE);
        return TCL_OK;
    }

    if (strncmp (cmd, "scan", len) == 0) {
	if (argc == 4) {
	    result = MIB_Scan (name, exact, arg);
	} else {
	    Tcl_AppendResult (interp, "wrong # args: should be \"mib ",
			      "?-exact? scan name value\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (! result) goto notFound;
        Tcl_SetResult (interp, result, TCL_VOLATILE);
        return TCL_OK;
    }

    if (argc != 3) {
	goto wrongArgs;
    }
    
    if (strncmp (cmd, "load", len) == 0) {
	return LoadFile (interp, name);

    } else if (strncmp (cmd, "oid", len) == 0) {
	result = MIB_Oid (name, exact);

    } else if (strncmp (cmd, "name", len) == 0) {
	result = MIB_Name (name, exact);

    } else if (strncmp (cmd, "syntax", len) == 0) {
	result = MIB_Syntax (name, exact);

    } else if (strncmp (cmd, "description", len) == 0) {
	result = MIB_Description (name, exact);

    } else if (strncmp (cmd, "access", len) == 0) {
	result = MIB_Access (name, exact);

    } else if (strncmp (cmd, "successor", len) == 0) {
	result = MIB_Succ (name);

    } else if (strncmp (cmd, "tc", len) == 0) {
	result = MIB_TC (name, exact);

    } else if (strncmp (cmd, "file", len) == 0) {
	result = MIB_File (name, exact);

    } else if (strncmp (cmd, "index", len) == 0) {
	result = MIB_Index (name, exact);

    } else if (strncmp (cmd, "parent", len) == 0) {
	result = MIB_Parent (name, exact);

    } else if (strncmp (cmd, "defval", len) == 0) {
	result = MIB_DefVal (name, exact);

    } else {

	Tcl_AppendResult (interp, "bad option \"", cmd,
			  "\": should be load, name, oid, syntax, descr, ",
			  "access, index, succ, format, scan, tc, or walk", 
			  (char *) NULL);
	return TCL_ERROR;
    }

    if (! result) goto notFound;

    Tcl_SetResult (interp, result, TCL_VOLATILE);
    return TCL_OK;

  notFound:
    Tcl_AppendResult (interp, "no object \"", name, "\"",
		      (char *) NULL);
    return TCL_ERROR;
}

/*
 * MIB_Load() reads another MIB definition and adds the objects to the
 * internal MIB tree. This function expands ~ filenames and searches
 * in $scotty_library/site and $scotty_library/mibs for the given filename.
 * The compressed image of MIB definitions is written in a machine specifc
 * directory. This allows to share the scotty library directory across
 * multiple platforms.
 */

static int
LoadFile (interp, file)
     Tcl_Interp	*interp;
     char *file;
{
    Tcl_DString dst;
    char *library, *arch, *fileName, *frozenName, *tail;
    int code = TCL_OK;

    library = Tcl_GetVar(interp, "scotty_library", TCL_GLOBAL_ONLY);
    if (! library) {
        library = "/usr/local/lib/scotty";
    }

    arch = Tcl_GetVar(interp, "scotty_arch", TCL_GLOBAL_ONLY);
    if (! arch) {
        library = "unknown-os";
    }

    tail = strrchr(file, '/');
    if (tail != NULL) {
        tail = tail+1;
    } else {
        tail = file;
    }

    Tcl_DStringInit(&dst);
    Tcl_DStringAppend(&dst, library, -1);
    Tcl_DStringAppend(&dst, "/", 1);
    Tcl_DStringAppend(&dst, arch, -1);
    frozenName = Tcl_DStringValue(&dst);
    if (access(frozenName, F_OK) != 0) {
        if (mkdir(frozenName, 0777) != 0) {
	    frozenName = NULL;
	}
    }
    if (frozenName) {
        Tcl_DStringAppend(&dst, "/", 1);
	Tcl_DStringAppend(&dst, tail, -1);
	Tcl_DStringAppend(&dst, ".idy", 4);
	frozenName = ckstrdup(Tcl_DStringValue(&dst));
    }

    if ((fileName = Tcl_TildeSubst(interp, file, &dst)) == NULL) {
	code = TCL_ERROR;
	goto exit;
    }

    if (access(fileName, R_OK) != 0) {

	if (library) {
	    Tcl_DStringFree(&dst);
	    Tcl_DStringAppend(&dst, library, -1);
	    Tcl_DStringAppend(&dst, "/site/", 6);
	    Tcl_DStringAppend(&dst, file, -1);
	    fileName = Tcl_DStringValue(&dst);
	}

	if (library && (access(fileName, R_OK) != 0)) {
	    Tcl_DStringFree(&dst);
            Tcl_DStringAppend(&dst, library, -1);
            Tcl_DStringAppend(&dst, "/mibs/", 6);
            Tcl_DStringAppend(&dst, file, -1);
            fileName = Tcl_DStringValue(&dst);
	}
	    
	if (library && (access(fileName, R_OK) != 0)) {
	    Tcl_AppendResult(interp, "couldn't open MIB file \"", file,
			     "\": ", Tcl_PosixError(interp), (char *) NULL);
	    code = TCL_ERROR;
	    goto exit;
	}
    }

    mib_Tree = MIB_Parse(fileName, frozenName, mib_Tree);
    if (mib_Tree == NULL) {
	Tcl_AppendResult(interp, "loading MIB file ", fileName," failed", 
			 (char *) NULL);
	code = TCL_ERROR;
	goto exit;
    }

  exit:
    Tcl_DStringFree(&dst);
    if (frozenName) {
	ckfree(frozenName);
    }
    return code;
}

/*
 * WalkTree() implements a recursive MIB walk and may be used to write
 * fast MIB browsers. The varName argument defines the Tcl variable
 * used to identify the current node and label identifies the base
 * node. The current position in the MIB tree is given by nodePtr.
 *
 * The oid and len arguments are optional and only used when the
 * label is an object identifier and not a name. In this case, we
 * assemble the current path in the tree in the buffer pointed to
 * by oid. Note, that the caller of MIBWalk() must ensure that the
 * buffer is large enough and not shared by others.
 */

static int
WalkTree (interp, varName, label, body, exact, nodePtr, oid, len)
     Tcl_Interp *interp;
     char *varName;
     char *label;
     char *body;
     int exact;
     MIB_Node* nodePtr;
     ASN1_OID *oid;
     int len;
{
    int doall = 1;
    int result = TCL_OK;

    if (! nodePtr) {
	nodePtr = MIB_FindNode (label, NULL, exact);
	if (!nodePtr) {
	    Tcl_AppendResult (interp, "no object \"", label, "\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}
	doall = 0;
    }

    while (nodePtr) {

	char *val, *label;

	if (doall && oid) {
	    oid[len-1] = nodePtr->subid;
	}

	label = nodePtr->label;

	if (oid) {
	    label = ASN1_Oid2Str (oid, len);
	}
	
	val = Tcl_SetVar (interp, varName, label, TCL_LEAVE_ERR_MSG);	
	if (! val) {
	    result = TCL_ERROR;
	    goto loopDone;
	}
	
	result = Tcl_Eval (interp, body);

        if ((result == TCL_OK || result == TCL_CONTINUE) 
	    && nodePtr->childPtr) {
	    result = WalkTree (interp, varName, label, body, exact, 
			       nodePtr->childPtr, oid, len+1);
	}

	if (result != TCL_OK) {
	    if (result == TCL_CONTINUE) {
		result = TCL_OK;
	    } else if (result == TCL_BREAK) {
		goto loopDone;
	    } else if (result == TCL_ERROR) {
		char msg[100];
		sprintf (msg, "\n    (\"mib walk\" body line %d)",
			 interp->errorLine);
		Tcl_AddErrorInfo (interp, msg);
		goto loopDone;
	    } else {
		goto loopDone;
	    }
	}

	nodePtr = doall ? nodePtr->nextPtr : NULL;
    }

  loopDone:
    if (result == TCL_OK) {
	Tcl_ResetResult(interp);
    }
    return result;
}

