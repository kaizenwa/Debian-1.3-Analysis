/*
 * cmipEvent.c 
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

#include "cmip.h"

int
CMIP_EventReport(cmiph, interp, argc, argv)
    CMIP_Handle  *cmiph;
    Tcl_Interp	 *interp;
    int		  argc;
    char	**argv;
{
    char         *callback    = NULLCP;

    CMIP_Request *rh;

    Tcl_HashEntry *ht_entry;
    int            flag = 0;

    int		status;

    char	buf[256];

    if (argc < 2 || argc == 3 || argc > 4) {
        goto usage;
    } else if (argc == 4) {
	if (strcmp(argv[2], "-callback") == 0) {
	    /* the optional parameter */

	    callback = argv[3];
	} else {
	    goto usage;
	}
    }


    /* first look, if CMIP_Request already exists, if so delete it */

    ht_entry = Tcl_FindHashEntry(cmiph->req_table, "eventreport");
    if (ht_entry != NULL) {
	rh = (CMIP_Request *) Tcl_GetHashValue(ht_entry);
	CMIP_FreeRequest(rh);
	Tcl_DeleteHashEntry(ht_entry);
    }

    /* compose the request handle */

    rh = (CMIP_Request *) ckalloc(sizeof(CMIP_Request));
    rh->name	     = ckstrdup("eventreport");
    rh->request_id   = 0;
    rh->type	     = M_EVENT_REP;
    rh->reqcallback  = callback ? ckstrdup(callback) : NULLCP;
    Tcl_DStringInit (&rh->dStrResult);

    /* it is important that the request is registered under
     * `eventreport' see CMIP_Wait()
     */
    ht_entry = Tcl_CreateHashEntry(cmiph->req_table, rh->name, &flag);
    if (flag != 1) {
        /* shouldn't really happen */
	Tcl_AppendResult(interp, "*** request handle already exist!? ***",
			 (char *)NULL);
        return TCL_ERROR;
    }
    Tcl_SetHashValue(ht_entry, (ClientData) rh);

    if (callback) { /* asynchron */
	status = TCL_OK;
    } else {
	/* an dieser stelle vielleicht einige zeit warten */
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
		Tcl_AppendResult(interp, "CMIP_EventReport():", 
				 " unexpected returnvalue from wait() ",
				 buf, (char*)NULL);
		return TCL_ERROR;
	    }
	} while (status == TCL_CONTINUE);

	Tcl_DeleteHashEntry(ht_entry);
	CMIP_FreeRequest(rh);
    }
    return status;

 usage:
    Tcl_AppendResult(interp, "wrong # args: should be \"",
		     argv[0], " ", argv[1],
		     " ?-callback callback?\"",
		     (char *)NULL);
    return TCL_ERROR;
} /* CMIP_EventReport */
