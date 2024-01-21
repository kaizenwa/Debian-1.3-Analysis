/*
 * cmipCancel.c 
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
CMIP_CancelGet(cmiph, interp, argc, argv)
     CMIP_Handle *cmiph;
     Tcl_Interp	 *interp;
     int	  argc;
     char	**argv;
{
    char	 *reqHdl	= NULLCP;
    char	 *callback	= NULLCP;

    MSAPIndication	mi;

    int		  status;

    CMIP_Request *rh;

    Tcl_HashEntry *ht_entry;

    /* parse command line arguments */

    if (argc < 3 || argc == 4 || argc > 5) {
	goto usage;
    } else if (argc == 3) {
	reqHdl = argv[2];
    } else if (argc == 5) {
	if (strcmp(argv[3], "-callback") == 0) {
	    /* the optional parameter */

	    reqHdl = argv[2];
	    callback = argv[4];
	} else {
	    goto usage;
	}
    }

    ht_entry = Tcl_FindHashEntry(cmiph->req_table, reqHdl);
    if (ht_entry == NULL) {
        Tcl_AppendResult(interp, "no such request handle \"", reqHdl, "\"",
                         (char *) NULL);
        return TCL_ERROR;
    }
    rh = (CMIP_Request *) Tcl_GetHashValue(ht_entry);
    if (rh->type != M_GET) {
	Tcl_AppendResult(interp, "no get request", (char*) NULL);
	return TCL_ERROR;
    }

    /* perform the M-CANCELGET request */
    (cmiph->req_nr)++;
    status = M_CancelGet(cmiph->msd, cmiph->req_nr, rh->request_id, &mi);
    if (status == NOTOK) {
        Tcl_AppendResult(interp, argv[0], " ", argv[1], ": ",
			  mi.mi_preject.mpr_data,
                         (char *)NULL);
        return TCL_ERROR;
    }

    status = CMIP_WaitResponse(interp, cmiph, M_CANCEL_GET, callback);

    return status;

 usage:
    Tcl_AppendResult(interp, "wrong # args: should be \"",
		     argv[0], " ", argv[1], " requesthandle",
		     " ?-callback callback?\"",
		     (char *)NULL);
    return TCL_ERROR;
} /* CMIP_CancelGet() */
