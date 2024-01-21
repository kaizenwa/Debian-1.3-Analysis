/*
 * cmipGet.c 
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
CMIP_Get(cmiph, interp, argc, argv)
     CMIP_Handle *cmiph;
     Tcl_Interp	 *interp;
     int	  argc;
     char	**argv;
{
    char	 *strObjClass = NULLCP;
    char	 *strObjInst  = NULLCP;
    char	 *strScope    = NULLCP;
    char	 *strSync     = NULLCP;
    char	 *strFilter   = NULLCP;

    char	**strAttrs    = NULL;
    char	**attrsList   = NULL;
    int		  nattrs      = GET_ALLATTRS; /* default: all attributes */

    char	 *callback    = NULLCP;

    int		  listArgc;
    char	**listArgv;
    int		  strFilterLength;

    int		  i, j, status;
 
    MIDentifier	  obj_class;
    MName	  obj_inst;
    CMISScope	 *scope    = NULLMSCOPE;   /* default: no scoping */
    CMISSync	  sync     = s_bestEffort; /* default: best effort synchron. */
    CMISFilter	 *filter   = NULLMFILTER;  /* default: no filtering */
    MIDentifier	 *attrs;

    MSAPIndication	mi;

    /* parse command line arguments */

    if (argc < 4) {
	goto usage;
    }

    if (argv[2][0] != '\0') strObjClass = argv[2];
    if (argv[3][0] != '\0') strObjInst  = argv[3];

    if (!strObjClass ||
	(!strObjInst && strcmp(strObjClass, "system") != 0)) {
	goto usage;
    }

    /* the optional parameter */
    i = 4;
    while (i < argc) {
        if (i == argc-1) {
	    goto usage;
	}
	if (strcmp(argv[i], "-scope") == 0) {
	    if (strScope) {
		Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
            strScope = argv[++i];
	    /* synchronisation only makes sense, if there is a scope */
	    if ((i < argc-1) && (strcmp(argv[i+1], "-atomic") == 0)) {
		strSync = ckstrdup("atomic");
		++i;
	    }
        } else if (strcmp(argv[i], "-filter") == 0) {
	    if (strFilter) {
		Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
            if (Tcl_SplitList(interp, argv[++i], &listArgc,
                              &listArgv) != TCL_OK) {
                return TCL_ERROR;
            }
	    if (listArgc <= 0) goto usage;
            for (j=0, strFilterLength=0; j < listArgc; j++)
                strFilterLength += strlen(listArgv[j])+1; /* +1 for ' ' */
            strFilter = ckalloc(strFilterLength+1);
            *strFilter= '\0';
            for (j=0; j < listArgc; j++) {
                strcat(strFilter, listArgv[j]);
                strcat(strFilter, " ");
            }
            ckfree((char *)listArgv);
        } else if (strcmp(argv[i], "-attributes") == 0) {
	    if (nattrs > 0) {
		Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
            if (Tcl_SplitList(interp, argv[++i], &nattrs,
                              &attrsList) != TCL_OK) {
                return TCL_ERROR;
            }
	    if (nattrs < 0) goto usage;
	    if (nattrs == 0) {
		nattrs = GET_NOATTRS; /* an empty list */
	    } else {
		strAttrs = (char**) ckalloc(nattrs * sizeof(*strAttrs));
		for (j = 0; j < nattrs; j++) {
		    if (Tcl_SplitList(interp, attrsList[j], &listArgc,
				      &listArgv) != TCL_OK) {
			return TCL_ERROR;
		    }
		    if (listArgc < 1) goto usage;
		    /* only the first element, cause it should be
		       the attributename, ignore everything behind it! */
		    strAttrs[j] = ckstrdup(listArgv[0]);
		}
		ckfree((char *) listArgv);
	    }
	    ckfree((char *) attrsList);
        } else if (strcmp(argv[i], "-callback") == 0) {
	    if (callback) {
		Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
            callback = argv[++i];
        } else {
	    Tcl_AppendResult(interp, "no such keyword: \"", argv[i], "\"",
			      (char *)NULL);
	    return TCL_ERROR;
	}
        i++;
    }
#ifdef DEBUG_CMIP
    fprintf(stderr, "Aufruf: \"%s %s", argv[0], argv[1]);
    if (strObjClass)
      fprintf(stderr, " %s", strObjClass);
    if (strObjInst)
      fprintf(stderr, " %s", strObjInst);
    if (strScope)
      fprintf(stderr, " scope %s", strScope);
    if (strSync)
      fprintf(stderr, " sync %s", strSync);
    if (strFilter)
      fprintf(stderr, " filter {%s}", strFilter);
    if (nattrs>0) {
      int q;
      fprintf(stderr, " attributes {");
      for (q = 0; q < nattrs; q++) {
	fprintf(stderr, "%s ", strAttrs[q]);
      }
      fprintf(stderr, "}");
    } else if (nattrs==GET_NOATTRS)
	    fprintf(stderr, " attributes {}");
    if (callback)
      fprintf(stderr, " callback %s", callback);
    fprintf(stderr, "\"\n");
#endif

    /* convert the string arguments to CMIS parameters */

    if ((obj_class.mid_global = name2oid(strObjClass)) == NULLOID) {
        Tcl_AppendResult(interp, "no such object class \"", strObjClass, "\"",
                         (char *)NULL);
        return TCL_ERROR;
    }
    obj_class.mid_type = MID_GLOBAL;

    if (strObjInst) {
	if ((obj_inst.mn_dn = str2dn(strObjInst)) == NULLDN) {
	    Tcl_AppendResult(interp, "no such object instance \"", strObjInst,
			      "\"", (char *)NULL);
	    return TCL_ERROR;
	}
    } else {
	obj_inst.mn_dn = dn_comp_new(NULLRDN);
    }
    if (strObjInst && strncasecmp(strObjInst, "sysName=", 8) == 0) {
	obj_inst.mn_type = MN_DN;
    } else {
	obj_inst.mn_type = MN_LOCALDN;
    }

    if (strScope && (scope = str2scope(strScope)) == NULLMSCOPE) {
        Tcl_AppendResult(interp, "no such scope \"", strScope, "\"",
                          (char *)NULL);
        return TCL_ERROR;
    }

    if (strSync && (sync = str2sync(strSync)) == s_invalid) {
        Tcl_AppendResult(interp, "no such synchronisation \"", strSync, "\"",
                          (char *)NULL);
        return TCL_ERROR;
    }

    if (strFilter && strFilter[0] != '\0'  &&
                (filter = str2mfilter(strFilter)) == NULLMFILTER) {
        Tcl_AppendResult(interp, "no such filter \"", strFilter, "\"",
                         (char *)NULL);
        return TCL_ERROR;
    }

    attrs= (MIDentifier *) ckalloc(nattrs * sizeof(MIDentifier));
    for (i = 0; i < nattrs; i++) {
	if ((attrs[i].mid_global = name2oid(strAttrs[i])) == NULLOID) {
	    Tcl_AppendResult(interp, "no such attribute name \"",
			      strAttrs[i], "\"", (char *)NULL);
	    ckfree((char *) attrs);
	    return TCL_ERROR;
	}
	attrs[i].mid_type = MID_GLOBAL;
    }

/*
 * eventuell m"u"sen noch andere access-Arten ber"ucksichtigt werden
 */
    /* perform the M-GET request */
    (cmiph->req_nr)++;
    status = M_Get(cmiph->msd, cmiph->req_nr, &obj_class, &obj_inst, scope,
                   filter, NULLMACCESS, sync, nattrs, attrs, &mi);

    if (status == NOTOK) {
        Tcl_AppendResult(interp, argv[0], " ", argv[1], ": ",
			  mi.mi_preject.mpr_data,
                         (char *)NULL);
        return TCL_ERROR;
    }

    status = CMIP_WaitResponse(interp, cmiph, M_GET, callback);

    /* release memory of arguments */

    mid_free(&obj_class);
    mn_free(&obj_inst);
    if (scope) {
        free((char*) scope);
    }
    if (filter) {
        mfilter_free(filter);
    }
    for (i = 0; i < nattrs; i++) {
        mid_free(&attrs[i]);
    }
    if (strFilter) {
	ckfree((char*) strFilter);
    }
    if (strSync) {
	ckfree((char*) strSync);
    }
    if (attrs) {
	ckfree((char*) attrs);
    }

    return status;

 usage:
    Tcl_AppendResult(interp, "wrong # args: should be \"",
		     argv[0], " ", argv[1],
		     " class instance",
		     " ?-scope scope ?-atomic? ?",
		     " ?-filter filterlist?",
		     " ?-attributes attributes?",
		     " ?-callback callback?\"",
		     (char *)NULL);
    return TCL_ERROR;
} /* CMIP_Get() */
