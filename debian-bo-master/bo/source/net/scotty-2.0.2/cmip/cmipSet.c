/*
 * cmipSet.c 
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
CMIP_Set(cmiph, interp, argc, argv)
    CMIP_Handle  *cmiph;
    Tcl_Interp	 *interp;
    int		  argc;
    char	**argv;
{
    char	 *strObjClass  = NULLCP;
    char	 *strObjInst   = NULLCP;
    char	 *strScope     = NULLCP;
    char	 *strSync      = NULLCP;
    char	 *strFilter    = NULLCP;

    char	**strAttrs     = NULL;
    int		  nattrs       = 0;
    char	**strAttrModify= NULL;
    char	**strAttrType  = NULL;
    char	**strAttrValue = NULL;

    char	 *callback    = NULLCP;

    int		  nonconfirmed = 0;	/* default: confirmed service */

    int		  listArgc;
    char	**listArgv;
    int		  strFilterLength;

    int		  i, j, status;

    MIDentifier	  obj_class;
    MName	  obj_inst;
    CMISSetAttr	 *attrs    = NULLMSETATTR;
    CMISScope	 *scope    = NULLMSCOPE;   /* default: no scoping */
    CMISSync	  sync     = s_bestEffort; /* default: best effort synchron. */
    CMISFilter	 *filter   = NULLMFILTER;  /* default: no filtering */

    MSAPIndication	mi;

    /* parse command line arguments */

    if (argc < 5) {
	goto usage;
    }

    if (argv[2][0] != '\0') strObjClass = argv[2];
    if (argv[3][0] != '\0') strObjInst  = argv[3];

    if (!strObjClass ||
	(!strObjInst && strcmp(strObjClass, "system") != 0)) {
	goto usage;
    }

    /* extract the attribute list */

    if (Tcl_SplitList(interp, argv[4], &nattrs, &strAttrs) != TCL_OK) {
	return TCL_ERROR;
    }
    if (nattrs <= 0) goto usage; /* there has to be at least one */

    /* and now extract modifyOp, attributeTyp and
     * if there the attributeValue
     */
    strAttrType   = (char **) ckalloc(nattrs * sizeof(*strAttrType));
    strAttrValue  = (char **) ckalloc(nattrs * sizeof(*strAttrValue));
    strAttrModify = (char **) ckalloc(nattrs * sizeof(*strAttrModify));
    for (j=0; j < nattrs; j++) {
	if (Tcl_SplitList(interp, strAttrs[j],
			  &listArgc, &listArgv) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (listArgc == 2) {
	    strAttrType[j]   = ckstrdup(listArgv[0]);
	    strAttrValue[j]  = ckstrdup(listArgv[1]);
	    strAttrModify[j] = ckstrdup("replace");
	} else if (listArgc == 3) {
	    strAttrType[j]   = ckstrdup(listArgv[0]);
	    strAttrValue[j]  = ckstrdup(listArgv[1]);
	    strAttrModify[j] = ckstrdup(listArgv[2]);
	} else { /* no correct attribute-list item */
	    ckfree((char *) strAttrModify);
	    ckfree((char *) strAttrType);
	    ckfree((char *) strAttrValue);
	    ckfree((char *) strAttrs);
	    ckfree((char *) listArgv);
	    goto usage;
	}
	ckfree((char *) listArgv);
    }
    ckfree((char *) strAttrs);

    /* the optional parameter */
    i = 5;
    while (i < argc) {
        if (i == argc-1) {
	    if (strcmp(argv[i], "-nonconfirmed") == 0) {
		nonconfirmed = 1;
		++i;
	    } else {
		goto usage;
	    }
        }
	if (strcmp(argv[i], "-scope") == 0) {
	    if (strScope) {
		Tcl_AppendResult (interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    strScope = argv[++i];
            if ((i < argc-1) && (strcmp(argv[i+1], "-atomic") == 0)) {
		strSync = ckstrdup("atomic");
		++i;
            }
        } else if (strcmp(argv[i], "-filter") == 0) {
	    if (strFilter) {
		Tcl_AppendResult (interp, "duplicate keyword \"", argv[i],
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
        } else if (strcmp(argv[i], "-callback") == 0) {
	    if (callback) {
		Tcl_AppendResult (interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
            callback = argv[++i];
        } else {
	    Tcl_AppendResult (interp, "no such keyword: \"", argv[i], "\"",
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
    if (nattrs>0) {
      int q;
      fprintf(stderr, "{");
      for (q = 0; q < nattrs; q++) {
	fprintf(stderr, "{%s %s %s}",
		strAttrType[q], strAttrValue[q], strAttrModify[q]);
      }
      fprintf(stderr, "}");
    }
    if (strScope)
      fprintf(stderr, " scope %s", strScope);
    if (strSync)
      fprintf(stderr, " sync %s", strSync);
    if (strFilter)
      fprintf(stderr, " filter {%s}", strFilter);
    if (callback)
      fprintf(stderr, " callback %s", callback);
    fprintf(stderr, "\"\n");
#endif

    /* convert the string arguments to CMIS parameters */

    if ((obj_class.mid_global = name2oid(strObjClass)) == NULLOID) {
	Tcl_AppendResult (interp, "no such object class \"", strObjClass, "\"",
                          (char *)NULL);
        return TCL_ERROR;
    }
    obj_class.mid_type   = MID_GLOBAL;

    if (strObjInst) {
	if ((obj_inst.mn_dn = str2dn(strObjInst)) == NULLDN) {
	    Tcl_AppendResult (interp, "no such object instance \"", strObjInst,
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
	Tcl_AppendResult (interp, "no such scope \"", strScope, "\"",
                          (char *)NULL);
        return TCL_ERROR;
    }
    if (strSync && (sync = str2sync(strSync)) == s_invalid) {
	Tcl_AppendResult (interp, "no such synchronisation \"", strSync, "\"",
                          (char *)NULL);
        return TCL_ERROR;
    }
    if (strFilter && strFilter[0] != '\0'  &&
		(filter = str2mfilter(strFilter)) == NULLMFILTER) {
	Tcl_AppendResult (interp, "no such filter \"", strFilter, "\"",
                          (char *)NULL);
        return TCL_ERROR;
    }

    attrs = (CMISSetAttr *) ckalloc(nattrs * sizeof(CMISSetAttr));
    for (i = 0; i < nattrs; i++) {
	Ava* aVa;
	if (strcasecmp(strAttrModify[i], "replace") == 0) {
	    attrs[i].sa_modify = m_replace;
	} else if (strcasecmp(strAttrModify[i], "addvalue") == 0) {
	    attrs[i].sa_modify = m_addValue;
	} else if (strcasecmp(strAttrModify[i], "removevalue") == 0) {
	    attrs[i].sa_modify = m_removeValue;
	} else if (strcasecmp(strAttrModify[i], "settodefault") == 0) {
	    attrs[i].sa_modify = m_setToDefault;
	} else {
	    Tcl_AppendResult(interp, "no such modify operation \"",
			     strAttrModify[i], "\"",
			     (char *)NULL);
	    ckfree((char *) strAttrModify);
	    ckfree((char *) strAttrType);
	    ckfree((char *) strAttrValue);
	    ckfree((char *) attrs);
	    return TCL_ERROR;
	}
	if ((aVa = str2ava(strAttrType[i], strAttrValue[i])) == NULLAVA) {
	    Tcl_AppendResult (interp, "no such attribute type \"",
                              strAttrType[i],
			      "\" or invalid attribute value \"",
                              strAttrValue[i], "\"",
                              (char *)NULL);
	    ckfree((char *) strAttrModify);
	    ckfree((char *) strAttrType);
	    ckfree((char *) strAttrValue);
	    ckfree((char *) attrs);
	    free((char *) aVa);
            return TCL_ERROR;
        }
	attrs[i].sa_id.mid_type = MID_GLOBAL;
	attrs[i].sa_id.mid_global = aVa->ava_oid;
	attrs[i].sa_val = aVa->ava_value;
	attrs[i].sa_error = m_noError; /* ??? */
	free ((char*) aVa);
    }

    /* perform the M-SET request */
    (cmiph->req_nr)++;
    if (nonconfirmed) {
	status = M_Set(cmiph->msd, cmiph->req_nr, &obj_class, &obj_inst,
		       scope, filter, NULLMACCESS, sync, nattrs, attrs, &mi);
    } else {
	status = M_SetConf(cmiph->msd, cmiph->req_nr, &obj_class, &obj_inst,
			   scope, filter, NULLMACCESS, sync, nattrs, attrs,
			   &mi);
    }
    if (status == NOTOK) {
	Tcl_AppendResult (interp, argv[0], " ", argv[1], ": ",
			  mi.mi_preject.mpr_data,
                          (char *)NULL);
        return TCL_ERROR;
    }

    if (!nonconfirmed) {
	status = CMIP_WaitResponse(interp, cmiph, M_SET, callback);
    }

    /* release memory of arguments (for the sake of it...) */

    mid_free(&obj_class);
    mn_free(&obj_inst);
    if (scope) {
        free((char*) scope);
    }
    if (filter) {
        mfilter_free(filter);
    }
/* muss noch ein paar mehr wieder freigeben */
    if (strSync) {
	ckfree((char*) strSync);
    }
    if (strFilter) {
	ckfree((char *) strFilter);
    }
    for (i = 0; i < nattrs; i++) {
	ckfree((char *) strAttrType[i]);
	ckfree((char *) strAttrValue[i]);
	ckfree((char *) strAttrModify[i]);
    }
    if (strAttrModify) {
	ckfree((char *) strAttrModify);
    }
    if (strAttrType) {
	ckfree((char *) strAttrType);
    }
    if (strAttrValue) {
	ckfree((char *) strAttrValue);
    }
    if (attrs) {
	ckfree((char *) attrs);
    }

    return status;

 usage:
    Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
		      " ", argv[1], " class instance attributes",
                      " ?-scope scope ?-atomic? ?",
		      " ?-filter filter?",
		      " ?-callback callback?",
		      " ?-nonconfirmed?\"",
                      (char *)NULL);
    return TCL_ERROR;

} /* CMIP_Set() */
