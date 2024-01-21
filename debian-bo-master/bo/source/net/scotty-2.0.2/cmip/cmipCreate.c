/*
 * cmipCreate.c 
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
CMIP_Create(cmiph, interp, argc, argv)
    CMIP_Handle *cmiph;
    Tcl_Interp  *interp;
    int          argc;
    char       **argv;
{
    char	 *strObjClass = NULLCP;
    char	 *strObjInst  = NULLCP;
    char	 *strRefInst  = NULLCP;

    char	**strAttrs    = NULL;
    int		  nattrs      = 0;
    char	**strAttrType = NULL;
    char	**strAttrValue= NULL;

    char	 *callback    = NULLCP;

    int		  listArgc;
    char	**listArgv;

    int		  i, j, status;
 
    MIDentifier	  obj_class;
    MName	  obj_inst;
    MName	  ref_inst;
    int		  instType	= -1; /* see CMISCreateArg in mparm.h */
    CMISParam	 *attrs;

    MSAPIndication	mi;

    obj_class.mid_type   = MID_GLOBAL;
    obj_class.mid_global = NULLOID;

    obj_inst.mn_type = ref_inst.mn_type = -1;	/* not given */
    obj_inst.mn_dn   = ref_inst.mn_dn = NULLDN;

    /* parse command line arguments */

    if (argc < 3)
	goto usage;

    if (argv[2][0] != '\0') strObjClass = argv[2];

    if (!strObjClass)
	goto usage;

    i = 3;
    while (i < argc) {
        if (i == argc-1) {
	    goto usage;
	}
        if (strcmp(argv[i], "-instance") == 0) {
	    if (strObjInst) {
		if (instType == CA_SUPERIOR_INST) {
		    Tcl_AppendResult(interp, "-instance and -superior used:",
				      " try only one of them",
				     (char *) NULL);
		    return TCL_ERROR;
		} else {
		    Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				      "\"", (char *) NULL);
		    return TCL_ERROR;
		}
	    }
            strObjInst = argv[++i];
	    instType = CA_OBJECT_INST;
        } else if (strcmp(argv[i], "-superior") == 0) {
	    if (strObjInst) {
		if (instType == CA_OBJECT_INST) {
		    Tcl_AppendResult(interp, "-instance and -superior used:",
				      " try only one of them",
				     (char *) NULL);
		    return TCL_ERROR;
		} else {
		    Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				      "\"", (char *) NULL);
		    return TCL_ERROR;
		}
	    }
            strObjInst = argv[++i];
	    instType = CA_SUPERIOR_INST;
        } else if (strcmp(argv[i], "-reference") == 0) {
	    if (strRefInst) {
		Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    strRefInst = argv[++i];
        } else if (strcmp(argv[i], "-attributes") == 0) {
	    if (nattrs > 0) {
		Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
            if (Tcl_SplitList(interp, argv[++i], &nattrs, &strAttrs) != TCL_OK)
	    {
                return TCL_ERROR;
            }
	    if (nattrs <= 0) goto usage;
	    strAttrType = (char **) ckalloc(nattrs * sizeof(*strAttrType));
	    strAttrValue = (char **) ckalloc(nattrs * sizeof(*strAttrValue));
	    for (j=0; j < nattrs; j++) {
		if (Tcl_SplitList(interp, strAttrs[j],
				  &listArgc, &listArgv) != TCL_OK) {
		    return TCL_ERROR;
		}
		if (listArgc == 2) {
		    strAttrType[j]  = ckstrdup(listArgv[0]);
		    strAttrValue[j] = ckstrdup(listArgv[1]);
		} else {
		    ckfree((char *) strAttrs);
		    ckfree((char *) listArgv);
		    Tcl_AppendResult(interp, "malformed attribute-list:",
				      " should be a list of attribute-type",
				      " and -value pairs",
				     (char *)NULL);
		    return TCL_ERROR;
		}
		ckfree((char *) listArgv);
	    }
            ckfree((char *) strAttrs);
        } else if (strcmp(argv[i], "-callback") == 0) {
	    if (callback) {
		Tcl_AppendResult(interp, "duplicate keyword \"", argv[i],
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
            callback = argv[++i];
        } else {
	    Tcl_AppendResult(interp, "wrong keyword \"", argv[i], "\":",
			     "should be -instance, -superior,",
			     " -reference, -attributes",
			     " or -callback",
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
      if (instType==CA_OBJECT_INST)
        fprintf(stderr, " instance %s", strObjInst);
      else
        fprintf(stderr, " superior %s", strObjInst);
    if (strRefInst)
      fprintf(stderr, " reference %s", strRefInst);
    if (nattrs>0) {
      int q;
      fprintf(stderr, " attributes {");
      for (q = 0; q < nattrs; q++) {
	fprintf(stderr, " {%s %s}", strAttrType[q], strAttrValue[q]);
      }
      fprintf(stderr, "}");
    }
    fprintf(stderr, "\"\n");
#endif

    /* convert the string arguments to CMIS parameters */

    if ((obj_class.mid_global = name2oid(strObjClass)) == NULLOID) {
	Tcl_AppendResult(interp, "no such object class \"", strObjClass, "\"",
                         (char *)NULL);
        return TCL_ERROR;
    }

    if (strObjInst) {
	if ((obj_inst.mn_dn = str2dn(strObjInst)) == NULLDN) {
	    Tcl_AppendResult(interp, "no such object instance \"", strObjInst,
			      "\"", (char *)NULL);
	    return TCL_ERROR;
	}
	/* in MCREATE: MN_LOCALDN aber in M_Create(3N): MN_DN */
	obj_inst.mn_type = MN_DN;
    }
    if (strRefInst) {
	if ((ref_inst.mn_dn = str2dn(strRefInst)) == NULLDN) {
	    Tcl_AppendResult(interp, "no such reference instance \"",
			      strRefInst, "\"",
			     (char *)NULL);
	    return TCL_ERROR;
	}
	/* ^ s. o. obj_inst ^ */
	ref_inst.mn_type = MN_DN;
    }
    attrs= (CMISParam *) ckalloc(nattrs*sizeof(CMISParam));
    for (i = 0; i < nattrs; i++) {
	Ava* aVa;
	if ((aVa = str2ava(strAttrType[i], strAttrValue[i])) == NULLAVA) {
	    Tcl_AppendResult(interp, "no such attribute type \"",
			      strAttrType[i], "\" or value \"",
			      strAttrValue[i], "\"",
			     (char *)NULL);
	    ckfree((char *) attrs);
	    return TCL_ERROR;
	}
	attrs[i].mp_id.mid_type = MID_GLOBAL;
	attrs[i].mp_id.mid_global = aVa->ava_oid;
	attrs[i].mp_val = aVa->ava_value;
	free((char*) aVa);
    }

    /* perform the M-CREATE request */
    (cmiph->req_nr)++;
    status = M_Create(cmiph->msd, cmiph->req_nr, &obj_class,
		      obj_inst.mn_type == -1 ? NULLMN : &obj_inst, instType,
		      ref_inst.mn_type == -1 ? NULLMN : &ref_inst,
		      NULLMACCESS, nattrs, attrs, &mi);

    if (status == NOTOK) {
        Tcl_AppendResult(interp, argv[0], " ", argv[1], ": ",
			  mi.mi_preject.mpr_data,
                         (char *)NULL);
        return TCL_ERROR;
    }

    status = CMIP_WaitResponse(interp, cmiph, M_CREATE, callback);

    /* release memory of arguments */
    mid_free(&obj_class);
    if (obj_inst.mn_dn)
	mn_free(&obj_inst);
    if (ref_inst.mn_dn)
	mn_free(&ref_inst);
    for (i = 0; i < nattrs; i++)
	mparm_free(&attrs[i]);
    ckfree((char*) attrs);
    if (strAttrType) {
	ckfree((char*) strAttrType);
    }
    if (strAttrValue) {
	ckfree((char*) strAttrValue);
    }

    return status;

 usage:
    Tcl_AppendResult(interp, "wrong # args: should be \"",
		     argv[0], " ", argv[1],
		     " class ?-instance |",
		     " -superior instance? ?-reference instance?",
		     " ?-attributes attributes?",
		     " ?-callback callback?\"",
		     (char *)NULL);
    return TCL_ERROR;
} /* CMIP_Create() */
