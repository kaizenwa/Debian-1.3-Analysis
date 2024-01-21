/*
 * cmipWait.c 
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

/* what an attribute request-result may include */
typedef struct attrRes {
    CMISModifyOp	modify;	/* only for SET-responses */
				/* is m_noModifyOp for others */
    CMISParam		attr;   /* type and/or value */
    CMISErrors		error;  /* possible error occured */
} attrRes;

/* the routine, that waits for request-results on a management association

   parameters:
   clientData:	a char* to the cmip association handle (created by connect)
   interp:	a pointer to the tcl-interpreter structure
   CMIP_Request: maybe a pointer to an CMIP_Request structure or
		a NULL-pointer (when called asynchronous)

   it maybe used in an asynchronous or synchronous way:
   asynchronous: rh is a NULL-pointer
   synchonous:	 rh is a pointer to an CMIP_Request structure of an
		 outstanding request-result

   possible return values: TCL_CONTINUE, TCL_OK or TCL_ERROR
   TCL_CONTINUE: incomplete result there will be more, use wait() again
   TCL_OK:	 the result is complete and correct
   TCL_ERROR:	 an error has occured
 */
int
CMIP_Wait(cmiph, interp, rh)
    CMIP_Handle		*cmiph;
    Tcl_Interp		*interp;
    CMIP_Request	*rh;
{
    MSAPIndication  my_mi, *mi = &my_mi;

    int		 id	 = 0;
    MID		 MOClass = NULLMID;
    MN		 MOInst  = NULLMN;
    char	*curtime = NULLCP;
    CMISErrors	 error	 = m_noError;
    Tcl_DString	 errorMsg;

    int		 nattrs  = 0;
    attrRes	*attrs   = ((attrRes *) 0);

    Tcl_HashEntry	*ht_entry;
    Tcl_HashSearch	 search;
    
    int		 status = TCL_ERROR;
    int		 i;

    int		 dochnichtsynchron = FALSE; /* hmmm!? */

    Tcl_DStringInit(&errorMsg);

    /* wait for a result to come */
    M_WaitReq(cmiph->msd, NOTOK, mi); /* NOTOK means wait blocked */

#ifdef DEBUG_CMIP
    fprintf(stderr, "[%d]", mi->mi_type);
#endif

    switch (mi->mi_type) { /* which type of result */

    case MI_GET_RES: {	/* only one object in remote MIB */
	CMISGetRes	*gr;

	id =  mi->mi_getres.id;
	gr = &mi->mi_getres.result;

	MOClass = &gr->gr_class;
	MOInst  = &gr->gr_inst;
	curtime =  gr->gr_time;
	nattrs  =  gr->gr_nattrs;
	attrs   = (attrRes *) ckalloc(nattrs * sizeof(attrRes));
	for (i=0; i<nattrs; i++) {
	    attrs[i].modify = m_noModifyOp; /* cause its a GET-response */
	    attrs[i].attr   = gr->gr_attrs[i].ga_ava;
	    attrs[i].error  = gr->gr_attrs[i].ga_error;
	}
	error = mi->mi_errtype;
	status = TCL_OK;
	break;
    }
    case MI_SET_RES: {	/* only one object in remote MIB */
	CMISSetRes	*sr;

	id =  mi->mi_setres.id;
	sr = &mi->mi_setres.result;

	MOClass = &sr->sr_class;
	MOInst  = &sr->sr_inst;
	curtime =  sr->sr_time;
	nattrs  =  sr->sr_nattrs;
	attrs   = (attrRes *) ckalloc(nattrs * sizeof(attrRes));
	for (i=0; i<nattrs; i++) {
	    attrs[i].modify = sr->sr_attrs[i].sa_modify;
	    attrs[i].attr   = sr->sr_attrs[i].sa_ava;
	    attrs[i].error  = sr->sr_attrs[i].sa_error;
	}
	error = mi->mi_errtype;
	status = TCL_OK;
	break;
    }
    case MI_ACTION_RES: {	/* only one object in remote MIB */
	CMISActionRes	*ar;

	id =  mi->mi_actionres.id;
	ar = &mi->mi_actionres.result;

	MOClass = &ar->ar_class;
	MOInst  = &ar->ar_inst;
	curtime =  ar->ar_time;
	nattrs  =  1;
	attrs   = (attrRes *) ckalloc(sizeof(attrRes));
	attrs[1].modify = m_noModifyOp; /* cause its an ACTION-response */
	attrs[1].attr   = ar->ar_reply;
	attrs[1].error  = m_noError;

	error = mi->mi_errtype;
	status = TCL_OK;
	break;
    }
    case MI_CREATE_RES: {	/* only one object in remote MIB */
	CMISCreateRes	*cr;

	id =  mi->mi_createres.id;
	cr = &mi->mi_createres.result;

	MOClass = &cr->cr_class;
	MOInst  = &cr->cr_inst;
	curtime =  cr->cr_time;
	nattrs  =  cr->cr_nattrs;
	attrs   = (attrRes *) ckalloc(nattrs * sizeof(attrRes));
	for (i=0; i<nattrs; i++) {
	    attrs[i].modify = m_noModifyOp; /* cause its an CREATE-response */
	    attrs[i].attr   = cr->cr_attrs[i];
	    attrs[i].error  = m_noError;
	}
	error = mi->mi_errtype;
	status = TCL_OK;
	break;
    }
    case MI_DELETE_RES: {	/* only one object in remote MIB */
	CMISDeleteRes	*dr;

	id =  mi->mi_deleteres.id;
	dr = &mi->mi_deleteres.result;

	MOClass = &dr->dr_class;
	MOInst  = &dr->dr_inst;
	curtime =  dr->dr_time;

	error = mi->mi_errtype;
	status = TCL_OK;
	break;
    }
    case MI_EVENT_REP: {
        struct CMISEventReportArg* ea;

	id =  mi->mi_eventrepinv.id;
        ht_entry = Tcl_FindHashEntry(cmiph->req_table, "eventreport");
        if (ht_entry == NULL) {
            Tcl_AppendResult(interp, "no such request handle \"",
                              "eventreport", "\"",
                             (char *) NULL);
            return TCL_ERROR;
        }
	rh = (CMIP_Request *) Tcl_GetHashValue(ht_entry);
	rh->request_id = id;
	rh = (CMIP_Request *) NULL;

	ea = &mi->mi_eventrepinv.args;

        MOClass = &ea->ea_class;
        MOInst  = &ea->ea_inst;
        curtime =  ea->ea_time;
	nattrs  = 1;
        attrs[1].modify = m_noModifyOp; /* cause its an EVENT-request */
        attrs[1].attr.mp_id  = ea->ea_type;
	attrs[1].attr.mp_val = ea->ea_info;
        attrs[1].error  = m_noError;

	error = mi->mi_errtype;
	status = TCL_OK;
        break;
    }
    case MI_EVENT_REPC: {
        struct CMISEventReportArg* ea;

	MSAPIndication	temp_mi;

	id =  mi->mi_eventrepinv.id;
        ht_entry = Tcl_FindHashEntry(cmiph->req_table, "eventreport");
        if (ht_entry == NULL) {
            Tcl_AppendResult(interp, "no such request handle \"",
                              "eventreport", "\"",
                             (char *) NULL);
            return TCL_ERROR;
        }
	rh = (CMIP_Request *) Tcl_GetHashValue(ht_entry);
	rh->request_id = id;
	rh = (CMIP_Request *) NULL;

	ea = &mi->mi_eventrepinv.args;

	/* next three should be given back confirm */
        MOClass = &ea->ea_class;
        MOInst  = &ea->ea_inst;
        curtime =  ea->ea_time;

	(cmiph->req_nr)++;
/* eventuell andere curtime (aktuelle?), NULLMPARAM (werweiswas?)
 *		NULLMERRORINFO (werweiswasfuereinfehler?)
 */
	status = M_EventRepRes(cmiph->msd, cmiph->req_nr, MOClass, MOInst,
			       curtime, NULLMPARM, m_noError,
			       NULLMERRORINFO, &temp_mi);

	if (status == NOTOK) {
	    Tcl_AppendResult (interp, "M_EventRepRes(): ",
			      temp_mi.mi_preject.mpr_data,
			      (char *)NULL);
	    return TCL_ERROR;
	}

	nattrs  = 1;
        attrs[1].modify = m_noModifyOp; /* cause its an EVENT-request */
        attrs[1].attr.mp_id  = ea->ea_type;
	attrs[1].attr.mp_val = ea->ea_info;
        attrs[1].error  = m_noError;

	error = mi->mi_errtype;
	status = TCL_OK;
        break;
    }
    case MI_ERROR: {
	error = mi->mi_errtype;
	Tcl_DStringAppend(&errorMsg, MErrString(error), -1);
	status = TCL_ERROR; /* ausser bei getListError und setListError */

	switch (mi->mi_errtype) {
	  case m_getListError: {
	      /* only one object in remote MIB */
	      CMISGetRes	*gr;

	      id =  mi->mi_getres.id;
	      gr = &mi->mi_getres.result;

	      MOClass = &gr->gr_class;
	      MOInst  = &gr->gr_inst;
	      curtime =  gr->gr_time;
	      nattrs  =  gr->gr_nattrs;
	      attrs   = (attrRes *) ckalloc(nattrs * sizeof(attrRes));
	      for (i=0; i<nattrs; i++) {
		  attrs[i].modify = m_noModifyOp; /* GET-response */
		  attrs[i].attr   = gr->gr_attrs[i].ga_ava;
		  attrs[i].error  = gr->gr_attrs[i].ga_error;
	      }
	      status = TCL_OK;
	      break;
	  }
	  case m_setListError: {
	      /* only one object in remote MIB */
	      CMISSetRes	*sr;

	      id =  mi->mi_setres.id;
	      sr = &mi->mi_setres.result;

	      MOClass = &sr->sr_class;
	      MOInst  = &sr->sr_inst;
	      curtime =  sr->sr_time;
	      nattrs  =  sr->sr_nattrs;
	      attrs   = (attrRes *) ckalloc(nattrs * sizeof(attrRes));
	      for (i=0; i<nattrs; i++) {
		  attrs[i].modify = sr->sr_attrs[i].sa_modify;
		  attrs[i].attr   = sr->sr_attrs[i].sa_ava;
		  attrs[i].error  = sr->sr_attrs[i].sa_error;
	      }
	      status = TCL_OK;
	      break;
	  }

	    /* extract id and errorMsg see mparm.h: CMISErrorInfo */

	  case m_noSuchObjectClass: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg,
				    oid2name(
			    mi->mi_error.info.ei_noSuchObjectClass.mid_global,
					     OIDPART), -1);
	      }
	      break;
	  }
	  case m_noSuchAttribute: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg,
				    oid2name(
			    mi->mi_error.info.ei_noSuchAttribute.mid_global,
					     OIDPART), -1);
	      }
	      break;
	  }
	  case m_accessDenied: { /* not sure ??? */
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg,
				    oid2name(
		    mi->mi_error.info.ei_linkedActionAccessDenied.mid_global,
					     OIDPART), -1);
	      }
	      break;
	  }
	  case m_missingAttributeValue: {
#ifdef OSIMIS_3
	      if (&mi->mi_error.info) {
		  CMISMissingAttrValue	ei_mav;

		  id = mi->mi_error.id;
		  ei_mav = mi->mi_error.info.ei_missingAttributeValue;

		  Tcl_DStringAppend(&errorMsg, "(s):", -1);
		  for (i=0; i<ei_mav.mav_nattrs; i++) {
		      Tcl_DStringAppend(&errorMsg, " ", -1);
		      Tcl_DStringAppend(&errorMsg,
				    oid2name(ei_mav.mav_attrs[i].mid_global,
					     OIDPART), -1);
		  }
	      }
#else
	      Tcl_DStringAppend(&errorMsg, "missing Attribute Value", -1);
#endif
	      break;
	  }
	  case m_noSuchObjectInstance: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, dn2str(
			    mi->mi_error.info.ei_noSuchObjectInstance.mn_dn),
				    -1);
	      }
	      break;
	  }
	  case m_invalidObjectInstance: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, dn2str(
			    mi->mi_error.info.ei_invalidObjectInstance.mn_dn),
				    -1);
	      }
	      break;
	  }
	  case m_duplicateManagedObjectInstance: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, dn2str(
			mi->mi_error.info.ei_duplicateObjectInstance.mn_dn),
				    -1);
	      }
	      break;
	  }
	  case m_noSuchReferenceObject: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, dn2str(
			    mi->mi_error.info.ei_noSuchReferenceObject.mn_dn),
				    -1);
	      }
	      break;
	  }
	  case m_invalidAttributeValue: {
	      if (&mi->mi_error.info) {
		  CMISParam	ei_iav;
		  char *argid, *argval;

		  id = mi->mi_error.id;
		  ei_iav = mi->mi_error.info.ei_invalidAttributeValue;

		  ava2str(ei_iav.mp_id.mid_global,
			  ei_iav.mp_val,
			  &argid, &argval);
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, argid, -1);
		  Tcl_DStringAppend(&errorMsg, " ", -1);
		  Tcl_DStringAppend(&errorMsg, argval, -1);
	      }
	      break;
	  }
	  case m_invalidScope: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, sprintscope(
					&mi->mi_error.info.ei_invalidScope),
				    -1);
	      }
	      break;
	  }
	  case m_invalidFilter: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, sprintmfilter(
					mi->mi_error.info.ei_invalidFilter),
				    -1);
	      }
	      break;
	  }
	  case m_syncNotSupported: {
	      if (&mi->mi_error.info) {
		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, sprintsync(
				    mi->mi_error.info.ei_syncNotSupported),
				    -1);
	      }
	      break;
	  }
	  case m_invalidOperator: {
#ifdef OSIMIS_3
	      if (&mi->mi_error.info) {
		  char	buf[10];

		  id = mi->mi_error.id;
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  sprintf(buf, "%d", mi->mi_error.info.ei_invalidOperator);
		  Tcl_DStringAppend(&errorMsg, buf, -1);
	      }
#else
	      Tcl_DStringAppend(&errorMsg, "invalid Operator", -1);
#endif
	      break;
	  }
	  case m_noSuchInvokeId: {
	      if (&mi->mi_error.info) {
		  char	buf[10];

		  id = mi->mi_error.id; /* same as noSuchInvokeId ??? */
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  sprintf(buf, "%d", mi->mi_error.info.ei_noSuchInvokeId);
		  Tcl_DStringAppend(&errorMsg, buf, -1);
	      }
	      break;
	  }
	  case m_noSuchAction: {
	      if (&mi->mi_error.info) {
		  CMISNoSuchArgument	ei_nsa;

		  id = mi->mi_error.id;
		  ei_nsa = mi->mi_error.info.ei_noSuchAction;

		  Tcl_DStringAppend(&errorMsg, ": class ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_nsa.nsa_class.mid_global,
							OIDPART),
				    -1);
		  Tcl_DStringAppend(&errorMsg, ", type ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_nsa.nsa_type.mid_global,
							OIDPART),
				    -1);
		  /* nsa_operation ??? */
	      }
	      break;
	  }
	  case m_noSuchEventType: {
	      if (&mi->mi_error.info) {
		  CMISNoSuchArgument	ei_nsa;

		  id = mi->mi_error.id;
		  ei_nsa = mi->mi_error.info.ei_noSuchEventType;

		  Tcl_DStringAppend(&errorMsg, ": class ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_nsa.nsa_class.mid_global,
							OIDPART),
				    -1);
		  Tcl_DStringAppend(&errorMsg, ", type ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_nsa.nsa_type.mid_global,
							OIDPART),
				    -1);
		  /* nsa_operation ??? */
	      }
	      break;
	  }
	  case m_noSuchArgument: {
	      if (&mi->mi_error.info) {
		  CMISNoSuchArgument	ei_nsa;

		  id = mi->mi_error.id;
		  ei_nsa = mi->mi_error.info.ei_noSuchArgument;

		  Tcl_DStringAppend(&errorMsg, ": class ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_nsa.nsa_class.mid_global,
							OIDPART),
				    -1);
		  Tcl_DStringAppend(&errorMsg, ", type ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_nsa.nsa_type.mid_global,
							OIDPART),
				    -1);
		  /* nsa_operation ??? */
	      }
	      break;
	  }
	  case m_invalidArgumentValue: {
	      if (&mi->mi_error.info) {
		  CMISInvalidArgumentValue	ei_iav;
		  char *argid, *argval;

		  id = mi->mi_error.id;
		  ei_iav = mi->mi_error.info.ei_invalidArgumentValue;

		  ava2str(ei_iav.iav_id.mid_global,
			  ei_iav.iav_val,
			  &argid, &argval);
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, argid, -1);
		  Tcl_DStringAppend(&errorMsg, " ", -1);
		  Tcl_DStringAppend(&errorMsg, argval, -1);
	      }
	      break;
	  }
	  case m_processingFailure: {
	      if (&mi->mi_error.info) {
		  CMISProcessingFailure	ei_pf;
		  char *argid, *argval;

		  id = mi->mi_error.id;
		  ei_pf = mi->mi_error.info.ei_processingFailure;

		  Tcl_DStringAppend(&errorMsg, ": class ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_pf.pf_class.mid_global,
							OIDPART),
				    -1);
		  Tcl_DStringAppend(&errorMsg, ", instance ", -1);
		  Tcl_DStringAppend(&errorMsg, dn2str(ei_pf.pf_inst.mn_dn),
				    -1);
		  /* es gibt noch ein ei_pf.pf_error vom typ CMISParam: */
		  ava2str(ei_pf.pf_error.mp_id.mid_global,
			  ei_pf.pf_error.mp_val,
			  &argid, &argval);
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, argid, -1);
		  Tcl_DStringAppend(&errorMsg, " ", -1);
		  Tcl_DStringAppend(&errorMsg, argval, -1);
	      }
	      break;
	  }
	  case m_complexityLimitation: {
	      if (&mi->mi_error.info) {
		  CMISComplexityLimitation ei_cl;

		  id = mi->mi_error.id;
		  ei_cl = mi->mi_error.info.ei_complexityLimitation;

		  Tcl_DStringAppend(&errorMsg, ":", -1);
		  if (&ei_cl.cl_scope != NULLMSCOPE) {
		      Tcl_DStringAppend(&errorMsg, " scope ", -1);
		      Tcl_DStringAppend(&errorMsg, sprintscope(
							   &ei_cl.cl_scope),
					-1);
		  }
		  if (ei_cl.cl_filter != NULLMFILTER) {
		      Tcl_DStringAppend(&errorMsg, " filter ", -1);
		      Tcl_DStringAppend(&errorMsg, sprintmfilter(
							     ei_cl.cl_filter),
					-1);
		  }
		  Tcl_DStringAppend(&errorMsg, " sync ", -1);
		  Tcl_DStringAppend(&errorMsg, sprintsync(ei_cl.cl_sync), -1);
	      }
	      break;
	  }
	  case m_classInstanceConflict: {
	      if (&mi->mi_error.info) {
		  CMISClassInstanceConflict ei_cic;

		  id = mi->mi_error.id;
		  ei_cic = mi->mi_error.info.ei_classInstanceConflict;

		  Tcl_DStringAppend(&errorMsg, ": class ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ei_cic.cic_class.mid_global,
							OIDPART),
				    -1);
		  Tcl_DStringAppend(&errorMsg, ", instance ", -1);
		  Tcl_DStringAppend(&errorMsg, dn2str(ei_cic.cic_inst.mn_dn),
				    -1);
	      }
	      break;
	  }
	  default:
	    {
		char	buf[10];
		sprintf (buf, "\"%d\"", mi->mi_errtype);
		Tcl_DStringAppend(&errorMsg, ": unknown error type ", -1);
		Tcl_DStringAppend(&errorMsg, buf, -1);
	    }
	}
	break;

    }
    case MI_UREJECT: {
	id = mi->mi_ureject.mur_id;
	error = mi->mi_errtype;
	Tcl_DStringAppend(&errorMsg, MErrString(error), -1);
	Tcl_DStringAppend(&errorMsg, ": ", -1);
	Tcl_DStringAppend(&errorMsg, mi->mi_ureject.mur_data, -1);
/*
	Tcl_AppendResult(interp, "Error: ", MErrString(mi->mi_errtype),
			 (char *)NULL);
*/
	status = TCL_ERROR;
	break;
    }
    case MI_LINKED_REPLY: {	/* more than one object in remote MIB */
	id = mi->mi_linkedreplyinv.args.lr_linkid;

	switch (mi -> mi_linkedreplyinv.args.lr_type) {

	case LR_GET_LISTERR:
	    error = m_getListError;
	    Tcl_DStringAppend(&errorMsg, MErrString(error), -1);
	case LR_GET_RES: {
	    CMISGetRes	*gr;

	    gr = &mi->mi_linkedreplyinv.args.lr_getres;

	    MOClass = &gr->gr_class;
	    MOInst  = &gr->gr_inst;
	    curtime =  gr->gr_time;
	    nattrs  = gr->gr_nattrs;
	    attrs   = (attrRes *) ckalloc(nattrs * sizeof(attrRes));
	    for (i=0; i<nattrs; i++) {
		attrs[i].modify = m_noModifyOp; /* cause its a GET-response */
		attrs[i].attr  = gr->gr_attrs[i].ga_ava;
		attrs[i].error = gr->gr_attrs[i].ga_error;
	    }
	    status = TCL_CONTINUE;
	    break;
	}
	case LR_SET_LISTERR:
	    error = m_setListError;
	    Tcl_DStringAppend(&errorMsg, MErrString(error), -1);
	case LR_SET_RES: {
	    CMISSetRes	*sr;

	    sr = &mi->mi_linkedreplyinv.args.lr_setres;

	    MOClass = &sr->sr_class;
	    MOInst  = &sr->sr_inst;
	    curtime =  sr->sr_time;
	    nattrs  =  sr->sr_nattrs;
	    attrs   = (attrRes *) ckalloc(nattrs * sizeof(attrRes));
	    for (i=0; i<nattrs; i++) {
		attrs[i].modify = sr->sr_attrs[i].sa_modify;
		attrs[i].attr   = sr->sr_attrs[i].sa_ava;
		attrs[i].error  = sr->sr_attrs[i].sa_error;
	    }
	    status = TCL_CONTINUE;
	    break;
	}
	case LR_ACTION_RES: {
	    CMISActionRes	*ar;

	    ar = &mi->mi_linkedreplyinv.args.lr_actionres;

	    MOClass = &ar->ar_class;
	    MOInst  = &ar->ar_inst;
	    curtime =  ar->ar_time;
	    nattrs  =  1;
	    attrs   = (attrRes *) ckalloc(sizeof(attrRes));
	    attrs[1].modify = m_noModifyOp; /* cause its an ACTION-response */
	    attrs[1].attr   = ar->ar_reply;
	    attrs[1].error  = m_noError;

	    error = mi->mi_errtype;
	    status = TCL_CONTINUE;
	    break;
	}
	case LR_ACTION_ERR: {
	    CMISActionError	*ae;

	    ae = &mi->mi_linkedreplyinv.args.lr_actionerr;

	    MOClass = &ae->ae_class;
	    MOInst  = &ae->ae_inst;
	    curtime =  ae->ae_time;
	    error   =  ae->ae_error;
	    Tcl_DStringAppend(&errorMsg, MErrString(error), -1);

	    /* see CMISActionError in mparm.h */
	    switch (error) {
	      case m_accessDenied: {
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ae->ae_accessDenied.mid_global,
							OIDPART),
				    -1);
		  break;
	      }
	      case m_noSuchAction: {
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, oid2name(
						ae->ae_noSuchAction.mid_global,
							OIDPART),
				    -1);
		  break;
	      }
	      case m_noSuchArgument: {
		  Tcl_DStringAppend(&errorMsg, ": class ", -1);
		  Tcl_DStringAppend(&errorMsg,
			 oid2name(ae->ae_noSuchArgument.nsa_class.mid_global,
				  OIDPART), -1);
		  Tcl_DStringAppend(&errorMsg, ", type ", -1);
		  Tcl_DStringAppend(&errorMsg,
			 oid2name(ae->ae_noSuchArgument.nsa_type.mid_global,
				  OIDPART), -1);
		  /* nsa_operation ??? */
		  break;
	      }
	      case m_invalidArgumentValue: {
		  char *argid, *argval;

		  ava2str(ae->ae_invalidArgumentValue.iav_id.mid_global,
			  ae->ae_invalidArgumentValue.iav_val,
			  &argid, &argval);
		  Tcl_DStringAppend(&errorMsg, ": ", -1);
		  Tcl_DStringAppend(&errorMsg, argid, -1);
		  Tcl_DStringAppend(&errorMsg, " ", -1);
		  Tcl_DStringAppend(&errorMsg, argval, -1);
		  break;
	      }
	      default:
		{
		    char buf[10];
		    sprintf (buf, "\"%d\"", mi->mi_errtype);
		    Tcl_DStringAppend(&errorMsg, ": unknown error type ", -1);
		    Tcl_DStringAppend(&errorMsg, buf, -1);
		}
	    }
	    status = TCL_CONTINUE;
	    break;
	}
	case LR_DELETE_RES: {
	    CMISDeleteRes	*dr;

	    dr = &mi->mi_linkedreplyinv.args.lr_deleteres;

	    MOClass = &dr->dr_class;
	    MOInst  = &dr->dr_inst;
	    curtime =  dr->dr_time;

	    error = mi->mi_errtype;
	    status = TCL_CONTINUE;
	    break;
	}
	case LR_DELETE_ERR: {
	    CMISDeleteError	*de;

	    de = &mi->mi_linkedreplyinv.args.lr_deleteerr;

	    MOClass = &de->de_class;
	    MOInst  = &de->de_inst;
	    curtime =  de->de_time;
	    error   =  de->de_error;
	    Tcl_DStringAppend(&errorMsg, MErrString(error), -1);

	    status = TCL_CONTINUE;
	    break;
	}
	case LR_PROC_FAILURE: {
	    /* should not really happen hoffentlich !? */
	    CMISProcessingFailure	*pf;

	    pf = &mi->mi_linkedreplyinv.args.lr_procfailure;

	    MOClass = &pf->pf_class;
	    MOInst  = &pf->pf_inst;
	    /* es gibt noch ein &pf->pf_error vom typ CMISParam: */
	    nattrs  = 1;
	    attrs[1].modify = m_noModifyOp;
	    attrs[1].attr   = pf->pf_error;
	    attrs[1].error  = m_processingFailure; /* ??? */
/* kann nach auftreten eines processingFailure denn noch etwas folgen ??? */
	    error   = mi->mi_errtype;
	    Tcl_DStringAppend(&errorMsg, MErrString(error), -1);
	    
	    status = TCL_CONTINUE; /* endlosschleife moeglich ? */
	    break;
	}
	default: {
	    char	buf[50];
	    sprintf(buf, "Unexpected linked reply type: %d",
		    mi->mi_linkedreplyinv.args.lr_type);
	    Tcl_AppendResult(interp, buf, (char*)NULL);
	    return TCL_ERROR;
	}
	} /* switch (mi -> mi_linkedreply...) */

	break;
    }
    case MI_CANCEL_GET: /* gibts das ueberhaupt, wenn ja was kommt an? */
	status = MI_CANCEL_GET; /* 22 */
      break;

    case MI_EMPTY_RES: /* e.g. last response to a linked reply */
	id = mi->mi_emptyres.id;
	status = MI_EMPTY_RES; /* 23 */
	break;

    case MI_PREJECT: {
	id = mi->mi_preject.mpr_id;
	error = mi->mi_errtype;
	Tcl_DStringAppend(&errorMsg, MErrString(error), -1);
	Tcl_DStringAppend(&errorMsg, ": ", -1);
	Tcl_DStringAppend(&errorMsg, mi->mi_preject.mpr_data, -1);
	status = TCL_ERROR;
#ifdef DEBUG_CMIP
fprintf(stderr, "M_WaitReq(): received provider reject, exits");
#endif
	break;
    }
    case MI_ABORT: { /* what about MI_TERM ??? */
	/* sorry no id */
#ifdef DEBUG_CMIP
fprintf(stderr, "M_WaitReq(): received association abort, exits");
#endif
	error = mi->mi_errtype;
	Tcl_DStringAppend(&errorMsg, MErrString(error), -1);
	Tcl_DStringAppend(&errorMsg, ": ", -1);
	Tcl_DStringAppend(&errorMsg, mi->mi_abort.ma_data, -1);
	Tcl_DStringResult(interp, &errorMsg);
	return TCL_ERROR;
    }
    default: {
	char	buf[50];
	sprintf(buf, "Unexpected mindication type: %d", mi->mi_type);
	Tcl_AppendResult(interp, buf, (char*)NULL);
	return TCL_ERROR;
    }
    } /* switch (mi->mi_type) */

    if (rh && id != rh->request_id) { /* asynchronous */
	dochnichtsynchron = TRUE;
	rh = (CMIP_Request *) NULL;
    }
    if (!rh) { /* asynchronous */
	for (ht_entry = Tcl_FirstHashEntry(cmiph->req_table, &search);
	     ht_entry != NULL;
	     ht_entry = Tcl_NextHashEntry(&search)) {
	    rh = (CMIP_Request *) Tcl_GetHashValue(ht_entry);
	    if (rh->request_id == id) break;
	}
    }

    if (id != rh->request_id) {
	/* should not happen for a correctly behaving agent */
	Tcl_AppendResult(interp, rh->name, " result with a wrong",
			 " request id received", (char *)NULL);
	return TCL_ERROR;
    }

    if (status == MI_EMPTY_RES || status == MI_CANCEL_GET) {
	status = TCL_OK;
    } else if (status != TCL_ERROR) {
	/* compose the result */
	Tcl_DStringStartSublist(&rh->dStrResult);
	if (MOClass && MOClass->mid_global) {
	    Tcl_DStringAppendElement(&rh->dStrResult,
				     oid2name(MOClass->mid_global, OIDPART));
	} else {
	    Tcl_DStringAppendElement(&rh->dStrResult, "");
	}
	if (MOInst && MOInst->mn_dn) {
	    Tcl_DStringAppendElement(&rh->dStrResult,
						 dn2str(MOInst->mn_dn));
	} else {
	    Tcl_DStringAppendElement(&rh->dStrResult, "");
	}
	if (curtime) {
	    Tcl_DStringAppendElement(&rh->dStrResult, curtime);
	} else {
	    Tcl_DStringAppendElement(&rh->dStrResult, "");
	}
	if (error) { /* m_noError == 0 */
	    Tcl_DStringAppendElement(&rh->dStrResult, 
						 MErrString(error));
	} else {
	    Tcl_DStringAppendElement(&rh->dStrResult, "");
	}
	
	Tcl_DStringStartSublist(&rh->dStrResult);
	for (i = 0; i < nattrs; i++) {
	    if (!attrs[i].error) { /* m_noError == 0 */
		char *attrid, *attrval;
		ava2str(attrs[i].attr.mp_id.mid_global,
			attrs[i].attr.mp_val,
			&attrid, &attrval);
		Tcl_DStringStartSublist(&rh->dStrResult);
		Tcl_DStringAppendElement(&rh->dStrResult,
						     attrid);
		Tcl_DStringAppendElement(&rh->dStrResult,
						     attrval);
		Tcl_DStringAppendElement(&rh->dStrResult, "");
		Tcl_DStringEndSublist(&rh->dStrResult);
		free(attrid); free(attrval);
	    } else {
		/* error */
		Tcl_DStringStartSublist(&rh->dStrResult);
		Tcl_DStringAppendElement(&rh->dStrResult,
					 oid2name(
					     attrs[i].attr.mp_id.mid_global,
						  OIDPART));
		Tcl_DStringAppendElement(&rh->dStrResult, "");
		Tcl_DStringAppendElement(&rh->dStrResult,
						  MErrString(attrs[i].error));
		Tcl_DStringEndSublist(&rh->dStrResult);
	    }
	}
	Tcl_DStringEndSublist(&rh->dStrResult);
	Tcl_DStringEndSublist(&rh->dStrResult);
    }
    if (status != TCL_CONTINUE && rh->reqcallback) {
	/* status is TCL_CONTINUE in case of a linked reply */
	Tcl_DString	 dstr_eval;
	char		*startPtr, *scanPtr;
	char		 buf[50];

	Tcl_DStringInit(&dstr_eval);
	startPtr = rh->reqcallback;
	for (scanPtr = startPtr; *scanPtr != '\0'; scanPtr++) {
	    if (*scanPtr != '%') continue;
	    Tcl_DStringAppend(&dstr_eval, startPtr, scanPtr - startPtr);
	    scanPtr++;
	    startPtr = scanPtr + 1;
	    switch (*scanPtr) {
	    case 'R':  
		Tcl_DStringAppend(&dstr_eval, rh->name, -1);
		break;
	    case 'S':
		Tcl_DStringAppend(&dstr_eval, cmiph->name, -1);
		break;
	    case 'V':
		Tcl_DStringAppend(&dstr_eval,
				  Tcl_DStringValue(&rh->dStrResult), -1);
		break;
	    case 'E':
		if (status == TCL_ERROR) {
		    if (Tcl_DStringValue(&errorMsg) != '\0') {
			Tcl_DStringAppend(&dstr_eval, 
					  Tcl_DStringValue(&errorMsg), -1);
		    } else {
			Tcl_DStringAppend(&dstr_eval, MErrString(error), -1);
		    }
		} else {
		    Tcl_DStringAppend(&dstr_eval, "noError", -1);
		}
		break;
	    case 'I':
		sprintf(buf, "%d", error);
		Tcl_DStringAppend(&dstr_eval, buf, -1);
		break;
	    case '%':
		Tcl_DStringAppend(&dstr_eval, "%", -1);
		break;
	    default:
		sprintf(buf, "%%%c", *scanPtr);
		Tcl_DStringAppend(&dstr_eval, buf, -1);
	    }
	}
	Tcl_DStringAppend(&dstr_eval, startPtr, scanPtr - startPtr);
    
    /*-----------------------------------------------------------------------
      execute the callback function, supported by the application
      -----------------------------------------------------------------------*/
    
	status = Tcl_GlobalEval(interp, Tcl_DStringValue(&dstr_eval));

	if (status == TCL_ERROR) {
	    Tcl_AddErrorInfo (interp, "\n    (cmip callback)");
	    Tk_BackgroundError (interp);
	    status = TCL_OK;
	}

	/* free the request handle */
	Tcl_DStringFree(&dstr_eval);
	ht_entry = Tcl_FindHashEntry(cmiph->req_table, rh->name);
	if (ht_entry == NULL) {
	    Tcl_AppendResult(interp, "no such request handle \"", 
			      rh->name, "\"", (char *) NULL);
	    return TCL_ERROR;
	}
	Tcl_DeleteHashEntry(ht_entry);
	CMIP_FreeRequest(rh);
    }
    if (status == TCL_ERROR && !rh->reqcallback) {
	if (Tcl_DStringValue(&errorMsg) != '\0') {
	    Tcl_DStringResult(interp, &errorMsg);
	} else {
	    Tcl_AppendResult(interp, MErrString(error), (char *) NULL);
	}
    }

    if (attrs) ckfree((char *) attrs);
    mi_free(mi);
    Tcl_DStringFree(&errorMsg);

    if (dochnichtsynchron) return TCL_CONTINUE;
    return status;
} /* wait() */
