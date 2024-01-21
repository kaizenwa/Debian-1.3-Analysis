/*
Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: iluerrs.h,v 1.61 1996/07/12 20:04:20 janssen Exp $ */
/* Last edited by Mike Spreitzer April 2, 1996 10:37 pm PST */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _ILUERRS_H_
#define _ILUERRS_H_

ILU_DECL_PARMLESS_ERR(success);
/* A non-error that reserves a code for successful completion. */


/*
 * First, the errors that can appear at the kernel interface.  Each
 * of these has a defined mapping into a CORBA 1.2 standard
 * exception; that mapping is documented in these declarations,
 * except where the mapping is to the exception of the same name.
 * Some, which are noted as such, aren't actually used --- but
 * appear in this list so that a prefix of this list corresponds in
 * number and position to the published list for CORBA.
 */

ILU_DECL_PARMLESS_ERR(unknown);	/* not used */

typedef enum {
  ilu_bpm_duh,			/* It should be pretty obvious */
  ilu_bpm_true,			/* Attempting RPC on true server */
  ilu_bpm_asynch_unreliable,	/* asynch method call on unreliable
				 * transport */
  ilu_bpm_late,			/* called ilu_SetFoo too late */
  ilu_bpm_not_exported,		/* asked to output object of server
				 * with no ports */
  ilu_bpm_tinfo,		/* invalid transport info string */
  ilu_bpm_pinfo,		/* invalid protocol info string */
  ilu_bpm_typeID,		/* invalid type ID */
  ilu_bpm_OID,			/* invalid object ID */
  ilu_bpm_SBH,			/* bad SBH */
  ilu_bpm_URL,			/* invalid URL */
  ilu_bpm_serverId,		/* bad server ID string */
  ilu_bpm_networkAddr,		/* bad network address or host name */
  ilu_bpm_badPointer,		/* attempt to address memory not in
				 * the process address space */
  ilu_bpm_fd,			/* bad file descriptor */
  ilu_bpm_sequenceLimit,	/* sequence too long for its limit */
  ilu_bpm_unionDiscSize,	/* invalid discriminant size */
  ilu_bpm_nil,			/* NIL passed where not allowed */
  ilu_bpm_closed,		/* op invoked on closed something */
  ilu_bpm_small_buffer,		/* callee needs larger buffer */
  ilu_bpm_string_null_char,	/* octet 0 in `string' parm */
  ilu_bpm_threading,
  /*
   * A proc appropriate only for single-threaded runtimes was called
   * in a mult-threaded runtime --- or the other way around.
   */
  ilu_bpm_protocol_registered,
  /* attempt to register already-registered protocol */
  ilu_bpm_transport_registered,
  /* attempt to register already-registered transport */
  ilu_bpm_identity_type_registered,
  /* attempting to register already-registered identity type */

  ilu_bpm_bogus_raise,		/* Method tried to raise an
				 * exception not in its RAISES list. */
  ilu_bpm_some_raise		/* Method with empty exn list tried
				 * to raise an exn. */
}               ilu_bad_param_Minor;

ILU_DECL_ERR(bad_param)
{
  ilu_bad_param_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_ERR(no_memory)
{
  ilu_cardinal    nbytes;	/* size of allocation that failed; 0
				 * if not known */
} ILU_END_DECL_ERR;

typedef enum {
  ilu_ilm_strlen,
  /*
   * ILU will marshal only strings & byte-sequences less than a
   * certain length.
   */
  ilu_ilm_nomst,
  /*
   * When importing a surrogate, ILU requires that the importing
   * program know some of the object's types, and that one of those
   * known types is a subtype of all the other known types.
   */
  ilu_ilm_max_protocols,	/* too many protocols registered */
  ilu_ilm_max_transports,	/* too many transports registered */
  ilu_ilm_max_identity_types	/* too many identity types registered */

}               ilu_imp_limit_Minor;

ILU_DECL_ERR(imp_limit)
{
  ilu_imp_limit_Minor minor;
} ILU_END_DECL_ERR;

typedef enum {
  /* First, failures to establish connection: */

  ilu_cfm_socket_type,		/* OS doesn't support sock type or
				 * protocol */
  ilu_cfm_bad_address,		/* local or remote addr not
				 * available */
  ilu_cfm_connect_refused,	/* remote end refused connection */
  ilu_cfm_timeout,		/* timeout */
  ilu_cfm_nonblock,		/* can't achieve non-blocking I/O */
  ilu_cfm_connect_failed,	/* some other, or unknown, reason */

  /* Second, failures of established connections: */

  ilu_cfm_eof,			/* unexpected eof on connection */
  ilu_cfm_protocol_sync_lost,	/* unexpected bytes with no way to recover */
  ilu_cfm_conn_lost		/* other or unknown reason */
}               ilu_comm_failure_Minor;

ILU_DECL_ERR(comm_failure)
{
  ilu_comm_failure_Minor minor;
} ILU_END_DECL_ERR;

typedef enum {
  ilu_iom_sbh,			/* malformed SBH */
  ilu_iom_ci,			/* malformed contact info */
  ilu_iom_pi,			/* malformed protocol info */
  ilu_iom_pc,			/* unknown protocol class */
  ilu_iom_ps,			/* protocol-specific part invalid */
  ilu_iom_ti,			/* malformed transport info */
  ilu_iom_tc,			/* unknown transport class */
  ilu_iom_ts,			/* transport-specific part invalid */
  ilu_iom_sid,			/* malformed server ID */
  ilu_iom_ih,			/* malformed instance handle */
  ilu_iom_mstid,		/* malformed MSTID */
  ilu_iom_nil,			/* NIL object found in invalid context */
  ilu_iom_bad_url_scheme,	/* invalid scheme tag */
  ilu_iom_tf,
  /* Transport class used in inappropriately filterly position. */
  ilu_iom_ior,			/* invalid IIOP IOR */
  ilu_iom_svr_closed,		/* server closed */
  ilu_iom_inst_nf,		/* instance doesn't exist */
  ilu_iom_wrong_type		/* instance doesn't have right type */

}               ilu_inv_objref_Minor;

ILU_DECL_ERR(inv_objref)
{
  ilu_inv_objref_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(no_permission);

typedef enum {
  ilu_im_inv_mutex,		/* some mutex was deemed "invalid" */
  ilu_im_broken,		/* kernel data str broken --- NIL
				 * where shouldn't be */
  ilu_im_unhandled,		/* Unexpected ilu_Error type raised
				 * in MOP */
  ilu_im_errno,			/* syscall raised unexpected errno */
  ilu_im_trBufSize,		/* tc_get_output_buffer couldn't */
  ilu_im_tInfoLen,		/* tinfo too long */
  ilu_im_callFail,		/* an internal call failed */
  ilu_im_badLocks,		/* bad_locks maps to this case */
  ilu_im_brokenLocks,		/* broken_locks maps to this case */
  ilu_im_inputBuffer,		/* Input buffer expected but not found */
  ilu_im_outputBuffer,		/* Output buffer expected but not found */
  ilu_im_endMessage,		/* tc_end_message when no msg active */
  ilu_im_beginMessage,		/* tc_begin_message when msg active */
  ilu_im_bytesWithoutMsg,	/* byte I/O outside message boundaries */
  ilu_im_tcBytesDropped,	/* for tc_end_message */
  ilu_im_tcBug,			/* TransportClass didn't meet contract */
  ilu_im_tcInputSkipsBuff,	/* a particular Trans'Class caller bug */
  ilu_im_tcNotBoundaried,	/* for b'd method of unb'd Trans'Class */
  ilu_im_tcReliable,		/* for unreliable call on reliable TC */
  ilu_im_tcBadBuff,		/* bad buffer given to Trans'Class proc */
  ilu_im_tcNoMsgHandle,		/* for tc_end_message */
  ilu_im_noHostName,		/* unable to get a name for this host */
  ilu_im_noHostIpAddr,		/* unable to get IP addr for this host */
  ilu_im_bufxpMisuse,		/* bufxp caller violated contract */
  ilu_im_typeMismatch,		/* two stubs with different ideas */
  ilu_im_typeIncomplete,	/* type not completely constructed */
  ilu_im_threading,		/* confusion on whether threaded */
  ilu_im_threadFork,		/* fork failed */
  ilu_im_tportRole,		/* incoming vs. outgoing transport err */
  ilu_im_check			/* internal consistency check failed */
}               ilu_internal_Minor;

ILU_DECL_ERR(internal)
{
  ilu_internal_Minor minor;
} ILU_END_DECL_ERR;

typedef enum {
  ilu_mm_eom,			/* attempted read past end of msg */
  ilu_mm_alien_disc,		/* unmarshalling discriminator of
				 * different server */
  ilu_mm_wronglen,		/* fixed length array came in with
				 * different length */
  ilu_mm_sequenceLimit,		/* attempt to read or write a
				 * sequence longer than its limit */
  ilu_mm_badMagicNumber,	/* bad message header magic number */
  ilu_mm_versionMismatch,	/* wrong version of message protocol */
  ilu_mm_badInteger,		/* signed or unsigned integer that
				 * doesn't fit position */
  ilu_mm_badFloat,		/* floating point value that doesn't
				 * fit position */
  ilu_mm_dgramLimit,		/* datagram (eg, UDP) size limit
				 * exceeded */
  ilu_mm_msgTypeUnknown,	/* invalid message type received */
  ilu_mm_utf2Len		/* UTF2 encoded string's length
				 * doesn't correspond to plain
				 * string length */
}               ilu_marshal_Minor;

ILU_DECL_ERR(marshal)
{
  ilu_marshal_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(initialize);	/* not used */
ILU_DECL_PARMLESS_ERR(no_implement);

typedef enum {
  ilu_btm_unknownType			/* reference to unknown (by this addr space) type */
}		ilu_bad_typecode_Minor;

ILU_DECL_ERR(bad_typecode)
{
  ilu_bad_typecode_Minor minor;
} ILU_END_DECL_ERR;

typedef enum {
  ilu_bom_noSuchOperationOnType		/* specified operation not defined on specified type */
}		ilu_bad_operation_Minor;

ILU_DECL_ERR(bad_operation)
{
  ilu_bad_operation_Minor minor;
} ILU_END_DECL_ERR;

typedef enum {
  ilu_nrm_EMFILE,		/* per-process descriptor table full */
  ilu_nrm_ENFILE,		/* system file table full */
  ilu_nrm_ENOBUFS,		/* insufficient buffer space avail */
  ilu_nrm_fds,			/* FD budget */
  ilu_nrm_mlreg			/* main loop registrations */
}               ilu_no_resources_Minor;

ILU_DECL_ERR(no_resources)
{
  ilu_no_resources_Minor minor;
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(no_response);	/* not used */
ILU_DECL_PARMLESS_ERR(persist_store);	/* not used */
ILU_DECL_PARMLESS_ERR(bad_inv_order);	/* not used */
ILU_DECL_PARMLESS_ERR(transient);	/* not used */
ILU_DECL_PARMLESS_ERR(free_mem);	/* not used */
ILU_DECL_PARMLESS_ERR(inv_ident);	/* not used */
ILU_DECL_PARMLESS_ERR(inv_flag);	/* not used */
ILU_DECL_PARMLESS_ERR(intf_repos);	/* not used */
ILU_DECL_PARMLESS_ERR(bad_context);	/* not used */
ILU_DECL_PARMLESS_ERR(obj_adapter);	/* not used */
ILU_DECL_PARMLESS_ERR(data_conversion);	/* not used */

/* This is the end of the prefix that corresponds to CORBA's list. */

ILU_DECL_PARMLESS_ERR(bad_locks);
/*
 * Raised when the calling thread is detected to have violated the
 * locking precondition.  => CORBA::INTERNAL (cf discussion w David
 * Brownell)
 */

ILU_DECL_PARMLESS_ERR(broken_locks);
/*
 * This is raised when the implementation of mutexes & condition
 * variables either (a) refuses to operate on the mutexes and
 * condition variables of the kernel or (b) raises an error that the
 * kernel knows it shouldn't.  When broken_locks is raised, we can't
 * even establish the locking postcondition of the raising
 * procedure.  => CORBA::INTERNAL
 */

ILU_DECL_ERR(interrupted)
{
  unsigned short  ilu_interruptSet;
  /*
   * A threaded LS runtime can set this to indicate which of several
   * possible reasons for an interrupt are active.
   */
} ILU_END_DECL_ERR;

typedef enum {
  ilu_gsm_GSS_S_BAD_BINDINGS,		/* Channel Binding Mismatch */
  ilu_gsm_GSS_S_BAD_MECH,		/* Unsupported Mechanism Requested */
  ilu_gsm_GSS_S_BAD_NAME,		/* Invalid Name Provided */
  ilu_gsm_GSS_S_BAD_NAMETYPE,		/* Name Of Unsupported Type Provided */
  ilu_gsm_GSS_S_BAD_STATUS,		/* Invalid Input Status Selector */
  ilu_gsm_GSS_S_BAD_SIG,		/* Token Had Invalid Signature */
  ilu_gsm_GSS_S_CONTEXT_EXPIRED,	/* Specified Security Context Expired */
  ilu_gsm_GSS_S_CREDENTIALS_EXPIRED,	/* Expired Credentials Detected */
  ilu_gsm_GSS_S_DEFECTIVE_CREDENTIAL,	/* Defective Credential Detected */
  ilu_gsm_GSS_S_DEFECTIVE_TOKEN,	/* Defective Token Detected */
  ilu_gsm_GSS_S_FAILURE,		/* Failure, Unspecified At GSS-API Level */
  ilu_gsm_GSS_S_NO_CONTEXT,		/* No Valid Security Context Specified */
  ilu_gsm_GSS_S_NO_CRED,		/* No Valid Credentials Provided */
  ilu_gsm_GSS_S_BAD_QOP,		/* Unsupported QOP Value */
  ilu_gsm_GSS_S_UNAUTHORIZED,		/* Operation Unauthorized */
  ilu_gsm_GSS_S_UNAVAILABLE,		/* Operation Unavailable */
  ilu_gsm_GSS_S_CONTINUE_NEEDED,	/* Continuation Call To Routine Required */
  ilu_gsm_GSS_S_DUPLICATE_TOKEN,	/* Duplicate Per-Message Token Detected */
  ilu_gsm_GSS_S_OLD_TOKEN,		/* Timed-Out Per-Message Token Detected */
  ilu_gsm_GSS_S_UNSEQ_TOKEN,		/* Reordered (Early) Per-Message Token Detected */
  ilu_gsm_GSS_S_GAP_TOKEN		/* Skipped Predecessor Token(S) Detected */
  } ilu_gss_security_Major;

ILU_DECL_ERR(gss_security)
{
  ilu_gss_security_Major	major;
  /* The GSS major code for the error.  See the GSS spec for mappings. */

  ilu_cardinal			minor;
  /* The GSS minor code for the error. */

} ILU_END_DECL_ERR;

/* Following are the internal errors of the new draft. */




/*
 * Following are the internal errors of the old draft; they're being
 * eliminated or promoted, as appropriate.
 */


/* Some standard errors for use throughout the kernel */

/*
 * Used internally in the kernel to report that some invariant
 * doesn't hold.  That is, a bug has been detected in the kernel.
 * Used only for invariants that don't require cooperation from the
 * app or LS runtime.  When raised, the called procedure has done
 * an unspecified part of its job.  BadDataStructure is a special
 * case, and is used instead where it applies.
 */
ILU_DECL_ERR(KernelBroken)
{
  char           *what;		/* constant string that alludes to
				 * the invariant that's broken */
} ILU_END_DECL_ERR;

/*
 * Used internally in the kernel to report that something in the
 * kernel is "broken" in some structural way (e.g., a pointer is
 * NULL that shouldn't be).  Called proc has done an unspecified
 * part of its job. This is a special case of KernelBroken.
 */
ILU_DECL_ERR(BadDataStructure)
{
  void           *data_str_ptr;
  char           *supposed_type;/* pointer to constant string,
				 * never freed */
  char           *why_bad;	/* pointer to constant string,
				 * never freed */
} ILU_END_DECL_ERR;

#define BIND_BadDataStructure(e,p,st,wb) \
ILU_BIND_ERR(BadDataStructure, e, ev) \
      { \
	  ev->data_str_ptr = p; \
	  ev->supposed_type = st; \
	  ev->why_bad = wb; \
      } \
ILU_END_BIND_ERR;

/*
 * Used when ilu_malloc() returns a NIL pointer.
 */
ILU_DECL_ERR(MallocFailure)
{
  ilu_cardinal	nbytes;		/* number of bytes attempted to allocate */
} ILU_END_DECL_ERR;

/* Raised when there is some reason to believe that a kernel
   object is broken.  The "reason" is some explanation as to
   why that belief is held. */

ILU_DECL_ERR(MalformedKernelObject) {
  ilu_Object obj;
  ilu_string reason;
} ILU_END_DECL_ERR;

/* Raised when a fixed-length array in the impl overflows. */

ILU_DECL_ERR(MaxCountExceeded) {
  int max_count;
}
ILU_END_DECL_ERR;

/* Signalled by ilu_RegisterProtocol */

ILU_DECL_ERR(ProtocolAlreadyRegistered) {
  char * name;
  ilu_Protocol (*old_protocol) (void);
  ilu_Protocol (*new_protocol) (void);
}
ILU_END_DECL_ERR;

/* Signalled by ilu_RegisterTransport */

ILU_DECL_ERR(TransportAlreadyRegistered)
{
  char				*name;
  ilu_TransportCreator (*old_transport) (ilu_TransportInfo, ilu_Error *);
  ilu_TransportCreator (*new_transport) (ilu_TransportInfo, ilu_Error *);
}
ILU_END_DECL_ERR;

/*
 * Raised when the caller violates a locking precondition of a
 * procedure.  /which/ identifies a mutex relevent to the violated
 * constraint.  Signalled by lots of stuff.  When raised, nothing
 * else has been done, unless otherwise noted at the procedure in
 * question.
 */

typedef enum {
  ilu_lock_smu, ilu_lock_otmu, ilu_lock_cmu, ilu_lock_prmu,
  ilu_lock_trmu, ilu_lock_gcmu, ilu_lock_timu, ilu_lock_server,
  ilu_lock_io, ilu_lock_call
}               ilu_lock;

ILU_DECL_ERR(WrongLocks)
{
  ilu_lock        which;
} ILU_END_DECL_ERR;

ILU_DECL_ERR(BadProtocolInfo)
{
  ilu_string      x;		/* owned by error */
} ILU_END_DECL_ERR;

ILU_DECL_ERR(BadArguments)
{
  ilu_string      function_name;/* global -- not to be freed */
  ilu_string      reason;	/* explanation of what was wrong;
				 * owned by err */
} ILU_END_DECL_ERR;

ILU_DECL_ERR(GcRegFailed)
{
  ilu_string      why;		/* global -- not to be freed */
} ILU_END_DECL_ERR;



/* used when marshalling or unmarshalling call is made upon a
   connectionless "call" object */
ILU_DECL_PARMLESS_ERR(NoConnection);

ILU_DECL_ERR(ClosedServer) {
  char *oid;
  ilu_Server server;
} ILU_END_DECL_ERR;

ILU_DECL_ERR(ObjectNotFoundInServer) {
  char *oid;
  ilu_Server server;
} ILU_END_DECL_ERR;

ILU_DECL_ERR(NoObjectForSBH) {
  char           *sbh;		/* owned by error */
} ILU_END_DECL_ERR;

ILU_DECL_PARMLESS_ERR(CantCondition);

#define IoErrs bad_param, imp_limit, marshal, comm_failure, no_memory, internal, broken_locks, interrupted
/* Errors that might be rasied when doing I/O. */

/* And now we drop the shoe you've all been waiting for! */

#define ILU_ERRLIST 			\
					\
    ILU_ERRLISTELT(success)		\
    					\
    /* public types */			\
    ILU_ERRLISTELT(unknown)		\
    ILU_ERRLISTELT(bad_param)		\
    ILU_ERRLISTELT(no_memory)		\
    ILU_ERRLISTELT(imp_limit)		\
    ILU_ERRLISTELT(comm_failure)	\
    ILU_ERRLISTELT(inv_objref)		\
    ILU_ERRLISTELT(no_permission)	\
    ILU_ERRLISTELT(internal)		\
    ILU_ERRLISTELT(marshal)		\
    ILU_ERRLISTELT(initialize)		\
    ILU_ERRLISTELT(no_implement)	\
    ILU_ERRLISTELT(bad_typecode)	\
    ILU_ERRLISTELT(bad_operation)	\
    ILU_ERRLISTELT(no_resources)	\
    ILU_ERRLISTELT(no_response)		\
    ILU_ERRLISTELT(persist_store)	\
    ILU_ERRLISTELT(bad_inv_order)	\
    ILU_ERRLISTELT(transient)		\
    ILU_ERRLISTELT(free_mem)		\
    ILU_ERRLISTELT(inv_ident)		\
    ILU_ERRLISTELT(inv_flag)		\
    ILU_ERRLISTELT(intf_repos)		\
    ILU_ERRLISTELT(bad_context)		\
    ILU_ERRLISTELT(obj_adapter)		\
    ILU_ERRLISTELT(data_conversion)	\
    ILU_ERRLISTELT(bad_locks)		\
    ILU_ERRLISTELT(broken_locks)	\
    ILU_ERRLISTELT(interrupted)		\
    ILU_ERRLISTELT(gss_security)	\
    					\
    /* approved private types (not to cross kernel interface) */	\
    					\
    /* unresolved private types (not to cross kernel interface) */	\
    ILU_ERRLISTELT(KernelBroken)	\
    ILU_ERRLISTELT(BadDataStructure)	\
    ILU_ERRLISTELT(MallocFailure)	\
    ILU_ERRLISTELT(MalformedKernelObject)	\
    ILU_ERRLISTELT(MaxCountExceeded)	\
    ILU_ERRLISTELT(ProtocolAlreadyRegistered)	\
    ILU_ERRLISTELT(TransportAlreadyRegistered)	\
    ILU_ERRLISTELT(WrongLocks)		\
    ILU_ERRLISTELT(BadProtocolInfo)	\
    ILU_ERRLISTELT(BadArguments)	\
    ILU_ERRLISTELT(GcRegFailed)		\
    ILU_ERRLISTELT(NoConnection)	\
    ILU_ERRLISTELT(ClosedServer)	\
    ILU_ERRLISTELT(ObjectNotFoundInServer)	\
    ILU_ERRLISTELT(NoObjectForSBH)	\
    ILU_ERRLISTELT(CantCondition)

/* Define the enumeration of error types */

#define ILU_ERRLISTELT(id) ILU_ERRTYP(id),
typedef enum {ILU_ERRLIST ILU_ERRTYP(ErrListLen)} ilu_ErrorType;
#undef ILU_ERRLISTELT

/* Define the error struct */

#define ILU_ERRLISTELT(id) ILU_ERRMEM_T(id) ILU_ERRLBL(id);

struct ilu_Error_s {
  const char     *ilu_file;
  int             ilu_line;
  ilu_ErrorType   ilu_type;
  union {
  ILU_ERRLIST
  }               u;
};

#undef ILU_ERRLISTELT

ILU_PUBLIC ilu_Error ilu_success_err;

ILU_PUBLIC unsigned long
  ilu_CORBAizeSystemErr(ilu_Error * err,
		      ilu_integer * major);
/*
 * Call this to translate an ilu_Error from the kernel interface
 * into the terms of a CORBA system exception.  On success, sets
 * *major to the 0-based index into CORBA's list of standard
 * exceptions, and returns the minor code.  On failure, sets *major
 * to -1 and returns 0.  Calls ILU_HANDLED(*err).
 */

#endif /* ndef _ILUERRS_H_ */
#ifdef __cplusplus
}
#endif

