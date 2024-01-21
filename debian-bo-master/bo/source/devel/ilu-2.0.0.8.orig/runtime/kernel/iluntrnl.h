/*
Copyright (c) 1991-1996 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: iluntrnl.h,v 1.161 1996/07/15 06:41:32 janssen Exp $ */
/* Last edited by Mike Spreitzer June 27, 1996 4:08 pm PDT */

#ifndef _ILU_INTERNALS_
#define _ILU_INTERNALS_

#include <iluxport.h>

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <string.h>

#define NIL ILU_NIL
#define NULLFN 0
#define NULLCH 0

#define OPTIONAL(x)	x
#define PASS(x)		x
#define RETAIN(x)	x
#define GLOBAL(x)	x

/* ==================== internal type info ==================== */

typedef struct hashTable *ilu_HashTable;

#define MAX_CCPEER_LEN 64

#define MAX_LANGUAGES 10 /* An upper bound on the # of languages
			    found in the same address space. */
extern ilu_cardinal _ilu_NLanguages; /* # of languages registered. */

typedef enum ilu_PacketTypes {
  ilu_PacketType_Request = 0,
  ilu_PacketType_Reply = 1
} ilu_PacketType;

/*Main Invariant holds; L2 >= {call's conn's callmu}*/
extern void
  _ilu_AddConnIdentities (ilu_Call, ILU_ERRS((no_memory, bad_param)) *);
/* Adds the identities in conn->co_auth_info
   to the call->ca_caller passport.  Creates the passport if that field is NIL */

/* ----- utility functions for dealing with WString IO ----- */

/* L1, L2 unconstrained */
ilu_cardinal
  _ilu_SizeOfWString (ilu_Call call,
		      ilu_wstring s,
		      ilu_cardinal l1	/* size of wstring */,
		      ilu_cardinal limit,
		      ILU_ERRS((IoErrs)) *err);

/* Main holds, L2 >= {call's connection's callmu, iomu} */
void
  _ilu_OutputWString (ilu_Call call,
		      ilu_wstring s,
		      ilu_cardinal l1,
		      ilu_cardinal limit,
		      ILU_ERRS((IoErrs)) * err);

/* Main holds, L2 >= {call's connection's callmu, iomu} */
void
  _ilu_InputWString (ilu_Call call,
		     ilu_wstring * s,
		     ilu_cardinal * l,
		     ilu_cardinal limit,
		     ILU_ERRS((IoErrs)) * err);

/* L1, L2 unconstrained */
ilu_cardinal
  _ilu_SizeOfWStringVec (ilu_Call call,
			 ilu_wstring s,
			 ilu_cardinal l1	/* size of wstringvec */,
			 ILU_ERRS((IoErrs)) *err);

/* Main holds, L2 >= {call's connection's callmu, iomu} */
void
  _ilu_OutputWStringVec (ilu_Call call,
			 ilu_wstring s,
			 ilu_cardinal l1,
			 ILU_ERRS((IoErrs)) * err);

/* Main holds, L2 >= {call's connection's callmu, iomu} */
void
  _ilu_InputWStringVec (ilu_Call call,
			ilu_wstring * s,
			ilu_cardinal l,
			ILU_ERRS((IoErrs)) * err);

/* ----- utility functions for dealing with object IO ----- */

/*L2 >= {call's connection's callmu, iomu}.
  kobj == NIL => Main Invariant holds.
  kobj != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = kobj's server and cl = kobj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ilu_boolean 
_ilu_OutputObjectID(ilu_Call call, ilu_Object kobj,
		    ilu_boolean discriminator_p,
		    ilu_Class static_type,
		    ILU_ERRS((IoErrs)) * err);

/*kobj!=NIL => L1 >= {kobj's server}, L1_sup < prmu;
  kobj==NIL => L1 unconstrained*/
ilu_cardinal _ilu_SizeOfObjectID (ilu_Call call, ilu_Object kobj,
				  ilu_boolean discriminator_p,
				  ilu_Class static_type,
				  ILU_ERRS((IoErrs)) *err);

/*before: L1 = {};
  after:  *kobj!=NIL => Inside(*kobj's server, static_type);
  after:  *kobj==NIL => L1 = {};
  Main Remnant holds;
  L2 >= {call's connection's callmu, iomu}*/
ilu_boolean 
_ilu_InputObjectID(ilu_Call call, ilu_Object * kobj,
		   ilu_boolean discriminator_p,
		   ilu_Class static_type,
		   ILU_ERRS((IoErrs)) * err);

typedef enum {
  ilu_rhrc_ok,			/* Header successfully started */
  ilu_rhrc_eof,			/* Found EOF instead of header */
  ilu_rhrc_nothing,		/* No input available w/o blocking */
  ilu_rhrc_error,		/* An error is being raised */
  ilu_rhrc_handled		/* A protocol-specific request was handled */
}               ilu_ReadHeaderResultCode;

struct _ilu_Protocol_s {
  /* A protocol is never changed or freed once created, so these fields
   * are readonly.  The locking comments refer to invoc'ns of the methods.
   */

  ilu_boolean	pr_concurrent_requests;
  /*
   * Does this protocol support concurrent requests on one
   * connection?
   */

  ilu_boolean	pr_sizing_required;
  /*
   * TRUE if this protocol requires accurate argument sizes to be passed to
   * pr_start_request, pr_begin_reply, and pr_begin_exception.  When FALSE,
   * implies that the pr_size_of_* calls may be invalid or NIL.
   */

  /*L2, Main unconstrained*/
  /*L1_sup < prmu (prmu protects class<->prog&ver registry)*/
  ilu_string (*pr_form_handle)(ilu_Object obj);
  /* A function concerned with string binding handles */

  /*L1, L2, Main unconstrained*/

  ilu_refany (*pr_create_data_block)(ILU_ERRS((no_memory)) *err);

  void (*pr_free_data_block)(ilu_refany block);

  /*Main Invariant holds*/
  ilu_boolean(*pr_init_call) (ilu_Call call,
			      ILU_ERRS((IoErrs)) * err);
  /*
   * Called early in the life of incoming and outgoing calls, so
   * that ca_prdata1 and ca_prdata2 may be initialized; ca_server
   * and ca_connection are the only other fields that are certainly
   * initialized yet.  Next: sizing of arguments, or pr_read_header.
   */
  
  /*Main Invariant holds; L2 >= {call's conn's callmu, iomu}*/

  ilu_boolean(*pr_start_request) (ilu_Call call,
				  ilu_cardinal arg_size,
				  ILU_ERRS((IoErrs)) * err);
  /*
   * intro_type should be the class that originally defined the
   * method.  Can rely on call->ca_method and call->ca_intro_type
   * being already filled in.  Marshalling of arguments is next.
   */

  ilu_boolean(*pr_discard_output) (ilu_Call /* call */ ,
				   ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Call this any time between pr_start_request and
   * pr_finish_request, between pr_begin_reply and pr_finish_reply,
   * or between pr_begin_exception and pr_finish_exception, to abort
   * composing, and not send, the message being composed.  Caller
   * next calls pr_finish_call, pr_begin_sizing_exn, or
   * pr_begin_exception.
   */
  
  ilu_boolean(*pr_finish_request) (ilu_Call call,
				   ilu_Message * /* msg */ ,
				   ILU_ERRS((IoErrs)) * err);
  /*
   * End bracket of sending of the call parameters, including
   * discriminator.  If transport is reliable, msg is not
   * meaningful.  Otherwise, callee returns (including ownership) a
   * copy of the whole call message in *msg.  Next the caller waits
   * for input on the call's connection, then calls pr_read_header.
   */

  ilu_ReadHeaderResultCode(*pr_read_header) (ilu_Call call,
				       ilu_PacketType * packetType,
					   ilu_cardinal * packetSN,
					 ILU_ERRS((IoErrs)) * err);
  /*
   * Caller has reason to suspect that either EOF or a new message
   * is waiting on call's connection.  Check it out.  If a message
   * is found, begin interpreting it, setting *packetType and
   * *packetSN (if appropriate), and return ilu_rhrc_ok; caller will
   * then call pr_delay_interp, pr_interpret_reply,
   * pr_interpret_request, or pr_discard_input.  If EOF is found,
   * return ilu_rhrc_eof; caller will close the ilu_Connection and
   * proceed to pr_finish_call.  Note that finding EOF should not
   * be considered an error.  If it is impossible to tell whether
   * a message or EOF is next without blocking (i.e., caller was
   * wrong about something waiting), return ilu_rhrc_nothing; client
   * stub will go back to waiting for input, server stub will
   * proceed to pr_finish_call.  If a control message internal to
   * the protocol was received and handled, return ilu_rhrc_handled;
   * client stub will go back to waiting for an input, server stub
   * will proceed to pr_finish_call.  Return ilu_rhrc_error iff
   * raising an error; caller will proceed to pr_finish_call.
   */

  ilu_refany(*pr_delay_interp) (ilu_Call call,
				ILU_ERRS((IoErrs)) * err);
  /*
   * Meaningful only in concurrent protocol.  May be called after
   * pr_read_header, to save a copy of the current reply message and
   * interpreter state for use in a different call.  On success,
   * returns a thing that will later be passed to pr_resume_interp
   * (with a different call), and advances call's transport to just
   * after the end of this message.  On failure, returns NIL; caller
   * next calls pr_discard_input then pr_finish_call.
   */

  void            (*pr_resume_interp) (ilu_Call call, ilu_refany x);
  /*
   * Continue processing a reply started in the wrong call.
   * pr_interpret_reply will be called next.
   */
  
  ilu_boolean(*pr_abandon_delayed_interp) (ilu_refany x,
				       ILU_ERRS((internal)) * err);
  /*
   * Sorry, pr_resume_interp will never be called on x; free
   * associated resources (including x itself).
   */

  ilu_ProtocolException(*pr_interpret_reply) (ilu_Call call,
					      ilu_cardinal * exn_code,
					 ILU_ERRS((IoErrs)) * err);
  /*
   * After a reply packet with the right serial number and
   * type==ilu_PacketType_Reply has been found, call this to
   * continue decoding the reply msg.  The result is success or a
   * protocol-level error.  If a protocol error is being reported,
   * the caller next calls pr_discard_input then pr_finish_call, and
   * then passes the error to client code.  This procedure also
   * decodes whether the marshalled results are normal results or an
   * exception parameter; `*exn_code` gets 0 if success is being
   * reported, otherwise 1 + (index into method's exception vector).
   * Unmarshalling of results/exception parameter is next, followed
   * by pr_reply_read.
   */

  ilu_boolean(*pr_discard_input) (ilu_Call /* call */ ,
				ILU_ERRS((internal)) * /* err */ );
  /*
   * Abandon processing current input message.  Call any time
   * between pr_read_header/pr_resume_interp and
   * pr_reply_read/pr_delay_interp on client side, between
   * pr_read_header and pr_request_read on server side.  Caller next
   * calls pr_finish_call or pr_begin_sizing_exn.
   */
  
  void            (*pr_reply_read) (ilu_Call, ILU_ERRS((IoErrs)) *);
  /*
   * Called after arguments have been unmarshalled.  Cleans up after
   * use of call's connection's co_protocol_data slot.  Note that
   * call's connection may already be closed.  Caller next calls
   * pr_finish_call.
   */

  ilu_boolean(*pr_interpret_request) (ilu_Call /* call */,
				      ILU_ERRS((IoErrs)) * /* err */);
  /*
   * Server stub calls this after pr_read_header.  Fills in the
   * intro_type and method fields of call; serialNumber, server,
   * connection fields are already set.  Stores in the call's
   * connection's co_protocol_data slot some private data used for
   * unmarshalling arguments.  Fails, returning FALSE, when setting
   * either *err or call->ca_pe to a failure; caller next calls
   * pr_discard_input then pr_finish_call. If success, caller next
   * unmarshalls arguments.
   */

  void            (*pr_request_read) (ilu_Call, ILU_ERRS((IoErrs)) *);
  /*
   * Called after arguments have been unmarshalled.  Cleans up after
   * use of call's connection's co_protocol_data slot.  Note that
   * call's connection may already be closed.  Caller next executes
   * true method, then pr_begin_sizing_reply, pr_begin_sizing_exn,
   * pr_begin_exception, or pr_finish_call.
   */

  ilu_cardinal(*pr_begin_sizing_reply) (ilu_Call call,
					ilu_boolean exns_possible,
					ILU_ERRS((IoErrs)) * err);
  /*
   * Starts computation of reply_size parm of pr_begin_reply; add to
   * result of this proc the sizes of all the results.
   */

  ilu_boolean(*pr_begin_reply) (ilu_Call call,
				ilu_boolean exceptions_possible,
				ilu_cardinal reply_size,
				ILU_ERRS((IoErrs)) * err);

  ilu_boolean(*pr_finish_reply) (ilu_Call call,
				 ILU_ERRS((IoErrs)) * err);
  /*
   * pr_begin_reply and pr_finish_reply bracket the sending of the
   * results.  This proc calls _ilu_CacheCall if transport timesout.
   * Caller next calls pr_finish_call.
   */

  ilu_cardinal(*pr_begin_sizing_exn) (ilu_Call call,
				      ilu_cardinal eindex,
				      ilu_ProtocolException sys_ex_index,
				      ILU_ERRS((IoErrs)) * err);
  /*
   * Starts computation of reply_size parm of pr_begin_exception,
   * when either a user or system exception is being returned.  Add
   * to result of this proc the size the exn's parm, if any.  For a
   * user exception, eindex is 1 + (the subscript into the method's
   * exceptionVector), and sys_ex_index is
   * ilu_ProtocolException_Success.  If a system exception is being
   * signalled, eindex should be 0, and the sys_ex_index value
   * should indicate the system exception.
   */

  ilu_boolean(*pr_begin_exception) (ilu_Call call,
				    ilu_cardinal exception_code,
				    ilu_ProtocolException sys_ex_index,
				    ilu_cardinal reply_size,
				    ILU_ERRS((IoErrs)) * err);
  /*
   * Introduce exceptional results, either a protocol error or a
   * programmer-defined one.  In the former case, exception_code=0
   * and sys_ex_index is the ilu_ProtocolException; in the latter
   * case, exception_code is 1 + (the subscript into the method's
   * exceptionVector) and sys_ex_index is
   * ilu_ProtocolException_Success.  reply_size is the marshalled
   * size of the exeption parameter.
   */

  ilu_boolean(*pr_finish_exception) (ilu_Call call,
				     ILU_ERRS((IoErrs)) * err);
  /*
   * pr_begin_exception and pr_finish_exception bracket the sending
   * of exceptional results.  pr_finish_exception calls
   * _ilu_CacheCall if transport timesout.  Caller next calls
   * pr_finish_call.
   */

  /**L1 >= {cmu, calls' server}, L1.sup < trmu;
     Main Remnant holds; L2 >= {call's conn's callmu, iomu}*/
  ilu_boolean(*pr_prefinish_call) (ilu_Call call,
				   ILU_ERRS((IoErrs)) * err);
  /*
   * Called whenever pr_finish_call is, unless kernel is too broken.
   * May be null, meaning NO-OP.  Called just before pr_finish_call.
   * Note that call's connection may already be closed.
   */

  /*Main Invariant holds; L2 disjoint {call's conn's callmu, iomu}*/
  ilu_boolean(*pr_finish_call) (ilu_Call call,
				ILU_ERRS((IoErrs)) * err);
  /*
   * Called at the end of processing of an incoming or outgoing
   * call.  Cleans up after use of call's connection's
   * co_protocol_data slot.  Note that call's connection may already
   * be closed.
   */

  /* [Un]Marshalling routines */
  /*L1, L2 unconstrained for sizing, end*/
  /*Main holds, L2 >= {call's connection's callmu, iomu} for output*/
  /*Main holds, L2 >= {call's connection's callmu, iomu} for input*/
  
  void            (*pr_output_optional) (ilu_Call call, ilu_boolean i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_optional) (ilu_Call call, ilu_boolean * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_optional) (ilu_Call call, ilu_boolean i,
				      ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_integer) (ilu_Call call, ilu_integer i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_integer) (ilu_Call call, ilu_integer * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_integer) (ilu_Call call, ilu_integer i,
				     ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_cardinal) (ilu_Call call, ilu_cardinal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_cardinal) (ilu_Call call, ilu_cardinal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_cardinal) (ilu_Call call, ilu_cardinal i,
				      ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_shortinteger) (ilu_Call call,
				                ilu_shortinteger i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_shortinteger) (ilu_Call call,
			                      ilu_shortinteger * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortinteger) (ilu_Call call,
					  ilu_shortinteger i,
					  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_shortcardinal) (ilu_Call call,
			                       ilu_shortcardinal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_shortcardinal) (ilu_Call call,
			                     ilu_shortcardinal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortcardinal) (ilu_Call call,
					   ilu_shortcardinal i,
					 ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_real) (ilu_Call call, ilu_real i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_real) (ilu_Call call, ilu_real * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_real) (ilu_Call call, ilu_real i,
				  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_shortreal) (ilu_Call call, ilu_shortreal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_shortreal) (ilu_Call call, ilu_shortreal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortreal) (ilu_Call call, ilu_shortreal i,
				       ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_longinteger) (ilu_Call call,
				                 ilu_longinteger i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_longinteger) (ilu_Call call,
			                       ilu_longinteger * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_longinteger) (ilu_Call call,
					 ilu_longinteger i,
					 ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_longcardinal) (ilu_Call call,
				                ilu_longcardinal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_longcardinal) (ilu_Call call,
			                      ilu_longcardinal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_longcardinal) (ilu_Call call,
					  ilu_longcardinal i,
					  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_longreal) (ilu_Call call, ilu_longreal i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_longreal) (ilu_Call call, ilu_longreal * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_longreal) (ilu_Call call, ilu_longreal i,
				      ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_byte) (ilu_Call call, ilu_byte i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_byte) (ilu_Call call, ilu_byte * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_byte) (ilu_Call call, ilu_byte i,
				  ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_boolean) (ilu_Call call, ilu_boolean i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_boolean) (ilu_Call call, ilu_boolean * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_boolean) (ilu_Call call, ilu_boolean i,
				     ILU_ERRS((IoErrs)) * err);

  /* this is a Unicode character */
  void            (*pr_output_character) (ilu_Call call, ilu_character i,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_character) (ilu_Call call, ilu_character * i,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_character) (ilu_Call call, ilu_character i,
				       ILU_ERRS((IoErrs)) * err);

  /* this is an ISO Latin-1 character using the ISO 8859-1 encoding */
  void(*pr_output_shortchar) (ilu_Call call, ilu_shortcharacter i,
				     ILU_ERRS((IoErrs)) * err);
  void(*pr_input_shortchar) (ilu_Call call,
				    ilu_shortcharacter * i,
				    ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_shortchar) (ilu_Call call,
				       ilu_shortcharacter i,
				       ILU_ERRS((IoErrs)) * err);

  void(*pr_output_enum_code) (ilu_Call call, ilu_shortcardinal i,
				     ILU_ERRS((IoErrs)) * err);
  void(*pr_input_enum_code) (ilu_Call call, ilu_shortcardinal * i,
				    ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_enum_code) (ilu_Call call,
				       ilu_shortcardinal i,
				       ILU_ERRS((IoErrs)) * err);

  /* a string is a sequence of shortcharacter */
  void(*pr_output_string) (ilu_Call call, ilu_string s,
				  ilu_cardinal length,
				  ilu_cardinal limit,
				  ILU_ERRS((IoErrs)) * err);
  void(*pr_input_string) (ilu_Call call, ilu_string * s,
				 ilu_cardinal * length,
				 ilu_cardinal limit,
				 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_string) (ilu_Call call, ilu_string s,
				    ilu_cardinal length,
				    ilu_cardinal limit,
				    ILU_ERRS((IoErrs)) * err);

  /*
   * a stringvec is a vector of shortcharacter -- not
   * null-terminated
   */
  void(*pr_output_stringvec) (ilu_Call call, ilu_string s,
				     ilu_cardinal length,
				     ILU_ERRS((IoErrs)) * err);
  void(*pr_input_stringvec) (ilu_Call call, ilu_string * s,
				    ilu_cardinal length,
				    ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_stringvec) (ilu_Call call, ilu_string s,
				       ilu_cardinal length,
				       ILU_ERRS((IoErrs)) * err);

  /* a wstring is a sequence of character */
  void(*pr_output_wstring) (ilu_Call call, ilu_wstring s,
				   ilu_cardinal length,
				   ilu_cardinal limit,
				   ILU_ERRS((IoErrs)) * err);
  void(*pr_input_wstring) (ilu_Call call, ilu_wstring * s,
				  ilu_cardinal * length,
				  ilu_cardinal limit,
				  ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_wstring) (ilu_Call call, ilu_wstring s,
				     ilu_cardinal length,
				     ilu_cardinal limit,
				     ILU_ERRS((IoErrs)) * err);

  /*
   * a wstringvec is a vector of character -- not null-terminated
   */
  void(*pr_output_wstringvec) (ilu_Call call, ilu_wstring s,
				      ilu_cardinal length,
				      ILU_ERRS((IoErrs)) * err);
  void(*pr_input_wstringvec) (ilu_Call call, ilu_wstring * s,
				     ilu_cardinal length,
				     ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_wstringvec) (ilu_Call call, ilu_wstring s,
					ilu_cardinal length,
					ILU_ERRS((IoErrs)) * err);

  /* opaque is a vector of bytes */
  void(*pr_output_opaque) (ilu_Call call, ilu_opaque s,
				  ilu_cardinal length,
				  ILU_ERRS((IoErrs)) * err);
  void(*pr_input_opaque) (ilu_Call call, ilu_opaque * s,
				 ilu_cardinal length,
				 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_opaque) (ilu_Call call, ilu_opaque s,
				    ilu_cardinal length,
				    ILU_ERRS((IoErrs)) * err);

  /* bytes is a sequence of bytes */
  void(*pr_output_bytes) (ilu_Call call, ilu_bytes s,
				 ilu_cardinal length,
				 ilu_cardinal limit,
				 ILU_ERRS((IoErrs)) * err);
  void(*pr_input_bytes) (ilu_Call call, ilu_bytes * s,
				ilu_cardinal * length,
				ilu_cardinal limit,
				ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_bytes) (ilu_Call call, ilu_bytes s,
				   ilu_cardinal length,
				   ilu_cardinal limit,
				   ILU_ERRS((IoErrs)) * err);

  /* object references vary from protocol to protocol */
  
/*L2 >= {call's conn's callmu, iomu}.
  kobj == NIL => Main Invariant holds.
  kobj != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = kobj's server and cl = kobj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
  ilu_boolean(*pr_output_object_id) (ilu_Call call, ilu_Object kobj,
				     ilu_boolean discriminator_p,
				     ilu_Class static_type,
				     ILU_ERRS((IoErrs)) * err);

/*before: L1 = {};
  after:  *kobj!=NIL => Inside(*kobj's server, static_type);
  after:  *kobj==NIL => L1 = {};
  L2 >= {call's connection's callmu, iomu};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained*/
  ilu_boolean(*pr_input_object_id) (ilu_Call call, ilu_Object * kobj,
				    ilu_boolean discriminator_p,
				    ilu_Class static_type,
				    ILU_ERRS((IoErrs)) * err);

/*kobj!=NIL => L1 >= {kobj's server};
  kobj==NIL => L1 unconstrained*/
  ilu_cardinal(*pr_size_of_object_id) (ilu_Call call, ilu_Object kobj,
				       ilu_boolean discriminator_p,
				       ilu_Class static_type,
				       ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_sequence) (ilu_Call call,
			                       ilu_cardinal length,
				                ilu_cardinal limit,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_output_sequence_mark) (ilu_Call call,
			                       ilu_cardinal extent,
			                 ILU_ERRS((IoErrs)) * err);
  /* ...called every 2^16-1 items */
  void            (*pr_input_sequence) (ilu_Call call,
			                     ilu_cardinal * length,
				                ilu_cardinal limit,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_sequence_mark) (ilu_Call call,
			                       ilu_cardinal extent,
			                 ILU_ERRS((IoErrs)) * err);
  /* ...called every 2^16-1 items */
  void            (*pr_end_sequence) (ilu_Call call,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_sequence) (ilu_Call call, ilu_cardinal length,
				      ilu_cardinal limit,
				      ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_array) (ilu_Call call, ilu_cardinal length,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_array) (ilu_Call call,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_end_array) (ilu_Call call,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_array) (ilu_Call call, ilu_cardinal length,
				   ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_record) (ilu_Call call,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_record) (ilu_Call call,
			                 ILU_ERRS((IoErrs)) * err);
  void            (*pr_end_record) (ilu_Call call,
			                 ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_record) (ilu_Call call,
				    ILU_ERRS((IoErrs)) * err);

  void            (*pr_output_union) (ilu_Call call,
				      ilu_cardinal discrim,
				      ilu_cardinal dsize,
				      ILU_ERRS((IoErrs)) * err);
  void            (*pr_input_union) (ilu_Call call,
		                     ilu_cardinal * typeIndex,
				     ilu_cardinal dsize,
				     ILU_ERRS((IoErrs)) * err);
  void            (*pr_end_union) (ilu_Call call,
				   ILU_ERRS((IoErrs)) * err);
  ilu_cardinal(*pr_size_of_union) (ilu_Call call,
				   ilu_cardinal typeIndex,
				   ilu_cardinal dsize,
				   ILU_ERRS((IoErrs)) * err);
};

struct _ilu_TransportCreator_s {
  /*
   * A TransportCreator is never changed once created, so these
   * fields are readonly.  Caller is responsible for using
   * sequentially (e.g., not making two concurrent calls); this
   * is in addition to the constraints in the locking comments.  The
   * locking comments refer to invocationns of the methods.
   */

  ilu_boolean     tcr_boundaried;
  ilu_boolean     tcr_reliable;
  
  /* L1_sup < trmu; L2 unconstrained */
  ilu_integer(*tcr_dfd) (ilu_TransportCreator /* self */ ,
			 ilu_boolean /* mooring */ );
  /*
   * Estimates how many FDs will be consumed when creating a mooring
   * (if mooring) or outgoing transport (if !mooring).  Estimate may
   * err high, but not low.
   */

  /* Main Invariant holds */
  ilu_Transport(*tcr_createTransport) (ilu_TransportCreator /* self */ ,
				       ilu_boolean /* buffer */ ,
				       ilu_integer * /* dfd */ ,
				       ilu_Passport	/* for getting identities from */,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * The Transport instance creation procedure.  Caller promises
   * exposed buffers won't be used if !buffer (in which case callee
   * is free to not allocate any).  The result is owned by the
   * caller (who will eventually close it).  Always stores at *dfd
   * the number of FDs actually consumed, even when erring.
   */

  /* L1_sup < trmu; L2 unconstrained */
  ilu_Mooring(*tcr_createMooring) (ilu_TransportCreator /* self */ ,
				   ilu_TransportInfo * /* tinfo_out */ ,
				   ilu_boolean /* buffer */ ,
				   ilu_integer * /* dfd */ ,
				   ilu_Passport /* passport */,
				   ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * The Mooring instance creation procedure.  Caller promises
   * exposed buffers of results of mo_accept_connection won't be
   * used if !buffer (in which case mo_accept_connection is free to
   * not allocate any).  If tinfo_out != NIL, store through it a
   * fully-detailed transport info string, which will be passed to
   * ilu_GetTransportCreator in a peer.  Always store at *dfd the
   * number of FDs actually consumed.  The results are owned by the
   * caller (who will eventually close the mooring and ilu_free the
   * string).
   */

  /* L1, L2 unconstrained */
  void            (*tcr_close) (ilu_TransportCreator /* self */ );
  /* Even frees self. */

  ilu_refany      tcr_data;
  /* For the private use of the above methods. */
};

/*
 * An ILU transport is used to transport either bytes or messages
 * bidirectionally between peers.
 * 
 * Each transport is either message-oriented (we say "boundaried") or
 * byte-oriented. For a boundaried transport, the caller makes
 * explicit calls to begin and finish processing each message; for a
 * non-boundaried transport these calls are not made.  The content
 * of each message is a byte sequence, processed with the same calls
 * that process the whole content of a non-boundaried trasnport.
 * 
 * Some transports are reliable, others aren't.  Only boundaried
 * transports may be unreliable.  An unreliable transport may drop
 * or re-order messages; higher layers cope with this through the
 * use of timeouts, retransmissions, and duplicate-handling
 * techniques.
 */

struct _ilu_Transport_s {
  /* L1, L2, Main unconstrained */

  /*
   * Following are a pair of buffers used by both generic procs and
   * tr_class.  When tr_inBuff != NIL, bytes at indices [tr_inNext,
   * tr_inLimit) are the next input bytes.  When tr_outBuff != NIL,
   * it contains some amount (known to the transport) of past
   * output, and space for more output at indices [tr_outNext,
   * tr_outLimit).  A boundaried transport presents no input bytes
   * when not inputting a message and no output space when not
   * outputting a message.  We place the buffers first in this
   * struct on the suspicion that some machines can access data at
   * small offsets more quickly than at large offsets.
   */
  ilu_bytes       tr_inBuff, tr_outBuff;
  ilu_cardinal    tr_inNext, tr_outNext;
  ilu_cardinal    tr_inLimit, tr_outLimit;

  ilu_TransportClass tr_class;
  ilu_refany      tr_data;	/* For use by tr_class. */

  /* Timeouts used by callers of unreliable transports. */
  ilu_FineTime    tr_to1;	/* Initial timeout */
  ilu_FineTime    tr_toN;	/* Max timeout */
  ilu_FineTime    tr_tto;	/* Total timeout */

  ilu_byte        tr_tinBuff[16];
  /* Used by _ilu_transportGetInputBuffer. */
};

typedef struct {
  ilu_boolean     tr_eom;
  ilu_boolean     tr_eof;
}               ilu_TransportReport;
/*
 * Used by tc_read_bytes to report whether end-of-message and/or
 * end-of-file follows bytes just read.  ("file" is a bit of a
 * misnomer; we just mean the end of whatever the source is.) tr_eom
 * is meaningful only for boundaried transports, which also set it
 * TRUE whenever they set tr_eof TRUE.
 */

typedef struct {
  ilu_TransportInputHandler tih_proc;
  ilu_refany      tih_rock;
}               ilu_TIH;

struct _ilu_TransportClass_s {
  /* A TransportClass is never changed or freed once created, so
   * these fields are readonly.  The locking comments refer to
   * invocationns of the methods. */

  ilu_boolean     tc_boundaried;	/* am I boundaried? */
  ilu_boolean     tc_reliable;		/* am I reliable? */

  /* L1_sup < trmu; L2 unconstrained*/
  
  ilu_integer(*tc_closeDFd) (ilu_Transport /* self */ );
  /*
   * Estimates how many FDs would be freed by closing this
   * transport.  May err high, but not low.
   */
  
  /**tih => Main Invariant holds && L2 disjoint {conn's iomu, callmu}*/
  ilu_boolean(*tc_set_input_handler) (ilu_Transport /* self */ ,
			      ilu_TransportInputHandler /* tih */ ,
				      ilu_refany /* tih_rock */ ,
				      ILU_ERRS((no_memory, internal,
					     no_resources)) * err);
  /*
   * Used for incoming and outgoing Transports.  Used only (a) in
   * single-threaded runtimes or (b) for the in-memory transport;
   * may raise internal/threading in the other situations.  Normally
   * returns TRUE after guaranteeing that from now on, until changed
   * by another call to this proc, presence of input or EOF will
   * cause a call on this procedure, which should read a message and
   * deal with it.  May instead return FALSE after calling the
   * handler once immediately to deal with buffered input (exposed
   * buffer is empty if tih isn't null, else may raise
   * internal/tcInputSkipsBuff).  May pass a null tih to cancel the
   * previous setting.  Also returns FALSE when raising an error.
   */
  
  /*Main Invariant holds; L2 not further constrained*/

  ilu_boolean(*tc_wait_for_input) (ilu_Transport /* self */ ,
				   ilu_FineTime * /* limit */ ,
			     ILU_ERRS((interrupted)) * /* err */ );
  /*
   * Returns ILU_ERROK(*err) after either: (1) there is a decent
   * reason to suspect that, after a tc_begin_message if boundaried
   * and not already reading a message, "input progress" can be
   * made, or an error or EOM or EOF can be detected, without
   * blocking, (2) *limit passed (limit==NIL means *limit ==
   * +infinity), or (3) interrupt requested in a multi-threaded
   * runtime (in which case interrupted is raised), or (4)
   * tc_interrupt(self, ..) was called in a single-threaded runtime
   * (in which case interrupted is *not* raised).  The exposed input
   * buffer holds no more input (else internal/tcInputSkipsBuff is
   * raised).  "Input progress" means any advancement in the state
   * of any stage in the input pipeline --- regardless of whether
   * any bytes dribble out this end.  Used in single-threaded and
   * multi-threaded runtimes.  In S-T R/T, blocks by running main
   * loop; in M-T, blocks only the calling thread.  Note there's no
   * guarantee that /self/ won't be closed at return time.  In fact,
   * there's a design bug here: because no mutex is held at call
   * time, there's not really any guarantee /self/ will be open at
   * call time (but callee must assume so --- otherwise he has no
   * guarantee that /self/ isn't a dangling pointer!).
   */

  ilu_boolean(*tc_interrupt) (ilu_Transport /* self */ ,
			      ILU_ERRS((bad_param)) * /* err */ );
  /*
   * Applicable only in single-threaded runtime.  Cause current
   * calls (there may be several nested ones) on tc_wait_for_input
   * to return normally.
   */

  /*Main Invariant holds; L2 >= {conn's iomu}*/

  ilu_ReadHeaderResultCode(*tc_begin_message) (ilu_Transport /* self */ ,
					  ilu_boolean /* input */ ,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Begin a new message in the given direction (with caveats, in
   * the input case).  Raises internal/tcNotBoundaried if transport
   * is not boundaried.  Raises internal/beginMessage if not done
   * with previous message.  For output, returns either ilu_rhrc_ok
   * or _error.  May return any of the four codes for input. Returns
   * _ok if input message successfully started.  Returns _eof if EOF
   * instead of a message was waiting; caller will close the
   * ilu_Transport.  Returns _nothing if it is impossible to tell
   * whether EOF or a message is next without blocking; message has
   * not been started.  Once it has been determined that a message
   * is next, may block while reading complete message header.
   * Returns _error iff raising an error.
   */

  ilu_boolean(*tc_end_message) (ilu_Transport /* self */ ,
				ilu_boolean /* flush */ ,
				ilu_Message * /* msg */ ,
				ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Finish the current message in the current direction.  If flush,
   * be sure to start it on its way.  Block as necessary (note that
   * it may be necessary even in the input direction); if
   * single-threaded, use the main loop; if multi-threaded, block
   * only the calling thread.  If unreliable and output, return a
   * copy (including ownership) of the whole message (i.e., all the
   * bytes given to tc_write_bytes) to the caller through OUT
   * parameter /msg/, so that it may later be retransmitted with
   * tc_send_whole_message; otherwise ignore /msg/.  Raises
   * internal/endMessage if not currently I/Oing a message.  Raises
   * internal/tcBytesDropped if direction is input and the whole
   * message has not yet been read.  Raises internal/tcNotBoundaried
   * if transport is not boundaried. Raises internal/tcNoMsgHandle
   * if unreliable, output, and msg == NIL.
   */

  ilu_boolean(*tc_send_whole_message) (ilu_Transport /* self */ ,
				       ilu_Message * /* msg */ ,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Applicable only to unreliable transports; others may raise
   * internal/tcReliable.  Like {begin_message; write_bytes;
   * end_message}, except that the message is not returned again at
   * the end.  Caller retains ownership of *msg and *msg->msg_base.
   */

  ilu_boolean(*tc_write_bytes) (ilu_Transport /* self */ ,
				ilu_bytes /* buf */ ,
				ilu_cardinal /* bufLen */ ,
				ilu_boolean /* flush */ ,
				ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Write the contents of the exposed buffer (if any), followed by
   * the given buffer (if buf != NIL).  If flush and not boundaried,
   * be sure to start these bytes on their way; flush is not
   * significant when boundaried.  Prepare the exposed buffer to
   * receive at least 16 more bytes of output.  Raises
   * internal/bytesWithoutMsg if boundaried and not currently
   * outputting a message.  Caller retains ownership of buf.
   */


  ilu_cardinal(*tc_read_bytes) (ilu_Transport /* self */ ,
				ilu_bytes /* buf */ ,
				ilu_cardinal /* bufLen */ ,
				ilu_TransportReport * /* rpt */ ,
				ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Read some bytes into the given buffer (buf==NIL means the
   * exposed buffer is being given and bufLen is not meaningful).
   * The exposed input buffer holds no more input (else
   * internal/tcInputSkipsBuff is raised).  When buf==NIL, result is
   * final inLimit - inNext.  Note that buf==NIL grants freedom to
   * transfer headers and trailers along with payload.  Does not
   * read past message boundary (if boundaried), EOF, or end of
   * given buffer.  Does not block (so any loop involving this
   * procedure must also involve tc_wait_for_input).  Makes as much
   * input progress as is permitted by the above restrictions.  Sets
   * EOM and EOF bits in *rpt; may falsely set them to FALSE if at
   * least 1 byte was delivered.  An unboundaried transport always
   * sets EOM to FALSE.  EOF is not a reason to raise an error.
   * Returns number of bytes transferred into given buffer, even
   * when an error is raised.  Raises internal/bytesWithoutMsg if
   * boundaried and not currently inputting a message.  Caller
   * retains ownership of *buf.
   */

  /*L2 >= {conn's iomu}; no further restrictions*/
  ilu_boolean(*tc_close) (ilu_Transport /* self */ ,
			  ilu_integer * /* dfd */ ,
			  ILU_ERRS((internal)) * /* err */ );
  /*
   * Even frees self.  Aborts I/O in progress.  Stores at *dfd the
   * number of FDs freed, regardless of whether an error is raised.
   */
};

struct _ilu_Mooring_s {		/* transport-level port */
  /*
   * L1, L2, Main unconstrained for access; calling comments
   * folllow.
   */

  /*L1.sup < trmu; L2 unconstrained*/
  ilu_integer(*mo_dfd) (ilu_Mooring /* self */ , ilu_boolean /* add */ );
  /*
   * Estimates how many FDs will be freed by closing this mooring
   * (if !add) or consumed by accepting a connection (if add).  In
   * either case, may err high but not low.
   */
  
  /*L1, L2 unconstrained*/
  ilu_boolean(*mo_set_req_handler) (ilu_Mooring /* self */ ,
				    ilu_Server s,
			      ilu_TransportInputHandler /* tih */ ,
				    ilu_refany /* tih_rock */ ,
				    ILU_ERRS((no_memory, imp_limit,
					no_resources, broken_locks,
					      internal)) * err);
  /*
   * In a single-threaded runtime, call this to register a
   * connection request handler with the main loop.
   */

  /*Before: L1 = {s},
            forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
    After: Main Invariant holds*/
  ilu_boolean(*mo_wait_for_req) (ilu_Mooring /* self */ ,
				 ilu_Server s,
				 ILU_ERRS((interrupted,
					   broken_locks)) * err);
  /*
   * In a multi-threaded runtime, a thread calls this to wait for
   * the next connection request.  The server mutex is released
   * while this thread is blocked.  Because the server mutex is held
   * at entry, another thread cannot close the Mooring until after
   * the impl releases it.  Returns TRUE after a connection request
   * arrives; may also return TRUE after the Mooring's been closed.
   */

  /* L1_sup < trmu; L2, Main unconstrained */
  ilu_Transport(*mo_accept_connection) (ilu_Mooring /* self */ ,
				      ilu_string * /* peerinfo */ ,
					ilu_integer * /* dfd */ ,
					ilu_Passport  /* for stuffing identities into */,
				  ILU_ERRS((IoErrs)) * /* err */ );
  /*
   * Create a new transport in response to the waiting connection
   * request; will return NIL if there wasn't really a connection
   * request waiting.  Also returns (with ownership) through
   * peerinfo (if not NIL) a string (not necessarily a real "tinfo")
   * identifying the peer.  Stores at *dfd the number of FDs
   * consumed (regardless of whether error is raised).
   */

  /* L1, L2, Main unconstrained */
  ilu_boolean(*mo_close) (ilu_Mooring /* self */ ,
			  ilu_integer * /* dfd */ ,
			  ILU_ERRS((internal)) * /* err */ );
  /*
   * Even frees self.  Stores at *dfd the number of FDs freed (even
   * if error raised).
   */

  ilu_private     mo_data;
  /* For private use by above methods. */
};

typedef struct {
  ilu_Connection next, prev;	/* links in doubly-linked list */
} ilu_ConnLinks;

typedef struct ilu_replyCons *ilu_ReplyList;

struct ilu_replyCons {
  /* L1, L2, Main unconstrained */

  ilu_cardinal    rp_SN;
  ilu_refany      rp_queued;	/* from pr_delay_interp */

  /* L2 >= {connection's iomu} */
  ilu_ReplyList   rp_next;
};

struct _ilu_Connection_s {
  /*L1 >= {server}; L2, Main unconstrained*/

  ilu_Call co_mucall;		/* the one holding my call mutex */
  ilu_boolean co_ioing;		/* implements iomu */
  
  /*L1 unconstrained*/
  
  ilu_Protocol co_protocol;
  union {
    ilu_string co_peerinfo;	/* always use "conn_peerinfo()" */
    ilu_TransportInfo co_tinfo;	/* always use "conn_tinfo()" */
  } co_tinfo;			/* never access this field directly */
  /*
   * Never NIL.  Owned by conn.  For outoing connections, the
   * "tinfo" from which co_transport was created; for incoming
   * connections, a string (not necessarily a real "tinfo"), <
   * MAX_CCPEER_LEN long, identifying the peer for purposes of
   * replay detection.
   */
  ilu_string co_pinfo;		/* never NIL; conn owns; not complete */
  ilu_Transport co_transport;	/* actual byte stream */
  ilu_Port co_port;		/* NIL on client side,
				   points to port on server side */
  ilu_Passport co_auth_info;	/* information about the identity
				   of the caller */
  struct _ilu_IdentityInfo_s co_conn_identity;
  /* holds pointer to peerinfo info in identityinfo form */

  ilu_Server co_server;		/* never NIL */

  /*L2 >= {iomu}*/

  ilu_refany co_protocol_data;	/* NIL when closed */

  /* The following three fields are meaningful if the protocol is
     concurrent and the connection is outgoing. */
  ilu_ReplyList   co_replies;	/* queue of unprocessed replies */
  
  /* The following two fields are meaningful if furthermore
     the runtime can do condition variables; NIL otherwise. */
  /*L1 >= {server}; L2 unconstrained*/

  ilu_Call co_reader;		/* the one reading input, queuing it */
  ilu_private co_cc;		/* cond var for changes under server */
  
  ilu_boolean co_closed;
  ilu_cardinal co_next_sn;	/* meaningful for outgoing connections;
				   the next request msg serial number */
  ilu_integer co_nOuts;		/* For concurrent protocols, the
				   number of calls between holding callmu
				   for request and for reply;
				   for serial protocols, 0. */

  /*L1 >= {server} for [ilu_psl], which are for the doubly-linked list of
				the port's or server's connections;
   *L1 >= {cmu} for [ilu_lru],	which are for the global doubly-linked
				LRU list of idle connections. */
  ilu_ConnLinks co_links[2];	/* NIL-terminated, not circular */
  
};

typedef enum {ilu_psl, ilu_lru} ilu_ConnLinkKind;

#if 0

struct _ilu_Pipe_s {
  /*L1, L2 not designed yet*/
  ilu_Mutex pi_lock;
  ilu_Mooring pi_mooring;		/* non-NIL for local pipes */
  ilu_Call pi_call;		/* wrapper around connection */
  ilu_Protocol pi_protocol;	/* protocol for pipe object */
  ilu_string pi_contact_info;	/* contact info for transport
				   connection */
  ilu_boolean pi_sink_p;		/* pipe end is a sink */
  ilu_boolean pi_valid;		/* pipe can be read/written */
  ilu_string pi_type;		/* name of type of pipe */
};

#endif /* 0 */

typedef struct {
  char		cc_peerinfo[MAX_CCPEER_LEN]; /* Address of requestor */
  ilu_cardinal	cc_sn;
  ilu_Class	cc_intro_type;
  ilu_Method	cc_meth;
  ilu_Message	cc_replyMsg;
} ilu_CachedCall;

struct _ilu_Port_s {		/* where a server listens for
				 * connection requests */
  /* L1, L2, Main unconstrained */

  ilu_Server      po_server;	/* The server exported via this
				 * port. */
  ilu_string      po_pinfo;	/* given at create time; owned by
				 * port */
  ilu_Protocol    po_protocol;	/* protocol being used */
  ilu_TransportInfo po_tinfo;	/* transport info for this port;
				 * owned by the port */
  ilu_Mooring     po_mooring;	/* where this port listens */

  /* L1 >= {server} */

  ilu_boolean     po_closed;
  ilu_Port        po_next;	/* ..in chain of server's ports */
  ilu_ConnLinks   po_connHead;	/* head of chain of conns from me */
  ilu_TransportCreator po_tcr;
  ilu_CachedCall *po_call_cache;/* non-NIL when transport unreliable */
  int             po_call_cache_size;	/* non-0 when transport
					 * unreliable */
  int             po_call_cache_finger;	/* cache is a FIFO; next
					 * goes here */
};

struct _ilu_Server_s {
  /* L1, L2, Main unconstrained */

  ilu_Mutex       sr_lock;
  ilu_boolean     sr_true;	/* is this a true server? */

  /*
   * Ids of servers may have two different forms, their "plain"
   * form, in which any character except ((octet)0) may occur, and
   * their "encoded" form, in which any character aside from
   * alphanumeric and $-.+ is expressed as "%xx" where "xx" is the
   * hex form of code for the offending character.  The encoded form
   * is usually only seen in ILU URLs.
   */

  ilu_string	sr_id;		/* part of an oid; plain form */
  ilu_cardinal	sr_crc32;	/* CRC32 of the plain server ID */

  ilu_LanguageIndex sr_true_language; /* index in language table */


  /* L1 >= {self} */

  /*
   * The next four fields are meaningful for outgoing connections.
   * They are derived from the one and only contact info known by
   * this surrogate server, and are used to contact the true server.
   * A true server's contact infos are kept in its ports.
   */

  ilu_TransportInfo sr_tinfo;	/* transport info; owned by server */
  ilu_TransportCreator sr_tcr;	/* from tinfo */
  ilu_string      sr_pinfo;	/* protocol info; owned by server */
  ilu_Protocol    sr_protocol;	/* from pinfo */

  ilu_boolean     sr_closing;	/* ilu_BankServer sets this TRUE */
  ilu_boolean     sr_cfails;	/* connect info suspect */
  ilu_ConnLinks   sr_connHead;	/* meaningful for surrogates: list
				 * of open connections */
  ilu_Port        sr_ports;	/* head of chain of ports usable for
				 * contacting this true server. */
  ilu_Port        sr_local_port; /* port for connection from this
				    true server to itself */
  ilu_HashTable   sr_objs;	/* maps ih -> ilu_Object, including
				 * singletons; goes NIL when closing
				 * && all objects deleted */
  ilu_HashTable   sr_singles;	/* maps singleton ilu_Class ->
				 * ilu_Object; goes NIL when closing
				 * && all objects deleted */
  ilu_ObjectTable sr_objtab;	/* significant in a true server; if
				 * not NIL, the app's chance to
				 * instantiate an object not found
				 * in objs */
  ilu_Port        sr_default_port;	/* default port; could be
					 * NIL */
};

/*
 * Instance handles of objects may have two different forms, their
 * "plain" form, in which any character except ((octet)0) may occur,
 * and their "encoded" form, in which any character aside from
 * alphanumeric and $-.+ is expressed as "%xx" where "xx" is the hex
 * form of code for the offending character.
 */

struct _ilu_Object_s {
  /* L1, L2, Main unconstrained */

  ilu_string      ob_ih;	/* id wrt server; never NIL; plain */
  ilu_Server      ob_server;	/* never NIL */
  ilu_integer     ob_timeout;	/* num. sec.s after which to GC */
  ilu_Class       ob_class;
  /*
   * Can only be NIL when doing _ilu_FindClassViaRPC. No locking
   * requirements because ilu_ObjectOfSBH doesn't insert an object
   * in its server's hash table 'till after class is set.
   */
  ilu_string      ob_mstid;	/* unique_id of true object's most
				 * specific type (may be NIL); plain */
  /* L1 >= {server} */

  ilu_string      ob_sbh;	/* really a cache, lazily eval'ed;
				 * may be NIL at any time; encoded */
  ilu_private     ob_lspos[MAX_LANGUAGES];
  /*
   * Vector of slots in which to store LS pointers, indexed by
   * language-index
   */
  unsigned int	ob_intNoted : 1;/* LSR thinks this about whether
				 * kernel is very interested in me */
  unsigned int	ob_holds : 31;	/* Count of uses in root method
				 * stubs and gc.c. */
  union {

    struct {
      /* The following fields are meaningful only for coll. surrogate
       * objs. */

      ilu_boolean	ob_notifying;	/* we're calling true server now */
      ilu_boolean	ob_known;	/* when last !notifying, did true
					 * server know about this surrogate? */
    } ob_surrogate;

    struct {
      /* The following fields are meaningful only for collectible true
       * objs. */

      /* L1 >= {gcmu} */

      ilu_Alarmette_s ob_gco;	/* mxamu==gcmu */
      ilu_integer     ob_lastRemote;/* when we last knew the object to
				     * be loose in the network; not
				     * meaningful while gclist non-empty */
      void           *ob_gclist;	/* set of gc.c:counted_client*; NIL
					 * may represent the empty set */
    } ob_true;

  } ob_collectibleInfo;
};
/*
The string fields are owned by the object.

The kernel is very interested in this object if either:
* gclist not empty (for collectible true objects);
* gclist empty and now < lastRemote+timeout (for coll. true objects);
* notifying && !known (for collectible surrogates);
* holds != 0.

As long as either:
* gclist not empty (for collectible true objects);
* gclist empty and now < lastRemote+timeout (for coll. true objects);
* notifying (for collectible surrogates);
* holds != 0;
* lspo != NIL;
the kernel is "interested" in this object, the object is in the server's hash table, and the object is not freed.

The kernel needs to make a GC callback when
	known != (lspo != NIL || holds != 0).
*/


/* ================ Old ilu.h above ================ */
/* ================ Old iluntrnl.h below ================ */

#define NOT		!
#define AND		&&
#define OR		||

#define TRUE		1
#define FALSE		0

#include <stdlib.h>
#include <errno.h>

#include "iludebug.h"

#define FREETOKEN(s) {if((s)!=NIL)ilu_free(s);}

#define PROTOCOL_TYPE(call)	(call->ca_connection->co_protocol->pr_type)

#define OBJECT_HASHTABLESIZE	113		/* number of different object slots */
#define SERVER_HASHTABLESIZE	53		/* number of different server slots */
#define CLASS_HASHTABLESIZE	53		/* number of different classes */

#define INITIAL_GCLIST_SIZE		5
#define INITIAL_GCDOBJECTSET_SIZE	200
#define INITIAL_GCCLIENTS_SIZE		20

#define GC_CALLBACK_CLASS_ID		"ilu.GCCallback"

/* ================ Internal Consistency Checking ================ */

#define ASSERT(flg,buf,spargs) {if (!(flg)) {char buf[1000]; sprintf spargs; _ilu_Assert(0, buf);}}

typedef struct {
  ilu_FailureConsumer fc;
  ilu_boolean     printMsg;
}               _ilu_FailureHandler;

extern _ilu_FailureHandler 
_ilu_FailureActionToConsumer(int fa, int which);
/*
 * Translates an integer code to the procedure it calls for.
 * which==0 for ilu_must_malloc, 1 for _ilu_Assert, and 2 for
 * ilu_Check.
 */

extern void     _ilu_ConsumeByLoop(const char *file, int line);
/* The result of _ilu_FaultActionToConsumer(-1, x) */


/* ================ Locking routines ================ */

/*
We use simple mutual exclusion (ie, semaphores with only two states).  When we get an error system, we may have a way of expressing the interruption of an Acquire operation.
*/

/*L1, L2, Main unconstrained*/

ilu_Mutex _ilu_CreateMutex( ilu_string d1, ilu_string d2 );
/*
 * The pair (d1, d2) describes the mutex; storage for strings owned
 * by caller.  Returns NIL on mem. alloc. failure.
 */

/*L1_sup < m before, L1_sup = m after*/
void _ilu_AcquireMutex( ilu_Mutex m );
/* Blocks until acquisition succeeds. */

/*L1 >= {m}*/
void _ilu_HoldMutex(ilu_Mutex m);
/* Checks that the caller holds the given mutex. */

/*L1 >= {m} before, L1 not >= {m} after*/
void _ilu_ReleaseMutex( ilu_Mutex m );
/* Releases held lock. */

ilu_boolean _ilu_CanCondition(void);

ilu_Condition
_ilu_CreateCondition(ilu_string d1, ilu_string d2,
		     ILU_ERRS((CantCondition)) * err);

ILU_ERRS((CantCondition)) _ilu_NotifyCondition(ilu_Condition c);

ILU_ERRS((CantCondition)) _ilu_DestroyCondition(ilu_Condition c);

/* L1_sup = m */
ILU_ERRS((CantCondition)) _ilu_WaitCondition(ilu_Condition c,
					     ilu_Mutex m);



/* ================ Synthesized Locking ================ */

/*L1 >= {conn's server}*/
/*L2 as implied by name*/
/*Main unconstrained*/

/*L1 >= {conn's server}*/
extern          ilu_boolean
_ilu_TakeConnIO(ilu_Connection conn, ilu_boolean hard,
		ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * No thread holds conn's I/O mutex; take it.  Returns TRUE iff
 * success; sets *err in all cases.  On failure, rasies bad_locks.
 */

/*L1 = {conn's server, cmu}*/
extern          ilu_boolean
_ilu_EnterConnIO(ilu_Connection conn, ilu_boolean hard,
		 ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Returns TRUE iff success.  On failure (not reliably detected),
 * rasies broken_locks if hard, bad_locks otherwise.  Set hard when
 * kernel code ensures calling thread doesn't already hold the I/O
 * mutex.  Sets *err in all cases.
 */

/*L1 >= {cmu, conn's server}; L1_sup < trmu*/
extern          ilu_boolean
_ilu_ReleaseConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Release I/O mutex, after closing the connection if its server is
 * closing.  On success, returns TRUE without modifying *err.  On
 * failure raises (hard ? broken_locks : bad_locks) .
 */

/* The following also shift conn into and out of the idle list. */

/*L1 = {conn's server, cmu}*/
extern          ilu_boolean
_ilu_EnterConnCall(ilu_Connection conn, ilu_Call call,
		   ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Returns TRUE iff success.  On failure (not reliably detected),
 * rasies broken_locks if hard, bad_locks otherwise.  Set hard when
 * kernel code ensures calling thread doesn't already hold the call
 * mutex.  Sets *err in all cases.
 * 
 * In a single-threaded runtime, an outgoing connection has a
 * monitoring input handler registered when its callmu isn't held
 * and nOuts == 0; callers of the callmu enter/exit procedures are
 * responsible for maintaining this invariant.
 */

/* L1 = {conn's server}; conn->co_nOuts > 0 */
extern          ilu_boolean
_ilu_AltEnterConnCall(ilu_Connection conn, ilu_Call call,
		      ilu_boolean hard,
		      ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * Returns TRUE iff success.  On failure (not reliably detected),
 * rasies broken_locks if hard, bad_locks otherwise.  Set hard when
 * kernel code ensures calling thread doesn't already hold the call
 * mutex.  Sets *err in all cases.
 */


/*L1 >= {conn's server};
  conn->co_nOuts==0 => L1 >= {cmu}*/
extern          ilu_boolean
_ilu_ReleaseConnCall(ilu_Connection conn, ilu_Call call,
		     ilu_boolean hard,
		     ILU_ERRS((bad_locks, broken_locks)) * err);
/*
 * On success, returns TRUE without setting *err.  Returns FALSE on
 * failure, after setting *err if it's not already set for an error.
 */


/* ================ FD & Connection Management ================ */

/*L1 >= {cmu}*/
/*L2, Main unconstrained*/

extern ilu_integer ilu_fdbudget;	/* # FDs allowed */
extern ilu_integer ilu_fdstaken;	/* # FDs used (incl idle) */

#define ilu_DeltaFD(n)	ilu_FullDeltaFD((n),__FILE__,__LINE__)

extern void     ilu_FullDeltaFD(ilu_integer n, char *file, int line);
/* Call this to change ilu_fdstaken, by n. */

extern ilu_ConnLinks ilu_idleConns;
/* Head of list of idle FD-consuming connections.  A connection is in this list while its callmu isn't held and its nOuts=0.  */

/*L1_sup = cmu*/
/*forall conn: (L2 >= {conn's iomu}) => (L2 >= {conn's callmu})*/
ILU_ERRS((WrongLocks)) _ilu_ReduceFdsTo(ilu_integer goal);
/*
 * Close idle connections until ilu_fdstaken <= goal or we run out
 * of idle connections.  If WrongLocks(io) or WrongLocks(call) is
 * raised, some idle connections may have been closed.
 */


/* ================ iluxport.h counterparts ================ */
/* These procedures are like their similarly-named iluxport.h
 * counterparts, except that they require more mutexes to be held.
 */

/*L2, Main unconstrained*/

/*Main Invariant holds*/
ilu_boolean
_ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				      ilu_FineTime * limit,
				      ILU_ERRS((interrupted)) * err);
/*
 * Calls conn's transport's wait_for_input method.  Note lack of
 * guarantee that /conn/ isn't closed at return time. (In fact
 * there's a bug here: no mutex is held to guarantee that conn isn't
 * closed at call time.)
 */

/*L1 >= {port's server}*/

void _ilu_ClearPortFromServer(ilu_Port port, ilu_Server s);
/* Unlink this port from its server, s.
   Called when port is closing and last connection is closed. */

ilu_boolean _ilu_ServerEmptyP (ilu_Server s);
/* True if no objects in server, and no object table defined. */

/*L1_sup = port's server; L1 >= {cmu}*/
void _ilu_ClosePort(ilu_Port port);
/* If port was the server's default, one of the server's other ports
 * becomes the default. */

/*L1 >= {cmu, conn's server}; L1_sup < trmu*/
/*L2 >= {conn's iomu}*/
extern void
_ilu_CloseIoingConnection(ilu_Connection conn,
			  ilu_boolean set_cfails);
/*
 * Usable on either incoming or outgoing connections. For a
 * connection of a surrogate server, set_cfails if you suspect the
 * server's connect info is bad.
 */


/*L1 = {conn's server, cmu}; L2 disjoint {conn's callmu, iomu}*/
extern ILU_ERRS((bad_locks, broken_locks))
_ilu_CloseConnection(ilu_Connection connection);
/* Usable on either incoming or outgoing connections. */


/*Main Invariant, L2 >= {conn's iomu}*/
extern ilu_boolean 
_ilu_CloseConnWithIo(ilu_Connection conn, ilu_boolean set_cfails,
		     ILU_ERRS((IoErrs)) * err);


/* ================ Disorganized ================ */


/*L1 >= {daimu}; L2, Main unconstrained*/

extern struct _ilu_DefaultAlarm_struct _ilu_gcoDefaultAlarm_s,
                _ilu_gccDefaultAlarm_s, _ilu_iotDefaultAlarm_s,
                _ilu_grDefaultAlarm_s, _ilu_udpDefaultAlarm_s;
/*
 * The default implementations of the four alarms used by the
 * kernel.  Note that this header file doesn't define struct
 * _ilu_DefaultAlarm_struct; those details can be private to the
 * default alarm implementation (apparently ANSI C doesn't mind
 * declarations of partially typed variables, at least as long as
 * only the address of such a variable is used in code to which the
 * type is only partial).
 */

/*L1_sup < trmu*/
void _ilu_HandleSigPIPE(void);
/* Called to set up signal halder for SIGPIPE before attempting
   read or write or connect.  Checked with ilu_SIGPIPE_Handled. */

/*L1, L2, Main unconstrained*/

extern ilu_MainLoop _ilu_DefaultMainLoop;
/* The one that's used unless otherwise specified. */

void _ilu_AutoSetDebugLevel (void);

void _ilu_debug_DumpPacket (ilu_byte * /* packet */,
			    ilu_cardinal /* length */,
			    ilu_string /* packet type - optional, retained */);

ilu_cardinal _ilu_SafeStrlen (ilu_string s);

void _ilu_FreeToken (ilu_refany token);

extern ilu_string
  _ilu_Strdup(const ilu_string /* str */);
#define _ilu_Strdup(s)		_ilu_full_Strdup((s),__FILE__,__LINE__)
extern ilu_string
  _ilu_full_Strdup(const ilu_string /* str */,
		  const char * /* filename */,
		  int /* line number */);

extern ilu_string _ilu_StringifyTinfo (ilu_TransportInfo, ILU_ERRS((no_memory)) *);
     /* returned string malloced by callee, owned by caller */
     
extern ilu_boolean _ilu_CompareTinfo (ilu_TransportInfo, ilu_TransportInfo);
     /* returns TRUE if tinfos are equal */

extern ilu_TransportInfo _ilu_ConcatTinfo (ilu_string,	     /* retained, required */
					   ilu_TransportInfo,/* retained, required */
					   ILU_ERRS((no_memory)) *);
     /* creates a new tinfo with the string at the beginning, and returns it */

extern ilu_TransportInfo _ilu_CopyTinfo (ilu_TransportInfo,
					 ILU_ERRS((no_memory)) *);
     /* creates and returns a copy of the arg */

#ifdef ENABLE_DEBUGGING
extern void _ilu_PrintTinfo (ilu_TransportInfo);
     /* prints the tinfo using ilu_DebugPrintf */
#endif /* ENABLE_DEBUGGING */

ilu_string _ilu_Hostname (void);
     /* returns pointer to static string giving name of host machine */

ilu_integer _ilu_atoi (ilu_string buf, /*OPTIONAL*/ ilu_string * nextp);
     /* returns int read from buf, and if nextp is non-NIL,
	returns pointer to next non-number char in buf.
	Handles explicit bases like 0xff3, or 0303, or 0d129,
	or 0b11001101010, and uses strtol(). */

extern int      _ilu_casefree_cmp(const ilu_string, const ilu_string);
/* returns 0 if s1 == s2, -1 if s1 < s2, 1 if s1 > s2 */

extern int 
_ilu_casefree_ncmp(const ilu_string, const ilu_string,
		   ilu_cardinal n);
/* Compares at most n chars. */

ilu_string _ilu_Strcat3(const ilu_string s1, const ilu_string s2,
			const ilu_string s3);
/* Returns s1+s2+s3, in fresh storage; NIL may be used to input
 * an empty string.  Result will be NIL only if malloc fails. */

ilu_string _ilu_Strcat5(const ilu_string s1, const ilu_string s2,
			const ilu_string s3, const ilu_string s4,
			const ilu_string s5);
/* Like _ilu_Strcat3, only more parts. */

/*L1 >= {trmu}*/
ilu_string _ilu_tcp_CurrentHostInetName (ILU_ERRS((IoErrs)) *);
/* Obsolete, and no longer implemented.  See CurrentHostIPAddrString */

extern          ilu_boolean
_ilu_ParseConnectInfo(ilu_string encodedContactInfo,
		      ilu_cardinal encodedContactInfoLen,
		      ilu_string * plainProtocolInfo,
		      ilu_TransportInfo * /* tinfo */ ,
		      ILU_ERRS((no_memory, inv_objref,
				internal)) * err);
/*
 * Parse first contact info in given sequence.  Returns TRUE iff
 * parse successful.  Stores through non-NIL pointer arguments;
 * storage ownership of strings and ilu_TransportInfo is returned to
 * caller iff successful.
 */

/*L1_sup < trmu, L2, Main unconstrained */
ilu_boolean _ilu_CheckTransportInfo (ilu_TransportInfo /* tinfo */,
				     ILU_ERRS((bad_param)) *err);
						   

/*L1, L2, Main unconstrained*/

/*L1_sup < smu*/
ilu_Server
_ilu_FindServer(ilu_string serverid, ilu_boolean add,
		ilu_string cinfo, ilu_cardinal cinfolen,
		ILU_ERRS((BadProtocolInfo, internal,
			  no_memory, inv_objref)) * err);
/*
 * Looks for the server with the given id.  If add and not found,
 * adds a new surrogate server of the given name and contact info.
 * cinfo's length is cinfolen; cinfo is not NULL-terminated.  Caller
 * owns id and cinfo.
 */

/*before: Inside(s, static_type)
  after:  result!=NIL => Inside(s, static_type);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained*/
ilu_Object _ilu_FindOrCreateObject (ilu_string /* ih (retain) */,
				    ilu_Server /* s */,
				    ilu_Class /* found_class (optional) */,
				    ilu_Class /* static_type */,
				    char * /* mstid (optional,retain) */,
				    char * /* sbh (optional,retain) */,
				    ilu_Error * /* err */);
/* Looks for object in s with the given ih.  If not there, creates
 * new surrogate object.  If "found_class" is not specified, does
 * RPC to object to determine appropriate type.  If "sbh" is specified,
 * copies it to object's SBH cache.  If "mstid" is specified, will
 * check object's type against it to detect type clashes.
 * If result!=NIL && ilu_GetLanguageSpecificObject(result)==NIL,
 * the caller must invoke ilu_RegisterLanguageSpecificObject or
 * ilu_DeltaHolds on the result before unlocking the server. */

/*L2    >=    {conn's callmu, iomu} before,
  L2 disjoint {conn's callmu, iomu} after*/
void _ilu_HandlePing (ilu_Call call);
/* Handles the built-in Ping method. */

/*L1 >= {cmu, server}; L1_sup < trmu*/
ILU_ERRS((BadDataStructure, KernelBroken, bad_locks, broken_locks))
_ilu_ServerRemoveObject(ilu_Server s, ilu_Object obj);

/*Inside(server, result's type)*/
ilu_Object _ilu_FindObjectInServer(ilu_string ih, ilu_Server s);

/*L1 >= {s}*/

ilu_boolean _ilu_Addable(ilu_Server s, ilu_Class t, ilu_Object *h);

void _ilu_AddSingleton(ilu_Server s, ilu_Class t, ilu_Object o);

extern          ilu_boolean
_ilu_CacheCall(ilu_Call call, ilu_Message * reply,
	       ILU_ERRS((internal)) * err);
/*
 * Called from a Protocol's finish_reply and _exception methods to
 * cache the reply.  Caller relinquishes ownership of
 * *reply->msg_base, but retains ownsership of *reply.
 */

/*L1_sup = s; L1 >= {cmu}*/
/*L2, Main unconstrained*/

ilu_Connection 
_ilu_CreateConnection(ilu_Transport bs,
		      ilu_TransportInfo tinfo,
		      ilu_string peerinfo,
		      ilu_Protocol pr, ilu_string pinfo,
		      ilu_Port port, ilu_Server s,
		      ilu_Passport,	/* optional, pass */
		      ILU_ERRS((no_memory)) * err);
/*
 * Used internally to create both incoming and outgoing connections.
 * bs, tinfo, pr, pinfo, and s aren't NIL; port is NIL iff s is a
 * surrogate.  tinfo is real tinfo of peer when outgoing, but NIL
 * when incoming.  peerinfo is NIL when outgoing, but some
 * arbitrary identifying string when incoming.  String args owned by
 * caller.  Result's call and I/O mutexes are not held by any
 * thread.  Caller is responsible for staying within FD budget. Sets
 * *err for return from kernel interface.
 */

/*L1 >= {conn's server if k=psl; cmu if k=lru}*/

void _ilu_LinkConnection(ilu_ConnLinks *head, ilu_Connection conn,
			 ilu_ConnLinkKind k);

void _ilu_UnlinkConnection(ilu_ConnLinks *head, ilu_Connection conn,
			   ilu_ConnLinkKind k);

/*L1 >= {s}*/
void _ilu_ClearConnectionFromServer (ilu_Connection c, ilu_Server s);

/*L1.sup < cmu; L2 unconstrained*/

extern ilu_Condition _ilu_connHandoffChange;

extern ilu_boolean
_ilu_HandOffNewConnection(ilu_Connection conn,
			  ilu_Error * err);
/*
 * On success, returns TRUE without modifying (*err).  On failure,
 * returns FALSE, after setting (*err) if it had contained success.
 * May raise one of bad_locks, broken_locks, internal.
 */

/*L2 >= {conn.iomu}*/
/*L1, Main unconstrained*/

ilu_integer _ilu_NDataPresent(ilu_Connection conn);
/* positive<=>some, 0<=>none, negative<=>error */

/*L2, Main unconstrained*/

extern ilu_refany _ilu_ioTimeoutAlarm;

extern ilu_refany _ilu_grAlarm;		/* for use in ilu_GetReply */
extern ilu_refany _ilu_udpAlarm;	/* for use in udp.c */

/*L1, L2, Main unconstrained*/

#ifdef UDPSOCKET_TRANSPORT
void _ilu_udp_SetTimeouts (double to1, double toN, double tto);
#endif

/*L1_sup < trmu*/

ilu_TransportCreator 
  _ilu_GetTransportCreator(ilu_TransportInfo tinfo,
			   ILU_ERRS((no_memory, inv_objref)) * err);
/* tinfo is owned by caller, and is syntactically complete. */

/*L1_sup < prmu*/

ilu_Protocol _ilu_GetProtocolFromInfo (ilu_string pinfo);
/* Caller owns the string arg. */

/*Main Invariant holds*/

void
_ilu_WaitForInputOnFD(int fd, ilu_boolean * sure,
		      ilu_FineTime * limit,
		      ILU_ERRS((interrupted)) * err);
void 
_ilu_WaitForOutputOnFD(int fd, ilu_boolean * sure,
		       ilu_FineTime * limit,
		       ILU_ERRS((interrupted)) * err);
/*
 * These two procedures return when either (1) the appropriate kind
 * of I/O can be done on the given file descriptor without blocking,
 * (2) an exceptional condition exists on the FD, (3) *limit has
 * arrived, (4) _ilu_InterruptFD(fd,..) has been called, or (5) the
 * implementation feels like it.  NIL may be passed for limit, in
 * which case *limit is effectively +infinity.
 * 
 * In a multi-threaded program, these procedures block only the calling
 * thread.  *sure is set, but not read.  When *sure is set true, (1)
 * or (2) is known to hold upon return.  When *sure is set false,
 * any of (1), (2), (3), or (5) may hold.  In case 5, the
 * implementation may raise interrupted, indicating that something
 * has asked the thread to interrupt its current RPC.
 * 
 * In a single-threaded program, these procedures process input on
 * other FDs and the alarm while waiting.  This processing may lead
 * to a nested call on one of these procedures, with the same or a
 * different FD.  When I/O is finally enabled on an FD, all nested
 * calls waiting on the same FD are unblocked; *sure is set TRUE for
 * the innermost call, FALSE for the others.  After calling one of
 * these procedures, the caller proceeds to do I/O.  When a socket
 * reaches EOF, reads will return 0 bytes (but no error will be
 * indicated).  The calling code can conclude it is seeing EOF only
 * if the first read after WaitForInputOnFD returns 0 bytes and
 * *sure was set to true.
 * 
 * Use of _ilu_InterruptFD does not cause interrupted to be raised.
 * 
 * Note lack of guarantees about what's not closed at return time.
 */

ilu_boolean
_ilu_InterruptFD(int fd, ILU_ERRS((bad_param)) * err);
/*
 * Interrupt all current waits on I/O on the given FD.  Applicable
 * only when single-threaded; raises bad_param/threading otherwise.
 * For use by transports in implementing their interrupt methods,
 * which are just one low-level part of the scheme for interrupting
 * calls.
 */

/*L2 >= {fd's connection's callmu, iomu}*/

extern          ilu_cardinal
_ilu_NbSockRead(int fd, ilu_byte * buf, ilu_cardinal bufLen,
		ilu_TransportReport * rpt,
		ILU_ERRS((internal / errno)) * err);
/*
 * A thin veneer on one of the underlying OS's "read from socket"
 * operations; this veneer iterates until the system call is not
 * interrupted. In particular, this procedure does not block trying
 * to fill the buffer; instead it just returns the number of bytes
 * read. Will return 0 if no bytes can be read without blocking.
 * Sets rpt->tr_eof iff EOF detected.  Raises only internal/errno,
 * for impossible errnos.
 */


extern          ilu_boolean
_ilu_SockWrite(int fd, ilu_byte * buf, ilu_cardinal nbytes,
	       ILU_ERRS((comm_failure / conn_lost,
			 internal / errno)) * err);
/*
 * This is like UNIX send(), except that it iterates over multiple
 * system calls, blocking only the calling thread, until the entire
 * transfer has been completed or has failed.  In single-threaded
 * runtimes, this assumes there'll be no nested calls to the same
 * procedure with the same FD.  May raise comm_failure/conn_lost or
 * internal/errno.
 */

/*Inside(obj's server, obj's type)*/

extern
ILU_ERRS((BadDataStructure, KernelBroken, GcRegFailed,
	  bad_locks, broken_locks))
_ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds);

extern
ILU_ERRS((BadDataStructure, KernelBroken, GcRegFailed,
	  bad_locks, broken_locks))
_ilu_VIUpdate(ilu_Object obj);
/*
 * Call this after holds or gclist's emptiness changes, or
 * lastRemote+timeout passes, and you're ready to have the server
 * invariant restored. L1 mutexes are exited and re-entered inside
 * this procedure!
 */


/****************************** from type.c ********************/

/*L1, L2, Main unconstrained */

extern const ilu_Class _ilu_rootClass;
/* The one with all the methods every object supports */

extern ilu_Method _ilu_GetTypesMethod;
extern ilu_Method _ilu_RegisterGCInterestMethod;
extern ilu_Method _ilu_UnregisterGCInterestMethod;
extern ilu_Method _ilu_PingMethod;
/* The methods of _ilu_rootClass */

/*L1_sup < otmu*/
ilu_Class _ilu_FindMSKA(ilu_string tid);
/* Returns the one most specific known ancestor of the type identified
   by the given string; returns NIL if that's not well-defined, or we
   don't yet know about the ancestry of the given type. */

/*L1_sup < otmu*/
void _ilu_EnumerateClasses (void (*proc) (ilu_Class, ilu_refany rock), ilu_refany rock);
/* Calls "proc" on every registered class, passing the class and "rock".
   The order in which the classes are supplied is not specified. */

/*Main Invariant holds; L2 otherwise unconstrained*/

ilu_Class _ilu_FindClassViaRPC (ilu_Object o);
/* o->ob_class is temporarily set to some known type of o */

#ifdef IIOP_PROTOCOL

/*Main Invariant holds; L2 otherwise unconstrained*/

ilu_Class _ilu_IIOP_FindClassViaRPC(ilu_Object o);
/* o->ob_class is temporarily set to some known type of o */

/*L1, L2 unconstrained*/

extern ilu_boolean _ilu_IIOP_ParseIIOP (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_Error *);
extern ilu_boolean _ilu_IIOP_ParseIOR (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_Error *);

/*L1, L2, Main unconstrained (this is only for calling from debugger)*/
ilu_cardinal _ilu_IIOP_SetMaxStringSize (ilu_cardinal size);

#endif


#ifdef HTTP_PROTOCOL
extern ilu_boolean _ilu_Parse_HTTP_URL(ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *, ilu_cardinal *, ilu_Error *);
#endif


/*L2    >=    {conn's callmu, iomu} before,
  L2 disjoint {conn's callmu, iomu} after*/
void _ilu_HandleGetTypes (ilu_Call call);

/* L1 > otmu */
/* To be used inside an _ilu_EnumerateClasses enumeration */
ilu_boolean 
_ilu_IsSubObjectType(ilu_Class a, ilu_Class b);
/* Returns TRUE iff a is a subtype of b
   (including the degenerate case of a=b). */

/****************************** Hash Table ops ********************/

#include "iluhash.h"

/********************* from gc.c ********************/

/*L1, L2, Main unconstrained*/

extern const ilu_Class _ilu_GcCallbackClass;

/*L1 >= {gcmu}*/

void _ilu_StartGCingTrueObj(ilu_Object obj);

void _ilu_StopGCingTrueObj(ilu_Object obj);

/*L1 >= {gcmu, cmu, obj's server}*/
ILU_ERRS((BadDataStructure, KernelBroken))
_ilu_TouchedObj(ilu_Object obj);
/* Applicable to collectible true objects;
   call this when lastRemote or gclist's emptiness changes. */

extern ilu_refany _ilu_gcoAlarm;
extern ilu_refany _ilu_gccAlarm;

/*Main Invariant holds*/
/*L2    >=    {conn's callmu, iomu} before,
  L2 disjoint {conn's callmu, iomu} after*/

void _ilu_HandleGCInterestDeregistration(ilu_Call call);
void _ilu_HandleGCInterestRegistration(ilu_Call call);
/* Server stubs for two built-in methods. */

/*Main Invariant holds; L2 otherwise unconstrained*/

ILU_ERRS((GcRegFailed)) _ilu_RegisterGCInterest(ilu_Object obj);
ILU_ERRS((GcRegFailed)) _ilu_UnregisterGCInterest(ilu_Object obj);
/* Notify the true server of surrogate obj's (non-)existance. */

#endif /* ndef _ILU_INTERNALS_ */
