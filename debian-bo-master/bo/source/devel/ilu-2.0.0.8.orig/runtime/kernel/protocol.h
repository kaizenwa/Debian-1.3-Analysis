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
/* $Id: protocol.h,v 1.39 1996/06/19 02:40:45 janssen Exp $ */
/* Last edited by Mike Spreitzer March 8, 1996 8:36 am PST */

#define MAX_PROTOCOLS	10	/* max number of protocols in registration table */

#define protocol_type(proto)		((proto)->pr_type)
#define protocol_concurrent(proto)	((proto)->pr_concurrent_requests)
#define protocol_needs_sizing(proto)	((proto)->pr_sizing_required)

#define protocol_free_data_block(proto,block)	((*((proto)->pr_free_data_block))(block))
#define protocol_create_data_block(proto,err)		((*((proto)->pr_create_data_block))(err))
#define protocol_form_handle(proto,object)	((*((proto)->pr_form_handle))(object))

#define protocol_read_header(proto,call,ptype,psn,se)	((*((proto)->pr_read_header))((call),(ptype),(psn),(se)))
#define protocol_delay_interp(proto,call,err) ((*((proto)->pr_delay_interp))((call),(err)))
#define protocol_resume_interp(proto,call,x) ((*((proto)->pr_resume_interp))((call),(x)))
#define protocol_abandon_delayed_interp(proto,x,err) ((*((proto)->pr_abandon_delayed_interp))(x,err))
#define protocol_discard_message(proto,call,err) ((*((proto)->pr_discard_message))((call),(err)))
#define protocol_discard_input(proto,call,err) ((*((proto)->pr_discard_input))((call),(err)))
#define protocol_discard_output(proto,call,err) ((*((proto)->pr_discard_output))((call),(err)))

#define protocol_interpret_request(proto,call,err) ((*((proto)->pr_interpret_request))((call),(err)))
#define protocol_interpret_reply(proto,call,estatus,err) ((*((proto)->pr_interpret_reply))((call),(estatus),(err)))

#define protocol_start_request(proto,call,argSize,err) ((*((proto)->pr_start_request))((call),(argSize),(err)))
#define protocol_begin_sizing_reply(proto,call,exns,err) ((*((proto)->pr_begin_sizing_reply))((call),(exns),(err)))
#define protocol_begin_reply(proto,call,exns,argSize,err) ((*((proto)->pr_begin_reply))((call),(exns),(argSize),(err)))
#define protocol_begin_sizing_exn(proto,call,exceptionCode,sysExnIdx,err) ((*((proto)->pr_begin_sizing_exn))((call),(exceptionCode),(sysExnIdx),(err)))
#define protocol_begin_exception(proto,call,exceptionCode,sysExnIdx,argSize,err) ((*((proto)->pr_begin_exception))((call),(exceptionCode),(sysExnIdx),(argSize),(err)))
#define protocol_finish_exception(proto,call,err) ((*((proto)->pr_finish_exception))((call),(err)))
#define protocol_finish_request(proto,call,msgh,err) ((*((proto)->pr_finish_request))((call),(msgh),(err)))
#define protocol_finish_reply(proto,call,err) ((*((proto)->pr_finish_reply))((call),(err)))
#define protocol_request_read(proto,call,err) ((*((proto)->pr_request_read))((call),(err)))
#define protocol_reply_read(proto,call,err) ((*((proto)->pr_reply_read))((call),(err)))

#define protocol_skip_bytes(proto,call,numBytes) ((*((proto)->pr_skip_bytes))((call),(numBytes)))

#define protocol_output_optional(proto,call,i,e) ((*((proto)->pr_output_optional))((call),(i),(e)))
#define protocol_input_optional(proto,call,i,e) ((*((proto)->pr_input_optional))((call),(i),(e)))
#define protocol_size_of_optional(proto,call,i,err) ((*((proto)->pr_size_of_optional))((call),(i),(err)))

#define protocol_output_integer(proto,call,i,e) ((*((proto)->pr_output_integer))((call),(i),(e)))
#define protocol_input_integer(proto,call,i,e) ((*((proto)->pr_input_integer))((call),(i),(e)))
#define protocol_size_of_integer(proto,call,i,err) ((*((proto)->pr_size_of_integer))((call),(i),(err)))

#define protocol_output_cardinal(proto,call,i,e) ((*((proto)->pr_output_cardinal))((call),(i),(e)))
#define protocol_input_cardinal(proto,call,i,e) ((*((proto)->pr_input_cardinal))((call),(i),(e)))
#define protocol_size_of_cardinal(proto,call,i,err) ((*((proto)->pr_size_of_cardinal))((call),(i),(err)))

#define protocol_output_short_integer(proto,call,i,e) ((*((proto)->pr_output_shortinteger))((call),(i),(e)))
#define protocol_input_short_integer(proto,call,i,e) ((*((proto)->pr_input_shortinteger))((call),(i),(e)))
#define protocol_size_of_short_integer(proto,call,i,err) ((*((proto)->pr_size_of_shortinteger))((call),(i),(err)))

#define protocol_output_short_cardinal(proto,call,i,e) ((*((proto)->pr_output_shortcardinal))((call),(i),(e)))
#define protocol_input_short_cardinal(proto,call,i,e) ((*((proto)->pr_input_shortcardinal))((call),(i),(e)))
#define protocol_size_of_short_cardinal(proto,call,i,err) ((*((proto)->pr_size_of_shortcardinal))((call),(i),(err)))

#define protocol_output_long_integer(proto,call,i,e) ((*((proto)->pr_output_longinteger))((call),(i),(e)))
#define protocol_input_long_integer(proto,call,i,e) ((*((proto)->pr_input_longinteger))((call),(i),(e)))
#define protocol_size_of_long_integer(proto,call,i,err) ((*((proto)->pr_size_of_longinteger))((call),(i),(err)))

#define protocol_output_long_cardinal(proto,call,i,e) ((*((proto)->pr_output_longcardinal))((call),(i),(e)))
#define protocol_input_long_cardinal(proto,call,i,e) ((*((proto)->pr_input_longcardinal))((call),(i),(e)))
#define protocol_size_of_long_cardinal(proto,call,i,err) ((*((proto)->pr_size_of_longcardinal))((call),(i),(err)))

#define protocol_output_long_real(proto,call,i,e) ((*((proto)->pr_output_longreal))((call),(i),(e)))
#define protocol_input_long_real(proto,call,i,e) ((*((proto)->pr_input_longreal))((call),(i),(e)))
#define protocol_size_of_long_real(proto,call,i,err) ((*((proto)->pr_size_of_longreal))((call),(i),(err)))

#define protocol_output_real(proto,call,i,e) ((*((proto)->pr_output_real))((call),(i),(e)))
#define protocol_input_real(proto,call,i,e) ((*((proto)->pr_input_real))((call),(i),(e)))
#define protocol_size_of_real(proto,call,i,err) ((*((proto)->pr_size_of_real))((call),(i),(err)))

#define protocol_output_short_real(proto,call,i,e) ((*((proto)->pr_output_shortreal))((call),(i),(e)))
#define protocol_input_short_real(proto,call,i,e) ((*((proto)->pr_input_shortreal))((call),(i),(e)))
#define protocol_size_of_short_real(proto,call,i,err) ((*((proto)->pr_size_of_shortreal))((call),(i),(err)))

#define protocol_output_enum_code(proto,call,i,e) ((*((proto)->pr_output_enum_code))((call),(i),(e)))
#define protocol_input_enum_code(proto,call,i,e) ((*((proto)->pr_input_enum_code))((call),(i),(e)))
#define protocol_size_of_enum_code(proto,call,i,err) ((*((proto)->pr_size_of_enum_code))((call),(i),(err)))

#define protocol_output_character(proto,call,i,e) ((*((proto)->pr_output_character))((call),(i),(e)))
#define protocol_input_character(proto,call,i,e) ((*((proto)->pr_input_character))((call),(i),(e)))
#define protocol_size_of_character(proto,call,i,err) ((*((proto)->pr_size_of_character))((call),(i),(err)))

#define protocol_output_shortchar(proto,call,i,e) ((*((proto)->pr_output_shortchar))((call),(i),(e)))
#define protocol_input_shortchar(proto,call,i,e) ((*((proto)->pr_input_shortchar))((call),(i),(e)))
#define protocol_size_of_shortchar(proto,call,i,err) ((*((proto)->pr_size_of_shortchar))((call),(i),(err)))

#define protocol_output_byte(proto,call,i,e) ((*((proto)->pr_output_byte))((call),(i),(e)))
#define protocol_input_byte(proto,call,i,e) ((*((proto)->pr_input_byte))((call),(i),(e)))
#define protocol_size_of_byte(proto,call,i,err) ((*((proto)->pr_size_of_byte))((call),(i),(err)))

#define protocol_output_boolean(proto,call,i,e) ((*((proto)->pr_output_boolean))((call),(i),(e)))
#define protocol_input_boolean(proto,call,i,e) ((*((proto)->pr_input_boolean))((call),(i),(e)))
#define protocol_size_of_boolean(proto,call,i,err) ((*((proto)->pr_size_of_boolean))((call),(i),(err)))

#define protocol_output_string(proto,call,s,len,limit,e) ((*((proto)->pr_output_string))((call),(s),(len),(limit),(e)))
#define protocol_input_string(proto,call,s,len,limit,e) ((*((proto)->pr_input_string))((call),(s),(len),(limit),(e)))
#define protocol_size_of_string(proto,call,i,len,limit,err) ((*((proto)->pr_size_of_string))((call),(i),(len),(limit),(err)))

#define protocol_output_stringvec(proto,call,i,len,e) ((*((proto)->pr_output_stringvec))((call),(i),(len),(e)))
#define protocol_input_stringvec(proto,call,i,len,e) ((*((proto)->pr_input_stringvec))((call),(i),(len),(e)))
#define protocol_size_of_stringvec(proto,call,i,len,err) ((*((proto)->pr_size_of_stringvec))((call),(i),(len),(err)))

#define protocol_output_wstring(proto,call,s,len,limit,e) ((*((proto)->pr_output_wstring))((call),(s),(len),(limit),(e)))
#define protocol_input_wstring(proto,call,s,len,limit,e) ((*((proto)->pr_input_wstring))((call),(s),(len),(limit),(e)))
#define protocol_size_of_wstring(proto,call,i,len,limit,err) ((*((proto)->pr_size_of_wstring))((call),(i),(len),(limit),(err)))

#define protocol_output_wstringvec(proto,call,i,len,e) ((*((proto)->pr_output_wstringvec))((call),(i),(len),(e)))
#define protocol_input_wstringvec(proto,call,i,len,e) ((*((proto)->pr_input_wstringvec))((call),(i),(len),(e)))
#define protocol_size_of_wstringvec(proto,call,i,len,err) ((*((proto)->pr_size_of_wstringvec))((call),(i),(len),(err)))

#define protocol_output_bytes(proto,call,i,len,limit,e) ((*((proto)->pr_output_bytes))((call),(i),(len),(limit),(e)))
#define protocol_input_bytes(proto,call,i,len,limit,e) ((*((proto)->pr_input_bytes))((call),(i),(len),(limit),(e)))
#define protocol_size_of_bytes(proto,call,i,len,limit,err) ((*((proto)->pr_size_of_bytes))((call),(i),(len),(limit),(err)))

#define protocol_output_opaque(proto,call,i,len,e) ((*((proto)->pr_output_opaque))((call),(i),(len),(e)))
#define protocol_input_opaque(proto,call,i,len,e) ((*((proto)->pr_input_opaque))((call),(i),(len),(e)))
#define protocol_size_of_opaque(proto,call,i,len,err) ((*((proto)->pr_size_of_opaque))((call),(i),(len),(err)))

#define protocol_output_object_id(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_output_object_id))((call),(kernel_obj),(disc_p),(static_type),(err)))
#define protocol_input_object_id(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_input_object_id))((call),(kernel_obj),(disc_p),(static_type),(err)))
#define protocol_size_of_object_id(proto,call,kernel_obj,disc_p,static_type,err) ((*((proto)->pr_size_of_object_id))((call),(kernel_obj),(disc_p),(static_type),(err)))

#define protocol_output_pipe(proto,call,i) ((*((proto)->pr_output_pipe))((call),(i)))
#define protocol_input_pipe(proto,call,i) ((*((proto)->pr_input_pipe))((call),(i)))
#define protocol_size_of_pipe(proto,call,i) ((*((proto)->pr_size_of_pipe))((call),(i)))

#define protocol_output_sequence(proto,call,i,limit,e) ((*((proto)->pr_output_sequence))((call),(i),(limit),(e)))
#define protocol_output_sequence_mark(proto,call,extent,err)	((*((proto)->pr_output_sequence_mark))((call),(extent),(err)))
#define protocol_input_sequence(proto,call,i,limit,e) ((*((proto)->pr_input_sequence))((call),(i),(limit),(e)))
#define protocol_input_sequence_mark(proto,call,extent,err)	((*((proto)->pr_input_sequence_mark))((call),(extent),(err)))
#define protocol_end_sequence(proto,call,err) ((*((proto)->pr_end_sequence))((call),(err)))
#define protocol_size_of_sequence(proto,call,i,limit,err) ((*((proto)->pr_size_of_sequence))((call),(i),(limit),(err)))

#define protocol_output_union(proto,call,i,dsize,e) ((*((proto)->pr_output_union))((call),(i),(dsize),(e)))
#define protocol_input_union(proto,call,i,dsize,e) ((*((proto)->pr_input_union))((call),(i),(dsize),(e)))
#define protocol_end_union(proto,call,err) ((*((proto)->pr_end_union))((call),(err)))
#define protocol_size_of_union(proto,call,i,dsize,err) ((*((proto)->pr_size_of_union))((call),(i),(dsize),(err)))

#define protocol_output_array(proto,call,len,e) ((*((proto)->pr_output_array))((call),(len),(e)))
#define protocol_input_array(proto,call,e) ((*((proto)->pr_input_array))((call),(e)))
#define protocol_end_array(proto,call,err) ((*((proto)->pr_end_array))((call),(err)))
#define protocol_size_of_array(proto,call,len,err) ((*((proto)->pr_size_of_array))((call),(len),(err)))

#define protocol_output_record(proto,call,e) ((*((proto)->pr_output_record))((call),(e)))
#define protocol_input_record(proto,call,e) ((*((proto)->pr_input_record))((call),(e)))
#define protocol_end_record(proto,call,err) ((*((proto)->pr_end_record))((call),(err)))
#define protocol_size_of_record(proto,call,err) ((*((proto)->pr_size_of_record))((call),(err)))

/* protocol "instantiators" (see protocol.c) */

#ifdef SUNRPC_PROTOCOL
extern ilu_Protocol _ilu_sunrpc_Protocol(void);
extern ilu_Protocol _ilu_bsunrpc_Protocol(void);
extern ilu_Protocol _ilu_csunrpc_Protocol(void);
extern ilu_Protocol _ilu_bcsunrpc_Protocol(void);
#endif /* def SUNRPC_PROTOCOL */

#ifdef COURIER_PROTOCOL
extern ilu_Protocol _ilu_courier_Protocol(void);
#endif /* def COURIER_PROTOCOL */

#ifdef IIOP_PROTOCOL
extern ilu_Protocol _ilu_IIOP_Protocol(void);
#endif /* IIOP_PROTOCOL */

#ifdef W3MUX_PROTOCOL
extern ilu_Protocol _ilu_w3ng_Protocol(void);
#endif /* def W3MUX_PROTOCOL */

#ifdef HTTP_PROTOCOL
extern ilu_Protocol _ilu_http_Protocol(void);
#endif

