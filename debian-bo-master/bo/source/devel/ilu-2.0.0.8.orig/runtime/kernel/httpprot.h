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

/* $Id: httpprot.h,v 1.5 1996/07/16 04:15:56 janssen Exp $ */


/* Provide HTTP Protocol for ILU

  Dan Larner, larner@parc.xerox.com
  4-4-96
  */



/* ********************************************************* */
/* prevent more than one inclusion of this header file       */
/* ********************************************************* */

#ifndef _HTTP_PROT_H
#define _HTTP_PROT_H



/* ********************************************************* */
/* Includes                                                  */ 
/* ********************************************************* */

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>

#include <fcntl.h>


/* ********************************************************* */
/* Defines                                                   */ 
/* ********************************************************* */

/* default port number to use for http if it's not specified */
#define DEFAULT_HTTP_PORT_NUMBER 80

/* the type id of a http_resource_object
   NOTE HTTP_RESOURCE_OBJECT_TYPE_ID MUST AGREE with the TYPEID for http.Resource objects
   in the file src/stubbers/parser/http.isl   */
#define HTTP_RESOURCE_OBJECT_TYPE_ID "ilu:Ilu_Http_1_0_resource_object"

/* name of the environment variable that may be used to set http 
   proxy server information 
  ILU_HTTP_PROXY_INFO is of the form proxy.host.name:portnumber */
#define ILU_HTTP_PROXY_INFO_ENV_VAR "ILU_HTTP_PROXY_INFO"

/* ********************************************************* */
/* function declarations                                     */
/* ********************************************************* */

/* used to setup the http prtocol */
ILU_ERRS((ProtocolAlreadyRegistered, MaxCountExceeded)) setup_http_protocol (void);



/* ********************************************************* */
/* Simple singly linked list                                 */
/* ********************************************************* */

/* a node in a list */
typedef struct _ilu_list_node_s  * _ilu_p_list_node;

typedef struct _ilu_list_node_s {
	ilu_refany			m_p_node_contents;
	/* intent is that m_p_node_into_contents
	   points into m_p_node_contents - thus it should never
	   be freed */
	ilu_refany			m_p_node_into_contents; 
	_ilu_p_list_node	m_p_next_list_node;
} _ilu_list_node_s;


/* a list */
typedef struct _ilu_list_s * _ilu_p_list;

typedef struct _ilu_list_s {
	_ilu_p_list_node	m_p_start_list_node;
	_ilu_p_list_node	m_p_last_list_node;
	ilu_cardinal		m_card_num_added; 
} _ilu_list_s;



/* ********************************************************* */
/* type safe lists for htpp header lines                     */

typedef _ilu_p_list			_http_p_header_list;
typedef _ilu_p_list_node	_http_p_list_node;




/* ********************************************************* */
/* contains any (member-var like) attributes specific to the 
   protocol - currently specifies batching and concurrency
   but batching probably really doesn't apply for http       */
/* ********************************************************* */

typedef struct _http_members_s http_members_s;

struct _http_members_s {
  
  ilu_boolean m_b_batching;
  ilu_boolean m_b_concurrent;
};


/*
Http Protocol state description
-------------------------------

Capitalized (sub)terms refer to http ISL concepts, e.g. 'Request' is the
ISL concept (that is an ILU record) that a method gets as an argument.
'request' is an ilu message that comes into a server.  This distinction
using capitalization is to help disambiguate between similar HTTP and 
ILU terms.

clnt2http_  prefix denotes a state that occurs on the client (surrogate) side
when performing a GET HEAD or POST call to a httpResource (or derived)
object.

clnt2ilu_  prefix denotes a state that occurs on the client (surrogate) side
when performing a method that is NOT a GET HEAD or POST call to a 
httpResource (or derived) Object.

srvr4http_  prefix denotes a state that occurs on the server (true) side
when performing a GET HEAD or POST call to a httpResource (or derived)
Object.

srvr4ilu_  prefix denotes a state that occurs on the server (true) side
when performing a method that is NOT a GET HEAD or POST call to a 
httpResource (or derived) Object.

in or out denotes which way bits are flowing with respect to the
side whose state it is.  e.g. clnt2http_out_... means bits
going out of a client.



	PROTOCOL FUNCTION			STATE (state represents what we're expecting to do next)
*/

typedef enum _http_enum_call_state {

	/* _http_init_call				initialization (the start state of a call) */

	/* *************************************************************** */
	/* *******************  begin clnt2http states ******************* */
	/* *************************************************************** */

	/* send the ilu request */
	/* _http_start_request			*/ clnt2http_out_method_name = 0,

	/* _http_output_object_id		*/ clnt2http_out_URL_path,
	/* _http_output_record			*/ clnt2http_out_Request_record,
	/* _http_output_string			*/ clnt2http_out_Request_URI,
	/* _http_output_sequence		*/ clnt2http_out_Header_sequence,
	/* _http_output_record			*/ clnt2http_out_Header_record,
	/* _http_output_string			*/ clnt2http_out_Header_Name,
	/* _http_output_optional		*/ clnt2http_out_Header_Value_present,
	/* _http_output_string			*/ clnt2http_out_Header_value,
	/* _http_end_record				*/ clnt2http_out_end_Header_record,
	/* _http_end_sequence			*/ clnt2http_out_end_Header_sequence,
	/* _http_output_optional		*/ clnt2http_out_Body_present,
	/* _http_output_bytes			*/ clnt2http_out_Body,
	/* _http_end_record				*/ clnt2http_out_end_Request_record,

	/* _http_finish_request			*/ clnt2http_out_finish_request,

	/* read in the ilu reply */
	/* _http_read_header			*/ clnt2http_in_read_reply_header = 100,
	/* _http_interpret_reply		*/ clnt2http_in_interpret_reply,

	/* _http_input_record			*/ clnt2http_in_Response_record,
	/* _http_input_enum_code		*/ clnt2http_in_Status_Line,
	/* _http_input_sequence			*/ clnt2http_in_Header_sequence,
	/* _http_input_record			*/ clnt2http_in_Header_record,
	/* _http_input_string			*/ clnt2http_in_Header_Name,
	/* _http_input_optional			*/ clnt2http_in_Header_Value_present,
	/* _http_input_string			*/ clnt2http_in_Header_value,
	/* _http_end_record				*/ clnt2http_in_end_Header_record,
	/* _http_end_sequence			*/ clnt2http_in_end_Header_sequence,
	/* _http_input_optional			*/ clnt2http_in_Body_present,
	/* _http_input_bytes			*/ clnt2http_in_Body,
	/* _http_end_record				*/ clnt2http_in_end_Response_record,
	/* _http_reply_read				*/ clnt2http_in_reply_read,

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ clnt2http_finish_call,


	/* *************************************************************** */
	/* *******************  begin clnt2ilu states ******************* */
	/* *************************************************************** */

	/* send the ilu request */
	/* _http_start_request			*/ clnt2ilu_out_method_name = 200,

	/* _http_output_object_id		*/ clnt2ilu_out_object_id,
	/* _http_output_VARIOUS			*/ clnt2ilu_out_arguments,
	/* _http_finish_request			*/ 

	/* read in the ilu reply */
	/* _http_read_header			*/ clnt2ilu_in_read_reply_header,
	/* _http_interpret_reply		*/ clnt2ilu_in_interpret_reply,

	/* note that state clnt2ilu_in_return_values is used in both
	the situation where we're reading return values, as well as when
	we're actually reading in exception values */
	/* _http_input_VARIOUS			*/ clnt2ilu_in_return_values,
	/* _http_reply_read				*/ 

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ clnt2ilu_finish_call,


	/* *************************************************************** */
	/* *******************  begin srvr4http states ******************* */
	/* *************************************************************** */

	/* read in the ilu request */
	/* note srvr_in_read_header is used for both srvr4http and srvr4ilu */
	/* _http_read_header			*/ srvr_in_read_header = 300,
	/* _http_interpret_request		*/ srvr4http_in_interpret_request,	
	/* _http_input_string			*/ srvr4http_in_server_id,
	/* _http_input_string			*/ srvr4http_in_discriminator_id,

	/* _http_input_record			*/ srvr4http_in_Request_record,
	/* _http_input_string			*/ srvr4http_in_Request_URI,
	/* _http_input_sequence			*/ srvr4http_in_Header_sequence,
	/* _http_input_record			*/ srvr4http_in_Header_record,
	/* _http_input_string			*/ srvr4http_in_Header_Name,
	/* _http_input_optional			*/ srvr4http_in_Header_Value_present,
	/* _http_input_string			*/ srvr4http_in_Header_value,
	/* _http_end_record				*/ srvr4http_in_end_Header_record,
	/* _http_end_sequence			*/ srvr4http_in_end_Header_sequence,
	/* _http_input_optional			*/ srvr4http_in_Body_present,
	/* _http_input_bytes			*/ srvr4http_in_Body,
	/* _http_end_record				*/ srvr4http_in_end_Request_record,
	/* _http_request_read			*/ srvr4http_in_request_read,

	/* send the ilu reply */
	/* _http_begin_reply			*/ srvr4http_out_begin_reply = 400,

	/* _http_output_record			*/ srvr4http_out_Response_record,
	/* _http_output_enum			*/ srvr4http_out_Status_Line,
	/* _http_output_sequence		*/ srvr4http_out_Header_sequence,
	/* _http_output_record			*/ srvr4http_out_Header_record,
	/* _http_output_string			*/ srvr4http_out_Header_Name,
	/* _http_output_optional		*/ srvr4http_out_Header_Value_present,
	/* _http_output_string			*/ srvr4http_out_Header_value,
	/* _http_end_record				*/ srvr4http_out_end_Header_record,
	/* _http_end_sequence			*/ srvr4http_out_end_Header_sequence,
	/* _http_output_optional		*/ srvr4http_out_Body_present,
	/* _http_output_bytes			*/ srvr4http_out_Body,
	/* _http_end_record				*/ srvr4http_out_end_Response_record,

	/* _http_finish_reply			*/ srvr4http_out_finish_reply,

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ srvr4http_finish_call,

	/* _http_output_VARIOUS			*/ srvr4http_out_exception,
	/* _http_finish_exception		sets state to srvr4http_finish_call */ 


	/* *************************************************************** */
	/* *******************  begin srvr4ilu states ******************** */
	/* *************************************************************** */

	/* read in the ilu request */
	/* _http_read_header			 srvr_in_read_header, delcared in srvr4http section */
	/* _http_interpret_request		*/ srvr4ilu_in_interpret_request = 500,	
	/* _http_input_string			*/ srvr4ilu_in_server_id,
	/* _http_input_string			*/ srvr4ilu_in_discriminator_id,

	/* _http_input_VARIOUS			*/ srvr4ilu_in_arguments,
	/* _http_request_read			*/ 

	/* send the ilu reply */
	/* _http_begin_reply			*/ srvr4ilu_out_begin_reply,

	/* _http_output_VARIOUS			*/ srvr4ilu_out_return_values,
	/* _http_finish_reply			*/ 

	/* finish up */
	/* _http_prefinish_call
	   _http_finish_call			*/ srvr4ilu_finish_call,	
			

	/* _http_output_VARIOUS			*/ srvr4ilu_out_exception,
	/* _http_finish_exception		sets state to srvr4ilu_finish_call */ 


  	/* *************************************************************** */
	/* *******************  bogus state ****************************** */
	/* *************************************************************** */

	/* used for development / debuging only */  http_bogus_state = 3333

	} _http_call_state ; /* end http_enum_call_state */



/* an enum denoting which side of / which kind of the call we're on */

typedef enum _http_enum_call_type {
	clnt2http,
	clnt2ilu,
	srvr4http,
	srvr4ilu,
	srvr4something
} _http_call_type;


/* **************************************************************** */
/* http call specific data put into a http call's ca_prdata2 member */
/* **************************************************************** */

typedef struct _http_call_info_s http_call_info_s;

struct _http_call_info_s {

	/* note that note all members may be used in any particular call - the
	   pointer ones that are not are guaranteed to be nil */

	/* the state of call processing we're in */
	_http_call_state m_call_state;

	/* which side of / which kind of the call we're on */
	_http_call_type m_call_type;

	/* contains number of headers yet to be processed on outgoing calls */
	ilu_cardinal m_card_num_headers_to_process;

	/* indicates whether this is a HEAD method call - we need to know this
       so we'll know that the Content-Length field is to be ignored when 
       it comes to reading in any entity body in a reply */
	ilu_boolean m_b_is_head_method;

	/* indicates whether the user supplied their own content-length header */
	ilu_boolean m_b_user_supplied_content_length;

	/* list used to hold header lines as we read them in, in either a reply back to 
	   a client or in a request to a server */
	_http_p_header_list m_p_http_headers_list;
	_http_p_list_node m_p_http_headers_current_node;

	/* holds request or status lines for server incoming requests and client incoming replies */
	ilu_string m_pc_request_or_status_line;

	/* note that none the next three strings  should be freed as they are merely pointers 
	   into other allocated spaces */
	ilu_string m_pc_method_name;	/* in requests, where in m_pc_request_or_status_line method name is */
	ilu_string m_pc_object_id;		/* in requests, where in m_pc_request_or_status_line object_id is */
	ilu_string m_pc_params_queries;	/* in requests, where in m_pc_request_or_status_line params&queries (if any) is */
	char m_c_params_queries_delim;	/* in requests, what the delimiting char was if params&queries were present */
	ilu_string m_pc_server_id;		/* in requests, where in the headers the server_id is */
	ilu_string m_pc_version;		/* in requests and resonses, where in m_pc_request_or_status_line the version string is */
	ilu_string m_pc_status_code;	/* in responses, where in the headers the status code string is */
	ilu_string m_pc_reason_phrase;	/* in responses, where in the headers the reason_phrase string is */

	ilu_shortcardinal m_scard_major;	/* version numbers in an servers incoming request or a clients incoming reply */
	ilu_shortcardinal m_scard_minor;

	ilu_shortcardinal m_scard_status_code;	/* in a reply, what the status code is */

	ilu_cardinal m_card_body_length;	/* how long the body is according to the Content-Length header */

}; /* end struct _http_call_info_s */



/* ************************************************************** */
/* #defined accessors for members of the _http_call_info_s struct */
/* ************************************************************** */

/* ********************************************************* */
/* returns (lvalue) state of the call on an object */

/* XXX eventually arrange to use DEBUG macro */

/*
#define _http_call_state(p_call) \
	fprintf(stderr, "_http_call_state is %hd at %s %d \n", \
	(( http_call_info_s *)(p_call->ca_prdata2))->m_call_state, __FILE__, __LINE__ ),\
	((( http_call_info_s *)(p_call->ca_prdata2))->m_call_state)

*/

#define _http_call_state(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_call_state)


/* ********************************************************* */
/* returns (lvalue) type of the call on an object            */

/* XXX eventually arrange to use DEBUG macro */

/*
#define _http_call_type(p_call) \
	fprintf(stderr, "_http_call_type is %hd at %s %d \n", \
	(( http_call_info_s *)(p_call->ca_prdata2))->m_call_type, __FILE__, __LINE__ ),\
	((( http_call_info_s *)(p_call->ca_prdata2))->m_call_type)
*/


#define _http_call_type(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_call_type)


/* ********************************************************* */
/* returns (lvalue) number of headers yet to be processed on outgoing calls */

#define _http_num_headers_to_process(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_card_num_headers_to_process)


/* ********************************************************* */
/* returns (lvalue) whether this is a HEAD method call */

#define _http_is_HEAD_method(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_b_is_head_method)


/* ********************************************************* */
/* returns (lvalue) whether the user supplied their own content-length header */

#define _http_user_supplied_content_length(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_b_user_supplied_content_length)


/* ********************************************************* */
/* returns (lvalue) pointer to list where we read in our headers in an
   incoming request   */

#define _http_headers_list(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_p_http_headers_list)


/* ********************************************************* */
/* returns (lvalue) pointer to next node to process in the header list */

#define _http_headers_current_node(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_p_http_headers_current_node)


/* ********************************************************* */
/* returns (lvalue) buffer used for a reply status line      */

#define _http_status_line(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_request_or_status_line)


/* ********************************************************* */
/* returns (lvalue) buffer used for a request request line   */

#define _http_request_line(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_request_or_status_line)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line method name is */

#define _http_method_name(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_method_name)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line object_id is */

#define _http_object_id(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_object_id)


/* ********************************************************* */
/* in requests, (lvalue) where in m_pc_request_or_status_line params&queries (if any) is */

#define _http_params_queries(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_params_queries)


/* ********************************************************* */
/* in requests, (lvalue) what the delimiting char was if params&queries were present */

#define _http_params_queries_delim(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_c_params_queries_delim)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line version string is */

#define _http_version_string(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_version)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line status code string is */

#define _http_status_code_string(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_status_code)


/* ********************************************************* */
/* returns (lvalue) status code                              */

#define _http_status_code(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_scard_status_code)


/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line reason phrase string is */

#define _http_version_reason_phrase(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_reason_phrase)



/* ********************************************************* */
/* returns (lvalue) where in m_pc_request_or_status_line server_id is */

#define _http_server_id(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_pc_server_id)


/* ********************************************************* */
/* returns (lvalue) what the major version is                */

#define _http_major_version(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_scard_major)


/* ********************************************************* */
/* returns (lvalue) what the minor version is                */

#define _http_minor_version(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_scard_minor)


/* ********************************************************* */
/* returns (lvalue) how loing the body is according to 
   any Content-Length header                                 */

#define _http_body_length(p_call) \
	((( http_call_info_s *)(p_call->ca_prdata2))->m_card_body_length)


/* ********************************************************* */
/* ensures no compiler complaints about passing
	transport_write_bytes a char* where it expects ilu_bytes */

#define _http_transport_write_bytes(the_transport, the_bytes, the_length, the_error) \
		transport_write_bytes(the_transport, (ilu_bytes) the_bytes, the_length, the_error)


/* ********************************************************* */
/* ensures no compiler complaints about passing
	transport_read_bytes a char* where it expects ilu_bytes */

#define _http_transport_read_bytes(the_transport, the_bytes, the_length, the_error) \
		transport_read_bytes(the_transport, (ilu_bytes) the_bytes, the_length, the_error)


/* ********************************************************* */
#endif /* not _HTTP_PROT_H */



/* ********************************************************* */
/* end of file                                               */
/* ********************************************************* */


