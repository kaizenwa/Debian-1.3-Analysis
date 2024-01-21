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

/* $Id: 2.0alpha8-patches.html,v 1.15 1996/10/22 20:18:09 janssen Exp $ */

/* provide  http protocol 

  Dan Larner, larner@parc.xerox.com
  4-4-96
*/

/* ********************************************************* */
/* Notes

	If there is an entity body, and no content-length header was supplied
	by the user, then the protocol will automatically add one.  Note that
	when responding to a Head method then (since there is no body) the 
	user should supply a Content-Length header.

	This code only addressed HTTP 1.0, (nothing earlier, nothing later (yet))

	Any chars in the URL's params and or queries (if present) that are required by 
	http to be %%HEX or otherwise encoded, are assumed to be already in that form.

	Assumes the request URI is well formed. Eventually check on this and error if not.

*/


/* ********************************************************* */
/* Includes                                                  */ 
/* ********************************************************* */

#include <string.h>
#include <ctype.h>

#if (defined WIN32 || defined WIN16)
#include <winsock.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#endif

#include <iluntrnl.h>
#include <call.h>
#include <connect.h>
#include <transprt.h>
#include <object.h>
#include <method.h>
#include <server.h>
#include <type.h>
#include <protocol.h>

#include <httpprot.h>


/* ********************************************************* */
/* Defines                                                   */ 
/* ********************************************************* */

/* uncomment HTTP_STRICT_END_OF_LINE if you want end of line to be 
CRLF as it's specified by the protocol, leave commented out if
just LF is acceptable This is needed to deal with some http servers -  
at least one existing http demon (NCSA version 1.3) has been 
encountered that only sends out LF in spots where CRLF is required by the
protocol.  Encountering such a server from this code will show up as a 
communication exception if HTTP_STRICT_END_OF_LINE is defined.
(Note that some version between NCSA 1.3 and NCSA 1.5.1 corrected this.) */
/* #define HTTP_STRICT_END_OF_LINE */

/* during reading in lines, how much to increase allocations each time */
#define HTTP_READLINE_ALLOC_SIZE		128

/* during reading in bodies, how much to increase allocations each time */
#define HTTP_BODY_ALLOC_SIZE			256

/* used for non native http calls to contain the server id in a http header */
#define ILU_SERVER_ID_HEADER_NAME "ILU_ServerID"
#define ILU_SERVER_ID_HEADER_NAME_LENGTH 12

/* used to cause a protection fault - xxx temporary debugging measure only */
#define FAULT {fprintf(stderr, "Bad HTTP State %d\n", _http_call_state(p_call)); *((char*)0) = 0;}

/* advance a pointer to next non space or tab character */
#define skipwhite(pc) while((*pc == ' ') || (*pc == '\t')) pc++;

/* advance a pointer to next space tab or null character */
#define nextwhite(pc) while(*pc && (*pc != ' ') && (*pc != '\t')) pc++;


/* ********************************************************* */
/* Constants                                                 */ 
/* ********************************************************* */

 /* version of http we implement */
static const ilu_shortcardinal HTTPMajorVersion = 1;
static const ilu_shortcardinal HTTPMinorVersion = 0;

/* lengths of the above when they are written out as a string */
static const ilu_shortcardinal HTTPMajorVersionStrLength = 1;
static const ilu_shortcardinal HTTPMinorVersionStrLength = 1;

/* carriage return linefeed combinations */
static char g_c_CRLF[] = "\r\n";
static char g_c_CRLFCRLF[] = "\r\n\r\n";

/* represents an optional object that's NIL */
static char g_c_NILOBJ [] = "NIL_OBJ";

/* represents that an optional is Present */
static char g_c_OPTIONAL_PRESENT [] = "OPT_PRESENT";

/* represents that an optional is Present */
static char g_c_OPTIONAL_NOT_PRESENT [] = "OPT_NOT_PRESENT";

/* a space */
static char g_c_SP[] = " ";

/* spaces and tabs */
static char g_c_White[] = { ' ', '\t', '\0' };

/* spaces and tabs and URL param/query separators */
static char g_c_objid_ends[] = { ' ', '\t', ';', '?', '\0' };

/* spaces, tabs, and colons */
static char g_c_NameValueDelims[] = { ':', ' ', '\t', '\0' };

/* message passed to assert if we're in an unexpected state */
static char g_c_bad_state[] = "Unexpected http state";

/* message used in assert if a function for concurrent protocols
  gets used with http (a non-concurrent) protocol */
static char g_c_not_concurrent[] = "Not a concurrent protocol";

/* reason phrases sent back in status lines when an exception occurs */
static char g_c_protocol_exception[] = "Protocol_Exception";
static char g_c_non_protocol_exception[] = "Non_Protocol_Exception";


/* ********************************************************* */
/* Simple singly linked list Operations                      */
/* ********************************************************* */

/* make a list - return new list or NIL on error */
static _ilu_p_list _ilu_make_list (ILU_ERRS((IoErrs)) *p_error) {

	_ilu_p_list p_the_list = ilu_MallocE(sizeof(_ilu_list_s), p_error);
	if (p_the_list == NIL)
		return NIL;

	p_the_list->m_p_start_list_node = NIL;
	p_the_list->m_p_last_list_node = NIL;
	p_the_list->m_card_num_added = 0; 

	return p_the_list;
}


/* add a refany to the list - returns new node or NIL on error */
static _ilu_p_list_node _ilu_add_to_list (_ilu_p_list p_the_list, ilu_refany p_node_contents,
										  ilu_refany p_node_into_contents,
									 ILU_ERRS((IoErrs)) *p_error) {

	_ilu_p_list_node p_the_node = ilu_MallocE(sizeof(_ilu_list_node_s), p_error);
	if (p_the_node == NIL) 
		return NIL;

	p_the_node->m_p_node_contents = p_node_contents;
	p_the_node->m_p_node_into_contents = p_node_into_contents;
	p_the_node->m_p_next_list_node = NIL;

	if (p_the_list->m_p_start_list_node == NIL) { /* first node */
		p_the_list->m_p_start_list_node = p_the_node;
		p_the_list->m_p_last_list_node = p_the_node;
	}
	else {
		p_the_list->m_p_last_list_node->m_p_next_list_node = p_the_node;
		p_the_list->m_p_last_list_node = p_the_node;
	}

	p_the_list->m_card_num_added++;

	return p_the_node;	
}


/* frees a list and everything in it */
static void _ilu_free_list_and_contents (_ilu_p_list p_the_list) {

	_ilu_p_list_node p_next_node;
	_ilu_p_list_node p_a_node = p_the_list->m_p_start_list_node;

	while (p_a_node) {
		p_next_node = p_a_node->m_p_next_list_node;
		ilu_free(p_a_node->m_p_node_contents);
		ilu_free(p_a_node);
		p_a_node = p_next_node;
	}

	ilu_free(p_the_list);
}

/* returns size of the list */
static ilu_cardinal _ilu_list_size (_ilu_p_list p_the_list) {
	return p_the_list->m_card_num_added;
}

/* returns the contents of a node */
static ilu_refany _ilu_list_node_contents (_ilu_p_list_node p_the_node) {
	return p_the_node->m_p_node_contents;
}

/* returns the 'into' contents of a node */
static ilu_refany _ilu_list_node_into_contents (_ilu_p_list_node p_the_node) {
	return p_the_node->m_p_node_into_contents;
}

/* set the contents of a node */
static void _ilu_set_list_node_contents (_ilu_p_list_node p_the_node, ilu_refany p_new_value) {
	p_the_node->m_p_node_contents = p_new_value;
}

/* set the 'into' contents of a node */
static void _ilu_set_list_node_into_contents (_ilu_p_list_node p_the_node, ilu_refany p_new_value) {
	p_the_node->m_p_node_into_contents = p_new_value;
}


/* ********************************************************* */
/* type safe lists for htpp header lines */

static _http_p_header_list _http_make_list (ILU_ERRS((IoErrs)) *p_error) {
	return ((_http_p_header_list)_ilu_make_list(p_error));
}


static _http_p_list_node _http_add_to_list (_http_p_header_list p_the_list, 
											ilu_string p_node_contents,
											ilu_string p_node_into_contents,
											ILU_ERRS((IoErrs)) *p_error) {
	return ((_http_p_list_node)_ilu_add_to_list((_ilu_p_list)p_the_list,
				(ilu_refany)p_node_contents, (ilu_refany)p_node_into_contents, p_error));
}


static void _http_free_list_and_contents (_http_p_header_list p_the_list) {
	_ilu_free_list_and_contents((_ilu_p_list)p_the_list);
}


static ilu_cardinal _http_list_size (_http_p_header_list p_the_list) {
	return (_ilu_list_size((_ilu_p_list)p_the_list));
}


static ilu_string _http_list_node_contents (_http_p_list_node p_the_node) {
	return (ilu_string)(_ilu_list_node_contents((_ilu_p_list_node)p_the_node));
}

static ilu_string _http_list_node_into_contents (_http_p_list_node p_the_node) {
	return (ilu_string)(_ilu_list_node_into_contents((_ilu_p_list_node)p_the_node));
}

static void _http_set_list_node_contents (_http_p_list_node p_the_node, ilu_string p_new_value) {
	_ilu_set_list_node_contents((_ilu_p_list_node)p_the_node, (ilu_refany) p_new_value);
}

static void _http_set_list_node_into_contents (_http_p_list_node p_the_node, ilu_string p_new_value) {
	_ilu_set_list_node_into_contents((_ilu_p_list_node)p_the_node, (ilu_refany) p_new_value);
}


/* concatenates the pc_buffer onto the contents of the last node in the list */
static void _http_add_to_last_list_node(_http_p_header_list p_the_list, ilu_string pc_buffer, 
										ILU_ERRS((IoErrs)) *p_error) {

	ilu_cardinal card_into_offset;
	ilu_string pc_contents = p_the_list->m_p_last_list_node->m_p_node_contents;
	ilu_string pc_into = p_the_list->m_p_last_list_node->m_p_node_into_contents;
	ilu_cardinal pc_contents_length = strlen(pc_contents);
	ilu_cardinal pc_buffer_length = strlen(pc_buffer);

	if (pc_into != NIL)
		card_into_offset = pc_into - pc_contents;

	/* increase the size */
	pc_contents = ilu_ReallocE(pc_contents, pc_contents_length + pc_buffer_length + 1, p_error);
	if (ILU_ERRNOK(*p_error))
		return;

	p_the_list->m_p_last_list_node->m_p_node_contents = pc_contents;

	strcat(pc_contents, pc_buffer);  /* append the string */

	if (pc_into != NIL) /* reset the into pointer in case we're in a different spot in memory */
		p_the_list->m_p_last_list_node->m_p_node_into_contents = pc_contents + card_into_offset;
}



/* ********************************************************* */
/* Utility                                                   */
/* ********************************************************* */


/* ********************************************************* */
/* strtok_r'ish - written because visual c++ runtime doesn't have strtok_r 
pc_string_to_search is searched looking for the first token in it that
is deliminated by characters in pc_delim_chars.  The token is null terminated
and a pointer to where it starts returned.  *ppc_where_to_next_search
is set to where the search for the next token should begin.  NIL is returned
when there is nothing left to find. If p_the_delim is not NIL, then the
char where it points is set to the deliminator that was actually found - 
note that this can be '\0' when there were no more tokens.
*/

static char* _http_strtok_r (char* pc_string_to_search, char* pc_delim_chars, 
							 char** ppc_where_to_next_search, char* p_the_delim) {

	char* pc_start = pc_string_to_search;
	char* pc_end;

	if (!pc_string_to_search || !ppc_where_to_next_search || !pc_delim_chars)
		return NIL;

	while (*pc_start && strchr(pc_delim_chars, *pc_start) != NULL)
		pc_start++;

	if (*pc_start == '\0') /* nothing but delims */
		return NIL;

	pc_end = pc_start;	/* search for end */

	while (*pc_end && strchr(pc_delim_chars, *pc_end) == NULL)
		pc_end++;

	if (p_the_delim) /* make note of deliminator if we have a valid pointer */
		*p_the_delim = *pc_end;

	if (*pc_end == '\0')  /* reached end of string to search */
		*ppc_where_to_next_search = NIL;
	else {	/* null terminate and set where to start looking for the next token */
		*pc_end = '\0';
		*ppc_where_to_next_search = pc_end + 1;
	}

	return pc_start;
}


/* ********************************************************* */
/* returns the reason phrase associated with the status code */
static ilu_string _http_phrase_of_status_code(ilu_shortcardinal scard_enum_code) {

	switch (scard_enum_code) {
		case 200:   return "OK";
		case 201:   return "Created";
		case 202:   return "Accepted";
		case 204:   return "No Content";
		case 301:   return "Moved Permanently";
		case 302:   return "Moved Temporarily";
		case 304:   return "Not Modified";
		case 400:   return "Bad Request";
		case 401:   return "Unauthorized";
		case 403:   return "Forbidden";
		case 404:   return "Not Found";
		case 500:   return "Internal Server Error";
		case 501:   return "Not Implemented";
		case 502:   return "Bad Gateway";
		case 503:   return "Service Unavailable";
		default :	return "extension-code";
	}

}


/* ********************************************************* */
/* returns a pointer to the first occurrance of pc_substr in pc_to_search 
  assumes pc_to_search is at least as (validly) long as ul_search_length,
  and that pc_substr is NIL terminated. If b_case_sensitive is ilu_FALSE
  then for alphabetic characters, case doesn't matter in the comparison. */

static char* _http_strnstr ( ilu_bytes pc_to_search, 
							 char* pc_substr,
							 unsigned long ul_search_length,
							 ilu_boolean b_case_sensitive) {

	ilu_bytes pc_search_walker;
	ilu_bytes pc_search_runner;
	ilu_bytes pc_substr_runner;
	unsigned long ul_last_position;
	unsigned long ul_substr_length = strlen(pc_substr);

	if ((pc_to_search == NIL) || (ul_substr_length > ul_search_length))
		return (NIL); /* substring couldn't possibly fit */

	/* try each position along the string to search in turn */
	pc_search_walker = pc_to_search;
	for (ul_last_position = ul_search_length - ul_substr_length + 1;
		 ul_last_position > 0; 
		 ul_last_position--) {

		/* at this position, see if there's a match */
		pc_search_runner = pc_search_walker;
		pc_substr_runner = (ilu_bytes) pc_substr;

		if (b_case_sensitive == ilu_TRUE) {
			while (*pc_substr_runner) { 
				if (*pc_substr_runner != *pc_search_runner)
					break;
				pc_search_runner++;
				pc_substr_runner++;
			}
		}
		else {
			while (*pc_substr_runner) { 
				if (isalpha(*pc_substr_runner) && isalpha(*pc_search_runner) &&
					(tolower(*pc_substr_runner) == tolower(*pc_search_runner))) ;
				else if (*pc_substr_runner == *pc_search_runner) ;
				else break;
			pc_search_runner++;
			pc_substr_runner++;
			}
		}

		/* if we reached the end - they must be equal */
		if (!*pc_substr_runner) 
			return((char*)pc_search_walker);

		pc_search_walker++;
	}

	return(NIL);
}



/* ********************************************************* */
/* inefficient way to read in a line but necessary since
   there's no way to know in advance how much a browser will be sending
   in its request.  We cant take a larger grained 'chunking' approach 
   because we could end up trying to read more than what was
   sent, and hang there since we don't get an eof because the connection 
   is still open - sigh...           */
/* Returns a null terminated buffer containing all the bytes from the
call's transport upto but not including the next end of line encountered.  
The end of line is discarded;  NIL is returned on any error and p_error may be set
accordingly */

static ilu_string _http_readline(ilu_Call p_call, ILU_ERRS((IoErrs)) *p_error) {

	ilu_cardinal card_num_chars_read;
	ilu_cardinal card_space_left;
	ilu_string pc_buffer;
	ilu_string pc_newbuffer;
	ilu_string pc_where;
	ilu_boolean b_found_cr = ilu_FALSE;
	ilu_Transport p_transport = p_call->ca_prTrans;

	/* allocate some initial buffer space */
	if ((pc_buffer = (ilu_string) ilu_MallocE( HTTP_READLINE_ALLOC_SIZE, p_error))
			== NIL)
		return NIL;

	card_num_chars_read = 0;
	pc_where = pc_buffer;
	card_space_left = HTTP_READLINE_ALLOC_SIZE;

	while (1) {	/* loop reading chars, looking for end of line */

		if (card_space_left == 0) { /* if we're out of space, grow */
			pc_newbuffer = ilu_ReallocE(pc_buffer, 
				card_num_chars_read + HTTP_READLINE_ALLOC_SIZE, p_error);
			if (pc_newbuffer == NIL) {
				ilu_free(pc_buffer);
				return NIL;
			}
			else { 
				pc_buffer = pc_newbuffer;
				pc_where = pc_buffer + card_num_chars_read;
				card_space_left = HTTP_READLINE_ALLOC_SIZE;
			}
		}

		/* get our single byte */
		_http_transport_read_bytes(p_transport, pc_where, 1, p_error);
		if (ILU_ERRNOK(*p_error)) {
			ilu_free(pc_buffer);
			return NIL;
		}

		card_space_left--;		/* adjust counts */
		card_num_chars_read++;

#ifdef HTTP_STRICT_END_OF_LINE

		/* end of line is a CRLF */
		switch (*pc_where) {

		case '\r':
			b_found_cr = ilu_TRUE;
			break;

		case '\n':
			if (b_found_cr != ilu_TRUE)
				break;

			/* found crlf, null terminate */ 
			*(pc_where - 1) = '\0';

			/* shrink down to just enough size to hold line */
			pc_newbuffer = ilu_ReallocE(pc_buffer, card_num_chars_read - 1 , p_error);
			if (pc_newbuffer == NIL) {
				ilu_free(pc_buffer);
				return NIL;
			}
			else 
				return pc_newbuffer;

		default:
			b_found_cr = ilu_FALSE;
		}

#else
		/* allow just a lf to indicate end of line ( to accomodate some http servers) */

		if (*pc_where == '\n') { /* found end of line */

			if ((pc_where != pc_buffer) && (*(pc_where - 1) == '\r')) {
				/* drop back to any any preceeding CR */
				card_num_chars_read--;
				pc_where--;
			}

			*pc_where = '\0';	/* null terminate */

			/* shrink down to just enough size to hold line */
			pc_newbuffer = ilu_ReallocE(pc_buffer, card_num_chars_read , p_error);
			if (pc_newbuffer == NIL) {
				ilu_free(pc_buffer);
				return NIL;
			}
			else 
				return pc_newbuffer;
		}

#endif 

		pc_where++;
	}
}


/* ********************************************************* */
/* Put the request line into the m_pc_request_or_status_line
   in the call's _http_call_info_s structure, and fill in all the
   other parts of the _http_call_info_s structure that can be 
   deduced from it, i.e. method name, object id and version.
   Returns ilu_TRUE if there were no errors, else false.  
   errors may be set if they occur */

static ilu_boolean _http_get_request_line(ilu_Call p_call, 
											ILU_ERRS((IoErrs)) *p_error) {

	ilu_string pc_next_token;
	char c_delim_found;

	/* read in a line */
	if ((_http_request_line(p_call) = _http_readline(p_call, p_error)) == NIL)
		goto bad_request_line;

	/* get the method name */
	if ((_http_method_name(p_call) = 
		_http_strtok_r(_http_request_line(p_call), g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_request_line;

	/* make note if it's a HEAD method */
	if (strcmp("HEAD", _http_method_name(p_call)) == 0)
		_http_is_HEAD_method(p_call) = ilu_TRUE;

	/* get the object id */
	if ((_http_object_id(p_call) = 
		_http_strtok_r(pc_next_token, g_c_objid_ends, &pc_next_token, &c_delim_found)) == NIL)
		goto bad_request_line;

	if ((c_delim_found == ';') || (c_delim_found == '?')) {
		/* the deliminator indicates that there were some params or queries */

		/* save what the found delimator was - we'll need it later when we 
		reconstruct the RequestURI part of the Request */
		_http_params_queries_delim(p_call) = c_delim_found;

		/* get the param or queries */
		if ((_http_params_queries(p_call) = 
			_http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL)) == NIL)
			goto bad_request_line;
	}

	/* get the version */
	if ((_http_version_string(p_call) = 
		_http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_request_line;

	/* check for valid version */
	if ((sscanf(_http_version_string(p_call), "HTTP/%hu.%hu", &(_http_major_version(p_call)), 
				&(_http_minor_version(p_call))) != 2) ||
		(_http_major_version(p_call) != HTTPMajorVersion) || 
		(_http_minor_version(p_call) != HTTPMinorVersion ))
		return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_versionMismatch, ilu_FALSE);

	return ilu_TRUE;


	bad_request_line:

	if (ILU_ERRNOK(*p_error)) /* return any already set error */
		return ilu_FALSE;

	return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_msgTypeUnknown, ilu_FALSE);
}



/* ********************************************************* */
/* Put the status line into the m_pc_request_or_status_line
   in the call's _http_call_info_s structure, and fill in all the
   other parts of the _http_call_info_s structure that can be 
   deduced from it, i.e. statuscode and version.
   Returns ilu_TRUE if the line was not a 'simple-status',
   else false.  errors may be set if they occur */

static ilu_boolean _http_get_status_line(ilu_Call p_call, 
											ILU_ERRS((IoErrs)) *p_error) {

	ilu_string pc_next_token;

	/* read in a line */
	if ((_http_status_line(p_call) = _http_readline(p_call, p_error)) == NIL)
		goto bad_status_line;

	/* get the version string */
	if ((_http_version_string(p_call) = 
		_http_strtok_r(_http_status_line(p_call), g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_status_line;

	/* check for valid version */
	if ((sscanf(_http_version_string(p_call), "HTTP/%hu.%hu", &(_http_major_version(p_call)), 
				&(_http_minor_version(p_call))) != 2) ||
		(_http_major_version(p_call) != HTTPMajorVersion) || 
		(_http_minor_version(p_call) != HTTPMinorVersion ))
		return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_versionMismatch, ilu_FALSE);
	
	/* get the status code string */
	if (( _http_status_code_string(p_call) = 
		_http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL)) == NIL)
		goto bad_status_line;

	/* assign the status code */
	if (sscanf(_http_status_code_string(p_call), "%hd", &(_http_status_code(p_call))) != 1)
		goto bad_status_line;

	/* get the reason phrase */
	_http_version_reason_phrase(p_call) = _http_strtok_r(pc_next_token, g_c_White, &pc_next_token, NIL);

	return ilu_TRUE;


	bad_status_line:

	if (ILU_ERRNOK(*p_error)) /* return any already set error */
		return ilu_FALSE;

	return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_msgTypeUnknown, ilu_FALSE);
}



/* ********************************************************* */
/* Put all the incoming headers of a request or reply into the 
   _http_p_header_list in the call's _http_call_info_s structure.
   Also sets content length and server id if these headers are present;
   Returns number of headers */

static ilu_cardinal _http_fill_headers_list(ilu_Call p_call, 
											ILU_ERRS((IoErrs)) *p_error) {

	ilu_string pc_buffer;
	ilu_string pc_value_start;
	ilu_string pc_next_token;
	
	if ((_http_headers_list(p_call) = _http_make_list(p_error)) == NIL)
			return 0;

	while (1) {
			
		/* read in a line */
		if ((pc_buffer = _http_readline(p_call, p_error)) == NIL)
			goto bad_header;

		if (*pc_buffer == '\0') { /* if we hit the body 
			(note - transport is now ready to read the first byte of the body if any) */
			ilu_free(pc_buffer);
			_http_headers_current_node(p_call) = _http_headers_list(p_call)->m_p_start_list_node;
			return _http_list_size(_http_headers_list(p_call));
		}
		
		if (*pc_buffer == ' ' || *pc_buffer == '\t') { 
			/* must be continuation of previous header's value */
			_http_add_to_last_list_node(_http_headers_list(p_call), pc_buffer, p_error);
			if (ILU_ERRNOK(*p_error)) 
				goto bad_header;
			continue;
		}

		/* null terminate the name  */
		if (_http_strtok_r(pc_buffer, g_c_NameValueDelims, &pc_next_token, NIL) == NIL)
			goto bad_header;

		/* find the value  */
		pc_value_start = pc_next_token;
		skipwhite(pc_value_start);

		/* see if the header is anything we want to take note of */
		if (strcmp(pc_buffer, ILU_SERVER_ID_HEADER_NAME) == 0)
			_http_server_id(p_call) = pc_value_start;

		else 
			if (_http_strnstr((ilu_bytes)pc_buffer, "Content-Length", strlen(pc_buffer), ilu_FALSE) != NULL) {

				if (sscanf(pc_value_start, "%lu", &(_http_body_length(p_call))) != 1) {
					_http_body_length(p_call) = 0;
					goto bad_header;
				}
			}

		/* stick the header into the list */
		_http_add_to_list(_http_headers_list(p_call), pc_buffer, pc_value_start, p_error);
		if (ILU_ERRNOK(*p_error)) 
			goto bad_header;
	}

	bad_header :

	if (pc_buffer)				/* cleanup */
		ilu_free(pc_buffer);

	if (ILU_ERRNOK(*p_error)) /* return any already set error */
		return _http_list_size(_http_headers_list(p_call));

	return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_msgTypeUnknown, 
		_http_list_size(_http_headers_list(p_call)));
}


/* ********************************************************* */
/* writes a cardinal out followed by a a crlf          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_write_cardinal_line(ilu_Call p_call, 
									   ilu_cardinal card_to_write, 
									   ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [16];

	ILU_CLER(*p_error);

	/* write out the cardinal value */
	sprintf(c_buffer, "%lu\r\n", card_to_write);
	_http_transport_write_bytes(p_call->ca_prTrans, (ilu_bytes) c_buffer, strlen(c_buffer), p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* reads a line and sets a cardinal based on it's contents          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_read_cardinal_line(ilu_Call p_call, 
									   ilu_cardinal* p_card_to_set, 
									   ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, p_error)) == NIL)
		return ilu_FALSE;

	/* readin the cardinal value */
	if (sscanf(pc_buffer, "%lu\r\n", p_card_to_set) != 1) {
		ilu_free(pc_buffer);
		return ilu_FALSE;
	}

	ilu_free(pc_buffer);
	return ilu_TRUE;
}


/* ********************************************************* */
/* writes a integer out followed by a a crlf          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_write_integer_line(ilu_Call p_call, 
									   ilu_integer integer_to_write, 
									   ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [16];

	ILU_CLER(*p_error);

	/* write out the integer value */
	sprintf(c_buffer, "%ld\r\n", integer_to_write);
	_http_transport_write_bytes(p_call->ca_prTrans, (ilu_bytes) c_buffer, strlen(c_buffer), p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* reads a line and sets a integer based on it's contents          
   returns ilu_TRUE on success, else ilu_FALSE               */

static ilu_boolean _http_read_integer_line(ilu_Call p_call, 
									   ilu_integer* p_integer_to_set, 
									   ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, p_error)) == NIL)
		return ilu_FALSE;

	/* readin the integer value */
	if (sscanf(pc_buffer, "%ld\r\n", p_integer_to_set) != 1) {
		ilu_free(pc_buffer);
		return ilu_FALSE;
	}

	ilu_free(pc_buffer);
	return ilu_TRUE;
}



/* ********************************************************* */
/* Uses p_transport to read bytes in chunks of card_chunk_size, to into a buffer 
pointed to by * ppc_buffer_loc, until at least pc_termination_string is seen.
NIL is put into the buffer to terminate where the pc_termination_string was found.
If card_num_initial_bytes is non zero, then pc_initial_bytes indicates card_num_initial_bytes
initial bytes to treat as if they were read from the transport first.  If
p_card_leftover_length is non zero, ppc_leftover_start points to p_card_leftover_length
bytes that were read past the termination string . If b_read_till_eof is true, 
pc_termination_string is ignored and eof is considered to be the stopping condition.
*p_card_total_read gets set to num bytes in allocated buffer before termination reached.
ilu_FALSE is returned if any error (except eof) is encountered.

ilu_Transport p_transport,		- pointer to the transport from which to read bytes
char** ppc_buffer_loc,			- where to put the address of the allocated buffer
ilu_cardinal card_chunk_size,	- size of allocations to use
ilu_bytes pc_termination_string,		- what denotes the end of the thing we're trying to get
ilu_bytes pc_initial_bytes,		- some initial bytes to put in
ilu_cardinal card_num_initial_bytes,	- how many initial bytes there are
ilu_bytes* ppc_leftover_start,			- where to set pointer to start of any leftover bytes from the read
ilu_cardinal* p_card_leftover_length,	- where to write how many leftover bytes there are
ilu_boolean b_read_till_eof,			- if true, igores pc_termination_string and considers eof to be the stopping condition
ilu_cardinal* p_card_total_read			- get set to num bytes in allocated buffer before termination reached
ILU_ERRS((IoErrs)) *p_error)		- set if an error

*/

static ilu_boolean 
_http_fill_buffer_till (ilu_Transport p_transport,
						ilu_bytes* ppc_buffer_loc,
						ilu_cardinal card_chunk_size,
						ilu_string pc_termination_string,
						ilu_bytes pc_initial_bytes,
						ilu_cardinal card_num_initial_bytes,
						ilu_bytes* ppc_leftover_start,
						ilu_cardinal* p_card_leftover_length,
						ilu_boolean b_read_till_eof,
						ilu_cardinal* p_card_total_read,
						ILU_ERRS((IoErrs)) *p_error) {

	ilu_bytes pc_end;				/* points at end of what we're looking for */
	ilu_bytes pc_where;				/* where we're beginning search for pc_termination_string*/
	ilu_cardinal card_num_read;		/* how may bytes the transport read returned */
	ilu_cardinal card_where_offset;	/* offset from start of buffer where we search */
	ilu_cardinal card_term_str_length;	/* length of termination string */
	ilu_cardinal card_last_chunk_size;	/* size of last allocation - to deal with initial bytes*/
	ilu_boolean b_hit_eof = ilu_FALSE;	/* true if we hit the end of file */

	ILU_CLER(*p_error);

	*p_card_total_read = 0;		/* nothing read so far */

	/* get some initial  memory */
	if (card_num_initial_bytes > 0) {
		if (((*ppc_buffer_loc) = 
			(ilu_bytes) ilu_MallocE(card_num_initial_bytes, p_error)) == NIL)
			return ilu_FALSE; /* no memory */
		memcpy((void *) *ppc_buffer_loc, (void *) pc_initial_bytes, card_num_initial_bytes);
		card_last_chunk_size = card_num_initial_bytes;
	}
	else {
		if (((*ppc_buffer_loc) = 
			(ilu_bytes) ilu_MallocE(card_chunk_size, p_error)) == NIL)
			return ilu_FALSE; /* no memory */
		card_last_chunk_size = card_chunk_size;
	}

	card_term_str_length = strlen(pc_termination_string);

	pc_where = (*ppc_buffer_loc);	/* init our search pointer */
	while (ilu_TRUE) { /* keep looking for pc_termination_string */

		/* read in chunk  */
		if (card_num_initial_bytes == 0) {
			/* we're done dealing with any initial bytes */
			card_num_read = _http_transport_read_bytes(p_transport, pc_where, 
				card_chunk_size, p_error);
		}
		else {
			card_num_read = card_num_initial_bytes;
			card_num_initial_bytes = 0;
		}

		*p_card_total_read = *p_card_total_read + card_num_read; /* add to count of bytes read */
		
		ILU_ERR_SWITCH(*p_error) {
			ILU_SUCCESS_CASE ;
			ILU_ERR_CASE(comm_failure, p_the_err_struct) {
				if (p_the_err_struct->minor == ilu_cfm_eof)
						b_hit_eof = ilu_TRUE;
				else return ilu_FALSE;
			}
			ILU_ERR_ELSE
				 /* had a non eof problem reading, we're just hosed */
				return ilu_FALSE;
		} ILU_ERR_ENDSWITCH;

		/* see if there was a pc_termination_string in what we read */
		if (b_read_till_eof == ilu_TRUE)
			pc_end = NIL;
		else 
			pc_end = (ilu_bytes)_http_strnstr ( pc_where, pc_termination_string, card_num_read, ilu_TRUE );
							 
		if (pc_end != NIL) {	/* found it - no need to read in any more */

			*pc_end = '\0'; /* NIL terminate where the pc_termination_string was found */

			 /* leftovers start just after the pc_termination_string */
			*ppc_leftover_start = pc_end + card_term_str_length;

			/* and we have this many bytes of them */
			*p_card_leftover_length = card_num_read - 
				(pc_end + card_term_str_length - pc_where);

			/* reset number read to reflect count of bytes before termination string */
			*p_card_total_read = pc_end - (*ppc_buffer_loc);

			/* clear any eof error since we got what we were after */
			if (b_hit_eof == ilu_TRUE)	
				ILU_CLER(*p_error);

			return ilu_TRUE;
		}
		else if (b_hit_eof == ilu_TRUE) { 
			if (b_read_till_eof == ilu_TRUE) {
				/* we were supposed to read to eof, so things are cool */
				ILU_CLER(*p_error);
				return ilu_TRUE;
			}
			/* hit end of file and still haven't found it */
			else
				return ilu_FALSE;
		}
		else {	/* haven't got all we need yet */ 

			/* determine how far into what we've read so far we've searched */
			card_where_offset = pc_where + card_last_chunk_size -
				(*ppc_buffer_loc);

			/* increase our buffer size by another chunk */
			if (((*ppc_buffer_loc) = 
				(ilu_bytes) ilu_ReallocE((*ppc_buffer_loc),
					card_where_offset + card_chunk_size, p_error)) == NIL)
				return ilu_FALSE; /* no memory */

			card_last_chunk_size = card_chunk_size; /* for initial bytes case */

			/* set where we are to continue searching */
			pc_where = (*ppc_buffer_loc) + card_where_offset;
		}
	}
}


/* ********************************************************* */
/* returns the method from the root class that has the given 
   name (if any)  */

static ilu_Method _http_root_method_from_name (ilu_string pc_the_method_name) {

	ilu_cardinal card_method_index;
	ilu_Method p_the_method;

	/* search internal (ilu_rootClass) methods */
	p_the_method = class_methods(ilu_rootClass);

	for(card_method_index = 0; card_method_index < class_method_count(ilu_rootClass);
		card_method_index++) {

		if (strcmp(method_name(p_the_method), pc_the_method_name) == 0) 
			return p_the_method;

		p_the_method++;
	}

	return NIL;
}

	 
/* ********************************************************* */
/* returns the method and sets class that has the given method name */

 static ilu_Method _http_inherited_method_from_name (ilu_Class* pp_class, ilu_string pc_the_method_name) {

	ilu_cardinal card_method_index;
	ilu_Method p_the_method;
	ilu_cardinal card_index_super_classes;
	ilu_cardinal card_super_classes_count;
	ilu_Class* pp_super_class;

	/* search this class */
	p_the_method = class_methods(*pp_class);
	for(card_method_index = 0; card_method_index < class_method_count(*pp_class);
		card_method_index++) {

		if (strcmp(method_name(p_the_method), pc_the_method_name) == 0) 
			return p_the_method;

	p_the_method++;
	}

	/* now (recursively depth first) search the super classes */
	pp_super_class = class_superclasses(*pp_class);
	card_super_classes_count = class_superclass_count(*pp_class);
	for (card_index_super_classes = 0; 
		 card_index_super_classes < card_super_classes_count;
		 card_index_super_classes++) {

		/* set the class to where to search next */
		*pp_class = *pp_super_class;
		p_the_method = _http_inherited_method_from_name(pp_class, pc_the_method_name);

		if (p_the_method != NIL) 
			return p_the_method;

		pp_super_class++; /* advance to next super class */
	}


	/* didn't find one */
	return NIL;
}


/* ********************************************************* */
/* returns the method from the root or class that has the given name */

 static ilu_Method _http_method_from_name (ilu_Class* pp_class, ilu_string pc_the_method_name) {

	ilu_Method p_the_method;

	/* search internal (ilu_rootClass) methods */
	p_the_method = _http_root_method_from_name(pc_the_method_name);
	if (p_the_method != NIL) {
		*pp_class = ilu_rootClass;
		return p_the_method;
	}

	/* search inherited methods */
	return _http_inherited_method_from_name(pp_class, pc_the_method_name);
 }


/* ********************************************************* */
/* returns a string that's used to form the protocol info part of a 
   string binding handle */

static ilu_string _http_form_protocol_handle (ilu_Object obj) {

	char buf[32];
	ilu_Error p_error;
	ilu_string pc_protohandle;

	sprintf (buf, "http_%hu_%hu", HTTPMajorVersion, HTTPMinorVersion);
	pc_protohandle = ilu_StrdupE(buf, &p_error);

	if (ILU_ERRNOK(p_error)) {
		fprintf(stderr, "_http_form_protocol_handle couldn't ilu_StrdupE\n");
		pc_protohandle = ILU_NIL;
	}

	return (pc_protohandle); 
}
     
/* ********************************************************* */
/* create and free a http data block - contains any (member-var like)
   attributes specific to the protocol - currently specifies 
   batching and concurrency but batching probably really doesn't 
   apply for http                                            */

static ilu_refany _http_create_non_batching_non_concurrent_data_block(ILU_ERRS((no_memory)) * p_error) {

	http_members_s* p_new_member_struct = 
		( http_members_s *) ilu_MallocE(sizeof(http_members_s), p_error);
	if (ILU_ERRNOK(*p_error))
	return NIL;

	p_new_member_struct->m_b_batching = ilu_FALSE;
	p_new_member_struct->m_b_concurrent = ilu_FALSE;
	return ((ilu_refany) p_new_member_struct);
}


static void _http_free_data_block (http_members_s* p_httpmemberdatablock) {
	ilu_free(p_httpmemberdatablock);
}


/* ********************************************************* */
/* returns true if the method name is one of the http1.0 method names 
- i.e. GET, HEAD or POST  */

static ilu_boolean _http_is_http_method_name(ilu_string pc_method_name) {

	return ((strcmp("GET", pc_method_name) == 0)  ||
			(strcmp("HEAD", pc_method_name) == 0) ||
			(strcmp("POST", pc_method_name) == 0));
}


/* ********************************************************* */
/* returns ilu_TRUE if the call is on an object of HTTP_RESOURCE_OBJECT_TYPE_ID
or one of its subtypes, and the method is a normal http1.0 method */

static ilu_boolean _http_is_http_resource_object_and_method (ilu_Call p_call) {

	/* get the classes of the object involved and the class with the 
	   id HTTP_RESOURCE_OBJECT_TYPE_ID */
	ilu_Class p_called_object_class = call_intro_type(p_call);
	ilu_Class p_http_resource_object_class = 
		ilu_FindClassFromID(HTTP_RESOURCE_OBJECT_TYPE_ID);

	if (!p_http_resource_object_class || !p_called_object_class)
		return ilu_FALSE;

	/* ilu_IsSubObjectType Returns TRUE iff p_called_object_class is a subtype 
	of p_http_resource_object_class (including the degenerate case of when 
	they're the same). */

	return (ilu_IsSubObjectType(p_called_object_class, p_http_resource_object_class) &&
			_http_is_http_method_name(method_name(call_method(p_call))));
}



/* ********************************************************* */
/* Call initialization and finialization                     */
/* ********************************************************* */


/* Perform any protocol specific initialization              */

static ilu_boolean _http_init_call(ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {

	http_call_info_s* p_http_call_info_s;

	ILU_CLER(*p_error);

	/* create a new call specific information structure and hang it off the call */
	p_http_call_info_s = 
		( http_call_info_s *) ilu_MallocE(sizeof(http_call_info_s), p_error);
	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	/* hang the new call specific information structure off the call */
	p_call->ca_prdata2 = p_http_call_info_s;

	/* set the transport used for this call */
	p_call->ca_prTrans = connection_transport(p_call->ca_connection);


	if (!call_incoming(p_call)) { /* outgoing call */

		if (_http_is_http_resource_object_and_method (p_call)) {
			/* this is a http1.0 call on a HTTP_RESOURCE_OBJECT */
			/* set to next state accordingly */
			_http_call_type(p_call) = clnt2http;
			_http_call_state(p_call) = clnt2http_out_method_name;
		}
		else {
			/* this is not a http1.0 call on a HTTP_RESOURCE_OBJECT we use 
			   general purpose marshalling */
			_http_call_type(p_call) = clnt2ilu;
			_http_call_state(p_call) = clnt2ilu_out_method_name;
		}
	}
	else { 
		/* don't yet have a way of knowing what type the object is */
		/* so just set to next state */
		_http_call_type(p_call) = srvr4something;
		_http_call_state(p_call) = srvr_in_read_header;
	}

	/* init to no buffers, etc. */
	_http_num_headers_to_process(p_call) = 0;
	_http_is_HEAD_method(p_call) = ilu_FALSE;
	_http_user_supplied_content_length(p_call) = ilu_FALSE;
	_http_headers_list(p_call) = NIL;
	_http_headers_current_node(p_call) = NIL;
	_http_status_line(p_call) = NIL;
	_http_method_name(p_call) = NIL;	
	_http_object_id(p_call) = NIL;
	_http_params_queries(p_call) = NIL;
	_http_params_queries_delim(p_call) = '\0';
	_http_server_id(p_call) = NIL;		
	_http_version_string(p_call) = NIL;		
	_http_status_code_string(p_call) = NIL;	
	_http_version_reason_phrase(p_call) = NIL;	
	_http_major_version(p_call) = 0;
	_http_minor_version(p_call) = 0;
	_http_status_code(p_call) = 0;
	_http_body_length(p_call) = 0;

	return ilu_TRUE;
}


/* ********************************************************* */
/* frees up a calls's data structures                       */

static void _http_freeup_call (ilu_Call p_call) {

	/* delete any buffers used for incoming bytes, etc. */
	ilu_free(_http_status_line(p_call));

	/* free any list we may have used */
	if (_http_headers_list(p_call) != NIL)
		_http_free_list_and_contents(_http_headers_list(p_call));

	/* delete our call private structure */
	ilu_free(p_call->ca_prdata2);
}


/* ********************************************************* */
/* pre finishes a call                                       */

static  ilu_boolean _http_prefinish_call (ilu_Call p_call,
		      ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);


	switch (_http_call_state(p_call)) {

	case clnt2http_finish_call:
	case srvr4http_finish_call:

		/* close the call's connection so we don't try to reuse it (since
		the httpd on the other end will have closed as per current practice,
		resulting in an	unusable connection). (or we are acting as the httpd
		and need to close it per current practice) */
		_ilu_CloseIoingConnection(call_connection(p_call), ilu_FALSE);
		break;

	case clnt2ilu_finish_call:

		break;

	case srvr4ilu_finish_call:

		break;

	default:
		/* somthing really bad must have happened to get us here prematurely, so
		blow everything away to be on the safe side */

		/* close the call's connection so we don't try to reuse it (since
		the httpd on the other end will have closed as per current practice,
		resulting in an	unusable connection). (or we are acting as the httpd
		and need to close it per current practice) */
		_ilu_CloseIoingConnection(call_connection(p_call), ilu_FALSE);
		break;

	}

	return ilu_TRUE;
}


/* ********************************************************* */
/* finishes a call                                           */

static  ilu_boolean _http_finish_call (ilu_Call p_call,
		      ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_finish_call:
	case srvr4http_finish_call:
	case clnt2ilu_finish_call:
	case srvr4ilu_finish_call:
	default:
		/* free up the data structures, etc. used for this call */
		_http_freeup_call (p_call);

		break;

	}

	return ilu_TRUE;
}



/* ********************************************************* */
/* reading headers                                           */
/* ********************************************************* */

/* ********************************************************* */
/* reads an incoming header                                  */

static ilu_ReadHeaderResultCode _http_read_header 
	(ilu_Call p_call, ilu_PacketType* p_packetType,
		 ilu_cardinal* p_card_packetSN, ILU_ERRS((IoErrs)) * p_error) {
	
	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) { 

	case clnt2http_in_read_reply_header:

		/* read in status line and any headers */
		if (_http_get_status_line(p_call, p_error) == ilu_FALSE)
			goto reading_error_label;

		_http_fill_headers_list(p_call, p_error);
		if (ILU_ERRNOK(*p_error))
			goto reading_error_label;

		/* since we don't allow multiple calls over a connection, the
		serial number must be the same as the calls. [we really have
		no choice since a httpd knows nothing about serial numbers ] */
		*p_card_packetSN = call_serial_number(p_call);

		/* can only get replies from an httpd */
		*p_packetType = ilu_PacketType_Reply;

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_interpret_reply;
			
		break;

	case clnt2ilu_in_read_reply_header:

		/* read in status line and any headers */
		if (_http_get_status_line(p_call, p_error) == ilu_FALSE)
			goto reading_error_label;

		_http_fill_headers_list(p_call, p_error);
		if (ILU_ERRNOK(*p_error))
			goto reading_error_label;

		/* since we don't allow multiple calls over a connection, the
		serial number must be the same as the calls. [we really have
		no choice since a httpd knows nothing about serial numbers ] */
		*p_card_packetSN = call_serial_number(p_call);

		/* can only get replies from an httpd */
		*p_packetType = ilu_PacketType_Reply;

		/* set to next state */
		_http_call_state(p_call) = clnt2ilu_in_interpret_reply;

		break;

	case srvr_in_read_header:

		/* read in request line and any headers */
		if (_http_get_request_line(p_call, p_error) == ilu_FALSE)
			goto reading_error_label;

		_http_fill_headers_list(p_call, p_error);
		if (ILU_ERRNOK(*p_error))
			goto reading_error_label;

		/* we have no explicit packet types or serial numbers - so just set to request because of 
		how control flow got us here, fill in serial number for the heck of it */
		*p_packetType = ilu_PacketType_Request;
		*p_card_packetSN = 0;

		/* set to next state (if there's a server id, then it must be a general ilu call) */
		/* Any call that comes from ilu and isn't a GET HEAD or POST call on an object that 
		is of HTTP_RESOURCE_OBJECT_TYPE will have a server id header*/
		if (_http_server_id(p_call) == NIL) {
			_http_call_type(p_call) = srvr4http;
			_http_call_state(p_call) = srvr4http_in_interpret_request;
		}
		else {
			_http_call_type(p_call) = srvr4ilu;
			_http_call_state(p_call) = srvr4ilu_in_interpret_request;
		}

		break;

	default:

		/* we may have got here due to activity (e.g. a close) on a outgoing 
		connection e.g. via ReadExtraMsg */
		switch (_http_call_type(p_call)) {

		case clnt2http:
		case clnt2ilu: /* activity must be because connection closed or if some wacko decides to 
						send stuff on this outgoing (from us) connection that isn't a reply, return 
						that we're at end of file */
			return ilu_rhrc_eof;

		default:
			_ilu_Assert(ilu_FALSE, g_c_bad_state);
		}
	}

	return ilu_rhrc_ok;

	reading_error_label:

	/* if we had an error and it's due to eof, clear the error and return ilu_rhrc_eof 
	This is because _http_read_header can be called when some activity occurs on the 
	connection - one of these kinds of activity is closing, which is really not an error */
	ILU_ERR_SWITCH(*p_error) {
		ILU_SUCCESS_CASE return ilu_rhrc_ok;
		ILU_ERR_CASE(comm_failure, p_the_err_struct) {
			if (p_the_err_struct->minor == ilu_cfm_eof) {
					ILU_CLER(*p_error);
					return ilu_rhrc_eof;
			}
			else return ilu_rhrc_error;
		}
		ILU_ERR_ELSE
			 /* had a non eof problem reading, we're just hosed */
			return ilu_rhrc_error;
	} ILU_ERR_ENDSWITCH;

}


/* ********************************************************* */
/* request initialization and finialization, and interpretation */
/* ********************************************************* */


/* ********************************************************* */
/* start actually sending a request (puts out method name SP)*/

static ilu_boolean _http_start_request (ilu_Call p_call, 
									   ilu_cardinal card_arg_size, 
									   ILU_ERRS((IoErrs)) * p_error) {
	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_out_method_name:

		/* if it's a HEAD method, then make note of it so that when we get
		back the response headers, we'll know that the Content-Length field
		is to be ignored when it comes to reading in any entity body */
		if (strcmp("HEAD", method_name(call_method(p_call))) == 0) 
			_http_is_HEAD_method(p_call) = ilu_TRUE;	
		else 
			_http_is_HEAD_method(p_call) = ilu_FALSE;
		
		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_URL_path;
			
		break;

	case clnt2ilu_out_method_name:
		/* set to next state */
		_http_call_state(p_call) = clnt2ilu_out_object_id;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

	/* have the transport write out the method name*/
	_http_transport_write_bytes(p_call->ca_prTrans, method_name(call_method(p_call)), 
		strlen(method_name(call_method(p_call))), p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	/* have the transport write out a space */
	_http_transport_write_bytes(p_call->ca_prTrans, g_c_SP, 1, p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* finishes sending a request                                */

static  ilu_boolean _http_finish_request(ilu_Call p_call, ilu_Message * p_msg,
		      ILU_ERRS((IoErrs)) * p_error) {

	ilu_Transport the_transport = p_call->ca_prTrans;
	ilu_TransportClass the_transport_class = the_transport->tr_class;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_out_finish_request:
		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_read_reply_header;
		break;

	case clnt2ilu_out_arguments:
		/* set to next state */
		_http_call_state(p_call) = clnt2ilu_in_read_reply_header;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

	/* have the transport flush itself */
	(*the_transport_class->tc_write_bytes) 
				(the_transport, (ilu_bytes)"", 0, TRUE, p_error);

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;

	return ilu_TRUE;
}



/* ********************************************************* */
/* 	interpret an incoming request                            */

static ilu_boolean _http_interpret_request (ilu_Call p_call, 
											ILU_ERRS((IoErrs)) * p_error) {

	ilu_Object p_the_object;
	ilu_Class p_the_class;
	ilu_Method p_the_method;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case srvr4http_in_interpret_request:
		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_server_id;
		break;

	case srvr4ilu_in_interpret_request:

		if ((strcmp(_http_server_id(p_call), server_id(call_server(p_call))) != 0)) 
		/* we're in the wrong server ! */
		return ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_alien_disc, ilu_FALSE);

		/* set to next state */
		_http_call_state(p_call) = srvr4ilu_in_server_id;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

	/* find the object for this object id in the server -
	use ilu_rootClass to enter server since we have no idea at this
	point what the class is.  XXX eventually for ilu-ilu situations - have
	client send information about the object type */
	ilu_EnterServer(call_server(p_call), ilu_rootClass);
	if ((p_the_object = _ilu_FindObjectInServer(_http_object_id(p_call), 
			call_server(p_call))) == NIL) {
		ilu_ExitServer(call_server(p_call), ilu_rootClass);
		return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, ilu_FALSE);
	}
	ilu_ExitServer(call_server(p_call), ilu_rootClass);

	/* get and set the object's (most specific) class */
	p_the_class = object_class(p_the_object);
	call_intro_type(p_call) = p_the_class;

	/* get and set the method and class (potentially a superclass) with the given name */
	if ((p_the_method = _http_method_from_name(&(call_intro_type(p_call)), _http_method_name(p_call)))
			== NIL)
		return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, ilu_FALSE);
	call_method(p_call) = p_the_method;

	return ilu_TRUE; 
}


/* ********************************************************* */
/* Do anything that needs to be done after 
   arguments have been unmarshalled */

static void _http_request_read (ilu_Call p_call,
				    ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case srvr4http_in_request_read:

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_begin_reply;
		break;

	case srvr4ilu_in_arguments: /* done reading in request arguments */

		/* set to next state */
		_http_call_state(p_call) = srvr4ilu_out_begin_reply;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}



/* ********************************************************* */
/* reply beginning, interpreting, finishing, read            */
/* ********************************************************* */

/* ********************************************************* */
/* Begin a reply                                             */

static ilu_boolean _http_begin_reply (ilu_Call p_call, 
									  ilu_boolean b_exceptions_possible,
									  ilu_cardinal reply_size, 
									  ILU_ERRS((IoErrs)) * p_error) {
	char c_buffer[64];

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case srvr4http_out_begin_reply:

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_Response_record;
		break;

	case srvr4ilu_out_begin_reply:

		/* have the transport write out the version and an OK status */
		sprintf(c_buffer, "HTTP/%d.%d 200 OK\r\n\r\n", HTTPMajorVersion, HTTPMinorVersion);
		_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;

		/* set to next state */
		_http_call_state(p_call) = srvr4ilu_out_return_values;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

	return ilu_TRUE;
}


/* ********************************************************* */
/* Finish a reply                                            */

static ilu_boolean _http_finish_reply (ilu_Call p_call, 
									  ILU_ERRS((IoErrs)) * p_error) {

	ilu_Transport the_transport = p_call->ca_prTrans;
	ilu_TransportClass the_transport_class = the_transport->tr_class;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case srvr4http_out_finish_reply:

		/* set to next state */
		_http_call_state(p_call) = srvr4http_finish_call;
		break;

	case srvr4ilu_out_return_values:

		/* set to next state */
		_http_call_state(p_call) = srvr4ilu_finish_call;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}


	/* have the transport flush itself */
	return ((*the_transport_class->tc_write_bytes) 
			(the_transport, (ilu_bytes)"", 0, TRUE, p_error));
}


/* ********************************************************* */
/* take care of end of reply                                 */

static void _http_reply_read (ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_in_reply_read:

		/* set to next state */
		_http_call_state(p_call) = clnt2http_finish_call;
		break;

	case clnt2ilu_in_return_values:

		/* set to next state */
		_http_call_state(p_call) = clnt2ilu_finish_call;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

}


/* ********************************************************* */
/* interprets http reply                                     */

static ilu_ProtocolException _http_interpret_reply 
	(ilu_Call p_call, ilu_cardinal* p_card_exception_code, ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_in_interpret_reply:

		/* note that any returned error in the status line will be placed
		in the Response Record's Status Line for examination by the client,
		so we don't tell ilu that there was any problem */

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_Response_record;

		/* indicate success */
		*p_card_exception_code = 0;
		return ilu_ProtocolException_Success;


	case clnt2ilu_in_interpret_reply:

		/* take a look at the status code and reason phrase 
		   to look for exceptions */
		if (strcmp(_http_version_reason_phrase(p_call),
				g_c_protocol_exception) == 0) {
			/* had a protocol exception */

			/* just treat as if we were reading in return values */
			_http_call_state(p_call) = clnt2ilu_in_return_values;

			*p_card_exception_code = 0;
			return _http_status_code(p_call);

		}
		else if (strcmp(_http_version_reason_phrase(p_call),
				g_c_non_protocol_exception) == 0) {
				/* had a non protocol exception */

				/* just treat as if we were reading in return values */
				_http_call_state(p_call) = clnt2ilu_in_return_values;

				*p_card_exception_code = _http_status_code(p_call);
				return ilu_ProtocolException_Success;
		}
		else {
			/* everything must have been OK */

			/* set to next state */
			_http_call_state(p_call) = clnt2ilu_in_return_values;

			/* indicate success */
			*p_card_exception_code = 0;
			return ilu_ProtocolException_Success;
		}


	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

	/* indicate problem (never really reached) */
	*p_card_exception_code = 0;
	return ilu_ProtocolException_Unknown;
}

/* ********************************************************* */
/* finishes off record and sequence                          */
/* ********************************************************* */

/* ********************************************************* */
/* finishes off a record                                     */

static void _http_end_record (ilu_Call p_call,
				    ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	/* mostly merely progress to the next state */
	switch (_http_call_state(p_call)) {

	case clnt2http_in_end_Header_record :

		/* advance to next header in list */
		_http_headers_current_node(p_call) = 
			_http_headers_current_node(p_call)->m_p_next_list_node;

		if (_http_headers_current_node(p_call) == NIL)
			/* we're done with the headers */
			_http_call_state(p_call) = clnt2http_in_end_Header_sequence;
			/* else we have more headers to deal with  */
		else _http_call_state(p_call) = clnt2http_in_Header_record;

		break;

	case srvr4http_in_end_Header_record :

		/* advance to next header in list */
		_http_headers_current_node(p_call) = 
			_http_headers_current_node(p_call)->m_p_next_list_node;

		if (_http_headers_current_node(p_call) == NIL)
			/* we're done with the headers */
			_http_call_state(p_call) = srvr4http_in_end_Header_sequence;
			/* else we have more headers to deal with  */
		else _http_call_state(p_call) = srvr4http_in_Header_record;

		break;

	case clnt2http_out_end_Header_record :

		/* write out trailing cr lf */
		_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* advance to next header */
		_http_num_headers_to_process(p_call)--;

		if (_http_num_headers_to_process(p_call) == 0)
			/* we're done with the headers */
			_http_call_state(p_call) = clnt2http_out_end_Header_sequence;
			/* else we have more headers to deal with  */
		else _http_call_state(p_call) = clnt2http_out_Header_record;

		break;

	case srvr4http_out_end_Header_record :

		/* write out trailing cr lf */
		_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* advance to next header */
		_http_num_headers_to_process(p_call)--;

		if (_http_num_headers_to_process(p_call) == 0)
			/* we're done with the headers */
			_http_call_state(p_call) = srvr4http_out_end_Header_sequence;
			/* else we have more headers to deal with  */
		else _http_call_state(p_call) = srvr4http_out_Header_record;
		break;

	case clnt2http_in_end_Response_record :
		_http_call_state(p_call) = clnt2http_in_reply_read;
		break;

	case srvr4http_in_end_Request_record :
		_http_call_state(p_call) = srvr4http_in_request_read;
		break;

	case clnt2http_out_end_Request_record :
		_http_call_state(p_call) = clnt2http_out_finish_request;
		break;

	case srvr4http_out_end_Response_record :
		_http_call_state(p_call) = srvr4http_out_finish_reply;
		break;

	case clnt2ilu_in_return_values :
		break;

	case srvr4ilu_in_arguments :
		break;

	case clnt2ilu_out_arguments :
		break;

	case srvr4ilu_out_return_values :
		break;

	case srvr4ilu_out_exception:
		break;

	case clnt2http_out_method_name: /* may end up here in these states because of */
	case clnt2ilu_out_method_name:	/* the way some lang mappings make their sizing calls */
	case srvr4http_out_begin_reply:
	case srvr4ilu_out_begin_reply:
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* finishes off a sequence                                   */

static void _http_end_sequence (ilu_Call p_call,
				    ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	/* merely progress to the next state */
	switch (_http_call_state(p_call)) {

	case clnt2http_in_end_Header_sequence:
		_http_call_state(p_call) = clnt2http_in_Body_present;
		break;

	case clnt2http_out_end_Header_sequence:

		/* we would normally think we should write out the header's
		trailing crlf here, but we won't because we want to ensure
		that there is a Content-Length header that reflects the length
		of the body (if there is one).  So, in state, clnt2http_out_Body_present
		we'll write out the trailing cr lf if there's no body, and in 
		clnt2http_out_Body we'll write out a Content-Length header,
		the trailing crlf, and then the body bytes */
		
		_http_call_state(p_call) = clnt2http_out_Body_present;
		break;

	case srvr4http_in_end_Header_sequence:
		_http_call_state(p_call) = srvr4http_in_Body_present;
		break;

	case srvr4http_out_end_Header_sequence:

		/* we would normally think we should write out the header's
		trailing crlf here, but we won't because we want to ensure
		that there is a Content-Length header that reflects the length
		of the body (if there is one).  So, in state, srvr4http_out_Body_present
		we'll write out the trailing cr lf if there's no body, and in 
		srvr4http_out_Body we'll write out a Content-Length header,
		the trailing crlf, and then the body bytes */

		_http_call_state(p_call) = srvr4http_out_Body_present;
		break;

	case clnt2ilu_in_return_values :
		break;

	case srvr4ilu_in_arguments :
		break;

	case clnt2ilu_out_arguments :
		break;

	case srvr4ilu_out_return_values :
		break;

	case srvr4ilu_out_exception:
		break;

	case clnt2http_out_method_name: /* may end up here in these states because of */
	case clnt2ilu_out_method_name:	/* the way some lang mappings make their sizing calls */
	case srvr4http_out_begin_reply:
	case srvr4ilu_out_begin_reply:
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* begin and finish an exception                             */
/* ********************************************************* */

static ilu_boolean _http_begin_exception (ilu_Call p_call, 
										  ilu_cardinal card_exception_code, 
										  ilu_ProtocolException sys_ex_index, 
										  ilu_cardinal card_reply_size,
										  ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_code;
	ilu_string pc_reason_phrase;
	char c_buffer[128];

	ILU_CLER(*p_error);

	switch (_http_call_type(p_call)) {

	case srvr4http:

		/* send back an internal server error since any other http'ish response code would
		simply be in the status line of the Response's Status Line 
		xxx note should make the 'phrase' be more indicative of the actual error */
		sprintf(c_buffer, "HTTP/%d.%d %hd %s\r\n", HTTPMajorVersion, HTTPMinorVersion,
		500, _http_phrase_of_status_code(500));

		_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return ilu_FALSE;

		/* Now all the output functions check to see if we're in the srvr4http_out_exception
		state, and if so, actually output nothing */

		_http_call_state(p_call) = srvr4http_out_exception;
		break;

	case srvr4ilu:

		/* if "card_exception_code" == 0, then sys_ex_index contains a protocol 
		    exception detail code */

		if (card_exception_code == 0) {
			card_code = sys_ex_index;
			pc_reason_phrase = g_c_protocol_exception;
		}
		else {
			card_code = card_exception_code;
			pc_reason_phrase = g_c_non_protocol_exception;
		}

		/* send back an error code and phrase indicating that an exception occurred */
		sprintf(c_buffer, "HTTP/%d.%d %lu %s\r\n\r\n", 
			HTTPMajorVersion, HTTPMinorVersion, card_code, pc_reason_phrase);

		_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return ilu_FALSE;
		
		/* now just proceed basically as if we were sending back return values */
		_http_call_state(p_call) = srvr4ilu_out_exception;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

	return ilu_TRUE;
}



static ilu_boolean _http_finish_exception  (ilu_Call p_call, 
											ILU_ERRS((IoErrs)) * p_error) {

	ilu_Transport the_transport = p_call->ca_prTrans;
	ilu_TransportClass the_transport_class = the_transport->tr_class;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case srvr4http_out_exception:

		/* put out final crlf (after status line) */
		_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return ilu_FALSE;

		_http_call_state(p_call) = srvr4http_finish_call;

		break;

	case srvr4ilu_out_exception: 
		
		/* get here after sending back exception members */
		_http_call_state(p_call) = srvr4ilu_finish_call;
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

	/* have the transport flush itself */
	return ((*the_transport_class->tc_write_bytes) 
			(the_transport, (ilu_bytes)"", 0, TRUE, p_error));

}




/* ********************************************************* */
/* output and input functions                                */
/* ********************************************************* */


/* ********************************************************* */
/* outputs starting of a record (actually outputs nothing)   */

static void _http_output_record (ilu_Call p_call,
				    ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	/* not much to do except advance state */
	switch (_http_call_state(p_call)) {

	case clnt2http_out_Request_record:

		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_Request_URI;
		break;

	case clnt2http_out_Header_record:

		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_Header_Name;

		break;

	case srvr4http_out_Response_record:

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_Status_Line;

		break;

	case srvr4http_out_Header_record:

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_Header_Name;

		break;

	case clnt2ilu_out_arguments: /* stay in same state */
		break;

	case srvr4ilu_out_return_values: /* stay in same state */
		break;

	case srvr4ilu_out_exception:
		break;

	case srvr4http_out_exception: 
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

}

/* ********************************************************* */
/* inputs begining of a record                               */

static void _http_input_record (ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_in_Response_record:

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_Status_Line;

		break;

	case clnt2http_in_Header_record:

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_Header_Name;

		break;

	case srvr4http_in_Request_record:

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_Request_URI;

		break;

	case srvr4http_in_Header_record:

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_Header_Name;

		break;


	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* just stay in same state */

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* outputs an enum                                           */

static void _http_output_enum_code (ilu_Call p_call, ilu_shortcardinal scard_enum,
				    ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer[64];

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case srvr4http_out_Status_Line:

		/* we're sending a status line in a reply */
		/* have the transport write out the version, code, phrase, and crlf */
		sprintf(c_buffer, "HTTP/%d.%d %hd ", HTTPMajorVersion, HTTPMinorVersion,
			scard_enum);

		_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return;

		_http_transport_write_bytes(p_call->ca_prTrans, _http_phrase_of_status_code(scard_enum), 
			strlen(_http_phrase_of_status_code(scard_enum)), p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return;

		_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
		
		if (ILU_ERRNOK(*p_error)) 
			return;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_Header_sequence;

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		if (_http_write_cardinal_line(p_call, scard_enum, p_error) == ilu_FALSE)
			return;

		/* stay in same state */

		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* inputs an enumeration code                                */

static void _http_input_enum_code (ilu_Call p_call, ilu_shortcardinal* p_scard_code,
				    ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_enum_line;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_in_Status_Line:

		/* return the code we extracted during get_status_line */
		*p_scard_code = _http_status_code(p_call);

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_Header_sequence;

		return;

	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		pc_enum_line = _http_readline(p_call, p_error);
		if (pc_enum_line == NIL)
			return;

		if (sscanf(pc_enum_line, "%hd", p_scard_code) !=  1) 
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);

		ilu_free(pc_enum_line);

		return;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/*  outputs a string                                         */

static void _http_output_string (ilu_Call p_call, ilu_string p_string,
				    ilu_cardinal card_strlength,
				    ilu_cardinal card_strlimit,
				    ILU_ERRS((IoErrs)) * p_error) {

  	ilu_string pc_params_queries;
	char c_buffer[32];

	ILU_CLER(*p_error);

	if ((card_strlimit > 0) && (card_strlength > card_strlimit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	switch (_http_call_state(p_call)) {

	case clnt2http_out_Request_URI:

		/* Note that back in state clnt2http_out_URL_path we didn't finish off the 
		request line.  We waited till we got here, because at this point
		we have access to any params or queries that should be placed after the
		object id. */  

		/* find where params or queries (if any) start */
		pc_params_queries = p_string;
		while (*pc_params_queries && (*pc_params_queries != ';') && (*pc_params_queries != '?'))
			pc_params_queries++;

		if (*pc_params_queries) {  /* some params or queries were present, write them out */
			_http_transport_write_bytes(p_call->ca_prTrans, pc_params_queries, 
				strlen(pc_params_queries), p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}

		/* have the transport write out the space and version */
		sprintf(c_buffer, " HTTP/%d.%d\r\n", HTTPMajorVersion, HTTPMinorVersion);

		_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* NOTE That we never actually send the Request_URI over the wire - this will be
		just picked out of our http request line and reconstructed on the server side
		in the case of an ilu http server, and it wouldn't be recognized by an existing 
		http server daemon*/

		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_Header_sequence;
		break;

	case clnt2http_out_Header_Name:

		if (_http_strnstr((ilu_bytes)p_string, "Content-Length", card_strlength, ilu_FALSE) != NULL) {
			/* the user specified their own content-length header - make note of this
			so we don't automatically generate one later on */
			_http_user_supplied_content_length(p_call) = ilu_TRUE;
		}

		/* have the transport write out the header, colon*/
		_http_transport_write_bytes(p_call->ca_prTrans, p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(p_call->ca_prTrans, ":", 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_Header_Value_present;

		return ;

	case clnt2http_out_Header_value:

		/* have the transport write out the space, value */
		_http_transport_write_bytes(p_call->ca_prTrans, g_c_SP, 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(p_call->ca_prTrans, p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_end_Header_record;

		return ;

	case srvr4http_out_Header_Name:

		if (_http_strnstr((ilu_bytes)p_string, "Content-Length", card_strlength, ilu_FALSE) != NULL) {
			/* the user specified their own content-length header - make note of this
			so we don't automatically generate one later on */
			_http_user_supplied_content_length(p_call) = ilu_TRUE;
		}

		/* have the transport write out the header, colon*/
		_http_transport_write_bytes(p_call->ca_prTrans, p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(p_call->ca_prTrans, ":", 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_Header_Value_present;

		return ;

	case srvr4http_out_Header_value:

		/* have the transport write out the space, value */
		_http_transport_write_bytes(p_call->ca_prTrans, g_c_SP, 1, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(p_call->ca_prTrans, p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_end_Header_record;
		
		return ;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:
										
		/* write out the string length  - this is how much
		 space the receiver should allocate to input the string, -1 */
		if (_http_write_cardinal_line(p_call, card_strlength, p_error) == ilu_FALSE)
			return;

		/* have the transport write out the string followed by cr lf */
		_http_transport_write_bytes(p_call->ca_prTrans, p_string, card_strlength, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* stay in same state */

		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* inputs a string                                           */

static void _http_input_string (ilu_Call p_call, ilu_string* ppc_string,
				 ilu_cardinal* p_card_length,
				 ilu_cardinal card_limit,
				 ILU_ERRS((IoErrs)) * p_error) {
					
	ilu_string pc_string;
	ilu_cardinal card_size;
	char c_junk[8];

	ILU_CLER(*p_error);

	*ppc_string = NIL;
	*p_card_length = 0;
	pc_string = NIL;

	switch (_http_call_state(p_call)) {

	case clnt2http_in_Header_Name:

		card_size = strlen(_http_list_node_contents(_http_headers_current_node(p_call)));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* just duplicate the header name we parsed our during _http_fill_headers_list */
		*ppc_string = ilu_StrdupE(_http_list_node_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 
		*p_card_length = card_size;

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_Header_Value_present;

		break;

	case clnt2http_in_Header_value:

		card_size = strlen(_http_list_node_into_contents(_http_headers_current_node(p_call)));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* just duplicate the header value we parsed our during _http_fill_headers_list */
		*ppc_string = ilu_StrdupE(_http_list_node_into_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 
		*p_card_length = card_size;

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_end_Header_record;

		break;

	case srvr4http_in_server_id:

		card_size = strlen(server_id(call_server(p_call)));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* since there is no notion of a server id for straight http, we just
		   return the server's id string */

		*ppc_string = ilu_StrdupE(server_id(call_server(p_call)), p_error);
		if (ILU_ERRNOK(*p_error)) { 
			*ppc_string = NIL;
			return ;
		}
		*p_card_length = card_size;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_discriminator_id;

		break;

	case srvr4http_in_discriminator_id:

		card_size = strlen(_http_object_id(p_call));

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		/* return a copy of the object id we saved during get_request_line */
		*ppc_string = ilu_StrdupE(_http_object_id(p_call), p_error);
		if (ILU_ERRNOK(*p_error)) { 
			*ppc_string = NIL;
			return ;
		}
		*p_card_length = card_size;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_Request_record;

		break;

	case srvr4http_in_Request_URI:

		/* we already have this stashed (with null terminated parts) in our
		call data m_pc_request_or_status_line from when we did get_request_line */

		pc_string = ilu_MallocE(strlen(_http_object_id(p_call)) + 
			(_http_params_queries(p_call) ? strlen(_http_params_queries(p_call)) + 1 : 0) +
			1, p_error);

		if (pc_string == NIL) 
			return;

		if (_http_params_queries(p_call)) 
			sprintf(pc_string, "%s%c%s", _http_object_id(p_call), 
				_http_params_queries_delim(p_call),
				_http_params_queries(p_call));
		else
			sprintf(pc_string, "%s", _http_object_id(p_call));
		
		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = pc_string;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_Header_sequence;

		break;

	case srvr4http_in_Header_Name:

		/* just duplicate the header name we parsed our during _http_fill_headers_list */
		pc_string = ilu_StrdupE(_http_list_node_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = pc_string;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_Header_Value_present;

		break;

	case srvr4http_in_Header_value:

		/* just duplicate the header value we parsed our during _http_fill_headers_list */
		pc_string = ilu_StrdupE(_http_list_node_into_contents(_http_headers_current_node(p_call)),
			p_error);
		if (ILU_ERRNOK(*p_error)) return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = pc_string;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_end_Header_record;

		break;

	case srvr4ilu_in_server_id:

		/* just duplicate the server id we parsed our during _http_fill_headers_list */
		pc_string = ilu_StrdupE(_http_server_id(p_call), p_error);
		if (ILU_ERRNOK(*p_error)) 
			return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = pc_string;

		/* set to next state */
		_http_call_state(p_call) = srvr4ilu_in_discriminator_id;

		break;

	case srvr4ilu_in_discriminator_id:

		/* just duplicate the object id we parsed our during get_request_line */
		pc_string = ilu_StrdupE(_http_object_id(p_call), p_error);
		if (ILU_ERRNOK(*p_error)) 
			return; 

		card_size = strlen(pc_string);

		if (card_limit > 0 && card_size > card_limit) {
			ilu_free(pc_string);
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;
		*ppc_string = pc_string;

		/* set to next state */
		_http_call_state(p_call) = srvr4ilu_in_arguments;

		break;

	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* read in the string size */
		if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
			goto bad_string_label;
		}

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;

		/* alloc space to hold the string, plus a null to terminate it in case that's what the
		 language expects */
		*ppc_string = ilu_MallocE((*p_card_length) + 1, p_error);
		if (ILU_ERRNOK(*p_error))
			goto bad_string_label;

		/* read in the string */
		_http_transport_read_bytes(p_call->ca_prTrans, *ppc_string, *p_card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			goto bad_string_label;

		/* null terminate it */
		*((*ppc_string) + (*p_card_length)) = '\0';

		/* read up the trailing crlf we put there */
		_http_transport_read_bytes(p_call->ca_prTrans, c_junk, 2, p_error);
		if (ILU_ERRNOK(*p_error)) 
			goto bad_string_label;

		return;

		bad_string_label:

		ilu_free(*ppc_string);
		*ppc_string = NIL;
		*p_card_length = 0;
		return;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* output an object id                                       */
/* note that when the function is called, we have 'entered the 
   server', and must 'exit it' before leaving the function, or 
   the lock will be still on. */

static ilu_boolean _http_output_object_id (ilu_Call p_call, ilu_Object p_object,
				       ilu_boolean b_discriminator_p,
				       ilu_Class p_class,
				       ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer[32];

	ILU_CLER(*p_error);

	/* ensure we have valid args */
	if (call_connection(p_call) == NIL) {
		if (p_object != NIL)	/* release the lock if we had bad args */
			ilu_ExitServer(object_server(p_object), object_class(p_object));
		return ILU_ERR_CONS1(comm_failure, p_error, minor, ilu_cfm_conn_lost, FALSE);
	}
	
	switch (_http_call_state(p_call)) {

	case clnt2http_out_URL_path:

		if (!b_discriminator_p) {
			/* we should never get a non descriminator here if this 
			   is a call to a http_resource_object */
			return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, 0);
		}

		/* write out the object identifier as the URL path */

		/* xxx *really* need to better determine if this method is being sent to a 
			proxy server, and if so, need to output full sbh instead
			of just object_id this way has problems if ILU_HTTP_PROXY_INFO_ENV_VAR
		    is set dynamically */

		if (getenv(ILU_HTTP_PROXY_INFO_ENV_VAR) == NIL) 
			/* assume we're not using any proxy */
			_http_transport_write_bytes(p_call->ca_prTrans, object_ih(p_object), 
				strlen(object_ih(p_object)), p_error);
		else
			_http_transport_write_bytes(p_call->ca_prTrans, object_sbh(p_object), 
				strlen(object_sbh(p_object)), p_error);

		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;

		/* Note that at this point we don't yet finish off the request line.  We
		wait till we get to state clnt2http_out_Request_URI, because at that point
		we'll have access to any params or queries that should be placed after the
		object id. */  

		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_Request_record;

		break;

	case clnt2ilu_out_object_id:

		if (b_discriminator_p) {	/* if we're the descriminator object */

			if (p_object == NIL)	/* ensure object isn't nil */
				return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, FALSE);

			/* have the transport write out object id, the space and version */
			_http_transport_write_bytes(p_call->ca_prTrans, object_ih(p_object), 
				strlen(object_ih(p_object)), p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;

			sprintf(c_buffer, " HTTP/%d.%d\r\n", HTTPMajorVersion, HTTPMinorVersion);
			_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;

			/* now write out a single http header that contains the server id */
			_http_transport_write_bytes(p_call->ca_prTrans, ILU_SERVER_ID_HEADER_NAME, 
				ILU_SERVER_ID_HEADER_NAME_LENGTH, p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;

			/* write out separating colon and space */
			_http_transport_write_bytes(p_call->ca_prTrans, ": ", 2, p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;

			_http_transport_write_bytes(p_call->ca_prTrans, server_id(call_server(p_call)), 
				strlen(server_id(call_server(p_call))), p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;

			_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLFCRLF, 4, p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;

			/* set to next state */
			_http_call_state(p_call) = clnt2ilu_out_arguments;
		}
		else /* shouldn't be in this state if we're not the discriminant */
			_ilu_Assert(ilu_FALSE, g_c_bad_state);

		break;

	case clnt2ilu_out_arguments: 
	
	  	if (!b_discriminator_p) { /* we're not the discrimnator object */
			
			/* else if we're nil, make sure we're of a class that's OPTIONAL */
			if (p_object == NIL && !p_class->cl_optional) {
				ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, FALSE);
				goto problem_label;
			}

			if (p_object == NIL) { 
				/* write out our nil object indicator */
				_http_output_string(p_call, g_c_NILOBJ, strlen(g_c_NILOBJ), 0xFFFF, p_error);
				if (ILU_ERRNOK(*p_error))
					goto problem_label;
			}
			else { /* write out the object's sbh */
				ilu_string pc_sbh =  ilu_SBHOfObject(p_object);
				_http_output_string(p_call, pc_sbh, strlen(pc_sbh), 0xFFFF, p_error);
				if (ILU_ERRNOK(*p_error))
					goto problem_label;
			}
		}
		else /* shouldn't be in this state if we're not the descriminant */
			_ilu_Assert(ilu_FALSE, g_c_bad_state);

		/* stay in same state */

		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	case srvr4ilu_out_exception:
	case srvr4ilu_out_return_values:

		/* if we're nil, make sure we're of a class that's OPTIONAL */
		if (p_object == NIL && !p_class->cl_optional) {
			ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_OID, FALSE);
				goto problem_label;
		}

		if (p_object == NIL) { 
			/* write out our nil object indicator */
			_http_output_string(p_call, g_c_NILOBJ, strlen(g_c_NILOBJ), 0xFFFF, p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;
		}
		else { /* write out the object's sbh */
			ilu_string pc_sbh =  ilu_SBHOfObject(p_object);
			_http_output_string(p_call, pc_sbh, strlen(pc_sbh), 0xFFFF, p_error);
			if (ILU_ERRNOK(*p_error))
				goto problem_label;
		}

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

problem_label:

	/* release the lock */
	if (p_object != NIL) 
		ilu_ExitServer(object_server(p_object), object_class(p_object));

	if (ILU_ERRNOK(*p_error))
		return ilu_FALSE;
	
	return ilu_TRUE;
}


/* ********************************************************* */
/* input an object id                                        */

/* a mildly reworked version of _ilu_InputObjectID as it stood
at the time this was written */

static ilu_boolean _http_input_object_id(ilu_Call p_call, ilu_Object * p_object,
										 ilu_boolean b_discriminator_p,
										 ilu_Class p_class,
										 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string      pc_server_id = NIL;
	ilu_string		pc_object_id = NIL;
	ilu_cardinal    card_server_id_length = 0;
	ilu_cardinal	card_object_id_length = 0;
	ilu_Server      server = connection_server(call_connection(p_call));

	ILU_CLER(*p_error);

	*p_object = NIL;

	/* ensure valid params */
	if ((call_connection(p_call) == NIL) || (p_class == NIL))
		return ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_duh, ilu_FALSE);

	if (b_discriminator_p) {

		/* input the server id as a string */
		_http_input_string (p_call, &pc_server_id, &card_server_id_length, 0xFFFF, p_error);
		if (ILU_ERRNOK(*p_error))
			return ilu_FALSE;
	}

	/* input the object id */
	_http_input_string(p_call, &pc_object_id, &card_object_id_length, 0xFFFF, p_error);
	if (ILU_ERRNOK(*p_error)) {
		ilu_free(pc_server_id);
		return ilu_FALSE;
	}

	if (b_discriminator_p) {

		ilu_EnterServer(server, p_class);

		/* check to see if we're in the right server */
		if (strcmp(pc_server_id, server_id(server)) != 0) {
			(void) ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_alien_disc, 6);
			ilu_ExitServer(server, p_class);
		}

		/* check to see if we're in a closed server */
		else if (server_objs(server) == NIL) {
			(void) ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_svr_closed, 6);
			ilu_ExitServer(server, p_class);
		} 
		
		/* check to see that this object is in this server */
		else if ((*p_object = _ilu_FindObjectInServer(pc_object_id, server)) == NIL) {
			(void) ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_inst_nf, 6);
			ilu_ExitServer(server, p_class);
		} 
		
		/* check that the object is of the class */
		else if (!ilu_IsSubObjectType((*p_object)->ob_class, p_class)) {
			(void) ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_wrong_type, 6);
			*p_object = NIL;
			ilu_ExitServer(server, p_class);
		}

		/* cleanup */
		ilu_free(pc_server_id);
		if (ILU_ERRNOK(*p_error)) {
			ilu_free(pc_object_id);
			return ilu_FALSE;
		}
	}

	else { /* not the discriminator */
		if (strcmp(g_c_NILOBJ, pc_object_id) == 0) { /* really got nil obj */
			*p_object = NIL;
			ilu_free(pc_object_id);

			if (p_class->cl_optional) { /* if it's an optional, we're OK */
				return ilu_TRUE;
			} 
			else /* NIL when it shouldn't have been */
				return ILU_ERR_CONS1(NoObjectForSBH, p_error, sbh, pc_object_id, ilu_FALSE);
		} 
		
		else { /* we got something, get the object */
			*p_object = ilu_ObjectOfSBH(pc_object_id, p_class, p_error);
			if (ILU_ERRNOK(*p_error)) { /* couldn't find the object! */
				ilu_free(pc_object_id);
				return ilu_FALSE;
			}
		} 
	} /* end of not the discriminator */

	ilu_free(pc_object_id);
	return ilu_TRUE;
}


/* ********************************************************* */
/*  starting to send a sequence                              */

static void _http_output_sequence (ilu_Call p_call, ilu_cardinal card_length,
				      ilu_cardinal card_limit,
				      ILU_ERRS((IoErrs)) * p_error) {

	ILU_CLER(*p_error);

	if ((card_limit > 0) && (card_length > card_limit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
    }

	switch (_http_call_state(p_call)) {

	case clnt2http_out_Header_sequence:

		_http_num_headers_to_process(p_call) = card_length;

		/* set to next state */
		if (card_length > 0)
			_http_call_state(p_call) = clnt2http_out_Header_record;
		else 
			_http_call_state(p_call) = clnt2http_out_end_Header_sequence;
		break;

	case srvr4http_out_Header_sequence:

		_http_num_headers_to_process(p_call) = card_length;

		/* set to next state */
		if (card_length > 0)
			_http_call_state(p_call) = srvr4http_out_Header_record;
		else 
			_http_call_state(p_call) = srvr4http_out_end_Header_sequence;

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		/* have the transport write out the sequence length followed by cr lf */
		_http_write_cardinal_line(p_call, card_length, p_error);
		return ;

		/* stay in same state */

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}

}


/* ********************************************************* */
/* inputs an sequence                                        */

static void _http_input_sequence (ilu_Call p_call, ilu_cardinal* pcard_length,
						   ilu_cardinal card_limit, ILU_ERRS((IoErrs)) * p_error) {
					
	ilu_cardinal card_size;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_in_Header_sequence:

		card_size = _http_list_size(_http_headers_list(p_call));

		if ((card_limit > 0) && (card_size > card_limit)) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		if ((*pcard_length = card_size) > 0) {
			_http_call_state(p_call) = clnt2http_in_Header_record;
		}
		else 
			_http_call_state(p_call) = clnt2http_in_end_Header_sequence;
		break;

	case srvr4http_in_Header_sequence:

		card_size = _http_list_size(_http_headers_list(p_call));

		if ((card_limit > 0) && (card_size > card_limit)) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		if ((*pcard_length = card_size) > 0) {
			_http_call_state(p_call) = srvr4http_in_Header_record;
		}
		else 
			_http_call_state(p_call) = srvr4http_in_end_Header_sequence;
		break;

	case clnt2ilu_in_return_values:
	case srvr4ilu_in_arguments:

		if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) 
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);

		if ((card_limit > 0) && (card_size > card_limit)) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*pcard_length = card_size;

		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);

	}
}


/* ********************************************************* */
/*  sending an optional                                      */

static void _http_output_optional (ilu_Call p_call, ilu_boolean b_falseifnil,
				      ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer[32];

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_out_Header_Value_present:

		/* set to next state */
		if (b_falseifnil)
			_http_call_state(p_call) = clnt2http_out_Header_value;
		else 
			_http_call_state(p_call) = clnt2http_out_end_Header_record;

		break;

	case clnt2http_out_Body_present:

		/* set to next state */
		if (b_falseifnil)
			_http_call_state(p_call) = clnt2http_out_Body;

		else {

			/* write out trailing cr lf - see state clnt2http_out_end_Header_sequence
			for explantion of why we're doing this here */
			_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);

			_http_call_state(p_call) = clnt2http_out_end_Request_record;
		}

		break;

	case srvr4http_out_Header_Value_present:

		/* set to next state */
		if (b_falseifnil)
			_http_call_state(p_call) = srvr4http_out_Header_value;
		else 
			_http_call_state(p_call) = srvr4http_out_end_Header_record;
		break;


	case srvr4http_out_Body_present:

		/* set to next state */
		if (b_falseifnil)
			_http_call_state(p_call) = srvr4http_out_Body;

		else {

			/* write out trailing cr lf - see state srvr4http_out_end_Header_sequence
			for explantion of why we're doing this here */
			_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);

			_http_call_state(p_call) = srvr4http_out_end_Response_record;
		}

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		/* have the transport write out a presence indication */
		if (b_falseifnil)
			sprintf(c_buffer, "%s\r\n", g_c_OPTIONAL_PRESENT);
		else
			sprintf(c_buffer, "%s\r\n", g_c_OPTIONAL_NOT_PRESENT);

		_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		/* stay in same state */
		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* inputs an optional                                        */

static void _http_input_optional (ilu_Call p_call, ilu_boolean * p_b_present,
			                 ILU_ERRS((IoErrs)) * p_error) {
					
	char* pc_line;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_in_Header_Value_present:

		/* if our into contents is non nil (as established during fill_headers_list) then
		   there's something there */
		*p_b_present = _http_list_node_into_contents(_http_headers_current_node(p_call)) ? 
						ilu_TRUE : ilu_FALSE;
		
		/* set to next state */
		if (*p_b_present)
			_http_call_state(p_call) = clnt2http_in_Header_value;
		else
			_http_call_state(p_call) = clnt2http_in_end_Header_record;
		break;

	case clnt2http_in_Body_present:

		/* if there was a content length header, and it's not a head method, we know there's a body */
		/* xxx actually, we need to ignore the content length header for http1.0 since
		bodies in responses are not required to have a content length field !!!  - so we'll just
		say it's there and read till eof */
		*p_b_present = (/* (_http_body_length(p_call) > 0) && */ (!(_http_is_HEAD_method(p_call)))) ? 
						ilu_TRUE : ilu_FALSE;

		/* set to next state */
		if (*p_b_present)
			_http_call_state(p_call) = clnt2http_in_Body;
		else
			_http_call_state(p_call) = clnt2http_in_end_Response_record;
		break;

	case srvr4http_in_Header_Value_present:

		/* if our into contents is non nil (as established during fill_headers_list) then
		   there's something there */
		*p_b_present = _http_list_node_into_contents(_http_headers_current_node(p_call)) ? 
						ilu_TRUE : ilu_FALSE;
		
		/* set to next state */
		if (*p_b_present)
			_http_call_state(p_call) = srvr4http_in_Header_value;
		else
			_http_call_state(p_call) = srvr4http_in_end_Header_record;
		break;

	case srvr4http_in_Body_present:

		/* if there was a content length header, we know there's a body */
		*p_b_present = (_http_body_length(p_call) > 0) ? ilu_TRUE : ilu_FALSE;

		/* set to next state */
		if (*p_b_present)
			_http_call_state(p_call) = srvr4http_in_Body;
		else
			_http_call_state(p_call) = srvr4http_in_end_Request_record;
		break;


	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* read in the line containing the presence indication */
		if ((pc_line = _http_readline(p_call, p_error)) == NIL)
			return;

		*p_b_present = (strcmp(pc_line, g_c_OPTIONAL_PRESENT) == 0);
		ilu_free(pc_line);

		/* stay in same state */
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/*  sending a bunch of bytes                                 */

static void _http_output_bytes(ilu_Call p_call, ilu_bytes p_the_bytes,
				   ilu_cardinal card_length,
				   ilu_cardinal card_limit,
				   ILU_ERRS((IoErrs)) * p_error) {


	char c_buffer[64];

	ILU_CLER(*p_error);

	if ((card_limit > 0) && (card_length > card_limit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	switch (_http_call_state(p_call)) {

	case clnt2http_out_Body:

		/* write out Content-length header and trailing cr lf - see state 
		clnt2http_out_end_Header_sequence for explantion of why we're 
		doing this here */

		if (_http_user_supplied_content_length(p_call)) {
			/* user already supplied a content-length header, so don't
			auto generate, just put out trailing crlf to indicate end of headers */
			_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}
		else {
			/* auto generate a content length header */
			sprintf(c_buffer, "Content-Length: %lu\r\n\r\n", card_length);

			_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}

		/* write out the body bytes */
		_http_transport_write_bytes(p_call->ca_prTrans, p_the_bytes, card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* set to next state */
		_http_call_state(p_call) = clnt2http_out_end_Request_record;

		break;

	case srvr4http_out_Body:
		
		/* write out Content-length header and trailing cr lf - see state 
		srvr4http_out_end_Header_sequence for explantion of why we're 
		doing this here */

		if (_http_user_supplied_content_length(p_call)) {
			/* user already supplied a content-length header, so don't
			auto generate, just put out trailing crlf to indicate end of headers */
			_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}
		else {
			/* auto generate a content length header */
			sprintf(c_buffer, "Content-Length: %lu\r\n\r\n", card_length);

			_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
			if (ILU_ERRNOK(*p_error))
				return;
		}

		/* write out the body bytes */
		_http_transport_write_bytes(p_call->ca_prTrans, p_the_bytes, card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		/* set to next state */
		_http_call_state(p_call) = srvr4http_out_end_Response_record;

		break;

	case clnt2ilu_out_arguments:
	case srvr4ilu_out_return_values:
	case srvr4ilu_out_exception:

		/* write out count of bytes */
		if (_http_write_cardinal_line(p_call, card_length, p_error) == ilu_FALSE)
			return;

		/* write out the bytes themselves, and add a crlf */
		_http_transport_write_bytes(p_call->ca_prTrans, p_the_bytes, card_length, p_error);
		if (ILU_ERRNOK(*p_error))
			return;

		_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
		if (ILU_ERRNOK(*p_error))
			return ;

		break;

	case srvr4http_out_exception: 
		/* don't send back exception members to existing http clients */
		break;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* inputs bytes                                              */

static void _http_input_bytes (ilu_Call p_call, ilu_bytes* ppc_bytes,
				ilu_cardinal* p_card_length,
				ilu_cardinal card_limit,
				ILU_ERRS((IoErrs)) * p_error) {

	char c_junk[8];
	ilu_cardinal card_size;

	ILU_CLER(*p_error);

	switch (_http_call_state(p_call)) {

	case clnt2http_in_Body:

		card_size = _http_body_length(p_call);

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;

		/* if http1.0 responses were required to have a content length header
		we'd do the following, however, since they are not, we'll read till eof 
		*ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
		if (*ppc_bytes == NIL) {
			*p_card_length = 0;
			return;
		}
		_http_transport_read_bytes(p_call->ca_prTrans, *ppc_bytes, *p_card_length, p_error);
		if (ILU_ERRNOK(*p_error)) {
			*p_card_length = 0;
			ilu_free(*ppc_bytes);
			*ppc_bytes = NIL;
			return;
		} 
		*/

		/* read till end of file (i.e. connection closes) */
		if (_http_fill_buffer_till (p_call->ca_prTrans, ppc_bytes, HTTP_BODY_ALLOC_SIZE,
						"", NIL, 0, NIL, NIL, ilu_TRUE, p_card_length, p_error) == ilu_FALSE) {
			*p_card_length = 0;
			ilu_free(*ppc_bytes);
			*ppc_bytes = NIL;
			return;
		} 

		/* set to next state */
		_http_call_state(p_call) = clnt2http_in_end_Response_record;

		break;

	case srvr4http_in_Body:

		card_size = _http_body_length(p_call);

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;

		/* get right amount of space */
		*ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
		if (*ppc_bytes == NIL) {
			*p_card_length = 0;
			return;
		}

		_http_transport_read_bytes(p_call->ca_prTrans, *ppc_bytes, *p_card_length, p_error);
		if (ILU_ERRNOK(*p_error)) {
			*p_card_length = 0;
			ilu_free(*ppc_bytes);
			*ppc_bytes = NIL;
			return;
		}

		/* set to next state */
		_http_call_state(p_call) = srvr4http_in_end_Request_record;

		break;

	case srvr4ilu_in_arguments:
	case clnt2ilu_in_return_values:

		/* get count of bytes */
		if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
			*p_card_length = 0;
			*ppc_bytes = NIL;
			return;
		}

		if (card_limit > 0 && card_size > card_limit) {
			ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
			return;
		}

		*p_card_length = card_size;

		/* get right amount of space */
		*ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
		if (*ppc_bytes == NIL) {
			*p_card_length = 0;
			return;
		}

		/* read in the bytes and our trailing crlf */
		_http_transport_read_bytes(p_call->ca_prTrans, *ppc_bytes, *p_card_length, p_error);
		
		if (ILU_ERRNOK(*p_error)) {
			ilu_free(*ppc_bytes);
			*p_card_length = 0;
			*ppc_bytes = NIL;
			return;
		}

		_http_transport_read_bytes(p_call->ca_prTrans, c_junk, 2, p_error);
		if (ILU_ERRNOK(*p_error)) {
			ilu_free(*ppc_bytes);
			*p_card_length = 0;
			*ppc_bytes = NIL;
			return;
		}

		return;

	default:
		_ilu_Assert(ilu_FALSE, g_c_bad_state);
	}
}


/* ********************************************************* */
/* outputs a readable representation of the bytes            */

static void _http_output_readable_bytes(ilu_Call p_call, ilu_bytes p_the_bytes,
				   ilu_cardinal card_length,
				   ilu_cardinal card_limit,
				   ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_index;
	ilu_integer i_temp;
	ilu_bytes pc_source = p_the_bytes;
	char c_buffer [16];

	ILU_CLER(*p_error);

	if ((card_limit > 0) && (card_length > card_limit)) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	/* write out count of bytes */
	if (_http_write_cardinal_line(p_call, card_length, p_error) == ilu_FALSE)
		return;

	/* write out a readable representation */
	for (card_index = 0; card_index < card_length; card_index++) {

		i_temp = (ilu_integer)(*pc_source);
		sprintf(c_buffer, "%x", i_temp);
		_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		pc_source++;
	}

	/* write out trailing cr lf */
	_http_transport_write_bytes(p_call->ca_prTrans, g_c_CRLF, 2, p_error);
}


/* ********************************************************* */
/* inputs a readable representation of the bytes             */

static void _http_input_readable_bytes (ilu_Call p_call, ilu_bytes* ppc_bytes,
				ilu_cardinal* p_card_length,
				ilu_cardinal card_limit,
				ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_index;
	ilu_cardinal card_size;
	ilu_integer i_temp;
	ilu_bytes pc_dest;
	char c_buffer [16];

	ILU_CLER(*p_error);

	/* get count of bytes */
	if (_http_read_cardinal_line(p_call, &card_size, p_error) == ilu_FALSE) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
		*p_card_length = 0;
		*ppc_bytes = NIL;
		return;
	}

	if (card_limit > 0 && card_size > card_limit) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
		return;
	}

	*p_card_length = card_size;

	/* get right amount of space */
	*ppc_bytes = (ilu_bytes) ilu_MallocE(*p_card_length, p_error);
	if (*ppc_bytes == NIL) {
		*p_card_length = 0;
		return;
	}

	/* read in the readable bytes */
	pc_dest = *ppc_bytes;
	c_buffer[2] = '\0';
	for (card_index = 0; card_index < *p_card_length; card_index++) {

		_http_transport_read_bytes(p_call->ca_prTrans, c_buffer, 2, p_error);

		if (ILU_ERRNOK(*p_error) || (sscanf(c_buffer, "%x", &i_temp) != 1)) {
			ilu_free(*ppc_bytes);
			*p_card_length = 0;
			*ppc_bytes = NIL;
			return;
		}

		*pc_dest = (ilu_byte)i_temp;
		pc_dest++;
	}
	
	/* read in the trailing crlf */
	_http_transport_read_bytes(p_call->ca_prTrans, c_buffer, 2, p_error);
	if (ILU_ERRNOK(*p_error)) {
		ilu_free(*ppc_bytes);
		*p_card_length = 0;
		*ppc_bytes = NIL;
		return;
	}
}


/* ********************************************************* */
/* output a cardinal                                         */

static void _http_output_cardinal (ilu_Call p_call, ilu_cardinal card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_cardinal_line(p_call, card, p_error);
}


/* ********************************************************* */
/* input a cardinal                                          */

static void _http_input_cardinal (ilu_Call p_call, ilu_cardinal * p_card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	_http_read_cardinal_line(p_call, p_card, p_error);
}


/* ********************************************************* */
/* output a short cardinal                                   */

static void _http_output_shortcardinal (ilu_Call p_call, ilu_shortcardinal scard,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_temp = scard;

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_cardinal_line(p_call, card_temp, p_error);
}


/* ********************************************************* */
/* input a short cardinal                                    */

static void _http_input_shortcardinal (ilu_Call p_call, ilu_shortcardinal * p_scard,
			                 ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_temp;

	_http_read_cardinal_line(p_call, &card_temp, p_error);

	*p_scard = (ilu_shortcardinal)card_temp;
}


/* ********************************************************* */
/* output a integer                                         */

static void _http_output_integer (ilu_Call p_call, ilu_integer card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_integer_line(p_call, card, p_error);
}


/* ********************************************************* */
/* input a integer                                          */

static void _http_input_integer (ilu_Call p_call, ilu_integer * p_card,
			                 ILU_ERRS((IoErrs)) * p_error) {

	_http_read_integer_line(p_call, p_card, p_error);
}


/* ********************************************************* */
/* output a short integer                                   */

static void _http_output_shortinteger (ilu_Call p_call, ilu_shortinteger scard,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_integer card_temp = scard;

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_write_integer_line(p_call, card_temp, p_error);
}


/* ********************************************************* */
/* input a short integer                                    */

static void _http_input_shortinteger (ilu_Call p_call, ilu_shortinteger * p_scard,
			                 ILU_ERRS((IoErrs)) * p_error) {
	ilu_integer card_temp;

	_http_read_integer_line(p_call, &card_temp, p_error);

	*p_scard = (ilu_shortinteger)card_temp;
}


/* ********************************************************* */
/* output a byte                                         */

static void _http_output_byte (ilu_Call p_call, ilu_byte abyte,
			                 ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [32];
	ilu_integer i_temp = (ilu_integer)abyte;

	ILU_CLER(*p_error);

	if (_http_call_state(p_call) == srvr4http_out_exception) 
		/* don't send back exception members to existing http clients */
		return;

	/* write out the byte value */
	sprintf(c_buffer, "0x%x\r\n", i_temp);
	_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
}


/* ********************************************************* */
/* input a byte                                          */

static void _http_input_byte (ilu_Call p_call, ilu_byte * p_abyte,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;
	ilu_integer i_temp;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, p_error)) == NIL)
		return ;

	/* readin the integer value */
	if (sscanf(pc_buffer, "0x%x\r\n", &i_temp) != 1) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_badInteger, 0);
		ilu_free(pc_buffer);
		return ;
	}

	*p_abyte = (ilu_byte)i_temp;

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output a shortcharacter                                   */

static void _http_output_shortchar (ilu_Call p_call, ilu_shortcharacter schar,
			                 ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_byte(p_call, (ilu_byte)schar, p_error);
}


/* ********************************************************* */
/* input a shortcharacter                                    */

static void _http_input_shortchar (ilu_Call p_call, ilu_shortcharacter * p_schar,
			                 ILU_ERRS((IoErrs)) * p_error) {

	_http_input_byte(p_call, (ilu_byte*)p_schar, p_error);
}


/* ********************************************************* */
/* output a boolean                                         */

static void _http_output_boolean (ilu_Call p_call, ilu_boolean aboolean,
			                 ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer [32];

	ILU_CLER(*p_error);

	if (_http_call_state(p_call) == srvr4http_out_exception) 
		/* don't send back exception members to existing http clients */
		return;

	/* write out the boolean value */
	sprintf(c_buffer, "%s\r\n", aboolean ? "TRUE" : "FALSE");
	_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
}


/* ********************************************************* */
/* input a boolean                                          */

static void _http_input_boolean (ilu_Call p_call, ilu_boolean * p_boolean,
			                 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, p_error)) == NIL)
		return ;

	if (strcmp(pc_buffer, "TRUE") == 0)
		*p_boolean = ilu_TRUE;
	else
		*p_boolean = ilu_FALSE;

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output opaque                                             */

static void _http_output_opaque (ilu_Call p_call, ilu_opaque an_opaque,
				  ilu_cardinal card_length,
				  ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_bytes(p_call, (ilu_bytes)an_opaque, 
		card_length, card_length, p_error);
}


/* ********************************************************* */
/* input opaque                                              */

static void _http_input_opaque (ilu_Call p_call, ilu_opaque * p_an_opaque,
				 ilu_cardinal card_length,
				 ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_num_read;

	_http_input_bytes (p_call, (ilu_bytes*)p_an_opaque,
				   &card_num_read, card_length, p_error);

	if (card_num_read != card_length)
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_duh, FALSE);
}


/* ********************************************************* */
/* output union                                              */

 static void _http_output_union (ilu_Call p_call,
				      ilu_cardinal card_discrim,
				      ilu_cardinal card_dsize,
				      ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	 _http_output_cardinal(p_call, card_discrim, p_error);
 }


/* ********************************************************* */
/* input union                                               */

 static void _http_input_union (ilu_Call p_call,
								ilu_cardinal * p_card_typeIndex,
								ilu_cardinal card_dsize,
								ILU_ERRS((IoErrs)) * p_error) {


	 _http_input_cardinal(p_call, p_card_typeIndex, p_error);
}


/* ********************************************************* */
/* end union                                                 */

 static void _http_end_union (ilu_Call p_call, 
							  ILU_ERRS((IoErrs)) * p_error) {

  ILU_CLER(*p_error); /* nothing to do */
}


/* ********************************************************* */
/* output, input, end array - really do nothing              */

static void _http_output_array(ilu_Call p_call, 
							   ilu_cardinal card_len,
							   ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

static void _http_input_array(ilu_Call p_call, 
							  ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

static void _http_end_array (ilu_Call p_call, 
							 ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

/* ********************************************************* */
/* output, input, sequence mark - really do nothing          */

static void _http_output_sequence_mark (ilu_Call p_call,
										ilu_cardinal card_extent,
										ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}

static void _http_input_sequence_mark(ilu_Call p_call, 
									  ilu_cardinal card_extent,
									  ILU_ERRS((IoErrs)) * p_error) {
  ILU_CLER(*p_error);
}


/* ********************************************************* */
/* output stringvec                                          */

static void _http_output_stringvec (ilu_Call p_call, ilu_string astring,
									ilu_cardinal card_length,
									ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_opaque (p_call, (ilu_opaque)astring, card_length, p_error);
 }


/* ********************************************************* */
/* input stringvec                                           */

static void _http_input_stringvec (ilu_Call p_call, ilu_string * p_astring,
								   ilu_cardinal card_length,
								   ILU_ERRS((IoErrs)) * p_error) {

	_http_input_opaque (p_call, (ilu_opaque*)p_astring, card_length, p_error);
 }


/* ********************************************************* */
/* output longcardinal                                       */

static void _http_output_longcardinal (ilu_Call p_call, ilu_longcardinal longcard_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_cardinal(p_call, ILU_LONGCARD_HIGH_WORD(&longcard_i), p_error);

  if (ILU_ERROK(*p_error))
	_http_output_cardinal(p_call, ILU_LONGCARD_LOW_WORD(&longcard_i), p_error);
}


/* ********************************************************* */
/* input longcardinal                                        */

static void _http_input_longcardinal(ilu_Call p_call,
									 ilu_longcardinal * p_longcard_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	_http_input_cardinal (p_call, &ILU_LONGCARD_HIGH_WORD(p_longcard_i), p_error);

	if (ILU_ERROK(*p_error))
		_http_input_cardinal (p_call, &ILU_LONGCARD_LOW_WORD(p_longcard_i), p_error);
}


/* ********************************************************* */
/* output longinteger                                       */

static void _http_output_longinteger (ilu_Call p_call, ilu_longinteger longint_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_integer(p_call, ILU_LONGINT_HIGH_WORD(&longint_i), p_error);

  if (ILU_ERROK(*p_error))
	_http_output_integer(p_call, ILU_LONGINT_LOW_WORD(&longint_i), p_error);
}


/* ********************************************************* */
/* input longinteger                                        */

static void _http_input_longinteger(ilu_Call p_call,
									 ilu_longinteger * p_longint_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	_http_input_integer (p_call, &ILU_LONGINT_HIGH_WORD(p_longint_i), p_error);

	if (ILU_ERROK(*p_error))
		_http_input_integer (p_call, (int*)(&ILU_LONGINT_LOW_WORD(p_longint_i)), p_error);
}



/* ********************************************************* */
/* output real                                       */

static void _http_output_real (ilu_Call p_call, 
							   ilu_real real_i,
							   ILU_ERRS((IoErrs)) * p_error) {

	char c_buffer[128];

	ILU_CLER(*p_error);

	if (_http_call_state(p_call) == srvr4http_out_exception) 
		/* don't send back exception members to existing http clients */
		return;

	sprintf(c_buffer, "%.64g\r\n", real_i); 
	_http_transport_write_bytes(p_call->ca_prTrans, c_buffer, strlen(c_buffer), p_error);
}


/* ********************************************************* */
/* input real                                        */

static void _http_input_real(ilu_Call p_call, 
							 ilu_real * p_real_i, 
							 ILU_ERRS((IoErrs)) * p_error) {

	ilu_string pc_buffer;

	ILU_CLER(*p_error);

	if ((pc_buffer = _http_readline(p_call, p_error)) == NIL)
		return;

	/* readin the real value */
	if (sscanf(pc_buffer, "%lg", p_real_i) != 1) {
		ilu_free(pc_buffer);
		return;
	}

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output longreal                                       */

static void _http_output_longreal (ilu_Call p_call, ilu_longreal longreal_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_readable_bytes(p_call, (ilu_bytes)&longreal_i, sizeof(ilu_longreal),
		sizeof(ilu_longreal), p_error);
}


/* ********************************************************* */
/* input longreal                                            */

static void _http_input_longreal(ilu_Call p_call,
									 ilu_longreal * p_longreal_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	ilu_cardinal card_length;
	ilu_bytes pc_buffer;

	_http_input_readable_bytes(p_call, &pc_buffer, &card_length, sizeof(ilu_longreal), p_error);

	if (ILU_ERROK(*p_error))
		memcpy((void *) p_longreal_i, (void *) pc_buffer, sizeof(ilu_longreal));

	ilu_free(pc_buffer);
}


/* ********************************************************* */
/* output shortreal                                       */

static void _http_output_shortreal (ilu_Call p_call, ilu_shortreal shortreal_i,
			   ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_real (p_call, (ilu_real) shortreal_i,  p_error); 

}


/* ********************************************************* */
/* input shortreal                                            */

static void _http_input_shortreal(ilu_Call p_call,
									 ilu_shortreal * p_shortreal_i,
									 ILU_ERRS((IoErrs)) * p_error) {

	ilu_real real_temp;

	_http_input_real(p_call, &real_temp, p_error); 

	*p_shortreal_i = (ilu_shortreal)real_temp;
}


/* ********************************************************* */
/* output character                                          */

static void _http_output_character (ilu_Call p_call, 
									ilu_character char_i, 
									ILU_ERRS((IoErrs)) * p_error) {

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	_http_output_shortcardinal(p_call, (ilu_shortcardinal) char_i, p_error);
}


/* ********************************************************* */
/* input character                                           */

static void _http_input_character (ilu_Call p_call, 
								   ilu_character * p_char_i, 
								   ILU_ERRS((IoErrs)) * p_error) {

	ilu_shortcardinal scard_temp;

	_http_input_shortcardinal(p_call, &scard_temp, p_error);

	*p_char_i = (ilu_character)scard_temp;
}



/* ********************************************************* */
/* output wide string                                        */

static void _http_output_wstring (ilu_Call p_call, 
								  ilu_wstring astring, 
								  ilu_cardinal card_length, 
								  ilu_cardinal card_limit, 
								  ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_shortcardinal* p_scard;

	ILU_CLER(*p_error);

	if (card_limit > 0 && card_length > card_limit) {
		ILU_ERR_CONS1(bad_param, p_error, minor, ilu_bpm_sequenceLimit, 0);
		return;
	}

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		/* don't send back exception members to existing http clients */
		return;
	}

	/* write out the length (+ 1 for termination) */
	_http_output_cardinal(p_call, card_length + 1, p_error);
	if (ILU_ERRNOK(*p_error))
		return;

	/* put out all the wide chars */
	p_scard = (ilu_shortcardinal*)astring;
	for (card_index = 0; card_index < card_length; card_index++) {
		_http_output_shortcardinal(p_call, *p_scard, p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		p_scard++;
	}
}


/* ********************************************************* */
/* input wide string                                         */

static void _http_input_wstring (ilu_Call p_call, 
								 ilu_wstring * p_string, 
								 ilu_cardinal * p_card_length, 
								 ilu_cardinal card_limit, 
								 ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_cardinal card_size;
	ilu_shortcardinal* p_scard;

	/* read in the length */
	_http_input_cardinal(p_call, &card_size, p_error);

	if (card_limit > 0 && card_size > card_limit) {
		ILU_ERR_CONS1(marshal, p_error, minor, ilu_mm_sequenceLimit, 0);
		return;
	}

	*p_card_length = card_size;

	/* get right amount of space */
	*p_string = (ilu_wstring) ilu_MallocE(((*p_card_length) * sizeof(ilu_wstring)),
											p_error);
	if (*p_string == NIL) {
		*p_card_length = 0;
		return;
	}

	/* read in the chars */
	p_scard = (ilu_shortcardinal*)(*p_string);
	for (card_index = 0; card_index < *p_card_length; card_index++) {
		_http_input_shortcardinal(p_call, p_scard, p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		p_scard++;
	}

	/* null terminate */
	*p_scard = 0;

}


/* ********************************************************* */
/* output wide char vector                                   */

static void _http_output_wstringvec (ilu_Call p_call, 
									 ilu_wstring astring, 
									 ilu_cardinal card_length, 
									 ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_shortcardinal* p_scard;

	if (_http_call_state(p_call) == srvr4http_out_exception) {
		ILU_CLER(*p_error);
		/* don't send back exception members to existing http clients */
		return;
	}

	/* put out all the wide chars */
	p_scard = (ilu_shortcardinal*)astring;
	for (card_index = 0; card_index < card_length; card_index++) {
		_http_output_shortcardinal(p_call, *p_scard, p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		p_scard++;
	}
}


/* ********************************************************* */
/* input wide char vector                                    */

static void _http_input_wstringvec (ilu_Call p_call, 
									ilu_wstring * p_string, 
									ilu_cardinal card_length, 
									ILU_ERRS((IoErrs)) * p_error) {
	ilu_cardinal card_index;
	ilu_shortcardinal* p_scard;

	/* get right amount of space */
	*p_string = (ilu_wstring) ilu_MallocE((card_length * sizeof(ilu_wstring)),
											p_error);
	if (*p_string == NIL)
		return;

	/* read in the chars */
	p_scard = (ilu_shortcardinal*)(*p_string);
	for (card_index = 0; card_index < card_length; card_index++) {
		_http_input_shortcardinal(p_call, p_scard, p_error);
		if (ILU_ERRNOK(*p_error))
			return;
		p_scard++;
	}
}


/* ********************************************************* */
/* delay, resume abandon interp functions                    */
/* - do nothing because we're not concurrent                 */
/* ********************************************************* */

static ilu_refany _http_delay_interp(ilu_Call p_call, ILU_ERRS((IoErrs)) * p_error) {
	_ilu_Assert(ilu_FALSE, g_c_not_concurrent);
	return NIL; /* to satisfy compiler only */
}


static void _http_resume_interp (ilu_Call p_call, ilu_refany refany_x) {
	_ilu_Assert(ilu_FALSE, g_c_not_concurrent);
}


static ilu_boolean _http_abandon_delayed_interp (ilu_refany refany_x, 
												 ILU_ERRS((internal)) * p_error) {
	_ilu_Assert(ilu_FALSE, g_c_not_concurrent);
	return ilu_FALSE; /* to satisfy compiler only */
}


/* ********************************************************* */
/* discard functions                                         */
/* really don't do anything - they rely on finish call being 
   entered in an unexpected state, in which case it just blows 
   away the whole connection. */
/* ********************************************************* */

/* ********************************************************* */
/* 	discard output                                           */

static ilu_boolean _http_discard_output (ilu_Call p_call, 
										 ILU_ERRS((IoErrs)) * p_error) {
	ILU_CLER(*p_error);
	return ilu_TRUE; /* nothing to really do */
}

/* ********************************************************* */
/* 	discard input                                            */

static ilu_boolean _http_discard_input (ilu_Call p_call, 
										  ILU_ERRS((IoErrs)) * p_error) {
	ILU_CLER(*p_error);
	return ilu_TRUE; /* nothing to really do */
}



/* ********************************************************* */
/* creates and returns an ilu_Protocol for http              */
/* ********************************************************* */

ilu_Protocol _ilu_http_Protocol (void) {

	ilu_Protocol p_http_protocol;

	/* first create a husk containing all NILs so we'll 
	   seg fault if anything unimplemented is called */

	p_http_protocol = ilu_malloc(sizeof(struct _ilu_Protocol_s));
	if (p_http_protocol == ILU_NIL)
	  return ILU_NIL;
	memset((void *) p_http_protocol, 0, sizeof(struct _ilu_Protocol_s));

	/* now fill in the actual implementations */

	/* don't need sizing information */
	p_http_protocol->pr_sizing_required = ilu_FALSE;

	/* returns a string that's used to form the protocol info part of a string binding handle */
	p_http_protocol->pr_form_handle = _http_form_protocol_handle;

	/* create and free a http data block - contains any (member-var like)
	   attributes specific to the protocol */
	p_http_protocol->pr_create_data_block = _http_create_non_batching_non_concurrent_data_block;
	p_http_protocol->pr_free_data_block = (void (*)(ilu_refany block)) _http_free_data_block;
	p_http_protocol->pr_concurrent_requests = ilu_FALSE; /* must agree with type of data block */

	/* protocol specific initialization */
	p_http_protocol->pr_init_call = _http_init_call;

	/* finishing send of a record */
	p_http_protocol->pr_end_record = _http_end_record;

	/* finishing send of a sequence */
	p_http_protocol->pr_end_sequence = _http_end_sequence;

	/* start actually sending a request (puts out method name, SP) */
	p_http_protocol->pr_start_request = _http_start_request;

	/* function that will output an objects id as it's passed across the wire */
	p_http_protocol->pr_output_object_id = _http_output_object_id;

	/* inputs an object id */
	p_http_protocol->pr_input_object_id = _http_input_object_id;

	/* outputs starting of a record (actually outputs nothing) */
	p_http_protocol->pr_output_record = _http_output_record;

	/* outputs a string */	
	p_http_protocol->pr_output_string = _http_output_string;

	/* send start of a sequence */	
	p_http_protocol->pr_output_sequence = _http_output_sequence;

	/* sending an optional */
	p_http_protocol->pr_output_optional = _http_output_optional;

	/* sends a bunch of bytes */
	p_http_protocol->pr_output_bytes = _http_output_bytes;

	/* finishes sending a request */
	p_http_protocol->pr_finish_request = _http_finish_request;

	/* reads in a header */
	p_http_protocol->pr_read_header = _http_read_header;

	/* interprets http reply */
	p_http_protocol->pr_interpret_reply = _http_interpret_reply; 

	/* inputs begining of a record */
	p_http_protocol->pr_input_record = _http_input_record;

	/* inputs an enumeration code */
	p_http_protocol->pr_input_enum_code = _http_input_enum_code;

	/* inputs a sequence */
	p_http_protocol->pr_input_sequence = _http_input_sequence;

	/* input a string */
	p_http_protocol->pr_input_string = _http_input_string;

	/* input an optional */
	p_http_protocol->pr_input_optional = _http_input_optional;

	/* input an bytes */
	p_http_protocol->pr_input_bytes = _http_input_bytes;

	/* what to do at end of receiving reply */
	p_http_protocol->pr_reply_read = _http_reply_read;

	/* discard output */
	p_http_protocol->pr_discard_output = _http_discard_output;

	/* discard input */
	p_http_protocol->pr_discard_input = _http_discard_input;

	/*pre finishes and finishes a call */
	p_http_protocol->pr_finish_call = _http_finish_call;
	p_http_protocol->pr_prefinish_call = _http_prefinish_call;

	/* interprets a request */
	p_http_protocol->pr_interpret_request = _http_interpret_request;
	
	/* Do what needs to be done after arguments unmarshalling */
	p_http_protocol->pr_request_read = _http_request_read;

	/* begins a reply */
	p_http_protocol->pr_begin_reply = _http_begin_reply;

	/* outputs an enum value */
	p_http_protocol->pr_output_enum_code = _http_output_enum_code;

	/* finishes a reply */
	p_http_protocol->pr_finish_reply = _http_finish_reply;

	/* output and input of a cardinal */
	p_http_protocol->pr_output_cardinal = _http_output_cardinal;
	p_http_protocol->pr_input_cardinal = _http_input_cardinal;

	/* output and input of a short cardinal */
	p_http_protocol->pr_output_shortcardinal = _http_output_shortcardinal;
	p_http_protocol->pr_input_shortcardinal = _http_input_shortcardinal;

	/* output and input of a integer */
	p_http_protocol->pr_output_integer = _http_output_integer;
	p_http_protocol->pr_input_integer = _http_input_integer;

	/* output and input of a short integer */
	p_http_protocol->pr_output_shortinteger = _http_output_shortinteger;
	p_http_protocol->pr_input_shortinteger = _http_input_shortinteger;

	/* output and input of a byte */
	p_http_protocol->pr_output_byte = _http_output_byte;
	p_http_protocol->pr_input_byte = _http_input_byte;

	/* output and input of a shortchar */
	p_http_protocol->pr_output_shortchar = _http_output_shortchar;
	p_http_protocol->pr_input_shortchar = _http_input_shortchar;

	/* output and input of a boolean */
	p_http_protocol->pr_output_boolean = _http_output_boolean;
	p_http_protocol->pr_input_boolean = _http_input_boolean;

	/* output and input of an opaque */
	p_http_protocol->pr_output_opaque = _http_output_opaque;
	p_http_protocol->pr_input_opaque = _http_input_opaque;

	/* output and input of a union */
	p_http_protocol->pr_output_union = _http_output_union;
	p_http_protocol->pr_input_union = _http_input_union;
	p_http_protocol->pr_end_union = _http_end_union;

	/* output, input and end of an array */
	p_http_protocol->pr_output_array = _http_output_array;
	p_http_protocol->pr_input_array = _http_input_array;
	p_http_protocol->pr_end_array = _http_end_array;

	/* output and input of a sequence mark */
	p_http_protocol->pr_output_sequence_mark = _http_output_sequence_mark;
	p_http_protocol->pr_input_sequence_mark = _http_input_sequence_mark;

	/* output and input of a stringvec */
	p_http_protocol->pr_output_stringvec = _http_output_stringvec;
	p_http_protocol->pr_input_stringvec = _http_input_stringvec;

	/* output and input of a longcardinal */
	p_http_protocol->pr_output_longcardinal = _http_output_longcardinal;
	p_http_protocol->pr_input_longcardinal = _http_input_longcardinal;

	/* output and input of a longinteger */
	p_http_protocol->pr_output_longinteger = _http_output_longinteger;
	p_http_protocol->pr_input_longinteger = _http_input_longinteger;

	/* output and input of a real */
	p_http_protocol->pr_output_real = _http_output_real;
	p_http_protocol->pr_input_real = _http_input_real;

	/* output and input of a longreal */
	p_http_protocol->pr_output_longreal = _http_output_longreal;
	p_http_protocol->pr_input_longreal = _http_input_longreal;

	/* output and input of a shortreal */
	p_http_protocol->pr_output_shortreal = _http_output_shortreal;
	p_http_protocol->pr_input_shortreal = _http_input_shortreal;

	/* output and input of a character */
	p_http_protocol->pr_output_character = _http_output_character;
	p_http_protocol->pr_input_character = _http_input_character;

	/* output and input of a wstring */
	p_http_protocol->pr_output_wstring = _http_output_wstring;
	p_http_protocol->pr_input_wstring = _http_input_wstring;

	/* output and input of a wstringvec */
	p_http_protocol->pr_output_wstringvec = _http_output_wstringvec;
	p_http_protocol->pr_input_wstringvec = _http_input_wstringvec;

	/* delay, resume, abandon interp - do nothing because we're not concurrent */
	p_http_protocol->pr_delay_interp = _http_delay_interp;
	p_http_protocol->pr_resume_interp = _http_resume_interp;
	p_http_protocol->pr_abandon_delayed_interp = _http_abandon_delayed_interp;

	/* begin and finish exception */
	p_http_protocol->pr_begin_exception = _http_begin_exception;
	p_http_protocol->pr_finish_exception = _http_finish_exception;

	/* return back our filled in protocol structure */
	return p_http_protocol;
}


/* ********************************************************* */
/* if we're supposed to be using a proxy server, copys the 
proxy hostname into *pc_proxyname, puts the port numbers
into p_ul_proxy_port, and returns ilu_TRUE, else leaves the args
alone and returns ilu_FALSE   */


static ilu_boolean _http_get_proxy_name_and_port(char* pc_proxyname, 
												 unsigned long* p_ul_proxy_port) {

	char* pc_the_info;

	/* ILU_HTTP_PROXY_INFO is of the form proxy.host.name:portnumber */

	if ((pc_the_info = getenv(ILU_HTTP_PROXY_INFO_ENV_VAR)) == NIL)
		return ilu_FALSE; /* no proxy info set */

	/* copy the proxy host name */
	while (*pc_the_info && (*pc_the_info != ':')) {
		*pc_proxyname = *pc_the_info;
		pc_proxyname++;
		pc_the_info++;
	}

	*pc_proxyname = '\0';

	if (*pc_the_info != ':') /* must specify port number */
		return ilu_FALSE;

	pc_the_info++; /* advance to start of port number */

	/* convert it */
	if (sscanf(pc_the_info, "%lu", p_ul_proxy_port) != 1)
		return ilu_FALSE;

	return ilu_TRUE;
}


/* ********************************************************* */
/* returns ilu_TRUE if the two hostnames are in the same 
domain - i.e. they don't need a proxy to talk, else ilu_FALSE */

static ilu_boolean _http_same_domain(char* pc_proxyname, char* pc_hostname) {

	/* XXX I dont' believe there is a general solution to this question  -
	I think it's site specific */

	return ilu_FALSE;
}


/* ********************************************************* */
/* if we're supposed to be using a proxy server to talk to the
host in pc_hostname, sets the args appropriately and returns 
ilu_TRUE, else leaves the args alone and returns ilu_FALSE   */

static ilu_boolean _http_proxy_contact_info(char** ppc_dotdec, 
											unsigned long* p_ul_port,
											char* pc_hostname,
											ilu_Error* p_error) {

	char c_proxyname[1024];
	struct hostent* p_hostent;
	struct in_addr inaddr;
	char* pc_dotdec;
	static char* s_pc_proxy_dot_dec = NIL;	/* cache dotted decimal for proxy server*/
	static char* s_pc_proxy_name = NIL;	    /* cache name of proxy server */
	static unsigned long s_ul_proxy_port = 8000;    /* cache port of proxy server */

	ILU_CLER(*p_error);

	/* if we're not using a proxy, just return */
	if (!_http_get_proxy_name_and_port(c_proxyname, &s_ul_proxy_port))
		return ilu_FALSE;

	if (_http_same_domain(c_proxyname, pc_hostname))
		/* we're in the same domain, so don't need to use proxy */
		return ilu_FALSE;

	if ((s_pc_proxy_name == NIL) || 
		(strcmp(s_pc_proxy_name, c_proxyname) != 0)) {

		/* first time or proxy name changed */
		ilu_free(s_pc_proxy_name);
		s_pc_proxy_name = ilu_StrdupE(c_proxyname, p_error);
		if (ILU_ERRNOK(*p_error))
			goto error_cleanup_label;

		/* we're using a proxy server */
		if ((p_hostent = gethostbyname(c_proxyname)) == NIL) 	/* get dotted decimal ip address */
			goto error_cleanup_label;

		memcpy((void *) &inaddr.s_addr, *p_hostent->h_addr_list, sizeof (inaddr.s_addr));

		if ((pc_dotdec = inet_ntoa(inaddr)) == NIL)
			goto error_cleanup_label;

		/* XXX pc_dotdec points to a static buffer - potential for problems in a 
		   threaded environment !!  - need a mutex of some sort */

		ilu_free(s_pc_proxy_dot_dec);
		s_pc_proxy_dot_dec = ilu_StrdupE(pc_dotdec, p_error);
		if (ILU_ERRNOK(*p_error)) 
			goto error_cleanup_label;
	}

	/* return cached values */
	*ppc_dotdec = s_pc_proxy_dot_dec;
	*p_ul_port = s_ul_proxy_port;

	return ilu_TRUE;
	
error_cleanup_label:

	ilu_free(s_pc_proxy_name);
	ilu_free(s_pc_proxy_dot_dec);
	s_pc_proxy_name = NIL;
	s_pc_proxy_dot_dec = NIL;
	return ilu_FALSE;
}


/* ********************************************************* */
/* sets the encoded constact information for an object, 
   taking potential use of http proxy servers into account 
   return s false on error  */

static ilu_boolean _http_generate_contact_info (ilu_string* p_str_encodedContactInfo, 
												char* pc_hostname,
												unsigned long ul_port,
												ilu_Error* p_error) {

	ilu_string str_protohandle;
	struct hostent* p_hostent;
	struct in_addr inaddr;
	char* pc_dotdec;

	ILU_CLER(*p_error);

	str_protohandle = _http_form_protocol_handle(NIL);		/* get protocol information */

	/* see if we're supposed to be using a proxy server to talk to the host in pc_hostname*/
	if (_http_proxy_contact_info(&pc_dotdec, &ul_port, pc_hostname, p_error) == ilu_FALSE) {

		if (ILU_ERRNOK(*p_error)) /* if we returned false because of an error */
			return ilu_FALSE;

		/* we're not using a proxy, so create based on passed hostname and port */

		if ((p_hostent = gethostbyname(pc_hostname)) == NIL)	{ /* get dotted decimal ip address */
			ilu_free(str_protohandle);
			return ilu_FALSE;
		}
		memcpy((void *) &inaddr.s_addr, *p_hostent->h_addr_list, sizeof (inaddr.s_addr));
		if ((pc_dotdec = inet_ntoa(inaddr)) == NIL) {
			ilu_free(str_protohandle);
			return ilu_FALSE;
		}
		/* XXX pc_dotdec points to a static buffer - potential for problems in a 
	   threaded environment !!  - need a mutex of some sort */
	}

	*p_str_encodedContactInfo  = ilu_MallocE(strlen(str_protohandle) + 
									1 +		/* ILU_CINFO_DIVIDER */
									4 +		/* tcp_ */
									strlen(pc_dotdec) + 
									1 +		/* _ */
									10 +	/* port */
									1,		/* null */ 
									p_error);
	if (ILU_ERRNOK(*p_error))  
		return ilu_FALSE;

	/* cons them together */
	sprintf(*p_str_encodedContactInfo, "%s%ctcp_%s_%lu", str_protohandle, 
		ILU_CINFO_DIVIDER, pc_dotdec, ul_port);
	ilu_free(str_protohandle);

	return ilu_TRUE;
}



/* ********************************************************* */
/* function to parse up a http url                           */

#define RETURN_FALSE_IF_STRDUPEERROR(p_error) if (ILU_ERRNOK(p_error)) { \
		fprintf(stderr, "_ilu_Parse_HTTP_URL couldn't ilu_StrdupE\n"); \
		return ilu_FALSE; \
	}


ilu_boolean _ilu_Parse_HTTP_URL (ilu_string    istr_encodedSBH, 
								 ilu_string*   p_str_plainInstanceHandle, 
								 ilu_string*   p_str_plainServerID,
								 ilu_string*   p_str_plainMstid, 
								 ilu_string*   p_str_encodedContactInfo, 
								 ilu_cardinal* p_card_encodedContactInfoLen,
								 ilu_Error*    p_error) {

	/* Parse HTTP style URL: http://<host>[:<port>]/<path> */

	unsigned long ul_port;
	ilu_string pc_walker;
	char* pc_hostname;
	char* pc_hostname_end;
	char* pc_hostname_copy;
	char* pc_path;
	char* pc_path_end;
	ilu_cardinal card_size;
	char c_path_default[] = "/";


	ILU_CLER(*p_error);

	/* check for valid spots to store things */
	if (p_str_plainInstanceHandle == NIL	|| 
		p_str_plainServerID == NIL			||
		p_str_plainMstid == NIL				|| 
		p_str_encodedContactInfo == NIL		||
		p_card_encodedContactInfoLen == NIL)	
		return ilu_FALSE;

	/* ensure that the istr_encodedSBH is for http */
	pc_walker = istr_encodedSBH;
	if (strstr(pc_walker, "http://") != pc_walker)
		return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);

	/* we go though the following parsing & copying contortions because we don't want to make any
	modifications to the contents of istr_encodedSBH, and we don't want to try using
	any fixed sized buffers (e.g. with sscanf) because the passed istr_encodedSBH could
	be quite huge */

	/* point to where the hostname should start */
	pc_walker = pc_walker + 7;
	pc_hostname = pc_walker;
	while (*pc_walker && (*pc_walker != ':') && (*pc_walker != '/'))
		pc_walker++;	/* advance to end of hostname */

	pc_hostname_end = pc_walker;		/* save where a null in the hostname would be */
	ul_port = DEFAULT_HTTP_PORT_NUMBER;	/*  default the port to start with */

	switch (*pc_walker) {

	case '/':	/* no port specified - use default port */
		pc_path = pc_walker;				/* save where the path starts */
		break;

	case ':':	/* maybe found a port number */
		while (*pc_walker && (*pc_walker != '/'))
			pc_walker++;	/* advance to end of port number */

		switch (*pc_walker) {

		case '/': /* a port with some following path */

			if (sscanf (pc_hostname_end + 1, "%lu/", &ul_port) != 1)
				/* bad somehow, return malformed SBH error */
				return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);

			pc_path = pc_walker;				/* save where the path starts */

			break;

		case '\0': /* just a port with no training / for a path */

			if (sscanf (pc_hostname_end + 1, "%lu", &ul_port) != 1)
				/* bad somehow, return malformed SBH error */
				return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);

			pc_path = c_path_default;			/* set to default path */
	
			break;

		default: /* what else could it be??? */
			return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);
		}

		break;

	case '\0': /* must have been just hostname without port or path  */
		pc_path = c_path_default;			/* set to default path */
		break;

	default: /* what else could it be??? */
		return ILU_ERR_CONS1(inv_objref, p_error, minor, ilu_iom_sbh, ilu_FALSE);
	}

	/* find the end of the actual path (dont' want any params or queries in it) */
	pc_path_end = pc_path;
	while (*pc_path_end && (*pc_path_end != ';') && (*pc_path_end != '?'))
		pc_path_end++;

	/* so now, pc_hostname points to the start of the hostname, pc_hostname_end points
	   to where a null would be in a null terminated version of the hostname, and
	   pc_path points to the path, and pc_path_end points to where a null would be 
	   in a null terminated version of the path */

	/* copy the path in as the instance handle */
	card_size = pc_path_end - pc_path + 1;
	*p_str_plainInstanceHandle = ilu_MallocE(pc_path_end - pc_path + 1, p_error);
	if (ILU_ERRNOK(*p_error))  
		return ilu_FALSE;
	
	memcpy(*p_str_plainInstanceHandle, pc_path, pc_path_end - pc_path);
	*(*p_str_plainInstanceHandle + (pc_path_end - pc_path)) = '\0';  /* null terminate */

	/* make a convenient copy of the hostname */
	pc_hostname_copy =  ilu_MallocE(pc_hostname_end - pc_hostname + 1, p_error);
	if (ILU_ERRNOK(*p_error)) {
		ilu_free(*p_str_plainInstanceHandle);
		return ilu_FALSE;
	}
	memcpy(pc_hostname_copy, pc_hostname, pc_hostname_end - pc_hostname);
	*(pc_hostname_copy + (pc_hostname_end - pc_hostname)) = '\0';

	/* invent a server id based on target host name and port */
	card_size =	6 +								/* http. */
				pc_hostname_end - pc_hostname +	/* hostname */
				1 +								/* underscore */
				10 +							/* portnumber */
				1;								/* null terminate */
	*p_str_plainServerID = ilu_MallocE(card_size, p_error);
	if (ILU_ERRNOK(*p_error)) {
		ilu_free(pc_hostname_copy);
		ilu_free(*p_str_plainInstanceHandle);
		return ilu_FALSE;
	}

	sprintf(*p_str_plainServerID, "%s%s_%lu", "httpd.", pc_hostname_copy, ul_port);

	/* we know that http SBH's are for this type */
	*p_str_plainMstid = ilu_StrdupE(HTTP_RESOURCE_OBJECT_TYPE_ID, p_error);
	if (ILU_ERRNOK(*p_error)) { 
		ilu_free(pc_hostname_copy);
		ilu_free(*p_str_plainInstanceHandle);
		ilu_free(*p_str_plainServerID);
		return ilu_FALSE;
	}
  
	/* cons up the contact information */

	if (!_http_generate_contact_info (p_str_encodedContactInfo, pc_hostname_copy, ul_port, p_error)) {
		ilu_free(pc_hostname_copy);
		ilu_free(*p_str_plainInstanceHandle);
		ilu_free(*p_str_plainServerID);
		ilu_free(*p_str_plainMstid);
		return ilu_FALSE;
	}

	ilu_free(pc_hostname_copy);

	*p_card_encodedContactInfoLen = strlen(*p_str_encodedContactInfo);

	return ilu_TRUE;
}


#if 0

/* ********************************************************* */
/* setup functionality for http                              */

ILU_ERRS((ProtocolAlreadyRegistered, MaxCountExceeded)) 
setup_http_protocol (void) {

	ilu_RegisterSBHParser ("http", _ilu_Parse_HTTP_URL);
	return ilu_RegisterProtocol("http", _ilu_http_Protocol, ilu_FALSE);
}

#endif


/* ********************************************************* */
/* end of file                                               */
/* ********************************************************* */
