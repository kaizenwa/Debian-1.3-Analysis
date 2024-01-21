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
/* $Id: transprt.h,v 1.34 1996/05/04 04:54:12 janssen Exp $ */
/* Last edited by Mike Spreitzer November 1, 1995 11:09 am PST */

#ifndef _TRANSPORT_H_
#define _TRANSPORT_H_ 1

/* some field accessors */

#define MAX_TRANSPORTS	10

#define transport_data(bs)		((bs)->tr_data)
#define transport_class(bs)		((bs)->tr_class)

#define transport_reliable(bs)	((bs)->tr_class->tc_reliable)
#define transport_boundaried(bs)  ((bs)->tr_class->tc_boundaried)

/* some macros for method dereferencing */

#define transport_close(bs,dfd,err)			((*((bs)->tr_class->tc_close))(bs,dfd,err))
#define transport_wait_for_input(bs,limit,err)	((*((bs)->tr_class->tc_wait_for_input))(bs,limit,err))
#define transport_interrupt(bs,err)		((*((bs)->tr_class->tc_interrupt))((bs),(err)))

#define transport_begin_message(bs,inputp,err)	((*((bs)->tr_class->tc_begin_message))((bs),(inputp),(err)))
#define transport_end_message(bs,flushp,msgh,err)	((*((bs)->tr_class->tc_end_message))((bs),(flushp),(msgh),(err)))
#define transport_send_whole_message(bs,msgh,err)	((*((bs)->tr_class->tc_send_whole_message))((bs),(msgh),(err)))

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu}*/
extern ilu_boolean 
ilu_SetTransportInputHandler(ilu_Transport trans,
			     ilu_TransportInputHandler tih,
			     ilu_refany tih_rock,
			     ILU_ERRS((no_memory, internal,
				       no_resources)) * err);

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
extern          ilu_boolean
_ilu_transportWriteBytes(ilu_Transport /* self */ ,
			 ilu_bytes /* buf */ ,
			 ilu_cardinal /* len */ ,
			 ILU_ERRS((IoErrs)) * /* err */ );
/* Write the given buffer contents to the given transport. */

#define transport_write_bytes(bs,buf,len,err)			\
(((bs)->tr_outBuff != NIL && (bs)->tr_outNext < (bs)->tr_outLimit \
  &&(len) <= (bs)->tr_outLimit - (bs)->tr_outNext)		\
 ? (memcpy((void *) ((bs)->tr_outBuff + (bs)->tr_outNext),	\
	   (void *) (buf), (len)),				\
    (bs)->tr_outNext += (len), ILU_CLER(*(err)), ilu_TRUE)	\
 : _ilu_transportWriteBytes(bs, buf, len, err))
/* Efficient wrapper around above procedure. */

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
extern ilu_bytes 
_ilu_transportGetOutputBuffer(ilu_Transport /* self */ ,
			      ilu_cardinal /* len */ ,
			      ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Return the location where the next /len/ bytes of output are to
 * be put.
 */

#define transport_get_output_buffer(bs,len,err)			\
(((bs)->tr_outBuff != NIL && (bs)->tr_outNext < (bs)->tr_outLimit \
  && (len) <= (bs)->tr_outLimit - (bs)->tr_outNext)		\
 ? (ILU_CLER(*(err)),						\
    (bs)->tr_outBuff + ((bs)->tr_outNext += (len)) - (len))	\
 : _ilu_transportGetOutputBuffer(bs, len, err))
/* Efficient wrapper around above procedure. */

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
extern ilu_cardinal 
_ilu_transportReadBytes(ilu_Transport /* self */ ,
			ilu_bytes /* buf */ ,
			ilu_cardinal /* len */ ,
			ILU_ERRS((IoErrs)) * /* err */ );
/* Read the next /len/ bytes into given buffer. Returns num read */

#define transport_read_bytes(bs,buf,len,err)                     \
(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit   \
  && (len) <= (bs)->tr_inLimit - (bs)->tr_inNext)                \
 ? (memcpy((void *) (buf),                                       \
	   (void *) ((bs)->tr_inBuff + (bs)->tr_inNext), (len)), \
    (bs)->tr_inNext += (len), ILU_CLER(*(err)), len)             \
 : _ilu_transportReadBytes(bs, buf, len, err))
/* Efficient wrapper around above procedure. */


/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
extern ilu_bytes 
_ilu_transportGetInputBuffer(ilu_Transport /* self */ ,
			     ilu_cardinal /* len */ ,
			     ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Return a location from which the next /len/ bytes of input can be
 * read; len <= 16.
 */

#define transport_get_input_buffer(bs,len,err)			\
(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit	\
  && (len) <= (bs)->tr_inLimit - (bs)->tr_inNext)		\
 ? (ILU_CLER(*(err)),						\
    (bs)->tr_inBuff + ((bs)->tr_inNext += (len)) - (len))	\
 : _ilu_transportGetInputBuffer(bs, len, err))
/* Efficient wrapper around above procedure. */

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
extern          ilu_cardinal
_ilu_transportReadUpToBytes(ilu_Transport /* self */ ,
			    ilu_bytes /* buf */ ,
			    ilu_cardinal /* len */ ,
			    ilu_TransportReport * /* rpt */ ,
			    ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Read some bytes into the given buffer. Does not read past message
 * boundary (if boundaried), EOF, or end of given buffer.  Does not
 * block.  Makes as much input progress as is permitted by the above
 * restrictions.  Sets EOM and EOF bits in *rpt; may falsely set
 * them to FALSE if at least 1 byte was delivered.  Returns number
 * of bytes delivered.  Raises internal/bytesWithoutMsg if
 * boundaried and not currently inputting a message.  Caller retains
 * ownership of *buf.
 */

#define transport_read_upto_bytes(bs,buf,len,rpt,err)			\
(((bs)->tr_inBuff != NIL && (bs)->tr_inNext < (bs)->tr_inLimit		\
  && (len) <= (bs)->tr_inLimit - (bs)->tr_inNext)			\
 ? (memcpy((void *) (buf),						\
	   (void *) ((bs)->tr_inBuff + (bs)->tr_inNext), (len)),	\
    (rpt)->tr_eom = (rpt)->tr_eof = FALSE,				\
    (bs)->tr_inNext += (len), ILU_CLER(*(err)), (len))			\
 : _ilu_transportReadUpToBytes(bs, buf, len, rpt, err))
/* Efficient wrapper around above procedure. */

/*Main Invariant holds; L2 >= {conn's I/O mutex}*/
extern          ilu_boolean
_ilu_transportReadMessage(ilu_Transport /* self */ ,
			  ilu_bytes * /* msg */ ,
			  ilu_cardinal * /* len */ ,
			  ilu_TransportReport * /* rpt */ ,
			  ILU_ERRS((IoErrs)) * /* err */ );
/*
 * Applicable to boundaried transports currently inputting a
 * message.  Does not call begin_message.  Reads the rest of the
 * current message and returns it (the part read) in a contiguous
 * buffer.  Does not call end_message.  rpt->tr_eom is never
 * significant.
 */

/*Main Invariant holds; L2 >= {t's conn's I/O mutex}*/
extern          ilu_Transport
_ilu_BufferInputMessage(ilu_Transport t,
			ILU_ERRS((IoErrs)) * err);
/*
 * Reads the rest of the message currently being input, and calls
 * transport_end_message(t, ..) --- unless and until error is
 * raised.  Returns a reliable boundaried transport that can be used
 * to re-read those bytes, then reaches EOM and EOF.  Close the
 * result to free the last of its resources.
 */

/* Main Invariant holds */
ilu_Transport
_ilu_BufferTransport_Create (ilu_cardinal size, ilu_bytes buffer,
			     ILU_ERRS((IoErrs)) * err);
/*
 * Create a Buffer Transport, usable for one message in one
 * direction.  When /buffer/ == NIL, create an output buffer,
 * initially /size/; when /buffer/ != NIL, create an input buffer,
 * of /size/.  If /buffer is non-NIL, ownership of /buffer/ is
 * transferred to the Transport.
 */

/*L1, l2?*/
extern void 
_ilu_BufferTransport_Destroy (ilu_Transport self,
			      ilu_cardinal * size, ilu_bytes * buffer,
			      ILU_ERRS((IoErrs)) * err);
/* Ends message and tears down transport.  Call with non-NIL "buffer"
   to transfer buffer back to caller; otherwise it's freed when the
   transport is freed. */

/* L1_sup < trmu; L2 unconstrained */

#ifdef TCPIP_TRANSPORT
extern ilu_TransportCreator 
_ilu_tcp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* TCPIP_TRANSPORT */

#ifdef UDPSOCKET_TRANSPORT
extern ilu_TransportCreator 
_ilu_udp_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* UDPSOCKET_TRANSPORT */

#ifdef SUNRPCRM_TRANSPORT
extern ilu_TransportCreator 
_ilu_sunrpcrm_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* def SUNRPCRM_TRANSPORT */

#ifdef SECURE_TRANSPORT
extern ilu_TransportCreator 
_ilu_security_TransportCreator(ilu_TransportInfo tinfo,
			       ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* def SECURE_TRANSPORT */

#ifdef W3MUX_TRANSPORT
extern ilu_TransportCreator 
_ilu_w3mux_TransportCreator(ilu_TransportInfo tinfo,
			    ILU_ERRS((no_memory, inv_objref)) * err);
#endif /* W3MUX_TRANSPORT */

extern ilu_TransportCreator 
_ilu_inmem_TransportCreator(ilu_TransportInfo tinfo,
			  ILU_ERRS((no_memory, inv_objref)) * err);


#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#endif
