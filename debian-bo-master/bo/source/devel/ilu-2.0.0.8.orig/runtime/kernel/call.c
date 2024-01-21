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
/* $Id: call.c,v 1.191 1996/07/15 22:33:49 janssen Exp $ */
/* Last edited by Mike Spreitzer June 27, 1996 4:29 pm PDT */

#define _POSIX_SOURCE

#include <stddef.h>

#include "iluntrnl.h"
#include "protocol.h"
#include "connect.h"
#include "transprt.h"
#include "port.h"
#include "call.h"
#include "object.h"
#include "method.h"
#include "server.h"
#include "type.h"

#if 0
/* Do we still need this?  errno not used any more */
#if (defined(WIN32) || defined(WIN16))
/* xxx dll are all the errno's in here really realated to socket calls ??? */
#include <winsock.h>
#endif /* WIN32 or WIN16 */
#endif

const char     *ilu_PENames[ilu_ProtocolException_Not + 1] = {
  "Success",
  "NoSuchClassAtServer",
  "ClassVersionMismatch",
  "NoSuchMethodOnClass",
  "GarbageArguments",
  "Unknown",
  "LostConnection",
  "RequestRejected",
  "RequestTimeout",
  "Not"
};

typedef ilu_shortcardinal ilu_WChar;
typedef struct ilu_WString_s {
  /* L1, L2, Main unconstrained */

  ilu_cardinal    len;
  ilu_WChar      *chars;
}              *ilu_WString;

/*L1, L2, Main unconstrained*/

ilu_cardinal _ilu_SafeStrlen(ilu_string s)
{
  if (s == NIL)
    return 0;
  else
    return (strlen(s));
}

ilu_Class ilu_IntroTypeOfCall (ilu_Call call)
{
  if (call == NIL)
    return (NIL);
  else
    return (call->ca_intro_type);
}

ilu_Connection ilu_ConnectionOfCall (ilu_Call call)
{
  if (call == NIL)
    return (NIL);
  else
    return (call->ca_connection);
}

ilu_boolean ilu_CallNeedsSizing (ilu_Call call)
{
  if (call == NIL)
    return (ilu_FALSE);
  else
    return (protocol_needs_sizing(connection_protocol(call_connection(call))));
}

ilu_Method ilu_MethodOfCall (ilu_Call call)
{
  if (call == NIL)
    return (NIL);
  else
    return (call->ca_method);
}

static void
BuildCall(ilu_Call call, ilu_Connection connection, ilu_Server server,
	  ilu_cardinal serialNumber, ilu_boolean incoming)
{
  static ilu_Message nomsg = {NIL, 0};
  call->ca_SN = serialNumber;
  call->ca_server = server;
  call->ca_intro_type = NIL;
  call->ca_method = NIL;
  call->ca_connection = connection;
  call->ca_private = NIL;
  call->ca_callee = NIL;
  call->ca_caller = NIL;
  call->ca_irq = FALSE;
  call->ca_prdata1 = 0;
  call->ca_prdata2 = NIL;
  call->ca_msg = nomsg;
  call->ca_prTrans = NIL;
  call->ca_incoming = incoming;
  call->ca_ios = ilu_ciosNone;
  call->ca_ms = ilu_cmsNo;
  call->ca_pe = ilu_ProtocolException_Success;
  call->ca_reqs_enabled = FALSE;
  return;
}

/*
 * Returns TRUE iff connection should continue to be monitored.
 * Returns FALSE if (but not only if) raising an ilu_Error.
 */
/*L1 = {cmu, server}; Main Remnant; L2&conn = {callmu, iomu}*/
static          ilu_boolean
ProcessExtraInput(ilu_Connection conn, ilu_Call dummy,
		  ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      server = connection_server(conn);
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_ReadHeaderResultCode rhrc;
  ilu_PacketType  packetType;
  ilu_cardinal    packetSN;
  if (!ilu_ExitMutex(server_lock(server), TRUE, err))
    return FALSE;
  if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
    return FALSE;
  /* Main Invariant; L2 >= {conn's callmu, iomu} */
  rhrc = protocol_read_header(proto, dummy,
			      &packetType, &packetSN, err);
  switch (rhrc) {
  case ilu_rhrc_ok:
    /*
     * We know no calls are in progess, so nobody cares about this
     * reply, so flush it.  When there exist other kinds of
     * messages, this code will have to become prepared to handle
     * them.
     */
    if (!protocol_discard_input(proto, dummy, err))
      goto closeout;
  case ilu_rhrc_nothing:
  case ilu_rhrc_handled:
    if (!ilu_ReEnterMutex(ilu_cmu, err))
      return FALSE;
    if (!ilu_ReEnterMutex(server_lock(server), err))
      return FALSE;
    return TRUE;
  case ilu_rhrc_eof:
  case ilu_rhrc_error:
closeout:
    if (ILU_ERROK(*err)) {
      DEBUG(CONNECTION_DEBUG,
	    (stderr, "Server closed connection %p to <%s>.\n",
	     conn, server->sr_id));
    } else {
      DEBUG(CONNECTION_DEBUG,
	    (stderr,
	     "Closing connection %p to <%s>, due to error %s"
	     " (raised at %s:%d).\n",
	     conn, server->sr_id, ILU_ERR_NAME(*err),
	     ilu_ErrorFile(err), ilu_ErrorLine(err)));
    }
    goto dunX;
  default:
    if (!ilu_Check(0, err))
      goto dunX;
    return TRUE;
  }
dunX:
  if (!ilu_ReEnterMutex(ilu_cmu, err))
    return FALSE;
  if (!ilu_ReEnterMutex(server_lock(server), err))
    return FALSE;
  _ilu_CloseIoingConnection(conn, ILU_ERRNOK(*err));
  return ILU_ERROK(*err);
}

/*Main Invariant holds*/
static void ReadExtraMsg(ilu_refany rock)
{
  ilu_Connection  conn = (ilu_Connection) rock;
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Server      server = connection_server(conn);
  ilu_boolean     prco = protocol_concurrent(proto);
  ilu_Call_s      dummyCall;
  ilu_Error       lerr;
  ilu_boolean     rereg = FALSE;
  if (!ilu_EnterMutex(ilu_cmu, &lerr))
    goto dun0;
  if (!ilu_EnterMutex(server_lock(server), &lerr))
    goto dun1;
  if (conn->co_mucall != NIL)
    /* Spurrious call; stop here. */
    goto dun2;
  if (conn->co_nOuts != 0)
    goto dun2;
  if (!ilu_Check(conn->co_reader == NIL, &lerr))
    goto dun2;
  BuildCall(&dummyCall, conn, server, 0, FALSE);
  if (!(*proto->pr_init_call) (&dummyCall, &lerr))
    goto dun2;
  if (!_ilu_EnterConnCall(conn, &dummyCall, TRUE, &lerr))
    goto dun2;
  if (!_ilu_EnterConnIO(conn, TRUE, &lerr))
    goto dun3;
  /* L1 = {cmu, server}; L2&conn = {callmu, iomu} */
  if (connection_closed(conn))
    goto dun4;
  if (!ilu_ClearConnectionInputHandler(conn, &lerr))
    goto dun4;
  rereg = ProcessExtraInput(conn, &dummyCall, &lerr);
dun4:
  if (ILU_ERROK(lerr) && proto->pr_prefinish_call != NULLFN) {
    (void) (*proto->pr_prefinish_call) (&dummyCall, &lerr);
    rereg = rereg && !connection_closed(conn);
  }
  (void) _ilu_ReleaseConnIO(conn, TRUE, &lerr);
dun3:
  (void) _ilu_ReleaseConnCall(conn, &dummyCall, TRUE, &lerr);
dun2:
  (void) ilu_ExitMutex(server_lock(server), TRUE, &lerr);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, &lerr);
  if (rereg)
    if (!ilu_SetConnectionInputHandler(conn, ReadExtraMsg, conn, &lerr))
      goto dun0;
dun0:
  ILU_HANDLED(lerr);
  (*proto->pr_finish_call) (&dummyCall, &lerr);
  ILU_HANDLED(lerr);
  return;
}

/**Main Invariant holds;
   after: success => Call-Invariant(result, err) && Call-Hi(result)*/

ilu_boolean
ilu_StartCall(ilu_Call_s * c, ilu_Server s, ilu_Class intro_type,
	      ilu_Method method, ilu_LanguageIndex lang,
	      ilu_Passport pp, ilu_Connection * new_conn,
	      ILU_ERRS((IoErrs, bad_locks,
			inv_objref, no_resources)) * err)
{
  ilu_Connection  conn = NIL;
  ilu_integer     dfd, cdfd;
  ilu_TransportInfo tinfo;
  ilu_string      pinfo;
  ilu_Protocol    proto;
  ilu_TransportCreator tcr;
  ilu_Transport   t;
  ilu_boolean     isnew = FALSE;
  ilu_Error       lerr = ILU_INIT_NO_ERR;

  ILU_CLER(*err);
  *new_conn = NIL;
  if (s == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);

  if ((server_is_true(s) &&
       (method_id(method) > 0xFF80))
  /* calling "internal" method on true server */
      || (server_true_language(s) == lang)
  /* calling through kernel for should-be direct call */
    )
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_true, FALSE);

  if (method->me_asynchronous &&
      !(server_is_true(s) || s->sr_tcr->tcr_reliable))
    return ILU_ERR_CONS1(bad_param, err, minor,
			 ilu_bpm_asynch_unreliable, FALSE);

  BuildCall(c, NIL, s, 0, FALSE);
  c->ca_intro_type = intro_type;
  c->ca_method = method;
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!ilu_EnterMutex(server_lock(s), err))
    goto dun1;
  while (1) {
    /* L1 = {cmu, s}; ILU_ERROK(*err) */
    for (conn = server_connections(s);
	 (conn != NIL) && (conn->co_mucall != NIL);
	 conn = connection_next(conn)) {
    }
    if (conn != NIL)
      break;
    /* conn==NIL, L1={cmu, s}, L2 as at entry */
    if (server_is_true(s)) {
      tinfo = port_tinfo(server_local_port(s));
      pinfo = port_pinfo(server_local_port(s));
      proto = port_protocol(server_local_port(s));
      tcr = port_transport_creator(server_local_port(s));
    } else {
      tinfo = s->sr_tinfo;
      tcr = s->sr_tcr;
      pinfo = s->sr_pinfo;
      proto = s->sr_protocol;
    }
    if (!ilu_ExitMutex(server_lock(s), TRUE, err))
      goto dun1;
    dfd = (*tcr->tcr_dfd) (tcr, FALSE);
    if ((ilu_fdbudget < ilu_fdstaken + dfd)
	&& (dfd > 0)) {
      lerr = _ilu_ReduceFdsTo(ilu_fdbudget - dfd);
      ILU_ERR_SWITCH(lerr) {
	ILU_SUCCESS_CASE;
	ILU_ERR_CASE(WrongLocks, v) {
	  ILU_ERR_CONS0(bad_locks, err, 0);
	  goto dun1;
	}
      } ILU_ERR_ENDSWITCH;
      if ((ilu_fdbudget < ilu_fdstaken + dfd)
	  && (dfd > 0)) {
	DEBUG(CALL_DEBUG,
	      (stderr, "StartCall: FD budget exhausted.\n"));
	ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_fds, 0);
	goto dun1;
      }
      if (!ilu_EnterMutex(server_lock(s), err))
	goto dun1;
    } else {
      ilu_DeltaFD(dfd);
      if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
	goto dun0;
      t = (*tcr->tcr_createTransport) (tcr, TRUE, &cdfd, pp, err);
      if (!ilu_ReEnterMutex(ilu_cmu, err))
	goto dun0;
      ilu_DeltaFD(cdfd - dfd);
      if (t == NIL) {
	if (ilu_EnterMutex(server_lock(s), err)) {
	  if (!server_is_true(s) && _ilu_CompareTinfo(tinfo, s->sr_tinfo))
	    s->sr_cfails = ilu_TRUE;
	  ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, 0);
	  (void) ilu_ExitMutex(server_lock(s), TRUE, err);
	} else
	  ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ts, 0);
	goto dun1;
      }
      if (!ilu_EnterMutex(server_lock(s), err))
	goto dun1;
      conn = _ilu_CreateConnection(t, tinfo, NIL, proto, pinfo, NIL, s,
				   pp, err);
      if (conn == NIL)
	goto dun2;
      if (ilu_CanCondition()) {
	*new_conn = conn;
      }
      isnew = TRUE;
      break;
    }
  }
  /* conn != NIL && conn's IO and call mutexes not held */
  if (!ilu_Check(!connection_closed(conn), err))
    goto dun2;
  if (!ilu_Check(!connection_incoming(conn), err))
    goto dun2;
  if (!_ilu_CanCondition() && conn->co_nOuts == 0 && !isnew) {
    if (!ilu_ClearConnectionInputHandler(conn, err))
      goto dun2;
  }
  if (!_ilu_EnterConnCall(conn, c, FALSE, err))
    goto dun2;
  if (!_ilu_EnterConnIO(conn, FALSE, err))
    goto dun3;
  if (!ilu_Check(!connection_closed(conn), err))
    goto dun4;
  c->ca_ms = ilu_cmsHi;
  proto = conn->co_protocol;
  /* conn != NIL && conn's IO and call mutexes held */
  if (conn->co_next_sn > 0xFFFFFF)	/* why not 0xFFFF ? */
    conn->co_next_sn = 1;
  c->ca_connection = conn;
  c->ca_SN = conn->co_next_sn++;
  if (!(*proto->pr_init_call) (c, err))
    goto dun2;
  goto dun2;
dun4:
  (void) _ilu_ReleaseConnIO(conn, TRUE, err);
dun3:
  (void) _ilu_ReleaseConnCall(conn, c, TRUE, err);
dun2:
  (void) ilu_ExitMutex(server_lock(s), TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
dun0:
  if (ILU_ERROK(*err)) {
    DEBUG(CALL_DEBUG,
	  (stderr, "%-20.20s(%p to \"%s\" #%lu, %s.%s) is %p\n",
	   "ilu_StartCall", call_connection(c), call_server_id(c),
	   call_serial_number(c),
	   class_name(intro_type), method_name(method), c));
    c->ca_caller = pp;
    return TRUE;
  } else {
    DEBUG(CALL_DEBUG,
	  (stderr,
	   "%-20.20s(to %s, %s.%s) raises err %s (from %s ln %d)\n",
	   "ilu_StartCall", server_id(s),
	   class_name(intro_type), method_name(method),
	   ILU_ERR_NAME(*err),
	   ilu_ErrorFile(err), ilu_ErrorLine(err)));
    ILU_HANDLED(lerr);
    return FALSE;
  }
}

/**Before: Call-Invariant(call, err).
   After:  Main Invariant holds, and
	   L2 disjoint {call's conn's callmu, iomu},
	   if possible (*err indicates bad_locks or broken_locks
	   when not possible).*/
void
ilu_FinishCall(ilu_Call call, ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      s;
  ilu_Connection  conn;
  ilu_Protocol    proto;
  ILU_ERRS((bad_locks, broken_locks)) lerr = ILU_INIT_NO_ERR;
  ilu_boolean     closeconn = FALSE;

#ifdef ENABLE_DEBUGGING
  if (call->ca_incoming) {
    DEBUG(CALL_DEBUG,
	  (stderr, "%-20.20s(%s #%lu) => err=%s ca_pe=%s\n",
	   "ilu_FinishCall", call_connection_id(call),
	   (unsigned long) call_serial_number(call),
	   ILU_ERR_NAME(*err),
	   ((call->ca_pe <= ilu_ProtocolException_Not)
	    ? ilu_PENames[call->ca_pe] : "*")));
  } else {
    DEBUG(CALL_DEBUG,
	  (stderr,
	   "%-20.20s(%p to \"%s\" #%lu) => err=%s ca_pe=%s\n",
	   "ilu_FinishCall", call_connection(call),
	   call_server_id(call),
	   (unsigned long) call_serial_number(call),
	   ILU_ERR_NAME(*err),
	   ((call->ca_pe <= ilu_ProtocolException_Not)
	    ? ilu_PENames[call->ca_pe] : "*")));
  }
#endif				/* ENABLE_DEBUGGING */
  ILU_ERR_SWITCH(*err) {
    ILU_SUCCESS_CASE
      0 /* statement with no effect */ ;
    ILU_ERR_CASE(comm_failure, v)
      closeconn = TRUE;
    ILU_ERR_CASE2(bad_locks, broken_locks)
    /*
     * Here we leak like a sieve, but we can't free everything
     * reliably, and so we leave everything lying around, in hopes
     * that it's useful for debugging.
     */
      return;
    ILU_ERR_ELSE
      0 /* statement with no effect */ ;
  } ILU_ERR_ENDSWITCH;
  conn = call_connection(call);
  s = connection_server(conn);
  proto = call_proto(call);
  _ilu_Assert((call_incoming(call) ||
	       call->ca_pe == ilu_ProtocolException_Success),
	      "client PE");
  if (call->ca_ms == ilu_cmsNo && !closeconn
      && proto->pr_prefinish_call == NULLFN
      && ((call->ca_pe == ilu_ProtocolException_Success
	   && ILU_ERROK(*err))
	  || !call_incoming(call)))
    goto dun0;
  if (!ilu_EnterMutex(ilu_cmu, &lerr))
    goto dun0;
  if (!ilu_EnterMutex(server_lock(s), &lerr))
    goto dun1;
  if (call->ca_ms != ilu_cmsHi) {
    ilu_boolean     conc = connection_concurrent(conn);
    if (!ilu_Check(call->ca_ios == ilu_ciosNone, err))
      goto dun2;
    if (call->ca_ms == ilu_cmsNo || conc) {
      if (!_ilu_EnterConnCall(conn, call, FALSE, &lerr))
	goto dun2;
      if (conc && call->ca_ms == ilu_cmsLo)
	conn->co_nOuts--;
    }
    if (!_ilu_EnterConnIO(conn, FALSE, &lerr))
      goto dun3;
    if (connection_closed(conn))
      closeconn = TRUE;
  }
  /* L2 >= {call's conn's callmu, iomu} */
  if (closeconn)
    _ilu_CloseIoingConnection(conn, TRUE);
  else {
    switch (call->ca_ios) {
    case ilu_ciosIn:
      if (!proto->pr_discard_input(call, &lerr))
	goto dunX;
      break;
    case ilu_ciosOut:
      if (!proto->pr_discard_output(call, &lerr))
	goto dunX;
    case ilu_ciosNone:
      break;
    default:
      if (!ilu_Check(FALSE, &lerr))
	goto dunX;
    }
    if (call_incoming(call) &&
	(call->ca_pe != ilu_ProtocolException_Success ||
	 ILU_ERRNOK(*err))) {
      ilu_cardinal    exsize;
      if (call->ca_pe == ilu_ProtocolException_Success)
	call->ca_pe = ilu_ProtocolException_Unknown;
      ILU_CLER(*err);
      exsize = ilu_BeginSizingException(call, - (ilu_integer)call->ca_pe, err);
      if (ILU_ERRNOK(*err))
	goto dunX;
      DEBUG(CALL_DEBUG,
	    (stderr,
	     "%-20.20s(%s #%lu, excn ilu_ProtocolException_%s)\n",
	     "pr_begin_exception", call_connection_id(call),
	     (unsigned long) call_serial_number(call),
	     ((call->ca_pe <= ilu_ProtocolException_Not)
	      ? ilu_PENames[call->ca_pe] : "*")));
      if (!protocol_begin_exception(proto, call, 0, call->ca_pe,
				    exsize, err))
	goto dunX;
      DEBUG(CALL_DEBUG,
	    (stderr, "%-20.20s(%s #%lu)\n", "pr_finish_exception",
	     call_connection_id(call),
	     (unsigned long) call_serial_number(call)));
      if (!protocol_finish_exception(proto, call, err))
	goto dunX;
  dunX:
      ILU_ERR_SWITCH(*err) {
	ILU_SUCCESS_CASE
	  0 /* no effect */ ;
	ILU_ERR_CASE(comm_failure, v)
	  _ilu_CloseIoingConnection(conn, TRUE);
	ILU_ERR_ELSE
	  0 /* no effect */ ;
      } ILU_ERR_ENDSWITCH;
    }
  }
dun4:
  if (proto != NIL && proto->pr_prefinish_call != NULLFN)
    (void) (*proto->pr_prefinish_call) (call, &lerr);
  if (!_ilu_ReleaseConnIO(conn, FALSE, &lerr))
    goto dun2;
dun3:
  (void) _ilu_ReleaseConnCall(conn, call, FALSE, &lerr);
dun2:
  (void) ilu_ExitMutex(server_lock(s), TRUE, &lerr);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, &lerr);
dun0:
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  if (proto != NIL && proto->pr_finish_call != NULLFN)
    (void) (*proto->pr_finish_call) (call, &lerr);
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  if (!connection_closed(conn) && !connection_incoming(conn)
      && !_ilu_CanCondition() && conn->co_nOuts == 0) {
    (void) ilu_SetConnectionInputHandler(conn, ReadExtraMsg, conn, &lerr);
    if (ILU_ERROK(*err))
      *err = lerr;
    else
      ILU_HANDLED(lerr);
  }
  if (call->ca_incoming && (call->ca_caller != NIL)) {
    ilu_DestroyPassport (call->ca_caller, err);
    call->ca_caller = NIL;
  };    
  return;
}

/*Main Invariant holds*/

/*L1, L2 unconstrained*/
static ilu_RcvReqStat Unbuild(ilu_Call call, ilu_RcvReqStat ans)
{
  return ans;
}

/*Main Invariant holds*/
/*before: L2 disjoint {conn's callmu, iomu},
 *after:  Call-Invariant(*call, err)        if *call != NIL,
 *after:  L2 disjoint {conn's callmu, iomu} if *call == NIL*/

ilu_RcvReqStat
ilu_ReceiveRequest(ilu_Call_s * call, ilu_boolean * initted,
		   ilu_Connection conn, ilu_Class * pit,
		   ilu_Method * meth, ilu_cardinal * sn,
		   ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      server = connection_server(conn);
  ilu_PacketType  pkt_type;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_Protocol    proto = conn->co_protocol;
  ilu_ReadHeaderResultCode rhrc = ilu_rhrc_error;
  ilu_boolean     stdMeth = FALSE;

  BuildCall(call, conn, server, 0, TRUE);
  *initted = FALSE;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return Unbuild(call, ilu_RcvReqStat_noop);
  if (!ilu_EnterMutex(server_lock(server), err)) {
    (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_noop);
  }
  if (connection_closed(conn)) {
    (void) ilu_ExitMutex(server_lock(server), TRUE, err);
    (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_quit);
  }
  if (!_ilu_EnterConnCall(conn, call, FALSE, err))
    goto lab0;
  if (!_ilu_EnterConnIO(conn, FALSE, err))
    goto lab1;
  if (connection_closed(conn)) {
    (void) _ilu_ReleaseConnIO(conn, TRUE, err);
    (void) _ilu_ReleaseConnCall(conn, call, TRUE, err);
    (void) ilu_ExitMutex(server_lock(server), TRUE, err);
    (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
    return Unbuild(call, ilu_RcvReqStat_quit);
  }
  call->ca_ms = ilu_cmsHi;
  if ((*proto->pr_init_call) (call, err)) {
    /* We're now obliged to eventually call pr_finish_call */
    *initted = TRUE;
    goto lab0;
  }
  (void) _ilu_ReleaseConnIO(conn, TRUE, err);
lab1:
  (void) _ilu_ReleaseConnCall(conn, call, TRUE, err);
  call->ca_ms = ilu_cmsNo;
lab0:
  (void) ilu_ExitMutex(server_lock(server), TRUE, err);
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
  if (ILU_ERRNOK(*err))
    return ilu_RcvReqStat_noop;
  rhrc = protocol_read_header(proto, call, &pkt_type, sn, &lerr);
  switch (rhrc) {
  case ilu_rhrc_ok:
    call->ca_ios = ilu_ciosIn;
    break;
  case ilu_rhrc_eof:
    (void) _ilu_CloseConnWithIo(conn, FALSE, err);
    return ilu_RcvReqStat_quit;
  case ilu_rhrc_nothing:
  case ilu_rhrc_handled:
    return ilu_RcvReqStat_noop;
  case ilu_rhrc_error:
    ILU_ERR_SWITCH(lerr) {
      ILU_ERR_CASE(comm_failure, v) {
	*err = lerr;
	return ilu_RcvReqStat_quit;
      }
      ILU_ERR_CASE(bad_param, v) {
	ILU_HANDLED(lerr);
	ILU_ERR_CONS1(internal, err, minor, ilu_im_unhandled, 0);
	return ilu_RcvReqStat_noop;
      }
      ILU_ERR_CASE3(no_memory, internal, broken_locks) {
	*err = lerr;
	return ilu_RcvReqStat_noop;
      }
      ILU_ERR_CASE2(imp_limit, marshal) {
	*err = lerr;
	return ilu_RcvReqStat_noop;
      }
    } ILU_ERR_ENDSWITCH;
  default:
    if (!ilu_Check(FALSE, err))
      return ilu_RcvReqStat_noop;
  }
  if (pkt_type != ilu_PacketType_Request)
    return ilu_RcvReqStat_noop;
  call_serial_number(call) = *sn;
  if (_ilu_AddConnIdentities (call, err), ILU_ERRNOK(*err))
    {
      call->ca_caller = NIL;
      return ilu_RcvReqStat_noop;
    }
  if (protocol_interpret_request(proto, call, err)) {
    ilu_Method      m = call_method(call);
    stdMeth = m != NIL && method_id(m) >= 0xFF00 && method_id(m) < 0xFFFF;
    DEBUG((CALL_DEBUG | INCOMING_DEBUG),
	  (stderr, "%-20.20s(%s #%lu, %s.%s)\n", "ilu_ReceiveRequest",
	   call_connection_id(call),
	   (unsigned long) call_serial_number(call),
	   class_name(call_intro_type(call)),
	   method_name(call_method(call))));
  } else {
    DEBUG(INCOMING_DEBUG,
	  (stderr, "ilu_ReceiveRequest (%s #%lu) error:  *err=%s, ca_pe=%s.\n",
	   call_connection_id(call),
	   (long unsigned) call_serial_number(call),
	   ILU_ERR_NAME(*err), ilu_PEName(call->ca_pe)));
    (void) ilu_Check((ILU_ERRNOK(*err) ||
		      call->ca_pe != ilu_ProtocolException_Success),
		     err);
    return ilu_RcvReqStat_noop;
  }
  if (!ilu_ReEnterMutex(server_lock(server), err))
    return ilu_RcvReqStat_noop;
  if (conn->co_port->po_call_cache != NIL) {
    ilu_CachedCall *cc = conn->co_port->po_call_cache;
    int             i;
    for (i = 0; i < conn->co_port->po_call_cache_size; i++) {
      if ((cc[i].cc_sn == *sn)
	  && (cc[i].cc_intro_type == call->ca_intro_type)
	  && (cc[i].cc_meth == call_method(call))
	  && (cc[i].cc_replyMsg.msg_base != NIL)
	  && strcmp(cc[i].cc_peerinfo, conn_peerinfo(conn)) == 0) {
	if (!ilu_ExitMutex(server_lock(server), TRUE, err))
	  return ilu_RcvReqStat_noop;
	DEBUG(CALL_DEBUG,
	      (stderr,
	       "ilu_ReceiveRequest: resending cached reply"
	       " to call %ld from %s.\n",
	       cc[i].cc_sn, cc[i].cc_peerinfo));
	call->ca_ios = ilu_ciosNone;
	if (!protocol_discard_input(proto, call, err)) {
	  return ilu_RcvReqStat_noop;
	}
	if (!transport_send_whole_message(conn->co_transport,
					  &cc[i].cc_replyMsg,
					  err))
	  return ilu_RcvReqStat_noop;
	return ilu_RcvReqStat_noop;
      }
    }
  }
  if (!ilu_ExitMutex(server_lock(server), TRUE, err))
    return ilu_RcvReqStat_noop;
  if (stdMeth) {
    (call_method(call)->me_stubproc) (call);
    return ilu_RcvReqStat_noop;
  }
  *pit = call->ca_intro_type;
  *meth = call_method(call);
  call->ca_pe = ilu_ProtocolException_GarbageArguments;
  return ilu_RcvReqStat_request;
}

#ifdef WIN32
/* AVOID COMPILER BUG by turning off global optimization for this function
  F:\ilu\src\runtime\kernel\call.c(457) : fatal error C1001: INTERNAL COMPILER ERROR
  (compiler file 'l:\b_bld\c2\P2\main.c', line 374) */
#pragma optimize("g", off)
#endif

/*L1, L2, Main unconstrained*/
static ilu_FineTime ClipAddTime(ilu_FineTime a, ilu_FineTime b,
				ilu_FineTime l)
{
  ilu_FineTime c = ilu_FineTime_Add(a, b);
  if (ilu_FineTime_Cmp(c, l) > 0)
       return l;
  else return c;
}

#ifdef WIN32
/* restore optimizations to original settings */
#pragma optimize("", on)
#endif

/*mxamu = ilu_cmu*/
typedef struct {
  /*L1 >= {ilu_cmu}; L2 unconstrained*/
  
  ilu_Alarmette_s	gra_alarmette;
  ilu_private		gra_cc;
} GetReplyAlarm;

/*L1_sup = ilu_cmu; L2 unconstrained*/

static void GRInvoke(ilu_Alarmette a);
static void GRSet(ilu_FineTime t);
static void GRCancel(void);

static ilu_Alarmette_s grHead = {&grHead, &grHead, FALSE, {0, 0}};
static ilu_AlarmRep grar = {&grHead, GRInvoke, GRSet, GRCancel};

/*Main Invariant holds*/
static void GraInvoke(ilu_private rock)
{
  ilu_FineTime now = ilu_FineTime_Now();
  _ilu_AcquireMutex(ilu_cmu);
  ilu_MXAProc(now, &grar);
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

static void GRInvoke(ilu_Alarmette a)
{
  ilu_Error err;
  GetReplyAlarm *gra = (GetReplyAlarm*) a;
  err = _ilu_NotifyCondition(gra->gra_cc);
  ILU_MUST_BE_SUCCESS(err);
  return;
}

static void GRSet(ilu_FineTime t)
{
  ilu_SetAlarm(_ilu_grAlarm, t, GraInvoke, NIL);
  return;
}

static void GRCancel(void)
{
  ilu_UnsetAlarm(_ilu_grAlarm);
  return;
}

/*L1, L2 unconstrained*/
static void FreeMessage(ilu_Message * msg)
{
  if (msg->msg_base != NIL) {
    ilu_free(msg->msg_base);
    msg->msg_base = NIL;
  }
  return;
}

/**Before: Main Invariant, Call-Hi(call);
    After: Call-Invariant(call, err),
	   success => Call-Hi(call)*/
ilu_ProtocolException
ilu_GetReply(ilu_Call call, ilu_cardinal * estatus,
	     ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Server      server = connection_server(conn);
  ilu_Transport   trans = connection_transport(conn);
  ilu_cardinal    pktsn;
  ilu_PacketType  pkttype;
  ilu_ProtocolException ans;
  ilu_boolean     irqFromWait;
  ilu_ReplyList   r, *pr;
  ilu_boolean     prco = protocol_concurrent(proto);
  ilu_boolean     conc = prco && _ilu_CanCondition();
  ilu_FineTime    now;
  ilu_FineTime    timeout;
  ilu_FineTime    limit, *elimit;
  ilu_FineTime    grandLimit;
  GetReplyAlarm   gra = {{NIL, NIL, FALSE, {0, 0}}, NIL};
  ILU_ERRS((interrupted)) lerr;

  if (method_asynchronous(call_method(call)))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh,
			 ilu_ProtocolException_Not);

  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%p to \"%s\" #%lu)...\n",
		     "ilu_GetReply", call_connection(call),
		     call_server_id(call),
		     (unsigned long) call_serial_number(call)));

  timeout = trans->tr_to1;
  if (!transport_reliable(trans)) {
    now = ilu_FineTime_Now();
    limit = ilu_FineTime_Add(now, timeout);
    grandLimit = ilu_FineTime_Add(now, trans->tr_tto);
    elimit = &limit;
  } else
    elimit = NIL;
  if (conn->co_mucall != call)
    return ILU_ERR_CONS0(bad_locks, err, ilu_ProtocolException_Not);
  while (1) {
    /* Main Invariant, Call-Hi(call) */
    if (!ilu_EnterMutex(ilu_cmu, err))
      return ilu_ProtocolException_Not;
    if (!ilu_EnterMutex(server_lock(server), err)) {
      (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
      return ilu_ProtocolException_Not;
    }
    if (conc && connection_reader(conn) == NIL)
      connection_reader(conn) = call;
    if (conc && connection_reader(conn) != call
	&& !trans->tr_class->tc_reliable) {
      gra.gra_cc = connection_cc(conn);
      ilu_MXASet(&grar, &gra.gra_alarmette, limit);
    }
    if (!_ilu_ReleaseConnIO(conn, FALSE, err)) {
      (void) ilu_ExitMutex(server_lock(server), TRUE, err);
      (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
      return ilu_ProtocolException_Not;
    }
    if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
      return ilu_ProtocolException_Not;
    if (prco) {			/* release the call mutex */
      conn->co_nOuts++;
      conn->co_mucall = NIL;
    }
    call->ca_ms = ilu_cmsLo;
    /* L1 = {server}; Call-Lo(call) */
    if (conc && connection_reader(conn) != call) {
      if (!ilu_CMWait1(connection_cc(conn), server_lock(server), err))
	return ilu_ProtocolException_Not;
      if (!trans->tr_class->tc_reliable) {
	if (!ilu_ExitMutex(server_lock(server), TRUE, err))
	  return ilu_ProtocolException_Not;
	if (!ilu_ReEnterMutex(ilu_cmu, err))
	  return ilu_ProtocolException_Not;
	if (!ilu_ReEnterMutex(server_lock(server), err))
	  return ilu_ProtocolException_Not;
	ilu_MXAClear(&grar, &gra.gra_alarmette);
	if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
	  return ilu_ProtocolException_Not;
      }
      ILU_CLER(lerr);
    } else {
      if (!ilu_ExitMutex(server_lock(server), TRUE, err))
	return ilu_ProtocolException_Not;
      irqFromWait = !_ilu_BlockingWaitForInputOnConnection(conn, elimit,
							   &lerr);
      if (!ilu_ReEnterMutex(server_lock(server), err))
	return ilu_ProtocolException_Not;
    }
    /* L1 = {server}; Call-Lo(call) */
    if (conc && connection_reader(conn) == call) {
      connection_reader(conn) = NIL;
      if (!ilu_CondNotify(connection_cc(conn), err))
	return ilu_ProtocolException_Not;
    }
    if (call->ca_irq || ILU_ERRNOK(lerr)) {
      if (!ilu_ExitMutex(server_lock(server), TRUE, err))
	return ilu_ProtocolException_Not;
      if (ILU_ERRNOK(lerr))
	*err = lerr;
      else
	ILU_ERR_CONS1(interrupted, err, ilu_interruptSet, 0, (void) 6);
      return ilu_ProtocolException_Not;
    }
    if (prco) {			/* re-acquire the call mutex */
      _ilu_Assert(conn->co_nOuts > 0, "GetReply: conn->co_nOuts==0 a");
      if (!_ilu_AltEnterConnCall(conn, call, TRUE, err))
	return ilu_ProtocolException_Not;
      _ilu_Assert(conn->co_nOuts > 0, "GetReply: conn->co_nOuts==0 b");
      conn->co_nOuts--;
    }
    if (connection_closed(conn)) {
      if (!ilu_ExitMutex(server_lock(server), TRUE, err))
	return ilu_ProtocolException_Not;
      return ilu_ProtocolException_LostConnection;
    }
    if (!_ilu_TakeConnIO(conn, TRUE, err))	/* re-acquire the I/O
						 * mutex */
      return ilu_ProtocolException_Not;
    call->ca_ms = ilu_cmsHi;
    if (!ilu_ExitMutex(server_lock(server), TRUE, err))
      return ilu_ProtocolException_Not;
    /* L1 = {}; Call-Hi(call) */
    if (prco) {
      for (pr = &conn->co_replies, r = *pr;
	   r != NIL;
	   pr = &r->rp_next, r = *pr)
	if (r->rp_SN == call_serial_number(call)) {
	  FreeMessage(&call->ca_msg);
	  *pr = r->rp_next;
	  protocol_resume_interp(proto, call, r->rp_queued);
	  call->ca_ios = ilu_ciosIn;
	  ilu_free(r);
	  ans = protocol_interpret_reply(proto, call, estatus, err);
	  DEBUG((CALL_DEBUG | INCOMING_DEBUG),
		(stderr, "%-20.20s(%p to \"%s\" #%lu) => ans=%s estatus=%lu err=%s\n",
		 "ilu_GetReply", call_connection(call),
		 call_server_id(call),
		 (unsigned long) call_serial_number(call),
		 ILU_ERROK(*err) ? ilu_PEName(ans) : "*",
		 (unsigned long) *estatus, ILU_ERR_NAME(*err)));
	  return ans;
	}
    }
    while (1) {
      /* Read all available replies */
      /* Main holds; Call-Hi(call) */
      ilu_boolean     closeit = FALSE;
      ilu_ReadHeaderResultCode rhrc;
      rhrc = protocol_read_header(proto, call, &pkttype, &pktsn, err);
      switch (rhrc) {
      case ilu_rhrc_ok:
	call->ca_ios = ilu_ciosIn;
	break;
      case ilu_rhrc_nothing:
	goto repliesRead;
      case ilu_rhrc_handled:
	break;
      case ilu_rhrc_eof:
	DEBUG(CONNECTION_DEBUG,
	      (stderr, "Closing connection %p to %s due to EOF.\n",
	       conn, server->sr_id));
	closeit = TRUE;
      case ilu_rhrc_error:
	ILU_ERR_SWITCH(*err) {
	  ILU_ERR_CASE(bad_param, v) {
	    ILU_HANDLED(*err);
	    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_unhandled,
				 0);
	  }
	  ILU_ERR_CASE(comm_failure, v) {
	    DEBUG(CONNECTION_DEBUG,
		  (stderr,
		   "%s %p to %s due to comm_failure, minor=%d.\n",
		   "Closing connection", conn,
		   server->sr_id, v->minor));
	    closeit = TRUE;
	  }
	  ILU_ERR_ELSE
	    0 /* statement with no effect */;
	} ILU_ERR_ENDSWITCH;
	if (!closeit)
	  return ilu_ProtocolException_Not;
	else
	  break;
      default:
	_ilu_Assert(FALSE, "GetReply vs read_header");
      }
      if (closeit) {
	if (!ilu_ReEnterMutex(ilu_cmu, err))
	  return ilu_ProtocolException_Not;
	if (!ilu_ReEnterMutex(server_lock(server), err))
	  return ilu_ProtocolException_Not;
	_ilu_CloseIoingConnection(conn, ilu_TRUE);
	if (!ilu_ExitMutex(server_lock(server), TRUE, err))
	  return ilu_ProtocolException_Not;
	if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
	  return ilu_ProtocolException_Not;
	return ilu_ProtocolException_LostConnection;
      }
      if (pkttype == ilu_PacketType_Reply) {
	if (pktsn == call_serial_number(call)) {
	  FreeMessage(&call->ca_msg);
	  ans = protocol_interpret_reply(proto, call, estatus, err);
	  DEBUG((CALL_DEBUG | INCOMING_DEBUG),
		(stderr, "%-20.20s(%p to \"%s\" #%lu) => ans=%s estatus=%lu err=%s\n",
		 "ilu_GetReply", call_connection(call),
		 call_server_id(call),
		 (unsigned long) call_serial_number(call),
		 ilu_PEName(ans), (unsigned long) *estatus,
		 ILU_ERR_NAME(*err)));
	  return ans;
	}
	if (prco) {
	  ilu_refany      queued2;
	  call->ca_ios = ilu_ciosNone;
	  queued2 = protocol_delay_interp(proto, call, err);
	  if (ILU_ERRNOK(*err))
	    return ilu_ProtocolException_Not;
	  r = (ilu_ReplyList) ilu_MallocE(sizeof(*r), err);
	  if (ILU_ERRNOK(*err))
	    return ilu_ProtocolException_Not;
	  r->rp_queued = queued2;
	  r->rp_SN = pktsn;
	  r->rp_next = conn->co_replies;
	  conn->co_replies = r;
	} else {
	  /* Reply to different call; discard. */
	  call->ca_ios = ilu_ciosNone;
	  if (!protocol_discard_input(proto, call, err))
	    return ilu_ProtocolException_Not;
	}
      } else {
	/* not a reply */
	call->ca_ios = ilu_ciosNone;
	if (!protocol_discard_input(proto, call, err))
	  return ilu_ProtocolException_Not;
      }
    }
repliesRead:
    ILU_MUST_BE_SUCCESS(*err);
    if (!trans->tr_class->tc_reliable) {
      now = ilu_FineTime_Now();
      if (ilu_FineTime_Cmp(now, grandLimit) >= 0)
	break;
      if (ilu_FineTime_Cmp(now, limit) >= 0) {
	DEBUG(CONNECTION_DEBUG,
	      (stderr,
	   "GetReply: retransmitting request on conn %p\n", conn));
	if (!transport_send_whole_message(trans, &call->ca_msg, err))
	  return ilu_ProtocolException_Not;
	timeout = ClipAddTime(timeout, timeout, trans->tr_toN);
	limit = ClipAddTime(now, timeout, grandLimit);
      }
    }
  }
  return ilu_ProtocolException_RequestTimeout;
}


/*Main Invariant holds; L2 disjoint {conn's callmu, iomu}*/
ilu_boolean
ilu_OutgoingConnectionThreadProc(ilu_Connection conn,
				 ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Server      server = connection_server(conn);
  ilu_Call_s      dummyCall;
  ILU_ERRS((interrupted)) lerr;
  ILU_ERRS((IoErrs)) ferr;
  ilu_boolean     ok;
  if (!ilu_CanCondition())
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  BuildCall(&dummyCall, conn, server, 0, FALSE);
  if (!(*proto->pr_init_call) (&dummyCall, err))
    return FALSE;
  while (1) {
    /* Main Invariant; L2 disjoint {conn's callmu, iomu} */
    while (1) {			/* wait for input outside of a call */
      ok = _ilu_BlockingWaitForInputOnConnection(conn, NIL, &lerr);
      if (!ilu_Check(ILU_ERROK(lerr), err))
	/* LSR has no reason to abort this thread */
	goto dun0;
      if (!ilu_EnterMutex(ilu_cmu, err))
	goto dun0;
      if (!ilu_EnterMutex(server_lock(server), err))
	goto dun1;
      if (!_ilu_EnterConnCall(conn, &dummyCall, TRUE, err))
	goto dun2;
      if (!_ilu_EnterConnIO(conn, TRUE, err))
	goto dun3;
      if (connection_closed(conn))
	goto dun4;
      if (conn->co_nOuts == 0) {
	if (!ilu_Check(conn->co_reader == NIL, err))
	  goto dun4;
	goto doit;
      }
      if (!_ilu_ReleaseConnIO(conn, TRUE, err))
	goto dun3;
      if (!_ilu_ReleaseConnCall(conn, &dummyCall, TRUE, err))
	goto dun2;
      if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
	goto dun1x;
      while (conn->co_nOuts > 0) {
	if (!ilu_CMWait1(connection_cc(conn), server_lock(server), err))
	  goto dun0;
      }
      /* The input we got earlier has been read; wait for more. */
      if (!ilu_ExitMutex(server_lock(server), TRUE, err))
	goto dun0;
    }
doit:
    /* L1 = {cmu, server}; L2&conn = {conn's callmu, iomu} */
    if (!ProcessExtraInput(conn, &dummyCall, err))
      goto dun4;
    if (!_ilu_ReleaseConnIO(conn, TRUE, err))
      goto dun3;
    if (!_ilu_ReleaseConnCall(conn, &dummyCall, TRUE, err))
      goto dun2;
    if (!ilu_ExitMutex(server_lock(server), TRUE, err))
      goto dun1;
    if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
      goto dun0;
  }
dun1x:
  (void) ilu_ExitMutex(server_lock(server), TRUE, err);
  goto dun0;
dun4:
  if (ILU_ERROK(*err) && proto->pr_prefinish_call != NULLFN)
    (void) (*proto->pr_prefinish_call) (&dummyCall, err);
  (void) _ilu_ReleaseConnIO(conn, TRUE, err);
dun3:
  (void) _ilu_ReleaseConnCall(conn, &dummyCall, TRUE, err);
dun2:
  (void) ilu_ExitMutex(server_lock(server), TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
dun0:
  (*proto->pr_finish_call) (&dummyCall, &ferr);
  if (ILU_ERROK(*err))
    *err = ferr;
  else
    ILU_HANDLED(ferr);
  return ILU_ERROK(*err);
}

/* Main holds, L2 no further constrained */
ilu_boolean
ilu_InterruptCall(ilu_Call call,
		  ILU_ERRS((bad_param)) * err)
{
  ilu_Connection  conn = call ? call_connection(call) : NIL;
  ilu_Transport   t;
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  t = conn->co_transport;
  call->ca_irq = TRUE;
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%p to \"%s\" #%lu)\n", "ilu_InterruptCall",
		     call_connection(call), call_server_id(call),
		     (unsigned long) call_serial_number(call)));
  return transport_interrupt(t, err);
}

/**Before: Main Invariant, Call-Hi(call);
    After: Call-Invariant(call, err),
	   success => Call-Hi(call)*/

ilu_boolean
ilu_StartRequest(ilu_Call call, ilu_cardinal argSize,
		 ILU_ERRS((bad_param, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_boolean ans;

  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  ans = protocol_start_request(call_proto(call), call, argSize, err);
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%p to \"%s\" #%lu, argSize %lu) => %s\n",
		     "ilu_StartRequest", call_connection(call),
		     call_server_id(call),
		     (unsigned long) call_serial_number(call),
		     argSize, ILU_ERR_NAME(*err)));
  if (!ans)
    return FALSE;
  call->ca_ios = ilu_ciosOut;
  return TRUE;
}

ilu_boolean
ilu_FinishRequest(ilu_Call call,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  call->ca_ios = ilu_ciosNone;
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%p to \"%s\" #%lu)\n",
		     "ilu_FinishRequest", call_connection(call),
		     call_server_id(call),
		     (unsigned long) call_serial_number(call)));
  return protocol_finish_request(call_proto(call), call,
				 &call->ca_msg, err);
}

/**Before: Main Invariant, Call-Hi(call);
    After: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsLo*/
ilu_boolean
ilu_RequestRead(ilu_Call call,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_Server      s = connection_server(conn);
  ilu_Protocol    p = call_proto(call);
  call->ca_ios = ilu_ciosNone;
  call->ca_pe = ilu_ProtocolException_Success;
  protocol_request_read(p, call, err);
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%s #%lu) => %s\n",
		     "ilu_RequestRead", call_connection_id(call),
		     (unsigned long) call_serial_number(call),
		     ILU_ERR_NAME(*err)));
  if (ILU_ERRNOK(*err))
    return ilu_FALSE;
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!ilu_EnterMutex(server_lock(s), err))
    goto dun1;
  if (!_ilu_ReleaseConnIO(conn, FALSE, err))
    goto dun2;
  if (connection_concurrent(conn)) {
    conn->co_nOuts++;
    (void) _ilu_ReleaseConnCall(conn, call, FALSE, err);
  }
  call->ca_ms = ilu_cmsLo;
dun2:
  (void) ilu_ExitMutex(server_lock(s), TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

/**Before: Main Invariant, Call-Hi(call);
    After: Call-Invariant(call, err),
	   success => Call-Hi(call)*/

ilu_boolean
ilu_ReplyRead(ilu_Call call,
	      ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    p = call_proto(call);
  if (method_asynchronous(call_method(call)))
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  call->ca_ios = ilu_ciosNone;
  protocol_reply_read(p, call, err);
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%p to \"%s\" #%lu) => %s\n",
		     "ilu_ReplyRead", call_connection(call),
		     call_server_id(call),
		     (unsigned long) call_serial_number(call),
		     ILU_ERR_NAME(*err)));
  return ILU_ERROK(*err);
}

/*Main Invariant holds, L2 otherwise unconstrained*/

ilu_cardinal
ilu_BeginSizingReply(ilu_Call call,
		     ilu_boolean exns_possible,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    proto = call_proto(call);
  if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  return ((*proto->pr_begin_sizing_reply) (call, exns_possible, err));
}

ilu_cardinal
ilu_BeginSizingException(ilu_Call call,
			 ilu_integer eindex,
			 ILU_ERRS((IoErrs)) * err)
{
  ilu_Protocol    proto = call_proto(call);

  _ilu_Assert(eindex!=0,"BeginSizingException called with zero exceptionVal");
  if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  return ((*proto->pr_begin_sizing_exn)(call, (eindex > 0) ? eindex : 0, (eindex > 0) ? ilu_ProtocolException_Success : (ilu_ProtocolException) -eindex, err));
}

/**before: Main Invariant && Call-Lo(call);
    after: Call-Invariant(call, err),
	   success => call->ca_ms == ilu_cmsHi. */

ilu_boolean
ilu_BeginReply(ilu_Call call, ilu_boolean exceptions_p,
	       ilu_cardinal argSize,
	       ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      s = call_server(call);
  ilu_Connection  conn = call_connection(call);
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!ilu_EnterMutex(server_lock(s), err))
    goto dun1;
  if (connection_concurrent(conn)) {
    if (!_ilu_EnterConnCall(conn, call, FALSE, err))
      goto dun2;
    conn->co_nOuts--;
  }
  if (!_ilu_EnterConnIO(conn, FALSE, err))
    goto dun3;
  if (connection_closed(conn)) {
    (void) ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, 6);
    goto dun4;
  }
  call->ca_ms = ilu_cmsHi;
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%s #%lu, argSize %lu)\n",
		     "ilu_BeginReply", call_connection_id(call),
		     (unsigned long) call_serial_number(call),
		     (unsigned long) argSize));
  if (protocol_begin_reply(call_proto(call), call, exceptions_p,
			   argSize, err))
    call->ca_ios = ilu_ciosOut;
  goto dun2;
dun4:
  (void)  _ilu_ReleaseConnIO(conn, TRUE, err);
dun3:
  (void) _ilu_ReleaseConnCall(conn, call, TRUE, err);
dun2:
  (void) ilu_ExitMutex(server_lock(s), TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

ilu_boolean
ilu_BeginException(ilu_Call call, ilu_integer exceptionVal,
		   ilu_cardinal argSize,
		   ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Server      s = call_server(call);
  ilu_Connection  conn = call_connection(call);
  ilu_cardinal	exceptionCode = (exceptionVal <= 0) ? 0 : exceptionVal;
  ilu_ProtocolException sys_excn = (exceptionVal > 0) ?
    ilu_ProtocolException_Success :
      (ilu_ProtocolException) -exceptionVal;

  _ilu_Assert(exceptionVal!=0,"BeginException called with zero exceptionVal");

  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!ilu_EnterMutex(server_lock(s), err))
    goto dun1;
  if (connection_concurrent(conn)) {
    if (!_ilu_EnterConnCall(conn, call, FALSE, err))
      goto dun2;
    conn->co_nOuts--;
  }
  if (!_ilu_EnterConnIO(conn, FALSE, err))
    goto dun3;
  if (connection_closed(conn)) {
    (void) ILU_ERR_CONS1(comm_failure, err, minor, ilu_cfm_conn_lost, 6);
    goto dun4;
  }
  call->ca_ms = ilu_cmsHi;
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%s #%lu, excn %ld, argSize %lu)\n",
		     "ilu_BeginException", call_connection_id(call),
		     (unsigned long) call_serial_number(call),
		     (long) exceptionVal, (unsigned long) argSize));
  if (protocol_begin_exception(call_proto(call), call, exceptionCode,
			       sys_excn, argSize, err))
    call->ca_ios = ilu_ciosOut;
  goto dun2;
dun4:
  (void)  _ilu_ReleaseConnIO(conn, TRUE, err);
dun3:
  (void) _ilu_ReleaseConnCall(conn, call, TRUE, err);
dun2:
  (void) ilu_ExitMutex(server_lock(s), TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

/**before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err) && Call-Hi(call)*/

ilu_boolean
ilu_FinishReply(ilu_Call call,
		ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  call->ca_ios = ilu_ciosNone;
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%s #%lu)\n", "ilu_FinishReply",
		     call_connection_id(call), (unsigned long) call_serial_number(call)));
  return protocol_finish_reply(call_proto(call), call, err);
}

ilu_boolean 
ilu_FinishException(ilu_Call call,
		    ILU_ERRS((bad_locks, IoErrs)) * err)
{
  ilu_Connection  conn = call_connection(call);
  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  call->ca_ios = ilu_ciosNone;
  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%s #%lu)\n", "ilu_FinishException",
		     call_connection_id(call), (unsigned long) call_serial_number(call)));
  return protocol_finish_exception(call_proto(call), call, err);
}

/*L1 >= {call's server}; L2, Main unconstrained */
ilu_boolean
_ilu_CacheCall(ilu_Call call, ilu_Message * reply,
	       ILU_ERRS((internal)) * err)
{
  ilu_Connection  conn = call_connection(call);
  ilu_Port        p = connection_port(conn);
  ilu_string peerinfo = conn_peerinfo(conn);
  ilu_integer     peerinfo_len = strlen(peerinfo);
  ilu_CachedCall *cc = p->po_call_cache;
  if (peerinfo_len >= MAX_CCPEER_LEN)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_tInfoLen, FALSE);
  if (cc[p->po_call_cache_finger].cc_replyMsg.msg_base != NIL)
    ilu_free(cc[p->po_call_cache_finger].cc_replyMsg.msg_base);
  strcpy(cc[p->po_call_cache_finger].cc_peerinfo, peerinfo);
  cc[p->po_call_cache_finger].cc_sn = call_serial_number(call);
  cc[p->po_call_cache_finger].cc_intro_type = call_intro_type(call);
  cc[p->po_call_cache_finger].cc_meth = call_method(call);
  cc[p->po_call_cache_finger].cc_replyMsg = *reply;
  p->po_call_cache_finger++;
  p->po_call_cache_finger %= p->po_call_cache_size;
  ILU_CLER(*err);
  return TRUE;
}

/*before: L2 not >=   {call's conn's iomu},
	  L2     >=   {call's conn's callmu} iff protocol not concurrent;
  after:  L2 disjoint {call's conn's callmu, iomu}*/
ilu_boolean 
ilu_NoReply(ilu_Call call,
	    ILU_ERRS((bad_param, bad_locks, broken_locks)) * err)
{
  ilu_Server      s = call_server(call);
  ilu_Connection  conn = call_connection(call);

  DEBUG(CALL_DEBUG, (stderr, "%-20.20s(%s #%lu)\n", "ilu_NoReply",
		     call_connection_id(call), (unsigned long) call_serial_number(call)));
  if (!ilu_EnterMutex(ilu_cmu, err))
    goto dun0;
  if (!ilu_EnterMutex(server_lock(s), err))
    goto dun1;
  if (connection_concurrent(conn)) {
    if (!_ilu_EnterConnCall(conn, call, FALSE, err))
      goto dun2;
    conn->co_nOuts--;
  }
  (void) _ilu_ReleaseConnCall(conn, call,
			      connection_concurrent(conn), err);
dun2:
  call->ca_ms = ilu_cmsNo;
  (void) ilu_ExitMutex(server_lock(s), TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
dun0:
  return ILU_ERROK(*err);
}

/*L1, L2 unconstrained for sizing*/
/**for input and output:
   before: Main Invariant, Call-Hi(call);
    after: Call-Invariant(call, err),
	   success => Call-Hi(call)*/

/* ==================== optional ==================== */

void ilu_OutputOptional (ilu_Call call, ilu_boolean i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_optional(call_proto(call), call, i, err);
  return;
}

void ilu_InputOptional (ilu_Call call, ilu_boolean *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_optional(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfOptional (ilu_Call call, ilu_boolean i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_optional(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== integer ==================== */

void 
ilu_OutputInteger(ilu_Call call, ilu_integer i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_integer(call_proto(call), call, i, err);
  return;
}

void 
ilu_InputInteger(ilu_Call call, ilu_integer * i,
		 ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_integer(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfInteger (ilu_Call call, ilu_integer i,
				      ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_integer(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== cardinal ==================== */

void ilu_OutputCardinal (ilu_Call call, ilu_cardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_cardinal(call_proto(call), call, i, err);
  return;
}

void ilu_InputCardinal (ilu_Call call, ilu_cardinal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_cardinal(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfCardinal (ilu_Call call, ilu_cardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_cardinal(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== character ==================== */

void 
ilu_OutputCharacter(ilu_Call call, ilu_character i,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_character(call_proto(call), call, i, err);
  return;
}

void ilu_InputCharacter (ilu_Call call, ilu_character *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_character(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfCharacter (ilu_Call call, ilu_character i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_character(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short integer ==================== */

void
ilu_OutputShortInteger(ilu_Call call, ilu_shortinteger i,
		       ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_short_integer(call_proto(call), call, i, err);
  return;
}

void ilu_InputShortInteger (ilu_Call call, ilu_shortinteger *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_short_integer(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfShortInteger (ilu_Call call, ilu_shortinteger i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_short_integer(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== long integer ==================== */

void ilu_OutputLongInteger (ilu_Call call, ilu_longinteger i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_long_integer(call_proto(call), call, i, err);
  return;
}

void ilu_InputLongInteger (ilu_Call call, ilu_longinteger *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_long_integer(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfLongInteger (ilu_Call call, ilu_longinteger i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_long_integer(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short cardinal ==================== */

void ilu_OutputShortCardinal (ilu_Call call, ilu_shortcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_short_cardinal(call_proto(call), call, i, err);
  return;
}

void ilu_InputShortCardinal (ilu_Call call, ilu_shortcardinal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_short_cardinal(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfShortCardinal (ilu_Call call, ilu_shortcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_short_cardinal(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== long cardinal ==================== */

void ilu_OutputLongCardinal (ilu_Call call, ilu_longcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_long_cardinal(call_proto(call), call, i, err);
  return;
}

void ilu_InputLongCardinal (ilu_Call call, ilu_longcardinal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_long_cardinal(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfLongCardinal (ilu_Call call, ilu_longcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_long_cardinal(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== enumeration ==================== */

void ilu_OutputEnum (ilu_Call call, ilu_shortcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_enum_code(call_proto(call), call, i, err);
  return;
}

void ilu_InputEnum (ilu_Call call, ilu_shortcardinal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_enum_code(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfEnum (ilu_Call call, ilu_shortcardinal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_enum_code(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== real ==================== */

void ilu_OutputReal (ilu_Call call, double i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_real(call_proto(call), call, i, err);
  return;
}

void ilu_InputReal (ilu_Call call, double *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_real(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfReal (ilu_Call call, double i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_real(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short real ==================== */

void ilu_OutputShortReal (ilu_Call call, float i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_short_real(call_proto(call), call, i, err);
  return;
}

void ilu_InputShortReal (ilu_Call call, float *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_short_real(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfShortReal (ilu_Call call, float i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_short_real(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== long real ==================== */

void ilu_OutputLongReal (ilu_Call call, ilu_longreal i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_long_real(call_proto(call), call, i, err);
  return;
}

void ilu_InputLongReal (ilu_Call call, ilu_longreal *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_long_real(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfLongReal (ilu_Call call, ilu_longreal i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_long_real(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== byte ==================== */

void ilu_OutputByte (ilu_Call call, ilu_byte i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_byte(call_proto(call), call, i, err);
  return;
}

void ilu_InputByte (ilu_Call call, ilu_byte *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_byte(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfByte (ilu_Call call, ilu_byte i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_byte(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== short character ==================== */

void ilu_OutputShortCharacter (ilu_Call call, ilu_shortcharacter i,
			       ILU_ERRS((IoErrs)) * err)
{
  ilu_OutputByte (call, (ilu_byte) i, err);
}

void ilu_InputShortCharacter (ilu_Call call, ilu_shortcharacter *i,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_InputByte (call, (ilu_byte *) i, err);
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfShortCharacter (ilu_Call call, ilu_shortcharacter i,
				       ILU_ERRS((IoErrs)) * err)
{
  return (ilu_SizeOfByte (call, (ilu_byte) i, err));
}

/* ==================== boolean ==================== */

void ilu_OutputBoolean (ilu_Call call, ilu_boolean i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_boolean(call_proto(call), call, i, err);
  return;
}

void ilu_InputBoolean (ilu_Call call, ilu_boolean *i,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_input_boolean(call_proto(call), call, i, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfBoolean (ilu_Call call, ilu_boolean i,
		  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_boolean(call_proto(call), call, i, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== string ==================== */

void
ilu_OutputString(ilu_Call call, ilu_string s, ilu_cardinal len,
		 ilu_cardinal limit,
		 ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (limit > 0 && len > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel != 0)
    {
      /* check to see that the string has no octet 0 chars in it,
	 as ILU string types don't allow that */
      if (strlen(s) < len)
	{
	  ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_string_null_char, 0);
	  return;
	}
    }
#endif
  protocol_output_string(call_proto(call), call, s, len, limit, err);
  return;
}

void
ilu_InputString(ilu_Call call, ilu_string * s, ilu_cardinal * len,
		ilu_cardinal limit,
		ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_string(call_proto(call), call, s, len, limit, err);
#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel != 0 && ILU_ERROK(*err))
    {
      /* check to see that the string has no octet 0 chars in it,
	 as ILU string types don't allow that */
      if (strlen(*s) < *len)
	ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_string_null_char, 0);
    }
#endif
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfString (ilu_Call call, ilu_string i,
			       ilu_cardinal l, ilu_cardinal limit,
			       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (limit > 0 && l > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
#ifdef ENABLE_DEBUGGING
  if (_ilu_DebugLevel != 0)
    {
      /* check to see that the string has no octet 0 chars in it,
	 as ILU string types don't allow that */
      if (strlen(i) < l)
	{
	  ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_string_null_char, 0);
	  return 0;
	}
    }
#endif
  len = protocol_size_of_string(call_proto(call), call, i, l, limit, err);
  return (ILU_ERROK(*err) ? len : 0);
}

/* ==================== stringvec ==================== */

void 
ilu_OutputStringVec(ilu_Call call, ilu_string i, ilu_cardinal len,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_stringvec(call_proto(call), call, i, len, err);
  return;
}

void 
ilu_InputStringVec(ilu_Call call, ilu_string * i, ilu_cardinal len,
		   ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_stringvec(call_proto(call), call, i, len, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfStringVec (ilu_Call call, ilu_string i,
				  ilu_cardinal len,
				  ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len2;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len2 = protocol_size_of_stringvec(call_proto(call), call, i, len,
				    err);
  return (ILU_ERROK(*err) ? len2 : 0);
}

/* ==================== wstring ==================== */

ilu_cardinal _ilu_SizeOfWString (ilu_Call call, ilu_wstring s,
				ilu_cardinal l1	/* size of wstring */,
				ilu_cardinal limit,
				ILU_ERRS((IoErrs)) *err)
{
  ilu_string      buf;
  register ilu_string p1;
  register ilu_character *p2;
  ilu_cardinal    len, size;
  ilu_character  *p2Limit;

  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (limit > 0 && l1 > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  size = protocol_size_of_cardinal(call_proto(call), call, l1, err);
  if (ILU_ERRNOK(*err))
    return 0;
  p2Limit = s + l1;
  buf = ilu_malloc(l1 * 3);
  for (p1 = buf, p2 = s; p2 < p2Limit;) {
    if (*p2 & 0xF800) {
      *p1++ = (char) (0xE0 | (*p2 >> 12));
      *p1++ = (char) (0x80 | (((ilu_cardinal) (*p2 & 0x0FC0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x3F));
    } else if (*p2 & 0x0780) {
      *p1++ = (char) (0xC0 | ((ilu_cardinal) (*p2 & 0x07C0)) >> 6);
      *p1++ = (char) (0x80 | (*p2++ & 0x003F));
    } else
      *p1++ = (char) (*p2++ & 0x7F);
  }
  len = p1 - buf;
  size += protocol_size_of_bytes(call_proto(call), call,
				 (ilu_bytes) buf, len, 0, err);
  ilu_free(buf);
  return (ILU_ERROK(*err) ? size : 0);
}

void _ilu_OutputWString (ilu_Call call, ilu_wstring s, ilu_cardinal l1,
			 ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf;
  register ilu_string p1;
  register ilu_character *p2, *p2Limit;
  ilu_cardinal    len;

  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (limit > 0 && l1 > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_cardinal(call_proto(call), call, l1, err);
  if (ILU_ERRNOK(*err))
    return;
  p2Limit = s + l1;
  buf = (ilu_string) ilu_malloc(l1 * 3);
  for (p1 = buf, p2 = s; p2 < p2Limit;) {
    if (*p2 & 0xF800) {
      *p1++ = (char) (0xE0 | (*p2 >> 12));
      *p1++ = (char) (0x80 | (((ilu_cardinal) (*p2 & 0x0FC0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x3F));
    } else if (*p2 & 0x0780) {
      *p1++ = (char) (0xC0 | (((ilu_cardinal) (*p2 & 0x07C0)) >> 6));
      *p1++ = (char) (0x80 | (*p2++ & 0x003F));
    } else
      *p1++ = (char) (*p2++ & 0x7F);
  }
  len = p1 - buf;
  protocol_output_bytes(call_proto(call), call, (ilu_bytes) buf,
			len, 0, err);
  ilu_free(buf);
  return;
}

static          ilu_boolean
UTF2Decode(ilu_character * tobuf, ilu_string frombuf,
	   ilu_cardinal tolen, ilu_cardinal fromlen,
	   ILU_ERRS((internal)) * err)
{
  ilu_string      p2 = frombuf, p2lim = frombuf + fromlen;
  ilu_character  *p1 = tobuf;
  ilu_cardinal    count;
  for (count = 0; count < tolen && p2 < p2lim; count++) {
    if ((*p2 & 0xF0) == 0xE0) {
      *p1++ = ((p2[0] & 0x0F) << 12) | ((p2[1] & 0x3F) << 6)
	| (p2[2] & 0x3F);
      p2 += 3;
    } else if ((*p2 & 0xE0) == 0xC0) {
      *p1++ = ((p2[0] & 0x1F) << 6) | (p2[1] & 0x3F);
      p2 += 2;
    } else
      *p1++ = *p2++ & 0x7F;
  }
  if (count == tolen && p2 == p2lim)
    return ILU_CLER(*err);
  else
    return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_utf2Len, FALSE);
}

void
_ilu_InputWString(ilu_Call call, ilu_wstring * s, ilu_cardinal * l,
		  ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf = NIL;
  ilu_character  *ubuf = NIL;
  ilu_cardinal    len = 0, len2;

  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_cardinal(call_proto(call), call, &len, err);
  if (ILU_ERRNOK(*err))
    return;
  protocol_input_bytes(call_proto(call), call, (ilu_bytes *) &buf, &len2, 0, err);
  if (ILU_ERRNOK(*err))
    return;
  ubuf = (ilu_wstring) ilu_MallocE((len + 1) * sizeof(ilu_character),
				   err);
  if (ubuf == NIL)
    return;
  if (!UTF2Decode(ubuf, buf, len, len2, err)) {
    ilu_free(ubuf);
    *s = NIL;
    *l = 0;
    return;
  }
  ubuf[len] = 0;
  *s = ubuf;
  *l = len;
  return;
}

ilu_cardinal ilu_SizeOfWString (ilu_Call call, ilu_wstring s,
				ilu_cardinal l1	/* size of wstring */,
				ilu_cardinal limit,
				ILU_ERRS((IoErrs)) *err)
{
  ilu_string      buf;
  register ilu_string p1;
  register ilu_character *p2;
  ilu_cardinal    len, size;
  ilu_character  *p2Limit;

  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (limit > 0 && l1 > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len = protocol_size_of_wstring(call_proto(call), call, s, l1, limit, err);
  return (ILU_ERROK(*err) ? len : 0);
}

void ilu_OutputWString (ilu_Call call, ilu_wstring s, ilu_cardinal l1,
			ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf;
  register ilu_string p1;
  register ilu_character *p2, *p2Limit;
  ilu_cardinal    len;

  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (limit > 0 && l1 > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_wstring(call_proto(call), call, s, l1, limit, err);
  return;
}

void
ilu_InputWString(ilu_Call call, ilu_wstring * s, ilu_cardinal * l,
		 ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf = NIL;
  ilu_character  *ubuf = NIL;
  ilu_cardinal    len = 0, len2;

  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_wstring(call_proto(call), call, s, l, limit, err);
  return;
}

/* ==================== wstringvec ==================== */

void 
  _ilu_OutputWStringVec(ilu_Call call, ilu_wstring i,
			ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  ilu_OutputWString(call, i, len, len, err);
}

void
  _ilu_InputWStringVec(ilu_Call call, ilu_wstring * s,
		       ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  ilu_string      buf = NIL;
  ilu_cardinal    len1 = 0, len2;
  ilu_boolean     isnew = (*s) == NIL;

  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_cardinal(call_proto(call), call, &len1, err);
  if (ILU_ERRNOK(*err))
    return;
  if (len1 != len) {
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_wronglen, 6);
    return;
  }
  protocol_input_string(call_proto(call), call, &buf, &len2, 0, err);
  if (ILU_ERRNOK(*err))
    return;
  if (isnew) {
    *s = (ilu_wstring) ilu_MallocE(sizeof(ilu_character) * len + 1,
				   err);
    if (*s == NIL)
      return;
  }
  if (!UTF2Decode(*s, buf, len, len2, err) && isnew) {
    ilu_free(*s);
    *s = NIL;
  }
  return;
}

ilu_cardinal
  _ilu_SizeOfWStringVec (ilu_Call call, ilu_wstring i,
			 ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  return (ilu_SizeOfWString (call, i, len, len, err));
}

void 
ilu_OutputWStringVec(ilu_Call call, ilu_wstring i, ilu_cardinal len,
		     ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_wstringvec(call_proto(call), call, i, len, err);
}

void
ilu_InputWStringVec(ilu_Call call, ilu_wstring * s, ilu_cardinal len,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_wstringvec(call_proto(call), call, s, len, err);
}

ilu_cardinal ilu_SizeOfWStringVec (ilu_Call call, ilu_wstring i,
				   ilu_cardinal len,
				   ILU_ERRS((IoErrs)) * err)
{
  if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  return (protocol_size_of_wstringvec(call_proto(call), call, i, len, err));
}

/* ==================== bytes ==================== */

void 
ilu_OutputBytes(ilu_Call call, ilu_bytes i, ilu_cardinal len,
		ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (limit > 0 && len > limit)
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else
    protocol_output_bytes(call_proto(call), call, i, len, limit, err);
  return;
}

void 
ilu_InputBytes(ilu_Call call, ilu_bytes * i, ilu_cardinal * len,
	       ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_bytes(call_proto(call), call, i, len, limit, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfBytes (ilu_Call call, ilu_bytes i,
			      ilu_cardinal len, ilu_cardinal limit,
			      ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len2;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (limit > 0 && len > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len2 = protocol_size_of_bytes(call_proto(call), call, i, len, limit,
				err);
  return (ILU_ERROK(*err) ? len2 : 0);
}

/* ==================== opaque ==================== */

void 
ilu_OutputOpaque(ilu_Call call, ilu_opaque i,
		 ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_opaque(call_proto(call), call, i, len, err);
  return;
}

void 
ilu_InputOpaque(ilu_Call call, ilu_opaque * i,
		ilu_cardinal len, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_opaque(call_proto(call), call, i, len, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfOpaque (ilu_Call call, ilu_opaque i,
			       ilu_cardinal len,
			       ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    len2;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len2 = protocol_size_of_opaque(call_proto(call), call, i, len, err);
  return (ILU_ERROK(*err) ? len2 : 0);
}

/* ==================== object ==================== */

/*Main Remnant holds.
  before: h!=NIL => L1 = {h's server};
	  h==NIL => L1 = {}.
  after:  exn    => L1 = {} if possible; else
	  h!=NIL => L1 = {h's server}; else
	  h==NIL => L1 = {}.*/
ilu_cardinal ilu_SizeOfObjectID(ilu_Call call, ilu_Object h,
				ilu_boolean discriminator_p,
				ilu_Class static_type,
				ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal    len2;
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    goto faild;
  }
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  len2 = protocol_size_of_object_id(call_proto(call), call, h,
				    discriminator_p, static_type,
				    err);
  if (ILU_ERROK(*err))
    return len2;
faild:
  if (h != NIL) {
    ilu_Server      s = h->ob_server;
    ILU_ERRS((internal, bad_locks)) lerr;
    ilu_ExitMutex(server_lock(s), FALSE, &lerr);
    ILU_HANDLED(lerr);
  }
  return 0;
}

/**L2 >= {call's conn's callmu, iomu}.
  h == NIL => Main Invariant holds.
  h != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = h's server and cl = h's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
void ilu_OutputObjectID (ilu_Call call, ilu_Object h,
			 ilu_boolean discriminator_p,
			 ilu_Class static_type,
			 ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  (void) protocol_output_object_id(call_proto(call), call, h,
				discriminator_p, static_type, err);
  return;
}

/**Main Remnant holds, L2 >= {call's connection's callmu, iomu};
  before: L1 = {},
  after:  *o!=NIL => Inside(*o's server, static_type);
  after:  *o==NIL => L1 = {};
  after:  ILU_ERRNOK(*err) => *o==NIL*/
void ilu_InputObjectID (ilu_Call call, ilu_Object *h,
			ilu_boolean discriminator_p,
			ilu_Class static_type,
			ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  *h = NIL;
  (void) protocol_input_object_id(call_proto(call), call, h,
				discriminator_p, static_type, err);
  return;
}

/* ==================== generic object ==================== */

/*L2 >= {call's connection's callmu, iomu}.
  h == NIL => Main Invariant holds.
  h != NIL => all the following:
  before: Inside(s, cl);
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = h's server and cl = h's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ilu_boolean 
_ilu_OutputObjectID(ilu_Call call, ilu_Object h,
		    ilu_boolean discriminator_p,
		    ilu_Class static_type,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal	server_id_hash;
  ilu_string      ostr2 = NIL;
  ilu_boolean     is_nil = (h == NIL);

  if (call_connection(call) == NIL) {
    if (h != NIL)
      ilu_ExitServer(object_server(h), object_class(h));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  }
  if (discriminator_p) {
    if (h == NIL)
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
    server_id_hash = object_server(h)->sr_crc32;
    ostr2 = h->ob_ih;
  } else if (h == NIL) {
    if (!static_type->cl_optional)
      return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
    ostr2 = "";
  } else {
    ostr2 = ilu_SBHOfObject(h);
  }

  if (ostr2 == NIL) {
    ilu_ExitServer(object_server(h), object_class(h));
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_not_exported,
			 FALSE);
  }
  if (!is_nil) {
    if (object_is_true(h) && object_collectible(h)) {
      ILU_ERRS((BadDataStructure, KernelBroken)) lerr;
      object_lastRemote(h) = ilu_CoarseTime_Now();
      lerr = _ilu_TouchedObj(h);
      ILU_MUST_BE_SUCCESS(lerr);
    }
    ilu_ExitServer(object_server(h), object_class(h));
  }
  if (discriminator_p) {
    protocol_output_cardinal(call_proto(call), call, server_id_hash, err);
    if (ILU_ERRNOK(*err))
      return FALSE;
  }
  protocol_output_string(call_proto(call), call, ostr2,
			 _ilu_SafeStrlen(ostr2), 0xFFFF, err);
  return (ILU_ERROK(*err));
}

/*before: L1 = {};
  after:  *h!=NIL => Inside(*h's server, static_type);
  after:  *h==NIL => L1 = {};
  L2 >= {call's connection's callmu, iomu};
  Main Remnant holds*/
ilu_boolean 
_ilu_InputObjectID(ilu_Call call, ilu_Object * h,
		   ilu_boolean discriminator_p,
		   ilu_Class static_type,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal server_id_hash;
  ilu_string      istr2 = NIL;
  ilu_cardinal    istrlen2 = 0;
  ilu_Server      server = connection_server(call_connection(call));

  *h = NIL;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  if (static_type == NIL) {
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  }
  if (discriminator_p) {
    protocol_input_cardinal (call_proto(call),
			     call, &server_id_hash, err);
    if (ILU_ERRNOK(*err))
      return FALSE;
    DEBUG(INCOMING_DEBUG | OBJECT_DEBUG,
	  (stderr,
	   "ilu_InputObjectID:  server id hash is %lx\n",
	   server_id_hash));
  }
  protocol_input_string(call_proto(call),
			call, &istr2, &istrlen2, 0xFFFF, err);
  if (ILU_ERRNOK(*err)) {
    return FALSE;
  }
  DEBUG(INCOMING_DEBUG,
	(stderr, "ilu_InputObjectID:  instance handle/sbh is <%s>\n",
	 istr2));
  ILU_CLER(*err);
  if (discriminator_p) {
    ilu_string      ih = istr2;
    ilu_EnterServer(server, static_type);
    if (server_id_hash != server->sr_crc32) {
      DEBUG(INCOMING_DEBUG,
	    (stderr,
	     "%s %lx is for wrong server (not expected %lx, for <%s>).\n",
	     "ilu_InputObjectID:  incoming sid hash", server_id_hash,
	     server->sr_crc32, server_id(server)));
      (void) ILU_ERR_CONS1(marshal, err, minor, ilu_mm_alien_disc, 6);
      ilu_ExitServer(server, static_type);
    } else if (server_objs(server) == NIL) {
      DEBUG(INCOMING_DEBUG,
	    (stderr, "%s %s is in closed server <%s>.\n",
	     "ilu_InputObjectID:  instance", ih, server_id(server)));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, 6);
      ilu_ExitServer(server, static_type);
    } else if ((*h = _ilu_FindObjectInServer(ih, server))
	       == NIL) {
      DEBUG(INCOMING_DEBUG,
	    (stderr, "%s %s not found in server <%s>.\n",
	     "ilu_InputObjectID:  instance", ih, server_id(server)));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_inst_nf, 6);
      ilu_ExitServer(server, static_type);
    } else if (!ilu_IsSubObjectType((*h)->ob_class, static_type)) {
      DEBUG(INCOMING_DEBUG,
	    (stderr,
	  "%s %s/%s has type %s (%s), not a subtype of %s (%s).\n",
	     "_ilu_InputObjectID: Existing object",
	     server_id(server), ih,
	     (*h)->ob_class->cl_unique_id, (*h)->ob_class->cl_name,
	     static_type->cl_unique_id, static_type->cl_name));
      (void) ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_wrong_type, 6);
      *h = NIL;
      ilu_ExitServer(server, static_type);
    }
    if (ILU_ERRNOK(*err)) {
      FREETOKEN(istr2);
      return (FALSE);
    }
  } else {
    if (istrlen2 == 0) {
      *h = NIL;
      if (static_type->cl_optional) {
	if (istr2 != NIL)
	  ilu_free(istr2);
	return TRUE;
      } else {
	DEBUG(INCOMING_DEBUG,
	      (stderr, "ilu_InputObjectID:  bad NIL obj.\n"));
	return ILU_ERR_CONS1(NoObjectForSBH, err, sbh, istr2, FALSE);
      }
    } else {
      *h = ilu_ObjectOfSBH(istr2, static_type, err);
      if (ILU_ERRNOK(*err)) {
	DEBUG(INCOMING_DEBUG,
	      (stderr,
	   "ilu_InputObjectID:  error:  No object for SBH <%s>.\n",
	       istr2));
	ilu_free(istr2);
	return FALSE;
      }
    }
  }

  ilu_free(istr2);
  return (TRUE);
}

/*h!=NIL => L1 >= {h's server}, L1_sup < prmu;
  h==NIL => L1 unconstrained*/
ilu_cardinal _ilu_SizeOfObjectID(ilu_Call call, ilu_Object h,
				 ilu_boolean discriminator_p,
				 ilu_Class static_type,
				 ILU_ERRS((IoErrs)) *err)
{
  ilu_cardinal	server_id_hash;
  ilu_string      strB;		/* discriminator_p ? ih : SBH */
  ilu_cardinal    size1, size2;
  ilu_Connection  conn = call_connection(call);

  ILU_CLER(*err);		/* assume success */

  if (conn == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);

  if (h == NIL) {
    strB = "";
  } else if (discriminator_p) {
    server_id_hash = h->ob_server->sr_crc32;
    strB = h->ob_ih;
  } else {
    strB = ilu_SBHOfObject(h);
  }
  if (strB == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_broken, 0);

  if (discriminator_p) {
    size1 = protocol_size_of_cardinal(connection_protocol(conn), call,
				      server_id_hash, err);
    if (ILU_ERRNOK(*err))
      return 0;
  } else
    size1 = 0;

  size2 = protocol_size_of_string(connection_protocol(conn), call,
				  strB, _ilu_SafeStrlen(strB),
				  0xFFFF, err);
  if (ILU_ERRNOK(*err))
    return 0;
  return (size1 + size2);
}

/*before: Main Invariant holds, L2 >= {call's conn's callmu, iomu};
  after:  Call-Remnant  (call, err) && call->ca_ms == ilu_cmsHi;
  after:  result!=NIL => Inside(call->ca_server, call->ca_intro_type);
  after:  result==NIL => L1 = {}*/
ilu_Object 
ilu_GetCallSingleton(ilu_Call call,
		     ILU_ERRS((bad_param)) * err)
{
  ilu_Server      s = call->ca_server;
  ilu_Class       intro_type = call->ca_intro_type;
  ilu_Object      ans = NIL;
  ilu_EnterServer(s, intro_type);
  if (server_singles(s) != NIL)
    ans = (ilu_Object) _ilu_hash_FindInTable(server_singles(s),
					     intro_type);
  if (ans != NIL)
    return (ILU_CLER(*err), ans);
  ilu_ExitServer(s, intro_type);
  call->ca_pe = ilu_ProtocolException_NoSuchClassAtServer;
  return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, NIL);
}

/*************************************************************
**************************************************************
**************************************************************

  Code for sequences, records, arrays, and unions

**************************************************************
**************************************************************
*************************************************************/

void 
ilu_OutputSequence(ilu_Call call, ilu_cardinal i, ilu_cardinal limit,
		   ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  if (limit > 0 && i > limit) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_sequence(call_proto(call), call, i, limit, err);
  return;
}

void ilu_OutputSequenceMark (ilu_Call call, ilu_cardinal extent,
		  ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    {ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0); return;}
  protocol_output_sequence_mark(call_proto(call), call, extent, err);
  return;
}

void
ilu_InputSequenceMark(ilu_Call call, ilu_cardinal extent,
		      ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_sequence_mark(call_proto(call), call,
				      extent, err);
  return;
}

void 
ilu_InputSequence(ilu_Call call, ilu_cardinal * i,
		  ilu_cardinal limit, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_sequence(call_proto(call), call, i, limit, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal
ilu_SizeOfSequence(ilu_Call call, ilu_cardinal i, ilu_cardinal limit,
		   ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  if (limit > 0 && i > limit)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  size = protocol_size_of_sequence(call_proto(call), call,
				   i, limit, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndSequence (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  protocol_end_sequence(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

void 
ilu_OutputUnion(ilu_Call call, ilu_cardinal i, ilu_cardinal dsize,
		ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_union(call_proto(call), call, i, dsize, err);
  return;
}

void 
ilu_InputUnion(ilu_Call call, ilu_cardinal * i, ilu_cardinal dsize,
	       ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_union(call_proto(call), call, i, dsize, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal 
ilu_SizeOfUnion(ilu_Call call, ilu_cardinal i, ilu_cardinal dsize,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  size = protocol_size_of_union(call_proto(call), call, i, dsize, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndUnion (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  protocol_end_union(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

void 
ilu_OutputArray(ilu_Call call, ilu_cardinal length,
		ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_array(call_proto(call), call, length, err);
  return;
}

void 
ilu_InputArray(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_array(call_proto(call), call, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal 
ilu_SizeOfArray(ilu_Call call, ilu_cardinal length,
		ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  size = protocol_size_of_array(call_proto(call), call,
				length, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndArray (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  protocol_end_array(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

void 
ilu_OutputRecord(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_output_record(call_proto(call), call, err);
  return;
}

void 
ilu_InputRecord(ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
    return;
  }
  protocol_input_record(call_proto(call), call, err);
  return;
}

/*ARGSUSED*/
ilu_cardinal ilu_SizeOfRecord (ilu_Call call, ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  size = protocol_size_of_record(call_proto(call), call, err);
  return (ILU_ERROK(*err) ? size : 0);
}

/*L1, L2 unconstrained*/
ilu_boolean ilu_EndRecord (ilu_Call call, ILU_ERRS((IoErrs)) *err)
{
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_duh, FALSE);
  protocol_end_record(call_proto(call), call, err);
  return ILU_ERROK(*err);
}

ilu_Passport ilu_CallerPassportOfCall (ilu_Call call)
{
  return (call->ca_caller);
}

void ilu_SetCallerPassportOfCall (ilu_Call call, ilu_Passport pport)
{
  call->ca_caller = pport;
}
