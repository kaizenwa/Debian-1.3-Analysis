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
/* $Id: connect.c,v 1.67 1996/07/02 17:26:17 larner Exp $ */
/* Last edited by Mike Spreitzer June 21, 1996 8:55 am PDT */

#define _POSIX_SOURCE

#ifdef MACOS
#pragma segment ilu
#endif

#include "iluntrnl.h"

#include "object.h"
#include "server.h"
#include "connect.h"
#include "transprt.h"
#include "protocol.h"

/*L1, L2, Main unconstrained*/

ilu_Server ilu_ServerOfConnection(ilu_Connection conn)
{
  return (conn -> co_server);
}

/*Main Invariant holds*/

ilu_boolean 
_ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				      ilu_FineTime * limit,
				      ILU_ERRS((interrupt)) * err)
{
  ilu_Transport   p_transport;
  if (connection_closed(conn)) 
    /* if we were to proceed with this call on a closed connection, we're
       likely to crash since the transport info has been closed - freed 
       we can end up here with a closed connection (at least) in the case 
       where the garbage collector fired up and closed the connection .*/
    return ilu_FALSE;
  p_transport = connection_transport(conn);
  return (transport_wait_for_input(p_transport, limit, err));
}

ilu_boolean
ilu_BlockingWaitForInputOnConnection(ilu_Connection conn,
				     ilu_FineTime * limit)
{
  ilu_boolean     noIrq;
  ILU_ERRS((interrupt)) lerr;
  noIrq = _ilu_BlockingWaitForInputOnConnection(conn, limit, &lerr);
  ILU_HANDLED(lerr);
  return (noIrq);
}

/*Main Invariant holds; L2 disjoint {conn's iomu, callmu}*/
extern          ilu_boolean
ilu_SetConnectionInputHandler(ilu_Connection conn,
			      ilu_TransportInputHandler tih,
			      ilu_refany tih_rock,
			      ILU_ERRS((no_memory, internal,
					no_resources)) * err)
{
  ilu_Server      s = conn->co_server;
  ilu_Transport   trans = conn->co_transport;
  ilu_Mutex       sm = s->sr_lock;
  ilu_boolean     hardin = FALSE, closed, regd;
  while (1) {
    if (!ilu_EnterMutexWork(sm, hardin, err, __FILE__, __LINE__))
      return FALSE;
    hardin = TRUE;
    closed = conn->co_closed;
    if (!ilu_ExitMutex(sm, TRUE, err))
      return FALSE;
    if (closed)
      return TRUE;
    regd = ilu_SetTransportInputHandler(trans, tih, tih_rock, err);
    if (regd)
      return TRUE;
    if (ILU_ERRNOK(*err))
      return FALSE;
  }
}

/*L1 >= {conn's server}, L2 unconstrained*/
extern          ilu_boolean
ilu_ClearConnectionInputHandler(ilu_Connection conn,
				ILU_ERRS((no_memory, internal,
					  no_resources)) * err)
{
  ilu_Transport   trans = conn->co_transport;
  ilu_boolean     regd;
  if (conn->co_closed)
    return ILU_CLER(*err);
  regd = ((*trans->tr_class->tc_set_input_handler)
	  (trans, 0, NIL, err));
  if (ILU_ERRNOK(*err))
    return FALSE;
  if (!ilu_Check(regd, err))
    return FALSE;
  return TRUE;
}

/*Main unconstrained*/

/*L1 >= {cmu, conn's server}; L1_sup < trmu*/
/*L2 >= {conn's iomu}*/
void 
_ilu_CloseIoingConnection(ilu_Connection conn,
			  ilu_boolean set_cfails)
{
  ilu_Error       err;
  ilu_Protocol    proto = connection_protocol(conn);
  ilu_Transport   trans = conn->co_transport;
  ilu_integer     dfd;

  _ilu_Assert(conn->co_ioing, "bad call on _ilu_CloseIoingConnection");
  if (set_cfails)
    conn->co_server->sr_cfails = ilu_TRUE;
  if (!connection_closed(conn)) {
#ifdef ENABLE_DEBUGGING
    if (connection_incoming(conn))
      {
	DEBUG(CONNECTION_DEBUG,
	      (stderr,
	       "%s:  incoming conn %p trans %p via %s from %s to %s; cfails=%s.\n",
	       "_ilu_CloseIoingConnection", conn, trans, conn->co_pinfo,
	       conn_peerinfo(conn), conn->co_server->sr_id,
	       set_cfails ? "T" : "F"));
      }
    else
      {
	ilu_string t = _ilu_StringifyTinfo(conn_tinfo(conn), &err);
	ILU_MUST_BE_SUCCESS(err);
	DEBUG(CONNECTION_DEBUG,
	      (stderr,
	       "%s:  outgoing conn %p trans %p via %s %s to %s; cfails=%s.\n",
	       "_ilu_CloseIoingConnection", conn, trans, conn->co_pinfo, t,
	       conn->co_server->sr_id, set_cfails ? "T" : "F"));
	ilu_free(t);
      }
#endif /* ENABLE_DEBUGGING */
    if ((conn->co_mucall == NIL) && (conn->co_nOuts == 0))
      _ilu_UnlinkConnection(&ilu_idleConns, conn, ilu_lru);
    _ilu_ClearConnectionFromServer(conn, conn->co_server);
    transport_close(trans, &dfd, &err);
    ilu_DeltaFD(-dfd);
    ILU_MUST_BE_SUCCESS(err);
    protocol_free_data_block(proto, connection_protocol_data(conn));
    while (conn->co_replies != NIL) {
      ilu_ReplyList   r = conn->co_replies;
      ilu_ReplyList   next = r->rp_next;
      (void) protocol_abandon_delayed_interp(proto, r->rp_queued, &err);
      ILU_MUST_BE_SUCCESS(err);
      ilu_free(r);
      conn->co_replies = next;
    }
    if (conn->co_auth_info != NIL)
      ilu_DestroyPassport(conn->co_auth_info, &err);
    ILU_MUST_BE_SUCCESS(err);
    conn->co_closed = TRUE;
    conn->co_protocol_data = NIL;
    if (_ilu_CanCondition()) {
      err = _ilu_NotifyCondition(conn->co_cc);
      ILU_MUST_BE_SUCCESS(err);
    }
  }
  return;
}

/*L1, L2 unconstrained*/
static struct _ilu_Call_s fauxcall = {0};

/* Usable on either incoming or outgoing connections. */
/*L1 = {conn's server, cmu}; L2 disjoint {conn's callmu, iomu}*/
ILU_ERRS((bad_locks, broken_locks))
_ilu_CloseConnection(ilu_Connection connection)
{
  ilu_Error       err;
  if (!_ilu_EnterConnIO(connection, FALSE, &err))
    return err;
  _ilu_CloseIoingConnection(connection, ilu_FALSE);
  (void) _ilu_ReleaseConnIO(connection, TRUE, &err);
  return err;
}

/*L2 = {}*/
/*L1_sup < cmu*/
void ilu_CloseConnection (ilu_Connection connection)
{
  ilu_Server      s = connection->co_server;
  ilu_Error       err;
  if (connection_incoming(connection)) {
    _ilu_AcquireMutex(ilu_cmu);
    _ilu_AcquireMutex(server_lock(s));
    err = _ilu_CloseConnection(connection);
    ILU_MUST_BE_SUCCESS(err);
    _ilu_ReleaseMutex(server_lock(s));
    _ilu_ReleaseMutex(ilu_cmu);
  } else {
    /* Bitch to the caller, when we get our error system. */
    return;
  }
}

/*Main Invariant, L2 >= {conn's iomu}*/
extern          ilu_boolean
_ilu_CloseConnWithIo(ilu_Connection conn, ilu_boolean set_cfails,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_Server      s = conn->co_server;
  if (!ilu_ReEnterMutex(ilu_cmu, err))
    return FALSE;
  if (!ilu_ReEnterMutex(server_lock(s), err))
    goto dun1;
  _ilu_CloseIoingConnection(conn, set_cfails);
  (void) ilu_ExitMutex(server_lock(s), TRUE, err);
dun1:
  (void) ilu_ExitMutex(ilu_cmu, TRUE, err);
  return ILU_ERROK(*err);
}

/*L2 = {}*/
/*L1_sup < cmu*/
void ilu_DestroyConnection (ilu_Connection connection)
{
  ilu_Error       err;
  ilu_Server      s = connection->co_server;
#ifdef ENABLE_DEBUGGING
  if (connection_incoming(connection))
    {
      DEBUG(CONNECTION_DEBUG,
	    (stderr, "ilu_DestroyConnection:  conn %p via %s|%s to %s.\n",
	     connection, connection->co_pinfo, conn_peerinfo(connection),
	     connection->co_server->sr_id));
    }
  else
    {
      ilu_string t = _ilu_StringifyTinfo(conn_tinfo(connection), &err);
      ILU_MUST_BE_SUCCESS(err);
      DEBUG(CONNECTION_DEBUG,
	    (stderr, "ilu_DestroyConnection:  conn %p via %s|%s to %s.\n",
	     connection, connection->co_pinfo, t, connection->co_server->sr_id));
      ilu_free(t);
    }
#endif
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireMutex(server_lock(s));
  err = _ilu_CloseConnection(connection);
  ILU_MUST_BE_SUCCESS(err);
  ilu_free(conn_tinfo(connection));
  ilu_free(connection->co_pinfo);
  ilu_free(connection);
  _ilu_ReleaseMutex(server_lock(s));
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

/*L1 >= {cmu}*/
/*L2, Main unconstrained*/

ilu_integer     ilu_fdbudget = 16;	/* # FDs allowed */
ilu_integer     ilu_fdstaken = 0;	/* # FDs used (incl idle) */

void 
ilu_FullDeltaFD(ilu_integer n, char *file, int line)
{
  DEBUG(CONNECTION_DEBUG,
	(stderr, "ilu_fdstaken(was %ld) += %ld at %s:%d\n",
	 (long int) ilu_fdstaken,
	 (long int) n, file, line));
  ilu_fdstaken += n;
  return;
}

ilu_ConnLinks ilu_idleConns = {NIL, NIL};

/*L1_sup < cmu; L2 unconstrained*/
ilu_cardinal ilu_GetFDBudget(void)
{
  ilu_cardinal ans;
  ilu_AcquireMutex(ilu_cmu);
  ans = ilu_fdbudget;
  ilu_ReleaseMutex(ilu_cmu);
  return ans;
}

/*Main Invariant holds; L2 otherwise unconstrained*/
extern ilu_cardinal ilu_SetFDBudget(ilu_cardinal n)
{
  ILU_ERRS((WrongLocks)) err;
  ilu_cardinal    ans;
  ilu_AcquireMutex(ilu_cmu);
  err = _ilu_ReduceFdsTo(n);
  ILU_MUST_BE_SUCCESS(err);
  ilu_fdbudget = ilu_fdstaken;
  if (ilu_fdbudget < 0 || n > (ilu_cardinal) ilu_fdbudget)
    ilu_fdbudget = n;
  ans = ilu_fdbudget;
  ilu_ReleaseMutex(ilu_cmu);
  return ans;
}

/*for all conn: (L2 >= {conn's iomu}) => (L2 >= {conn's callmu})*/
/*Main unconstrained*/

/*L1_sup = cmu*/
ILU_ERRS((WrongLocks)) _ilu_ReduceFdsTo(ilu_integer goal)
{
  ilu_Connection  next, cur = ilu_idleConns.next;
  ilu_Error       err;
  _ilu_HoldMutex(ilu_cmu);
  while ((cur != NIL) && ((ilu_fdstaken) > goal)) {
    ilu_Server      s = cur->co_server;
    next = cur->co_links[ilu_lru].next;
    _ilu_AcquireMutex(server_lock(s));
    if ((cur->co_mucall == NIL) && (cur->co_nOuts == 0))
      /* What about waiting input? */
      if (0 < ((*cur->co_transport->tr_class->tc_closeDFd)
	       (cur->co_transport))) {
	(void) _ilu_EnterConnCall(cur, &fauxcall, TRUE, &err);
	/* can't block or err */
	ILU_MUST_BE_SUCCESS(err);
	(void) _ilu_EnterConnIO(cur, TRUE, &err);
	/* can't block or err */
	ILU_MUST_BE_SUCCESS(err);
	_ilu_CloseIoingConnection(cur, FALSE);
      }
    _ilu_ReleaseMutex(server_lock(s));
    cur = next;
  }
  return ILU_NO_ERR;
}

/*L1_sup = s; L1 >= {cmu}*/
/*L2, Main unconstrained*/
ilu_Connection 
_ilu_CreateConnection(ilu_Transport bs,
		      ilu_TransportInfo tinfo,	/* NIL for incoming connections */
		      ilu_string peerinfo,	/* NIL for outgoing connections */
		      ilu_Protocol pr, ilu_string pinfo,
		      ilu_Port port, ilu_Server s,
		      ilu_Passport pp,
		      ILU_ERRS((no_memory)) * err)
{
  ilu_Error		lerr = ILU_INIT_NO_ERR;
  ilu_Connection	ans;

  ans = (ilu_Connection) ilu_MallocE(sizeof(*ans), err);
  if (ans == NIL)
    return ans;
  _ilu_HoldMutex(server_lock(s));
  ans->co_mucall = NIL;
  ans->co_ioing = FALSE;
  ans->co_protocol = pr;
  ans->co_protocol_data = protocol_create_data_block(pr, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ans->co_pinfo = ilu_StrdupE(pinfo, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ans->co_transport = bs;
  ans->co_port = port;
  if (port != NIL)
    conn_peerinfo(ans) = ilu_StrdupE(peerinfo, err);
  else
    conn_tinfo(ans) = _ilu_CopyTinfo(tinfo, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ans->co_conn_identity.ii_owned_by_passport = ilu_FALSE;
  if (port != NIL) {
    ans->co_conn_identity.ii_type = ilu_ConnectionIdentity;
    ans->co_conn_identity.ii_info = (ilu_refany) conn_peerinfo(ans);
    ans->co_auth_info = pp;
    if (!ilu_AddIdentity (pp, &ans->co_conn_identity, err))
      goto faild;
  } else {
    ans->co_conn_identity.ii_type = ilu_NoIdentity;
    ans->co_auth_info = NIL;
  };
  ans->co_server = s;
  ans->co_replies = NIL;
  ans->co_reader = NIL;
  if (_ilu_CanCondition()) {
    ans->co_cc = _ilu_CreateCondition("a connection of server ",
				      s->sr_id, &lerr);
    ILU_MUST_BE_SUCCESS(lerr);
  } else
    ans->co_cc = NIL;
  ans->co_closed = FALSE;
  ans->co_next_sn = 1;
  ans->co_nOuts = 0;
  if (connection_incoming(ans)) {
    _ilu_LinkConnection(&port->po_connHead, ans, ilu_psl);
  } else {
    _ilu_LinkConnection(&s->sr_connHead, ans, ilu_psl);
  }
  _ilu_LinkConnection(&ilu_idleConns, ans, ilu_lru);
#ifdef DEBUGGING_ENABLED
  if ((_ilu_DebugLevel & CONNECTION_DEBUG) != 0)
    {
      if (connection_incoming(ans))
	ilu_DebugPrintf ("new connection %p, transport %p, cinfo %s %s\n",
			 ans, bs, pinfo, peerinfo);
      else
	{
	  ilu_string t = _ilu_StringifyTinfo (tinfo, err);
	  if (ILU_ERRNOK(*err)) goto faild;
	  ilu_DebugPrintf ("new connection %p, transport %p, cinfo %s %s\n",
			   ans, bs, pinfo, t);
	  ilu_free(t);
	}
    }
#endif /* DEBUGGING_ENABLED */
  return (ans);
faild:
  if (connection_incoming(ans) && conn_peerinfo(ans) != NIL)
    ilu_free(conn_peerinfo(ans));
  else if (!connection_incoming(ans) && conn_tinfo(ans) != NIL)
    ilu_free(conn_tinfo(ans));
  if (ans->co_pinfo != NIL)
    ilu_free(ans->co_pinfo);
  if (ans->co_protocol_data != NIL)
    (*pr->pr_free_data_block) (ans->co_protocol_data);
  ilu_free(ans);
  return NIL;
}

/*L1, L2, Main unconstrained*/

ilu_boolean ilu_ThreadPerRequest(ilu_Connection conn)
{
  return (connection_protocol(conn)->pr_concurrent_requests);
}

/*L1 >= {conn's server}*/
/*L2 as implied by name*/

/*L1 >= {conn's server}*/
ilu_boolean
_ilu_TakeConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks)) * err)
{
  ilu_Mutex       sl = server_lock(conn->co_server);
  ILU_CLER(*err);
  _ilu_HoldMutex(sl);
  if (conn->co_ioing) {
    if (hard)
      ILU_ERR_CONS0(broken_locks, err, 0);
    else
      ILU_ERR_CONS0(bad_locks, err, 0);
    return FALSE;
  }
  DEBUG(LOCK_DEBUG,
	(stderr, "TakeConnIO(%p) succeeds.\n", conn));
  conn->co_ioing = TRUE;
  return TRUE;
}

/*L1 = {conn's server, cmu}*/
ilu_boolean
_ilu_EnterConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks)) * err)
{
  ilu_Mutex       sl = server_lock(conn->co_server);
  ILU_CLER(*err);
  _ilu_HoldMutex(sl);
  _ilu_HoldMutex(ilu_cmu);
  while (conn->co_ioing == TRUE) {
    /*
     * We shouldn't actually get here unless the I/O mutex is held
     * by another thread, in which case the runtime should have
     * provided a wait-on-condition operation.  We could also get
     * here if the caller mistakenly holds the I/O mutex.
     */
    DEBUG(LOCK_DEBUG,
	  (stderr, "EnterConnIO(%p) waiting.\n", conn));
    (void) ilu_CMWait2(conn->co_cc, sl, ilu_cmu, err);
    DEBUG(LOCK_DEBUG,
	  (stderr, "EnterConnIO(%p) resuming.\n", conn));
    if (ILU_ERRNOK(*err))
      return FALSE;
  }
  DEBUG(LOCK_DEBUG,
	(stderr, "EnterConnIO(%p) succeeds.\n", conn));
  conn->co_ioing = TRUE;
  return TRUE;
}

/*L1 >= {cmu, conn's server}; L1_sup < trmu*/
ilu_boolean
_ilu_ReleaseConnIO(ilu_Connection conn, ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Mutex       sl = server_lock(conn->co_server);
  _ilu_HoldMutex(ilu_cmu);
  _ilu_HoldMutex(sl);
  if (!conn->co_ioing) {
    if (hard)
      return ILU_ERR_CONS0(broken_locks, err, FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, FALSE);
  }
  DEBUG(LOCK_DEBUG,
	(stderr, "ReleaseConnIO(%p)\n", conn));
  if (conn->co_server->sr_closing == TRUE)
    _ilu_CloseIoingConnection(conn, ilu_FALSE);
  conn->co_ioing = FALSE;
  if (_ilu_CanCondition()) {
    ilu_Error       lerr;
    lerr = _ilu_NotifyCondition(conn->co_cc);
    ILU_ERR_SWITCH(lerr) {
      ILU_SUCCESS_CASE;
      ILU_ERR_CASE(CantCondition, v) {
	ILU_HANDLED(lerr);
	return ILU_ERR_CONS0(broken_locks, err, FALSE);
      }
    } ILU_ERR_ENDSWITCH;
  }
  return TRUE;
}

/*L1 = alt?{conn's server}:{conn's server, cmu}*/
static          ilu_boolean
FullEnterConnCall(ilu_Connection conn, ilu_Call call,
		  ilu_boolean hard, ilu_boolean alt,
		  ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Mutex       sl = server_lock(conn->co_server);
  _ilu_HoldMutex(sl);
  if (conn->co_mucall == call) {
    if (hard)
      return ILU_ERR_CONS0(broken_locks, err, FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, FALSE);
  }
  ILU_CLER(*err);
  while (conn->co_mucall != NIL) {
    /*
     * We shouldn't actually get here unless the call mutex is held
     * by another thread, in which case the runtime should have
     * provided a wait-on-condition operation.  We could also get
     * here if the caller mistakenly holds the call mutex on behalf
     * of a different call.
     */
    DEBUG(LOCK_DEBUG,
	  (stderr, "EnterConnCall(%p, %p) waits for %p\n",
	   conn, call, conn->co_mucall));
    (void) ilu_CMWait2(conn->co_cc, sl, (alt ? sl : ilu_cmu), err);
    DEBUG(LOCK_DEBUG,
	  (stderr, "EnterConnCall(%p, %p) resumes from %p\n",
	   conn, call, conn->co_mucall));
    if (ILU_ERRNOK(*err))
      return FALSE;
  }
  DEBUG(LOCK_DEBUG,
	(stderr, "EnterConnCall(%p, %p) succeeds\n",
	 conn, call));
  conn->co_mucall = call;
  if (conn->co_nOuts == 0 && !connection_closed(conn)) {
    _ilu_Assert(!alt, "FullEnterConnCall alt vs. co_nOuts");
    _ilu_HoldMutex(ilu_cmu);
    _ilu_UnlinkConnection(&ilu_idleConns, conn, ilu_lru);
  }
  return TRUE;
}

/*L1 = {conn's server, cmu}*/
ilu_boolean
_ilu_EnterConnCall(ilu_Connection conn, ilu_Call call,
		   ilu_boolean hard,
		   ILU_ERRS((bad_locks, broken_locks)) * err)
{
  return FullEnterConnCall(conn, call, hard, FALSE, err);
}

/* L1 = {conn's server}; conn->co_nOuts > 0 */
ilu_boolean
_ilu_AltEnterConnCall(ilu_Connection conn, ilu_Call call,
		      ilu_boolean hard,
		      ILU_ERRS((bad_locks, broken_locks)) * err)
{
  _ilu_Assert(conn->co_nOuts > 0, "AltEnterConnCall");
  return FullEnterConnCall(conn, call, hard, TRUE, err);
}

ilu_boolean
_ilu_ReleaseConnCall(ilu_Connection conn, ilu_Call call,
		     ilu_boolean hard,
		     ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Mutex       sl = server_lock(conn->co_server);
  _ilu_HoldMutex(sl);
  DEBUG(LOCK_DEBUG,
	(stderr, "ReleaseConnCall(%p, %p, holder=%p)\n",
	 conn, call, conn->co_mucall));
  if (conn->co_mucall != call) {
    if (ILU_ERRNOK(*err))
      return FALSE;
    else if (hard)
      return ILU_ERR_CONS0(broken_locks, err, FALSE);
    else
      return ILU_ERR_CONS0(bad_locks, err, FALSE);
  }
  conn->co_mucall = NIL;
  if (conn->co_nOuts == 0 && !connection_closed(conn)) {
    _ilu_HoldMutex(ilu_cmu);
    _ilu_LinkConnection(&ilu_idleConns, conn, ilu_lru);
  }
  if (_ilu_CanCondition()) {
    ilu_Error       lerr;
    lerr = _ilu_NotifyCondition(conn->co_cc);
    if (ILU_ERRNOK(lerr)) {
      ILU_HANDLED(lerr);
      if (ILU_ERRNOK(*err))
	return FALSE;
      return ILU_ERR_CONS0(broken_locks, err, FALSE);
    }
  }
  return TRUE;
}

/*L2, Main unconstrained*/
/*L1 >= {conn's server if k=psl; cmu if k=lru}*/

void _ilu_LinkConnection(ilu_ConnLinks *head, ilu_Connection conn,
			 ilu_ConnLinkKind k)
{
  _ilu_Assert((head->prev == NIL && head->next == NIL) ||
	      (head->prev != NIL && head->next != NIL &&
	       head->prev->co_links[k].next == NIL &&
	       head->next->co_links[k].prev == NIL),
	      "LinkConnection");
  conn->co_links[k].next = NIL;
  conn->co_links[k].prev = head->prev;
  if ( conn->co_links[k].prev != NIL )
       conn->co_links[k].prev->co_links[k].next = conn;
  else head->next = conn;
  head->prev = conn;
  return;
}

void _ilu_UnlinkConnection(ilu_ConnLinks *head, ilu_Connection conn,
			   ilu_ConnLinkKind k)
{
  _ilu_Assert(  (conn->co_links[k].prev == NIL)
		? head->next == conn
		: conn->co_links[k].prev->co_links[k].next == conn,
	      "UnlinkConnection 1");
  _ilu_Assert(  (conn->co_links[k].next == NIL)
		? head->prev == conn
		: conn->co_links[k].next->co_links[k].prev == conn,
	      "UnlinkConnection 2");
  if ( conn->co_links[k].prev != NIL )
       conn->co_links[k].prev->co_links[k].next = conn->co_links[k].next;
  else head->next			  = conn->co_links[k].next;
  if ( conn->co_links[k].next != NIL )
       conn->co_links[k].next->co_links[k].prev = conn->co_links[k].prev;
  else head->prev			  = conn->co_links[k].prev;
  return;
}

/*L1.sup < cmu; L2 unconstrained*/

static ilu_Connection handoff = NIL;
static ilu_boolean handoffReady = FALSE;
ilu_Condition   _ilu_connHandoffChange = NIL;

ilu_boolean
_ilu_HandOffNewConnection(ilu_Connection conn,
			  ilu_Error * err)
{
  ILU_ERRS((bad_locks, broken_locks, internal)) lerr;
  if (!ilu_Check(handoffReady, &lerr))
    goto faild;
  if (!ilu_EnterMutex(ilu_cmu, &lerr))
    goto faild;
  while (handoff != NIL) {
    if (!ilu_CMWait1(_ilu_connHandoffChange, ilu_cmu, &lerr))
      goto faild;
  }
  handoff = conn;
  if (!ilu_CondNotify(_ilu_connHandoffChange, &lerr))
    goto faild;
  if (!ilu_ExitMutex(ilu_cmu, TRUE, &lerr))
    goto faild;
  return TRUE;
faild:
  if (ILU_ERROK(*err))
    *err = lerr;
  else
    ILU_HANDLED(lerr);
  return FALSE;
}

ilu_Connection 
ilu_OtherNewConnection(ILU_ERRS((internal)) * err)
{
  ilu_Connection  ans;
  if (!ilu_Check(handoffReady, err))
    return NIL;
  if (!ilu_EnterMutex(ilu_cmu, err))
    return NIL;
  while (handoff == NIL) {
    if (!ilu_CMWait1(_ilu_connHandoffChange, ilu_cmu, err))
      return NIL;
  }
  ans = handoff;
  handoff = NIL;
  if (!ilu_CondNotify(_ilu_connHandoffChange, err))
    return NIL;
  if (!ilu_ExitMutex(ilu_cmu, TRUE, err))
    return NIL;
  return ans;
}

/*Main Invariant holds; L2 not further constrained*/
ilu_boolean
ilu_NewConnectionGetterForked(ILU_ERRS((internal)) * err)
{
  if (!ilu_Check(_ilu_connHandoffChange != NIL, err))
    return FALSE;
  handoffReady = TRUE;
  return TRUE;
}
