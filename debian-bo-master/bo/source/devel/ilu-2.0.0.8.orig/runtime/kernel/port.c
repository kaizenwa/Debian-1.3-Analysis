/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: port.c,v 1.54 1996/05/29 19:14:48 janssen Exp $ */
/* Last edited by Mike Spreitzer November 1, 1995 3:25 pm PST */

#define _POSIX_SOURCE

#include "iluntrnl.h"

#include "port.h"
#include "mooring.h"
#include "protocol.h"
#include "transprt.h"
#include "object.h"
#include "server.h"

/*Main Invariant holds; L2 not further constrained*/

ilu_boolean
ilu_SetConnectionRequestHandler(ilu_Port port,
				ilu_TransportInputHandler tih,
				ilu_refany tih_rock,
				ILU_ERRS((no_memory, imp_limit,
					  no_resources, bad_param,
					  bad_locks, internal,
					  broken_locks)) * err)
{
  ilu_Mooring     m = port->po_mooring;
  ilu_Server      s = port_server(port);
  ilu_boolean     closed;
  if (!ilu_EnterMutex(server_lock(s), err))
    return FALSE;
  closed = port->po_closed;
  if (!ilu_ExitMutex(server_lock(s), TRUE, err))
    return FALSE;
  if (closed)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_closed, FALSE);
  return ((*m->mo_set_req_handler) (m, s, tih, tih_rock, err));
}

ilu_boolean
ilu_WaitForPortConnectionRequest(ilu_Port port)
{
  ilu_Mooring     m = port->po_mooring;
  ilu_Server      s = port_server(port);
  ILU_ERRS((interrupted, broken_locks)) lerr;
  (void) ilu_EnterMutex(server_lock(s), &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  if (!port->po_closed) {
    ilu_boolean     ans;
    ans = ((*m->mo_wait_for_req) (m, s, &lerr));
    ILU_MUST_BE_SUCCESS(lerr);
    return ans;
  }
  (void) ilu_ExitMutex(server_lock(s), TRUE, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  return FALSE;
}

ilu_Connection ilu_HandleNewConnection (ilu_Port port,
					ilu_boolean *closed)
{
  ilu_Transport t = NIL;
  ilu_Connection c = NIL;
  ilu_Server s = port->po_server;
  ilu_Mooring m = port->po_mooring;
  ilu_TransportCreator tcr = NIL;
  ilu_integer dfd = 0;
  ilu_Error err;
  ilu_string peerinfo = NIL;
  ilu_Passport pp;

  pp = ilu_CreatePassport(NIL, &err);
  ILU_MUST_BE_SUCCESS(err);

  _ilu_AcquireMutex(server_lock(s));
  *closed = port->po_closed;
  if (!port->po_closed) {
    tcr = port->po_tcr;
    dfd = (*m->mo_dfd) (m, TRUE);
  }
  _ilu_ReleaseMutex(server_lock(s));
  if (*closed)
      return (NIL);
  _ilu_AcquireMutex(ilu_cmu);
  if (ilu_fdbudget < ilu_fdstaken + dfd) {
    err = _ilu_ReduceFdsTo(ilu_fdbudget - dfd);
    ILU_MUST_BE_SUCCESS(err);
    if ((ilu_fdbudget < ilu_fdstaken + dfd) && (dfd > 0)) {
      DEBUG(CONNECTION_DEBUG,
	    (stderr,
	     "HandleNewConnection: FD budget exhausted.\n"));
      _ilu_ReleaseMutex(ilu_cmu);
      return NIL;
    }
  }
  t = (*m->mo_accept_connection)(m, &peerinfo, &dfd, pp, &err);
  ilu_DeltaFD(dfd);
  if (t != NIL) {
    ILU_MUST_BE_SUCCESS(err);
    _ilu_AcquireMutex(server_lock(s));
    if (port->po_closed) {
      ilu_integer cdfd;
      *closed = TRUE;
      transport_close(t, &cdfd, &err);
      ilu_DeltaFD(-cdfd);
      ILU_MUST_BE_SUCCESS(err);
      ilu_free(peerinfo);
      ilu_DestroyPassport(pp, &err);
      ILU_MUST_BE_SUCCESS(err);
      return (NIL);
    }
    c = _ilu_CreateConnection(t, NIL, peerinfo, port_protocol(port),
			      port->po_pinfo, port,
			      port_server(port), pp, &err);
    ILU_MUST_BE_SUCCESS(err);
    DEBUG(CONNECTION_DEBUG,
	  (stderr, "ilu_HandleNewConnection:  new connection <%s> to server \"%s\"\n",
	   (peerinfo == NIL) ? "??" : peerinfo, server_id(s)));
    _ilu_ReleaseMutex(server_lock(s));
    ilu_free(peerinfo);
  }
  ILU_MUST_BE_SUCCESS(err);
  _ilu_ReleaseMutex(ilu_cmu);
  return (c);
}

int call_cache_size = 5;


ilu_Port ilu_CreatePort (ilu_Server s,
			 ilu_string protocolinfo,
			 ilu_TransportInfo mooringinfo,
			 ilu_Passport pp,
			 ilu_Error *err)
{
  ilu_Protocol    p;
  ilu_TransportCreator tcr;
  ilu_Port        ans = NIL;
  ilu_integer     dfd, cdfd;
  ilu_boolean     port_is_local;
  static const ilu_CachedCall c0 = {"\0", 0, NIL, NIL, {NIL, 0}};

  if (protocolinfo == NIL || mooringinfo == NIL)
    return (NIL);

  /* Snoop the tinfo.  Rather gruesome. */
  port_is_local = (mooringinfo[0] != NIL && mooringinfo[1] == NIL &&
		   _ilu_casefree_ncmp(mooringinfo[0], "inmem", 5) == 0 &&
		   (mooringinfo[0][5] == 0 || mooringinfo[0][5] == '_') );

  if ((tcr = _ilu_GetTransportCreator(mooringinfo, err)) == NIL) {
    return NIL;
  }
  if ((p = _ilu_GetProtocolFromInfo(protocolinfo)) == NIL)
    return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_pinfo, NIL);

  dfd = (*tcr->tcr_dfd) (tcr, TRUE);

  _ilu_AcquireMutex(ilu_cmu);
  *err = _ilu_ReduceFdsTo(ilu_fdbudget - dfd);
  if (ILU_ERRNOK(*err))
    return NIL;
  if ((ilu_fdbudget < ilu_fdstaken + dfd) && (dfd > 0)) {
    ILU_ERR_CONS1(no_resources, err, minor, ilu_nrm_fds, NIL);
    ILU_ERRPRINTF("%s %s %s because over FD budget\n",
	    "ilu_CreatePort:  can't create port",
	    protocolinfo, mooringinfo[0]);
    _ilu_ReleaseMutex(ilu_cmu);
    return NIL;
  }
  ilu_DeltaFD(dfd);
  _ilu_ReleaseMutex(ilu_cmu);
  ans = (ilu_Port) ilu_malloc(sizeof(*ans));
  ans->po_server = s;
  ans->po_tcr = tcr;
  ans->po_pinfo = _ilu_Strdup(protocolinfo);
  ans->po_protocol = p;
  ans->po_mooring = ((*tcr->tcr_createMooring)
		     (tcr, &ans->po_tinfo, TRUE, &cdfd, pp, err));
  if (ILU_ERRNOK(*err))
    {
      ilu_free(ans->po_pinfo);
      ilu_free(ans);
      return NIL;
    }
  ans->po_closed = FALSE;
  ans->po_next = NIL;
  ans->po_connHead.next = ans->po_connHead.prev = NIL;

  _ilu_AcquireMutex(ilu_cmu);
  ilu_DeltaFD(cdfd - dfd);
  _ilu_ReleaseMutex(ilu_cmu);
  _ilu_AcquireMutex(server_lock(s));
  {				/* Under server lock: */
    if (!port_is_local) {
      ans->po_next = server_ports(s);
      server_ports(s) = ans;
      if (server_default_port(s) == NIL)
	server_default_port(s) = ans;
    } else
      server_local_port(s) = ans;
    if (!tcr->tcr_reliable) {
      int             i;
      ans->po_call_cache = (ilu_CachedCall *)
	ilu_malloc(call_cache_size
		   * sizeof(ilu_CachedCall));
      _ilu_Assert(ans->po_call_cache != NIL,
		  "CreatePort: ilu_malloc call cache failed");
      ans->po_call_cache_size = call_cache_size;
      for (i = 0; i < ans->po_call_cache_size; i++)
	ans->po_call_cache[i] = c0;
    } else {
      ans->po_call_cache = NIL;
      ans->po_call_cache_size = 0;
    }
    ans->po_call_cache_finger = 0;
  }
  _ilu_ReleaseMutex(server_lock(s));
#ifdef ENABLE_DEBUGGING
  if ((_ilu_DebugLevel & (EXPORT_DEBUG|SERVER_DEBUG)) != 0)
    {
      ilu_DebugPrintf ("ilu_CreatePort:  new port %p (pinfo=%s, tinfo=",
	 ans, protocolinfo);
      for (dfd = 0;  mooringinfo[dfd] != NIL;  dfd++)
	ilu_DebugPrintf ("%s%s", (dfd > 0) ? "," : "", mooringinfo[dfd]);
      ilu_DebugPrintf (") %s%son server \"%s\".\n",
		       (port_is_local) ? "(local) " : "",
		       (server_default_port(s) == ans) ? "(default) " : "",
		       server_id(s));
    }
#endif /* ENABLE_DEBUGGING */
  ILU_CLER(*err);
  return (ans);
}

/*L1_sup < cmu*/
/*L2, Main unconstrained*/

void ilu_ClosePort(ilu_Port port)
{
  ilu_Server s = port->po_server;
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireMutex(server_lock(s));
  _ilu_ClosePort(port);
  _ilu_ReleaseMutex(server_lock(s));
  _ilu_ReleaseMutex(ilu_cmu);
}

void ilu_DestroyPort(ilu_Port port)
{
  ilu_Server      s = port->po_server;
  int             i;
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireMutex(server_lock(s));
  DEBUG(EXPORT_DEBUG,
	(stderr, "ilu_DestroyPort:  port %p on server %s being destroyed\n",
	 port, server_id(s)));
  _ilu_ClosePort(port);
  _ilu_Assert(port_connections(port) == NIL,
	      "DestroyPort: some connections still exist");
  if (port->po_call_cache != NIL)
    for (i = 0; i < port->po_call_cache_size; i++)
      if (port->po_call_cache[i].cc_replyMsg.msg_base != NIL)
	ilu_free(port->po_call_cache[i].cc_replyMsg.msg_base);
  ilu_free(port->po_tinfo);
  ilu_free(port->po_pinfo);
  ilu_free(port);
  _ilu_ReleaseMutex(server_lock(s));
  _ilu_ReleaseMutex(ilu_cmu);
}


/*L1 >= {port's server}*/
void _ilu_ClearPortFromServer(ilu_Port port, ilu_Server s)
{
  ilu_Port       *pp;
  if (server_local_port(s) == port)
    server_local_port(s) = NIL;
  else {
    for (pp = &server_ports(s); (*pp) != NIL; pp = &port_next(*pp)) {
      if ((*pp) == port) {
	*pp = port_next(port);
	return;
      }
    }
    _ilu_Assert(0, "ClosePort: not found in server");
  }
}

/*L1_sup = port's server; L1 >= {cmu}*/
void
_ilu_ClosePort(ilu_Port port)
{
  ILU_ERRS((internal))       err = ILU_INIT_NO_ERR;
  ilu_Server      s = port->po_server;
  ilu_Mooring     m = port->po_mooring;
  ilu_TransportCreator tcr = port->po_tcr;
  ilu_integer     dfd;
  if (port_closed(port))
    return;
  port->po_closed = TRUE;
  (*tcr->tcr_close)(tcr);
  (*m->mo_close) (m, &dfd, &err);
  ILU_MUST_BE_SUCCESS(err);
  ilu_DeltaFD(-dfd);
  if (s->sr_default_port == port) {
    s->sr_default_port = s->sr_ports;
    while (s->sr_default_port != NIL && port_closed(s->sr_default_port))
      s->sr_default_port = s->sr_default_port->po_next;
  }
  if (port_connections(port) == NIL)
    _ilu_ClearPortFromServer(port, s);
  return;
}
