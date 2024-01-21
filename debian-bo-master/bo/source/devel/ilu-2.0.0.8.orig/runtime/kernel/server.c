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
/* $Id: server.c,v 1.64 1996/06/14 03:56:05 janssen Exp $ */
/* Last tweaked by Mike Spreitzer December 7, 1995 9:32 am PST */

#define _POSIX_SOURCE

#include "iluntrnl.h"
#include "server.h"
#include "object.h"
#include "type.h"
#include "connect.h"
#include "port.h"

#define NO_LANG 86

/*L1 >= {smu}*/
/*L2, Main unconstrained*/

static          HashTable /* sid -> ilu_Server */ Servers = NIL;
/* When sr_closing, effectively not in table. */

/*L1, L2, Main unconstrained*/

ilu_LanguageIndex _ilu_NLanguages = 0;
ilu_string      _ilu_LangNames[MAX_LANGUAGES] = {NIL};

ilu_LanguageIndex ilu_RegisterLanguage(ilu_string name)
{
  ilu_LanguageIndex li;
  _ilu_AutoSetDebugLevel();
  for (li = 0; li < _ilu_NLanguages; li++) {
    if (strcmp(name, _ilu_LangNames[li]) == 0)
      return li;
  }
  DEBUG(SERVER_DEBUG,
	(stderr, "ilu_RegisterLanguage(%s) returns %u.\n",
	 name, (unsigned) _ilu_NLanguages));
  _ilu_Assert(_ilu_NLanguages < MAX_LANGUAGES, "RegisterLanguage");
  _ilu_LangNames[_ilu_NLanguages] = name;
  return (_ilu_NLanguages++);
}

ilu_string ilu_IDOfServer ( ilu_Server s )
{
  return (s->sr_id);
}

ilu_cardinal ilu_CRC32OfIDOfServer ( ilu_Server s )
{
  return (s->sr_crc32);
}

ilu_boolean ilu_TrueServerP ( ilu_Server s )
{
  return (s->sr_true);
}

/*L1 >= {smu}*/
static void BeStarted(void)
{
  if (Servers == NIL)
    Servers = _ilu_hash_MakeNewTable(SERVER_HASHTABLESIZE,
				     NULLFN, NULLFN);
  return;
}

/*L1_sup < smu*/

ilu_ConsiderSbhResult
ilu_ConsiderSBH(ilu_string sbh,
		ilu_Server * ps,
		ILU_ERRS((BadProtocolInfo,
			  no_memory, inv_objref, internal)) * err)
{
  ilu_Server      s;
  ilu_string      ih = NIL, sid = NIL, cinfo = NIL, pinfo = NIL;
  ilu_TransportInfo tinfo = NIL;
  ilu_TransportCreator tcr;
  ilu_Protocol    p;
  ilu_ConsiderSbhResult ans;
  ilu_cardinal    cinfolen;
  *ps = NIL;
  if (!ilu_ParseSBH(sbh, &ih, &sid, NIL, &cinfo, &cinfolen, err))
    return ilucsr_err;
  _ilu_AcquireMutex(ilu_smu);
  BeStarted();
  s = (ilu_Server) _ilu_hash_FindInTable(Servers, sid);
  if (s == NIL) {
    ilu_ReleaseMutex(ilu_smu);
    ans = ilucsr_notReified;
  } else {
    ilu_AcquireMutex(s->sr_lock);
    if (s->sr_closing)
      ans = ilucsr_notReified;
    else if ((*ps = s, !s->sr_cfails))
      ans = ilucsr_noProblem;
    else if (s->sr_true) {
      ans = ilucsr_isTrue;
    } else if (!_ilu_ParseConnectInfo(cinfo, cinfolen, &pinfo, &tinfo,
				      err)) {
      ans = ilucsr_err;
    } else if ((tcr = _ilu_GetTransportCreator(tinfo, err)) == NIL) {
      ans = ilucsr_err;
    } else if ((p = _ilu_GetProtocolFromInfo(pinfo)) == NIL) {
      ans = ILU_ERR_CONS1(BadProtocolInfo, err, x, pinfo, ilucsr_err);
      pinfo = NIL;
    } else {
      if (_ilu_CompareTinfo(s->sr_tinfo, tinfo)
	  && strcmp(s->sr_pinfo, pinfo) == 0) {
	ans = ilucsr_noNews;
      } else {
	HashEnumerator  he;
	ilu_refany      key, data;
	(*s->sr_tcr->tcr_close) (s->sr_tcr);
	ilu_free(s->sr_tinfo);
	ilu_free(s->sr_pinfo);
	s->sr_tinfo = tinfo;
	tinfo = NIL;
	s->sr_tcr = tcr;
	s->sr_pinfo = pinfo;
	pinfo = NIL;
	s->sr_protocol = p;
	s->sr_cfails = ilu_FALSE;
	_ilu_hash_BeginEnumeration(s->sr_objs, &he);
	while (_ilu_hash_Next(&he, &key, &data)) {
	  ilu_Object      obj = (ilu_Object) data;
	  if (obj->ob_sbh != NIL) {
	    ilu_free(obj->ob_sbh);
	    obj->ob_sbh = NIL;
	  }
	}
	ans = ilucsr_changed;
      }
      *err = ILU_NO_ERR;
    }
    ilu_ReleaseMutex(s->sr_lock);
    ilu_ReleaseMutex(ilu_smu);
  }
  ilu_free(ih);
  ilu_free(sid);
  ilu_free(tinfo);
  ilu_free(pinfo);
  return ans;
}

ilu_Server
_ilu_FindServer(ilu_string serverID, ilu_boolean add,
		ilu_string cinfo, ilu_cardinal cinfolen,
		ILU_ERRS((BadProtocolInfo, internal,
			  no_memory, inv_objref)) * err)
{
  ilu_Server      s;
  ilu_string      pinfo = NIL;
  ilu_TransportInfo tinfo = NIL;
  ilu_Protocol    p;
  ilu_TransportCreator tcr;
  _ilu_AcquireMutex(ilu_smu);
  BeStarted();
  s = (ilu_Server) _ilu_hash_FindInTable(Servers, serverID);
  if (s != NIL && s->sr_closing)
    /* Why is it OK to access sr_closing outside s? */
    s = NIL;
  if (s != NIL || !add)
    ILU_CLER(*err);
  else if (!_ilu_ParseConnectInfo(cinfo, cinfolen, &pinfo, &tinfo, err))
    0;
  else if ((tcr = _ilu_GetTransportCreator(tinfo, err)) == NIL)
    { ilu_free (pinfo); ilu_free(tinfo); }
  else if ((p = _ilu_GetProtocolFromInfo(pinfo)) == NIL)
    ILU_ERR_CONS1(BadProtocolInfo, err, x, pinfo, ilu_free(tinfo));
  else {
    if (s != NIL) {
      _ilu_Assert(_ilu_hash_RemoveFromTable(Servers, serverID) == s,
		  "FindServer: table strange");
      /*
       * When we figure out how to free servers, it might happen
       * here.
       */ 
    }
    s = (ilu_Server) ilu_malloc(sizeof(struct _ilu_Server_s));
    *err = ILU_NO_ERR;
    s->sr_lock = _ilu_CreateMutex("server ", serverID);
    s->sr_true = ilu_FALSE;
    s->sr_id = _ilu_Strdup(serverID);
    s->sr_crc32 = _ilu_CRC32 ((ilu_bytes) serverID, (ilu_cardinal) strlen(serverID));
    s->sr_true_language = NO_LANG;
    s->sr_tinfo = tinfo;
    s->sr_tcr = tcr;
    s->sr_pinfo = pinfo;
    s->sr_protocol = p;
    s->sr_closing = ilu_FALSE;
    s->sr_cfails = ilu_FALSE;
    s->sr_connHead.next = s->sr_connHead.prev = NIL;
    s->sr_ports = NIL;
    s->sr_local_port = NIL;
    s->sr_objs = _ilu_hash_MakeNewTable(OBJECT_HASHTABLESIZE,
					NULLFN, NULLFN);
    s->sr_singles = _ilu_hash_MakeNewTable(3, _ilu_hash_HashPointer,
					_ilu_hash_PointerCompare);
    s->sr_objtab = NIL;
    s->sr_default_port = NIL;
    _ilu_Assert((int) _ilu_hash_AddToTable(Servers, server_id(s), s),
		"FindServer AddToTable Servers");
#ifdef ENABLE_DEBUGGING
    DEBUG(CONNECTION_DEBUG,
	  (stderr, "%s %p <%s> via pinfo=<%s>, tinfo=",
	   "_ilu_FindServer:  Created new server", s, serverID, s->sr_pinfo));
    if ((_ilu_DebugLevel & CONNECTION_DEBUG) != 0)
      _ilu_PrintTinfo(s->sr_tinfo);
    DEBUG(CONNECTION_DEBUG,
	  (stderr, "\n"));
#endif /* def ENABLE_DEBUGGING */
  /* FREETOKEN(tinfo); FREETOKEN(pinfo) when server closed */
  }
  _ilu_ReleaseMutex(ilu_smu);
  return (s);
}

ilu_Server
ilu_CreateTrueServer(ilu_string id, ilu_ObjectTable objtab,
		     ilu_LanguageIndex language)
{
  ilu_Server      ans;
  _ilu_AcquireMutex(ilu_smu);
  BeStarted();
  ans = (ilu_Server) _ilu_hash_FindInTable(Servers, id);
  if (ans != NIL && !ans->sr_closing) {
    DEBUG(0xFFFF, (stderr,
		   "ilu_CreateTrueServer:  given non-new id %s.\n",
		   id));
    _ilu_ReleaseMutex(ilu_smu);
    return NIL;
  }
  if (ans != NIL) {
    _ilu_Assert(_ilu_hash_RemoveFromTable(Servers, id) == ans,
		"CreateTrueServer: table strange");
    /* Maybe, someday, free(ans) here. */
  }
  ans = (ilu_Server) ilu_malloc(sizeof(*ans));
  ans->sr_lock = _ilu_CreateMutex("server ", id);
  ans->sr_true = ilu_TRUE;
  ans->sr_id = id;
  ans->sr_crc32 = _ilu_CRC32 ((ilu_bytes) id, (ilu_cardinal) strlen(id));
  ans->sr_true_language = language;
  ans->sr_tinfo = NIL;
  ans->sr_pinfo = NIL;
  ans->sr_tcr = NIL;
  ans->sr_protocol = NIL;
  ans->sr_closing = ilu_FALSE;
  ans->sr_cfails = ilu_FALSE;
  ans->sr_connHead.next = ans->sr_connHead.prev = NIL;
  ans->sr_ports = NIL;
  ans->sr_local_port = NIL;
  ans->sr_objs = _ilu_hash_MakeNewTable(OBJECT_HASHTABLESIZE,
					NULLFN, NULLFN);
  ans->sr_singles = _ilu_hash_MakeNewTable(3, _ilu_hash_HashPointer,
					 _ilu_hash_PointerCompare);
  ans->sr_objtab = objtab;
  ans->sr_default_port = NIL;
  _ilu_Assert((int) _ilu_hash_AddToTable(Servers, server_id(ans), ans),
	      "CreateTrueServer AddToTable Servers");
  DEBUG(SERVER_DEBUG,
	(stderr, "ilu_CreateTrueServer:  created new server \"%s\", objtable %p, true lang \"%s\".\n",
	 server_id(ans), objtab, _ilu_LangNames[language]));
  _ilu_ReleaseMutex(ilu_smu);
  return (ans);
}

/*L1_sup < s*/
void ilu_SetServerDefaultPort(ilu_Server s, ilu_Port p)
{
  if (port_server(p) == s) {
      _ilu_AcquireMutex(server_lock(s));
      server_default_port(s) = p;
      _ilu_ReleaseMutex(server_lock(s));
    }
  else {
      /* Bitch to caller, when we get our error system. */
      return;
    }
}

/*L1 >= {s}*/
void _ilu_ClearConnectionFromServer (ilu_Connection c, ilu_Server s)
{
  _ilu_HoldMutex(server_lock(s));
  _ilu_Assert(connection_server(c) == s, "ClearConnectionFromServer");
  if (connection_incoming(c)) {
    ilu_Port        p = c->co_port;
    _ilu_UnlinkConnection(&p->po_connHead, c, ilu_psl);
    if (port_connections(p) == NIL && port_closed(p))
      _ilu_ClearPortFromServer(p, s);
    return;
  } else {
    _ilu_UnlinkConnection(&s->sr_connHead, c, ilu_psl);
    return;
  }
}

/*L1 >= {cmu, s}, L1_sup < trmu; L2, Main unconstrained*/

static          ilu_boolean
CloseNonIoingConns(ilu_Server s, ilu_Connection first,
		   ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Connection  cur = first, next;
  while (cur != NIL) {
    next = cur->co_links[ilu_psl].next;
    if (cur->co_ioing != TRUE) {
      cur->co_ioing = TRUE;
      _ilu_CloseIoingConnection(cur, ilu_FALSE);
      if (!_ilu_ReleaseConnIO(cur, TRUE, err))
	return FALSE;
    }
    cur = next;
  }
  return ILU_CLER(*err);
}

static void
DisconnectServer(ilu_Server s,
		 ILU_ERRS((bad_locks, broken_locks)) * err)
{
  ilu_Port        p = s->sr_local_port;
  if (p != NIL)
    if (!CloseNonIoingConns(s, p->po_connHead.next, err))
      return;
  p = server_ports(s);
  while (p != NIL) {
    if (!CloseNonIoingConns(s, p->po_connHead.next, err))
      return;
    p = p->po_next;
  }
  (void) CloseNonIoingConns(s, server_connections(s), err);
  return;
}

/*L1 >= {s}*/
static void 
_ilu_RemSingleton(ilu_Server s, ilu_Class t, ilu_Object o)
{
  ilu_cardinal             i, l;
  ilu_Object      o2;
  if (!class_singleton(t))
    return;
  o2 = _ilu_hash_RemoveFromTable(server_singles(s), t);
  if (o2 == NIL)
    return;			/* already visited */
  _ilu_Assert(o2 == o, "RemSingleton");
  l = class_superclass_count(t);
  for (i = 0; i < l; i++)
    _ilu_RemSingleton(s, class_superclass(t, i), o);
  return;
}

ILU_ERRS((BadDataStructure, KernelBroken, bad_locks, broken_locks))
_ilu_ServerRemoveObject(ilu_Server s, ilu_Object obj)
{
  ilu_Error       err = ILU_NO_ERR;
  if (s->sr_objs == NIL)
    return ILU_ERR_CONS3(BadDataStructure, &err, data_str_ptr, s,
			 supposed_type, "ilu_Server",
			 why_bad, "has NIL sr_objs", err);
  if (_ilu_hash_RemoveFromTable(s->sr_objs, object_ih(obj)) != obj)
    return ILU_ERR_CONS1(KernelBroken, &err, what,
			 "ServerRemoveObject: obj not in table",
			 err);
  _ilu_RemSingleton(s, object_class(obj), obj);
  if (_ilu_hash_PairsInTable(s->sr_objs) > 0) {
  } else if (s->sr_closing == TRUE) {
    if (_ilu_hash_PairsInTable(s->sr_singles) > 0)
      return ILU_ERR_CONS1(KernelBroken, &err, what,
			   "sr_objs empty but sr_singles not",
			   err);
    _ilu_hash_FreeHashTable(s->sr_objs, NULLFN, NULLFN);
    s->sr_objs = NIL;
    _ilu_hash_FreeHashTable(s->sr_singles, NULLFN, NULLFN);
    s->sr_singles = NIL;
  } else
    DisconnectServer(s, &err);
  return err;
}

/*L1 >= {s}; L2, Main unconstrained*/

int
ilu_ScanServerObjs(ilu_Server s,
		   ilu_objectCallback cb,
		   ilu_refany rock)
{
  HashEnumerator  he;
  ilu_refany      key, data;
  if (s->sr_objs == NIL)
    return 0;
  _ilu_hash_BeginEnumeration(s->sr_objs, &he);
  while ((s->sr_objs != NIL) && (_ilu_hash_Next(&he, &key, &data))) {
    ilu_Object      obj = (ilu_Object) data;
    int             ans;
    ans = (*cb)(obj, rock);
    if (ans != 0)
      return ans;
  }
  return 0;
}

ilu_cardinal ilu_NumObjsInServer(ilu_Server s)
{
  if (s->sr_objs == NIL)
    return 0;
  else
    return _ilu_hash_PairsInTable(s->sr_objs);
}

static ilu_cardinal
  CountIoingConns(ilu_Server s, ilu_Connection first)
{
  ilu_cardinal    ans = 0;
  ilu_Connection  cur;
  for (cur = first; cur != NIL; cur = cur->co_links[ilu_psl].next)
    if (cur->co_ioing == TRUE)
      ans++;
  return ans;
}

ilu_cardinal ilu_NumIoingConnsOfServer(ilu_Server s)
{
  ilu_cardinal    ans = 0;
  ilu_Port        p = s->sr_local_port;
  if (p != NIL)
    ans = CountIoingConns(s, p->po_connHead.next);
  p = server_ports(s);
  while (p != NIL) {
    ans += CountIoingConns(s, p->po_connHead.next);
    p = p->po_next;
  }
  ans += CountIoingConns(s, server_connections(s));
  return ans;
}

ilu_boolean
_ilu_ServerEmptyP(ilu_Server s)
{
  return (ilu_NumObjsInServer(s) == 0);
}

/* L1 = {cmu, smu, s}; L2, Main unconstrained */
void ilu_InnerBankServer(ilu_Server s)
{
  ilu_PreBankServer(s);
  if (Servers != NIL) {
    ilu_string      sid = server_id(s);
    ilu_Server      s2 = _ilu_hash_FindInTable(Servers, sid);
    if (s2 == s)
      _ilu_Assert(_ilu_hash_RemoveFromTable(Servers, sid) == s,
		  "BankServer RemoveFromTable");
  }
  return;
}

/* L1 = {cmu, s}; L2, Main unconstrained */
void ilu_PreBankServer(ilu_Server s)
{
  ILU_ERRS((bad_locks, broken_locks)) lerr;
  if (s->sr_closing == TRUE)
    return;
  s->sr_closing = TRUE;
  if (s->sr_objtab != NIL) {
    (*s->sr_objtab->ot_free_self) (s->sr_objtab);
    s->sr_objtab = NIL;
  }
  DisconnectServer(s, &lerr);
  ILU_MUST_BE_SUCCESS(lerr);
  if (server_is_true(s)) {
    ilu_Port        cur = server_ports(s), next;
    while (cur != NIL) {
      next = cur->po_next;
      _ilu_ClosePort(cur);
      cur = next;
    }
    if (s->sr_local_port != NIL)
      _ilu_ClosePort(s->sr_local_port);
  }
  if (_ilu_hash_PairsInTable(s->sr_objs) == 0) {
    _ilu_Assert(_ilu_hash_PairsInTable(s->sr_singles) == 0,
		"BankServer: singles not empty too");
    _ilu_hash_FreeHashTable(s->sr_objs, NULLFN, NULLFN);
    s->sr_objs = NIL;
    _ilu_hash_FreeHashTable(s->sr_singles, NULLFN, NULLFN);
    s->sr_singles = NIL;
  }
  return;
}


/*L1 < cmu; L2, Main unconstrained*/

void ilu_BankServer (ilu_Server s)
{
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireMutex(ilu_smu);
  _ilu_AcquireMutex(server_lock(s));
  ilu_InnerBankServer(s);
  _ilu_ReleaseMutex(server_lock(s));
  _ilu_ReleaseMutex(ilu_smu);
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

/* L1 < gcmu; L2, Main unconstrained */
int
ilu_BankAndScanServer(ilu_Server s,
		      ilu_objectCallback cb,
		      ilu_refany rock,
		      ilu_cardinal * nconns)
{
  ilu_boolean     istrue = ilu_TrueServerP(s);
  int             ans;
  if (istrue)
    _ilu_AcquireMutex(ilu_gcmu);
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireMutex(ilu_smu);
  _ilu_AcquireMutex(server_lock(s));
  (void) ilu_InnerBankServer(s);
  _ilu_ReleaseMutex(ilu_smu);
  ans = ilu_ScanServerObjs(s, cb, rock);
  if (nconns != NIL)
    *nconns = ilu_NumIoingConnsOfServer(s);
  _ilu_ReleaseMutex(server_lock(s));
  _ilu_ReleaseMutex(ilu_cmu);
  if (istrue)
    _ilu_ReleaseMutex(ilu_gcmu);
  return ans;
}


/*before: 				       L1 disjoint {cmu, server};
  before: cl collectible		    => L1  not >=  {gcmu};
  before: cl collectible & server surrogate => Main Invariant holds;
  after:  Inside(server, cl)*/
void ilu_EnterServer(ilu_Server server, ilu_Class cl)
{
  if (class_collectible(cl) && server->sr_true)
      _ilu_AcquireMutex(ilu_gcmu);
  _ilu_AcquireMutex(ilu_cmu);
  _ilu_AcquireMutex(server_lock(server));
  return;
}

/*before: Inside(server, cl);
  after:				      L1 disjoint {cmu, server};
  after: cl collectible			   => L1  not >=  {gcmu};
  after: cl collectible & server surrogate => Main Invariant holds*/
void ilu_ExitServer(ilu_Server server, ilu_Class cl)
{
  _ilu_ReleaseMutex(server_lock(server));
  _ilu_ReleaseMutex(ilu_cmu);
  if (class_collectible(cl) && server->sr_true)
      _ilu_ReleaseMutex(ilu_gcmu);
  return;
}

/*Inside(server, result's type)*/
ilu_Object _ilu_FindObjectInServer(ilu_string ih, ilu_Server s)
{
  ilu_Object      o;
  o = (ilu_Object) _ilu_hash_FindInTable(server_objs(s), ih);
  if (o == NIL && server_is_true(s) && server_objtab(s) != NIL) {
    o = (*server_objtab(s)->ot_object_of_ih) (server_objtab(s), ih);
    if (o == NIL) {
    } else
      _ilu_Assert(_ilu_hash_FindInTable(server_objs(s), ih) == o,
      "_ilu_FindObjectInServer: obj tab returned uninterned obj");
  }
  return (o);
}
